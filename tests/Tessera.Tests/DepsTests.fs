module Tessera.Tests.DepsTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Deps
open Tessera.Tests.Fixtures

/// The sample model plus expressions exercising each resolution rule.
let private model () =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]
    let date = db.Model.Tables.["Date"]

    sales.Measures.Add(new Measure(Name = "Avg", Expression = "DIVIDE([Total], COUNTROWS(Sales))"))

    sales.Measures.Add(
        new Measure(
            Name = "With Vars",
            Expression =
                "VAR Sales = 1 // shadows the table\nVAR t = ADDCOLUMNS('Date', \"Local\", 1)\n"
                + "RETURN SUMX(t, [Local] + t[Year]) + Sales + 'Date'[Year] + Double(2)"
        )
    )

    sales.Measures.Add(new Measure(Name = "Broken", Expression = "'Nope'[X] + [Missing] + Sales[Gone]"))
    sales.Measures.Add(new Measure(Name = "Commented", Expression = "1 // [Total]\n/* 'Date' */"))
    sales.Columns.Add(new CalculatedColumn(Name = "Doubled", Expression = "[Amount] * 2"))

    sales.Measures.["Total"].FormatStringDefinition <-
        new FormatStringDefinition(Expression = "IF([Avg] > 1, \"0\", \"0.0\")")

    date.Columns.["Month"].SortByColumn <- date.Columns.["DateKey"]

    let readers = db.Model.Roles.["Readers"]
    readers.TablePermissions.Add(new TablePermission(Table = sales, FilterExpression = "[Amount] > 0"))

    db.Model.Functions.Add(new Function(Name = "AddOne", Expression = "(Sales: INT64) => Sales + 1 + Double(Sales)"))
    db

let private targets (refs: Reference list) =
    refs
    |> List.map (fun r ->
        match r.Target with
        | Resolved t -> ObjectRef.format t, r.Via
        | Unresolved text -> "? " + text, r.Via)

let private refOf s =
    match ObjectRef.parse s with
    | Ok r -> r
    | Error e -> failtestf "%A" e

[<Tests>]
let tests =
    testList "Deps" [
        let g = lazy (build (model ()))
        let deps s = dependsOn g.Value (refOf s) |> targets

        let refBy s =
            referencedBy g.Value (refOf s)
            |> List.map (fun r -> ObjectRef.format r.From, r.Via)

        testCase "[Measure] and bare table names"
        <| fun () ->
            Expect.equal
                (deps "measure:Sales/Avg")
                [ "measure:Sales/Total", "expression"; "table:Sales", "expression" ]
                ""

        testCase "unqualified [Column] resolves in the object's own table"
        <| fun () -> Expect.equal (deps "column:Sales/Doubled") [ "column:Sales/Amount", "expression" ] ""

        testCase "VAR names shadow tables; locally made columns and VAR tables are skipped"
        <| fun () ->
            Expect.equal
                (deps "measure:Sales/With Vars")
                [
                    "table:Date", "expression"
                    "column:Date/Year", "expression"
                    "function:Double", "expression"
                ]
                ""

        testCase "unresolved references are kept, as written"
        <| fun () ->
            Expect.equal
                (deps "measure:Sales/Broken")
                [
                    "? 'Nope'[X]", "expression"
                    "? [Missing]", "expression"
                    "? Sales[Gone]", "expression"
                ]
                ""

        testCase "comments are ignored"
        <| fun () -> Expect.isEmpty (deps "measure:Sales/Commented") ""

        testCase "format string expressions and RLS filters are DAX too"
        <| fun () ->
            Expect.contains (deps "measure:Sales/Total") ("measure:Sales/Avg", "formatStringDefinition.expression") ""

            Expect.equal (deps "role:Readers") [ "column:Sales/Amount", "tablePermissions.Sales.filterExpression" ] ""

        testCase "UDF parameters shadow tables; UDF calls are dependencies"
        <| fun () -> Expect.equal (deps "function:AddOne") [ "function:Double", "expression" ] ""

        testCase "calculation items"
        <| fun () ->
            Expect.equal
                (deps "calcitem:Time Intelligence/YTD")
                // A qualified 'Date'[DateKey] is a column reference, not a separate table one.
                [ "column:Date/DateKey", "expression" ]
                ""

        testCase "structural links: sort-by, levels, relationships"
        <| fun () ->
            let r = refBy "column:Date/DateKey"
            Expect.contains r ("column:Date/Month", "sortByColumn") ""
            Expect.contains r ("relationship:Sales-Date", "toColumn") ""
            Expect.contains r ("calcitem:Time Intelligence/YTD", "expression") ""
            Expect.contains (refBy "column:Date/Year") ("level:Date/Calendar/Year", "column") ""

        testCase "line numbers"
        <| fun () ->
            let lines =
                dependsOn g.Value (refOf "measure:Sales/With Vars")
                |> List.map (fun r -> r.Line)
                |> List.distinct

            Expect.equal lines [ Some 2; Some 3 ] "'Date' on line 2, the rest on line 3"

        testCase "DAX references carry their source span; structural links don't"
        <| fun () ->
            let db = model ()
            let dax = db.Model.Tables.["Sales"].Measures.["With Vars"].Expression

            for r in dependsOn g.Value (refOf "measure:Sales/With Vars") do
                match r.Start, r.Length with
                | Some start, Some length -> Expect.equal (dax.Substring(start, length)) r.Text ""
                | _ -> failtest "a DAX reference without a span"

            for r in g.Value.References |> List.filter (fun r -> r.Line.IsNone) do
                Expect.equal (r.Start, r.Length) (None, None) r.Via

        testCase "tree: depth, and cycles are marked not followed"
        <| fun () ->
            let db = model ()
            let sales = db.Model.Tables.["Sales"]
            sales.Measures.Add(new Measure(Name = "A", Expression = "[B]"))
            sales.Measures.Add(new Measure(Name = "B", Expression = "[A] + [Total]"))
            let g = build db
            let one = tree DependsOn 1 g (refOf "measure:Sales/A")

            Expect.equal
                (one |> List.map (fun n -> n.Target, n.Children.IsEmpty))
                [ Resolved(refOf "measure:Sales/B"), true ]
                ""

            let all = tree DependsOn 0 g (refOf "measure:Sales/A")

            let b = all |> List.exactlyOne

            let cycle =
                b.Children |> List.find (fun n -> n.Target = Resolved(refOf "measure:Sales/A"))

            Expect.isTrue cycle.Cycle "A -> B -> A is circular"
            Expect.isEmpty cycle.Children ""

            Expect.isTrue
                (b.Children
                 |> List.exists (fun n -> n.Target = Resolved(refOf "measure:Sales/Total")))
                ""

        testCase "the same target twice is one node with both occurrences"
        <| fun () ->
            let db = model ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Twice", Expression = "[Total]\n+ [Total]"))
            let nodes = tree DependsOn 1 (build db) (refOf "measure:Sales/Twice")

            Expect.equal (nodes |> List.map (fun n -> n.Via)) [ [ "expression", Some 1; "expression", Some 2 ] ] ""
    ]
