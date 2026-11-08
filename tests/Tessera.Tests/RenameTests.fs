module Tessera.Tests.RenameTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Inventory
open Tessera.Rename
open Tessera.Tests.Fixtures

/// The sample model plus references to rename from every kind of DAX holder.
let private model () =
    let db = sampleDatabase ()
    let m = db.Model
    let sales = m.Tables.["Sales"]
    let date = m.Tables.["Date"]

    date.Measures.Add(
        new Measure(
            Name = "Uses",
            Expression =
                "CALCULATE ( [Total], 'Sales'[Amount] > 0 ) // [Total] and 'Sales' in a comment\n"
                + "    + LEN ( \"[Total] Sales[Amount]\" ) /* Sales[Amount] */ + Sales [Total]"
        )
    )

    sales.Measures.Add(
        new Measure(
            Name = "Avg",
            Expression = "DIVIDE([Total], COUNTROWS(Sales))",
            FormatStringDefinition = new FormatStringDefinition(Expression = "IF([Total] > 1, \"0\", \"0.0\")")
        )
    )

    sales.Measures.Add(
        new Measure(
            Name = "Shadowed",
            Expression = "VAR Sales = 1\nRETURN Sales + COUNTROWS ( 'Sales' ) + Sales[Total] + 'Sales'[Total]"
        )
    )

    sales.Measures.Add(new Measure(Name = "Broken", Expression = "[Missing] + 1"))
    sales.Columns.Add(new CalculatedColumn(Name = "Doubled", Expression = "[Amount] * 2"))

    let summary = new Table(Name = "Summary")

    summary.Partitions.Add(
        new Partition(
            Name = "Summary",
            Source = new CalculatedPartitionSource(Expression = "SUMMARIZE(Sales, Sales[DateKey], \"T\", [Total])")
        )
    )

    m.Tables.Add summary

    m.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.Add(
        new CalculationItem(
            Name = "Sales Only",
            Expression = "IF(ISSELECTEDMEASURE([Total]), SELECTEDMEASURE())",
            Ordinal = 2
        )
    )

    m.Roles.["Readers"].TablePermissions.Add(new TablePermission(Table = sales, FilterExpression = "Sales[Amount] > 0"))

    m.Functions.Add(
        new Function(Name = "SalesTotal", Expression = "() => SUMX(Sales, Sales[Amount]) + Double([Total])")
    )

    db

let private entry (db: Database) (selector: string) =
    match Selector.resolve (objects db) selector with
    | Ok e -> e
    | Error e -> failtestf "%A" e

let private planOk db selector newName =
    match plan db (entry db selector) newName with
    | Ok p -> p
    | Error e -> failtestf "%s" (TessError.message e)

let private planError db selector newName =
    match plan db (entry db selector) newName with
    | Ok p -> failtestf "expected an error, got %A" (p.Edits |> List.map (fun e -> e.After))
    | Error e -> e

/// (owner ref, property) -> after text
let private edits (p: RenamePlan) =
    p.Edits
    |> List.map (fun e -> (ObjectRef.format e.Owner.Ref, e.Property), e.After)
    |> Map.ofList

let private after (p: RenamePlan) owner property =
    match Map.tryFind (owner, property) (edits p) with
    | Some text -> text
    | None -> failtestf "no edit of %s %s; edits: %A" owner property (edits p |> Map.keys |> List.ofSeq)

let private refOf s =
    match ObjectRef.parse s with
    | Ok r -> r
    | Error e -> failtestf "%A" e

let private referencers (db: Database) (r: string) =
    Deps.referencedBy (Deps.build db) (refOf r)
    |> List.map (fun x -> ObjectRef.format x.From, x.Via)
    |> List.sort

let private unresolvedCount (db: Database) =
    Deps.unresolved (Deps.build db) |> List.length

[<Tests>]
let tests =
    testList "Rename" [
        testCase "measure: [Old], 'T'[Old] and T [Old], everywhere; comments and strings untouched"
        <| fun () ->
            let p = planOk (model ()) "[Total]" "Total Sales"
            Expect.equal p.NewRef (refOf "measure:Sales/Total Sales") ""

            Expect.equal
                (after p "measure:Date/Uses" "expression")
                ("CALCULATE ( [Total Sales], 'Sales'[Amount] > 0 ) // [Total] and 'Sales' in a comment\n"
                 + "    + LEN ( \"[Total] Sales[Amount]\" ) /* Sales[Amount] */ + Sales [Total Sales]")
                "only the reference tokens change"

            Expect.equal (after p "measure:Sales/Total (hidden)" "expression") "[Total Sales]" ""
            Expect.equal (after p "measure:Sales/Avg" "expression") "DIVIDE([Total Sales], COUNTROWS(Sales))" ""

            Expect.equal
                (after p "measure:Sales/Avg" "formatStringDefinition.expression")
                "IF([Total Sales] > 1, \"0\", \"0.0\")"
                "format string expressions"

            Expect.equal
                (after p "calcitem:Time Intelligence/Sales Only" "expression")
                "IF(ISSELECTEDMEASURE([Total Sales]), SELECTEDMEASURE())"
                "calculation items"

            Expect.equal
                (after p "table:Summary" "expression")
                "SUMMARIZE(Sales, Sales[DateKey], \"T\", [Total Sales])"
                "calculated tables"

            Expect.equal
                (after p "function:SalesTotal" "expression")
                "() => SUMX(Sales, Sales[Amount]) + Double([Total Sales])"
                "UDFs"

            Expect.equal
                (after p "measure:Sales/Shadowed" "expression")
                "VAR Sales = 1\nRETURN Sales + COUNTROWS ( 'Sales' ) + Sales[Total] + 'Sales'[Total Sales]"
                "Sales[Total] is a column of the VAR"

            Expect.equal p.Edits.Length 8 "nothing else is edited"
            Expect.isEmpty p.Warnings ""

        testCase "column: qualified and unqualified, including calculated columns and RLS"
        <| fun () ->
            let p = planOk (model ()) "Sales[Amount]" "Net Amount"
            Expect.equal (after p "measure:Sales/Total" "expression") "SUM(Sales[Net Amount])" ""
            Expect.equal (after p "column:Sales/Doubled" "expression") "[Net Amount] * 2" ""

            Expect.equal
                (after p "role:Readers" "tablePermissions.Sales.filterExpression")
                "Sales[Net Amount] > 0"
                "RLS filters"

            Expect.equal
                (after p "measure:Date/Uses" "expression")
                ("CALCULATE ( [Total], 'Sales'[Net Amount] > 0 ) // [Total] and 'Sales' in a comment\n"
                 + "    + LEN ( \"[Total] Sales[Amount]\" ) /* Sales[Amount] */ + Sales [Total]")
                ""

            Expect.equal
                (after p "function:SalesTotal" "expression")
                "() => SUMX(Sales, Sales[Net Amount]) + Double([Total])"
                ""

        testCase "a calculated column can be renamed too"
        <| fun () ->
            let db = model ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "D", Expression = "SUM(Sales[Doubled])"))
            let p = planOk db "Sales[Doubled]" "Twice"
            Expect.equal (after p "measure:Sales/D" "expression") "SUM(Sales[Twice])" ""

        testCase "table: unquoted stays unquoted for plain names; VAR shadowing respected"
        <| fun () ->
            let p = planOk (model ()) "Sales" "Fact"
            Expect.equal (after p "measure:Sales/Total" "expression") "SUM(Fact[Amount])" ""
            Expect.equal (after p "measure:Sales/Avg" "expression") "DIVIDE([Total], COUNTROWS(Fact))" ""

            Expect.equal
                (after p "measure:Sales/Shadowed" "expression")
                "VAR Sales = 1\nRETURN Sales + COUNTROWS ( 'Fact' ) + Sales[Total] + 'Fact'[Total]"
                "the VAR named Sales is not the table"

            Expect.equal
                (after p "measure:Date/Uses" "expression")
                ("CALCULATE ( [Total], 'Fact'[Amount] > 0 ) // [Total] and 'Sales' in a comment\n"
                 + "    + LEN ( \"[Total] Sales[Amount]\" ) /* Sales[Amount] */ + Fact [Total]")
                ""

            Expect.equal (after p "role:Readers" "tablePermissions.Sales.filterExpression") "Fact[Amount] > 0" ""

            Expect.equal (after p "table:Summary" "expression") "SUMMARIZE(Fact, Fact[DateKey], \"T\", [Total])" ""

            Expect.equal
                (after p "function:SalesTotal" "expression")
                "() => SUMX(Fact, Fact[Amount]) + Double([Total])"
                ""

        testCase "table: names that aren't plain identifiers are quoted"
        <| fun () ->
            let p = planOk (model ()) "Sales" "Sales Data"
            Expect.equal (after p "measure:Sales/Total" "expression") "SUM('Sales Data'[Amount])" ""

            Expect.equal (after p "measure:Sales/Avg" "expression") "DIVIDE([Total], COUNTROWS('Sales Data'))" ""

            let q = planOk (model ()) "Sales" "O'Brien"
            Expect.equal (after q "measure:Sales/Total" "expression") "SUM('O''Brien'[Amount])" ""
            let k = planOk (model ()) "Sales" "Desc"
            Expect.equal (after k "measure:Sales/Total" "expression") "SUM('Desc'[Amount])" "keywords are quoted"

        testCase "table: a new name the expression already uses (a VAR) is quoted, not captured"
        <| fun () ->
            let db = model ()

            db.Model.Tables.["Date"].Measures.Add(
                new Measure(Name = "V", Expression = "VAR x = 1 RETURN COUNTROWS(Sales) + x")
            )

            let p = planOk db "Sales" "x"
            Expect.equal (after p "measure:Date/V" "expression") "VAR x = 1 RETURN COUNTROWS('x') + x" ""
            Expect.equal (after p "measure:Sales/Total" "expression") "SUM(x[Amount])" "elsewhere it's unquoted"

        testCase "table: a calculation group table"
        <| fun () ->
            let db = model ()

            db.Model.Tables.["Sales"].Measures.Add(
                new Measure(Name = "Ytd", Expression = "CALCULATE([Total], 'Time Intelligence'[Period] = \"YTD\")")
            )

            let p = planOk db "'Time Intelligence'" "TI"
            Expect.equal (after p "measure:Sales/Ytd" "expression") "CALCULATE([Total], 'TI'[Period] = \"YTD\")" ""

        testCase "calendar: 'Old' references that resolve to it"
        <| fun () ->
            let db = model ()
            db.Model.Tables.["Date"].Calendars.Add(new Calendar(Name = "Fiscal"))

            db.Model.Tables.["Sales"].Measures.Add(
                new Measure(Name = "Fy", Expression = "CALCULATE([Total], DATESYTD('Fiscal'))")
            )

            let p = planOk db "calendar:Date/Fiscal" "FY Calendar"
            Expect.equal (after p "measure:Sales/Fy" "expression") "CALCULATE([Total], DATESYTD('FY Calendar'))" ""

            match planError db "calendar:Date/Fiscal" "sales" with
            | NameCollision(_, existing) -> Expect.equal existing "table:Sales" "tables and calendars share names"
            | e -> failtestf "%A" e

        testCase "table: a calculated table"
        <| fun () ->
            let db = model ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "N", Expression = "COUNTROWS(Summary)"))
            let p = planOk db "Summary" "Sales Summary"
            Expect.equal (after p "measure:Sales/N" "expression") "COUNTROWS('Sales Summary')" ""

        testCase "measure and column names are bracketed with ]] escaped"
        <| fun () ->
            let p = planOk (model ()) "[Total]" "Total [EUR]"
            Expect.equal (after p "measure:Sales/Total (hidden)" "expression") "[Total [EUR]]]" ""
            let q = planOk (model ()) "Sales[Amount]" "It's"
            Expect.equal (after q "measure:Sales/Total" "expression") "SUM(Sales[It's])" ""

        testCase "function: calls are rewritten; names must be identifiers"
        <| fun () ->
            let p = planOk (model ()) "function:Double" "Math.Double"

            Expect.equal
                (after p "function:SalesTotal" "expression")
                "() => SUMX(Sales, Sales[Amount]) + Math.Double([Total])"
                ""

            for bad in [ "2x"; "a b"; "a-b"; ".a"; "a."; "a..b"; "'a'" ] do
                match planError (model ()) "function:Double" bad with
                | InvalidName _ -> ()
                | e -> failtestf "%s: %A" bad e

        testCase "other kinds: renamed only, and structural links follow"
        <| fun () ->
            let db = model ()
            let p = planOk db "hierarchy:Date/Calendar" "Cal"
            Expect.isEmpty p.Edits ""
            apply p
            Expect.isTrue (objects db |> List.exists (fun e -> e.Ref = refOf "level:Date/Cal/Year")) ""

            for selector, name in
                [
                    "level:Date/Cal/Year", "Yr"
                    "partition:Sales/Sales", "Sales Partition"
                    "calcitem:Time Intelligence/YTD", "Year to date"
                    "role:Readers", "Viewers"
                    "perspective:Sales View", "Everything"
                    "relationship:Sales-Date", "r1"
                ] do
                let p = planOk db selector name
                Expect.isEmpty p.Edits selector
                apply p

            let refs = objects db |> List.map (fun e -> ObjectRef.format e.Ref) |> Set.ofList

            for r in
                [
                    "level:Date/Cal/Yr"
                    "partition:Sales/Sales Partition"
                    "calcitem:Time Intelligence/Year to date"
                    "role:Viewers"
                    "perspective:Everything"
                    "relationship:r1"
                ] do
                Expect.isTrue (refs.Contains r) r

            Expect.equal
                (db.Model.Roles.["Viewers"].TablePermissions.[0].FilterExpression)
                "Sales[Amount] > 0"
                "the role keeps its filters"

        testCase "structural links survive a column and table rename through save and reload"
        <| fun () ->
            let db = model ()
            let date = db.Model.Tables.["Date"]
            date.Columns.["Month"].SortByColumn <- date.Columns.["DateKey"]
            let view = db.Model.Perspectives.["Sales View"]
            let pt = new PerspectiveTable(Table = date)
            pt.PerspectiveColumns.Add(new PerspectiveColumn(Column = date.Columns.["DateKey"]))
            view.PerspectiveTables.Add pt
            let da = db.Model.Cultures.["da-DK"]
            da.ObjectTranslations.SetTranslation(date.Columns.["DateKey"], TranslatedProperty.Caption, "Datonøgle")

            planOk db "Date[DateKey]" "Key" |> apply
            planOk db "Date" "Calendar Date" |> apply

            use tmp = new TempDir()
            TmdlSerializer.SerializeDatabaseToFolder(db, tmp.Path)
            let back = TmdlSerializer.DeserializeDatabaseFromFolder tmp.Path
            let date = back.Model.Tables.["Calendar Date"]
            let key = date.Columns.["Key"]
            let rel = back.Model.Relationships.["Sales-Date"] :?> SingleColumnRelationship
            Expect.equal rel.ToColumn key "relationship"
            Expect.equal date.Columns.["Month"].SortByColumn key "sort-by"
            Expect.equal date.Hierarchies.["Calendar"].Levels.["Year"].Column date.Columns.["Year"] "levels"

            let pcol =
                back.Model.Perspectives.["Sales View"].PerspectiveTables.["Calendar Date"]

            Expect.isNotNull (pcol.PerspectiveColumns.Find "Key") "perspective"

            Expect.equal
                (back.Model.Cultures.["da-DK"].ObjectTranslations.[key, TranslatedProperty.Caption].Value)
                "Datonøgle"
                "translation"

            Expect.equal
                (back.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["YTD"].Expression)
                "TOTALYTD(SELECTEDMEASURE(), 'Calendar Date'[Key])"
                "and the DAX"

        testCase "named expressions: renamed, with a warning that M isn't updated"
        <| fun () ->
            let db = model ()

            db.Model.Tables.["Sales"].Partitions.["Sales"].Source <-
                new MPartitionSource(Expression = "let s = Sql.Database(ServerName, \"db\") in s")

            let p = planOk db "expression:ServerName" "Server"
            Expect.isEmpty p.Edits ""
            Expect.equal p.Warnings.Length 1 ""
            Expect.stringContains p.Warnings.[0] "M references" ""
            Expect.stringContains p.Warnings.[0] "partition:Sales/Sales" "names what mentions it"

        testCase "PBI_QueryOrder lists queries by name and is kept in step"
        <| fun () ->
            let db = model ()

            db.Model.Annotations.Add(
                new Annotation(Name = "PBI_QueryOrder", Value = "[\"Sales\",\"Date\",\"ServerName\"]")
            )

            let p = planOk db "Sales" "Fact"
            Expect.equal (after p "model" "annotations.PBI_QueryOrder") "[\"Fact\",\"Date\",\"ServerName\"]" ""
            Expect.equal (RenamePlan.expressionEdits p).Length (p.Edits.Length - 1) ""
            let q = planOk db "expression:ServerName" "Serveur é"
            Expect.equal (after q "model" "annotations.PBI_QueryOrder") "[\"Sales\",\"Date\",\"Serveur é\"]" ""

        testCase "apply: referencers point at the new ref, with no new unresolved references"
        <| fun () ->
            for selector, newName, newRef in
                [
                    "[Total]", "Total Sales", "measure:Sales/Total Sales"
                    "Sales[Amount]", "Net Amount", "column:Sales/Net Amount"
                    "Sales", "Sales Data", "table:Sales Data"
                    "Sales", "Fact", "table:Fact"
                    "function:Double", "Math.Double", "function:Math.Double"
                    "[Total]", "TOTAL", "measure:Sales/TOTAL"
                ] do
                let db = model ()
                let target = entry db selector
                let before = referencers db (ObjectRef.format target.Ref)
                let unresolved = unresolvedCount db
                let p = planOk db selector newName
                apply p

                let mapped (s: string) =
                    match selector, newName with
                    | "Sales", n -> s.Replace(":Sales/", $":{n}/").Replace(".Sales.", $".{n}.")
                    | _ -> s

                let expected = before |> List.map (fun (f, v) -> mapped f, mapped v) |> List.sort
                Expect.isNonEmpty before selector
                Expect.equal (referencers db newRef) expected $"{selector} -> {newName}"
                Expect.equal (unresolvedCount db) unresolved $"{selector}: no new unresolved references"

        testCase "a case-only rename is allowed and rewrites references"
        <| fun () ->
            let p = planOk (model ()) "[Total]" "TOTAL"
            Expect.equal (after p "measure:Sales/Total (hidden)" "expression") "[TOTAL]" ""

        testCase "the same name is a no-op"
        <| fun () ->
            let db = model ()
            let p = planOk db "[Total]" "Total"
            Expect.isTrue (RenamePlan.isNoOp p) ""
            Expect.isEmpty p.Edits ""
            Expect.equal p.NewRef p.Target.Ref ""
            apply p
            Expect.equal db.Model.Tables.["Sales"].Measures.["Total"].Name "Total" ""

        testCase "collisions, case-insensitive"
        <| fun () ->
            for selector, name, clash in
                [
                    "[Total]", "amount", "column:Sales/Amount"
                    "[Total]", "uses", "measure:Date/Uses"
                    "[Total]", "AVG", "measure:Sales/Avg"
                    "Sales[Amount]", "total", "measure:Sales/Total"
                    "Sales[Amount]", "datekey", "column:Sales/DateKey"
                    "Date[Year]", "calendar", "hierarchy:Date/Calendar"
                    "Sales", "date", "table:Date"
                    "function:Double", "salestotal", "function:SalesTotal"
                ] do
                match planError (model ()) selector name with
                | NameCollision(_, existing) -> Expect.equal existing clash $"{selector} -> {name}"
                | e -> failtestf "%s -> %s: %A" selector name e

            // A column may share a name with a measure in another table.
            planOk (model ()) "Date[Year]" "Avg" |> ignore

        testCase "invalid names and the model"
        <| fun () ->
            for bad in [ ""; "   "; " Total"; "Total "; "a\nb"; "a\tb" ] do
                match planError (model ()) "[Total]" bad with
                | InvalidName _ -> ()
                | e -> failtestf "%A: %A" bad e

            match planError (model ()) "model" "M" with
            | UnsupportedRename _ -> ()
            | e -> failtestf "%A" e

        testCase "a new name that would capture other references is refused"
        <| fun () ->
            // [Uses] in a Sales expression means the Date measure; a Sales column named Uses would win.
            let db = model ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "U", Expression = "[Uses] + 1"))

            match planError db "Sales[DateKey]" "Uses" with
            | RenameConflict(_, _, details) ->
                Expect.equal details.Length 1 ""
                Expect.stringContains details.[0] "measure:Sales/U expression:1" ""
            | e -> failtestf "%A" e

        testCase "a reference that named nothing and now resolves is reported"
        <| fun () ->
            let p = planOk (model ()) "[Total]" "Missing"
            Expect.exists p.Warnings (fun w -> w.Contains "measure:Sales/Broken") ""

        testCase "apply refuses a stale plan"
        <| fun () ->
            let db = model ()
            let p = planOk db "[Total]" "T2"
            db.Model.Tables.["Sales"].Measures.["Avg"].Expression <- "1"
            Expect.throws (fun () -> apply p) ""

        testCase "the plan doesn't touch the database"
        <| fun () ->
            let db = model ()
            let before = ModelIO.toBimText ModelIO.SaveOptions.defaults db
            planOk db "Sales" "Fact" |> ignore
            Expect.equal (ModelIO.toBimText ModelIO.SaveOptions.defaults db) before ""
    ]
