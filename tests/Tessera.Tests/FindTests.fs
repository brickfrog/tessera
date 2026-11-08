module Tessera.Tests.FindTests

open Expecto
open Tessera
open Tessera.Find
open Tessera.Tests.Fixtures

let private run (opts: Options) =
    match search (sampleDatabase ()) opts with
    | Ok hits -> hits |> List.map (fun h -> ObjectRef.format h.Ref, h.Path, h.Line)
    | Error e -> failtestf "%s" (TessError.message e)

[<Tests>]
let tests =
    testList "Find" [
        testCase "literal, case-insensitive by default, across names and expressions"
        <| fun () ->
            let hits = run (Options.literal "sales[amount]")
            Expect.contains hits ("measure:Sales/Total", "expression", 1) ""

        testCase "case-sensitive"
        <| fun () ->
            Expect.isEmpty
                (run {
                    Options.literal "sales[amount]" with
                        CaseSensitive = true
                })
                ""

            Expect.isNonEmpty
                (run {
                    Options.literal "Sales[Amount]" with
                        CaseSensitive = true
                })
                ""

        testCase "literal text is not a regex"
        <| fun () ->
            Expect.equal (run (Options.literal "Total (hidden)")) [ "measure:Sales/Total (hidden)", "name", 1 ] ""

        testCase "regex"
        <| fun () ->
            let hits =
                run {
                    Options.literal "^TOTALYTD\\(" with
                        Regex = true
                }

            Expect.equal hits [ "calcitem:Time Intelligence/YTD", "expression", 1 ] ""

        testCase "line numbers inside multi-line values"
        <| fun () ->
            let hits = run (Options.literal "Indented second")
            Expect.equal hits [ "table:Sales", "description", 3 ] ""

        testCase "kind and table filters"
        <| fun () ->
            let all = run (Options.literal "DateKey")

            let columnsOnly =
                run {
                    Options.literal "DateKey" with
                        Objects = {
                            Inventory.Filter.none with
                                Kinds = [ ObjectKind.Column ]
                                Tables = [ "Date" ]
                        }
                }

            Expect.isGreaterThan all.Length columnsOnly.Length ""
            Expect.all columnsOnly (fun (r, _, _) -> r.StartsWith "column:Date/") ""

        testCase "--in restricts properties; lineage tags are skipped unless asked for"
        <| fun () ->
            let db = sampleDatabase ()
            db.Model.Tables.["Sales"].LineageTag <- "abc-unique-tag"

            let find opts =
                search db opts |> Result.defaultValue []

            Expect.isEmpty (find (Options.literal "abc-unique-tag")) "excluded by default"

            Expect.hasLength
                (find {
                    Options.literal "abc-unique-tag" with
                        Properties = [ "lineageTag" ]
                })
                1
                ""

            Expect.isEmpty
                (find {
                    Options.literal "SUM" with
                        Properties = [ "description" ]
                })
                ""

        testCase "annotations are searched"
        <| fun () ->
            let db = sampleDatabase ()

            db.Model.Tables.["Sales"].Annotations.Add(
                new Microsoft.AnalysisServices.Tabular.Annotation(Name = "Owner", Value = "finance team")
            )

            let hits = search db (Options.literal "finance") |> Result.defaultValue []
            Expect.equal (hits |> List.map (fun h -> h.Path)) [ "annotations.Owner" ] ""

        testCase "invalid regex and empty pattern are errors"
        <| fun () ->
            for opts in
                [
                    {
                        Options.literal "(" with
                            Regex = true
                    }
                    Options.literal ""
                ] do
                match search (sampleDatabase ()) opts with
                | Error(InvalidPattern _) -> ()
                | other -> failtestf "%A" other
    ]
