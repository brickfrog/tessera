module Tessera.Tests.InventoryTests

open Expecto
open Tessera
open Tessera.Inventory
open Tessera.Tests.Fixtures

let private refs (entries: ObjectEntry list) =
    entries |> List.map (fun e -> ObjectRef.format e.Ref)

let private sampleSource: ModelSource = {
    Format = Tmdl
    Path = "/x"
    SemanticModelFolder = None
}

[<Tests>]
let tests =
    testList "Inventory" [
        testCase "lists every object kind in tree order"
        <| fun () ->
            let all = refs (objects (sampleDatabase ()))

            Expect.equal
                all
                [
                    "model"
                    "table:Sales"
                    "column:Sales/Amount"
                    "column:Sales/DateKey"
                    @"column:Sales/Path\/With\\Slashes"
                    "measure:Sales/Total"
                    "measure:Sales/Total (hidden)"
                    "partition:Sales/Sales"
                    "table:Date"
                    "column:Date/DateKey"
                    "column:Date/Year"
                    "column:Date/Month"
                    "hierarchy:Date/Calendar"
                    "level:Date/Calendar/Year"
                    "level:Date/Calendar/Month"
                    "partition:Date/Date"
                    "table:Time Intelligence"
                    "column:Time Intelligence/Period"
                    "calcitem:Time Intelligence/Current"
                    "calcitem:Time Intelligence/YTD"
                    "partition:Time Intelligence/Time Intelligence"
                    "relationship:Sales-Date"
                    "role:Readers"
                    "perspective:Sales View"
                    "culture:da-DK"
                    "expression:ServerName"
                    "function:Double"
                ]
                ""

        testCase "every listed ref parses back to itself"
        <| fun () ->
            for e in objects (sampleDatabase ()) do
                Expect.equal (ObjectRef.parse (ObjectRef.format e.Ref)) (Ok e.Ref) (ObjectRef.format e.Ref)

        testCase "hidden flags and relationship detail"
        <| fun () ->
            let byRef =
                objects (sampleDatabase ())
                |> List.map (fun e -> ObjectRef.format e.Ref, e)
                |> Map.ofList

            Expect.equal byRef.["column:Sales/DateKey"].IsHidden (Some true) ""
            Expect.equal byRef.["column:Sales/Amount"].IsHidden (Some false) ""
            Expect.equal byRef.["partition:Sales/Sales"].IsHidden None "partitions have no visibility"
            Expect.equal byRef.["relationship:Sales-Date"].Detail (Some "Sales[DateKey] -> Date[DateKey]") ""

        testCase "inactive relationships are marked"
        <| fun () ->
            let db = sampleDatabase ()
            db.Model.Relationships.["Sales-Date"].IsActive <- false
            let rel = objects db |> List.find (fun e -> e.Ref.Kind = ObjectKind.Relationship)
            Expect.equal rel.Detail (Some "Sales[DateKey] -> Date[DateKey] (inactive)") ""

        testList "filters" [
            let all = objects (sampleDatabase ())
            let run f = Filter.apply f all |> refs

            testCase "by kind"
            <| fun () ->
                Expect.equal
                    (run {
                        Filter.none with
                            Kinds = [ ObjectKind.Measure; ObjectKind.CalculationItem ]
                    })
                    [
                        "measure:Sales/Total"
                        "measure:Sales/Total (hidden)"
                        "calcitem:Time Intelligence/Current"
                        "calcitem:Time Intelligence/YTD"
                    ]
                    ""

            testCase "by table (case-insensitive), including the table itself"
            <| fun () ->
                let r = run { Filter.none with Tables = [ "date" ] }
                Expect.contains r "table:Date" ""
                Expect.contains r "level:Date/Calendar/Month" ""
                Expect.all r (fun s -> s.Contains ":Date") "only Date objects"

            testCase "by name wildcard"
            <| fun () ->
                Expect.equal
                    (run {
                        Filter.none with
                            NamePattern = Some "total*"
                    })
                    [ "measure:Sales/Total"; "measure:Sales/Total (hidden)" ]
                    ""

                Expect.equal
                    (run {
                        Filter.none with
                            NamePattern = Some "Y??r"
                            Kinds = [ ObjectKind.Column ]
                    })
                    [ "column:Date/Year" ]
                    ""

            testCase "wildcards don't treat regex characters specially"
            <| fun () ->
                Expect.equal
                    (run {
                        Filter.none with
                            NamePattern = Some "Total (hidden)"
                    })
                    [ "measure:Sales/Total (hidden)" ]
                    ""
        ]

        testCase "info counts"
        <| fun () ->
            let i = info sampleSource (sampleDatabase ())

            let count k =
                i.Counts |> List.find (fst >> (=) k) |> snd

            Expect.equal i.Name "Sample" ""
            Expect.equal i.CompatibilityLevel 1702 ""
            Expect.equal i.CompatibilityMode "PowerBI" "1702 is a Power BI-only level"
            Expect.equal i.CalculationGroups 1 ""
            Expect.equal (count ObjectKind.Table) 3 ""
            Expect.equal (count ObjectKind.Column) 7 "RowNumber columns are not counted"
            Expect.equal (count ObjectKind.Measure) 2 ""
            Expect.equal (count ObjectKind.Function) 1 ""
            Expect.isFalse (i.Counts |> List.exists (fst >> (=) ObjectKind.Model)) ""
    ]
