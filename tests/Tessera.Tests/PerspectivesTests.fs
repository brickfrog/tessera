module Tessera.Tests.PerspectivesTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Perspectives
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private failsWith (expected: string) =
    function
    | Ok _ -> failtestf "expected an error containing %s" expected
    | Error e -> Expect.stringContains (TessError.message e) expected ""

let private members (db: Database) =
    Perspectives.members db.Model.Perspectives.["Sales View"]
    |> List.map ObjectRef.format

let private applied db edit = Mutation.apply db edit |> ok

let private add db selectors =
    applied db (fun db -> edit db Change.Add "Sales View" selectors)

let private remove db selectors =
    applied db (fun db -> edit db Change.Remove "Sales View" selectors)

let private salesMembers = [
    "table:Sales"
    "column:Sales/Amount"
    "column:Sales/DateKey"
    "column:Sales/Path\\/With\\\\Slashes"
    "measure:Sales/Total"
    "measure:Sales/Total (hidden)"
]

[<Tests>]
let tests =
    testList "Perspectives" [
        testCase "adding a table adds it with all its columns, measures and hierarchies"
        <| fun () ->
            let db = sampleDatabase ()
            let a = add db [ "Sales"; "Date" ]

            Expect.equal
                (members db)
                (salesMembers
                 @ [
                     "table:Date"
                     "column:Date/DateKey"
                     "column:Date/Year"
                     "column:Date/Month"
                     "hierarchy:Date/Calendar"
                 ])
                ""

            Expect.equal a.Effect.Notes [ "Added table:Sales, table:Date to perspective:Sales View" ] ""
            Expect.isEmpty (db.Model.Validate().Errors) "TOM-valid"

        testCase "adding a member adds its table too, with just that member"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "'Date'[Year]"; "hierarchy:Date/Calendar"; "[Total]" ] |> ignore

            Expect.equal
                (members db)
                [
                    "table:Sales"
                    "measure:Sales/Total"
                    "table:Date"
                    "column:Date/Year"
                    "hierarchy:Date/Calendar"
                ]
                ""

        testCase "the diff shows the members line by line, on the perspective"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Date[Year]" ] |> ignore
            let a = add db [ "[Total]" ]

            Expect.equal
                (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref, c.ChangeKind))
                [ "perspective:Sales View", ChangeSet.ChangeKind.Modified ]
                ""

            Expect.equal
                (a.Changes.Head.Properties |> List.map (fun p -> p.Path, p.Before, p.After))
                [
                    "members",
                    Some "table:Date\ncolumn:Date/Year",
                    Some "table:Sales\nmeasure:Sales/Total\ntable:Date\ncolumn:Date/Year"
                ]
                ""

        testCase "what's already in is noted, not refused, and changes nothing"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Sales" ] |> ignore
            let a = add db [ "Sales"; "[Total]" ]
            Expect.isEmpty a.Changes ""

            Expect.equal
                a.Effect.Notes
                [
                    "table:Sales is already in perspective:Sales View with all its members"
                    "measure:Sales/Total is already in perspective:Sales View"
                ]
                ""

        testCase "adding a table that's partly in adds the rest"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "[Total]" ] |> ignore
            add db [ "Sales" ] |> ignore
            Expect.equal (members db) salesMembers ""

        testCase "removing a member keeps the table while it has others"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Sales" ] |> ignore
            let a = remove db [ "[Total]"; "Sales[Amount]" ]

            Expect.equal
                (members db)
                (salesMembers
                 |> List.filter (fun r -> r <> "measure:Sales/Total" && r <> "column:Sales/Amount"))
                ""

            Expect.equal
                a.Effect.Notes
                [
                    "Removed measure:Sales/Total, column:Sales/Amount from perspective:Sales View"
                ]
                ""

        testCase "removing a table's last member removes the table too"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Date[Year]"; "[Total]" ] |> ignore
            let a = remove db [ "Date[Year]" ]
            Expect.equal (members db) [ "table:Sales"; "measure:Sales/Total" ] ""
            Expect.isNull (db.Model.Perspectives.["Sales View"].PerspectiveTables.Find "Date") ""

            Expect.contains
                a.Effect.Notes
                "Removed table:Date from perspective:Sales View too: none of its members are left"
                ""

        testCase "removing a table removes all of it; what isn't in is noted"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Sales"; "Date[Year]" ] |> ignore
            let a = remove db [ "Sales"; "Date[Month]" ]
            Expect.equal (members db) [ "table:Date"; "column:Date/Year" ] ""
            Expect.contains a.Effect.Notes "column:Date/Month is not in perspective:Sales View" ""

        testCase "the perspective is found by name in any case, or by reference"
        <| fun () ->
            let db = sampleDatabase ()
            applied db (fun db -> edit db Change.Add "sales view" [ "[Total]" ]) |> ignore

            applied db (fun db -> edit db Change.Add "perspective:Sales View" [ "Date[Year]" ])
            |> ignore

            Expect.equal (members db) [ "table:Sales"; "measure:Sales/Total"; "table:Date"; "column:Date/Year" ] ""

        testCase "refused: a missing perspective, objects perspectives don't hold, a missing object"
        <| fun () ->
            let db = sampleDatabase ()

            edit db Change.Add "Finance" [ "Sales" ]
            |> failsWith
                "there is no such perspective (the model has 'Sales View'; add it with `tess add <model> perspective:Finance`)"

            edit db Change.Add "Sales View" [ "level:Date/Calendar/Year" ]
            |> failsWith
                "level:Date/Calendar/Year is a level; perspectives hold tables, columns, measures and hierarchies"

            edit db Change.Add "Sales View" [ "calcitem:Time Intelligence/YTD" ]
            |> failsWith "is a calcitem"

            edit db Change.Add "Sales View" [ "Sales"; "[Nope]" ]
            |> failsWith "No object matches '[Nope]'"

            Expect.isEmpty (members db) "nothing applied"

        testCase "a renamed member stays in its perspective, and the perspective shows the new name"
        <| fun () ->
            let db = sampleDatabase ()
            add db [ "Date[Year]" ] |> ignore
            db.Model.Tables.["Date"].Columns.["Year"].Name <- "Calendar Year"
            Expect.equal (members db) [ "table:Date"; "column:Date/Calendar Year" ] ""
    ]
