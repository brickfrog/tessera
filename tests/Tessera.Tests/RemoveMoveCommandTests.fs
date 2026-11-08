module Tessera.Tests.RemoveMoveCommandTests

open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// Every file under a folder, with its contents.
let private snapshot (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.sort
    |> Array.map (fun f -> f, File.ReadAllText f)

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

let private changes (doc: JsonDocument) = [
    for c in doc.RootElement.GetProperty("changes").EnumerateArray() ->
        c.GetProperty("objectPath").GetString(), c.GetProperty("changeKind").GetString()
]

let private errorCode (doc: JsonDocument) =
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

[<Tests>]
let tests =
    testList "CLI remove / move" [
        testCase "remove: a dry run by default, writing nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "remove"; t.Path; "[Total (hidden)]" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "- measure:Sales/Total (hidden)" ""
            Expect.stringContains r.Err "Removed measure:Sales/Total (hidden)" ""
            Expect.stringContains r.Err "Dry run - nothing saved. Add --save to persist." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "remove --save: a table's file goes, its relationship too, and the model reloads"
        <| fun () ->
            use t = project ()
            let r = tess [ "remove"; t.Path; "Sales"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "Also removed relationship:Sales-Date" ""
            Expect.stringContains r.Err "Saved pbip" ""
            let tables = t.Combine("Sales.SemanticModel", "definition", "tables")
            Expect.isFalse (File.Exists(Path.Combine(tables, "Sales.tmdl"))) "the stale file is removed"
            let db = load t.Path
            Expect.isFalse (db.Model.Tables.Contains "Sales") ""
            Expect.equal db.Model.Relationships.Count 0 ""
            Expect.isTrue (db.Model.Tables.Contains "Date") "the rest stays"

        testCase "remove --json: the change set lists deletions"
        <| fun () ->
            use t = project ()
            let r = tess [ "remove"; t.Path; "Date[Year]"; "[Total (hidden)]"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            let doc = json r
            Expect.equal (doc.RootElement.GetProperty("command").GetString()) "remove" ""

            Expect.equal
                (changes doc |> List.filter (fun (_, k) -> k = "deleted") |> List.sort)
                [
                    "column:Date/Year", "deleted"
                    "level:Date/Calendar/Year", "deleted"
                    "measure:Sales/Total (hidden)", "deleted"
                ]
                ""

            Expect.isTrue (changes doc |> List.contains ("level:Date/Calendar/Month", "modified")) "renumbered"
            Expect.isTrue (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (doc.RootElement.GetProperty("introducedErrors").GetArrayLength()) 0 ""

        testCase "remove: refused while referenced (exit 3), listing what refers to it"
        <| fun () ->
            use t = project ()
            let r = tess [ "remove"; t.Path; "Sales[Amount]" ]
            Expect.equal r.Code ExitCode.input ""
            Expect.stringContains r.Err "Can't remove column:Sales/Amount: other objects refer to it" ""
            Expect.stringContains r.Err "measure:Sales/Total  expression:1  Sales[Amount]  -> column:Sales/Amount" ""
            let j = tess [ "remove"; t.Path; "Sales[Amount]"; "--json" ]
            Expect.equal j.Code ExitCode.input ""
            Expect.equal (errorCode (json j)) "remove-blocked" ""

        testCase "remove --force: broken references show up as introduced errors, and --save goes ahead"
        <| fun () ->
            use t = project ()
            let r = tess [ "remove"; t.Path; "Sales[Amount]"; "--force"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            let doc = json r

            let introduced = [
                for f in doc.RootElement.GetProperty("introducedErrors").EnumerateArray() ->
                    f.GetProperty("objectPath").GetString(), f.GetProperty("code").GetString()
            ]

            Expect.equal introduced [ "measure:Sales/Total", "unresolved-reference" ] ""
            Expect.equal (doc.RootElement.GetProperty("warnings").GetArrayLength()) 1 ""

            let saved = tess [ "remove"; t.Path; "Sales[Amount]"; "--force"; "--save" ]
            Expect.equal saved.Code ExitCode.ok saved.Err
            Expect.stringContains saved.Err "tess: warning: left a broken reference (--force)" ""
            Expect.stringContains saved.Err "this change introduces 1 error" ""
            Expect.isFalse ((load t.Path).Model.Tables.["Sales"].Columns.Contains "Amount") ""

        testCase "remove: missing objects fail (exit 3) unless --if-exists"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "remove"; t.Path; "[Nope]" ]).Code ExitCode.input ""
            let r = tess [ "remove"; t.Path; "[Nope]"; "--if-exists" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "No changes." ""
            Expect.stringContains r.Err "No object matches '[Nope]'; skipped (--if-exists)" ""

        testCase "remove: the model and a last partition are refused (exit 3); no object is a usage error"
        <| fun () ->
            use t = project ()
            let m = tess [ "remove"; t.Path; "model" ]
            Expect.equal m.Code ExitCode.input ""
            Expect.stringContains m.Err "the model itself can't be removed" ""
            let p = tess [ "remove"; t.Path; "partition:Sales/Sales"; "--json" ]
            Expect.equal p.Code ExitCode.input ""
            Expect.equal (errorCode (json p)) "unsupported-remove" ""
            Expect.equal (tess [ "remove"; t.Path ]).Code ExitCode.usage ""

        testCase "move: a dry run shows a move, not a delete and a create"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "move"; t.Path; "[Total]"; "Date"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            let doc = json r
            Expect.equal (changes doc) [ "measure:Date/Total", "moved" ] ""
            let change = doc.RootElement.GetProperty("changes").[0]
            Expect.equal (change.GetProperty("movedFromObjectPath").GetString()) "measure:Sales/Total" ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "move --save --display-folder round-trips"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "move"
                    t.Path
                    "'Sales'[Total]"
                    "Date"
                    "--display-folder"
                    "KPIs"
                    "--save"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "> measure:Sales/Total -> measure:Date/Total" ""
            Expect.stringContains r.Err "Moved measure:Sales/Total -> measure:Date/Total" ""
            let db = load t.Path
            let moved = db.Model.Tables.["Date"].Measures.["Total"]
            Expect.equal moved.DisplayFolder "KPIs" ""
            Expect.equal moved.Expression "SUM(Sales[Amount])" ""
            Expect.isFalse (db.Model.Tables.["Sales"].Measures.Contains "Total") ""
            Expect.equal db.Model.Tables.["Sales"].Measures.["Total (hidden)"].Expression "[Total]" ""

        testCase "move: refusals exit 3; the same table is a no-op"
        <| fun () ->
            use t = project ()
            let cg = tess [ "move"; t.Path; "[Total]"; "Time Intelligence"; "--json" ]
            Expect.equal cg.Code ExitCode.input ""
            Expect.equal (errorCode (json cg)) "unsupported-move" ""
            Expect.equal (tess [ "move"; t.Path; "[Total]"; "Nope" ]).Code ExitCode.input ""
            Expect.equal (tess [ "move"; t.Path; "Sales[Amount]"; "Date" ]).Code ExitCode.input ""
            let same = tess [ "move"; t.Path; "[Total]"; "Sales" ]
            Expect.equal same.Code ExitCode.ok same.Err
            Expect.stringContains same.Out "No changes." ""
            Expect.stringContains same.Err "already in table 'Sales'" ""
    ]
