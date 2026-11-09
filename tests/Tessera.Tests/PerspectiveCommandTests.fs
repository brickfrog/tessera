module Tessera.Tests.PerspectiveCommandTests

open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

let private snapshot (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.sort
    |> Array.map (fun f -> f, File.ReadAllText f)

let private members (path: string) =
    Perspectives.members (load path).Model.Perspectives.["Sales View"]
    |> List.map ObjectRef.format

[<Tests>]
let tests =
    testList "CLI perspective" [
        testCase "add: a dry run by default, showing the members as a line diff"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "perspective"; "add"; t.Path; "Sales View"; "Date[Year]"; "[Total]" ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                r.Out
                (String.concat "\n" [
                    "~ perspective:Sales View"
                    "    members:"
                    "      + table:Sales"
                    "      + measure:Sales/Total"
                    "      + table:Date"
                    "      + column:Date/Year"
                    ""
                    "1 modified"
                    ""
                ])
                ""

            Expect.stringContains r.Err "Added column:Date/Year, measure:Sales/Total to perspective:Sales View" ""
            Expect.stringContains r.Err "Dry run - nothing saved." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "add --save, then remove --save; list shows the members"
        <| fun () ->
            use t = project ()
            let a = tess [ "perspective"; "add"; t.Path; "sales view"; "Sales"; "--save" ]
            Expect.equal a.Code ExitCode.ok a.Err
            Expect.contains (members t.Path) "column:Sales/Amount" ""

            let r =
                tess [ "perspective"; "remove"; t.Path; "Sales View"; "Sales[Amount]"; "--save" ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.isFalse (members t.Path |> List.contains "column:Sales/Amount") ""

            let l = tess [ "perspective"; "list"; t.Path; "Sales View" ]
            Expect.equal l.Code ExitCode.ok l.Err
            Expect.equal (l.Out.TrimEnd().Split('\n') |> List.ofArray) (members t.Path) ""

        testCase "add --json: the change set, with the members before and after"
        <| fun () ->
            use t = project ()
            let r = tess [ "perspective"; "add"; t.Path; "Sales View"; "[Total]"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            let doc = (json r).RootElement
            Expect.equal (doc.GetProperty("command").GetString()) "perspective add" ""
            let c = doc.GetProperty("changes").[0]
            Expect.equal (c.GetProperty("objectPath").GetString()) "perspective:Sales View" ""
            let p = c.GetProperty("properties").[0]
            Expect.equal (p.GetProperty("property").GetString()) "members" ""
            Expect.equal (p.GetProperty("after").GetString()) "table:Sales\nmeasure:Sales/Total" ""

        testCase "list: every perspective with counts; --json for one perspective"
        <| fun () ->
            use t = project ()
            tess [ "perspective"; "add"; t.Path; "Sales View"; "Date"; "--save" ] |> ignore
            let r = tess [ "perspective"; "list"; t.Path ]
            Expect.equal r.Out "perspective:Sales View  1 table, 3 columns, 0 measures, 1 hierarchy\n" ""

            let all = (json (tess [ "perspective"; "list"; t.Path; "--json" ])).RootElement.[0]
            Expect.equal (all.GetProperty("columns").GetInt32()) 3 ""

            let one =
                (json (tess [ "perspective"; "list"; t.Path; "perspective:Sales View"; "--json" ])).RootElement

            Expect.equal
                [
                    for m in one.GetProperty("members").EnumerateArray() -> m.GetProperty("kind").GetString()
                ]
                [ "table"; "column"; "column"; "column"; "hierarchy" ]
                ""

        testCase "refused: a level (exit 3), a missing perspective (exit 3), too few arguments (exit 2)"
        <| fun () ->
            use t = project ()

            let level =
                tess [
                    "perspective"
                    "add"
                    t.Path
                    "Sales View"
                    "level:Date/Calendar/Year"
                    "--json"
                ]

            Expect.equal level.Code ExitCode.input level.Out

            Expect.equal
                ((json level).RootElement.GetProperty("error").GetProperty("code").GetString())
                "unsupported-perspective-change"
                ""

            let missing = tess [ "perspective"; "remove"; t.Path; "Finance"; "Sales" ]
            Expect.equal missing.Code ExitCode.input missing.Err
            Expect.stringContains missing.Err "tess add <model> perspective:Finance" ""

            Expect.equal (tess [ "perspective"; "add"; t.Path; "Sales View" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "perspective"; "list"; t.Path; "Nope" ]).Code ExitCode.input ""

        testCase "set refuses members, pointing at `perspective add`"
        <| fun () ->
            use t = project ()
            let r = tess [ "set"; t.Path; "perspective:Sales View"; "-p"; "members=x" ]
            Expect.equal r.Code ExitCode.usage r.Err
            Expect.stringContains r.Err "tess perspective add" ""
    ]
