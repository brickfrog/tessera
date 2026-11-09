module Tessera.Tests.UnusedCommandTests

open Expecto
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

let private refs (r: Run) = [
    for e in (json r).RootElement.EnumerateArray() -> e.GetProperty("ref").GetString()
]

[<Tests>]
let tests =
    testList "CLI deps --unused" [
        testCase "hidden objects nothing uses, one reference per line; exit 0"
        <| fun () ->
            use t = project ()
            let r = tess [ "deps"; t.Path; "--unused" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "measure:Sales/Total (hidden)\n" ""

        testCase "--include-visible marks visible objects and names their dead users"
        <| fun () ->
            use t = project ()

            let r =
                tess [ "deps"; t.Path; "--unused"; "--include-visible"; "-k"; "measure,function" ]

            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                r.Out
                ("measure:Sales/Total  (visible)  used only by unused measure:Sales/Total (hidden)\n"
                 + "measure:Sales/Total (hidden)\n"
                 + "function:Double  (visible)\n")
                ""

        testCase "--json: one object per unused object"
        <| fun () ->
            use t = project ()

            let r =
                tess [ "deps"; t.Path; "--unused"; "--include-visible"; "-t"; "Sales"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Out

            Expect.equal
                (refs r)
                [
                    "column:Sales/Amount"
                    "column:Sales/Path\\/With\\\\Slashes"
                    "measure:Sales/Total"
                    "measure:Sales/Total (hidden)"
                ]
                "-t keeps one table"

            let total =
                (json r).RootElement.EnumerateArray()
                |> Seq.find (fun e -> e.GetProperty("ref").GetString() = "measure:Sales/Total")

            Expect.equal (total.GetProperty("kind").GetString()) "measure" ""
            Expect.equal (total.GetProperty("table").GetString()) "Sales" ""
            Expect.isFalse (total.GetProperty("hidden").GetBoolean()) ""

            Expect.equal
                [ for x in total.GetProperty("usedBy").EnumerateArray() -> x.GetString() ]
                [ "measure:Sales/Total (hidden)" ]
                ""

        testCase "--json with nothing unused is an empty array"
        <| fun () ->
            use t = project ()
            let r = tess [ "deps"; t.Path; "--unused"; "-k"; "column"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.equal (refs r) [] ""

        testCase "--fail-on-unused: exit 1 when anything is listed, 0 otherwise"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "deps"; t.Path; "--unused"; "--fail-on-unused" ]).Code ExitCode.findings ""
            Expect.equal (tess [ "deps"; t.Path; "--unused"; "-k"; "table"; "--fail-on-unused" ]).Code ExitCode.ok ""

        testCase "usage errors"
        <| fun () ->
            use t = project ()

            for args in
                [
                    [ "deps"; t.Path ]
                    [ "deps"; t.Path; "[Total]"; "--unused" ]
                    [ "deps"; t.Path; "--unused"; "-k"; "level" ]
                    [ "deps"; t.Path; "--unused"; "-k"; "nonsense" ]
                    [ "deps"; t.Path; "--unused"; "--depth"; "2" ]
                    [ "deps"; t.Path; "--unused"; "--referenced-by" ]
                    [ "deps"; t.Path; "[Total]"; "--include-visible" ]
                    [ "deps"; t.Path; "[Total]"; "--fail-on-unused" ]
                ] do
                Expect.equal (tess args).Code ExitCode.usage (String.concat " " args)

            let r = tess [ "deps"; t.Path; "--unused"; "-k"; "level"; "--json" ]
            Expect.equal r.Code ExitCode.usage ""

            Expect.equal
                ((json r).RootElement.GetProperty("error").GetProperty("code").GetString())
                "usage"
                "usage errors are JSON with --json"

        testCase "a model that can't be loaded is an input error"
        <| fun () -> Expect.equal (tess [ "deps"; "/nonexistent/model.bim"; "--unused" ]).Code ExitCode.input ""
    ]
