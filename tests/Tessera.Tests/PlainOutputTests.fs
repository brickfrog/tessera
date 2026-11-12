/// tess's output stays plain (no colour, no spinner, no redrawn lines): always today, and by
/// rule under CI or when redirected, for anything that decorates output later.
module Tessera.Tests.PlainOutputTests

open System.IO
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.XmlaFakes
open Tessera.Cli

let private env (vars: (string * string) list) (name: string) =
    vars |> List.tryFind (fst >> (=) name) |> Option.map snd

/// ANSI escapes, or a carriage return left after CRLF is normalised (a redrawn line).
let private decorated (s: string) = s.Contains '\u001b' || s.Contains '\r'

[<Tests>]
let tests =
    testList "plain output" [
        testCase "plainOutput: CI, NO_COLOR, TERM=dumb or not a terminal"
        <| fun () ->
            Expect.isFalse (Output.plainOutput (env []) true) "a terminal, nothing set"
            Expect.isTrue (Output.plainOutput (env []) false) "redirected"
            Expect.isTrue (Output.plainOutput (env [ "CI", "true" ]) true) "CI"
            Expect.isTrue (Output.plainOutput (env [ "CI", "1" ]) true) "CI, any value"
            Expect.isFalse (Output.plainOutput (env [ "CI", "" ]) true) "CI empty is unset"
            Expect.isTrue (Output.plainOutput (env [ "NO_COLOR", "1" ]) true) "NO_COLOR"
            Expect.isTrue (Output.plainOutput (env [ "TERM", "dumb" ]) true) "TERM=dumb"
            Expect.isFalse (Output.plainOutput (env [ "TERM", "xterm-256color" ]) true) ""

        testCase "no command writes colour or redrawn lines, with CI set or not"
        <| fun () ->
            use t = new TempDir()
            let model = t.Combine "model.bim"
            let db = sampleDatabase ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Bad", Expression = "SUM([Amount]) + 'Nope'[X]"))
            File.WriteAllText(model, JsonSerializer.SerializeDatabase db)
            let data = t.Combine "data"

            let commands = [
                [ "--help" ]
                [ "info"; model ]
                [ "list"; model ]
                [ "validate"; model; "--ci"; "github" ]
                [ "bpa"; model; "--ci"; "azdo" ]
                [ "lint"; model ]
                [ "format"; model; "--check" ]
                [ "diff"; model; model ]
                [ "data"; "init"; model; data; "--rows"; "2" ]
                [ "data"; "check"; model; data ]
                [ "query"; model; "--data"; data; "EVALUATE ROW(\"x\", 1)" ]
                [ "vaildate"; model ]
            ]

            for vars in [ []; [ "CI", "true" ] ] do
                for args in commands do
                    let r = tessWithEnv vars args
                    let what = $"""tess {String.concat " " args} (env {vars})"""
                    Expect.isFalse (decorated r.Out) $"{what}: stdout"
                    Expect.isFalse (decorated r.Err) $"{what}: stderr"

                let server = FakeServer()
                let sales = sampleDatabase ()
                sales.Name <- "Sales"
                server.Add sales

                let refresh =
                    tessOn server (tokenEnv @ vars) [
                        "refresh"
                        $"xmla:{workspace};Sales"
                        "--execute"
                        "--progress"
                        "all"
                    ]

                Expect.isFalse (decorated refresh.Err) "refresh progress is one plain line per event"
    ]
