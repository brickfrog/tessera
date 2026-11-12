module Tessera.Tests.ScriptCommandTests

open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

let private snapshot (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.sort
    |> Array.map (fun f -> f, File.ReadAllText f)

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

let private errorOf (r: Run) =
    use doc = json r
    let e = doc.RootElement.GetProperty("error")
    e.GetProperty("code").GetString(), e.Clone()

[<Tests>]
let tests =
    testList "CLI script" [
        testList "export" [
            testCase "writes the script to stdout"
            <| fun () ->
                use t = project ()
                let r = tess [ "script"; "export"; t.Path ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringStarts r.Out "-------------------\n-- Measure: [Total]\n" ""

                Expect.stringContains
                    r.Out
                    "MEASURE 'Sales'[Total] = SUM(Sales[Amount])\n    FormatString = \"#,0\"\n"
                    ""

                Expect.stringContains r.Out "CALCULATIONGROUP 'Time Intelligence'[Period]" ""
                Expect.equal r.Err "" "nothing else"

            testCase "selectors, -k and -t narrow it down"
            <| fun () ->
                use t = project ()
                let r = tess [ "script"; "export"; t.Path; "[Total (hidden)]"; "Double" ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringContains r.Out "MEASURE 'Sales'[Total (hidden)] = [Total]" ""
                Expect.stringContains r.Out "FUNCTION Double" ""
                Expect.isFalse (r.Out.Contains "MEASURE 'Sales'[Total] =") ""

                let r =
                    tess [ "script"; "export"; t.Path; "-k"; "calcitem"; "-t"; "Time Intelligence" ]

                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringStarts r.Out "-----------------------------------------\n-- Calculation Group:" ""
                Expect.isFalse (r.Out.Contains "MEASURE '") ""

            testCase "-o writes a file; --json describes the export"
            <| fun () ->
                use t = project ()
                let file = t.Combine "out.dax"
                let r = tess [ "script"; "export"; t.Path; "-o"; file ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "" ""
                Expect.stringContains r.Err "Wrote 6 objects to" ""
                Expect.stringContains (File.ReadAllText file) "FUNCTION Double = (x: INT64) => x * 2\n" ""

                File.WriteAllText(file, "edited")
                let again = tess [ "script"; "export"; t.Path; "-o"; file ]
                Expect.equal again.Code ExitCode.output "won't overwrite without --force"
                Expect.equal (File.ReadAllText file) "edited" "the file is kept"
                Expect.equal (tess [ "script"; "export"; t.Path; "-o"; file; "--force" ]).Code ExitCode.ok ""
                Expect.stringContains (File.ReadAllText file) "FUNCTION Double" ""

                let r = tess [ "script"; "export"; t.Path; "[Total]"; "--json" ]
                Expect.equal r.Code ExitCode.ok r.Err
                use doc = json r
                let root = doc.RootElement
                Expect.equal (root.GetProperty("command").GetString()) "script export" ""

                Expect.equal
                    [ for o in root.GetProperty("objects").EnumerateArray() -> o.GetString() ]
                    [ "measure:Sales/Total" ]
                    ""

                Expect.stringContains (root.GetProperty("script").GetString()) "MEASURE 'Sales'[Total]" ""
                Expect.isFalse (root.TryGetProperty "output" |> fst) ""
                Expect.equal (root.GetProperty("warnings").GetArrayLength()) 0 ""

            testCase "objects without DAX, and kinds a script doesn't hold, are refused"
            <| fun () ->
                use t = project ()
                let r = tess [ "script"; "export"; t.Path; "Sales[Amount]"; "--json" ]
                Expect.equal r.Code ExitCode.input r.Out
                Expect.equal (fst (errorOf r)) "cannot-script" ""

                let r = tess [ "script"; "export"; t.Path; "-k"; "hierarchy" ]
                Expect.equal r.Code ExitCode.usage r.Err
                Expect.stringContains r.Err "a DAX script holds only these kinds" ""

            testCase "nothing matched says so"
            <| fun () ->
                use t = project ()
                let r = tess [ "script"; "export"; t.Path; "-t"; "Nope" ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "" ""
                Expect.stringContains r.Err "Nothing to script" ""
        ]

        testList "apply" [
            testCase "a dry run by default: shows the change and writes nothing"
            <| fun () ->
                use t = project ()
                let before = snapshot t.Path
                let script = t.Combine "change.dax"

                write
                    script
                    "MEASURE 'Sales'[Total] = SUMX(Sales, Sales[Amount])\nMEASURE 'Sales'[Margin] = [Total] * 0.2\n"

                let r = tess [ "script"; "apply"; t.Path; script ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringContains r.Out "~ measure:Sales/Total" ""
                Expect.stringContains r.Out "expression: \"SUM(Sales[Amount])\" -> \"SUMX(Sales, Sales[Amount])\"" ""
                Expect.stringContains r.Out "+ measure:Sales/Margin" ""
                Expect.stringContains r.Err "Applied 2 objects from" ""
                Expect.stringContains r.Err "Dry run - nothing saved. Add --save to persist." ""
                File.Delete script
                Expect.equal (snapshot t.Path) before "nothing written"

            testCase "--save writes it; export then apply is then a no-op"
            <| fun () ->
                use t = project ()
                let script = t.Combine "change.dax"
                write script "MEASURE 'Sales'[Margin] = [Total] * 0.2\n    DisplayFolder = \"KPIs\"\n"
                let r = tess [ "script"; "apply"; t.Path; script; "--save" ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringContains r.Err "Saved pbip" ""
                let margin = (load t.Path).Model.Tables.["Sales"].Measures.["Margin"]
                Expect.equal margin.Expression "[Total] * 0.2" ""
                Expect.equal margin.DisplayFolder "KPIs" ""

                let all = t.Combine "all.dax"
                Expect.equal (tess [ "script"; "export"; t.Path; "-o"; all ]).Code ExitCode.ok ""
                let r = tess [ "script"; "apply"; t.Path; all ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "No changes.\n" ""

            testCase "--json: the change set"
            <| fun () ->
                use t = project ()
                let script = t.Combine "change.dax"
                write script "MEASURE 'Sales'[Total]\n    Description = \"The sum\"\n"
                let r = tess [ "script"; "apply"; t.Path; script; "--json" ]
                Expect.equal r.Code ExitCode.ok r.Out
                use doc = json r
                let root = doc.RootElement
                Expect.equal (root.GetProperty("command").GetString()) "script apply" ""
                Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
                let change = root.GetProperty("changes").[0]
                Expect.equal (change.GetProperty("objectPath").GetString()) "measure:Sales/Total" ""
                Expect.equal (change.GetProperty("changeKind").GetString()) "modified" ""
                Expect.equal (root.GetProperty("introducedErrors").GetArrayLength()) 0 ""
                Expect.isFalse (root.TryGetProperty "saved" |> fst) ""

            testCase "a syntax error: exit 3, with the line and column"
            <| fun () ->
                use t = project ()
                let script = t.Combine "bad.dax"
                write script "MEASURE 'Sales'[A] = 1\n    , Colour = \"red\"\n"
                let r = tess [ "script"; "apply"; t.Path; script ]
                Expect.equal r.Code ExitCode.input r.Err
                Expect.stringContains r.Err $"{script}:2:7: 'Colour' is not a property of a measure" ""

                let r = tess [ "script"; "apply"; t.Path; script; "--json" ]
                Expect.equal r.Code ExitCode.input r.Out
                let code, e = errorOf r
                Expect.equal code "invalid-script" ""
                Expect.equal (e.GetProperty("line").GetInt32(), e.GetProperty("column").GetInt32()) (2, 7) ""
                Expect.equal (e.GetProperty("source").GetString()) script ""

            testCase "an unknown table: exit 3, at the object"
            <| fun () ->
                use t = project ()
                let script = t.Combine "bad.dax"
                write script "MEASURE 'Sales'[A] = 1\nMEASURE 'Sails'[B] = 2\n"
                let r = tess [ "script"; "apply"; t.Path; script; "--json" ]
                Expect.equal r.Code ExitCode.input r.Out
                let code, e = errorOf r
                Expect.equal code "object-not-found" ""
                Expect.equal (e.GetProperty("line").GetInt32()) 2 ""
                Expect.stringContains (e.GetProperty("message").GetString()) "did you mean table:Sales" ""

            testCase "a change that introduces errors isn't saved without --force"
            <| fun () ->
                use t = project ()
                let script = t.Combine "bad.dax"
                write script "MEASURE 'Sales'[A] = 'Nope'[X]\n"
                let before = snapshot t.Path
                let r = tess [ "script"; "apply"; t.Path; script; "--save" ]
                Expect.equal r.Code ExitCode.findings r.Err
                Expect.stringContains r.Err "unresolved-reference" ""
                Expect.equal (snapshot t.Path) before "refused"
                let r = tess [ "script"; "apply"; t.Path; script; "--save"; "--force" ]
                Expect.equal r.Code ExitCode.ok r.Err

            testCase "a missing script file"
            <| fun () ->
                use t = project ()
                let r = tess [ "script"; "apply"; t.Path; t.Combine "nope.dax" ]
                Expect.equal r.Code ExitCode.input r.Err
                Expect.stringContains r.Err "Path not found" ""

            testCase "usage errors"
            <| fun () ->
                use t = project ()
                Expect.equal (tess [ "script" ]).Code ExitCode.usage ""
                Expect.equal (tess [ "script"; "apply"; t.Path ]).Code ExitCode.usage ""
                let r = tess [ "script"; "apply"; t.Path; "x.dax"; "--save"; "--save-to"; "y" ]
                Expect.equal r.Code ExitCode.usage r.Err
        ]

        testSequenced
        <| testList "stdin" [
            testCase "apply - reads the script from stdin"
            <| fun () ->
                use t = project ()

                let r =
                    tessWithStdin "DEFINE MEASURE Sales[Q] = [Total] / 4\nEVALUATE { [Q] }" [
                        "script"
                        "apply"
                        t.Path
                        "-"
                        "--save"
                    ]

                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringContains r.Err "Applied 1 object from <stdin> (1 new)" ""
                Expect.equal (load t.Path).Model.Tables.["Sales"].Measures.["Q"].Expression "[Total] / 4" ""

            testCase "errors in stdin say <stdin>"
            <| fun () ->
                use t = project ()
                let r = tessWithStdin "CALCULATIONITEM \"X\" = 1" [ "script"; "apply"; t.Path; "-" ]
                Expect.equal r.Code ExitCode.input r.Err
                Expect.stringContains r.Err "<stdin>:1:1: CALCULATIONITEM must follow its CALCULATIONGROUP" ""
        ]
    ]
