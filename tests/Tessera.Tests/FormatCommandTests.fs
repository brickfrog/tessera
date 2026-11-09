module Tessera.Tests.FormatCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// The sample model plus two unformatted measures, as a .bim file.
let private model (t: TempDir) =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]
    sales.Measures.Add(new Measure(Name = "Ugly", Expression = "sum(Sales[Amount])+1"))
    sales.Measures.Add(new Measure(Name = "Fine", Expression = "SUM ( Sales[Amount] ) + 2"))
    sales.Measures.Add(new Measure(Name = "Broken", Expression = "SUM(Sales[Amount]"))
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    path

let private measure (path: string) (name: string) =
    let _, db =
        ModelIO.loadPath path
        |> Result.defaultWith (fun e -> failtest (TessError.message e))

    db.Model.Tables.["Sales"].Measures.[name].Expression

[<Tests>]
let looseTests =
    testList "format command: loose DAX" [
        testCase "--expr prints the formatted DAX"
        <| fun () ->
            let r = tess [ "format"; "--expr"; "sum(Sales[Amount])+1" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "SUM ( Sales[Amount] ) + 1\n" ""

        testCase "options: --long, --width, --no-space-after-function"
        <| fun () ->
            let r =
                tess [
                    "format"
                    "--expr"
                    "DIVIDE(SUM(x), 2)"
                    "--long"
                    "--no-space-after-function"
                ]

            Expect.equal r.Out "DIVIDE(\n    SUM( x ),\n    2\n)\n" ""

            let r =
                tess [ "format"; "-e"; "CALCULATE([Sales], 'Date'[Year] = 2024)"; "--width"; "25" ]

            Expect.equal r.Out "CALCULATE (\n    [Sales],\n    'Date'[Year] = 2024\n)\n" ""

        testCase "a .dax file"
        <| fun () ->
            use t = new TempDir()
            let path = t.Combine "q.dax"
            File.WriteAllText(path, "evaluate row(\"a\", 1)")
            let r = tess [ "format"; path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "EVALUATE\nROW ( \"a\", 1 )\n" ""
            Expect.equal (File.ReadAllText path) "evaluate row(\"a\", 1)" "the file isn't changed"

        testCase "--semicolons keeps the dialect"
        <| fun () ->
            let r = tess [ "format"; "--expr"; "IF([a] > 1,5;2;3)"; "--semicolons" ]
            Expect.equal r.Out "IF ( [a] > 1,5; 2; 3 )\n" r.Err

        testCase "malformed DAX: exit 3, errors with line and column, nothing on stdout"
        <| fun () ->
            let r = tess [ "format"; "--expr"; "SUM(x" ]
            Expect.equal r.Code ExitCode.input ""
            Expect.equal r.Out "" ""
            Expect.equal r.Err "tess: --expr: line 1, column 4: '(' is never closed\n" ""

        testCase "--json: {success, formatted, errors}"
        <| fun () ->
            let r = tess [ "format"; "--expr"; "sum(x)"; "--json" ]
            Expect.equal r.Code ExitCode.ok ""
            let j = (json r).RootElement
            Expect.isTrue (j.GetProperty("success").GetBoolean()) ""
            Expect.equal (j.GetProperty("formatted").GetString()) "SUM ( x )" ""
            Expect.equal (j.GetProperty("errors").GetArrayLength()) 0 ""

            let r = tess [ "format"; "--expr"; "1 +\n  )"; "--json" ]
            Expect.equal r.Code ExitCode.input ""
            let j = (json r).RootElement
            Expect.isFalse (j.GetProperty("success").GetBoolean()) ""
            Expect.equal (j.GetProperty("formatted").ValueKind) JsonValueKind.Null ""
            let e = j.GetProperty("errors").[0]
            Expect.equal (e.GetProperty("line").GetInt32(), e.GetProperty("column").GetInt32()) (2, 3) ""
            Expect.equal (e.GetProperty("message").GetString()) "')' without a matching '('" ""

        testCase "--check: exit 1 if it would change, 0 if it's formatted"
        <| fun () ->
            let r = tess [ "format"; "--expr"; "sum(x)"; "--check" ]
            Expect.equal r.Code ExitCode.findings ""
            Expect.equal r.Out "" ""
            Expect.stringContains r.Err "not formatted" ""
            let r = tess [ "format"; "--expr"; "SUM ( x )"; "--check" ]
            Expect.equal r.Code ExitCode.ok r.Err

            let r = tess [ "format"; "--expr"; "sum(x)"; "--check"; "--json" ]
            Expect.isTrue ((json r).RootElement.GetProperty("changed").GetBoolean()) ""

        testCase "usage errors: model options with loose DAX, --expr with a file, nothing to format"
        <| fun () ->
            for args in
                [
                    [ "format"; "--expr"; "1"; "--save" ]
                    [ "format"; "--expr"; "1"; "-k"; "measure" ]
                    [ "format"; "--expr"; "1"; "x.bim" ]
                    [ "format" ]
                    [ "format"; "--expr"; "1"; "--width"; "0" ]
                ] do
                Expect.equal (tess args).Code ExitCode.usage (String.concat " " args)
    ]

[<Tests>]
let stdinTests =
    testSequenced
    <| testList "format command: stdin" [
        testCase "- reads stdin"
        <| fun () ->
            let r = tessWithStdin "var a = 1 return a" [ "format"; "-" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "VAR a = 1\nRETURN\n    a\n" ""
    ]

[<Tests>]
let modelTests =
    testList "format command: models" [
        testCase "a dry run by default: shows the change, saves nothing, warns about what it can't format"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "measure:Sales/Ugly" ""
            Expect.stringContains r.Out "SUM ( Sales[Amount] ) + 1" ""
            Expect.isFalse (r.Out.Contains "measure:Sales/Fine") "already formatted"
            Expect.stringContains r.Err "Dry run" ""
            Expect.stringContains r.Err "measure:Sales/Broken expression: not formatted: line 1, column 4" ""
            Expect.equal (measure path "Ugly") "sum(Sales[Amount])+1" "not saved"

        testCase "--save writes it; a second run has nothing to do"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (measure path "Ugly") "SUM ( Sales[Amount] ) + 1" ""
            Expect.equal (measure path "Broken") "SUM(Sales[Amount]" "left as it was"
            let r = tess [ "format"; path; "--check" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "" ""

        testCase "selectors, -k and -t"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path; "[Ugly]"; "--name-only" ]
            Expect.equal r.Out "measure:Sales/Ugly\n" r.Err
            let r = tess [ "format"; path; "-k"; "function"; "--name-only" ]
            Expect.equal r.Out "function:Double\n" r.Err
            let r = tess [ "format"; path; "-t"; "Time Intelligence"; "--name-only" ]
            let names = r.Out.TrimEnd().Split('\n') |> Array.sort |> List.ofArray
            Expect.equal names [ "calcitem:Time Intelligence/Current"; "calcitem:Time Intelligence/YTD" ] r.Err
            let r = tess [ "format"; path; "[Nope]" ]
            Expect.equal r.Code ExitCode.input ""

        testCase "--check: lists what would change, exit 1; changes nothing"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path; "--check" ]
            Expect.equal r.Code ExitCode.findings r.Err
            let lines = r.Out.TrimEnd().Split('\n') |> List.ofArray
            Expect.contains lines "measure:Sales/Ugly expression" ""
            Expect.isFalse (List.contains "measure:Sales/Fine expression" lines) ""
            Expect.stringContains r.Err "would be reformatted" ""
            Expect.equal (measure path "Ugly") "sum(Sales[Amount])+1" ""

        testCase "--check --json"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path; "--check"; "--json" ]
            Expect.equal r.Code ExitCode.findings ""
            let j = (json r).RootElement
            Expect.isTrue (j.GetProperty("check").GetBoolean()) ""

            let unformatted = [
                for x in j.GetProperty("unformatted").EnumerateArray() -> x.GetProperty("ref").GetString()
            ]

            Expect.contains unformatted "measure:Sales/Ugly" ""
            let failed = j.GetProperty("failed").[0]
            Expect.equal (failed.GetProperty("ref").GetString()) "measure:Sales/Broken" ""
            Expect.equal (failed.GetProperty("errors").[0].GetProperty("column").GetInt32()) 4 ""

        testCase "--json without --check: the editing pipeline's change set"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            let r = tess [ "format"; path; "[Ugly]"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let j = (json r).RootElement
            Expect.isTrue (j.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (j.GetProperty("changes").GetArrayLength()) 1 ""
            Expect.equal (j.GetProperty("introducedErrors").GetArrayLength()) 0 ""

        testCase "usage errors: --semicolons or --check with saving, on a model"
        <| fun () ->
            use t = new TempDir()
            let path = model t
            Expect.equal (tess [ "format"; path; "--semicolons" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "format"; path; "--check"; "--save" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "format"; path; "--save"; "--save-to"; t.Combine "x.bim" ]).Code ExitCode.usage ""

        testCase "a PBIP project folder is a model, not DAX"
        <| fun () ->
            use p = project ()
            let r = tess [ "format"; p.Path; "--check" ]
            Expect.equal r.Code ExitCode.findings r.Err
            Expect.stringContains r.Out "measure:Sales/Total expression" ""
    ]
