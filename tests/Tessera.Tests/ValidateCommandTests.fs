module Tessera.Tests.ValidateCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// The sample model, plus one error and one warning, as a .bim file.
let private brokenModel (t: TempDir) =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]
    sales.Measures.Add(new Measure(Name = "Broken", Expression = "1 +\n'Nope'[X]"))
    sales.Measures.Add(new Measure(Name = "Blank", Expression = ""))
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    path

let private lines (s: string) =
    s.Split('\n') |> Array.map (fun l -> l.TrimEnd '\r') |> Array.filter ((<>) "")

[<Tests>]
let tests =
    testList "CLI validate" [
        testCase "a clean model: exit 0, 'No issues.'"
        <| fun () ->
            use t = project ()
            let r = tess [ "validate"; t.Path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (lines r.Out) [| "No issues." |] ""

        testCase "text: one aligned line per issue, then a summary; errors exit 1"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; brokenModel t ]
            Expect.equal r.Code ExitCode.findings r.Err

            Expect.equal
                (lines r.Out)
                [|
                    "error    measure:Sales/Broken  expression:2  'Nope'[X] does not exist: there is no table 'Nope'  [unresolved-reference]"
                    "warning  measure:Sales/Blank   expression    measure has no DAX expression  [empty-expression]"
                    "1 error, 1 warning"
                |]
                ""

        testCase "--json: summary and issues"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; brokenModel t; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Err
            use doc = json r
            let root = doc.RootElement
            let summary = root.GetProperty "summary"
            Expect.equal (summary.GetProperty("errors").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("warnings").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("infos").GetInt32()) 0 ""
            let issues = root.GetProperty("issues").EnumerateArray() |> List.ofSeq
            Expect.equal issues.Length 2 ""
            let first = issues.[0]
            Expect.equal (first.GetProperty("rule").GetString()) "unresolved-reference" ""
            Expect.equal (first.GetProperty("severity").GetString()) "error" ""
            Expect.equal (first.GetProperty("ref").GetString()) "measure:Sales/Broken" ""
            Expect.equal (first.GetProperty("property").GetString()) "expression" ""
            Expect.equal (first.GetProperty("line").GetInt32()) 2 ""
            Expect.stringContains (first.GetProperty("message").GetString()) "'Nope'[X]" ""
            Expect.equal (issues.[1].GetProperty("line").ValueKind) JsonValueKind.Null ""

        testCase "--rule picks rules; warnings alone exit 0 unless --fail-on warning"
        <| fun () ->
            use t = new TempDir()
            let path = brokenModel t
            let r = tess [ "validate"; path; "--rule"; "empty-expression" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (Array.last (lines r.Out)) "1 warning" ""

            let failing =
                tess [ "validate"; path; "--rule"; "empty-expression"; "--fail-on"; "warning" ]

            Expect.equal failing.Code ExitCode.findings ""

            let both =
                tess [
                    "validate"
                    path
                    "--rule"
                    "empty-expression,unresolved-reference"
                    "--json"
                ]

            use doc = json both
            Expect.equal (doc.RootElement.GetProperty("issues").GetArrayLength()) 2 ""

        testCase "--min-severity hides issues, but --fail-on still sees them"
        <| fun () ->
            use t = new TempDir()
            let path = brokenModel t

            let r =
                tess [
                    "validate"
                    path
                    "--rule"
                    "empty-expression"
                    "--min-severity"
                    "error"
                    "--fail-on"
                    "warning"
                ]

            Expect.equal (lines r.Out) [| "No issues." |] ""
            Expect.equal r.Code ExitCode.findings ""

            let shown = tess [ "validate"; path; "--min-severity"; "ERROR" ]
            Expect.equal (Array.last (lines shown.Out)) "1 error" ""

        testCase "--fail-on info fails on any issue"
        <| fun () ->
            use t = new TempDir()

            let r =
                tess [ "validate"; brokenModel t; "--rule"; "empty-expression"; "--fail-on"; "info" ]

            Expect.equal r.Code ExitCode.findings ""

        testCase "unknown rules and severities are usage errors"
        <| fun () ->
            use t = project ()
            let rule = tess [ "validate"; t.Path; "--rule"; "nope" ]
            Expect.equal rule.Code ExitCode.usage ""
            Expect.stringContains rule.Err "unknown rule 'nope'" ""
            Expect.stringContains rule.Err "circular-reference" "lists the known rules"

            let severity = tess [ "validate"; t.Path; "--fail-on"; "fatal"; "--json" ]
            Expect.equal severity.Code ExitCode.usage ""
            use doc = json severity
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "usage" ""

        testCase "a missing model is an input error"
        <| fun () ->
            use t = new TempDir()
            Expect.equal (tess [ "validate"; t.Combine "missing.bim" ]).Code ExitCode.input ""
    ]
