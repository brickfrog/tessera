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

        testCase "--json: the shared findings envelope"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; brokenModel t; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "validate" ""
            Expect.isTrue (root.GetProperty("durationMs").GetInt32() >= 0) ""
            Expect.isFalse (root.GetProperty("valid").GetBoolean()) "an error makes the model invalid"
            let summary = root.GetProperty "summary"
            Expect.equal (summary.GetProperty("errors").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("warnings").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("info").GetInt32()) 0 ""
            Expect.equal (summary.GetProperty("total").GetInt32()) 2 ""
            let findings = root.GetProperty("findings").EnumerateArray() |> List.ofSeq
            Expect.equal findings.Length 2 ""
            let first = findings.[0]
            Expect.equal (first.GetProperty("code").GetString()) "unresolved-reference" ""
            Expect.equal (first.GetProperty("source").GetString()) "validate" ""
            Expect.equal (first.GetProperty("severity").GetString()) "error" ""
            Expect.equal (first.GetProperty("objectPath").GetString()) "measure:Sales/Broken" ""
            Expect.equal (first.GetProperty("object").GetString()) "Broken" ""
            Expect.equal (first.GetProperty("objectType").GetString()) "Measure" ""
            let position = first.GetProperty "expressionPosition"
            Expect.equal (position.GetProperty("property").GetString()) "expression" ""
            Expect.equal (position.GetProperty("lineNumber").GetInt32()) 2 ""
            Expect.isTrue (position.GetProperty("column").GetInt32() >= 1) ""
            Expect.stringContains (first.GetProperty("message").GetString()) "'Nope'[X]" ""
            Expect.isFalse (first.GetProperty("fixable").GetBoolean()) ""
            // Optional keys are absent, not null.
            let mutable none = Unchecked.defaultof<JsonElement>
            Expect.isFalse (findings.[1].TryGetProperty("expressionPosition", &none)) "no position: key left out"
            Expect.isFalse (first.TryGetProperty("ruleName", &none)) ""

        testCase "--ci writes annotations to stderr"
        <| fun () ->
            use t = new TempDir()
            let path = brokenModel t
            let gh = tess [ "validate"; path; "--ci"; "github" ]
            Expect.stringContains gh.Err "::error title=unresolved-reference::measure:Sales/Broken: " ""
            Expect.stringContains gh.Err "::warning title=empty-expression::" ""
            let azdo = tess [ "validate"; path; "--ci"; "azdo" ]
            Expect.stringContains azdo.Err "##vso[task.logissue type=error;code=unresolved-reference]" ""
            Expect.equal (tess [ "validate"; path; "--ci"; "jenkins" ]).Code ExitCode.usage ""

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
            Expect.equal (doc.RootElement.GetProperty("findings").GetArrayLength()) 2 ""

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
