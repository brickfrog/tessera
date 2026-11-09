module Tessera.Tests.LintCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// The sample model plus a fixable warning (DI004), an unfixable one (DI008), and an info (DR014),
/// as a .bim file.
let private lintModel (t: TempDir) (extra: (string * string) list) =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]
    sales.Measures.Add(new Measure(Name = "Unqualified", Expression = "SUM([Amount])"))

    sales.Measures.Add(
        new Measure(Name = "Calc", Expression = "VAR x = [Total]\nRETURN CALCULATE(x, Sales[Amount] > 0)")
    )

    sales.Measures.Add(new Measure(Name = "Months", Expression = "'Date'[Month] = \"Jan\" || 'Date'[Month] = \"Feb\""))

    for name, expr in extra do
        sales.Measures.Add(new Measure(Name = name, Expression = expr))

    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    path

let private lines (s: string) =
    s.Split('\n') |> Array.map (fun l -> l.TrimEnd '\r') |> Array.filter ((<>) "")

let private expression (path: string) (measure: string) =
    let db = JsonSerializer.DeserializeDatabase(File.ReadAllText path)
    db.Model.Tables.["Sales"].Measures.[measure].Expression

[<Tests>]
let tests =
    testList "CLI lint" [
        testCase "a clean model: exit 0, 'No issues.'"
        <| fun () ->
            use t = project ()
            let r = tess [ "lint"; t.Path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (lines r.Out) [| "No issues." |] ""

        testCase "text: one aligned line per finding, a summary, and what --fix would fix; warnings exit 0"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "lint"; lintModel t [] ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (lines r.Out)
                [|
                    "warning  measure:Sales/Unqualified  expression:1  [Amount] is a column: write it with its table, as 'Sales'[Amount]  [DI004]"
                    "warning  measure:Sales/Calc         expression:2  CALCULATE's first argument is the variable x, which was evaluated where it was defined, so its filter arguments don't change the result  [DI008]"
                    "info     measure:Sales/Months       expression:1  2 comparisons of 'Date'[Month] joined by ||: write 'Date'[Month] IN {\"Jan\", \"Feb\"}  [DR014]"
                    "2 warnings, 1 info"
                    "2 can be fixed with --fix."
                |]
                ""

        testCase "--fail-on, --min-severity and --rule (case-insensitive)"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []
            Expect.equal (tess [ "lint"; path; "--fail-on"; "warning" ]).Code ExitCode.findings ""

            let hidden =
                tess [ "lint"; path; "--min-severity"; "error"; "--fail-on"; "warning" ]

            Expect.equal hidden.Code ExitCode.findings "hidden findings still count"
            Expect.equal (lines hidden.Out) [| "No issues." |] ""
            let one = tess [ "lint"; path; "--rule"; "dr014"; "--fail-on"; "warning" ]
            Expect.equal one.Code ExitCode.ok "only DR014, an info, is checked"
            Expect.equal (lines one.Out |> Array.skip 1) [| "1 info"; "1 can be fixed with --fix." |] ""

        testCase "an error (non-standard whitespace) exits 1"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "lint"; lintModel t [ "Spaced", "1\u00A0+ 2" ]; "--rule"; "TS001" ]
            Expect.equal r.Code ExitCode.findings r.Out
            Expect.stringContains r.Out "U+00A0 (no-break space)" ""

        testCase "--json: the shared findings envelope, source lint, with rule names and fixable"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "lint"; lintModel t []; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "lint" ""
            Expect.equal (root.GetProperty("fixable").GetInt32()) 2 ""
            let summary = root.GetProperty "summary"
            Expect.equal (summary.GetProperty("warnings").GetInt32(), summary.GetProperty("info").GetInt32()) (2, 1) ""
            let findings = root.GetProperty("findings").EnumerateArray() |> List.ofSeq
            let first = findings.[0]
            Expect.equal (first.GetProperty("source").GetString()) "lint" ""
            Expect.equal (first.GetProperty("code").GetString()) "DI004" ""
            Expect.equal (first.GetProperty("ruleName").GetString()) "Add table name to column references" ""
            Expect.equal (first.GetProperty("category").GetString()) "Improvement" ""
            Expect.equal (first.GetProperty("objectPath").GetString()) "measure:Sales/Unqualified" ""
            Expect.isTrue (first.GetProperty("fixable").GetBoolean()) ""
            let position = first.GetProperty "expressionPosition"
            Expect.equal (position.GetProperty("property").GetString()) "expression" ""

            Expect.equal
                (position.GetProperty("lineNumber").GetInt32(), position.GetProperty("column").GetInt32())
                (1, 5)
                ""

            Expect.isFalse (findings.[1].GetProperty("fixable").GetBoolean()) "DI008 is never fixed"

        testCase "--ci github annotates stderr"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "lint"; lintModel t []; "--ci"; "github" ]
            Expect.stringContains r.Err "::warning title=DI004::measure:Sales/Unqualified: [Amount] is a column" ""
            Expect.stringContains r.Err "::notice title=DR014::" ""

        testCase "usage errors exit 2"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []

            for args in
                [
                    [ "--rule"; "DI999" ]
                    [ "--min-severity"; "loud" ]
                    [ "--ci"; "jenkins" ]
                    [ "--save" ]
                    [ "--stat" ]
                    [ "--fix"; "--fail-on"; "warning" ]
                    [ "--fix"; "--ci"; "github" ]
                    [ "--fix"; "--save"; "--save-to"; t.Combine "out.bim" ]
                ] do
                let r = tess ([ "lint"; path ] @ args)
                Expect.equal r.Code ExitCode.usage (String.concat " " args)

            let asJson = tess [ "lint"; path; "--rule"; "DI999"; "--json" ]
            Expect.stringContains asJson.Out "unknown rule 'DI999'" ""

        testCase "--fix: a dry run by default, showing the diff"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []
            let r = tess [ "lint"; path; "--fix" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "\"SUM([Amount])\" -> \"SUM('Sales'[Amount])\"" ""
            Expect.stringContains r.Out "'Date'[Month] IN {\\\"Jan\\\", \\\"Feb\\\"}" ""
            Expect.stringContains r.Err "Fixed 2 findings in 2 expressions (DI004: 1, DR014: 1)." ""
            Expect.stringContains r.Err "1 finding can't be fixed automatically" ""
            Expect.stringContains r.Err "Dry run - nothing saved." ""
            Expect.equal (expression path "Unqualified") "SUM([Amount])" "nothing written"

        testCase "--fix --save writes the fixes; fixing again changes nothing"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []
            let r = tess [ "lint"; path; "--fix"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (expression path "Unqualified") "SUM('Sales'[Amount])" ""
            Expect.equal (expression path "Months") "'Date'[Month] IN {\"Jan\", \"Feb\"}" ""
            let again = tess [ "lint"; path; "--fix"; "--save" ]
            Expect.equal (lines again.Out) [| "No changes." |] ""
            let after = tess [ "lint"; path ]
            Expect.equal (lines after.Out |> Array.last) "1 warning" "only DI008 is left"

        testCase "--fix --rule fixes only those rules; --min-severity skips less severe ones"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []
            let r = tess [ "lint"; path; "--fix"; "--rule"; "DR014"; "--name-only" ]
            Expect.equal (lines r.Out) [| "measure:Sales/Months" |] ""
            let w = tess [ "lint"; path; "--fix"; "--min-severity"; "warning"; "--stat" ]
            Expect.equal (lines w.Out) [| "~ measure:Sales/Unqualified  1 property"; "1 modified" |] "DR014 is info"

        testCase "--fix --json: the change set, a dry run"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "lint"; lintModel t []; "--fix"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "lint" ""
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (root.GetProperty("summary").GetProperty("modified").GetInt32()) 2 ""
            Expect.equal (root.GetProperty("introducedErrors").GetArrayLength()) 0 ""
            Expect.equal (root.GetProperty("warnings").GetArrayLength()) 1 ""
            Expect.equal (root.GetProperty("saved").ValueKind) JsonValueKind.Null ""

        testCase "--fix --save-to writes a copy in PBIP"
        <| fun () ->
            use t = new TempDir()
            let path = lintModel t []
            let out = t.Combine "Fixed.SemanticModel"
            let r = tess [ "lint"; path; "--fix"; "--save-to"; out ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (expression path "Unqualified") "SUM([Amount])" "the source is untouched"
            let relint = tess [ "lint"; out; "--rule"; "DI004,DR014" ]
            Expect.equal (lines relint.Out) [| "No issues." |] ""
    ]
