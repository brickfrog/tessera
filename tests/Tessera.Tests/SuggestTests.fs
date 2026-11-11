/// "Did you mean" hints: the helper, and each place a name, command, option, value or rule ID
/// can be mistyped.
module Tessera.Tests.SuggestTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.XmlaFakes
open Tessera.Cli.Common

let private model (t: TempDir) =
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase(sampleDatabase ()))
    path

/// The JSON error's suggestions ([] when it has none).
let private suggestions (r: Run) =
    use doc = JsonDocument.Parse r.Out
    let error = doc.RootElement.GetProperty "error"

    match error.TryGetProperty "suggestions" with
    | true, s -> [ for x in s.EnumerateArray() -> x.GetString() ]
    | _ -> []

let private errorCode (r: Run) =
    use doc = JsonDocument.Parse r.Out
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

[<Tests>]
let tests =
    testList "did you mean" [
        testList "Suggest" [
            testCase "distance: Damerau-Levenshtein, case-insensitive"
            <| fun () ->
                Expect.equal (Suggest.distance "validate" "vaildate") 1 "a swap is one edit"
                Expect.equal (Suggest.distance "Total" "total") 0 "case doesn't count"
                Expect.equal (Suggest.distance "" "abc") 3 ""
                Expect.equal (Suggest.distance "kitten" "sitting") 3 ""

            testProperty "distance is symmetric, zero only for equal strings, at most the longer length"
            <| fun (a: string) (b: string) ->
                let a = if isNull a then "" else a
                let b = if isNull b then "" else b
                let d = Suggest.distance a b

                d = Suggest.distance b a
                && (d = 0) = (a.ToLowerInvariant() = b.ToLowerInvariant())
                && d <= max a.Length b.Length

            testCase "closest: within about a third of the length, closest first, at most three"
            <| fun () ->
                Expect.equal (Suggest.closest [ "warning"; "error"; "info" ] "eror") [ "error" ] ""
                Expect.equal (Suggest.closest [ "warning"; "error"; "info" ] "fatal") [] "nothing close"
                Expect.equal (Suggest.closest [ "a"; "b"; "ab" ] "c") [] "a one-letter name suggests nothing"

                Expect.equal
                    (Suggest.closest [ "abcd"; "abc"; "abd"; "abe"; "abf" ] "abx")
                    [ "abc"; "abd"; "abe" ]
                    "ties keep order"

                Expect.equal
                    (Suggest.closest [ "Sales"; "Sale"; "sales" ] "Sales")
                    [ "sales"; "Sale" ]
                    "not the input itself; case-only first"

            testCase "hint"
            <| fun () ->
                Expect.equal (Suggest.hint []) "" ""
                Expect.equal (Suggest.hint [ "a" ]) " (did you mean 'a'?)" ""
                Expect.equal (Suggest.hint [ "a"; "b" ]) " (did you mean 'a' or 'b'?)" ""
        ]

        testCase "an object name: the closest references, in the message and in JSON"
        <| fun () ->
            use t = new TempDir()
            let m = model t
            let r = tess [ "get"; m; "measure:Sales/Totl" ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err "did you mean measure:Sales/Total" ""
            let j = tess [ "get"; m; "measure:Sales/Totl"; "--json" ]
            Expect.equal (errorCode j) "object-not-found" ""
            Expect.equal (suggestions j) [ "measure:Sales/Total" ] ""

            Expect.equal
                (suggestions (tess [ "get"; m; "'Sales'[Totl]"; "--json" ]))
                [ "measure:Sales/Total" ]
                "Table[Name]"

            Expect.equal (suggestions (tess [ "get"; m; "[Totl]"; "--json" ])) [ "measure:Sales/Total" ] "[Name]"

            Expect.equal
                (suggestions (tess [ "get"; m; "Sles"; "--json" ]))
                [ "table:Sales" ]
                "a bare name: tables first"

            let none = tess [ "get"; m; "measure:Sales/Nothing Like It"; "--json" ]
            Expect.equal (errorCode none) "object-not-found" ""
            Expect.equal (suggestions none) [] "nothing close: no suggestions key"

        testCase "remove --if-exists still skips a name that is only close"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "remove"; model t; "measure:Sales/Totl"; "--if-exists" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "skipped" ""

        testCase "move to a table that is only close: the move is refused, naming the table meant"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "move"; model t; "measure:Sales/Total"; "Dte" ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err "there is no table 'Dte' (did you mean 'table:Date'?)" ""

        testCase "set: a reference to an object that is only close is still not found"
        <| fun () ->
            use t = new TempDir()

            let r =
                tess [ "set"; model t; "column:Date/Month"; "-p"; "sortByColumn=Yaer"; "--json" ]

            Expect.equal (errorCode r) "object-not-found" ""
            Expect.contains (suggestions r) "column:Date/Year" ""

        testCase "refresh: a target that is only close"
        <| fun () ->
            let server = FakeServer()
            let sales = sampleDatabase ()
            sales.Name <- "Sales"
            server.Add sales

            let r =
                tessOn server tokenEnv [ "refresh"; $"xmla:{workspace};Sales"; "measure:Sales/Totl"; "--json" ]

            Expect.equal (suggestions r) [ "measure:Sales/Total" ] ""

        testCase "a command"
        <| fun () ->
            let r = tess [ "vaildate"; "x" ]
            Expect.equal r.Code ExitCode.usage ""
            Expect.stringStarts r.Err "ERROR: unrecognized argument: 'vaildate' (did you mean 'validate'?)." ""
            Expect.equal (suggestions (tess [ "--json"; "vaildate"; "x" ])) [ "validate" ] ""
            Expect.equal (suggestions (tess [ "bpa"; "rules"; "shwo"; "x"; "--json" ])) [ "show" ] "a subcommand"
            Expect.equal (suggestions (tess [ "--json"; "frobnicate" ])) [] "nothing close"

        testCase "an option"
        <| fun () ->
            let r = tess [ "validate"; "x"; "--fial-on"; "error" ]
            Expect.equal r.Code ExitCode.usage ""
            Expect.stringStarts r.Err "ERROR: unrecognized argument: '--fial-on' (did you mean '--fail-on'?)." ""
            Expect.equal (suggestions (tess [ "lint"; "x"; "--fixx"; "--json" ])) [ "--fix" ] ""
            Expect.equal (suggestions (tess [ "bpa"; "x"; "-x"; "--json" ])) [] "a one-letter option suggests nothing"

        testCase "option values: --ci, --fail-on, --min-severity, --kind"
        <| fun () ->
            use t = new TempDir()
            let m = model t
            let ci = tess [ "validate"; m; "--ci"; "githb" ]
            Expect.equal ci.Code ExitCode.usage ""
            Expect.stringStarts ci.Err "--ci: unknown CI system 'githb' (did you mean 'github'?)" ""
            Expect.equal (suggestions (tess [ "bpa"; m; "--ci"; "azdoo"; "--json" ])) [ "azdo" ] ""
            Expect.equal (suggestions (tess [ "lint"; m; "--fail-on"; "wraning"; "--json" ])) [ "warning" ] ""
            Expect.equal (suggestions (tess [ "bpa"; m; "--fail-on"; "eror"; "--json" ])) [ "error" ] ""

            Expect.equal
                (suggestions (tess [ "data"; "check"; m; t.Path; "--min-severity"; "inf"; "--json" ]))
                [ "info" ]
                ""

            Expect.equal (suggestions (tess [ "list"; m; "--kind"; "mesure"; "--json" ])) [ "measure" ] ""
            Expect.equal (suggestions (tess [ "find"; m; "x"; "--kind"; "colum"; "--json" ])) [ "column" ] ""

            Expect.stringContains
                (tess [ "convert"; m; t.Combine "x"; "--format"; "tmld" ]).Err
                "did you mean 'tmdl'?"
                ""

        testCase "rule IDs: validate, lint and data check --rule; bpa rules show"
        <| fun () ->
            use t = new TempDir()
            let m = model t

            Expect.equal
                (suggestions (tess [ "validate"; m; "--rule"; "circular-refrence"; "--json" ]))
                [ "circular-reference" ]
                ""

            let lint = tess [ "lint"; m; "--rule"; "DI00l" ]
            Expect.stringStarts lint.Err "--rule: unknown rule 'DI00l' (did you mean 'DI001'" ""

            Expect.equal
                (suggestions (tess [ "data"; "check"; m; t.Path; "--rule"; "blank-kye"; "--json" ]))
                [ "blank-key" ]
                ""

            let show = tess [ "bpa"; "rules"; "show"; m; "TESS_DIVDE"; "--json" ]
            Expect.equal (errorCode show) "rule-not-found" ""
            Expect.equal (suggestions show) [ "TESS_DIVIDE" ] ""
            let text = tess [ "bpa"; "rules"; "show"; m; "TESS_DIVDE" ]
            Expect.equal text.Code ExitCode.input ""

            Expect.stringContains
                text.Err
                "No BPA rule 'TESS_DIVDE' in the effective rules (did you mean 'TESS_DIVIDE'?)"
                ""
    ]
