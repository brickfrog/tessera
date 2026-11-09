module Tessera.Tests.BpaRulesCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.BpaTests
open Tessera.Cli.Common

/// A temp folder with the BPA fixture saved as model.bim (after `change`).
let private model (change: Database -> unit) =
    let t = new TempDir()
    let db = bpaDatabase ()
    change db

    match ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false db (t.Combine "model.bim") with
    | Ok _ -> t
    | Error e -> failtest (TessError.message e)

let private named (id: string) (name: string) (expression: string) : (string * obj) list = [
    "ID", box id
    "Name", box name
    "Severity", box 3
    "Scope", box "Model"
    "Expression", box expression
]

let private withModelRule (id: string) (name: string) (db: Database) =
    db.Model.Annotations.Add(new Annotation(Name = "BestPracticeAnalyzer", Value = ruleJson [ named id name "true" ]))

let private findingsOf (doc: JsonDocument) = [
    for f in doc.RootElement.GetProperty("findings").EnumerateArray() ->
        f.GetProperty("code").GetString(), f.GetProperty("objectPath").GetString()
]

let private ruleIn (doc: JsonDocument) (id: string) =
    doc.RootElement.GetProperty("rules").EnumerateArray()
    |> Seq.find (fun r -> r.GetProperty("id").GetString() = id)

[<Tests>]
let tests =
    testList "CLI bpa rules" [
        testCase "bpa: --rules beats TESS_BPA_RULES, which beats config and the model's own rules"
        <| fun () ->
            use t = model (withModelRule "SAME" "from the model")
            let m = t.Combine "model.bim"
            let config = ruleJson [ named "SAME" "from config" "true" ]
            // The user config: ~/.config/tessera on Linux and macOS, %APPDATA%\tessera on Windows.
            write (t.Combine("home", ".config", "tessera", "bpa.json")) config
            write (t.Combine("appdata", "tessera", "bpa.json")) config
            write (t.Combine "env.json") (ruleJson [ named "SAME" "from env" "true" ])
            write (t.Combine "cli.json") (ruleJson [ named "SAME" "from cli" "true" ])
            let userDirs = [ "HOME", t.Combine "home"; "APPDATA", t.Combine "appdata" ]
            let env = userDirs @ [ "TESS_BPA_RULES", t.Combine "env.json" ]

            let nameOf (r: Run) =
                use doc = json r
                (ruleIn doc "SAME").GetProperty("name").GetString()

            let list extra =
                tessWithEnv env ([ "bpa"; "rules"; "list"; m; "--json" ] @ extra)

            Expect.equal (nameOf (list [ "--rules"; t.Combine "cli.json" ])) "from cli" ""
            Expect.equal (nameOf (list [])) "from env" ""

            Expect.equal (nameOf (tessWithEnv userDirs [ "bpa"; "rules"; "list"; m; "--json" ])) "from config" ""

            Expect.equal (nameOf (list [ "--no-config" ])) "from the model" ""

            // The same precedence applies when analyzing.
            let r =
                tessWithEnv env [ "bpa"; m; "--no-builtin"; "--rules"; t.Combine "cli.json" ]

            Expect.equal r.Code ExitCode.findings r.Err
            Expect.stringContains r.Out "SAME  from cli" ""

        testCase "bpa rules list: layers, sources and overrides; --layer filters"
        <| fun () ->
            use t = model (withModelRule "TESS_DIVIDE" "Model's divide")
            let m = t.Combine "model.bim"
            let r = tess [ "bpa"; "rules"; "list"; m; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let divide = ruleIn doc "TESS_DIVIDE"
            Expect.equal (divide.GetProperty("layer").GetString()) "model" ""
            Expect.equal (divide.GetProperty("source").GetString()) "model annotation" ""
            let over = divide.GetProperty("overrides").[0]
            Expect.equal (over.GetProperty("layer").GetString()) "builtin" ""
            Expect.equal ((ruleIn doc "TESS_IFERROR").GetProperty("layer").GetString()) "builtin" ""
            Expect.isTrue (doc.RootElement.GetProperty("fetched").GetArrayLength() = 0) ""

            let text = tess [ "bpa"; "rules"; "list"; m; "--layer"; "model" ]
            Expect.equal text.Code ExitCode.ok text.Err
            let lines = text.Out.TrimEnd().Split('\n')
            Expect.equal lines.Length 1 "only the model's rule"
            Expect.stringContains lines.[0] "TESS_DIVIDE" ""
            Expect.stringContains lines.[0] "[overrides builtin]" ""

            let bad = tess [ "bpa"; "rules"; "list"; m; "--layer"; "nope" ]
            Expect.equal bad.Code ExitCode.usage ""

        testCase "--builtin-only, --no-builtin (and the old --no-default-rules)"
        <| fun () ->
            use t = model (withModelRule "MINE" "mine")
            let m = t.Combine "model.bim"
            write (t.Combine "r.json") (ruleJson [ named "FILE" "file" "false" ])

            let ids (r: Run) =
                use doc = json r

                [
                    for x in doc.RootElement.GetProperty("rules").EnumerateArray() -> x.GetProperty("id").GetString()
                ]

            let only = tess [ "bpa"; "rules"; "list"; m; "--builtin-only"; "--json" ]
            Expect.isFalse (ids only |> List.contains "MINE") ""
            Expect.contains (ids only) "TESS_DIVIDE" ""

            for flag in [ "--no-builtin"; "--no-default-rules" ] do
                let none = tess [ "bpa"; "rules"; "list"; m; flag; "--json" ]
                Expect.equal (ids none) [ "MINE" ] flag

            let conflict = tess [ "bpa"; m; "--builtin-only"; "--rules"; t.Combine "r.json" ]
            Expect.equal conflict.Code ExitCode.usage ""
            Expect.stringContains conflict.Err "--builtin-only" ""
            Expect.equal (tess [ "bpa"; m; "--builtin-only"; "--no-builtin" ]).Code ExitCode.usage ""

        testCase "bpa without a model, or bpa options before `rules`, is a usage error"
        <| fun () ->
            use t = model ignore
            Expect.equal (tess [ "bpa" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "bpa"; t.Combine "model.bim"; t.Combine "model.bim" ]).Code ExitCode.usage "one model"

            let r = tess [ "bpa"; "--no-builtin"; "rules"; "list"; t.Combine "model.bim" ]
            Expect.equal r.Code ExitCode.usage ""
            Expect.stringContains r.Err "after the subcommand" ""

        testCase "bpa rules show: one rule, text and JSON; an unknown ID exits 3"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"
            let r = tess [ "bpa"; "rules"; "show"; m; "tess_divide" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "ID:            TESS_DIVIDE" ""
            Expect.stringContains r.Out "Layer:         builtin" ""
            Expect.stringContains r.Out "Expression:" ""

            let j = tess [ "bpa"; "rules"; "show"; m; "TESS_DIVIDE"; "--json" ]
            use doc = json j
            Expect.equal (doc.RootElement.GetProperty("severity").GetInt32()) 2 ""

            let missing = tess [ "bpa"; "rules"; "show"; m; "NOPE"; "--json" ]
            Expect.equal missing.Code ExitCode.input ""
            use err = json missing
            Expect.equal (err.RootElement.GetProperty("error").GetProperty("code").GetString()) "rule-not-found" ""

        testCase "bpa rules add --to model: a dry run, then --save; bpa then runs the rule"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"
            let before = File.ReadAllText m

            let args = [
                "bpa"
                "rules"
                "add"
                m
                "--id"
                "LONG_NAMES"
                "--name"
                "Keep names short"
                "--severity"
                "3"
                "--scope"
                "Measure"
                "-e"
                "Name.Length > 4"
            ]

            let dry = tess (args @ [ "--json" ])
            Expect.equal dry.Code ExitCode.ok dry.Err
            use doc = json dry
            Expect.isTrue (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (doc.RootElement.GetProperty("command").GetString()) "bpa rules add" ""
            Expect.equal (File.ReadAllText m) before "nothing written"

            let saved = tess (args @ [ "--save" ])
            Expect.equal saved.Code ExitCode.ok saved.Err
            Expect.stringContains saved.Out "annotations.BestPracticeAnalyzer" "the change, as a diff"
            Expect.stringContains saved.Err "Added rule LONG_NAMES" ""

            let run = tess [ "bpa"; m; "--no-builtin"; "--json" ]
            Expect.equal run.Code ExitCode.findings run.Err
            use findings = json run
            Expect.contains (findingsOf findings) ("LONG_NAMES", "measure:Sales/Ratio") ""

            let again = tess (args @ [ "--save" ])
            Expect.equal again.Code ExitCode.input "the ID is taken"
            Expect.stringContains again.Err "--replace" ""
            Expect.equal (tess (args @ [ "--save"; "--replace" ])).Code ExitCode.ok ""

            let removed = tess [ "bpa"; "rules"; "remove"; m; "long_names"; "--save" ]
            Expect.equal removed.Code ExitCode.ok removed.Err
            Expect.isFalse ((File.ReadAllText m).Contains "LONG_NAMES") ""

        testCase "bpa rules add: missing switches and bad scopes are usage errors; non-compiling rules warn"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"

            let missing = tess [ "bpa"; "rules"; "add"; m; "--id"; "X"; "--scope"; "Measure" ]
            Expect.equal missing.Code ExitCode.usage ""
            Expect.stringContains missing.Err "--expression" ""

            let empty =
                tess [ "bpa"; "rules"; "add"; m; "--id"; "X"; "--scope"; ""; "-e"; "true" ]

            Expect.equal empty.Code ExitCode.usage ""

            let broken =
                tess [ "bpa"; "rules"; "add"; m; "--id"; "X"; "--scope"; "Measure"; "-e"; "Nope" ]

            Expect.equal broken.Code ExitCode.ok broken.Err
            Expect.stringContains broken.Err "won't run" ""

        testCase "bpa rules add/remove --to <file>: creates and edits a TE rule file"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"
            let file = t.Combine("rules", "team.json")

            let add id =
                tess [
                    "bpa"
                    "rules"
                    "add"
                    m
                    "--to"
                    file
                    "--id"
                    id
                    "--scope"
                    "Table"
                    "-e"
                    "Name <> \"\""
                    "--save"
                ]

            let dry =
                tess [
                    "bpa"
                    "rules"
                    "add"
                    m
                    "--to"
                    file
                    "--id"
                    "A"
                    "--scope"
                    "Table"
                    "-e"
                    "true"
                ]

            Expect.equal dry.Code ExitCode.ok dry.Err
            Expect.stringContains dry.Out "+ rule A" ""
            Expect.isFalse (File.Exists file) "a dry run writes nothing"

            Expect.equal (add "A").Code ExitCode.ok ""
            Expect.equal (add "B").Code ExitCode.ok ""
            Expect.stringContains (File.ReadAllText file) "\"Expression\": \"Name <> \\\"\\\"\"" ""

            match BpaRules.loadFile file with
            | Ok rules -> Expect.equal (rules |> List.map (fun r -> r.Id)) [ "A"; "B" ] ""
            | Error e -> failtest (TessError.message e)

            let removed =
                tess [ "bpa"; "rules"; "remove"; m; "A"; "--from"; file; "--save"; "--json" ]

            Expect.equal removed.Code ExitCode.ok removed.Err
            use doc = json removed
            Expect.equal (doc.RootElement.GetProperty("removed").[0].GetString()) "A" ""

            match BpaRules.loadFile file with
            | Ok rules -> Expect.equal (rules |> List.map (fun r -> r.Id)) [ "B" ] ""
            | Error e -> failtest (TessError.message e)

            let saveTo =
                tess [ "bpa"; "rules"; "remove"; m; "B"; "--to"; file; "--save-to"; t.Combine "x" ]

            Expect.equal saveTo.Code ExitCode.usage ""

        testCase "bpa rules remove: a built-in rule can't be removed, and says how to turn it off"
        <| fun () ->
            use t = model ignore
            let r = tess [ "bpa"; "rules"; "remove"; t.Combine "model.bim"; "TESS_DIVIDE" ]
            Expect.equal r.Code ExitCode.input ""
            Expect.stringContains r.Err "built-in" ""
            Expect.stringContains r.Err "bpa rules ignore" ""

        testCase "bpa rules ignore/unignore: TE's annotation, honoured by bpa, through the edit pipeline"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"

            let divideOnRatio () =
                use doc = json (tess [ "bpa"; m; "--json" ])
                findingsOf doc |> List.contains ("TESS_DIVIDE", "measure:Sales/Ratio")

            Expect.isTrue (divideOnRatio ()) "flagged to begin with"

            let dry = tess [ "bpa"; "rules"; "ignore"; m; "TESS_DIVIDE"; "[Ratio]" ]
            Expect.equal dry.Code ExitCode.ok dry.Err
            Expect.stringContains dry.Out "BestPracticeAnalyzer_IgnoreRules" ""
            Expect.stringContains dry.Err "Dry run" ""
            Expect.isTrue (divideOnRatio ()) "not saved"

            let saved =
                tess [ "bpa"; "rules"; "ignore"; m; "TESS_DIVIDE"; "[Ratio]"; "--save"; "--json" ]

            Expect.equal saved.Code ExitCode.ok saved.Err
            use doc = json saved
            Expect.isFalse (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.isFalse (divideOnRatio ()) "ignored now"
            Expect.stringContains (File.ReadAllText m) "{\\\"RuleIDs\\\":[\\\"TESS_DIVIDE\\\"]}" "TE's format"

            use after = json (tess [ "bpa"; m; "--json" ])

            Expect.isTrue
                (after.RootElement.GetProperty("ignored").EnumerateArray()
                 |> Seq.exists (fun f -> f.GetProperty("objectPath").GetString() = "measure:Sales/Ratio"))
                "listed as ignored"

            let noop = tess [ "bpa"; "rules"; "ignore"; m; "TESS_DIVIDE"; "[Ratio]" ]
            Expect.stringContains noop.Out "No changes." ""

            let un = tess [ "bpa"; "rules"; "unignore"; m; "TESS_DIVIDE"; "[Ratio]"; "--save" ]
            Expect.equal un.Code ExitCode.ok un.Err
            Expect.isTrue (divideOnRatio ()) "flagged again"
            Expect.isFalse ((File.ReadAllText m).Contains "BestPracticeAnalyzer_IgnoreRules") "annotation removed"

        testCase "bpa rules ignore: on the model by default; unknown IDs warn; calculation items exit 3"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"

            let r = tess [ "bpa"; "rules"; "ignore"; m; "TESS_DIVIDE,TYPO"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "on model" ""
            Expect.stringContains r.Err "no rule with ID TYPO" ""

            use doc = json (tess [ "bpa"; m; "--json" ])

            Expect.isTrue
                (doc.RootElement.GetProperty("skippedRules").EnumerateArray()
                 |> Seq.exists (fun s -> s.GetProperty("ruleId").GetString() = "TESS_DIVIDE"))
                "turned off for the model"

            let listed = tess [ "bpa"; "rules"; "list"; m ]
            Expect.stringContains listed.Out "[ignored on the model]" ""

            let calc =
                tess [ "bpa"; "rules"; "ignore"; m; "TESS_DIVIDE"; "calcitem:Time Intelligence/YTD" ]

            Expect.equal calc.Code ExitCode.input ""
            Expect.stringContains calc.Err "no annotations" ""

            Expect.equal (tess [ "bpa"; "rules"; "ignore"; m ]).Code ExitCode.usage "a rule ID is required"

        testCase "bpa --path: only findings on matching objects count"
        <| fun () ->
            use t = model ignore
            let m = t.Combine "model.bim"

            let paths (extra: string list) =
                use doc = json (tess ([ "bpa"; m; "--json" ] @ extra))
                findingsOf doc |> List.map snd |> List.distinct

            let all = paths []
            Expect.isTrue (all |> List.exists (fun p -> not (p.StartsWith "measure:"))) "the fixture has others"

            let measures = paths [ "--path"; "measure:" ]
            Expect.isNonEmpty measures ""
            Expect.isTrue (measures |> List.forall (fun p -> p.StartsWith "measure:")) "a prefix"

            let glob = paths [ "--path"; "*:sales/*" ]
            Expect.isNonEmpty glob ""
            Expect.isTrue (glob |> List.forall (fun p -> p.Contains ":Sales/")) "a case-insensitive glob"

            let r = tess [ "bpa"; m; "--path"; "perspective:Nothing*" ]
            Expect.equal r.Code ExitCode.ok "no findings there: exit 0"
            Expect.stringStarts r.Out "No findings" ""

        testCase "external rule URLs are not fetched without --allow-external-rules, and are named"
        <| fun () ->
            use t =
                model (fun db ->
                    db.Model.Annotations.Add(
                        new Annotation(
                            Name = "BestPracticeAnalyzer_ExternalRuleFiles",
                            Value = """["https://example.invalid/rules.json"]"""
                        )
                    ))

            let r = tess [ "bpa"; t.Combine "model.bim" ]
            Expect.stringContains r.Err "note: external rule file https://example.invalid/rules.json not fetched" ""
            Expect.stringContains r.Err "--allow-external-rules" ""
            Expect.isFalse (r.Err.Contains "requested") "no request"

            let j = tess [ "bpa"; t.Combine "model.bim"; "--json" ]
            Expect.equal j.Err "" "nothing on stderr in JSON mode"
            use doc = json j
            Expect.stringContains (doc.RootElement.GetProperty("notes").[0].GetString()) "example.invalid" ""
            Expect.equal (doc.RootElement.GetProperty("fetched").GetArrayLength()) 0 ""

        testCase "requested URLs are always named on stderr, even with --json"
        <| fun () ->
            use err = new StringWriter()

            let ctx: Ctx = {
                Out = TextWriter.Null
                Err = err
                Json = true
                GetEnv = fun _ -> None
            }

            Tessera.Cli.BpaRulesCommand.reportCollected ctx "bpa" {
                All = []
                Effective = []
                Warnings = [ "w" ]
                Notes = [ "n" ]
                Fetched = [ "https://example.com/r.json" ]
            }

            Expect.equal
                (err.ToString().Trim())
                "tess: bpa: requested external rules from https://example.com/r.json"
                "the URL, but no notes or warnings (JSON has them)"
    ]
