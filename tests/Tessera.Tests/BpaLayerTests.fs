module Tessera.Tests.BpaLayerTests

open System
open System.IO
open System.Text.Json.Nodes
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.BpaRules
open Tessera.Tests.Fixtures
open Tessera.Tests.BpaTests

let private ruleNamed (id: string) (name: string) : (string * obj) list = [
    "ID", box id
    "Name", box name
    "Severity", box 2
    "Scope", box "Model"
    "Expression", box "false"
]

/// Save `db` as model.bim under `dir` and load it back (so the source is real).
let private saveAndLoad (db: Database) (dir: string) =
    Directory.CreateDirectory dir |> ignore
    let bim = Path.Combine(dir, "model.bim")

    match ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false db bim with
    | Error e -> failtest (TessError.message e)
    | Ok _ ->
        match ModelIO.loadPath bim with
        | Ok x -> x
        | Error e -> failtest (TessError.message e)

let private envOf (vars: (string * string) list) : Bpa.RuleEnvironment = {
    GetEnv = fun n -> vars |> List.tryFind (fst >> (=) n) |> Option.map snd
    IsWindows = false
}

let private collect source db opts =
    match Bpa.collectRules source db opts with
    | Ok c -> c
    | Error e -> failtest (TessError.message e)

let private effective (c: Bpa.CollectedRules) (id: string) =
    c.Effective
    |> List.tryFind (fun r -> String.Equals(r.Id, id, StringComparison.OrdinalIgnoreCase))
    |> Option.defaultWith (fun () -> failtest $"no effective rule {id}")

let private annotate (o: Model) (name: string) (value: string) =
    o.Annotations.Add(new Annotation(Name = name, Value = value))

/// A fake fetcher that records the URLs it was asked for.
type private FakeFetcher(respond: Uri -> Result<string, string>) =
    let calls = Collections.Generic.List<string>()
    member _.Calls = List.ofSeq calls

    member _.Fetch: Bpa.Fetcher =
        fun u ->
            calls.Add(string u)
            respond u

[<Tests>]
let layerTests =
    testList "BPA rule layers" [
        testCase "a higher layer replaces a lower one's rule with the same ID: builtin < model < config < env < cli"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            // The rule ID of a built-in rule, so the built-in layer takes part too.
            let id = "TESS_DIVIDE"
            annotate db.Model "BestPracticeAnalyzer" (ruleJson [ ruleNamed id "model" ])
            let source, loaded = saveAndLoad db (t.Combine("repo", "models"))
            Directory.CreateDirectory(t.Combine("repo", ".git")) |> ignore
            write (t.Combine("repo", ".tessera", "bpa.json")) (ruleJson [ ruleNamed id "project" ])
            write (t.Combine("home", ".config", "tessera", "bpa.json")) (ruleJson [ ruleNamed id "user" ])
            write (t.Combine "env1.json") (ruleJson [ ruleNamed id "env1" ])
            write (t.Combine "env2.json") (ruleJson [ ruleNamed id "env2" ])
            write (t.Combine "cli.json") (ruleJson [ ruleNamed id "cli" ])

            let env =
                envOf [
                    "HOME", t.Combine "home"
                    "TESS_BPA_RULES", t.Combine "env1.json" + string Path.PathSeparator + t.Combine "env2.json"
                ]

            let opts = {
                Bpa.RuleOptions.defaults with
                    Files = [ t.Combine "cli.json" ]
                    Environment = env
            }

            let name o =
                (effective (collect source loaded o) id).Name

            Expect.equal (name opts) "cli" "--rules wins"
            Expect.equal (name { opts with Files = [] }) "env2" "then TESS_BPA_RULES, the later file winning"

            Expect.equal
                (name {
                    opts with
                        Files = []
                        Environment = envOf [ "HOME", t.Combine "home" ]
                })
                "project"
                "then the project config over the user's"

            Expect.equal
                (name {
                    opts with
                        Files = []
                        Environment = Bpa.RuleEnvironment.empty
                })
                "project"
                ""

            Expect.equal (name { opts with Files = []; Config = false }) "model" "then the model's own rules"

            Expect.equal
                (name {
                    opts with
                        Files = []
                        Config = false
                        ModelRules = false
                })
                "Use DIVIDE when the divisor can be zero"
                "then the built-in rule"

            let all = collect source loaded opts
            let overridden = Bpa.CollectedRules.overridden all (effective all id)

            Expect.equal
                (overridden |> List.map (fun r -> RuleSource.layer r.Source))
                [
                    RuleLayer.BuiltIn
                    RuleLayer.Model
                    RuleLayer.Config
                    RuleLayer.Config
                    RuleLayer.Env
                    RuleLayer.Env
                ]
                "every definition it replaced, lowest first"

        testCase "layers merge by ID, case-insensitively: other rules from every layer stay"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            annotate db.Model "BestPracticeAnalyzer" (ruleJson [ ruleNamed "MODEL_ONLY" "m"; ruleNamed "shared" "m" ])
            let source, loaded = saveAndLoad db (t.Combine "m")
            write (t.Combine "cli.json") (ruleJson [ ruleNamed "CLI_ONLY" "c"; ruleNamed "SHARED" "c" ])

            let c =
                collect source loaded {
                    Bpa.RuleOptions.defaults with
                        BuiltIn = false
                        Files = [ t.Combine "cli.json" ]
                }

            Expect.equal
                (c.Effective |> List.map (fun r -> r.Id, r.Name))
                [ "MODEL_ONLY", "m"; "SHARED", "c"; "CLI_ONLY", "c" ]
                "one SHARED, from the higher layer, in the first definition's position"

            Expect.equal c.All.Length 4 "All keeps every definition"

        testCase "the model's external rule files: the first listed wins, as in TE; embedded rules beat them"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            write (t.Combine("m", "a.json")) (ruleJson [ ruleNamed "X" "a"; ruleNamed "Y" "a" ])
            write (t.Combine("m", "b.json")) (ruleJson [ ruleNamed "X" "b"; ruleNamed "Y" "b" ])
            annotate db.Model "BestPracticeAnalyzer_ExternalRuleFiles" """["a.json", "b.json"]"""
            annotate db.Model "BestPracticeAnalyzer" (ruleJson [ ruleNamed "Y" "embedded" ])
            let source, loaded = saveAndLoad db (t.Combine "m")

            let c =
                collect source loaded {
                    Bpa.RuleOptions.defaults with
                        BuiltIn = false
                }

            Expect.equal (effective c "X").Name "a" ""
            Expect.equal (effective c "Y").Name "embedded" ""

        testCase "files the user names must be valid; the model's own references only warn"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            annotate db.Model "BestPracticeAnalyzer_ExternalRuleFiles" """["missing.json"]"""
            let source, loaded = saveAndLoad db (t.Combine "m")
            write (t.Combine "bad.json") "{ not json"

            let withEnv value = {
                Bpa.RuleOptions.defaults with
                    Environment = envOf [ "TESS_BPA_RULES", value ]
            }

            match Bpa.collectRules source loaded (withEnv (t.Combine "nope.json")) with
            | Error(PathNotFound _) -> ()
            | other -> failtestf "expected PathNotFound, got %A" (Result.map (fun _ -> ()) other)

            match Bpa.collectRules source loaded (withEnv (t.Combine "bad.json")) with
            | Error(InvalidRules _) -> ()
            | other -> failtestf "expected InvalidRules, got %A" (Result.map (fun _ -> ()) other)

            let c = collect source loaded Bpa.RuleOptions.defaults
            Expect.equal c.Warnings [ "external rule file missing.json not found" ] ""

            write (t.Combine("m", ".tessera", "bpa.json")) "[ {} ]"

            match Bpa.collectRules source loaded Bpa.RuleOptions.defaults with
            | Error(InvalidRules(path, _)) -> Expect.stringEnds path "bpa.json" "a broken config file is an error"
            | other -> failtestf "expected InvalidRules, got %A" (Result.map (fun _ -> ()) other)

            let c =
                collect source loaded {
                    Bpa.RuleOptions.defaults with
                        Config = false
                        Environment = envOf [ "TESS_BPA_RULES", t.Combine "nope.json" ]
                }

            Expect.isNonEmpty c.Effective "Config = false skips TESS_BPA_RULES and config files"

        testCase "builtInOnly ignores everything but the built-in rules"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            annotate db.Model "BestPracticeAnalyzer" (ruleJson [ ruleNamed "MODEL_ONLY" "m" ])
            let source, loaded = saveAndLoad db (t.Combine "m")
            write (t.Combine("m", ".tessera", "bpa.json")) (ruleJson [ ruleNamed "PROJECT" "p" ])

            let c = collect source loaded Bpa.RuleOptions.builtInOnly
            Expect.isTrue (c.Effective |> List.forall (fun r -> r.Source = BuiltIn)) ""
            Expect.equal c.Effective.Length BpaRules.builtIn.Value.Length ""
    ]

[<Tests>]
let configTests =
    testList "BPA config files" [
        testCase "the user's config file: XDG_CONFIG_HOME, else ~/.config; APPDATA on Windows"
        <| fun () ->
            let root = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "x"))

            let file (parts: string list) =
                Some(Path.Combine(Array.ofList (root :: parts)))

            Expect.equal
                (Bpa.RuleConfig.userFile (envOf [ "XDG_CONFIG_HOME", root; "HOME", "/elsewhere" ]))
                (file [ "tessera"; "bpa.json" ])
                ""

            Expect.equal
                (Bpa.RuleConfig.userFile (envOf [ "HOME", root ]))
                (file [ ".config"; "tessera"; "bpa.json" ])
                ""

            Expect.equal
                (Bpa.RuleConfig.userFile (envOf [ "XDG_CONFIG_HOME", "relative"; "HOME", root ]))
                (file [ ".config"; "tessera"; "bpa.json" ])
                "a relative XDG_CONFIG_HOME is ignored, as the spec says"

            Expect.equal (Bpa.RuleConfig.userFile (envOf [])) None "no variables, no user config"

            Expect.equal
                (Bpa.RuleConfig.userFile {
                    envOf [ "APPDATA", root; "HOME", "/elsewhere" ] with
                        IsWindows = true
                })
                (file [ "tessera"; "bpa.json" ])
                ""

        testCase "TESS_BPA_RULES is split like PATH"
        <| fun () ->
            let sep = string Path.PathSeparator

            Expect.equal
                (Bpa.RuleConfig.envFiles (envOf [ "TESS_BPA_RULES", $"a.json{sep}{sep} b.json " ]))
                [ Path.GetFullPath "a.json"; Path.GetFullPath "b.json" ]
                ""

        testCase "project config: the nearest .tessera/bpa.json up to the git repository's root"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            let source, _ = saveAndLoad db (t.Combine("repo", "a", "b"))
            File.WriteAllText(t.Combine("repo", ".git"), "gitdir: elsewhere") // a worktree's .git file

            let at (parts: string list) =
                t.Combine(Array.ofList (parts @ [ ".tessera"; "bpa.json" ]))

            Expect.equal (Bpa.RuleConfig.projectFileFor source) None ""
            write (at []) "[]"
            Expect.equal (Bpa.RuleConfig.projectFileFor source) None "above the repository: not used"
            write (at [ "repo" ]) "[]"
            Expect.equal (Bpa.RuleConfig.projectFileFor source) (Some(at [ "repo" ])) "the repository root"
            write (at [ "repo"; "a" ]) "[]"
            Expect.equal (Bpa.RuleConfig.projectFileFor source) (Some(at [ "repo"; "a" ])) "nearer wins"

        testCase "project config outside a repository: only the model's own folders"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            let source, _ = saveAndLoad db (t.Combine("a", "b"))
            write (t.Combine("a", ".tessera", "bpa.json")) "[]"
            Expect.equal (Bpa.RuleConfig.projectFileFor source) None ""
            write (t.Combine("a", "b", ".tessera", "bpa.json")) "[]"

            Expect.equal (Bpa.RuleConfig.projectFileFor source) (Some(t.Combine("a", "b", ".tessera", "bpa.json"))) ""

        testCase "project config of a PBIP project: next to the .pbip counts as the model's folder"
        <| fun () ->
            use t = new TempDir()
            writePbipProject t.Path "Sales" |> ignore
            write (t.Combine(".tessera", "bpa.json")) (ruleJson [ ruleNamed "PROJECT" "p" ])

            match ModelIO.loadPath (t.Combine "Sales.pbip") with
            | Error e -> failtest (TessError.message e)
            | Ok(source, db) ->
                let c = collect source db Bpa.RuleOptions.defaults
                Expect.equal (effective c "PROJECT").Source (ConfigFile(t.Combine(".tessera", "bpa.json"))) ""
    ]

[<Tests>]
let externalTests =
    testList "BPA external rule URLs" [
        let setup (urls: string list) =
            let t = new TempDir()
            let db = bpaDatabase ()
            let quoted = urls |> List.map (fun u -> $"\"{u}\"") |> String.concat ", "
            annotate db.Model "BestPracticeAnalyzer_ExternalRuleFiles" $"[{quoted}]"
            let source, loaded = saveAndLoad db (t.Combine "m")
            t, source, loaded

        testCase "without AllowExternal nothing is fetched, and each URL is named in a note"
        <| fun () ->
            let t, source, db =
                setup [ "https://example.com/a.json"; "http://example.com/b.json" ]

            use _t = t
            let fake = FakeFetcher(fun _ -> failtest "must not fetch")

            let c =
                collect source db {
                    Bpa.RuleOptions.defaults with
                        Fetch = fake.Fetch
                }

            Expect.isEmpty fake.Calls "no request"
            Expect.isEmpty c.Fetched ""
            Expect.equal c.Notes.Length 2 ""
            Expect.stringContains c.Notes.[0] "https://example.com/a.json" ""
            Expect.stringContains c.Notes.[1] "http://example.com/b.json" ""

        testCase "with AllowExternal: https only, no credentials in the URL, and the rules are used"
        <| fun () ->
            let t, source, db =
                setup [
                    "https://example.com/a.json"
                    "http://example.com/b.json"
                    "https://user:secret@example.com/c.json"
                ]

            use _t = t
            let fake = FakeFetcher(fun _ -> Ok(ruleJson [ ruleNamed "REMOTE" "r" ]))

            let c =
                collect source db {
                    Bpa.RuleOptions.defaults with
                        AllowExternal = true
                        Fetch = fake.Fetch
                }

            Expect.equal fake.Calls [ "https://example.com/a.json" ] "only the https URL without credentials"
            Expect.equal c.Fetched [ "https://example.com/a.json" ] ""
            Expect.equal (effective c "REMOTE").Source (ExternalRuleUrl "https://example.com/a.json") ""
            Expect.equal (RuleSource.layer (effective c "REMOTE").Source) RuleLayer.Model ""
            Expect.equal c.Warnings.Length 2 "the refused ones"
            Expect.stringContains c.Warnings.[0] "only https://" ""
            Expect.stringContains c.Warnings.[1] "credentials" ""

        testCase "a failed fetch or a bad file is a warning, not an error"
        <| fun () ->
            let t, source, db =
                setup [ "https://example.com/a.json"; "https://example.com/b.json" ]

            use _t = t

            let fake =
                FakeFetcher(fun u ->
                    if u.AbsolutePath.EndsWith "a.json" then
                        Error "timed out"
                    else
                        Ok "not json")

            let c =
                collect source db {
                    Bpa.RuleOptions.defaults with
                        AllowExternal = true
                        Fetch = fake.Fetch
                }

            Expect.equal fake.Calls.Length 2 ""
            Expect.equal c.Warnings.Length 2 ""
            Expect.stringContains c.Warnings.[0] "timed out" ""

        testCase "the real fetcher refuses http:// and credentials before any request"
        <| fun () ->
            Expect.isError (BpaFetch.fetch (Uri "http://example.com/a.json")) ""
            Expect.isError (BpaFetch.fetch (Uri "https://u:p@example.com/a.json")) ""
    ]

[<Tests>]
let editTests =
    testList "BPA rule and ignore edits" [
        testCase "ignore lists are written as TE writes them, and the analyzer honours them"
        <| fun () ->
            let db = bpaDatabase ()

            let edit ignore ids objects =
                match BpaRuleEdit.setIgnored ignore ids objects db with
                | Ok e -> e
                | Error e -> failtest (TessError.message e)

            edit true [ "TESS_DIVIDE"; "X" ] [ "measure:Sales/Ratio" ] |> ignore
            let ratio = db.Model.Tables.["Sales"].Measures.["Ratio"]

            Expect.equal
                ratio.Annotations.["BestPracticeAnalyzer_IgnoreRules"].Value
                """{"RuleIDs":["TESS_DIVIDE","X"]}"""
                "TE's JSON: compact, RuleIDs"

            let again = edit true [ "tess_divide" ] [ "measure:Sales/Ratio" ]
            Expect.stringContains (List.head again.Notes) "already ignored" "IDs compare case-insensitively"

            let divide = BpaRules.builtIn.Value |> List.filter (fun r -> r.Id = "TESS_DIVIDE")
            let result = Bpa.analyze db divide |> List.head

            Expect.isTrue
                (result.Ignored
                 |> List.exists (fun f -> ObjectRef.format f.Ref = "measure:Sales/Ratio"))
                "suppressed"

            edit false [ "TESS_DIVIDE" ] [ "measure:Sales/Ratio" ] |> ignore
            Expect.equal ratio.Annotations.["BestPracticeAnalyzer_IgnoreRules"].Value """{"RuleIDs":["X"]}""" ""
            edit false [ "X" ] [ "measure:Sales/Ratio" ] |> ignore
            Expect.isNull (ratio.Annotations.Find "BestPracticeAnalyzer_IgnoreRules") "empty: removed"

        testCase "TE's misspelled annotation is read and replaced; bare arrays and spacing are read"
        <| fun () ->
            let db = bpaDatabase ()
            let ratio = db.Model.Tables.["Sales"].Measures.["Ratio"]
            ratio.Annotations.Add(new Annotation(Name = "BestPractizeAnalyzer_IgnoreRules", Value = """[ "OLD" ]"""))

            match BpaRuleEdit.setIgnored true [ "NEW" ] [ "[Ratio]" ] db with
            | Error e -> failtest (TessError.message e)
            | Ok _ ->
                Expect.isNull (ratio.Annotations.Find "BestPractizeAnalyzer_IgnoreRules") ""

                Expect.equal
                    ratio.Annotations.["BestPracticeAnalyzer_IgnoreRules"].Value
                    """{"RuleIDs":["OLD","NEW"]}"""
                    ""

            let spaced = """{ "RuleIDs" : [ "A", "B" ] }"""
            Expect.equal (parseIgnoreList spaced) [ "A"; "B" ] ""

        testCase "ignoring on the model turns the rule off; calculation items can't be annotated"
        <| fun () ->
            let db = bpaDatabase ()

            match BpaRuleEdit.setIgnored true [ "TESS_DIVIDE" ] [] db with
            | Error e -> failtest (TessError.message e)
            | Ok _ ->
                let divide = BpaRules.builtIn.Value |> List.filter (fun r -> r.Id = "TESS_DIVIDE")
                Expect.equal (Bpa.analyze db divide |> List.head).Status Bpa.Disabled ""

            match BpaRuleEdit.setIgnored true [ "TESS_DIVIDE" ] [ "calcitem:Time Intelligence/YTD" ] db with
            | Error(CannotIgnore(r, _)) -> Expect.equal r "calcitem:Time Intelligence/YTD" ""
            | other -> failtestf "expected CannotIgnore, got %A" (Result.map (fun _ -> ()) other)

        testCase "ignore annotations survive a save to .bim and TMDL"
        <| fun () ->
            for format, name in [ ModelIO.BimFile, "model.bim"; ModelIO.TmdlFolder, "tmdl" ] do
                use t = new TempDir()
                let db = bpaDatabase ()

                BpaRuleEdit.setIgnored true [ "TESS_DIVIDE" ] [ "[Ratio]"; "Sales" ] db
                |> Result.mapError TessError.message
                |> Result.defaultWith failtest
                |> ignore

                match ModelIO.save format ModelIO.SaveOptions.defaults false db (t.Combine name) with
                | Error e -> failtest (TessError.message e)
                | Ok _ ->
                    match ModelIO.loadPath (t.Combine name) with
                    | Error e -> failtest (TessError.message e)
                    | Ok(_, loaded) ->
                        let sales = loaded.Model.Tables.["Sales"]

                        for a in
                            [
                                sales.Annotations.["BestPracticeAnalyzer_IgnoreRules"]
                                sales.Measures.["Ratio"].Annotations.["BestPracticeAnalyzer_IgnoreRules"]
                            ] do
                            Expect.equal a.Value """{"RuleIDs":["TESS_DIVIDE"]}""" name

        testCase "rules added to the model: TE's format, readable expressions, and read back as model rules"
        <| fun () ->
            let db = bpaDatabase ()

            let rule =
                JsonNode.Parse(
                    """{"ID":"MINE","Name":"Mine","Severity":3,"Scope":"Measure","Expression":"Name.Length > 3 && IsHidden","Extra":1}"""
                )
                :?> JsonObject

            match BpaRuleEdit.addToModel false [ rule ] db with
            | Error e -> failtest (TessError.message e)
            | Ok effect -> Expect.equal effect.Notes [ "Added rule MINE to the model's rules" ] ""

            let text = db.Model.Annotations.["BestPracticeAnalyzer"].Value
            Expect.stringContains text "\"Name.Length > 3 && IsHidden\"" "no \\u003E escapes"
            Expect.stringContains text "\"Extra\": 1" "unknown fields kept"
            Expect.stringStarts text "[\n  {\n    \"ID\": \"MINE\"," "an indented array, as TE writes it"

            match parse ModelAnnotation text with
            | Ok [ r ] -> Expect.equal (r.Id, r.Severity, r.Expression) ("MINE", 3, "Name.Length > 3 && IsHidden") ""
            | other -> failtestf "%A" other

            match BpaRuleEdit.addToModel false [ rule ] db with
            | Error(RuleExists("MINE", _)) -> ()
            | other -> failtestf "expected RuleExists, got %A" (Result.map (fun _ -> ()) other)

            match BpaRuleEdit.removeFromModel [ "mine" ] db with
            | Error e -> failtest (TessError.message e)
            | Ok _ -> Expect.isNull (db.Model.Annotations.Find "BestPracticeAnalyzer") "empty: removed, as TE does"

            match BpaRuleEdit.removeFromModel [ "MINE" ] db with
            | Error(RuleNotFound("MINE", _, _)) -> ()
            | other -> failtestf "expected RuleNotFound, got %A" (Result.map (fun _ -> ()) other)

        testCase "rule files: add, replace and remove keep other rules and fields, and the line endings"
        <| fun () ->
            use t = new TempDir()
            let path = t.Combine "rules.json"

            File.WriteAllText(
                path,
                "[\r\n  // team rules\r\n  { \"ID\": \"A\", \"Scope\": \"Model\", \"Expression\": \"true\", \"Source\": \"x\" },\r\n]"
            )

            let set =
                BpaRuleEdit.readFile path
                |> Result.mapError TessError.message
                |> Result.defaultWith failtest

            Expect.isTrue set.HadComments ""
            Expect.equal set.Eol "\r\n" ""

            let b =
                JsonNode.Parse("""{"ID":"B","Scope":"Measure","Expression":"Name <> \"\""}""") :?> JsonObject

            let added =
                BpaRuleEdit.addRules false [ b ] set
                |> Result.defaultWith (failtestf "exists: %s")

            Expect.equal (BpaRuleEdit.ids added.Set) [ "A"; "B" ] ""
            Expect.equal (BpaRuleEdit.ids set) [ "A" ] "the original is unchanged"
            Expect.equal (BpaRuleEdit.addRules false [ b ] added.Set |> Result.map ignore) (Error "B") ""

            let replaced =
                BpaRuleEdit.addRules true [ b ] added.Set |> Result.defaultWith (failtestf "%s")

            Expect.equal replaced.Replaced [ "B" ] ""

            BpaRuleEdit.writeFile path replaced.Set
            |> Result.mapError TessError.message
            |> Result.defaultWith failtest

            let written = File.ReadAllText path
            Expect.stringContains written "\"Source\": \"x\"" ""
            Expect.stringContains written "\r\n" ""
            Expect.isFalse (written.Replace("\r\n", "").Contains "\n") "CRLF throughout"

            match loadFile path with
            | Ok rules -> Expect.equal (rules |> List.map (fun r -> r.Id)) [ "A"; "B" ] "a valid rule file"
            | Error e -> failtest (TessError.message e)

            let removed =
                BpaRuleEdit.removeRules [ "a" ] replaced.Set
                |> Result.defaultWith (failtestf "%s")

            Expect.equal (BpaRuleEdit.ids removed) [ "B" ] ""
            Expect.equal (BpaRuleEdit.removeRules [ "Z" ] removed |> Result.map ignore) (Error "Z") ""

        testCase "ruleObjects checks each rule"
        <| fun () ->
            Expect.isError (BpaRuleEdit.ruleObjects """{"ID":"X"}""") "no scope or expression"
            Expect.isError (BpaRuleEdit.ruleObjects "3") ""

            match BpaRuleEdit.ruleObjects """[{"ID":"X","Scope":"Measure","Expression":"true"}]""" with
            | Ok [ _, r ] -> Expect.equal r.Scopes [ Scope.Measure ] ""
            | other -> failtestf "%A" other
    ]
