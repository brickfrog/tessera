module Tessera.Tests.SarifTests

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli
open Tessera.Cli.Common

/// The vendored OASIS schema (embedded in the test assembly; see sarif/NOTICE.txt).
let private schemaText =
    lazy
        (use s =
            Assembly.GetExecutingAssembly().GetManifestResourceStream "sarif-schema-2.1.0.json"

         use r = new StreamReader(s)
         r.ReadToEnd())

let private schema = lazy (JsonDocument.Parse schemaText.Value)

let private problems (sarif: string) =
    use doc = JsonDocument.Parse sarif
    JsonSchema.validate schema.Value.RootElement doc.RootElement

let private expectValid (sarif: string) =
    match problems sarif with
    | [] -> ()
    | ps -> failtestf "not valid SARIF 2.1.0:\n%s" (String.concat "\n" (List.truncate 20 ps))

/// A repository (a folder with .git) holding a TMDL model under "My Model/definition": the sample
/// model plus a table whose name has a space, with a broken measure and one that lints.
let private repo () =
    let t = new TempDir()
    Directory.CreateDirectory(t.Combine ".git") |> ignore
    let db = sampleDatabase ()
    let targets = new Table(Name = "Sales Targets")
    targets.Columns.Add(new DataColumn(Name = "Target", DataType = DataType.Double, SourceColumn = "Target"))

    targets.Partitions.Add(
        new Partition(Name = "Sales Targets", Source = new MPartitionSource(Expression = "let x = 1 in x"))
    )

    targets.Measures.Add(new Measure(Name = "Broken", Expression = "SUM('Nope'[Amount])"))
    targets.Measures.Add(new Measure(Name = "Loose", Expression = "SUM([Target])"))
    db.Model.Tables.Add targets
    let dir = t.Combine("My Model", "definition")
    TmdlSerializer.SerializeDatabaseToFolder(db, dir)
    t, dir

let private results (sarif: string) =
    use doc = JsonDocument.Parse sarif
    let run = doc.RootElement.GetProperty("runs").[0]

    [
        for r in run.GetProperty("results").EnumerateArray() ->
            let loc = r.GetProperty("locations").[0]

            let physical =
                match loc.TryGetProperty "physicalLocation" with
                | true, p ->
                    Some(
                        p.GetProperty("artifactLocation").GetProperty("uri").GetString(),
                        p.GetProperty("region").GetProperty("startLine").GetInt32()
                    )
                | _ -> None

            {|
                RuleId = r.GetProperty("ruleId").GetString()
                Level = r.GetProperty("level").GetString()
                Path = loc.GetProperty("logicalLocations").[0].GetProperty("fullyQualifiedName").GetString()
                Physical = physical
                Exact = r.GetProperty("properties").GetProperty("exactLocation").GetBoolean()
                Suppressed = r.TryGetProperty("suppressions") |> fst
            |}
    ]

let private ruleIds (sarif: string) =
    use doc = JsonDocument.Parse sarif

    [
        for r in
            doc.RootElement
                .GetProperty("runs")
                .[0].GetProperty("tool")
                .GetProperty("driver")
                .GetProperty("rules")
                .EnumerateArray() -> r.GetProperty("id").GetString()
    ]

/// The line a result points at, read from the file under the repository root.
let private lineAt (root: string) (uri: string, line: int) =
    let file =
        Path.Combine(Array.append [| root |] (uri.Split('/') |> Array.map Uri.UnescapeDataString))

    (File.ReadAllLines file).[line - 1]

let private sha256 (s: string) =
    Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes s)).ToLowerInvariant()

[<Tests>]
let tests =
    testList "SARIF" [
        testCase "the vendored schema is OASIS's file, unmodified"
        <| fun () ->
            Expect.equal
                (sha256 (schemaText.Value.Replace("\r\n", "\n")))
                "c3b4bb2d6093897483348925aaa73af03b3e3f4bd4ca38cef26dcb4212a2682e"
                "sarif/sarif-schema-2.1.0.json differs from the published schema"

        testCase "the schema check rejects what the schema forbids"
        <| fun () ->
            let ok =
                """{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"x"}},"results":[{"message":{"text":"m"},"level":"error"}]}]}"""

            Expect.isEmpty (problems ok) "a minimal log is valid"

            let bad (why: string) (doc: string) = Expect.isNonEmpty (problems doc) why

            bad
                "a level that isn't one"
                """{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"x"}},"results":[{"message":{"text":"m"},"level":"fatal"}]}]}"""

            bad
                "a property the schema doesn't have"
                """{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"x","colour":"red"}}}]}"""

            bad
                "a result with no message"
                """{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"x"}},"results":[{"level":"error"}]}]}"""

            bad "the wrong version" """{"version":"2.0.0","runs":[]}"""

            bad
                "a region with line 0"
                """{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"x"}},"results":[{"message":{"text":"m"},"locations":[{"physicalLocation":{"artifactLocation":{"uri":"a"},"region":{"startLine":0}}}]}]}]}"""

        testCase "validate --sarif: valid, every issue a result at its declaration, relative to the repository"
        <| fun () ->
            let t, dir = repo ()
            use _ = t
            let out = t.Combine "validate.sarif"
            let r = tess [ "validate"; dir; "--sarif"; out ]
            Expect.equal r.Code ExitCode.findings r.Err
            let sarif = File.ReadAllText out
            expectValid sarif
            let rs = results sarif

            let broken = rs |> List.find (fun x -> x.Path = "measure:Sales Targets/Broken")

            Expect.equal broken.RuleId "unresolved-reference" ""
            Expect.equal broken.Level "error" ""
            Expect.isTrue broken.Exact ""
            let uri, line = broken.Physical.Value
            Expect.equal uri "My%20Model/definition/tables/Sales%20Targets.tmdl" "relative, '/', percent-encoded"
            Expect.stringContains (lineAt t.Path (uri, line)) "measure Broken" "the line declares the measure"
            Expect.contains (ruleIds sarif) "unresolved-reference" "every validate rule is described"
            Expect.isFalse (sarif.Contains t.Path) "no absolute path to files under the root"

        testCase "lint --sarif on a .bim: the line of the object's name; rules carry their severity"
        <| fun () ->
            use t = new TempDir()
            Directory.CreateDirectory(t.Combine ".git") |> ignore
            let db = sampleDatabase ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Unqualified", Expression = "SUM([Amount])"))
            let path = t.Combine "model.bim"
            File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
            let out = t.Combine "lint.sarif"
            let r = tess [ "lint"; path; "--sarif"; out ]
            Expect.equal r.Code ExitCode.ok r.Err
            let sarif = File.ReadAllText out
            expectValid sarif
            let x = results sarif |> List.find (fun x -> x.RuleId = "DI004")
            Expect.equal x.Level "warning" ""
            Expect.equal (fst x.Physical.Value) "model.bim" ""
            Expect.stringContains (lineAt t.Path x.Physical.Value) "\"Unqualified\"" ""

            use doc = JsonDocument.Parse sarif

            let di004 =
                doc.RootElement
                    .GetProperty("runs")
                    .[0].GetProperty("tool")
                    .GetProperty("driver")
                    .GetProperty("rules")
                    .EnumerateArray()
                |> Seq.find (fun r -> r.GetProperty("id").GetString() = "DI004")

            Expect.equal (di004.GetProperty("defaultConfiguration").GetProperty("level").GetString()) "warning" ""
            Expect.isFalse (di004.TryGetProperty "helpUri" |> fst) "lint rules carry no URL, so none is made up"

        testCase "lint --sarif doesn't combine with --fix"
        <| fun () ->
            let t, dir = repo ()
            use _ = t
            let r = tess [ "lint"; dir; "--fix"; "--sarif"; t.Combine "x.sarif" ]
            Expect.equal r.Code ExitCode.usage r.Err
            Expect.isFalse (File.Exists(t.Combine "x.sarif")) ""

        testCase "bpa --sarif: helpUri only from a rule's LearnMoreUrl; ignored findings are suppressed"
        <| fun () ->
            let t, dir = repo ()
            use _ = t

            let rules =
                """[
  { "ID": "WITH_URL", "Name": "Measures are fine", "Category": "Demo", "Severity": 3,
    "Scope": "Measure", "Expression": "Name == \"Broken\"", "LearnMoreUrl": "https://example.org/rule" },
  { "ID": "NO_URL", "Name": "Tables are fine", "Category": "Demo", "Severity": 1, "Description": "Every table.",
    "Scope": "Table", "Expression": "true" }
]"""

            let rulesFile = t.Combine "rules.json"
            File.WriteAllText(rulesFile, rules)
            let out = t.Combine "bpa.sarif"

            let r =
                tess [ "bpa"; dir; "--no-builtin"; "--no-config"; "-r"; rulesFile; "--sarif"; out ]

            Expect.equal r.Code ExitCode.findings r.Err
            let sarif = File.ReadAllText out
            expectValid sarif
            use doc = JsonDocument.Parse sarif

            let rule id =
                doc.RootElement
                    .GetProperty("runs")
                    .[0].GetProperty("tool")
                    .GetProperty("driver")
                    .GetProperty("rules")
                    .EnumerateArray()
                |> Seq.find (fun r -> r.GetProperty("id").GetString() = id)

            Expect.equal ((rule "WITH_URL").GetProperty("helpUri").GetString()) "https://example.org/rule" ""
            Expect.isFalse ((rule "NO_URL").TryGetProperty "helpUri" |> fst) ""

            Expect.equal
                ((rule "NO_URL").GetProperty("fullDescription").GetProperty("text").GetString())
                "Every table."
                ""

            Expect.equal
                ((rule "NO_URL").GetProperty("defaultConfiguration").GetProperty("level").GetString())
                "note"
                ""

            let rs = results sarif
            let table = rs |> List.find (fun x -> x.Path = "table:Sales Targets")
            Expect.equal table.Level "note" "BPA severity 1 is a note"
            Expect.stringContains (lineAt t.Path table.Physical.Value) "table 'Sales Targets'" ""

        testCase "bpa --sarif: a finding ignored by annotation is a suppressed result"
        <| fun () ->
            use t = new TempDir()
            Directory.CreateDirectory(t.Combine ".git") |> ignore
            let db = sampleDatabase ()

            db.Model.Tables.["Date"].Annotations.Add(
                new Annotation(Name = "BestPracticeAnalyzer_IgnoreRules", Value = """{"RuleIDs":["ALL_TABLES"]}""")
            )

            let path = t.Combine "model.bim"
            File.WriteAllText(path, JsonSerializer.SerializeDatabase db)

            File.WriteAllText(
                t.Combine "rules.json",
                """[{ "ID": "ALL_TABLES", "Name": "x", "Severity": 2, "Scope": "Table", "Expression": "true" }]"""
            )

            let out = t.Combine "bpa.sarif"

            tess [
                "bpa"
                path
                "--no-builtin"
                "--no-config"
                "-r"
                t.Combine "rules.json"
                "--sarif"
                out
            ]
            |> ignore

            let sarif = File.ReadAllText out
            expectValid sarif
            let rs = results sarif
            Expect.isTrue (rs |> List.find (fun x -> x.Path = "table:Date")).Suppressed ""
            Expect.isFalse (rs |> List.find (fun x -> x.Path = "table:Sales")).Suppressed ""

        testCase "a clean run still writes a log, with no results; and the same input gives the same bytes"
        <| fun () ->
            let t, dir = repo ()
            use _ = t
            let a = t.Combine "a.sarif"
            let b = t.Combine "b.sarif"
            let r = tess [ "validate"; dir; "--rule"; "relationship-self-join"; "--sarif"; a ]
            Expect.equal r.Code ExitCode.ok r.Err
            expectValid (File.ReadAllText a)
            Expect.isEmpty (results (File.ReadAllText a)) ""
            Expect.equal (ruleIds (File.ReadAllText a)) [ "relationship-self-join" ] "only the rules that ran"

            tess [ "validate"; dir; "--sarif"; a ] |> ignore
            tess [ "validate"; dir; "--sarif"; b ] |> ignore
            Expect.equal (File.ReadAllBytes a) (File.ReadAllBytes b) "deterministic"
            Expect.isFalse ((File.ReadAllText a).Contains "\r") "LF line endings on every platform"

        testCase "outside a repository, a model outside the current folder gets absolute file URIs"
        <| fun () ->
            use t = new TempDir()
            let path = t.Combine "model.bim"
            let db = sampleDatabase ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Bad", Expression = "'Nope'[X]"))
            File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
            let out = t.Combine "v.sarif"
            tess [ "validate"; path; "--sarif"; out ] |> ignore
            let sarif = File.ReadAllText out
            expectValid sarif
            let uri = fst (results sarif |> List.head).Physical.Value
            Expect.stringStarts uri "file:///" ""

        testCase "a file that can't be written is an output error"
        <| fun () ->
            let t, dir = repo ()
            use _ = t

            let r =
                tess [ "validate"; dir; "--sarif"; t.Combine("no", "such", "dir", "x.sarif") ]

            Expect.equal r.Code ExitCode.output r.Err

        testCase "data check --sarif: an issue in a fixture file points at its line there"
        <| fun () ->
            use t = new TempDir()
            Directory.CreateDirectory(t.Combine ".git") |> ignore
            let model = t.Combine "model.bim"
            File.WriteAllText(model, JsonSerializer.SerializeDatabase(FixtureTests.model ()))
            let dir = t.Combine "data"
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Product.csv"), "ProductKey,Name\n1,Bike\nx,Bell\n")
            let out = t.Combine "data.sarif"
            let r = tess [ "data"; "check"; model; dir; "--sarif"; out ]
            Expect.equal r.Code ExitCode.findings r.Err
            let sarif = File.ReadAllText out
            expectValid sarif
            let bad = results sarif |> List.find (fun x -> x.RuleId = "invalid-value")
            Expect.equal bad.Physical (Some("data/Product.csv", 3)) "the CSV record's line"
            Expect.isTrue bad.Exact ""

        testCase "source locations: an object's own declaration, else its parent's, else the model file"
        <| fun () ->
            let t, dir = repo ()
            use _ = t

            let index =
                SourceLocations.build {
                    Format = ModelFormat.Tmdl
                    Path = dir
                    SemanticModelFolder = None
                }

            let measure = ObjectRef.inTable ObjectKind.Measure "Sales Targets" "broken"

            match SourceLocations.locate index measure with
            | Some(loc, true) ->
                Expect.equal (Path.GetFileName loc.File) "Sales Targets.tmdl" "case-insensitive, like Tabular"
            | other -> failtestf "expected the measure's own line, got %A" other

            match SourceLocations.locate index (ObjectRef.inTable ObjectKind.Measure "Sales Targets" "Nothing") with
            | Some(loc, false) -> Expect.stringContains (File.ReadAllLines(loc.File).[loc.Line.Value - 1]) "table" ""
            | other -> failtestf "expected the table's line, got %A" other

            match SourceLocations.locate index (ObjectRef.table "Nothing") with
            | Some(loc, false) -> Expect.equal (Path.GetFileName loc.File) "model.tmdl" ""
            | other -> failtestf "expected the model file, got %A" other

        testCase "source locations: a name declared twice is not placed, and a remote model has none"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "m"
            write (Path.Combine(dir, "model.tmdl")) "model Model\n"
            write (Path.Combine(dir, "tables", "A.tmdl")) "table A\n\tmeasure X = 1\n\tmeasure x = 2\n\tmeasure Y = 3\n"

            let index =
                SourceLocations.build {
                    Format = ModelFormat.Tmdl
                    Path = dir
                    SemanticModelFolder = None
                }

            Expect.isNone (SourceLocations.tryFind index (ObjectRef.inTable ObjectKind.Measure "A" "X")) ""

            Expect.equal
                ((SourceLocations.tryFind index (ObjectRef.inTable ObjectKind.Measure "A" "Y")).Value.Line)
                (Some 4)
                ""

            let remote =
                SourceLocations.build {
                    Format = ModelFormat.Xmla
                    Path = "xmla:server;db"
                    SemanticModelFolder = None
                }

            Expect.isNone remote.MainFile ""
            Expect.isEmpty remote.Declarations ""

        testCase "source locations: keyword-like lines inside expressions and properties are not declarations"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "m"
            write (Path.Combine(dir, "model.tmdl")) "model Model\n"

            write
                (Path.Combine(dir, "tables", "A.tmdl"))
                (String.concat "\n" [
                    "table A"
                    "\tmeasure M ="
                    "\t\t\tVAR x = 1"
                    "\t\t\tmeasure Fake = 2"
                    "\t\tformatString: 0"
                    "\tcalculationGroup"
                    "\t\tcalculationItem 'It''s' = SELECTEDMEASURE()"
                    "\tpartition A = m"
                    "\t\tsource ="
                    "\t\t\tcolumn Fake"
                    ""
                ])

            let index =
                SourceLocations.build {
                    Format = ModelFormat.Tmdl
                    Path = dir
                    SemanticModelFolder = None
                }

            let find kind name =
                SourceLocations.tryFind index (ObjectRef.inTable kind "A" name)
                |> Option.bind _.Line

            Expect.equal (find ObjectKind.Measure "M") (Some 2) ""
            Expect.isNone (find ObjectKind.Measure "Fake") ""
            Expect.isNone (find ObjectKind.Column "Fake") ""
            Expect.equal (find ObjectKind.CalculationItem "It's") (Some 7) "quoted, with '' for a quote"
            Expect.equal (find ObjectKind.Partition "A") (Some 8) ""

        testCase "sample model: every exact location holds the object's keyword and name"
        <| fun () ->
            match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
            | null
            | "" -> skiptest "TESSERA_SAMPLE_MODEL not set"
            | sample ->
                use t = new TempDir()

                for command in [ "validate"; "bpa"; "lint" ] do
                    let out = t.Combine $"{command}.sarif"
                    tess [ command; sample; "--sarif"; out ] |> ignore
                    let sarif = File.ReadAllText out
                    expectValid sarif
                    let root = Sarif.rootFor sample

                    for x in results sarif do
                        match x.Physical with
                        | Some(uri, line) when x.Exact ->
                            let r = (ObjectRef.parse x.Path |> Result.toOption).Value
                            let text = (lineAt root (uri, line)).ToLowerInvariant()
                            let name = (ObjectRef.name r).ToLowerInvariant()

                            Expect.isTrue
                                (text.Contains name || text.Contains(name.Replace("'", "''")))
                                $"{x.Path} at {uri}:{line}: {text}"
                        | _ -> ()
    ]
