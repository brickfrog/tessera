/// The server runner, up to the seam: everything but the live connection, against the fake server.
module Tessera.Tests.ConformanceServerTests

open System
open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes
open Tessera.Tests.Conformance

let private model =
    """{ "tables": [ { "name": "T", "columns": [ { "name": "k", "type": "int64", "key": true }, { "name": "s", "type": "string" } ],
                       "calculatedColumns": [ { "name": "k2", "type": "int64", "expression": "T[k] * 2" } ],
                       "rows": [ [1, "a"], [2, null] ] },
                     { "name": "F", "columns": [ { "name": "k", "type": "int64" }, { "name": "amount", "type": "decimal" } ],
                       "rows": [ [1, 10.5], [1, 2.0] ] } ],
         "relationships": [ { "from": "F[k]", "to": "T[k]" } ] }"""

let private case (query: string) (expected: string) (extra: string) =
    $$"""{ "title": "{{query}}", "model": "m", "query": "{{query}}", "expected": {{expected}},
           "source": { "url": "https://example.org/dax", "supports": "says so" },
           "confidence": "derived", "derivation": "by hand", "tags": [ "x" ] {{extra}} }"""

let private one (value: string) =
    $$"""[ { "columns": [ { "name": "[Value]", "type": "int64" } ], "rows": [ [{{value}}] ] } ]"""

let private files = [
    "a/right.json", case "EVALUATE { 1 }" (one "1") ""
    "a/wrong.json", case "EVALUATE { 2 }" (one "3") """, "uncertain": true, "alternative": "it may be 2" """
    "a/type.json", case "EVALUATE { 4 }" (one "4") ""
    "a/error-expected.json", case "EVALUATE { 5 }" """{ "error": "must fail" }""" ""
    "a/error-unexpected.json", case "EVALUATE { 6 }" (one "6") ""
    "b/other-model.json",
    (case "EVALUATE { 7 }" (one "7") "")
        .Replace("\"model\": \"m\"", """"model": { "base": "m", "rows": { "F": [ ] } }""")
]

let private corpus () =
    let t = new TempDir()
    write (t.Combine("models", "m.json")) model

    for path, text in files do
        write (t.Combine(path.Split '/')) text

    let c = load t.Path
    Expect.isEmpty c.Problems "the test corpus loads"
    t, c.Cases

let private int64Result (value: int64) : QueryResult = {
    Columns = [
        {
            Name = "[Value]"
            Type = QueryType.Int64
        }
    ]
    Rows = [ [| QueryValue.Int64 value |] ]
    Truncated = false
}

/// The fake's answers: the tables as deployed, and something for each case's query.
let private answer (cases: Case list) (dax: string) : QueryResult list =
    let table (name: string) =
        let c = cases |> List.find (fun c -> c.ModelName = "m")

        match prepared c with
        | Ok p -> [
            ConformanceServer.tableAsResult p.Database.Model.Tables.[name] (rawRows p name)
          ]
        | Error e -> failwith e

    match dax with
    | "EVALUATE 'T'" ->
        // As a server would: the calculated column comes back too.
        let r = (table "T").Head

        [
            {
                r with
                    Columns =
                        r.Columns
                        @ [
                            {
                                Name = "T[k2]"
                                Type = QueryType.Int64
                            }
                        ]
                    Rows = r.Rows |> List.map (fun row -> Array.append row [| QueryValue.Blank |])
            }
        ]
    | "EVALUATE 'F'" -> table "F"
    | "EVALUATE { 1 }" -> [ int64Result 1L ]
    | "EVALUATE { 2 }" -> [ int64Result 2L ]
    | "EVALUATE { 4 }" -> [
        {
            Columns = [
                {
                    Name = "[Value]"
                    Type = QueryType.Double
                }
            ]
            Rows = [ [| QueryValue.Double 4.0 |] ]
            Truncated = false
        }
      ]
    | "EVALUATE { 5 }" -> raise (XmlaQueryError "the query failed, as it should")
    | "EVALUATE { 6 }" -> raise (XmlaQueryError $"no such thing; token {fakeToken}")
    | "EVALUATE { 7 }" -> [ int64Result 7L ]
    | other -> failwithf "unexpected query %s" other

let private access (server: FakeServer) : Remote.Access = {
    Connector = server
    Environment =
        Credentials.processEnvironment (fun name -> tokenEnv |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd)
    Notify = ignore
    IsWindows = false
}

let private options: ConformanceServer.Options = {
    Server = workspace
    Filter = None
    Drop = false
    Compare = CompareOptions.defaults
}

let private runOn (server: FakeServer) (options: ConformanceServer.Options) (cases: Case list) =
    match ConformanceServer.run (access server) options cases ignore with
    | Ok report -> report
    | Error e -> failtest (TessError.message e)

let private verdicts (report: ConformanceServer.Report) =
    report.Models
    |> List.collect _.Cases
    |> List.map (fun r -> r.Case.Id, ConformanceServer.verdict r)

[<Tests>]
let tests =
    testList "conformance (server runner, fake server)" [
        testCase "each model is embedded, deployed to its scratch database, refreshed, then queried"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases
            let report = runOn server options cases

            Expect.equal report.Models.Length 2 "m, and the case that changes it"
            let database = "tessera-conformance-m"
            Expect.equal report.Models.Head.Database database ""
            Expect.stringStarts report.Models.[1].Database "tessera-conformance-m-" ""

            // The scripts, in order: createOrReplace then a full refresh, per model.
            let kinds =
                server.Executed
                |> List.map (fun script ->
                    use doc = JsonDocument.Parse script
                    let root = doc.RootElement

                    match root.EnumerateObject() |> Seq.head with
                    | p when p.Name = "createOrReplace" ->
                        "createOrReplace", p.Value.GetProperty("object").GetProperty("database").GetString()
                    | p when p.Name = "refresh" ->
                        Expect.equal (p.Value.GetProperty("type").GetString()) "full" ""
                        "refresh", p.Value.GetProperty("objects").[0].GetProperty("database").GetString()
                    | p -> p.Name, "")

            Expect.equal
                kinds
                [
                    "createOrReplace", database
                    "refresh", database
                    "createOrReplace", report.Models.[1].Database
                    "refresh", report.Models.[1].Database
                ]
                ""

            // What was deployed needs no data source: every table is a calculated table with its rows.
            let deployed = server.Find(database).Value
            Expect.equal deployed.Model.DataSources.Count 0 ""

            for table in deployed.Model.Tables do
                Expect.equal table.Partitions.[0].SourceType PartitionSourceType.Calculated table.Name

            let rows =
                EmbedTests.readTable
                    (deployed.Model.Tables.["F"].Partitions.[0].Source :?> CalculatedPartitionSource).Expression

            Expect.equal (snd rows).Length 2 "F's rows are in the model"
            Expect.equal deployed.Model.Relationships.Count 1 ""
            Expect.isTrue (deployed.Model.Tables.["T"].Columns.ContainsName "k2") "the calculated column is kept"

            // Queries go to the model's own database: the tables first, then the cases.
            let onFirst =
                server.Queries
                |> List.filter (fun (db, _, _) -> db = database)
                |> List.map (fun (_, dax, _) -> dax)

            Expect.equal
                onFirst
                [
                    "EVALUATE 'T'"
                    "EVALUATE 'F'"
                    "EVALUATE { 5 }"
                    "EVALUATE { 6 }"
                    "EVALUATE { 1 }"
                    "EVALUATE { 4 }"
                    "EVALUATE { 2 }"
                ]
                ""

            Expect.isTrue (server.Queries |> List.forall (fun (_, _, limit) -> limit.IsNone)) "no row limit"
            Expect.equal server.Closed server.Requests.Length "every connection is closed"

        testCase "each case gets a verdict; an expected error agrees, an unexpected one doesn't"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases
            let report = runOn server options cases

            Expect.equal
                (verdicts report |> List.sort)
                ([
                    "a/right", ConformanceServer.Verdict.Agrees
                    "a/wrong", ConformanceServer.Verdict.Disagrees
                    "a/type", ConformanceServer.Verdict.TypeOnly
                    "a/error-expected", ConformanceServer.Verdict.Agrees
                    "a/error-unexpected", ConformanceServer.Verdict.ServerError
                    "b/other-model", ConformanceServer.Verdict.Agrees
                 ]
                 |> List.sort)
                ""

            Expect.isFalse (ConformanceServer.agrees report) ""
            Expect.isEmpty report.Models.Head.DataProblems "the deployed tables hold the fixture's rows"

            let unexpected =
                report.Models.Head.Cases
                |> List.find (fun r -> r.Case.Id = "a/error-unexpected")

            match unexpected.Outcome with
            | Outcome.Error message ->
                Expect.stringContains message "no such thing" ""
                Expect.isFalse (message.Contains fakeToken) "a credential in a server message is scrubbed"
            | other -> failtestf "%A" other

        testCase "the report lists every disagreement, in JSON and in words"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases
            let report = runOn server options cases
            use out = new TempDir()
            let jsonPath, markdownPath = ConformanceServer.write out.Path report
            use doc = JsonDocument.Parse(File.ReadAllText jsonPath)
            let root = doc.RootElement
            let summary = root.GetProperty "summary"
            Expect.equal (summary.GetProperty("cases").GetInt32()) 6 ""
            Expect.equal (summary.GetProperty("agrees").GetInt32()) 3 ""
            Expect.equal (summary.GetProperty("disagrees").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("type-only").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("server-error").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("not-run").GetInt32()) 0 ""

            let wrong =
                root.GetProperty("cases").EnumerateArray()
                |> Seq.find (fun c -> c.GetProperty("id").GetString() = "a/wrong")

            Expect.equal (wrong.GetProperty("verdict").GetString()) "disagrees" ""

            Expect.equal
                (wrong.GetProperty("differences").[0].GetString())
                "row 0, [Value]: 2 (int64), expected 3 (int64)"
                ""

            Expect.equal
                (wrong.GetProperty("actual").[0].GetProperty("rows").[0].[0].GetInt32())
                2
                "what the server said"

            Expect.equal (wrong.GetProperty("expected").[0].GetProperty("rows").[0].[0].GetInt32()) 3 ""
            Expect.equal (wrong.GetProperty("alternative").GetString()) "it may be 2" ""
            Expect.isTrue (wrong.GetProperty("uncertain").GetBoolean()) ""

            let right =
                root.GetProperty("cases").EnumerateArray()
                |> Seq.find (fun c -> c.GetProperty("id").GetString() = "a/right")

            Expect.isFalse (fst (right.TryGetProperty "actual")) "agreeing cases are one line"

            let markdown = File.ReadAllText markdownPath
            Expect.stringContains markdown "## The server disagrees" ""
            Expect.stringContains markdown "### a/wrong" ""
            Expect.stringContains markdown "- row 0, [Value]: 2 (int64), expected 3 (int64)" ""
            Expect.stringContains markdown "- the other reading: it may be 2" ""
            Expect.stringContains markdown "(derived, uncertain)" ""
            Expect.stringContains markdown "## Values agree, a type doesn't" ""
            Expect.stringContains markdown "### a/type" ""
            Expect.stringContains markdown "## The server failed the query" ""
            Expect.isFalse (markdown.Contains "### a/right") "agreeing cases are only counted"
            Expect.isFalse (markdown.Contains fakeToken) ""

        testCase "a server that agrees with everything gives a clean report"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases

            let report = runOn server { options with Filter = Some "right" } cases

            Expect.equal
                (verdicts report)
                [ "a/right", ConformanceServer.Verdict.Agrees ]
                "the filter picks cases by id"

            Expect.isTrue (ConformanceServer.agrees report) ""
            Expect.isFalse ((ConformanceServer.toMarkdown report).Contains "## The server") ""

        testCase "deployed rows that aren't the fixture's are reported before any case is believed"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()

            server.OnQuery <-
                fun dax ->
                    match answer cases dax with
                    | [ r ] when dax = "EVALUATE 'F'" -> [ { r with Rows = List.tail r.Rows } ]
                    | other -> other

            let report = runOn server { options with Filter = Some "a/right" } cases
            let problems = report.Models.Head.DataProblems
            Expect.equal (problems |> List.map fst) [ "F" ] ""
            Expect.exists (snd problems.Head) (fun d -> d.Contains "1 rows, expected 2") ""
            Expect.isFalse (ConformanceServer.agrees report) "even though the case agreed"
            Expect.stringContains (ConformanceServer.toMarkdown report) "`F` doesn't hold the fixture's rows" ""

        testCase "a deployment the server refuses: the model's cases are not run, the others are"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases

            server.ExecuteMessages <- [
                {
                    IsError = true
                    Text = "the capacity is paused"
                }
            ]

            let report = runOn server options cases

            for m in report.Models do
                Expect.stringContains m.Problem.Value "the capacity is paused" ""

            Expect.isTrue
                (verdicts report
                 |> List.forall (fun (_, v) -> v = ConformanceServer.Verdict.NotRun))
                ""

            Expect.isEmpty server.Queries "nothing was queried"
            Expect.equal server.Executed.Length 2 "one attempt per model, no refresh after a failed deployment"
            Expect.stringContains (ConformanceServer.toMarkdown report) "not deployed: " ""

        testCase "a server that can't be reached stops the run after the first model"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OpenError <- Some(exn "No such host is known")
            let report = runOn server options cases
            Expect.equal server.Requests.Length 1 "one attempt, not one per model"
            Expect.isTrue (report.Models |> List.forall (fun m -> m.Problem.IsSome)) ""

            Expect.isTrue
                (verdicts report
                 |> List.forall (fun (_, v) -> v = ConformanceServer.Verdict.NotRun))
                ""

        testCase "without credentials nothing is contacted"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()

            let noCredentials = {
                access server with
                    Environment = Credentials.processEnvironment (fun _ -> None)
            }

            match ConformanceServer.run noCredentials options cases ignore with
            | Error e -> failtest (TessError.message e)
            | Ok report ->
                Expect.isEmpty server.Requests ""
                Expect.stringContains report.Models.Head.Problem.Value "TESS_XMLA_TOKEN" "says what to set"

        testCase "an existing scratch database is replaced, with this run's rows"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases
            runOn server { options with Filter = Some "a/right" } cases |> ignore
            // Last run's database is there; change it, as stale rows would be.
            let stale = server.Find("tessera-conformance-m").Value
            (stale.Model.Tables.["F"].Partitions.[0].Source :?> CalculatedPartitionSource).Expression <- "{ 0 }"
            runOn server { options with Filter = Some "a/right" } cases |> ignore
            let now = server.Find("tessera-conformance-m").Value

            Expect.stringContains
                (now.Model.Tables.["F"].Partitions.[0].Source :?> CalculatedPartitionSource).Expression
                "DATATABLE"
                ""

            Expect.equal server.Databases.Count 1 ""

        testCase "--drop deletes each scratch database after its cases"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()
            server.OnQuery <- answer cases

            runOn
                server
                {
                    options with
                        Filter = Some "a/right"
                        Drop = true
                }
                cases
            |> ignore

            let last = List.last server.Executed
            use doc = JsonDocument.Parse last

            Expect.equal
                (doc.RootElement.GetProperty("delete").GetProperty("object").GetProperty("database").GetString())
                "tessera-conformance-m"
                ""

        testCase "only tessera-conformance- databases are ever named in a script"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t
            let server = FakeServer()

            let precious =
                new Database(Name = "Finance", ID = "Finance", CompatibilityLevel = 1702)

            precious.Model <- new Model(Name = "Model")
            server.Add precious
            server.OnQuery <- answer cases
            runOn server { options with Drop = true } cases |> ignore
            Expect.isSome (server.Find "Finance") ""

            for script in server.Executed do
                Expect.isFalse (script.Contains "Finance") script
                Expect.stringContains script ConformanceServer.DatabasePrefix ""

            Expect.stringStarts (databaseName cases.Head) ConformanceServer.DatabasePrefix ""

        testCase "an address that is no server is an error before anything else"
        <| fun () ->
            let t, cases = corpus ()
            use _ = t

            Expect.isError (ConformanceServer.run (access (FakeServer())) { options with Server = "" } cases ignore) ""

        testCase "the command does nothing without TESSERA_XMLA_SERVER, and rejects unknown arguments"
        <| fun () ->
            let server = FakeServer()
            use out = new StringWriter()
            use err = new StringWriter()
            Expect.equal (ConformanceServer.main server (fun _ -> None) out err []) 0 ""
            Expect.stringContains (out.ToString()) "TESSERA_XMLA_SERVER is not set" ""
            Expect.isEmpty server.Requests ""
            Expect.equal (ConformanceServer.main server (fun _ -> None) out err [ "--nope" ]) 2 ""
            Expect.equal (ConformanceServer.main server (fun _ -> None) out err [ "--help" ]) 0 ""
            Expect.stringContains (out.ToString()) "REPLACES a database named tessera-conformance-<model>" ""

        testCase "the command runs the real corpus through the seam and writes the report"
        <| fun () ->
            match corpusDirectory with
            | None -> skiptest "tests/conformance was not found"
            | Some _ ->
                let server = FakeServer()
                // A server that answers BLANK + 5 with 5, and holds the scalar model's table.
                server.OnQuery <-
                    fun dax ->
                        if dax = "EVALUATE 'T'" then
                            [
                                {
                                    Columns = [
                                        {
                                            Name = "T[n]"
                                            Type = QueryType.Int64
                                        }
                                    ]
                                    Rows = [ for n in 1L .. 3L -> [| QueryValue.Int64 n |] ]
                                    Truncated = false
                                }
                            ]
                        else
                            [
                                {
                                    Columns = [ { Name = "[r]"; Type = QueryType.Int64 } ]
                                    Rows = [ [| QueryValue.Int64 5L |] ]
                                    Truncated = false
                                }
                            ]

                let env (name: string) =
                    if name = "TESSERA_XMLA_SERVER" then
                        Some workspace
                    else
                        tokenEnv |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd

                use dir = new TempDir()
                use out = new StringWriter()
                use err = new StringWriter()

                let code =
                    ConformanceServer.main server env out err [ "--out"; dir.Path; "--filter"; "blank/add-number" ]

                Expect.equal code 0 (out.ToString() + err.ToString())
                Expect.stringContains (out.ToString()) "1 agree, 0 disagree" ""
                Expect.isTrue (File.Exists(dir.Combine "conformance-report.json")) ""
                Expect.isTrue (File.Exists(dir.Combine "conformance-report.md")) ""
                Expect.equal (server.Databases |> Seq.map _.Name |> List.ofSeq) [ "tessera-conformance-scalar" ] ""
    ]
