/// The CLI against a fake server: remote models as a source, `connect`, `query`, `deploy`.
module Tessera.Tests.XmlaCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes
open Tessera.Cli
open Tessera.Cli.Common
open Tessera.Tests.CliHarness

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

/// A server holding the sample model as "Sales" (and an empty "Other").
let private salesServer () =
    let server = FakeServer()
    let sales = sampleDatabase ()
    sales.Name <- "Sales"
    sales.ID <- "sales-id"
    server.Add sales
    let other = new Database(Name = "Other", ID = "other-id", CompatibilityLevel = 1702)
    other.Model <- new Model(Name = "Model")
    server.Add other
    server

let private remote = $"xmla:{workspace};Sales"
let private run (server: FakeServer) (args: string list) = tessOn server tokenEnv args

let private errorCode (r: Run) =
    use doc = json r
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

/// The sample model as files, in a temp folder.
let private localModel (edit: Database -> unit) =
    let t = new TempDir()
    let db = sampleDatabase ()
    edit db
    let path = Path.Combine(t.Path, "model.bim")

    ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false db path
    |> ok
    |> ignore

    t, path

[<Tests>]
let sourceTests =
    testList "CLI remote source" [
        testCase "info: the remote model, its address as the path"
        <| fun () ->
            let server = salesServer ()
            let r = run server [ "info"; $"xmla:{workspace};sales"; "--json" ]
            Expect.equal r.Code 0 r.Err
            use doc = json r
            let source = doc.RootElement.GetProperty "source"
            Expect.equal (source.GetProperty("format").GetString()) "xmla" ""
            Expect.equal (source.GetProperty("path").GetString()) remote "the database's exact name"
            Expect.equal (doc.RootElement.GetProperty("name").GetString()) "Sales" ""
            Expect.equal (doc.RootElement.GetProperty("counts").GetProperty("measure").GetInt32()) 2 ""
            Expect.equal server.Requests.Length 1 "one connection"
            Expect.equal server.Closed 1 "closed"

            let text = run server [ "info"; remote ]
            Expect.stringContains text.Out "xmla" ""
            Expect.stringContains text.Out remote ""

        testCase "every read-only command gives the same answer for the remote model as for its files"
        <| fun () ->
            let server = salesServer ()
            use t = new TempDir()
            let local = Path.Combine(t.Path, "Sales.bim")
            let db = sampleDatabase ()
            db.Name <- "Sales"
            db.ID <- "sales-id"

            ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false db local
            |> ok
            |> ignore

            let commands = [
                [ "list"; "-k"; "measure" ]
                [ "list"; "--json" ]
                [ "get"; "[Total]" ]
                [ "get"; "Sales"; "--json"; "--children" ]
                [ "get"; "[Total]"; "--properties" ]
                [ "find"; "SELECTEDMEASURE" ]
                [ "deps"; "[Total]" ]
                [ "deps"; "--unused" ]
                [ "validate" ]
                [ "bpa" ]
                [ "lint" ]
                [ "script"; "export" ]
            ]

            for command in commands do
                let insert (model: string) =
                    match command with
                    | "script" :: "export" :: rest -> "script" :: "export" :: model :: rest
                    | name :: rest -> name :: model :: rest
                    | [] -> []

                let before = server.Requests.Length
                let fromServer = run server (insert remote)
                let fromFiles = tess (insert local)
                let name = String.concat " " command
                Expect.equal fromServer.Code fromFiles.Code name

                if not (List.contains "--json" command && command.Head = "validate") then
                    // Outputs are identical apart from timings.
                    let strip (s: string) =
                        System.Text.RegularExpressions.Regex.Replace(s, "\"durationMs\": \\d+", "")

                    Expect.equal (strip fromServer.Out) (strip fromFiles.Out) name

                Expect.equal (server.Requests.Length - before) 1 $"{name}: one connection"

            Expect.equal server.Closed server.Requests.Length "every session closed"

        testCase "diff: remote against files, and remote against remote"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.["Total"].Expression <- "SUM(Sales[Amount]) + 0")

            use _ = t
            let r = run server [ "diff"; remote; local; "--name-only"; "--ignore"; "name,id" ]
            Expect.equal r.Code 1 r.Err
            Expect.equal r.Out "measure:Sales/Total\n" ""

            let same = run server [ "diff"; remote; remote ]
            Expect.equal same.Code 0 same.Out
            Expect.equal server.Requests.Length 3 ""

        testCase "convert: a remote model to files that load back identical"
        <| fun () ->
            let server = salesServer ()
            use t = new TempDir()
            let folder = Path.Combine(t.Path, "Sales.SemanticModel")
            let r = run server [ "convert"; remote; folder ]
            Expect.equal r.Code 0 r.Err
            Expect.stringContains r.Out $"Read  xmla  {remote}" ""
            let _, saved = ModelIO.loadPath folder |> ok
            Expect.isEmpty (Diff.compare Diff.Options.none (server.Find "Sales").Value saved) ""

        testCase "an editing command: the dry run and --save-to work, --save is refused without connecting"
        <| fun () ->
            let server = salesServer ()
            let dry = run server [ "rename"; remote; "[Total]"; "Total Sales" ]
            Expect.equal dry.Code 0 dry.Err
            Expect.stringContains dry.Out "Total Sales" ""
            Expect.isEmpty server.Executed ""

            let before = server.Requests.Length

            let save =
                run server [ "rename"; remote; "[Total]"; "Total Sales"; "--save"; "--json" ]

            Expect.equal save.Code ExitCode.input save.Out
            Expect.equal (errorCode save) "remote-source-unsupported" ""
            Expect.stringContains save.Out "tess deploy" ""
            Expect.equal server.Requests.Length before "refused before connecting"

            use t = new TempDir()
            let copy = Path.Combine(t.Path, "renamed.bim")

            let saveTo =
                run server [ "rename"; remote; "[Total]"; "Total Sales"; "--save-to"; copy ]

            Expect.equal saveTo.Code 0 saveTo.Err
            let _, renamed = ModelIO.loadPath copy |> ok
            Expect.isNotNull (renamed.Model.Tables.["Sales"].Measures.Find "Total Sales") ""

            Expect.isNotNull
                ((server.Find "Sales").Value.Model.Tables.["Sales"].Measures.Find "Total")
                "the server is untouched"

        testCase "commands that only work on files say so, without connecting"
        <| fun () ->
            let server = salesServer ()

            for args in
                [
                    [ "format"; remote; "--save" ]
                    [ "translations"; "cultures"; remote ]
                    [ "perspective"; "list"; remote; "Sales View" ]
                ] do
                let r = run server (args @ [ "--json" ])
                Expect.equal r.Code ExitCode.input (String.concat " " args)
                Expect.equal (errorCode r) "remote-source-unsupported" (String.concat " " args)

            Expect.isEmpty server.Requests ""

        testCase "a path is never taken for a server: no prefix, no connection"
        <| fun () ->
            // tess (not tessOn) fails the test if a connection is attempted.
            for path in
                [
                    "localhost:51234"
                    @"host\instance"
                    "powerbi://api.powerbi.com/v1.0/myorg/Ws"
                ] do
                let r = tess [ "info"; path; "--json" ]
                Expect.equal r.Code ExitCode.input path
                Expect.equal (errorCode r) "path-not-found" path

        testCase "failures: no credentials, unknown database, bad address, secrets on the command line"
        <| fun () ->
            let server = salesServer ()
            let none = tessOn server [] [ "info"; remote; "--json" ]
            Expect.equal none.Code ExitCode.remote none.Out
            Expect.equal (errorCode none) "authentication-required" ""
            Expect.isEmpty server.Requests "nothing was contacted"

            let missing = run server [ "info"; $"xmla:{workspace};Nope" ]
            Expect.equal missing.Code ExitCode.remote ""
            Expect.stringContains missing.Err "No database 'Nope'" ""
            Expect.stringContains missing.Err "  Other\n  Sales" "the ones that exist"

            let bad = run server [ "info"; "xmla:ftp://x/y;Db"; "--json" ]
            Expect.equal bad.Code ExitCode.usage ""
            Expect.equal (errorCode bad) "invalid-server-address" ""

            let secret =
                run server [ "info"; "xmla:Data Source=srv;Password=hunter2"; "--json" ]

            Expect.equal secret.Code ExitCode.usage ""
            Expect.equal (errorCode secret) "secret-on-command-line" ""
            Expect.isFalse (secret.Out.Contains "hunter2") ""

        testCase "a credential never reaches stdout or stderr, whatever the server says"
        <| fun () ->
            let server = salesServer ()
            server.OpenError <- Some(exn ($"401 Unauthorized: token {fakeToken} rejected; Password=" + fakeToken))

            for args in
                [
                    [ "info"; remote ]
                    [ "info"; remote; "--json" ]
                    [ "connect"; workspace ]
                    [ "query"; remote; "EVALUATE x" ]
                    [ "deploy"; remote.Replace("Sales", "Other"); workspace; "Sales"; "--json" ]
                ] do
                let r = run server args
                Expect.equal r.Code ExitCode.remote (String.concat " " args)
                Expect.isFalse ((r.Out + r.Err).Contains fakeToken) (r.Out + r.Err)
                Expect.isFalse ((r.Out + r.Err).Contains "eyJ") (r.Out + r.Err)
    ]

[<Tests>]
let connectTests =
    testList "CLI connect" [
        testCase "lists the databases and how it signed in (not with what)"
        <| fun () ->
            let server = salesServer ()
            let r = run server [ "connect"; workspace ]
            Expect.equal r.Code 0 r.Err
            Expect.stringContains r.Out workspace ""
            Expect.stringContains r.Out "access token" ""
            Expect.stringContains r.Out "Fake Analysis Services 16.0.1.2" ""
            Expect.stringContains r.Out "Other  1702\nSales  1702\n" ""
            Expect.isFalse (r.Out.Contains fakeToken) ""

            let j = run server [ "connect"; $"xmla:{workspace}"; "--json" ]
            use doc = json j
            let databases = doc.RootElement.GetProperty "databases"
            Expect.equal (databases.GetArrayLength()) 2 ""
            Expect.equal (databases.[1].GetProperty("source").GetString()) remote "ready to paste as a model argument"
            Expect.equal (doc.RootElement.GetProperty("authentication").GetString()) "access token" ""

        testCase "a database in the address must exist"
        <| fun () ->
            let server = salesServer ()
            Expect.equal (run server [ "connect"; $"{workspace};Sales" ]).Code 0 ""
            let r = run server [ "connect"; $"{workspace};Nope"; "--json" ]
            Expect.equal r.Code ExitCode.remote ""
            Expect.equal (errorCode r) "database-not-found" ""
    ]

let private queryResults: QueryResult list = [
    {
        Columns = [
            {
                Name = "Date[Year]"
                Type = QueryType.Int64
            }
            {
                Name = "[Total]"
                Type = QueryType.Decimal
            }
            {
                Name = "[Note]"
                Type = QueryType.String
            }
        ]
        Rows = [
            [|
                QueryValue.Int64 2023L
                QueryValue.Decimal 1234.5M
                QueryValue.String "a, b"
            |]
            [| QueryValue.Int64 2024L; QueryValue.Blank; QueryValue.String "" |]
            [| QueryValue.Blank; QueryValue.Decimal 10M; QueryValue.Blank |]
        ]
        Truncated = false
    }
    {
        Columns = [
            {
                Name = "[When]"
                Type = QueryType.DateTime
            }
        ]
        Rows = [ [| QueryValue.DateTime(System.DateTime(2024, 1, 31)) |] ]
        Truncated = false
    }
]

[<Tests>]
let queryTests =
    testSequenced
    <| testList "CLI query" [
        testCase "a table per EVALUATE"
        <| fun () ->
            let server = salesServer ()
            server.OnQuery <- fun _ -> queryResults
            let r = run server [ "query"; remote; "EVALUATE x EVALUATE y" ]
            Expect.equal r.Code 0 r.Err

            Expect.equal
                r.Out
                ("Date[Year]  [Total]  [Note]\n"
                 + "----------  -------  ------\n"
                 + "      2023   1234.5  a, b\n"
                 + "      2024\n"
                 + "               10.0\n"
                 + "\n"
                 + "[When]\n"
                 + "-------------------\n"
                 + "2024-01-31T00:00:00\n")
                ""

            Expect.equal r.Err "" ""
            Expect.equal server.Queries [ "Sales", "EVALUATE x EVALUATE y", Some QueryCommand.defaultMaxRows ] ""

        testCase "--json: the documented shape"
        <| fun () ->
            let server = salesServer ()
            server.OnQuery <- fun _ -> queryResults
            let r = run server [ "query"; $"xmla:{workspace};SALES"; "EVALUATE x"; "--json" ]
            use doc = json r
            let root = doc.RootElement

            Expect.equal
                [ for p in root.EnumerateObject() -> p.Name ]
                [ "command"; "source"; "results"; "durationMs" ]
                ""

            Expect.equal (root.GetProperty("source").GetString()) remote ""
            let first = root.GetProperty("results").[0]

            Expect.equal
                (first.GetProperty("rows").GetRawText().Replace(" ", "").Replace("\n", ""))
                """[[2023,1234.5,"a,b"],[2024,null,""],[null,10.0,null]]"""
                ""

            Expect.equal (first.GetProperty("columns").[1].GetProperty("type").GetString()) "decimal" ""
            Expect.equal (first.GetProperty("rowCount").GetInt32()) 3 ""

            Expect.equal
                (root.GetProperty("results").[1].GetProperty("rows").[0].[0].GetString())
                "2024-01-31T00:00:00"
                ""

        testCase "--csv, results separated by an empty line"
        <| fun () ->
            let server = salesServer ()
            server.OnQuery <- fun _ -> queryResults
            let r = run server [ "query"; remote; "EVALUATE x"; "--csv" ]

            Expect.equal
                r.Out
                "Date[Year],[Total],[Note]\n2023,1234.5,\"a, b\"\n2024,,\"\"\n,10.0,\n\n[When]\n2024-01-31T00:00:00\n"
                ""

        testCase "--max-rows cuts each result and says so; 0 means all"
        <| fun () ->
            let server = salesServer ()
            server.OnQuery <- fun _ -> queryResults
            let r = run server [ "query"; remote; "EVALUATE x"; "--max-rows"; "2"; "--json" ]
            use doc = json r
            let results = doc.RootElement.GetProperty "results"

            Expect.equal
                [
                    for x in results.EnumerateArray() ->
                        x.GetProperty("rowCount").GetInt32(), x.GetProperty("truncated").GetBoolean()
                ]
                [ 2, true; 1, false ]
                ""

            let text = run server [ "query"; remote; "EVALUATE x"; "--max-rows"; "2" ]
            Expect.stringContains text.Err "first 2 rows" ""

            run server [ "query"; remote; "EVALUATE x"; "--max-rows"; "0" ] |> ignore
            Expect.equal (server.Queries |> List.last) ("Sales", "EVALUATE x", None) ""

        testCase "the query from a file, and from standard input"
        <| fun () ->
            let server = salesServer ()
            use t = new TempDir()
            let file = Path.Combine(t.Path, "q.dax")
            File.WriteAllText(file, "EVALUATE\n  'Date'")
            Expect.equal (run server [ "query"; remote; "-f"; file ]).Code 0 ""
            Expect.equal (server.Queries |> List.last) ("Sales", "EVALUATE\n  'Date'", Some 1000) ""

            let piped =
                tessWithStdinOn server tokenEnv "EVALUATE piped" [ "query"; remote; "-" ]

            Expect.equal piped.Code 0 piped.Err
            Expect.equal (server.Queries |> List.last) ("Sales", "EVALUATE piped", Some 1000) ""

            let missing = run server [ "query"; remote; "-f"; Path.Combine(t.Path, "nope.dax") ]
            Expect.equal missing.Code ExitCode.input ""

        testCase "a query error is the server's message, exit 5"
        <| fun () ->
            let server = salesServer ()
            server.OnQuery <- fun _ -> raise (XmlaQueryError "Query (1, 10) Cannot find table 'Nope'.")
            let r = run server [ "query"; remote; "EVALUATE Nope"; "--json" ]
            Expect.equal r.Code ExitCode.remote ""
            use doc = json r
            let error = doc.RootElement.GetProperty "error"
            Expect.equal (error.GetProperty("code").GetString()) "query-failed" ""
            Expect.stringContains (error.GetProperty("message").GetString()) "Cannot find table 'Nope'" ""

        testCase "usage errors, without connecting"
        <| fun () ->
            use t = new TempDir()

            for args in
                [
                    [ "query" ]
                    [ "query"; remote ]
                    [ "query"; remote; "EVALUATE x"; "EVALUATE y" ]
                    [ "query"; remote; "   " ]
                    [ "query"; remote; "EVALUATE x"; "--max-rows"; "-1" ]
                    [ "query"; remote; "EVALUATE x"; "-f"; "q.dax" ]
                    [ "query"; remote; "EVALUATE x"; "--csv"; "--json" ]
                    [ "query"; t.Path; "EVALUATE x" ]
                ] do
                let r = tessWithEnv tokenEnv args
                Expect.equal r.Code ExitCode.usage (String.concat " " args)
    ]

[<Tests>]
let deployTests =
    testList "CLI deploy" [
        testCase "a dry run by default: the changes and the script, nothing executed"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.Add(
                        new Measure(Name = "Average", Expression = "AVERAGE(Sales[Amount])")
                    ))

            use _ = t
            let r = run server [ "deploy"; local; workspace; "sales" ]
            Expect.equal r.Code 0 r.Err
            Expect.stringContains r.Out $"replace the model of 'Sales' on {workspace}" "the database's exact name"
            Expect.stringContains r.Out "kept: partitions" ""
            Expect.stringContains r.Out "+ measure:Sales/Average" ""
            Expect.stringContains r.Out "TMSL:\n{\n  \"createOrReplace\"" ""

            Expect.stringContains
                r.Err
                "Dry run: nothing deployed. Add --execute to deploy. It also needs --overwrite"
                ""

            Expect.isEmpty server.Executed ""
            Expect.isNull ((server.Find "Sales").Value.Model.Tables.["Sales"].Measures.Find "Average") ""

            let stat = run server [ "deploy"; local; $"{workspace};Sales"; "--stat" ]
            Expect.stringContains stat.Out "measure:Sales/Average" "the database can be in the server argument"

            let tmsl = run server [ "deploy"; local; workspace; "Sales"; "--tmsl" ]
            use doc = JsonDocument.Parse tmsl.Out

            Expect.equal
                (doc.RootElement
                    .GetProperty("createOrReplace")
                    .GetProperty("object")
                    .GetProperty("database")
                    .GetString())
                "Sales"
                "only the script, pipeable"

            Expect.isEmpty server.Executed ""

        testCase "--json: the plan in one document"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.Add(
                        new Measure(Name = "Average", Expression = "AVERAGE(Sales[Amount])")
                    ))

            use _ = t
            let r = run server [ "deploy"; local; workspace; "Sales"; "--json" ]
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "deploy" ""
            Expect.equal (root.GetProperty("server").GetString()) workspace ""
            Expect.equal (root.GetProperty("database").GetString()) "Sales" ""
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            Expect.isFalse (root.GetProperty("creates").GetBoolean()) ""
            Expect.isFalse (root.GetProperty("deployed").GetBoolean()) ""
            Expect.equal (root.GetProperty("changes").GetArrayLength()) 1 ""
            Expect.isGreaterThan (root.GetProperty("kept").GetArrayLength()) 0 ""
            Expect.stringContains (root.GetProperty("tmsl").GetString()) "createOrReplace" ""
            Expect.equal (root.GetProperty("introducedErrors").GetArrayLength()) 0 ""

        testCase "--execute on an existing database needs --overwrite; then it deploys"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.Add(
                        new Measure(Name = "Average", Expression = "AVERAGE(Sales[Amount])")
                    ))

            use _ = t
            let refused = run server [ "deploy"; local; workspace; "Sales"; "--execute" ]
            Expect.equal refused.Code ExitCode.output refused.Err
            Expect.stringContains refused.Err "already exists" ""
            Expect.isEmpty server.Executed ""

            let refusedJson =
                run server [ "deploy"; local; workspace; "Sales"; "--execute"; "--json" ]

            Expect.equal refusedJson.Code ExitCode.output ""
            Expect.equal (errorCode refusedJson) "database-exists" ""

            server.ExecuteMessages <- [
                {
                    IsError = false
                    Text = "table Sales needs processing"
                }
            ]

            let done' =
                run server [ "deploy"; local; workspace; "Sales"; "--execute"; "--overwrite" ]

            Expect.equal done'.Code 0 done'.Err
            Expect.equal server.Executed.Length 1 ""
            Expect.stringContains done'.Err "Deployed 'Sales'" ""
            Expect.stringContains done'.Err "server: table Sales needs processing" ""
            Expect.isFalse (done'.Out.Contains "TMSL:") "the script isn't repeated after running it"
            let deployed = (server.Find "Sales").Value
            Expect.isNotNull (deployed.Model.Tables.["Sales"].Measures.Find "Average") ""
            Expect.equal deployed.ID "sales-id" "kept its ID"

            // Deploying again changes nothing.
            let again = run server [ "deploy"; local; workspace; "Sales" ]
            Expect.stringContains again.Out "No changes." again.Out

        testCase "a database that doesn't exist is created without --overwrite"
        <| fun () ->
            let server = salesServer ()
            let t, local = localModel ignore
            use _ = t
            let dry = run server [ "deploy"; local; workspace; "Brand New" ]
            Expect.stringContains dry.Out "create 'Brand New'" ""
            Expect.stringContains dry.Err "Add --execute to deploy.\n" "no other flag needed"

            let r =
                run server [ "deploy"; local; workspace; "Brand New"; "--execute"; "--json" ]

            Expect.equal r.Code 0 r.Out
            use doc = json r
            Expect.isTrue (doc.RootElement.GetProperty("creates").GetBoolean()) ""
            Expect.isTrue (doc.RootElement.GetProperty("deployed").GetBoolean()) ""
            Expect.isFalse (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.isSome (server.Find "Brand New") ""

        testCase "a model with new errors isn't deployed without --force"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Broken", Expression = "'Gone'[X]")))

            use _ = t
            let dry = run server [ "deploy"; local; workspace; "Sales" ]
            Expect.equal dry.Code 0 "a dry run reports, it doesn't fail"
            Expect.stringContains dry.Err "error: measure:Sales/Broken" ""
            Expect.stringContains dry.Err "--force" ""

            let refused =
                run server [ "deploy"; local; workspace; "Sales"; "--execute"; "--overwrite" ]

            Expect.equal refused.Code ExitCode.findings refused.Err
            Expect.stringContains refused.Err "Not deployed" ""
            Expect.isEmpty server.Executed ""

            let forced =
                run server [ "deploy"; local; workspace; "Sales"; "--execute"; "--overwrite"; "--force" ]

            Expect.equal forced.Code 0 forced.Err
            Expect.equal server.Executed.Length 1 ""

        testCase "the server rejecting the script: exit 5, its messages, nothing applied"
        <| fun () ->
            let server = salesServer ()

            let t, local =
                localModel (fun db ->
                    db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Average", Expression = "1")))

            use _ = t

            server.ExecuteMessages <- [
                {
                    IsError = true
                    Text = "The database is read-only."
                }
            ]

            let r =
                run server [ "deploy"; local; workspace; "Sales"; "--execute"; "--overwrite"; "--json" ]

            Expect.equal r.Code ExitCode.remote r.Out
            Expect.equal (errorCode r) "deploy-failed" ""
            Expect.stringContains r.Out "The database is read-only." ""
            Expect.isNull ((server.Find "Sales").Value.Model.Tables.["Sales"].Measures.Find "Average") ""

        testCase "--deploy-full replaces what the server would keep"
        <| fun () ->
            let server = salesServer ()

            (server.Find "Sales")
                .Value.Model.Roles.Add(new ModelRole(Name = "Server Only", ModelPermission = ModelPermission.Read))

            let t, local = localModel ignore
            use _ = t
            let kept = run server [ "deploy"; local; workspace; "Sales"; "--name-only" ]
            Expect.isFalse (kept.Out.Contains "role:Server Only") "kept by default"

            let full =
                run server [ "deploy"; local; workspace; "Sales"; "--deploy-full"; "--name-only" ]

            Expect.stringContains full.Out "role:Server Only" "removed when roles are deployed"
            Expect.isFalse (full.Out.Contains "kept:") ""

        testCase "remote to remote works; a database onto itself is refused"
        <| fun () ->
            let server = salesServer ()
            let copy = run server [ "deploy"; remote; workspace; "Copy"; "--execute" ]
            Expect.equal copy.Code 0 copy.Err
            Expect.isSome (server.Find "Copy") ""

            let self = run server [ "deploy"; remote; workspace + "/"; "SALES"; "--json" ]
            Expect.equal self.Code ExitCode.input self.Out
            Expect.equal (errorCode self) "remote-source-unsupported" ""

        testCase "usage errors, without connecting"
        <| fun () ->
            let t, local = localModel ignore
            use _ = t

            for args, code in
                [
                    [ "deploy" ], ExitCode.usage
                    [ "deploy"; local ], ExitCode.usage
                    [ "deploy"; local; workspace ], ExitCode.usage
                    [ "deploy"; local; workspace; "A"; "B" ], ExitCode.usage
                    [ "deploy"; local; $"{workspace};A"; "B" ], ExitCode.usage
                    [ "deploy"; local; workspace; "A"; "--stat"; "--name-only" ], ExitCode.usage
                    [ "deploy"; local; workspace; "A"; "--skip-refresh-policy" ], ExitCode.usage
                    [ "deploy"; local; "Data Source=x;Password=y"; "A" ], ExitCode.usage
                    [ "deploy"; Path.Combine(t.Path, "missing.bim"); workspace; "A" ], ExitCode.input
                ] do
                let r = tessWithEnv tokenEnv args
                Expect.equal r.Code code (String.concat " " args + "\n" + r.Err)
    ]
