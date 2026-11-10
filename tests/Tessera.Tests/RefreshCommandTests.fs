/// `tess refresh` against a fake server.
module Tessera.Tests.RefreshCommandTests

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes
open Tessera.Cli.Common
open Tessera.Tests.CliHarness

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

/// A server holding the sample model as "Sales", with a second partition and a refresh-policy table.
let private salesServer () =
    let server = FakeServer()
    let sales = sampleDatabase ()
    sales.Name <- "Sales"
    sales.ID <- "sales-id"

    sales.Model.Tables.["Sales"].Partitions.Add(
        new Partition(Name = "Sales 2024", Source = new MPartitionSource(Expression = "x"))
    )

    let orders = new Table(Name = "Orders")
    orders.Columns.Add(new DataColumn(Name = "Id", DataType = DataType.Int64, SourceColumn = "Id"))
    orders.Partitions.Add(new Partition(Name = "Orders-2023", Source = new MPartitionSource(Expression = "x")))

    orders.RefreshPolicy <-
        new BasicRefreshPolicy(
            RollingWindowGranularity = RefreshGranularityType.Year,
            RollingWindowPeriods = 3,
            IncrementalGranularity = RefreshGranularityType.Day,
            IncrementalPeriods = 7,
            SourceExpression = "x"
        )

    sales.Model.Tables.Add orders
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

let private sameJson (actual: string) (expected: string) =
    if not (JsonNode.DeepEquals(JsonNode.Parse actual, JsonNode.Parse expected)) then
        failtestf "expected:\n%s\nactual:\n%s" expected actual

let private event (eventClass: string) (subclass: string) (text: string) : XmlaTraceEvent = {
    EventClass = eventClass
    EventSubclass = subclass
    ObjectName = "Sales"
    ObjectPath = "Model.Sales"
    Text = text
    IntegerData = None
    DurationMs = None
}

let private trace = [
    event "CommandBegin" "" "the command"
    event "ProgressReportBegin" "ReadData" "Reading data for the 'Sales' table started."
    {
        event "ProgressReportEnd" "ReadData" "Finished reading data for the 'Sales' table." with
            IntegerData = Some 2500L
            DurationMs = Some 1500L
    }
    {
        event "ProgressReportEnd" "Process" "Finished processing partition 'Sales' of table 'Sales'." with
            DurationMs = Some 2000L
    }
]

[<Tests>]
let tests =
    testList "CLI refresh" [
        testCase "a dry run by default: what it covers and the script; nothing is run"
        <| fun () ->
            let server = salesServer ()
            let r = run server [ "refresh"; $"xmla:{workspace};sales" ]
            Expect.equal r.Code 0 r.Err

            Expect.stringContains r.Out $"Refresh (automatic) 'Sales' on {workspace}: the whole model" ""

            Expect.stringContains r.Out "  Sales: partitions Sales, Sales 2024\n" ""
            Expect.stringContains r.Out "  Date: 1 partition\n" ""
            Expect.stringContains r.Out "  Orders: partition Orders-2023 [refresh policy]\n" ""
            Expect.stringContains r.Out "TMSL:\n{" ""
            Expect.stringContains r.Out "\"database\": \"Sales\"" "the database's exact name"
            Expect.stringContains r.Err "warning: the incremental refresh policy of 'Orders' is applied" ""
            Expect.stringContains r.Err "Dry run: nothing refreshed. Add --execute to refresh." ""
            Expect.isEmpty server.Executed "nothing run"
            Expect.equal server.Traced 0 ""
            Expect.equal server.Requests.Length 1 "one connection"
            Expect.equal server.Closed 1 "closed"

        testCase "--tmsl prints only the script, which is the planned one"
        <| fun () ->
            let server = salesServer ()

            let r =
                run server [
                    "refresh"
                    remote
                    "Sales"
                    "partition:Date/Date"
                    "--type"
                    "dataonly"
                    "--max-parallelism"
                    "2"
                    "--tmsl"
                ]

            Expect.equal r.Code 0 r.Err

            sameJson
                r.Out
                """{ "sequence": { "maxParallelism": 2, "operations": [ { "refresh": { "type": "dataOnly",
                     "objects": [ { "database": "Sales", "table": "Sales" },
                                  { "database": "Sales", "table": "Date", "partition": "Date" } ] } } ] } }"""

            Expect.isEmpty server.Executed ""

        testCase "the refresh-policy options"
        <| fun () ->
            let server = salesServer ()

            let script args =
                (run server ([ "refresh"; remote; "Orders"; "--tmsl" ] @ args)).Out

            sameJson
                (script [ "-t"; "full"; "--no-apply-refresh-policy" ])
                """{ "refresh": { "type": "full", "applyRefreshPolicy": false,
                     "objects": [ { "database": "Sales", "table": "Orders" } ] } }"""

            sameJson
                (script [ "--apply-refresh-policy" ])
                """{ "refresh": { "type": "automatic", "applyRefreshPolicy": true,
                     "objects": [ { "database": "Sales", "table": "Orders" } ] } }"""

            sameJson
                (script [ "--effective-date"; "2024-06-30" ])
                """{ "refresh": { "type": "automatic", "applyRefreshPolicy": true, "effectiveDate": "06/30/2024",
                     "objects": [ { "database": "Sales", "table": "Orders" } ] } }"""

        testCase "--json: one document describing the dry run"
        <| fun () ->
            let server = salesServer ()

            let r =
                run server [
                    "refresh"
                    remote
                    "sales[sales 2024]"
                    "--json"
                    "Orders"
                    "--max-parallelism"
                    "3"
                ]

            Expect.equal r.Code 0 r.Err
            Expect.equal r.Err "" "nothing beside the document"
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "refresh" ""
            Expect.equal (root.GetProperty("source").GetString()) remote ""
            Expect.equal (root.GetProperty("server").GetString()) workspace ""
            Expect.equal (root.GetProperty("database").GetString()) "Sales" ""
            Expect.equal (root.GetProperty("type").GetString()) "automatic" ""
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            Expect.isFalse (root.GetProperty("refreshed").GetBoolean()) ""
            Expect.equal (root.GetProperty("applyRefreshPolicy").ValueKind) JsonValueKind.Null "not asked for"
            Expect.equal (root.GetProperty("effectiveDate").ValueKind) JsonValueKind.Null ""
            Expect.equal (root.GetProperty("maxParallelism").GetInt32()) 3 ""
            Expect.equal (root.GetProperty("durationMs").ValueKind) JsonValueKind.Null ""
            Expect.equal (root.GetProperty("traceError").ValueKind) JsonValueKind.Null ""
            Expect.equal (root.GetProperty("progress").GetArrayLength()) 0 ""

            Expect.equal
                [
                    for o in root.GetProperty("objects").EnumerateArray() -> o.GetProperty("ref").GetString()
                ]
                [ "partition:Sales/Sales 2024"; "table:Orders" ]
                ""

            let covers = root.GetProperty("covers")
            Expect.equal (covers.GetArrayLength()) 2 ""
            Expect.equal (covers.[0].GetProperty("table").GetString()) "Sales" ""
            Expect.isFalse (covers.[0].GetProperty("wholeTable").GetBoolean()) ""
            Expect.equal (covers.[0].GetProperty("partitions").[0].GetString()) "Sales 2024" ""
            Expect.isTrue (covers.[1].GetProperty("refreshPolicy").GetBoolean()) ""
            Expect.equal (root.GetProperty("warnings").GetArrayLength()) 1 ""

            sameJson
                (root.GetProperty("tmsl").GetString())
                """{ "sequence": { "maxParallelism": 3, "operations": [ { "refresh": { "type": "automatic",
                     "objects": [ { "database": "Sales", "table": "Sales", "partition": "Sales 2024" },
                                  { "database": "Sales", "table": "Orders" } ] } } ] } }"""

            Expect.isEmpty server.Executed ""

            let dated =
                run server [ "refresh"; remote; "Orders"; "--effective-date"; "2024-06-30"; "--json" ]

            use doc = json dated
            Expect.isTrue (doc.RootElement.GetProperty("applyRefreshPolicy").GetBoolean()) "implied"
            Expect.equal (doc.RootElement.GetProperty("effectiveDate").GetString()) "2024-06-30" "ISO in the document"

        testCase "--execute runs exactly the script the dry run shows, and reports progress on stderr"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            server.ExecuteMessages <- [ { IsError = false; Text = "a warning" } ]
            // (The harness gives stdout with LF; the script itself has the platform's newlines.)
            let lf (s: string) = s.Replace("\r\n", "\n")
            let shown = (run server [ "refresh"; remote; "Sales"; "--tmsl" ]).Out.TrimEnd()
            let r = run server [ "refresh"; remote; "Sales"; "--execute" ]
            Expect.equal r.Code 0 r.Err
            Expect.equal (server.Executed |> List.map lf) [ shown ] ""
            Expect.equal server.Traced 1 ""
            Expect.stringContains r.Out "Refresh (automatic) 'Sales'" ""
            Expect.isFalse (r.Out.Contains "TMSL:") "the script is for the dry run"

            Expect.stringContains
                r.Err
                ("Refreshing...\n"
                 + "  [ReadData] Finished reading data for the 'Sales' table. (2,500 rows, 1.5s)\n"
                 + "  [Process] Finished processing partition 'Sales' of table 'Sales'. (2.0s)\n"
                 + "server: a warning\n"
                 + $"Refreshed 'Sales' on {workspace} in ")
                ""

            Expect.isFalse (r.Err.Contains "Dry run") ""

        testCase "--execute --json: progress in the document, nothing on stderr"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let r = run server [ "refresh"; remote; "--execute"; "--json"; "--progress"; "all" ]
            Expect.equal r.Code 0 r.Out
            Expect.equal r.Err "" ""
            use doc = json r
            let root = doc.RootElement
            Expect.isFalse (root.GetProperty("dryRun").GetBoolean()) ""
            Expect.isTrue (root.GetProperty("refreshed").GetBoolean()) ""
            Expect.equal (root.GetProperty("durationMs").ValueKind) JsonValueKind.Number ""
            let progress = root.GetProperty("progress")
            Expect.equal (progress.GetArrayLength()) 3 "the three progress events, not the command's own"
            Expect.equal (progress.[0].GetProperty("phase").GetString()) "begin" ""
            Expect.equal (progress.[1].GetProperty("phase").GetString()) "end" ""
            Expect.equal (progress.[1].GetProperty("step").GetString()) "ReadData" ""
            Expect.equal (progress.[1].GetProperty("object").GetString()) "Sales" ""
            Expect.equal (progress.[1].GetProperty("objectPath").GetString()) "Model.Sales" ""
            Expect.equal (progress.[1].GetProperty("integerData").GetInt64()) 2500L ""
            Expect.equal (progress.[1].GetProperty("durationMs").GetInt64()) 1500L ""
            Expect.equal (progress.[0].GetProperty("integerData").ValueKind) JsonValueKind.Null ""
            Expect.equal server.Executed.Length 1 ""

        testCase "--progress: all shows every progress event, none doesn't trace"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let all = run server [ "refresh"; remote; "--execute"; "--progress"; "all" ]
            Expect.stringContains all.Err "  [ReadData begin] Reading data for the 'Sales' table started.\n" ""
            Expect.isFalse (all.Err.Contains "the command") "not the session's other events"

            let none = run server [ "refresh"; remote; "--execute"; "--progress"; "none" ]
            Expect.equal none.Code 0 none.Err
            Expect.equal server.Traced 1 "only the first run was traced"
            Expect.equal server.Executed.Length 2 ""
            Expect.isFalse (none.Err.Contains "[ReadData]") ""
            Expect.stringContains none.Err "Refreshed 'Sales'" ""

        testCase "the server's errors: exit 5 with its messages, and no secret"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace

            server.ExecuteMessages <- [
                {
                    IsError = true
                    Text = $"The credentials for the data source are wrong ({fakeToken})."
                }
            ]

            let r = run server [ "refresh"; remote; "Sales"; "--execute" ]
            Expect.equal r.Code ExitCode.remote r.Err
            Expect.stringContains r.Err $"tess: Refreshing 'Sales' on {workspace} failed:" ""
            Expect.stringContains r.Err "The credentials for the data source are wrong" ""
            Expect.stringContains r.Err "[ReadData]" "progress up to the failure"
            Expect.isFalse (r.Err.Contains "Refreshed") ""
            Expect.isFalse ((r.Out + r.Err).Contains fakeToken) ""

            let j = run server [ "refresh"; remote; "Sales"; "--execute"; "--json" ]
            Expect.equal j.Code ExitCode.remote j.Out
            Expect.equal (errorCode j) "refresh-failed" ""
            Expect.isFalse (j.Out.Contains fakeToken) ""
            use doc = json j
            Expect.isFalse (doc.RootElement.GetProperty("refreshed").GetBoolean()) ""
            Expect.isFalse (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (doc.RootElement.GetProperty("progress").GetArrayLength()) 2 "kept with the failure"

        testCase "the script being refused outright: exit 5"
        <| fun () ->
            let server = salesServer ()
            server.ExecuteError <- Some(exn "The remote server returned an error: (403) Forbidden.")
            let r = run server [ "refresh"; remote; "--execute"; "--json" ]
            Expect.equal r.Code ExitCode.remote r.Out
            Expect.equal (errorCode r) "refresh-failed" ""
            Expect.stringContains r.Out "403" ""

        testCase "a trace that doesn't work: the refresh still runs, and says why there is no progress"
        <| fun () ->
            let server = salesServer ()
            server.TraceFailure <- Some "the trace couldn't be started: no permission"
            let r = run server [ "refresh"; remote; "--execute" ]
            Expect.equal r.Code 0 r.Err
            Expect.equal server.Executed.Length 1 ""

            Expect.stringContains
                r.Err
                "warning: no progress could be reported: the trace couldn't be started: no permission"
                ""

            Expect.stringContains r.Err "Refreshed 'Sales'" ""
            let j = run server [ "refresh"; remote; "--execute"; "--json" ]
            use doc = json j
            Expect.stringContains (doc.RootElement.GetProperty("traceError").GetString()) "no permission" ""
            Expect.isTrue (doc.RootElement.GetProperty("refreshed").GetBoolean()) ""

        testCase "what can't be refreshed is refused before anything runs"
        <| fun () ->
            let server = salesServer ()

            for args, code, error in
                [
                    [ "[Total]" ], ExitCode.input, "cannot-refresh"
                    [ "Nope" ], ExitCode.input, "object-not-found"
                    [ "Sales"; "--type"; "add" ], ExitCode.input, "cannot-refresh"
                    [ "partition:Sales/Sales"; "--type"; "defragment" ], ExitCode.input, "cannot-refresh"
                    [ "partition:Sales/Sales"; "--no-apply-refresh-policy" ], ExitCode.input, "cannot-refresh"
                ] do
                let r = run server ([ "refresh"; remote; "--execute"; "--json" ] @ args)
                Expect.equal r.Code code (String.concat " " args + "\n" + r.Out)
                Expect.equal (errorCode r) error ""

            Expect.isEmpty server.Executed ""
            let text = run server [ "refresh"; remote; "[Total]" ]

            Expect.stringContains
                text.Err
                "tess: Can't refresh measure:Sales/Total: a measure isn't refreshed by itself"
                ""

        testCase "a database that isn't there, and one left out"
        <| fun () ->
            let server = salesServer ()

            let missing =
                run server [ "refresh"; $"xmla:{workspace};Nope"; "--execute"; "--json" ]

            Expect.equal missing.Code ExitCode.remote missing.Out
            Expect.equal (errorCode missing) "database-not-found" ""
            let unnamed = run server [ "refresh"; $"xmla:{workspace}"; "--json" ]
            Expect.equal (errorCode unnamed) "database-required" ""
            Expect.isEmpty server.Executed ""

        testCase "model files are refused, and usage errors found, without connecting"
        <| fun () ->
            use t = new TempDir()
            let local = Path.Combine(t.Path, "model.bim")

            ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false (sampleDatabase ()) local
            |> ok
            |> ignore

            // `tessWithEnv` fails the test if the command asks for a connection.
            let files = tessWithEnv tokenEnv [ "refresh"; local; "--execute" ]
            Expect.equal files.Code ExitCode.usage files.Err
            Expect.stringContains files.Err "is not a server model" ""
            Expect.stringContains files.Err "model files hold no data to refresh" ""

            for args in
                [
                    [ "refresh" ]
                    [ "refresh"; remote; "--type"; "indexes" ]
                    [ "refresh"; remote; "--type"; "process" ]
                    [ "refresh"; remote; "--effective-date"; "06/30/2024" ]
                    [ "refresh"; remote; "--effective-date"; "2024-13-01" ]
                    [
                        "refresh"
                        remote
                        "--effective-date"
                        "2024-06-30"
                        "--no-apply-refresh-policy"
                    ]
                    [ "refresh"; remote; "--apply-refresh-policy"; "--no-apply-refresh-policy" ]
                    [ "refresh"; remote; "--max-parallelism"; "0" ]
                    [ "refresh"; remote; "--max-parallelism"; "many" ]
                    [ "refresh"; remote; "--progress"; "verbose" ]
                    [ "refresh"; "xmla:Data Source=x;Password=y" ]
                ] do
                let r = tessWithEnv tokenEnv args
                Expect.equal r.Code ExitCode.usage (String.concat " " args + "\n" + r.Err)

            let json = tessWithEnv tokenEnv [ "refresh"; local; "--json" ]
            Expect.equal json.Code ExitCode.usage json.Out
            Expect.equal (errorCode json) "usage" ""

        testCase "without credentials even the dry run stops before connecting"
        <| fun () ->
            let r = tessWithEnv [] [ "refresh"; remote; "--json" ]
            Expect.equal r.Code ExitCode.remote r.Out
            Expect.equal (errorCode r) "authentication-required" ""

        testCase "it is in the help"
        <| fun () ->
            let top = tess [ "--help" ]
            Expect.stringContains top.Out "refresh" ""
            let help = tess [ "refresh"; "--help" ]
            Expect.equal help.Code 0 help.Err

            for flag in
                [
                    "--execute"
                    "--type"
                    "--max-parallelism"
                    "--apply-refresh-policy"
                    "--no-apply-refresh-policy"
                    "--effective-date"
                    "--progress"
                    "--tmsl"
                ] do
                Expect.stringContains help.Out flag flag
    ]
