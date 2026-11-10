/// Running a refresh through the seam (RemoteRefresh), against the fake server: failures, the
/// progress events kept at each level of detail, and a trace that doesn't work.
module Tessera.Tests.XmlaRefreshTests

open Expecto
open Tessera
open Tessera.Xmla
open Tessera.Xmla.RemoteRefresh
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private access (server: IXmlaConnector) : Remote.Access = {
    Connector = server
    Environment = {
        GetEnv = fun name -> Map.tryFind name (Map.ofList tokenEnv)
        ReadSecret = fun path -> Error $"no file {path}"
    }
    Notify = ignore
    IsWindows = false
}

let private salesServer () =
    let server = FakeServer()
    let db = sampleDatabase ()
    db.Name <- "Sales"
    server.Add db
    server

let private event (eventClass: string) (subclass: string) (name: string) (text: string) : XmlaTraceEvent = {
    EventClass = eventClass
    EventSubclass = subclass
    ObjectName = name
    ObjectPath = ""
    Text = text
    IntegerData = None
    DurationMs = None
}

/// Roughly what a server reports for one partition, with other session noise around it.
let private trace = [
    event "CommandBegin" "" "" "{ \"refresh\": {} }"
    event "ProgressReportBegin" "TabularRefresh" "Sales" "Started processing"
    event "ProgressReportBegin" "ReadData" "Sales" "Reading data for the 'Sales' table started."
    {
        event "ProgressReportCurrent" "ReadData" "Sales" "Reading data for the 'Sales' table." with
            IntegerData = Some 100000L
    }
    {
        event "ProgressReportEnd" "ExecuteSql" "Sales" "SELECT *\nFROM dbo.Sales" with
            DurationMs = Some 250L
    }
    {
        event "ProgressReportEnd" "ReadData" "Sales" "Finished reading data for the 'Sales' table." with
            IntegerData = Some 1234567L
            DurationMs = Some 12340L
    }
    {
        event "ProgressReportEnd" "CompressSegment" "Amount" "Compression for the 'Amount' column completed." with
            DurationMs = Some 3L
    }
    {
        event "ProgressReportEnd" "Process" "Sales" "Finished processing partition 'Sales' of table 'Sales'." with
            DurationMs = Some 13000L
    }
    event "CommandEnd" "" "" "{ \"refresh\": {} }"
]

let private runWith (server: FakeServer) (detail: Detail) (onProgress: XmlaTraceEvent -> unit) =
    Remote.withConnection (access server) (XmlaAddress.parseServer $"{workspace};Sales" |> ok) (fun c ->
        Ok(run c "Sales" "{ \"refresh\": {} }" detail onProgress))
    |> ok

[<Tests>]
let tests =
    testList "Xmla refresh" [
        testCase "objects: the steps that finish per object, as they arrive, and nothing else from the session"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let seen = ResizeArray<XmlaTraceEvent>()
            let ran = runWith server Detail.Objects seen.Add
            Expect.isNone ran.Failure ""
            Expect.equal server.Executed [ "{ \"refresh\": {} }" ] "the script, once"
            Expect.equal server.Traced 1 ""
            Expect.equal (ran.Progress |> List.map _.EventSubclass) [ "ExecuteSql"; "ReadData"; "Process" ] ""
            Expect.equal (List.ofSeq seen) ran.Progress "reported as they came"
            Expect.isNone ran.TraceFailure ""

        testCase "all: every progress event, but still not the session's other events"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let ran = runWith server Detail.All ignore
            Expect.equal ran.Progress.Length 7 ""
            Expect.all ran.Progress Progress.isProgress ""

        testCase "none: the session isn't traced"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let ran = runWith server Detail.Off (fun _ -> failtest "no events without a trace")
            Expect.isNone ran.Failure ""
            Expect.equal server.Traced 0 ""
            Expect.equal server.Executed.Length 1 ""
            Expect.isEmpty ran.Progress ""

        testCase "lines: the step, the server's text, rows and time"
        <| fun () ->
            let lines =
                trace |> List.filter (Progress.wanted Detail.All) |> List.map Progress.line

            Expect.equal
                lines
                [
                    "[TabularRefresh begin] Started processing"
                    "[ReadData begin] Reading data for the 'Sales' table started."
                    "[ReadData current] Reading data for the 'Sales' table. (100,000 rows)"
                    "[ExecuteSql] SELECT * ... (0.3s)"
                    "[ReadData] Finished reading data for the 'Sales' table. (1,234,567 rows, 12.3s)"
                    "[CompressSegment] Compression for the 'Amount' column completed. (0.0s)"
                    "[Process] Finished processing partition 'Sales' of table 'Sales'. (13.0s)"
                ]
                ""

            Expect.equal
                (Progress.line (event "ProgressReportEnd" "Process" "Sales" ""))
                "[Process] Sales"
                "no text: the object's name"

            Expect.equal
                (Progress.line (event "ProgressReportError" "Process" "Sales" "The key didn't match any rows."))
                "error: [Process] The key didn't match any rows."
                ""

        testCase "errors are always progress; class names match in any case"
        <| fun () ->
            let error = event "progressreporterror" "VertiPaq" "Sales" "Out of memory."
            Expect.isTrue (Progress.wanted Detail.Objects error) ""
            Expect.isTrue (Progress.wanted Detail.Objects (event "ProgressReportEnd" "READDATA" "" "")) ""
            Expect.isFalse (Progress.wanted Detail.Objects (event "ProgressReportEnd" "VertiPaq" "" "")) ""
            Expect.isFalse (Progress.wanted Detail.Off error) ""
            Expect.equal (Progress.phase error) "error" ""

        testCase "the server's errors fail the refresh with its messages; warnings alone don't"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            server.ExecuteMessages <- [ { IsError = false; Text = "a warning" } ]
            let fine = runWith server Detail.Objects ignore
            Expect.isNone fine.Failure ""
            Expect.equal fine.ServerMessages [ "a warning" ] ""

            server.ExecuteMessages <- [
                { IsError = false; Text = "a warning" }
                {
                    IsError = true
                    Text = "Column 'X' doesn't exist in the rowset."
                }
            ]

            let failed = runWith server Detail.Objects ignore

            match failed.Failure with
            | Some(RefreshFailed(s, "Sales", [ "Column 'X' doesn't exist in the rowset." ]) as e) ->
                Expect.equal s workspace ""
                Expect.stringContains (TessError.message e) "Refreshing 'Sales' on" ""
            | other -> failtestf "%A" other

            Expect.equal failed.Progress.Length 3 "what was reported before it failed is kept"

        testCase "the script being refused is a refresh failure, not a connection failure"
        <| fun () ->
            for detail in [ Detail.Off; Detail.Objects ] do
                let server = salesServer ()
                server.ExecuteError <- Some(exn "The remote server returned an error: (403) Forbidden.")
                let ran = runWith server detail ignore

                match ran.Failure with
                | Some(RefreshFailed(_, "Sales", reason :: _)) -> Expect.stringContains reason "403" ""
                | other -> failtestf "%A" other

        testCase "no secret leaves in a message, an event or a trace failure"
        <| fun () ->
            let server = salesServer ()

            server.TraceEvents <- [
                event "ProgressReportEnd" "ExecuteSql" "Sales" $"Provider=x;Password=hunter2;Token={fakeToken}"
            ]

            server.TraceFailure <- Some $"the trace stopped: bearer {fakeToken}"

            server.ExecuteMessages <- [
                {
                    IsError = true
                    Text = $"Login failed with {fakeToken}"
                }
            ]

            let ran = runWith server Detail.All ignore
            let everything = sprintf "%A" ran
            Expect.isFalse (everything.Contains fakeToken) everything
            Expect.isFalse (everything.Contains "hunter2") everything

        testCase "a trace that doesn't work never fails the refresh"
        <| fun () ->
            let server = salesServer ()
            server.TraceFailure <- Some "the trace couldn't be started: no permission"
            let ran = runWith server Detail.Objects ignore
            Expect.isNone ran.Failure ""
            Expect.equal ran.TraceFailure (Some "the trace couldn't be started: no permission") ""
            Expect.equal server.Executed.Length 1 "it still ran"

        testCase "events that arrive after the script returned are dropped"
        <| fun () ->
            let server = salesServer ()
            server.TraceEvents <- trace
            let seen = ResizeArray<XmlaTraceEvent>()
            let ran = runWith server Detail.Objects seen.Add
            let before = seen.Count
            server.LastOnEvent(event "ProgressReportEnd" "Process" "Late" "late")
            Expect.equal seen.Count before ""
            Expect.equal ran.Progress.Length before ""

        testCase "the names matched are the client library's names for the events"
        <| fun () ->
            // The live connector reports an event's class and subclass by their enum names.
            let ended = string Microsoft.AnalysisServices.TraceEventClass.ProgressReportEnd

            for c in
                [
                    Microsoft.AnalysisServices.TraceEventClass.ProgressReportBegin
                    Microsoft.AnalysisServices.TraceEventClass.ProgressReportCurrent
                    Microsoft.AnalysisServices.TraceEventClass.ProgressReportEnd
                    Microsoft.AnalysisServices.TraceEventClass.ProgressReportError
                ] do
                Expect.isTrue (Progress.isProgress (event (string c) "" "" "")) (string c)

            Expect.isTrue
                (Progress.isError (
                    event (string Microsoft.AnalysisServices.TraceEventClass.ProgressReportError) "" "" ""
                ))
                ""

            Expect.isFalse
                (Progress.isProgress (event (string Microsoft.AnalysisServices.TraceEventClass.CommandEnd) "" "" ""))
                ""

            for s in
                [
                    Microsoft.AnalysisServices.TraceEventSubclass.ExecuteSql
                    Microsoft.AnalysisServices.TraceEventSubclass.ReadData
                    Microsoft.AnalysisServices.TraceEventSubclass.Process
                    Microsoft.AnalysisServices.TraceEventSubclass.TabularRefresh
                    Microsoft.AnalysisServices.TraceEventSubclass.Commit
                    Microsoft.AnalysisServices.TraceEventSubclass.TabularCommit
                ] do
                Expect.isTrue (Progress.wanted Detail.Objects (event ended (string s) "" "")) (string s)

            Expect.isFalse
                (Progress.wanted
                    Detail.Objects
                    (event ended (string Microsoft.AnalysisServices.TraceEventSubclass.CompressSegment) "" ""))
                "a per-column step"

        testCase "detail names"
        <| fun () ->
            Expect.equal (Detail.all |> List.map Detail.token) [ "none"; "objects"; "all" ] ""
            Expect.equal (Detail.tryParse "ALL") (Some Detail.All) ""
            Expect.equal (Detail.tryParse "verbose") None ""
    ]
