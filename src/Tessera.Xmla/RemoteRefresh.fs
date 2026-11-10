/// Running a refresh script on a server and following its progress, in terms of the seam
/// (Session.fs): testable without a server.
///
/// Progress comes from the server's trace of the session. What the events hold (which subclasses
/// a refresh reports, what their text says) is the server's business and differs between versions,
/// so this module assumes little: it keeps the "Progress Report" events, shows the server's own
/// text, and picks the per-object steps by subclass name.
module Tessera.Xmla.RemoteRefresh

open System
open System.Diagnostics
open System.Globalization
open Tessera

/// How much of the trace to report.
[<RequireQualifiedAccess>]
type Detail =
    /// Don't trace at all: only start, finish and duration are known.
    | Off
    /// The steps that finish per table or partition (query, rows read, processing), and errors.
    | Objects
    /// Every progress event the server reports, including "begin" and "current" ones.
    | All

module Detail =
    let token =
        function
        | Detail.Off -> "none"
        | Detail.Objects -> "objects"
        | Detail.All -> "all"

    let all = [ Detail.Off; Detail.Objects; Detail.All ]

    let tryParse (s: string) =
        all
        |> List.tryFind (fun d -> String.Equals(token d, s, StringComparison.OrdinalIgnoreCase))

module Progress =

    let private isClass (name: string) (e: XmlaTraceEvent) =
        String.Equals(e.EventClass, name, StringComparison.OrdinalIgnoreCase)

    let isError = isClass "ProgressReportError"
    let isEnd = isClass "ProgressReportEnd"

    /// One of the server's "Progress Report" events (begin, current, end, error). A session
    /// trace also carries everything else the session does; none of that is progress.
    let isProgress (e: XmlaTraceEvent) =
        e.EventClass.StartsWith("ProgressReport", StringComparison.OrdinalIgnoreCase)

    /// The steps a refresh reports once per table or partition, rather than once per column or
    /// segment: running the source query, reading its rows, processing the object, and the
    /// refresh and its commit as a whole.
    let private objectSteps =
        set [
            "executesql"
            "readdata"
            "process"
            "tabularrefresh"
            "commit"
            "tabularcommit"
        ]

    /// Whether an event is reported at this level of detail.
    let wanted (detail: Detail) (e: XmlaTraceEvent) =
        match detail with
        | Detail.Off -> false
        | Detail.All -> isProgress e
        | Detail.Objects ->
            isError e
            || (isEnd e && objectSteps.Contains(e.EventSubclass.ToLowerInvariant()))

    let private seconds (ms: int64) =
        (float ms / 1000.0).ToString("0.0", CultureInfo.InvariantCulture) + "s"

    /// "begin", "current", "end" or "error" (or the class's name, for anything else).
    let phase (e: XmlaTraceEvent) =
        if isProgress e then
            e.EventClass.Substring("ProgressReport".Length).ToLowerInvariant()
        else
            e.EventClass

    /// One line for a person: the step, the server's own text (or the object's name), the rows
    /// read and the time taken where the event has them.
    let line (e: XmlaTraceEvent) =
        let step = if e.EventSubclass = "" then phase e else e.EventSubclass

        let what =
            // The text can be a whole SQL or M query: one line of it is enough here.
            match e.Text.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries) with
            | [||] -> e.ObjectName
            | lines ->
                let first = lines.[0].Trim()
                if lines.Length > 1 then first + " ..." else first

        let rows =
            match e.IntegerData with
            | Some n when String.Equals(e.EventSubclass, "ReadData", StringComparison.OrdinalIgnoreCase) -> [
                n.ToString("N0", CultureInfo.InvariantCulture) + " rows"
              ]
            | _ -> []

        let took =
            match e.DurationMs with
            | Some ms when isEnd e -> [ seconds ms ]
            | _ -> []

        let prefix = if isError e then "error: " else ""

        let facts =
            match rows @ took with
            | [] -> ""
            | f -> " (" + String.concat ", " f + ")"

        let tag = if isEnd e || isError e then step else $"{step} {phase e}"
        $"{prefix}[{tag}] {what}{facts}".TrimEnd()

/// What running a refresh script came to.
type Ran = {
    /// Why it failed, if it did.
    Failure: TessError option
    /// What the server said that wasn't an error (warnings), when it succeeded.
    ServerMessages: string list
    /// The progress events reported, in the order they arrived.
    Progress: XmlaTraceEvent list
    /// Why progress is missing or incomplete, when tracing was asked for and didn't work.
    TraceFailure: string option
    Elapsed: TimeSpan
}

/// Run a refresh script against `database`. With a detail other than Off the session is traced:
/// `onProgress` gets each event wanted at that detail as it arrives (on another thread, one call
/// at a time), with secrets removed. A trace that doesn't work never fails the refresh.
let run
    (c: Remote.Connection)
    (database: string)
    (tmsl: string)
    (detail: Detail)
    (onProgress: XmlaTraceEvent -> unit)
    : Ran =
    let server = XmlaAddress.server c.Address
    let scrub = Secrets.scrub c.Secrets
    let gate = obj ()
    let events = ResizeArray<XmlaTraceEvent>()
    let mutable finished = false

    let onEvent (e: XmlaTraceEvent) =
        if Progress.wanted detail e then
            let e = {
                e with
                    Text = scrub e.Text
                    ObjectName = scrub e.ObjectName
                    ObjectPath = scrub e.ObjectPath
            }

            lock gate (fun () ->
                // Events that arrive after the script returned are dropped, so nothing is
                // printed once the command has reported its result.
                if not finished then
                    events.Add e
                    onProgress e)

    let clock = Stopwatch.StartNew()

    let ran =
        try
            match detail with
            | Detail.Off ->
                Ok {
                    Messages = c.Session.Execute tmsl
                    TraceFailure = None
                }
            | _ -> Ok(c.Session.ExecuteTraced(tmsl, onEvent))
        with e ->
            // The connection was fine a moment ago: this is the script being refused.
            match Failures.classify c.Address c.Secrets e with
            | ConnectionFailed(_, reason, hint) -> Error(RefreshFailed(server, database, reason :: Option.toList hint))
            | other -> Error other

    clock.Stop()

    let progress =
        lock gate (fun () ->
            finished <- true
            List.ofSeq events)

    let result failure messages traceFailure = {
        Failure = failure
        ServerMessages = messages
        Progress = progress
        TraceFailure = traceFailure |> Option.map scrub
        Elapsed = clock.Elapsed
    }

    match ran with
    | Error e -> result (Some e) [] None
    | Ok traced ->
        let text (m: XmlaMessage) = scrub m.Text

        match traced.Messages |> List.filter _.IsError with
        | [] -> result None (traced.Messages |> List.map text) traced.TraceFailure
        | errors -> result (Some(RefreshFailed(server, database, errors |> List.map text))) [] traced.TraceFailure
