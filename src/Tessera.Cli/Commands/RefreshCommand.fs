/// `tess refresh`: refresh a model on a server, or some of its tables or partitions. A dry run by
/// default: it connects, reads the model to find what the selectors name, and shows the TMSL
/// script it would run. Nothing is refreshed without --execute.
module Tessera.Cli.RefreshCommand

open System
open System.Globalization
open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "refresh a server model, or some tables or partitions: shows the TMSL script; --execute to run it."

type Args =
    | [<MainCommand>] Target of string list
    | Execute
    | [<AltCommandLine("-t")>] Type of ``type``: string
    | Max_Parallelism of threads: int
    | Apply_Refresh_Policy
    | No_Apply_Refresh_Policy
    | Effective_Date of date: string
    | Progress of detail: string
    | Tmsl

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model, xmla:<server>[;<database>] (quote it: `;` ends a shell command), then what to refresh: "
                + "tables (Sales, table:Sales) and partitions (partition:Sales/2024, 'Sales'[2024]). Nothing, or "
                + "`model`, is the whole model."
            | Execute -> "refresh for real (default: dry run; it connects and reads the model, but refreshes nothing)."
            | Type _ ->
                "full, automatic (the default), dataOnly, calculate, clearValues, defragment (the model or tables) "
                + "or add (partitions)."
            | Max_Parallelism _ ->
                "process at most this many objects at once (wraps the refresh in a TMSL sequence). Default: the server decides."
            | Apply_Refresh_Policy ->
                "apply the incremental refresh policy of the tables refreshed (the server's default; this says so in the script)."
            | No_Apply_Refresh_Policy ->
                "ignore incremental refresh policies: refresh the partitions the tables have, as they are."
            | Effective_Date _ ->
                "the date (YYYY-MM-DD) an applied refresh policy treats as today. Implies --apply-refresh-policy."
            | Progress _ ->
                "with --execute: objects (the default: steps finished per table or partition), all (every progress "
                + "event the server reports) or none (don't trace the session)."
            | Tmsl -> "print only the TMSL script."

type private Outcome' = {
    Address: XmlaAddress
    Plan: Refresh.Plan
    /// None for a dry run.
    Ran: RemoteRefresh.Ran option
}

let private targetJson (t: Refresh.Target) : JsonNode =
    let o = JsonObject()
    o.["ref"] <- str (ObjectRef.format (Refresh.Target.ref t))

    match t with
    | Refresh.WholeModel -> ()
    | Refresh.Table table -> o.["table"] <- str table
    | Refresh.Partition(table, partition) ->
        o.["table"] <- str table
        o.["partition"] <- str partition

    o

let private optNumber (n: int64 option) : JsonNode =
    match n with
    | Some n -> JsonValue.Create n
    | None -> null

let private eventJson (e: XmlaTraceEvent) : JsonNode =
    obj [
        "phase", str (RemoteRefresh.Progress.phase e)
        "step", str e.EventSubclass
        "object", str e.ObjectName
        "objectPath", str e.ObjectPath
        "text", str e.Text
        "integerData", optNumber e.IntegerData
        "durationMs", optNumber e.DurationMs
    ]

let private resultJson (r: Outcome') : JsonNode =
    let request = r.Plan.Request

    let o =
        obj [
            "command", str "refresh"
            "source", str (XmlaAddress.display r.Address)
            "server", str r.Address.DataSource
            "database", str request.Database
            "type", str (Refresh.Kind.token request.Kind)
            "dryRun", bool r.Ran.IsNone
            "refreshed", bool (r.Ran |> Option.exists _.Failure.IsNone)
            "objects", arr (request.Targets |> List.map targetJson)
            "covers",
            arr (
                r.Plan.Covers
                |> List.map (fun c ->
                    obj [
                        "table", str c.Table
                        "partitions", arr (c.Partitions |> List.map str)
                        "wholeTable", bool c.WholeTable
                        "refreshPolicy", bool c.HasRefreshPolicy
                    ])
            )
            "applyRefreshPolicy",
            (match request.ApplyRefreshPolicy, request.EffectiveDate with
             | _, Some _ -> bool true
             | Some apply, None -> bool apply
             | None, None -> null)
            "effectiveDate",
            optStr (
                request.EffectiveDate
                |> Option.map (fun d -> d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))
            )
            "maxParallelism", optNumber (request.MaxParallelism |> Option.map int64)
            "warnings", arr (r.Plan.Warnings |> List.map str)
            "tmsl", str r.Plan.Tmsl
            "serverMessages", arr (r.Ran |> Option.map _.ServerMessages |> Option.defaultValue [] |> List.map str)
            "progress", arr (r.Ran |> Option.map _.Progress |> Option.defaultValue [] |> List.map eventJson)
            "traceError", optStr (r.Ran |> Option.bind _.TraceFailure)
            "durationMs", optNumber (r.Ran |> Option.map (fun ran -> int64 ran.Elapsed.TotalMilliseconds))
        ]

    match r.Ran |> Option.bind _.Failure with
    | Some e -> o.AsObject().["error"] <- obj [ "code", str (TessError.code e); "message", str (TessError.message e) ]
    | None -> ()

    o

/// "the whole model", "2 tables", "1 table and 3 partitions".
let private scope (targets: Refresh.Target list) =
    let count (n: int) (noun: string) =
        if n = 1 then $"1 {noun}" else $"{n} {noun}s"

    let tables =
        targets
        |> List.filter (function
            | Refresh.Table _ -> true
            | _ -> false)
        |> List.length

    let partitions =
        targets
        |> List.filter (function
            | Refresh.Partition _ -> true
            | _ -> false)
        |> List.length

    if List.contains Refresh.WholeModel targets then
        "the whole model"
    else
        [
            if tables > 0 then
                count tables "table"
            if partitions > 0 then
                count partitions "partition"
        ]
        |> String.concat " and "

let private seconds (t: TimeSpan) =
    t.TotalSeconds.ToString("0.0", CultureInfo.InvariantCulture) + "s"

let private planText (out: TextWriter) (address: XmlaAddress) (plan: Refresh.Plan) =
    let request = plan.Request
    let kind = Refresh.Kind.token request.Kind

    out.WriteLine($"Refresh ({kind}) '{request.Database}' on {address.DataSource}: {scope request.Targets}")

    for c in plan.Covers do
        let partitions =
            match c.Partitions with
            | [ one ] when one = c.Table && c.WholeTable -> "1 partition"
            | [ one ] -> $"partition {one}"
            | many when c.WholeTable && many.Length > 6 -> $"{many.Length} partitions"
            | many -> "partitions " + String.concat ", " many

        let part = if c.WholeTable then "" else " (not the whole table)"
        let policy = if c.HasRefreshPolicy then " [refresh policy]" else ""
        out.WriteLine($"  {c.Table}: {partitions}{part}{policy}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let execute = args.Contains Execute

    let kind =
        match args.TryGetResult Args.Type with
        | None -> Ok Refresh.Options.defaults.Kind
        | Some text ->
            match Refresh.Kind.tryParse text with
            | Some k -> Ok k
            | None ->
                let tokens = Refresh.Kind.all |> List.map Refresh.Kind.token
                let known = String.concat ", " tokens
                Error $"unknown refresh type '{text}'{Suggest.hint (Suggest.closest tokens text)} (known: {known})"

    let effectiveDate =
        match args.TryGetResult Effective_Date with
        | None -> Ok None
        | Some text ->
            match
                DateOnly.TryParseExact(text.Trim(), "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None)
            with
            | true, d -> Ok(Some d)
            | _ -> Error $"--effective-date '{text}' is not a date: use YYYY-MM-DD"

    let detail =
        match args.TryGetResult Args.Progress with
        | None -> Ok RemoteRefresh.Detail.Objects
        | Some text ->
            match RemoteRefresh.Detail.tryParse text with
            | Some d -> Ok d
            | None ->
                Error
                    $"""unknown --progress '{text}'{Suggest.hint (Suggest.closest [ "objects"; "all"; "none" ] text)} (known: objects, all, none)"""

    // Every positional argument, wherever the options are among them.
    match args.GetResults Args.Target |> List.concat, kind, effectiveDate, detail with
    | [], _, _, _ -> UsageError "expected a model (xmla:<server>;<database>), then any tables or partitions to refresh"
    | source :: _, _, _, _ when not (XmlaAddress.isRemote source) ->
        UsageError
            $"'{source}' is not a server model: a refresh loads data into a model on a server (xmla:<server>;<database>); model files hold no data to refresh"
    | _, Error message, _, _
    | _, _, Error message, _
    | _, _, _, Error message -> UsageError message
    | _ when args.Contains Apply_Refresh_Policy && args.Contains No_Apply_Refresh_Policy ->
        UsageError "--apply-refresh-policy and --no-apply-refresh-policy can't be combined"
    | _, _, Ok(Some _), _ when args.Contains No_Apply_Refresh_Policy ->
        UsageError
            "--effective-date and --no-apply-refresh-policy can't be combined (the date only matters to an applied policy)"
    | _ when args.TryGetResult Max_Parallelism |> Option.exists (fun n -> n < 1) ->
        UsageError "--max-parallelism must be 1 or more"
    | source :: selectors, Ok kind, Ok effectiveDate, Ok detail ->
        let options: Refresh.Options = {
            Kind = kind
            ApplyRefreshPolicy =
                if args.Contains No_Apply_Refresh_Policy then Some false
                elif args.Contains Apply_Refresh_Policy then Some true
                else None
            EffectiveDate = effectiveDate
            MaxParallelism = args.TryGetResult Max_Parallelism
        }

        let text = not ctx.Json
        let tmslOnly = text && args.Contains Tmsl
        // Progress arrives on another thread while the main one waits: one writer at a time.
        let gate = Object()

        let onProgress (e: XmlaTraceEvent) =
            if text then
                lock gate (fun () -> ctx.Err.WriteLine("  " + RemoteRefresh.Progress.line e))

        let result =
            XmlaAddress.parse source
            |> Result.bind (fun address ->
                Remote.withConnection (Source.access ctx) address (fun c ->
                    Remote.resolveDatabase c
                    |> Result.bind (fun database ->
                        Remote.readDatabase c database.Name
                        |> Result.bind (fun db ->
                            let address = XmlaAddress.withDatabase database.Name address

                            Refresh.plan options database.Name db selectors
                            |> Result.map (fun plan ->
                                // Shown before running: a refresh can take hours.
                                if tmslOnly then
                                    ctx.Out.WriteLine plan.Tmsl
                                elif text then
                                    planText ctx.Out address plan

                                    if not execute then
                                        ctx.Out.WriteLine()
                                        ctx.Out.WriteLine("TMSL:")
                                        ctx.Out.WriteLine plan.Tmsl

                                if text then
                                    ctx.Out.Flush()

                                    for w in plan.Warnings do
                                        ctx.Err.WriteLine($"warning: {w}")

                                let ran =
                                    if execute then
                                        if text then
                                            ctx.Err.WriteLine("Refreshing...")

                                        Some(RemoteRefresh.run c plan.Request.Database plan.Tmsl detail onProgress)
                                    else
                                        None

                                {
                                    Address = address
                                    Plan = plan
                                    Ran = ran
                                })))))

        match result with
        | Error e -> Failed e
        | Ok r ->
            if ctx.Json then
                writeJson ctx.Out (resultJson r)

            let database = r.Plan.Request.Database

            match r.Ran with
            | None ->
                if text then
                    ctx.Err.WriteLine("Dry run: nothing refreshed. Add --execute to refresh.")

                Done
            | Some ran ->
                if text then
                    lock gate (fun () ->
                        match ran.TraceFailure with
                        | Some reason -> ctx.Err.WriteLine($"warning: no progress could be reported: {reason}")
                        | None -> ()

                        for m in ran.ServerMessages do
                            ctx.Err.WriteLine($"server: {m}"))

                match ran.Failure with
                | Some e when ctx.Json -> DoneWith(ExitCode.ofError e)
                | Some e ->
                    ctx.Err.WriteLine($"Failed after {seconds ran.Elapsed}.")
                    Failed e
                | None ->
                    if text then
                        ctx.Err.WriteLine($"Refreshed '{database}' on {r.Address.DataSource} in {seconds ran.Elapsed}.")

                    Done
