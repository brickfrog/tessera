/// The conformance corpus against a real server: for each model, embed its rows, deploy it to a
/// scratch database, run every case's query over XMLA and compare the answer with the expectation.
/// The report says which expectations the server disagrees with.
///
/// Not part of the test run: it deploys. It is the `conformance-server` command of the test
/// program (Main.fs; docs/xmla-manual-test.md, section G). Everything here goes through the seam
/// (`Remote.Access`), so the tests drive it with the fake server. It has never run against a real one.
module Tessera.Tests.ConformanceServer

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Tessera
open Tessera.Xmla
open Tessera.Tests.Conformance

/// Every database this creates, replaces or deletes has a name that starts with this.
[<Literal>]
let DatabasePrefix = "tessera-conformance-"

type Options = {
    /// The server: `powerbi://api.powerbi.com/v1.0/myorg/<workspace>`, with or without `xmla:`.
    Server: string
    /// Only the cases whose id contains this.
    Filter: string option
    /// Delete each scratch database when its cases have run.
    Drop: bool
    Compare: CompareOptions
}

type CaseReport = {
    Case: Case
    Outcome: Outcome
    /// What the server returned, when it returned results.
    Actual: QueryResult list option
    DurationMs: int64
}

type ModelReport = {
    Model: string
    Database: string
    /// Why the model's cases didn't run: it couldn't be embedded, deployed or refreshed.
    Problem: string option
    Warnings: string list
    /// Tables whose rows on the server are not the fixture's (table, what differs). A case on such
    /// a model can't be trusted either way.
    DataProblems: (string * string list) list
    Cases: CaseReport list
}

type Report = {
    Server: string
    Started: DateTime
    Models: ModelReport list
}

/// `{ "refresh": { "type": "full", ... } }` for a whole database. A deployed calculated table has
/// no rows until it is processed, and "full" is the type documented to recalculate a calculation
/// partition (https://learn.microsoft.com/en-us/analysis-services/tmsl/refresh-command-tmsl).
let refreshScript (database: string) : string =
    let target = JsonObject()
    target.["database"] <- JsonValue.Create database
    let refresh = JsonObject()
    refresh.["type"] <- JsonValue.Create "full"
    refresh.["objects"] <- JsonArray(target)
    let root = JsonObject()
    root.["refresh"] <- refresh
    root.ToJsonString()

let deleteScript (database: string) : string =
    let target = JsonObject()
    target.["database"] <- JsonValue.Create database
    let delete = JsonObject()
    delete.["object"] <- target
    let root = JsonObject()
    root.["delete"] <- delete
    root.ToJsonString()

let private tableReference (name: string) = "'" + name.Replace("'", "''") + "'"

let private queryValue (v: DaxValue) : QueryValue =
    match v with
    | DaxValue.Blank -> QueryValue.Blank
    | DaxValue.Integer i -> QueryValue.Int64 i
    | DaxValue.Decimal d -> QueryValue.Double d
    | DaxValue.Currency c -> QueryValue.Decimal(DaxValue.currencyValue c)
    | DaxValue.String s -> QueryValue.String s
    | DaxValue.Boolean b -> QueryValue.Boolean b
    | DaxValue.DateTime serial ->
        match DaxValue.dateTimeOfSerial serial with
        | Some d -> QueryValue.DateTime d
        | None -> QueryValue.Blank

let private queryType =
    function
    | DaxType.Integer -> QueryType.Int64
    | DaxType.Decimal -> QueryType.Double
    | DaxType.Currency -> QueryType.Decimal
    | DaxType.String -> QueryType.String
    | DaxType.Boolean -> QueryType.Boolean
    | DaxType.DateTime -> QueryType.DateTime
    | DaxType.Variant -> QueryType.Variant

/// A loaded table as the result `EVALUATE 'Table'` should give: its data columns, as
/// `Table[Column]`, with the rows as the model file gives them (each in the order of those columns).
let tableAsResult (table: Microsoft.AnalysisServices.Tabular.Table) (rows: DaxValue array list) : QueryResult = {
    Columns =
        Fixture.dataColumns table
        |> List.map (fun (c, t) -> {
            Name = $"{table.Name}[{c.Name}]"
            Type = queryType t
        })
    Rows = rows |> List.map (Array.map queryValue)
    Truncated = false
}

/// Run a query on an open connection; a failure is the server's message, with secrets removed.
let private query (c: Remote.Connection) (database: string) (dax: string) : Result<QueryResult list, TessError> =
    try
        Ok(c.Session.Query(database, dax, None))
    with
    | XmlaQueryError message -> Error(QueryFailed(Secrets.scrub c.Secrets message))
    | e -> Error(Failures.classify c.Address c.Secrets e)

/// A real server never says "not supported" in a way that should excuse a case: an error is an error.
let private serverConfig (options: Options) : RunConfig = {
    IsNotSupported = fun _ -> false
    Compare = options.Compare
}

let private notRun (why: string) (cases: Case list) : CaseReport list =
    cases
    |> List.map (fun c -> {
        Case = c
        Outcome = Outcome.Error $"not run: {why}"
        Actual = None
        DurationMs = 0L
    })

/// Deploy one model and run its cases.
let private runModel
    (access: Remote.Access)
    (options: Options)
    (server: XmlaAddress)
    (log: string -> unit)
    (cases: Case list)
    : ModelReport * string option =
    let first = cases.Head
    let database = databaseName first

    let failed (why: string) (warnings: string list) =
        log $"  {database}: {why}"

        {
            Model = first.ModelName
            Database = database
            Problem = Some why
            Warnings = warnings
            DataProblems = []
            Cases = notRun why cases
        },
        None

    if not (database.StartsWith(DatabasePrefix, StringComparison.Ordinal)) then
        failed $"refusing to touch a database whose name doesn't start with {DatabasePrefix}" []
    else
        match prepared first with
        | Error e -> failed $"the model can't be prepared: {e}" []
        | Ok model ->
            match embedForServer model with
            | Error e -> failed (TessError.message e) []
            | Ok embedded ->
                let address = XmlaAddress.withDatabase database server

                let ran =
                    Remote.withConnection access address (fun c ->
                        Remote.serverProperties c
                        |> Result.bind (fun properties ->
                            Remote.findDatabase c database
                            |> Result.bind (fun existing ->
                                match existing with
                                | Some d -> Remote.readDatabase c d.Name |> Result.map Some
                                | None -> Ok None)
                            |> Result.bind (fun deployed ->
                                let target: Deploy.Target = {
                                    Database = database
                                    Deployed = deployed
                                    CompatibilityMode = properties.CompatibilityMode
                                    SupportedCompatibilityLevels = properties.SupportedCompatibilityLevels
                                }

                                // Everything from the model: a scratch database has nothing worth keeping,
                                // and kept partitions would be last run's rows.
                                Deploy.plan Deploy.Options.full Diff.Options.none target embedded.Database)
                            |> Result.bind (fun plan ->
                                log $"  {database}: deploying ({embedded.Tables.Length} tables), refreshing"
                                Remote.execute c database plan.Tmsl)
                            |> Result.bind (fun _ -> Remote.execute c database (refreshScript database))
                            |> Result.map (fun _ ->
                                // First: does the server hold the fixture's rows? If not, embed is at
                                // fault, and no case on this model says anything about DAX.
                                let dataProblems = [
                                    for table in Fixture.dataTables model.Database do
                                        if not (Fixture.dataColumns table).IsEmpty then
                                            let expected = tableAsResult table (rawRows model table.Name)

                                            match query c database $"EVALUATE {tableReference table.Name}" with
                                            | Error e -> table.Name, [ TessError.message e ]
                                            | Ok [ actual ] ->
                                                let differences =
                                                    (QueryResult.compare options.Compare expected actual).Differences
                                                    // Calculated columns come back too.
                                                    |> List.filter (function
                                                        | ResultDifference.UnexpectedColumn _ -> false
                                                        | _ -> true)

                                                if not differences.IsEmpty then
                                                    table.Name, differences |> List.map ResultDifference.describe
                                            | Ok results ->
                                                table.Name, [ $"{results.Length} result sets for one EVALUATE" ]
                                ]

                                let reports =
                                    cases
                                    |> List.map (fun case ->
                                        let clock = Stopwatch.StartNew()

                                        let answer =
                                            match case.As with
                                            | None -> query c database case.Query
                                            // As members of roles: a connection of its own, with
                                            // Roles= (and CustomData=).
                                            | Some a ->
                                                let extra = [
                                                    "Roles", String.concat "," a.Roles

                                                    match a.CustomData with
                                                    | Some text -> "CustomData", text
                                                    | None -> ()
                                                ]

                                                Remote.withConnection
                                                    access
                                                    {
                                                        address with
                                                            Properties = address.Properties @ extra
                                                    }
                                                    (fun own -> query own database case.Query)

                                        {
                                            Case = case
                                            Outcome = judge (serverConfig options) case answer
                                            Actual =
                                                match answer with
                                                | Ok results -> Some results
                                                | Error _ -> None
                                            DurationMs = clock.ElapsedMilliseconds
                                        })

                                let dropProblem =
                                    if options.Drop then
                                        match Remote.execute c database (deleteScript database) with
                                        | Ok _ -> None
                                        | Error e -> Some $"{database} could not be deleted: {TessError.message e}"
                                    else
                                        None

                                dataProblems, reports, dropProblem)))

                match ran with
                // A failure to connect is the same for every model: stop the run.
                | Error(AuthenticationRequired _ | AuthenticationFailed _ | ConnectionFailed _ | UnsupportedConnection _ | InvalidCredentials _ as e) ->
                    let report, _ = failed (TessError.message e) embedded.Warnings
                    report, Some(TessError.message e)
                | Error e -> failed (TessError.message e) embedded.Warnings
                | Ok(dataProblems, reports, dropProblem) ->
                    let passed =
                        reports |> List.filter (fun r -> r.Outcome = Outcome.Pass) |> List.length

                    log $"  {database}: {passed} of {reports.Length} cases agree"

                    {
                        Model = first.ModelName
                        Database = database
                        Problem = None
                        Warnings = embedded.Warnings @ Option.toList dropProblem
                        DataProblems = dataProblems
                        Cases = reports
                    },
                    None

/// Run the cases against a server, model by model. Fails only when the server address is no
/// address; everything else is in the report.
let run
    (access: Remote.Access)
    (options: Options)
    (cases: Case list)
    (log: string -> unit)
    : Result<Report, TessError> =
    XmlaAddress.parseServer options.Server
    |> Result.map (fun server ->
        let selected =
            match options.Filter with
            | Some part ->
                cases
                |> List.filter (fun c -> c.Id.Contains(part, StringComparison.OrdinalIgnoreCase))
            | None -> cases

        let groups = selected |> List.groupBy _.ModelKey |> List.map snd
        log $"{selected.Length} cases on {groups.Length} models -> {XmlaAddress.server server}"
        let started = DateTime.UtcNow
        let models = ResizeArray<ModelReport>()
        let mutable stopped = None

        for group in groups do
            match stopped with
            | Some why ->
                models.Add {
                    Model = group.Head.ModelName
                    Database = databaseName group.Head
                    Problem = Some why
                    Warnings = []
                    DataProblems = []
                    Cases = notRun why group
                }
            | None ->
                let report, fatal = runModel access options server log group
                models.Add report
                stopped <- fatal

        {
            Server = XmlaAddress.server server
            Started = started
            Models = List.ofSeq models
        })

// ---- The report -----------------------------------------------------------------------------------

/// How a case came out, for counting and for the report's sections.
[<RequireQualifiedAccess>]
type Verdict =
    | Agrees
    /// The values agree; a declared type or a number's type doesn't.
    | TypeOnly
    | Disagrees
    /// The server failed the query where results were expected.
    | ServerError
    | NotRun

let verdict (r: CaseReport) : Verdict =
    match r.Outcome with
    | Outcome.Pass -> Verdict.Agrees
    | Outcome.Fail mismatch when Mismatch.isTypeOnly mismatch -> Verdict.TypeOnly
    | Outcome.Fail _ -> Verdict.Disagrees
    | Outcome.Error message when message.StartsWith "not run:" -> Verdict.NotRun
    | Outcome.Error _
    | Outcome.NotSupported _ -> Verdict.ServerError

let private verdictToken =
    function
    | Verdict.Agrees -> "agrees"
    | Verdict.TypeOnly -> "type-only"
    | Verdict.Disagrees -> "disagrees"
    | Verdict.ServerError -> "server-error"
    | Verdict.NotRun -> "not-run"

let private allCases (report: Report) = report.Models |> List.collect _.Cases

let count (report: Report) (v: Verdict) =
    allCases report |> List.filter (fun r -> verdict r = v) |> List.length

/// Nothing to correct: every case ran and agreed, and every model held its fixture's rows.
let agrees (report: Report) =
    allCases report |> List.forall (fun r -> verdict r = Verdict.Agrees)
    && report.Models
       |> List.forall (fun m -> m.Problem.IsNone && m.DataProblems.IsEmpty)

let private detail (r: CaseReport) : string list =
    match r.Outcome with
    | Outcome.Pass -> []
    | Outcome.Fail mismatch -> Mismatch.describe mismatch
    | Outcome.Error message
    | Outcome.NotSupported message -> [ message ]

let private text (s: string) : JsonNode = JsonValue.Create s

let private strings (items: string seq) : JsonNode =
    JsonArray(items |> Seq.map text |> Array.ofSeq)

let toJson (report: Report) : JsonNode =
    let root = JsonObject()
    root.["server"] <- text report.Server

    root.["started"] <-
        text (report.Started.ToString("yyyy-MM-dd'T'HH:mm:ss'Z'", Globalization.CultureInfo.InvariantCulture))

    let summary = JsonObject()
    summary.["cases"] <- JsonValue.Create (allCases report).Length

    for v in
        [
            Verdict.Agrees
            Verdict.TypeOnly
            Verdict.Disagrees
            Verdict.ServerError
            Verdict.NotRun
        ] do
        summary.[verdictToken v] <- JsonValue.Create(count report v)

    root.["summary"] <- summary

    root.["models"] <-
        JsonArray(
            [|
                for m in report.Models ->
                    let o = JsonObject()
                    o.["model"] <- text m.Model
                    o.["database"] <- text m.Database
                    o.["problem"] <- (m.Problem |> Option.map text |> Option.toObj)
                    o.["warnings"] <- strings m.Warnings

                    o.["dataProblems"] <-
                        JsonArray(
                            [|
                                for table, differences in m.DataProblems ->
                                    let d = JsonObject()
                                    d.["table"] <- text table
                                    d.["differences"] <- strings differences
                                    d :> JsonNode
                            |]
                        )

                    o :> JsonNode
            |]
        )

    root.["cases"] <-
        JsonArray(
            [|
                for m in report.Models do
                    for r in m.Cases ->
                        let o = JsonObject()
                        o.["id"] <- text r.Case.Id
                        o.["verdict"] <- text (verdictToken (verdict r))
                        o.["title"] <- text r.Case.Title
                        o.["database"] <- text m.Database

                        o.["confidence"] <-
                            text (
                                if r.Case.Confidence = Confidence.Published then
                                    "published"
                                else
                                    "derived"
                            )

                        o.["uncertain"] <- JsonValue.Create r.Case.Uncertain
                        o.["durationMs"] <- JsonValue.Create r.DurationMs

                        if verdict r <> Verdict.Agrees then
                            o.["query"] <- text r.Case.Query
                            o.["differences"] <- strings (detail r)
                            o.["alternative"] <- (r.Case.Alternative |> Option.map text |> Option.toObj)
                            o.["sources"] <- strings (r.Case.Sources |> List.map _.Url)

                            o.["expected"] <-
                                match r.Case.Expected with
                                | Expected.Results results -> QueryResult.listToJson results
                                | Expected.Error reason ->
                                    let e = JsonObject()
                                    e.["error"] <- text reason
                                    e

                            o.["actual"] <- (r.Actual |> Option.map QueryResult.listToJson |> Option.toObj)

                        o :> JsonNode
            |]
        )

    root

let toMarkdown (report: Report) : string =
    let sb = StringBuilder()
    let line (s: string) = sb.Append(s).Append('\n') |> ignore
    let cases = allCases report
    let n (v: Verdict) = count report v

    line "# DAX conformance: the corpus against a server"
    line ""
    line $"Server: `{report.Server}`. {cases.Length} cases on {report.Models.Length} models."
    line ""
    line "| | Cases |"
    line "|---|---|"
    line $"| The server agrees with the expectation | {n Verdict.Agrees} |"
    line $"| Values agree, a type doesn't | {n Verdict.TypeOnly} |"
    line $"| The server disagrees | {n Verdict.Disagrees} |"
    line $"| The server failed the query | {n Verdict.ServerError} |"
    line $"| Not run (the model didn't deploy) | {n Verdict.NotRun} |"
    line ""

    let broken =
        report.Models
        |> List.filter (fun m -> m.Problem.IsSome || not m.DataProblems.IsEmpty)

    if not broken.IsEmpty then
        line "## Models that are not what the corpus says"
        line ""
        line "Look at these first. The rows were deployed exactly as the model files give them, so a table that"
        line "differs only in text casing or trailing spaces shows the server's own storage rules. Anything else is"
        line "about the embedding or the deployment, not about DAX, and the cases on such a model prove nothing."
        line ""

        for m in broken do
            line $"### {m.Database}"
            line ""
            m.Problem |> Option.iter (fun p -> line $"- not deployed: {p}")

            for table, differences in m.DataProblems do
                line $"- `{table}` doesn't hold the fixture's rows:"

                for d in differences |> List.truncate 10 do
                    line $"  - {d}"

                if differences.Length > 10 then
                    line $"  - ... and {differences.Length - 10} more"

            line ""

    let section (title: string) (intro: string) (v: Verdict) =
        let these = cases |> List.filter (fun r -> verdict r = v)

        if not these.IsEmpty then
            line $"## {title}"
            line ""
            line intro
            line ""

            for r in these do
                let confidence =
                    match r.Case.Confidence, r.Case.Uncertain with
                    | _, true -> "derived, uncertain"
                    | Confidence.Published, _ -> "published"
                    | Confidence.Derived, _ -> "derived"

                line $"### {r.Case.Id}"
                line ""
                line $"{r.Case.Title} ({confidence})"
                line ""
                line "```dax"
                line r.Case.Query
                line "```"
                line ""

                for d in detail r do
                    line $"- {d}"

                r.Case.Alternative |> Option.iter (fun a -> line $"- the other reading: {a}")

                for s in r.Case.Sources do
                    line $"- source: {s.Url}"

                line ""

    section
        "The server disagrees"
        "The expectation is wrong, or was read wrongly from its source. Correct the case file (and its confidence)."
        Verdict.Disagrees

    section
        "The server failed the query"
        "Results were expected. The query may use something this server doesn't have, or the case is wrong."
        Verdict.ServerError

    section
        "Values agree, a type doesn't"
        "The numbers are right; a column's declared type or a number's type isn't what the case says."
        Verdict.TypeOnly

    let warnings =
        report.Models
        |> List.collect (fun m -> m.Warnings |> List.map (fun w -> m.Database, w))

    if not warnings.IsEmpty then
        line "## Warnings"
        line ""

        for database, w in warnings do
            line $"- {database}: {w}"

        line ""

    sb.ToString()

/// Write conformance-report.json and conformance-report.md into a directory; returns their paths.
let write (directory: string) (report: Report) : string * string =
    let directory = Path.GetFullPath directory
    Directory.CreateDirectory directory |> ignore
    let json = Path.Combine(directory, "conformance-report.json")
    let markdown = Path.Combine(directory, "conformance-report.md")

    let options =
        JsonSerializerOptions(
            WriteIndented = true,
            Encoder = Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        )

    File.WriteAllText(json, (toJson report).ToJsonString options + "\n", UTF8Encoding false)
    File.WriteAllText(markdown, toMarkdown report, UTF8Encoding false)
    json, markdown

// ---- The command ----------------------------------------------------------------------------------

let private usage =
    """Run the DAX conformance corpus against a real server.

  TESSERA_XMLA_SERVER=<server> dotnet run --project tests/Tessera.Tests -c Release -- conformance-server [options]

  --out <dir>       where to write conformance-report.json and .md (default: ./conformance-report)
  --filter <text>   only the cases whose id contains this (e.g. blank/)
  --drop            delete each scratch database after its cases have run
  --tolerance <x>   relative tolerance for doubles (default 1e-9)

Credentials come from the TESS_XMLA_* variables, as for tess itself. For every model of the corpus this
creates or REPLACES a database named tessera-conformance-<model> on the server, and nothing else."""

/// `conformance-server [options]`, with the environment and the connector to use. Exit codes: 0 the
/// server agrees with every case (or TESSERA_XMLA_SERVER is not set: nothing to do), 1 it doesn't,
/// 2 usage, 3 no corpus, 5 the server couldn't be used.
let main
    (connector: IXmlaConnector)
    (getEnv: string -> string option)
    (out: TextWriter)
    (err: TextWriter)
    (args: string list)
    : int =
    let rec parse (options: Options) (directory: string) (rest: string list) =
        match rest with
        | [] -> Ok(options, directory)
        | "--out" :: dir :: tail -> parse options dir tail
        | "--filter" :: part :: tail -> parse { options with Filter = Some part } directory tail
        | "--drop" :: tail -> parse { options with Drop = true } directory tail
        | "--tolerance" :: value :: tail ->
            match
                Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)
            with
            | true, t when t >= 0.0 ->
                parse
                    {
                        options with
                            Compare = {
                                options.Compare with
                                    RelativeTolerance = t
                            }
                    }
                    directory
                    tail
            | _ -> Error $"--tolerance: '{value}' is not a number"
        | other :: _ -> Error $"unknown argument '{other}'"

    let defaults = {
        Server = ""
        Filter = None
        Drop = false
        Compare = CompareOptions.defaults
    }

    match args with
    | [ "--help" ]
    | [ "-h" ] ->
        out.WriteLine usage
        0
    | _ ->
        match parse defaults "conformance-report" args with
        | Error message ->
            err.WriteLine message
            err.WriteLine usage
            2
        | Ok(options, directory) ->
            match getEnv "TESSERA_XMLA_SERVER", corpusDirectory with
            | None, _ ->
                out.WriteLine "TESSERA_XMLA_SERVER is not set: nothing to do. (--help says what this would do.)"
                0
            | _, None ->
                err.WriteLine "tests/conformance was not found (set TESSERA_CONFORMANCE_DIR)"
                3
            | Some server, Some corpusPath ->
                let corpus = load corpusPath

                for file, why in corpus.Problems do
                    err.WriteLine $"skipped {file}: {why}"

                let access: Remote.Access = {
                    Connector = connector
                    Environment = Credentials.processEnvironment getEnv
                    Notify = fun message -> err.WriteLine message
                    IsWindows = OperatingSystem.IsWindows()
                }

                match run access { options with Server = server } corpus.Cases (fun s -> err.WriteLine s) with
                | Error e ->
                    err.WriteLine(TessError.message e)
                    2
                | Ok report ->
                    let json, markdown = write directory report
                    let n (v: Verdict) = count report v

                    out.WriteLine(
                        $"{n Verdict.Agrees} agree, {n Verdict.Disagrees} disagree, {n Verdict.TypeOnly} differ in type only, "
                        + $"{n Verdict.ServerError} failed on the server, {n Verdict.NotRun} not run"
                    )

                    out.WriteLine $"Report: {markdown}"
                    out.WriteLine $"        {json}"

                    if agrees report then
                        0
                    elif report.Models |> List.forall (fun m -> m.Problem.IsSome) then
                        5
                    else
                        1
