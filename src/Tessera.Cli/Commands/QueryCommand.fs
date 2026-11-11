/// `tess query`: run a DAX query and print the result: against a model on a server, or against
/// model files with fixture data (`--data`), evaluated by the local engine.
module Tessera.Cli.QueryCommand

open System
open System.Diagnostics
open System.IO
open Argu
open Tessera
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "run a DAX query on a server model (xmla:<server>;<database>), or offline on model files with --data <fixture>."

/// Rows shown per result unless --max-rows says otherwise.
let defaultMaxRows = 1000

type Args =
    | [<MainCommand>] Target of string list
    | [<AltCommandLine("-f")>] File of path: string
    | Csv
    | Max_Rows of rows: int
    | Data of directory: string
    | Coverage
    | Now of datetime: string
    | User_Name of name: string
    | User_Principal_Name of upn: string
    | User_Object_Id of id: string
    | User_Culture of culture: string
    | Custom_Data of text: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model: xmla:<server>[;<database>] (quote it: `;` ends a shell command), or model files with "
                + "--data; followed by the DAX query, or `-` to read it from standard input."
            | File _ -> "read the DAX query from this file."
            | Csv -> "write CSV (several results are separated by an empty line)."
            | Max_Rows _ ->
                $"rows to return per result (default {defaultMaxRows}; 0 for all). A result cut short is marked truncated."
            | Data _ ->
                "evaluate offline with the local DAX engine, over this fixture directory (see `tess data`). "
                + "Nothing leaves the machine."
            | Coverage ->
                "instead of running a query: list which measures, calculated columns and calculated tables of "
                + "the model files the local engine can evaluate, and what the others use that it doesn't support."
            | Now _ ->
                "with --data: the date and time TODAY and NOW return, as ISO 8601 (2024-03-05T14:30:00), in "
                + "the query and in calculated tables and columns. With a UTC offset (...+02:00, ...Z), UTCTODAY "
                + "and UTCNOW return that instant in UTC; without one, the same as NOW. Default: the machine's clock."
            | User_Name _ -> "with --data: what USERNAME () returns (refused unless given)."
            | User_Principal_Name _ -> "with --data: what USERPRINCIPALNAME () returns (refused unless given)."
            | User_Object_Id _ -> "with --data: what USEROBJECTID () returns (refused unless given)."
            | User_Culture _ -> "with --data: what USERCULTURE () returns (refused unless given)."
            | Custom_Data _ ->
                "with --data: what CUSTOMDATA () returns (BLANK if another identity option is given, else refused)."

/// The identity options given, if any: who the local engine says is asking.
let private identity (args: ParseResults<Args>) : DaxEngine.Identity option =
    let id: DaxEngine.Identity = {
        UserName = args.TryGetResult Args.User_Name
        UserPrincipalName = args.TryGetResult Args.User_Principal_Name
        UserObjectId = args.TryGetResult Args.User_Object_Id
        UserCulture = args.TryGetResult Args.User_Culture
        CustomData = args.TryGetResult Args.Custom_Data
    }

    if id = DaxEngine.Identity.none then None else Some id

let private queryJson (source: string) (results: QueryResult list) (elapsed: TimeSpan) =
    obj [
        "command", str "query"
        "source", str source
        "results", QueryResult.listToJson results
        "durationMs", num (int elapsed.TotalMilliseconds)
    ]

let private readDax (args: ParseResults<Args>) (rest: string list) : Result<string, Outcome> =
    match args.TryGetResult Args.File, rest with
    | Some _, _ :: _ -> Error(UsageError "give the query as an argument or with --file, not both")
    | Some path, [] ->
        try
            Ok(File.ReadAllText path)
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException -> Error(Failed(PathNotFound(Path.GetFullPath path)))
        | e -> Error(Failed(LoadFailed(path, e.Message)))
    | None, [ "-" ] -> Ok(Console.In.ReadToEnd())
    | None, [ dax ] -> Ok dax
    | None, [] -> Error(UsageError "expected a DAX query: an argument, `-` (standard input) or --file <path>")
    | None, _ -> Error(UsageError "expected one DAX query (quote it)")

/// Print results as JSON, CSV or tables.
let private render
    (ctx: Ctx)
    (args: ParseResults<Args>)
    (source: string)
    (results: QueryResult list)
    (elapsed: TimeSpan)
    (maxRows: int)
    =
    if ctx.Json then
        writeJson ctx.Out (queryJson source results elapsed)
    elif args.Contains Args.Csv then
        results
        |> List.iteri (fun i r ->
            if i > 0 then
                ctx.Out.WriteLine()

            ctx.Out.Write(QueryResult.toCsv ctx.Out.NewLine r))
    else
        results
        |> List.iteri (fun i r ->
            if i > 0 then
                ctx.Out.WriteLine()

            for line in QueryResult.toTable r do
                ctx.Out.WriteLine line)

    if not ctx.Json && results |> List.exists _.Truncated then
        ctx.Err.WriteLine($"Showing the first {maxRows} rows of each result (--max-rows 0 for all).")

    Done

/// The first `limit` rows of a result, marked truncated if there were more.
let private cap (limit: int option) (r: QueryResult) : QueryResult =
    match limit with
    | Some n when r.Rows.Length > n -> {
        r with
            Rows = List.truncate n r.Rows
            Truncated = true
      }
    | _ -> r

/// `--coverage`: what the local engine can evaluate of a model, told from the model alone.
let private coverage (ctx: Ctx) (source: string) =
    match ModelIO.loadPath source with
    | Error e -> Failed e
    | Ok(src, db) ->
        let items = Engine.coverage db

        let ofKind (kind: ObjectKind) =
            items |> List.filter (fun (r, _) -> r.Kind = kind)

        let supported (xs: (ObjectRef * string list) list) =
            xs |> List.filter (fun (_, u) -> u.IsEmpty) |> List.length

        let reasons =
            items
            |> List.collect (fun (_, unsupported) -> unsupported)
            |> List.countBy id
            |> List.sortBy (fun (reason, n) -> -n, reason)

        let kinds = [
            "measures", ObjectKind.Measure
            "calculatedColumns", ObjectKind.Column
            "calculatedTables", ObjectKind.Table
        ]

        if ctx.Json then
            writeJson
                ctx.Out
                (obj [
                    "command", str "query coverage"
                    "source", str (src.Path)
                    "summary",
                    obj [
                        for (name, kind) in kinds ->
                            name,
                            obj [
                                "total", num (ofKind kind).Length
                                "supported", num (supported (ofKind kind))
                            ]
                    ]
                    "objects",
                    arr [
                        for (r, unsupported) in items ->
                            obj [
                                "ref", str (ObjectRef.format r)
                                "kind", str (ObjectKind.token r.Kind)
                                "supported", bool unsupported.IsEmpty
                                "unsupported", arr (unsupported |> List.map str)
                            ]
                    ]
                    "unsupported", arr [ for (reason, n) in reasons -> obj [ "reason", str reason; "objects", num n ] ]
                ])
        else
            for (r, unsupported) in items do
                if not unsupported.IsEmpty then
                    let why = String.concat "; " unsupported
                    ctx.Out.WriteLine($"{ObjectRef.format r}  {why}")

            if items |> List.exists (fun (_, u) -> not u.IsEmpty) then
                ctx.Out.WriteLine()

            for (name, kind) in
                [
                    "measures", ObjectKind.Measure
                    "calculated columns", ObjectKind.Column
                    "calculated tables", ObjectKind.Table
                ] do
                let all = ofKind kind

                if not all.IsEmpty then
                    ctx.Out.WriteLine($"{supported all} of {all.Length} {name} can be evaluated by the local engine")

            if not reasons.IsEmpty then
                ctx.Out.WriteLine()
                ctx.Out.WriteLine "Not supported, by how many objects use it:"

                for (reason, n) in reasons do
                    ctx.Out.WriteLine($"  {n, 4}  {reason}")

        Done

/// A query on model files, evaluated by the local engine over a fixture.
let private local
    (ctx: Ctx)
    (args: ParseResults<Args>)
    (source: string)
    (directory: string)
    (dax: string)
    (maxRows: int)
    (now: Engine.Clock option)
    =
    let clock = Stopwatch.StartNew()

    match ModelIO.loadPath source with
    | Error e -> Failed e
    | Ok(src, db) ->
        match Fixture.loadAndCheck db directory with
        | Error e -> Failed e
        | Ok(loaded, issues) ->
            let errors =
                issues
                |> List.filter (fun i -> i.Severity = Validate.Severity.Error)
                |> List.length

            if errors > 0 then
                Failed(FixtureHasErrors(loaded.Directory, errors))
            else
                match
                    Engine.prepareWith
                        {
                            Engine.Options.defaults with
                                Clock = now
                        }
                        db
                        loaded.Store
                    |> Result.map (fun p ->
                        match identity args with
                        | Some id -> Engine.withIdentity id p
                        | None -> p)
                    |> Result.bind (fun p -> Engine.evaluateQuery p dax)
                with
                | Error e -> Failed e
                | Ok results ->
                    let limit = if maxRows = 0 then None else Some maxRows
                    render ctx args (src.Path) (results |> List.map (cap limit)) clock.Elapsed maxRows

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let maxRows = args.TryGetResult Args.Max_Rows |> Option.defaultValue defaultMaxRows
    let data = args.TryGetResult Args.Data
    let now = args.TryGetResult Args.Now |> Option.map Engine.Clock.parse

    // The model and the query may have options between them (`query <model> --data <dir> "<DAX>"`).
    match args.GetResults Args.Target |> List.concat with
    | [] -> UsageError "expected a model (xmla:<server>;<database>, or model files with --data) and a DAX query"
    | _ when maxRows < 0 -> UsageError "--max-rows must be 0 (all rows) or more"
    | _ when ctx.Json && args.Contains Args.Csv -> UsageError "--json and --csv can't be combined"
    | source :: _ when XmlaAddress.isRemote source && (data.IsSome || args.Contains Args.Coverage) ->
        UsageError "--data and --coverage are for model files: a server model is queried on the server"
    | source :: _ when
        (identity args).IsSome
        && (XmlaAddress.isRemote source || args.Contains Args.Coverage)
        ->
        UsageError
            "--user-name, --user-principal-name, --user-object-id, --user-culture and --custom-data are for --data: a server takes them from the connection"
    | _ when now.IsSome && (data.IsNone || args.Contains Args.Coverage) ->
        UsageError "--now sets the clock of the local engine: it goes with --data and a query"
    | _ when
        (match now with
         | Some(Error _) -> true
         | _ -> false)
        ->
        match now with
        | Some(Error message) -> UsageError $"--now: {message}"
        | _ -> UsageError "--now: not a date and time"
    | source :: rest when args.Contains Args.Coverage ->
        if not rest.IsEmpty || args.Contains Args.File then
            UsageError "--coverage takes the model only, not a query"
        else
            coverage ctx source
    | source :: _ when not (XmlaAddress.isRemote source) && data.IsNone ->
        UsageError
            $"'{source}' is model files: give the fixture data to evaluate over with --data <directory> (see `tess data init`), or query a server model (xmla:<server>;<database>)"
    | source :: rest when not (XmlaAddress.isRemote source) ->
        match readDax args rest with
        | Error outcome -> outcome
        | Ok dax when String.IsNullOrWhiteSpace dax -> UsageError "the DAX query is empty"
        | Ok dax ->
            let now =
                match now with
                | Some(Ok clock) -> Some clock
                | _ -> None

            local ctx args source data.Value dax maxRows now
    | source :: rest ->
        match readDax args rest with
        | Error outcome -> outcome
        | Ok dax when String.IsNullOrWhiteSpace dax -> UsageError "the DAX query is empty"
        | Ok dax ->
            let clock = Stopwatch.StartNew()
            let limit = if maxRows = 0 then None else Some maxRows

            match
                XmlaAddress.parse source
                |> Result.bind (fun address -> Remote.query (Source.access ctx) address dax limit)
            with
            | Error e -> Failed e
            | Ok(address, results) -> render ctx args (XmlaAddress.display address) results clock.Elapsed maxRows
