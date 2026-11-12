module Tessera.Cli.DataCommand

open System
open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Validate
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "fixture data for the local DAX engine: create (init), check, summarize (info) or embed in a model a directory of CSV/JSON files."

type InitArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * directory: string
    | Rows of count: int
    | Seed of seed: int64
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the directory to write <Table>.csv files to."
            | Rows _ ->
                "write this many rows of synthetic data per table, with keys that satisfy the relationships. "
                + "Default: 0, the header row only."
            | Seed _ -> "seed for the synthetic data; the same model, rows and seed give the same files. Default: 1."
            | Force -> "replace the files that are already there."

type CheckArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * directory: string
    | Min_Severity of level: string
    | Fail_On of level: string
    | Rule of rules: string
    | Ci of system: string
    | Sarif of file: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the fixture directory."
            | Sarif _ -> Tessera.Cli.Sarif.usage
            | Min_Severity _ -> "only show issues this severe or more: error, warning or info (default: info)."
            | Fail_On _ ->
                "exit 1 if any issue is this severe or more, shown or not: error, warning or info (default: error)."
            | Rule _ ->
                let ids = Fixture.rules |> List.map fst |> String.concat ", "
                $"only these rules (repeatable or comma-separated): {ids}."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."

type InfoArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * directory: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the fixture directory."

type EmbedArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * directory: string * destination: string
    | [<AltCommandLine("-f")>] Format of format: string
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, the fixture directory, and where to write the model with the data inside it."
            | Format _ ->
                "output format: bim | tmdl | pbip | database.json. Default: inferred from the destination "
                + "(*.bim -> bim, *.SemanticModel -> pbip, otherwise a tmdl folder)."
            | Force -> "overwrite a non-empty destination."

type Args =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>
    | [<CliPrefix(CliPrefix.None)>] Check of ParseResults<CheckArgs>
    | [<CliPrefix(CliPrefix.None)>] Info of ParseResults<InfoArgs>
    | [<CliPrefix(CliPrefix.None)>] Embed of ParseResults<EmbedArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init _ ->
                "write a CSV file per table: the header row, or --rows N rows of synthetic data "
                + "(refuses to overwrite without --force)."
            | Check _ ->
                "check a fixture against the model: files, columns, data types, keys and relationships "
                + "(exit 1: errors)."
            | Info _ -> "list the fixture's tables with row counts, and distinct and BLANK counts per column."
            | Embed _ ->
                "write a copy of the model whose loaded tables are calculated tables holding the fixture's rows, "
                + "so it can be deployed with no data source (refuses to overwrite without --force)."

// ---- init --------------------------------------------------------------------------------------

let private init (ctx: Ctx) (args: ParseResults<InitArgs>) =
    let model, directory = args.GetResult InitArgs.Target
    let rows = args.GetResult(InitArgs.Rows, 0)
    let seed = args.GetResult(InitArgs.Seed, FixtureInit.Options.defaults.Seed)

    if rows < 0 then
        UsageError "--rows must not be negative"
    else
        match ModelIO.loadPath model with
        | Error e -> Failed e
        | Ok(_, db) ->
            let opts: FixtureInit.Options = { Rows = rows; Seed = seed }

            match FixtureInit.write db directory opts (args.Contains InitArgs.Force) with
            | Error e -> Failed e
            | Ok written ->
                let full = Path.GetFullPath directory

                let computed = [
                    for t in db.Model.Tables do
                        if Fixture.isComputed t then
                            t.Name
                ]

                if ctx.Json then
                    writeJson
                        ctx.Out
                        (obj [
                            "command", str "data init"
                            "directory", str full
                            "rows", num rows
                            "seed", JsonValue.Create seed
                            "files",
                            arr [
                                for w in written ->
                                    obj [
                                        "table", str w.Table
                                        "path", str w.Path
                                        "rows", num w.Rows
                                        "columns", num w.Columns
                                    ]
                            ]
                            "computed", arr (computed |> List.map str)
                        ])
                else
                    let files =
                        if written.Length = 1 then
                            "1 file"
                        else
                            $"{written.Length} files"

                    let what =
                        if rows = 0 then
                            "headers only"
                        else
                            $"up to {rows} rows each, seed {seed}"

                    ctx.Out.WriteLine($"Wrote {files} to {full} ({what})")

                    let width =
                        written
                        |> List.map (fun w -> (Path.GetFileName w.Path).Length)
                        |> List.fold max 0

                    for w in written do
                        let r = if w.Rows = 1 then "1 row" else $"{w.Rows} rows"
                        let c = if w.Columns = 1 then "1 column" else $"{w.Columns} columns"
                        ctx.Out.WriteLine($"  {(Path.GetFileName w.Path).PadRight width}  {r}, {c}")

                    if not computed.IsEmpty then
                        let names = String.concat ", " computed
                        ctx.Out.WriteLine($"No file for tables the engine computes: {names}")

                Done

// ---- check -------------------------------------------------------------------------------------

let private severity =
    function
    | Severity.Error -> Findings.Severity.Error
    | Severity.Warning -> Findings.Severity.Warning
    | Severity.Info -> Findings.Severity.Info

/// "Sales.csv:12", "Sales.csv" or "".
let private location (i: FixtureIssue) =
    match i.File, i.Line with
    | Some f, Some l -> $"{Path.GetFileName f}:{l}"
    | Some f, None -> Path.GetFileName f
    | None, _ -> ""

/// An issue as a finding in the shared envelope, with its extra keys: file, line, columnName.
let toFinding (i: FixtureIssue) : Findings.Finding * (string * JsonNode) list =
    let message =
        match location i with
        | "" -> i.Message
        | at -> $"{at}: {i.Message}"

    Findings.about i.Ref (severity i.Severity) "data" i.Rule message,
    [
        match i.File with
        | Some f -> "file", str f
        | None -> ()
        match i.Line with
        | Some l -> "line", num l
        | None -> ()
        match i.Field with
        | Some c -> "columnName", str c
        | None -> ()
    ]

let private tablesJson (loaded: Fixture.Loaded) =
    arr [
        for t in loaded.Tables -> obj [ "table", str t.Table; "file", optStr t.File; "rows", num t.Rows ]
    ]

/// The findings envelope, plus "valid" (no errors among all the issues), the directory and its tables.
let checkJson (duration: TimeSpan) (loaded: Fixture.Loaded) (all: FixtureIssue list) (shown: FixtureIssue list) =
    Findings.envelopeWith "data check" duration (shown |> List.map toFinding) [
        "valid", bool (not (Fixture.hasErrors all))
        "directory", str loaded.Directory
        "tables", tablesJson loaded
    ]

/// severity  ref  file:line  message  [rule], aligned; then a summary line.
let checkText (out: TextWriter) (issues: FixtureIssue list) =
    let width f =
        issues |> List.map (fun i -> String.length (f i)) |> List.fold max 0

    let severityWidth = width (fun i -> Severity.token i.Severity)
    let refWidth = width (fun i -> ObjectRef.format i.Ref)
    let locationWidth = width location

    for i in issues do
        let columns = [
            (Severity.token i.Severity).PadRight severityWidth
            (ObjectRef.format i.Ref).PadRight refWidth
            if locationWidth > 0 then
                (location i).PadRight locationWidth
            i.Message
            $"[{i.Rule}]"
        ]

        out.WriteLine(String.concat "  " columns)

    let summary =
        issues
        |> List.map (fun i -> fst (toFinding i))
        |> Findings.summarize
        |> Findings.summaryText

    out.WriteLine(if summary = "" then "No issues." else summary)

/// The issues as SARIF: an issue in a fixture file points at its line there; any other at the
/// model object it is about.
let private checkSarif (model: string) (source: ModelSource) (selected: string list) (shown: FixtureIssue list) =
    let index = SourceLocations.build source

    let items = [
        for i in shown ->
            let finding = fst (toFinding i)

            match i.File with
            | Some file -> {
                Sarif.Item.Finding = finding
                Sarif.Item.Location =
                    Some(
                        {
                            SourceLocations.File = file
                            SourceLocations.Line = i.Line
                        },
                        i.Line.IsSome
                    )
              }
            | None -> List.head (Sarif.modelItems index [ finding ])
    ]

    let rules = [
        for id, description in Fixture.rules do
            if selected.IsEmpty || List.contains id selected then
                Sarif.Rule.simple id description
    ]

    Sarif.log (Sarif.rootFor model) rules items []

let private check (ctx: Ctx) (args: ParseResults<CheckArgs>) =
    let model, directory = args.GetResult CheckArgs.Target
    let selected = parseList (args.GetResults CheckArgs.Rule)
    let known = Fixture.rules |> List.map fst

    let options =
        match selected |> List.tryFind (fun r -> not (List.contains r known)) with
        | Some bad -> Error(unknownValue "--rule" "rule" bad known)
        | None ->
            ValidateCommand.severityArgWith "--min-severity" (args.TryGetResult CheckArgs.Min_Severity) Severity.Info
            |> Result.bind (fun minimum ->
                ValidateCommand.severityArgWith "--fail-on" (args.TryGetResult CheckArgs.Fail_On) Severity.Error
                |> Result.map (fun failOn -> minimum, failOn))

    match options, ValidateCommand.ciArgWith (args.TryGetResult CheckArgs.Ci) with
    | Error problem, _
    | _, Error problem -> usage problem
    | Ok(minimum, failOn), Ok ci ->
        let clock = Diagnostics.Stopwatch.StartNew()

        match ModelIO.loadPath model with
        | Error e -> Failed e
        | Ok(source, db) ->
            let checkedData =
                Fixture.loadAndCheck db directory
                |> Result.bind (fun (loaded, issues) ->
                    let issues =
                        issues
                        |> List.filter (fun i -> selected.IsEmpty || List.contains i.Rule selected)

                    let shown = issues |> List.filter (fun i -> Severity.atLeast minimum i.Severity)

                    match args.TryGetResult CheckArgs.Sarif with
                    | Some path ->
                        Sarif.write path (checkSarif model source selected shown)
                        |> Result.map (fun () -> loaded, issues, shown)
                    | None -> Ok(loaded, issues, shown))

            match checkedData with
            | Error e -> Failed e
            | Ok(loaded, issues, shown) ->

                if ctx.Json then
                    writeJson ctx.Out (checkJson clock.Elapsed loaded issues shown)
                else
                    checkText ctx.Out shown

                Findings.annotate ci ctx.Err (shown |> List.map (toFinding >> fst))

                // --fail-on looks at every selected issue, so hiding warnings can't turn a failing check green.
                if issues |> List.exists (fun i -> Severity.atLeast failOn i.Severity) then
                    DoneWith ExitCode.findings
                else
                    Done

// ---- info --------------------------------------------------------------------------------------

let private info (ctx: Ctx) (args: ParseResults<InfoArgs>) =
    let model, directory = args.GetResult InfoArgs.Target

    match ModelIO.loadPath model with
    | Error e -> Failed e
    | Ok(_, db) ->
        match Fixture.load db directory with
        | Error e -> Failed e
        | Ok loaded ->
            let tables =
                loaded.Tables
                |> List.choose (fun t -> loaded.Store.TryTable t.Table |> Option.map (fun st -> t, st))

            let blanks (c: StoreColumn) =
                if c.BlankId < 0 then 0 else c.CountOfId c.BlankId

            let errors =
                loaded.Issues
                |> List.filter (fun i -> i.Severity = Severity.Error)
                |> List.length

            let warnings =
                loaded.Issues
                |> List.filter (fun i -> i.Severity = Severity.Warning)
                |> List.length

            if ctx.Json then
                writeJson
                    ctx.Out
                    (obj [
                        "command", str "data info"
                        "directory", str loaded.Directory
                        "tables",
                        arr [
                            for t, st in tables ->
                                obj [
                                    "table", str t.Table
                                    "ref", str (ObjectRef.format (ObjectRef.table t.Table))
                                    "file", optStr t.File
                                    "rows", num t.Rows
                                    "columns",
                                    arr [
                                        for c in st.Columns ->
                                            obj [
                                                "name", str c.Name
                                                "ref",
                                                str (
                                                    ObjectRef.format (
                                                        ObjectRef.inTable ObjectKind.Column t.Table c.Name
                                                    )
                                                )
                                                "dataType", str (DaxType.token c.DataType)
                                                "distinct", num c.DistinctCount
                                                "blanks", num (blanks c)
                                            ]
                                    ]
                                ]
                        ]
                        "computed", arr (loaded.Computed |> List.map str)
                        "issues", obj [ "errors", num errors; "warnings", num warnings ]
                    ])
            else
                for t, st in tables do
                    let file =
                        match t.File with
                        | Some f -> Path.GetFileName f
                        | None -> "(no file)"

                    let rows = if t.Rows = 1 then "1 row" else $"{t.Rows} rows"
                    ctx.Out.WriteLine($"{t.Table}  {file}  {rows}")

                    let nameWidth = st.Columns |> Array.map (fun c -> c.Name.Length) |> Array.fold max 0

                    let typeWidth =
                        st.Columns
                        |> Array.map (fun c -> (DaxType.token c.DataType).Length)
                        |> Array.fold max 0

                    for c in st.Columns do
                        let blank =
                            match blanks c with
                            | 0 -> ""
                            | n -> $", {n} blank"

                        ctx.Out.WriteLine(
                            $"  {c.Name.PadRight nameWidth}  {(DaxType.token c.DataType).PadRight typeWidth}  {c.DistinctCount} distinct{blank}"
                        )

                if not loaded.Computed.IsEmpty then
                    let names = String.concat ", " loaded.Computed
                    ctx.Out.WriteLine($"Computed by the engine, not loaded: {names}")

            if errors > 0 && not ctx.Json then
                let n = if errors = 1 then "1 error" else $"{errors} errors"

                ctx.Err.WriteLine(
                    $"tess: {n} in the fixture (invalid values show as BLANK); run `tess data check` for details"
                )

            Done

// ---- embed -------------------------------------------------------------------------------------

let private embed (ctx: Ctx) (args: ParseResults<EmbedArgs>) =
    let model, directory, destination = args.GetResult EmbedArgs.Target

    let format =
        match args.TryGetResult EmbedArgs.Format with
        | None -> Ok(ModelIO.OutputFormat.infer destination)
        | Some f ->
            ModelIO.OutputFormat.tryParse f
            |> Option.map Ok
            |> Option.defaultValue (
                Error
                    $"""unknown format '{f}'{Suggest.hint (Suggest.closest ModelIO.OutputFormat.tokens f)} (expected {String.Join(", ", ModelIO.OutputFormat.tokens)})"""
            )

    match format with
    | Error message -> UsageError message
    | Ok format ->
        let result =
            ModelIO.loadPath model
            |> Result.bind (fun (source, db) ->
                Fixture.loadAndCheck db directory
                |> Result.bind (fun (loaded, issues) ->
                    // Invalid values are BLANK in the store: embedding them would deploy other data
                    // than the fixture says.
                    match issues |> List.filter (fun i -> i.Severity = Severity.Error) with
                    | [] -> Ok loaded
                    | errors ->
                        let n =
                            if errors.Length = 1 then
                                "1 error"
                            else
                                $"{errors.Length} errors"

                        Error(InvalidFixture(loaded.Directory, $"it has {n}; run `tess data check` to see them"))
                    |> Result.bind (fun loaded ->
                        FixtureEmbed.embed db loaded.Store
                        |> Result.bind (fun embedded ->
                            let opts = {
                                ModelIO.SaveOptions.defaults with
                                    Eol = ModelIO.Eol.resolveAuto source.Path destination
                            }

                            ModelIO.save format opts (args.Contains EmbedArgs.Force) embedded.Database destination
                            |> Result.map (fun saved -> loaded, embedded, saved)))))

        match result with
        | Error e -> Failed e
        | Ok(loaded, embedded, saved) ->
            if ctx.Json then
                writeJson
                    ctx.Out
                    (obj [
                        "command", str "data embed"
                        "directory", str loaded.Directory
                        "output",
                        obj [
                            "format", str (ModelIO.OutputFormat.token saved.Format)
                            "path", str saved.Path
                            "files", arr [ for f in saved.Files -> str f ]
                        ]
                        "tables",
                        arr [
                            for t in embedded.Tables ->
                                obj [ "table", str t.Table; "rows", num t.Rows; "columns", num t.Columns ]
                        ]
                        "warnings", arr (embedded.Warnings |> List.map str)
                    ])
            else
                let count = embedded.Tables.Length
                let tables = if count = 1 then "1 table" else $"{count} tables"

                ctx.Out.WriteLine(
                    $"Wrote {ModelIO.OutputFormat.token saved.Format}  {saved.Path}  ({tables} now hold their fixture rows)"
                )

                let width = embedded.Tables |> List.map (fun t -> t.Table.Length) |> List.fold max 0

                for t in embedded.Tables do
                    let rows = if t.Rows = 1 then "1 row" else $"{t.Rows} rows"
                    ctx.Out.WriteLine($"  {t.Table.PadRight width}  {rows}")

                for w in embedded.Warnings do
                    ctx.Err.WriteLine($"warning: {w}")

            Done

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetSubCommand() with
    | Some(Init a) -> init ctx a
    | Some(Check a) -> check ctx a
    | Some(Info a) -> info ctx a
    | Some(Embed a) -> embed ctx a
    | None -> UsageError(args.Parser.PrintUsage())
