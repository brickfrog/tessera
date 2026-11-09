/// `tess format`: a local DAX formatter, for loose DAX (stdin, --expr, a file) and for the DAX in a model.
module Tessera.Cli.FormatCommand

open System
open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "format DAX (SQLBI style, offline): stdin, --expr, a .dax file, or a model's expressions (--check: exit 1 if any would change)."

type Args =
    | [<MainCommand>] Targets of input_or_model_and_objects: string list
    | [<AltCommandLine("-e")>] Expr of dax: string
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | Long
    | Width of columns: int
    | No_Space_After_Function
    | Semicolons
    | Check
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Targets _ ->
                "`-` (DAX on stdin), a DAX file, or a model followed by the objects to format (any form `get` "
                + "accepts; a table stands for every expression in it). Files with a model extension (.bim, .pbip, "
                + ".pbism, .pbir, .tmdl) and folders are models; other files are DAX."
            | Expr _ -> "format this DAX instead of a file."
            | Kind _ -> "model: only expressions of these kinds of object (repeatable or comma-separated)."
            | Table _ -> "model: only expressions in this table (repeatable)."
            | Long -> "put the arguments of every call with two or more arguments on lines of their own."
            | Width _ -> $"the line width to fit within (default {Format.FormatOptions.defaults.Width})."
            | No_Space_After_Function -> "write `SUM( x )` rather than `SUM ( x )`."
            | Semicolons ->
                "loose DAX only: the input separates arguments with ';' and writes decimals with ','; so does the output."
            | Check -> "change nothing; exit 1 if anything isn't formatted, and list it."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let private modelExtensions = [ ".bim"; ".pbip"; ".pbism"; ".pbir"; ".tmdl"; ".tmd" ]

/// A path names DAX (rather than a model) if it's a file without a model extension.
let isDaxFile (path: string) =
    File.Exists path
    && not (
        modelExtensions
        |> List.exists (fun e -> String.Equals(Path.GetExtension path, e, StringComparison.OrdinalIgnoreCase))
    )

let private problemJson (p: Format.Problem) =
    obj [ "line", num p.Line; "column", num p.Column; "message", str p.Message ]

let private errorsOf =
    function
    | Format.Syntax problems -> problems
    | Format.Unsafe reason ->
        let p: Format.Problem = {
            Line = 1
            Column = 1
            Message = $"refused to format: {reason} (a tessera bug; please report it)"
        }

        [ p ]

let private exitOf =
    function
    | Format.Syntax _ -> ExitCode.input
    | Format.Unsafe _ -> ExitCode.internalError

/// Format loose DAX: print it (or with --check, say whether it's formatted).
let private loose (ctx: Ctx) (opts: Format.FormatOptions) (check: bool) (name: string) (dax: string) =
    let result = Format.format opts dax

    if ctx.Json then
        let o =
            match result with
            | Ok formatted -> obj [ "success", bool true; "formatted", str formatted; "errors", arr [] ] :?> JsonObject
            | Error e ->
                obj [
                    "success", bool false
                    "formatted", null
                    "errors", arr (errorsOf e |> List.map problemJson)
                ]
                :?> JsonObject

        if check then
            o.["changed"] <-
                match result with
                | Ok formatted -> bool (formatted <> dax)
                | Error _ -> null

        writeJson ctx.Out o
    else
        match result with
        | Ok formatted when check ->
            if formatted <> dax then
                ctx.Err.WriteLine($"{name} is not formatted.")
        | Ok formatted -> ctx.Out.WriteLine(formatted)
        | Error e ->
            for p in errorsOf e do
                ctx.Err.WriteLine($"tess: {name}: line {p.Line}, column {p.Column}: {p.Message}")

    match result with
    | Error e -> DoneWith(exitOf e)
    | Ok formatted when check && formatted <> dax -> DoneWith ExitCode.findings
    | Ok _ -> Done

let private describe (o: Format.Outcome) =
    $"{ObjectRef.format o.Source.Owner.Ref} {o.Source.Property}"

let private warnings (result: Format.ModelFormat) = [
    for s in result.WithoutDax do
        $"{s} has no DAX expression to format"

    for o in result.Outcomes do
        match o.Result with
        | Error e -> $"{describe o}: not formatted: {Format.FormatError.message e}"
        | Ok _ -> ()
]

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private outcomeJson (o: Format.Outcome) =
    obj [
        "ref", str (ObjectRef.format o.Source.Owner.Ref)
        "kind", str (ObjectKind.token o.Source.Owner.Ref.Kind)
        "property", str o.Source.Property
    ]

/// --check on a model: a report; nothing is changed.
let private check (ctx: Ctx) (result: Format.ModelFormat) =
    let changed = result.Outcomes |> List.filter Format.Outcome.changed
    let failed = result.Outcomes |> List.filter (fun o -> Result.isError o.Result)

    if ctx.Json then
        writeJson
            ctx.Out
            (obj [
                "command", str "format"
                "check", bool true
                "expressions", num result.Outcomes.Length
                "unformatted", arr (changed |> List.map outcomeJson)
                "failed",
                arr (
                    failed
                    |> List.map (fun o ->
                        let j = outcomeJson o :?> JsonObject

                        j.["errors"] <-
                            match o.Result with
                            | Error e -> arr (errorsOf e |> List.map problemJson)
                            | Ok _ -> arr []

                        j :> JsonNode)
                )
                "warnings", arr (warnings result |> List.map str)
            ])
    else
        for o in changed do
            ctx.Out.WriteLine(describe o)

        for w in warnings result do
            ctx.Err.WriteLine($"tess: warning: {w}")

        match changed.Length with
        | 0 -> ctx.Err.WriteLine($"""All {plural result.Outcomes.Length "expression"} are formatted.""")
        | n ->
            ctx.Err.WriteLine(
                $"""{plural n "expression"} of {result.Outcomes.Length} would be reformatted; `tess format` without --check shows how."""
            )

    if changed.IsEmpty then Done else DoneWith ExitCode.findings

let private effect (result: Format.ModelFormat) : Tessera.Mutation.Effect =
    let changed = result.Outcomes |> List.filter Format.Outcome.changed |> List.length

    {
        Tessera.Mutation.Effect.none with
            Notes = [
                if changed = 0 then
                    $"""Nothing to format: {plural result.Outcomes.Length "expression"} already formatted."""
                else
                    $"""Formatted {plural changed "expression"} of {result.Outcomes.Length}."""
            ]
            Warnings = warnings result
    }

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let targets = args.GetResult(Args.Targets, defaultValue = [])
    let expr = args.TryGetResult Args.Expr
    let checking = args.Contains Args.Check

    let opts: Format.FormatOptions = {
        Width = args.GetResult(Args.Width, defaultValue = Format.FormatOptions.defaults.Width)
        Long = args.Contains Args.Long
        SpaceAfterFunction = not (args.Contains Args.No_Space_After_Function)
        Semicolons = args.Contains Args.Semicolons
    }

    let editSwitches =
        [
            args.Contains Args.Save
            args.Contains Args.Save_To
            args.Contains Args.Force
            args.Contains Args.Stat
            args.Contains Args.Name_Only
        ]
        |> List.exists id

    let modelOnly = args.Contains Args.Kind || args.Contains Args.Table || editSwitches

    let isLoose =
        expr.IsSome
        || (match targets with
            | [ "-" ]
            | [] -> true
            | [ path ] -> isDaxFile path
            | _ -> false)

    match targets with
    | _ when opts.Width < 1 -> UsageError "--width must be at least 1"
    | _ :: _ when expr.IsSome -> UsageError "--expr formats the DAX it's given; it doesn't take a file or model"
    | [] when expr.IsNone -> UsageError "expected DAX to format: `-` (stdin), --expr \"<dax>\", a DAX file, or a model"
    | _ when isLoose && modelOnly ->
        UsageError "-k, -t, --save, --save-to, --force, --stat and --name-only are for models, not loose DAX"
    | _ when isLoose ->
        let input =
            match expr, targets with
            | Some dax, _ -> Ok("--expr", dax)
            | None, [ "-" ] -> Ok("<stdin>", Console.In.ReadToEnd())
            | None, path :: _ ->
                try
                    Ok(path, File.ReadAllText path)
                with e ->
                    Error(LoadFailed(path, e.Message))
            | None, [] -> Error(PathNotFound "-")

        match input with
        | Error e -> Failed e
        | Ok(name, dax) -> loose ctx opts checking name dax
    | model :: selectors ->
        let selection =
            parseKinds (args.GetResults Args.Kind)
            |> Result.map (fun kinds -> {
                Format.Selection.Selectors = selectors
                Format.Selection.Kinds = kinds
                Format.Selection.Tables = args.GetResults Args.Table
            })

        match selection with
        | Error message -> UsageError message
        | Ok _ when opts.Semicolons ->
            UsageError "--semicolons is for loose DAX; a model's DAX always separates arguments with ','"
        | Ok _ when checking && editSwitches ->
            UsageError "--check only reports; it doesn't combine with --save, --save-to, --force, --stat or --name-only"
        | Ok selection when checking ->
            match ModelIO.loadPath model with
            | Error e -> Failed e
            | Ok(_, db) ->
                match Format.formatModel opts db selection with
                | Error e -> Failed e
                | Ok result -> check ctx result
        | Ok selection ->
            let mutationOpts =
                Mutation.options
                    (args.Contains Args.Save)
                    (args.TryGetResult Args.Save_To)
                    (args.Contains Args.Force)
                    (args.Contains Args.Stat)
                    (args.Contains Args.Name_Only)

            match mutationOpts with
            | Error message -> UsageError message
            | Ok mutationOpts ->
                Mutation.run ctx "format" mutationOpts model (fun db ->
                    Format.formatModel opts db selection
                    |> Result.map (fun result ->
                        Format.apply result.Outcomes
                        effect result))
    | [] -> UsageError "expected DAX to format"
