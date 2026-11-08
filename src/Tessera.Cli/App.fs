/// The CLI entry logic, kept separate from Program.main so tests can drive it with captured writers.
module Tessera.Cli.App

open System
open System.IO
open System.Reflection
open Argu
open Tessera
open Tessera.Cli.Args

/// Process exit codes. Stable: scripts and CI may depend on them.
module ExitCode =
    let ok = 0
    /// Reserved for commands that report findings (bpa, diff, validate).
    let findings = 1
    let usage = 2
    let input = 3
    let output = 4
    let internalError = 70

    let ofError =
        function
        | PathNotFound _
        | UnrecognizedModel _
        | AmbiguousModel _
        | InvalidPbip _
        | RemoteModelReference _
        | LoadFailed _
        | InvalidObjectRef _ -> input
        | DestinationNotEmpty _
        | SaveFailed _ -> output

let version () =
    let asm = Assembly.GetExecutingAssembly()

    match asm.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | null -> string (asm.GetName().Version)
    | a -> a.InformationalVersion.Split('+').[0]

type private Outcome =
    | Done
    | Failed of TessError
    | UsageError of string

type private Ctx = {
    Out: TextWriter
    Err: TextWriter
    Json: bool
}

let private parseList (values: string list) =
    values
    |> List.collect (fun v ->
        v.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> List.ofArray)

let private runInfo (ctx: Ctx) (args: ParseResults<InfoArgs>) =
    match ModelIO.loadPath (args.GetResult InfoArgs.Model) with
    | Error e -> Failed e
    | Ok(source, db) ->
        let info = Inventory.info source db

        if ctx.Json then
            Render.writeJson ctx.Out (Render.infoJson info)
        else
            Render.infoText ctx.Out info

        Done

let private runList (ctx: Ctx) (args: ParseResults<ListArgs>) =
    let kindTokens = parseList (args.GetResults ListArgs.Kind)
    let kinds = kindTokens |> List.map (fun t -> t, ObjectKind.tryParse t)

    match kinds |> List.tryFind (fun (_, k) -> k.IsNone) with
    | Some(bad, _) ->
        let known = ObjectKind.all |> List.map ObjectKind.token |> String.concat ", "
        UsageError $"unknown kind '{bad}' (known: {known})"
    | None ->
        match ModelIO.loadPath (args.GetResult ListArgs.Model) with
        | Error e -> Failed e
        | Ok(_, db) ->
            let filter: Inventory.Filter = {
                Kinds = kinds |> List.choose snd
                Tables = args.GetResults ListArgs.Table
                NamePattern = args.TryGetResult ListArgs.Name
            }

            let entries = Inventory.objects db |> Inventory.Filter.apply filter

            if ctx.Json then
                Render.writeJson ctx.Out (Render.listJson entries)
            else
                Render.listText ctx.Out entries

            Done

let private runConvert (ctx: Ctx) (args: ParseResults<ConvertArgs>) =
    let source, destination = args.GetResult ConvertArgs.Paths

    let format =
        match args.TryGetResult ConvertArgs.Format with
        | None -> Ok(ModelIO.OutputFormat.infer destination)
        | Some f ->
            ModelIO.OutputFormat.tryParse f
            |> Option.map Ok
            |> Option.defaultValue (Error $"unknown format '{f}' (expected bim, tmdl or pbip)")

    // None = auto: decided once the source is resolved (see Eol.resolveAuto).
    let eol =
        match args.TryGetResult ConvertArgs.Eol |> Option.map (fun s -> s.ToLowerInvariant()) with
        | None
        | Some "auto" -> Ok None
        | Some "lf" -> Ok(Some ModelIO.Lf)
        | Some "crlf" -> Ok(Some ModelIO.CrLf)
        | Some other -> Error $"unknown line ending '{other}' (expected auto, lf or crlf)"

    let indent =
        match args.TryGetResult ConvertArgs.Indent_Spaces with
        | Some n when n < 1 || n > 16 -> Error "--indent-spaces must be between 1 and 16"
        | other -> Ok other

    match format, eol, indent with
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e -> UsageError e
    | Ok format, Ok eol, Ok indent ->
        let result =
            ModelIO.loadPath source
            |> Result.bind (fun (src, db) ->
                let opts: ModelIO.SaveOptions = {
                    Eol =
                        eol
                        |> Option.defaultWith (fun () -> ModelIO.Eol.resolveAuto src.Path destination)
                    IncludeSensitive = args.Contains ConvertArgs.Include_Sensitive
                    IndentSpaces = indent
                }

                ModelIO.save format opts (args.Contains ConvertArgs.Force) db destination
                |> Result.map (fun saved -> src, saved))

        match result with
        | Error e -> Failed e
        | Ok(src, saved) ->
            if ctx.Json then
                Render.writeJson ctx.Out (Render.convertJson src saved)
            else
                Render.convertText ctx.Out src saved

            Done

let private parser = lazy (ArgumentParser.Create<CliArgs>(programName = "tess"))

/// Run the CLI. Returns the process exit code.
let run (out: TextWriter) (err: TextWriter) (argv: string array) : int =
    // Decide JSON mode before parsing so usage errors can be reported as JSON too.
    let jsonRequested = argv |> Array.exists (fun a -> a = "--json" || a = "-j")

    let usageError (message: string) =
        if jsonRequested then
            Render.writeJson out (Render.errorJson "usage" message)
        else
            err.WriteLine(message)

        ExitCode.usage

    try
        let results = parser.Value.ParseCommandLine(argv, raiseOnUsage = true)

        let ctx = {
            Out = out
            Err = err
            Json = results.Contains Json
        }

        if results.Contains Version then
            if ctx.Json then
                Render.writeJson out (Render.versionJson (version ()))
            else
                out.WriteLine($"tess {version ()}")

            ExitCode.ok
        else
            let outcome =
                match results.TryGetSubCommand() with
                | Some(Convert args) -> runConvert ctx args
                | Some(Info args) -> runInfo ctx args
                | Some(List args) -> runList ctx args
                | _ -> UsageError(parser.Value.PrintUsage())

            match outcome with
            | Done -> ExitCode.ok
            | UsageError message -> usageError message
            | Failed e ->
                if ctx.Json then
                    Render.writeJson out (Render.tessErrorJson e)
                else
                    err.WriteLine($"tess: {TessError.message e}")

                ExitCode.ofError e
    with
    | :? ArguParseException as e when e.ErrorCode = ErrorCode.HelpText ->
        out.WriteLine(e.Message)
        ExitCode.ok
    | :? ArguParseException as e -> usageError e.Message
    | e ->
        if jsonRequested then
            Render.writeJson out (Render.errorJson "internal" e.Message)
        else
            err.WriteLine($"tess: internal error: {e}")

        ExitCode.internalError
