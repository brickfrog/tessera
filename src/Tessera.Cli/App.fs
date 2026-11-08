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
    /// find: no matches; diff: differences; bpa/validate: issues. See README.
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
        | InvalidObjectRef _
        | ObjectNotFound _
        | AmbiguousObject _
        | PropertyNotFound _ -> input
        | DestinationNotEmpty _
        | SaveFailed _ -> output
        | InvalidPattern _ -> usage

let version () =
    let asm = Assembly.GetExecutingAssembly()

    match asm.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | null -> string (asm.GetName().Version)
    | a -> a.InformationalVersion.Split('+').[0]

type private Outcome =
    | Done
    /// Completed, but with the "negative" result its Unix counterpart signals with exit 1
    /// (find: nothing matched; diff: models differ).
    | DoneWith of exitCode: int
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

let private runGet (ctx: Ctx) (args: ParseResults<GetArgs>) =
    let modelPath, selector = args.GetResult GetArgs.Target
    let children = args.Contains GetArgs.Children
    let wanted = args.GetResults GetArgs.Property

    let result =
        ModelIO.loadPath modelPath
        |> Result.bind (fun (_, db) ->
            Inventory.Selector.resolve (Inventory.objects db) selector
            |> Result.map (fun e -> db, e))

    match result with
    | Error e -> Failed e
    | Ok(db, e) ->
        let json = Serialize.objectJson db children e.Object

        match wanted with
        | [] ->
            if ctx.Json then
                Render.writeJson ctx.Out (Render.getJson e json)
            else
                ctx.Out.Write(Serialize.objectTmdl children e.Object)

            Done
        | paths ->
            let props =
                let own = Serialize.flatten json

                if e.Ref.Kind = ObjectKind.Model then
                    ("compatibilityLevel", string db.CompatibilityLevel) :: own
                else
                    own

            let selected = paths |> List.map (fun p -> p, Serialize.select p props)

            match selected |> List.tryFind (snd >> List.isEmpty) with
            | Some(missing, _) -> Failed(PropertyNotFound(ObjectRef.format e.Ref, missing))
            | None ->
                let leaves = selected |> List.collect snd

                if ctx.Json then
                    Render.writeJson ctx.Out (Render.propertiesJson e leaves)
                else
                    match paths, leaves with
                    // One exact property: print the bare value, for scripting.
                    | [ p ], [ (leaf, value) ] when leaf = p -> ctx.Out.WriteLine(value)
                    | _ ->
                        for leaf, value in leaves do
                            ctx.Out.WriteLine($"{leaf}: {value}")

                Done

let private parseKinds (tokens: string list) =
    let kinds = parseList tokens |> List.map (fun t -> t, ObjectKind.tryParse t)

    match kinds |> List.tryFind (fun (_, k) -> k.IsNone) with
    | Some(bad, _) ->
        let known = ObjectKind.all |> List.map ObjectKind.token |> String.concat ", "
        Error $"unknown kind '{bad}' (known: {known})"
    | None -> Ok(kinds |> List.choose snd)

let private runFind (ctx: Ctx) (args: ParseResults<FindArgs>) =
    let modelPath, text = args.GetResult FindArgs.Target

    match parseKinds (args.GetResults FindArgs.Kind) with
    | Error e -> UsageError e
    | Ok kinds ->
        let opts: Find.Options = {
            Pattern = text
            Regex = args.Contains FindArgs.Regex
            CaseSensitive = args.Contains FindArgs.Case_Sensitive
            Objects = {
                Inventory.Filter.none with
                    Kinds = kinds
                    Tables = args.GetResults FindArgs.Table
            }
            Properties = parseList (args.GetResults FindArgs.In)
        }

        match ModelIO.loadPath modelPath |> Result.bind (fun (_, db) -> Find.search db opts) with
        | Error e -> Failed e
        | Ok hits ->
            if ctx.Json then
                Render.writeJson ctx.Out (Render.findJson hits)
            else
                Render.findText ctx.Out hits

            if hits.IsEmpty then DoneWith ExitCode.findings else Done

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
                | Some(Get args) -> runGet ctx args
                | Some(Find args) -> runFind ctx args
                | _ -> UsageError(parser.Value.PrintUsage())

            match outcome with
            | Done -> ExitCode.ok
            | DoneWith code -> code
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
