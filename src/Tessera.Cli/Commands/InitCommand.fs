module Tessera.Cli.InitCommand

open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "create a new, empty semantic model."

type Args =
    | [<MainCommand; ExactlyOnce>] Destination of path: string
    | [<AltCommandLine("-n")>] Name of name: string
    | Compatibility_Level of level: int
    | [<AltCommandLine("-f")>] Format of format: string
    | Culture of culture: string
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Destination _ -> "where to create the model (X.SemanticModel folder, .bim file or TMDL folder)."
            | Name _ -> "model name. Default: the folder or file name, without .SemanticModel or .bim."
            | Compatibility_Level _ ->
                $"compatibility level (at least {Scaffold.minCompatibilityLevel}). "
                + $"Default: {Scaffold.defaultCompatibilityLevel}."
            | Format _ ->
                "output format: bim | tmdl | pbip | database.json. Default: inferred from the path "
                + "(*.bim -> bim, *.SemanticModel -> pbip, otherwise a tmdl folder)."
            | Culture _ -> $"model culture, also used as the source query culture. Default: {Scaffold.defaultCulture}."
            | Force -> "overwrite a non-empty destination."

let initJson (opts: Scaffold.Options) (saved: ModelIO.SavedModel) =
    obj [
        "model",
        obj [
            "name", str opts.Name
            "compatibilityLevel", num opts.CompatibilityLevel
            "culture", str opts.Culture
        ]
        "output",
        obj [
            "format", str (ModelIO.OutputFormat.token saved.Format)
            "path", str saved.Path
            "eol", str (ModelIO.Eol.token saved.Eol)
            "files", arr [ for f in saved.Files -> str f ]
        ]
    ]

let initText (out: TextWriter) (opts: Scaffold.Options) (saved: ModelIO.SavedModel) =
    let n = saved.Files.Length
    let files = if n = 1 then "1 file" else $"{n} files"
    out.WriteLine($"Created {ModelIO.OutputFormat.token saved.Format}  {saved.Path}  ({files})")
    out.WriteLine($"  model {opts.Name}, compatibility level {opts.CompatibilityLevel}, culture {opts.Culture}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let path = args.GetResult Args.Destination
    let defaults = Scaffold.defaults path

    let format =
        match args.TryGetResult Args.Format with
        | None -> Ok(ModelIO.OutputFormat.infer path)
        | Some f ->
            ModelIO.OutputFormat.tryParse f
            |> Option.map Ok
            |> Option.defaultValue (
                Error
                    $"""unknown format '{f}'{Suggest.hint (Suggest.closest ModelIO.OutputFormat.tokens f)} (expected {System.String.Join(", ", ModelIO.OutputFormat.tokens)})"""
            )

    let name =
        match args.TryGetResult Args.Name with
        | None -> Ok defaults.Name
        | Some n when System.String.IsNullOrWhiteSpace n -> Error "--name must not be empty"
        | Some n -> Ok(n.Trim())

    let level =
        match args.TryGetResult Args.Compatibility_Level with
        | None -> Ok defaults.CompatibilityLevel
        | Some l when l < Scaffold.minCompatibilityLevel ->
            Error $"--compatibility-level must be at least {Scaffold.minCompatibilityLevel}"
        | Some l -> Ok l

    let culture =
        match args.TryGetResult Args.Culture with
        | None -> Ok defaults.Culture
        | Some c ->
            Scaffold.tryCulture c
            |> Option.map Ok
            |> Option.defaultValue (Error $"unknown or non-specific culture '{c}' (expected e.g. en-US)")

    match format, name, level, culture with
    | Error e, _, _, _
    | _, Error e, _, _
    | _, _, Error e, _
    | _, _, _, Error e -> UsageError e
    | Ok format, Ok name, Ok level, Ok culture ->
        let opts: Scaffold.Options = {
            Name = name
            CompatibilityLevel = level
            Culture = culture
        }

        match Scaffold.init format opts (args.Contains Args.Force) path with
        | Error e -> Failed e
        | Ok saved ->
            if ctx.Json then
                writeJson ctx.Out (initJson opts saved)
            else
                initText ctx.Out opts saved

            Done
