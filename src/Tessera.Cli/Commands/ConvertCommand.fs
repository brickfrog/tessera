module Tessera.Cli.ConvertCommand

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Inventory
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "convert a model between .bim, TMDL and PBIP."

type Args =
    | [<MainCommand; ExactlyOnce>] Paths of source: string * destination: string
    | [<AltCommandLine("-f")>] Format of format: string
    | Force
    | Eol of eol: string
    | Indent_Spaces of spaces: int
    | Include_Sensitive

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Paths _ -> "model to read, and where to write it."
            | Format _ ->
                "output format: bim | tmdl | pbip. Default: inferred from the destination "
                + "(*.bim -> bim, *.SemanticModel -> pbip, otherwise a tmdl folder)."
            | Force -> "overwrite a non-empty destination."
            | Eol _ ->
                "line endings: auto (default: match files already at the destination, else the source, else lf) "
                + "| lf | crlf."
            | Indent_Spaces _ -> "indent TMDL with this many spaces instead of tabs."
            | Include_Sensitive -> "keep restricted information such as data source passwords."

let convertJson (source: ModelSource) (saved: ModelIO.SavedModel) =
    obj [
        "source", sourceJson source
        "output",
        obj [
            "format", str (ModelIO.OutputFormat.token saved.Format)
            "path", str saved.Path
            "eol", str (ModelIO.Eol.token saved.Eol)
            "files", arr [ for f in saved.Files -> str f ]
        ]
    ]

let convertText (out: TextWriter) (source: ModelSource) (saved: ModelIO.SavedModel) =
    let n = saved.Files.Length
    let files = if n = 1 then "1 file" else $"{n} files"
    out.WriteLine($"Read  {ModelFormat.token source.Format}  {source.Path}")

    out.WriteLine(
        $"Wrote {ModelIO.OutputFormat.token saved.Format}  {saved.Path}  ({files}, {ModelIO.Eol.token saved.Eol})"
    )

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let source, destination = args.GetResult Args.Paths

    let format =
        match args.TryGetResult Args.Format with
        | None -> Ok(ModelIO.OutputFormat.infer destination)
        | Some f ->
            ModelIO.OutputFormat.tryParse f
            |> Option.map Ok
            |> Option.defaultValue (Error $"unknown format '{f}' (expected bim, tmdl or pbip)")

    // None = auto: decided once the source is resolved (see Eol.resolveAuto).
    let eol =
        match args.TryGetResult Args.Eol |> Option.map (fun s -> s.ToLowerInvariant()) with
        | None
        | Some "auto" -> Ok None
        | Some "lf" -> Ok(Some ModelIO.Lf)
        | Some "crlf" -> Ok(Some ModelIO.CrLf)
        | Some other -> Error $"unknown line ending '{other}' (expected auto, lf or crlf)"

    let indent =
        match args.TryGetResult Args.Indent_Spaces with
        | Some n when n < 1 || n > 16 -> Error "--indent-spaces must be between 1 and 16"
        | other -> Ok other

    match format, eol, indent with
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e -> UsageError e
    | Ok format, Ok eol, Ok indent ->
        let result =
            Source.load ctx source
            |> Result.bind (fun (src, db) ->
                let opts: ModelIO.SaveOptions = {
                    Eol =
                        eol
                        |> Option.defaultWith (fun () -> ModelIO.Eol.resolveAuto src.Path destination)
                    IncludeSensitive = args.Contains Args.Include_Sensitive
                    IndentSpaces = indent
                }

                ModelIO.save format opts (args.Contains Args.Force) db destination
                |> Result.map (fun saved -> src, saved))

        match result with
        | Error e -> Failed e
        | Ok(src, saved) ->
            if ctx.Json then
                writeJson ctx.Out (convertJson src saved)
            else
                convertText ctx.Out src saved

            Done
