module Tessera.Cli.FindCommand

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
let summary =
    "search names, expressions, descriptions and other properties (exit 1: no matches)."

type Args =
    | [<MainCommand; ExactlyOnce>] Target of model: string * text: string
    | [<AltCommandLine("-E")>] Regex
    | [<AltCommandLine("-s")>] Case_Sensitive
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | In of properties: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "model path, and the text to look for."
            | Regex -> "treat the text as a .NET regular expression."
            | Case_Sensitive -> "match case (default: case-insensitive)."
            | Kind _ -> "only these kinds of object (repeatable or comma-separated)."
            | Table _ -> "only objects in this table (repeatable)."
            | In _ ->
                "only these properties: path wildcards, repeatable or comma-separated, e.g. expression, "
                + "description, annotations.*. Default: all except lineage tags."

let findJson (hits: Find.Hit list) =
    arr [
        for h in hits ->
            obj [
                "ref", str (ObjectRef.format h.Ref)
                "kind", str (ObjectKind.token h.Ref.Kind)
                "property", str h.Path
                "line", num h.Line
                "text", str h.Text
            ]
    ]

/// ref  property[:line]  text: the line number only when the value has several lines.
let findText (out: TextWriter) (hits: Find.Hit list) =
    let multiLine =
        hits
        |> List.filter (fun h -> h.Line > 1)
        |> List.map (fun h -> h.Ref, h.Path)
        |> Set.ofList

    for h in hits do
        let where =
            if h.Line > 1 || multiLine.Contains(h.Ref, h.Path) then
                $"{h.Path}:{h.Line}"
            else
                h.Path

        out.WriteLine($"{ObjectRef.format h.Ref}  {where}  {h.Text}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath, text = args.GetResult Args.Target

    match parseKinds (args.GetResults Args.Kind) with
    | Error e -> UsageError e
    | Ok kinds ->
        let opts: Find.Options = {
            Pattern = text
            Regex = args.Contains Args.Regex
            CaseSensitive = args.Contains Args.Case_Sensitive
            Objects = {
                Inventory.Filter.none with
                    Kinds = kinds
                    Tables = args.GetResults Args.Table
            }
            Properties = parseList (args.GetResults Args.In)
        }

        match Source.load ctx modelPath |> Result.bind (fun (_, db) -> Find.search db opts) with
        | Error e -> Failed e
        | Ok hits ->
            if ctx.Json then
                writeJson ctx.Out (findJson hits)
            else
                findText ctx.Out hits

            if hits.IsEmpty then DoneWith ExitCode.findings else Done
