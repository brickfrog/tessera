module Tessera.Cli.ListCommand

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
let summary = "list the objects in a model."

type Args =
    | [<MainCommand; ExactlyOnce>] Model of path: string
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | [<AltCommandLine("-n")>] Name of pattern: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."
            | Kind _ -> "only these kinds (repeatable or comma-separated), e.g. measure,column."
            | Table _ -> "only objects in this table (repeatable)."
            | Name _ -> "only objects whose name matches this wildcard (* and ?), case-insensitive."

let listJson (entries: ObjectEntry list) =
    arr [
        for e in entries ->
            obj [
                "ref", str (ObjectRef.format e.Ref)
                "kind", str (ObjectKind.token e.Ref.Kind)
                "name", str (ObjectEntry.name e)
                "table", optStr (ObjectEntry.table e |> Option.filter (fun _ -> e.Ref.Kind <> ObjectKind.Table))
                "hidden",
                (match e.IsHidden with
                 | Some h -> bool h
                 | None -> null)
                "detail", optStr e.Detail
            ]
    ]

/// One reference per line (pipe-friendly), with hidden flag and detail after two spaces.
let listText (out: TextWriter) (entries: ObjectEntry list) =
    for e in entries do
        let extras = [
            if e.IsHidden = Some true then
                "(hidden)"
            yield! Option.toList e.Detail
        ]

        match extras with
        | [] -> out.WriteLine(ObjectRef.format e.Ref)
        | xs -> out.WriteLine(ObjectRef.format e.Ref + "  " + String.concat "  " xs)

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let kindTokens = parseList (args.GetResults Args.Kind)
    let kinds = kindTokens |> List.map (fun t -> t, ObjectKind.tryParse t)

    match kinds |> List.tryFind (fun (_, k) -> k.IsNone) with
    | Some(bad, _) ->
        let known = ObjectKind.all |> List.map ObjectKind.token |> String.concat ", "
        UsageError $"unknown kind '{bad}' (known: {known})"
    | None ->
        match Source.load ctx (args.GetResult Args.Model) with
        | Error e -> Failed e
        | Ok(_, db) ->
            let filter: Inventory.Filter = {
                Kinds = kinds |> List.choose snd
                Tables = args.GetResults Args.Table
                NamePattern = args.TryGetResult Args.Name
            }

            let entries = Inventory.objects db |> Inventory.Filter.apply filter

            if ctx.Json then
                writeJson ctx.Out (listJson entries)
            else
                listText ctx.Out entries

            Done
