module Tessera.Cli.DiffCommand

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
let summary = "compare two models object by object (exit 1: they differ)."

type Args =
    | [<MainCommand; ExactlyOnce>] Models of before: string * after: string
    | Ignore of properties: string
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Models _ -> "the two models to compare (any path tess accepts)."
            | Ignore _ ->
                "leave these properties out: path wildcards, repeatable or comma-separated, e.g. lineageTag, "
                + "annotations.PBI_*."
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let beforePath, afterPath = args.GetResult Args.Models

    let opts: Diff.Options = {
        Diff.Options.none with
            Ignore = parseList (args.GetResults Args.Ignore)
    }

    let result =
        ModelIO.loadPath beforePath
        |> Result.bind (fun (_, a) -> ModelIO.loadPath afterPath |> Result.map (fun (_, b) -> a, b))

    match result with
    | Error e -> Failed e
    | Ok(a, b) ->
        let changes = Diff.compare opts a b

        if ctx.Json then
            writeJson ctx.Out (Changes.changesJson "diff" (ChangeSet.ofDiff changes))
        elif args.Contains Args.Stat then
            Changes.statText ctx.Out (ChangeSet.ofDiff changes)
        elif args.Contains Args.Name_Only then
            Changes.namesText ctx.Out (ChangeSet.ofDiff changes)
        else
            Changes.diffText ctx.Out changes

        if changes.IsEmpty then Done else DoneWith ExitCode.findings
