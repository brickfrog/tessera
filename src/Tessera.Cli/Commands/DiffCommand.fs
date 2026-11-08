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

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Models _ -> "the two models to compare (any path tess accepts)."
            | Ignore _ ->
                "leave these properties out: path wildcards, repeatable or comma-separated, e.g. lineageTag, "
                + "annotations.PBI_*."

let private changeToken =
    function
    | Diff.Added -> "added"
    | Diff.Removed -> "removed"
    | Diff.Renamed -> "renamed"
    | Diff.Modified -> "modified"

let diffJson (changes: Diff.Change list) =
    let s = Diff.summarize changes
    let refOpt = Option.map ObjectRef.format >> optStr

    obj [
        "summary",
        obj [
            "added", num s.Added
            "removed", num s.Removed
            "renamed", num s.Renamed
            "modified", num s.Modified
        ]
        "changes",
        arr [
            for c in changes ->
                obj [
                    "change", str (changeToken c.Kind)
                    "label", str c.Label
                    "before", refOpt c.Before
                    "after", refOpt c.After
                    "properties",
                    arr [
                        for p in c.Properties ->
                            obj [ "path", str p.Path; "before", optStr p.Before; "after", optStr p.After ]
                    ]
                ]
        ]
    ]

let private quote (v: string option) =
    match v with
    | None -> "(none)"
    | Some s -> JsonSerializer.Serialize(s, jsonOptions |> fun o -> JsonSerializerOptions(o, WriteIndented = false))

/// Changed lines with this many unchanged lines of context around them.
let private context = 2

let lineDiffText (out: TextWriter) (before: string) (after: string) =
    let ops = Diff.LineDiff.diff (before.Split '\n') (after.Split '\n') |> Array.ofList

    let changed i =
        match ops.[i] with
        | Diff.LineDiff.Same _ -> false
        | _ -> true

    let near i =
        seq { max 0 (i - context) .. min (ops.Length - 1) (i + context) }
        |> Seq.exists changed

    let mutable gap = false

    for i in 0 .. ops.Length - 1 do
        if near i then
            if gap then
                out.WriteLine("        …")

            gap <- false

            match ops.[i] with
            | Diff.LineDiff.Same l -> out.WriteLine($"        {l}")
            | Diff.LineDiff.Removed l -> out.WriteLine($"      - {l}")
            | Diff.LineDiff.Added l -> out.WriteLine($"      + {l}")
        else
            gap <- true

let diffText (out: TextWriter) (changes: Diff.Change list) =
    for c in changes do
        let symbol =
            match c.Kind with
            | Diff.Added -> "+"
            | Diff.Removed -> "-"
            | Diff.Renamed -> ">"
            | Diff.Modified -> "~"

        out.WriteLine($"{symbol} {c.Label}")

        for p in c.Properties do
            let multiLine (v: string option) =
                v |> Option.exists (fun s -> s.Contains '\n')

            if multiLine p.Before || multiLine p.After then
                out.WriteLine($"    {p.Path}:")
                lineDiffText out (defaultArg p.Before "") (defaultArg p.After "")
            else
                out.WriteLine($"    {p.Path}: {quote p.Before} -> {quote p.After}")

    let s = Diff.summarize changes

    if changes.IsEmpty then
        out.WriteLine("No differences.")
    else
        let parts =
            [
                s.Added, "added"
                s.Removed, "removed"
                s.Renamed, "renamed"
                s.Modified, "modified"
            ]
            |> List.filter (fun (n, _) -> n > 0)
            |> List.map (fun (n, w) -> $"{n} {w}")

        out.WriteLine()
        out.WriteLine(String.concat ", " parts)

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let beforePath, afterPath = args.GetResult Args.Models

    let opts: Diff.Options = {
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
            writeJson ctx.Out (diffJson changes)
        else
            diffText ctx.Out changes

        if changes.IsEmpty then Done else DoneWith ExitCode.findings
