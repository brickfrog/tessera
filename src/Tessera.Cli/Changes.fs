/// Rendering model changes: the change-set JSON shared by `diff` and the editing commands (the
/// Tabular Editor CLI's changes[] shape), and the text forms (line diff, --stat, --name-only).
module Tessera.Cli.Changes

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Tessera
open Tessera.ChangeSet
open Tessera.Cli.Output

/// {objectPath, objectType, changeKind, movedFromObjectPath?, label, properties[{property, before, after}]}
let changeJson (c: ObjectChange) : JsonNode =
    obj [
        yield "objectPath", str (ObjectRef.format c.Ref)
        yield "objectType", str (ObjectKind.typeName c.Ref.Kind)
        yield "changeKind", str (ChangeKind.token c.ChangeKind)

        match c.MovedFrom with
        | Some old -> yield "movedFromObjectPath", str (ObjectRef.format old)
        | None -> ()

        yield "label", str c.Label

        yield
            "properties",
            arr [
                for p in c.Properties ->
                    obj [ "property", str p.Path; "before", optStr p.Before; "after", optStr p.After ]
            ]
    ]

let summaryJson (s: Summary) =
    obj [
        "created", num s.Created
        "deleted", num s.Deleted
        "modified", num s.Modified
        "moved", num s.Moved
        "total", num s.Total
    ]

/// {command, summary, changes[]} plus command-specific keys.
let changesJson (command: string) (changes: ObjectChange list) =
    obj [
        "command", str command
        "summary", summaryJson (summarize changes)
        "changes", arr (changes |> List.map changeJson)
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


/// One object per line with how many of its properties changed, then the summary (like git --stat).
let statText (out: TextWriter) (changes: ObjectChange list) =
    let symbol =
        function
        | ChangeKind.Created -> "+"
        | ChangeKind.Deleted -> "-"
        | ChangeKind.Moved -> ">"
        | ChangeKind.Modified -> "~"

    let width = changes |> List.map (fun c -> c.Label.Length) |> List.fold max 0

    for c in changes do
        let n = c.Properties.Length

        let detail =
            match n with
            | 0 -> ""
            | 1 -> "  1 property"
            | n -> $"  {n} properties"

        out.WriteLine($"{symbol c.ChangeKind} {c.Label.PadRight width}{detail}")

    let s = summarize changes

    let parts =
        // The same words as the diff text; JSON uses the change-set tokens (created/deleted/moved).
        [
            s.Created, "added"
            s.Deleted, "removed"
            s.Moved, "renamed"
            s.Modified, "modified"
        ]
        |> List.filter (fun (n, _) -> n > 0)
        |> List.map (fun (n, w) -> $"{n} {w}")

    if not parts.IsEmpty then
        out.WriteLine(String.concat ", " parts)

/// The changed objects' references, one per line (pipe-friendly).
let namesText (out: TextWriter) (changes: ObjectChange list) =
    for c in changes do
        out.WriteLine(ObjectRef.format c.Ref)
