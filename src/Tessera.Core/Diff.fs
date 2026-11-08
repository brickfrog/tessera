/// Object-level comparison of two models.
module Tessera.Diff

open System
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

/// A minimal line diff (LCS), for multi-line values such as DAX and M.
module LineDiff =

    type Op =
        | Same of string
        | Removed of string
        | Added of string

    /// Beyond this many cells the LCS table gets expensive; fall back to "replace everything".
    let private maxCells = 4_000_000

    let diff (a: string array) (b: string array) : Op list =
        let n, m = a.Length, b.Length

        if int64 n * int64 m > int64 maxCells then
            [ for l in a -> Removed l ] @ [ for l in b -> Added l ]
        else
            // lcs.[i, j] = length of the LCS of a.[i..] and b.[j..]
            let lcs = Array2D.zeroCreate<int> (n + 1) (m + 1)

            for i in n - 1 .. -1 .. 0 do
                for j in m - 1 .. -1 .. 0 do
                    lcs.[i, j] <-
                        if a.[i] = b.[j] then
                            lcs.[i + 1, j + 1] + 1
                        else
                            max lcs.[i + 1, j] lcs.[i, j + 1]

            let ops = ResizeArray<Op>()
            let mutable i = 0
            let mutable j = 0

            while i < n || j < m do
                if i < n && j < m && a.[i] = b.[j] then
                    ops.Add(Same a.[i])
                    i <- i + 1
                    j <- j + 1
                // On a tie, removals come before additions (as in unified diffs).
                elif i < n && (j = m || lcs.[i + 1, j] >= lcs.[i, j + 1]) then
                    ops.Add(Removed a.[i])
                    i <- i + 1
                else
                    ops.Add(Added b.[j])
                    j <- j + 1

            List.ofSeq ops

type PropertyChange = {
    Path: string
    Before: string option
    After: string option
}

type ChangeKind =
    | Added
    | Removed
    /// Matched by lineageTag under a different name (possibly with other changes too).
    | Renamed
    | Modified

type Change = {
    Kind: ChangeKind
    Before: ObjectRef option
    After: ObjectRef option
    /// Human-readable identity: the ref, or a relationship's endpoints.
    Label: string
    Properties: PropertyChange list
}

type Options = {
    /// Property-path globs to leave out of the comparison (see Serialize.pathGlob).
    Ignore: string list
}

module Options =
    let none = { Ignore = [] }

type Summary = {
    Added: int
    Removed: int
    Renamed: int
    Modified: int
}

/// An object's identity for matching. Relationships are keyed by their endpoints: their names are
/// GUIDs in Power BI models, so a server export and a PBIP would otherwise never match.
let private keyOf (e: ObjectEntry) =
    match e.Object with
    | :? Relationship as r -> "relationship:" + relationshipEndpoints r
    | _ -> ObjectRef.format e.Ref

type private Side = {
    Db: Database
    Entries: ObjectEntry list
    Keys: Map<string, ObjectEntry>
    KeyOf: Collections.Generic.Dictionary<ObjectEntry, string>
    Props: Collections.Generic.Dictionary<ObjectEntry, (string * string) list>
}

let private side (db: Database) =
    let entries = objects db
    let keyed = entries |> List.map (fun e -> keyOf e, e)

    // If endpoint keys collide (two relationships over the same columns), fall back to names.
    let keyed =
        let dupes =
            keyed
            |> List.countBy fst
            |> List.filter (fun (_, n) -> n > 1)
            |> List.map fst
            |> Set.ofList

        keyed
        |> List.map (fun (k, e) -> (if dupes.Contains k then ObjectRef.format e.Ref else k), e)

    let keyOf =
        Collections.Generic.Dictionary<ObjectEntry, string>(HashIdentity.Reference)

    for k, e in keyed do
        keyOf.[e] <- k

    {
        Db = db
        Entries = entries
        Keys = Map.ofList keyed
        KeyOf = keyOf
        Props = Collections.Generic.Dictionary<ObjectEntry, (string * string) list>(HashIdentity.Reference)
    }

let private keyFor (s: Side) (e: ObjectEntry) = s.KeyOf.[e]

/// All flattened properties of an entry, serialized once per diff.
let private allProps (s: Side) (e: ObjectEntry) =
    match s.Props.TryGetValue e with
    | true, p -> p
    | _ ->
        let p = Serialize.properties s.Db e
        s.Props.[e] <- p
        p

let private props (opts: Options) (s: Side) (e: ObjectEntry) =
    let ignored = opts.Ignore |> List.map Serialize.pathGlob

    allProps s e
    // "name" is implied by the ref (and renames are reported as such).
    |> List.filter (fun (p, _) -> p <> "name" && not (ignored |> List.exists (fun g -> g p)))

let private propertyChanges (before: (string * string) list) (after: (string * string) list) =
    let a = Map.ofList before
    let b = Map.ofList after

    let paths =
        (before |> List.map fst)
        @ (after |> List.map fst |> List.filter (a.ContainsKey >> not))

    [
        for p in paths do
            let x = Map.tryFind p a
            let y = Map.tryFind p b

            if x <> y then
                yield { Path = p; Before = x; After = y }
    ]

let private lineageTag (s: Side) (e: ObjectEntry) =
    allProps s e
    |> List.tryFind (fun (p, _) -> p = "lineageTag")
    |> Option.map snd
    |> Option.filter (String.IsNullOrWhiteSpace >> not)

/// Pair removed and added objects of the same kind that share a lineageTag. Tags must be unique
/// per kind on each side, otherwise that kind is left as plain removes and adds.
let private renames (a: Side) (b: Side) (removed: ObjectEntry list) (added: ObjectEntry list) =
    let tagged (s: Side) (entries: ObjectEntry list) =
        entries
        |> List.choose (fun e -> lineageTag s e |> Option.map (fun t -> (e.Ref.Kind, t), e))

    let unique (s: Side) =
        tagged s s.Entries
        |> List.countBy fst
        |> List.filter (fun (_, n) -> n = 1)
        |> List.map fst
        |> Set.ofList

    let okA, okB = unique a, unique b

    let removedByTag =
        tagged a removed |> List.filter (fun (k, _) -> okA.Contains k) |> Map.ofList

    tagged b added
    |> List.filter (fun (k, _) -> okB.Contains k)
    |> List.choose (fun (k, after) -> Map.tryFind k removedByTag |> Option.map (fun before -> before, after))

let compare (opts: Options) (before: Database) (after: Database) : Change list =
    let a = side before
    let b = side after
    let label (s: Side) (e: ObjectEntry) = keyFor s e

    let removed =
        a.Entries |> List.filter (fun e -> not (b.Keys.ContainsKey(keyFor a e)))

    let added = b.Entries |> List.filter (fun e -> not (a.Keys.ContainsKey(keyFor b e)))
    let renamePairs = renames a b removed added
    let renamedFrom = renamePairs |> List.map (fst >> keyFor a) |> Set.ofList
    let renamedTo = renamePairs |> List.map (fun (x, y) -> keyFor b y, x) |> Map.ofList

    [
        for e in b.Entries do
            let k = keyFor b e

            match Map.tryFind k a.Keys, Map.tryFind k renamedTo with
            | Some old, _ ->
                let changes = propertyChanges (props opts a old) (props opts b e)

                if not changes.IsEmpty then
                    yield {
                        Kind = Modified
                        Before = Some old.Ref
                        After = Some e.Ref
                        Label = label b e
                        Properties = changes
                    }
            | None, Some old ->
                yield {
                    Kind = Renamed
                    Before = Some old.Ref
                    After = Some e.Ref
                    Label = $"{label a old} -> {label b e}"
                    Properties = propertyChanges (props opts a old) (props opts b e)
                }
            | None, None ->
                yield {
                    Kind = Added
                    Before = None
                    After = Some e.Ref
                    Label = label b e
                    Properties = []
                }

        for e in removed do
            if not (renamedFrom.Contains(keyFor a e)) then
                yield {
                    Kind = Removed
                    Before = Some e.Ref
                    After = None
                    Label = label a e
                    Properties = []
                }
    ]

let summarize (changes: Change list) =
    let count k =
        changes |> List.filter (fun c -> c.Kind = k) |> List.length

    {
        Added = count Added
        Removed = count Removed
        Renamed = count Renamed
        Modified = count Modified
    }
