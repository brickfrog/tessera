/// Perspective membership: which tables, columns, measures and hierarchies a perspective shows.
/// In TOM a perspective lists its tables (PerspectiveTable), and under each table the columns,
/// measures and hierarchies it includes, so a member needs its table's entry.
module Tessera.Perspectives

open System
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// A perspective by name (any case), or by its reference (perspective:Name).
let find (db: Database) (name: string) : Result<Perspective, TessError> =
    let name =
        match ObjectRef.parse name with
        | Ok r when r.Kind = ObjectKind.Perspective -> ObjectRef.name r
        | _ -> name

    let all = List.ofSeq db.Model.Perspectives

    match
        all
        |> List.tryFind (fun p -> p.Name = name)
        |> Option.orElse (all |> List.tryFind (fun p -> eqCi p.Name name))
    with
    | Some p -> Ok p
    | None ->
        let fix = $"add it with `tess add <model> perspective:{name}`"

        let known =
            match all with
            | [] -> "the model has no perspectives"
            | ps ->
                "the model has "
                + (ps |> List.map (fun p -> $"'{p.Name}'") |> String.concat ", ")

        Error(UnsupportedPerspectiveChange($"perspective:{name}", $"there is no such perspective ({known}; {fix})"))

/// Its members, in model order: each table, then its columns, measures and hierarchies.
let members (p: Perspective) : ObjectRef list = Serialize.perspectiveMembers p

[<RequireQualifiedAccess>]
type Change =
    | Add
    | Remove

let private refOf (p: Perspective) =
    ObjectRef.format (ObjectRef.modelLevel ObjectKind.Perspective p.Name)

let private ensureTable (p: Perspective) (t: Table) =
    match p.PerspectiveTables.Find t.Name with
    | null ->
        let pt = new PerspectiveTable(Table = t)
        p.PerspectiveTables.Add pt
        pt, true
    | pt -> pt, false

let private hasMembers (pt: PerspectiveTable) =
    pt.PerspectiveColumns.Count > 0
    || pt.PerspectiveMeasures.Count > 0
    || pt.PerspectiveHierarchies.Count > 0

/// Add a column, measure or hierarchy (with its table's entry, if needed). True if anything changed.
let private addMember (p: Perspective) (o: MetadataObject) =
    match o with
    | :? Column as c ->
        let pt, added = ensureTable p c.Table

        match pt.PerspectiveColumns.Find c.Name with
        | null ->
            pt.PerspectiveColumns.Add(new PerspectiveColumn(Column = c))
            true
        | _ -> added
    | :? Measure as m ->
        let pt, added = ensureTable p m.Table

        match pt.PerspectiveMeasures.Find m.Name with
        | null ->
            pt.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = m))
            true
        | _ -> added
    | :? Hierarchy as h ->
        let pt, added = ensureTable p h.Table

        match pt.PerspectiveHierarchies.Find h.Name with
        | null ->
            pt.PerspectiveHierarchies.Add(new PerspectiveHierarchy(Hierarchy = h))
            true
        | _ -> added
    | _ -> false

/// Remove a column, measure or hierarchy. True if it was there.
let private removeMember (p: Perspective) (o: MetadataObject) =
    let table =
        match o with
        | :? Column as c -> Some c.Table
        | :? Measure as m -> Some m.Table
        | :? Hierarchy as h -> Some h.Table
        | _ -> None

    match table |> Option.map (fun t -> p.PerspectiveTables.Find t.Name) with
    | None
    | Some null -> false
    | Some pt ->
        match o with
        | :? Column as c ->
            match pt.PerspectiveColumns.Find c.Name with
            | null -> false
            | x -> pt.PerspectiveColumns.Remove x
        | :? Measure as m ->
            match pt.PerspectiveMeasures.Find m.Name with
            | null -> false
            | x -> pt.PerspectiveMeasures.Remove x
        | :? Hierarchy as h ->
            match pt.PerspectiveHierarchies.Find h.Name with
            | null -> false
            | x -> pt.PerspectiveHierarchies.Remove x
        | _ -> false

/// Add objects to a perspective, or remove them from it, as one edit.
///   - Adding a table adds it with all its columns, measures and hierarchies; adding a member adds
///     its table too, with just that member.
///   - Removing a table removes it with all its members. Removing a table's last member removes the
///     table as well, since a table with no members shows nothing.
///   - Objects already in (or not in) the perspective are noted, not refused.
let edit
    (db: Database)
    (change: Change)
    (perspective: string)
    (selectors: string list)
    : Result<Mutation.Effect, TessError> =
    find db perspective
    |> Result.bind (fun p ->
        let entries = objects db
        let pRef = refOf p

        let resolved =
            selectors
            |> List.fold
                (fun acc selector ->
                    acc
                    |> Result.bind (fun xs ->
                        Selector.resolve entries selector
                        |> Result.bind (fun e ->
                            match e.Ref.Kind with
                            | ObjectKind.Table
                            | ObjectKind.Column
                            | ObjectKind.Measure
                            | ObjectKind.Hierarchy -> Ok(xs @ [ e ])
                            | k ->
                                Error(
                                    UnsupportedPerspectiveChange(
                                        pRef,
                                        $"{ObjectRef.format e.Ref} is a {ObjectKind.token k}; perspectives hold tables, columns, measures and hierarchies"
                                    )
                                ))))
                (Ok [])

        resolved
        |> Result.map (fun targets ->
            let notes = ResizeArray<string>()
            let changed = ResizeArray<string>()

            for e in targets |> List.distinctBy (fun e -> e.Ref) do
                let name = ObjectRef.format e.Ref

                match change, e.Object with
                | Change.Add, (:? Table as t) ->
                    let _, added = ensureTable p t

                    let children: MetadataObject list = [
                        for c in t.Columns do
                            if c.Type <> ColumnType.RowNumber then
                                c
                        for m in t.Measures do
                            m
                        for h in t.Hierarchies do
                            h
                    ]

                    // Every child, so evaluate them all (no short-circuit).
                    let anyAdded = children |> List.map (addMember p) |> List.fold (||) false

                    if added || anyAdded then
                        changed.Add name
                    else
                        notes.Add $"{name} is already in {pRef} with all its members"
                | Change.Add, o ->
                    if addMember p o then
                        changed.Add name
                    else
                        notes.Add $"{name} is already in {pRef}"
                | Change.Remove, (:? Table as t) ->
                    match p.PerspectiveTables.Find t.Name with
                    | null -> notes.Add $"{name} is not in {pRef}"
                    | pt ->
                        p.PerspectiveTables.Remove pt |> ignore
                        changed.Add name
                | Change.Remove, o ->
                    if removeMember p o then
                        changed.Add name

                        match ObjectEntry.table e |> Option.map (fun t -> p.PerspectiveTables.Find t) with
                        | Some pt when not (isNull pt) && not (hasMembers pt) ->
                            p.PerspectiveTables.Remove pt |> ignore

                            notes.Add
                                $"Removed {ObjectRef.format (ObjectRef.table pt.Table.Name)} from {pRef} too: none of its members are left"
                        | _ -> ()
                    else
                        notes.Add $"{name} is not in {pRef}"

            let verb, prep =
                match change with
                | Change.Add -> "Added", "to"
                | Change.Remove -> "Removed", "from"

            {
                Mutation.Effect.none with
                    Notes = [
                        if changed.Count > 0 then
                            $"""{verb} {String.concat ", " changed} {prep} {pRef}"""
                        yield! notes
                    ]
            }))
