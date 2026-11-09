/// Objects nothing uses. An object is used when something live keeps it alive, transitively: the
/// DAX of a live object refers to it, or a live object points at it (sort-by column, hierarchy level,
/// relationship, variation, group-by column, alternate-of, calendar). Liveness starts from roots:
/// what is evaluated or offered to clients whether or not anything in the model refers to it.
/// When in doubt an object counts as used: a false "unused" invites deleting something that matters.
module Tessera.Unused

open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

type Options = {
    /// Also check visible objects. By default they count as used, since reports can use them.
    IncludeVisible: bool
}

module Options =
    let defaults = { IncludeVisible = false }

type UnusedObject = {
    Entry: ObjectEntry
    /// Hidden itself, or in a hidden (or private) table, so no report can show it.
    Hidden: bool
    /// Unused objects that refer to it: it is unused because they are.
    UsedBy: ObjectRef list
    /// Anything the user should know before removing it.
    Note: string option
}

/// The kinds `find` reports. Others are never reported: calculation items, levels and partitions go
/// with their table or hierarchy; roles, relationships and calendars always count as used; M
/// expressions and data sources are used from M, which tess doesn't read.
let kinds = [
    ObjectKind.Table
    ObjectKind.Column
    ObjectKind.Measure
    ObjectKind.Hierarchy
    ObjectKind.Function
]

let private tableHidden (t: Table) =
    t.IsHidden || t.IsPrivate || t.ShowAsVariationsOnly

/// Whether an object is out of the field list: hidden itself or by its table. None for kinds
/// that aren't reported.
let hidden (e: ObjectEntry) =
    match e.Object with
    | :? Table as t -> Some(tableHidden t)
    | :? Column as c -> Some(c.IsHidden || tableHidden c.Table)
    | :? Measure as m -> Some(m.IsHidden || tableHidden m.Table)
    | :? Hierarchy as h -> Some(h.IsHidden || tableHidden h.Table)
    | :? Function as f -> Some f.IsHidden
    | _ -> None

/// Power BI's auto date/time template table: no relationships and no references, but Power BI
/// copies it for every new date column.
let private isDateTemplate (t: Table) =
    t.Annotations
    |> Seq.exists (fun a -> a.Name = "__PBI_TemplateDateTable" && a.Value = "true")

let private tableOf (o: MetadataObject) =
    match o with
    | :? Table as t -> Some t
    | :? Column as c -> Some c.Table
    | :? Measure as m -> Some m.Table
    | :? Hierarchy as h -> Some h.Table
    | :? Level as l -> Some l.Hierarchy.Table
    | :? CalculationItem as ci -> Some ci.CalculationGroup.Table
    | :? Calendar as c -> Some c.Table
    | :? Partition as p -> Some p.Table
    | _ -> None

/// Every unused object of the reported kinds, in model order.
let find (options: Options) (g: Deps.Graph) : UnusedObject list =
    let byObject = Dictionary<MetadataObject, ObjectRef>(HashIdentity.Reference)

    for e in g.Entries do
        byObject.[e.Object] <- e.Ref

    let refOf (o: MetadataObject) =
        match o with
        | null -> None
        | o ->
            match byObject.TryGetValue o with
            | true, r -> Some r
            | _ -> None

    // "from keeps to alive". References are what an object uses (reported as UsedBy); containment
    // edges only tie an object to its owner or parts.
    let keeps = Dictionary<ObjectRef, HashSet<ObjectRef>>()
    let referrers = Dictionary<ObjectRef, HashSet<ObjectRef>>()

    let add (table: Dictionary<ObjectRef, HashSet<ObjectRef>>) (key: ObjectRef) (value: ObjectRef) =
        match table.TryGetValue key with
        | true, set -> set.Add value |> ignore
        | _ -> table.[key] <- HashSet<ObjectRef>([ value ])

    let contains (from: ObjectRef) (target: ObjectRef) = add keeps from target

    let uses (from: ObjectRef) (target: ObjectRef) =
        add keeps from target

        if from <> target then
            add referrers target from

    let usesObject (from: ObjectRef) (target: MetadataObject) = refOf target |> Option.iter (uses from)

    for r in g.References do
        match r.Target with
        | Deps.Resolved target -> uses r.From target
        | Deps.Unresolved _ -> ()

    let roots = HashSet<ObjectRef>()

    for e in g.Entries do
        // A part keeps its table, so a used column or measure keeps the table it is in.
        match tableOf e.Object |> Option.bind (fun t -> refOf t) with
        | Some table when table <> e.Ref -> contains e.Ref table
        | _ -> ()

        match e.Object with
        | :? Table as t when isDateTemplate t -> roots.Add e.Ref |> ignore
        | :? Column as c when isDateTemplate c.Table -> roots.Add e.Ref |> ignore
        | :? Measure as m when isDateTemplate m.Table -> roots.Add e.Ref |> ignore
        | :? Hierarchy as h when isDateTemplate h.Table -> roots.Add e.Ref |> ignore
        | _ -> ()

        match e.Object with
        | :? Column as c ->
            // A table's key column (e.g. a date table's) goes wherever the table goes.
            if c.IsKey then
                refOf c.Table |> Option.iter (fun t -> contains t e.Ref)

            for v in c.Variations do
                usesObject e.Ref v.DefaultHierarchy
                usesObject e.Ref v.DefaultColumn

            if not (isNull c.RelatedColumnDetails) then
                for gb in c.RelatedColumnDetails.GroupByColumns do
                    usesObject e.Ref gb.GroupingColumn

            // An aggregation column serves queries of its base column; the engine uses it unseen.
            if not (isNull c.AlternateOf) then
                roots.Add e.Ref |> ignore
                usesObject e.Ref c.AlternateOf.BaseColumn
                usesObject e.Ref c.AlternateOf.BaseTable
        | :? Hierarchy as h ->
            for l in h.Levels do
                refOf l |> Option.iter (contains e.Ref)
        | :? CalculationItem as ci ->
            // Applied through the calculation group's column, never by name.
            refOf ci.CalculationGroup.Table |> Option.iter (fun t -> contains t e.Ref)
        | :? Calendar as cal ->
            roots.Add e.Ref |> ignore

            for group in cal.CalendarColumnGroups do
                match group with
                | :? TimeUnitColumnAssociation as a ->
                    usesObject e.Ref a.PrimaryColumn

                    for c in a.AssociatedColumns do
                        usesObject e.Ref c
                | :? TimeRelatedColumnGroup as rg ->
                    for c in rg.Columns do
                        usesObject e.Ref c
                | _ -> ()
        | :? Relationship
        | :? ModelRole -> roots.Add e.Ref |> ignore
        | _ -> ()

        if not options.IncludeVisible && hidden e = Some false then
            roots.Add e.Ref |> ignore

    let live = HashSet<ObjectRef>(roots)
    let queue = Queue<ObjectRef>(roots)

    while queue.Count > 0 do
        match keeps.TryGetValue(queue.Dequeue()) with
        | true, targets ->
            for t in targets do
                if live.Add t then
                    queue.Enqueue t
        | _ -> ()

    // Levels aren't reported; a level's column is used by its hierarchy.
    let owner (r: ObjectRef) =
        match r.Kind, r.Path with
        | ObjectKind.Level, [ table; hierarchy; _ ] -> ObjectRef.inTable ObjectKind.Hierarchy table hierarchy
        | _ -> r

    let order = g.Entries |> List.mapi (fun i x -> x.Ref, i) |> dict

    [
        for e in g.Entries do
            if List.contains e.Ref.Kind kinds && not (live.Contains e.Ref) then
                let usedBy =
                    match referrers.TryGetValue e.Ref with
                    | true, xs ->
                        xs
                        |> Seq.map owner
                        |> Seq.filter (fun r -> r <> e.Ref)
                        |> Seq.distinct
                        |> List.ofSeq
                    | _ -> []

                {
                    Entry = e
                    Hidden = hidden e |> Option.defaultValue false
                    UsedBy =
                        usedBy
                        |> List.sortBy (fun r ->
                            match order.TryGetValue r with
                            | true, i -> i
                            | _ -> System.Int32.MaxValue)
                    Note =
                        match e.Object with
                        | :? CalculatedTableColumn -> Some "made by its table's expression; change the DAX to drop it"
                        | _ -> None
                }
    ]
