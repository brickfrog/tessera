/// Removing objects from a model. Refuses while other objects still refer to them through DAX
/// (unless forced), and cleans up the structural links TOM would otherwise leave pointing at
/// removed objects: relationships, hierarchy levels, sort-by columns, perspectives, security,
/// translations and the like.
module Tessera.RemoveObject

open System
open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

type RemoveSpec = {
    /// What to remove, in any form `get` accepts.
    Selectors: string list
    /// Also remove the objects whose DAX refers to them (measures, calculated columns and tables,
    /// calculation items, functions), and so on transitively.
    Cascade: bool
    /// Skip selectors that match nothing (with a note) instead of failing.
    IfExists: bool
    /// Remove even what other DAX still refers to, leaving those references broken.
    Force: bool
}

module RemoveSpec =
    let ofSelectors (selectors: string list) = {
        Selectors = selectors
        Cascade = false
        IfExists = false
        Force = false
    }

/// TOM's own validation errors, such as a property that links to an object no longer in the model.
let tomErrors (db: Database) =
    db.Model.Validate().Errors |> Seq.map (fun e -> e.Message) |> Set.ofSeq

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

/// Power BI lists its queries (tables and named expressions) by name in this model annotation.
let private queryOrder = "PBI_QueryOrder"

let private relaxedJson =
    Text.Json.JsonSerializerOptions(Encoder = Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

/// Take an object out of the collection that holds it.
let private detach (m: Model) (o: MetadataObject) =
    match o with
    | :? Table as t -> m.Tables.Remove t |> ignore
    | :? Column as c -> c.Table.Columns.Remove c |> ignore
    | :? Measure as x -> x.Table.Measures.Remove x |> ignore
    | :? Hierarchy as h -> h.Table.Hierarchies.Remove h |> ignore
    | :? Level as l -> l.Hierarchy.Levels.Remove l |> ignore
    | :? Partition as p -> p.Table.Partitions.Remove p |> ignore
    | :? CalculationItem as ci -> ci.CalculationGroup.CalculationItems.Remove ci |> ignore
    | :? Calendar as c -> c.Table.Calendars.Remove c |> ignore
    | :? Relationship as r -> m.Relationships.Remove r |> ignore
    | :? ModelRole as r -> m.Roles.Remove r |> ignore
    | :? Perspective as p -> m.Perspectives.Remove p |> ignore
    | :? Culture as c -> m.Cultures.Remove c |> ignore
    | :? NamedExpression as x -> m.Expressions.Remove x |> ignore
    | :? Function as f -> m.Functions.Remove f |> ignore
    | :? DataSource as d -> m.DataSources.Remove d |> ignore
    | o -> invalidOp $"can't remove a {o.GetType().Name}"

/// Kinds whose whole definition is their DAX, so --cascade may remove them along with what they
/// refer to. A role or a table-level expression (detail rows, calculation group defaults) is part
/// of something bigger, and is left for the user to edit.
let private cascadable (r: Deps.Reference) =
    match r.From.Kind with
    | ObjectKind.Measure
    | ObjectKind.Column
    | ObjectKind.CalculationItem
    | ObjectKind.Function -> true
    // A calculated table's own DAX.
    | ObjectKind.Table -> r.Via = "expression"
    | _ -> false

let private describe (r: Deps.Reference) =
    let at = r.Line |> Option.map (fun l -> $":{l}") |> Option.defaultValue ""

    let target =
        match r.Target with
        | Deps.Resolved t -> ObjectRef.format t
        | Deps.Unresolved text -> text

    $"{ObjectRef.format r.From}  {r.Via}{at}  {r.Text}  -> {target}"

/// Remove the objects `spec` selects (and everything inside them), as one edit.
let remove (db: Database) (spec: RemoveSpec) : Result<Mutation.Effect, TessError> =
    let model = db.Model
    // Both before any change: the dependency graph, and TOM's verdict on the model as it is.
    let tomBefore = tomErrors db
    let graph = Deps.build db
    let entries = graph.Entries

    let byObject = Dictionary<MetadataObject, ObjectEntry>(HashIdentity.Reference)

    for e in entries do
        byObject.[e.Object] <- e

    let byRef = entries |> List.map (fun e -> e.Ref, e) |> dict

    let fmt (o: MetadataObject) =
        match byObject.TryGetValue o with
        | true, e -> ObjectRef.format e.Ref
        | _ ->
            match o with
            | :? NamedMetadataObject as n -> n.Name
            | o -> o.GetType().Name

    let resolved =
        spec.Selectors
        |> List.fold
            (fun acc selector ->
                acc
                |> Result.bind (fun (found, skipped) ->
                    match Selector.resolve entries selector with
                    | Ok e -> Ok(found @ [ e ], skipped)
                    | Error(ObjectNotFound _) when spec.IfExists -> Ok(found, skipped @ [ selector ])
                    | Error(ObjectNotFoundSuggest _) when spec.IfExists -> Ok(found, skipped @ [ selector ])
                    | Error e -> Error e))
            (Ok([], []))

    match resolved with
    | Error e -> Error e
    | Ok(found, _) when found |> List.exists (fun e -> e.Ref.Kind = ObjectKind.Model) ->
        Error(UnsupportedRemove("model", "the model itself can't be removed"))
    | Ok(found, skipped) ->
        let removed = HashSet<MetadataObject>(HashIdentity.Reference)

        /// Removed itself, or inside something removed.
        let rec isRemoved (o: MetadataObject) =
            not (isNull o) && (removed.Contains o || isRemoved o.Parent)

        let rec isInside (container: MetadataObject) (o: MetadataObject) =
            not (isNull o.Parent)
            && (obj.ReferenceEquals(o.Parent, container) || isInside container o.Parent)

        let selected =
            let seen = HashSet<MetadataObject>(HashIdentity.Reference)
            found |> List.map (fun e -> e.Object) |> List.filter seen.Add

        for o in selected do
            removed.Add o |> ignore

        let removedRefs () =
            entries
            |> List.filter (fun e -> isRemoved e.Object)
            |> List.map (fun e -> e.Ref)
            |> HashSet

        // RLS filters belong to their table's permission, which goes with the table.
        let rlsTable = Dictionary<ObjectRef * string, Table>()

        for e in entries do
            match e.Object with
            | :? ModelRole as r ->
                for tp in r.TablePermissions do
                    rlsTable.[(e.Ref, $"tablePermissions.{tp.Table.Name}.filterExpression")] <- tp.Table
            | _ -> ()

        /// DAX outside the removed set that refers to something in it.
        let daxDependents (refs: HashSet<ObjectRef>) =
            graph.References
            |> List.filter (fun r ->
                r.Start.IsSome
                && (match r.Target with
                    | Deps.Resolved t -> refs.Contains t
                    | Deps.Unresolved _ -> false)
                && not (refs.Contains r.From)
                && not (
                    match rlsTable.TryGetValue((r.From, r.Via)) with
                    | true, t -> isRemoved t
                    | _ -> false
                ))

        // What else has to go: levels on removed columns, hierarchies left without levels,
        // relationships over removed columns and (with --cascade) DAX dependents. Repeat until
        // nothing more is added, since each can lead to another.
        let fallout = ResizeArray<string>()

        let add (o: MetadataObject) (why: string) =
            if not (isRemoved o) then
                removed.Add o |> ignore
                fallout.Add $"Also removed {fmt o}: {why}"

        let mutable growing = true

        while growing do
            let before = removed.Count

            for t in model.Tables do
                for h in t.Hierarchies do
                    for l in h.Levels do
                        if not (isRemoved l) && isRemoved l.Column then
                            add l $"its column {fmt l.Column} is removed"

                    if
                        not (isRemoved h)
                        && h.Levels.Count > 0
                        && h.Levels |> Seq.forall (fun l -> isRemoved l)
                    then
                        add h "none of its levels remain"

            for r in model.Relationships do
                match r with
                | :? SingleColumnRelationship as s when
                    not (isRemoved r) && (isRemoved s.FromColumn || isRemoved s.ToColumn)
                    ->
                    add r $"it uses a removed column ({relationshipEndpoints r})"
                | _ -> ()

            if spec.Cascade then
                for d in daxDependents (removedRefs ()) do
                    if cascadable d then
                        match byRef.TryGetValue d.From with
                        | true, e when not (isRemoved e.Object) ->
                            let target =
                                match d.Target with
                                | Deps.Resolved t -> ObjectRef.format t
                                | Deps.Unresolved text -> text

                            add e.Object $"its {d.Via} refers to {target} (--cascade)"
                        | _ -> ()

            growing <- removed.Count > before

        let refs = removedRefs ()

        let refusal =
            entries
            |> List.tryPick (fun e ->
                if not (isRemoved e.Object) then
                    None
                else
                    let refuse reason =
                        Some(UnsupportedRemove(ObjectRef.format e.Ref, reason))

                    match e.Object with
                    | :? Partition as p when
                        not (isRemoved p.Table)
                        && p.Table.Partitions |> Seq.forall (fun x -> isRemoved x)
                        ->
                        refuse $"it is the last partition of table '{p.Table.Name}' (remove the table instead)"
                    | :? CalculatedTableColumn as c when not (isRemoved c.Table) ->
                        refuse "a calculated table's columns come from its expression; change the expression instead"
                    | :? Column as c when not (isRemoved c.Table) && not (isNull c.Table.CalculationGroup) ->
                        refuse "a calculation group's column can't be removed (remove the calculation group instead)"
                    | _ -> None)

        // Links from objects that stay which tess can't sensibly cut: a partition's data source or
        // shared expression.
        let structural = [
            for t in model.Tables do
                for p in t.Partitions do
                    if not (isRemoved p) then
                        match p.Source with
                        | :? QueryPartitionSource as q when isRemoved q.DataSource ->
                            fmt p, "data source", fmt q.DataSource
                        | :? EntityPartitionSource as s ->
                            if isRemoved s.DataSource then
                                fmt p, "data source", fmt s.DataSource

                            if isRemoved s.ExpressionSource then
                                fmt p, "expression source", fmt s.ExpressionSource
                        | _ -> ()

            for x in model.Expressions do
                if not (isRemoved x) && isRemoved x.ExpressionSource then
                    fmt x, "expression source", fmt x.ExpressionSource
        ]

        let dependents = daxDependents refs
        let selectedRefs = selected |> List.map fmt

        match refusal, structural with
        | Some e, _ -> Error e
        | None, (_ :: _) ->
            let targets = structural |> List.map (fun (_, _, t) -> t) |> List.distinct

            let uses =
                structural
                |> List.map (fun (from, via, target) -> $"{from} uses {target} as its {via}")
                |> String.concat "; "

            Error(UnsupportedRemove(String.concat ", " targets, $"{uses} (change that first)"))
        | None, [] when not dependents.IsEmpty && not spec.Force ->
            Error(RemoveBlocked(selectedRefs, dependents |> List.map describe))
        | None, [] ->
            let notes = ResizeArray<string>()
            let warnings = ResizeArray<string>()

            for s in skipped do
                notes.Add $"No object matches '{s}'; skipped (--if-exists)"

            // Top-level removals: whatever isn't inside something else removed.
            let top =
                removed
                |> Seq.filter (fun o -> not (isRemoved o.Parent))
                |> List.ofSeq
                |> List.sortBy (fun o ->
                    match o with
                    | :? Relationship -> 0
                    | _ -> 1)

            for o in selected do
                if not (isRemoved o.Parent) then
                    let inside = entries |> List.filter (fun e -> isInside o e.Object) |> List.length

                    let rest =
                        if inside > 0 then
                            $""" and the {plural inside "object"} in it"""
                        else
                            ""

                    notes.Add $"Removed {fmt o}{rest}"

            notes.AddRange fallout

            // M isn't parsed, so other queries that use a removed query or source by name stay as
            // they are.
            for o in top do
                match byObject.TryGetValue o with
                | true, e when
                    (match e.Ref.Kind with
                     | ObjectKind.Table
                     | ObjectKind.Expression
                     | ObjectKind.DataSource -> true
                     | _ -> false)
                    ->
                    let name = ObjectEntry.name e

                    match
                        Rename.mMentions entries e name
                        |> List.filter (fun r -> not (refs |> Seq.exists (fun x -> ObjectRef.format x = r)))
                    with
                    | [] -> ()
                    | mentions ->
                        let listed = String.concat ", " mentions
                        warnings.Add $"M that mentions '{name}' is not updated: {listed}"
                | _ -> ()

            for d in dependents do
                warnings.Add $"left a broken reference (--force): {describe d}"

            // Structural links that would dangle.
            for t in model.Tables do
                if not (isRemoved t) then
                    for c in t.Columns do
                        if not (isRemoved c) then
                            if isRemoved c.SortByColumn then
                                notes.Add $"Cleared the sort-by column of {fmt c} (was {fmt c.SortByColumn})"
                                c.SortByColumn <- null

                            if
                                not (isNull c.AlternateOf)
                                && (isRemoved c.AlternateOf.BaseColumn || isRemoved c.AlternateOf.BaseTable)
                            then
                                notes.Add $"Cleared {fmt c}'s alternateOf (its base is removed)"
                                c.AlternateOf <- null

                            if not (isNull c.RelatedColumnDetails) then
                                let groupBy = c.RelatedColumnDetails.GroupByColumns

                                for g in List.ofSeq groupBy do
                                    if isRemoved g.GroupingColumn then
                                        notes.Add
                                            $"Removed group-by column {fmt g.GroupingColumn} from {fmt c}'s related column details"

                                        groupBy.Remove g |> ignore

                            for v in List.ofSeq c.Variations do
                                if
                                    isRemoved v.Relationship
                                    || isRemoved v.DefaultHierarchy
                                    || isRemoved v.DefaultColumn
                                then
                                    notes.Add $"Removed variation '{v.Name}' of {fmt c}: it uses a removed object"
                                    c.Variations.Remove v |> ignore

                    for cal in t.Calendars do
                        if not (isRemoved cal) then
                            let mutable changed = false

                            for g in List.ofSeq cal.CalendarColumnGroups do
                                match g with
                                | :? TimeUnitColumnAssociation as a ->
                                    if isRemoved a.PrimaryColumn then
                                        cal.CalendarColumnGroups.Remove g |> ignore
                                        changed <- true
                                    else
                                        for col in List.ofSeq a.AssociatedColumns do
                                            if isRemoved col then
                                                a.AssociatedColumns.Remove col |> ignore
                                                changed <- true
                                | :? TimeRelatedColumnGroup as r ->
                                    for col in List.ofSeq r.Columns do
                                        if isRemoved col then
                                            r.Columns.Remove col |> ignore
                                            changed <- true

                                    if r.Columns.Count = 0 then
                                        cal.CalendarColumnGroups.Remove g |> ignore
                                | _ -> ()

                            if changed then
                                notes.Add $"Removed {fmt cal}'s associations with removed columns"

            for x in model.Expressions do
                if not (isRemoved x) && isRemoved x.ParameterValuesColumn then
                    notes.Add $"Cleared {fmt x}'s parameter values column (was {fmt x.ParameterValuesColumn})"
                    x.ParameterValuesColumn <- null

            for p in model.Perspectives do
                if not (isRemoved p) then
                    let gone = ResizeArray<string>()

                    for pt in List.ofSeq p.PerspectiveTables do
                        if isRemoved pt.Table then
                            gone.Add(fmt pt.Table)
                            p.PerspectiveTables.Remove pt |> ignore
                        else
                            for x in List.ofSeq pt.PerspectiveColumns do
                                if isRemoved x.Column then
                                    gone.Add(fmt x.Column)
                                    pt.PerspectiveColumns.Remove x |> ignore

                            for x in List.ofSeq pt.PerspectiveMeasures do
                                if isRemoved x.Measure then
                                    gone.Add(fmt x.Measure)
                                    pt.PerspectiveMeasures.Remove x |> ignore

                            for x in List.ofSeq pt.PerspectiveHierarchies do
                                if isRemoved x.Hierarchy then
                                    gone.Add(fmt x.Hierarchy)
                                    pt.PerspectiveHierarchies.Remove x |> ignore

                    if gone.Count > 0 then
                        notes.Add $"""Removed from {fmt p}: {String.concat ", " gone}"""

            for r in model.Roles do
                if not (isRemoved r) then
                    for tp in List.ofSeq r.TablePermissions do
                        if isRemoved tp.Table then
                            let filter =
                                if String.IsNullOrWhiteSpace tp.FilterExpression then
                                    ""
                                else
                                    " (including its row filter)"

                            notes.Add $"Removed {fmt r}'s permissions on {fmt tp.Table}{filter}"
                            r.TablePermissions.Remove tp |> ignore
                        else
                            for cp in List.ofSeq tp.ColumnPermissions do
                                if isRemoved cp.Column then
                                    notes.Add $"Removed {fmt r}'s object-level security on {fmt cp.Column}"
                                    tp.ColumnPermissions.Remove cp |> ignore

            for c in model.Cultures do
                if not (isRemoved c) then
                    let stale =
                        c.ObjectTranslations |> Seq.filter (fun t -> isRemoved t.Object) |> List.ofSeq

                    for t in stale do
                        c.ObjectTranslations.Remove t |> ignore

                    if not stale.IsEmpty then
                        notes.Add $"""Removed {plural stale.Length "translation"} of removed objects from {fmt c}"""

            match model.Annotations.Find queryOrder with
            | null -> ()
            | a ->
                let queries =
                    top
                    |> List.choose (fun o ->
                        match o with
                        | :? Table as t -> Some t.Name
                        | :? NamedExpression as x -> Some x.Name
                        | _ -> None)
                    |> Set.ofList

                try
                    let names = Text.Json.JsonSerializer.Deserialize<string array>(a.Value)

                    if not (isNull names) && names |> Array.exists queries.Contains then
                        let kept = names |> Array.filter (queries.Contains >> not)
                        a.Value <- Text.Json.JsonSerializer.Serialize(kept, relaxedJson)
                        notes.Add $"Updated the model's {queryOrder} annotation"
                with _ ->
                    ()

            // Levels removed from hierarchies that stay: renumber the rest, keeping their order.
            let renumber =
                top
                |> List.choose (fun o ->
                    match o with
                    | :? Level as l -> Some l.Hierarchy
                    | _ -> None)
                |> List.distinct

            for o in top do
                detach model o

            for h in renumber do
                h.Levels
                |> Seq.sortBy (fun l -> l.Ordinal)
                |> List.ofSeq
                |> List.iteri (fun i l ->
                    if l.Ordinal <> i then
                        l.Ordinal <- i)

            // Anything still linked to a removed object would make the model unsaveable.
            match Set.difference (tomErrors db) tomBefore |> Set.toList with
            | [] ->
                Ok {
                    Warnings = List.ofSeq warnings
                    Notes = List.ofSeq notes
                    Renamed = []
                }
            | errors ->
                let listed = errors |> List.map (fun e -> "\n  " + e) |> String.concat ""

                Error(
                    UnsupportedRemove(
                        String.concat ", " selectedRefs,
                        $"other objects would still link to it, which tess can't clean up:{listed}"
                    )
                )
