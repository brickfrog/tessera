/// Enumerating and summarising the objects in a model.
module Tessera.Inventory

open Microsoft.AnalysisServices.Tabular

type ObjectEntry = {
    Ref: ObjectRef
    IsHidden: bool option
    /// Extra human-readable context, e.g. a relationship's columns.
    Detail: string option
}

module ObjectEntry =
    let kind e = e.Ref.Kind
    let name e = ObjectRef.name e.Ref
    let table e = ObjectRef.tableName e.Ref

let private entry r = {
    Ref = r
    IsHidden = None
    Detail = None
}

let private hidden r isHidden = {
    entry r with
        IsHidden = Some isHidden
}

let private relationshipDetail (r: Relationship) =
    let core =
        match r with
        | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) ->
            $"{s.FromTable.Name}[{s.FromColumn.Name}] -> {s.ToTable.Name}[{s.ToColumn.Name}]"
        | _ -> $"{r.FromTable.Name} -> {r.ToTable.Name}"

    if r.IsActive then core else core + " (inactive)"

let private tableObjects (t: Table) =
    seq {
        hidden (ObjectRef.table t.Name) t.IsHidden

        for c in t.Columns do
            // RowNumber is an internal column every table has; it's not user-addressable.
            if c.Type <> ColumnType.RowNumber then
                hidden (ObjectRef.inTable ObjectKind.Column t.Name c.Name) c.IsHidden

        for m in t.Measures do
            hidden (ObjectRef.inTable ObjectKind.Measure t.Name m.Name) m.IsHidden

        for h in t.Hierarchies do
            hidden (ObjectRef.inTable ObjectKind.Hierarchy t.Name h.Name) h.IsHidden

            for l in h.Levels |> Seq.sortBy (fun l -> l.Ordinal) do
                entry (ObjectRef.level t.Name h.Name l.Name)

        if not (isNull t.CalculationGroup) then
            for ci in t.CalculationGroup.CalculationItems |> Seq.sortBy (fun ci -> ci.Ordinal) do
                entry (ObjectRef.inTable ObjectKind.CalculationItem t.Name ci.Name)

        for cal in t.Calendars do
            entry (ObjectRef.inTable ObjectKind.Calendar t.Name cal.Name)

        for p in t.Partitions do
            entry (ObjectRef.inTable ObjectKind.Partition t.Name p.Name)
    }

/// Every addressable object, in model-tree order.
let objects (db: Database) : ObjectEntry list =
    let m = db.Model

    [
        entry ObjectRef.model

        for t in m.Tables do
            yield! tableObjects t

        for r in m.Relationships do
            {
                entry (ObjectRef.modelLevel ObjectKind.Relationship r.Name) with
                    Detail = Some(relationshipDetail r)
            }

        for r in m.Roles do
            entry (ObjectRef.modelLevel ObjectKind.Role r.Name)

        for p in m.Perspectives do
            entry (ObjectRef.modelLevel ObjectKind.Perspective p.Name)

        for c in m.Cultures do
            entry (ObjectRef.modelLevel ObjectKind.Culture c.Name)

        for e in m.Expressions do
            entry (ObjectRef.modelLevel ObjectKind.Expression e.Name)

        for f in m.Functions do
            entry (ObjectRef.modelLevel ObjectKind.Function f.Name)

        for d in m.DataSources do
            entry (ObjectRef.modelLevel ObjectKind.DataSource d.Name)
    ]

/// Filters for `list`. Empty lists mean "no restriction".
type Filter = {
    Kinds: ObjectKind list
    Tables: string list
    /// Wildcard pattern (* and ?) matched case-insensitively against the object's name.
    NamePattern: string option
}

module Filter =
    let none = {
        Kinds = []
        Tables = []
        NamePattern = None
    }

    let private wildcardRegex (pattern: string) =
        let escaped =
            System.Text.RegularExpressions.Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".")

        System.Text.RegularExpressions.Regex(
            "^" + escaped + "$",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase
            ||| System.Text.RegularExpressions.RegexOptions.CultureInvariant
        )

    let apply (filter: Filter) (entries: ObjectEntry list) =
        let byName =
            filter.NamePattern
            |> Option.map (fun p ->
                let rx = wildcardRegex p
                fun e -> rx.IsMatch(ObjectEntry.name e))

        let tables = filter.Tables |> List.map (fun t -> t.ToLowerInvariant()) |> Set.ofList

        entries
        |> List.filter (fun e ->
            (filter.Kinds.IsEmpty || List.contains (ObjectEntry.kind e) filter.Kinds)
            && (tables.IsEmpty
                || (ObjectEntry.table e
                    |> Option.exists (fun t -> tables.Contains(t.ToLowerInvariant()))))
            && (byName |> Option.forall (fun f -> f e)))

type ModelInfo = {
    /// Database name, falling back to the model name (PBIP databases are often unnamed).
    Name: string
    CompatibilityLevel: int
    CompatibilityMode: string
    Culture: string option
    Source: ModelSource
    CalculationGroups: int
    Counts: (ObjectKind * int) list
}

let info (source: ModelSource) (db: Database) : ModelInfo =
    let entries = objects db

    let countOf k =
        entries |> List.filter (fun e -> e.Ref.Kind = k) |> List.length

    {
        Name =
            [ db.Name; (if isNull db.Model then null else db.Model.Name) ]
            |> List.tryFind (System.String.IsNullOrWhiteSpace >> not)
            |> Option.defaultValue ""
        CompatibilityLevel = db.CompatibilityLevel
        CompatibilityMode = string (ModelIO.effectiveCompatibilityMode db)
        Culture =
            match db.Model.Culture with
            | null
            | "" -> None
            | c -> Some c
        Source = source
        CalculationGroups =
            db.Model.Tables
            |> Seq.filter (fun t -> not (isNull t.CalculationGroup))
            |> Seq.length
        Counts =
            ObjectKind.all
            |> List.filter (fun k -> k <> ObjectKind.Model)
            |> List.map (fun k -> k, countOf k)
    }
