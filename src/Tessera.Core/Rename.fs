/// Renaming an object and rewriting every DAX reference to it ("formula fixup"). Only the reference
/// tokens change: every other character of every expression, comments and strings included, stays.
module Tessera.Rename

open System
open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

/// One property rewritten by a rename.
type Edit = {
    /// The object holding the property (as Deps attributes it: a calculated table's DAX is the table's).
    Owner: ObjectEntry
    /// The property path, as in Deps ("expression", "tablePermissions.Sales.filterExpression") or
    /// "annotations.PBI_QueryOrder".
    Property: string
    Before: string
    After: string
    /// How many references in it were rewritten.
    References: int
}

type RenamePlan = {
    Target: ObjectEntry
    OldName: string
    NewName: string
    /// The object's reference after the rename (the same as Target.Ref for a no-op).
    NewRef: ObjectRef
    /// Edits, in model order; empty for kinds DAX can't refer to by name.
    Edits: Edit list
    /// Things the rename didn't (or couldn't) update, for the user to check.
    Warnings: string list
}

module RenamePlan =
    let isNoOp (p: RenamePlan) = p.OldName = p.NewName

    /// Edits of DAX expressions (as opposed to annotations).
    let expressionEdits (p: RenamePlan) =
        p.Edits
        |> List.filter (fun e -> not (e.Property.StartsWith("annotations.", StringComparison.Ordinal)))

let private ci = StringComparison.OrdinalIgnoreCase
let private eqCi (a: string) (b: string) = String.Equals(a, b, ci)

/// letters, digits and underscores, not starting with a digit: usable as an unquoted table name.
let private isPlainIdentifier (s: string) =
    s.Length > 0
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_')

/// Words that can't stand for a table unquoted. Function names (DATE, CALENDAR...) are fine
/// unquoted, since only Name( is a call.
let private keywords =
    HashSet<string>(
        [
            "VAR"
            "RETURN"
            "DEFINE"
            "EVALUATE"
            "MEASURE"
            "COLUMN"
            "TABLE"
            "FUNCTION"
            "ORDER"
            "BY"
            "ASC"
            "DESC"
            "START"
            "AT"
            "IN"
            "NOT"
            "AND"
            "OR"
            "TRUE"
            "FALSE"
        ],
        StringComparer.OrdinalIgnoreCase
    )

/// A UDF name: identifier characters (letters, digits, '_', '.'), not starting with a digit or dot.
let private isFunctionName (s: string) =
    s.Length > 0
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '.')
    && not (s.EndsWith ".")
    && not (s.Contains "..")

let private validateName (kind: ObjectKind) (name: string) =
    let invalid reason =
        Error(InvalidName(defaultArg (Option.ofObj name) "", reason))

    if String.IsNullOrWhiteSpace name then
        invalid "the name is empty"
    elif name.Trim() <> name then
        invalid "the name has leading or trailing whitespace"
    elif name |> Seq.exists Char.IsControl then
        invalid "the name contains control characters (such as line breaks)"
    elif kind = ObjectKind.Function && not (isFunctionName name) then
        invalid "function names must be identifiers: letters, digits, '_' and '.', not starting with a digit"
    else
        Ok()

/// The objects whose names a new name must not repeat (case-insensitively).
let private siblings (entries: ObjectEntry list) (target: ObjectEntry) =
    let kind = target.Ref.Kind

    let sameTable (e: ObjectEntry) =
        ObjectEntry.table e = ObjectEntry.table target

    let sameHierarchy (e: ObjectEntry) =
        List.truncate 2 e.Ref.Path = List.truncate 2 target.Ref.Path

    let anywhere (_: ObjectEntry) = true

    let scopes =
        match kind with
        | ObjectKind.Table
        | ObjectKind.Calendar -> [ [ ObjectKind.Table; ObjectKind.Calendar ], anywhere ]
        | ObjectKind.Column
        | ObjectKind.Hierarchy -> [ [ ObjectKind.Column; ObjectKind.Measure; ObjectKind.Hierarchy ], sameTable ]
        | ObjectKind.Measure -> [
            [ ObjectKind.Column; ObjectKind.Hierarchy ], sameTable
            [ ObjectKind.Measure ], anywhere
          ]
        | ObjectKind.Level -> [ [ ObjectKind.Level ], sameHierarchy ]
        | ObjectKind.Partition
        | ObjectKind.CalculationItem -> [ [ kind ], sameTable ]
        | other -> [ [ other ], anywhere ]

    entries
    |> List.filter (fun e ->
        not (obj.ReferenceEquals(e.Object, target.Object))
        && scopes
           |> List.exists (fun (kinds, inScope) -> List.contains e.Ref.Kind kinds && inScope e))

/// The ref an object (or one of its children) has after the target is renamed.
let private mapRef (target: ObjectRef) (newRef: ObjectRef) (r: ObjectRef) =
    let n = target.Path.Length

    if r = target then
        newRef
    elif
        (target.Kind = ObjectKind.Table || target.Kind = ObjectKind.Hierarchy)
        && r.Path.Length > n
        && List.truncate n r.Path = target.Path
        && r.Kind <> ObjectKind.Table
    then
        {
            r with
                Path = List.updateAt (n - 1) (ObjectRef.name newRef) r.Path
        }
    else
        r

/// A Deps property path after the target is renamed (RLS paths embed the table name).
let private mapVia (target: ObjectEntry) (newName: string) (via: string) =
    match target.Object with
    | :? Table as t when via = $"tablePermissions.{t.Name}.filterExpression" ->
        $"tablePermissions.{newName}.filterExpression"
    | _ -> via

/// Rewrite one expression: the references to the target, right to left so offsets stay valid.
let private rewrite (target: ObjectEntry) (newName: string) (dax: string) (refs: Deps.Reference list) =
    let oldName = ObjectEntry.name target
    let isTarget (r: Deps.Reference) = r.Target = Deps.Resolved target.Ref

    let allTokens = tokenize dax

    // An unquoted name the expression already uses for something else (a VAR, a parameter, a
    // function) would be captured by it; quote instead.
    let usedElsewhere =
        allTokens
        |> List.exists (fun t ->
            t.Kind = TokenKind.Identifier
            && eqCi t.Text newName
            && not (eqCi t.Text oldName))

    let tableText (original: Token) =
        if
            original.Kind = TokenKind.Identifier
            && isPlainIdentifier newName
            && not (keywords.Contains newName)
            && not usedElsewhere
        then
            newName
        else
            quoteTable newName

    let replacements =
        refs
        |> List.choose (fun r ->
            match r.Start, r.Length with
            | Some start, Some length ->
                let tokens =
                    tokenize (dax.Substring(start, length))
                    |> significant
                    |> List.map (fun t -> { t with Start = t.Start + start })

                let first, last = List.head tokens, List.last tokens
                let qualified = tokens.Length > 1 && last.Kind = TokenKind.Bracketed

                match target.Ref.Kind with
                | ObjectKind.Measure
                | ObjectKind.Column when isTarget r -> Some(last.Start, last.Length, bracket newName)
                // 'Old'[X]: the table part names the table whatever X resolves to (or doesn't).
                | ObjectKind.Table when qualified && Token.isName first && eqCi first.Text oldName ->
                    Some(first.Start, first.Length, tableText first)
                | ObjectKind.Table
                | ObjectKind.Calendar when not qualified && isTarget r ->
                    Some(first.Start, first.Length, tableText first)
                | ObjectKind.Function when isTarget r -> Some(first.Start, first.Length, newName)
                | _ -> None
            | _ -> None)
        |> List.sortByDescending (fun (start, _, _) -> start)

    let text =
        replacements
        |> List.fold
            (fun (s: string) (start, length, replacement) ->
                s.Substring(0, start) + replacement + s.Substring(start + length))
            dax

    text, replacements.Length

/// Power BI lists its queries (tables and named expressions) by name in this model annotation.
let private queryOrder = "PBI_QueryOrder"

let private relaxedJson =
    Text.Json.JsonSerializerOptions(Encoder = Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let private queryOrderEdit (db: Database) (entries: ObjectEntry list) (oldName: string) (newName: string) =
    match db.Model.Annotations.Find queryOrder with
    | null -> None
    | a ->
        try
            let names = Text.Json.JsonSerializer.Deserialize<string array>(a.Value)

            if isNull names || not (Array.contains oldName names) then
                None
            else
                let renamed = names |> Array.map (fun n -> if n = oldName then newName else n)

                Some {
                    Owner = entries |> List.find (fun e -> e.Ref.Kind = ObjectKind.Model)
                    Property = "annotations." + queryOrder
                    Before = a.Value
                    After = Text.Json.JsonSerializer.Serialize(renamed, relaxedJson)
                    References = 1
                }
        with _ ->
            None

/// Objects holding M (partitions, named expressions) that mention a name.
let private mMentions (entries: ObjectEntry list) (target: ObjectEntry) (name: string) =
    entries
    |> List.filter (fun e ->
        not (obj.ReferenceEquals(e.Object, target.Object))
        && match e.Object with
           | :? Partition as p ->
               match p.Source with
               | :? MPartitionSource as m -> not (isNull m.Expression) && m.Expression.Contains name
               | _ -> false
           | :? NamedExpression as x -> not (isNull x.Expression) && x.Expression.Contains name
           | _ -> false)
    |> List.map (fun e -> ObjectRef.format e.Ref)

/// Calculated-table columns whose sourceColumn ('T'[C]) names the target table or column.
let private sourceColumnMentions (entries: ObjectEntry list) (target: ObjectEntry) =
    let oldName = ObjectEntry.name target

    entries
    |> List.filter (fun e ->
        match e.Object with
        | :? CalculatedTableColumn as c when not (isNull c.SourceColumn) ->
            match tokenize c.SourceColumn |> significant with
            | [ t; col ] when Token.isName t && col.Kind = TokenKind.Bracketed ->
                match target.Object with
                | :? Table -> eqCi t.Text oldName
                | :? Column as tc -> eqCi t.Text tc.Table.Name && eqCi col.Text oldName
                | _ -> false
            | _ -> false
        | _ -> false)
    |> List.map (fun e -> ObjectRef.format e.Ref)

let private warnings (entries: ObjectEntry list) (target: ObjectEntry) =
    let oldName = ObjectEntry.name target
    let listed (refs: string list) = String.concat ", " refs

    [
        match target.Ref.Kind with
        | ObjectKind.Expression ->
            match mMentions entries target oldName with
            | [] -> $"M references to '{oldName}' are not updated (none of the model's M mentions it)"
            | refs -> $"M references to '{oldName}' are not updated; these mention it: {listed refs}"
        | ObjectKind.Table
        | ObjectKind.DataSource ->
            match mMentions entries target oldName with
            | [] -> ()
            | refs -> $"M references to '{oldName}' are not updated; these mention it: {listed refs}"
        | _ -> ()

        match target.Ref.Kind with
        | ObjectKind.Table
        | ObjectKind.Column ->
            match sourceColumnMentions entries target with
            | [] -> ()
            | refs -> $"calculated-table columns that map to it by sourceColumn are not updated: {listed refs}"
        | _ -> ()
    ]

/// Where a Deps property path lives on its owner, as a getter and setter.
let private accessor (o: MetadataObject) (property: string) : ((unit -> string) * (string -> unit)) option =
    let fsd (d: FormatStringDefinition) =
        if isNull d then
            None
        else
            Some((fun () -> d.Expression), (fun v -> d.Expression <- v))

    match o, property with
    | :? Measure as m, "expression" -> Some((fun () -> m.Expression), (fun v -> m.Expression <- v))
    | :? Measure as m, "formatStringDefinition.expression" -> fsd m.FormatStringDefinition
    | :? Measure as m, "detailRowsDefinition.expression" when not (isNull m.DetailRowsDefinition) ->
        let d = m.DetailRowsDefinition
        Some((fun () -> d.Expression), (fun v -> d.Expression <- v))
    | :? Measure as m, "kpi.statusExpression" when not (isNull m.KPI) ->
        Some((fun () -> m.KPI.StatusExpression), (fun v -> m.KPI.StatusExpression <- v))
    | :? Measure as m, "kpi.targetExpression" when not (isNull m.KPI) ->
        Some((fun () -> m.KPI.TargetExpression), (fun v -> m.KPI.TargetExpression <- v))
    | :? Measure as m, "kpi.trendExpression" when not (isNull m.KPI) ->
        Some((fun () -> m.KPI.TrendExpression), (fun v -> m.KPI.TrendExpression <- v))
    | :? CalculatedColumn as c, "expression" -> Some((fun () -> c.Expression), (fun v -> c.Expression <- v))
    | :? Table as t, "expression" ->
        t.Partitions
        |> Seq.tryPick (fun p ->
            match p.Source with
            | :? CalculatedPartitionSource as s -> Some((fun () -> s.Expression), (fun v -> s.Expression <- v))
            | _ -> None)
    | :? Table as t, "defaultDetailRowsDefinition.expression" when not (isNull t.DefaultDetailRowsDefinition) ->
        let d = t.DefaultDetailRowsDefinition
        Some((fun () -> d.Expression), (fun v -> d.Expression <- v))
    | :? Table as t, p when
        not (isNull t.CalculationGroup)
        && p.StartsWith("calculationGroup.", StringComparison.Ordinal)
        ->
        let cg = t.CalculationGroup

        let expr =
            if p.StartsWith("calculationGroup.noSelectionExpression.", StringComparison.Ordinal) then
                cg.NoSelectionExpression
            else
                cg.MultipleOrEmptySelectionExpression

        if isNull expr then
            None
        elif p.EndsWith(".formatStringDefinition.expression", StringComparison.Ordinal) then
            fsd expr.FormatStringDefinition
        else
            Some((fun () -> expr.Expression), (fun v -> expr.Expression <- v))
    | :? CalculationItem as c, "expression" -> Some((fun () -> c.Expression), (fun v -> c.Expression <- v))
    | :? CalculationItem as c, "formatStringDefinition.expression" -> fsd c.FormatStringDefinition
    | :? ModelRole as r, p when
        p.StartsWith("tablePermissions.", StringComparison.Ordinal)
        && p.EndsWith(".filterExpression", StringComparison.Ordinal)
        ->
        let table = p.Substring(17, p.Length - 17 - 17)

        r.TablePermissions
        |> Seq.tryFind (fun tp -> tp.Table.Name = table)
        |> Option.map (fun tp -> (fun () -> tp.FilterExpression), (fun v -> tp.FilterExpression <- v))
    | :? Function as f, "expression" -> Some((fun () -> f.Expression), (fun v -> f.Expression <- v))
    | :? Model as m, p when p.StartsWith("annotations.", StringComparison.Ordinal) ->
        match m.Annotations.Find(p.Substring 12) with
        | null -> None
        | a -> Some((fun () -> a.Value), (fun v -> a.Value <- v))
    | _ -> None

let private setName (o: MetadataObject) (name: string) =
    match o with
    | :? NamedMetadataObject as n -> n.Name <- name
    | _ -> invalidOp $"cannot rename a {o.GetType().Name}"

/// Apply a plan to a database, finding each object with `find` (identity, or by ref in a copy).
let private applyWith (find: ObjectEntry -> MetadataObject) (plan: RenamePlan) =
    // Edits first: RLS property paths embed the table name as it is before the rename.
    for e in plan.Edits do
        match accessor (find e.Owner) e.Property with
        | Some(get, set) ->
            if get () <> e.Before then
                invalidOp $"{ObjectRef.format e.Owner.Ref} {e.Property} changed since the rename was planned"

            set e.After
        | None -> invalidOp $"{ObjectRef.format e.Owner.Ref} has no property {e.Property}"

    if not (RenamePlan.isNoOp plan) then
        setName (find plan.Target) plan.NewName

/// Carry out a plan on the database it was made from.
let apply (plan: RenamePlan) = applyWith (fun e -> e.Object) plan

/// Check that, after the rename, every DAX reference means what it meant before: references to the
/// target now resolve to its new ref, and nothing else was captured by the new name. Applies the
/// plan to a copy of the database. Returns conflicts, and references that were unresolved before
/// and now resolve to the renamed object.
let private verify (db: Database) (before: Deps.Graph) (plan: RenamePlan) =
    let copy = db.Clone()
    let copyEntries = objects copy |> List.map (fun e -> e.Ref, e.Object) |> dict
    applyWith (fun e -> copyEntries.[e.Ref]) plan
    let after = Deps.build copy

    let mapR = mapRef plan.Target.Ref plan.NewRef
    let mapV = mapVia plan.Target plan.NewName

    let mapTarget =
        function
        | Deps.Resolved r -> Deps.Resolved(mapR r)
        | t -> t

    let group (refs: (ObjectRef * string * Deps.Target * Deps.Reference) list) =
        refs
        |> List.groupBy (fun (f, v, _, _) -> f, v)
        |> List.map (fun (k, xs) -> k, xs |> List.map (fun (_, _, t, r) -> t, r))
        |> Map.ofList

    let dax (g: Deps.Graph) =
        g.References |> List.filter (fun r -> r.Start.IsSome)

    let a =
        dax before
        |> List.map (fun r -> mapR r.From, mapV r.Via, mapTarget r.Target, r)
        |> group

    let b = dax after |> List.map (fun r -> r.From, r.Via, r.Target, r) |> group
    let keys = Seq.append a.Keys b.Keys |> Seq.distinct |> List.ofSeq

    let describe (t: Deps.Target) =
        match t with
        | Deps.Resolved r -> ObjectRef.format r
        | Deps.Unresolved _ -> "nothing"

    let conflicts = ResizeArray<string>()
    let resolvedNow = ResizeArray<string>()

    for (from, via) as k in keys do
        let xs = Map.tryFind k a |> Option.defaultValue []
        let ys = Map.tryFind k b |> Option.defaultValue []
        let where = $"{ObjectRef.format from} {via}"

        if xs.Length <> ys.Length then
            conflicts.Add $"{where}: would have {ys.Length} references instead of {xs.Length}"
        else
            for (x, _), (y, yr) in List.zip xs ys do
                let at = yr.Line |> Option.map (fun l -> $":{l}") |> Option.defaultValue ""

                match x, y with
                | Deps.Resolved p, Deps.Resolved q when p = q -> ()
                | Deps.Unresolved _, Deps.Unresolved _ -> ()
                | Deps.Unresolved _, Deps.Resolved _ ->
                    resolvedNow.Add $"{where}{at}: {yr.Text} was unresolved and now refers to {describe y}"
                | _ -> conflicts.Add $"{where}{at}: {yr.Text} would refer to {describe y} instead of {describe x}"

    List.ofSeq conflicts, List.ofSeq resolvedNow

/// Plan renaming an object: validate the new name and work out every expression edit.
let plan (db: Database) (target: ObjectEntry) (newName: string) : Result<RenamePlan, TessError> =
    let oldName = ObjectEntry.name target
    let kind = target.Ref.Kind
    let entries = objects db

    let noOp = {
        Target = target
        OldName = oldName
        NewName = newName
        NewRef = target.Ref
        Edits = []
        Warnings = []
    }

    if kind = ObjectKind.Model then
        Error(UnsupportedRename(ObjectRef.format target.Ref, "the model object can't be renamed"))
    else
        validateName kind newName
        |> Result.bind (fun () ->
            if newName = oldName then
                Ok noOp
            else
                match
                    siblings entries target
                    |> List.tryFind (fun e -> eqCi (ObjectEntry.name e) newName)
                with
                | Some clash -> Error(NameCollision(newName, ObjectRef.format clash.Ref))
                | None ->
                    let newRef = {
                        target.Ref with
                            Path = List.updateAt (target.Ref.Path.Length - 1) newName target.Ref.Path
                    }

                    let graph = Deps.build db

                    let byExpression =
                        graph.References
                        |> List.filter (fun r -> r.Start.IsSome)
                        |> List.groupBy (fun r -> r.From, r.Via)
                        |> Map.ofList

                    let daxEdits =
                        match kind with
                        | ObjectKind.Measure
                        | ObjectKind.Column
                        | ObjectKind.Table
                        | ObjectKind.Calendar
                        | ObjectKind.Function ->
                            Deps.daxSources graph.Entries
                            |> List.choose (fun s ->
                                match Map.tryFind (s.Owner.Ref, s.Property) byExpression with
                                | None -> None
                                | Some refs ->
                                    match rewrite target newName s.Dax refs with
                                    | _, 0 -> None
                                    | after, n ->
                                        Some {
                                            Owner = s.Owner
                                            Property = s.Property
                                            Before = s.Dax
                                            After = after
                                            References = n
                                        })
                        | _ -> []

                    let annotationEdits =
                        match kind with
                        | ObjectKind.Table
                        | ObjectKind.Expression -> queryOrderEdit db entries oldName newName |> Option.toList
                        | _ -> []

                    let p = {
                        noOp with
                            NewRef = newRef
                            Edits = daxEdits @ annotationEdits
                            Warnings = warnings entries target
                    }

                    match verify db graph p with
                    | [], resolvedNow ->
                        Ok {
                            p with
                                Warnings =
                                    p.Warnings
                                    @ (resolvedNow
                                       |> List.map (fun s -> "a reference that named nothing now resolves: " + s))
                        }
                    | conflicts, _ -> Error(RenameConflict(ObjectRef.format target.Ref, newName, conflicts)))
