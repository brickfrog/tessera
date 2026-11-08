/// Enumerating and summarising the objects in a model.
module Tessera.Inventory

open Microsoft.AnalysisServices.Tabular

type ObjectEntry = {
    Ref: ObjectRef
    /// The underlying TOM object (the Model for the model entry).
    Object: MetadataObject
    IsHidden: bool option
    /// Extra human-readable context, e.g. a relationship's columns.
    Detail: string option
}

module ObjectEntry =
    let kind e = e.Ref.Kind
    let name e = ObjectRef.name e.Ref
    let table e = ObjectRef.tableName e.Ref

let private entry r (o: MetadataObject) = {
    Ref = r
    Object = o
    IsHidden = None
    Detail = None
}

let private hidden r o isHidden = {
    entry r o with
        IsHidden = Some isHidden
}

/// "Sales[DateKey] -> Date[DateKey]": a relationship's identity, independent of its (often GUID) name.
let relationshipEndpoints (r: Relationship) =
    match r with
    | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) ->
        $"{s.FromTable.Name}[{s.FromColumn.Name}] -> {s.ToTable.Name}[{s.ToColumn.Name}]"
    | _ -> $"{r.FromTable.Name} -> {r.ToTable.Name}"

let private relationshipDetail (r: Relationship) =
    let core = relationshipEndpoints r
    if r.IsActive then core else core + " (inactive)"

let private tableObjects (t: Table) =
    seq {
        hidden (ObjectRef.table t.Name) t t.IsHidden

        for c in t.Columns do
            // RowNumber is an internal column every table has; it's not user-addressable.
            if c.Type <> ColumnType.RowNumber then
                hidden (ObjectRef.inTable ObjectKind.Column t.Name c.Name) c c.IsHidden

        for m in t.Measures do
            hidden (ObjectRef.inTable ObjectKind.Measure t.Name m.Name) m m.IsHidden

        for h in t.Hierarchies do
            hidden (ObjectRef.inTable ObjectKind.Hierarchy t.Name h.Name) h h.IsHidden

            for l in h.Levels |> Seq.sortBy (fun l -> l.Ordinal) do
                entry (ObjectRef.level t.Name h.Name l.Name) l

        if not (isNull t.CalculationGroup) then
            for ci in t.CalculationGroup.CalculationItems |> Seq.sortBy (fun ci -> ci.Ordinal) do
                entry (ObjectRef.inTable ObjectKind.CalculationItem t.Name ci.Name) ci

        for cal in t.Calendars do
            entry (ObjectRef.inTable ObjectKind.Calendar t.Name cal.Name) cal

        for p in t.Partitions do
            entry (ObjectRef.inTable ObjectKind.Partition t.Name p.Name) p
    }

/// Every addressable object, in model-tree order.
let objects (db: Database) : ObjectEntry list =
    let m = db.Model

    [
        entry ObjectRef.model m

        for t in m.Tables do
            yield! tableObjects t

        for r in m.Relationships do
            {
                entry (ObjectRef.modelLevel ObjectKind.Relationship r.Name) r with
                    Detail = Some(relationshipDetail r)
            }

        for r in m.Roles do
            entry (ObjectRef.modelLevel ObjectKind.Role r.Name) r

        for p in m.Perspectives do
            entry (ObjectRef.modelLevel ObjectKind.Perspective p.Name) p

        for c in m.Cultures do
            entry (ObjectRef.modelLevel ObjectKind.Culture c.Name) c

        for e in m.Expressions do
            entry (ObjectRef.modelLevel ObjectKind.Expression e.Name) e

        for f in m.Functions do
            entry (ObjectRef.modelLevel ObjectKind.Function f.Name) f

        for d in m.DataSources do
            entry (ObjectRef.modelLevel ObjectKind.DataSource d.Name) d
    ]

/// How a user can point at an object.
type Selector =
    /// kind:segment/segment
    | Exact of ObjectRef
    /// DAX style: 'Sales'[Total] or Sales[Total] (a column, measure or hierarchy)
    | Qualified of table: string * name: string
    /// DAX style: [Total] (a measure, else a uniquely named column or hierarchy)
    | Member of name: string
    /// Just a name: a table if one has it, else any uniquely named object
    | Bare of name: string

module Selector =

    /// Read a delimited DAX name starting at i (s.[i] is the opening delimiter); a doubled
    /// closing delimiter is an escaped one. Returns the name and the index after the closer.
    let private delimited (s: string) (i: int) (close: char) =
        let sb = System.Text.StringBuilder()
        let mutable j = i + 1
        let mutable result = None

        while result.IsNone && j < s.Length do
            if s.[j] = close then
                if j + 1 < s.Length && s.[j + 1] = close then
                    sb.Append(close) |> ignore
                    j <- j + 2
                else
                    result <- Some(sb.ToString(), j + 1)
            else
                sb.Append(s.[j]) |> ignore
                j <- j + 1

        result

    let private kindPrefix (s: string) =
        match s.IndexOf ':' with
        | -1 -> None
        | i -> ObjectKind.tryParse (s.Substring(0, i))

    let parse (input: string) : Result<Selector, TessError> =
        let s = input.Trim()
        let fail reason = Error(InvalidObjectRef(input, reason))

        let bracketTail (table: string) (at: int) =
            match delimited s at ']' with
            | Some(name, next) when next = s.Length && name <> "" ->
                if table = "" then
                    Ok(Member name)
                else
                    Ok(Qualified(table, name))
            | Some(_, next) when next <> s.Length -> fail "unexpected text after ']'"
            | Some _ -> fail "empty [name]"
            | None -> fail "missing closing ']'"

        if s = "" then
            fail "empty reference"
        elif
            s.Equals("model", System.StringComparison.OrdinalIgnoreCase)
            || (kindPrefix s).IsSome
        then
            ObjectRef.parse s |> Result.map Exact
        elif s.StartsWith "[" then
            bracketTail "" 0
        elif s.StartsWith "'" then
            match delimited s 0 '\'' with
            | None -> fail "missing closing quote"
            | Some(table, next) when next = s.Length -> Ok(Bare table)
            | Some(table, next) when s.[next] = '[' -> bracketTail table next
            | Some _ -> fail "expected '[' after the quoted table name"
        else
            match s.IndexOf '[' with
            | -1 -> Ok(Bare s)
            | i -> bracketTail (s.Substring(0, i).Trim()) i

    let private eqExact (a: string) (b: string) = a = b

    let private eqIgnoreCase (a: string) (b: string) =
        System.String.Equals(a, b, System.StringComparison.OrdinalIgnoreCase)

    /// Tabular names are unique case-insensitively, so prefer an exact-case match, then any case.
    let private matching (pred: (string -> string -> bool) -> ObjectEntry -> bool) entries =
        match entries |> List.filter (pred eqExact) with
        | [] -> entries |> List.filter (pred eqIgnoreCase)
        | exact -> exact

    let private single input =
        function
        | [ one ] -> Ok one
        | [] -> Error(ObjectNotFound input)
        | many -> Error(AmbiguousObject(input, many |> List.map (fun e -> ObjectRef.format e.Ref)))

    let private isMember e =
        match ObjectEntry.kind e with
        | ObjectKind.Column
        | ObjectKind.Measure
        | ObjectKind.Hierarchy -> true
        | _ -> false

    let resolve (entries: ObjectEntry list) (input: string) : Result<ObjectEntry, TessError> =
        parse input
        |> Result.bind (fun selector ->
            match selector with
            | Exact r ->
                entries
                |> List.filter (fun e -> e.Ref.Kind = r.Kind)
                |> matching (fun eq e -> List.length e.Ref.Path = r.Path.Length && List.forall2 eq e.Ref.Path r.Path)
                |> single input
            | Qualified(table, name) ->
                entries
                |> List.filter isMember
                |> matching (fun eq e ->
                    eq (ObjectEntry.name e) name
                    && ObjectEntry.table e |> Option.exists (fun t -> eq t table))
                |> single input
            | Member name ->
                let named kinds =
                    entries
                    |> List.filter (fun e -> List.contains (ObjectEntry.kind e) kinds)
                    |> matching (fun eq e -> eq (ObjectEntry.name e) name)

                match named [ ObjectKind.Measure ] with
                | [] -> named [ ObjectKind.Column; ObjectKind.Hierarchy ] |> single input
                | measures -> single input measures
            | Bare name ->
                let named =
                    entries
                    |> List.filter (fun e -> e.Ref.Kind <> ObjectKind.Model)
                    |> matching (fun eq e -> eq (ObjectEntry.name e) name)

                match named |> List.filter (fun e -> e.Ref.Kind = ObjectKind.Table) with
                | [ table ] -> Ok table
                | _ -> single input named)

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
