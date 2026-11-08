/// Creating one object (`add`): a measure, column, table, hierarchy, level, calculation item,
/// relationship, role, perspective, culture, named expression, function or partition, shaped the way
/// Power BI Desktop creates it. Names are checked with the same rules as a rename. Run through
/// Mutation.apply to get the change set and the errors the new object introduces.
module Tessera.AddObject

open System
open System.Text.Json
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

/// Optional properties that several kinds share. Setting one on a kind that doesn't have it is an error.
type Metadata = {
    Description: string option
    /// Columns, measures and hierarchies.
    DisplayFolder: string option
    /// Columns and measures.
    FormatString: string option
    /// Tables, columns, measures, hierarchies and functions.
    Hidden: bool
}

module Metadata =
    let none = {
        Description = None
        DisplayFolder = None
        FormatString = None
        Hidden = false
    }

[<RequireQualifiedAccess>]
type ColumnSource =
    /// A calculated column.
    | Calculated of expression: string
    /// A data column, read from the table's source. The source column defaults to the column's name.
    | Data of dataType: DataType * sourceColumn: string option

[<RequireQualifiedAccess>]
type TableSource =
    /// A calculated table (one calculated partition). Its columns are inferred by the engine.
    | Calculated of expression: string
    /// A table loaded by Power Query (one M partition, named after the table).
    | M of expression: string * mode: ModeType
    /// A calculation-group table. Precedence defaults to one more than the highest in the model;
    /// the column the items appear in defaults to "Name".
    | CalculationGroup of precedence: int option * column: string option

[<RequireQualifiedAccess>]
type Cardinality =
    | ManyToOne
    | OneToOne
    | ManyToMany

[<RequireQualifiedAccess>]
type CrossFilter =
    | Single
    | Both

type RelationshipSpec = {
    /// The many side, as any form `get` accepts ('Sales'[DateKey], column:Sales/DateKey).
    From: string
    /// The one side.
    To: string
    Inactive: bool
    Cardinality: Cardinality
    /// Defaults to single for many-to-one and many-to-many, both for one-to-one (as Desktop does).
    CrossFilter: CrossFilter option
}

module RelationshipSpec =
    let between (from: string) (``to``: string) = {
        From = from
        To = ``to``
        Inactive = false
        Cardinality = Cardinality.ManyToOne
        CrossFilter = None
    }

/// What to create; the case decides the kind, which must match the reference's.
[<RequireQualifiedAccess>]
type Details =
    | Measure of expression: string
    | Column of ColumnSource
    | Table of TableSource
    /// Levels on these columns of the table, in order; each level is named after its column.
    | Hierarchy of levels: string list
    /// A level on a column (default: the column named like the level), at an ordinal (default: last).
    | Level of column: string option * ordinal: int option
    | CalculationItem of expression: string * formatStringExpression: string option * ordinal: int option
    | Relationship of RelationshipSpec
    /// Row-level security filters: (table, DAX filter expression).
    | Role of permission: ModelPermission * filters: (string * string) list
    /// Members to include, as any form `get` accepts: tables (with all their columns, measures and
    /// hierarchies), columns, measures, hierarchies.
    | Perspective of includes: string list
    | Culture
    /// A named M expression (a Power Query query or parameter).
    | Expression of m: string
    /// A DAX user-defined function (compatibility level 1702 or higher).
    | Function of expression: string
    /// An M partition; the mode defaults to that of the table's existing partitions, else import.
    | Partition of m: string * mode: ModeType option

module Details =
    let kind =
        function
        | Details.Measure _ -> ObjectKind.Measure
        | Details.Column _ -> ObjectKind.Column
        | Details.Table _ -> ObjectKind.Table
        | Details.Hierarchy _ -> ObjectKind.Hierarchy
        | Details.Level _ -> ObjectKind.Level
        | Details.CalculationItem _ -> ObjectKind.CalculationItem
        | Details.Relationship _ -> ObjectKind.Relationship
        | Details.Role _ -> ObjectKind.Role
        | Details.Perspective _ -> ObjectKind.Perspective
        | Details.Culture -> ObjectKind.Culture
        | Details.Expression _ -> ObjectKind.Expression
        | Details.Function _ -> ObjectKind.Function
        | Details.Partition _ -> ObjectKind.Partition

type AddSpec = {
    /// Where the object goes and its name. A relationship's path may be empty: it then gets a new
    /// GUID as its name, as in Power BI.
    Ref: ObjectRef
    Details: Details
    Metadata: Metadata
    /// If an object with this reference already exists, do nothing (and say so) instead of failing.
    IfNotExists: bool
}

module AddSpec =
    let create (r: ObjectRef) (details: Details) = {
        Ref = r
        Details = details
        Metadata = Metadata.none
        IfNotExists = false
    }

/// The kinds `add` creates.
let addableKinds = [
    ObjectKind.Measure
    ObjectKind.Column
    ObjectKind.Table
    ObjectKind.Hierarchy
    ObjectKind.Level
    ObjectKind.CalculationItem
    ObjectKind.Relationship
    ObjectKind.Role
    ObjectKind.Perspective
    ObjectKind.Culture
    ObjectKind.Expression
    ObjectKind.Function
    ObjectKind.Partition
]

/// Which shared metadata each kind has: (description, displayFolder, formatString, hidden).
let private metadataOf =
    function
    | ObjectKind.Column
    | ObjectKind.Measure -> true, true, true, true
    | ObjectKind.Hierarchy -> true, true, false, true
    | ObjectKind.Table
    | ObjectKind.Function -> true, false, false, true
    | ObjectKind.Level
    | ObjectKind.CalculationItem
    | ObjectKind.Role
    | ObjectKind.Perspective
    | ObjectKind.Expression
    | ObjectKind.Partition -> true, false, false, false
    | _ -> false, false, false, false

/// The shared metadata a kind supports, by name: "description", "displayFolder", "formatString", "hidden".
let supportsMetadata (kind: ObjectKind) (property: string) =
    let d, f, s, h = metadataOf kind

    match property with
    | "description" -> d
    | "displayFolder" -> f
    | "formatString" -> s
    | "hidden" -> h
    | _ -> false

/// The first compatibility level with calculation groups, and with DAX user-defined functions.
let private calculationGroupLevel = 1470
/// Calculation items have ordinals from 1500.
let private ordinalLevel = 1500
let private functionLevel = 1702

let private ci = StringComparison.OrdinalIgnoreCase
let private eqCi (a: string) (b: string) = String.Equals(a, b, ci)

/// Power BI's lineage tags: a lowercase, hyphenated GUID.
let private newTag () = Guid.NewGuid().ToString()

type private ResultBuilder() =
    member _.Bind(r: Result<'a, TessError>, f: 'a -> Result<'b, TessError>) = Result.bind f r
    member _.Return(v: 'a) : Result<'a, TessError> = Ok v
    member _.ReturnFrom(r: Result<'a, TessError>) = r
    member _.Zero() : Result<unit, TessError> = Ok()
    member _.Delay(f: unit -> Result<'a, TessError>) = f
    member _.Run(f: unit -> Result<'a, TessError>) = f ()
    member _.Combine(r: Result<unit, TessError>, f: unit -> Result<'a, TessError>) = Result.bind f r

    member _.For(xs: 'a seq, body: 'a -> Result<unit, TessError>) =
        xs |> Seq.fold (fun acc x -> acc |> Result.bind (fun () -> body x)) (Ok())

let private result = ResultBuilder()

let private check (ok: bool) (error: TessError) = if ok then Ok() else Error error

// --- finding parents ------------------------------------------------------------------------------

/// Edit distance, case-insensitive (for "did you mean").
let private distance (a: string) (b: string) =
    let a, b = a.ToLowerInvariant(), b.ToLowerInvariant()

    let d =
        Array2D.init (a.Length + 1) (b.Length + 1) (fun i j ->
            if i = 0 then j
            elif j = 0 then i
            else 0)

    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)

    d.[a.Length, b.Length]

/// Names close to `wanted`: within a third of its length (at least 2 edits), or containing it.
let private similar (candidates: string seq) (wanted: string) =
    let limit = max 2 (wanted.Length / 3)

    candidates
    |> Seq.map (fun c -> c, distance c wanted)
    |> Seq.filter (fun (c, d) -> d <= limit || c.Contains(wanted, ci) || wanted.Contains(c, ci))
    |> Seq.sortBy snd
    |> Seq.truncate 3
    |> Seq.map fst
    |> List.ofSeq

/// Exact case first, then any case (Tabular names are case-insensitive).
let private pick (items: 'T seq) (nameOf: 'T -> string) (wanted: string) =
    let items = List.ofSeq items

    items
    |> List.tryFind (fun x -> nameOf x = wanted)
    |> Option.orElse (items |> List.tryFind (fun x -> eqCi (nameOf x) wanted))

let private notFound (input: ObjectRef) (candidates: ObjectRef list) =
    match similar (candidates |> List.map ObjectRef.name) (ObjectRef.name input) with
    | [] -> ObjectNotFound(ObjectRef.format input)
    | close ->
        let byName =
            candidates
            |> List.filter (fun c -> List.contains (ObjectRef.name c) close)
            |> List.sortBy (fun c -> List.findIndex ((=) (ObjectRef.name c)) close)

        ObjectNotFoundSuggest(ObjectRef.format input, byName |> List.map ObjectRef.format)

let private findTable (m: Model) (name: string) =
    match pick m.Tables (fun t -> t.Name) name with
    | Some t -> Ok t
    | None -> Error(notFound (ObjectRef.table name) [ for t in m.Tables -> ObjectRef.table t.Name ])

let private findColumn (t: Table) (name: string) =
    let columns = t.Columns |> Seq.filter (fun c -> c.Type <> ColumnType.RowNumber)

    match pick columns (fun c -> c.Name) name with
    | Some c -> Ok c
    | None ->
        Error(
            notFound (ObjectRef.inTable ObjectKind.Column t.Name name) [
                for c in columns -> ObjectRef.inTable ObjectKind.Column t.Name c.Name
            ]
        )

let private findHierarchy (t: Table) (name: string) =
    match pick t.Hierarchies (fun h -> h.Name) name with
    | Some h -> Ok h
    | None ->
        Error(
            notFound (ObjectRef.inTable ObjectKind.Hierarchy t.Name name) [
                for h in t.Hierarchies -> ObjectRef.inTable ObjectKind.Hierarchy t.Name h.Name
            ]
        )

/// A column given in any form `get` accepts.
let private resolveColumn (entries: ObjectEntry list) (what: string) (selector: string) =
    Selector.resolve entries selector
    |> Result.bind (fun e ->
        match e.Object with
        | :? Column as c -> Ok c
        | _ -> Error(CannotAdd(what, $"'{selector}' is {ObjectRef.format e.Ref}, not a column")))

// --- existence and names --------------------------------------------------------------------------

let private existingAt (entries: ObjectEntry list) (r: ObjectRef) =
    entries
    |> List.tryFind (fun e ->
        e.Ref.Kind = r.Kind
        && e.Ref.Path.Length = r.Path.Length
        && List.forall2 eqCi e.Ref.Path r.Path)

let private already (r: ObjectRef) = {
    Mutation.Effect.none with
        Notes = [ $"{ObjectRef.format r} already exists; nothing to do." ]
}

/// Claim `r` for a new object: if one is there already, a no-op (with ifNotExists) or a collision;
/// a sibling with the name is always a collision. Otherwise run `create`.
let private claim
    (entries: ObjectEntry list)
    (ifNotExists: bool)
    (r: ObjectRef)
    (create: unit -> Result<Mutation.Effect, TessError>)
    =
    match existingAt entries r with
    | Some e when ifNotExists -> Ok(already e.Ref)
    | Some e -> Error(NameCollision(ObjectRef.name r, ObjectRef.format e.Ref))
    | None ->
        match Rename.collision entries r with
        | Some clash -> Error(NameCollision(ObjectRef.name r, ObjectRef.format clash.Ref))
        | None -> create ()

let private created (r: ObjectRef) (notes: string list) (warnings: string list) : Mutation.Effect = {
    Warnings = warnings
    Notes = $"Added {ObjectRef.format r}" :: notes
    Renamed = []
}

let private required (r: ObjectRef) (what: string) (value: string) =
    check (not (String.IsNullOrWhiteSpace value)) (CannotAdd(ObjectRef.format r, $"{what} is empty"))

// --- metadata -------------------------------------------------------------------------------------

let private checkMetadata (r: ObjectRef) (md: Metadata) =
    let kind = ObjectKind.token r.Kind

    let unsupported =
        [
            md.Description.IsSome, "description", "a description"
            md.DisplayFolder.IsSome, "displayFolder", "a display folder"
            md.FormatString.IsSome, "formatString", "a format string"
            md.Hidden, "hidden", "a hidden flag"
        ]
        |> List.filter (fun (set, name, _) -> set && not (supportsMetadata r.Kind name))

    match unsupported with
    | [] -> Ok()
    | (_, _, what) :: _ -> Error(CannotAdd(ObjectRef.format r, $"{kind} objects can't have {what}"))

let private applyMetadata (md: Metadata) (o: MetadataObject) =
    let set (v: string option) (f: string -> unit) = Option.iter f v

    match o with
    | :? Measure as x ->
        set md.Description (fun v -> x.Description <- v)
        set md.DisplayFolder (fun v -> x.DisplayFolder <- v)
        set md.FormatString (fun v -> x.FormatString <- v)
        x.IsHidden <- md.Hidden
    | :? Column as x ->
        set md.Description (fun v -> x.Description <- v)
        set md.DisplayFolder (fun v -> x.DisplayFolder <- v)
        set md.FormatString (fun v -> x.FormatString <- v)
        x.IsHidden <- md.Hidden
    | :? Hierarchy as x ->
        set md.Description (fun v -> x.Description <- v)
        set md.DisplayFolder (fun v -> x.DisplayFolder <- v)
        x.IsHidden <- md.Hidden
    | :? Table as x ->
        set md.Description (fun v -> x.Description <- v)
        x.IsHidden <- md.Hidden
    | :? Function as x ->
        set md.Description (fun v -> x.Description <- v)
        x.IsHidden <- md.Hidden
    | :? Level as x -> set md.Description (fun v -> x.Description <- v)
    | :? CalculationItem as x -> set md.Description (fun v -> x.Description <- v)
    | :? ModelRole as x -> set md.Description (fun v -> x.Description <- v)
    | :? Perspective as x -> set md.Description (fun v -> x.Description <- v)
    | :? NamedExpression as x -> set md.Description (fun v -> x.Description <- v)
    | :? Partition as x -> set md.Description (fun v -> x.Description <- v)
    | _ -> ()

// --- Power BI conventions -------------------------------------------------------------------------

let private isNumeric (t: DataType) =
    t = DataType.Int64 || t = DataType.Double || t = DataType.Decimal

/// A data column as Desktop creates it: numeric columns summarize by sum, others by none, marked as
/// set automatically.
let private dataColumn (name: string) (dataType: DataType) (sourceColumn: string) =
    let c =
        new DataColumn(
            Name = name,
            DataType = dataType,
            SourceColumn = sourceColumn,
            LineageTag = newTag (),
            SummarizeBy =
                (if isNumeric dataType then
                     AggregateFunction.Sum
                 else
                     AggregateFunction.None)
        )

    c.Annotations.Add(new Annotation(Name = "SummarizationSetBy", Value = "Automatic"))
    c

/// Power BI lists its queries (M tables and named expressions) in this model annotation; keep it in
/// step, as `rename` does.
let private queryOrder = "PBI_QueryOrder"

let private relaxedJson =
    JsonSerializerOptions(Encoder = Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let private appendQueryOrder (m: Model) (name: string) =
    match m.Annotations.Find queryOrder with
    | null -> ()
    | a ->
        try
            match JsonSerializer.Deserialize<string array>(a.Value) with
            | null -> ()
            | names when Array.contains name names -> ()
            | names -> a.Value <- JsonSerializer.Serialize(Array.append names [| name |], relaxedJson)
        with _ ->
            ()

/// A table and a named expression are both Power Query queries, so they can't share a name.
let private queryClash (m: Model) (kind: ObjectKind) (name: string) =
    match kind with
    | ObjectKind.Table ->
        pick m.Expressions (fun x -> x.Name) name
        |> Option.map (fun x -> ObjectRef.modelLevel ObjectKind.Expression x.Name)
    | _ ->
        pick m.Tables (fun t -> t.Name) name
        |> Option.map (fun t -> ObjectRef.table t.Name)

let private noQueryClash (m: Model) (kind: ObjectKind) (name: string) =
    match queryClash m kind name with
    | Some clash -> Error(NameCollision(name, ObjectRef.format clash + " (a Power Query query)"))
    | None -> Ok()

let private isCalculatedTable (t: Table) =
    isNull t.CalculationGroup
    && t.Partitions
       |> Seq.exists (fun p -> p.SourceType = PartitionSourceType.Calculated)

/// Insert at `ordinal` (0-based position; default last) and renumber 0..n.
let private placeAt (existing: 'T list) (ordinalOf: 'T -> int) (setOrdinal: 'T -> int -> unit) (ordinal: int option) =
    let ordered = existing |> List.sortBy ordinalOf

    match ordinal with
    | None -> (existing |> List.map ordinalOf |> List.fold max (-1)) + 1
    | Some o ->
        ordered
        |> List.iteri (fun i x ->
            let wanted = if i < o then i else i + 1

            if ordinalOf x <> wanted then
                setOrdinal x wanted)

        o

let private checkOrdinal (r: ObjectRef) (count: int) (ordinal: int option) =
    match ordinal with
    | Some o when o < 0 || o > count ->
        Error(CannotAdd(ObjectRef.format r, $"ordinal {o} is out of range (0 to {count})"))
    | _ -> Ok()

// --- each kind ------------------------------------------------------------------------------------

let private addMeasure (db: Database) entries (spec: AddSpec) (table: string) (name: string) (expression: string) =
    result {
        let! t = findTable db.Model table
        let r = ObjectRef.inTable ObjectKind.Measure t.Name name

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    do! required r "the expression" expression

                    let x = new Measure(Name = name, Expression = expression, LineageTag = newTag ())
                    applyMetadata spec.Metadata x
                    t.Measures.Add x
                    return created r [] []
                })
    }

let private addColumn (db: Database) entries (spec: AddSpec) (table: string) (name: string) (source: ColumnSource) =
    result {
        let! t = findTable db.Model table
        let r = ObjectRef.inTable ObjectKind.Column t.Name name

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    do!
                        check
                            (isNull t.CalculationGroup)
                            (CannotAdd(
                                ObjectRef.format r,
                                $"'{t.Name}' is a calculation group; its columns are fixed (add calculation items instead)"
                            ))

                    do!
                        check
                            (not (isCalculatedTable t))
                            (CannotAdd(
                                ObjectRef.format r,
                                $"'{t.Name}' is a calculated table: its columns come from its DAX expression and are inferred by the engine (change the table's expression instead)"
                            ))

                    let! c, notes =
                        match source with
                        | ColumnSource.Calculated expression ->
                            required r "the expression" expression
                            |> Result.map (fun () ->
                                (new CalculatedColumn(Name = name, Expression = expression, LineageTag = newTag ())
                                :> Column),
                                [])
                        | ColumnSource.Data(dataType, sourceColumn) ->
                            let sourceColumn = defaultArg sourceColumn name

                            required r "the source column" sourceColumn
                            |> Result.map (fun () -> (dataColumn name dataType sourceColumn :> Column), [])

                    applyMetadata spec.Metadata c
                    t.Columns.Add c
                    return created r notes []
                })
    }

let private addTable (db: Database) entries (spec: AddSpec) (name: string) (source: TableSource) =
    let m = db.Model
    let r = ObjectRef.table name

    claim entries spec.IfNotExists r (fun () ->
        result {
            let t = new Table(Name = name, LineageTag = newTag ())

            let! notes =
                match source with
                | TableSource.Calculated expression ->
                    required r "the expression" expression
                    |> Result.map (fun () ->
                        t.Partitions.Add(
                            new Partition(
                                Name = name,
                                Mode = ModeType.Import,
                                Source = new CalculatedPartitionSource(Expression = expression)
                            )
                        )

                        [ "Its columns are inferred from the expression when the model is processed." ])
                | TableSource.M(expression, mode) ->
                    result {
                        do! required r "the M expression" expression
                        do! noQueryClash m ObjectKind.Table name

                        t.Partitions.Add(
                            new Partition(
                                Name = name,
                                Mode = mode,
                                Source = new MPartitionSource(Expression = expression)
                            )
                        )

                        return [
                            "It has no columns yet: add data columns, or let Power BI read them from the query on refresh."
                        ]
                    }
                | TableSource.CalculationGroup(precedence, column) ->
                    let column = defaultArg column "Name"

                    result {
                        do!
                            check
                                (db.CompatibilityLevel >= calculationGroupLevel)
                                (CannotAdd(
                                    ObjectRef.format r,
                                    $"calculation groups need compatibility level {calculationGroupLevel} or higher (the model is at {db.CompatibilityLevel})"
                                ))

                        do! Rename.validateName ObjectKind.Column column

                        do!
                            check
                                (not (eqCi column "Ordinal"))
                                (CannotAdd(
                                    ObjectRef.format r,
                                    "the calculation group's column can't be named 'Ordinal'"
                                ))

                        let precedence =
                            precedence
                            |> Option.defaultWith (fun () ->
                                m.Tables
                                |> Seq.choose (fun t -> Option.ofObj t.CalculationGroup)
                                |> Seq.map (fun g -> g.Precedence)
                                |> Seq.fold max (-1)
                                |> (+) 1)

                        t.CalculationGroup <- new CalculationGroup(Precedence = precedence)
                        let ordinal = dataColumn "Ordinal" DataType.Int64 "Ordinal"
                        ordinal.IsHidden <- true
                        ordinal.FormatString <- "0"
                        let items = dataColumn column DataType.String "Name"
                        items.SortByColumn <- ordinal
                        t.Columns.Add items
                        t.Columns.Add ordinal
                        t.Partitions.Add(new Partition(Name = name, Source = new CalculationGroupSource()))

                        let notes = [ $"Precedence {precedence}; it has no calculation items yet." ]

                        if m.DiscourageImplicitMeasures then
                            return notes
                        else
                            m.DiscourageImplicitMeasures <- true

                            return
                                notes
                                @ [
                                    "Set discourageImplicitMeasures on the model: calculation groups require it."
                                ]
                    }

            applyMetadata spec.Metadata t
            m.Tables.Add t

            match source with
            | TableSource.M _ -> appendQueryOrder m name
            | _ -> ()

            return created r notes []
        })

let private addHierarchy (db: Database) entries (spec: AddSpec) (table: string) (name: string) (levels: string list) =
    result {
        let! t = findTable db.Model table
        let r = ObjectRef.inTable ObjectKind.Hierarchy t.Name name

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    do!
                        check
                            (not levels.IsEmpty)
                            (CannotAdd(ObjectRef.format r, "a hierarchy needs at least one level"))

                    let! columns =
                        levels
                        |> List.fold
                            (fun acc l ->
                                acc
                                |> Result.bind (fun cs -> findColumn t l |> Result.map (fun c -> cs @ [ c ])))
                            (Ok [])

                    let names = columns |> List.map (fun c -> c.Name)

                    do!
                        check
                            (List.distinct names = names)
                            (CannotAdd(ObjectRef.format r, "a column can be a level only once in a hierarchy"))

                    let h = new Hierarchy(Name = name, LineageTag = newTag ())

                    columns
                    |> List.iteri (fun i c ->
                        h.Levels.Add(new Level(Name = c.Name, Ordinal = i, Column = c, LineageTag = newTag ())))

                    applyMetadata spec.Metadata h
                    t.Hierarchies.Add h
                    return created r [] []
                })
    }

let private addLevel
    (db: Database)
    entries
    (spec: AddSpec)
    (table: string)
    (hierarchy: string)
    (name: string)
    (column: string option)
    (ordinal: int option)
    =
    result {
        let! t = findTable db.Model table
        let! h = findHierarchy t hierarchy
        let r = ObjectRef.level t.Name h.Name name

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    let! c = findColumn t (defaultArg column name)

                    match h.Levels |> Seq.tryFind (fun l -> obj.ReferenceEquals(l.Column, c)) with
                    | Some l ->
                        return!
                            Error(
                                CannotAdd(
                                    ObjectRef.format r,
                                    $"column '{c.Name}' is already level '{l.Name}' of this hierarchy; a column can be a level only once"
                                )
                            )
                    | None ->
                        do! checkOrdinal r h.Levels.Count ordinal

                        let at =
                            placeAt (List.ofSeq h.Levels) (fun l -> l.Ordinal) (fun l o -> l.Ordinal <- o) ordinal

                        let l = new Level(Name = name, Ordinal = at, Column = c, LineageTag = newTag ())
                        applyMetadata spec.Metadata l
                        h.Levels.Add l
                        return created r [] []
                })
    }

let private addCalculationItem
    (db: Database)
    entries
    (spec: AddSpec)
    (table: string)
    (name: string)
    (expression: string)
    (formatString: string option)
    (ordinal: int option)
    =
    result {
        let! t = findTable db.Model table
        let r = ObjectRef.inTable ObjectKind.CalculationItem t.Name name

        do!
            check
                (not (isNull t.CalculationGroup))
                (CannotAdd(ObjectRef.format r, $"'{t.Name}' is not a calculation group table"))

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    do! required r "the expression" expression
                    let items = t.CalculationGroup.CalculationItems
                    do! checkOrdinal r items.Count ordinal
                    let ordered = db.CompatibilityLevel >= ordinalLevel

                    do!
                        check
                            (ordered || ordinal.IsNone)
                            (CannotAdd(
                                ObjectRef.format r,
                                $"calculation item ordinals need compatibility level {ordinalLevel} or higher (the model is at {db.CompatibilityLevel})"
                            ))

                    let item = new CalculationItem(Name = name, Expression = expression)

                    if ordered then
                        item.Ordinal <-
                            placeAt (List.ofSeq items) (fun i -> i.Ordinal) (fun i o -> i.Ordinal <- o) ordinal

                    formatString
                    |> Option.iter (fun f -> item.FormatStringDefinition <- new FormatStringDefinition(Expression = f))

                    applyMetadata spec.Metadata item
                    items.Add item
                    return created r [] []
                })
    }

let private addRelationship (db: Database) entries (spec: AddSpec) (rel: RelationshipSpec) =
    let m = db.Model

    let named, name =
        match spec.Ref.Path with
        | [ n ] -> true, n
        | _ -> false, newTag ()

    let r = ObjectRef.modelLevel ObjectKind.Relationship name
    let what = if named then ObjectRef.format r else "relationship"

    result {
        do!
            if named then
                Rename.validateName ObjectKind.Relationship name
            else
                Ok()

        let! from = resolveColumn entries what rel.From
        let! ``to`` = resolveColumn entries what rel.To

        do!
            check
                (not (obj.ReferenceEquals(from.Table, ``to``.Table)))
                (CannotAdd(
                    what,
                    $"both columns are in '{from.Table.Name}'; a relationship can't join a table to itself"
                ))

        let joins (a: Column) (b: Column) (x: Relationship) =
            match x with
            | :? SingleColumnRelationship as s ->
                (obj.ReferenceEquals(s.FromColumn, a) && obj.ReferenceEquals(s.ToColumn, b))
                || (obj.ReferenceEquals(s.FromColumn, b) && obj.ReferenceEquals(s.ToColumn, a))
            | _ -> false

        let sameEndpoints = m.Relationships |> Seq.tryFind (joins from ``to``)
        let byName = if named then existingAt entries r else None

        match sameEndpoints, byName with
        | _, Some e when spec.IfNotExists -> return already e.Ref
        | Some x, _ when spec.IfNotExists ->
            return {
                Mutation.Effect.none with
                    Notes = [
                        $"{ObjectRef.format (ObjectRef.modelLevel ObjectKind.Relationship x.Name)} already joins {relationshipEndpoints x}; nothing to do."
                    ]
            }
        | Some x, _ ->
            return!
                Error(
                    CannotAdd(
                        what,
                        $"{ObjectRef.format (ObjectRef.modelLevel ObjectKind.Relationship x.Name)} already joins {relationshipEndpoints x}"
                    )
                )
        | None, _ ->
            return!
                claim entries false r (fun () ->
                    let from', to' =
                        match rel.Cardinality with
                        | Cardinality.ManyToOne -> RelationshipEndCardinality.Many, RelationshipEndCardinality.One
                        | Cardinality.OneToOne -> RelationshipEndCardinality.One, RelationshipEndCardinality.One
                        | Cardinality.ManyToMany -> RelationshipEndCardinality.Many, RelationshipEndCardinality.Many

                    let crossFilter =
                        match rel.CrossFilter, rel.Cardinality with
                        | Some CrossFilter.Both, _
                        | None, Cardinality.OneToOne -> CrossFilteringBehavior.BothDirections
                        | _ -> CrossFilteringBehavior.OneDirection

                    let sameTables (x: Relationship) =
                        x.IsActive
                        && ((obj.ReferenceEquals(x.FromTable, from.Table)
                             && obj.ReferenceEquals(x.ToTable, ``to``.Table))
                            || (obj.ReferenceEquals(x.FromTable, ``to``.Table)
                                && obj.ReferenceEquals(x.ToTable, from.Table)))

                    let activeAlready =
                        if rel.Inactive then
                            None
                        else
                            m.Relationships |> Seq.tryFind sameTables

                    let x =
                        new SingleColumnRelationship(
                            Name = name,
                            FromColumn = from,
                            ToColumn = ``to``,
                            FromCardinality = from',
                            ToCardinality = to',
                            CrossFilteringBehavior = crossFilter,
                            IsActive = (not rel.Inactive && activeAlready.IsNone)
                        )

                    m.Relationships.Add x

                    let warnings = [
                        match activeAlready with
                        | Some other ->
                            $"{ObjectRef.format (ObjectRef.modelLevel ObjectKind.Relationship other.Name)} already joins '{from.Table.Name}' and '{``to``.Table.Name}' and is active, so the new relationship is inactive (as Power BI Desktop makes it); use it with USERELATIONSHIP"
                        | None -> ()

                        if from.DataType <> ``to``.DataType then
                            $"the columns have different data types ({from.DataType} and {``to``.DataType})"
                    ]

                    Ok {
                        Warnings = warnings
                        Notes = [ $"Added {ObjectRef.format r} ({relationshipEndpoints x})" ]
                        Renamed = []
                    })
    }

let private addRole (db: Database) entries (spec: AddSpec) (name: string) permission (filters: (string * string) list) =
    let r = ObjectRef.modelLevel ObjectKind.Role name

    claim entries spec.IfNotExists r (fun () ->
        result {
            let! tables =
                filters
                |> List.fold
                    (fun acc (table, dax) ->
                        acc
                        |> Result.bind (fun xs ->
                            result {
                                let! t = findTable db.Model table
                                do! required r $"the filter on '{t.Name}'" dax
                                return xs @ [ t, dax ]
                            }))
                    (Ok [])

            let names = tables |> List.map (fun (t, _) -> t.Name)

            do!
                check
                    (List.distinct names = names)
                    (CannotAdd(ObjectRef.format r, "a table can have only one filter per role"))

            let role = new ModelRole(Name = name, ModelPermission = permission)

            for t, dax in tables do
                role.TablePermissions.Add(new TablePermission(Table = t, FilterExpression = dax))

            applyMetadata spec.Metadata role
            db.Model.Roles.Add role
            return created r [] []
        })

let private addPerspective (db: Database) entries (spec: AddSpec) (name: string) (includes: string list) =
    let r = ObjectRef.modelLevel ObjectKind.Perspective name

    claim entries spec.IfNotExists r (fun () ->
        result {
            let! members =
                includes
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
                                | ObjectKind.Hierarchy -> Ok(xs @ [ e.Object ])
                                | k ->
                                    Error(
                                        CannotAdd(
                                            ObjectRef.format r,
                                            $"'{selector}' is a {ObjectKind.token k}; perspectives hold tables, columns, measures and hierarchies"
                                        )
                                    ))))
                    (Ok [])

            let p = new Perspective(Name = name)
            applyMetadata spec.Metadata p
            db.Model.Perspectives.Add p

            let tableIn (t: Table) =
                match p.PerspectiveTables |> Seq.tryFind (fun pt -> obj.ReferenceEquals(pt.Table, t)) with
                | Some pt -> pt
                | None ->
                    let pt = new PerspectiveTable(Table = t)
                    p.PerspectiveTables.Add pt
                    pt

            let column (t: Table) (c: Column) =
                let pt = tableIn t

                if not (pt.PerspectiveColumns |> Seq.exists (fun x -> obj.ReferenceEquals(x.Column, c))) then
                    pt.PerspectiveColumns.Add(new PerspectiveColumn(Column = c))

            let measure (t: Table) (x: Measure) =
                let pt = tableIn t

                if
                    not (
                        pt.PerspectiveMeasures
                        |> Seq.exists (fun y -> obj.ReferenceEquals(y.Measure, x))
                    )
                then
                    pt.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = x))

            let hierarchy (t: Table) (h: Hierarchy) =
                let pt = tableIn t

                if
                    not (
                        pt.PerspectiveHierarchies
                        |> Seq.exists (fun y -> obj.ReferenceEquals(y.Hierarchy, h))
                    )
                then
                    pt.PerspectiveHierarchies.Add(new PerspectiveHierarchy(Hierarchy = h))

            for o in members do
                match o with
                | :? Table as t ->
                    tableIn t |> ignore

                    for c in t.Columns do
                        if c.Type <> ColumnType.RowNumber then
                            column t c

                    for x in t.Measures do
                        measure t x

                    for h in t.Hierarchies do
                        hierarchy t h
                | :? Column as c -> column c.Table c
                | :? Measure as x -> measure x.Table x
                | :? Hierarchy as h -> hierarchy h.Table h
                | _ -> ()

            return created r [] []
        })

let private addCulture (db: Database) entries (spec: AddSpec) (name: string) =
    match Scaffold.tryCulture name with
    | None -> Error(InvalidName(name, "not a specific culture name, such as da-DK"))
    | Some canonical ->
        let r = ObjectRef.modelLevel ObjectKind.Culture canonical

        claim entries spec.IfNotExists r (fun () ->
            db.Model.Cultures.Add(new Culture(Name = canonical))
            Ok(created r [] []))

let private addExpression (db: Database) entries (spec: AddSpec) (name: string) (m: string) =
    let r = ObjectRef.modelLevel ObjectKind.Expression name

    claim entries spec.IfNotExists r (fun () ->
        result {
            do! required r "the M expression" m
            do! noQueryClash db.Model ObjectKind.Expression name

            let x =
                new NamedExpression(Name = name, Kind = ExpressionKind.M, Expression = m, LineageTag = newTag ())

            applyMetadata spec.Metadata x
            db.Model.Expressions.Add x
            appendQueryOrder db.Model name
            return created r [] []
        })

let private addFunction (db: Database) entries (spec: AddSpec) (name: string) (expression: string) =
    let r = ObjectRef.modelLevel ObjectKind.Function name

    claim entries spec.IfNotExists r (fun () ->
        result {
            do!
                check
                    (db.CompatibilityLevel >= functionLevel)
                    (CannotAdd(
                        ObjectRef.format r,
                        $"functions need compatibility level {functionLevel} or higher (the model is at {db.CompatibilityLevel})"
                    ))

            do! required r "the expression" expression
            let f = new Function(Name = name, Expression = expression, LineageTag = newTag ())
            applyMetadata spec.Metadata f
            db.Model.Functions.Add f
            return created r [] []
        })

let private addPartition (db: Database) entries (spec: AddSpec) (table: string) (name: string) (m: string) mode =
    result {
        let! t = findTable db.Model table
        let r = ObjectRef.inTable ObjectKind.Partition t.Name name

        let refuse reason =
            Error(CannotAdd(ObjectRef.format r, reason))

        return!
            claim entries spec.IfNotExists r (fun () ->
                result {
                    do! required r "the M expression" m

                    do!
                        if not (isNull t.CalculationGroup) then
                            refuse $"'{t.Name}' is a calculation group, which has exactly one partition"
                        elif isCalculatedTable t then
                            refuse $"'{t.Name}' is a calculated table, which has exactly one partition"
                        else
                            match t.Partitions |> Seq.tryFind (fun p -> p.SourceType <> PartitionSourceType.M) with
                            | Some p ->
                                refuse
                                    $"'{t.Name}' has {p.SourceType} partitions; tess adds M partitions, which can't be mixed with them"
                            | None -> Ok()

                    let mode =
                        mode
                        |> Option.orElse (t.Partitions |> Seq.tryHead |> Option.map (fun p -> p.Mode))
                        |> Option.defaultValue ModeType.Import

                    let p =
                        new Partition(Name = name, Mode = mode, Source = new MPartitionSource(Expression = m))

                    applyMetadata spec.Metadata p
                    t.Partitions.Add p
                    return created r [] []
                })
    }

/// Create the object `spec` describes, in `db`. Run it through Mutation.apply for the change set.
let add (db: Database) (spec: AddSpec) : Result<Mutation.Effect, TessError> =
    let r = spec.Ref
    let kind = Details.kind spec.Details
    let entries = objects db

    let refText =
        if r.Path.IsEmpty then
            ObjectKind.token r.Kind
        else
            ObjectRef.format r

    let arityOk =
        r.Path.Length = ObjectKind.arity r.Kind
        || (r.Kind = ObjectKind.Relationship && r.Path.IsEmpty)

    result {
        do!
            check
                (List.contains r.Kind addableKinds)
                (CannotAdd(refText, $"tess can't add a {ObjectKind.token r.Kind}"))

        do!
            check
                (r.Kind = kind)
                (CannotAdd(
                    refText,
                    $"the details given are for a {ObjectKind.token kind}, not a {ObjectKind.token r.Kind}"
                ))

        do!
            check
                arityOk
                (InvalidObjectRef(
                    refText,
                    $"'{ObjectKind.token r.Kind}' takes {ObjectKind.arity r.Kind} name segment(s)"
                ))

        do! checkMetadata r spec.Metadata

        do!
            match r.Kind, r.Path with
            | (ObjectKind.Culture | ObjectKind.Relationship), _
            | _, [] -> Ok()
            | k, path -> Rename.validateName k (List.last path)

        return!
            match spec.Details, r.Path with
            | Details.Measure e, [ t; n ] -> addMeasure db entries spec t n e
            | Details.Column source, [ t; n ] -> addColumn db entries spec t n source
            | Details.Table source, [ n ] -> addTable db entries spec n source
            | Details.Hierarchy levels, [ t; n ] -> addHierarchy db entries spec t n levels
            | Details.Level(column, ordinal), [ t; h; n ] -> addLevel db entries spec t h n column ordinal
            | Details.CalculationItem(e, f, ordinal), [ t; n ] -> addCalculationItem db entries spec t n e f ordinal
            | Details.Relationship rel, _ -> addRelationship db entries spec rel
            | Details.Role(permission, filters), [ n ] -> addRole db entries spec n permission filters
            | Details.Perspective includes, [ n ] -> addPerspective db entries spec n includes
            | Details.Culture, [ n ] -> addCulture db entries spec n
            | Details.Expression m, [ n ] -> addExpression db entries spec n m
            | Details.Function e, [ n ] -> addFunction db entries spec n e
            | Details.Partition(m, mode), [ t; n ] -> addPartition db entries spec t n m mode
            | _ -> Error(InvalidObjectRef(refText, "wrong number of name segments"))
    }
