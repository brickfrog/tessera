/// The objects Best Practice Analyzer rule expressions run against: one wrapper per TOM object,
/// exposing the property and method names Tabular Editor 2's TOMWrapper gives rules (Name, IsVisible,
/// ReferencedBy, UsedInRelationships, Tokenize(), GetAnnotation(), ...). Read-only.
///
/// Names and semantics follow TE2 (MIT, https://github.com/TabularEditor/TabularEditor), in
/// particular TOMWrapper's Column/Table/Measure classes, Utils/DependencyList.cs and the indexers.
module rec Tessera.BpaObjects

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory
open Tessera.BpaDaxToken

// TOM types share names with the wrappers below, which shadow them; these keep TOM reachable.
type internal TomModel = Microsoft.AnalysisServices.Tabular.Model
type internal TomTable = Microsoft.AnalysisServices.Tabular.Table
type internal TomColumn = Microsoft.AnalysisServices.Tabular.Column
type internal TomDataColumn = Microsoft.AnalysisServices.Tabular.DataColumn
type internal TomCalculatedColumn = Microsoft.AnalysisServices.Tabular.CalculatedColumn
type internal TomCalculatedTableColumn = Microsoft.AnalysisServices.Tabular.CalculatedTableColumn
type internal TomMeasure = Microsoft.AnalysisServices.Tabular.Measure
type internal TomKPI = Microsoft.AnalysisServices.Tabular.KPI
type internal TomHierarchy = Microsoft.AnalysisServices.Tabular.Hierarchy
type internal TomLevel = Microsoft.AnalysisServices.Tabular.Level
type internal TomPartition = Microsoft.AnalysisServices.Tabular.Partition
type internal TomRelationship = Microsoft.AnalysisServices.Tabular.SingleColumnRelationship
type internal TomPerspective = Microsoft.AnalysisServices.Tabular.Perspective
type internal TomCulture = Microsoft.AnalysisServices.Tabular.Culture
type internal TomNamedExpression = Microsoft.AnalysisServices.Tabular.NamedExpression
type internal TomModelRole = Microsoft.AnalysisServices.Tabular.ModelRole
type internal TomTablePermission = Microsoft.AnalysisServices.Tabular.TablePermission
type internal TomModelRoleMember = Microsoft.AnalysisServices.Tabular.ModelRoleMember
type internal TomCalculationItem = Microsoft.AnalysisServices.Tabular.CalculationItem
type internal TomFunction = Microsoft.AnalysisServices.Tabular.Function
type internal TomCalendar = Microsoft.AnalysisServices.Tabular.Calendar
type internal TomProviderDataSource = Microsoft.AnalysisServices.Tabular.ProviderDataSource
type internal TomStructuredDataSource = Microsoft.AnalysisServices.Tabular.StructuredDataSource
type internal TomDataSource = Microsoft.AnalysisServices.Tabular.DataSource
type internal TomVariation = Microsoft.AnalysisServices.Tabular.Variation

/// TE2's ObjectType values (TOMWrapper/TabularObject.cs).
type ObjectType =
    | CalculationGroupTable = -7
    | KPIMeasure = -3
    | Folder = -1
    | Null = 0
    | Model = 1
    | DataSource = 2
    | Table = 3
    | Column = 4
    | AttributeHierarchy = 5
    | Partition = 6
    | Relationship = 7
    | Measure = 8
    | Hierarchy = 9
    | Level = 10
    | Annotation = 11
    | KPI = 12
    | Culture = 13
    | ObjectTranslation = 14
    | LinguisticMetadata = 15
    | Perspective = 29
    | PerspectiveTable = 30
    | PerspectiveColumn = 31
    | PerspectiveHierarchy = 32
    | PerspectiveMeasure = 33
    | Role = 34
    | RoleMembership = 35
    | TablePermission = 36
    | Variation = 37
    | Set = 38
    | PerspectiveSet = 39
    | ExtendedProperty = 40
    | Expression = 41
    | ColumnPermission = 42
    | DetailRowsDefinition = 43
    | RelatedColumnDetails = 44
    | GroupByColumn = 45
    | CalculationGroup = 46
    | CalculationItem = 47
    | AlternateOf = 48
    | RefreshPolicy = 49
    | FormatStringDefinition = 50
    | QueryGroup = 51
    | AnalyticsAIMetadata = 52
    | Calendar = 60
    | Function = 63
    | Database = 1000

let private ci = StringComparer.OrdinalIgnoreCase

let private nameEq (a: string) (b: string) = ci.Equals(a, b)

/// "CalculationGroupTable" -> "Calculation Group Table"
let internal splitCamelCase (s: string) =
    let sb = Text.StringBuilder()

    for i in 0 .. s.Length - 1 do
        if i > 0 && Char.IsUpper s.[i] && not (Char.IsUpper s.[i - 1]) then
            sb.Append ' ' |> ignore

        sb.Append s.[i] |> ignore

    sb.ToString()

/// A read-only list of model objects: Count, [index], ["name"], and LINQ (Any, Count(pred), ...).
type ObjectCollection<'T when 'T :> TabularObject and 'T: null>(items: 'T seq) =
    inherit ReadOnlyCollection<'T>(List<'T>(items))

    /// The item with this name (case-insensitive), or null.
    member this.Item
        with get (name: string): 'T = this |> Seq.tryFind (fun x -> nameEq x.Name name) |> Option.toObj

    /// Whether an item has this name.
    member this.ContainsName(name: string) =
        this |> Seq.exists (fun x -> nameEq x.Name name)

/// Per-key values (TE2's indexers): enumerates the values for every key; index by key object or name.
type Indexer<'K, 'V when 'K :> TabularObject and 'K: null>(keys: 'K list, value: 'K -> 'V) =
    inherit ReadOnlyCollection<'V>(List<'V>(keys |> List.map value))

    member _.Item
        with get (name: string): 'V =
            match keys |> List.tryFind (fun k -> nameEq k.Name name) with
            | Some k -> value k
            | None -> Unchecked.defaultof<'V>

    member _.Keys = keys |> List.map (fun k -> k.Name)

/// Translations of one property, per culture. Missing translations are "".
type TranslationIndexer(cultures: Culture list, value: Culture -> string) =
    inherit Indexer<Culture, string>(cultures, value)

    member _.Item
        with get (culture: Culture) = value culture

/// Perspective membership, per perspective.
type PerspectiveIndexer(perspectives: Perspective list, value: Perspective -> bool) =
    inherit Indexer<Perspective, bool>(perspectives, value)

    member _.Item
        with get (perspective: Perspective) = value perspective

/// A table's row-level filter per role (null where the role has none).
type TableRLSIndexer(roles: ModelRole list, value: ModelRole -> string) =
    inherit Indexer<ModelRole, string>(roles, value)

    member _.Item
        with get (role: ModelRole) = value role

/// A role's row-level filter per table (null where it has none).
type RoleRLSIndexer(tables: Table list, value: Table -> string) =
    inherit Indexer<Table, string>(tables, value)

    member _.Item
        with get (table: Table) = value table

/// Object-level security per role.
type OLSIndexer(roles: ModelRole list, value: ModelRole -> MetadataPermission) =
    inherit Indexer<ModelRole, MetadataPermission>(roles, value)

    member _.Item
        with get (role: ModelRole) = value role

/// One place an expression refers to an object.
type ObjectReference(property: string, line: int, fullyQualified: bool) =
    /// The DAX property holding the reference: Expression, FormatStringExpression, ...
    member _.Property = property
    /// 1-based line of the reference in that property.
    member _.Line = line
    /// Written with its table, e.g. 'Sales'[Amount] rather than [Amount].
    member _.FullyQualified = fullyQualified

/// What an object's DAX refers to: object -> its references (TE2's DependsOnList).
type DependsOnList(entries: (TabularObject * List<ObjectReference>) list) =
    inherit
        ReadOnlyCollection<KeyValuePair<TabularObject, List<ObjectReference>>>(List(entries |> List.map KeyValuePair))

    member _.Keys = entries |> List.map fst
    member _.Values = entries |> List.map snd

    member _.Item
        with get (key: TabularObject) = entries |> List.find (fun (k, _) -> obj.ReferenceEquals(k, key)) |> snd

    member _.ContainsKey(key: TabularObject) =
        entries |> List.exists (fun (k, _) -> obj.ReferenceEquals(k, key))

    member this.Measures = ofType<Measure> this.Keys
    member this.Columns = ofType<Column> this.Keys
    member this.Tables = ofType<Table> this.Keys

    /// Everything this depends on, directly or through other objects.
    member this.Deep() =
        let seen = HashSet<TabularObject>(HashIdentity.Reference)

        let rec go (keys: TabularObject list) =
            for k in keys do
                if seen.Add k then
                    go k.DependsOn.Keys

        go this.Keys
        seen

/// The objects whose DAX refers to an object (TE2's ReferencedByList).
type ReferencedByList(items: TabularObject list) =
    inherit ReadOnlyCollection<TabularObject>(List items)

    /// Everything that refers to this, directly or through other objects.
    member this.Deep() =
        let seen = HashSet<TabularObject>(HashIdentity.Reference)

        let rec go (xs: TabularObject seq) =
            for x in xs do
                if seen.Add x then
                    go x.ReferencedBy

        go this
        seen

    member this.Measures = ofType<Measure> this
    member this.Columns = ofType<CalculatedColumn> this
    member this.Tables = ofType<CalculatedTable> this

    member this.Roles =
        ofType<TablePermission> this
        |> List.map (fun tp -> tp.Role)
        |> distinctByReference

    member this.AllMeasures = ofType<Measure>(this.Deep())
    member this.AllColumns = ofType<CalculatedColumn>(this.Deep())
    member this.AllTables = ofType<CalculatedTable>(this.Deep())

    /// Whether any (indirectly) referring object is visible.
    member this.AnyVisible =
        this.Deep()
        |> Seq.exists (fun o ->
            match o with
            | :? Measure as m -> m.IsVisible
            | :? Column as c -> c.IsVisible
            | :? KPI as k -> k.Measure.IsVisible
            | _ -> false)

let internal ofType<'T> (xs: System.Collections.IEnumerable) : 'T list =
    System.Linq.Enumerable.OfType<'T> xs |> List.ofSeq

/// Distinct by object identity, keeping the first occurrence.
let internal distinctByReference<'T> (xs: 'T list) : 'T list =
    let seen = HashSet<obj>(HashIdentity.Reference)
    xs |> List.filter (fun x -> seen.Add(box x))

/// The database: compatibility level and mode.
type Database(db: Microsoft.AnalysisServices.Tabular.Database) =
    member _.Name = db.Name
    member _.ID = db.ID
    member _.CompatibilityLevel = db.CompatibilityLevel
    member _.CompatibilityMode = string db.CompatibilityMode

/// Builds and caches one wrapper per TOM object, and indexes the dependency graph.
type Context(db: Microsoft.AnalysisServices.Tabular.Database) as this =
    let graph = Deps.build db
    let cache = Dictionary<obj, TabularObject>(HashIdentity.Reference)

    let entries =
        let d = Dictionary<MetadataObject, ObjectEntry>(HashIdentity.Reference)

        for e in graph.Entries do
            d.[e.Object] <- e

        d

    let byRef = graph.Entries |> List.map (fun e -> e.Ref, e) |> dict

    // TE2 gives a calculated table's DAX no context table, so an unqualified [X] there is never the
    // table's own column (it names a column of a table expression, e.g. CALENDAR's [Date]).
    let ownUnqualifiedColumn (r: Deps.Reference) =
        match r.From.Kind, r.Target with
        | ObjectKind.Table, Deps.Resolved t when t.Kind = ObjectKind.Column && r.Text.StartsWith "[" ->
            ObjectRef.tableName t = ObjectRef.tableName r.From
        | _ -> false

    let index (key: Deps.Reference -> ObjectRef option) =
        graph.References
        |> List.filter (fun r -> r.Line.IsSome) // DAX references only; structural links have no line
        |> List.filter (ownUnqualifiedColumn >> not)
        |> List.choose (fun r -> key r |> Option.map (fun k -> k, r))
        |> List.groupBy fst
        |> List.map (fun (k, rs) -> k, rs |> List.map snd)
        |> dict

    let bySource = index (fun r -> Some r.From)

    let byTarget =
        index (fun r ->
            match r.Target with
            | Deps.Resolved t -> Some t
            | Deps.Unresolved _ -> None)

    let isCalculatedTable (t: TomTable) =
        isNull t.CalculationGroup
        && t.Partitions
           |> Seq.exists (fun p -> p.SourceType = PartitionSourceType.Calculated)

    let create (o: obj) : TabularObject =
        match o with
        | :? TomModel as m -> Model(this, m)
        | :? TomTable as t when not (isNull t.CalculationGroup) -> CalculationGroupTable(this, t)
        | :? TomTable as t when isCalculatedTable t -> CalculatedTable(this, t)
        | :? TomTable as t -> Table(this, t)
        | :? TomCalculatedTableColumn as c -> CalculatedTableColumn(this, c)
        | :? TomCalculatedColumn as c -> CalculatedColumn(this, c)
        | :? TomDataColumn as c -> DataColumn(this, c)
        | :? TomMeasure as m -> Measure(this, m)
        | :? TomKPI as k -> KPI(this, k)
        | :? TomHierarchy as h -> Hierarchy(this, h)
        | :? TomLevel as l -> Level(this, l)
        | :? TomPartition as p -> Partition(this, p)
        | :? TomRelationship as r -> SingleColumnRelationship(this, r)
        | :? TomPerspective as p -> Perspective(this, p)
        | :? TomCulture as c -> Culture(this, c)
        | :? TomNamedExpression as e -> NamedExpression(this, e)
        | :? TomModelRole as r -> ModelRole(this, r)
        | :? TomTablePermission as tp -> TablePermission(this, tp)
        | :? TomModelRoleMember as m -> ModelRoleMember(this, m)
        | :? TomCalculationItem as c -> CalculationItem(this, c)
        | :? TomFunction as f -> Function(this, f)
        | :? TomCalendar as c -> Calendar(this, c)
        | :? TomProviderDataSource as d -> ProviderDataSource(this, d)
        | :? TomStructuredDataSource as d -> StructuredDataSource(this, d)
        | :? TomVariation as v -> Variation(this, v)
        | o -> failwithf "BPA: no wrapper for %s" (o.GetType().Name)

    member _.TomDatabase = db
    member val Database = Database(db)

    /// The wrapper for a TOM object (the same instance every time); null for null.
    member _.Wrap(o: obj) : TabularObject =
        match o with
        | null -> null
        | o ->
            match cache.TryGetValue o with
            | true, w -> w
            | _ ->
                let w = create o
                cache.[o] <- w
                w

    member this.Get<'T when 'T :> TabularObject and 'T: null>(o: obj) : 'T =
        match this.Wrap o with
        | null -> null
        | w -> w :?> 'T

    member this.All<'T when 'T :> TabularObject and 'T: null>(items: System.Collections.IEnumerable) : 'T list =
        items |> Seq.cast<obj> |> Seq.map this.Get<'T> |> List.ofSeq

    member this.Model: Model = this.Get<Model> db.Model

    /// The inventory entry (and so the object reference) for a TOM object.
    member _.Entry(o: MetadataObject) : ObjectEntry option =
        match entries.TryGetValue o with
        | true, e -> Some e
        | _ -> None

    member this.ByRef(r: ObjectRef) : TabularObject =
        match byRef.TryGetValue r with
        | true, e -> this.Wrap e.Object
        | _ -> null

    /// DAX references made by the object with this reference.
    member _.DaxFrom(r: ObjectRef) : Deps.Reference list =
        match bySource.TryGetValue r with
        | true, rs -> rs
        | _ -> []

    /// DAX references to the object with this reference.
    member _.DaxTo(r: ObjectRef) : Deps.Reference list =
        match byTarget.TryGetValue r with
        | true, rs -> rs
        | _ -> []

    member _.IsCalculatedTable(t: TomTable) = isCalculatedTable t

/// The base of every wrapper: names, annotations, dependencies, translations.
[<AbstractClass; AllowNullLiteral>]
type TabularObject internal (ctx: Context, tom: MetadataObject) =
    let annotations =
        lazy
            (match tom.GetType().GetProperty("Annotations") with
             | null -> []
             | p ->
                 match p.GetValue tom with
                 | :? System.Collections.IEnumerable as xs -> [ for a in Seq.cast<Annotation> xs -> a.Name, a.Value ]
                 | _ -> [])

    let extendedProperties =
        lazy
            (match tom.GetType().GetProperty("ExtendedProperties") with
             | null -> []
             | p ->
                 match p.GetValue tom with
                 | :? System.Collections.IEnumerable as xs -> [
                     for x in Seq.cast<ExtendedProperty> xs ->
                         x.Name,
                         match x with
                         | :? StringExtendedProperty as s -> s.Value
                         | :? JsonExtendedProperty as j -> j.Value
                         | _ -> null
                   ]
                 | _ -> [])

    let dependsOn =
        lazy
            (let refs =
                match ctx.Entry tom with
                | Some e -> ctx.DaxFrom e.Ref
                | None -> []

             TabularObject.MakeDependsOn(ctx, refs))

    let referencedBy =
        lazy
            (match ctx.Entry tom with
             | Some e ->
                 ctx.DaxTo e.Ref
                 |> List.map (fun r -> TabularObject.Referrer(ctx, r))
                 |> List.filter (isNull >> not)
                 |> distinctByReference
                 |> ReferencedByList
             | None -> ReferencedByList [])

    let cache = Dictionary<string, obj>()

    member internal _.Context = ctx
    member internal _.Tom = tom

    /// A value computed once per object (collections are read many times while rules run).
    member internal _.Cached<'T>(key: string, compute: unit -> 'T) : 'T =
        match cache.TryGetValue key with
        | true, v -> unbox<'T> v
        | _ ->
            let v = compute ()
            cache.[key] <- box v
            v

    abstract Name: string
    default _.Name = (tom :?> NamedMetadataObject).Name

    abstract ObjectType: ObjectType

    /// TE2's display type name, e.g. "Calculated Table", "Table (DirectQuery)", "Partition (M - Import)".
    abstract ObjectTypeName: string
    default this.ObjectTypeName = splitCamelCase (string this.ObjectType)

    abstract Description: string
    default _.Description = null

    abstract LineageTag: string
    default _.LineageTag = null

    member _.Model = ctx.Model

    member _.HasAnnotation(name: string) =
        annotations.Value |> List.exists (fun (n, _) -> nameEq n name)

    /// The annotation's value, or null.
    member _.GetAnnotation(name: string) : string =
        annotations.Value
        |> List.tryFind (fun (n, _) -> nameEq n name)
        |> Option.map snd
        |> Option.toObj

    member _.GetAnnotation(index: int) : string =
        if index >= 0 && index < annotations.Value.Length then
            snd annotations.Value.[index]
        else
            null

    member _.GetAnnotations() = annotations.Value |> List.map fst
    member _.GetAnnotationsCount() = annotations.Value.Length

    member _.HasExtendedProperty(name: string) =
        extendedProperties.Value |> List.exists (fun (n, _) -> nameEq n name)

    member _.GetExtendedProperty(name: string) : string =
        extendedProperties.Value
        |> List.tryFind (fun (n, _) -> nameEq n name)
        |> Option.map snd
        |> Option.toObj

    member _.GetExtendedProperties() =
        extendedProperties.Value |> List.map fst

    member _.GetExtendedPropertyCount() = extendedProperties.Value.Length

    /// The objects this object's DAX refers to, with each reference.
    abstract DependsOn: DependsOnList
    default _.DependsOn = dependsOn.Value

    /// The objects whose DAX refers to this object.
    abstract ReferencedBy: ReferencedByList
    default _.ReferencedBy = referencedBy.Value

    /// Tokens of the object's main DAX expression (none if it has none), comments and whitespace included.
    member this.Tokenize() = tokenize true (daxText this)
    member this.Tokenize(includeHidden: bool) = tokenize includeHidden (daxText this)

    member _.TranslatedNames: TranslationIndexer =
        TabularObject.Translations(ctx, tom, TranslatedProperty.Caption)

    member _.TranslatedDescriptions: TranslationIndexer =
        TabularObject.Translations(ctx, tom, TranslatedProperty.Description)

    override this.ToString() = this.Name

    static member internal Translations
        (ctx: Context, tom: MetadataObject, property: TranslatedProperty)
        : TranslationIndexer =
        TranslationIndexer(
            List.ofSeq ctx.Model.Cultures,
            fun c ->
                match (c.Tom :?> TomCulture).ObjectTranslations.[tom, property] with
                | null -> ""
                | t -> t.Value
        )

    static member internal MakeDependsOn(ctx: Context, refs: Deps.Reference list) : DependsOnList =
        refs
        |> List.choose (fun r ->
            match r.Target with
            | Deps.Resolved t ->
                match ctx.ByRef t with
                | null -> None
                | o -> Some(o, r)
            | Deps.Unresolved _ -> None)
        |> List.fold
            (fun (acc: (TabularObject * List<ObjectReference>) list) (o, r) ->
                let reference =
                    ObjectReference(daxProperty r.Via, Option.defaultValue 0 r.Line, not (r.Text.StartsWith "["))

                match acc |> List.tryFind (fun (k, _) -> obj.ReferenceEquals(k, o)) with
                | Some(_, refs) ->
                    refs.Add reference
                    acc
                | None -> (o, List [ reference ]) :: acc)
            []
        |> List.rev
        |> DependsOnList

    /// The wrapper a DAX reference comes from: KPI expressions belong to the KPI, row filters to the
    /// table permission.
    static member internal Referrer(ctx: Context, r: Deps.Reference) : TabularObject =
        match ctx.ByRef r.From with
        | :? Measure as m when r.Via.StartsWith "kpi." -> m.KPI :> TabularObject
        | :? ModelRole as role when r.Via.StartsWith "tablePermissions." ->
            role.TablePermissions
            |> Seq.tryFind (fun tp -> r.Via = $"tablePermissions.{tp.Table.Name}.filterExpression")
            |> Option.map (fun tp -> tp :> TabularObject)
            |> Option.defaultValue (role :> TabularObject)
        | o -> o

/// The DAX Tokenize() reads: TE2's default DAX property per object type.
let internal daxText (o: TabularObject) : string =
    match o with
    | :? Measure as m -> m.Expression
    | :? CalculatedColumn as c -> c.Expression
    | :? CalculatedTable as t -> t.Expression
    | :? Table as t -> t.DefaultDetailRowsExpression
    | :? KPI as k -> k.StatusExpression
    | :? TablePermission as tp -> tp.FilterExpression
    | :? CalculationItem as c -> c.Expression
    | :? Function as f -> f.Expression
    | _ -> null

/// TE2's DAXProperty name for a Deps property path.
let internal daxProperty (via: string) =
    match via with
    | "formatStringDefinition.expression" -> "FormatStringExpression"
    | "detailRowsDefinition.expression" -> "DetailRowsExpression"
    | "defaultDetailRowsDefinition.expression" -> "DefaultDetailRowsExpression"
    | "kpi.statusExpression" -> "StatusExpression"
    | "kpi.targetExpression" -> "TargetExpression"
    | "kpi.trendExpression" -> "TrendExpression"
    | v when v.StartsWith "calculationGroup.noSelectionExpression" -> "NoSelectionExpression"
    | v when v.StartsWith "calculationGroup.multipleOrEmptySelectionExpression" -> "MultipleOrEmptySelectionExpression"
    | _ -> "Expression"

let private perspectiveIndexer (ctx: Context) (isIn: TomPerspective -> bool) =
    PerspectiveIndexer(List.ofSeq ctx.Model.Perspectives, fun p -> isIn (p.Tom :?> TomPerspective))

let private olsIndexer (ctx: Context) (permission: TomModelRole -> MetadataPermission) =
    OLSIndexer(List.ofSeq ctx.Model.Roles, fun r -> permission (r.Tom :?> TomModelRole))

[<AllowNullLiteral>]
type Model internal (ctx: Context, m: TomModel) =
    inherit TabularObject(ctx, m)

    let tables = lazy (ObjectCollection(ctx.All<Table> m.Tables))

    let columns =
        lazy (ObjectCollection(tables.Value |> Seq.collect (fun t -> t.Columns)))

    let measures =
        lazy (ObjectCollection(tables.Value |> Seq.collect (fun t -> t.Measures)))

    let hierarchies =
        lazy (ObjectCollection(tables.Value |> Seq.collect (fun t -> t.Hierarchies)))

    let partitions =
        lazy (ObjectCollection(tables.Value |> Seq.collect (fun t -> t.Partitions)))

    let calendars =
        lazy (ObjectCollection(tables.Value |> Seq.collect (fun t -> t.Calendars)))

    let calculationGroups =
        lazy (ObjectCollection(ofType<CalculationGroupTable> tables.Value))

    override _.ObjectType = ObjectType.Model
    override _.Description = m.Description
    member _.Database = ctx.Database
    member _.Culture = m.Culture
    member _.Collation = m.Collation
    member _.DefaultMode = m.DefaultMode
    member _.DefaultDataView = m.DefaultDataView
    member _.DefaultPowerBIDataSourceVersion = m.DefaultPowerBIDataSourceVersion
    member _.DiscourageImplicitMeasures = m.DiscourageImplicitMeasures
    member _.ForceUniqueNames = m.ForceUniqueNames
    member _.SourceQueryCulture = m.SourceQueryCulture
    member _.Tables: ObjectCollection<Table> = tables.Value
    member _.AllColumns: ObjectCollection<Column> = columns.Value
    member _.AllMeasures: ObjectCollection<Measure> = measures.Value
    member _.AllHierarchies: ObjectCollection<Hierarchy> = hierarchies.Value

    member this.AllLevels =
        this.Cached("AllLevels", fun () -> ObjectCollection(hierarchies.Value |> Seq.collect (fun h -> h.Levels)))

    member _.AllPartitions: ObjectCollection<Partition> = partitions.Value
    member _.AllCalendars: ObjectCollection<Calendar> = calendars.Value
    member _.CalculationGroups = calculationGroups.Value

    member this.AllCalculationItems =
        this.Cached(
            "AllCalculationItems",
            fun () -> ObjectCollection(calculationGroups.Value |> Seq.collect (fun cg -> cg.CalculationItems))
        )

    member this.Relationships: ObjectCollection<SingleColumnRelationship> =
        this.Cached(
            "Relationships",
            fun () ->
                ObjectCollection(
                    ofType<TomRelationship> m.Relationships
                    |> List.map ctx.Get<SingleColumnRelationship>
                )
        )

    member this.Perspectives: ObjectCollection<Perspective> =
        this.Cached("Perspectives", fun () -> ObjectCollection(ctx.All<Perspective> m.Perspectives))

    member this.Cultures: ObjectCollection<Culture> =
        this.Cached("Cultures", fun () -> ObjectCollection(ctx.All<Culture> m.Cultures))

    member this.Roles: ObjectCollection<ModelRole> =
        this.Cached("Roles", fun () -> ObjectCollection(ctx.All<ModelRole> m.Roles))

    member this.Expressions: ObjectCollection<NamedExpression> =
        this.Cached("Expressions", fun () -> ObjectCollection(ctx.All<NamedExpression> m.Expressions))

    member this.DataSources: ObjectCollection<DataSource> =
        this.Cached("DataSources", fun () -> ObjectCollection(ctx.All<DataSource> m.DataSources))

    member this.Functions: ObjectCollection<Function> =
        this.Cached("Functions", fun () -> ObjectCollection(ctx.All<Function> m.Functions))

[<AllowNullLiteral>]
type Table internal (ctx: Context, t: TomTable) =
    inherit TabularObject(ctx, t)

    let columns =
        lazy
            (t.Columns
             |> Seq.filter (fun c -> c.Type <> ColumnType.RowNumber)
             |> Seq.map ctx.Get<Column>
             |> ObjectCollection)

    override _.ObjectType = ObjectType.Table
    override _.Description = t.Description
    override _.LineageTag = t.LineageTag

    override this.ObjectTypeName =
        let modes =
            t.Partitions
            |> Seq.map (fun p ->
                if p.Mode = ModeType.Default then
                    ctx.TomDatabase.Model.DefaultMode
                else
                    p.Mode)
            |> Seq.distinct
            |> List.ofSeq

        match this with
        | :? CalculationGroupTable -> "Calculation Group Table"
        | :? CalculatedTable -> "Calculated Table"
        | _ ->
            match modes with
            | [ mode ] when mode <> ModeType.Default -> $"Table ({mode})"
            | [ _ ]
            | [] -> "Table (Import)"
            | _ -> "Table (Hybrid)"

    member _.IsHidden = t.IsHidden

    member this.IsVisible: bool =
        not t.IsHidden || this.Measures |> Seq.exists (fun m -> not m.IsHidden)

    member _.IsPrivate = t.IsPrivate
    member _.DataCategory = t.DataCategory
    member _.ShowAsVariationsOnly = t.ShowAsVariationsOnly
    member _.ExcludeFromModelRefresh = t.ExcludeFromModelRefresh
    member _.AlternateSourcePrecedence = t.AlternateSourcePrecedence

    member _.DefaultDetailRowsExpression: string =
        if isNull t.DefaultDetailRowsDefinition then
            null
        else
            t.DefaultDetailRowsDefinition.Expression

    member _.Columns: ObjectCollection<Column> = columns.Value

    member this.Measures: ObjectCollection<Measure> =
        this.Cached("Measures", fun () -> ObjectCollection(ctx.All<Measure> t.Measures))

    member this.Hierarchies: ObjectCollection<Hierarchy> =
        this.Cached("Hierarchies", fun () -> ObjectCollection(ctx.All<Hierarchy> t.Hierarchies))

    member this.Partitions: ObjectCollection<Partition> =
        this.Cached("Partitions", fun () -> ObjectCollection(ctx.All<Partition> t.Partitions))

    member this.Calendars: ObjectCollection<Calendar> =
        this.Cached("Calendars", fun () -> ObjectCollection(ctx.All<Calendar> t.Calendars))

    member _.EnableRefreshPolicy = t.RefreshPolicy :? BasicRefreshPolicy

    /// The incremental refresh policy's source expression, or null.
    member _.SourceExpression =
        match t.RefreshPolicy with
        | :? BasicRefreshPolicy as rp -> rp.SourceExpression
        | _ -> null

    member _.DaxObjectName = Dax.quoteTable t.Name
    member this.DaxObjectFullName = this.DaxObjectName
    member this.DaxTableName = this.DaxObjectName

    member this.UsedInRelationships: SingleColumnRelationship list =
        this.Cached(
            "UsedInRelationships",
            fun () ->
                ctx.Model.Relationships
                |> Seq.filter (fun r -> obj.ReferenceEquals(r.FromTable, this) || obj.ReferenceEquals(r.ToTable, this))
                |> List.ofSeq
        )

    member _.InPerspective: PerspectiveIndexer =
        perspectiveIndexer ctx (fun p -> p.PerspectiveTables.Contains t.Name)

    /// The row filter on this table, per role.
    member _.RowLevelSecurity =
        TableRLSIndexer(
            List.ofSeq ctx.Model.Roles,
            fun r ->
                match (r.Tom :?> TomModelRole).TablePermissions.Find t.Name with
                | null -> null
                | tp -> tp.FilterExpression
        )

    member _.ObjectLevelSecurity =
        olsIndexer ctx (fun r ->
            match r.TablePermissions.Find t.Name with
            | null -> MetadataPermission.Default
            | tp -> tp.MetadataPermission)

    member _.GetChildren() : TabularObject list = [
        for m in t.Measures -> ctx.Wrap m
        for c in t.Columns do
            if c.Type <> ColumnType.RowNumber then
                yield ctx.Wrap c
        for h in t.Hierarchies -> ctx.Wrap h
    ]

[<AllowNullLiteral>]
type CalculatedTable internal (ctx: Context, t: TomTable) =
    inherit Table(ctx, t)

    /// The calculated table's DAX.
    member _.Expression: string =
        t.Partitions
        |> Seq.tryPick (fun p ->
            match p.Source with
            | :? CalculatedPartitionSource as s -> Some s.Expression
            | _ -> None)
        |> Option.toObj

[<AllowNullLiteral>]
type CalculationGroupTable internal (ctx: Context, t: TomTable) =
    inherit Table(ctx, t)
    let cg = t.CalculationGroup

    override _.ObjectType = ObjectType.CalculationGroupTable

    member this.CalculationItems: ObjectCollection<CalculationItem> =
        this.Cached(
            "CalculationItems",
            fun () ->
                cg.CalculationItems
                |> Seq.sortBy (fun ci -> ci.Ordinal)
                |> Seq.map ctx.Get<CalculationItem>
                |> ObjectCollection
        )

    member _.CalculationGroupPrecedence = cg.Precedence
    member _.Precedence = cg.Precedence

    member _.CalculationGroupDescription = cg.Description

    member _.NoSelectionExpression =
        if isNull cg.NoSelectionExpression then
            null
        else
            cg.NoSelectionExpression.Expression

    member _.MultipleOrEmptySelectionExpression =
        if isNull cg.MultipleOrEmptySelectionExpression then
            null
        else
            cg.MultipleOrEmptySelectionExpression.Expression

    /// The calculation group's column.
    member this.Field = this.Columns |> Seq.tryHead |> Option.toObj

[<AbstractClass; AllowNullLiteral>]
type Column internal (ctx: Context, c: TomColumn) =
    inherit TabularObject(ctx, c)

    override _.ObjectType = ObjectType.Column
    override _.Description = c.Description
    override _.LineageTag = c.LineageTag
    member _.Table: Table = ctx.Get<Table> c.Table
    member _.DataType = c.DataType
    member _.Type = c.Type
    member _.FormatString = c.FormatString
    member _.DisplayFolder = c.DisplayFolder
    member _.IsHidden = c.IsHidden
    member _.IsVisible: bool = not (c.IsHidden || c.Table.IsHidden)
    member _.IsKey = c.IsKey
    member _.IsNullable = c.IsNullable
    member _.IsUnique = c.IsUnique
    member _.IsAvailableInMDX = c.IsAvailableInMDX
    member _.IsDefaultLabel = c.IsDefaultLabel
    member _.IsDefaultImage = c.IsDefaultImage
    member _.IsDataTypeInferred = c.IsDataTypeInferred
    member _.KeepUniqueRows = c.KeepUniqueRows
    member _.SummarizeBy = c.SummarizeBy
    member _.DataCategory = c.DataCategory
    member _.SourceProviderType = c.SourceProviderType
    member _.Alignment = c.Alignment
    member _.TableDetailPosition = c.TableDetailPosition
    member _.EncodingHint = c.EncodingHint
    member _.ErrorMessage = c.ErrorMessage
    member _.SortByColumn: Column = ctx.Get<Column> c.SortByColumn

    member this.Variations: ObjectCollection<Variation> =
        this.Cached("Variations", fun () -> ObjectCollection(ctx.All<Variation> c.Variations))

    member _.AlternateOf: AlternateOf =
        if isNull c.AlternateOf then
            null
        else
            AlternateOf(ctx, c.AlternateOf)

    member _.DaxObjectName = Dax.bracket c.Name
    member _.DaxTableName = Dax.quoteTable c.Table.Name
    member this.DaxObjectFullName = this.DaxTableName + this.DaxObjectName

    member this.UsedInRelationships: SingleColumnRelationship list =
        this.Cached(
            "UsedInRelationships",
            fun () ->
                ctx.Model.Relationships
                |> Seq.filter (fun r ->
                    obj.ReferenceEquals(r.FromColumn, this) || obj.ReferenceEquals(r.ToColumn, this))
                |> List.ofSeq
        )

    member this.UsedInHierarchies =
        this.Cached(
            "UsedInHierarchies",
            fun () ->
                this.Table.Hierarchies
                |> Seq.filter (fun h -> h.Levels |> Seq.exists (fun l -> obj.ReferenceEquals(l.Column, this)))
                |> List.ofSeq
        )

    member this.UsedInLevels =
        this.Cached(
            "UsedInLevels",
            fun () ->
                this.Table.Hierarchies
                |> Seq.collect (fun h -> h.Levels)
                |> Seq.filter (fun l -> obj.ReferenceEquals(l.Column, this))
                |> List.ofSeq
        )

    member this.UsedInSortBy =
        this.Cached(
            "UsedInSortBy",
            fun () ->
                this.Table.Columns
                |> Seq.filter (fun x -> obj.ReferenceEquals(x.SortByColumn, this))
                |> List.ofSeq
        )

    member this.UsedInVariations =
        this.Cached(
            "UsedInVariations",
            fun () ->
                ctx.Model.AllColumns
                |> Seq.collect (fun x -> x.Variations)
                |> Seq.filter (fun v -> obj.ReferenceEquals(v.DefaultColumn, this))
                |> List.ofSeq
        )

    member this.UsedInAlternateOfs =
        this.Cached(
            "UsedInAlternateOfs",
            fun () ->
                ctx.Model.AllColumns
                |> Seq.choose (fun x -> Option.ofObj x.AlternateOf)
                |> Seq.filter (fun a -> obj.ReferenceEquals(a.BaseColumn, this))
                |> List.ofSeq
        )

    member _.TranslatedDisplayFolders: TranslationIndexer =
        TabularObject.Translations(ctx, c, TranslatedProperty.DisplayFolder)

    member _.InPerspective: PerspectiveIndexer =
        perspectiveIndexer ctx (fun p ->
            match p.PerspectiveTables.Find c.Table.Name with
            | null -> false
            | pt -> pt.PerspectiveColumns.Contains c.Name)

    member _.ObjectLevelSecurity =
        olsIndexer ctx (fun r ->
            match r.TablePermissions.Find c.Table.Name with
            | null -> MetadataPermission.Default
            | tp ->
                match tp.ColumnPermissions.Find c.Name with
                | null -> MetadataPermission.Default
                | cp -> cp.MetadataPermission)

[<AllowNullLiteral>]
type DataColumn internal (ctx: Context, c: TomDataColumn) =
    inherit Column(ctx, c)
    member _.SourceColumn = c.SourceColumn

[<AllowNullLiteral>]
type CalculatedColumn internal (ctx: Context, c: TomCalculatedColumn) =
    inherit Column(ctx, c)
    member _.Expression: string = c.Expression

[<AllowNullLiteral>]
type CalculatedTableColumn internal (ctx: Context, c: TomCalculatedTableColumn) =
    inherit Column(ctx, c)
    member _.SourceColumn = c.SourceColumn
    member _.ColumnOrigin: Column = ctx.Get<Column> c.ColumnOrigin

/// A column's alternate-of (aggregation) definition.
[<AllowNullLiteral>]
type AlternateOf internal (ctx: Context, a: Microsoft.AnalysisServices.Tabular.AlternateOf) =
    member _.BaseColumn: TabularObject = ctx.Wrap a.BaseColumn
    member _.BaseTable: Table = ctx.Get<Table> a.BaseTable
    member _.Summarization = a.Summarization

[<AllowNullLiteral>]
type Measure internal (ctx: Context, m: TomMeasure) =
    inherit TabularObject(ctx, m)

    let dependsOn =
        lazy
            (match ctx.Entry m with
             | Some e ->
                 TabularObject.MakeDependsOn(
                     ctx,
                     ctx.DaxFrom e.Ref |> List.filter (fun r -> not (r.Via.StartsWith "kpi."))
                 )
             | None -> DependsOnList [])

    override _.ObjectType = ObjectType.Measure
    override _.Description = m.Description
    override _.LineageTag = m.LineageTag
    override _.DependsOn = dependsOn.Value
    member _.Table: Table = ctx.Get<Table> m.Table
    member _.Expression: string = m.Expression
    member _.FormatString = m.FormatString

    member _.FormatStringExpression =
        if isNull m.FormatStringDefinition then
            null
        else
            m.FormatStringDefinition.Expression

    member _.DetailRowsExpression =
        if isNull m.DetailRowsDefinition then
            null
        else
            m.DetailRowsDefinition.Expression

    member _.DisplayFolder = m.DisplayFolder
    member _.IsHidden: bool = m.IsHidden
    member _.IsVisible: bool = not m.IsHidden
    member _.IsSimpleMeasure = m.IsSimpleMeasure
    member _.DataType = m.DataType
    member _.DataCategory = m.DataCategory
    member _.ErrorMessage = m.ErrorMessage
    member _.KPI: KPI = ctx.Get<KPI> m.KPI
    member _.DaxObjectName = Dax.bracket m.Name
    member _.DaxTableName = Dax.quoteTable m.Table.Name
    /// As in TE2, a measure's full DAX name is unqualified: [Total].
    member this.DaxObjectFullName = this.DaxObjectName

    member _.TranslatedDisplayFolders: TranslationIndexer =
        TabularObject.Translations(ctx, m, TranslatedProperty.DisplayFolder)

    member _.InPerspective: PerspectiveIndexer =
        perspectiveIndexer ctx (fun p ->
            match p.PerspectiveTables.Find m.Table.Name with
            | null -> false
            | pt -> pt.PerspectiveMeasures.Contains m.Name)

[<AllowNullLiteral>]
type KPI internal (ctx: Context, k: TomKPI) =
    inherit TabularObject(ctx, k)

    let dependsOn =
        lazy
            (match ctx.Entry k.Measure with
             | Some e ->
                 TabularObject.MakeDependsOn(ctx, ctx.DaxFrom e.Ref |> List.filter (fun r -> r.Via.StartsWith "kpi."))
             | None -> DependsOnList [])

    override _.Name = "KPI"
    override _.ObjectType = ObjectType.KPI
    override _.ObjectTypeName = "KPI"
    override _.Description = k.Description
    override _.DependsOn = dependsOn.Value
    override _.ReferencedBy = ReferencedByList []
    member _.Measure: Measure = ctx.Get<Measure> k.Measure
    member this.Table = this.Measure.Table
    member _.StatusExpression: string = k.StatusExpression
    member _.TargetExpression = k.TargetExpression
    member _.TrendExpression = k.TrendExpression
    member _.StatusGraphic = k.StatusGraphic
    member _.TrendGraphic = k.TrendGraphic
    member _.TargetFormatString = k.TargetFormatString
    member _.TargetDescription = k.TargetDescription
    member _.StatusDescription = k.StatusDescription
    member _.TrendDescription = k.TrendDescription

[<AllowNullLiteral>]
type Hierarchy internal (ctx: Context, h: TomHierarchy) =
    inherit TabularObject(ctx, h)

    override _.ObjectType = ObjectType.Hierarchy
    override _.Description = h.Description
    override _.LineageTag = h.LineageTag
    member _.Table: Table = ctx.Get<Table> h.Table
    member _.IsHidden = h.IsHidden
    member _.IsVisible = not (h.IsHidden || h.Table.IsHidden)
    member _.DisplayFolder = h.DisplayFolder
    member _.HideMembers = h.HideMembers

    member this.Levels: ObjectCollection<Level> =
        this.Cached(
            "Levels",
            fun () ->
                h.Levels
                |> Seq.sortBy (fun l -> l.Ordinal)
                |> Seq.map ctx.Get<Level>
                |> ObjectCollection
        )

    member _.DaxObjectName = Dax.bracket h.Name
    member _.DaxTableName = Dax.quoteTable h.Table.Name
    member this.DaxObjectFullName = this.DaxTableName + this.DaxObjectName

    member this.UsedInVariations =
        this.Cached(
            "UsedInVariations",
            fun () ->
                ctx.Model.AllColumns
                |> Seq.collect (fun x -> x.Variations)
                |> Seq.filter (fun v -> obj.ReferenceEquals(v.DefaultHierarchy, this))
                |> List.ofSeq
        )

    member _.TranslatedDisplayFolders: TranslationIndexer =
        TabularObject.Translations(ctx, h, TranslatedProperty.DisplayFolder)

    member _.InPerspective: PerspectiveIndexer =
        perspectiveIndexer ctx (fun p ->
            match p.PerspectiveTables.Find h.Table.Name with
            | null -> false
            | pt -> pt.PerspectiveHierarchies.Contains h.Name)

[<AllowNullLiteral>]
type Level internal (ctx: Context, l: TomLevel) =
    inherit TabularObject(ctx, l)
    override _.ObjectType = ObjectType.Level
    override _.Description = l.Description
    override _.LineageTag = l.LineageTag
    member _.Hierarchy: Hierarchy = ctx.Get<Hierarchy> l.Hierarchy
    member _.Table: Table = ctx.Get<Table> l.Hierarchy.Table
    member _.Column: Column = ctx.Get<Column> l.Column
    member _.Ordinal = l.Ordinal

[<AllowNullLiteral>]
type Partition internal (ctx: Context, p: TomPartition) =
    inherit TabularObject(ctx, p)

    override _.ObjectType = ObjectType.Partition
    override _.Description = p.Description

    override _.ObjectTypeName =
        let mode =
            if p.Mode = ModeType.Default then
                ctx.TomDatabase.Model.DefaultMode
            else
                p.Mode

        match p.Source with
        | :? PolicyRangePartitionSource -> "Partition (Policy Range)"
        | :? EntityPartitionSource -> "Partition (DQ over AS)"
        | :? MPartitionSource -> $"Partition (M - {mode})"
        | _ -> $"Partition (Legacy - {mode})"

    member _.Table: Table = ctx.Get<Table> p.Table
    member _.SourceType = p.SourceType
    member _.Mode = p.Mode
    member _.DataView = p.DataView

    /// The partition's query: M, SQL, or (for calculated partitions) DAX.
    member _.Expression =
        match p.Source with
        | :? MPartitionSource as s -> s.Expression
        | :? QueryPartitionSource as s -> s.Query
        | :? CalculatedPartitionSource as s -> s.Expression
        | _ -> null

    member this.Query = this.Expression

    member _.DataSource: DataSource =
        match p.Source with
        | :? QueryPartitionSource as s -> ctx.Get<DataSource> s.DataSource
        | _ -> null

    member _.EntityName =
        match p.Source with
        | :? EntityPartitionSource as s -> s.EntityName
        | _ -> null

[<AllowNullLiteral>]
type SingleColumnRelationship internal (ctx: Context, r: TomRelationship) =
    inherit TabularObject(ctx, r)
    override _.ObjectType = ObjectType.Relationship
    override _.ObjectTypeName = "Relationship"

    /// TE2 names relationships by their columns: 'Sales'[DateKey] --> 'Date'[DateKey] ("<-->" when
    /// filtering both ways), since the stored name is often a GUID.
    override this.Name =
        let name (c: Column) =
            if isNull c then "(none)" else c.DaxObjectFullName

        let arrow =
            if r.CrossFilteringBehavior = CrossFilteringBehavior.OneDirection then
                "-->"
            else
                "<-->"

        $"{name this.FromColumn} {arrow} {name this.ToColumn}"

    member _.FromTable: Table = ctx.Get<Table> r.FromTable
    member _.ToTable: Table = ctx.Get<Table> r.ToTable
    member _.FromColumn: Column = ctx.Get<Column> r.FromColumn
    member _.ToColumn: Column = ctx.Get<Column> r.ToColumn
    member _.FromCardinality = r.FromCardinality
    member _.ToCardinality = r.ToCardinality
    member _.CrossFilteringBehavior = r.CrossFilteringBehavior
    member _.SecurityFilteringBehavior = r.SecurityFilteringBehavior
    member _.IsActive = r.IsActive
    member _.RelyOnReferentialIntegrity = r.RelyOnReferentialIntegrity
    member _.JoinOnDateBehavior = r.JoinOnDateBehavior

    member this.UsedInVariations =
        this.Cached(
            "UsedInVariations",
            fun () ->
                ctx.Model.AllColumns
                |> Seq.collect (fun x -> x.Variations)
                |> Seq.filter (fun v -> obj.ReferenceEquals(v.Relationship, this))
                |> List.ofSeq
        )

[<AllowNullLiteral>]
type Perspective internal (ctx: Context, p: TomPerspective) =
    inherit TabularObject(ctx, p)
    override _.ObjectType = ObjectType.Perspective
    override _.Description = p.Description

[<AllowNullLiteral>]
type Culture internal (ctx: Context, c: TomCulture) =
    inherit TabularObject(ctx, c)
    override _.ObjectType = ObjectType.Culture
    override _.ObjectTypeName = "Translation"

[<AllowNullLiteral>]
type NamedExpression internal (ctx: Context, e: TomNamedExpression) =
    inherit TabularObject(ctx, e)
    override _.ObjectType = ObjectType.Expression
    override _.ObjectTypeName = "Shared Expression"
    override _.Description = e.Description
    override _.LineageTag = e.LineageTag
    member _.Kind = e.Kind
    member _.Expression = e.Expression

[<AllowNullLiteral>]
type ModelRole internal (ctx: Context, r: TomModelRole) =
    inherit TabularObject(ctx, r)
    override _.ObjectType = ObjectType.Role
    override _.ObjectTypeName = "Role"
    override _.Description = r.Description
    member _.ModelPermission = r.ModelPermission

    member this.Members: ObjectCollection<ModelRoleMember> =
        this.Cached("Members", fun () -> ObjectCollection(ctx.All<ModelRoleMember> r.Members))

    member this.TablePermissions: ObjectCollection<TablePermission> =
        this.Cached("TablePermissions", fun () -> ObjectCollection(ctx.All<TablePermission> r.TablePermissions))

    /// The row filter per table.
    member _.RowLevelSecurity =
        RoleRLSIndexer(
            List.ofSeq ctx.Model.Tables,
            fun t ->
                match r.TablePermissions.Find t.Name with
                | null -> null
                | tp -> tp.FilterExpression
        )

[<AllowNullLiteral>]
type TablePermission internal (ctx: Context, tp: TomTablePermission) =
    inherit TabularObject(ctx, tp)

    let dependsOn =
        lazy
            (match ctx.Entry tp.Role with
             | Some e ->
                 let via = $"tablePermissions.{tp.Table.Name}.filterExpression"
                 TabularObject.MakeDependsOn(ctx, ctx.DaxFrom e.Ref |> List.filter (fun r -> r.Via = via))
             | None -> DependsOnList [])

    override _.Name = tp.Table.Name
    override _.ObjectType = ObjectType.TablePermission
    override _.DependsOn = dependsOn.Value
    override _.ReferencedBy = ReferencedByList []
    member _.Role: ModelRole = ctx.Get<ModelRole> tp.Role
    member _.Table: Table = ctx.Get<Table> tp.Table
    member _.FilterExpression: string = tp.FilterExpression
    member _.Expression = tp.FilterExpression
    member _.MetadataPermission = tp.MetadataPermission

[<AllowNullLiteral>]
type ModelRoleMember internal (ctx: Context, m: TomModelRoleMember) =
    inherit TabularObject(ctx, m)
    override _.Name = m.MemberName
    override _.ObjectType = ObjectType.RoleMembership
    member _.Role: ModelRole = ctx.Get<ModelRole> m.Role
    member _.MemberName = m.MemberName
    member _.MemberID = m.MemberID

[<AllowNullLiteral>]
type CalculationItem internal (ctx: Context, c: TomCalculationItem) =
    inherit TabularObject(ctx, c)
    override _.ObjectType = ObjectType.CalculationItem
    override _.Description = c.Description
    member _.Expression: string = c.Expression

    member _.FormatStringExpression =
        if isNull c.FormatStringDefinition then
            null
        else
            c.FormatStringDefinition.Expression

    member _.Ordinal = c.Ordinal

    member _.CalculationGroup: CalculationGroupTable =
        ctx.Get<CalculationGroupTable> c.CalculationGroup.Table

    member _.CalculationGroupTable: CalculationGroupTable =
        ctx.Get<CalculationGroupTable> c.CalculationGroup.Table

    member _.Table: Table = ctx.Get<Table> c.CalculationGroup.Table

[<AllowNullLiteral>]
type Function internal (ctx: Context, f: TomFunction) =
    inherit TabularObject(ctx, f)
    override _.ObjectType = ObjectType.Function
    override _.ObjectTypeName = "User-Defined Function"
    override _.Description = f.Description
    override _.LineageTag = f.LineageTag
    member _.Expression: string = f.Expression
    member _.IsHidden = f.IsHidden
    member _.IsVisible = not f.IsHidden
    member _.DaxObjectName = f.Name
    member _.DaxObjectFullName = f.Name

[<AllowNullLiteral>]
type Calendar internal (ctx: Context, c: TomCalendar) =
    inherit TabularObject(ctx, c)
    override _.ObjectType = ObjectType.Calendar
    override _.Description = c.Description
    override _.LineageTag = c.LineageTag
    member _.Table: Table = ctx.Get<Table> c.Table
    member _.DaxObjectName = Dax.quoteTable c.Name
    member this.DaxObjectFullName = this.DaxObjectName
    member this.DaxTableName = this.DaxObjectName

[<AbstractClass; AllowNullLiteral>]
type DataSource internal (ctx: Context, d: TomDataSource) =
    inherit TabularObject(ctx, d)
    override _.ObjectType = ObjectType.DataSource
    override _.Description = d.Description
    member _.Type = d.Type

    member this.UsedByPartitions =
        this.Cached(
            "UsedByPartitions",
            fun () ->
                ctx.Model.AllPartitions
                |> Seq.filter (fun p -> obj.ReferenceEquals(p.DataSource, this))
                |> List.ofSeq
        )

[<AllowNullLiteral>]
type ProviderDataSource internal (ctx: Context, d: TomProviderDataSource) =
    inherit DataSource(ctx, d)
    override _.ObjectTypeName = "Data Source (Legacy)"
    member _.ConnectionString = d.ConnectionString
    member _.Provider = d.Provider
    member _.ImpersonationMode = d.ImpersonationMode
    member _.Account = d.Account
    member _.Timeout = d.Timeout
    member _.MaxConnections = d.MaxConnections
    member _.Isolation = d.Isolation

[<AllowNullLiteral>]
type StructuredDataSource internal (ctx: Context, d: TomStructuredDataSource) =
    inherit DataSource(ctx, d)
    override _.ObjectTypeName = "Data Source (Power Query)"
    member _.ContextExpression = d.ContextExpression
    member _.MaxConnections = d.MaxConnections

[<AllowNullLiteral>]
type Variation internal (ctx: Context, v: TomVariation) =
    inherit TabularObject(ctx, v)
    override _.ObjectType = ObjectType.Variation
    override _.Description = v.Description
    member _.Column: Column = ctx.Get<Column> v.Column

    member _.Relationship: SingleColumnRelationship =
        ctx.Get<SingleColumnRelationship> v.Relationship

    member _.DefaultHierarchy: Hierarchy = ctx.Get<Hierarchy> v.DefaultHierarchy
    member _.DefaultColumn: Column = ctx.Get<Column> v.DefaultColumn
    member _.IsDefault = v.IsDefault
