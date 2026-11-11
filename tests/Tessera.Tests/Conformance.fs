/// The DAX conformance corpus (tests/conformance): reading it, checking it, and running an engine
/// against it. The format is documented in tests/conformance/README.md.
///
/// Nothing here evaluates DAX. Expected results come from the case files, whose authors took them
/// from published sources; an engine (local, or a real server through ConformanceServer) is handed
/// in as a function.
module Tessera.Tests.Conformance

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera

// ---- The corpus on disk ---------------------------------------------------------------------------

/// The repository root: the nearest folder above the test binaries that holds tessera.sln.
let repositoryRoot: string option =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then
            None
        elif File.Exists(Path.Combine(dir.FullName, "tessera.sln")) then
            Some dir.FullName
        else
            up dir.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

/// tests/conformance, or what TESSERA_CONFORMANCE_DIR names.
let corpusDirectory: string option =
    match Environment.GetEnvironmentVariable "TESSERA_CONFORMANCE_DIR" with
    | null
    | "" ->
        repositoryRoot
        |> Option.map (fun root -> Path.Combine(root, "tests", "conformance"))
        |> Option.filter Directory.Exists
    | dir -> Some(Path.GetFullPath dir)

// ---- Models ---------------------------------------------------------------------------------------

type ColumnDef = {
    Name: string
    Type: QueryType
    SortBy: string option
    Key: bool
}

type CalculatedColumnDef = {
    Name: string
    Type: QueryType
    Expression: string
}

type MeasureDef = {
    Name: string
    Expression: string
    FormatString: string option
    /// A dynamic format string (TOM `FormatStringDefinition`).
    FormatStringExpression: string option
}

type TableDef = {
    Name: string
    Columns: ColumnDef list
    CalculatedColumns: CalculatedColumnDef list
    Measures: MeasureDef list
    /// One array per row, values in the order of Columns.
    Rows: QueryValue array list
}

type CalculatedTableDef = {
    Name: string
    Expression: string
    /// The columns the expression returns, declared so that relationships and measures can name them.
    Columns: ColumnDef list
    CalculatedColumns: CalculatedColumnDef list
    Measures: MeasureDef list
}

type RelationshipDef = {
    FromTable: string
    FromColumn: string
    ToTable: string
    ToColumn: string
    FromMany: bool
    ToMany: bool
    Both: bool
    Active: bool
    /// The relationship's security filter goes both ways (`"securityFilter": "both"`).
    SecurityBoth: bool
}

type FunctionDef = { Name: string; Expression: string }

type CalculationItemDef = {
    Name: string
    Expression: string
    FormatStringExpression: string option
}

type CalculationGroupDef = {
    Name: string
    Column: string
    Precedence: int
    Items: CalculationItemDef list
}

/// A calendar (calendar-based time intelligence): its categories, each with a primary column and
/// associated columns, and its time-related columns.
type CalendarDef = {
    Name: string
    Table: string
    Groups: (TimeUnit * string * string list) list
    TimeRelated: string list
}

/// A role: its permission (`read`, the default, `readRefresh`, `administrator`, `none`, `refresh`)
/// and its row filters, by table.
type RoleDef = {
    Name: string
    Permission: string
    Filters: (string * string) list
}

type ModelDef = {
    Culture: string
    Collation: string option
    Tables: TableDef list
    CalculatedTables: CalculatedTableDef list
    Relationships: RelationshipDef list
    Functions: FunctionDef list
    CalculationGroups: CalculationGroupDef list
    Calendars: CalendarDef list
    Roles: RoleDef list
}

/// A model with its data loaded, ready for an engine.
type Prepared = {
    Database: Database
    Store: DataStore
    /// What `Fixture.loadAndCheck` said about the rows (errors make the case invalid; warnings such
    /// as `unmatched-key` are how a case asks for orphan rows).
    Issues: FixtureIssue list
    /// The rows as the model file gives them, by table, each in the order of the table's data
    /// columns. Unlike the store's, text here has its own casing and trailing spaces: what a server
    /// must be given, so that its own storage rules are the ones observed.
    Rows: Map<string, DaxValue array list>
}

exception private Bad of string

let private bad (message: string) = raise (Bad message)

let private documentOptions =
    JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

let private parseJson (text: string) : JsonNode =
    try
        JsonNode.Parse(text, Nullable(), documentOptions)
    with :? JsonException as e ->
        bad $"not JSON: {e.Message}"

let private kindOf (node: JsonNode) =
    if isNull node then
        JsonValueKind.Null
    else
        node.GetValueKind()

let private asObject (what: string) (node: JsonNode) : JsonObject =
    if kindOf node = JsonValueKind.Object then
        node.AsObject()
    else
        bad $"{what} must be an object"

let private known (what: string) (o: JsonObject) (keys: string list) =
    for p in o do
        if not (List.contains p.Key keys) then
            let list = String.concat ", " keys
            bad $"{what}: unknown property \"{p.Key}\" (known: {list})"

let private optString (what: string) (o: JsonObject) (key: string) : string option =
    match o.[key] with
    | null -> None
    | n when kindOf n = JsonValueKind.String -> Some(n.GetValue<string>())
    // Long DAX reads better as an array of lines.
    | n when
        kindOf n = JsonValueKind.Array
        && n.AsArray() |> Seq.forall (fun l -> kindOf l = JsonValueKind.String)
        ->
        Some(n.AsArray() |> Seq.map (fun l -> l.GetValue<string>()) |> String.concat "\n")
    | _ -> bad $"{what}: \"{key}\" must be a string (or an array of lines)"

let private reqString (what: string) (o: JsonObject) (key: string) : string =
    match optString what o key with
    | Some s when s.Trim() <> "" -> s
    | _ -> bad $"{what}: \"{key}\" is required"

let private optBool (what: string) (o: JsonObject) (key: string) (fallback: bool) : bool =
    match o.[key] with
    | null -> fallback
    | n when kindOf n = JsonValueKind.True || kindOf n = JsonValueKind.False -> n.GetValue<bool>()
    | _ -> bad $"{what}: \"{key}\" must be true or false"

let private items (what: string) (o: JsonObject) (key: string) : JsonNode list =
    match o.[key] with
    | null -> []
    | n when kindOf n = JsonValueKind.Array -> List.ofSeq (n.AsArray())
    | _ -> bad $"{what}: \"{key}\" must be an array"

/// The types a model column can have (everything a result can, but binary and variant).
let private columnType (what: string) (o: JsonObject) : QueryType =
    let text = reqString what o "type"

    match QueryType.tryParse text with
    | Some QueryType.Binary
    | Some QueryType.Variant
    | None -> bad $"{what}: type '{text}' is not one of string, int64, double, decimal, dateTime, boolean"
    | Some t -> t

let private columnDef (table: string) (node: JsonNode) : ColumnDef =
    let o = asObject $"a column of {table}" node
    let name = reqString $"a column of {table}" o "name"
    let what = $"{table}[{name}]"
    known what o [ "name"; "type"; "sortBy"; "key" ]

    {
        Name = name
        Type = columnType what o
        SortBy = optString what o "sortBy"
        Key = optBool what o "key" false
    }

let private calculatedColumnDef (table: string) (node: JsonNode) : CalculatedColumnDef =
    let o = asObject $"a calculated column of {table}" node
    let name = reqString $"a calculated column of {table}" o "name"
    let what = $"{table}[{name}]"
    known what o [ "name"; "type"; "expression"; "table" ]

    {
        Name = name
        Type = columnType what o
        Expression = reqString what o "expression"
    }

let private measureDef (table: string) (node: JsonNode) : MeasureDef =
    let o = asObject $"a measure of {table}" node
    let name = reqString $"a measure of {table}" o "name"
    let what = $"measure {table}[{name}]"
    known what o [ "name"; "expression"; "formatString"; "formatStringExpression"; "table" ]

    {
        Name = name
        Expression = reqString what o "expression"
        FormatString = optString what o "formatString"
        FormatStringExpression = optString what o "formatStringExpression"
    }

let private rowsOf (table: string) (columns: ColumnDef list) (o: JsonObject) : QueryValue array list =
    let types = columns |> List.map _.Type |> Array.ofList

    items table o "rows"
    |> List.mapi (fun r row ->
        if kindOf row <> JsonValueKind.Array then
            bad $"{table}: row {r} must be an array of values in column order"

        let cells = row.AsArray()

        if cells.Count <> types.Length then
            bad $"{table}: row {r} has {cells.Count} values for {types.Length} columns"

        Array.init types.Length (fun i ->
            match QueryValue.ofJson types.[i] cells.[i] with
            | Ok v when QueryValue.typeOf v |> Option.forall (fun t -> t = types.[i]) -> v
            | Ok _ -> bad $"{table}: row {r}, {columns.[i].Name}: the value is not of the column's type"
            | Error e -> bad $"{table}: row {r}, {columns.[i].Name}: {e}"))

let private tableDef (node: JsonNode) : TableDef =
    let o = asObject "a table" node
    let name = reqString "a table" o "name"
    known $"table {name}" o [ "name"; "columns"; "calculatedColumns"; "measures"; "rows" ]
    let columns = items name o "columns" |> List.map (columnDef name)

    {
        Name = name
        Columns = columns
        CalculatedColumns = items name o "calculatedColumns" |> List.map (calculatedColumnDef name)
        Measures = items name o "measures" |> List.map (measureDef name)
        Rows = rowsOf name columns o
    }

let private calculatedTableDef (node: JsonNode) : CalculatedTableDef =
    let o = asObject "a calculated table" node
    let name = reqString "a calculated table" o "name"
    known $"calculated table {name}" o [ "name"; "expression"; "columns"; "calculatedColumns"; "measures" ]

    {
        Name = name
        Expression = reqString $"calculated table {name}" o "expression"
        Columns = items name o "columns" |> List.map (columnDef name)
        CalculatedColumns = items name o "calculatedColumns" |> List.map (calculatedColumnDef name)
        Measures = items name o "measures" |> List.map (measureDef name)
    }

/// `Table[Column]` or `'Table name'[Column]`.
let private columnRef (what: string) (text: string) : string * string =
    let t = text.Trim()
    let bracket = t.IndexOf '['

    if bracket <= 0 || not (t.EndsWith ']') then
        bad $"{what}: '{text}' is not Table[Column]"

    let table = t.Substring(0, bracket).Trim()

    let table =
        if table.Length >= 2 && table.StartsWith '\'' && table.EndsWith '\'' then
            table.Substring(1, table.Length - 2).Replace("''", "'")
        else
            table

    table, t.Substring(bracket + 1, t.Length - bracket - 2).Replace("]]", "]")

let private relationshipDef (node: JsonNode) : RelationshipDef =
    let o = asObject "a relationship" node
    let from = reqString "a relationship" o "from"
    let towards = reqString "a relationship" o "to"
    let what = $"relationship {from} -> {towards}"
    known what o [ "from"; "to"; "cardinality"; "crossFilter"; "active"; "securityFilter" ]
    let fromTable, fromColumn = columnRef what from
    let toTable, toColumn = columnRef what towards

    let fromMany, toMany =
        match optString what o "cardinality" |> Option.defaultValue "many-to-one" with
        | "many-to-one" -> true, false
        | "one-to-one" -> false, false
        | "many-to-many" -> true, true
        | other ->
            bad
                $"{what}: cardinality '{other}' is not many-to-one, one-to-one or many-to-many (\"from\" is the many side)"

    let both =
        match optString what o "crossFilter" |> Option.defaultValue "single" with
        | "single" -> false
        | "both" -> true
        | other -> bad $"{what}: crossFilter '{other}' is not single or both"

    {
        FromTable = fromTable
        FromColumn = fromColumn
        ToTable = toTable
        ToColumn = toColumn
        FromMany = fromMany
        ToMany = toMany
        Both = both
        Active = optBool what o "active" true
        SecurityBoth =
            match optString what o "securityFilter" |> Option.defaultValue "single" with
            | "single" -> false
            | "both" -> true
            | other -> bad $"{what}: securityFilter '{other}' is not single or both"
    }

let private roleDef (node: JsonNode) : RoleDef =
    let o = asObject "a role" node
    let name = reqString "a role" o "name"
    let what = $"role {name}"
    known what o [ "name"; "permission"; "filters" ]

    let permission = optString what o "permission" |> Option.defaultValue "read"

    if not (List.contains permission [ "read"; "readRefresh"; "administrator"; "none"; "refresh" ]) then
        bad $"{what}: permission '{permission}' is not read, readRefresh, administrator, none or refresh"

    {
        Name = name
        Permission = permission
        Filters =
            match o.["filters"] with
            | null -> []
            | n when kindOf n = JsonValueKind.Object -> [
                for p in n.AsObject() ->
                    match p.Value with
                    | v when kindOf v = JsonValueKind.String -> p.Key, v.GetValue<string>()
                    | _ -> bad $"{what}: the filter on {p.Key} must be a DAX expression (a string)"
              ]
            | _ -> bad $"{what}: \"filters\" must be an object: table name to DAX expression"
    }

let private functionDef (node: JsonNode) : FunctionDef =
    let o = asObject "a function" node
    let name = reqString "a function" o "name"
    known $"function {name}" o [ "name"; "expression" ]

    {
        Name = name
        Expression = reqString $"function {name}" o "expression"
    }

let private calculationGroupDef (node: JsonNode) : CalculationGroupDef =
    let o = asObject "a calculation group" node
    let name = reqString "a calculation group" o "name"
    let what = $"calculation group {name}"
    known what o [ "name"; "column"; "precedence"; "items" ]

    {
        Name = name
        Column = reqString what o "column"
        Precedence =
            match o.["precedence"] with
            | null -> 0
            | n when kindOf n = JsonValueKind.Number -> n.GetValue<int>()
            | _ -> bad $"{what}: \"precedence\" must be a number"
        Items =
            items what o "items"
            |> List.map (fun item ->
                let i = asObject $"an item of {what}" item
                let itemName = reqString $"an item of {what}" i "name"
                known $"{what} item {itemName}" i [ "name"; "expression"; "formatStringExpression" ]

                {
                    Name = itemName
                    Expression = reqString $"{what} item {itemName}" i "expression"
                    FormatStringExpression = optString $"{what} item {itemName}" i "formatStringExpression"
                })
    }

let private calendarDef (node: JsonNode) : CalendarDef =
    let o = asObject "a calendar" node
    let name = reqString "a calendar" o "name"
    let what = $"calendar {name}"
    known what o [ "name"; "table"; "groups"; "timeRelated" ]

    let strings (owner: JsonObject) (key: string) =
        items what owner key
        |> List.map (fun n ->
            if kindOf n = JsonValueKind.String then
                n.GetValue<string>()
            else
                bad $"{what}: \"{key}\" lists column names")

    {
        Name = name
        Table = reqString what o "table"
        Groups =
            items what o "groups"
            |> List.map (fun g ->
                let g = asObject $"a group of {what}" g
                known $"a group of {what}" g [ "category"; "primary"; "associated" ]
                let category = reqString $"a group of {what}" g "category"

                let unit =
                    match Enum.TryParse<TimeUnit>(category, true) with
                    | true, u when u <> TimeUnit.Unknown -> u
                    | _ -> bad $"{what}: unknown category \"{category}\""

                unit, reqString $"a group of {what}" g "primary", strings g "associated")
        TimeRelated = strings o "timeRelated"
    }

let private modelKeys = [
    "description"
    "culture"
    "collation"
    "tables"
    "calculatedTables"
    "relationships"
    "functions"
    "calculationGroups"
    "calendars"
    "roles"
]

let private modelDef (node: JsonNode) : ModelDef =
    let o = asObject "a model" node
    known "the model" o modelKeys

    {
        Culture = optString "the model" o "culture" |> Option.defaultValue "en-US"
        Collation = optString "the model" o "collation"
        Tables = items "the model" o "tables" |> List.map tableDef
        CalculatedTables = items "the model" o "calculatedTables" |> List.map calculatedTableDef
        Relationships = items "the model" o "relationships" |> List.map relationshipDef
        Functions = items "the model" o "functions" |> List.map functionDef
        CalculationGroups = items "the model" o "calculationGroups" |> List.map calculationGroupDef
        Calendars = items "the model" o "calendars" |> List.map calendarDef
        Roles = items "the model" o "roles" |> List.map roleDef
    }

let private tomType =
    function
    | QueryType.String -> DataType.String
    | QueryType.Int64 -> DataType.Int64
    | QueryType.Double -> DataType.Double
    | QueryType.Decimal -> DataType.Decimal
    | QueryType.DateTime -> DataType.DateTime
    | QueryType.Boolean -> DataType.Boolean
    | QueryType.Binary -> DataType.Binary
    | QueryType.Variant -> DataType.Variant

/// The model as a TOM database. Loaded tables get a placeholder M partition: their rows come from
/// the store (or, on a server, from `tess data embed`).
let buildDatabase (name: string) (def: ModelDef) : Database =
    let db = new Database(Name = name, ID = name, CompatibilityLevel = 1702)

    let model =
        new Model(Name = "Model", Culture = def.Culture, DiscourageImplicitMeasures = not def.CalculationGroups.IsEmpty)

    def.Collation |> Option.iter (fun c -> model.Collation <- c)
    db.Model <- model

    let addMeasures (table: Table) (measures: MeasureDef list) =
        for m in measures do
            let measure = new Measure(Name = m.Name, Expression = m.Expression)
            m.FormatString |> Option.iter (fun f -> measure.FormatString <- f)

            m.FormatStringExpression
            |> Option.iter (fun f -> measure.FormatStringDefinition <- new FormatStringDefinition(Expression = f))

            table.Measures.Add measure

    let addCalculated (table: Table) (columns: CalculatedColumnDef list) =
        for c in columns do
            table.Columns.Add(new CalculatedColumn(Name = c.Name, DataType = tomType c.Type, Expression = c.Expression))

    let finish (table: Table) (columns: ColumnDef list) =
        for c in columns do
            let column = table.Columns.[c.Name]
            column.IsKey <- c.Key

            match c.SortBy with
            | Some other when table.Columns.ContainsName other -> column.SortByColumn <- table.Columns.[other]
            | Some other -> bad $"{table.Name}[{c.Name}]: sortBy names a column the table doesn't have: {other}"
            | None -> ()

    for t in def.Tables do
        let table = new Table(Name = t.Name)

        for c in t.Columns do
            table.Columns.Add(new DataColumn(Name = c.Name, DataType = tomType c.Type, SourceColumn = c.Name))

        addCalculated table t.CalculatedColumns
        addMeasures table t.Measures
        finish table t.Columns

        table.Partitions.Add(
            new Partition(
                Name = t.Name,
                Source = new MPartitionSource(Expression = "let Source = #table({}, {}) in Source")
            )
        )

        model.Tables.Add table

    for t in def.CalculatedTables do
        let table = new Table(Name = t.Name)

        for c in t.Columns do
            table.Columns.Add(
                new CalculatedTableColumn(
                    Name = c.Name,
                    DataType = tomType c.Type,
                    SourceColumn = "[" + c.Name.Replace("]", "]]") + "]"
                )
            )

        addCalculated table t.CalculatedColumns
        addMeasures table t.Measures
        finish table t.Columns

        table.Partitions.Add(
            new Partition(Name = t.Name, Source = new CalculatedPartitionSource(Expression = t.Expression))
        )

        model.Tables.Add table

    for g in def.CalculationGroups do
        let table =
            new Table(Name = g.Name, CalculationGroup = new CalculationGroup(Precedence = g.Precedence))

        table.Columns.Add(new DataColumn(Name = g.Column, DataType = DataType.String, SourceColumn = "Name"))

        g.Items
        |> List.iteri (fun ordinal item ->
            let calculationItem =
                new CalculationItem(Name = item.Name, Expression = item.Expression, Ordinal = ordinal)

            item.FormatStringExpression
            |> Option.iter (fun f ->
                calculationItem.FormatStringDefinition <- new FormatStringDefinition(Expression = f))

            table.CalculationGroup.CalculationItems.Add calculationItem)

        table.Partitions.Add(new Partition(Name = g.Name, Source = new CalculationGroupSource()))
        model.Tables.Add table

    let column (table: string) (name: string) : Column =
        if not (model.Tables.ContainsName table) then
            bad $"a relationship names a table the model doesn't have: {table}"

        let t = model.Tables.[table]

        if not (t.Columns.ContainsName name) then
            bad $"a relationship names a column the model doesn't have: {table}[{name}]"

        t.Columns.[name]

    for r in def.Relationships do
        let cardinality many =
            if many then
                RelationshipEndCardinality.Many
            else
                RelationshipEndCardinality.One

        model.Relationships.Add(
            new SingleColumnRelationship(
                Name = $"{r.FromTable}[{r.FromColumn}] -> {r.ToTable}[{r.ToColumn}]",
                FromColumn = column r.FromTable r.FromColumn,
                ToColumn = column r.ToTable r.ToColumn,
                FromCardinality = cardinality r.FromMany,
                ToCardinality = cardinality r.ToMany,
                CrossFilteringBehavior =
                    (if r.Both then
                         CrossFilteringBehavior.BothDirections
                     else
                         CrossFilteringBehavior.OneDirection),
                SecurityFilteringBehavior =
                    (if r.SecurityBoth then
                         SecurityFilteringBehavior.BothDirections
                     else
                         SecurityFilteringBehavior.OneDirection),
                IsActive = r.Active
            )
        )

    for r in def.Roles do
        let permission =
            match r.Permission with
            | "readRefresh" -> ModelPermission.ReadRefresh
            | "administrator" -> ModelPermission.Administrator
            | "none" -> ModelPermission.None
            | "refresh" -> ModelPermission.Refresh
            | _ -> ModelPermission.Read

        let role = new ModelRole(Name = r.Name, ModelPermission = permission)

        for (table, dax) in r.Filters do
            if not (model.Tables.ContainsName table) then
                bad $"role {r.Name}: a filter on a table the model doesn't have: {table}"

            role.TablePermissions.Add(new TablePermission(Table = model.Tables.[table], FilterExpression = dax))

        model.Roles.Add role

    for f in def.Functions do
        model.Functions.Add(new Function(Name = f.Name, Expression = f.Expression))

    for c in def.Calendars do
        if not (model.Tables.ContainsName c.Table) then
            bad $"calendar {c.Name}: the model has no table '{c.Table}'"

        let table = model.Tables.[c.Table]

        let columnOf (name: string) =
            if not (table.Columns.ContainsName name) then
                bad $"calendar {c.Name}: '{c.Table}' has no column '{name}'"

            table.Columns.[name]

        let calendar = new Calendar(Name = c.Name)

        for (unit, primary, associated) in c.Groups do
            let group = new TimeUnitColumnAssociation(unit, PrimaryColumn = columnOf primary)

            for a in associated do
                group.AssociatedColumns.Add(columnOf a)

            calendar.CalendarColumnGroups.Add group

        if not c.TimeRelated.IsEmpty then
            let group = new TimeRelatedColumnGroup()

            for r in c.TimeRelated do
                group.Columns.Add(columnOf r)

            calendar.CalendarColumnGroups.Add group

        table.Calendars.Add calendar

    db

/// A row value as a fixture JSON file holds it. Numbers go as text, which the fixture reader takes
/// digit for digit.
let private fixtureValue (v: QueryValue) : JsonNode =
    match v with
    | QueryValue.Blank -> null
    | QueryValue.String s -> JsonValue.Create s
    | QueryValue.Boolean b -> JsonValue.Create b
    | other -> JsonValue.Create(QueryValue.text other)

/// Write a model's rows as a fixture directory: `<Table>.json` per loaded table.
let writeFixture (def: ModelDef) (directory: string) : unit =
    Directory.CreateDirectory directory |> ignore

    for t in def.Tables do
        let rows = JsonArray()

        for row in t.Rows do
            let o = JsonObject()
            t.Columns |> List.iteri (fun i c -> o.[c.Name] <- fixtureValue row.[i])
            rows.Add o

        File.WriteAllText(
            Path.Combine(directory, FixtureName.encode t.Name + ".json"),
            rows.ToJsonString(),
            UTF8Encoding false
        )

let private rawValue (v: QueryValue) : DaxValue =
    match v with
    | QueryValue.Blank -> DaxValue.Blank
    | QueryValue.String text -> DaxValue.String text
    | QueryValue.Int64 i -> DaxValue.Integer i
    | QueryValue.Double d -> DaxValue.Decimal d
    | QueryValue.Decimal d ->
        match DaxValue.tryCurrency d with
        | Some c -> c
        | None -> bad $"{d} is out of range for a fixed decimal"
    | QueryValue.DateTime d -> DaxValue.ofDateTime d
    | QueryValue.Boolean b -> DaxValue.Boolean b
    | QueryValue.Binary _ -> bad "a model column can't be binary"

let private scratch () =
    Path.Combine(Path.GetTempPath(), "tessera-tests", "conformance-" + Guid.NewGuid().ToString "N")

/// Build the database, write the rows as a fixture and load them back through `Fixture`.
let prepare (name: string) (def: ModelDef) : Result<Prepared, string> =
    let directory = scratch ()

    try
        try
            let db = buildDatabase name def
            writeFixture def directory

            match Fixture.loadAndCheck db directory with
            | Error e -> Error(TessError.message e)
            | Ok(loaded, issues) ->
                Ok {
                    Database = db
                    Store = loaded.Store
                    Issues = issues
                    Rows =
                        def.Tables
                        |> List.map (fun t -> t.Name, t.Rows |> List.map (Array.map rawValue))
                        |> Map.ofList
                }
        with
        | Bad message -> Error message
        | e -> Error $"the model doesn't build: {e.Message}"
    finally
        try
            Directory.Delete(directory, true)
        with _ ->
            ()

/// A model's rows by table name (names are case-insensitive); none for a table it doesn't load.
let rawRows (p: Prepared) (table: string) : DaxValue array list =
    p.Rows
    |> Map.tryPick (fun name rows ->
        if String.Equals(name, table, StringComparison.OrdinalIgnoreCase) then
            Some rows
        else
            None)
    |> Option.defaultValue []

/// The model with its rows inside it, for a server: from the rows as written, not from the store
/// (see `Prepared.Rows`).
let embedForServer (p: Prepared) : Result<FixtureEmbed.Embedded, TessError> =
    FixtureEmbed.embedRows p.Database (rawRows p)

// ---- Cases ----------------------------------------------------------------------------------------

type Source = {
    Url: string
    /// A line or two of the source, word for word (optional).
    Quote: string option
    /// What in the source justifies the expectation.
    Supports: string
}

[<RequireQualifiedAccess>]
type Confidence =
    /// The source shows this query's or formula's output, or states the result outright.
    | Published
    /// Worked out by hand from documented rules; `Derivation` shows the working.
    | Derived

[<RequireQualifiedAccess>]
type Expected =
    /// One result per EVALUATE.
    | Results of QueryResult list
    /// The query must fail; the text says why (it is not matched against the engine's message).
    | Error of reason: string

/// Who a case's query runs as (`"as"`): members of these roles, with this CUSTOMDATA. On a
/// server they are the connection's Roles= and CustomData=.
type RunAs = {
    Roles: string list
    CustomData: string option
}

type Case = {
    /// The file's path under the corpus, without `.json`, with `/`: `blank/add-number`.
    Id: string
    /// The first folder of the id.
    Area: string
    File: string
    Title: string
    /// The shared model's name, and for a case that changes it, a suffix that tells the variants apart.
    ModelName: string
    /// Cases with the same key share one prepared model (and one deployed database).
    ModelKey: string
    Model: Lazy<Result<ModelDef, string>>
    Query: string
    Expected: Expected
    /// Row order is part of the expectation (the query has an ORDER BY that decides it).
    Ordered: bool
    /// Result columns (by name) whose type the case doesn't assert.
    UntypedColumns: Set<string>
    /// A relative tolerance for doubles, when the default isn't right for the case.
    Tolerance: float option
    Sources: Source list
    Confidence: Confidence
    Derivation: string option
    /// The sources disagree or are silent: the expectation is the likelier reading.
    Uncertain: bool
    /// The other reading, and what it would give.
    Alternative: string option
    Tags: string list
    Notes: string option
    /// The query runs as members of roles of the model (`"as"`); None: as an administrator.
    As: RunAs option
}

type Corpus = {
    Directory: string
    Models: Map<string, JsonNode>
    Cases: Case list
    /// Files that couldn't be read as a model or a case: (path under the corpus, why).
    Problems: (string * string) list
}

let private caseKeys = [
    "title"
    "model"
    "query"
    "expected"
    "ordered"
    "tolerance"
    "source"
    "confidence"
    "derivation"
    "uncertain"
    "alternative"
    "tags"
    "notes"
    "as"
]

let private overlayKeys = [
    "base"
    "tables"
    "rows"
    "measures"
    "calculatedColumns"
    "calculatedTables"
    "relationships"
    "addRelationships"
    "functions"
    "calculationGroups"
    "calendars"
    "culture"
    "collation"
    "roles"
]

let private appendTo (target: JsonObject) (key: string) (nodes: JsonNode list) =
    if not nodes.IsEmpty then
        let array =
            match target.[key] with
            | null ->
                let a = JsonArray()
                target.[key] <- a
                a
            | a -> a.AsArray()

        for n in nodes do
            array.Add(n.DeepClone())

/// A shared model with a case's changes applied: tables, measures and calculated columns added,
/// rows or relationships replaced.
let private applyOverlay (baseModel: JsonNode) (overlay: JsonObject) : JsonNode =
    known "the case's model" overlay overlayKeys
    let model = baseModel.DeepClone().AsObject()

    let table (what: string) (name: string) : JsonObject =
        let all =
            items "the model" model "tables" @ items "the model" model "calculatedTables"

        match
            all
            |> List.tryFind (fun t ->
                kindOf t = JsonValueKind.Object
                && kindOf t.["name"] = JsonValueKind.String
                && String.Equals(t.["name"].GetValue<string>(), name, StringComparison.OrdinalIgnoreCase))
        with
        | Some t -> t.AsObject()
        | None -> bad $"{what}: the model has no table '{name}'"

    appendTo model "tables" (items "the case's model" overlay "tables")
    appendTo model "calculatedTables" (items "the case's model" overlay "calculatedTables")
    appendTo model "functions" (items "the case's model" overlay "functions")
    appendTo model "calculationGroups" (items "the case's model" overlay "calculationGroups")
    appendTo model "calendars" (items "the case's model" overlay "calendars")
    appendTo model "relationships" (items "the case's model" overlay "addRelationships")

    for key in [ "relationships"; "culture"; "collation"; "roles" ] do
        match overlay.[key] with
        | null -> ()
        | n -> model.[key] <- n.DeepClone()

    for key in [ "measures"; "calculatedColumns" ] do
        for node in items "the case's model" overlay key do
            let o = asObject $"an entry of \"{key}\"" node
            let target = table $"\"{key}\"" (reqString $"an entry of \"{key}\"" o "table")
            let copy = o.DeepClone().AsObject()
            copy.Remove "table" |> ignore
            appendTo target key [ copy ]

    match overlay.["rows"] with
    | null -> ()
    | rows ->
        for p in asObject "\"rows\"" rows do
            (table "\"rows\"" p.Key).["rows"] <- p.Value.DeepClone()

    model

let private hash8 (text: string) =
    Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes text)).Substring(0, 8).ToLowerInvariant()

let private parseCase (models: Map<string, JsonNode>) (id: string) (file: string) (text: string) : Case =
    let o = asObject "a case" (parseJson text)
    known "the case" o caseKeys

    let modelName, modelKey, modelNode =
        let shared (name: string) =
            match Map.tryFind name models with
            | Some m -> m
            | None ->
                let list = models |> Map.keys |> String.concat ", "
                bad $"\"model\": there is no models/{name}.json (there are: {list})"

        match o.["model"] with
        | n when kindOf n = JsonValueKind.String ->
            let name = n.GetValue<string>()
            let node = shared name
            // The key has the content too: two corpora may each have a model of this name.
            name, name + ":" + hash8 (node.ToJsonString()), lazy node
        | n when kindOf n = JsonValueKind.Object ->
            let overlay = n.AsObject()
            let name = reqString "\"model\"" overlay "base"
            let baseModel = shared name
            let key = hash8 (name + "\n" + overlay.ToJsonString())
            $"{name}-{key}", $"{name}-{key}:" + hash8 (baseModel.ToJsonString()), lazy (applyOverlay baseModel overlay)
        | _ -> bad "\"model\" must be the name of a shared model, or { \"base\": name, ...changes }"

    let untyped = Collections.Generic.HashSet<string>()

    let expected =
        match o.["expected"] with
        | n when kindOf n = JsonValueKind.Object ->
            let e = n.AsObject()
            known "\"expected\"" e [ "error" ]
            Expected.Error(reqString "\"expected\"" e "error")
        | n when kindOf n = JsonValueKind.Array ->
            // A column with no "type" doesn't assert one: read its values as a variant's.
            let results = n.DeepClone()

            for result in results.AsArray() do
                if
                    kindOf result = JsonValueKind.Object
                    && kindOf result.["columns"] = JsonValueKind.Array
                then
                    for column in result.["columns"].AsArray() do
                        if
                            kindOf column = JsonValueKind.Object
                            && isNull column.["type"]
                            && kindOf column.["name"] = JsonValueKind.String
                        then
                            column.["type"] <- JsonValue.Create "variant"
                            untyped.Add(column.["name"].GetValue<string>()) |> ignore

            match QueryResult.listOfJson results with
            | Ok r -> Expected.Results r
            | Error e -> bad $"\"expected\": {e}"
        | _ -> bad "\"expected\" must be an array of results (one per EVALUATE), or { \"error\": why }"

    let sources =
        let one (node: JsonNode) =
            let s = asObject "a source" node
            known "a source" s [ "url"; "quote"; "supports" ]

            {
                Url = reqString "a source" s "url"
                Quote = optString "a source" s "quote"
                Supports = reqString "a source" s "supports"
            }

        match o.["source"] with
        | n when kindOf n = JsonValueKind.Object -> [ one n ]
        | n when kindOf n = JsonValueKind.Array -> n.AsArray() |> Seq.map one |> List.ofSeq
        | _ -> bad "\"source\" is required: { \"url\", \"supports\" } or an array of them"

    {
        Id = id
        Area = id.Split('/').[0]
        File = file
        Title = reqString "the case" o "title"
        ModelName = modelName
        ModelKey = modelKey
        Model =
            lazy
                (try
                    Ok(modelDef modelNode.Value)
                 with Bad message ->
                     Error message)
        Query = reqString "the case" o "query"
        Expected = expected
        Ordered = optBool "the case" o "ordered" false
        UntypedColumns = Set.ofSeq untyped
        Tolerance =
            match o.["tolerance"] with
            | null -> None
            | n when kindOf n = JsonValueKind.Number -> Some(n.GetValue<float>())
            | _ -> bad "\"tolerance\" must be a number"
        Sources = sources
        Confidence =
            match reqString "the case" o "confidence" with
            | "published" -> Confidence.Published
            | "derived" -> Confidence.Derived
            | other -> bad $"\"confidence\" is '{other}', not published or derived"
        Derivation = optString "the case" o "derivation"
        Uncertain = optBool "the case" o "uncertain" false
        Alternative = optString "the case" o "alternative"
        Tags =
            items "the case" o "tags"
            |> List.map (fun t ->
                if kindOf t = JsonValueKind.String then
                    t.GetValue<string>()
                else
                    bad "\"tags\" must be strings")
        Notes = optString "the case" o "notes"
        As =
            match o.["as"] with
            | null -> None
            | n ->
                let a = asObject "\"as\"" n
                known "\"as\"" a [ "roles"; "customData" ]

                Some {
                    Roles =
                        items "\"as\"" a "roles"
                        |> List.map (fun r ->
                            if kindOf r = JsonValueKind.String then
                                r.GetValue<string>()
                            else
                                bad "\"as\": \"roles\" must be role names")
                    CustomData = optString "\"as\"" a "customData"
                }
    }

/// Read every model (models/*.json) and case (<area>/**/*.json) under a corpus directory. A file
/// that can't be read is a problem, not an exception.
let load (directory: string) : Corpus =
    let directory = Path.GetFullPath directory
    let problems = ResizeArray<string * string>()

    let relative (path: string) =
        Path.GetRelativePath(directory, path).Replace('\\', '/')

    let read (path: string) (f: string -> 'T) : 'T option =
        try
            Some(f (File.ReadAllText path))
        with
        | Bad message ->
            problems.Add(relative path, message)
            None
        | :? IOException as e ->
            problems.Add(relative path, e.Message)
            None

    let sorted (paths: string seq) =
        paths |> Seq.sortWith (fun a b -> String.CompareOrdinal(relative a, relative b))

    let modelsDirectory = Path.Combine(directory, "models")

    let models =
        if Directory.Exists modelsDirectory then
            Directory.EnumerateFiles(modelsDirectory, "*.json")
            |> sorted
            |> Seq.choose (fun path ->
                read path (fun text ->
                    let node = parseJson text
                    // Read it once now, so a broken shared model is one problem, not one per case.
                    modelDef node |> ignore
                    Path.GetFileNameWithoutExtension path, node))
            |> Map.ofSeq
        else
            Map.empty

    let cases =
        if Directory.Exists directory then
            Directory.EnumerateDirectories directory
            |> Seq.filter (fun d ->
                not (String.Equals(Path.GetFileName d, "models", StringComparison.OrdinalIgnoreCase)))
            |> Seq.collect (fun d -> Directory.EnumerateFiles(d, "*.json", SearchOption.AllDirectories))
            |> sorted
            |> Seq.choose (fun path ->
                let rel = relative path
                read path (parseCase models (rel.Substring(0, rel.Length - ".json".Length)) path))
            |> List.ofSeq
        else
            []

    {
        Directory = directory
        Models = models
        Cases = cases
        Problems = List.ofSeq problems
    }

// ---- Prepared models, shared between cases --------------------------------------------------------

let private preparedModels =
    Collections.Concurrent.ConcurrentDictionary<string, Lazy<Result<Prepared, string>>>()

/// A database name for a model: `tessera-conformance-<model>`, the variants of a shared model with
/// the suffix of their key.
let databaseName (c: Case) = "tessera-conformance-" + c.ModelName

/// The case's model with its rows loaded. Built once per model, however many cases use it: treat
/// the database as read-only.
let prepared (c: Case) : Result<Prepared, string> =
    preparedModels.GetOrAdd(c.ModelKey, fun _ -> lazy (c.Model.Value |> Result.bind (prepare (databaseName c)))).Value

// ---- Checking a case ------------------------------------------------------------------------------

/// The tags a case must carry one of, when it is about an area an engine may leave for later.
let laterAreas = [
    "time-intelligence"
    "allselected"
    "format"
    "text-functions"
    "date-functions"
    "math-functions"
    "calculation-groups"
    "window-functions"
    "security"
    "format-strings"
]

/// Everything wrong with a case as a case (not with any engine): empty when it is sound.
let validate (c: Case) : string list = [
    match prepared c with
    | Error e -> $"model: {e}"
    | Ok p ->
        for i in p.Issues do
            if i.Severity = Validate.Severity.Error then
                $"fixture: {ObjectRef.format i.Ref}: {i.Message} [{i.Rule}]"

        for i in Validate.validate p.Database do
            if i.Severity = Validate.Severity.Error then
                $"model: {ObjectRef.format i.Ref}: {i.Message} [{i.Rule}]"

    // A server refuses to process a column sorted by another unless each of its values has one
    // sort value. Nothing local checks that.
    match c.Model.Value with
    | Error _ -> ()
    | Ok def ->
        for t in def.Tables do
            for i, column in List.indexed t.Columns do
                match column.SortBy with
                | None -> ()
                | Some other ->
                    match
                        t.Columns
                        |> List.tryFindIndex (fun x -> String.Equals(x.Name, other, StringComparison.OrdinalIgnoreCase))
                    with
                    | None -> ()
                    | Some j ->
                        let key (v: QueryValue) =
                            (QueryValue.text v).TrimEnd(' ').ToUpperInvariant()

                        let clashes =
                            t.Rows
                            |> List.groupBy (fun row -> key row.[i])
                            |> List.filter (fun (_, rows) ->
                                rows |> List.map (fun row -> key row.[j]) |> List.distinct |> List.length > 1)
                            |> List.map fst

                        if not clashes.IsEmpty then
                            let list = String.concat ", " clashes
                            $"model: {t.Name}[{column.Name}] is sorted by {other}, but these values have more than one {other}: {list}"

    match DaxParser.parseQuery c.Query with
    | Error errors ->
        for e in errors do
            $"query: {e.Line}:{e.Column}: {e.Message}"
    | Ok query ->
        let hasOrderBy = query.Evaluates |> List.exists (fun e -> not e.OrderBy.IsEmpty)

        // An ordered expectation needs an ORDER BY that tells every row apart: rows that tie on its
        // keys come back in any order. Checked where the keys are columns of the result.
        match c.Expected with
        | Expected.Results results when c.Ordered && results.Length = query.Evaluates.Length ->
            for n, (evaluate, result) in List.indexed (List.zip query.Evaluates results) do
                let column (key: DaxAst.OrderKey) =
                    let rec name (e: DaxAst.Expr) =
                        match e.Kind with
                        | DaxAst.Paren inner -> name inner
                        | DaxAst.Qualified(table, column) -> Some(Some table.Name, column)
                        | DaxAst.Bracketed column -> Some(None, column)
                        | _ -> None

                    name key.Expr
                    |> Option.bind (fun (table, column) ->
                        result.Columns
                        |> List.tryFindIndex (fun x ->
                            let matches (candidate: string) =
                                String.Equals(x.Name, candidate, StringComparison.OrdinalIgnoreCase)

                            match table with
                            | Some t -> matches $"{t}[{column}]"
                            | None ->
                                matches $"[{column}]"
                                || x.Name.EndsWith($"[{column}]", StringComparison.OrdinalIgnoreCase)))

                let keys = evaluate.OrderBy |> List.map column

                if not keys.IsEmpty && keys |> List.forall Option.isSome then
                    let indexes = keys |> List.choose id

                    let keyOf (row: QueryValue array) =
                        indexes
                        |> List.map (fun i -> QueryValue.text row.[i] + "|" + string (QueryValue.typeOf row.[i]))

                    let whole (row: QueryValue array) =
                        row
                        |> Array.map (fun v -> QueryValue.text v + "|" + string (QueryValue.typeOf v))
                        |> List.ofArray

                    let tied =
                        result.Rows
                        |> List.groupBy keyOf
                        |> List.filter (fun (_, rows) -> rows |> List.map whole |> List.distinct |> List.length > 1)

                    if not tied.IsEmpty then
                        $"result {n}: \"ordered\" is true, but {tied.Length} group(s) of rows tie on the ORDER BY keys, so their order isn't defined"
        | _ -> ()

        if c.Ordered && not hasOrderBy then
            "\"ordered\" is true but the query has no ORDER BY: its row order isn't defined"

        match c.Expected with
        | Expected.Results results when results.Length <> query.Evaluates.Length ->
            $"\"expected\" has {results.Length} results for {query.Evaluates.Length} EVALUATE statements"
        | _ -> ()

    match c.Expected with
    | Expected.Error _ -> ()
    | Expected.Results results ->
        for n, r in List.indexed results do
            match
                r.Columns
                |> List.countBy (fun col -> col.Name.ToUpperInvariant())
                |> List.tryFind (fun (_, count) -> count > 1)
            with
            | Some(name, _) -> $"result {n}: column {name} appears twice"
            | None -> ()

        if results |> List.exists _.Truncated then
            "\"expected\" can't be a truncated result"

    if c.Sources.IsEmpty then
        "\"source\" is empty"

    for s in c.Sources do
        if not (s.Url.StartsWith("https://", StringComparison.Ordinal)) then
            $"source url is not https: {s.Url}"

        match s.Quote with
        | Some q when q.Length > 400 -> "a quote is longer than 400 characters: quote a line or two"
        | _ -> ()

    match c.Confidence, c.Derivation with
    | Confidence.Derived, None -> "a derived case needs a \"derivation\": the rule applied and the arithmetic"
    | _ -> ()

    match c.Uncertain, c.Alternative, c.Confidence with
    | true, None, _ -> "an uncertain case needs an \"alternative\": the other reading and what it would give"
    | true, _, Confidence.Published -> "a case can't be both published and uncertain"
    | false, Some _, _ -> "\"alternative\" is only for uncertain cases"
    | _ -> ()

    if c.Tags.IsEmpty then
        "\"tags\" is empty"

    for t in c.Tags do
        if t <> t.Trim() || t = "" || t <> t.ToLowerInvariant() then
            $"tag '{t}' must be lower case with no outer spaces"

    if c.Tags |> List.distinct |> List.length <> c.Tags.Length then
        "a tag is repeated"

    if List.contains c.Area laterAreas && not (List.contains c.Area c.Tags) then
        $"a case under {c.Area}/ must carry the tag '{c.Area}', so it can be skipped"

    match c.Tolerance with
    | Some t when t < 0.0 || t > 0.01 -> "\"tolerance\" must be between 0 and 0.01"
    | _ -> ()
]

// ---- Running an engine ----------------------------------------------------------------------------

/// An engine: a model, its data, a DAX query; one result per EVALUATE.
type Evaluate = Database -> DataStore -> string -> Result<QueryResult list, TessError>

/// An engine that can run a query as members of roles (a case's `"as"`).
type EvaluateAs = RunAs option -> Evaluate

[<RequireQualifiedAccess>]
type Mismatch =
    /// The results differ: (index of the result, how).
    | Differences of (int * Comparison) list
    | ResultCount of expected: int * actual: int
    /// The query had to fail, and returned results.
    | ExpectedError of reason: string * actual: QueryResult list

[<RequireQualifiedAccess>]
type Outcome =
    | Pass
    | Fail of Mismatch
    /// The engine said it doesn't implement something the query needs.
    | NotSupported of reason: string
    /// The engine failed where results were expected (or threw).
    | Error of message: string

module Mismatch =
    /// The values all agree: only declared types, or the types of numbers, differ.
    let isTypeOnly =
        function
        | Mismatch.Differences ds -> ds |> List.forall (fun (_, c) -> c.ValuesMatch)
        | _ -> false

    let describe (failure: Mismatch) : string list =
        match failure with
        | Mismatch.ResultCount(expected, actual) -> [ $"{actual} result sets, expected {expected}" ]
        | Mismatch.ExpectedError(reason, actual) ->
            let rows = actual |> List.sumBy (fun r -> r.Rows.Length)

            [
                $"expected an error ({reason}), but the query returned {actual.Length} result(s), {rows} row(s)"
            ]
        | Mismatch.Differences ds -> [
            for n, comparison in ds do
                for d in comparison.Differences do
                    if ds.Length > 1 then
                        $"result {n}: {ResultDifference.describe d}"
                    else
                        ResultDifference.describe d
          ]

type RunConfig = {
    /// Which of an engine's errors mean "not implemented" rather than "this query is wrong".
    IsNotSupported: TessError -> bool
    /// The comparison's defaults; a case's `ordered`, `tolerance` and untyped columns go on top.
    Compare: CompareOptions
}

module RunConfig =
    /// Not supported: an error whose stable code says so (`unsupported-...`, `...-not-supported`).
    let defaults = {
        IsNotSupported =
            fun e ->
                let code = TessError.code e
                code.Contains "unsupported" || code.Contains "not-supported"
        Compare = CompareOptions.defaults
    }

let compareOptions (config: RunConfig) (c: Case) : CompareOptions = {
    config.Compare with
        Ordered = c.Ordered
        RelativeTolerance = c.Tolerance |> Option.defaultValue config.Compare.RelativeTolerance
        IgnoreTypesOf = c.UntypedColumns
}

/// Judge what an engine answered for a case.
let judge (config: RunConfig) (c: Case) (answer: Result<QueryResult list, TessError>) : Outcome =
    match answer, c.Expected with
    | Error e, _ when config.IsNotSupported e -> Outcome.NotSupported(TessError.message e)
    | Error _, Expected.Error _ -> Outcome.Pass
    | Error e, Expected.Results _ -> Outcome.Error(TessError.message e)
    | Ok actual, Expected.Error reason -> Outcome.Fail(Mismatch.ExpectedError(reason, actual))
    | Ok actual, Expected.Results expected ->
        match QueryResult.compareAll (compareOptions config c) expected actual with
        | Error _ -> Outcome.Fail(Mismatch.ResultCount(expected.Length, actual.Length))
        | Ok comparisons ->
            match comparisons |> List.indexed |> List.filter (fun (_, x) -> not x.IsMatch) with
            | [] -> Outcome.Pass
            | different -> Outcome.Fail(Mismatch.Differences different)

/// Run an engine that takes roles on a case.
let runAsWith (config: RunConfig) (evaluate: EvaluateAs) (c: Case) : Outcome =
    match prepared c with
    | Error e -> Outcome.Error $"the case's model can't be prepared: {e}"
    | Ok p ->
        let answer =
            try
                Ok(evaluate c.As p.Database p.Store c.Query)
            with e ->
                Error $"the engine threw {e.GetType().Name}: {e.Message}"

        match answer with
        | Error thrown -> Outcome.Error thrown
        | Ok answer -> judge config c answer

/// Run an engine on a case. A case that runs as members of roles needs `runAsWith`.
let runWith (config: RunConfig) (evaluate: Evaluate) (c: Case) : Outcome =
    match c.As with
    | Some _ ->
        Outcome.Error "the case runs as members of roles (\"as\"): run it with an engine that takes them (runAsWith)"
    | None -> runAsWith config (fun _ -> evaluate) c

/// Run an engine on a case, with the default configuration.
let run (evaluate: Evaluate) (c: Case) : Outcome = runWith RunConfig.defaults evaluate c
