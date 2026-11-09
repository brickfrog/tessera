namespace Tessera

open System
open System.Collections.Concurrent
open System.Collections.Generic

// The in-memory data a local DAX query runs against: tables of dictionary-encoded columns, the way
// VertiPaq stores them. A column is an array of value ids (one int per row) and the array of its
// distinct values; everything an evaluator filters or groups by can work on the ids.
//
// Everything here is immutable once built. The arrays a column hands out are its own: read them,
// never write to them.

/// One column of a table in a `DataStore`.
[<Sealed>]
type StoreColumn
    internal (name: string, dataType: DaxType, collation: Collation, ids: int array, values: DaxValue array) =
    let blankId = Array.IndexOf(values, DaxValue.Blank)

    let lookup =
        lazy
            (let d =
                Dictionary<DaxValue, int>(values.Length, DaxValue.comparer collation :> IEqualityComparer<DaxValue>)

             for i in 0 .. values.Length - 1 do
                 d.TryAdd(values.[i], i) |> ignore

             d)

    /// Rows grouped by value id: `starts.[id] .. starts.[id + 1] - 1` index into `rows`.
    let index =
        lazy
            (let starts = Array.zeroCreate<int>(values.Length + 1)

             for id in ids do
                 starts.[id + 1] <- starts.[id + 1] + 1

             for i in 1 .. values.Length do
                 starts.[i] <- starts.[i] + starts.[i - 1]

             let next = Array.sub starts 0 values.Length
             let rows = Array.zeroCreate<int> ids.Length

             for row in 0 .. ids.Length - 1 do
                 let id = ids.[row]
                 rows.[next.[id]] <- row
                 next.[id] <- next.[id] + 1

             starts, rows)

    member _.Name = name
    member _.DataType = dataType
    member _.Collation = collation
    member _.RowCount = ids.Length

    /// The value id of each row: an index into `Values`. Do not modify.
    member _.ValueIds = ids

    /// The distinct values, in the order they first appear; BLANK is one of them if any row is
    /// BLANK. Text that differs only in case is one value, in the casing seen first. Do not modify.
    member _.Values = values

    member _.DistinctCount = values.Length

    /// The value of a row.
    member _.Item
        with get (row: int) = values.[ids.[row]]

    /// The id of BLANK, or -1 if no row is BLANK.
    member _.BlankId = blankId

    /// Whether every row has a different value (a BLANK counts as a value).
    member _.IsUnique = values.Length = ids.Length

    /// The id of a value, or -1 if no row has it. Matches as grouping does (`DaxValue.groupEquals`):
    /// text by the collation, numbers by type and value, BLANK only BLANK.
    member _.TryFindId(value: DaxValue) : int =
        match lookup.Value.TryGetValue value with
        | true, id -> id
        | _ -> -1

    /// The rows that hold a value id, in row order. The index is built on first use.
    member _.RowsOfId(id: int) : ArraySegment<int> =
        let starts, rows = index.Value
        ArraySegment<int>(rows, starts.[id], starts.[id + 1] - starts.[id])

    /// The rows that hold a value, in row order (empty if none does).
    member this.RowsOf(value: DaxValue) : ArraySegment<int> =
        match this.TryFindId value with
        | -1 -> ArraySegment<int>.Empty
        | id -> this.RowsOfId id

    /// How many rows hold a value id.
    member this.CountOfId(id: int) : int = this.RowsOfId(id).Count

/// Builds a `StoreColumn` row by row. Not thread-safe; use one per column.
[<Sealed>]
type ColumnBuilder(name: string, dataType: DaxType, collation: Collation) =
    let ids = ResizeArray<int>()
    let values = ResizeArray<DaxValue>()
    let mutable blankId = -1
    let mutable trueId = -1
    let mutable falseId = -1
    let ints = lazy Dictionary<int64, int>()
    let floats = lazy Dictionary<float, int>()
    // Text: an exact-match cache in front of the collation's dictionary, which costs more per lookup.
    let exact = lazy Dictionary<string, int>(StringComparer.Ordinal)
    let texts = lazy Dictionary<string, int>(collation.Comparer)
    // Variant columns hold any type, so they go through the general comparer.
    let any =
        lazy Dictionary<DaxValue, int>(DaxValue.comparer collation :> IEqualityComparer<DaxValue>)

    let fresh (v: DaxValue) =
        values.Add v
        values.Count - 1

    let variant = (dataType = DaxType.Variant)

    let addAny (v: DaxValue) =
        let d = any.Value

        match d.TryGetValue v with
        | true, id -> ids.Add id
        | _ ->
            let id = fresh v
            d.[v] <- id
            ids.Add id

    member _.Name = name
    member _.DataType = dataType
    member _.RowCount = ids.Count

    member _.AddBlank() =
        if blankId < 0 then
            blankId <- fresh DaxValue.Blank

        ids.Add blankId

    member private _.AddInt64(key: int64, make: int64 -> DaxValue) =
        let d = ints.Value

        match d.TryGetValue key with
        | true, id -> ids.Add id
        | _ ->
            let id = fresh (make key)
            d.[key] <- id
            ids.Add id

    member private _.AddFloat(key: float, make: float -> DaxValue) =
        // One id for 0.0 and -0.0 (Dictionary already treats every NaN as one key).
        let key = if key = 0.0 then 0.0 else key
        let d = floats.Value

        match d.TryGetValue key with
        | true, id -> ids.Add id
        | _ ->
            let id = fresh (make key)
            d.[key] <- id
            ids.Add id

    member private _.AddText(s: string) =
        let cache = exact.Value

        match cache.TryGetValue s with
        | true, id -> ids.Add id
        | _ ->
            // The engine doesn't keep trailing spaces.
            let trimmed = Collation.trimTrailing s
            let d = texts.Value

            let id =
                match d.TryGetValue trimmed with
                | true, id -> id
                | _ ->
                    let id = fresh (DaxValue.String trimmed)
                    d.[trimmed] <- id
                    id

            cache.[s] <- id
            ids.Add id

    member private _.AddBool(b: bool) =
        if b then
            if trueId < 0 then
                trueId <- fresh (DaxValue.Boolean true)

            ids.Add trueId
        else
            if falseId < 0 then
                falseId <- fresh (DaxValue.Boolean false)

            ids.Add falseId

    /// Add a row. The value must be BLANK or of the column's type (anything, for a Variant column):
    /// convert with `DaxValue.convertTo` first. Trailing spaces of text are dropped, as the engine does.
    member this.Add(v: DaxValue) =
        if not (DaxValue.fits dataType v) then
            invalidArg (nameof v) $"A {DaxType.name dataType} column can't hold {v}"

        match v with
        | DaxValue.Blank -> this.AddBlank()
        | DaxValue.String s when variant -> addAny (DaxValue.String(Collation.trimTrailing s))
        | _ when variant -> addAny v
        | DaxValue.Integer i -> this.AddInt64(i, DaxValue.Integer)
        | DaxValue.Currency c -> this.AddInt64(c, DaxValue.Currency)
        | DaxValue.Decimal d -> this.AddFloat(d, DaxValue.Decimal)
        | DaxValue.DateTime d -> this.AddFloat(d, DaxValue.DateTime)
        | DaxValue.String s -> this.AddText s
        | DaxValue.Boolean b -> this.AddBool b

    member _.Build() : StoreColumn =
        StoreColumn(name, dataType, collation, ids.ToArray(), values.ToArray())

module StoreColumn =

    /// A column from values, each converted to the column's type as storing it would
    /// (`DaxValue.convertTo`). The first value that can't be converted is the error.
    let ofValues
        (collation: Collation)
        (name: string)
        (dataType: DaxType)
        (values: DaxValue seq)
        : Result<StoreColumn, DaxError> =
        let b = ColumnBuilder(name, dataType, collation)
        let mutable error = None
        use e = values.GetEnumerator()

        while error.IsNone && e.MoveNext() do
            match DaxValue.convertTo dataType e.Current with
            | Ok v -> b.Add v
            | Error err -> error <- Some err

        match error with
        | Some err -> Error err
        | None -> Ok(b.Build())

    /// A column of `rows` BLANKs.
    let blank (collation: Collation) (name: string) (dataType: DaxType) (rows: int) : StoreColumn =
        if rows = 0 then
            StoreColumn(name, dataType, collation, [||], [||])
        else
            StoreColumn(name, dataType, collation, Array.zeroCreate rows, [| DaxValue.Blank |])

/// A table of a `DataStore`: columns of one length.
[<Sealed>]
type StoreTable private (name: string, columns: StoreColumn array, rowCount: int) =
    let byName =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

        for i in 0 .. columns.Length - 1 do
            d.[columns.[i].Name] <- i

        d

    member _.Name = name
    /// In the order given. Do not modify.
    member _.Columns = columns
    member _.RowCount = rowCount

    /// The position of a column, by name (names are case-insensitive, as in a model); -1 if absent.
    member _.ColumnIndex(columnName: string) : int =
        match byName.TryGetValue columnName with
        | true, i -> i
        | _ -> -1

    member this.TryColumn(columnName: string) : StoreColumn option =
        match this.ColumnIndex columnName with
        | -1 -> None
        | i -> Some columns.[i]

    /// One row's values, in column order.
    member _.Row(row: int) : DaxValue array = columns |> Array.map (fun c -> c.[row])

    /// A table from columns of one length with different names. `rows` only matters for a table
    /// with no columns.
    static member Create(name: string, columns: StoreColumn seq, ?rows: int) : Result<StoreTable, string> =
        let columns = Array.ofSeq columns

        let duplicate =
            columns
            |> Array.countBy (fun c -> c.Name.ToUpperInvariant())
            |> Array.tryFind (fun (_, n) -> n > 1)

        let rowCount =
            if columns.Length > 0 then
                columns.[0].RowCount
            else
                defaultArg rows 0

        match duplicate, columns |> Array.tryFind (fun c -> c.RowCount <> rowCount) with
        | Some(_, _), _ ->
            let dup =
                columns
                |> Array.find (fun c ->
                    columns
                    |> Array.filter (fun o -> o.Name.ToUpperInvariant() = c.Name.ToUpperInvariant())
                    |> Array.length
                        >
                        1)

            Error $"table '{name}' has more than one column named '{dup.Name}'"
        | None, Some odd ->
            Error $"column '{odd.Name}' of table '{name}' has {odd.RowCount} rows, the others have {rowCount}"
        | None, None -> Ok(StoreTable(name, columns, rowCount))

    /// The table with a column added, or replaced if it has one of that name (a calculated column,
    /// once the engine has computed it). Other columns keep their positions.
    member this.WithColumn(column: StoreColumn) : Result<StoreTable, string> =
        if column.RowCount <> rowCount && columns.Length > 0 then
            Error $"column '{column.Name}' has {column.RowCount} rows, table '{name}' has {rowCount}"
        else
            match this.ColumnIndex column.Name with
            | -1 -> StoreTable.Create(name, Array.append columns [| column |])
            | i -> StoreTable.Create(name, columns |> Array.mapi (fun j c -> if j = i then column else c))

/// One end of a relationship cardinality.
[<RequireQualifiedAccess>]
type Cardinality =
    | One
    | Many

/// Which way filters cross a relationship. `Single`: from the To table to the From table only.
[<RequireQualifiedAccess>]
type CrossFilter =
    | Single
    | Both
    /// `crossFilteringBehavior: automatic`: the engine picks (single for many-to-one).
    | Automatic

type ColumnName = { Table: string; Column: string }

/// A model relationship between two columns, by name. `From` is the many side of a many-to-one
/// relationship (the fact table's key), `To` the one side, as in TOM.
type StoreRelationship = {
    Name: string
    From: ColumnName
    To: ColumnName
    FromCardinality: Cardinality
    ToCardinality: Cardinality
    CrossFilter: CrossFilter
    IsActive: bool
}

/// A relationship whose columns are both in the store.
type ResolvedRelationship = {
    Relationship: StoreRelationship
    FromTable: StoreTable
    FromColumn: StoreColumn
    ToTable: StoreTable
    ToColumn: StoreColumn
}

/// How the keys of a relationship's two columns match up, by value id.
[<Sealed>]
type RelationshipMatch internal (r: ResolvedRelationship) =
    /// The id in `other` of each value of `column`, or -1. Keys of different types match as the
    /// value converted to the other column's type.
    let mapIds (column: StoreColumn) (other: StoreColumn) =
        column.Values
        |> Array.map (fun v ->
            if column.DataType = other.DataType || DaxValue.isBlank v then
                other.TryFindId v
            else
                match DaxValue.convertTo other.DataType v with
                | Ok converted -> other.TryFindId converted
                | Error _ -> -1)

    let toIdOfFromId = lazy (mapIds r.FromColumn r.ToColumn)
    let fromIdOfToId = lazy (mapIds r.ToColumn r.FromColumn)

    let unmatchedRows (column: StoreColumn) (map: int array) =
        if map |> Array.forall (fun id -> id >= 0) then
            [||]
        else
            let ids = column.ValueIds
            let rows = ResizeArray<int>()

            for row in 0 .. ids.Length - 1 do
                if map.[ids.[row]] < 0 then
                    rows.Add row

            rows.ToArray()

    let orphanFromRows = lazy (unmatchedRows r.FromColumn toIdOfFromId.Value)
    let orphanToRows = lazy (unmatchedRows r.ToColumn fromIdOfToId.Value)

    let toRowOfFromRow =
        lazy
            (let map = toIdOfFromId.Value
             let ids = r.FromColumn.ValueIds

             let firstRow =
                 map
                 |> Array.map (fun toId -> if toId < 0 then -1 else r.ToColumn.RowsOfId(toId).[0])

             Array.init ids.Length (fun row -> firstRow.[ids.[row]]))

    member _.Resolved = r

    /// For each value id of the From column: the id of the same key in the To column, or -1.
    member _.ToIdOfFromId: int array = toIdOfFromId.Value

    /// For each value id of the To column: the id of the same key in the From column, or -1.
    member _.FromIdOfToId: int array = fromIdOfToId.Value

    /// For each row of the From table: the row of the To table it relates to (RELATED), or -1 for
    /// the rows that belong to the To table's blank row. Where the To column repeats a key (which a
    /// one side must not; many-to-many relationships do), this is the first row with it.
    member _.ToRowOfFromRow: int array = toRowOfFromRow.Value

    /// The From rows whose key has no match in the To column, in row order: the rows DAX puts on
    /// the To table's blank row. A BLANK key is an orphan unless the To column has a BLANK too.
    member _.OrphanFromRows: int array = orphanFromRows.Value

    /// The To rows whose key no From row has (what a one-to-one relationship puts on the From
    /// table's blank row; in a many-to-one relationship these are just unused dimension rows).
    member _.OrphanToRows: int array = orphanToRows.Value

/// Tables of data and the model's relationships between them. Immutable: `WithTable` gives a new store.
[<Sealed>]
type DataStore private (collation: Collation, tables: StoreTable array, relationships: StoreRelationship list) =
    let byName =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

        for i in 0 .. tables.Length - 1 do
            d.[tables.[i].Name] <- i

        d

    let matches = ConcurrentDictionary<StoreRelationship, RelationshipMatch option>()

    member _.Collation = collation
    /// Do not modify.
    member _.Tables = tables
    /// Every single-column relationship of the model, including those to tables or columns the
    /// store doesn't hold (yet): calculated tables and columns arrive when the engine computes them.
    member _.Relationships = relationships

    /// A table by name (names are case-insensitive, as in a model).
    member _.TryTable(name: string) : StoreTable option =
        match byName.TryGetValue name with
        | true, i -> Some tables.[i]
        | _ -> None

    member this.TryColumn(name: ColumnName) : StoreColumn option =
        this.TryTable name.Table |> Option.bind (fun t -> t.TryColumn name.Column)

    /// The relationship's tables and columns, if the store has both ends.
    member this.Resolve(r: StoreRelationship) : ResolvedRelationship option =
        match this.TryTable r.From.Table, this.TryTable r.To.Table with
        | Some ft, Some tt ->
            match ft.TryColumn r.From.Column, tt.TryColumn r.To.Column with
            | Some fc, Some tc ->
                Some {
                    Relationship = r
                    FromTable = ft
                    FromColumn = fc
                    ToTable = tt
                    ToColumn = tc
                }
            | _ -> None
        | _ -> None

    /// How the relationship's keys match (computed once, on first use); None if the store doesn't
    /// have both ends.
    member this.Match(r: StoreRelationship) : RelationshipMatch option =
        matches.GetOrAdd(r, fun r -> this.Resolve r |> Option.map RelationshipMatch)

    /// The store with a table added, or replaced if it has one of that name.
    member _.WithTable(table: StoreTable) : DataStore =
        match byName.TryGetValue table.Name with
        | true, i -> DataStore(collation, tables |> Array.mapi (fun j t -> if j = i then table else t), relationships)
        | _ -> DataStore(collation, Array.append tables [| table |], relationships)

    static member Create
        (collation: Collation, tables: StoreTable seq, relationships: StoreRelationship list)
        : Result<DataStore, string> =
        let tables = Array.ofSeq tables

        match
            tables
            |> Array.countBy (fun t -> t.Name.ToUpperInvariant())
            |> Array.tryFind (fun (_, n) -> n > 1)
        with
        | Some(upper, _) ->
            let t = tables |> Array.find (fun t -> t.Name.ToUpperInvariant() = upper)
            Error $"more than one table named '{t.Name}'"
        | None -> Ok(DataStore(collation, tables, relationships))

    static member Empty(collation: Collation) = DataStore(collation, [||], [])

module DataStore =

    open Microsoft.AnalysisServices.Tabular

    let private cardinality =
        function
        | RelationshipEndCardinality.One -> Cardinality.One
        | _ -> Cardinality.Many

    /// The model's single-column relationships (the only kind import models have).
    let relationshipsOf (db: Database) : StoreRelationship list = [
        if not (isNull db.Model) then
            for r in db.Model.Relationships do
                match r with
                | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) -> {
                    Name = s.Name
                    From = {
                        Table = s.FromColumn.Table.Name
                        Column = s.FromColumn.Name
                    }
                    To = {
                        Table = s.ToColumn.Table.Name
                        Column = s.ToColumn.Name
                    }
                    FromCardinality = cardinality s.FromCardinality
                    ToCardinality = cardinality s.ToCardinality
                    CrossFilter =
                        match s.CrossFilteringBehavior with
                        | CrossFilteringBehavior.BothDirections -> CrossFilter.Both
                        | CrossFilteringBehavior.Automatic -> CrossFilter.Automatic
                        | _ -> CrossFilter.Single
                    IsActive = s.IsActive
                  }
                | _ -> ()
    ]

    /// The collation a model asks for: case-sensitive if its `collation` names a case-sensitive or
    /// binary Windows collation (`..._CS_...`, `..._BIN`, `..._BIN2`), otherwise the default.
    let collationOf (db: Database) : Collation =
        let name =
            if isNull db.Model || isNull db.Model.Collation then
                ""
            else
                db.Model.Collation

        let parts = name.ToUpperInvariant().Split('_')

        if parts |> Array.exists (fun p -> p = "CS" || p = "BIN" || p = "BIN2") then
            Collation.ordinal
        else
            Collation.ordinalIgnoreCase
