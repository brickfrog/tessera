/// `tess data init`: a fixture directory for a model, as header-only CSV files or filled with
/// synthetic rows that satisfy the model's relationships and keys.
///
/// The rows are a pure function of the model, the row count and the seed: the same on every run,
/// machine and OS (no `System.Random`, no string hash codes, invariant formatting, LF line ends).
module Tessera.FixtureInit

open System
open System.Collections.Generic
open System.IO
open System.Text
open Microsoft.AnalysisServices.Tabular

type Options = {
    /// Rows per table; 0 writes the header only. A table gets fewer where it must: a unique
    /// True/False column allows two rows.
    Rows: int
    Seed: int64
}

module Options =
    let defaults = { Rows = 0; Seed = 1L }

// ---- Deterministic pseudo-randomness -----------------------------------------------------------

/// SplitMix64's finalizer: a bijection on 64 bits that scrambles well.
let private mix (z: uint64) =
    let z = (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
    let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
    z ^^^ (z >>> 31)

/// FNV-1a over the UTF-16 code units: a hash of a name that never changes between runs.
let private hashName (s: string) =
    let mutable h = 0xCBF29CE484222325UL

    for c in s do
        h <- (h ^^^ uint64 c) * 0x100000001B3UL

    h

/// The pseudo-random number of a row of a column.
let private random (seed: uint64) (column: uint64) (row: int) =
    mix (mix (seed ^^^ column) + (uint64 row + 1UL) * 0x9E3779B97F4A7C15UL)

// ---- The plan ----------------------------------------------------------------------------------

type private Col = {
    Table: string
    Name: string
    Type: DaxType
    Hash: uint64
    /// Every row must have a different value: a key, a unique column, or the one side of a relationship.
    Unique: bool
    /// The column whose values this one takes (the other end of a relationship), so they match.
    mutable Parent: Col option
    /// The relationship that link came from is one-to-one: both tables need the same rows.
    mutable OneToOne: bool
    mutable IsParent: bool
}

/// What the columns sharing one root's keys have in common.
type private Group = {
    /// All text: the keys can be named after the root column ("Product 1").
    AllText: bool
    /// One is True/False: number keys start at 0, so that 0 and 1 are FALSE and TRUE.
    AnyBoolean: bool
}

let private key (table: string) (column: string) =
    table.ToUpperInvariant(), column.ToUpperInvariant()

let rec private root (c: Col) =
    match c.Parent with
    | Some p -> root p
    | None -> c

let private firstDay = DaxValue.serialOfDateTime (DateTime(2020, 1, 1))

/// The most rows a table with a unique column of this type can have.
let private capacity =
    function
    | DaxType.Boolean -> 2
    // Day-granular dates run out at year 9999.
    | DaxType.DateTime -> 2_000_000
    | _ -> Int32.MaxValue

/// The plan for a model: its fixture tables with their columns, and each table's row count.
let private plan (db: Database) (opts: Options) =
    let tables = Fixture.dataTables db
    let cols = Dictionary<string * string, Col>()
    let relationships = DataStore.relationshipsOf db

    let isOneSide (table: string) (column: string) =
        relationships
        |> List.exists (fun r ->
            (r.ToCardinality = Cardinality.One
             && key r.To.Table r.To.Column = key table column)
            || (r.FromCardinality = Cardinality.One
                && key r.From.Table r.From.Column = key table column))

    let byTable =
        tables
        |> List.map (fun t ->
            t,
            Fixture.dataColumns t
            |> List.map (fun (c, dt) ->
                let col = {
                    Table = t.Name
                    Name = c.Name
                    Type = dt
                    Hash = hashName (t.Name + "\u0000" + c.Name)
                    Unique = c.IsKey || c.IsUnique || isOneSide t.Name c.Name
                    Parent = None
                    OneToOne = false
                    IsParent = false
                }

                cols.[key t.Name c.Name] <- col
                col))

    for r in relationships do
        // The child takes the parent's values: the many side takes the one side's.
        let childName, parentName =
            if r.ToCardinality = Cardinality.One then r.From, r.To
            elif r.FromCardinality = Cardinality.One then r.To, r.From
            else r.From, r.To

        match
            cols.TryGetValue(key childName.Table childName.Column),
            cols.TryGetValue(key parentName.Table parentName.Column)
        with
        | (true, child), (true, parent) ->
            let loops = obj.ReferenceEquals(root parent, child)
            // A unique column can only copy a unique column.
            let fits = parent.Unique || not child.Unique

            if child.Parent.IsNone && not loops && fits then
                child.Parent <- Some parent
                child.OneToOne <- (r.ToCardinality = Cardinality.One && r.FromCardinality = Cardinality.One)
                parent.IsParent <- true
        | _ -> ()

    let rows = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

    for t, columns in byTable do
        let limit =
            columns
            |> List.filter (fun c -> c.Unique)
            |> List.map (fun c -> capacity c.Type)
            |> List.fold min Int32.MaxValue

        rows.[t.Name] <- min opts.Rows limit

    // A unique column that copies another can't have more rows than it; one-to-one needs as many.
    let mutable changed = true

    while changed do
        changed <- false

        for c in cols.Values do
            match c.Parent with
            | Some p ->
                if c.Unique && rows.[c.Table] > rows.[p.Table] then
                    rows.[c.Table] <- rows.[p.Table]
                    changed <- true

                if c.OneToOne && rows.[p.Table] > rows.[c.Table] then
                    rows.[p.Table] <- rows.[c.Table]
                    changed <- true
            | None -> ()

    // The types of the columns that take each root's values decide how its keys are written.
    let groups = Dictionary<Col, Group>(HashIdentity.Reference)

    for c in cols.Values do
        let r = root c

        let soFar =
            match groups.TryGetValue r with
            | true, g -> g
            | _ -> { AllText = true; AnyBoolean = false }

        groups.[r] <- {
            AllText = soFar.AllText && c.Type = DaxType.String
            AnyBoolean = soFar.AnyBoolean || c.Type = DaxType.Boolean
        }

    byTable, rows, groups

// ---- Values ------------------------------------------------------------------------------------

/// The i-th key of a type: all different (until a type runs out).
let private ownKey (t: DaxType) (label: string option) (first: int) (i: int) : DaxValue =
    match t with
    | DaxType.Integer -> DaxValue.Integer(int64 i + int64 first)
    | DaxType.Decimal -> DaxValue.Decimal(float i + float first)
    | DaxType.Currency -> DaxValue.Currency((int64 i + int64 first) * DaxValue.CurrencyScale)
    // Next to a True/False column the serials must be 0 and 1; otherwise days from 2020-01-01.
    | DaxType.DateTime -> DaxValue.DateTime((if first = 0 then 0.0 else firstDay) + float i)
    | DaxType.Boolean -> DaxValue.Boolean(i % 2 = 1)
    | DaxType.String
    | DaxType.Variant ->
        match label with
        | Some name -> DaxValue.String $"{name} {i + 1}"
        | None -> DaxValue.String(string (i + first))

let private generateTable
    (collation: Collation)
    (seed: uint64)
    (rows: Dictionary<string, int>)
    (groups: Dictionary<Col, Group>)
    (table: Table)
    (columns: Col list)
    : StoreTable =
    let n = rows.[table.Name]

    /// Which of its root's keys a row of a column holds.
    let rec keyIndex (c: Col) (row: int) =
        match c.Parent with
        | None -> row
        | Some p ->
            let parentRows = rows.[p.Table]

            if c.Unique || parentRows = 0 then
                keyIndex p row
            else
                keyIndex p (int (random seed c.Hash row % uint64 parentRows))

    let built =
        columns
        |> List.map (fun c ->
            let b = ColumnBuilder(c.Name, c.Type, collation)

            if c.Unique || c.Parent.IsSome || c.IsParent then
                let r = root c
                let group = groups.[r]
                let label = if group.AllText then Some r.Name else None
                let first = if group.AnyBoolean then 0 else 1
                let indices = Array.init n (keyIndex c)

                // The root's keys, as this column's type. If one of them can't be that type, the
                // column gets keys of its own type instead (all of it, so they stay distinct).
                let converted =
                    indices
                    |> Array.map (fun i -> DaxValue.convertTo c.Type (ownKey r.Type label first i))

                if converted |> Array.forall Result.isOk then
                    for v in converted do
                        match v with
                        | Ok v -> b.Add v
                        | Error _ -> ()
                else
                    for i in indices do
                        b.Add(ownKey c.Type None first i)
            else
                let distinctTexts = uint64 (max 1 (min n 10))

                for row in 0 .. n - 1 do
                    let x = random seed c.Hash row

                    match c.Type with
                    | DaxType.Integer -> b.AddInteger(int64 (x % 1000UL))
                    | DaxType.Decimal -> b.AddDecimal(float (x % 100000UL) / 100.0)
                    | DaxType.Currency -> b.AddCurrency(int64 (x % 100000UL) * 100L)
                    | DaxType.DateTime -> b.AddDateTime(firstDay + float (x % 1826UL))
                    | DaxType.Boolean -> b.AddBoolean(x % 2UL = 1UL)
                    | DaxType.String
                    | DaxType.Variant -> b.AddString $"{c.Name} {x % distinctTexts + 1UL}"

            b.Build())

    match StoreTable.Create(table.Name, built, n) with
    | Ok t -> t
    // Unreachable for a model TOM accepts: its column names are unique and every column has n rows.
    | Error e -> failwith e

/// The fixture tables for a model: every table a fixture supplies (not calculated tables or
/// calculation groups) with its data columns, in model order, and `opts.Rows` synthetic rows.
///
/// - A key, a unique column and the one side of a relationship get a different value per row.
/// - The many side of a relationship gets values of the one side, so no row is an orphan.
/// - Other columns get pseudo-random values: small numbers, a few repeating texts, dates in 2020-2024.
/// - No BLANKs.
///
/// Where a relationship's other end is computed by the engine (a calculated table), its key is
/// generated on its own and may not match what the engine computes.
let generate (db: Database) (opts: Options) : StoreTable list =
    let byTable, rows, groups = plan db opts
    let collation = DataStore.collationOf db
    let seed = mix (uint64 opts.Seed)

    byTable
    |> List.map (fun (t, columns) -> generateTable collation seed rows groups t columns)

// ---- Writing -----------------------------------------------------------------------------------

type Written = {
    Table: string
    Path: string
    Rows: int
    Columns: int
}

/// Write `generate`'s tables to a directory as `<Table>.csv` (UTF-8, no byte-order mark, LF).
///
/// Tables without data columns get no file. Refuses, before writing anything, if a table already
/// has a file there (of either format, under any casing): `DestinationNotEmpty`, naming it. With
/// `force` those files are replaced. Other files in the directory are left alone.
let write (db: Database) (directory: string) (opts: Options) (force: bool) : Result<Written list, TessError> =
    let full = Path.GetFullPath directory

    if File.Exists full then
        Error(InvalidFixture(full, "it is a file; a fixture is a directory"))
    else
        let tables = generate db opts |> List.filter (fun t -> t.Columns.Length > 0)

        let existing =
            if Directory.Exists full then
                Directory.EnumerateFiles full |> Seq.map Path.GetFileName |> List.ofSeq
            else
                []

        let taken =
            (Fixture.assign (tables |> List.map (fun t -> t.Name)) existing).Tables
            |> List.collect snd

        match taken with
        | first :: _ when not force -> Error(DestinationNotEmpty(Path.Combine(full, first)))
        | _ ->
            let mutable current = full

            try
                Directory.CreateDirectory full |> ignore

                for f in taken do
                    current <- Path.Combine(full, f)
                    File.Delete current

                Ok [
                    for t in tables do
                        let path = Path.Combine(full, FixtureName.encode t.Name + ".csv")
                        current <- path
                        use writer = new StreamWriter(path, false, UTF8Encoding false, 1 <<< 16)
                        Fixture.writeCsv writer t

                        {
                            Table = t.Name
                            Path = path
                            Rows = t.RowCount
                            Columns = t.Columns.Length
                        }
                ]
            with
            | :? IOException
            | :? UnauthorizedAccessException
            | :? NotSupportedException as e -> Error(SaveFailed(current, e.Message))
