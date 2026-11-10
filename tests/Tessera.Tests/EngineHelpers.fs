/// Small models built in code for the engine's tests, and ways to run DAX against them.
module Tessera.Tests.EngineHelpers

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera

/// A cell of a test table: an int or int64 is a whole number, a float a decimal number, a
/// `decimal` a fixed decimal (Currency), plus string, bool, DateTime; null is BLANK.
let value (o: obj) : DaxValue =
    match o with
    | null -> DaxValue.Blank
    | :? int as i -> DaxValue.Integer(int64 i)
    | :? int64 as i -> DaxValue.Integer i
    | :? float as f -> DaxValue.Decimal f
    | :? decimal as d ->
        match DaxValue.tryCurrency d with
        | Some c -> c
        | None -> failwithf "%M is out of range" d
    | :? string as s -> DaxValue.String s
    | :? bool as b -> DaxValue.Boolean b
    | :? DateTime as d -> DaxValue.ofDateTime d
    | other -> failwithf "unsupported test value %A" other

type TestTable = {
    Name: string
    Columns: (string * DataType) list
    Rows: obj list list
    /// Calculated columns: name, type, expression.
    Calculated: (string * DataType * string) list
    /// A calculated table's expression (then Columns name its columns and Rows is empty).
    Expression: string option
}

let table (name: string) (columns: (string * DataType) list) (rows: obj list list) : TestTable = {
    Name = name
    Columns = columns
    Rows = rows
    Calculated = []
    Expression = None
}

let withCalculated (columns: (string * DataType * string) list) (t: TestTable) = { t with Calculated = columns }

let calculatedTable (name: string) (columns: (string * DataType) list) (expression: string) : TestTable = {
    Name = name
    Columns = columns
    Rows = []
    Calculated = []
    Expression = Some expression
}

type TestRel = {
    From: string * string
    To: string * string
    Active: bool
    Both: bool
    FromCardinality: RelationshipEndCardinality
    ToCardinality: RelationshipEndCardinality
}

/// A many-to-one, single-direction, active relationship.
let rel (fromTable: string, fromColumn: string) (toTable: string, toColumn: string) : TestRel = {
    From = fromTable, fromColumn
    To = toTable, toColumn
    Active = true
    Both = false
    FromCardinality = RelationshipEndCardinality.Many
    ToCardinality = RelationshipEndCardinality.One
}

let inactive (r: TestRel) = { r with Active = false }
let bothWays (r: TestRel) = { r with Both = true }

type TestModel = {
    Tables: TestTable list
    Rels: TestRel list
    /// Table, name, expression.
    Measures: (string * string * string) list
    /// Name, definition.
    Functions: (string * string) list
}

let ci = Collation.ordinalIgnoreCase

/// The TOM database and the data of a test model.
let build (m: TestModel) : Database * DataStore =
    let db = new Database(Name = "Test", ID = "Test", CompatibilityLevel = 1702)
    let model = new Model(Name = "Model", Culture = "en-US")
    db.Model <- model

    for t in m.Tables do
        let tt = new Table(Name = t.Name)

        match t.Expression with
        | Some e ->
            for (name, dt) in t.Columns do
                tt.Columns.Add(new CalculatedTableColumn(Name = name, DataType = dt, SourceColumn = $"[{name}]"))

            tt.Partitions.Add(new Partition(Name = t.Name, Source = new CalculatedPartitionSource(Expression = e)))
        | None ->
            for (name, dt) in t.Columns do
                tt.Columns.Add(new DataColumn(Name = name, DataType = dt, SourceColumn = name))

            tt.Partitions.Add(
                new Partition(Name = t.Name, Source = new MPartitionSource(Expression = "let x = 1 in x"))
            )

        for (name, dt, e) in t.Calculated do
            tt.Columns.Add(new CalculatedColumn(Name = name, DataType = dt, Expression = e))

        for (table, name, e) in m.Measures do
            if table = t.Name then
                tt.Measures.Add(new Measure(Name = name, Expression = e))

        model.Tables.Add tt

    m.Rels
    |> List.iteri (fun i r ->
        let column (table: string, name: string) = model.Tables.[table].Columns.[name]

        model.Relationships.Add(
            new SingleColumnRelationship(
                Name = $"r{i}",
                FromColumn = column r.From,
                ToColumn = column r.To,
                FromCardinality = r.FromCardinality,
                ToCardinality = r.ToCardinality,
                IsActive = r.Active,
                CrossFilteringBehavior =
                    (if r.Both then
                         CrossFilteringBehavior.BothDirections
                     else
                         CrossFilteringBehavior.OneDirection)
            )
        ))

    for (name, e) in m.Functions do
        model.Functions.Add(new Function(Name = name, Expression = e))

    let tables = [
        for t in m.Tables do
            if t.Expression.IsNone then
                let columns = [
                    for i, (name, dt) in List.indexed t.Columns ->
                        let daxType = (DaxType.ofTom dt).Value

                        match StoreColumn.ofValues ci name daxType (t.Rows |> List.map (fun r -> value r.[i])) with
                        | Ok c -> c
                        | Error e -> failwithf "bad test data in %s[%s]: %A" t.Name name e
                ]

                match StoreTable.Create(t.Name, columns, t.Rows.Length) with
                | Ok s -> s
                | Error e -> failwith e
    ]

    match DataStore.Create(ci, tables, DataStore.relationshipsOf db) with
    | Ok store -> db, store
    | Error e -> failwith e

let prepare (m: TestModel) : Engine.Prepared =
    let db, store = build m

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

/// A query value from a test value.
let qv (o: obj) : QueryValue =
    match value o with
    | DaxValue.Blank -> QueryValue.Blank
    | DaxValue.Integer i -> QueryValue.Int64 i
    | DaxValue.Decimal d -> QueryValue.Double d
    | DaxValue.Currency c -> QueryValue.Decimal(DaxValue.currencyValue c)
    | DaxValue.String s -> QueryValue.String s
    | DaxValue.Boolean b -> QueryValue.Boolean b
    | DaxValue.DateTime s -> QueryValue.DateTime((DaxValue.dateTimeOfSerial s).Value)

/// The first result of a query.
let query (p: Engine.Prepared) (dax: string) : QueryResult =
    match Engine.evaluateQuery p dax with
    | Ok(r :: _) -> r
    | Ok [] -> failtest "the query has no EVALUATE"
    | Error e -> failtestf "query failed: %s\n%s" (TessError.message e) dax

/// The rows of a query's first result, as lists.
let rows (p: Engine.Prepared) (dax: string) : QueryValue list list =
    (query p dax).Rows |> List.map List.ofArray

/// The rows in a canonical order, for results whose order isn't defined.
let sortedRows (p: Engine.Prepared) (dax: string) : QueryValue list list =
    rows p dax |> List.sortBy (fun r -> r |> List.map (fun v -> sprintf "%A" v))

let expected (rows: obj list list) : QueryValue list list = rows |> List.map (List.map qv)

let expectedSorted (rows: obj list list) : QueryValue list list =
    expected rows |> List.sortBy (fun r -> r |> List.map (fun v -> sprintf "%A" v))

let scalar (p: Engine.Prepared) (dax: string) : QueryValue =
    match Engine.evaluateScalar p dax with
    | Ok v -> v
    | Error e -> failtestf "evaluation failed: %s\n%s" (TessError.message e) dax

/// The error a query fails with.
let queryError (p: Engine.Prepared) (dax: string) : TessError =
    match Engine.evaluateQuery p dax with
    | Ok r -> failtestf "expected an error, got %A\n%s" r dax
    | Error e -> e

let scalarError (p: Engine.Prepared) (dax: string) : TessError =
    match Engine.evaluateScalar p dax with
    | Ok v -> failtestf "expected an error, got %A\n%s" v dax
    | Error e -> e

/// `expr` evaluates to `value`.
let is (p: Engine.Prepared) (dax: string) (value: obj) =
    Expect.equal (scalar p dax) (qv value) dax

/// The query's rows, in any order, are these.
let returns (p: Engine.Prepared) (dax: string) (rows: obj list list) =
    Expect.equal (sortedRows p dax) (expectedSorted rows) dax

/// The query's rows, in this order.
let returnsInOrder (p: Engine.Prepared) (dax: string) (expectedRows: obj list list) =
    Expect.equal (rows p dax) (expected expectedRows) dax

let notSupported (p: Engine.Prepared) (dax: string) (mentions: string) =
    match queryError p dax with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported, got %A\n%s" other dax

// ---- The model most tests use ------------------------------------------------------------------

/// A star schema with the classic problems in it:
///
///   Product (4): 1 Bike Red Cycling | 2 Helmet Red Safety | 3 Lock Black Safety | 4 Pump Blue Cycling
///   Customer (3): 1 Ann US | 2 Bob UK | 3 Cy US
///   Date (4): 2023-12-31 (2023) | 2024-01-01 | 2024-01-02 | 2024-02-01 (2024)
///   Sales (7): Id ProductKey CustomerKey OrderDate ShipDate Qty Price
///      1  1  1  2024-01-01  2024-01-02  2  100
///      2  1  2  2024-01-01  2024-02-01  1  100
///      3  2  1  2024-01-02  2024-01-02  3   20
///      4  3  3  2024-02-01  2024-02-01  1   15
///      5  9  2  2024-02-01  2024-02-01  4   10     <- product 9 does not exist: the blank row
///      6  2  3  2023-12-31  2024-01-01  5   20
///      7  1  1  2024-02-01  2024-02-01  1  110
///
/// Sales[ProductKey] -> Product, Sales[CustomerKey] -> Customer, Sales[OrderDate] -> Date (active),
/// Sales[ShipDate] -> Date (inactive). Product 4 (Pump) has no sales.
///
/// Amount = Qty * Price per row: 200, 100, 60, 15, 40, 100, 110. Total 625.
let star: TestModel =
    let d (y: int) (m: int) (day: int) = box (DateTime(y, m, day))

    {
        Tables = [
            table "Product" [
                "ProductKey", DataType.Int64
                "Name", DataType.String
                "Color", DataType.String
                "Category", DataType.String
            ] [
                [ 1; "Bike"; "Red"; "Cycling" ]
                [ 2; "Helmet"; "Red"; "Safety" ]
                [ 3; "Lock"; "Black"; "Safety" ]
                [ 4; "Pump"; "Blue"; "Cycling" ]
            ]
            table "Customer" [
                "CustomerKey", DataType.Int64
                "Name", DataType.String
                "Country", DataType.String
            ] [ [ 1; "Ann"; "US" ]; [ 2; "Bob"; "UK" ]; [ 3; "Cy"; "US" ] ]
            table "Date" [ "Date", DataType.DateTime; "Year", DataType.Int64; "Month", DataType.String ] [
                [ d 2023 12 31; 2023; "Dec" ]
                [ d 2024 1 1; 2024; "Jan" ]
                [ d 2024 1 2; 2024; "Jan" ]
                [ d 2024 2 1; 2024; "Feb" ]
            ]
            table "Sales" [
                "Id", DataType.Int64
                "ProductKey", DataType.Int64
                "CustomerKey", DataType.Int64
                "OrderDate", DataType.DateTime
                "ShipDate", DataType.DateTime
                "Qty", DataType.Int64
                "Price", DataType.Int64
            ] [
                [ 1; 1; 1; d 2024 1 1; d 2024 1 2; 2; 100 ]
                [ 2; 1; 2; d 2024 1 1; d 2024 2 1; 1; 100 ]
                [ 3; 2; 1; d 2024 1 2; d 2024 1 2; 3; 20 ]
                [ 4; 3; 3; d 2024 2 1; d 2024 2 1; 1; 15 ]
                [ 5; 9; 2; d 2024 2 1; d 2024 2 1; 4; 10 ]
                [ 6; 2; 3; d 2023 12 31; d 2024 1 1; 5; 20 ]
                [ 7; 1; 1; d 2024 2 1; d 2024 2 1; 1; 110 ]
            ]
        ]
        Rels = [
            rel ("Sales", "ProductKey") ("Product", "ProductKey")
            rel ("Sales", "CustomerKey") ("Customer", "CustomerKey")
            rel ("Sales", "OrderDate") ("Date", "Date")
            inactive (rel ("Sales", "ShipDate") ("Date", "Date"))
        ]
        Measures = [
            "Sales", "Amount", "SUMX ( Sales, Sales[Qty] * Sales[Price] )"
            "Sales", "Units", "SUM ( Sales[Qty] )"
            "Sales", "Orders", "COUNTROWS ( Sales )"
        ]
        Functions = []
    }

let starPrepared = lazy (prepare star)
