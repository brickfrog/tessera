module Tessera.Tests.DataStoreTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Tessera

let private ci = Collation.ordinalIgnoreCase

let private ok (r: Result<'a, 'e>) =
    match r with
    | Ok v -> v
    | Error e -> failtestf "unexpected error: %A" e

let private ints (xs: int list) =
    xs |> List.map (fun i -> DaxValue.Integer(int64 i))

let private strs (xs: string list) = xs |> List.map DaxValue.String

let private column name t values =
    StoreColumn.ofValues ci name t values |> ok

let private relationship name (ft, fc) (tt, tc) = {
    Name = name
    From = { Table = ft; Column = fc }
    To = { Table = tt; Column = tc }
    FromCardinality = Cardinality.Many
    ToCardinality = Cardinality.One
    CrossFilter = CrossFilter.Single
    IsActive = true
}

/// Sales (5 rows, one orphan key and one BLANK key) -> Product (3 rows).
let private store () =
    let product =
        StoreTable.Create(
            "Product",
            [
                column "ProductKey" DaxType.Integer (ints [ 1; 2; 3 ])
                column "Name" DaxType.String (strs [ "Bike"; "Helmet"; "Lock" ])
            ]
        )
        |> ok

    let sales =
        StoreTable.Create(
            "Sales",
            [
                column "ProductKey" DaxType.Integer (ints [ 2; 1; 9; 2 ] @ [ DaxValue.Blank ])
                column "Qty" DaxType.Integer (ints [ 1; 2; 3; 4; 5 ])
            ]
        )
        |> ok

    DataStore.Create(ci, [ sales; product ], [ relationship "r" ("Sales", "ProductKey") ("Product", "ProductKey") ])
    |> ok

[<Tests>]
let tests =
    testList "DataStore" [
        testList "columns" [
            testCase "rows, distinct values in first-seen order, BLANK as a value"
            <| fun () ->
                let c =
                    column
                        "C"
                        DaxType.Integer
                        (ints [ 5; 3; 5 ] @ [ DaxValue.Blank ] @ ints [ 3; 7 ] @ [ DaxValue.Blank ])

                Expect.equal c.RowCount 7 ""
                Expect.equal (List.ofArray c.Values) (ints [ 5; 3 ] @ [ DaxValue.Blank ] @ ints [ 7 ]) ""
                Expect.equal c.DistinctCount 4 ""
                Expect.equal c.BlankId 2 ""
                Expect.equal c.[0] (DaxValue.Integer 5L) ""
                Expect.equal c.[3] DaxValue.Blank ""
                Expect.equal (List.ofArray c.ValueIds) [ 0; 1; 0; 2; 1; 3; 2 ] ""
                Expect.isFalse c.IsUnique ""

            testCase "the value index: rows of a value, in row order"
            <| fun () ->
                let c = column "C" DaxType.Integer (ints [ 5; 3; 5; 7; 3; 5 ])
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Integer 5L))) [ 0; 2; 5 ] ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Integer 3L))) [ 1; 4 ] ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Integer 8L))) [] "absent"
                Expect.equal (List.ofSeq (c.RowsOf DaxValue.Blank)) [] ""
                Expect.equal (c.TryFindId(DaxValue.Integer 7L)) 2 ""
                Expect.equal (c.TryFindId(DaxValue.Decimal 7.0)) -1 "another type is another value"
                Expect.equal (c.CountOfId 0) 3 ""

            testCase "text groups case-insensitively, keeps the first casing, and loses trailing spaces"
            <| fun () ->
                let c =
                    column
                        "Addressee"
                        DaxType.String
                        (strs [
                            "Taina Hasu"
                            "MURALI DAS"
                            "TAINA HASU"
                            "Murali Das "
                            "taina hasu  "
                            " Taina Hasu"
                            ""
                        ])

                Expect.equal (List.ofArray c.Values) (strs [ "Taina Hasu"; "MURALI DAS"; " Taina Hasu"; "" ]) ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.String "taina HASU"))) [ 0; 2; 4 ] ""
                Expect.equal c.[3] (DaxValue.String "MURALI DAS") "a row reads as the stored casing"
                Expect.equal c.BlankId -1 "the empty string is not BLANK"

                let trailing = column "T" DaxType.String (strs [ "a  " ])
                Expect.equal trailing.[0] (DaxValue.String "a") "stored without its trailing spaces"

                let cs =
                    StoreColumn.ofValues Collation.ordinal "C" DaxType.String (strs [ "a"; "A" ])
                    |> ok

                Expect.equal cs.DistinctCount 2 "a case-sensitive collation"

            testCase "decimals: 0.0 and -0.0 are one value, NaN is one value"
            <| fun () ->
                let c =
                    column "D" DaxType.Decimal [ for d in [ 0.0; -0.0; nan; nan; 1.5 ] -> DaxValue.Decimal d ]

                Expect.equal c.DistinctCount 3 ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Decimal -0.0))) [ 0; 1 ] ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Decimal nan))) [ 2; 3 ] ""

            testCase "storing converts to the column's type, or fails"
            <| fun () ->
                let c =
                    column "C" DaxType.Currency [ DaxValue.Integer 2L; DaxValue.Decimal 1.00005; DaxValue.String "3.5" ]

                Expect.equal
                    (List.ofArray c.Values)
                    [ DaxValue.Currency 20000L; DaxValue.Currency 10001L; DaxValue.Currency 35000L ]
                    ""

                Expect.equal
                    (StoreColumn.ofValues ci "C" DaxType.Integer (strs [ "x" ])
                     |> Result.mapError ignore)
                    (Error())
                    ""

                Expect.throws
                    (fun () -> ColumnBuilder("C", DaxType.Integer, ci).Add(DaxValue.String "x"))
                    "the builder takes only values of its type"

            testCase "a Variant column holds any type, each its own value"
            <| fun () ->
                let c =
                    column "V" DaxType.Variant [
                        DaxValue.Integer 1L
                        DaxValue.Decimal 1.0
                        DaxValue.String "A"
                        DaxValue.String "a "
                        DaxValue.Boolean true
                        DaxValue.Blank
                        DaxValue.Integer 1L
                    ]

                Expect.equal c.DistinctCount 5 ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.String "a"))) [ 2; 3 ] ""
                Expect.equal (List.ofSeq (c.RowsOf(DaxValue.Integer 1L))) [ 0; 6 ] ""

            testCase "an empty column and a column of BLANKs"
            <| fun () ->
                let e = column "E" DaxType.String []
                Expect.equal (e.RowCount, e.DistinctCount, e.IsUnique) (0, 0, true) ""
                Expect.equal (List.ofSeq (e.RowsOf DaxValue.Blank)) [] ""
                let b = StoreColumn.blank ci "B" DaxType.Integer 3
                Expect.equal (b.RowCount, b.DistinctCount, b.BlankId) (3, 1, 0) ""
                Expect.equal (List.ofSeq (b.RowsOf DaxValue.Blank)) [ 0; 1; 2 ] ""
                Expect.equal (StoreColumn.blank ci "B" DaxType.Integer 0).DistinctCount 0 ""

            testProperty "the index is the inverse of the rows, and Values are distinct"
            <| fun (xs: int list) (ys: string list) ->
                let texts =
                    ys
                    |> List.map (fun s ->
                        if isNull s then
                            DaxValue.Blank
                        else
                            DaxValue.String(s.ToLowerInvariant() + s))

                let check (c: StoreColumn) (source: DaxValue list) =
                    let cmp =
                        DaxValue.comparer ci :> System.Collections.Generic.IEqualityComparer<DaxValue>

                    let rowsAgree =
                        source |> List.mapi (fun row v -> cmp.Equals(c.[row], v)) |> List.forall id

                    let indexAgrees =
                        [ 0 .. c.DistinctCount - 1 ]
                        |> List.forall (fun id ->
                            let rows = List.ofSeq (c.RowsOfId id)

                            rows =
                                (c.ValueIds
                                 |> Array.indexed
                                 |> Array.filter (fun (_, i) -> i = id)
                                 |> Array.map fst
                                 |> List.ofArray))

                    let distinct =
                        c.Values
                        |> Array.distinctBy (fun v -> cmp.GetHashCode v, c.TryFindId v)
                        |> Array.length
                            =
                            c.DistinctCount

                    rowsAgree
                    && indexAgrees
                    && distinct
                    && (c.Values |> Array.mapi (fun i v -> c.TryFindId v = i) |> Array.forall id)

                check (column "I" DaxType.Integer (ints xs)) (ints xs)
                && check (column "S" DaxType.String texts) texts
        ]

        testList "tables" [
            testCase "columns by name (case-insensitive), rows"
            <| fun () ->
                let s = store ()
                let sales = s.TryTable "SALES" |> Option.get
                Expect.equal sales.Name "Sales" ""
                Expect.equal sales.RowCount 5 ""
                Expect.equal (sales.ColumnIndex "qty") 1 ""
                Expect.equal (sales.ColumnIndex "nope") -1 ""
                Expect.equal (sales.TryColumn "nope" |> Option.isSome) false ""
                Expect.equal (List.ofArray (sales.Row 2)) (ints [ 9; 3 ]) ""
                Expect.isNone (s.TryTable "Nope") ""

                Expect.equal
                    (s.TryColumn { Table = "product"; Column = "name" }
                     |> Option.map (fun c -> c.Name))
                    (Some "Name")
                    ""

            testCase "columns must have one length and different names"
            <| fun () ->
                let a = column "A" DaxType.Integer (ints [ 1; 2 ])

                Expect.isError
                    (StoreTable.Create("T", [ a; column "B" DaxType.Integer (ints [ 1 ]) ]))
                    "different lengths"

                Expect.isError (StoreTable.Create("T", [ a; column "a" DaxType.Integer (ints [ 1; 2 ]) ])) "same name"
                Expect.equal (StoreTable.Create("T", [], rows = 4) |> ok).RowCount 4 "no columns: rows as given"

            testCase "WithColumn adds or replaces, and leaves the original alone"
            <| fun () ->
                let t = StoreTable.Create("T", [ column "A" DaxType.Integer (ints [ 1; 2 ]) ]) |> ok
                let added = t.WithColumn(column "B" DaxType.Integer (ints [ 3; 4 ])) |> ok
                Expect.equal (added.Columns |> Array.map (fun c -> c.Name)) [| "A"; "B" |] ""
                Expect.equal t.Columns.Length 1 "immutable"
                let replaced = added.WithColumn(column "a" DaxType.Integer (ints [ 7; 8 ])) |> ok
                Expect.equal (replaced.Columns |> Array.map (fun c -> c.Name)) [| "a"; "B" |] "same position"
                Expect.equal replaced.Columns.[0].[0] (DaxValue.Integer 7L) ""
                Expect.isError (t.WithColumn(column "B" DaxType.Integer (ints [ 3 ]))) "wrong length"
        ]

        testList "relationships" [
            testCase "resolve to the store's columns"
            <| fun () ->
                let s = store ()
                let r = s.Relationships.Head
                let resolved = s.Resolve r |> Option.get
                Expect.equal resolved.FromTable.Name "Sales" ""
                Expect.equal resolved.ToColumn.Name "ProductKey" ""
                Expect.equal resolved.ToTable.RowCount 3 ""

            testCase "matching keys, the related row, and the orphans"
            <| fun () ->
                let s = store ()
                let m = s.Match s.Relationships.Head |> Option.get
                // From values: 2, 1, 9, BLANK. To values: 1, 2, 3.
                Expect.equal (List.ofArray m.ToIdOfFromId) [ 1; 0; -1; -1 ] ""
                Expect.equal (List.ofArray m.FromIdOfToId) [ 1; 0; -1 ] ""
                Expect.equal (List.ofArray m.ToRowOfFromRow) [ 1; 0; -1; 1; -1 ] "RELATED's row, -1 for the blank row"
                Expect.equal (List.ofArray m.OrphanFromRows) [ 2; 4 ] "key 9 and the BLANK key"
                Expect.equal (List.ofArray m.OrphanToRows) [ 2 ] "product 3 has no sales"
                Expect.isTrue (obj.ReferenceEquals(m, s.Match s.Relationships.Head |> Option.get)) "computed once"

            testCase "a relationship to a table the store doesn't have doesn't resolve until it arrives"
            <| fun () ->
                let s = store ()
                let toDate = relationship "d" ("Sales", "Qty") ("Date", "DateKey")

                let s = DataStore.Create(ci, s.Tables, s.Relationships @ [ toDate ]) |> ok

                Expect.isNone (s.Resolve toDate) "a calculated table, not computed yet"
                Expect.isNone (s.Match toDate) ""

                let date =
                    StoreTable.Create("Date", [ column "DateKey" DaxType.Integer (ints [ 1; 2; 3; 4 ]) ])
                    |> ok

                let later = s.WithTable date
                Expect.isSome (later.Resolve toDate) ""
                Expect.equal (later.Match toDate |> Option.get).OrphanFromRows [| 4 |] "Qty 5 has no date"
                Expect.isNone (s.Match toDate) "the earlier store is unchanged"
                Expect.equal later.Tables.Length 3 ""

            testCase "WithTable replaces a table of the same name"
            <| fun () ->
                let s = store ()

                let product =
                    StoreTable.Create("product", [ column "ProductKey" DaxType.Integer (ints [ 9 ]) ])
                    |> ok

                let s2 = s.WithTable product
                Expect.equal s2.Tables.Length 2 ""
                Expect.equal (s2.Match s2.Relationships.Head |> Option.get).OrphanFromRows [| 0; 1; 3; 4 |] ""
                Expect.equal (s.Match s.Relationships.Head |> Option.get).OrphanFromRows [| 2; 4 |] ""

            testCase "keys of different types match by converted value; text keys by the collation"
            <| fun () ->
                let dim =
                    StoreTable.Create(
                        "Dim",
                        [
                            column "K" DaxType.String (strs [ "1"; "2"; "abc" ])
                            column "Code" DaxType.String (strs [ "x"; "Y"; "z" ])
                        ]
                    )
                    |> ok

                let fact =
                    StoreTable.Create(
                        "Fact",
                        [
                            column "K" DaxType.Integer (ints [ 2; 3; 1 ])
                            column "Code" DaxType.String (strs [ "X "; "y"; "w" ])
                        ]
                    )
                    |> ok

                let byKey = relationship "k" ("Fact", "K") ("Dim", "K")
                let byCode = relationship "c" ("Fact", "Code") ("Dim", "Code")
                let s = DataStore.Create(ci, [ dim; fact ], [ byKey; byCode ]) |> ok
                Expect.equal (List.ofArray (s.Match byKey |> Option.get).ToRowOfFromRow) [ 1; -1; 0 ] ""
                Expect.equal (List.ofArray (s.Match byKey |> Option.get).OrphanToRows) [ 2 ] "'abc' is no integer"
                Expect.equal (List.ofArray (s.Match byCode |> Option.get).ToRowOfFromRow) [ 0; 1; -1 ] ""

            testCase "tables must have different names"
            <| fun () ->
                let t n = StoreTable.Create(n, []) |> ok
                Expect.isError (DataStore.Create(ci, [ t "A"; t "a" ], [])) ""
                Expect.equal (DataStore.Empty ci).Tables.Length 0 ""

            testCase "the model's relationships and collation"
            <| fun () ->
                let db = Fixtures.sampleDatabase ()

                Expect.equal
                    (DataStore.relationshipsOf db)
                    [
                        {
                            Name = "Sales-Date"
                            From = { Table = "Sales"; Column = "DateKey" }
                            To = { Table = "Date"; Column = "DateKey" }
                            FromCardinality = Cardinality.Many
                            ToCardinality = Cardinality.One
                            CrossFilter = CrossFilter.Single
                            IsActive = true
                        }
                    ]
                    ""

                Expect.equal (DataStore.collationOf db).Name "ordinal-ignore-case" ""
                db.Model.Collation <- "Latin1_General_100_BIN2_UTF8"
                Expect.equal (DataStore.collationOf db).Name "ordinal" ""
                db.Model.Collation <- "Latin1_General_CS_AS"
                Expect.equal (DataStore.collationOf db).Name "ordinal" ""
                db.Model.Collation <- "Latin1_General_CI_AS"
                Expect.equal (DataStore.collationOf db).Name "ordinal-ignore-case" ""
        ]
    ]
