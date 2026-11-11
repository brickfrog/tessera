/// The window functions of the local DAX engine: INDEX, OFFSET, WINDOW, RANK and ROWNUMBER with
/// ORDERBY, PARTITIONBY and MATCHBY. Each expected value follows from a rule a source states, on
/// the small model below; the source is cited next to the test.
///
/// Sources: Learn's pages for each function and "Understanding ORDERBY, PARTITIONBY, and MATCHBY
/// functions"; dax.guide; SQLBI, "Introducing window functions in DAX" and "Understanding apply
/// semantics for window functions in DAX".
module Tessera.Tests.EngineWindowTests

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// P (products, Key unique):
///   1 Bike Red Cycling 100 | 2 Helmet Red Safety 20 | 3 Lock Black Safety 15
///   4 Pump Blue Cycling 30 | 5 Bell Black Cycling 5
/// Names ascending: Bell, Bike, Helmet, Lock, Pump. Prices by category: Cycling 135, Safety 35.
///
/// Lines (order lines, no key and no unique column; within a product the amounts differ):
///   SO1 1 product 1 10 | SO1 2 product 2 7 | SO2 1 product 1 12 | SO2 2 product 2 10 | SO3 1 product 1 8
///
/// Tied: Lines with a sixth line SO3 2 product 2 7, so product 2 has two lines of 7.
///
/// S (scores with a BLANK): A 3 | B BLANK | C 1.
let private model = {
    Tables = [
        table "P" [
            "Key", DataType.Int64
            "Name", DataType.String
            "Color", DataType.String
            "Category", DataType.String
            "Price", DataType.Int64
        ] [
            [ 1; "Bike"; "Red"; "Cycling"; 100 ]
            [ 2; "Helmet"; "Red"; "Safety"; 20 ]
            [ 3; "Lock"; "Black"; "Safety"; 15 ]
            [ 4; "Pump"; "Blue"; "Cycling"; 30 ]
            [ 5; "Bell"; "Black"; "Cycling"; 5 ]
        ]
        table "Lines" [
            "Order", DataType.String
            "Line", DataType.Int64
            "Product", DataType.Int64
            "Amount", DataType.Int64
        ] [
            [ "SO1"; 1; 1; 10 ]
            [ "SO1"; 2; 2; 7 ]
            [ "SO2"; 1; 1; 12 ]
            [ "SO2"; 2; 2; 10 ]
            [ "SO3"; 1; 1; 8 ]
        ]
        table "Tied" [
            "Order", DataType.String
            "Line", DataType.Int64
            "Product", DataType.Int64
            "Amount", DataType.Int64
        ] [
            [ "SO1"; 1; 1; 10 ]
            [ "SO1"; 2; 2; 7 ]
            [ "SO2"; 1; 1; 12 ]
            [ "SO2"; 2; 2; 10 ]
            [ "SO3"; 1; 1; 8 ]
            [ "SO3"; 2; 2; 7 ]
        ]
        table "S" [ "Name", DataType.String; "Score", DataType.Int64 ] [ [ "A"; 3 ]; [ "B"; null ]; [ "C"; 1 ] ]
    ]
    Rels = []
    Measures = [ "P", "Total Price", "SUM ( P[Price] )" ]
    Functions = []
}

let private prepared = lazy (prepare model)
let private p () = prepared.Value

let private code (dax: string) = TessError.code (queryError (p ()) dax)

/// A DAX error (not a refusal).
let private fails (dax: string) (mentions: string) =
    match queryError (p ()) dax with
    | DaxNotSupported(what, _) -> failtestf "expected an error, the engine refused: %s\n%s" what dax
    | e -> Expect.stringContains (TessError.message e) mentions dax

let private refused (dax: string) (mentions: string) = notSupported (p ()) dax mentions

let private config = {
    FsCheckConfig.defaultConfig with
        maxTest = 40
}

/// A model with one table T(K, G): K the whole numbers given (distinct), G their group.
let private generated (rows: (int * int) list) =
    prepare {
        Tables = [
            table "T" [ "K", DataType.Int64; "G", DataType.Int64 ] [ for k, g in rows -> [ box k; box g ] ]
        ]
        Rels = []
        Measures = []
        Functions = []
    }

/// Distinct keys from FsCheck's numbers, each with a group of 0, 1 or 2.
let private distinctRows (xs: int list) : (int * int) list =
    xs |> List.distinct |> List.map (fun k -> k, ((k % 3) + 3) % 3)

let private ints (p: Engine.Prepared) (dax: string) : (int64 * int64 * int64) list =
    rows p dax
    |> List.map (fun r ->
        match r with
        | [ QueryValue.Int64 k; QueryValue.Int64 g; QueryValue.Int64 n ] -> k, g, n
        | [ QueryValue.Int64 k; QueryValue.Int64 g; QueryValue.Blank ] -> k, g, Int64.MinValue
        | other -> failtestf "unexpected row %A" other)

[<Tests>]
let tests =
    testList "Engine window functions" [
        testList "INDEX" [
            // "EVALUATE INDEX(1, ALL(DimDate[CalendarYear]))" returns the first year (Learn, INDEX,
            // example 1); without ORDERBY the order is "by every column in relation that is not
            // already specified in partitionBy".
            testCase "position 1 without ORDERBY is the first value" (fun () ->
                returns (p ()) "EVALUATE INDEX ( 1, ALL ( P[Name] ) )" [ [ "Bell" ] ])
            // "position is negative: -1 is the last row, -2 is the second last row" (Learn).
            testCase "negative positions count from the end" (fun () ->
                returns (p ()) "EVALUATE INDEX ( -1, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" [ [ "Pump" ] ]
                returns (p ()) "EVALUATE INDEX ( -2, ALL ( P[Name] ), ORDERBY ( P[Name], ASC ) )" [ [ "Lock" ] ]
                returns (p ()) "EVALUATE INDEX ( 1, ALL ( P[Name] ), ORDERBY ( P[Name], DESC ) )" [ [ "Pump" ] ])
            // "When position is out of the boundary, or zero, or BLANK(), INDEX will return an empty
            // table" (Learn).
            testCase "zero, BLANK and out of range are empty" (fun () ->
                for position in [ "0"; "BLANK ()"; "6"; "-6" ] do
                    returns (p ()) $"EVALUATE INDEX ( {position}, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" [])
            // SQLBI, "Introducing window functions in DAX": INDEX ( 1, ..., PARTITIONBY ( Category ) )
            // returns "the best-selling brand for each category" (apply semantics: every category is
            // a current partition).
            testCase "PARTITIONBY with no current partition gives one row per partition" (fun () ->
                returns
                    (p ())
                    "EVALUATE INDEX ( 1, ALL ( P[Category], P[Name], P[Price] ), ORDERBY ( P[Price], DESC ), PARTITIONBY ( P[Category] ) )"
                    [ [ "Cycling"; "Bike"; 100 ]; [ "Safety"; "Helmet"; 20 ] ])
            testCase "the current partition comes from the row context" (fun () ->
                returns
                    (p ())
                    "EVALUATE ADDCOLUMNS ( VALUES ( P[Category] ), \"Cheapest\", SELECTCOLUMNS ( INDEX ( 1, ALL ( P[Category], P[Name], P[Price] ), ORDERBY ( P[Price] ), PARTITIONBY ( P[Category] ) ), \"n\", P[Name] ) )"
                    [ [ "Cycling"; "Bell" ]; [ "Safety"; "Lock" ] ])
            // The engine adds columns to ORDERBY to break ties, in a way "not documented" (SQLBI): a
            // position inside a tie is refused, one outside it is not.
            testCase "a tie at the position is refused, one elsewhere is not" (fun () ->
                // Colors ascending: Black (Lock, Bell), Blue (Pump), Red (Bike, Helmet).
                returns (p ()) "EVALUATE SELECTCOLUMNS ( INDEX ( 3, P, ORDERBY ( P[Color] ) ), \"n\", P[Name] )" [
                    [ "Pump" ]
                ]

                refused "EVALUATE INDEX ( 1, P, ORDERBY ( P[Color] ) )" "tie")
            // A table of a model table's rows keeps them: as a CALCULATE filter it filters the table.
            testCase "as a CALCULATE filter" (fun () ->
                is (p ()) "CALCULATE ( [Total Price], INDEX ( 1, P, ORDERBY ( P[Price], DESC ) ) )" 100)
        ]

        testList "OFFSET" [
            // "The number of rows before (negative value) or after (positive value) the current row"
            // (Learn); "The delta value causes a shift to a row that does not exist within the
            // partition" gives an empty table.
            testCase "the next and previous rows in a row context" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"next\", MAXX ( OFFSET ( 1, P, ORDERBY ( P[Key] ) ), P[Name] ), \"prev\", MAXX ( OFFSET ( -1, P, ORDERBY ( P[Key] ) ), P[Name] ) )"
                    [
                        [ "Bike"; "Helmet"; null ]
                        [ "Helmet"; "Lock"; "Bike" ]
                        [ "Lock"; "Pump"; "Helmet" ]
                        [ "Pump"; "Bell"; "Lock" ]
                        [ "Bell"; null; "Pump" ]
                    ])
            // Learn, "Understanding ORDERBY, PARTITIONBY, and MATCHBY": PARTITIONBY ( [Color] )
            // "divide[s] the table into parts", and "for every first row in each part there is no
            // previous row".
            testCase "PARTITIONBY restarts" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"prev\", MAXX ( OFFSET ( -1, P, ORDERBY ( P[Price] ), PARTITIONBY ( P[Category] ) ), P[Name] ) )"
                    [
                        [ "Bike"; "Pump" ]
                        [ "Helmet"; "Lock" ]
                        [ "Lock"; null ]
                        [ "Pump"; "Bell" ]
                        [ "Bell"; null ]
                    ])
            // SQLBI, apply semantics, example 2 without GENERATEALL: with no row context every value
            // of the filter context is a current row, "Therefore, the result contains all rows
            // except the last one."
            testCase "with no row context every row is current: all but the last" (fun () ->
                returns (p ()) "EVALUATE OFFSET ( -1, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" [
                    [ "Bell" ]
                    [ "Bike" ]
                    [ "Helmet" ]
                    [ "Lock" ]
                ])
            // SQLBI, "Introducing window functions in DAX": OFFSET ( -1, ALL ( 'Date'[Year] ), ... ) in
            // a measure grouped by year gives the previous year; the first has none.
            testCase "the current row from the filter context" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( P[Category], \"Prev\", CALCULATE ( [Total Price], OFFSET ( -1, ALL ( P[Category] ), ORDERBY ( P[Category] ) ) ) )"
                    // Cycling has no previous category: its value is BLANK, and SUMMARIZECOLUMNS
                    // leaves out a row whose every value is BLANK.
                    [ [ "Safety"; 135 ] ])
            testCase "OFFSET 0 is the current row" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"same\", MAXX ( OFFSET ( 0, P, ORDERBY ( P[Price] ) ), P[Name] ) )"
                    [
                        [ "Bike"; "Bike" ]
                        [ "Helmet"; "Helmet" ]
                        [ "Lock"; "Lock" ]
                        [ "Pump"; "Pump" ]
                        [ "Bell"; "Bell" ]
                    ])
            // The delta "can be any DAX expression that returns a scalar value".
            testCase "the delta is an expression" (fun () ->
                is
                    (p ())
                    "CALCULATE ( MAXX ( OFFSET ( 1 + 1, ALL ( P[Name] ), ORDERBY ( P[Name] ) ), P[Name] ), P[Name] = \"Bell\" )"
                    "Helmet")
            testCase "a BLANK or fractional delta is refused" (fun () ->
                refused "EVALUATE OFFSET ( BLANK (), ALL ( P[Name] ), ORDERBY ( P[Name] ) )" "BLANK"
                refused "EVALUATE OFFSET ( 0.5, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" "whole number")
            // "An empty table is returned if: The corresponding outer value of an OrderBy or
            // PartitionBy column does not exist within relation."
            testCase "a current value that isn't in the relation gives no row" (fun () ->
                is
                    (p ())
                    "CALCULATE ( COUNTROWS ( OFFSET ( 1, FILTER ( ALL ( P[Name] ), P[Name] <> \"Bike\" ), ORDERBY ( P[Name] ) ) ), P[Name] = \"Bike\" )"
                    null)
        ]

        testList "MATCHBY and duplicate rows" [
            // Learn, "Understanding ORDERBY, PARTITIONBY, and MATCHBY", the FactInternetSales example:
            // the expression without MATCHBY "returns an error: \"OFFSET's Relation parameter may have
            // duplicate rows, which isn't allowed.\"", because the table "doesn't contain any
            // explicit keys or unique columns"; with MATCHBY on the order and line it works.
            testCase "a model table without a key whose ORDERBY ties is an error" (fun () ->
                fails
                    "EVALUATE ADDCOLUMNS ( Tied, \"Prev\", SELECTCOLUMNS ( OFFSET ( -1, Tied, ORDERBY ( Tied[Amount], DESC ), PARTITIONBY ( Tied[Product] ) ), \"a\", Tied[Amount] ) )"
                    "may have duplicate rows")
            testCase "MATCHBY identifies the current row" (fun () ->
                // Product 1 by amount descending: 12 (SO2 1), 10 (SO1 1), 8 (SO3 1); product 2: 10
                // (SO2 2), 7 (SO1 2).
                returns
                    (p ())
                    "EVALUATE ADDCOLUMNS ( Lines, \"Prev\", SELECTCOLUMNS ( OFFSET ( -1, Lines, ORDERBY ( Lines[Amount], DESC ), PARTITIONBY ( Lines[Product] ), MATCHBY ( Lines[Order], Lines[Line] ) ), \"a\", Lines[Amount] ) )"
                    [
                        [ "SO1"; 1; 1; 10; 12 ]
                        [ "SO1"; 2; 2; 7; 10 ]
                        [ "SO2"; 1; 1; 12; null ]
                        [ "SO2"; 2; 2; 10; null ]
                        [ "SO3"; 1; 1; 8; 10 ]
                    ])
            // "If MATCHBY is specified, the columns in MATCHBY and PARTITIONBY are used to uniquely
            // identify each row. If that's not possible, an error is returned."
            testCase "MATCHBY columns that don't identify the rows are an error" (fun () ->
                fails
                    "EVALUATE ADDCOLUMNS ( Lines, \"Prev\", SELECTCOLUMNS ( OFFSET ( -1, Lines, ORDERBY ( Lines[Amount] ), PARTITIONBY ( Lines[Product] ), MATCHBY ( Lines[Line] ) ), \"a\", Lines[Amount] ) )"
                    "MATCHBY")
            testCase "a key-less model table whose ORDERBY happens not to tie is refused" (fun () ->
                refused
                    "EVALUATE ADDCOLUMNS ( Lines, \"Prev\", SELECTCOLUMNS ( OFFSET ( -1, Lines, ORDERBY ( Lines[Amount] ), PARTITIONBY ( Lines[Product] ) ), \"a\", Lines[Amount] ) )"
                    "key")
            // Rows equal in every column can't be told apart by any columns: "If such columns cannot
            // be found, an error is returned."
            testCase "duplicate rows of a table expression are an error" (fun () ->
                fails
                    "EVALUATE OFFSET ( 1, UNION ( VALUES ( P[Color] ), VALUES ( P[Color] ) ), ORDERBY ( P[Color] ) )"
                    "may have duplicate rows")
            // "If all of relation's columns were added by DAX table functions, an error is returned."
            testCase "a relation with no model column is an error for OFFSET" (fun () ->
                fails "EVALUATE OFFSET ( 1, { 1, 2, 3 }, ORDERBY ( [Value] ) )" "added by a table function")
        ]

        testList "WINDOW" [
            // Learn, WINDOW, example 2: WINDOW ( 1, ABS, 0, REL, ... ) in SUMX is a running sum.
            testCase "1 ABS to 0 REL is a running total" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"k\", P[Key], \"rt\", SUMX ( WINDOW ( 1, ABS, 0, REL, P, ORDERBY ( P[Key] ) ), P[Price] ) )"
                    [ [ 1; 100 ]; [ 2; 120 ]; [ 3; 135 ]; [ 4; 165 ]; [ 5; 170 ] ])
            // "If from_type is REL, the number of rows to go back (negative value) or forward";
            // "Default is REL"; the window is clipped to the partition.
            testCase "relative bounds, REL by default, clipped" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"k\", P[Key], \"w\", SUMX ( WINDOW ( -1, 1, P, ORDERBY ( P[Key] ) ), P[Price] ) )"
                    [ [ 1; 120 ]; [ 2; 135 ]; [ 3; 65 ]; [ 4; 50 ]; [ 5; 35 ] ])
            // "0 and 1 mean the window starts from the beginning of the partition"; "-1 means the
            // last row in the partition".
            testCase "absolute bounds: 0 is 1, negatives from the end" (fun () ->
                returns (p ()) "EVALUATE WINDOW ( 0, ABS, -2, ABS, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" [
                    [ "Bell" ]
                    [ "Bike" ]
                    [ "Helmet" ]
                    [ "Lock" ]
                ])
            // "An empty table is returned if ... the beginning of the window is after its end."
            testCase "a window that ends before it starts is empty" (fun () ->
                returns (p ()) "EVALUATE WINDOW ( 3, ABS, 2, ABS, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" [])
            testCase "a window inside a partition" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"ytd\", SUMX ( WINDOW ( 1, ABS, 0, REL, P, ORDERBY ( P[Price] ), PARTITIONBY ( P[Category] ) ), P[Price] ) )"
                    [
                        [ "Bike"; 135 ]
                        [ "Helmet"; 35 ]
                        [ "Lock"; 15 ]
                        [ "Pump"; 35 ]
                        [ "Bell"; 5 ]
                    ])
            // SQLBI's pseudo-DAX of apply semantics takes the DISTINCT of the results: overlapping
            // windows give each row once.
            testCase "the windows of several current rows are put together without repeats" (fun () ->
                is (p ()) "COUNTROWS ( WINDOW ( -1, REL, 0, REL, ALL ( P[Name] ), ORDERBY ( P[Name] ) ) )" 5)
        ]

        testList "RANK" [
            // "If specified, the supported value is either DENSE, or SKIP. If omitted: - Defaults to
            // SKIP" (Learn, RANK).
            testCase "SKIP, DENSE and the default" (fun () ->
                // Colors ascending: Black (Lock, Bell), Blue (Pump), Red (Bike, Helmet).
                let ranks (ties: string) =
                    sortedRows
                        (p ())
                        $"EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"r\", RANK ( {ties}P, ORDERBY ( P[Color] ) ) )"

                Expect.equal
                    (ranks "DENSE, ")
                    (expectedSorted [ [ "Bike"; 3 ]; [ "Helmet"; 3 ]; [ "Lock"; 1 ]; [ "Pump"; 2 ]; [ "Bell"; 1 ] ])
                    "DENSE"

                let skip =
                    expectedSorted [ [ "Bike"; 4 ]; [ "Helmet"; 4 ]; [ "Lock"; 1 ]; [ "Pump"; 3 ]; [ "Bell"; 1 ] ]

                Expect.equal (ranks "SKIP, ") skip "SKIP"
                Expect.equal (ranks ", ") skip "omitted"
                Expect.equal (ranks "") skip "left out")
            // "BLANKS FIRST. Blanks are always ordered on the beginning, regardless of ascending or
            // descending sorting order. BLANKS LAST. Blanks are always ordered on the end" (Learn,
            // ORDERBY); the function's blanks argument applies to keys without their own.
            testCase "BLANKS FIRST and LAST" (fun () ->
                let ranks (order: string) (blanks: string) =
                    sortedRows
                        (p ())
                        $"EVALUATE SELECTCOLUMNS ( S, \"n\", S[Name], \"r\", RANK ( SKIP, S, ORDERBY ( S[Score]{order} ){blanks} ) )"

                Expect.equal
                    (ranks ", ASC BLANKS LAST" "")
                    (expectedSorted [ [ "A"; 2 ]; [ "B"; 3 ]; [ "C"; 1 ] ])
                    "ASC LAST"

                Expect.equal
                    (ranks ", DESC BLANKS FIRST" "")
                    (expectedSorted [ [ "A"; 2 ]; [ "B"; 1 ]; [ "C"; 3 ] ])
                    "DESC FIRST"

                Expect.equal
                    (ranks ", DESC BLANKS LAST" "")
                    (expectedSorted [ [ "A"; 1 ]; [ "B"; 3 ]; [ "C"; 2 ] ])
                    "DESC LAST"

                Expect.equal (ranks "" ", LAST") (expectedSorted [ [ "A"; 2 ]; [ "B"; 3 ]; [ "C"; 1 ] ]) "parent LAST"

                Expect.equal
                    (ranks ", ASC BLANKS FIRST" ", LAST")
                    (expectedSorted [ [ "A"; 3 ]; [ "B"; 1 ]; [ "C"; 2 ] ])
                    "the key's own wins")
            // "DEFAULT ... the behavior for numerical values is blank values are ordered between zero
            // and negative values": next to positive numbers only, BLANK is first ascending.
            testCase "BLANK next to positive numbers by default" (fun () ->
                Expect.equal
                    (sortedRows
                        (p ())
                        "EVALUATE SELECTCOLUMNS ( S, \"n\", S[Name], \"r\", RANK ( SKIP, S, ORDERBY ( S[Score] ) ) )")
                    (expectedSorted [ [ "A"; 3 ]; [ "B"; 1 ]; [ "C"; 2 ] ])
                    "ASC")
            // "RANK returns a blank value for total rows" (Learn); SQLBI: "when there is no current
            // row – like at the total level – RANK shows a blank".
            testCase "one current row gives its rank, several give BLANK" (fun () ->
                is (p ()) "CALCULATE ( RANK ( DENSE, ALL ( P[Name] ), ORDERBY ( P[Name] ) ), P[Name] = \"Lock\" )" 4
                is (p ()) "RANK ( DENSE, ALL ( P[Name] ), ORDERBY ( P[Name] ) )" null)
            // SQLBI's RANK measure: ORDERBY ( [Rounded Sales], DESC, 'Product'[Brand], ASC ) over
            // ALLSELECTED ( 'Product'[Brand] ), a measure as a key.
            testCase "a measure as a key" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( P[Name], \"r\", RANK ( DENSE, ALL ( P[Name] ), ORDERBY ( [Total Price], DESC, P[Name], ASC ) ) )"
                    [ [ "Bike"; 1 ]; [ "Pump"; 2 ]; [ "Helmet"; 3 ]; [ "Lock"; 4 ]; [ "Bell"; 5 ] ])
            testCase "RANK is a whole number" (fun () ->
                Expect.equal
                    ((query (p ()) "EVALUATE ROW ( \"r\", RANK ( DENSE, ALL ( P[Name] ), ORDERBY ( P[Name] ) ) )")
                        .Columns
                     |> List.map (fun c -> QueryType.token c.Type))
                    [ "int64" ]
                    "type")
        ]

        testList "ROWNUMBER" [
            // Learn, ROWNUMBER: "Returns the unique ranking for the current context within the
            // specified partition, sorted by the specified order".
            testCase "numbers the rows within each partition" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( P, \"n\", P[Name], \"r\", ROWNUMBER ( P, ORDERBY ( P[Price], DESC ), PARTITIONBY ( P[Category] ) ) )"
                    [ [ "Bike"; 1 ]; [ "Helmet"; 1 ]; [ "Lock"; 2 ]; [ "Pump"; 2 ]; [ "Bell"; 3 ] ])
            // "If a match cannot be found then rownumber is blank."
            testCase "no current row is BLANK" (fun () ->
                is
                    (p ())
                    "CALCULATE ( ROWNUMBER ( FILTER ( ALL ( P[Name] ), P[Name] <> \"Bike\" ), ORDERBY ( P[Name] ) ), P[Name] = \"Bike\" )"
                    null)
            testCase "a tie at the current row is refused" (fun () ->
                refused "EVALUATE SELECTCOLUMNS ( P, \"r\", ROWNUMBER ( P, ORDERBY ( P[Color] ) ) )" "tie")
        ]

        testList "what is refused" [
            // "If omitted: ... Defaults to ALLSELECTED() of all columns in orderBy and partitionBy."
            testCase "an omitted relation names ALLSELECTED" (fun () ->
                refused "EVALUATE INDEX ( 1, ORDERBY ( P[Name] ) )" "ALLSELECTED"
                refused "EVALUATE OFFSET ( 1, , ORDERBY ( P[Name] ) )" "ALLSELECTED"
                refused "EVALUATE ROW ( \"r\", RANK ( DENSE, ORDERBY ( P[Name] ) ) )" "ALLSELECTED")
            // "reset can be used in visual calculations only".
            testCase "reset and axes" (fun () ->
                refused "EVALUATE INDEX ( 1, ALL ( P[Name] ), ORDERBY ( P[Name] ), , , , HIGHESTPARENT )" "visual"
                refused "EVALUATE INDEX ( 1, ROWS )" "visual")
            // Learn: "If there is more than one corresponding outer column, an error is returned";
            // SQLBI: "the engine uses the innermost row context".
            testCase "nested row contexts over the same column" (fun () ->
                refused
                    "EVALUATE SELECTCOLUMNS ( P, \"x\", SUMX ( P, COUNTROWS ( OFFSET ( 1, P, ORDERBY ( P[Key] ) ) ) ) )"
                    "nested row contexts")
            // "no guarantee is given as to what order in which they'll be added" (Learn).
            testCase "an omitted ORDERBY over several columns, where the order matters" (fun () ->
                // By Color then Key: Black 3; by Key then Color: 1 Red.
                refused "EVALUATE INDEX ( 1, ALL ( P[Color], P[Key] ) )" "without ORDERBY")
            testCase "an omitted ORDERBY over several columns, where it doesn't" (fun () ->
                // Either order of (Color, Category) puts Black, Cycling first.
                returns (p ()) "EVALUATE INDEX ( 1, ALL ( P[Color], P[Category] ) )" [ [ "Black"; "Cycling" ] ])
            testCase "BLANK next to zero or negative numbers" (fun () ->
                refused "EVALUATE INDEX ( 1, { 0, BLANK (), 1 }, ORDERBY ( [Value] ) )" "BLANK")
            testCase "ORDERBY outside a window function is an error" (fun () ->
                Expect.equal (code "EVALUATE ROW ( \"x\", ORDERBY ( P[Name] ) )") "dax-invalid" "ORDERBY"
                Expect.equal (code "EVALUATE PARTITIONBY ( P[Name] )") "dax-invalid" "PARTITIONBY")
            testCase "an ORDERBY column that isn't the relation's" (fun () ->
                refused "EVALUATE INDEX ( 1, ALL ( P[Name] ), ORDERBY ( P[Price] ) )" "not a column of the relation")
            testCase "coverage reports the refusals the arguments show" (fun () ->
                let db, _ =
                    build {
                        model with
                            Measures = [
                                "P", "First", "COUNTROWS ( INDEX ( 1, ORDERBY ( P[Name] ) ) )"
                                "P", "Fine", "COUNTROWS ( INDEX ( 1, ALL ( P[Name] ), ORDERBY ( P[Name] ) ) )"
                            ]
                    }

                let coverage =
                    Engine.coverage db
                    |> List.map (fun (r, u) -> ObjectRef.format r, u)
                    |> Map.ofList

                Expect.stringContains (String.concat "; " coverage.["measure:P/First"]) "ALLSELECTED" "refused"
                Expect.equal coverage.["measure:P/Fine"] [] "supported")
        ]

        testList "properties" [
            testPropertyWithConfig config "ROWNUMBER is 1..n in each partition, in key order"
            <| fun (xs: int list) ->
                let data = distinctRows xs

                if data.IsEmpty then
                    true
                else
                    let p = generated data

                    let got =
                        ints
                            p
                            "EVALUATE SELECTCOLUMNS ( T, \"k\", T[K], \"g\", T[G], \"n\", ROWNUMBER ( T, ORDERBY ( T[K] ), PARTITIONBY ( T[G] ) ) )"

                    got
                    |> List.groupBy (fun (_, g, _) -> g)
                    |> List.forall (fun (_, members) ->
                        let byKey = members |> List.sortBy (fun (k, _, _) -> k)
                        byKey |> List.map (fun (_, _, n) -> n) = [ 1L .. int64 byKey.Length ])
            testPropertyWithConfig config "INDEX 1 and -1 are the ends of the order"
            <| fun (xs: int list) ->
                let data = distinctRows xs

                if data.IsEmpty then
                    true
                else
                    let p = generated data
                    let keys = data |> List.map fst

                    let one (position: int) (direction: string) =
                        match
                            rows
                                p
                                $"EVALUATE SELECTCOLUMNS ( INDEX ( {position}, T, ORDERBY ( T[K], {direction} ) ), \"k\", T[K] )"
                        with
                        | [ [ QueryValue.Int64 k ] ] -> int k
                        | other -> failtestf "%A" other

                    one 1 "ASC" = List.min keys
                    && one -1 "ASC" = List.max keys
                    && one 1 "DESC" = List.max keys
                    && one -1 "DESC" = List.min keys
            testPropertyWithConfig config "OFFSET 0 is the current row, OFFSET 1 the next key"
            <| fun (xs: int list) ->
                let data = distinctRows xs

                if data.IsEmpty then
                    true
                else
                    let p = generated data

                    let got =
                        ints
                            p
                            "EVALUATE SELECTCOLUMNS ( T, \"k\", T[K], \"g\", T[G], \"n\", MAXX ( OFFSET ( 0, T, ORDERBY ( T[K] ) ), T[K] ) )"

                    let next =
                        ints
                            p
                            "EVALUATE SELECTCOLUMNS ( T, \"k\", T[K], \"g\", T[G], \"n\", MAXX ( OFFSET ( 1, T, ORDERBY ( T[K] ) ), T[K] ) )"

                    let keys = data |> List.map fst |> List.sort

                    let expectedNext (k: int64) =
                        match keys |> List.tryFind (fun x -> int64 x > k) with
                        | Some x -> int64 x
                        | None -> Int64.MinValue

                    got |> List.forall (fun (k, _, n) -> n = k)
                    && next |> List.forall (fun (k, _, n) -> n = expectedNext k)
            testPropertyWithConfig config "RANK SKIP is 1 + the rows before, DENSE 1 + the distinct values before"
            <| fun (xs: int list) ->
                let data = xs |> List.truncate 12 |> List.mapi (fun i x -> i, ((x % 4) + 4) % 4)

                if data.IsEmpty then
                    true
                else
                    // K is the row's own number (unique); G a value with ties, ranked on.
                    let p = generated data

                    let ranks (ties: string) =
                        ints
                            p
                            $"EVALUATE SELECTCOLUMNS ( T, \"k\", T[K], \"g\", T[G], \"r\", RANK ( {ties}, T, ORDERBY ( T[G] ) ) )"

                    let gs = data |> List.map snd

                    ranks "SKIP"
                    |> List.forall (fun (_, g, r) -> r = 1L + int64 (gs |> List.filter (fun x -> int64 x < g)).Length)
                    && ranks "DENSE"
                       |> List.forall (fun (_, g, r) ->
                           r = 1L + int64 (gs |> List.filter (fun x -> int64 x < g) |> List.distinct).Length)
        ]
    ]
