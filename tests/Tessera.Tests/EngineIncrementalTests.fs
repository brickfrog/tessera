/// Incremental filter contexts ("Incremental filter contexts" in the author's guide, Engine/Types.fs):
/// a context made from another works out a table's rows from the rows under the filters it kept,
/// intersected with what the added filters allow. Each test runs a query where that applies and
/// the same query on a model where it must fall back (a relationship that filters both ways), in
/// check mode (every incremental result recomputed in full and compared), against answers worked
/// out by hand, and against the engine with incremental contexts off.
///
/// The model is `star` (EngineHelpers.fs): Amount per Sales row 200, 100, 60, 15, 40, 100, 110
/// (total 625); Ann has rows 1, 3, 7 (370), Bob 2, 5 (140), Cy 4, 6 (115); Bike is rows 1, 2, 7,
/// Helmet 3, 6, Lock 4; row 5's product doesn't exist (the blank row of Product); Red is Bike and
/// Helmet, Black is Lock, Blue (Pump) has no sales.
module Tessera.Tests.EngineIncrementalTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// `star` with Sales -> Customer filtering both ways: no table whose rows depend on Customer's is
/// worked out incrementally there.
let private bothWaysStar = {
    star with
        Rels =
            star.Rels
            |> List.map (fun r -> if r.From = ("Sales", "CustomerKey") then bothWays r else r)
}

/// How many results for `table` were worked out incrementally.
let private appliedTo (p: Engine.Prepared) (table: string) : int64 =
    match (Engine.incrementalStats p).AppliedTo.TryGetValue table with
    | true, n -> n
    | _ -> 0L

/// The query returns `expected` in check mode, with no difference from the full computation, and
/// the same with incremental contexts off. Returns how many results for `table` were incremental.
let private checkedRows (p: Engine.Prepared) (table: string) (dax: string) (expected: obj list list) : int64 =
    let stats = Engine.incrementalStats p
    stats.Check <- true
    let before = appliedTo p table
    returnsInOrder p dax expected
    Expect.equal stats.Mismatched 0L "no incremental result differs from the full computation"
    let applied = appliedTo p table - before
    stats.Enabled <- false

    try
        returnsInOrder p dax expected
    finally
        stats.Enabled <- true

    applied

/// The query applies incrementally to `table` on `star` and falls back on `bothWaysStar`.
let private pair (table: string) (dax: string) (onStar: obj list list) (onBothWays: obj list list) =
    let applied = checkedRows (prepare star) table dax onStar
    Expect.isGreaterThan applied 0L $"rows of {table} worked out incrementally on star"
    let fellBack = checkedRows (prepare bothWaysStar) table dax onBothWays
    Expect.equal fellBack 0L $"no rows of {table} worked out incrementally across a relationship that filters both ways"

[<Tests>]
let tests =
    testList "Engine incremental filter contexts" [

        testCase "a filter the iterator's transition replaces" (fun () ->
            // ALL ( Customer ) iterates all three customers under filters on Ann and on Red; each
            // row's transition replaces the one on Ann and keeps Red: Ann 200 + 60 + 110, Bob 100
            // (row 5's product has no color), Cy 100 (row 4 is Black). Starting from the parent's
            // rows (Ann's) instead of the rows under what was kept would give Ann's 370 alone.
            let dax =
                """
                EVALUATE ROW (
                    "all", CALCULATE ( SUMX ( ALL ( Customer ), CALCULATE ( [Amount] ) ), Customer[Name] = "Ann", Product[Color] = "Red" ),
                    "ann", CALCULATE ( [Amount], Customer[Name] = "Ann", Product[Color] = "Red" )
                )
                """

            pair "Sales" dax [ [ 570; 370 ] ] [ [ 570; 370 ] ])

        testCase "a paired filter that loses a column to the transition" (fun () ->
            // The pairs (Ann, Bike) and (Bob, Helmet): rows 1 and 7, 310. Iterating Customer
            // under them, each row's transition replaces the filter on Customer[Name], which
            // leaves Product[Name] in { Bike, Helmet }: Ann 200 + 60 + 110, Bob 100 (row 5's
            // product isn't Bike or Helmet). With Sales -> Customer both ways the customers
            // iterated are only those with a sale in the pairs: Ann, 370.
            let dax =
                """
                DEFINE VAR pairs = TREATAS ( { ( "Ann", "Bike" ), ( "Bob", "Helmet" ) }, Customer[Name], Product[Name] )
                EVALUATE ROW (
                    "paired", CALCULATE ( [Amount], pairs ),
                    "iterated", CALCULATE ( SUMX ( Customer, CALCULATE ( [Amount] ) ), pairs )
                )
                """

            pair "Sales" dax [ [ 310; 470 ] ] [ [ 310; 370 ] ])

        testCase "KEEPFILTERS against a filter the transition keeps" (fun () ->
            // Per color, under a filter on Ann: KEEPFILTERS ( Bob ) intersects, which leaves no
            // customer; without it, Bob replaces Ann: his Red 100 and his BLANK-color 40.
            let dax =
                """
                EVALUATE ROW (
                    "keep", CALCULATE ( SUMX ( VALUES ( Product[Color] ), CALCULATE ( [Amount], KEEPFILTERS ( Customer[Name] = "Bob" ) ) ), Customer[Name] = "Ann" ),
                    "replace", CALCULATE ( SUMX ( VALUES ( Product[Color] ), CALCULATE ( [Amount], Customer[Name] = "Bob" ) ), Customer[Name] = "Ann" )
                )
                """

            pair "Sales" dax [ [ null; 140 ] ] [ [ null; 140 ] ])

        testCase "a table reached over a relationship that filters both ways" (fun () ->
            // Per color, the US customers (Ann, Cy). One way, Customer isn't filtered by Product:
            // 2 for each of the four colors (Red, Black, Blue and the blank row's BLANK). Both
            // ways, those with a sale of the color: Red 2, Black 1 (Cy), Blue none, BLANK none
            // (row 5 is Bob's): the rows of Customer are the image of Sales' rows, which an
            // intersection doesn't distribute over.
            let dax =
                "EVALUATE ROW ( \"v\", CALCULATE ( SUMX ( VALUES ( Product[Color] ), CALCULATE ( COUNTROWS ( Customer ) ) ), Customer[Country] = \"US\" ) )"

            pair "Customer" dax [ [ 8 ] ] [ [ 3 ] ])

        testCase "a role" (fun () ->
            // Under the role UK the model has Bob's rows only: his 140, whichever way the
            // relationship filters. Without the role, all 625.
            let withRole (m: TestModel) =
                let db, store = build m
                let r = new ModelRole(Name = "UK", ModelPermission = ModelPermission.Read)

                r.TablePermissions.Add(
                    new TablePermission(
                        Table = db.Model.Tables.["Customer"],
                        FilterExpression = "Customer[Country] = \"UK\""
                    )
                )

                db.Model.Roles.Add r

                let p =
                    match Engine.prepare db store with
                    | Ok p -> p
                    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

                match Engine.asRoles [ "UK" ] p with
                | Ok uk -> p, uk
                | Error e -> failtestf "asRoles failed: %s" (TessError.message e)

            let dax =
                "EVALUATE ROW ( \"v\", SUMX ( ALL ( Customer ), CALCULATE ( [Amount] ) ), \"red\", CALCULATE ( SUMX ( ALL ( Customer ), CALCULATE ( [Amount] ) ), Product[Color] = \"Red\" ) )"

            let plain, uk = withRole star
            Expect.isGreaterThan (checkedRows plain "Sales" dax [ [ 625; 570 ] ]) 0L "incremental without the role"
            Expect.isGreaterThan (checkedRows uk "Sales" dax [ [ 140; 100 ] ]) 0L "incremental under the role"

            let _, ukBoth = withRole bothWaysStar
            Expect.equal (checkedRows ukBoth "Sales" dax [ [ 140; 100 ] ]) 0L "falls back under the role, both ways")

        testCase "nothing incremental while switched off" (fun () ->
            let p = prepare star
            let stats = Engine.incrementalStats p
            stats.Enabled <- false

            returnsInOrder p "EVALUATE ROW ( \"v\", SUMX ( ALL ( Customer ), CALCULATE ( [Amount] ) ) )" [ [ 625 ] ]

            Expect.equal stats.Applied 0L "switched off")
    ]
