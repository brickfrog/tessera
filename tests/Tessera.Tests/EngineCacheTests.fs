/// The engine's caches that outlive one filter context (Engine/Caching.fs): each test evaluates the
/// same expression under two contexts that differ in only one of the things a cached result
/// depends on, so a cache keyed on too little would give one context's answer to the other.
///
/// The model is `star` (EngineHelpers.fs): Amount per Sales row 200, 100, 60, 15, 40, 100, 110
/// (total 625); row 6 is the one order of 2023 (shipped 2024-01-01); rows 1, 3, 7 are Ann's,
/// 2, 5 Bob's, 4, 6 Cy's; Red products (1, 2) have rows 1, 2, 3, 6, 7, Black (3) row 4.
module Tessera.Tests.EngineCacheTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private p = starPrepared

/// `star` changed in TOM before it is prepared.
let private prepareWith (change: Database -> unit) : Engine.Prepared =
    let db, store = build star
    change db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private one (p: Engine.Prepared) (dax: string) : QueryValue list =
    match rows p dax with
    | [ row ] -> row
    | other -> failtestf "expected one row, got %A\n%s" other dax

[<Tests>]
let tests =
    testList "Engine caches" [

        testCase "the same filters with different scope marks: ISINSCOPE through a measure" (fun () ->
            // A group of SUMMARIZECOLUMNS is in scope; a filter argument of CALCULATE on the same
            // column, with the same value, is not (dax.guide/isinscope). [Outer] only names [In],
            // so the check that keeps such measures out of the cache must follow references.
            let dax =
                """
                DEFINE
                    MEASURE Sales[In] = IF ( ISINSCOPE ( Product[Color] ), "yes", "no" )
                    MEASURE Sales[Outer] = [In]
                EVALUATE
                    ADDCOLUMNS (
                        SUMMARIZECOLUMNS ( Product[Color], "grouped", [Outer] ),
                        "filtered", CALCULATE ( [Outer], REMOVEFILTERS ( Product ), Product[Color] = "Red" )
                    )
                """

            // The BLANK group is the blank row of Product (sale 5's product 9 doesn't exist).
            returns p.Value dax [
                [ "Red"; "yes"; "no" ]
                [ "Black"; "yes"; "no" ]
                [ "Blue"; "yes"; "no" ]
                [ null; "yes"; "no" ]
            ])

        testCase "the same filters with different scope marks: ISINSCOPE through a function" (fun () ->
            let dax =
                """
                DEFINE
                    FUNCTION InScope = ( ) => IF ( ISINSCOPE ( Product[Color] ), "yes", "no" )
                    MEASURE Sales[Via] = InScope ( )
                EVALUATE
                    ADDCOLUMNS (
                        SUMMARIZECOLUMNS ( Product[Color], "grouped", [Via] ),
                        "filtered", CALCULATE ( [Via], REMOVEFILTERS ( Product ), Product[Color] = "Red" )
                    )
                """

            // The BLANK group is the blank row of Product (sale 5's product 9 doesn't exist).
            returns p.Value dax [
                [ "Red"; "yes"; "no" ]
                [ "Black"; "yes"; "no" ]
                [ "Blue"; "yes"; "no" ]
                [ null; "yes"; "no" ]
            ])

        testCase "the same filters under different relationships: USERELATIONSHIP" (fun () ->
            // By order date, 2024 is every row but row 6 (525); by ship date it is all of them.
            let dax =
                """
                EVALUATE ROW (
                    "order", CALCULATE ( [Amount], 'Date'[Year] = 2024 ),
                    "ship", CALCULATE ( [Amount], 'Date'[Year] = 2024, USERELATIONSHIP ( Sales[ShipDate], 'Date'[Date] ) ),
                    "order again", CALCULATE ( [Amount], 'Date'[Year] = 2024 )
                )
                """

            Expect.equal (one p.Value dax) [ qv 525; qv 625; qv 525 ] dax)

        testCase "the same predicate with different variable values" (fun () ->
            // The True/False filter reads only its column and a variable: its filter is shared
            // between evaluations where the variable has the same value, and only those.
            let dax =
                """
                DEFINE
                    MEASURE Sales[Same Color] =
                        VAR c = SELECTEDVALUE ( Product[Color] )
                        RETURN CALCULATE ( COUNTROWS ( Sales ), REMOVEFILTERS ( Product ), Product[Color] = c )
                EVALUATE SUMMARIZECOLUMNS ( Product[Color], "n", [Same Color] )
                """

            // BLANK: c is BLANK, and Product[Color] = BLANK keeps the blank row's color (sale 5).
            returns p.Value dax [ [ "Red"; 5 ]; [ "Black"; 1 ]; [ null; 1 ] ])

        testCase "a cached value never hides a circular dependency" (fun () ->
            // [Z] alone is [X], which is 1. Under a Red filter [X] evaluates [Y], which evaluates
            // [Z] with the filter removed: the context [Z] was first evaluated in, but this time
            // inside [X], so [Z] reaching [X] is the circular dependency.
            let dax =
                """
                DEFINE
                    MEASURE Sales[X] = IF ( SELECTEDVALUE ( Product[Color] ) = "Red", [Y], 1 )
                    MEASURE Sales[Y] = CALCULATE ( [Z], REMOVEFILTERS ( Product ) )
                    MEASURE Sales[Z] = [X] + 0
                EVALUATE ROW ( "a", [Z], "b", CALCULATE ( [X], Product[Color] = "Red" ) )
                """

            match queryError p.Value dax with
            | DaxInvalid(what, _) -> Expect.stringContains what "circular" dax
            | other -> failtestf "expected a circular dependency, got %A" other)

        testCase "two queries that define a measure differently" (fun () ->
            let q (n: int) =
                $"DEFINE MEASURE Sales[A] = {n} MEASURE Sales[B] = [A] + COUNTROWS ( Sales ) EVALUATE ROW ( \"b\", [B] )"

            Expect.equal (one p.Value (q 1)) [ qv 8 ] "A = 1"
            Expect.equal (one p.Value (q 2)) [ qv 9 ] "A = 2"
            Expect.equal (one p.Value (q 1)) [ qv 8 ] "A = 1 again")

        testCase "two identities on one prepared model" (fun () ->
            let dax =
                "EVALUATE ROW ( \"n\", CALCULATE ( COUNTROWS ( Sales ), Customer[Name] = USERNAME ( ) ) )"

            let who (name: string) =
                Engine.withIdentity
                    {
                        DaxEngine.Identity.none with
                            UserName = Some name
                    }
                    p.Value

            Expect.equal (one (who "Ann") dax) [ qv 3 ] "Ann"
            Expect.equal (one (who "Bob") dax) [ qv 2 ] "Bob"
            Expect.equal (one (who "Ann") dax) [ qv 3 ] "Ann again")

        testCase "a role and no role" (fun () ->
            let secured =
                prepareWith (fun db ->
                    let r = new ModelRole(Name = "UK", ModelPermission = ModelPermission.Read)

                    r.TablePermissions.Add(
                        new TablePermission(
                            Table = db.Model.Tables.["Customer"],
                            FilterExpression = "Customer[Country] = \"UK\""
                        )
                    )

                    db.Model.Roles.Add r)

            let dax =
                "EVALUATE ROW ( \"a\", [Amount], \"red\", CALCULATE ( [Amount], Product[Color] = \"Red\" ) )"

            // UK: Bob, rows 2 (Red, 100) and 5 (40).
            Expect.equal (one secured dax) [ qv 625; qv 570 ] "no role"

            match Engine.asRoles [ "UK" ] secured with
            | Ok uk -> Expect.equal (one uk dax) [ qv 140; qv 100 ] "role UK"
            | Error e -> failtestf "asRoles failed: %s" (TessError.message e)

            Expect.equal (one secured dax) [ qv 625; qv 570 ] "no role again")

        testCase "a calculation item applied and not" (fun () ->
            let withGroup =
                prepareWith (fun db ->
                    let t =
                        new Table(Name = "Scale", CalculationGroup = new CalculationGroup(Precedence = 10))

                    t.Columns.Add(new DataColumn(Name = "Factor", DataType = DataType.String, SourceColumn = "Name"))

                    t.Columns.Add(
                        new DataColumn(Name = "Ordinal", DataType = DataType.Int64, SourceColumn = "Ordinal")
                    )

                    t.CalculationGroup.CalculationItems.Add(
                        new CalculationItem(Name = "Double", Expression = "SELECTEDMEASURE () * 2", Ordinal = 0)
                    )

                    t.Partitions.Add(new Partition(Name = "Scale", Source = new CalculationGroupSource()))
                    db.Model.Tables.Add t
                    db.Model.DiscourageImplicitMeasures <- true)

            let dax =
                """
                EVALUATE ROW (
                    "plain", [Amount],
                    "double", CALCULATE ( [Amount], Scale[Factor] = "Double" ),
                    "plain again", [Amount]
                )
                """

            Expect.equal (one withGroup dax) [ qv 625; qv 1250; qv 625 ] dax)

        testCase "a table's rows under filters that reach it over a relationship that filters both ways" (fun () ->
            // With Sales -> Customer filtering both ways, a filter on Product reaches Customer
            // through Sales: Black is row 4 (Cy), Red rows 1, 2, 3, 6, 7 (all three customers).
            let both =
                prepare {
                    star with
                        Rels =
                            star.Rels
                            |> List.map (fun r -> if r.From = ("Sales", "CustomerKey") then bothWays r else r)
                }

            let dax =
                """
                EVALUATE ROW (
                    "all", COUNTROWS ( Customer ),
                    "black", CALCULATE ( COUNTROWS ( Customer ), Product[Color] = "Black" ),
                    "red", CALCULATE ( COUNTROWS ( Customer ), Product[Color] = "Red" ),
                    "black again", CALCULATE ( COUNTROWS ( Customer ), Product[Color] = "Black" )
                )
                """

            Expect.equal (one both dax) [ qv 3; qv 1; qv 3; qv 1 ] dax)

        testCase "an iteration's transitions: the same measure for every row of an outer iteration" (fun () ->
            // The shape the caches are for (a measure per row of an iteration, under ALLSELECTED,
            // per group): [Share] is each color's Amount over the largest color's. Red 570,
            // Black 15, Blue BLANK, and the BLANK color of the blank row 40 (sale 5).
            let dax =
                """
                DEFINE
                    MEASURE Sales[Top] = MAXX ( ALLSELECTED ( Product[Color] ), [Amount] )
                    MEASURE Sales[Share] = DIVIDE ( [Amount], [Top] )
                EVALUATE SUMMARIZECOLUMNS ( Product[Color], "top", [Top], "share", [Share] )
                """

            returns p.Value dax [
                [ "Red"; 570; 1.0 ]
                [ "Black"; 570; 15.0 / 570.0 ]
                [ "Blue"; 570; null ]
                [ null; 570; 40.0 / 570.0 ]
            ])
    ]
