/// Evaluation contexts and relationships in the local DAX engine. Each expected value is worked
/// out by hand from the `star` model in EngineHelpers (its doc comment lists every row), with the
/// source of the rule.
///
/// Amount per Sales row (Qty * Price): 1:200  2:100  3:60  4:15  5:40  6:100  7:110, total 625.
///   by product:  Bike 410 (rows 1, 2, 7), Helmet 160 (3, 6), Lock 15 (4), Pump none, blank row 40 (5)
///   by colour:   Red 570, Black 15, Blue none, blank 40
///   by category: Cycling 410, Safety 175, blank 40
///   by customer: Ann 370 (1, 3, 7), Bob 140 (2, 5), Cy 115 (4, 6); US 485, UK 140
///   by order date: 2023-12-31 100 | 2024-01-01 300 | 2024-01-02 60 | 2024-02-01 165
///                  2023 100, 2024 525; Dec 100, Jan 360, Feb 165
///   by ship date:  2024-01-01 100 (6) | 2024-01-02 260 (1, 3) | 2024-02-01 265 (2, 4, 5, 7)
module Tessera.Tests.EngineContextTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private p () = starPrepared.Value

let private isError (code: string) (dax: string) =
    Expect.equal (TessError.code (scalarError (p ()) dax)) code dax

/// Rows that repeat in every column.
let private duplicates =
    lazy
        (prepare {
            Tables = [ table "Dup" [ "V", DataType.Int64 ] [ [ 1 ]; [ 1 ]; [ 2 ] ] ]
            Rels = []
            Measures = []
            Functions = []
        })

/// The star model with Sales <-> Product filtering both ways.
let private bidirectional =
    lazy
        (prepare {
            star with
                Rels = star.Rels |> List.mapi (fun i r -> if i = 0 then bothWays r else r)
        })

[<Tests>]
let tests =
    testList "Engine contexts" [
        testList "CALCULATE" [
            testCase "a filter argument filters the measure" (fun () ->
                is (p ()) "CALCULATE ( [Amount], Product[Color] = \"Red\" )" 570
                is (p ()) "CALCULATE ( [Amount], 'Date'[Year] = 2024 )" 525
                // Red rows are 1, 2, 3, 6, 7; row 6 was ordered in 2023: 200 + 100 + 60 + 110.
                is (p ()) "CALCULATE ( [Amount], Product[Color] = \"Red\", 'Date'[Year] = 2024 )" 470)
            // "when a column gets filtered inside CALCULATE, the new filter overrides any existing
            // filter on the same column" (SQLBI, KEEPFILTERS article).
            testCase "a filter replaces the filter on the same column" (fun () ->
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], Product[Color] = \"Black\" ), Product[Color] = \"Red\" )"
                    15)
            // "If a filter argument is modified by KEEPFILTERS, the filter is added to the filter
            // context without overwriting existing filters" (dax.guide/calculate, step 5).
            testCase "KEEPFILTERS intersects instead" (fun () ->
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], KEEPFILTERS ( Product[Color] = \"Black\" ) ), Product[Color] = \"Red\" )"
                    null

                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], KEEPFILTERS ( Product[Color] IN { \"Black\", \"Red\" } ) ), Product[Color] = \"Red\" )"
                    570)
            // Step 1: "evaluates all the explicit filter arguments in the original evaluation
            // context". The inner filter reads MAX in the outer context (2024-02-01 overall).
            testCase "filter arguments are evaluated in the original context" (fun () ->
                // The latest order date among Black sales is 2024-02-01 (row 4): all sales of that
                // day are rows 4, 5 and 7.
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], Sales[OrderDate] = MAX ( Sales[OrderDate] ), ALL ( Product ) ), Product[Color] = \"Black\" )"
                    165)
            // A predicate over two columns of one table: FILTER over ALL of both (SQLBI,
            // "Specifying multiple filter conditions in CALCULATE"). Qty * Price > 100: rows 1 and 7.
            testCase "a predicate over two columns of one table" (fun () ->
                is (p ()) "CALCULATE ( [Amount], Sales[Qty] * Sales[Price] > 100 )" 310)
            testCase "a predicate over two tables, or with a measure, is an error" (fun () ->
                isError "dax-invalid" "CALCULATE ( [Amount], Product[Color] = \"Red\" && Customer[Country] = \"US\" )"
                isError "dax-invalid" "CALCULATE ( [Amount], Sales[Amount] > 1 )")
            testCase "a table as a filter" (fun () ->
                is (p ()) "CALCULATE ( [Amount], FILTER ( ALL ( Product[Color] ), Product[Color] <> \"Red\" ) )" 55
                is (p ()) "CALCULATE ( [Amount], FILTER ( Customer, Customer[Country] = \"UK\" ) )" 140
                is (p ()) "CALCULATE ( [Amount], { \"Red\" } )" 625)
        ]

        testList "removing filters" [
            testCase "ALL of a column, of a table, and REMOVEFILTERS" (fun () ->
                let under (inner: string) =
                    $"CALCULATE ( CALCULATE ( [Amount], {inner} ), Product[Color] = \"Red\", Product[Category] = \"Safety\" )"

                // Red and Safety is the Helmet: 160.
                is (p ()) "CALCULATE ( [Amount], Product[Color] = \"Red\", Product[Category] = \"Safety\" )" 160
                // Without the colour filter: Safety, 175.
                is (p ()) (under "ALL ( Product[Color] )") 175
                is (p ()) (under "REMOVEFILTERS ( Product[Color] )") 175
                is (p ()) (under "ALL ( Product )") 625
                is (p ()) (under "ALL ( Product[Color], Product[Category] )") 625
                is (p ()) (under "REMOVEFILTERS ()") 625
                is (p ()) (under "ALL ()") 625
                // "removes the filters from the expanded table ... keeping only the filters in the
                // columns specified" (dax.guide/allexcept).
                is (p ()) (under "ALLEXCEPT ( Product, Product[Category] )") 175
                is (p ()) (under "ALLEXCEPT ( Product, Product[Color] )") 570)
            // "ALL ( Sales ) ... also removes filters from the derived columns of Sales, which
            // include Product[Color]. ... you can use ALL including all the columns of Sales, and
            // you will note that the filter on Product[Color] is still in place" (SQLBI,
            // "Filtering tables").
            testCase "ALL of a table removes the filters on its expanded table" (fun () ->
                is (p ()) "CALCULATE ( CALCULATE ( [Amount], ALL ( Sales ) ), Product[Color] = \"Red\" )" 625

                is
                    (p ())
                    "CALCULATE ( CALCULATE ( COUNTROWS ( Product ), ALL ( Sales ) ), Product[Color] = \"Red\" )"
                    4

                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], ALL ( Sales[Id], Sales[ProductKey], Sales[CustomerKey], Sales[OrderDate], Sales[ShipDate], Sales[Qty], Sales[Price] ) ), Product[Color] = \"Red\" )"
                    570)
            // Step 4 comes after step 3: "one can remove the effects of the context transition by
            // using ALL as a filter argument" (dax.guide/calculate).
            testCase "a modifier removes what context transition added" (fun () ->
                // Each of the 4 products sees all 17 units.
                is (p ()) "SUMX ( Product, CALCULATE ( SUM ( Sales[Qty] ), ALL ( Product ) ) )" 68)
            testCase "ALLSELECTED is not supported" (fun () ->
                isError "not-supported" "CALCULATE ( [Amount], ALLSELECTED ( Product ) )")
        ]

        testList "row context and context transition" [
            // "when you reference a table the blank row is never returned" (SQLBI, "Blank row in
            // DAX"): the orphan row's 40 is not reached by iterating Product.
            testCase "a measure in an iteration filters by the current row" (fun () ->
                is (p ()) "SUMX ( Product, [Amount] )" 585
                is (p ()) "SUMX ( VALUES ( Product ), [Amount] )" 625
                is (p ()) "SUMX ( VALUES ( Product[Color] ), [Amount] )" 625)
            testCase "an aggregation without CALCULATE ignores the row context" (fun () ->
                // 4 products, each seeing all 17 units.
                is (p ()) "SUMX ( Product, SUM ( Sales[Qty] ) )" 68
                // Units per product: Bike 2 + 1 + 1, Helmet 3 + 5, Lock 1.
                is (p ()) "SUMX ( Product, CALCULATE ( SUM ( Sales[Qty] ) ) )" 13)
            // "all the columns of the expanded table will be part of the newly-created filter
            // context" (SQLBI, "Understanding context transition in DAX"): a Sales row filters its
            // product. The orphan row 5 relates to the blank row, which a table reference leaves out.
            testCase "context transition on a table covers its expanded table" (fun () ->
                is (p ()) "SUMX ( Sales, CALCULATE ( COUNTROWS ( Product ) ) )" 6
                is (p ()) "SUMX ( Sales, CALCULATE ( COUNTROWS ( VALUES ( Product ) ) ) )" 7
                // Row 1 is Ann's: only her country is visible.
                is
                    (p ())
                    "MAXX ( FILTER ( Sales, Sales[Id] = 1 ), CALCULATE ( SELECTEDVALUE ( Customer[Country] ) ) )"
                    "US")
            // "if you use context transition in a table that contains duplicated rows, the filter
            // context generated by CALCULATE includes all the duplicates" (SQLBI, "Understanding
            // context transition"). Rows 1, 1, 2: each 1 sees both (2), the 2 sees itself.
            testCase "context transition keeps duplicate rows together" (fun () ->
                is duplicates.Value "SUMX ( Dup, CALCULATE ( SUM ( Dup[V] ) ) )" 6
                is duplicates.Value "SUMX ( Dup, CALCULATE ( COUNTROWS ( Dup ) ) )" 5)
            // "If the row context is iterating one column only, then that column will be part of
            // the filter context" (SQLBI, "Understanding context transition in DAX").
            testCase "iterating one column filters that column" (fun () ->
                // Red: 570 and 2 products; Black: 15 and 1; Blue: nothing; the blank colour: 40, but
                // no product row (COUNTROWS of a table leaves the blank row out).
                is (p ()) "SUMX ( VALUES ( Product[Color] ), [Amount] * CALCULATE ( COUNTROWS ( Product ) ) )" 1155)
            // Explicit filters come after the context transition and replace it (SQLBI, "Context
            // transition and expanded tables").
            testCase "a filter argument overrides the context transition" (fun () ->
                // For each of the 3 stored colours, Red's 570.
                is (p ()) "SUMX ( DISTINCT ( Product[Color] ), CALCULATE ( [Amount], Product[Color] = \"Red\" ) )" 1710)
            // Qty: 2 1 3 1 4 5 1. Rows with a larger Qty than the current one: for 2: three; for
            // each 1: four (twelve in all); for 3: two; for 4: one; for 5: none.
            testCase "EARLIER reads the outer row context" (fun () ->
                is (p ()) "SUMX ( Sales, COUNTROWS ( FILTER ( Sales, Sales[Qty] > EARLIER ( Sales[Qty] ) ) ) )" 18
                is (p ()) "SUMX ( Sales, COUNTROWS ( FILTER ( Sales, Sales[Qty] > EARLIEST ( Sales[Qty] ) ) ) )" 18
                isError "dax-invalid" "SUMX ( Sales, EARLIER ( Sales[Qty] ) )")
            testCase "nested iterations each have their row" (fun () ->
                // Every pair of a customer and a product: 3 * 4.
                is (p ()) "SUMX ( Customer, SUMX ( Product, 1 ) )" 12
                // Per customer and product, the amount of both: the sum over pairs is the total
                // with a product row, 585.
                is (p ()) "SUMX ( Customer, SUMX ( Product, [Amount] ) )" 585)
        ]

        testList "variables" [
            // "The expression is evaluated only once" and in the context of its definition
            // (Microsoft Learn, VAR).
            testCase "a variable keeps the value of where it was defined" (fun () ->
                is (p ()) "VAR x = [Amount] RETURN CALCULATE ( x, Product[Color] = \"Red\" )" 625
                is (p ()) "SUMX ( Product, VAR a = [Amount] RETURN a )" 585

                is
                    (p ())
                    "VAR t = FILTER ( Product, Product[Color] = \"Red\" ) RETURN CALCULATE ( COUNTROWS ( t ), Product[Color] = \"Black\" )"
                    2)
            testCase "a variable that is never used is never evaluated" (fun () ->
                is (p ()) "VAR boom = ERROR ( \"unused\" ) RETURN 1" 1)
            testCase "later variables see earlier ones" (fun () -> is (p ()) "VAR a = 2 VAR b = a * 3 RETURN a + b" 8)
            testCase "a table variable can be iterated and used as a filter" (fun () ->
                is
                    (p ())
                    "VAR red = FILTER ( ALL ( Product[Color] ), Product[Color] = \"Red\" ) RETURN CALCULATE ( [Amount], red )"
                    570)
        ]

        testList "arbitrarily shaped filters" [
            // The filter pairs 2023 with Dec and 2024 with Feb: 100 + 165.
            testCase "the pairs are kept" (fun () ->
                is
                    (p ())
                    "CALCULATE ( [Amount], TREATAS ( { ( 2023, \"Dec\" ), ( 2024, \"Feb\" ) }, 'Date'[Year], 'Date'[Month] ) )"
                    265)
            // "Because the filter applied only overwrites Date[Month], the values in Date[Calendar
            // Year] are still in the filter context without the corresponding month" (SQLBI, "When
            // to use KEEPFILTERS over iterators"): January of 2023 and 2024 is 360.
            testCase "replacing one column leaves the other column's values" (fun () ->
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], 'Date'[Month] = \"Jan\" ), TREATAS ( { ( 2023, \"Dec\" ), ( 2024, \"Feb\" ) }, 'Date'[Year], 'Date'[Month] ) )"
                    360

                // Year 2024 with the months Dec and Feb left over: February 2024.
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], 'Date'[Year] = 2024 ), TREATAS ( { ( 2023, \"Dec\" ), ( 2024, \"Feb\" ) }, 'Date'[Year], 'Date'[Month] ) )"
                    165)
            testCase "KEEPFILTERS keeps the pairs" (fun () ->
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], KEEPFILTERS ( 'Date'[Month] = \"Jan\" ) ), TREATAS ( { ( 2023, \"Dec\" ), ( 2024, \"Feb\" ) }, 'Date'[Year], 'Date'[Month] ) )"
                    null)
        ]

        testList "relationships" [
            testCase "a filter on the one side reaches the many side, not the reverse" (fun () ->
                // Ann's and Cy's rows: 1, 3, 4, 6, 7.
                is (p ()) "CALCULATE ( COUNTROWS ( Sales ), Customer[Country] = \"US\" )" 5
                is (p ()) "CALCULATE ( COUNTROWS ( Product ), Sales[Qty] > 3 )" 4)
            // "when you place a filter on Sales, it results in applying the filter not only on the
            // Sales table, but also on the Product table" (SQLBI, "Context transition and expanded
            // tables"). Qty > 3: rows 5 (no product) and 6 (the Helmet).
            testCase "a table filter filters its expanded table" (fun () ->
                is (p ()) "CALCULATE ( COUNTROWS ( Product ), FILTER ( Sales, Sales[Qty] > 3 ) )" 1
                is (p ()) "CALCULATE ( COUNTROWS ( VALUES ( Product ) ), FILTER ( Sales, Sales[Qty] > 3 ) )" 2
                // Products with sales: Bike, Helmet, Lock (and the blank row).
                is (p ()) "CALCULATE ( COUNTROWS ( Product ), Sales )" 3
                is (p ()) "CALCULATE ( COUNTROWS ( VALUES ( Product ) ), Sales )" 4
                // Customers of Black products: row 4 is Cy's.
                is
                    (p ())
                    "CALCULATE ( COUNTROWS ( Customer ), CALCULATETABLE ( Sales, Product[Color] = \"Black\" ) )"
                    1)
            testCase "CROSSFILTER changes the direction" (fun () ->
                is
                    (p ())
                    "CALCULATE ( COUNTROWS ( Product ), Sales[Qty] > 3, CROSSFILTER ( Sales[ProductKey], Product[ProductKey], BOTH ) )"
                    1

                is
                    (p ())
                    "CALCULATE ( COUNTROWS ( Customer ), Product[Color] = \"Black\", CROSSFILTER ( Sales[CustomerKey], Customer[CustomerKey], BOTH ) )"
                    1
                // NONE: the colour no longer reaches Sales.
                is
                    (p ())
                    "CALCULATE ( [Amount], Product[Color] = \"Red\", CROSSFILTER ( Sales[ProductKey], Product[ProductKey], NONE ) )"
                    625)
            testCase "a relationship that filters both ways in the model" (fun () ->
                let b = bidirectional.Value
                // Qty > 3: rows 5 and 6. The Helmet, and the blank row for the orphan.
                is b "CALCULATE ( COUNTROWS ( Product ), Sales[Qty] > 3 )" 1
                is b "CALCULATE ( COUNTROWS ( VALUES ( Product ) ), Sales[Qty] > 3 )" 2
                // Nothing filtered: every product, sold or not.
                is b "COUNTROWS ( Product )" 4
                // US customers bought the Bike, the Helmet and the Lock.
                is b "CALCULATE ( COUNTROWS ( Product ), Customer[Country] = \"US\" )" 3
                is b "CALCULATE ( [Amount], Product[Color] = \"Red\" )" 570)
            // "USERELATIONSHIP activates a disabled relationship, deactivating possible conflicting
            // relationships" (dax.guide/userelationship). February by ship date: rows 2, 4, 5, 7.
            testCase "USERELATIONSHIP" (fun () ->
                is (p ()) "CALCULATE ( [Amount], 'Date'[Month] = \"Feb\" )" 165

                is
                    (p ())
                    "CALCULATE ( [Amount], 'Date'[Month] = \"Feb\", USERELATIONSHIP ( Sales[ShipDate], 'Date'[Date] ) )"
                    265

                is
                    (p ())
                    "CALCULATE ( [Amount], 'Date'[Year] = 2023, USERELATIONSHIP ( Sales[ShipDate], 'Date'[Date] ) )"
                    null

                isError "dax-invalid" "CALCULATE ( [Amount], USERELATIONSHIP ( Sales[Qty], 'Date'[Date] ) )")
            // "follows a chain of one or more many-to-one relationships to fetch the value"
            // (dax.guide/related). Red rows: 1, 2, 3, 6, 7 with Qty 2 + 1 + 3 + 5 + 1.
            testCase "RELATED" (fun () ->
                is (p ()) "SUMX ( Sales, IF ( RELATED ( Product[Color] ) = \"Red\", Sales[Qty] ) )" 12
                // Row 5's product doesn't exist: its related values are BLANK.
                is (p ()) "COUNTROWS ( FILTER ( Sales, ISBLANK ( RELATED ( Product[Name] ) ) ) )" 1
                isError "dax-invalid" "SUMX ( Product, RELATED ( Sales[Qty] ) )"
                isError "dax-invalid" "RELATED ( Product[Color] )")
            // "a shortcut for CALCULATETABLE function with no additional filters" (dax.guide/relatedtable).
            testCase "RELATEDTABLE" (fun () -> is (p ()) "SUMX ( Product, COUNTROWS ( RELATEDTABLE ( Sales ) ) )" 6)
            // dax.guide/lookupvalue: one distinct value is the result; none is BLANK (or the
            // alternate result); several different ones are an error (or the alternate result).
            testCase "LOOKUPVALUE" (fun () ->
                is (p ()) "LOOKUPVALUE ( Product[Name], Product[ProductKey], 2 )" "Helmet"
                is (p ()) "LOOKUPVALUE ( Product[Name], Product[ProductKey], 99 )" null
                is (p ()) "LOOKUPVALUE ( Product[Name], Product[ProductKey], 99, \"none\" )" "none"
                is (p ()) "LOOKUPVALUE ( Product[Color], Product[Name], \"bike\" )" "Red"

                is
                    (p ())
                    "LOOKUPVALUE ( Product[Name], Product[Color], \"Red\", Product[Category], \"Safety\" )"
                    "Helmet"

                isError "dax-error" "LOOKUPVALUE ( Product[Name], Product[Color], \"Red\" )"
                is (p ()) "LOOKUPVALUE ( Product[Name], Product[Color], \"Red\", \"several\" )" "several"
                is (p ()) "LOOKUPVALUE ( Product[Category], Product[Name], \"Lock\" )" "Safety"
                // It removes the filters on the table it searches.
                is
                    (p ())
                    "CALCULATE ( LOOKUPVALUE ( Product[Name], Product[ProductKey], 2 ), Product[Color] = \"Black\" )"
                    "Helmet"
                // `==`: BLANK does not find the key 0, and a text can't be compared with a number.
                is (p ()) "LOOKUPVALUE ( Product[Name], Product[ProductKey], BLANK () )" null
                isError "dax-error" "LOOKUPVALUE ( Product[Name], Product[ProductKey], \"2\" )")
        ]

        testList "the blank row" [
            // "VALUES ... treats the blank row as a regular row, returning it. On the other hand,
            // DISTINCT never returns the blank row" (SQLBI, "Blank row in DAX").
            testCase "VALUES and ALL have it, DISTINCT and ALLNOBLANKROW don't" (fun () ->
                is (p ()) "COUNTROWS ( VALUES ( Product[Color] ) )" 4
                is (p ()) "COUNTROWS ( DISTINCT ( Product[Color] ) )" 3
                is (p ()) "COUNTROWS ( ALL ( Product[Color] ) )" 4
                is (p ()) "COUNTROWS ( ALLNOBLANKROW ( Product[Color] ) )" 3
                is (p ()) "COUNTROWS ( VALUES ( Product ) )" 5
                is (p ()) "COUNTROWS ( ALL ( Product ) )" 5
                is (p ()) "COUNTROWS ( ALLNOBLANKROW ( Product ) )" 4
                is (p ()) "COUNTROWS ( Product )" 4
                is (p ()) "COUNTROWS ( DISTINCT ( Product ) )" 4
                // "DISTINCTCOUNT ( table[column] ) corresponds to COUNTROWS ( DISTINCT ( table[column] ) )".
                is (p ()) "DISTINCTCOUNT ( Product[Color] )" 3)
            testCase "a table whose relationships are all valid has none" (fun () ->
                is (p ()) "COUNTROWS ( VALUES ( Customer ) )" 3
                is (p ()) "COUNTROWS ( ALL ( Customer[Country] ) )" 2)
            testCase "the rows without a match belong to it" (fun () ->
                is (p ()) "CALCULATE ( [Amount], ISBLANK ( Product[Color] ) )" 40
                is (p ()) "CALCULATE ( [Amount], Product[Color] = BLANK () )" 40
                is (p ()) "CALCULATE ( COUNTROWS ( Sales ), Product[Name] = BLANK () )" 1)
            testCase "a filter that leaves it out hides it" (fun () ->
                is (p ()) "CALCULATE ( COUNTROWS ( VALUES ( Product ) ), Product[Color] = \"Red\" )" 2
                is (p ()) "CALCULATE ( COUNTROWS ( VALUES ( Product[Color] ) ), Product[Category] = \"Safety\" )" 2)
        ]

        testList "information functions" [
            // "COUNTROWS ( VALUES ( table[column] ) ) = 1" (dax.guide/countrows).
            testCase "HASONEVALUE and SELECTEDVALUE" (fun () ->
                is (p ()) "HASONEVALUE ( Product[Color] )" false
                is (p ()) "CALCULATE ( HASONEVALUE ( Product[Color] ), Product[Name] = \"Bike\" )" true
                is (p ()) "CALCULATE ( SELECTEDVALUE ( Product[Color] ), Product[Name] = \"Bike\" )" "Red"
                is (p ()) "SELECTEDVALUE ( Product[Color] )" null
                is (p ()) "SELECTEDVALUE ( Product[Color], \"many\" )" "many"
                // Cross-filtered through the table: the Safety products are Red and Black.
                is (p ()) "CALCULATE ( HASONEVALUE ( Product[Color] ), Product[Category] = \"Safety\" )" false)
            // "When the argument is a table, ISFILTERED checks only the columns of that table"
            // (dax.guide/isfiltered).
            testCase "ISFILTERED" (fun () ->
                is (p ()) "ISFILTERED ( Product[Color] )" false
                is (p ()) "CALCULATE ( ISFILTERED ( Product[Color] ), Product[Color] = \"Red\" )" true
                is (p ()) "CALCULATE ( ISFILTERED ( Product[Name] ), Product[Color] = \"Red\" )" false
                is (p ()) "CALCULATE ( ISFILTERED ( Product ), Product[Color] = \"Red\" )" true
                is (p ()) "CALCULATE ( ISFILTERED ( Sales ), Product[Color] = \"Red\" )" false)
            // "TRUE when any column of the table specified or of a related table is being
            // filtered" (dax.guide/iscrossfiltered).
            testCase "ISCROSSFILTERED" (fun () ->
                is (p ()) "ISCROSSFILTERED ( Sales )" false
                is (p ()) "CALCULATE ( ISCROSSFILTERED ( Sales ), Product[Color] = \"Red\" )" true
                is (p ()) "CALCULATE ( ISCROSSFILTERED ( Product[Name] ), Product[Color] = \"Red\" )" true)
            testCase "HASONEFILTER" (fun () ->
                is (p ()) "CALCULATE ( HASONEFILTER ( Product[Color] ), Product[Color] = \"Red\" )" true

                is
                    (p ())
                    "CALCULATE ( HASONEFILTER ( Product[Color] ), Product[Color] IN { \"Red\", \"Black\" } )"
                    false
                // Filtered through another column: one value, but no filter of its own.
                is (p ()) "CALCULATE ( HASONEFILTER ( Product[Color] ), Product[Name] = \"Bike\" )" false)
        ]

        testList "refused rather than guessed" [
            testCase "KEEPFILTERS on an iterator's table" (fun () ->
                isError "not-supported" "SUMX ( KEEPFILTERS ( VALUES ( Product[Color] ) ), [Amount] )")
            testCase "a model table with columns added, where its related tables would matter" (fun () ->
                isError "not-supported" "SUMX ( ADDCOLUMNS ( Sales, \"x\", 1 ), [Amount] )"
                isError "not-supported" "CALCULATE ( [Amount], ADDCOLUMNS ( Sales, \"x\", 1 ) )"
                isError "not-supported" "SUMX ( ADDCOLUMNS ( Sales, \"x\", 1 ), RELATED ( Product[ProductKey] ) )"
                isError "not-supported" "COUNTROWS ( SUMMARIZE ( ADDCOLUMNS ( Sales, \"x\", 1 ), Product[Color] ) )"
                // A table that relates to no other has no such question: per product, its amount.
                is (p ()) "SUMX ( ADDCOLUMNS ( Product, \"x\", 1 ), [Amount] * [x] )" 585)
            testCase "a name that is both a column of the row context and a measure" (fun () ->
                isError "not-supported" "SUMX ( ADDCOLUMNS ( VALUES ( Product[Color] ), \"Amount\", 1 ), [Amount] )")
            // Nesting as deep as the parser allows: evaluated on a deep stack.
            testCase "deeply nested expressions" (fun () ->
                let nested = String.replicate 200 "IF ( TRUE, " + "1" + String.replicate 200 " )"
                is (p ()) nested 1
                let parens = String.replicate 250 "(" + "1" + String.replicate 250 ")"
                is (p ()) (parens + " + 1") 2)
        ]

        testList "measures and functions" [
            testCase "a measure that refers to itself is an error" (fun () ->
                let m =
                    prepare {
                        star with
                            Measures = star.Measures @ [ "Sales", "Loop", "[Loop] + 1" ]
                    }

                Expect.equal (TessError.code (scalarError m "[Loop]")) "dax-invalid" "circular")
            // Measures that call measures, 300 deep: no stack overflow.
            testCase "a deep chain of measures" (fun () ->
                let chain = [
                    for i in 1..300 -> "Sales", $"M{i}", (if i = 1 then "1" else $"[M{i - 1}] + 1")
                ]

                let m =
                    prepare {
                        star with
                            Measures = star.Measures @ chain
                    }

                is m "[M300]" 300)
            testCase "an error inside a measure names the measure" (fun () ->
                let m =
                    prepare {
                        star with
                            Measures = star.Measures @ [ "Sales", "Formatted", "1 +\n  FORMAT ( [Amount], \"0\" )" ]
                    }

                match scalarError m "[Formatted]" with
                | DaxNotSupported(what, at) ->
                    Expect.equal what "the function FORMAT" "what"
                    Expect.equal at "measure 'Sales'[Formatted] 2:3" "where"
                | other -> failtestf "%A" other)
            // "val (eager evaluation): The expression is evaluated once before invoking the
            // function"; "expr (lazy evaluation): The expression is evaluated inside the function,
            // potentially in a different context" (Microsoft Learn, "DAX user-defined functions").
            testCase "a function's VAL parameter is a value, its EXPR parameter an expression" (fun () ->
                let m =
                    prepare {
                        star with
                            Functions = [
                                "RedOf", "( x : ANYVAL EXPR ) => CALCULATE ( x, Product[Color] = \"Red\" )"
                                "RedOfValue", "( x ) => CALCULATE ( x, Product[Color] = \"Red\" )"
                                "AddTax", "( amount : NUMERIC, rate : NUMERIC = 0.1 ) => amount * ( 1 + rate )"
                                "Whole", "( a : INT64, b : INT64 ) => a + b"
                            ]
                    }

                is m "RedOf ( [Amount] )" 570
                is m "RedOfValue ( [Amount] )" 625
                is m "AddTax ( 100 )" 110.00000000000001
                is m "AddTax ( 100, 0.5 )" 150.0
                // "because each parameter is separately cast to an integer, the result of 3+2 is
                // five" (SQLBI, "Introducing user-defined functions in DAX"): 3.4 and 2.4.
                is m "Whole ( 3.4, 2.4 )" 5
                Expect.equal (TessError.code (scalarError m "AddTax ()")) "dax-invalid" "a missing argument")
        ]
    ]
