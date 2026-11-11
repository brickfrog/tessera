/// ALLSELECTED in the local DAX engine: shadow filter contexts and the rows of SUMMARIZECOLUMNS.
///
/// The queries are those of SQLBI's "The definitive guide to ALLSELECTED" (quoted as "the guide")
/// and "ALLSELECTED best practices", run on a model of the same shape: the guide's nine products,
/// of three brands and three colors (the guide's figure of the data isn't text, so the rows are
/// made up to give the results the guide states: blue or green of Contoso or Fabrikam are Helmet,
/// Shoes, Keyboard and Piano). Every expected value is worked out by hand.
///
///   Product  Brand     Color   Qty      by brand:  Contoso 110, Fabrikam 220, Litware 120
///   Bike     Litware   Red      10      by color:  Red 120, Blue 140, Green 190
///   Helmet   Contoso   Blue     20      total 450
///   Shoes    Contoso   Green    30
///   Robot    Litware   Blue     40      Sales has one row per product, Sales[Product] -> Product.
///   Shirt    Fabrikam  Red      50
///   Rollerblades Contoso Red    60
///   Motorbike Litware  Green    70
///   Keyboard Fabrikam  Blue     80
///   Piano    Fabrikam  Green    90
module Tessera.Tests.EngineAllSelectedTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private products = [
    "Bike", "Litware", "Red", 10
    "Helmet", "Contoso", "Blue", 20
    "Shoes", "Contoso", "Green", 30
    "Robot", "Litware", "Blue", 40
    "Shirt", "Fabrikam", "Red", 50
    "Rollerblades", "Contoso", "Red", 60
    "Motorbike", "Litware", "Green", 70
    "Keyboard", "Fabrikam", "Blue", 80
    "Piano", "Fabrikam", "Green", 90
]

let private shop: TestModel = {
    Tables = [
        table "Product" [
            "Product", DataType.String
            "Brand", DataType.String
            "Color", DataType.String
        ] [ for (p, b, c, _) in products -> [ box p; box b; box c ] ]
        table "Sales" [ "Product", DataType.String; "Qty", DataType.Int64 ] [
            for (p, _, _, q) in products -> [ box p; box q ]
        ]
    ]
    Rels = [ rel ("Sales", "Product") ("Product", "Product") ]
    Measures = [
        "Sales", "Qty", "SUM ( Sales[Qty] )"
        "Sales", "TotalSel", "CALCULATE ( [Qty], ALLSELECTED () )"
    ]
    Functions = []
}

let private p = lazy (prepare shop)

/// The same, with a calculated column that uses ALLSELECTED.
let private ranked =
    lazy
        (prepare {
            shop with
                Tables =
                    shop.Tables
                    |> List.map (fun t ->
                        if t.Name = "Product" then
                            t
                            |> withCalculated [
                                "Rank", DataType.Int64, "COUNTROWS ( ALLSELECTED ( Product[Brand] ) )"
                            ]
                        else
                            t)
        })

/// The guide's outer filter.
let private outer (inner: string) =
    $"EVALUATE CALCULATETABLE ( {inner}, Product[Color] IN {{ \"Blue\", \"Green\" }}, Product[Brand] IN {{ \"Contoso\", \"Fabrikam\" }} )"

let private list (table: string) (column: string) =
    $"CONCATENATEX ( {table}, {column}, \", \", {column}, ASC )"

let private selectedProducts =
    list "ALLSELECTED ( Product[Product] )" "Product[Product]"

let private visibleProducts = list "VALUES ( Product[Product] )" "Product[Product]"
let private selectedBrands = list "ALLSELECTED ( Product[Brand] )" "Product[Brand]"
let private productsOfTable = list "ALLSELECTED ( 'Product' )" "Product[Product]"
let private outerBrands = list "OuterBrands" "Product[Brand]"

/// Every value of a one-column result.
let private column (dax: string) : QueryValue list =
    rows p.Value dax |> List.map List.last |> List.distinct

[<Tests>]
let tests =
    testList "Engine ALLSELECTED" [
        testList "as a table, the guide's queries" [
            // "There is no active iteration when we call ALLSELECTED ... all the product names are
            // returned because neither Color nor Brand filter the Product[Product] column".
            testCase "a column with no iteration: every value, cross-filters ignored" (fun () ->
                returns p.Value (outer $"ROW ( \"Products\", {selectedProducts} )") [
                    [
                        "Bike, Helmet, Keyboard, Motorbike, Piano, Robot, Rollerblades, Shirt, Shoes"
                    ]
                ])
            // "it is VALUES that analyzes the products visible in the current context ...
            // ALLSELECTED did not contribute to the filtering in any way".
            testCase "as a modifier with no iteration, a cross-filtered column stays cross-filtered" (fun () ->
                returns
                    p.Value
                    (outer $"ROW ( \"Products\", CALCULATE ( {visibleProducts}, ALLSELECTED ( Product[Product] ) ) )")
                    [ [ "Helmet, Keyboard, Piano, Shoes" ] ])
            // "ADDCOLUMNS ... generates a shadow filter context containing the iterated table": the
            // two brands VALUES returns, then all three when it iterates ALL.
            testCase "inside ADDCOLUMNS: the iterated values" (fun () ->
                returns p.Value (outer $"ADDCOLUMNS ( VALUES ( Product[Brand] ), \"Brands\", {selectedBrands} )") [
                    [ "Contoso"; "Contoso, Fabrikam" ]
                    [ "Fabrikam"; "Contoso, Fabrikam" ]
                ]

                returns p.Value (outer $"ADDCOLUMNS ( ALL ( Product[Brand] ), \"Brands\", {selectedBrands} )") [
                    [ "Contoso"; "Contoso, Fabrikam, Litware" ]
                    [ "Fabrikam"; "Contoso, Fabrikam, Litware" ]
                    [ "Litware"; "Contoso, Fabrikam, Litware" ]
                ])
            // "The result is the list of brands iterated in the nearest shadow filter context".
            testCase "two iterations on the column: the nearest one" (fun () ->
                let nested (outerTable: string) (innerTable: string) (column: string) =
                    let selected = list ("ALLSELECTED ( " + column + " )") column

                    outer
                        $"GENERATE ( SELECTCOLUMNS ( {outerTable}, \"Outer Brand\", Product[Brand] ), GENERATE ( SELECTCOLUMNS ( {innerTable}, \"Inner Brand\", Product[Brand] ), ROW ( \"List\", {selected} ) ) )"

                let all = "ALL ( Product[Brand] )"
                let values = "VALUES ( Product[Brand] )"
                // 3 outer brands x 2 inner ones, each with the inner two.
                Expect.equal (rows p.Value (nested all values "Product[Brand]")).Length 6 "rows"
                Expect.equal (column (nested all values "Product[Brand]")) [ qv "Contoso, Fabrikam" ] "inner VALUES"
                // Swapped: 2 x 3 rows, each with all the brands.
                Expect.equal (rows p.Value (nested values all "Product[Brand]")).Length 6 "rows"

                Expect.equal
                    (column (nested values all "Product[Brand]"))
                    [ qv "Contoso, Fabrikam, Litware" ]
                    "inner ALL"

                // "because there is no shadow filter context filtering the product name,
                // ALLSELECTED simply returns all the product names".
                Expect.equal
                    (column (nested values all "Product[Product]"))
                    [
                        qv "Bike, Helmet, Keyboard, Motorbike, Piano, Robot, Rollerblades, Shirt, Shoes"
                    ]
                    "a column no iteration filters")
            // "save the ALLSELECTED values of Product[Brand] in a variable before the innermost
            // shadow filter context kicks in": the outer brands.
            testCase "a variable keeps the outer shadow" (fun () ->
                let dax =
                    outer
                        $"GENERATE ( SELECTCOLUMNS ( VALUES ( Product[Brand] ), \"Outer Brand\", Product[Brand] ), VAR OuterBrands = ALLSELECTED ( Product[Brand] ) RETURN GENERATE ( SELECTCOLUMNS ( ALL ( Product[Brand] ), \"Inner Brand\", Product[Brand] ), ROW ( \"Brands\", {outerBrands} ) ) )"

                Expect.equal (column dax) [ qv "Contoso, Fabrikam" ] dax)
            // "ALLSELECTED with a table used the explicit column filters created by CALCULATETABLE".
            testCase "a table with no iteration: the explicit filters" (fun () ->
                returns p.Value (outer $"ROW ( \"Products\", {productsOfTable} )") [
                    [ "Helmet, Keyboard, Piano, Shoes" ]
                ])
            // "the filter on Color is replaced, the filter on Brand is not": Contoso or Fabrikam,
            // any color.
            testCase "a table inside an iteration over one of its columns" (fun () ->
                let dax =
                    outer $"ADDCOLUMNS ( ALL ( Product[Color] ), \"Products\", {productsOfTable} )"

                Expect.equal (rows p.Value dax).Length 3 "rows"
                Expect.equal (column dax) [ qv "Helmet, Keyboard, Piano, Rollerblades, Shirt, Shoes" ] dax)
            // "the two shadow filters completely replace the explicit filters".
            testCase "a table inside iterations over two of its columns" (fun () ->
                let dax =
                    outer
                        $"GENERATE ( ALL ( Product[Brand] ), ADDCOLUMNS ( ALL ( Product[Color] ), \"Products\", {productsOfTable} ) )"

                Expect.equal (rows p.Value dax).Length 9 "rows"

                Expect.equal
                    (column dax)
                    [
                        qv "Bike, Helmet, Keyboard, Motorbike, Piano, Robot, Rollerblades, Shirt, Shoes"
                    ]
                    dax)
            // "ALLSELECTED will activate the last shadow filter context on each column ... the outer
            // shadow filter context is not activated".
            testCase "the innermost shadow wins on each column" (fun () ->
                let dax =
                    outer
                        $"GENERATE ( SELECTCOLUMNS ( ALL ( Product[Brand] ), \"Outer Brand\", Product[Brand] ), ADDCOLUMNS ( CROSSJOIN ( VALUES ( Product[Color] ), SELECTCOLUMNS ( VALUES ( Product[Brand] ), \"Inner Brand\", Product[Brand] ) ), \"Products\", {productsOfTable} ) )"

                // 3 outer brands x 2 colors x 2 inner brands.
                Expect.equal (rows p.Value dax).Length 12 "rows"
                Expect.equal (column dax) [ qv "Helmet, Keyboard, Piano, Shoes" ] dax)
            // "returns the values of the column that were visible in the last shadow filter
            // context": both columns of the CROSSJOIN iterated, its 9 combinations.
            testCase "several columns that one shadow holds" (fun () ->
                returns
                    p.Value
                    "EVALUATE ROW ( \"n\", SUMX ( CROSSJOIN ( VALUES ( Product[Brand] ), VALUES ( Product[Color] ) ), COUNTROWS ( ALLSELECTED ( Product[Brand], Product[Color] ) ) ) )"
                    [ [ 81 ] ])
        ]

        testList "as a modifier" [
            // "it performs ALLSELECTED on all the columns that are referenced by some shadow filter
            // context. In practice, restoring the last shadow filter context for each column" (the
            // guide). The brands iterated are Contoso and Fabrikam; per brand, the colors iterated
            // are its colors but red: blue and green. Every evaluation restores both: Helmet 20 +
            // Shoes 30 + Keyboard 80 + Piano 90 = 220; 2 brands x 2 colors x 220.
            testCase "ALLSELECTED () restores the last shadow of each column" (fun () ->
                returns
                    p.Value
                    "EVALUATE CALCULATETABLE ( ROW ( \"x\", SUMX ( VALUES ( Product[Brand] ), CALCULATE ( SUMX ( FILTER ( VALUES ( Product[Color] ), Product[Color] <> \"Red\" ), CALCULATE ( [Qty], ALLSELECTED () ) ) ) ) ), Product[Brand] IN { \"Contoso\", \"Fabrikam\" } )"
                    [ [ 880 ] ])
            // Without the inner CALCULATE the colors are iterated under the outer filter only:
            // all three colors of the two brands, so every evaluation is 330 (the guide's
            // "300x6=1800" grand total). 2 brands x 3 colors x 330.
            testCase "the guide's grand total of AllSelSumX" (fun () ->
                returns
                    p.Value
                    "EVALUATE CALCULATETABLE ( ROW ( \"x\", SUMX ( VALUES ( Product[Brand] ), SUMX ( VALUES ( Product[Color] ), CALCULATE ( [Qty], ALLSELECTED () ) ) ) ), Product[Brand] IN { \"Contoso\", \"Fabrikam\" } )"
                    [ [ 1980 ] ])
            // dax.guide/allselected's iterator example, and the guide's "Using ALLSELECTED as a
            // CALCULATE filter argument means transforming a shadow filter context into an explicit
            // filter context. This way, it gains precedence against the filter context generated
            // by context transition": every row sees all the colors iterated.
            testCase "over a transition: the iterated values, not the outer filter" (fun () ->
                returns
                    p.Value
                    "EVALUATE CALCULATETABLE ( ADDCOLUMNS ( ALL ( Product[Color] ), \"Sel\", CALCULATE ( [Qty], ALLSELECTED ( Product[Color] ) ) ), Product[Color] IN { \"Red\", \"Blue\" } )"
                    [ [ "Blue"; 450 ]; [ "Green"; 450 ]; [ "Red"; 450 ] ])
            // "ALLSELECTED ... works fine even though SUMMARIZECOLUMNS does not create the filter"
            // ("ALLSELECTED best practices"): the ADDCOLUMNS form of the same query, with the
            // shadow of VALUES holding the two brands. Contoso 110 / 330, Fabrikam 220 / 330.
            testCase "the percentage of the selection, with ADDCOLUMNS" (fun () ->
                returns
                    p.Value
                    "EVALUATE CALCULATETABLE ( ADDCOLUMNS ( VALUES ( Product[Brand] ), \"Pct\", DIVIDE ( [Qty], CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ) ), TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ) )"
                    [ [ "Contoso"; 110.0 / 330.0 ]; [ "Fabrikam"; 220.0 / 330.0 ] ])
        ]

        testList "in SUMMARIZECOLUMNS" [
            // "When used in SUMMARIZECOLUMNS, ALLSELECTED removes filters for the currently-iterated
            // group-by column, restoring the original filter set by SUMMARIZECOLUMNS itself".
            testCase "the percentage of the selection" (fun () ->
                let expected: obj list list = [ [ "Contoso"; 110.0 / 330.0 ]; [ "Fabrikam"; 220.0 / 330.0 ] ]

                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ), \"Pct\", DIVIDE ( [Qty], CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ) )"
                    expected

                // "The following query is equivalent ... despite the filter being set by
                // CALCULATETABLE rather than by SUMMARIZECOLUMNS".
                returns
                    p.Value
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Brand], \"Pct\", DIVIDE ( [Qty], CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ) ), TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ) )"
                    expected)
            // A group-by column the argument doesn't name keeps its group: per brand, all colors.
            testCase "only the named group-by column is released" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], Product[Color], \"All colors\", CALCULATE ( [Qty], ALLSELECTED ( Product[Color] ) ) )"
                    [
                        for (b, total) in [ "Contoso", 110; "Fabrikam", 220; "Litware", 120 ] do
                            for c in [ "Blue"; "Green"; "Red" ] -> [ box b; box c; box total ]
                    ])
            // A subtotal row of ROLLUPADDISSUBTOTAL has no group to release: the selection.
            testCase "the total row of ROLLUPADDISSUBTOTAL" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Product[Brand], \"Total\" ), TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ), \"Sel\", [TotalSel] )"
                    [ [ "Contoso"; false; 330 ]; [ "Fabrikam"; false; 330 ]; [ null; true; 330 ] ])
            // ALLSELECTED as a table in SUMMARIZECOLUMNS: "ALLSELECTED returns the values of the
            // brands selected in the visual filter context" (SQLBI, "ALLSELECTED best practices";
            // the MINX over ALLSELECTED of "Introducing ALLSELECTED in DAX").
            testCase "as a table, the selected values" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ), \"Min\", MINX ( ALLSELECTED ( Product[Brand] ), [Qty] ), \"Rows\", COUNTROWS ( ALLSELECTED ( Product ) ) )"
                    [ [ "Contoso"; 110; 6 ]; [ "Fabrikam"; 110; 6 ] ])
            // "Inside the iteration, ALLSELECTED restores the shadow filter context created by SUMX,
            // which is iterating over the products of the given brand"; "Outside of the iteration,
            // ALLSELECTED removes the filter over Product[Brand] and makes all the selected products
            // visible" (SQLBI, "ALLSELECTED best practices"). Wrong: 3 products x the brand's total;
            // Correct: 3 products x 450.
            testCase "the best-practices Wrong and Correct measures" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], \"Wrong\", SUMX ( 'Product', [TotalSel] ), \"Correct\", VAR TotalSales = [TotalSel] RETURN SUMX ( 'Product', TotalSales ) )"
                    [
                        [ "Contoso"; 330; 1350 ]
                        [ "Fabrikam"; 660; 1350 ]
                        [ "Litware"; 360; 1350 ]
                    ])
            // The guide's AllSelSumX in a matrix by brand: the shadows of both SUMX hold the one
            // brand and its three colors. Contoso 3 x 110, Fabrikam 3 x 220.
            testCase "iterations inside a row: the shadows" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], TREATAS ( { \"Contoso\", \"Fabrikam\" }, Product[Brand] ), \"AllSel\", [TotalSel], \"AllSelSumX\", SUMX ( VALUES ( Product[Brand] ), SUMX ( VALUES ( Product[Color] ), CALCULATE ( [Qty], ALLSELECTED () ) ) ) )"
                    [ [ "Contoso"; 330; 330 ]; [ "Fabrikam"; 330; 660 ] ])
            // What stays filtered is what SUMMARIZECOLUMNS was given: "restoring the original filter
            // set by SUMMARIZECOLUMNS itself". And ISINSCOPE: "a filter placed on the column derived
            // either from context transition or from a group by column" (dax.guide/isinscope),
            // which ALLSELECTED removed.
            testCase "ISFILTERED and ISINSCOPE after it" (fun () ->
                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], TREATAS ( { \"Contoso\" }, Product[Brand] ), \"F\", CALCULATE ( ISFILTERED ( Product[Brand] ), ALLSELECTED ( Product[Brand] ) ), \"S\", CALCULATE ( ISINSCOPE ( Product[Brand] ), ALLSELECTED ( Product[Brand] ) ), \"In\", ISINSCOPE ( Product[Brand] ) )"
                    [ [ "Contoso"; true; false; true ] ]

                returns
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], \"F\", CALCULATE ( ISFILTERED ( Product[Brand] ), ALLSELECTED ( Product[Brand] ) ) )"
                    [ [ "Contoso"; false ]; [ "Fabrikam"; false ]; [ "Litware"; false ] ])
        ]

        // Microsoft Learn's ALLSELECTED example: SUMMARIZE over a CROSSJOIN of the group-by
        // columns, the visual total on every row, and the total for all of one column per value of
        // the other. Here under a filter on Product[Product] (not a group-by column), which stays:
        // 450 - 10 = 440; all colors per brand: Contoso 110, Fabrikam 220, Litware 120 - 10.
        testCase "SUMMARIZE over the group-by columns: Learn's visual totals" (fun () ->
            returns
                p.Value
                "EVALUATE CALCULATETABLE ( SUMMARIZE ( CROSSJOIN ( DISTINCT ( Product[Brand] ), DISTINCT ( Product[Color] ) ), Product[Brand], Product[Color], \"VT\", [TotalSel], \"All colors\", CALCULATE ( [Qty], ALLSELECTED ( Product[Color] ) ) ), Product[Product] <> \"Bike\" )"
                [
                    for (b, total) in [ "Contoso", 110; "Fabrikam", 220; "Litware", 110 ] do
                        for c in [ "Blue"; "Green"; "Red" ] -> [ box b; box c; box 440; box total ]
                ])

        testList "refused" [
            testCase "a column with a filter of its own and no shadow" (fun () ->
                notSupported
                    p.Value
                    "EVALUATE CALCULATETABLE ( ROW ( \"n\", COUNTROWS ( ALLSELECTED ( Product[Brand] ) ) ), Product[Brand] = \"Contoso\" )"
                    "has a filter of its own"

                notSupported
                    p.Value
                    "EVALUATE CALCULATETABLE ( ROW ( \"n\", CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ), Product[Brand] = \"Contoso\" )"
                    "has a filter of its own")
            testCase "the shadow of a table restoring a related table's column" (fun () ->
                notSupported
                    p.Value
                    "EVALUATE ROW ( \"x\", SUMX ( Sales, CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ) )"
                    "a table related to 'Product'"

                notSupported p.Value "EVALUATE ROW ( \"x\", SUMX ( Sales, [TotalSel] ) )" "a table related to")
            testCase "a group-by column inside an iteration that doesn't iterate it" (fun () ->
                notSupported
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], \"x\", SUMX ( VALUES ( Product[Color] ), CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ) ) )"
                    "a group-by column of SUMMARIZECOLUMNS")
            testCase "a SUMMARIZECOLUMNS row whose filters CALCULATE changed" (fun () ->
                notSupported
                    p.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Brand], \"x\", CALCULATE ( CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ) ), Product[Color] = \"Red\" ) )"
                    "filters of the row were changed")
            testCase "row contexts that are not an iterator's" (fun () ->
                notSupported ranked.Value "EVALUATE ROW ( \"x\", SUM ( Product[Rank] ) )" "calculated column"

                notSupported
                    p.Value
                    "EVALUATE VALUES ( Product[Brand] ) ORDER BY COUNTROWS ( ALLSELECTED ( Product[Brand] ) )"
                    "ORDER BY"

                notSupported
                    p.Value
                    "EVALUATE ROW ( \"x\", CALCULATE ( [Qty], Product[Brand] IN ALLSELECTED ( Product[Brand] ) ) )"
                    "True/False filter argument"

                notSupported
                    p.Value
                    "EVALUATE SUMMARIZE ( Product, Product[Brand], \"n\", COUNTROWS ( ALLSELECTED ( Product[Brand] ) ) )"
                    "SUMMARIZE extension column")
            testCase "whether a restored shadow filters the column" (fun () ->
                notSupported
                    p.Value
                    "EVALUATE ADDCOLUMNS ( VALUES ( Product[Brand] ), \"f\", CALCULATE ( ISFILTERED ( Product[Brand] ), ALLSELECTED ( Product[Brand] ) ) )"
                    "is filtered by the filter ALLSELECTED restored"

                notSupported
                    p.Value
                    "EVALUATE ADDCOLUMNS ( VALUES ( Product[Brand] ), \"f\", CALCULATE ( ISCROSSFILTERED ( Product ), ALLSELECTED ( Product[Brand] ) ) )"
                    "ISCROSSFILTERED")
            testCase "forms coverage reports" (fun () ->
                for dax, what in
                    [
                        "CALCULATE ( [Qty], KEEPFILTERS ( ALLSELECTED ( Product[Brand] ) ) )",
                        "KEEPFILTERS ( ALLSELECTED"
                        "CALCULATE ( [Qty], ALLSELECTED ( Product[Brand] ), ALLSELECTED ( Product[Color] ) )",
                        "more than one ALLSELECTED"
                        "CALCULATE ( [Qty], ALLSELECTED (), ALL ( Sales ) )", "together with another"
                    ] do
                    notSupported p.Value $"EVALUATE ROW ( \"x\", {dax} )" what)
            testCase "ALLSELECTED () as a table is invalid DAX" (fun () ->
                Expect.equal
                    (TessError.code (queryError p.Value "EVALUATE ROW ( \"x\", COUNTROWS ( ALLSELECTED () ) )"))
                    "dax-invalid"
                    "no argument"

                Expect.equal
                    (TessError.code (
                        queryError
                            p.Value
                            "EVALUATE ROW ( \"x\", COUNTROWS ( ALLSELECTED ( VALUES ( Product[Brand] ) ) ) )"
                    ))
                    "dax-invalid"
                    "an expression")
        ]
    ]
