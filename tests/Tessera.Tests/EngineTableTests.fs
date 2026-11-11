/// Table functions, queries, calculated columns and tables, and the model shapes the local DAX
/// engine refuses. Expected values are worked out by hand from the `star` model in EngineHelpers;
/// EngineContextTests' header lists the amounts by product, colour, customer and date.
module Tessera.Tests.EngineTableTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private p () = starPrepared.Value

let private columns (r: QueryResult) =
    r.Columns |> List.map (fun c -> c.Name, QueryType.token c.Type)

let private queryCode (prepared: Engine.Prepared) (dax: string) =
    TessError.code (queryError prepared dax)

[<Tests>]
let tests =
    testList "Engine tables" [
        testList "result columns" [
            // "Sales[Amount]" for a model column, "[Name]" for an extension column (README, `query`).
            testCase "names" (fun () ->
                Expect.equal
                    (columns (query (p ()) "EVALUATE VALUES ( Product[Color] )"))
                    [ "Product[Color]", "string" ]
                    "a model column"

                Expect.equal
                    (columns (query (p ()) "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"A\", [Amount] )"))
                    [ "Customer[Country]", "string"; "[A]", "int64" ]
                    "an extension column"

                Expect.equal
                    (columns (query (p ()) "EVALUATE ROW ( \"x\", 1, \"y\", \"a\" )"))
                    [ "[x]", "int64"; "[y]", "string" ]
                    "ROW"

                Expect.equal (columns (query (p ()) "EVALUATE { 1, 2 }")) [ "[Value]", "int64" ] "one column"

                Expect.equal
                    (columns (query (p ()) "EVALUATE { ( 1, \"a\" ) }"))
                    [ "[Value1]", "int64"; "[Value2]", "string" ]
                    "several columns"

                Expect.equal
                    (columns (query (p ()) "EVALUATE 'Date'"))
                    [ "Date[Date]", "dateTime"; "Date[Year]", "int64"; "Date[Month]", "string" ]
                    "a table")
            testCase "types" (fun () ->
                // A column that is BLANK in every row still has its type.
                Expect.equal
                    (columns (
                        query (p ()) "EVALUATE ROW ( \"x\", CALCULATE ( SUM ( Sales[Qty] ), Sales[Qty] > 100 ) )"
                    ))
                    [ "[x]", "int64" ]
                    "all BLANK"

                Expect.equal
                    (columns (query (p ()) "EVALUATE ROW ( \"x\", DIVIDE ( 1, 0 ) )"))
                    [ "[x]", "double" ]
                    "DIVIDE"

                Expect.equal (columns (query (p ()) "EVALUATE ROW ( \"x\", 7 / 2 )")) [ "[x]", "double" ] "division"
                Expect.equal (columns (query (p ()) "EVALUATE ROW ( \"x\", 1 = 1 )")) [ "[x]", "boolean" ] "comparison"

                Expect.equal
                    (columns (query (p ()) "EVALUATE ROW ( \"x\", IF ( TRUE, 1, \"a\" ) )"))
                    [ "[x]", "variant" ]
                    "IF of two types"
                // "all values are converted to a common data type" (Microsoft Learn, table constructor).
                returnsInOrder (p ()) "EVALUATE { 1, 2.5 }" [ [ 1.0 ]; [ 2.5 ] ])
        ]

        testList "FILTER, ADDCOLUMNS, SELECTCOLUMNS" [
            testCase "FILTER keeps the rows that satisfy the condition" (fun () ->
                returns (p ()) "EVALUATE FILTER ( Product, Product[Color] = \"Red\" )" [
                    [ 1; "Bike"; "Red"; "Cycling" ]
                    [ 2; "Helmet"; "Red"; "Safety" ]
                ])
            // A row context only: without CALCULATE the filter context is the caller's.
            testCase "ADDCOLUMNS evaluates in a row context" (fun () ->
                returns
                    (p ())
                    "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"N\", COUNTROWS ( Sales ), \"A\", [Amount] )"
                    [ [ "US"; 7; 485 ]; [ "UK"; 7; 140 ] ])
            testCase "SELECTCOLUMNS names its columns" (fun () ->
                Expect.equal
                    (columns (query (p ()) "EVALUATE SELECTCOLUMNS ( Product, \"N\", Product[Name], Product[Color] )"))
                    [ "[N]", "string"; "Product[Color]", "string" ]
                    "names"

                returns (p ()) "EVALUATE SELECTCOLUMNS ( Customer, \"N\", Customer[Name] & \"!\" )" [
                    [ "Ann!" ]
                    [ "Bob!" ]
                    [ "Cy!" ]
                ])
            // "The <Name> argument can be skipped if the correspondent <Expression> argument is a
            // simple column reference of the iterated table" (dax.guide, SELECTCOLUMNS); Learn's
            // OFFSET example selects an unqualified [CurrentYearSales] this way.
            testCase "SELECTCOLUMNS takes an unnamed [Column] of the iterated table" (fun () ->
                let added =
                    "ADDCOLUMNS ( VALUES ( Customer[Country] ), \"N\", COUNTROWS ( Sales ) )"

                Expect.equal
                    (columns (query (p ()) $"EVALUATE SELECTCOLUMNS ( {added}, [N] )"))
                    [ "[N]", "int64" ]
                    "an extension column keeps its name"

                returns (p ()) $"EVALUATE SELECTCOLUMNS ( {added}, [N] )" [ [ 7 ]; [ 7 ] ]

                Expect.equal
                    (columns (query (p ()) "EVALUATE SELECTCOLUMNS ( Product, [Color] )"))
                    [ "Product[Color]", "string" ]
                    "a model column keeps its name"

                // ... and its lineage: red products are 1 and 2, 570.
                returns
                    (p ())
                    "EVALUATE ROW ( \"x\", CALCULATE ( [Amount], SELECTCOLUMNS ( FILTER ( Product, Product[Color] = \"Red\" ), [ProductKey] ) ) )"
                    [ [ 570 ] ]

                // A measure's name is an expression, which needs a name.
                Expect.equal
                    (queryCode (p ()) "EVALUATE SELECTCOLUMNS ( Product, [Amount] )")
                    "dax-invalid"
                    "a measure"

                // A table with two columns of that name (CROSSJOIN refuses to make one; SELECTCOLUMNS
                // refuses too, should one reach it).
                notSupported (p ()) "EVALUATE SELECTCOLUMNS ( CROSSJOIN ( Product, Customer ), [Name] )" "same name"

                // An outer row context's column is not a column of the iterated table.
                Expect.equal
                    (queryCode
                        (p ())
                        "EVALUATE ADDCOLUMNS ( SELECTCOLUMNS ( { 1 }, \"K\", [Value] ), \"x\", COUNTROWS ( SELECTCOLUMNS ( Product, [K] ) ) )")
                    "dax-invalid"
                    "an outer column")
            testCase "a column name used twice is an error" (fun () ->
                Expect.equal
                    (queryCode (p ()) "EVALUATE ADDCOLUMNS ( Customer, \"Name\", 1, \"name\", 2 )")
                    "dax-invalid"
                    "twice"

                Expect.equal (queryCode (p ()) "EVALUATE ROW ( \"a\", 1, \"A\", 2 )") "dax-invalid" "ROW")
        ]

        testList "data lineage" [
            // "Data lineage is maintained as long as an expression is only made up of one column
            // reference ... adding an empty string ... does break the data lineage" (SQLBI,
            // "Understanding data lineage in DAX"). Red products are 1 and 2: 570.
            testCase "a plain column reference keeps it, an expression breaks it" (fun () ->
                is
                    (p ())
                    "CALCULATE ( [Amount], SELECTCOLUMNS ( FILTER ( Product, Product[Color] = \"Red\" ), \"k\", Product[ProductKey] ) )"
                    570

                is
                    (p ())
                    "CALCULATE ( [Amount], SELECTCOLUMNS ( FILTER ( Product, Product[Color] = \"Red\" ), \"k\", Product[ProductKey] + 0 ) )"
                    625)
            testCase "VALUES of a column filters that column" (fun () ->
                is
                    (p ())
                    "CALCULATE ( CALCULATE ( [Amount], ALL ( Product ), VALUES ( Product[Color] ) ), Product[Name] = \"Lock\" )"
                    15)
            // "TREATAS assigns the data lineage"; "filters out any values that are not present in
            // its respective output column" (dax.guide/treatas).
            testCase "TREATAS gives values a lineage" (fun () ->
                is (p ()) "CALCULATE ( [Amount], TREATAS ( { \"Red\" }, Product[Color] ) )" 570
                is (p ()) "CALCULATE ( [Amount], TREATAS ( { \"Purple\" }, Product[Color] ) )" null
                is (p ()) "COUNTROWS ( TREATAS ( { \"Red\", \"Purple\", \"black\" }, Product[Color] ) )" 2

                is
                    (p ())
                    "CALCULATE ( [Amount], TREATAS ( { ( \"Red\", \"US\" ) }, Product[Color], Customer[Country] ) )"
                    470

                notSupported (p ()) "EVALUATE TREATAS ( { 1 }, Product[Color] )" "TREATAS")
            // "if combined columns have lineage to different base columns, or if there is an
            // extension column, the resulting column in UNION will have no lineage" (Microsoft
            // Learn, UNION).
            testCase "UNION keeps a lineage every table shares" (fun () ->
                is
                    (p ())
                    "CALCULATE ( [Amount], UNION ( FILTER ( ALL ( Product[Color] ), Product[Color] = \"Red\" ), FILTER ( ALL ( Product[Color] ), Product[Color] = \"Black\" ) ) )"
                    585

                is
                    (p ())
                    "CALCULATE ( [Amount], UNION ( FILTER ( ALL ( Product[Color] ), Product[Color] = \"Red\" ), { \"Black\" } ) )"
                    625)
        ]

        testList "SUMMARIZE" [
            // The colours that Sales has: Red, Black, and BLANK for the row without a product.
            testCase "groups by columns of the table and of related tables" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZE ( Sales, Product[Color] )" [ [ "Red" ]; [ "Black" ]; [ null ] ]

                // Units: US Cycling rows 1, 7 (2 + 1); UK Cycling row 2; US Safety rows 3, 4, 6
                // (3 + 1 + 5); UK and no category: row 5.
                returns
                    (p ())
                    "EVALUATE SUMMARIZE ( Sales, Customer[Country], Product[Category], \"U\", SUM ( Sales[Qty] ) )"
                    [
                        [ "US"; "Cycling"; 3 ]
                        [ "UK"; "Cycling"; 1 ]
                        [ "US"; "Safety"; 9 ]
                        [ "UK"; null; 4 ]
                    ])
            // "The cluster header contains Product[Color], but the cluster filter contains all the
            // columns in the expanded table Sales" (SQLBI, "All the secrets of SUMMARIZE"): the
            // products of each group's rows. Red: Bike and Helmet; Black: Lock; the last group's
            // row has no product (a table reference leaves the blank row out).
            testCase "an expression sees the group's rows as a filter" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZE ( Sales, Product[Color], \"A\", [Amount], \"P\", COUNTROWS ( Product ) )"
                    [ [ "Red"; 570; 2 ]; [ "Black"; 15; 1 ]; [ null; 40; null ] ])
            testCase "ROLLUP is not supported" (fun () ->
                notSupported (p ()) "EVALUATE SUMMARIZE ( Sales, ROLLUP ( Product[Color] ), \"A\", [Amount] )" "ROLLUP")
        ]

        testList "SUMMARIZECOLUMNS" [
            // "Only rows for which at least one of the supplied expressions return a non-blank
            // value are included" (Microsoft Learn): Blue has no sales.
            testCase "groups and drops the rows whose expressions are all BLANK" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"A\", [Amount] )" [
                    [ "Red"; 570 ]
                    [ "Black"; 15 ]
                    [ null; 40 ]
                ]

                // US Red: rows 1, 3, 6, 7; UK Red: row 2; US Black: row 4; UK without product: row 5.
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Customer[Country], Product[Color], \"A\", [Amount] )" [
                    [ "US"; "Red"; 470 ]
                    [ "UK"; "Red"; 100 ]
                    [ "US"; "Black"; 15 ]
                    [ "UK"; null; 40 ]
                ])
            testCase "an expression that is never BLANK keeps every group" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"N\", 1 )" [
                    [ "Red"; 1 ]
                    [ "Black"; 1 ]
                    [ "Blue"; 1 ]
                    [ null; 1 ]
                ])
            // "if you do not include any expression, you obtain a crossjoin as a result" (SQLBI,
            // "Introducing SUMMARIZECOLUMNS"): 2 countries by 4 colours (the blank row's included:
            // "The values obtained for the group-by columns include the blank values of invalid
            // relationships").
            testCase "columns of different tables are cross-joined" (fun () ->
                Expect.equal
                    (rows (p ()) "EVALUATE SUMMARIZECOLUMNS ( Customer[Country], Product[Color] )").Length
                    8
                    "2 x 4")
            // "auto-existed (same table)": only the combinations that exist in Product.
            testCase "columns of one table give the existing combinations" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Product[Category] )" [
                    [ "Red"; "Cycling" ]
                    [ "Red"; "Safety" ]
                    [ "Black"; "Safety" ]
                    [ "Blue"; "Cycling" ]
                    [ null; null ]
                ])
            // A filter on the same table restricts the groups (auto-exist); the Safety products
            // are the Helmet (Red, 160) and the Lock (Black, 15).
            testCase "a filter table on the same table" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"Safety\" }, Product[Category] ), \"A\", [Amount] )"
                    [ [ "Red"; 160 ]; [ "Black"; 15 ] ]

                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"Safety\" }, Product[Category] ) )"
                    [ [ "Red" ]; [ "Black" ] ])
            // "Filters in SUMMARIZECOLUMNS only apply to group-by columns from the same table and
            // to measures": Bob's rows are 2 (Red, 100) and 5 (no product, 40).
            testCase "a filter table on another table filters the measures" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"UK\" }, Customer[Country] ), \"A\", [Amount] )"
                    [ [ "Red"; 100 ]; [ null; 40 ] ]

                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"UK\" }, Customer[Country] ) )")
                        .Length
                    4
                    "without a measure the other table's filter doesn't touch the groups")
            // "SUMMARIZECOLUMNS always combines all the filters on the same table into a single
            // filter" (SQLBI, "Understanding DAX auto-exist"). In the Red group the combined filter
            // is { ( Red, Safety ) }: removing the category leaves Red (570); removing the colour
            // leaves Safety (175).
            testCase "the group and the filter table are one filter" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"Safety\" }, Product[Category] ), \"NoCat\", CALCULATE ( [Amount], ALL ( Product[Category] ) ), \"NoColor\", CALCULATE ( [Amount], ALL ( Product[Color] ) ) )"
                    [ [ "Red"; 570; 175 ]; [ "Black"; 15; 175 ] ])
            testCase "no group-by column: one row" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( \"A\", [Amount], \"U\", [Units] )" [ [ 625; 17 ] ])
            // "GroupBy columns within SummarizeColumns will no longer be filtered by columns from
            // external tables, even if a relationship exists between them" (Microsoft Learn,
            // SUMMARIZECOLUMNS, "SelfValue semantics"); the expressions are filtered. Bob (UK) has
            // rows 2 (Red, 100) and 5 (no product, 40); the Safety products are Red and Black.
            testCase "under an outer filter" (fun () ->
                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Color], \"A\", [Amount] ), Customer[Country] = \"UK\" )"
                    [ [ "Red"; 100 ]; [ null; 40 ] ]

                // A filter on the group-by table restricts the groups.
                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Color] ), Product[Category] = \"Safety\" )"
                    [ [ "Red" ]; [ "Black" ] ]

                // A filter on a related table does not: every customer key of Sales, though only
                // customer 3 bought the Black product.
                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Sales[CustomerKey] ), Product[Color] = \"Black\" )"
                    [ [ 1 ]; [ 2 ]; [ 3 ] ]

                // With an expression, only the groups where it isn't BLANK are left.
                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Sales[CustomerKey], \"A\", [Amount] ), Product[Color] = \"Black\" )"
                    [ [ 3; 15 ] ])
            // "normal filters (which override existing filters) and filters with KeepFilters
            // specified" (Microsoft Learn).
            testCase "a filter argument replaces the outer filter on its column, unless under KEEPFILTERS" (fun () ->
                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Color], TREATAS ( { \"Red\" }, Product[Color] ), \"A\", [Amount] ), Product[Color] = \"Black\" )"
                    [ [ "Red"; 570 ] ]

                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Color], KEEPFILTERS ( TREATAS ( { \"Red\" }, Product[Color] ) ), \"A\", [Amount] ), Product[Color] = \"Black\" )"
                    []

                returns
                    (p ())
                    "EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( Product[Color], KEEPFILTERS ( TREATAS ( { \"Red\", \"Black\" }, Product[Color] ) ), \"A\", [Amount] ), Product[Color] = \"Black\" )"
                    [ [ "Black"; 15 ] ])
            // "contextual SummarizeColumns ... allows SummarizeColumns to be evaluated in any context
            // transition" (Microsoft Learn). Colours with an amount: Ann (rows 1, 3, 7) Red; Bob
            // (2, 5) Red and no product; Cy (4, 6) Black and Red.
            testCase "in a measure, after a context transition" (fun () ->
                returns
                    (p ())
                    "DEFINE MEASURE Sales[Colours] = COUNTROWS ( SUMMARIZECOLUMNS ( Product[Color], \"A\", [Amount] ) )
                     EVALUATE ADDCOLUMNS ( VALUES ( Customer[Name] ), \"N\", [Colours] )"
                    [ [ "Ann"; 1 ]; [ "Bob"; 2 ]; [ "Cy"; 2 ] ]

                returns
                    (p ())
                    "EVALUATE ROW ( \"x\", SUMX ( VALUES ( Customer[Name] ), CALCULATE ( COUNTROWS ( SUMMARIZECOLUMNS ( Product[Color], \"A\", [Amount] ) ) ) ) )"
                    [ [ 5 ] ])
            // "SUMMARIZECOLUMNS ( year, color, Sales )" is "SUMMARIZE ( Sales, year, color )" (SQLBI,
            // "Introducing SUMMARIZECOLUMNS"): the table has the columns of its expanded table, so
            // it pairs the two group-by tables. Rows 1, 3, 6, 7 are Red in the US (200 + 60 + 100 +
            // 110), row 2 Red in the UK, row 4 Black in the US, row 5 no product in the UK.
            testCase "a model table as a filter pairs the group-by tables it relates to" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], Sales, \"A\", [Amount] )"
                    [
                        [ "Red"; "US"; 470 ]
                        [ "Red"; "UK"; 100 ]
                        [ "Black"; "US"; 15 ]
                        [ null; "UK"; 40 ]
                    ]

                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], Sales )" [
                    [ "Red"; "US" ]
                    [ "Red"; "UK" ]
                    [ "Black"; "US" ]
                    [ null; "UK" ]
                ]

                // The same as SUMMARIZE.
                Expect.equal
                    (sortedRows (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], Sales )")
                    (sortedRows (p ()) "EVALUATE SUMMARIZE ( Sales, Product[Color], Customer[Country] )")
                    ""

                // Only the Sales rows a filter on one of the tables leaves.
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], Sales, TREATAS ( { \"UK\" }, Customer[Country] ) )"
                    [ [ "Red"; "UK" ]; [ null; "UK" ] ])
            // "Rows for which all expressions not using IGNORE return BLANK/NULL will be excluded
            // independent of whether the expressions which do use IGNORE evaluate to BLANK/NULL or
            // not"; with every expression ignored, "they're included since there are no unignored
            // expressions which return blank" (Microsoft Learn).
            testCase "IGNORE" (fun () ->
                returns (p ()) "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"A\", IGNORE ( [Amount] ) )" [
                    [ "Red"; 570 ]
                    [ "Black"; 15 ]
                    [ "Blue"; null ]
                    [ null; 40 ]
                ]

                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"A\", IGNORE ( [Amount] ), \"Big\", IF ( [Amount] > 100, 1 ) )"
                    [ [ "Red"; 570; 1 ] ]

                Expect.equal
                    (queryCode (p ()) "EVALUATE ROW ( \"x\", IGNORE ( 1 ) )")
                    "dax-invalid"
                    "only in SUMMARIZECOLUMNS")
            // dax.guide/rollupaddissubtotal: the subtotal row has a blank group and TRUE in the
            // named column; the result is the group-by columns, the subtotal columns, the
            // expressions.
            testCase "ROLLUPADDISSUBTOTAL" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Product[Color], \"T\" ), \"A\", [Amount] )"
                    [
                        [ "Red"; false; 570 ]
                        [ "Black"; false; 15 ]
                        [ null; false; 40 ]
                        [ null; true; 625 ]
                    ]

                Expect.equal
                    ((query
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Customer[Country], \"CT\" ), ROLLUPADDISSUBTOTAL ( Product[Category], \"PT\" ), \"A\", [Amount] )")
                         .Columns
                     |> List.map _.Name)
                    [ "Customer[Country]"; "Product[Category]"; "[CT]"; "[PT]"; "[A]" ]
                    "columns"

                // The filters stay on a subtotal: the Safety products only (60 + 100 + 15).
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Product[Color], \"T\" ), TREATAS ( { \"Safety\" }, Product[Category] ), \"A\", [Amount] )"
                    [ [ "Red"; false; 160 ]; [ "Black"; false; 15 ]; [ null; true; 175 ] ])
            // Cycling is rows 1, 2, 7 (410), Safety rows 3, 4, 6 (175), no product row 5 (40); the
            // US is 485 and the UK 140. Two calls roll up independently ("matrices of subtotals").
            testCase "several ROLLUPADDISSUBTOTAL" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Customer[Country], \"CT\" ), ROLLUPADDISSUBTOTAL ( Product[Category], \"PT\" ), \"A\", [Amount] )"
                    [
                        [ "US"; "Cycling"; false; false; 310 ]
                        [ "US"; "Safety"; false; false; 175 ]
                        [ "UK"; "Cycling"; false; false; 100 ]
                        [ "UK"; null; false; false; 40 ]
                        [ "US"; null; false; true; 485 ]
                        [ "UK"; null; false; true; 140 ]
                        [ null; "Cycling"; true; false; 410 ]
                        [ null; "Safety"; true; false; 175 ]
                        [ null; null; true; false; 40 ]
                        [ null; null; true; true; 625 ]
                    ])
            // "multiple ROLLUP columns in the same ROLLUPADDISSUBTOTAL ... including only the group
            // totals at higher levels following the hierarchy of columns specified"
            // (dax.guide/rollupaddissubtotal); ROLLUPGROUP: "rolled together when reporting a
            // subtotal" (Microsoft Learn).
            testCase "levels in one ROLLUPADDISSUBTOTAL, and ROLLUPGROUP" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Customer[Country], \"CT\", Product[Category], \"PT\" ), \"A\", [Amount] )"
                    [
                        [ "US"; "Cycling"; false; false; 310 ]
                        [ "US"; "Safety"; false; false; 175 ]
                        [ "UK"; "Cycling"; false; false; 100 ]
                        [ "UK"; null; false; false; 40 ]
                        [ "US"; null; false; true; 485 ]
                        [ "UK"; null; false; true; 140 ]
                        [ null; null; true; true; 625 ]
                    ]

                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( ROLLUPGROUP ( Customer[Country], Product[Category] ), \"T\" ), \"A\", [Amount] )"
                    [
                        [ "US"; "Cycling"; false; 310 ]
                        [ "US"; "Safety"; false; 175 ]
                        [ "UK"; "Cycling"; false; 100 ]
                        [ "UK"; null; false; 40 ]
                        [ null; null; true; 625 ]
                    ])
            // "ISINSCOPE checks whether there is a filter placed on the column derived either from
            // context transition or from a group by column placed by SUMMARIZECOLUMNS"
            // (dax.guide/isinscope); "at the grand total ... Brand neither has one value, nor is it
            // in scope" (SQLBI, "Distinguishing HASONEVALUE from ISINSCOPE").
            testCase "ISINSCOPE" (fun () ->
                returns
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Product[Color], \"T\" ), \"S\", ISINSCOPE ( Product[Color] ), \"C\", ISINSCOPE ( Product[Category] ), \"X\", CALCULATE ( ISINSCOPE ( Product[Color] ), ALL ( Product[Color] ) ) )"
                    [
                        [ "Red"; false; true; false; false ]
                        [ "Black"; false; true; false; false ]
                        [ "Blue"; false; true; false; false ]
                        [ null; false; true; false; false ]
                        [ null; true; false; false; false ]
                    ]

                // A filter from a context transition counts; the measure sees it too.
                returns
                    (p ())
                    "DEFINE MEASURE Sales[In] = ISINSCOPE ( Customer[Country] )
                     EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"a\", CALCULATE ( ISINSCOPE ( Customer[Country] ) ), \"b\", [In], \"c\", CALCULATE ( [In], Customer[Country] = \"US\" ) )"
                    [ [ "US"; true; true; false ]; [ "UK"; true; true; false ] ]

                returns (p ()) "EVALUATE ROW ( \"x\", ISINSCOPE ( Product[Color] ) )" [ [ false ] ]

                notSupported
                    (p ())
                    "EVALUATE ADDCOLUMNS ( VALUES ( Product[Color] ), \"s\", ISINSCOPE ( Product[Color] ) )"
                    "ISINSCOPE inside a row context"

                // The transition over Sales also filters Product: whether that counts is not said.
                notSupported
                    (p ())
                    "EVALUATE ROW ( \"x\", SUMX ( Sales, IF ( CALCULATE ( ISINSCOPE ( Product[Color] ) ), 1 ) ) )"
                    "related")
            testCase "what is not supported says so" (fun () ->
                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Product[Color], \"s\", VALUES ( Product[Color] ) ), \"A\", [Amount] )"
                    "ROLLUPADDISSUBTOTAL with a filter"

                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], NONVISUAL ( VALUES ( Product[Color] ) ), \"A\", [Amount] )"
                    "NONVISUAL"

                notSupported
                    (p ())
                    "EVALUATE ROW ( \"x\", SUMX ( Product, COUNTROWS ( SUMMARIZECOLUMNS ( Product[Color] ) ) ) )"
                    "SUMMARIZECOLUMNS inside a row context"

                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], KEEPFILTERS ( VALUES ( Product[Color] ) ), VALUES ( Product[Category] ) )"
                    "KEEPFILTERS"

                // A filter that pairs columns of two group-by tables and isn't a model table.
                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], TREATAS ( { ( \"Red\", \"US\" ) }, Product[Color], Customer[Country] ) )"
                    "more than one"

                // No group and nothing to show: is the one row returned?
                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( \"A\", CALCULATE ( [Amount], Product[Color] = \"Blue\" ) )"
                    "total row")
        ]

        testList "other table functions" [
            // "If there is a tie ... then all tied rows are returned"; "If N_Value is 0 (zero) or
            // less than 0, then TOPN returns an empty table" (dax.guide/topn).
            testCase "TOPN" (fun () ->
                returns (p ()) "EVALUATE SELECTCOLUMNS ( TOPN ( 2, Product, [Amount] ), \"N\", Product[Name] )" [
                    [ "Bike" ]
                    [ "Helmet" ]
                ]

                returns (p ()) "EVALUATE TOPN ( 1, VALUES ( Customer[Name] ), Customer[Name], ASC )" [ [ "Ann" ] ]

                returns (p ()) "EVALUATE TOPN ( 1, { ( 1, \"a\" ), ( 1, \"b\" ), ( 0, \"c\" ) }, [Value1] )" [
                    [ 1; "a" ]
                    [ 1; "b" ]
                ]

                returns
                    (p ())
                    "EVALUATE TOPN ( 1, { ( 1, \"a\" ), ( 1, \"b\" ), ( 0, \"c\" ) }, [Value1], DESC, [Value2], DESC )"
                    [ [ 1; "b" ] ]

                returns (p ()) "EVALUATE TOPN ( 0, Customer )" []
                Expect.equal (rows (p ()) "EVALUATE TOPN ( 10, Customer )").Length 3 "fewer rows than asked"
                notSupported (p ()) "EVALUATE TOPN ( 1, { -1, BLANK () }, [Value] )" "BLANK")
            testCase "UNION, INTERSECT, EXCEPT" (fun () ->
                returns (p ()) "EVALUATE UNION ( { 1, 2 }, { 2 } )" [ [ 1 ]; [ 2 ]; [ 2 ] ]

                Expect.equal
                    (columns (query (p ()) "EVALUATE UNION ( ROW ( \"a\", 1 ), ROW ( \"b\", 2.5 ) )"))
                    [ "[a]", "double" ]
                    "names of the first"

                returns (p ()) "EVALUATE INTERSECT ( VALUES ( Product[Color] ), { \"Red\", \"Green\" } )" [ [ "Red" ] ]
                // "it and all duplicates in tableExpression1 are included".
                returns (p ()) "EVALUATE INTERSECT ( { 1, 1, 2 }, { 1 } )" [ [ 1 ]; [ 1 ] ]
                returns (p ()) "EVALUATE EXCEPT ( VALUES ( Customer[Name] ), { \"Bob\" } )" [ [ "Ann" ]; [ "Cy" ] ]
                // "data comparison with no type coercion": the whole number 1 is not the decimal 1.
                returns (p ()) "EVALUATE INTERSECT ( { 1 }, { 1.0 } )" []
                Expect.equal (queryCode (p ()) "EVALUATE UNION ( { 1 }, { ( 1, 2 ) } )") "dax-invalid" "column counts")
            // "Tables are joined on common columns (by name)"; "If the two tables have no common
            // column names, an error is returned"; "Columns being joined on must have the same data
            // type in both tables" (Microsoft Learn, NATURALINNERJOIN). "A row of the left table
            // without a match is returned once, with blank values in the columns of the right
            // table" (dax.guide/naturalleftouterjoin).
            testCase "NATURALINNERJOIN and NATURALLEFTOUTERJOIN" (fun () ->
                let left =
                    "DATATABLE ( \"K\", INTEGER, \"A\", STRING, { { 1, \"x\" }, { 2, \"y\" } } )"

                let right =
                    "DATATABLE ( \"K\", INTEGER, \"B\", STRING, { { 2, \"p\" }, { 2, \"q\" }, { 3, \"r\" } } )"

                let inner = $"EVALUATE NATURALINNERJOIN ( {left}, {right} )"
                // K = 2 matches twice; 1 and 3 have no partner.
                returns (p ()) inner [ [ 2; "y"; "p" ]; [ 2; "y"; "q" ] ]

                Expect.equal
                    (columns (query (p ()) inner))
                    [ "[K]", "int64"; "[A]", "string"; "[B]", "string" ]
                    "the key once"

                returns (p ()) $"EVALUATE NATURALLEFTOUTERJOIN ( {left}, {right} )" [
                    [ 1; "x"; null ]
                    [ 2; "y"; "p" ]
                    [ 2; "y"; "q" ]
                ]

                Expect.equal
                    (queryCode (p ()) "EVALUATE NATURALINNERJOIN ( { 1 }, ROW ( \"x\", 1 ) )")
                    "dax-invalid"
                    "no common column"

                Expect.equal
                    (queryCode
                        (p ())
                        "EVALUATE NATURALINNERJOIN ( DATATABLE ( \"K\", INTEGER, { { 1 } } ), DATATABLE ( \"K\", DOUBLE, { { 1 } } ) )")
                    "dax-invalid"
                    "different data types"

                notSupported (p ()) "EVALUATE NATURALINNERJOIN ( Sales, Product )" "lineage"
                notSupported (p ()) "EVALUATE NATURALINNERJOIN ( { 1, BLANK () }, { 1 } )" "BLANK")
            // "all the unique combinations of the columns in the table specified in the first
            // argument that are not listed in the following arguments" (dax.guide/allexcept).
            testCase "ALLEXCEPT as a table" (fun () ->
                returns (p ()) "EVALUATE ALLEXCEPT ( Customer, Customer[CustomerKey], Customer[Name] )" [
                    [ "US" ]
                    [ "UK" ]
                ]
                // No filter applies to it.
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE CALCULATETABLE ( ALLEXCEPT ( Customer, Customer[CustomerKey] ), Customer[Country] = \"UK\" )")
                        .Length
                    3
                    "all of Name, Country"
                // Product has the blank row of Sales' product 9.
                notSupported (p ()) "EVALUATE ALLEXCEPT ( Product, Product[ProductKey] )" "blank row"
                notSupported (p ()) "EVALUATE ALLEXCEPT ( Customer, Customer )" "table"
                notSupported (p ()) "EVALUATE ALLEXCEPT ( Sales, Sales[Id], Product[Color] )" "related")
            // "Returns the first value in the column for which the expression has a non blank
            // value"; a column reference "is replaced by CALCULATETABLE ( DISTINCT ( <ColumnName> )
            // )" (dax.guide/firstnonblank). FIRSTNONBLANKVALUE is "CALCULATE ( <Expression>,
            // FIRSTNONBLANK ( <ColumnName>, CALCULATE ( <Expression> ) ) )"
            // (dax.guide/firstnonblankvalue).
            testCase "FIRSTNONBLANK, LASTNONBLANK and their VALUE forms" (fun () ->
                // Ann has sales (200 + 60 + 110).
                returns (p ()) "EVALUATE FIRSTNONBLANK ( Customer[Name], [Amount] )" [ [ "Ann" ] ]
                // Bike, Helmet, Lock, Pump: Pump has no sales; the blank row is not among the values.
                returns (p ()) "EVALUATE LASTNONBLANK ( Product[Name], [Amount] )" [ [ "Lock" ] ]
                is (p ()) "FIRSTNONBLANKVALUE ( Product[Name], [Amount] )" 410
                is (p ()) "LASTNONBLANKVALUE ( Product[Name], [Amount] )" 15
                // Cy: 15 + 100.
                is (p ()) "LASTNONBLANKVALUE ( Customer[Name], [Amount] )" 115
                // The column's values come after a context transition: per country, its customers.
                returns
                    (p ())
                    "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"f\", FIRSTNONBLANK ( Customer[Name], [Amount] ) )"
                    [ [ "US"; "Ann" ]; [ "UK"; "Bob" ] ]
                // A table of one column, in a row context on each value.
                returns (p ()) "EVALUATE FIRSTNONBLANK ( { 3, 1, 2 }, IF ( [Value] > 1, 1 ) )" [ [ 2 ] ]
                returns (p ()) "EVALUATE LASTNONBLANK ( { 3, 1, 2 }, IF ( [Value] < 3, 1 ) )" [ [ 2 ] ]
                returns (p ()) "EVALUATE FIRSTNONBLANK ( Customer[Name], BLANK () )" []
                notSupported (p ()) "EVALUATE FIRSTNONBLANK ( Customer[Name] = \"Ann\", 1 )" "Boolean"
                notSupported (p ()) "EVALUATE FIRSTNONBLANK ( { 1, \"a\" }, 1 )" "different data types")
            // "Column names from table arguments must all be different in all tables or an error
            // is returned" (Microsoft Learn, CROSSJOIN).
            testCase "CROSSJOIN" (fun () ->
                Expect.equal
                    (rows (p ()) "EVALUATE CROSSJOIN ( VALUES ( Customer[Country] ), DISTINCT ( Product[Category] ) )")
                        .Length
                    4
                    "2 x 2"

                Expect.equal
                    (queryCode (p ()) "EVALUATE CROSSJOIN ( { 1, 2 }, { 3 } )")
                    "dax-invalid"
                    "both have a Value column")
            // GENERATE drops a row whose second table is empty; GENERATEALL keeps it with BLANKs
            // (Microsoft Learn, GENERATE and GENERATEALL).
            testCase "GENERATE and GENERATEALL" (fun () ->
                returns
                    (p ())
                    "EVALUATE GENERATE ( VALUES ( Customer[Country] ), CALCULATETABLE ( VALUES ( Customer[Name] ) ) )"
                    [ [ "US"; "Ann" ]; [ "US"; "Cy" ]; [ "UK"; "Bob" ] ]

                let second = "FILTER ( SELECTCOLUMNS ( { 10 }, \"B\", [Value] ), [A] = 1 )"

                returns (p ()) $"EVALUATE GENERATE ( SELECTCOLUMNS ( {{ 1, 2 }}, \"A\", [Value] ), {second} )" [
                    [ 1; 10 ]
                ]

                returns (p ()) $"EVALUATE GENERATEALL ( SELECTCOLUMNS ( {{ 1, 2 }}, \"A\", [Value] ), {second} )" [
                    [ 1; 10 ]
                    [ 2; null ]
                ])
            testCase "DATATABLE" (fun () ->
                let r =
                    query
                        (p ())
                        "EVALUATE DATATABLE ( \"a\", INTEGER, \"b\", STRING, \"c\", CURRENCY, { { 1, \"x\", 1.5 }, { 2, \"y\", BLANK () } } )"

                Expect.equal (columns r) [ "[a]", "int64"; "[b]", "string"; "[c]", "decimal" ] "columns"
                Expect.equal (r.Rows |> List.map List.ofArray) (expected [ [ 1; "x"; 1.5m ]; [ 2; "y"; null ] ]) "rows"

                Expect.equal
                    (queryCode (p ()) "EVALUATE DATATABLE ( \"a\", INTEGER, { { 1 + 1 } } )")
                    "dax-invalid"
                    "constants only")
            // Qty per customer: Ann 2 + 3 + 1, Bob 1 + 4, Cy 1 + 5. Highest price per colour: Red
            // 110, Black 15, no product 10.
            testCase "GROUPBY and CURRENTGROUP" (fun () ->
                returns
                    (p ())
                    "EVALUATE GROUPBY ( Sales, Sales[CustomerKey], \"Q\", SUMX ( CURRENTGROUP (), Sales[Qty] ) )"
                    [ [ 1; 6 ]; [ 2; 5 ]; [ 3; 6 ] ]

                returns
                    (p ())
                    "EVALUATE GROUPBY ( Sales, Product[Color], \"M\", MAXX ( CURRENTGROUP (), Sales[Price] ) )"
                    [ [ "Red"; 110 ]; [ "Black"; 15 ]; [ null; 10 ] ]

                notSupported (p ()) "EVALUATE GROUPBY ( Sales, Sales[CustomerKey], \"A\", [Amount] )" "CURRENTGROUP"

                // "GROUPBY is useful to group by columns with no lineage" (dax.guide/groupby). Qty is
                // 2, 1, 3, 1, 4, 5, 1: above 1 in rows 1, 3, 5, 6 (2 + 3 + 4 + 5), not in 2, 4, 7.
                returns
                    (p ())
                    "EVALUATE GROUPBY ( ADDCOLUMNS ( Sales, \"Size\", IF ( Sales[Qty] > 1, \"Large\", \"Small\" ) ), [Size], \"Q\", SUMX ( CURRENTGROUP (), Sales[Qty] ), \"N\", COUNTX ( CURRENTGROUP (), 1 ) )"
                    [ [ "Large"; 14; 4 ]; [ "Small"; 3; 3 ] ]

                // "SUMMARIZE has the option of grouping by columns computed in the query"
                // (dax.guide/summarize).
                returns
                    (p ())
                    "EVALUATE SUMMARIZE ( ADDCOLUMNS ( VALUES ( Sales[Qty] ), \"Size\", IF ( Sales[Qty] > 1, \"Large\", \"Small\" ) ), [Size] )"
                    [ [ "Large" ]; [ "Small" ] ]

                Expect.equal
                    (queryCode (p ()) "EVALUATE { COUNTROWS ( CURRENTGROUP () ) }")
                    "dax-invalid"
                    "outside GROUPBY")
        ]

        testList "queries" [
            // "The syntax after COLUMN defines a query column, which has a data lineage and it is
            // populated in an empty filter context before the query is executed"
            // (dax.guide/st/column). Units: Bike rows 1, 2, 7 (2 + 1 + 1), Helmet rows 3, 6 (3 + 5),
            // Lock row 4, Pump none.
            testCase "DEFINE COLUMN" (fun () ->
                returns
                    (p ())
                    "DEFINE COLUMN Product[Sold] = [Units]
                     EVALUATE SELECTCOLUMNS ( Product, \"n\", Product[Name], \"u\", Product[Sold] )"
                    [ [ "Bike"; 4 ]; [ "Helmet"; 8 ]; [ "Lock"; 1 ]; [ "Pump"; null ] ]

                // Computed before the query's filters exist.
                returns
                    (p ())
                    "DEFINE COLUMN Product[Sold] = [Units]
                     EVALUATE CALCULATETABLE ( SELECTCOLUMNS ( Product, \"n\", Product[Name], \"u\", Product[Sold] ), Customer[Country] = \"UK\" )"
                    [ [ "Bike"; 4 ]; [ "Helmet"; 8 ]; [ "Lock"; 1 ]; [ "Pump"; null ] ]

                // No context transition without CALCULATE: the grand total in every row.
                returns
                    (p ())
                    "DEFINE COLUMN Product[All] = SUM ( Sales[Qty] )
                     EVALUATE SELECTCOLUMNS ( Product, \"n\", Product[Name], \"u\", Product[All] )"
                    [ [ "Bike"; 17 ]; [ "Helmet"; 17 ]; [ "Lock"; 17 ]; [ "Pump"; 17 ] ]

                // A column like any other: grouped by, filtered on. The Bike is 410, the Helmet and
                // the Lock 175, and row 5 has no product.
                returns
                    (p ())
                    "DEFINE COLUMN Product[Kind] = IF ( Product[Name] = \"Bike\", \"B\", \"S\" )
                     EVALUATE SUMMARIZECOLUMNS ( Product[Kind], \"A\", [Amount] )"
                    [ [ "B"; 410 ]; [ "S"; 175 ]; [ null; 40 ] ]

                returns
                    (p ())
                    "DEFINE COLUMN Product[Kind] = IF ( Product[Name] = \"Bike\", \"B\", \"S\" )
                            MEASURE Sales[Small] = CALCULATE ( [Amount], Product[Kind] = \"S\" )
                     EVALUATE ROW ( \"x\", [Small] )"
                    [ [ 175 ] ])
            // "Definitions can reference other definitions that appear before or after the current
            // definition" (Microsoft Learn, "DAX queries").
            testCase "a query column uses a query measure defined after it, and another query column" (fun () ->
                returns
                    (p ())
                    "DEFINE COLUMN Product[Twice] = [Q2] + Product[One]
                            COLUMN Product[One] = 1
                            MEASURE Sales[Q2] = SUM ( Sales[Qty] ) * 2
                     EVALUATE SELECTCOLUMNS ( Product, \"n\", Product[Name], \"u\", Product[Twice] )"
                    [ [ "Bike"; 9 ]; [ "Helmet"; 17 ]; [ "Lock"; 3 ]; [ "Pump"; 1 ] ])
            // "The syntax after TABLE defines a query table, which has a data lineage" of its own
            // (dax.guide/st/table): it doesn't filter Product unless TREATAS says so.
            testCase "DEFINE TABLE" (fun () ->
                let result =
                    query (p ()) "DEFINE TABLE Colors = DISTINCT ( Product[Color] ) EVALUATE Colors"

                Expect.equal (result.Columns |> List.map _.Name) [ "Colors[Color]" ] "its columns are its own"

                returns (p ()) "DEFINE TABLE Colors = DISTINCT ( Product[Color] ) EVALUATE Colors" [
                    [ "Red" ]
                    [ "Black" ]
                    [ "Blue" ]
                ]

                returns
                    (p ())
                    "DEFINE TABLE Colors = DISTINCT ( Product[Color] )
                     EVALUATE ADDCOLUMNS ( Colors, \"A\", [Amount] )"
                    [ [ "Red"; 625 ]; [ "Black"; 625 ]; [ "Blue"; 625 ] ]

                returns
                    (p ())
                    "DEFINE TABLE Colors = DISTINCT ( Product[Color] )
                     EVALUATE ADDCOLUMNS ( Colors, \"A\", CALCULATE ( [Amount], TREATAS ( { Colors[Color] }, Product[Color] ) ) )"
                    [ [ "Red"; 570 ]; [ "Black"; 15 ]; [ "Blue"; null ] ]

                // A measure on the query table; the table uses a model measure. Only the Pump has
                // no order.
                returns
                    (p ())
                    "DEFINE TABLE Unsold = FILTER ( Product, [Orders] + 0 = 0 )
                            MEASURE Unsold[N] = COUNTROWS ( Unsold )
                     EVALUATE { [N] }"
                    [ [ 1 ] ])
            testCase "what a query defines is the query's own: the prepared model doesn't change" (fun () ->
                let prepared = p ()

                returns prepared "DEFINE COLUMN Product[One] = 1 EVALUATE ROW ( \"x\", SUM ( Product[One] ) )" [
                    [ 4 ]
                ]

                Expect.equal (queryCode prepared "EVALUATE ROW ( \"x\", SUM ( Product[One] ) )") "dax-invalid" "gone"

                returns prepared "DEFINE TABLE T2 = Product EVALUATE ROW ( \"x\", COUNTROWS ( T2 ) )" [ [ 4 ] ]
                Expect.equal (queryCode prepared "EVALUATE T2") "dax-invalid" "gone"
                Expect.equal (rows prepared "EVALUATE Product").Head.Length 4 "Product has its four columns")
            testCase "DEFINE COLUMN and DEFINE TABLE: errors and refusals" (fun () ->
                notSupported (p ()) "DEFINE COLUMN Product[Name] = 1 EVALUATE Product" "already has a column"
                notSupported (p ()) "DEFINE TABLE Product = Customer EVALUATE Product" "already has a table"

                Expect.equal
                    (queryCode (p ()) "DEFINE COLUMN Nope[x] = 1 EVALUATE Product")
                    "dax-invalid"
                    "no such table"

                match
                    queryError
                        (p ())
                        "DEFINE COLUMN Product[A] = Product[B] + 1 COLUMN Product[B] = Product[A] + 1 EVALUATE Product"
                with
                | DaxInvalid(message, _) -> Expect.stringContains message "circular" ""
                | other -> failtestf "%A" other

                // A query column that fails is an error only for the queries that read it.
                returns
                    (p ())
                    "DEFINE COLUMN Product[Bad] = ERROR ( \"no\" ) EVALUATE ROW ( \"x\", COUNTROWS ( Customer ) )"
                    [ [ 3 ] ]

                Expect.equal
                    (queryCode (p ()) "DEFINE COLUMN Product[Bad] = ERROR ( \"no\" ) EVALUATE VALUES ( Product[Bad] )")
                    "dax-error"
                    ""

                notSupported (p ()) "DEFINE VAR v = 1 COLUMN Product[C] = v EVALUATE Product" "uses the query variable")
            // The model's own calculated column and table are taken as `prepare` computed them.
            testCase "DEFINE COLUMN in a model with calculated columns and tables" (fun () ->
                let prepared =
                    prepare {
                        star with
                            Tables =
                                (star.Tables
                                 |> List.map (fun t ->
                                     if t.Name = "Product" then
                                         t |> withCalculated [ "Double", DataType.Int64, "Product[ProductKey] * 2" ]
                                     else
                                         t))
                                @ [
                                    calculatedTable
                                        "Colours"
                                        [ "Color", DataType.String ]
                                        "DISTINCT ( Product[Color] )"
                                ]
                    }

                returns
                    prepared
                    "DEFINE COLUMN Product[Triple] = Product[Double] + Product[ProductKey]
                     EVALUATE SELECTCOLUMNS ( Product, \"t\", Product[Triple] )"
                    [ [ 3 ]; [ 6 ]; [ 9 ]; [ 12 ] ]

                returns prepared "DEFINE COLUMN Product[Z] = 1 EVALUATE Colours" [ [ "Red" ]; [ "Black" ]; [ "Blue" ] ]

                returns prepared "DEFINE COLUMN Colours[Len] = IF ( Colours[Color] = \"Red\", 3, 0 ) EVALUATE Colours" [
                    [ "Red"; 3 ]
                    [ "Black"; 0 ]
                    [ "Blue"; 0 ]
                ])
            testCase "ORDER BY" (fun () ->
                returnsInOrder (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name] DESC" [
                    [ "Cy" ]
                    [ "Bob" ]
                    [ "Ann" ]
                ]

                returnsInOrder (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name]" [
                    [ "Ann" ]
                    [ "Bob" ]
                    [ "Cy" ]
                ]

                returnsInOrder
                    (p ())
                    "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"A\", [Amount] ) ORDER BY [A] DESC"
                    [ [ "US"; 485 ]; [ "UK"; 140 ] ]

                // Two keys: by country, then by amount downwards. Ann 370 and Cy 115 are in the US.
                returnsInOrder
                    (p ())
                    "EVALUATE ADDCOLUMNS ( SELECTCOLUMNS ( Customer, Customer[Country], Customer[Name] ), \"A\", [Amount] ) ORDER BY Customer[Country], [A] DESC"
                    [ [ "UK"; "Bob"; 140 ]; [ "US"; "Ann"; 370 ]; [ "US"; "Cy"; 115 ] ])
            testCase "ORDER BY where BLANK meets zero or negative values is refused" (fun () ->
                notSupported (p ()) "EVALUATE { -1, BLANK (), 2 } ORDER BY [Value]" "BLANK"
                returnsInOrder (p ()) "EVALUATE { 3, BLANK (), 2 } ORDER BY [Value]" [ [ null ]; [ 2 ]; [ 3 ] ])
            testCase "several EVALUATE statements give several results" (fun () ->
                match Engine.evaluateQuery (p ()) "EVALUATE { 1 } EVALUATE { 2, 3 }" with
                | Ok [ a; b ] ->
                    Expect.equal a.Rows.Length 1 "first"
                    Expect.equal b.Rows.Length 2 "second"
                | other -> failtestf "%A" other)
            // "A query measure overrides a model measure with the same name. However, its
            // definition is only used in other query measures; other model measures continue to
            // use the model measure definition" (dax.guide/define).
            testCase "DEFINE MEASURE" (fun () ->
                let m =
                    prepare {
                        star with
                            Measures = star.Measures @ [ "Sales", "Twice", "[Units] * 2" ]
                    }

                returns m "DEFINE MEASURE Sales[Avg] = DIVIDE ( [Amount], [Units] ) EVALUATE ROW ( \"v\", [Avg] )" [
                    [ 625.0 / 17.0 ]
                ]

                returns
                    m
                    "DEFINE MEASURE Sales[Units] = 1 MEASURE Sales[Mine] = [Units] + 1 EVALUATE ROW ( \"u\", [Units], \"mine\", [Mine], \"twice\", [Twice] )"
                    [ [ 1; 2; 34 ] ])
            testCase "DEFINE VAR and DEFINE FUNCTION" (fun () ->
                returns (p ()) "DEFINE VAR x = 5 VAR y = x * 2 EVALUATE ROW ( \"v\", x + y )" [ [ 15 ] ]
                returns (p ()) "DEFINE FUNCTION Twice = ( a ) => a * 2 EVALUATE ROW ( \"v\", Twice ( 21 ) )" [ [ 42 ] ]

                returns
                    (p ())
                    "DEFINE VAR t = FILTER ( Customer, Customer[Country] = \"US\" ) EVALUATE ROW ( \"n\", COUNTROWS ( t ) )"
                    [ [ 2 ] ])
            // "defines the rows to skip before the specified values according to the sort order
            // specified in the ORDER BY section" (dax.guide/st/start-at); "The second argument in
            // START AT defines the starting value in column 2 of the ORDER BY columns within the
            // rows that meet the first value for column 1"; "There can be as many arguments in
            // START AT as there are in ORDER BY, but not more" (Microsoft Learn, START AT).
            testCase "START AT" (fun () ->
                returnsInOrder (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name] START AT \"B\"" [
                    [ "Bob" ]
                    [ "Cy" ]
                ]
                // A value that is not in the result: Bob sorts before "Bz".
                returnsInOrder (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name] START AT \"Bz\"" [
                    [ "Cy" ]
                ]
                // (UK, Bob) and (US, Ann) come before (US, "B").
                returnsInOrder
                    (p ())
                    "EVALUATE ALL ( Customer[Country], Customer[Name] ) ORDER BY Customer[Country], Customer[Name] START AT \"US\", \"B\""
                    [ [ "US"; "Cy" ] ]
                // Only the first key: every US row.
                returnsInOrder
                    (p ())
                    "EVALUATE ALL ( Customer[Country], Customer[Name] ) ORDER BY Customer[Country], Customer[Name] START AT \"US\""
                    [ [ "US"; "Ann" ]; [ "US"; "Cy" ] ]
                // Descending: Pump, Lock, Helmet, Bike.
                returnsInOrder
                    (p ())
                    "EVALUATE DISTINCT ( Product[Name] ) ORDER BY Product[Name] DESC START AT \"Helmet\""
                    [ [ "Helmet" ]; [ "Bike" ] ]

                Expect.equal
                    (queryCode (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name] START AT \"A\", 1")
                    "dax-invalid"
                    "more values than keys"

                notSupported (p ()) "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name] START AT 1" "data type"
                notSupported (p ()) "EVALUATE VALUES ( Product[Name] ) ORDER BY Product[Name] START AT \"B\"" "BLANK")
            testCase "what a query can't have yet" (fun () ->
                notSupported (p ()) "EVALUATE ROW ( \"v\", @p )" "@p"
                notSupported (p ()) "EVALUATE ROW ( \"v\", PATHITEM ( 1, 1 ) )" "PATHITEM"
                notSupported (p ()) "EVALUATE INDEX ( 1, ROWS )" "visual")
            testCase "a syntax error in a query" (fun () ->
                match queryError (p ()) "EVALUATE FILTER ( Sales, " with
                | DaxSyntax("query", 1, _, _) -> ()
                | other -> failtestf "%A" other)
        ]

        testList "calculated columns and tables" [
            testCase "are computed when the model is prepared, in dependency order" (fun () ->
                let m =
                    prepare {
                        star with
                            Tables =
                                star.Tables
                                |> List.map (fun t ->
                                    match t.Name with
                                    | "Sales" ->
                                        t |> withCalculated [ "Amount", DataType.Int64, "Sales[Qty] * Sales[Price]" ]
                                    | "Product" ->
                                        t
                                        |> withCalculated [
                                            // Defined before the column it needs.
                                            "Double", DataType.Int64, "Product[Units] * 2"
                                            "Units", DataType.Int64, "CALCULATE ( SUM ( Sales[Qty] ) )"
                                            "Share",
                                            DataType.Double,
                                            "DIVIDE ( [Amount], CALCULATE ( [Amount], ALL ( Product ) ) )"
                                        ]
                                    | _ -> t)
                                |> fun tables ->
                                    tables
                                    @ [
                                        calculatedTable
                                            "ColorTotals"
                                            [ "Color", DataType.String; "Total", DataType.Int64 ]
                                            "SUMMARIZE ( Sales, Product[Color], \"Total\", SUM ( Sales[Amount] ) )"
                                    ]
                    }

                Expect.isEmpty (Engine.failures m) "nothing failed"
                is m "SUM ( Sales[Amount] )" 625

                // Units per product: Bike 2 + 1 + 1, Helmet 3 + 5, Lock 1, Pump none. Share of 625.
                returns
                    m
                    "EVALUATE SELECTCOLUMNS ( Product, \"n\", Product[Name], \"u\", Product[Units], \"d\", Product[Double], \"s\", Product[Share] )"
                    [
                        [ "Bike"; 4; 8; 410.0 / 625.0 ]
                        [ "Helmet"; 8; 16; 160.0 / 625.0 ]
                        [ "Lock"; 1; 2; 15.0 / 625.0 ]
                        [ "Pump"; null; null; null ]
                    ]

                returns m "EVALUATE ColorTotals" [ [ "Red"; 570 ]; [ "Black"; 15 ]; [ null; 40 ] ]
                is m "SUMX ( ColorTotals, ColorTotals[Total] )" 625
                // A calculated column filters like any other.
                is m "CALCULATE ( [Amount], Product[Units] > 3 )" 570)
            testCase "a circular dependency fails prepare" (fun () ->
                let db, store =
                    build {
                        star with
                            Tables =
                                star.Tables
                                |> List.map (fun t ->
                                    if t.Name = "Customer" then
                                        t
                                        |> withCalculated [
                                            "A", DataType.Int64, "Customer[B] + 1"
                                            "B", DataType.Int64, "Customer[A] + 1"
                                        ]
                                    else
                                        t)
                    }

                match Engine.prepare db store with
                | Error(DaxInvalid(message, _)) -> Expect.stringContains message "circular dependency" "message"
                | other -> failtestf "%A" other)
            // "CALCULATE in a calculated column performs a context transition and makes that column
            // dependent on all the columns in the table. If two such columns exist, they depend on
            // each other" (SQLBI, "Avoiding circular dependency errors in DAX"); a table with a
            // unique column doesn't have the problem.
            testCase "two context transitions in a table without a unique column are circular" (fun () ->
                let dup (calculated: (string * DataType * string) list) = {
                    Tables = [
                        table "Dup" [ "V", DataType.Int64 ] [ [ 1 ]; [ 1 ]; [ 2 ] ]
                        |> withCalculated calculated
                    ]
                    Rels = []
                    Measures = []
                    Functions = []
                }

                // One such column is fine: each 1 sees both 1s.
                let one = prepare (dup [ "S", DataType.Int64, "CALCULATE ( SUM ( Dup[V] ) )" ])
                returns one "EVALUATE Dup" [ [ 1; 2 ]; [ 1; 2 ]; [ 2; 2 ] ]

                let db, store =
                    build (
                        dup [
                            "S", DataType.Int64, "CALCULATE ( SUM ( Dup[V] ) )"
                            "N", DataType.Int64, "CALCULATE ( COUNTROWS ( Dup ) )"
                        ]
                    )

                match Engine.prepare db store with
                | Error(DaxInvalid(message, _)) -> Expect.stringContains message "circular dependency" "message"
                | other -> failtestf "%A" other)
            // With a key (a unique column on the one side of a relationship), a filter argument that
            // replaces part of the transition's filter keeps the same row, so the columns not
            // computed yet aren't needed (the sample model's Date table does this, and Power BI
            // computes it).
            testCase "filter arguments over a transition in a table with a key are not circular" (fun () ->
                let days (calculated: (string * DataType * string) list) = {
                    Tables = [
                        table "Days" [ "K", DataType.Int64; "Flag", DataType.Int64 ] [ [ 1; 1 ]; [ 2; 0 ]; [ 3; 0 ] ]
                        |> withCalculated calculated
                        table "Fact" [ "K", DataType.Int64 ] [ [ 1 ] ]
                    ]
                    Rels = [ rel ("Fact", "K") ("Days", "K") ]
                    Measures = []
                    Functions = []
                }

                let m =
                    prepare (
                        days [
                            "A", DataType.Int64, "CALCULATE ( COUNTROWS ( Days ), Days[Flag] <> 1 )"
                            "B", DataType.Int64, "CALCULATE ( COUNTROWS ( Days ), ALL ( Days[Flag] ) )"
                            "C", DataType.Int64, "Days[A]"
                        ]
                    )

                Expect.isEmpty (Engine.failures m) "every column is computed"
                returns m "EVALUATE Days" [ [ 1; 1; null; 1; null ]; [ 2; 0; 1; 1; 1 ]; [ 3; 0; 1; 1; 1 ] ]

                // Without the unique column the left-out columns could change the rows: refused.
                let refused =
                    prepare (days [ "D", DataType.Int64, "CALCULATE ( COUNTROWS ( Days ), ALL ( Days[K] ) )" ])

                match Engine.failures refused with
                | [ (ref, DaxNotSupported(what, _)) ] ->
                    Expect.equal (ObjectRef.format ref) "column:Days/D" "which"
                    Expect.stringContains what "unique column" "what"
                | other -> failtestf "%A" other

                // Unrelated, K is unique only by chance: each column depends on all the others
                // (SQLBI, "Understanding circular dependencies"), unless ALL ( Days[Flag] ) takes
                // the dependency away, which isn't decided: refused.
                let unrelated =
                    prepare {
                        days [
                            "A", DataType.Int64, "CALCULATE ( COUNTROWS ( Days ), Days[Flag] <> 1 )"
                            "B", DataType.Int64, "CALCULATE ( COUNTROWS ( Days ), ALL ( Days[Flag] ) )"
                        ] with
                            Rels = []
                    }

                match Engine.failures unrelated with
                | (_, DaxNotSupported(what, _)) :: _ -> Expect.stringContains what "depend on each other" "what"
                | other -> failtestf "%A" other)
            testCase "a column that can't be computed fails only the queries that read it" (fun () ->
                let m =
                    prepare {
                        star with
                            Tables =
                                star.Tables
                                |> List.map (fun t ->
                                    if t.Name = "Customer" then
                                        t
                                        |> withCalculated [
                                            "Label", DataType.String, "PATHITEM ( Customer[CustomerKey], 1 )"
                                        ]
                                    else
                                        t)
                    }

                match Engine.failures m with
                | [ (ref, DaxNotSupported(what, at)) ] ->
                    Expect.equal (ObjectRef.format ref) "column:Customer/Label" "which"
                    Expect.equal what "the function PATHITEM" "what"
                    Expect.equal at "column 'Customer'[Label] 1:1" "where"
                | other -> failtestf "%A" other

                is m "[Amount]" 625
                is m "CALCULATE ( [Amount], Customer[Country] = \"US\" )" 485
                notSupported m "EVALUATE Customer" "PATHITEM"
                notSupported m "EVALUATE VALUES ( Customer[Label] )" "PATHITEM")
        ]

        testList "model shapes that are refused" [
            testCase "an inactive one-to-one relationship closes the tables connected to it" (fun () ->
                for (fromEnd, toEnd, word) in
                    [ RelationshipEndCardinality.One, RelationshipEndCardinality.One, "one-to-one" ] do
                    let m =
                        prepare {
                            star with
                                Tables = star.Tables @ [ table "Other" [ "K", DataType.Int64 ] [ [ 1 ] ] ]
                                Rels =
                                    star.Rels
                                    |> List.mapi (fun i r ->
                                        if i = 1 then
                                            {
                                                r with
                                                    FromCardinality = fromEnd
                                                    ToCardinality = toEnd
                                                    Active = false
                                            }
                                        else
                                            r)
                        }

                    notSupported m "EVALUATE ROW ( \"v\", [Amount] )" word
                    notSupported m "EVALUATE Product" word
                    // A table with no relationship to them is fine.
                    is m "COUNTROWS ( Other )" 1)
            testCase "a one side with repeated keys" (fun () ->
                let m =
                    prepare {
                        star with
                            Tables =
                                star.Tables
                                |> List.map (fun t ->
                                    if t.Name = "Customer" then
                                        {
                                            t with
                                                Rows = t.Rows @ [ [ 1; "Ann again"; "US" ] ]
                                        }
                                    else
                                        t)
                    }

                notSupported m "EVALUATE ROW ( \"v\", [Amount] )" "repeat")
            // "Inactive relationships are also expanded, even when the relationship isn't used by a
            // calculation ... When a matching value from the "many" to the "one" side doesn't
            // exist, a blank virtual row is added to the "one" side table" (Learn, "Model
            // relationships in Power BI Desktop"). Filters still go through the active one: F's
            // row 2 (B = 7) reaches the blank row only under USERELATIONSHIP.
            testCase "keys without a match on an inactive relationship only give the blank row" (fun () ->
                let m =
                    prepare {
                        Tables = [
                            table "D" [ "K", DataType.Int64 ] [ [ 1 ]; [ 2 ] ]
                            table "F" [ "A", DataType.Int64; "B", DataType.Int64 ] [ [ 1; 1 ]; [ 2; 7 ] ]
                        ]
                        Rels = [ rel ("F", "A") ("D", "K"); inactive (rel ("F", "B") ("D", "K")) ]
                        Measures = []
                        Functions = []
                    }

                returns m "EVALUATE VALUES ( D[K] )" [ [ 1 ]; [ 2 ]; [ null ] ]
                returns m "EVALUATE DISTINCT ( D[K] )" [ [ 1 ]; [ 2 ] ]
                is m "COUNTROWS ( F )" 2
                is m "COUNTROWS ( ALL ( D ) )" 3
                is m "CALCULATE ( COUNTROWS ( F ), ISBLANK ( D[K] ) )" (null: obj)
                is m "CALCULATE ( COUNTROWS ( F ), ISBLANK ( D[K] ), USERELATIONSHIP ( F[B], D[K] ) )" 1
                is m "CALCULATE ( COUNTROWS ( F ), D[K] = 1, USERELATIONSHIP ( F[B], D[K] ) )" 1
                is m "CALCULATE ( COUNTROWS ( F ), D[K] = 2, USERELATIONSHIP ( F[B], D[K] ) )" (null: obj)

                // RELATED reads the active relationship (SQLBI, "Expanded tables in DAX").
                returns m "EVALUATE ADDCOLUMNS ( F, \"R\", RELATED ( D[K] ) )" [ [ 1; 1; 1 ]; [ 2; 7; 2 ] ])
            // Sales -> Product -> Category with a sale of a product that doesn't exist: whether
            // Category gets a blank row of its own is not documented.
            testCase "a blank row at the start of a chain of relationships" (fun () ->
                let snowflake (productRows: obj list list) =
                    prepare {
                        Tables = [
                            table "Category" [ "CategoryKey", DataType.Int64; "Name", DataType.String ] [
                                [ 1; "Cycling" ]
                            ]
                            table "Product" [ "ProductKey", DataType.Int64; "CategoryKey", DataType.Int64 ] productRows
                            table "Sales" [ "ProductKey", DataType.Int64; "Qty", DataType.Int64 ] [
                                [ 1; 2 ]
                                [ 9; 3 ]
                            ]
                        ]
                        Rels = [
                            rel ("Sales", "ProductKey") ("Product", "ProductKey")
                            rel ("Product", "CategoryKey") ("Category", "CategoryKey")
                        ]
                        Measures = []
                        Functions = []
                    }

                notSupported (snowflake [ [ 1; 1 ] ]) "EVALUATE VALUES ( Category[Name] )" "blank row"

                // When Category has a blank row anyway (a product with no category), the sale
                // without a product lands on it: the sources do settle that its related values
                // are BLANK.
                let m = snowflake [ [ 1; 1 ]; [ 2; 5 ] ]

                returns m "EVALUATE SUMMARIZECOLUMNS ( Category[Name], \"Q\", SUM ( Sales[Qty] ) )" [
                    [ "Cycling"; 2 ]
                    [ null; 3 ]
                ])
            testCase "two active paths between two tables" (fun () ->
                let m =
                    prepare {
                        star with
                            Rels = star.Rels |> List.map (fun r -> { r with Active = true })
                    }

                notSupported m "EVALUATE ROW ( \"v\", [Amount] )" "more than one filter path")
            // No source says whether a both-directions filter that keeps every row still hides the
            // one-side rows nothing refers to (the Pump has no sales).
            testCase "a both-directions filter that keeps every row" (fun () ->
                let m =
                    prepare {
                        star with
                            Rels = star.Rels |> List.mapi (fun i r -> if i = 0 then bothWays r else r)
                    }

                notSupported
                    m
                    "EVALUATE ROW ( \"n\", CALCULATE ( COUNTROWS ( Product ), Sales[Qty] > 0 ) )"
                    "keeps all of its rows")
            // Calculation groups are evaluated now (EngineCalcGroupTests): one with no items is an
            // empty table that changes no measure.
            testCase "a calculation group with no items is an empty table" (fun () ->
                let db, store = build star
                let cg = new Table(Name = "Time Calc", CalculationGroup = new CalculationGroup())
                cg.Columns.Add(new DataColumn(Name = "Name", DataType = DataType.String, SourceColumn = "Name"))
                db.Model.Tables.Add cg

                match Engine.prepare db store with
                | Error e -> failtestf "%A" e
                | Ok m ->
                    is m "[Amount]" 625
                    returns m "EVALUATE 'Time Calc'" [])
        ]

        testList "coverage" [
            testCase "lists what each expression uses that isn't supported" (fun () ->
                let db, _ =
                    build {
                        star with
                            Tables =
                                star.Tables
                                |> List.map (fun t ->
                                    if t.Name = "Customer" then
                                        t
                                        |> withCalculated [
                                            "Label", DataType.String, "PATHITEM ( Customer[Name], 1 )"
                                        ]
                                    else
                                        t)
                            Measures =
                                star.Measures
                                @ [
                                    "Sales", "Formatted", "PATHITEM ( [Amount], 1 )"
                                    "Sales", "Uses Formatted", "[Formatted] & \"!\""
                                    "Sales",
                                    "Selected",
                                    "CALCULATE ( [Amount], KEEPFILTERS ( ALLSELECTED ( Product ) ) )"
                                    "Sales", "Rolled", "COUNTROWS ( SUMMARIZE ( Sales, ROLLUP ( Sales[Qty] ) ) )"
                                    "Sales", "By Label", "CALCULATE ( [Amount], Customer[Label] = \"ANN\" )"
                                    "Sales", "Via Function", "Shout ( \"a\" )"
                                ]
                            Functions = [ "Shout", "( t : STRING ) => PATHITEM ( t, 1 ) & \"!\"" ]
                    }

                let coverage =
                    Engine.coverage db
                    |> List.map (fun (r, u) -> ObjectRef.format r, u)
                    |> Map.ofList

                Expect.equal coverage.["measure:Sales/Amount"] [] "supported"
                Expect.equal coverage.["measure:Sales/Formatted"] [ "PATHITEM" ] "a function"
                Expect.equal coverage.["measure:Sales/Uses Formatted"] [ "PATHITEM" ] "through a measure"

                Expect.equal
                    coverage.["measure:Sales/Selected"]
                    [ "KEEPFILTERS ( ALLSELECTED ( ... ) )" ]
                    "an argument form"

                Expect.equal coverage.["measure:Sales/Rolled"] [ "ROLLUP in SUMMARIZE"; "ROLLUP" ] "a helper"
                Expect.equal coverage.["measure:Sales/By Label"] [ "PATHITEM" ] "through a calculated column"
                Expect.equal coverage.["measure:Sales/Via Function"] [ "PATHITEM" ] "through a function"
                Expect.equal coverage.["column:Customer/Label"] [ "PATHITEM" ] "a calculated column")
            // What can be told from the arguments alone is told by coverage, with the words the
            // evaluator uses.
            testCase "argument forms that are refused show in coverage, and when evaluated" (fun () ->
                let forms = [
                    "COUNTROWS ( SUMMARIZE ( Sales, \"x\", 1 ) )", "SUMMARIZE without a group-by column"
                    "COUNTROWS ( GROUPBY ( Sales ) )", "GROUPBY without a group-by column"
                    "CALCULATE ( [Amount], CROSSFILTER ( Sales[ProductKey], Product[ProductKey], ONEWAY_LEFTFILTERSRIGHT ) )",
                    "CROSSFILTER direction"
                    "CALCULATE ( [Amount], ALLSELECTED (), ALL ( Product ) )", "ALLSELECTED together with"
                    "COUNTROWS ( SUMMARIZECOLUMNS ( Product[Color], NONVISUAL ( VALUES ( Product[Color] ) ) ) )",
                    "NONVISUAL"
                ]

                let model = {
                    star with
                        Measures = star.Measures @ (forms |> List.mapi (fun i (dax, _) -> "Sales", $"F{i}", dax))
                }

                let db, store = build model

                let coverage =
                    Engine.coverage db
                    |> List.map (fun (r, u) -> ObjectRef.format r, u)
                    |> Map.ofList

                match Engine.prepare db store with
                | Error e -> failtestf "%A" e
                | Ok m ->
                    forms
                    |> List.iteri (fun i (dax, mentions) ->
                        match coverage.[$"measure:Sales/F{i}"] with
                        | why :: _ -> Expect.stringContains why mentions dax
                        | [] -> failtestf "coverage cleared %s" dax

                        match Engine.evaluateScalar m $"[F{i}]" with
                        | Error(DaxNotSupported(what, _)) -> Expect.stringContains what mentions dax
                        | other -> failtestf "%s: %A" dax other))
            testCase "reports the relationships the engine doesn't do" (fun () ->
                let db, _ =
                    build {
                        star with
                            Rels =
                                star.Rels
                                |> List.mapi (fun i r ->
                                    if i = 1 then
                                        {
                                            r with
                                                FromCardinality = RelationshipEndCardinality.One
                                                Active = false
                                        }
                                    else
                                        r)
                    }

                match
                    Engine.coverage db
                    |> List.find (fun (r, _) -> ObjectRef.format r = "measure:Sales/Amount")
                with
                | _, [ why ] -> Expect.stringContains why "one-to-one" "why"
                | other -> failtestf "%A" other)
            // What coverage clears, the engine evaluates: the two use the same checks.
            testCase "a measure that coverage clears evaluates" (fun () ->
                let model = {
                    star with
                        Measures =
                            star.Measures
                            @ [
                                "Sales", "Share", "DIVIDE ( [Amount], CALCULATE ( [Amount], ALL ( Product ) ) )"
                                "Sales", "Best", "MAXX ( VALUES ( Product[Name] ), [Amount] )"
                                "Sales", "Formatted", "PATHITEM ( [Amount], 1 )"
                            ]
                }

                let db, store = build model

                match Engine.prepare db store with
                | Error e -> failtestf "%A" e
                | Ok m ->
                    for (ref, unsupported) in Engine.coverage db do
                        if ref.Kind = ObjectKind.Measure then
                            let name = List.last ref.Path

                            match Engine.evaluateScalar m $"[{name}]", unsupported with
                            | Ok _, [] -> ()
                            | Error(DaxNotSupported _), _ :: _ -> ()
                            | result, _ -> failtestf "%s: coverage says %A, evaluation gave %A" name unsupported result)
        ]
    ]
