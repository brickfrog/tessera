/// Model shapes for the local engine: one-to-one and many-to-many (limited) relationships, which
/// column identifies a row for a context transition in a calculated column, and NAMEOF.
module Tessera.Tests.EngineModelShapeTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// Names with spaces, quotes and brackets, for NAMEOF's escaping.
let private names: TestModel = {
    Tables = [
        table "Sales Region" [ "Region Code", DataType.String; "Col]umn", DataType.String ] [ [ "N"; "x" ] ]
        table "Ta'ble" [ "Key", DataType.Int64 ] [ [ 1 ] ]
        table "Sales" [
            "Amount", DataType.Int64
            "Order_Qty2", DataType.Int64
            "Prix é", DataType.Int64
        ] [ [ 1; 2; 3 ] ]
        table "2024" [ "X", DataType.Int64 ] [ [ 1 ] ]
    ]
    Rels = []
    Measures = [ "Sales", "Total", "SUM ( Sales[Amount] )"; "Sales Region", "m1", "1" ]
    Functions = []
}

let private nameof = lazy (prepare names)

[<Tests>]
let nameofTests =
    testList "NAMEOF" [
        // Microsoft Learn, NAMEOF: examples 1 and 2, and the table "Component behavior by input
        // type" (the names are the model's, whatever the reference's case).
        testCase "the fully qualified, escaped name: Learn's examples" (fun () ->
            let p = starPrepared.Value
            is p "NAMEOF ( 'sales'[QTY] )" "'Sales'[Qty]"
            is p "NAMEOF ( Sales[Amount] )" "'Sales'[Amount]"
            is p "NAMEOF ( [Amount] )" "'Sales'[Amount]"
            is p "NAMEOF ( Product )" "'Product'"
            is p "NAMEOF ( 'product' )" "'Product'"

            returns
                p
                "DEFINE MEASURE Sales[Projected Sales] = SUM ( Sales[Qty] ) * 1.06 EVALUATE { NAMEOF ( [Projected Sales] ) }"
                [ [ "'Sales'[Projected Sales]" ] ])

        testCase "components, by the kind of object" (fun () ->
            let p = starPrepared.Value
            is p "NAMEOF ( Sales[Qty], TABLE )" "'Sales'"
            is p "NAMEOF ( Sales[Qty], COLUMN )" "[Qty]"
            is p "NAMEOF ( Sales[Qty], FULL )" "'Sales'[Qty]"
            is p "NAMEOF ( Sales[Qty], SELF )" "[Qty]"
            is p "NAMEOF ( Sales[Qty], PARENT )" "'Sales'"
            is p "NAMEOF ( Sales, TABLE )" "'Sales'"
            is p "NAMEOF ( Sales, FULL )" "'Sales'"
            is p "NAMEOF ( Sales, SELF )" "'Sales'"
            is p "NAMEOF ( [Amount], TABLE )" "'Sales'"
            is p "NAMEOF ( [Amount], MEASURE )" "[Amount]"
            is p "NAMEOF ( Sales[Amount], SELF )" "[Amount]"
            is p "NAMEOF ( [Amount], PARENT, ESCAPED )" "'Sales'")

        testCase "the combinations the documentation calls errors are refused" (fun () ->
            let p = starPrepared.Value
            notSupported p "EVALUATE { NAMEOF ( Sales[Qty], MEASURE ) }" "calls an error"
            notSupported p "EVALUATE { NAMEOF ( Sales, COLUMN ) }" "calls an error"
            notSupported p "EVALUATE { NAMEOF ( Sales, PARENT ) }" "calls an error"
            notSupported p "EVALUATE { NAMEOF ( [Amount], COLUMN ) }" "calls an error"
            notSupported p "EVALUATE { NAMEOF ( Sales[Qty], CALENDAR ) }" "CALENDAR"
            notSupported p "EVALUATE { NAMEOF ( Sales[Qty], FULL, UNESCAPED ) }" "fully qualified")

        testCase "escaping: Learn's table, and quotes and brackets doubled" (fun () ->
            let p = nameof.Value
            is p "NAMEOF ( 'Sales Region' )" "'Sales Region'"
            is p "NAMEOF ( 'Sales Region', FULL, UNESCAPED )" "Sales Region"
            is p "NAMEOF ( 'Sales Region', FULL, MINIMALLYESCAPED )" "'Sales Region'"
            is p "NAMEOF ( 'Sales Region'[Region Code], FULL, MINIMALLYESCAPED )" "'Sales Region'[Region Code]"
            is p "NAMEOF ( Sales, FULL, UNESCAPED )" "Sales"
            is p "NAMEOF ( Sales, FULL, MINIMALLYESCAPED )" "Sales"
            is p "NAMEOF ( Sales[Total], FULL, MINIMALLYESCAPED )" "Sales[Total]"
            is p "NAMEOF ( 'Sales Region'[m1] )" "'Sales Region'[m1]"
            is p "NAMEOF ( Sales[Amount], SELF, UNESCAPED )" "Amount"
            is p "NAMEOF ( Sales[Amount], SELF, MINIMALLYESCAPED )" "Amount"
            is p "NAMEOF ( Sales[Order_Qty2], SELF, MINIMALLYESCAPED )" "Order_Qty2"
            is p "NAMEOF ( 'Sales Region'[Region Code], COLUMN, UNESCAPED )" "Region Code"
            is p "NAMEOF ( 'Sales Region'[Region Code], COLUMN, MINIMALLYESCAPED )" "[Region Code]"
            is p "NAMEOF ( 'Ta''ble' )" "'Ta''ble'"
            is p "NAMEOF ( 'Ta''ble'[Key] )" "'Ta''ble'[Key]"
            is p "NAMEOF ( 'Sales Region'[Col]]umn] )" "'Sales Region'[Col]]umn]")

        testCase "what the pages leave open is refused" (fun () ->
            let p = nameof.Value
            notSupported p "EVALUATE { NAMEOF ( 'Ta''ble', FULL, UNESCAPED ) }" "quote or a closing bracket"
            notSupported p "EVALUATE { NAMEOF ( 'Ta''ble', FULL, MINIMALLYESCAPED ) }" "quote or a closing bracket"
            notSupported p "EVALUATE { NAMEOF ( '2024', FULL, MINIMALLYESCAPED ) }" "starts with a digit"
            notSupported p "EVALUATE { NAMEOF ( Sales[Prix é], SELF, MINIMALLYESCAPED ) }" "outside A-Z"
            notSupported p "EVALUATE { NAMEOF ( 'Sales Region'[Total] ) }" "not its own")

        testCase "the argument is a reference: anything else is invalid, and it is not evaluated" (fun () ->
            let p = starPrepared.Value

            match queryError p "EVALUATE { NAMEOF ( 1 + 1 ) }" with
            | DaxInvalid(message, _) -> Expect.stringContains message "reference" "why"
            | other -> failtestf "%A" other

            match queryError p "EVALUATE { NAMEOF ( Sales[Nope] ) }" with
            | DaxInvalid _ -> ()
            | other -> failtestf "%A" other

            match queryError p "EVALUATE { NAMEOF ( Sales[Qty], LEFT ) }" with
            | DaxInvalid(message, _) -> Expect.stringContains message "must be one of" "why"
            | other -> failtestf "%A" other

            // A column has no single value outside a row context; NAMEOF only names it.
            is p "NAMEOF ( Sales[Qty] ) & \"\"" "'Sales'[Qty]")

        testCase "a field parameter table: its rows name the measures, and coverage doesn't follow them" (fun () ->
            let m = {
                star with
                    Tables =
                        star.Tables
                        @ [
                            calculatedTable
                                "Metric"
                                [
                                    "Value1", DataType.String
                                    "Value2", DataType.String
                                    "Value3", DataType.Int64
                                ]
                                "{ (\"Amount\", NAMEOF ( 'Sales'[Amount] ), 0), (\"Odd\", NAMEOF ( 'Sales'[Odd] ), 1) }"
                        ]
                    Measures = star.Measures @ [ "Sales", "Odd", "PATHITEM ( \"a|b\", 1 )" ]
            }

            let p = prepare m

            returns p "EVALUATE Metric" [ [ "Amount"; "'Sales'[Amount]"; 0 ]; [ "Odd"; "'Sales'[Odd]"; 1 ] ]

            let db, _ = build m

            let metric =
                Engine.coverage db
                |> List.find (fun (r, _) -> r = ObjectRef.table "Metric")
                |> snd

            Expect.equal metric [] "NAMEOF's argument is named, not used")

        // SQLBI, "Understanding Group By Columns in Power BI": "SUMMARIZE, SUMMARIZECOLUMNS,
        // DISTINCT, and VALUES are all going to fail when we specify only City (unique)".
        testCase "a field parameter's display column has Group By Columns: reading it is refused" (fun () ->
            let m = {
                star with
                    Tables =
                        star.Tables
                        @ [
                            calculatedTable
                                "Metric"
                                [
                                    "Value1", DataType.String
                                    "Value2", DataType.String
                                    "Value3", DataType.Int64
                                ]
                                "{ (\"Amount\", NAMEOF ( 'Sales'[Amount] ), 0), (\"Units\", NAMEOF ( 'Sales'[Units] ), 1) }"
                        ]
                    Measures = star.Measures @ [ "Sales", "Picked", "SELECTEDVALUE ( Metric[Value3] )" ]
            }

            let db, store = build m
            let metric = db.Model.Tables.["Metric"]
            let details = new RelatedColumnDetails()
            details.GroupByColumns.Add(new GroupByColumn(GroupingColumn = metric.Columns.["Value2"]))
            metric.Columns.["Value1"].RelatedColumnDetails <- details

            let p =
                match Engine.prepare db store with
                | Ok p -> p
                | Error e -> failtestf "%s" (TessError.message e)

            notSupported p "EVALUATE VALUES ( Metric[Value1] )" "Group By Columns ('Metric'[Value2])"
            notSupported p "EVALUATE Metric" "Group By Columns"
            returns p "EVALUATE VALUES ( Metric[Value2] )" [ [ "'Sales'[Amount]" ]; [ "'Sales'[Units]" ] ]

            // A query with definitions binds the model again: the computed table is copied, gate and all.
            returns
                p
                "DEFINE MEASURE Sales[X] = 1 COLUMN Product[Z] = 1 TABLE T = { 1 } EVALUATE ROW ( \"n\", COUNTROWS ( VALUES ( Metric[Value2] ) ) + [X] )"
                [ [ 3 ] ]

            notSupported p "DEFINE MEASURE Sales[X] = 1 EVALUATE VALUES ( Metric[Value1] )" "Group By Columns"
            is p "CALCULATE ( [Picked], Metric[Value3] = 1 )" 1

            let coverage = Engine.coverage db

            let gaps (name: string) =
                coverage
                |> List.find (fun (r, _) -> r = ObjectRef.inTable ObjectKind.Measure "Sales" name)
                |> snd

            Expect.equal (gaps "Picked") [] "the order column is fine"

            db.Model.Tables.["Sales"].Measures.Add(
                new Measure(Name = "Shown", Expression = "SELECTEDVALUE ( Metric[Value1] )")
            )

            Expect.stringContains
                (Engine.coverage db
                 |> List.find (fun (r, _) -> r = ObjectRef.inTable ObjectKind.Measure "Sales" "Shown")
                 |> snd
                 |> String.concat "; ")
                "Group By Columns"
                "coverage names the column")
    ]

// ---- Limited (many-to-many cardinality) relationships -------------------------------------------

let private manyToMany (r: TestRel) = {
    r with
        FromCardinality = RelationshipEndCardinality.Many
        ToCardinality = RelationshipEndCardinality.Many
}

/// The model of learn.microsoft.com/power-bi/guidance/relationships-many-to-many ("Relate
/// many-to-many facts"), as the corpus's c-weak has it: Target[Category] -> Product[Category],
/// many-to-many, Product filters Target. Target's Footwear (30) matches no product. Sales relates
/// to Product (regular) with a key 9 that Product lacks, so Product has a blank row.
let private weak (both: bool) : TestModel = {
    Tables = [
        table "Product" [
            "ProductID", DataType.Int64
            "Color", DataType.String
            "Category", DataType.String
        ] [
            [ 1; "Green"; "Clothing" ]
            [ 2; "Blue"; "Clothing" ]
            [ 3; "Blue"; "Accessories" ]
        ]
        table "Target" [
            "Category", DataType.String
            "Year", DataType.Int64
            "Quantity", DataType.Int64
        ] [
            [ "Clothing"; 2019; 100 ]
            [ "Accessories"; 2019; 50 ]
            [ "Clothing"; 2020; 120 ]
            [ "Accessories"; 2020; 60 ]
            [ "Footwear"; 2020; 30 ]
        ]
        table "Sales" [ "ProductID", DataType.Int64; "Amount", DataType.Int64 ] [ [ 1; 10 ]; [ 9; 5 ] ]
    ]
    Rels = [
        (if both then bothWays else id) (manyToMany (rel ("Target", "Category") ("Product", "Category")))
        rel ("Sales", "ProductID") ("Product", "ProductID")
    ]
    Measures = [ "Target", "Target Qty", "SUM ( Target[Quantity] )" ]
    Functions = []
}

let private weakOne = lazy (prepare (weak false))
let private weakBoth = lazy (prepare (weak true))

[<Tests>]
let limitedTests =
    testList "limited relationships" [
        // "Table joins are achieved by using INNER JOIN semantics" (Learn, "Model relationships in
        // Power BI Desktop"); "the categories of the products in the filter are what filters Target"
        // (Learn, relationships-many-to-many).
        testCase "a filter crosses by the values of the column: each product row gives its category" (fun () ->
            let p = weakOne.Value
            is p "CALCULATE ( [Target Qty], Product[Color] = \"Green\" )" 220
            is p "CALCULATE ( [Target Qty], Product[Color] = \"Blue\" )" 330
            // Context transition on each Product row: T-shirt 220, Jeans 220, Hat 110.
            is p "SUMX ( Product, [Target Qty] )" 550
            is p "CALCULATE ( [Target Qty], Product[ProductID] = 3 )" 110
            // RELATEDTABLE is CALCULATETABLE: the same filter (dax.guide/relatedtable).
            is p "SUMX ( Product, COUNTROWS ( RELATEDTABLE ( Target ) ) )" 6)

        testCase "nothing filters the other side, nothing is filtered: the total keeps unmatched rows" (fun () ->
            let p = weakOne.Value
            is p "[Target Qty]" 360
            is p "CALCULATE ( [Target Qty], ALL ( Product ) )" 360
            is p "CALCULATE ( [Target Qty], Target[Year] = 2020 )" 210)

        // "for many-to-many relationships, cross filter direction can be from either one of the
        // tables, or from both tables" (Learn, "Model relationships in Power BI Desktop").
        testCase "single direction doesn't filter back; both directions do, by values" (fun () ->
            is weakOne.Value "CALCULATE ( COUNTROWS ( Product ), Target[Year] = 2019 )" 3
            is weakOne.Value "CALCULATE ( COUNTROWS ( Product ), Target[Category] = \"Footwear\" )" 3
            is weakBoth.Value "CALCULATE ( COUNTROWS ( Product ), Target[Category] = \"Accessories\" )" 1
            is weakBoth.Value "CALCULATE ( COUNTROWS ( Product ), Target[Category] = \"Footwear\" )" (null: obj)
            is weakBoth.Value "CALCULATE ( COUNTROWS ( Product ), Target[Quantity] >= 100 )" 2)

        // "The additional blank row is never added to tables involved in a limited relationship"
        // (SQLBI, "Relationships in Power BI and Tabular models"); "no revenues in the Weak Amount
        // measure are related to the blank row" (SQLBI, "Strong and weak relationships in Power BI").
        testCase
            "no blank row from a limited relationship, and the blank row of a regular one matches nothing"
            (fun () ->
                let p = weakOne.Value
                returns p "EVALUATE VALUES ( Target[Category] )" [ [ "Clothing" ]; [ "Accessories" ]; [ "Footwear" ] ]
                // Product's blank row comes from Sales' key 9.
                is p "COUNTROWS ( VALUES ( Product[ProductID] ) )" 4
                is p "CALCULATE ( [Target Qty], Product[ProductID] = 1 )" 220
                is p "CALCULATE ( [Target Qty], Product[ProductID] = 9 )" (null: obj))

        // "Table expansion never occurs for limited relationships"; "The RELATED DAX function can't
        // be used to retrieve the "one" side column values" (Learn, "Model relationships").
        testCase "no table expansion across it: RELATED is an error, ALL ( Target ) leaves Product's filters" (fun () ->
            let p = weakOne.Value

            match queryError p "EVALUATE ADDCOLUMNS ( Target, \"Color\", RELATED ( Product[Color] ) )" with
            | DaxInvalid _ -> ()
            | other -> failtestf "%A" other

            is p "CALCULATE ( [Target Qty], Product[Color] = \"Green\", ALL ( Target ) )" 220)

        testCase "CROSSFILTER and USERELATIONSHIP" (fun () ->
            let p = weakOne.Value

            is
                p
                "CALCULATE ( [Target Qty], Product[Color] = \"Green\", CROSSFILTER ( Target[Category], Product[Category], NONE ) )"
                360

            is
                p
                "CALCULATE ( COUNTROWS ( Product ), Target[Category] = \"Accessories\", CROSSFILTER ( Target[Category], Product[Category], BOTH ) )"
                1

            notSupported
                p
                "EVALUATE ROW ( \"x\", CALCULATE ( [Target Qty], CROSSFILTER ( Target[Category], Product[Category], ONEWAY ) ) )"
                "ONEWAY on a many-to-many")

        testCase "what the sources leave open is refused" (fun () ->
            let p = weakOne.Value
            // A filter that keeps every Product row, its blank row too.
            notSupported
                p
                "EVALUATE ROW ( \"x\", CALCULATE ( [Target Qty], Product[Color] IN { \"Green\", \"Blue\", BLANK () } ) )"
                "keeps all of its rows"

            // One that leaves out only the blank row, which matches nothing anyway, is a filter.
            is p "CALCULATE ( [Target Qty], Product[Color] IN { \"Green\", \"Blue\" } )" 330

            // ALLCROSSFILTERED also removes what crosses a limited relationship: which filters those
            // are isn't implemented.
            notSupported
                p
                "EVALUATE ROW ( \"x\", CALCULATE ( [Target Qty], ALLCROSSFILTERED ( Target ) ) )"
                "ALLCROSSFILTERED"

            // ISCROSSFILTERED of a table connected through it, which no filter reaches.
            notSupported
                p
                "EVALUATE ROW ( \"x\", CALCULATE ( ISCROSSFILTERED ( Product ), Target[Year] = 2019 ) )"
                "ISCROSSFILTERED"

            is p "CALCULATE ( ISCROSSFILTERED ( Target ), Product[Color] = \"Green\" )" true)

        testCase "BLANK on both sides, and columns of different types, are refused" (fun () ->
            let blanks = {
                weak false with
                    Tables =
                        weak false
                        |> fun m ->
                            m.Tables
                            |> List.map (fun t ->
                                match t.Name with
                                | "Product" -> {
                                    t with
                                        Rows = t.Rows @ [ [ 4; "Red"; null ] ]
                                  }
                                | "Target" -> {
                                    t with
                                        Rows = t.Rows @ [ [ null; 2021; 7 ] ]
                                  }
                                | _ -> t)
            }

            let p = prepare blanks
            is p "CALCULATE ( [Target Qty], Product[Color] = \"Green\" )" 220
            notSupported p "EVALUATE ROW ( \"x\", CALCULATE ( [Target Qty], Product[Color] = \"Red\" ) )" "BLANK"

            let typed = {
                weak false with
                    Tables =
                        (weak false).Tables
                        |> List.map (fun t ->
                            if t.Name = "Target" then
                                {
                                    t with
                                        Columns = [
                                            "Category", DataType.Int64
                                            "Year", DataType.Int64
                                            "Quantity", DataType.Int64
                                        ]
                                        Rows = [ [ 1; 2019; 1 ] ]
                                }
                            else
                                t)
            }

            let db, store = build typed

            match Engine.prepare db store with
            | Ok p ->
                notSupported
                    p
                    "EVALUATE ROW ( \"x\", CALCULATE ( [Target Qty], Product[Color] = \"Green\" ) )"
                    "different types"
            | Error e -> Expect.stringContains (TessError.message e) "different types" "refused")
    ]

// ---- One-to-one relationships ------------------------------------------------------------------

let private oneToOne (r: TestRel) = {
    r with
        FromCardinality = RelationshipEndCardinality.One
        ToCardinality = RelationshipEndCardinality.One
        Both = true
}

/// The Product / Product Category rows of learn.microsoft.com/power-bi/guidance/relationships-one-to-one
/// (as the corpus's c-onetoone): Jeans (CL-02) has no category. Product also relates to Brand, and
/// Sales to Product.
let private identity: TestModel = {
    Tables = [
        table "Product" [
            "SKU", DataType.String
            "Product", DataType.String
            "Color", DataType.String
            "BrandId", DataType.Int64
        ] [
            [ "CL-01"; "T-shirt"; "Green"; 1 ]
            [ "CL-02"; "Jeans"; "Blue"; 2 ]
            [ "AC-01"; "Hat"; "Blue"; 1 ]
        ]
        table "ProductCategory" [ "SKU", DataType.String; "Category", DataType.String ] [
            [ "CL-01"; "Clothing" ]
            [ "AC-01"; "Accessories" ]
        ]
        table "Brand" [ "BrandId", DataType.Int64; "Brand", DataType.String ] [ [ 1; "Acme" ]; [ 2; "Zed" ] ]
        table "Sales" [ "SKU", DataType.String; "Qty", DataType.Int64 ] [
            [ "CL-01"; 2 ]
            [ "CL-02"; 3 ]
            [ "AC-01"; 5 ]
        ]
    ]
    Rels = [
        oneToOne (rel ("Product", "SKU") ("ProductCategory", "SKU"))
        rel ("Product", "BrandId") ("Brand", "BrandId")
        rel ("Sales", "SKU") ("Product", "SKU")
    ]
    Measures = [ "Sales", "Units", "SUM ( Sales[Qty] )" ]
    Functions = []
}

let private identityPrepared = lazy (prepare identity)

let private withRows (table: string) (rows: obj list list) (m: TestModel) = {
    m with
        Tables =
            m.Tables
            |> List.map (fun t -> if t.Name = table then { t with Rows = rows } else t)
}

[<Tests>]
let oneToOneTests =
    testList "one-to-one relationships" [
        // "For one-to-one relationships, the cross filter direction is always from both tables"
        // (Learn, "Model relationships in Power BI Desktop").
        testCase "filters cross both ways, and on to the tables beyond" (fun () ->
            let p = identityPrepared.Value
            is p "CALCULATE ( COUNTROWS ( Product ), ProductCategory[Category] = \"Clothing\" )" 1
            is p "CALCULATE ( COUNTROWS ( ProductCategory ), Product[Color] = \"Blue\" )" 1
            is p "CALCULATE ( [Units], ProductCategory[Category] = \"Accessories\" )" 5
            is p "CALCULATE ( COUNTROWS ( ProductCategory ), Brand[Brand] = \"Zed\" )" (null: obj))

        // "This join type ensures that blank virtual rows are added on either side, when
        // necessary" (Learn): Jeans has no category, so ProductCategory gets the blank row.
        testCase "the table without a match gets the blank row" (fun () ->
            let p = identityPrepared.Value
            returns p "EVALUATE VALUES ( ProductCategory[Category] )" [ [ "Clothing" ]; [ "Accessories" ]; [ null ] ]
            returns p "EVALUATE DISTINCT ( ProductCategory[Category] )" [ [ "Clothing" ]; [ "Accessories" ] ]
            is p "COUNTROWS ( ProductCategory )" 2
            is p "CALCULATE ( [Units], ISBLANK ( ProductCategory[Category] ) )" 3)

        // "a one-to-one relationship is known as an identity. For all intents and purposes you can
        // consider the two expanded tables as being the same" (SQLBI, "Expanded tables in DAX").
        testCase "each table's expanded table holds the other: RELATED, ALL and table filters" (fun () ->
            let p = identityPrepared.Value

            returns
                p
                "EVALUATE ADDCOLUMNS ( ProductCategory, \"P\", RELATED ( Product[Product] ), \"B\", RELATED ( Brand[Brand] ) )"
                [
                    [ "CL-01"; "Clothing"; "T-shirt"; "Acme" ]
                    [ "AC-01"; "Accessories"; "Hat"; "Acme" ]
                ]

            // ALL ( ProductCategory ) removes the filters of its expanded table, Product's among them
            // (the outer filter is there when the inner CALCULATE removes filters).
            is
                p
                "CALCULATE ( CALCULATE ( COUNTROWS ( Product ), ALL ( ProductCategory ) ), Product[Color] = \"Blue\" )"
                3

            is
                p
                "CALCULATE ( CALCULATE ( COUNTROWS ( Brand ), REMOVEFILTERS ( ProductCategory ) ), Brand[Brand] = \"Zed\" )"
                2

            // The other way, ALL ( Product ) removes ProductCategory's filters, as it would for any
            // table Product expands to.
            is
                p
                "CALCULATE ( CALCULATE ( COUNTROWS ( ProductCategory ), ALL ( Product ) ), ProductCategory[Category] = \"Clothing\" )"
                2
            // A table filter on ProductCategory filters its expanded table: Brand too.
            is
                p
                "CALCULATE ( COUNTROWS ( Brand ), FILTER ( ProductCategory, ProductCategory[Category] = \"Clothing\" ) )"
                1
            // A context transition on a ProductCategory row is a filter on the joined row.
            is p "SUMX ( ProductCategory, [Units] )" 7)

        testCase "a filter that pairs columns of the two tables tests the joined rows" (fun () ->
            let p = identityPrepared.Value

            is
                p
                "CALCULATE ( COUNTROWS ( Product ), TREATAS ( { ( \"Blue\", \"Accessories\" ) }, Product[Color], ProductCategory[Category] ) )"
                1

            // Jeans is joined to the blank row: its category is BLANK.
            is
                p
                "CALCULATE ( SELECTEDVALUE ( Product[Product] ), TREATAS ( { ( \"Blue\", BLANK () ) }, Product[Color], ProductCategory[Category] ) )"
                "Jeans"

            is
                p
                "CALCULATE ( COUNTROWS ( ProductCategory ), TREATAS ( { ( \"Blue\", BLANK () ) }, Product[Color], ProductCategory[Category] ) )"
                (null: obj)

            is
                p
                "CALCULATE ( [Units], TREATAS ( { ( \"Green\", \"Clothing\" ), ( \"Blue\", BLANK () ) }, Product[Color], ProductCategory[Category] ) )"
                5)

        // The engine takes the relationship's from side as the one whose rows are the joined rows:
        // the same data declared the other way round leaves ProductCategory with keys Product
        // lacks, which is refused.
        testCase "the same data declared the other way round is refused" (fun () ->
            let reversed = {
                identity with
                    Rels =
                        oneToOne (rel ("ProductCategory", "SKU") ("Product", "SKU"))
                        :: identity.Rels.Tail
            }

            notSupported (prepare reversed) "EVALUATE ROW ( \"x\", COUNTROWS ( ProductCategory ) )" "lacks")

        testCase "what isn't implemented is refused" (fun () ->
            // A category for a product that doesn't exist: Product would need a blank row.
            let extra =
                prepare (
                    identity
                    |> withRows "ProductCategory" [
                        [ "CL-01"; "Clothing" ]
                        [ "AC-01"; "Accessories" ]
                        [ "ZZ-01"; "Toys" ]
                    ]
                )

            notSupported extra "EVALUATE VALUES ( Product[Color] )" "lacks"
            notSupported extra "EVALUATE ROW ( \"x\", [Units] )" "lacks"

            // Two products without a category.
            let two =
                prepare (identity |> withRows "ProductCategory" [ [ "CL-01"; "Clothing" ] ])

            notSupported two "EVALUATE VALUES ( ProductCategory[Category] )" "2 rows of 'Product'"

            // Product with a blank row of its own, for Sales' key ZZ-09.
            let orphan =
                prepare (identity |> withRows "Sales" [ [ "CL-01"; 2 ]; [ "ZZ-09"; 1 ] ])

            notSupported orphan "EVALUATE VALUES ( Product[Color] )" "one-to-one"

            let p = identityPrepared.Value

            notSupported
                p
                "EVALUATE ROW ( \"x\", CALCULATE ( [Units], CROSSFILTER ( Product[SKU], ProductCategory[SKU], ONEWAY ) ) )"
                "ONEWAY on a one-to-one"

            is
                p
                "CALCULATE ( COUNTROWS ( ProductCategory ), Product[Color] = \"Green\", CROSSFILTER ( Product[SKU], ProductCategory[SKU], NONE ) )"
                2)
    ]

// ---- Which column identifies a row: the key the engine knows ----------------------------------

/// SQLBI, "Understanding circular dependencies": BestProducts, a table not related to anything
/// whose ProductKey is unique, with the article's two identical columns.
let private best (calculated: (string * DataType * string) list) (related: bool) : TestModel = {
    Tables = [
        table "BestProducts" [ "ProductKey", DataType.Int64; "Product Name", DataType.String ] [
            [ 1084; "Contoso SLR Camera X143 Grey" ]
            [ 1085; "Adventure Works Desktop" ]
            [ 1086; "Contoso Lens" ]
        ]
        |> withCalculated calculated
        table "Sales" [ "ProductKey", DataType.Int64 ] [ [ 1084 ]; [ 1086 ] ]
    ]
    Rels =
        if related then
            [ rel ("Sales", "ProductKey") ("BestProducts", "ProductKey") ]
        else
            []
    Measures = []
    Functions = []
}

let private test1 =
    "Test1", DataType.Int64, "CALCULATE ( COUNTROWS ( 'BestProducts' ), BestProducts[Product Name] >= \"Contoso\" )"

let private test2 =
    "Test2", DataType.Int64, "CALCULATE ( COUNTROWS ( 'BestProducts' ), BestProducts[Product Name] >= \"Contoso\" )"

let private prepared (m: TestModel) =
    let db, store = build m
    Engine.prepare db store

[<Tests>]
let keyTests =
    testList "the key of a context transition" [
        // "The first calculated column Test1 is created without issues."
        testCase "one column with a context transition is computed" (fun () ->
            let p = prepare (best [ test1 ] false)
            Expect.isEmpty (Engine.failures p) "computed"
            // Each row: its own row, if its name is >= "Contoso".
            returns
                p
                "EVALUATE SELECTCOLUMNS ( BestProducts, \"K\", BestProducts[ProductKey], \"T\", BestProducts[Test1] )"
                [ [ 1084; 1 ]; [ 1085; null ]; [ 1086; 1 ] ])

        // "the second calculated column Test2 – despite being identical to the first one – raises the
        // circular dependency error": ProductKey's values are unique, but no relationship says so.
        testCase "two of them in a table without a relationship are a circular dependency" (fun () ->
            match prepared (best [ test1; test2 ] false) with
            | Error(DaxInvalid(message, _)) -> Expect.stringContains message "circular dependency" "message"
            | other -> failtestf "%A" other)

        // "You can also obtain the same result if you create a relationship between Sales and
        // BestProducts based on ProductKey ... the context transition is optimized to rely on that
        // column, without filtering all the other columns."
        testCase "the one side of a relationship is the key: both are computed" (fun () ->
            let p = prepare (best [ test1; test2 ] true)
            Expect.isEmpty (Engine.failures p) "computed"

            returns
                p
                "EVALUATE SELECTCOLUMNS ( BestProducts, \"T1\", BestProducts[Test1], \"T2\", BestProducts[Test2] )"
                [ [ 1; 1 ]; [ null; null ]; [ 1; 1 ] ]

            // And a column that reads one of them is no cycle either.
            let p =
                prepare (best [ test1; "Twice", DataType.Int64, "BestProducts[Test1] * 2" ] true)

            is p "SUM ( BestProducts[Twice] )" 4)

        // "CALCULATE in a calculated column ... makes that column dependent on all the columns in
        // the table" (SQLBI, "Avoiding circular dependency errors in DAX"): a column that reads it
        // is a cycle without the key.
        testCase "without the key, a column that reads the transition's column is a cycle" (fun () ->
            match prepared (best [ test1; "Twice", DataType.Int64, "BestProducts[Test1] * 2" ] false) with
            | Error(DaxInvalid(message, _)) -> Expect.stringContains message "circular dependency" "message"
            | other -> failtestf "%A" other)

        // "By using the previous code based on ALLEXCEPT for both Test1 and Test2 ... The circular
        // dependency is no longer present": the engine can't tell yet what ALLEXCEPT leaves, so it
        // refuses rather than call the model invalid.
        testCase "ALLEXCEPT on the key: no cycle on a server, refused here" (fun () ->
            let narrowed (name: string) =
                name,
                DataType.Int64,
                "CALCULATE ( COUNTROWS ( 'BestProducts' ), BestProducts[Product Name] >= \"Contoso\", ALLEXCEPT ( BestProducts, BestProducts[ProductKey] ) )"

            match prepared (best [ narrowed "Test1"; narrowed "Test2" ] false) with
            | Ok p ->
                match Engine.failures p with
                | (_, DaxNotSupported(what, _)) :: _ -> Expect.stringContains what "ALLEXCEPT" "what"
                | other -> failtestf "%A" other
            | Error e -> failtestf "%A" e

            // With the key, it is computed: each row counts itself.
            let p = prepare (best [ narrowed "Test1"; narrowed "Test2" ] true)
            Expect.isEmpty (Engine.failures p) "computed"
            is p "SUM ( BestProducts[Test2] )" 2)

        // "Here are a couple of scenarios where this optimization is not activated: You use
        // USERELATIONSHIP in the code of the column."
        testCase "USERELATIONSHIP in the column turns the key off: a cycle is then refused" (fun () ->
            let m = {
                best
                    [
                        "U1",
                        DataType.Int64,
                        "CALCULATE ( COUNTROWS ( Sales ), USERELATIONSHIP ( Sales[Other], BestProducts[ProductKey] ) )"
                        "U2", DataType.Int64, "CALCULATE ( COUNTROWS ( 'BestProducts' ) )"
                    ]
                    true with
                    Tables =
                        (best [] true).Tables
                        |> List.map (fun t ->
                            match t.Name with
                            | "Sales" -> {
                                t with
                                    Columns = [ "ProductKey", DataType.Int64; "Other", DataType.Int64 ]
                                    Rows = [ [ 1084; 1085 ]; [ 1086; 1085 ] ]
                              }
                            | _ ->
                                t
                                |> withCalculated [
                                    "U1",
                                    DataType.Int64,
                                    "CALCULATE ( COUNTROWS ( Sales ), USERELATIONSHIP ( Sales[Other], BestProducts[ProductKey] ) )"
                                    "U2", DataType.Int64, "CALCULATE ( COUNTROWS ( 'BestProducts' ) )"
                                ])
                    Rels = [
                        rel ("Sales", "ProductKey") ("BestProducts", "ProductKey")
                        inactive (rel ("Sales", "Other") ("BestProducts", "ProductKey"))
                    ]
            }

            match prepared m with
            | Ok p ->
                match Engine.failures p with
                | (_, DaxNotSupported(what, _)) :: _ -> Expect.stringContains what "depend on each other" "what"
                | other -> failtestf "%A" other
            | Error e -> failtestf "%A" e

            // Alone, it is computed: with or without the key, nothing else depends on it.
            let alone = {
                m with
                    Tables =
                        m.Tables
                        |> List.map (fun t ->
                            if t.Name = "BestProducts" then
                                {
                                    t with
                                        Calculated = [ t.Calculated.Head ]
                                }
                            else
                                t)
            }

            let p = prepare alone
            Expect.isEmpty (Engine.failures p) "computed"
            is p "SUM ( BestProducts[U1] )" 2)

        // A calculated table that iterates the table with CALCULATE depends on all of its columns,
        // one of which reads the calculated table: a cycle, whatever computes first. What the
        // calculated table's expression does to the transition isn't read, so it is refused.
        testCase "a calculated table's transition over a table whose column reads it is not computed" (fun () ->
            let m = {
                Tables = [
                    table "T" [ "V", DataType.Int64 ] [ [ 1 ]; [ 1 ]; [ 2 ] ]
                    |> withCalculated [ "Y", DataType.Int64, "COUNTROWS ( X )" ]
                    calculatedTable
                        "X"
                        [ "n", DataType.Int64 ]
                        "ROW ( \"n\", SUMX ( T, CALCULATE ( COUNTROWS ( T ) ) ) )"
                ]
                Rels = []
                Measures = []
                Functions = []
            }

            match prepared m with
            | Ok p ->
                Expect.isNonEmpty (Engine.failures p) "refused"

                for (_, e) in Engine.failures p do
                    match e with
                    | DaxNotSupported _ -> ()
                    | other -> failtestf "%A" other
            | Error(DaxInvalid(message, _)) -> Expect.stringContains message "circular" "circular"
            | Error e -> failtestf "%A" e)

        testCase "a column marked as the key, without a relationship: a cycle is refused" (fun () ->
            let db, store = build (best [ test1; test2 ] false)
            db.Model.Tables.["BestProducts"].Columns.["ProductKey"].IsKey <- true

            match Engine.prepare db store with
            | Ok p ->
                match Engine.failures p with
                | (_, DaxNotSupported(what, _)) :: _ -> Expect.stringContains what "depend on each other" "what"
                | other -> failtestf "%A" other
            | Error e -> failtestf "%A" e)
    ]
