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
