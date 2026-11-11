/// Calculation groups in the local DAX engine: binding, applying items, precedence, recursion,
/// selection, format strings, the SELECTEDMEASURE family, coverage and the CLI. Each expected
/// value follows from a rule a source states (cited); the refusals are what the sources leave open.
module Tessera.Tests.EngineCalcGroupTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.EngineHelpers
open Tessera.Cli.Common

/// T: Qty 10, 20, 30. [Total] = 60 (format string "0.00"), [Count] = 3 (no format string),
/// [Ten] = 10, [Dynamic] = 1 with a dynamic format string. F is a function that calls
/// SELECTEDMEASURE ().
let private baseModel = {
    Tables = [
        table "T" [ "Item", DataType.String; "Qty", DataType.Int64 ] [ [ "A"; 10 ]; [ "B"; 20 ]; [ "C"; 30 ] ]
    ]
    Rels = []
    Measures = [
        "T", "Total", "SUM ( T[Qty] )"
        "T", "Count", "COUNTROWS ( T )"
        "T", "Ten", "10"
        "T", "Dynamic", "1"
    ]
    Functions = [ "F", "( ) => SELECTEDMEASURE ( )" ]
}

/// A calculation group as Power BI makes one: a Name column and an Ordinal column.
let private addGroup
    (db: Database)
    (name: string)
    (column: string)
    (precedence: int)
    (items: (string * string * string option) list)
    =
    let t =
        new Table(Name = name, CalculationGroup = new CalculationGroup(Precedence = precedence))

    t.Columns.Add(new DataColumn(Name = column, DataType = DataType.String, SourceColumn = "Name"))
    t.Columns.Add(new DataColumn(Name = "Ordinal", DataType = DataType.Int64, SourceColumn = "Ordinal"))

    items
    |> List.iteri (fun i (n, e, format) ->
        let item = new CalculationItem(Name = n, Expression = e, Ordinal = i)

        format
        |> Option.iter (fun f -> item.FormatStringDefinition <- new FormatStringDefinition(Expression = f))

        t.CalculationGroup.CalculationItems.Add item)

    t.Partitions.Add(new Partition(Name = name, Source = new CalculationGroupSource()))
    db.Model.Tables.Add t
    db.Model.DiscourageImplicitMeasures <- true
    t

let private item (name: string) (expression: string) = name, expression, None

/// Adjust (precedence 100) and Scale (precedence 200, a format string expression on its item).
let private standard (db: Database) =
    let measures = db.Model.Tables.["T"].Measures
    measures.["Total"].FormatString <- "0.00"
    measures.["Dynamic"].FormatStringDefinition <- new FormatStringDefinition(Expression = "\"0\"")

    addGroup db "Adjust" "Adjustment" 100 [
        item "Current" "SELECTEDMEASURE ()"
        item "Plus 2" "SELECTEDMEASURE () + 2"
        item "Self" "CALCULATE ( SELECTEDMEASURE (), Adjust[Adjustment] = \"Self\" )"
        item "Cycle A" "CALCULATE ( SELECTEDMEASURE (), Adjust[Adjustment] = \"Cycle B\" )"
        item "Cycle B" "CALCULATE ( SELECTEDMEASURE (), Adjust[Adjustment] = \"Cycle A\" )"
        item "Keep" "CALCULATE ( SELECTEDMEASURE (), KEEPFILTERS ( Adjust[Adjustment] IN { \"Keep\", \"Current\" } ) )"
        item "Twice" "[Total] + SELECTEDMEASURE ()"
        item "Name" "SELECTEDMEASURENAME ()"
        item "Fmt" "SELECTEDMEASUREFORMATSTRING ()"
        item "Bad Is" "ISSELECTEDMEASURE ( [Nope] )"
        item "Is Column" "ISSELECTEDMEASURE ( T[Qty] )"
        item "Via Function" "F ()"
        item "Rows" "SUMX ( T, SELECTEDMEASURE () )"
    ]
    |> ignore

    addGroup db "Scale" "Scaling" 200 [ "Times 2", "SELECTEDMEASURE () * 2", Some "\"0%\"" ]
    |> ignore

let private prepareWith (m: TestModel) (change: Database -> unit) : Engine.Prepared =
    let db, store = build m
    change db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private prepared = lazy (prepareWith baseModel standard)
let private p () = prepared.Value

let private invalid (p: Engine.Prepared) (dax: string) (mentions: string) =
    match queryError p dax with
    | DaxInvalid(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected dax-invalid, got %A\n%s" other dax

let private r (expression: string) = $"EVALUATE ROW ( \"r\", {expression} )"

[<Tests>]
let tests =
    testList "engine: calculation groups" [
        testList "binding" [
            // "a calculation item is like one row in the calculation group table" (SQLBI,
            // "Understanding calculation groups"); Ordinal is "the numeric order in which
            // calculation items in the calculation group appear" (Learn, "Calculation groups").
            testCase "the table's rows are the items, with their names and ordinals"
            <| fun () ->
                returns (p ()) "EVALUATE Scale" [ [ "Times 2"; 0 ] ]
                is (p ()) "COUNTROWS ( Adjust )" 13
                is (p ()) "MAXX ( FILTER ( Adjust, Adjust[Adjustment] = \"Plus 2\" ), Adjust[Ordinal] )" 1

            // "If the user does not select one value ..., then the engine does not apply any
            // variation to the original measure" (SQLBI, "Introducing calculation groups").
            testCase "an unfiltered group, even of one item, applies nothing"
            <| fun () -> is (p ()) "[Ten]" 10

            // Selection by the Ordinal column filters the same table.
            testCase "an item can be selected by its ordinal"
            <| fun () -> is (p ()) "CALCULATE ( [Total], Adjust[Ordinal] = 1 )" 62

            // "As soon as a calculation group is added to a semantic model, Power BI reports will use
            // the variant data type for all measures" (Learn): the values type the column.
            testCase "a measure's result column is typed by the values an item gives"
            <| fun () ->
                let result =
                    query
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( Adjust[Adjustment], TREATAS ( { \"Plus 2\", \"Name\" }, Adjust[Adjustment] ), \"v\", [Total] )"

                Expect.equal result.Columns.[1].Type QueryType.Variant "integers and a text: a variant"
        ]

        testList "applying one item" [
            // "A calculation item is applied by replacing measure references invoked when there is
            // a calculation item active in the filter context" (SQLBI, "Understanding the
            // application of calculation items").
            testCase "the item replaces the measure reference"
            <| fun () -> is (p ()) "CALCULATE ( [Total], Adjust[Adjustment] = \"Plus 2\" )" 62

            testCase "without a measure reference nothing changes"
            <| fun () -> is (p ()) "CALCULATE ( SUM ( T[Qty] ), Adjust[Adjustment] = \"Plus 2\" )" 60

            // "The calculation item is applied only once at the top-level calculation and never
            // again" (SQLBI, "Using calculation groups to selectively replace measures ...").
            testCase "a measure reference in the item's expression is not given the item again"
            <| fun () -> is (p ()) "CALCULATE ( [Total], Adjust[Adjustment] = \"Twice\" )" 120

            // SELECTEDMEASURE is a measure reference: a context transition (dax.guide lists it so).
            testCase "SELECTEDMEASURE () in a row context makes a context transition"
            <| fun () -> is (p ()) "CALCULATE ( [Total], Adjust[Adjustment] = \"Rows\" )" 60

            // A filter on the calculation group stays when the item is applied, so a measure
            // evaluated in an iterator gets the item per row.
            testCase "in an iterator, each row's measure reference gets the item"
            <| fun () -> is (p ()) "CALCULATE ( SUMX ( T, [Total] ), Adjust[Adjustment] = \"Plus 2\" )" 66
        ]

        testList "precedence" [
            // "( ( 10 ) + 2 ) * 2 = 24" (Learn, "Calculation groups").
            testCase "the higher precedence is applied first, around the lower"
            <| fun () ->
                is (p ()) "CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\", Scale[Scaling] = \"Times 2\" )" 24

            // The same rule with the precedences the other way round: Adjust wraps Scale,
            // ( 10 * 2 ) + 2 = 22.
            testCase "swapping the precedences swaps the order"
            <| fun () ->
                let swapped =
                    prepareWith baseModel (fun db ->
                        standard db
                        db.Model.Tables.["Adjust"].CalculationGroup.Precedence <- 300)

                is swapped "CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\", Scale[Scaling] = \"Times 2\" )" 22

            // "CALCULATE(DIVIDE(SELECTEDMEASURE(), COUNTROWS(DimDate)), DATESYTD(DimDate[Date]))", "Not
            // this expression: DIVIDE(CALCULATE(SELECTEDMEASURE(), DATESYTD(DimDate[Date])),
            // COUNTROWS(DimDate)))" (Learn, precedence with averages): the lower item is evaluated
            // in the filter context the higher item's CALCULATE makes.
            testCase "the lower item runs in the filter context of the higher item's CALCULATE"
            <| fun () ->
                let spread (outer: int) (inner: int) =
                    prepareWith baseModel (fun db ->
                        addGroup db "Spread" "Spread" outer [
                            item "All Rows" "CALCULATE ( SELECTEDMEASURE (), ALL ( T ) )"
                        ]
                        |> ignore

                        addGroup db "Per" "Per" inner [
                            item "Per Row" "DIVIDE ( SELECTEDMEASURE (), COUNTROWS ( T ) )"
                        ]
                        |> ignore)

                let q =
                    "CALCULATE ( [Total], T[Item] = \"A\", Spread[Spread] = \"All Rows\", Per[Per] = \"Per Row\" )"

                is (spread 300 5) q 20.0 // CALCULATE ( DIVIDE ( 60, 3 ), ALL ( T ) )
                is (spread 5 300) q 60.0 // DIVIDE ( CALCULATE ( 60, ALL ( T ) ), 1 )
        ]

        testList "recursion" [
            // "Other types of recursion aren't supported" (Learn): an item that selects itself.
            testCase "an item selecting itself again is refused"
            <| fun () -> notSupported (p ()) (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Self\" )") "selected again"

            testCase "two items selecting each other are refused"
            <| fun () ->
                notSupported (p ()) (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Cycle A\" )") "selected again"

            testCase "an item kept selected by KEEPFILTERS over its own filter is refused"
            <| fun () -> notSupported (p ()) (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Keep\" )") "selected again"
        ]

        testList "selection" [
            // Selection behaviour Visual: "If the calculation group does not define a
            // multipleOrEmptySelection expression the calculation group returns BLANK()" (Learn).
            testCase "several items under selectionExpressionBehavior Visual give BLANK"
            <| fun () ->
                let visual =
                    prepareWith baseModel (fun db ->
                        standard db
                        db.Model.SelectionExpressionBehavior <- SelectionExpressionBehaviorType.Visual)

                is visual "CALCULATE ( [Total], Adjust[Adjustment] IN { \"Plus 2\", \"Current\" } )" null
                is visual "CALCULATE ( [Total], Adjust[Adjustment] = \"Plus 2\" )" 62
                notSupported visual (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Nope\" )") "Visual"

            testCase "a multipleOrEmptySelectionExpression is refused when it would apply"
            <| fun () ->
                let defined =
                    prepareWith baseModel (fun db ->
                        standard db

                        db.Model.Tables.["Adjust"].CalculationGroup.MultipleOrEmptySelectionExpression <-
                            new CalculationGroupExpression(Expression = "BLANK ()"))

                is defined "CALCULATE ( [Total], Adjust[Adjustment] = \"Plus 2\" )" 62

                notSupported
                    defined
                    (r "CALCULATE ( [Total], Adjust[Adjustment] IN { \"Plus 2\", \"Current\" } )")
                    "multipleOrEmptySelectionExpression"

            testCase "a noSelectionExpression is refused when the group isn't filtered"
            <| fun () ->
                let defined =
                    prepareWith baseModel (fun db ->
                        standard db

                        db.Model.Tables.["Adjust"].CalculationGroup.NoSelectionExpression <-
                            new CalculationGroupExpression(Expression = "BLANK ()"))

                notSupported defined (r "[Total]") "noSelectionExpression"
                // Filtered, the group's item applies, whatever the other groups are.
                is defined "CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\" )" 12

            // "There cannot be two calculation groups in a model with the same Precedence" (SQLBI).
            testCase "two groups with the same precedence are refused"
            <| fun () ->
                let same =
                    prepareWith baseModel (fun db ->
                        standard db
                        db.Model.Tables.["Adjust"].CalculationGroup.Precedence <- 200)

                notSupported same (r "[Ten]") "same precedence"

                notSupported
                    same
                    (r "CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\", Scale[Scaling] = \"Times 2\" )")
                    "same precedence"

            testCase "items with no ordinal (-1) close the group: what its Ordinal column holds is not documented"
            <| fun () ->
                let unordered =
                    prepareWith baseModel (fun db ->
                        standard db
                        db.Model.Tables.["Scale"].CalculationGroup.CalculationItems.["Times 2"].Ordinal <- -1)

                notSupported unordered "EVALUATE Scale" "no ordinal"

            testCase "ROLLUPADDISSUBTOTAL over a calculation group's column is refused"
            <| fun () ->
                notSupported
                    (p ())
                    "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( Adjust[Adjustment], \"t\" ), \"v\", [Total] )"
                    "calculation group"

            testCase "an item applied in a calculated column is refused"
            <| fun () ->
                let m = {
                    baseModel with
                        Tables = [
                            table "T" [ "Item", DataType.String; "Qty", DataType.Int64 ] [ [ "A"; 10 ] ]
                            |> withCalculated [
                                "CC", DataType.Int64, "CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\" )"
                            ]
                        ]
                }

                let withColumn = prepareWith m standard
                notSupported withColumn (r "SUM ( T[CC] )") "calculated column"
        ]

        testList "the functions of an item" [
            // "A string value holding the name of the measure that is currently in context" (Learn).
            testCase "SELECTEDMEASURENAME"
            <| fun () -> is (p ()) "CALCULATE ( [Count], Adjust[Adjustment] = \"Name\" )" "Count"

            // "A string holding the format string of the measure that is currently in context" (Learn).
            testCase "SELECTEDMEASUREFORMATSTRING is the measure's format string"
            <| fun () -> is (p ()) "CALCULATE ( [Total], Adjust[Adjustment] = \"Fmt\" )" "0.00"

            // A dynamic format string is evaluated where the function is (EngineFormatStringTests).
            testCase "SELECTEDMEASUREFORMATSTRING of a dynamic format string"
            <| fun () -> is (p ()) "CALCULATE ( [Dynamic], Adjust[Adjustment] = \"Fmt\" )" "0"

            testCase "SELECTEDMEASUREFORMATSTRING is refused where the format string is not settled"
            <| fun () ->
                notSupported (p ()) (r "CALCULATE ( [Count], Adjust[Adjustment] = \"Fmt\" )") "no format string"
                // Scale (200, with a format string expression) is applied before Adjust.
                notSupported
                    (p ())
                    (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Fmt\", Scale[Scaling] = \"Times 2\" )")
                    "format string expression"

            // "fails with the error Invalid input arguments for ISSELECTEDMEASURE" (SQLBI).
            testCase "ISSELECTEDMEASURE of something that is not a measure is invalid"
            <| fun () ->
                invalid (p ()) (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Bad Is\" )") "ISSELECTEDMEASURE"
                invalid (p ()) (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Is Column\" )") "ISSELECTEDMEASURE"

            // "Can only be referenced in the expression for a calculation item" (Learn).
            testCase "outside an item's expression they are refused"
            <| fun () ->
                for f in
                    [
                        "SELECTEDMEASURE ()"
                        "SELECTEDMEASURENAME ()"
                        "SELECTEDMEASUREFORMATSTRING ()"
                    ] do
                    notSupported (p ()) (r f) "outside the expression of a calculation item"

                notSupported (p ()) (r "ISSELECTEDMEASURE ( [Total] )") "outside the expression of a calculation item"

                notSupported
                    (p ())
                    "DEFINE MEASURE T[Q] = SELECTEDMEASURENAME () EVALUATE ROW ( \"r\", CALCULATE ( [Q], Adjust[Adjustment] = \"Current\" ) )"
                    "outside the expression of a calculation item"

            testCase "inside a function an item calls they are refused"
            <| fun () ->
                notSupported
                    (p ())
                    (r "CALCULATE ( [Total], Adjust[Adjustment] = \"Via Function\" )")
                    "user-defined function"
        ]

        testList "coverage" [
            testCase "calculation items are reported like measures"
            <| fun () ->
                let db, _ = build baseModel
                standard db

                addGroup db "Odd" "Odd" 5 [ item "Path" "PATHITEM ( \"a|b\", 1 ) & SELECTEDMEASURENAME ()" ]
                |> ignore

                let report = Engine.coverage db |> Map.ofList

                let itemRef (g: string) (n: string) =
                    ObjectRef.inTable ObjectKind.CalculationItem g n

                Expect.equal report.[itemRef "Adjust" "Plus 2"] [] "a supported item"
                Expect.equal report.[itemRef "Odd" "Path"] [ "PATHITEM" ] "an unsupported function in an item"
                Expect.equal report.[ObjectRef.inTable ObjectKind.Measure "T" "Total"] [] "a measure"

            testCase "what refuses every measure reference is reported for each measure and item"
            <| fun () ->
                let db, _ = build baseModel
                standard db

                db.Model.Tables.["Adjust"].CalculationGroup.NoSelectionExpression <-
                    new CalculationGroupExpression(Expression = "BLANK ()")

                let report = Engine.coverage db |> Map.ofList

                for key in
                    [
                        ObjectRef.inTable ObjectKind.Measure "T" "Total"
                        ObjectRef.inTable ObjectKind.CalculationItem "Scale" "Times 2"
                    ] do
                    Expect.contains report.[key] "the noSelectionExpression of calculation group 'Adjust'" ""
        ]

        testList "CLI" [
            testCase "query --data evaluates a model with calculation groups, and --coverage counts its items"
            <| fun () ->
                use t = new TempDir()
                let db, store = build baseModel
                standard db
                let model = t.Combine "model.bim"
                File.WriteAllText(model, JsonSerializer.SerializeDatabase db)
                let dir = t.Combine "data"
                Directory.CreateDirectory dir |> ignore

                for table in store.Tables do
                    use writer =
                        new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

                    Fixture.writeCsv writer table

                let q =
                    tess [
                        "query"
                        model
                        "--data"
                        dir
                        "--json"
                        "EVALUATE SUMMARIZECOLUMNS ( Scale[Scaling], \"v\", CALCULATE ( [Ten], Adjust[Adjustment] = \"Plus 2\" ) )"
                    ]

                Expect.equal q.Code ExitCode.ok q.Err

                let rows =
                    JsonDocument.Parse(q.Out).RootElement.GetProperty("results").[0].GetProperty("rows")

                Expect.equal (rows.[0].[0].GetString()) "Times 2" "the group's column"
                Expect.equal (rows.[0].[1].GetInt64()) 24L "both items applied, by precedence"

                let c = tess [ "query"; model; "--coverage"; "--json" ]
                Expect.equal c.Code ExitCode.ok c.Err

                let items =
                    JsonDocument.Parse(c.Out).RootElement.GetProperty("summary").GetProperty("calculationItems")

                Expect.equal (items.GetProperty("total").GetInt32()) 14 "13 items in Adjust, 1 in Scale"
                Expect.equal (items.GetProperty("supported").GetInt32()) 14 "coverage sees no unsupported function"

                let text = tess [ "query"; model; "--coverage" ]
                Expect.stringContains text.Out "14 of 14 calculation items can be evaluated by the local engine" ""
        ]
    ]
