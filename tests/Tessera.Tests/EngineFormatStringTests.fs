/// Format strings in the local DAX engine: static and dynamic format strings of measures, the
/// internal `_<measure> FormatString` measure, calculation items' format string expressions,
/// SELECTEDMEASUREFORMATSTRING, `Engine.evaluateQueryFormatted` and `tess query --formatted`. Each
/// expectation follows from a rule a source states (cited); the refusals are what they leave open.
module Tessera.Tests.EngineFormatStringTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.EngineHelpers
open Tessera.Cli.Common

/// T: Item A 10, B 20, C 30.
///   [Total] = SUM ( T[Qty] ), format string "0.00"
///   [Plain] = SUM ( T[Qty] ), no format string
///   [Dyn] = SUM ( T[Qty] ), dynamic: IF ( SELECTEDMEASURE () > 15, "0.0", "0" )
///   [ByItem] = SUM ( T[Qty] ), dynamic: SELECTEDVALUE ( T[Fmt], "#,0" ) (A "0.000", B "0.0", C "0")
///   [Self] = SUM ( T[Qty] ), dynamic: IF ( [Self] > 25, "0.0", "0" ) (its own name)
///   [Const] = SUM ( T[Qty] ), dynamic: "0.0" (refers to no measure)
///   [Blanky] = IF ( SUM ( T[Qty] ) > 15, SUM ( T[Qty] ) ), format string "0.0": BLANK for A
let private baseModel = {
    Tables = [
        table "T" [ "Item", DataType.String; "Qty", DataType.Int64; "Fmt", DataType.String ] [
            [ "A"; 10; "0.000" ]
            [ "B"; 20; "0.0" ]
            [ "C"; 30; "0" ]
        ]
    ]
    Rels = []
    Measures = [
        "T", "Total", "SUM ( T[Qty] )"
        "T", "Plain", "SUM ( T[Qty] )"
        "T", "Dyn", "SUM ( T[Qty] )"
        "T", "ByItem", "SUM ( T[Qty] )"
        "T", "Self", "SUM ( T[Qty] )"
        "T", "Const", "SUM ( T[Qty] )"
        "T", "Blanky", "IF ( SUM ( T[Qty] ) > 15, SUM ( T[Qty] ) )"
        "T", "BadBlank", "1"
        "T", "BadNumber", "1"
        "T", "BadEmpty", "1"
        "T", "OwnSmfs", "1"
        "T", "Unformattable", "1"
    ]
    Functions = []
}

let private formats (db: Database) =
    let m = db.Model.Tables.["T"].Measures
    m.["Total"].FormatString <- "0.00"
    m.["Blanky"].FormatString <- "0.0"

    let dynamic (name: string) (e: string) =
        m.[name].FormatStringDefinition <- new FormatStringDefinition(Expression = e)

    dynamic "Dyn" "IF ( SELECTEDMEASURE () > 15, \"0.0\", \"0\" )"
    dynamic "ByItem" "SELECTEDVALUE ( T[Fmt], \"#,0\" )"
    dynamic "Self" "IF ( [Self] > 25, \"0.0\", \"0\" )"
    dynamic "Const" "\"0.0\""
    dynamic "BadBlank" "BLANK ()"
    dynamic "BadNumber" "1"
    dynamic "BadEmpty" "\"\""
    dynamic "OwnSmfs" "SELECTEDMEASUREFORMATSTRING ()"
    dynamic "Unformattable" "\"c\""

let private prepareWith (m: TestModel) (change: Database -> unit) : Engine.Prepared =
    let db, store = build m
    change db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private plain = lazy (prepareWith baseModel formats)

/// A calculation group as Power BI makes one: a Name column and an Ordinal column.
let private addGroup (db: Database) (name: string) (precedence: int) (items: (string * string * string option) list) =
    let t =
        new Table(Name = name, CalculationGroup = new CalculationGroup(Precedence = precedence))

    t.Columns.Add(new DataColumn(Name = name, DataType = DataType.String, SourceColumn = "Name"))
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

/// High (200): Pct (format string "0%"), Same (no format string), Named (a format string that
/// depends on the measure's name and falls back to SELECTEDMEASUREFORMATSTRING), Smfs (the
/// expression is the measure's format string), Uses Measure (a format string that evaluates
/// SELECTEDMEASURE), Blank Fs (a format string that is BLANK), Select Low (no format string, selects
/// an item of Low). Low (100): Lower Pct ("0.0%"), Low Same (no format string).
let private withGroups (db: Database) =
    formats db

    addGroup db "High" 200 [
        "Pct", "SELECTEDMEASURE () / 100", Some "\"0%\""
        "Same", "SELECTEDMEASURE () * 2", None
        "Named",
        "SELECTEDMEASURE ()",
        Some "IF ( SELECTEDMEASURENAME () = \"Plain\", \"#,0.0\", SELECTEDMEASUREFORMATSTRING () )"
        "Smfs", "SELECTEDMEASUREFORMATSTRING ()", None
        "Uses Measure", "SELECTEDMEASURE ()", Some "IF ( SELECTEDMEASURE () > 1, \"0\", \"0.0\" )"
        "Blank Fs", "SELECTEDMEASURE ()", Some "BLANK ()"
        "Select Low", "CALCULATE ( SELECTEDMEASURE (), Low[Low] = \"Lower Pct\" )", None
    ]

    addGroup db "Low" 100 [
        "Lower Pct", "SELECTEDMEASURE () / 1000", Some "\"0.0%\""
        "Low Same", "SELECTEDMEASURE ()", None
    ]

let private grouped = lazy (prepareWith baseModel withGroups)

let private formatted (p: Engine.Prepared) (dax: string) : FormattedResult =
    match Engine.evaluateQueryFormatted p dax with
    | Ok(r :: _) -> r
    | Ok [] -> failtest "the query has no EVALUATE"
    | Error e -> failtestf "query failed: %s\n%s" (TessError.message e) dax

let private formattedError (p: Engine.Prepared) (dax: string) : TessError =
    match Engine.evaluateQueryFormatted p dax with
    | Ok r -> failtestf "expected an error, got %A\n%s" r dax
    | Error e -> e

let private refused (p: Engine.Prepared) (dax: string) (mentions: string) =
    match formattedError p dax with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported, got %A\n%s" other dax

/// The cells of column `c`, row by row: (format string, text), or None.
let private column (r: FormattedResult) (c: int) =
    r.Cells
    |> List.map (fun row -> row.[c] |> Option.map (fun cell -> cell.FormatString, cell.Text))

/// Rows sorted by their first value, with the formatted cells of column `c`.
let private byKey (r: FormattedResult) (c: int) =
    List.zip r.Result.Rows (column r c)
    |> List.sortBy (fun (row, _) -> QueryValue.text row.[0])
    |> List.map snd

let private some (f: string) (text: string) = Some(f, text)

let private r (expression: string) = $"EVALUATE ROW ( \"r\", {expression} )"

let private summarize (measure: string) =
    $"EVALUATE SUMMARIZECOLUMNS ( T[Item], \"v\", {measure} )"

[<Tests>]
let tests =
    testList "engine: format strings" [
        testList "static format strings" [
            // A measure's FormatString, applied to each cell as FORMAT applies a format string.
            testCase "SUMMARIZECOLUMNS: each cell has the measure's format string"
            <| fun () ->
                let f = formatted (plain.Value) (summarize "[Total]")
                Expect.equal (byKey f 1) [ some "0.00" "10.00"; some "0.00" "20.00"; some "0.00" "30.00" ] ""
                Expect.equal (column f 0) [ None; None; None ] "a group-by column has none"

            testCase "ROW and ADDCOLUMNS, and T[Measure]"
            <| fun () ->
                let f = formatted (plain.Value) (r "T[Total]")
                Expect.equal (column f 0) [ some "0.00" "60.00" ] "ROW"

                let f =
                    formatted (plain.Value) "EVALUATE ADDCOLUMNS ( VALUES ( T[Item] ), \"v\", [Total] )"

                // A measure reference in a row context: context transition, so one value per row.
                Expect.equal (byKey f 1) [ some "0.00" "10.00"; some "0.00" "20.00"; some "0.00" "30.00" ] ""

            testCase "only a column that is a measure reference has one"
            <| fun () ->
                let f =
                    formatted
                        (plain.Value)
                        "EVALUATE ROW ( \"plain\", [Plain], \"sum\", [Total] + 1, \"text\", \"x\", \"paren\", ( [Total] ) )"

                Expect.equal (column f 0) [ None ] "a measure with no format string"
                Expect.equal (column f 1) [ None ] "an expression"
                Expect.equal (column f 2) [ None ] "a constant"
                Expect.equal (column f 3) [ some "0.00" "60.00" ] "a measure reference in parentheses"

            testCase "IGNORE ( [Measure] ) keeps it"
            <| fun () ->
                let f =
                    formatted (plain.Value) "EVALUATE SUMMARIZECOLUMNS ( T[Item], \"v\", IGNORE ( [Total] ) )"

                Expect.equal (byKey f 1) [ some "0.00" "10.00"; some "0.00" "20.00"; some "0.00" "30.00" ] ""

            // "Query measures do not have all the TOM properties; they only define the DAX code" (SQLBI).
            testCase "a query measure has none, also one that redefines a model measure"
            <| fun () ->
                let f =
                    formatted
                        (plain.Value)
                        "DEFINE MEASURE T[Q] = 1 MEASURE T[Total] = 2 EVALUATE ROW ( \"q\", [Q], \"t\", [Total] )"

                Expect.equal (column f 0) [ None ] "a new query measure"
                Expect.equal (column f 1) [ None ] "a model measure redefined"

            testCase "a BLANK cell is the empty text, and SUMMARIZECOLUMNS drops the same rows"
            <| fun () ->
                let q = summarize "[Blanky]"
                let f = formatted (plain.Value) q
                Expect.equal f.Result (query (plain.Value) q) "the same result as without format strings"
                Expect.equal (byKey f 1) [ some "0.0" "20.0"; some "0.0" "30.0" ] "A is BLANK: no row"

                let f = formatted (plain.Value) (r "CALCULATE ( [Blanky], T[Item] = \"A\" )")
                Expect.equal (column f 0) [ None ] "not a measure reference"

                let f =
                    formatted (plain.Value) "EVALUATE ADDCOLUMNS ( VALUES ( T[Item] ), \"v\", [Blanky] )"

                Expect.equal (byKey f 1) [ some "0.0" ""; some "0.0" "20.0"; some "0.0" "30.0" ] "BLANK is \"\""

            testCase "the result is the one evaluateQuery gives, with subtotals, ORDER BY and START AT"
            <| fun () ->
                for q in
                    [
                        "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( T[Item], \"IsTotal\" ), \"v\", [Total], \"d\", [Dyn] )"
                        "EVALUATE SUMMARIZECOLUMNS ( T[Item], \"v\", [Total] ) ORDER BY [v] DESC START AT 20"
                        "EVALUATE ADDCOLUMNS ( VALUES ( T[Item] ), \"v\", [Total] ) ORDER BY T[Item]"
                        "EVALUATE TOPN ( 1, SUMMARIZECOLUMNS ( T[Item], \"v\", [Total] ), [v] )"
                        "EVALUATE ROW ( \"a\", [Total] ) EVALUATE ROW ( \"b\", [Dyn] )"
                    ] do
                    match Engine.evaluateQueryFormatted (plain.Value) q, Engine.evaluateQuery (plain.Value) q with
                    | Ok f, Ok results ->
                        Expect.equal (f |> List.map _.Result) results q

                        for x in f do
                            Expect.equal x.Cells.Length x.Result.Rows.Length "one row of cells per row"
                    | Error e, _
                    | _, Error e -> failtestf "%s\n%s" (TessError.message e) q

            testCase "subtotal rows get their own format string"
            <| fun () ->
                let f =
                    formatted
                        (plain.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( ROLLUPADDISSUBTOTAL ( T[Item], \"IsTotal\" ), \"d\", [Dyn] )"

                let byItem =
                    List.zip f.Result.Rows (column f 2)
                    |> List.map (fun (row, cell) -> QueryValue.text row.[0], cell)
                    |> List.sort

                Expect.equal
                    byItem
                    [
                        "", some "0.0" "60.0"
                        "A", some "0" "10"
                        "B", some "0.0" "20.0"
                        "C", some "0.0" "30.0"
                    ]
                    ""

            testCase "a TOPN or another table function at the top has no format strings"
            <| fun () ->
                let f =
                    formatted (plain.Value) "EVALUATE TOPN ( 1, SUMMARIZECOLUMNS ( T[Item], \"v\", [Total] ), [v] )"

                Expect.equal (column f 1) [ None ] "only SUMMARIZECOLUMNS, ADDCOLUMNS and ROW at the top"
        ]

        testList "dynamic format strings" [
            // "it can change its value depending on the filter context or the value of the computed
            // measure"; "If you need the current value of a measure – from inside a format string –
            // you can use the SELECTEDMEASURE function" (SQLBI, "Introducing dynamic format strings").
            testCase "evaluated per cell, with SELECTEDMEASURE () the measure"
            <| fun () ->
                let f = formatted (plain.Value) (summarize "[Dyn]")
                Expect.equal (byKey f 1) [ some "0" "10"; some "0.0" "20.0"; some "0.0" "30.0" ] ""

            testCase "evaluated in the cell's filter context"
            <| fun () ->
                let f = formatted (plain.Value) (summarize "[ByItem]")
                Expect.equal (byKey f 1) [ some "0.000" "10.000"; some "0.0" "20.0"; some "0" "30" ] ""
                let f = formatted (plain.Value) (r "[ByItem]")
                Expect.equal (column f 0) [ some "#,0" "60" ] "several items: the alternate result"

            // "You can reference the measure itself in its dynamic format string directly by using
            // its name, such as [Measure A]" (Learn).
            testCase "it may name its own measure"
            <| fun () ->
                let f = formatted (plain.Value) (summarize "[Self]")
                Expect.equal (byKey f 1) [ some "0" "10"; some "0" "20"; some "0.0" "30.0" ] ""

            testCase "in ADDCOLUMNS, after the context transition"
            <| fun () ->
                let f =
                    formatted (plain.Value) "EVALUATE ADDCOLUMNS ( VALUES ( T[Item] ), \"v\", [Dyn] )"

                Expect.equal (byKey f 1) [ some "0" "10"; some "0.0" "20.0"; some "0.0" "30.0" ] ""

            // "The internal measure for the format string is [_Sales Amount FormatString]. However,
            // to reference such an expression in DAX, the code must be Sales[_Sales Amount
            // FormatString]" (SQLBI).
            testCase "the internal measure Table[_<measure> FormatString]"
            <| fun () ->
                returns
                    (plain.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( T[Item], \"v\", [Dyn], \"f\", IGNORE ( T[_Dyn FormatString] ) )"
                    [ [ "A"; 10; "0" ]; [ "B"; 20; "0.0" ]; [ "C"; 30; "0.0" ] ]

                is (plain.Value) "'T'[_ByItem FormatString]" "#,0"
                is (plain.Value) "CALCULATE ( T[_ByItem FormatString], T[Item] = \"B\" )" "0.0"
                // A row context: the reference is a measure reference, so a context transition.
                returns (plain.Value) "EVALUATE ADDCOLUMNS ( VALUES ( T[Item] ), \"f\", T[_Dyn FormatString] )" [
                    [ "A"; "0" ]
                    [ "B"; "0.0" ]
                    [ "C"; "0.0" ]
                ]

            // "The internal measure exists only for those with a dynamic format string."
            testCase "there is none for a static format string, nor without the table"
            <| fun () ->
                match queryError (plain.Value) (r "T[_Total FormatString]") with
                | DaxInvalid _ -> ()
                | other -> failtestf "expected dax-invalid, got %A" other

                match queryError (plain.Value) (r "[_Dyn FormatString]") with
                | DaxInvalid _ -> ()
                | other -> failtestf "expected dax-invalid, got %A" other

            testCase "a query that redefines the internal measure is used as defined, and --formatted refuses"
            <| fun () ->
                let q =
                    "DEFINE MEASURE T[_Dyn FormatString] = \"x\" EVALUATE ROW ( \"f\", T[_Dyn FormatString] )"

                Expect.equal (rows (plain.Value) q) [ [ QueryValue.String "x" ] ] ""

                refused
                    (plain.Value)
                    "DEFINE MEASURE T[_Dyn FormatString] = \"x\" EVALUATE ROW ( \"v\", [Dyn] )"
                    "defines the measure [_Dyn FormatString]"

            // "The format string expression must return a scalar string" (Learn).
            testCase "a format string that is BLANK, empty or not text is refused"
            <| fun () ->
                refused (plain.Value) (r "[BadBlank]") "returns BLANK"
                refused (plain.Value) (r "[BadEmpty]") "empty text"
                refused (plain.Value) (r "[BadNumber]") "not text"

            testCase "SELECTEDMEASUREFORMATSTRING in a measure's own format string is refused"
            <| fun () ->
                refused (plain.Value) (r "[OwnSmfs]") "SELECTEDMEASUREFORMATSTRING in the dynamic format string"

            testCase "a format string FORMAT doesn't implement is refused"
            <| fun () -> refused (plain.Value) (r "[Unformattable]") "formatting [r]"
        ]

        testList "calculation items" [
            // "YOY%, however, should be a percentage regardless of the format of the base measure"
            // (Learn): the item's format string expression replaces the measure's.
            testCase "an item's format string expression is the cell's"
            <| fun () ->
                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Pct\" }, High[High] ), \"v\", [Total] )"

                Expect.equal (column f 0) [ some "0%" "60%" ] "60 / 100, formatted 0%"

                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( T[Item], TREATAS ( { \"Pct\" }, High[High] ), \"v\", [Plain] )"

                Expect.equal (byKey f 1) [ some "0%" "10%"; some "0%" "20%"; some "0%" "30%" ] ""

            // "all the calculation items except YOY% should use the format of the current measure in
            // context" (Learn).
            testCase "an item without one leaves the measure's"
            <| fun () ->
                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Same\" }, High[High] ), \"v\", [Total] )"

                Expect.equal (column f 0) [ some "0.00" "120.00" ] ""

                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Same\" }, High[High] ), \"v\", [Plain] )"

                Expect.equal (column f 0) [ None ] "a measure with none"

            testCase "grouping by the group's column gives each item's"
            <| fun () ->
                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( High[High], TREATAS ( { \"Pct\", \"Same\" }, High[High] ), \"v\", [Total] )"

                Expect.equal (byKey f 1) [ some "0%" "60%"; some "0.00" "120.00" ] ""

            // SELECTEDMEASURENAME and SELECTEDMEASUREFORMATSTRING in a format string expression
            // (SQLBI, "Controlling format strings in calculation groups").
            testCase "an item's format string expression can read the measure's name and format string"
            <| fun () ->
                let named (m: string) =
                    column
                        (formatted
                            (grouped.Value)
                            $"EVALUATE SUMMARIZECOLUMNS ( TREATAS ( {{ \"Named\" }}, High[High] ), \"v\", [{m}] )")
                        0

                Expect.equal (named "Plain") [ some "#,0.0" "60.0" ] "by the name"
                Expect.equal (named "Total") [ some "0.00" "60.00" ] "the measure's own"
                Expect.equal (named "Const") [ some "0.0" "60.0" ] "a dynamic one that refers to no measure"

            // "A string holding the format string of the measure that is currently in context" (Learn).
            testCase "SELECTEDMEASUREFORMATSTRING of a dynamic format string is its value there"
            <| fun () ->
                is (grouped.Value) "CALCULATE ( [Const], High[High] = \"Smfs\" )" "0.0"
                is (grouped.Value) "CALCULATE ( [ByItem], High[High] = \"Smfs\", T[Item] = \"A\" )" "0.000"

            testCase "SELECTEDMEASUREFORMATSTRING of a dynamic format string that evaluates a measure is refused"
            <| fun () ->
                notSupported (grouped.Value) (r "CALCULATE ( [Dyn], High[High] = \"Smfs\" )") "SELECTEDMEASURE ()"
                notSupported (grouped.Value) (r "CALCULATE ( [Self], High[High] = \"Smfs\" )") "the measure [Self]"

            testCase "a dynamic format string under an item without one: only if it evaluates no measure"
            <| fun () ->
                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Same\" }, High[High] ), \"v\", [Const] )"

                Expect.equal (column f 0) [ some "0.0" "120.0" ] ""

                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Same\" }, High[High] ), \"v\", [Dyn] )"
                    "under the calculation item 'High'[Same]"

            // The sources disagree (FormatStrings.fs): Learn has the highest one "the only one
            // applied", SQLBI the lower one see the higher one's.
            testCase "two items with format string expressions selected are refused"
            <| fun () ->
                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Pct\" }, High[High] ), TREATAS ( { \"Lower Pct\" }, Low[Low] ), \"v\", [Total] )"
                    "the sources disagree"

                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Same\" }, High[High] ), TREATAS ( { \"Lower Pct\" }, Low[Low] ), \"v\", [Total] )"
                    "the sources disagree"

                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Select Low\" }, High[High] ), \"v\", [Total] )"
                    "lower precedence"

            testCase "a lower item without a format string expression changes nothing"
            <| fun () ->
                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Pct\" }, High[High] ), TREATAS ( { \"Low Same\" }, Low[Low] ), \"v\", [Total] )"

                Expect.equal (column f 0) [ some "0%" "60%" ] ""

                let f =
                    formatted
                        (grouped.Value)
                        "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Lower Pct\" }, Low[Low] ), \"v\", [Total] )"

                Expect.equal (column f 0) [ some "0.0%" "6.0%" ] "the only item applied"

            testCase "an item's format string expression that evaluates a measure, or is BLANK, is refused"
            <| fun () ->
                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Uses Measure\" }, High[High] ), \"v\", [Total] )"
                    "SELECTEDMEASURE ()"

                refused
                    (grouped.Value)
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Blank Fs\" }, High[High] ), \"v\", [Total] )"
                    "returns BLANK"

            testCase "SELECTEDMEASUREFORMATSTRING across two items with format string expressions is refused"
            <| fun () ->
                // In Named (High) with Lower Pct (Low, which has one) selected below it.
                notSupported
                    (grouped.Value)
                    (r "CALCULATE ( [Total], High[High] = \"Smfs\", Low[Low] = \"Lower Pct\" )")
                    "the sources disagree"

            testCase "the internal measure is refused in a model with calculation groups"
            <| fun () -> notSupported (grouped.Value) (r "T[_Dyn FormatString]") "calculation groups"

            testCase "without --formatted nothing is evaluated: a query with an item that would refuse works"
            <| fun () ->
                let q =
                    "EVALUATE SUMMARIZECOLUMNS ( TREATAS ( { \"Pct\" }, High[High] ), TREATAS ( { \"Lower Pct\" }, Low[Low] ), \"v\", [Total] )"

                Expect.equal (rows (grouped.Value) q) [ [ QueryValue.Double 0.0006 ] ] "60 / 1000 / 100"
        ]

        testList "the probe" [
            testCase "can't be called from DAX and isn't listed"
            <| fun () ->
                match queryError (plain.Value) (r "FORMAT STRING OF ( [Total] )") with
                | DaxSyntax _
                | DaxInvalid _ -> ()
                | other -> failtestf "expected a syntax error, got %A" other

                Expect.isFalse
                    (Engine.functions |> List.exists (fun (name, _) -> name.Contains ' '))
                    "no internal function in Engine.functions"
        ]

        testList "tess query --formatted" [
            testCase "text, CSV and JSON"
            <| fun () ->
                use t = new TempDir()
                let db, store = build baseModel
                formats db
                let model = t.Combine "model.bim"
                File.WriteAllText(model, JsonSerializer.SerializeDatabase db)
                let dir = t.Combine "data"
                Directory.CreateDirectory dir |> ignore

                for table in store.Tables do
                    use writer =
                        new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

                    Fixture.writeCsv writer table

                let q =
                    "EVALUATE SUMMARIZECOLUMNS ( T[Item], \"Total\", [Total], \"Dyn\", [Dyn], \"Plain\", [Plain] ) ORDER BY T[Item]"

                let text = tess [ "query"; model; "--data"; dir; "--formatted"; q ]
                Expect.equal text.Code ExitCode.ok text.Err
                Expect.stringContains text.Out "10.00" "formatted"
                Expect.stringContains text.Out "20.0" "dynamic"

                let csv = tess [ "query"; model; "--data"; dir; "--formatted"; "--csv"; q ]
                Expect.equal csv.Code ExitCode.ok csv.Err

                Expect.equal
                    (csv.Out.Replace("\r\n", "\n"))
                    "T[Item],[Total],[Dyn],[Plain]\nA,10.00,10,10\nB,20.00,20.0,20\nC,30.00,30.0,30\n"
                    ""

                let json = tess [ "query"; model; "--data"; dir; "--formatted"; "--json"; q ]
                Expect.equal json.Code ExitCode.ok json.Err
                let result = JsonDocument.Parse(json.Out).RootElement.GetProperty("results").[0]
                let columns = result.GetProperty "columns"
                let mutable none = Unchecked.defaultof<JsonElement>
                Expect.isFalse (columns.[0].TryGetProperty("formatString", &none)) "the group-by column has none"
                Expect.equal (columns.[1].GetProperty("formatString").GetString()) "0.00" "one for every row"

                Expect.equal
                    [
                        for f in columns.[2].GetProperty("formatStrings").EnumerateArray() -> f.GetString()
                    ]
                    [ "0"; "0.0"; "0.0" ]
                    "one per row"

                Expect.isFalse (columns.[3].TryGetProperty("formatString", &none)) "no format string"
                Expect.isFalse (columns.[3].TryGetProperty("formatStrings", &none)) "no format strings"
                Expect.equal (result.GetProperty("rows").[0].[1].GetInt64()) 10L "rows keep the values"
                let first = result.GetProperty("formatted").[0]
                Expect.equal first.[0].ValueKind JsonValueKind.Null "nothing to format"
                Expect.equal (first.[1].GetString()) "10.00" ""
                Expect.equal (first.[2].GetString()) "10" ""
                Expect.equal first.[3].ValueKind JsonValueKind.Null ""

                let capped =
                    tess [ "query"; model; "--data"; dir; "--formatted"; "--json"; "--max-rows"; "1"; q ]

                Expect.equal capped.Code ExitCode.ok capped.Err
                let result = JsonDocument.Parse(capped.Out).RootElement.GetProperty("results").[0]
                Expect.equal (result.GetProperty("formatted").GetArrayLength()) 1 "cut with the rows"
                Expect.isTrue (result.GetProperty("truncated").GetBoolean()) ""

                Expect.equal
                    (result.GetProperty("columns").[2].GetProperty("formatString").GetString())
                    "0"
                    "the rows shown"

                // Without the flag, the JSON is as it always was.
                let before = tess [ "query"; model; "--data"; dir; "--json"; q ]
                Expect.equal before.Code ExitCode.ok before.Err
                let result = JsonDocument.Parse(before.Out).RootElement.GetProperty("results").[0]
                Expect.isFalse (result.TryGetProperty("formatted", &none)) "no formatted"

                Expect.isFalse
                    (result.GetProperty("columns").[1].TryGetProperty("formatString", &none))
                    "no formatString"

                let refusal =
                    tess [ "query"; model; "--data"; dir; "--formatted"; r "[Unformattable]" ]

                Expect.equal refusal.Code ExitCode.input refusal.Err
                Expect.stringContains refusal.Err "Not supported" ""

            testCase "it is local only"
            <| fun () ->
                let remote =
                    tess [ "query"; "xmla:localhost;Db"; "--formatted"; "EVALUATE ROW ( \"a\", 1 )" ]

                Expect.equal remote.Code ExitCode.usage remote.Err
                Expect.stringContains remote.Err "no format strings" ""

                use t = new TempDir()
                let model = t.Combine "model.bim"
                let db, _ = build baseModel
                File.WriteAllText(model, JsonSerializer.SerializeDatabase db)
                let coverage = tess [ "query"; model; "--coverage"; "--formatted" ]
                Expect.equal coverage.Code ExitCode.usage coverage.Err
        ]
    ]
