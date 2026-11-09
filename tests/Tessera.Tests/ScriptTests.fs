module Tessera.Tests.ScriptTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.DaxScript
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private export (db: Database) (sel: ExportScript.Selection) = ExportScript.export db sel |> ok

let private exportAll (db: Database) = export db ExportScript.Selection.all

/// Apply a script's text to db through the editing pipeline.
let private apply (db: Database) (text: string) =
    Mutation.apply db (fun db -> ApplyScript.applyText "test.dax" db text)

let private applyOk db text = apply db text |> ok

let private applyError db text =
    match apply db text with
    | Ok a -> failtestf "expected an error, got %A" (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref))
    | Error e -> e

let private changed (a: Mutation.Applied) =
    a.Changes
    |> List.map (fun c -> ChangeSet.ChangeKind.token c.ChangeKind, ObjectRef.format c.Ref)

/// A model with every kind of DAX a script holds, and the awkward cases: multi-line expressions with
/// trailing comments, \r\n, leading and trailing whitespace, multi-line strings, dynamic format
/// strings, KPIs, detail rows, quotes and brackets in names.
let richDatabase () =
    let db = new Database(Name = "Rich", ID = "Rich", CompatibilityLevel = 1702)

    let m =
        new Model(Name = "Model", Culture = "en-US", DiscourageImplicitMeasures = true)

    db.Model <- m

    let sales = new Table(Name = "Sales")

    sales.Columns.Add(new DataColumn(Name = "Amount", DataType = DataType.Decimal, SourceColumn = "Amount"))

    sales.Columns.Add(
        new CalculatedColumn(
            Name = "Double",
            Expression = "Sales[Amount] * 2",
            DataType = DataType.Double,
            FormatString = "0.00",
            Description = "Twice the amount",
            DisplayFolder = "Calc"
        )
    )

    let measure name expression =
        let x = new Measure(Name = name, Expression = expression)
        sales.Measures.Add x
        x

    measure "Plain" "SUM(Sales[Amount])" |> ignore
    measure "Multi" "VAR x = 1\nRETURN\n    x + 1\n-- old: x\n" |> ignore
    measure "CRLF" "VAR x = 1\r\nRETURN\r\n\tx" |> ignore
    measure "Padded" "\n    SUM ( Sales[Amount] )\n  " |> ignore
    measure "Strings" "\"line1\n  line2\" & \"say \"\"hi\"\"\"" |> ignore
    measure "Same line comment" "1 -- note" |> ignore
    measure "Block" "/* a\n b */ 1" |> ignore
    measure "Visible var" "VAR Visible = 1\nRETURN\nVisible = 1" |> ignore
    measure "Empty" "" |> ignore
    measure "A]B" "[Plain]" |> ignore

    let dynamic = measure "Dynamic" "[Plain]"

    dynamic.FormatStringDefinition <- new FormatStringDefinition(Expression = "IF([Plain] > 1, \"0\", \"0.00\")")

    let literal = measure "Literal dynamic" "[Plain]"
    literal.FormatStringDefinition <- new FormatStringDefinition(Expression = "\"0.0\"")

    let both = measure "Both" "[Plain]"
    both.FormatString <- "0"
    both.FormatStringDefinition <- new FormatStringDefinition(Expression = "\"#\"")

    let kpi = measure "With KPI" "[Plain] / 100"

    kpi.KPI <-
        new KPI(
            TargetExpression = "100",
            TargetDescription = "The target",
            TargetFormatString = "0",
            StatusExpression = "VAR x = [Plain]\nRETURN\n    IF(x < 1, -1, 1)",
            StatusGraphic = "Shapes",
            TrendExpression = "1",
            TrendGraphic = "Standard Arrow"
        )

    let detail = measure "Detail" "[Plain]"

    detail.DetailRowsDefinition <- new DetailRowsDefinition(Expression = "SELECTCOLUMNS(Sales, \"A\", Sales[Amount])")

    let described = measure "Described" "[Plain]"
    described.Description <- "He said \"hi\"\nsecond line"
    described.DisplayFolder <- "A\\B"
    described.IsHidden <- true

    sales.Partitions.Add(new Partition(Name = "Sales", Source = new MPartitionSource(Expression = "let x = 1 in x")))
    m.Tables.Add sales

    let copy =
        new Table(Name = "Copy", Description = "A calculated table", IsHidden = true)

    copy.Partitions.Add(new Partition(Name = "Copy", Source = new CalculatedPartitionSource(Expression = "Sales")))
    copy.DefaultDetailRowsDefinition <- new DetailRowsDefinition(Expression = "Sales")
    m.Tables.Add copy

    let quoted = new Table(Name = "O'Brien")

    quoted.Columns.Add(new DataColumn(Name = "Key", DataType = DataType.Int64, SourceColumn = "Key"))
    quoted.Measures.Add(new Measure(Name = "Count", Expression = "COUNTROWS('O''Brien')"))
    quoted.Partitions.Add(new Partition(Name = "O'Brien", Source = new MPartitionSource(Expression = "1")))
    m.Tables.Add quoted

    let ti =
        new Table(
            Name = "Time Intelligence",
            Description = "Time calculations",
            CalculationGroup = new CalculationGroup(Precedence = 3)
        )

    ti.Columns.Add(new DataColumn(Name = "Period", DataType = DataType.String, SourceColumn = "Name"))

    ti.CalculationGroup.CalculationItems.Add(
        new CalculationItem(Name = "Current", Expression = "SELECTEDMEASURE()", Ordinal = 0)
    )

    let pct =
        new CalculationItem(
            Name = "YoY %",
            Expression = "DIVIDE(\n    SELECTEDMEASURE(),\n    1\n)",
            Ordinal = 1,
            Description = "Year over year"
        )

    pct.FormatStringDefinition <- new FormatStringDefinition(Expression = "\"0.0%\"")
    ti.CalculationGroup.CalculationItems.Add pct
    ti.Partitions.Add(new Partition(Name = "Time Intelligence", Source = new CalculationGroupSource()))
    m.Tables.Add ti

    m.Functions.Add(new Function(Name = "Lib.Twice", Expression = "(x: INT64) =>\n    x * 2", Description = "x2"))
    m.Functions.Add(new Function(Name = "Hidden", Expression = "() => 1", IsHidden = true))
    db

let private shapes (stmts: Statement list) =
    stmts
    |> List.map (fun s -> s.Type, s.Table, s.Name, s.Expression, s.Properties |> List.map (fun p -> p.Name, p.Value))

/// The sample model's script, exactly.
let private sampleScript =
    """-------------------
-- Measure: [Total]
-------------------
MEASURE 'Sales'[Total] = SUM(Sales[Amount])
    FormatString = "#,0"

----------------------------
-- Measure: [Total (hidden)]
----------------------------
MEASURE 'Sales'[Total (hidden)] = [Total]
    Visible = FALSE

-----------------------------------------
-- Calculation Group: 'Time Intelligence'
-----------------------------------------
CALCULATIONGROUP 'Time Intelligence'[Period]
    Precedence = 0

    CALCULATIONITEM "Current" = SELECTEDMEASURE()
        Ordinal = 0

    CALCULATIONITEM "YTD" = TOTALYTD(SELECTEDMEASURE(), 'Date'[DateKey])
        Ordinal = 1

-------------------
-- Function: Double
-------------------
FUNCTION Double = (x: INT64) => x * 2
"""

/// Lines of DAX, some awkward, for generated expressions.
let private fragments = [
    "SUM ( Sales[Amount] )"
    "VAR x = 1"
    "RETURN"
    "x + 1"
    "-- a comment"
    "// another"
    "/* block */ 1"
    "\"a string\""
    "\"a multi-line\nstring\""
    "Visible = 1"
    "Visible = TRUE"
    "Description = \"d\""
    "Ordinal = 2"
    "IF ( [Total] > 0,"
    "    [Total],"
    "    BLANK () )"
    "[Total]"
    "'Sales'[Amount]"
    ""
    "   "
    "\t"
    "MEASURE"
    "a, b"
]

let private expressionGen =
    gen {
        let! lines =
            Gen.listOf (
                gen {
                    let! indent = Gen.elements [ ""; " "; "    "; "\t"; "        " ]
                    let! text = Gen.elements fragments
                    let! trailing = Gen.elements [ ""; ""; " "; "\t" ]
                    return indent + text + trailing
                }
            )

        let! eol = Gen.elements [ "\n"; "\r\n" ]
        return String.concat eol lines
    }

[<Tests>]
let tests =
    testList "DAX scripts" [
        testList "export" [
            testCase "the sample model's script"
            <| fun () ->
                let e = exportAll (sampleDatabase ())
                Expect.equal e.Text sampleScript ""
                Expect.isEmpty e.Warnings ""

            testCase "the order: each table's own DAX, its calculated columns, its measures; then functions"
            <| fun () ->
                let refs =
                    (exportAll (richDatabase ())).Statements
                    |> List.map (ExportScript.refOf >> ObjectRef.format)

                Expect.equal
                    refs
                    [
                        "column:Sales/Double"
                        "measure:Sales/Plain"
                        "measure:Sales/Multi"
                        "measure:Sales/CRLF"
                        "measure:Sales/Padded"
                        "measure:Sales/Strings"
                        "measure:Sales/Same line comment"
                        "measure:Sales/Block"
                        "measure:Sales/Visible var"
                        "measure:Sales/Empty"
                        "measure:Sales/A]B"
                        "measure:Sales/Dynamic"
                        "measure:Sales/Literal dynamic"
                        "measure:Sales/Both"
                        "measure:Sales/With KPI"
                        "measure:Sales/Detail"
                        "measure:Sales/Described"
                        "table:Copy"
                        "measure:O'Brien/Count"
                        "table:Time Intelligence"
                        "calcitem:Time Intelligence/Current"
                        "calcitem:Time Intelligence/YoY %"
                        "function:Lib.Twice"
                        "function:Hidden"
                    ]
                    ""

            testCase "how properties are written"
            <| fun () ->
                let text = (exportAll (richDatabase ())).Text

                for fragment in
                    [
                        "MEASURE 'Sales'[Dynamic] = [Plain]\n    FormatString = IF([Plain] > 1, \"0\", \"0.00\")\n"
                        "MEASURE 'Sales'[Literal dynamic] = [Plain]\n    FormatStringDefinition = \"0.0\"\n"
                        "MEASURE 'Sales'[Both] = [Plain]\n    FormatString = \"0\"\n    FormatStringDefinition = \"#\"\n"
                        "    Description = \"He said \"\"hi\"\"\nsecond line\"\n    DisplayFolder = \"A\\B\"\n    Visible = FALSE\n"
                        "    KpiStatusExpression =\n        VAR x = [Plain]\n        RETURN\n            IF(x < 1, -1, 1)\n"
                        "    KpiTargetExpression = 100\n    KpiTargetDescription = \"The target\"\n    KpiTargetFormatString = \"0\"\n"
                        "MEASURE 'Sales'[Multi] =\n    VAR x = 1\n    RETURN\n        x + 1\n    -- old: x\n"
                        "MEASURE 'Sales'[CRLF] =\n    VAR x = 1\n    RETURN\n    \tx\n"
                        "MEASURE 'Sales'[Padded] = SUM ( Sales[Amount] )\n"
                        "MEASURE 'Sales'[Empty] =\n\n"
                        "COLUMN 'Sales'[Double] = Sales[Amount] * 2\n    Description = \"Twice the amount\"\n    FormatString = \"0.00\"\n    DisplayFolder = \"Calc\"\n    Datatype = DOUBLE\n"
                        "TABLE 'Copy' = Sales\n    Description = \"A calculated table\"\n    Visible = FALSE\n    DetailRows = Sales\n"
                        "MEASURE 'O''Brien'[Count] = COUNTROWS('O''Brien')\n"
                        "CALCULATIONGROUP 'Time Intelligence'[Period]\n    Description = \"Time calculations\"\n    Precedence = 3\n"
                        "    CALCULATIONITEM \"YoY %\" =\n        DIVIDE(\n            SELECTEDMEASURE(),\n            1\n        )\n        Description = \"Year over year\"\n        FormatString = \"0.0%\"\n        Ordinal = 1\n"
                        "FUNCTION Lib.Twice =\n    (x: INT64) =>\n        x * 2\n    Description = \"x2\"\n"
                        "FUNCTION Hidden = () => 1\n    Visible = FALSE\n"
                    ] do
                    Expect.stringContains text fragment ""

            testCase "selectors: a table stands for its DAX objects, an item brings its group's header"
            <| fun () ->
                let db = richDatabase ()

                let refs (sel: ExportScript.Selection) =
                    (export db sel).Statements |> List.map (ExportScript.refOf >> ObjectRef.format)

                let pick selectors = {
                    ExportScript.Selection.all with
                        Selectors = selectors
                }

                Expect.equal
                    (refs (pick [ "'O''Brien'"; "[Plain]"; "Copy" ]))
                    [ "measure:Sales/Plain"; "table:Copy"; "measure:O'Brien/Count" ]
                    "model order, not argument order"

                Expect.equal
                    (refs (pick [ "calcitem:Time Intelligence/YoY %" ]))
                    [ "table:Time Intelligence"; "calcitem:Time Intelligence/YoY %" ]
                    ""

                Expect.equal
                    (refs (pick [ "Time Intelligence" ]))
                    [
                        "table:Time Intelligence"
                        "calcitem:Time Intelligence/Current"
                        "calcitem:Time Intelligence/YoY %"
                    ]
                    ""

                Expect.equal
                    (refs {
                        ExportScript.Selection.all with
                            Kinds = [ ObjectKind.Column; ObjectKind.Function ]
                    })
                    [ "column:Sales/Double"; "function:Lib.Twice"; "function:Hidden" ]
                    "-k"

                Expect.equal
                    (refs {
                        ExportScript.Selection.all with
                            Tables = [ "o'brien"; "copy" ]
                    })
                    [ "table:Copy"; "measure:O'Brien/Count" ]
                    "-t, any case"

                Expect.equal
                    (refs {
                        ExportScript.Selection.all with
                            Kinds = [ ObjectKind.CalculationItem ]
                    })
                    [
                        "table:Time Intelligence"
                        "calcitem:Time Intelligence/Current"
                        "calcitem:Time Intelligence/YoY %"
                    ]
                    "items keep their header"

            testCase "objects without DAX can't be selected"
            <| fun () ->
                let db = richDatabase ()

                for selector in [ "Sales[Amount]"; "partition:Sales/Sales"; "model" ] do
                    match
                        ExportScript.export db {
                            ExportScript.Selection.all with
                                Selectors = [ selector ]
                        }
                    with
                    | Error(CannotScript _) -> ()
                    | other -> failtestf "%s: %A" selector other

                match
                    ExportScript.export db {
                        ExportScript.Selection.all with
                            Selectors = [ "[Nope]" ]
                    }
                with
                | Error(ObjectNotFound _ | ObjectNotFoundSuggest _) -> ()
                | other -> failtestf "%A" other

            testCase "an expression that wouldn't read back is reported"
            <| fun () ->
                let db = sampleDatabase ()
                let sales = db.Model.Tables.["Sales"]
                sales.Measures.["Total"].Expression <- "VAR Visible = 1\nRETURN\nVisible = TRUE"
                sales.Measures.Add(new Measure(Name = "Comma", Expression = "1,1"))
                let e = exportAll db

                Expect.equal
                    (e.Warnings |> List.map (fun w -> w.Split(':').[0] + ":" + w.Split(':').[1]))
                    [ "measure:Sales/Total"; "measure:Sales/Comma" ]
                    "only those two"
        ]

        testList "round trip" [
            testCase "export then apply changes nothing: the sample model"
            <| fun () ->
                let db = sampleDatabase ()
                let a = applyOk db (exportAll db).Text
                Expect.isEmpty a.Changes ""
                Expect.equal a.Effect.Notes [ "Applied 6 objects from test.dax (0 new)" ] ""

            testCase "export then apply changes nothing: every awkward case"
            <| fun () ->
                let db = richDatabase ()
                let e = exportAll db
                Expect.isEmpty e.Warnings ""
                let a = applyOk db e.Text
                Expect.isEmpty (changed a) ""

            testCase "\\r\\n line breaks in the script change nothing"
            <| fun () ->
                let db = richDatabase ()
                let a = applyOk db ((exportAll db).Text.Replace("\n", "\r\n"))
                Expect.isEmpty (changed a) ""

            testCase "applied to a model without them, the script recreates the objects"
            <| fun () ->
                let original = exportAll (richDatabase ())

                let db = richDatabase ()

                for name in [ "Copy"; "Time Intelligence" ] do
                    db.Model.Tables.Remove(db.Model.Tables.[name]) |> ignore

                let sales = db.Model.Tables.["Sales"]
                sales.Measures.Clear()
                sales.Columns.Remove(sales.Columns.["Double"]) |> ignore
                db.Model.Tables.["O'Brien"].Measures.Clear()
                db.Model.Functions.Clear()

                let a = applyOk db original.Text
                Expect.isEmpty a.IntroducedErrors "references between new objects resolve"
                Expect.stringContains a.Effect.Notes.Head "(24 new)" ""

                let again = exportAll db

                Expect.equal
                    (Set.ofList (shapes again.Statements))
                    (Set.ofList (shapes original.Statements))
                    "the same objects, with the same DAX and properties"

            testProperty "any measure expression either reads back unchanged, or export says it won't"
            <| Prop.forAll (Arb.fromGen expressionGen) (fun expression ->
                let db = sampleDatabase ()
                let total = db.Model.Tables.["Sales"].Measures.["Total"]
                total.Expression <- expression
                total.Description <- "d"
                let e = exportAll db

                if e.Warnings |> List.exists (fun w -> w.StartsWith "measure:Sales/Total:") then
                    true
                else
                    let a = applyOk db e.Text
                    a.Changes.IsEmpty)
        ]

        testCase "TESSERA_SAMPLE_MODEL: export then apply changes nothing"
        <| fun () ->
            match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
            | null
            | "" -> skiptest "TESSERA_SAMPLE_MODEL is not set"
            | path ->
                let load () = ModelIO.loadPath path |> ok |> snd
                let e = exportAll (load ())
                Expect.isEmpty e.Warnings "every object reads back"
                // Compared with an edit that does nothing, so differences the snapshot itself shows don't count.
                let baseline = Mutation.apply (load ()) (fun _ -> Ok Mutation.Effect.none) |> ok
                let a = applyOk (load ()) e.Text
                Expect.equal (changed a) (changed baseline) ""

        testList "apply" [
            testCase "creates and updates what the script names, and leaves the rest alone"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk
                        db
                        "MEASURE 'Sales'[Total] = SUMX(Sales, Sales[Amount])\nMEASURE Sales[New] = [Total] * 2\n    DisplayFolder = \"KPIs\""

                Expect.equal (changed a) [ "modified", "measure:Sales/Total"; "created", "measure:Sales/New" ] ""

                let sales = db.Model.Tables.["Sales"]
                Expect.equal sales.Measures.["Total"].FormatString "#,0" "properties not in the script are kept"
                Expect.equal sales.Measures.["New"].DisplayFolder "KPIs" ""
                Expect.equal a.Effect.Notes [ "Applied 2 objects from test.dax (1 new)" ] ""

            testCase "without `=` the expression is kept; with an empty one it's cleared"
            <| fun () ->
                let db = sampleDatabase ()

                applyOk
                    db
                    "MEASURE 'Sales'[Total]\n    Description = \"Sum\"\nMEASURE 'Sales'[Total (hidden)] =\n    , Visible = TRUE"
                |> ignore

                let sales = db.Model.Tables.["Sales"]
                Expect.equal sales.Measures.["Total"].Expression "SUM(Sales[Amount])" ""
                Expect.equal sales.Measures.["Total"].Description "Sum" ""
                Expect.equal sales.Measures.["Total (hidden)"].Expression "" ""
                Expect.isFalse sales.Measures.["Total (hidden)"].IsHidden ""

            testCase "objects may refer to ones defined later in the script"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk
                        db
                        "MEASURE 'Sales'[A] = [B] + 1\nMEASURE 'Sales'[B] = [C] * 2\nCOLUMN 'Sales'[C] = Sales[Amount]"

                Expect.isEmpty a.IntroducedErrors ""
                Expect.equal a.Changes.Length 3 ""

            testCase "broken references are left to the save gate"
            <| fun () ->
                let a = applyOk (sampleDatabase ()) "MEASURE 'Sales'[A] = 'Nope'[X]"
                Expect.equal (a.IntroducedErrors |> List.map (fun i -> i.Rule)) [ "unresolved-reference" ] ""

            testCase "a new measure in an unknown table: an error at its line, with suggestions"
            <| fun () ->
                match applyError (sampleDatabase ()) "MEASURE 'Sales'[A] = 1\n\nMEASURE 'Sale'[B] = 2" with
                | ScriptObjectFailed("test.dax", 3, 1, ObjectNotFoundSuggest(_, candidates)) as e ->
                    Expect.contains candidates "table:Sales" ""
                    Expect.stringStarts (TessError.message e) "test.dax:3:1: No object matches" ""
                | other -> failtestf "%A" other

            testCase "a measure name taken in another table is a collision"
            <| fun () ->
                match applyError (sampleDatabase ()) "MEASURE 'Date'[Total] = 1" with
                | ScriptObjectFailed(_, 1, 1, NameCollision _) -> ()
                | other -> failtestf "%A" other

            testCase "a new object needs an expression"
            <| fun () ->
                match applyError (sampleDatabase ()) "MEASURE 'Sales'[New]\n    Description = \"x\"" with
                | ScriptObjectFailed(_, 1, 1, CannotScript(_, reason)) ->
                    Expect.stringContains reason "needs an expression" ""
                | other -> failtestf "%A" other

            testCase "a data column's expression and a data table's can't be set"
            <| fun () ->
                for script in [ "COLUMN 'Sales'[Amount] = 1"; "TABLE 'Sales' = Date" ] do
                    match applyError (sampleDatabase ()) script with
                    | ScriptObjectFailed(_, 1, 1, CannotScript _) -> ()
                    | other -> failtestf "%s: %A" script other

                // Properties alone are fine on any column or table.
                let db = sampleDatabase ()

                applyOk db "COLUMN 'Sales'[Amount]\n    Visible = FALSE\nTABLE 'Sales'\n    Description = \"Facts\""
                |> ignore

                Expect.isTrue db.Model.Tables.["Sales"].Columns.["Amount"].IsHidden ""
                Expect.equal db.Model.Tables.["Sales"].Description "Facts" ""

            testCase "format strings: static replaces dynamic, DAX sets the definition"
            <| fun () ->
                let db = sampleDatabase ()
                let total = db.Model.Tables.["Sales"].Measures.["Total"]

                applyOk db "MEASURE 'Sales'[Total]\n    FormatString = IF([Total] > 1, \"0\", \"0.0\")"
                |> ignore

                Expect.equal total.FormatStringDefinition.Expression "IF([Total] > 1, \"0\", \"0.0\")" ""
                Expect.equal total.FormatString "#,0" "the static one stays"
                applyOk db "MEASURE 'Sales'[Total]\n    FormatString = \"0.00\"" |> ignore
                Expect.isNull total.FormatStringDefinition "a static format string replaces the dynamic one"
                Expect.equal total.FormatString "0.00" ""
                applyOk db "MEASURE 'Sales'[Total]\n    FormatString =" |> ignore
                Expect.isTrue (String.IsNullOrEmpty total.FormatString) "empty clears"

            testCase "KPIs, detail rows, descriptions"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk
                        db
                        "MEASURE 'Sales'[Total]\n    KpiTargetExpression = 100\n    KpiStatusGraphic = \"Shapes\"\n    DetailRows = Sales\n    Description = \"\""

                let total = db.Model.Tables.["Sales"].Measures.["Total"]
                Expect.equal total.KPI.TargetExpression "100" ""
                Expect.equal total.KPI.StatusGraphic "Shapes" ""
                Expect.equal total.DetailRowsDefinition.Expression "Sales" ""

                Expect.equal
                    (changed a)
                    [ "modified", "measure:Sales/Total" ]
                    "an empty description on none is no change"

                applyOk db "MEASURE 'Sales'[Total]\n    DetailRows =\n    , KpiTargetExpression ="
                |> ignore

                Expect.isNull total.DetailRowsDefinition "empty removes detail rows"
                Expect.isTrue (String.IsNullOrEmpty total.KPI.TargetExpression) ""

            testCase "calculation groups: created with their items, ordinals as given"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk
                        db
                        ("CALCULATIONGROUP 'Scenario'[Kind]\n    Precedence = 5\n\n"
                         + "    CALCULATIONITEM \"Actual\" = SELECTEDMEASURE()\n        Ordinal = 1\n"
                         + "    CALCULATIONITEM \"Budget\" = SELECTEDMEASURE() * 1.1\n        Ordinal = 0\n"
                         + "        FormatString = \"0.0\"\n"
                         + "CALCULATIONGROUP 'Time Intelligence'[Period]\n"
                         + "    CALCULATIONITEM \"YTD\" = TOTALYTD(SELECTEDMEASURE(), 'Date'[Year])\n")

                Expect.isEmpty a.IntroducedErrors ""
                let g = db.Model.Tables.["Scenario"].CalculationGroup
                Expect.equal g.Precedence 5 ""
                Expect.equal g.CalculationItems.["Actual"].Ordinal 1 ""
                Expect.equal g.CalculationItems.["Budget"].Ordinal 0 ""
                Expect.equal g.CalculationItems.["Budget"].FormatStringDefinition.Expression "\"0.0\"" ""

                Expect.equal
                    (db.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["YTD"].Expression)
                    "TOTALYTD(SELECTEDMEASURE(), 'Date'[Year])"
                    "an existing item is updated"

            testCase "a calculation group's column must match"
            <| fun () ->
                match applyError (sampleDatabase ()) "CALCULATIONGROUP 'Time Intelligence'[Name]" with
                | ScriptObjectFailed(_, 1, 1, CannotScript(_, reason)) -> Expect.stringContains reason "tess rename" ""
                | other -> failtestf "%A" other

                match applyError (sampleDatabase ()) "CALCULATIONGROUP 'Sales'[Name]" with
                | ScriptObjectFailed(_, 1, 1, CannotScript(_, reason)) ->
                    Expect.stringContains reason "not a calculation group" ""
                | other -> failtestf "%A" other

            testCase "ordinals need compatibility level 1500"
            <| fun () ->
                let db = new Database(Name = "Old", ID = "Old", CompatibilityLevel = 1470)
                db.Model <- new Model(Name = "Model", DiscourageImplicitMeasures = true)

                let ti =
                    new Table(Name = "Time Intelligence", CalculationGroup = new CalculationGroup())

                ti.Columns.Add(new DataColumn(Name = "Period", DataType = DataType.String, SourceColumn = "Name"))

                ti.CalculationGroup.CalculationItems.Add(
                    new CalculationItem(Name = "Current", Expression = "SELECTEDMEASURE()")
                )

                ti.Partitions.Add(new Partition(Name = "Time Intelligence", Source = new CalculationGroupSource()))
                db.Model.Tables.Add ti

                match
                    applyError
                        db
                        "CALCULATIONGROUP 'Time Intelligence'[Period]\n    CALCULATIONITEM \"Current\"\n        Ordinal = 2"
                with
                | ScriptObjectFailed(_, 3, 9, CannotScript(_, reason)) -> Expect.stringContains reason "1500" ""
                | other -> failtestf "%A" other

            testCase "calculated tables, columns and functions"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk
                        db
                        ("TABLE 'Dates' = CALENDARAUTO()\n    Description = \"Every date\"\n"
                         + "COLUMN 'Sales'[Big] = Sales[Amount] > 100\n    Datatype = BOOLEAN\n"
                         + "FUNCTION Triple = (x: INT64) => x * 3\n    Visible = FALSE\n"
                         + "FUNCTION Double = (x: INT64) => x + x")

                Expect.equal
                    (changed a |> List.sort)
                    [
                        "created", "column:Sales/Big"
                        "created", "function:Triple"
                        "created", "partition:Dates/Dates"
                        "created", "table:Dates"
                        "modified", "function:Double"
                    ]
                    ""

                Expect.equal db.Model.Tables.["Dates"].Description "Every date" ""
                Expect.equal db.Model.Tables.["Sales"].Columns.["Big"].DataType DataType.Boolean ""
                Expect.isTrue db.Model.Functions.["Triple"].IsHidden ""

            testCase "a DAX query's DEFINE section applies"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applyOk db "DEFINE\n    MEASURE Sales[Q] = [Total] / 2\nEVALUATE\n    { [Q] }"

                Expect.equal (changed a) [ "created", "measure:Sales/Q" ] ""

            testCase "names match case-insensitively"
            <| fun () ->
                let db = sampleDatabase ()
                let a = applyOk db "MEASURE 'SALES'[total] = 1"
                Expect.equal (changed a) [ "modified", "measure:Sales/Total" ] ""
        ]
    ]
