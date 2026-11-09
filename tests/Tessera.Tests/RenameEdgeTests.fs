/// Rename edge cases: calculated-table sourceColumns, NAMEOF/TABLEOF and field parameters, and
/// measures qualified with a table other than their own.
module Tessera.Tests.RenameEdgeTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Inventory
open Tessera.Rename
open Tessera.Tests.Fixtures

/// The sample model plus a calculated table copying Sales' columns, a field parameter, and DAX that
/// names measures through NAMEOF, TABLEOF and another table.
let model () =
    let db = sampleDatabase ()
    let m = db.Model

    let big = new Table(Name = "Big Sales")

    big.Partitions.Add(
        new Partition(
            Name = "Big Sales",
            Source = new CalculatedPartitionSource(Expression = "FILTER ( Sales, Sales[Amount] > 100 )")
        )
    )

    big.Columns.Add(
        new CalculatedTableColumn(Name = "Amount", SourceColumn = "Sales[Amount]", DataType = DataType.Decimal)
    )

    big.Columns.Add(
        new CalculatedTableColumn(Name = "Key", SourceColumn = "'Sales'[DateKey]", DataType = DataType.Int64)
    )

    big.Columns.Add(new CalculatedTableColumn(Name = "Local", SourceColumn = "[Amount]", DataType = DataType.Decimal))
    m.Tables.Add big

    let metric = new Table(Name = "Metric")

    metric.Partitions.Add(
        new Partition(
            Name = "Metric",
            Source =
                new CalculatedPartitionSource(
                    Expression =
                        "{\n    (\"Total\", NAMEOF('Sales'[Total]), 0),\n    (\"Amount\", NAMEOF(Sales[Amount]), 1)\n}"
                )
        )
    )

    metric.Columns.Add(
        new CalculatedTableColumn(Name = "Metric", SourceColumn = "[Value1]", DataType = DataType.String)
    )

    let fields =
        new CalculatedTableColumn(Name = "Metric Fields", SourceColumn = "[Value2]", DataType = DataType.String)

    fields.ExtendedProperties.Add(
        new JsonExtendedProperty(Name = "ParameterMetadata", Value = """{"version":3,"kind":2}""")
    )

    metric.Columns.Add fields

    metric.Columns.Add(
        new CalculatedTableColumn(Name = "Metric Order", SourceColumn = "[Value3]", DataType = DataType.Int64)
    )

    m.Tables.Add metric

    let date = m.Tables.["Date"]
    date.Measures.Add(new Measure(Name = "Odd", Expression = "'Date'[Total] + 1"))
    date.Measures.Add(new Measure(Name = "Table Of", Expression = "COUNTROWS ( TABLEOF ( 'Sales'[Total] ) )"))
    db

let private entry (db: Database) (selector: string) =
    match Selector.resolve (objects db) selector with
    | Ok e -> e
    | Error e -> failtestf "%A" e

let private planOk db selector newName =
    match plan db (entry db selector) newName with
    | Ok p -> p
    | Error e -> failtestf "%s" (TessError.message e)

let private after (p: RenamePlan) owner property =
    p.Edits
    |> List.tryFind (fun e -> ObjectRef.format e.Owner.Ref = owner && e.Property = property)
    |> Option.map (fun e -> e.After)

let private sourceColumn (db: Database) table column =
    (db.Model.Tables.[table].Columns.[column] :?> CalculatedTableColumn).SourceColumn

[<Tests>]
let tests =
    testList "rename edge cases" [
        testList "calculated-table sourceColumn" [
            testCase "a column: the calculated-table columns copying it map to the new name"
            <| fun () ->
                let db = model ()
                let p = planOk db "Sales[Amount]" "Net"
                Expect.equal (after p "column:Big Sales/Amount" "sourceColumn") (Some "Sales[Net]") ""
                Expect.equal (after p "column:Big Sales/Local" "sourceColumn") None "[Amount] is the table's own"
                Expect.equal (RenamePlan.sourceColumnEdits p).Length 1 ""
                Expect.isEmpty (p.Warnings |> List.filter (fun w -> w.Contains "sourceColumn")) "no longer a warning"
                apply p
                Expect.equal (sourceColumn db "Big Sales" "Amount") "Sales[Net]" ""
                Expect.equal (sourceColumn db "Big Sales" "Key") "'Sales'[DateKey]" "other columns stay"
                Expect.isNotNull (db.Model.Tables.["Big Sales"].Columns.Find "Amount") "the column keeps its own name"

                Expect.equal
                    (db.Model.Tables.["Big Sales"].Partitions.["Big Sales"].Source :?> CalculatedPartitionSource)
                        .Expression
                    "FILTER ( Sales, Sales[Net] > 100 )"
                    "and the table's DAX follows"

            testCase "a table: the table part, quoted as references are"
            <| fun () ->
                let db = model ()
                let p = planOk db "Sales" "FactSales"
                Expect.equal (after p "column:Big Sales/Amount" "sourceColumn") (Some "FactSales[Amount]") ""
                Expect.equal (after p "column:Big Sales/Key" "sourceColumn") (Some "'FactSales'[DateKey]") ""
                let p = planOk db "Sales" "Fact Sales"
                Expect.equal (after p "column:Big Sales/Amount" "sourceColumn") (Some "'Fact Sales'[Amount]") ""
                apply p
                Expect.equal (sourceColumn db "Big Sales" "Key") "'Fact Sales'[DateKey]" ""

            testCase "a column with brackets in its new name is escaped"
            <| fun () ->
                let db = model ()
                let p = planOk db "Sales[Amount]" "Net]"
                Expect.equal (after p "column:Big Sales/Amount" "sourceColumn") (Some "Sales[Net]]]") ""

            testCase "a column of another table with the same name is left alone"
            <| fun () ->
                let db = model ()
                let p = planOk db "Date[Year]" "Yr"
                Expect.isEmpty (RenamePlan.sourceColumnEdits p) ""
        ]

        testList "NAMEOF, TABLEOF and field parameters" [
            testCase "NAMEOF and TABLEOF arguments are references, and are rewritten"
            <| fun () ->
                let db = model ()
                let p = planOk db "[Total]" "Revenue"

                Expect.equal
                    (after p "table:Metric" "expression")
                    (Some
                        "{\n    (\"Total\", NAMEOF('Sales'[Revenue]), 0),\n    (\"Amount\", NAMEOF(Sales[Amount]), 1)\n}")
                    "the label is text and stays"

                Expect.equal
                    (after p "measure:Date/Table Of" "expression")
                    (Some "COUNTROWS ( TABLEOF ( 'Sales'[Revenue] ) )")
                    ""

            testCase "the field parameter keeps its columns, and a warning names the stale label"
            <| fun () ->
                let db = model ()
                let p = planOk db "[Total]" "Revenue"

                Expect.exists
                    p.Warnings
                    (fun w -> w.Contains "field parameter 'Metric' still shows the label \"Total\"")
                    (string p.Warnings)

                apply p
                Expect.equal (sourceColumn db "Metric" "Metric Fields") "[Value2]" "columns map to the tuple"
                Expect.equal (sourceColumn db "Metric" "Metric") "[Value1]" ""

                let errors =
                    Validate.validate db
                    |> List.filter (fun i ->
                        i.Severity = Validate.Severity.Error
                        && (ObjectRef.format i.Ref).Contains "Metric")

                Expect.isEmpty errors "the field parameter is still valid"

            testCase "a column in a field parameter, and a table"
            <| fun () ->
                let db = model ()
                let p = planOk db "Sales[Amount]" "Net"

                Expect.stringContains
                    (after p "table:Metric" "expression" |> Option.defaultValue "")
                    "NAMEOF(Sales[Net])"
                    ""

                Expect.exists p.Warnings (fun w -> w.Contains "label \"Amount\"") ""
                let p = planOk db "Sales" "Fact Sales"
                let text = after p "table:Metric" "expression" |> Option.defaultValue ""
                Expect.stringContains text "NAMEOF('Fact Sales'[Total])" ""
                Expect.stringContains text "NAMEOF('Fact Sales'[Amount])" ""

            testCase "an unqualified NAMEOF([Total]) next to the label \"Total\" is still a reference"
            <| fun () ->
                let db = model ()

                (db.Model.Tables.["Metric"].Partitions.["Metric"].Source :?> CalculatedPartitionSource).Expression <-
                    "{ (\"Total\", NAMEOF([Total]), 0), (\"Metric\", NAMEOF([Metric]), 1) }"

                db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Metric", Expression = "1"))

                Expect.equal
                    (Deps.dependsOn (Deps.build db) (ObjectRef.table "Metric")
                     |> List.map (fun r -> r.Target))
                    [
                        Deps.Resolved(ObjectRef.inTable ObjectKind.Measure "Sales" "Total")
                        Deps.Resolved(ObjectRef.inTable ObjectKind.Measure "Sales" "Metric")
                    ]
                    "not the label, and not the table's own Metric column"

                let p = planOk db "[Total]" "Revenue"

                Expect.equal
                    (after p "table:Metric" "expression")
                    (Some "{ (\"Total\", NAMEOF([Revenue]), 0), (\"Metric\", NAMEOF([Metric]), 1) }")
                    ""

            testCase "no label warning when the label differs from the name"
            <| fun () ->
                let db = model ()
                let p = planOk db "Date[Year]" "Yr"
                Expect.isEmpty (p.Warnings |> List.filter (fun w -> w.Contains "field parameter")) ""
        ]

        testList "a measure qualified with another table" [
            testCase "'Date'[Total] isn't resolved, so it isn't rewritten: a warning names it"
            <| fun () ->
                let db = model ()
                let p = planOk db "[Total]" "Revenue"
                Expect.equal (after p "measure:Date/Odd" "expression") None "left as written"

                Expect.exists
                    p.Warnings
                    (fun w ->
                        w.StartsWith
                            "'Date'[Total] in measure:Date/Odd expression:1 names it with table 'Date' instead of 'Sales'")
                    (string p.Warnings)

            testCase "validate reports it, before and after"
            <| fun () ->
                let db = model ()

                let unresolved () =
                    Validate.validate db
                    |> List.filter (fun i ->
                        i.Rule = "unresolved-reference" && ObjectRef.format i.Ref = "measure:Date/Odd")

                Expect.isNonEmpty (unresolved ()) ""
                apply (planOk db "[Total]" "Revenue")
                Expect.isNonEmpty (unresolved ()) ""

            testCase "other measures don't warn"
            <| fun () ->
                let db = model ()
                let p = planOk db "[Total (hidden)]" "Hidden"
                Expect.isEmpty (p.Warnings |> List.filter (fun w -> w.Contains "names it with table")) ""
        ]
    ]
