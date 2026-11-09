module Tessera.Tests.UnusedTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private sampleWith (change: Database -> unit) =
    let db = sampleDatabase ()
    change db
    db

let private sales (db: Database) = db.Model.Tables.["Sales"]
let private date (db: Database) = db.Model.Tables.["Date"]

let private hiddenMeasure (table: Table) name expression =
    let m = new Measure(Name = name, Expression = expression, IsHidden = true)
    table.Measures.Add m
    m

let private hiddenColumn (table: Table) name =
    let c =
        new DataColumn(Name = name, DataType = DataType.Int64, SourceColumn = name, IsHidden = true)

    table.Columns.Add c
    c

let private hiddenTable (db: Database) name =
    let t = new Table(Name = name, IsHidden = true)
    t.Partitions.Add(new Partition(Name = name, Source = new MPartitionSource(Expression = "let x = 1 in x")))
    db.Model.Tables.Add t
    t

let private findWith options (db: Database) = Deps.build db |> Unused.find options

/// The refs `find` reports, by default (hidden objects only).
let private unused (db: Database) =
    findWith Unused.Options.defaults db
    |> List.map (fun u -> ObjectRef.format u.Entry.Ref)

let private unusedAll (db: Database) =
    findWith { IncludeVisible = true } db
    |> List.map (fun u -> ObjectRef.format u.Entry.Ref)

let private usedBy (db: Database) (target: string) =
    findWith { IncludeVisible = true } db
    |> List.find (fun u -> ObjectRef.format u.Entry.Ref = target)
    |> fun u -> u.UsedBy |> List.map ObjectRef.format

[<Tests>]
let tests =
    testList "Unused" [
        testCase "the sample model: only the hidden, unreferenced measure"
        <| fun () ->
            // Sales[DateKey] is hidden too, but a relationship uses it.
            Expect.equal (unused (sampleDatabase ())) [ "measure:Sales/Total (hidden)" ] ""

        testCase "--include-visible: everything nothing in the model uses, transitively"
        <| fun () ->
            let all = unusedAll (sampleDatabase ())

            Expect.equal
                all
                [
                    "column:Sales/Amount"
                    "column:Sales/Path\\/With\\\\Slashes"
                    "measure:Sales/Total"
                    "measure:Sales/Total (hidden)"
                    "column:Date/Year"
                    "column:Date/Month"
                    "hierarchy:Date/Calendar"
                    "table:Time Intelligence"
                    "column:Time Intelligence/Period"
                    "function:Double"
                ]
                "relationship columns and the tables they're in stay; calculation items are never reported"

            Expect.equal (usedBy (sampleDatabase ()) "measure:Sales/Total") [ "measure:Sales/Total (hidden)" ] ""

            Expect.equal
                (usedBy (sampleDatabase ()) "column:Date/Year")
                [ "hierarchy:Date/Calendar" ]
                "a level counts as its hierarchy"

        testCase "usage is transitive: what only dead objects use is dead"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    hiddenMeasure (sales db) "A" "[B] * 2" |> ignore
                    hiddenMeasure (sales db) "B" "SUM(Sales[Hidden])" |> ignore
                    hiddenColumn (sales db) "Hidden" |> ignore
                    // A visible measure keeps its hidden helper.
                    (sales db).Measures.Add(new Measure(Name = "Shown", Expression = "[Helper]"))
                    hiddenMeasure (sales db) "Helper" "1" |> ignore)

            Expect.equal
                (unused db)
                [
                    "column:Sales/Hidden"
                    "measure:Sales/Total (hidden)"
                    "measure:Sales/A"
                    "measure:Sales/B"
                ]
                ""

            Expect.equal (usedBy db "measure:Sales/B") [ "measure:Sales/A" ] ""
            Expect.equal (usedBy db "column:Sales/Hidden") [ "measure:Sales/B" ] ""
            Expect.equal (usedBy db "measure:Sales/A") [] ""

        testCase "a cycle of dead objects stays dead; a self-reference doesn't count"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    hiddenMeasure (sales db) "X" "[Y]" |> ignore
                    let y = hiddenMeasure (sales db) "Y" "[X]"
                    y.FormatStringDefinition <- new FormatStringDefinition(Expression = "IF([Y] > 1, \"0\")"))

            Expect.containsAll (unused db) [ "measure:Sales/X"; "measure:Sales/Y" ] ""
            Expect.equal (usedBy db "measure:Sales/Y") [ "measure:Sales/X" ] ""

        testCase "a measure's format string, detail rows and KPI count only while the measure is used"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let dead = hiddenMeasure (sales db) "Dead" "1"
                    dead.FormatStringDefinition <- new FormatStringDefinition(Expression = "[FormatA]")
                    hiddenMeasure (sales db) "FormatA" "\"0\"" |> ignore
                    let live = (sales db).Measures.["Total"]
                    live.FormatStringDefinition <- new FormatStringDefinition(Expression = "[FormatB]")
                    live.DetailRowsDefinition <- new DetailRowsDefinition(Expression = "SELECTCOLUMNS(Sales, [Rows])")
                    live.KPI <- new KPI(TargetExpression = "[Target]")
                    hiddenMeasure (sales db) "FormatB" "\"0\"" |> ignore
                    hiddenColumn (sales db) "Rows" |> ignore
                    hiddenMeasure (sales db) "Target" "1" |> ignore)

            let u = unused db
            Expect.containsAll u [ "measure:Sales/Dead"; "measure:Sales/FormatA" ] ""

            for used in [ "measure:Sales/FormatB"; "column:Sales/Rows"; "measure:Sales/Target" ] do
                Expect.isFalse (List.contains used u) used

        testCase "sort-by columns and hierarchy levels of used columns are used"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let d = date db
                    d.Columns.["Month"].SortByColumn <- hiddenColumn d "MonthNo"
                    // Sorting a dead column doesn't keep its sort-by column.
                    let dead = hiddenColumn d "Dead"
                    dead.SortByColumn <- hiddenColumn d "DeadOrder"
                    d.Columns.["Year"].IsHidden <- true)

            let u = unused db
            Expect.isFalse (List.contains "column:Date/MonthNo" u) "sort-by of a visible column"
            Expect.isFalse (List.contains "column:Date/Year" u) "a level of a visible hierarchy"
            Expect.containsAll u [ "column:Date/Dead"; "column:Date/DeadOrder" ] ""
            Expect.equal (usedBy db "column:Date/DeadOrder") [ "column:Date/Dead" ] ""

        testCase "a hidden hierarchy is unused unless a variation uses it"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let d = date db
                    d.Hierarchies.["Calendar"].IsHidden <- true
                    let local = hiddenTable db "LocalDate"
                    let localDate = hiddenColumn local "Date"
                    let h = new Hierarchy(Name = "Date Hierarchy", IsHidden = true)
                    h.Levels.Add(new Level(Name = "Date", Ordinal = 0, Column = localDate))
                    local.Hierarchies.Add h

                    let rel =
                        new SingleColumnRelationship(
                            Name = "auto",
                            FromColumn = d.Columns.["DateKey"],
                            ToColumn = localDate,
                            IsActive = false
                        )

                    db.Model.Relationships.Add rel

                    d.Columns.["DateKey"].Variations.Add(
                        new Variation(Name = "Variation", Relationship = rel, DefaultHierarchy = h, IsDefault = true)
                    ))

            let u = unused db
            Expect.contains u "hierarchy:Date/Calendar" ""
            Expect.isFalse (List.contains "hierarchy:LocalDate/Date Hierarchy" u) "used by a variation"
            Expect.isFalse (List.contains "table:LocalDate" u) ""

        testCase "a table reference uses the table, not its columns; members of a hidden table are hidden"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let t = hiddenTable db "Lookup"
                    t.Columns.Add(new DataColumn(Name = "Code", DataType = DataType.String, SourceColumn = "Code"))
                    t.Measures.Add(new Measure(Name = "Shown In Hidden", Expression = "1"))
                    (sales db).Measures.Add(new Measure(Name = "Rows", Expression = "COUNTROWS(Lookup)")))

            let u = unused db
            Expect.isFalse (List.contains "table:Lookup" u) "COUNTROWS(Lookup) uses the table"
            Expect.containsAll u [ "column:Lookup/Code"; "measure:Lookup/Shown In Hidden" ] ""

        testCase "a dead table goes with its key column; a used one keeps it"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let dead = hiddenTable db "Dead"
                    (hiddenColumn dead "Key").IsKey <- true
                    let used = hiddenTable db "Used"
                    (hiddenColumn used "Key").IsKey <- true
                    hiddenColumn used "Other" |> ignore
                    (sales db).Measures.Add(new Measure(Name = "N", Expression = "COUNTROWS(Used)")))

            let u = unused db
            Expect.containsAll u [ "table:Dead"; "column:Dead/Key"; "column:Used/Other" ] ""
            Expect.isFalse (List.contains "column:Used/Key" u) ""
            Expect.isFalse (List.contains "table:Used" u) ""

        testCase "a used column keeps its table"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let t = hiddenTable db "Rates"
                    hiddenColumn t "Rate" |> ignore
                    (sales db).Measures.Add(new Measure(Name = "R", Expression = "MAX(Rates[Rate])")))

            Expect.isFalse (List.contains "table:Rates" (unused db)) ""

        testCase "row-level security, calculation items and calendars use what they refer to"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let security = hiddenTable db "Security"
                    hiddenColumn security "User" |> ignore

                    db.Model.Roles.["Readers"].TablePermissions.Add(
                        new TablePermission(Table = security, FilterExpression = "Security[User] = 1")
                    )

                    let d = date db
                    hiddenColumn d "Fiscal Year" |> ignore
                    let ti = db.Model.Tables.["Time Intelligence"]

                    ti.CalculationGroup.CalculationItems.["YTD"].Expression <-
                        "CALCULATE(SELECTEDMEASURE(), 'Date'[Fiscal Year] = 1)"

                    let cal = new Calendar(Name = "Fiscal")

                    cal.CalendarColumnGroups.Add(
                        new TimeUnitColumnAssociation(TimeUnit.Year, PrimaryColumn = hiddenColumn d "FY")
                    )

                    d.Calendars.Add cal)

            let u = unused db

            for used in
                [
                    "table:Security"
                    "column:Security/User"
                    "column:Date/Fiscal Year"
                    "column:Date/FY"
                ] do
                Expect.isFalse (List.contains used u) used

        testCase "a hidden, unused calculation group takes what its items use with it"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let ti = db.Model.Tables.["Time Intelligence"]
                    ti.IsHidden <- true
                    hiddenColumn (date db) "Fiscal Year" |> ignore

                    ti.CalculationGroup.CalculationItems.["YTD"].Expression <-
                        "CALCULATE(SELECTEDMEASURE(), 'Date'[Fiscal Year] = 1)")

            Expect.containsAll
                (unused db)
                [
                    "column:Date/Fiscal Year"
                    "table:Time Intelligence"
                    "column:Time Intelligence/Period"
                ]
                ""

            Expect.equal (usedBy db "column:Date/Fiscal Year") [ "calcitem:Time Intelligence/YTD" ] ""

        testCase "functions are used when called"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let fns = db.Model.Functions
                    fns.Add(new Function(Name = "Called", Expression = "() => 1", IsHidden = true))
                    fns.Add(new Function(Name = "Lonely", Expression = "() => 1", IsHidden = true))
                    fns.Add(new Function(Name = "Inner", Expression = "() => 1", IsHidden = true))
                    fns.Add(new Function(Name = "Outer", Expression = "() => Inner()", IsHidden = true))
                    (sales db).Measures.Add(new Measure(Name = "Calls", Expression = "Called()")))

            let u = unused db
            Expect.containsAll u [ "function:Lonely"; "function:Inner"; "function:Outer" ] ""
            Expect.isFalse (List.contains "function:Called" u) ""
            Expect.isFalse (List.contains "function:Double" u) "visible: may be called from reports"
            Expect.equal (usedBy db "function:Inner") [ "function:Outer" ] ""

        testCase "field parameters, aggregations and the auto date/time template count as used"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let d = date db
                    let order = hiddenColumn d "Order"

                    let details = new RelatedColumnDetails()
                    details.GroupByColumns.Add(new GroupByColumn(GroupingColumn = order))
                    d.Columns.["Month"].RelatedColumnDetails <- details

                    let agg = hiddenTable db "Sales Agg"
                    let amount = hiddenColumn agg "Amount"

                    amount.AlternateOf <-
                        new AlternateOf(
                            BaseColumn = (sales db).Columns.["Amount"],
                            Summarization = SummarizationType.Sum
                        )

                    let template = hiddenTable db "DateTableTemplate"
                    hiddenColumn template "Date" |> ignore
                    template.Annotations.Add(new Annotation(Name = "__PBI_TemplateDateTable", Value = "true")))

            let u = unused db

            for used in
                [
                    "column:Date/Order"
                    "table:Sales Agg"
                    "column:Sales Agg/Amount"
                    "table:DateTableTemplate"
                    "column:DateTableTemplate/Date"
                ] do
                Expect.isFalse (List.contains used u) used

        testCase "unused calculated-table columns are reported with a note"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    let t = new Table(Name = "Calc")

                    t.Partitions.Add(
                        new Partition(Name = "Calc", Source = new CalculatedPartitionSource(Expression = "{1}"))
                    )

                    t.Columns.Add(
                        new CalculatedTableColumn(
                            Name = "Value",
                            DataType = DataType.Int64,
                            SourceColumn = "[Value]",
                            IsHidden = true
                        )
                    )

                    db.Model.Tables.Add t)

            let u = findWith Unused.Options.defaults db

            let value =
                u |> List.find (fun x -> ObjectRef.format x.Entry.Ref = "column:Calc/Value")

            Expect.isSome value.Note ""
            Expect.isTrue value.Hidden ""
    ]
