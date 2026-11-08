module Tessera.Tests.RemoveMoveTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.RemoveObject
open Tessera.MoveObject
open Tessera.Tests.Fixtures

/// The sample model plus the structural links removal has to clean up.
let private model () =
    let db = sampleDatabase ()
    let m = db.Model
    let sales = m.Tables.["Sales"]
    let date = m.Tables.["Date"]
    let year = date.Columns.["Year"]
    date.Columns.["Month"].SortByColumn <- year

    let view = m.Perspectives.["Sales View"]
    let pSales = new PerspectiveTable(Table = sales)
    pSales.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = sales.Measures.["Total (hidden)"]))
    pSales.PerspectiveColumns.Add(new PerspectiveColumn(Column = sales.Columns.["Amount"]))
    view.PerspectiveTables.Add pSales
    let pDate = new PerspectiveTable(Table = date)
    pDate.PerspectiveColumns.Add(new PerspectiveColumn(Column = year))
    pDate.PerspectiveHierarchies.Add(new PerspectiveHierarchy(Hierarchy = date.Hierarchies.["Calendar"]))
    view.PerspectiveTables.Add pDate

    let readers = m.Roles.["Readers"]
    readers.TablePermissions.Add(new TablePermission(Table = sales, FilterExpression = "Sales[Amount] > 0"))
    let tp = new TablePermission(Table = date)
    tp.ColumnPermissions.Add(new ColumnPermission(Column = year, MetadataPermission = MetadataPermission.None))
    readers.TablePermissions.Add tp

    let da = m.Cultures.["da-DK"]
    da.ObjectTranslations.Add(new ObjectTranslation(Object = year, Property = TranslatedProperty.Caption, Value = "År"))

    da.ObjectTranslations.Add(
        new ObjectTranslation(Object = sales.Measures.["Total"], Property = TranslatedProperty.Caption, Value = "I alt")
    )

    db

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private spec selectors = RemoveSpec.ofSelectors selectors

let private removeOk db selectors = remove db (spec selectors) |> ok

/// Run through the editing pipeline, as the commands do.
let private applied db edit = Mutation.apply db edit |> ok

let private hasNote (effect: Mutation.Effect) (text: string) =
    let notes = String.concat "\n  " effect.Notes

    Expect.isTrue
        (effect.Notes |> List.exists (fun n -> n.Contains text))
        $"expected a note containing '{text}', got:\n  {notes}"

let private tomValid (db: Database) =
    Expect.isEmpty (tomErrors db) "TOM has nothing to complain about"

let private removeTests =
    testList "remove" [
        testCase "refuses while other DAX refers to it, and lists the references"
        <| fun () ->
            let db = model ()

            match remove db (spec [ "[Total]" ]) with
            | Error(RemoveBlocked(objects, dependents)) ->
                Expect.equal objects [ "measure:Sales/Total" ] ""

                Expect.equal
                    dependents
                    [
                        "measure:Sales/Total (hidden)  expression:1  [Total]  -> measure:Sales/Total"
                    ]
                    ""
            | other -> failtestf "expected RemoveBlocked, got %A" other

            Expect.isTrue (db.Model.Tables.["Sales"].Measures.Contains "Total") "nothing removed"

        testCase "references from inside what's removed don't count"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "[Total]"; "[Total (hidden)]" ]
            Expect.equal db.Model.Tables.["Sales"].Measures.Count 0 ""
            hasNote effect "Removed measure:Sales/Total"
            tomValid db

        testCase "a table goes with its contents, its relationships and its row filters"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Sales" ]
            Expect.isFalse (db.Model.Tables.Contains "Sales") ""
            Expect.equal db.Model.Relationships.Count 0 "its relationship"
            hasNote effect "Removed table:Sales and the"
            hasNote effect "Also removed relationship:Sales-Date: it uses a removed column"
            hasNote effect "Removed role:Readers's permissions on table:Sales (including its row filter)"
            Expect.isNull (db.Model.Roles.["Readers"].TablePermissions.Find "Sales") ""
            Expect.isNull (db.Model.Perspectives.["Sales View"].PerspectiveTables.Find "Sales") ""
            tomValid db

        testCase "a column takes its relationships with it"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Sales[DateKey]" ]
            Expect.equal db.Model.Relationships.Count 0 ""
            hasNote effect "Also removed relationship:Sales-Date"
            tomValid db

        testCase "a hierarchy loses the level on a removed column; the rest are renumbered"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Date[Year]" ]
            let h = db.Model.Tables.["Date"].Hierarchies.["Calendar"]
            Expect.equal [ for l in h.Levels -> l.Name, l.Ordinal ] [ "Month", 0 ] ""
            hasNote effect "Also removed level:Date/Calendar/Year: its column column:Date/Year is removed"
            tomValid db

        testCase "a hierarchy with no levels left goes too"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Date[Year]"; "Date[Month]" ]
            Expect.isFalse (db.Model.Tables.["Date"].Hierarchies.Contains "Calendar") ""
            hasNote effect "Also removed hierarchy:Date/Calendar: none of its levels remain"

            Expect.isNull
                (db.Model.Perspectives.["Sales View"].PerspectiveTables.["Date"].PerspectiveHierarchies.Find "Calendar")
                ""

            tomValid db

        testCase "removing a level explicitly renumbers the others"
        <| fun () ->
            let db = model ()
            removeOk db [ "level:Date/Calendar/Year" ] |> ignore
            let h = db.Model.Tables.["Date"].Hierarchies.["Calendar"]
            Expect.equal [ for l in h.Levels -> l.Name, l.Ordinal ] [ "Month", 0 ] ""
            Expect.isTrue (db.Model.Tables.["Date"].Columns.Contains "Year") "the column stays"

        testCase "sort-by a removed column is cleared"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Date[Year]" ]
            Expect.isNull db.Model.Tables.["Date"].Columns.["Month"].SortByColumn ""
            hasNote effect "Cleared the sort-by column of column:Date/Month (was column:Date/Year)"

        testCase "perspective memberships go"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "[Total (hidden)]"; "Date[Year]" ]
            let view = db.Model.Perspectives.["Sales View"]
            Expect.equal view.PerspectiveTables.["Sales"].PerspectiveMeasures.Count 0 ""
            Expect.equal view.PerspectiveTables.["Date"].PerspectiveColumns.Count 0 ""
            Expect.equal view.PerspectiveTables.["Sales"].PerspectiveColumns.Count 1 "others stay"

            hasNote effect "Removed from perspective:Sales View: measure:Sales/Total (hidden), column:Date/Year"

            tomValid db

        testCase "object-level security on a removed column goes"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Date[Year]" ]
            let tp = db.Model.Roles.["Readers"].TablePermissions.["Date"]
            Expect.equal tp.ColumnPermissions.Count 0 ""
            hasNote effect "Removed role:Readers's object-level security on column:Date/Year"
            tomValid db

        testCase "translations of removed objects go"
        <| fun () ->
            let db = model ()
            let effect = removeOk db [ "Date[Year]" ]
            Expect.equal db.Model.Cultures.["da-DK"].ObjectTranslations.Count 1 "only the measure's"
            hasNote effect "Removed 1 translation of removed objects from culture:da-DK"
            tomValid db

        testCase "calendars and variations let go of removed columns and hierarchies"
        <| fun () ->
            let db = model ()
            let date = db.Model.Tables.["Date"]
            let cal = new Calendar(Name = "Fiscal")

            cal.CalendarColumnGroups.Add(
                new TimeUnitColumnAssociation(TimeUnit.Year, PrimaryColumn = date.Columns.["Year"])
            )

            let related = new TimeRelatedColumnGroup()
            related.Columns.Add date.Columns.["Month"]
            related.Columns.Add date.Columns.["DateKey"]
            cal.CalendarColumnGroups.Add related
            date.Calendars.Add cal
            let key = db.Model.Tables.["Sales"].Columns.["DateKey"]

            key.Variations.Add(
                new Variation(
                    Name = "Variation",
                    Relationship = db.Model.Relationships.["Sales-Date"],
                    DefaultHierarchy = date.Hierarchies.["Calendar"]
                )
            )

            let effect = removeOk db [ "Date[Year]"; "Date[Month]" ]
            Expect.equal cal.CalendarColumnGroups.Count 1 "the Year association goes"
            Expect.equal [ for c in related.Columns -> c.Name ] [ "DateKey" ] ""
            Expect.equal key.Variations.Count 0 "its default hierarchy is gone"
            hasNote effect "Removed calendar:Date/Fiscal's associations with removed columns"
            hasNote effect "Removed variation 'Variation' of column:Sales/DateKey: it uses a removed object"
            tomValid db

        testCase "PBI_QueryOrder drops a removed table"
        <| fun () ->
            let db = model ()

            db.Model.Annotations.Add(
                new Annotation(Name = "PBI_QueryOrder", Value = """["Sales","Date","ServerName"]""")
            )

            removeOk db [ "Sales" ] |> ignore
            Expect.equal (db.Model.Annotations.Find "PBI_QueryOrder").Value """["Date","ServerName"]""" ""

        testCase "--if-exists skips what isn't there; without it, that's an error"
        <| fun () ->
            let db = model ()

            match remove db (spec [ "[Nope]"; "[Total (hidden)]" ]) with
            | Error(ObjectNotFound "[Nope]") -> ()
            | other -> failtestf "expected ObjectNotFound, got %A" other

            let effect =
                remove db {
                    spec [ "[Nope]"; "[Total (hidden)]" ] with
                        IfExists = true
                }
                |> ok

            hasNote effect "No object matches '[Nope]'; skipped (--if-exists)"
            Expect.isFalse (db.Model.Tables.["Sales"].Measures.Contains "Total (hidden)") "the rest is removed"

        testCase "--if-exists doesn't hide an ambiguous selector"
        <| fun () ->
            match
                remove (model ()) {
                    spec [ "[DateKey]" ] with
                        IfExists = true
                }
            with
            | Error(AmbiguousObject _) -> ()
            | other -> failtestf "expected AmbiguousObject, got %A" other

        testCase "the model and a table's last partition can't be removed"
        <| fun () ->
            let db = model ()

            match remove db (spec [ "model" ]) with
            | Error(UnsupportedRemove("model", _)) -> ()
            | other -> failtestf "expected UnsupportedRemove, got %A" other

            match remove db (spec [ "partition:Sales/Sales" ]) with
            | Error(UnsupportedRemove("partition:Sales/Sales", reason)) ->
                Expect.stringContains reason "last partition" ""
            | other -> failtestf "expected UnsupportedRemove, got %A" other

            db.Model.Tables.["Sales"].Partitions.Add(
                new Partition(Name = "Sales 2", Source = new MPartitionSource(Expression = "let x = 2 in x"))
            )

            removeOk db [ "partition:Sales/Sales" ] |> ignore
            Expect.equal [ for p in db.Model.Tables.["Sales"].Partitions -> p.Name ] [ "Sales 2" ] "one of two is fine"

        testCase "a calculated table's or calculation group's columns can't be removed on their own"
        <| fun () ->
            let db = model ()
            let summary = new Table(Name = "Summary")

            summary.Partitions.Add(
                new Partition(Name = "Summary", Source = new CalculatedPartitionSource(Expression = "{ 1 }"))
            )

            summary.Columns.Add(
                new CalculatedTableColumn(Name = "Value", DataType = DataType.Int64, SourceColumn = "[Value]")
            )

            db.Model.Tables.Add summary

            let refused selector =
                match remove db (spec [ selector ]) with
                | Error(UnsupportedRemove(_, reason)) -> reason
                | other -> failtestf "expected UnsupportedRemove, got %A" other

            Expect.stringContains (refused "Summary[Value]") "come from its expression" ""
            Expect.stringContains (refused "'Time Intelligence'[Period]") "calculation group" ""

            removeOk db [ "'Time Intelligence'"; "Summary" ] |> ignore
            Expect.isFalse (db.Model.Tables.Contains "Time Intelligence") "the whole group is fine"
            Expect.isFalse (db.Model.Tables.Contains "Summary") "so is the whole calculated table"

        testCase "--cascade removes DAX dependents, transitively"
        <| fun () ->
            let db = model ()

            let effect =
                remove db {
                    spec [ "Sales[Amount]" ] with
                        Cascade = true
                }

            // The role's row filter on Sales refers to Amount: a role isn't removed for that.
            match effect with
            | Error(RemoveBlocked(_, dependents)) ->
                Expect.equal
                    dependents
                    [
                        "role:Readers  tablePermissions.Sales.filterExpression:1  Sales[Amount]  -> column:Sales/Amount"
                    ]
                    ""
            | other -> failtestf "expected RemoveBlocked, got %A" other

            db.Model.Roles.["Readers"].TablePermissions.Remove "Sales" |> ignore

            let effect =
                remove db {
                    spec [ "Sales[Amount]" ] with
                        Cascade = true
                }
                |> ok

            Expect.equal db.Model.Tables.["Sales"].Measures.Count 0 "Total, and Total (hidden) through it"
            hasNote effect "Also removed measure:Sales/Total: its expression refers to column:Sales/Amount (--cascade)"
            hasNote effect "Also removed measure:Sales/Total (hidden): its expression refers to measure:Sales/Total"
            tomValid db

        testCase "--force removes anyway, with a warning per broken reference"
        <| fun () ->
            let db = model ()

            let a =
                applied db (fun db ->
                    remove db {
                        spec [ "Sales[Amount]" ] with
                            Force = true
                    })

            Expect.equal
                a.Effect.Warnings
                [
                    "left a broken reference (--force): measure:Sales/Total  expression:1  Sales[Amount]  -> column:Sales/Amount"
                    "left a broken reference (--force): role:Readers  tablePermissions.Sales.filterExpression:1  Sales[Amount]  -> column:Sales/Amount"
                ]
                ""

            Expect.equal
                (a.IntroducedErrors |> List.map (fun i -> ObjectRef.format i.Ref, i.Rule))
                [
                    "measure:Sales/Total", "unresolved-reference"
                    "role:Readers", "unresolved-reference"
                ]
                "the gate reports them"

        testCase "in the pipeline: deletions, and no new errors from structural fallout"
        <| fun () ->
            let a = applied (model ()) (fun db -> remove db (spec [ "Date[Year]" ]))

            Expect.equal
                (a.Changes
                 |> List.filter (fun c -> c.ChangeKind = ChangeSet.ChangeKind.Deleted)
                 |> List.map (fun c -> ObjectRef.format c.Ref))
                [ "column:Date/Year"; "level:Date/Calendar/Year" ]
                ""

            Expect.isEmpty a.IntroducedErrors ""

        testCase "a data source still used by a partition can't be removed"
        <| fun () ->
            let db = model ()

            let ds =
                new ProviderDataSource(Name = "SQL", ConnectionString = "Provider=SQLNCLI11")

            db.Model.DataSources.Add ds

            db.Model.Tables.["Sales"].Partitions.Add(
                new Partition(Name = "Legacy", Source = new QueryPartitionSource(DataSource = ds, Query = "SELECT 1"))
            )

            match remove db (spec [ "datasource:SQL" ]) with
            | Error(UnsupportedRemove("datasource:SQL", reason)) ->
                Expect.stringContains reason "partition:Sales/Legacy uses datasource:SQL as its data source" ""
            | other -> failtestf "expected UnsupportedRemove, got %A" other
    ]

let private moveSpec measure table = {
    Measure = measure
    Table = table
    DisplayFolder = None
}

let private moveOk db measure table = move db (moveSpec measure table) |> ok

let private moveTests =
    testList "move" [
        testCase "rewrites references qualified with the old table; nothing else"
        <| fun () ->
            let db = model ()

            let uses =
                "CALCULATE ( [Total], 'Sales'[Amount] > 0 ) // 'Sales'[Total] in a comment\n"
                + "    + LEN ( \"Sales[Total]\" ) /* Sales[Total] */ + Sales [Total] + 'Sales'[Total]"

            db.Model.Tables.["Date"].Measures.Add(new Measure(Name = "Uses", Expression = uses))
            let effect = moveOk db "[Total]" "Date"

            Expect.equal
                db.Model.Tables.["Date"].Measures.["Uses"].Expression
                ("CALCULATE ( [Total], 'Sales'[Amount] > 0 ) // 'Sales'[Total] in a comment\n"
                 + "    + LEN ( \"Sales[Total]\" ) /* Sales[Total] */ + Date [Total] + 'Date'[Total]")
                "unquoted stays unquoted, quoted stays quoted; comments and strings untouched"

            Expect.equal db.Model.Tables.["Sales"].Measures.["Total (hidden)"].Expression "[Total]" "unqualified stays"
            Expect.isTrue (db.Model.Tables.["Date"].Measures.Contains "Total") ""
            Expect.isFalse (db.Model.Tables.["Sales"].Measures.Contains "Total") ""

            Expect.equal
                effect.Renamed
                [
                    ObjectRef.parse "measure:Sales/Total" |> ok, ObjectRef.parse "measure:Date/Total" |> ok
                ]
                ""

            hasNote effect "Moved measure:Sales/Total -> measure:Date/Total; updated 2 qualified references in 1 object"
            tomValid db

        testCase "a table name that isn't an identifier is quoted"
        <| fun () ->
            let db = model ()
            let fact = new Table(Name = "Fact Sales")
            fact.Partitions.Add(new Partition(Name = "Fact Sales", Source = new MPartitionSource(Expression = "1")))
            db.Model.Tables.Add fact
            db.Model.Tables.["Sales"].Measures.["Total (hidden)"].Expression <- "Sales[Total]"
            moveOk db "[Total]" "Fact Sales" |> ignore
            Expect.equal db.Model.Tables.["Sales"].Measures.["Total (hidden)"].Expression "'Fact Sales'[Total]" ""

        testCase "keeps its properties, translations and perspectives; sets a display folder"
        <| fun () ->
            let db = model ()
            let total = db.Model.Tables.["Sales"].Measures.["Total"]
            total.LineageTag <- "tag-total"
            let pt = db.Model.Perspectives.["Sales View"].PerspectiveTables.["Sales"]
            pt.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = total))
            // Date isn't in a second perspective that has the measure: it joins it.
            let other = new Perspective(Name = "Other")
            let otherSales = new PerspectiveTable(Table = db.Model.Tables.["Sales"])
            otherSales.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = total))
            other.PerspectiveTables.Add otherSales
            db.Model.Perspectives.Add other

            let effect =
                move db {
                    moveSpec "[Total]" "Date" with
                        DisplayFolder = Some "KPIs"
                }
                |> ok

            let moved = db.Model.Tables.["Date"].Measures.["Total"]
            Expect.equal moved.Expression "SUM(Sales[Amount])" ""
            Expect.equal moved.FormatString "#,0" ""
            Expect.equal moved.LineageTag "tag-total" ""
            Expect.equal moved.DisplayFolder "KPIs" ""

            Expect.equal
                (db.Model.Cultures.["da-DK"].ObjectTranslations.[moved, TranslatedProperty.Caption]).Value
                "I alt"
                ""

            let view = db.Model.Perspectives.["Sales View"]
            Expect.isNull (view.PerspectiveTables.["Sales"].PerspectiveMeasures.Find "Total") ""
            Expect.isNotNull (view.PerspectiveTables.["Date"].PerspectiveMeasures.Find "Total") ""
            Expect.isNotNull (other.PerspectiveTables.["Date"].PerspectiveMeasures.Find "Total") ""
            hasNote effect "Added table 'Date' to perspective 'Other' (with just this measure)"
            hasNote effect "Set its display folder to 'KPIs'"
            tomValid db

        testCase "in the pipeline: a move, not a delete and a create"
        <| fun () ->
            let a = applied (model ()) (fun db -> move db (moveSpec "[Total]" "Date"))

            // Translations are keyed by the object's path, so the culture's entry moves too.
            Expect.equal
                (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref, c.ChangeKind))
                [
                    "measure:Date/Total", ChangeSet.ChangeKind.Moved
                    "culture:da-DK", ChangeSet.ChangeKind.Modified
                ]
                ""

            Expect.isEmpty a.IntroducedErrors ""

        testCase "refuses a move that would change what its own DAX means"
        <| fun () ->
            let db = model ()

            db.Model.Tables.["Sales"].Measures.Add(
                new Measure(Name = "Row Sum", Expression = "SUMX ( Sales, [Amount] )")
            )

            match move db (moveSpec "[Row Sum]" "Date") with
            | Error(MoveConflict("measure:Sales/Row Sum", "Date", [ detail ])) ->
                Expect.stringContains detail "[Amount] would refer to nothing instead of column:Sales/Amount" ""
            | other -> failtestf "expected MoveConflict, got %A" other

            Expect.isTrue (db.Model.Tables.["Sales"].Measures.Contains "Row Sum") "nothing moved"

        testCase "refuses a move that would let a column in the new table capture a reference"
        <| fun () ->
            let db = model ()
            // [Budget] is the measure; in Date, the column Date[Budget] would come first.
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Budget", Expression = "1"))
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Ratio", Expression = "[Budget] * 2"))

            db.Model.Tables.["Date"].Columns.Add(
                new DataColumn(Name = "Budget", DataType = DataType.Int64, SourceColumn = "Budget")
            )

            match move db (moveSpec "[Ratio]" "Date") with
            | Error(MoveConflict(_, _, [ detail ])) ->
                Expect.stringContains
                    detail
                    "[Budget] would refer to column:Date/Budget instead of measure:Sales/Budget"
                    ""
            | other -> failtestf "expected MoveConflict, got %A" other

        testCase "refuses a same-named column or hierarchy in the target table"
        <| fun () ->
            let db = model ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "year", Expression = "1"))

            match move db (moveSpec "[year]" "Date") with
            | Error(NameCollision("year", "column:Date/Year")) -> ()
            | other -> failtestf "expected NameCollision, got %A" other

        testCase "refuses what isn't a measure, a missing table and a calculation group"
        <| fun () ->
            let db = model ()

            let refused measure table =
                match move db (moveSpec measure table) with
                | Error(UnsupportedMove(_, reason)) -> reason
                | other -> failtestf "expected UnsupportedMove, got %A" other

            Expect.stringContains (refused "Sales[Amount]" "Date") "column" ""
            Expect.stringContains (refused "calcitem:Time Intelligence/YTD" "Date") "calculation group" ""
            Expect.stringContains (refused "[Total]" "Nope") "there is no table 'Nope'" ""
            Expect.stringContains (refused "[Total]" "Time Intelligence") "calculation group" ""

        testCase "the same table is a no-op"
        <| fun () ->
            let a = applied (model ()) (fun db -> move db (moveSpec "[Total]" "Sales"))
            Expect.isEmpty a.Changes ""
            hasNote a.Effect "is already in table 'Sales'; nothing moved"
    ]

[<Tests>]
let tests = testList "RemoveObject / MoveObject" [ removeTests; moveTests ]
