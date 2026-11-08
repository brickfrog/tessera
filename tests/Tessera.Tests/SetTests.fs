module Tessera.Tests.SetTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.SetProperty
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private entry db selector =
    Inventory.Selector.resolve (Inventory.objects db) selector |> ok

let private p path value = {
    Path = path
    Value = Value.Text value
}

let private unset = Assignment.unset

/// Set properties on one object through the editing pipeline.
let private run (db: Database) selector (assignments: Assignment list) =
    Mutation.apply db (fun db -> SetProperty.set db (entry db selector) assignments)

let private applied db selector assignments = run db selector assignments |> ok

/// The property changes reported, as (path, before, after).
let private changes (a: Mutation.Applied) =
    a.Changes
    |> List.collect (fun c -> c.Properties |> List.map (fun p -> p.Path, p.Before, p.After))

let private fails (db: Database) selector assignments =
    match run db selector assignments with
    | Error e -> e
    | Ok a -> failtestf "expected an error, got %A" (changes a)

let private sales (db: Database) = db.Model.Tables.["Sales"]
let private total (db: Database) = (sales db).Measures.["Total"]

let private descriptor db selector path =
    catalog db (entry db selector) |> List.find (fun d -> d.Path = path)

/// The sample model plus objects it lacks: a calculated column, a legacy data source and a query
/// partition, a Power Query data source, a calculated table.
let private richDatabase () =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]

    sales.Columns.Add(
        new CalculatedColumn(Name = "Double", DataType = DataType.Decimal, Expression = "Sales[Amount] * 2")
    )

    let sql =
        new ProviderDataSource(Name = "SQL", ConnectionString = "Provider=SQLOLEDB;Data Source=x")

    db.Model.DataSources.Add sql
    let legacy = new Table(Name = "Legacy")
    legacy.Columns.Add(new DataColumn(Name = "K", DataType = DataType.Int64, SourceColumn = "K"))

    legacy.Partitions.Add(
        new Partition(Name = "Legacy", Source = new QueryPartitionSource(DataSource = sql, Query = "SELECT 1 AS K"))
    )

    db.Model.Tables.Add legacy
    let calc = new Table(Name = "Calc")
    calc.Columns.Add(new CalculatedTableColumn(Name = "Value", DataType = DataType.Int64, SourceColumn = "[Value]"))
    calc.Partitions.Add(new Partition(Name = "Calc", Source = new CalculatedPartitionSource(Expression = "{ 1 }")))
    db.Model.Tables.Add calc

    let pq =
        new StructuredDataSource(Name = "PQ", ConnectionDetails = new ConnectionDetails(Protocol = "tds"))

    pq.Credential <- new Credential(AuthenticationKind = "UsernamePassword", Username = "u")
    db.Model.DataSources.Add pq
    db

[<Tests>]
let tests =
    testList "set" [
        testList "values" [
            testCase "strings are taken as-is"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applied db "[Total]" [ p "formatString" "0.00 \"x\""; p "description" "  Line 1\nLine 2" ]

                Expect.equal (total db).FormatString "0.00 \"x\"" ""
                Expect.equal (total db).Description "  Line 1\nLine 2" ""
                Expect.contains (changes a) ("formatString", Some "#,0", Some "0.00 \"x\"") ""
                Expect.equal (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref)) [ "measure:Sales/Total" ] ""

            testCase "booleans, case-insensitive"
            <| fun () ->
                let db = sampleDatabase ()

                applied db "Sales[Amount]" [ p "isHidden" "TRUE"; p "isNullable" "False" ]
                |> ignore

                Expect.isTrue (sales db).Columns.["Amount"].IsHidden ""
                Expect.isFalse (sales db).Columns.["Amount"].IsNullable ""

            testCase "integers and numbers"
            <| fun () ->
                let db = sampleDatabase ()
                applied db "Sales[Amount]" [ p "displayOrdinal" "-3" ] |> ignore
                Expect.equal (sales db).Columns.["Amount"].DisplayOrdinal -3 ""
                // Int64 and double, on a sub-object of the model that `set` creates.
                applied db "model" [
                    p "automaticAggregationOptions.detailTableMinRows" "5000000000"
                    p "automaticAggregationOptions.queryCoverage" "0.25"
                ]
                |> ignore

                Expect.equal db.Model.AutomaticAggregationOptions.DetailTableMinRows 5000000000L ""
                Expect.equal db.Model.AutomaticAggregationOptions.QueryCoverage 0.25 ""

            testCase "enums by TOM name, case-insensitive"
            <| fun () ->
                let db = sampleDatabase ()
                let a = applied db "Sales[Amount]" [ p "summarizeBy" "none"; p "dataType" "INT64" ]
                let amount = (sales db).Columns.["Amount"]
                Expect.equal amount.SummarizeBy AggregateFunction.None ""
                Expect.equal amount.DataType DataType.Int64 ""
                Expect.contains (changes a) ("dataType", Some "decimal", Some "int64") "as get -p prints it"

                applied db "relationship:Sales-Date" [ p "crossFilteringBehavior" "BothDirections" ]
                |> ignore

                Expect.equal
                    db.Model.Relationships.["Sales-Date"].CrossFilteringBehavior
                    CrossFilteringBehavior.BothDirections
                    ""

            testCase "enum members the compatibility level doesn't have are refused, and not offered"
            <| fun () ->
                let db = new Database(Name = "Old", ID = "Old", CompatibilityLevel = 1500)
                db.Model <- new Model(Name = "Model")
                let t = new Table(Name = "T")
                t.Columns.Add(new DataColumn(Name = "C", DataType = DataType.String, SourceColumn = "C"))
                t.Partitions.Add(new Partition(Name = "P", Source = new MPartitionSource(Expression = "x")))
                db.Model.Tables.Add t

                match fails db "partition:T/P" [ p "mode" "directLake" ] with
                | InvalidPropertyValue(_, "mode", "directLake", reason) ->
                    Expect.stringContains reason "needs compatibility level 1604" ""
                | e -> failtestf "%A" e

                let allowed = (descriptor db "partition:T/P" "mode").Allowed
                Expect.contains allowed "import" ""
                Expect.isFalse (List.contains "directLake" allowed) ""
                // A property the level lacks is read-only, with the reason.
                let d = descriptor db "column:T/C" "lineageTag"
                Expect.isFalse d.Writable ""
                Expect.stringContains (defaultArg d.Reason "") "needs compatibility level" ""
        ]

        testList "nested paths" [
            testCase "setting a nested path creates the sub-object"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applied db "[Total]" [
                        p "formatStringDefinition.expression" "\"0.0\""
                        p "detailRowsDefinition.expression" "Sales"
                        p "kpi.targetExpression" "100"
                        p "kpi.statusGraphic" "Traffic Light"
                    ]

                let m = total db
                Expect.equal m.FormatStringDefinition.Expression "\"0.0\"" ""
                Expect.equal m.DetailRowsDefinition.Expression "Sales" ""
                Expect.equal m.KPI.TargetExpression "100" ""
                Expect.equal m.KPI.StatusGraphic "Traffic Light" ""

                Expect.equal
                    (changes a |> List.map (fun (path, _, _) -> path) |> List.sort)
                    [
                        "detailRowsDefinition.expression"
                        "formatStringDefinition.expression"
                        "kpi.statusGraphic"
                        "kpi.targetExpression"
                    ]
                    "diff sees them"

            testCase "an existing sub-object is edited, not replaced"
            <| fun () ->
                let db = sampleDatabase ()
                (total db).KPI <- new KPI(TargetExpression = "1", StatusExpression = "2")
                applied db "[Total]" [ p "kpi.targetExpression" "3" ] |> ignore
                Expect.equal (total db).KPI.TargetExpression "3" ""
                Expect.equal (total db).KPI.StatusExpression "2" ""

            testCase "an existing sub-object's runtime type decides its properties (partition source)"
            <| fun () ->
                let db = sampleDatabase ()

                applied db "partition:Sales/Sales" [ p "source.expression" "let y = 2 in y" ]
                |> ignore

                Expect.equal
                    ((sales db).Partitions.["Sales"].Source :?> MPartitionSource).Expression
                    "let y = 2 in y"
                    ""

                match fails db "partition:Sales/Sales" [ p "source.query" "SELECT 1" ] with
                | UnknownProperty(_, "source.query", _) -> ()
                | e -> failtestf "%A" e

            testCase "unsetting a sub-object's last property, or the sub-object, removes it"
            <| fun () ->
                let db = sampleDatabase ()
                (total db).FormatStringDefinition <- new FormatStringDefinition(Expression = "\"0\"")
                (total db).KPI <- new KPI(TargetExpression = "1")

                applied db "[Total]" [ unset "formatStringDefinition.expression"; unset "kpi" ]
                |> ignore

                Expect.isNull (total db).FormatStringDefinition ""
                Expect.isNull (total db).KPI ""

            testCase "calculation group properties; the calculation group itself can't be created"
            <| fun () ->
                let db = sampleDatabase ()

                applied db "Time Intelligence" [
                    p "calculationGroup.precedence" "5"
                    p "calculationGroup.noSelectionExpression.expression" "SELECTEDMEASURE()"
                ]
                |> ignore

                let cg = db.Model.Tables.["Time Intelligence"].CalculationGroup
                Expect.equal cg.Precedence 5 ""
                Expect.equal cg.NoSelectionExpression.Expression "SELECTEDMEASURE()" ""

                match fails db "Sales" [ p "calculationGroup.precedence" "1" ] with
                | ReadOnlyProperty(_, _, reason) -> Expect.stringContains reason "can't create" ""
                | e -> failtestf "%A" e

                match fails db "Time Intelligence" [ unset "calculationGroup" ] with
                | ReadOnlyProperty(_, "calculationGroup", _) -> ()
                | e -> failtestf "%A" e
        ]

        testList "annotations and extended properties" [
            testCase "add, update (case-insensitively), remove"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applied db "[Total]" [ p "annotations.Owner" "finance"; p "annotations.a.b" "dotted" ]

                Expect.equal ((total db).Annotations.["Owner"].Value) "finance" ""
                Expect.equal ((total db).Annotations.["a.b"].Value) "dotted" "everything after annotations. is the name"
                Expect.contains (changes a) ("annotations.Owner", None, Some "finance") ""

                applied db "[Total]" [ p "annotations.owner" "sales" ] |> ignore
                Expect.equal (total db).Annotations.Count 2 "updated, not added"
                Expect.equal ((total db).Annotations.["Owner"].Value) "sales" ""

                applied db "[Total]" [ unset "annotations.Owner" ] |> ignore
                Expect.equal [ for a in (total db).Annotations -> a.Name ] [ "a.b" ] ""

            testCase "-p x=null and --unset remove an annotation; removing a missing one is a no-op"
            <| fun () ->
                let db = sampleDatabase ()
                (total db).Annotations.Add(new Annotation(Name = "X", Value = "1"))

                let a =
                    applied db "[Total]" [
                        {
                            Path = "annotations.X"
                            Value = Value.Null
                        }
                    ]

                Expect.equal (total db).Annotations.Count 0 ""
                Expect.contains (changes a) ("annotations.X", Some "1", None) ""
                Expect.isEmpty (applied db "[Total]" [ unset "annotations.Nope" ]).Changes ""

            testCase "annotations on sub-objects"
            <| fun () ->
                let db = sampleDatabase ()
                applied db "[Total]" [ p "kpi.annotations.Note" "n" ] |> ignore
                Expect.equal (total db).KPI.Annotations.["Note"].Value "n" ""

            testCase "extended properties: string ones are created; JSON ones need JSON"
            <| fun () ->
                let db = sampleDatabase ()
                let a = applied db "[Total]" [ p "extendedProperties.Tag" "v1" ]

                Expect.equal ((total db).ExtendedProperties.["Tag"] :?> StringExtendedProperty).Value "v1" ""

                Expect.contains (changes a) ("extendedProperties.Tag", None, Some "v1") ""

                (total db).ExtendedProperties.Add(new JsonExtendedProperty(Name = "J", Value = """{"a":1}"""))

                match fails db "[Total]" [ p "extendedProperties.J" "not json" ] with
                | InvalidPropertyValue(_, "extendedProperties.J", _, _) -> ()
                | e -> failtestf "%A" e

                applied db "[Total]" [ p "extendedProperties.J" """{"a":2}""" ] |> ignore

                Expect.equal ((total db).ExtendedProperties.["J"] :?> JsonExtendedProperty).Value """{"a":2}""" ""
        ]

        testList "references" [
            testCase "sortByColumn: a column name in the same table, or any selector"
            <| fun () ->
                let db = sampleDatabase ()
                let date = db.Model.Tables.["Date"]
                let a = applied db "Date[Month]" [ p "sortByColumn" "DateKey" ]
                Expect.equal date.Columns.["Month"].SortByColumn.Name "DateKey" ""
                Expect.equal date.Columns.["Month"].SortByColumn.Table.Name "Date" "its own table, not Sales"
                Expect.contains (changes a) ("sortByColumn", None, Some "DateKey") ""

                applied db "Date[Month]" [ p "sortByColumn" "'Date'[Year]" ] |> ignore
                Expect.equal date.Columns.["Month"].SortByColumn.Name "Year" ""
                applied db "Date[Month]" [ p "sortByColumn" "column:Date/DateKey" ] |> ignore
                Expect.equal date.Columns.["Month"].SortByColumn.Name "DateKey" ""

                let d = descriptor db "Date[Month]" "sortByColumn"
                Expect.equal d.Type PropertyType.Reference ""
                Expect.equal d.RefersTo (Some ObjectKind.Column) ""
                Expect.equal d.Value (PropertyValue.Reference(ObjectRef.inTable ObjectKind.Column "Date" "DateKey")) ""

            testCase "sortByColumn: null clears it"
            <| fun () ->
                let db = sampleDatabase ()
                let date = db.Model.Tables.["Date"]
                date.Columns.["Month"].SortByColumn <- date.Columns.["DateKey"]
                applied db "Date[Month]" [ unset "sortByColumn" ] |> ignore
                Expect.isNull date.Columns.["Month"].SortByColumn ""

            testCase "a reference to the wrong kind, nothing, or an ambiguous name"
            <| fun () ->
                let db = sampleDatabase ()

                match fails db "Date[Month]" [ p "sortByColumn" "[Total]" ] with
                | InvalidPropertyValue(_, "sortByColumn", "[Total]", reason) ->
                    Expect.stringContains reason "measure:Sales/Total, not a column" ""
                | e -> failtestf "%A" e

                match fails db "Date[Month]" [ p "sortByColumn" "Nope" ] with
                | ObjectNotFound "Nope" -> ()
                | e -> failtestf "%A" e

                // "DateKey" is in Sales and Date, and Time Intelligence has neither.
                match fails db "'Time Intelligence'[Period]" [ p "sortByColumn" "DateKey" ] with
                | AmbiguousObject("DateKey", _) -> ()
                | e -> failtestf "%A" e

            testCase "sortByColumn and a level's column must be in the same table"
            <| fun () ->
                let db = sampleDatabase ()

                for selector, path, value in
                    [
                        "Sales[Amount]", "sortByColumn", "'Date'[Year]"
                        "level:Date/Calendar/Year", "column", "'Sales'[DateKey]"
                    ] do
                    match fails db selector [ p path value ] with
                    | InvalidPropertyValue(_, actual, _, reason) ->
                        Expect.equal actual path ""
                        Expect.stringContains reason "same table" ""
                    | e -> failtestf "%A" e

            testCase "relationship columns resolve in their own end's table first"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applied db "relationship:Sales-Date" [ p "fromColumn" "Amount"; p "isActive" "false" ]

                let r = db.Model.Relationships.["Sales-Date"] :?> SingleColumnRelationship
                Expect.equal (r.FromColumn.Table.Name, r.FromColumn.Name) ("Sales", "Amount") ""
                Expect.isFalse r.IsActive ""

                Expect.equal
                    (a.Changes |> List.map (fun c -> c.ChangeKind))
                    [ ChangeSet.ChangeKind.Modified ]
                    "the same relationship, modified"

            testCase "other reference properties: the model's defaultMeasure, a level's column"
            <| fun () ->
                let db = sampleDatabase ()
                applied db "model" [ p "defaultMeasure" "[Total]" ] |> ignore
                Expect.equal db.Model.DefaultMeasure.Name "Total" ""
                applied db "level:Date/Calendar/Month" [ p "column" "DateKey" ] |> ignore
                Expect.equal db.Model.Tables.["Date"].Hierarchies.["Calendar"].Levels.["Month"].Column.Name "DateKey" ""
        ]

        testList "unset" [
            testCase "strings clear; everything else resets to TOM's default"
            <| fun () ->
                let db = sampleDatabase ()
                let amount = (sales db).Columns.["Amount"]
                amount.IsHidden <- true
                amount.SummarizeBy <- AggregateFunction.Sum
                amount.IsAvailableInMDX <- false
                amount.DisplayOrdinal <- 4

                applied db "Sales[Amount]" [
                    unset "isHidden"
                    unset "summarizeBy"
                    unset "isAvailableInMdx"
                    {
                        Path = "displayOrdinal"
                        Value = Value.Null
                    }
                ]
                |> ignore

                Expect.isFalse amount.IsHidden ""
                Expect.equal amount.SummarizeBy AggregateFunction.Default ""
                Expect.isTrue amount.IsAvailableInMDX "TOM's default, not false"
                Expect.equal amount.DisplayOrdinal 0 ""

                let a = applied db "[Total]" [ unset "formatString" ]
                Expect.isTrue (System.String.IsNullOrEmpty (total db).FormatString) ""
                Expect.contains (changes a) ("formatString", Some "#,0", None) ""
        ]

        testList "parsing assignments" [
            testCase "path=value; null; stdin; the value may hold '='"
            <| fun () ->
                let noStdin () = failtest "stdin read"
                Expect.equal (Assignment.parse noStdin "a=b=c") (Ok(p "a" "b=c")) ""
                Expect.equal (Assignment.parse noStdin "a=") (Ok(p "a" "")) ""

                Expect.equal
                    (Assignment.parse noStdin " isHidden =null")
                    (Ok {
                        Path = "isHidden"
                        Value = Value.Null
                    })
                    ""

                Expect.isError (Assignment.parse noStdin "isHidden") ""
                Expect.isError (Assignment.parse noStdin "=x") ""

            testCase "stdin values are literal, minus one trailing line break"
            <| fun () ->
                Expect.equal (Assignment.parse (fun () -> "SUM(x)\r\n") "expression=-") (Ok(p "expression" "SUM(x)")) ""

                Expect.equal (Assignment.parse (fun () -> "null") "x=-") (Ok(p "x" "null")) "not null"
                Expect.equal (Assignment.parse (fun () -> "a\n\n") "x=-") (Ok(p "x" "a\n")) ""

            testCase "a stdin value sets the property like any other"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    Assignment.parse (fun () -> "SUMX(Sales, Sales[Amount])\n") "expression=-"
                    |> Result.defaultWith failtest

                applied db "[Total]" [ a ] |> ignore
                Expect.equal (total db).Expression "SUMX(Sales, Sales[Amount])" ""
        ]

        testList "refusals" [
            testCase "unknown properties suggest close matches"
            <| fun () ->
                let db = sampleDatabase ()

                match fails db "[Total]" [ p "formatStrng" "0" ] with
                | UnknownProperty("measure:Sales/Total", "formatStrng", suggestions) ->
                    Expect.equal (List.head suggestions) "formatString" ""
                | e -> failtestf "%A" e

                match fails db "partition:Sales/Sales" [ p "expression" "x" ] with
                | UnknownProperty(_, _, suggestions) -> Expect.contains suggestions "source.expression" ""
                | e -> failtestf "%A" e

                match fails db "Sales[Amount]" [ p "expression" "1" ] with
                | UnknownProperty _ -> () // a data column has no expression
                | e -> failtestf "%A" e

                Expect.stringContains
                    (TessError.message (UnknownProperty("m", "formatStrng", [ "formatString" ])))
                    "did you mean 'formatString'?"
                    ""

            testCase "name points to rename"
            <| fun () ->
                match fails (sampleDatabase ()) "[Total]" [ p "name" "X" ] with
                | ReadOnlyProperty(_, "name", reason) -> Expect.stringContains reason "tess rename" ""
                | e -> failtestf "%A" e

            testCase "properties that hold children"
            <| fun () ->
                let db = sampleDatabase ()

                for selector, path in
                    [
                        "Sales", "columns"
                        "Sales", "Measures"
                        "Sales", "columns.Amount.isHidden"
                        "model", "tables"
                    ] do
                    match fails db selector [ p path "x" ] with
                    | ReadOnlyProperty(_, _, reason) -> Expect.stringContains reason "child objects" path
                    | e -> failtestf "%s: %A" path e

            testCase "read-only properties"
            <| fun () ->
                let db = sampleDatabase ()

                match fails db "Sales[Amount]" [ p "type" "calculated" ] with
                | ReadOnlyProperty(_, "type", _) -> ()
                | e -> failtestf "%A" e

                match fails db "Sales[Amount]" [ p "fullTextIndexingBehavior" "off" ] with
                | ReadOnlyProperty(_, _, reason) -> Expect.stringContains reason "needs compatibility level" ""
                | e -> failtestf "%A" e

                match fails db "partition:Sales/Sales" [ unset "source" ] with
                | ReadOnlyProperty(_, "source", _) -> ()
                | e -> failtestf "%A" e

                db.Model.DataSources.Add(new ProviderDataSource(Name = "SQL", ConnectionString = "x"))

                match fails db "datasource:SQL" [ p "password" "hunter2" ] with
                | ReadOnlyProperty(_, "password", reason) -> Expect.stringContains reason "secret" ""
                | e -> failtestf "%A" e

            testCase "bad values"
            <| fun () ->
                let db = sampleDatabase ()

                let bad selector path value =
                    match fails db selector [ p path value ] with
                    | InvalidPropertyValue(_, actual, v, reason) ->
                        Expect.equal actual path ""
                        Expect.equal v value ""
                        reason
                    | e -> failtestf "%s=%s: %A" path value e

                Expect.stringContains (bad "[Total]" "isHidden" "yes") "true or false" ""
                Expect.stringContains (bad "Sales[Amount]" "displayOrdinal" "1.5") "whole number" ""
                Expect.stringContains (bad "Sales[Amount]" "displayOrdinal" "99999999999") "32-bit" ""
                Expect.stringContains (bad "model" "automaticAggregationOptions.queryCoverage" "lots") "a number" ""

                Expect.stringContains
                    (bad "Sales[Amount]" "summarizeBy" "total")
                    "expected one of: default, none, sum"
                    ""

                Expect.stringContains (bad "Sales[Amount]" "summarizeBy" "1") "expected one of" "no numbers for enums"
                Expect.stringContains (bad "[Total]" "kpi" "x") "it's an object" ""
                Expect.stringContains (bad "Date[Month]" "sortByColumn" "") "use null to clear" ""

            testCase "the same property twice"
            <| fun () ->
                match fails (sampleDatabase ()) "[Total]" [ p "formatString" "0"; p "FormatString" "1" ] with
                | InvalidPropertyValue(_, "formatString", _, reason) -> Expect.stringContains reason "more than once" ""
                | e -> failtestf "%A" e

            testCase "error codes"
            <| fun () ->
                Expect.equal (TessError.code (UnknownProperty("", "", []))) "unknown-property" ""
                Expect.equal (TessError.code (ReadOnlyProperty("", "", ""))) "read-only-property" ""
                Expect.equal (TessError.code (InvalidPropertyValue("", "", "", ""))) "invalid-property-value" ""
        ]

        testList "several assignments" [
            testCase "apply as one change"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    applied db "[Total]" [
                        p "formatString" "0.0"
                        p "isHidden" "true"
                        p "displayFolder" "KPIs"
                        p "annotations.A" "1"
                    ]

                Expect.equal a.Changes.Length 1 "one object"
                Expect.equal (changes a).Length 4 "four properties"

                Expect.equal
                    a.Effect.Notes
                    [
                        "Set formatString, isHidden, displayFolder, annotations.A on measure:Sales/Total"
                    ]
                    ""

            testCase "one bad assignment means nothing is written"
            <| fun () ->
                let db = sampleDatabase ()

                match SetProperty.set db (entry db "[Total]") [ p "formatString" "0.0"; p "isHidden" "maybe" ] with
                | Error(InvalidPropertyValue(_, "isHidden", _, _)) -> ()
                | other -> failtestf "%A" other

                Expect.equal (total db).FormatString "#,0" "checked before anything is written"

            testCase "a value that makes the model invalid is applied, with the error reported"
            <| fun () ->
                let db = sampleDatabase ()
                let a = applied db "[Total]" [ p "expression" "SUM('Missing'[Amount])" ]

                Expect.equal (a.IntroducedErrors |> List.map (fun i -> i.Rule)) [ "unresolved-reference" ] ""
        ]

        testList "catalog" [
            testCase "every object has one; sorted by path; read-only entries say why"
            <| fun () ->
                let db = richDatabase ()

                for e in Inventory.objects db do
                    let c = catalog db e
                    let paths = c |> List.map (fun d -> d.Path)
                    let name = ObjectRef.format e.Ref
                    Expect.equal paths (paths |> List.sortBy (fun p -> p.ToLowerInvariant())) name
                    Expect.equal paths (List.distinct paths) name
                    Expect.equal (catalog db e) c $"{name}: deterministic"

                    for d in c do
                        Expect.equal d.Writable d.Reason.IsNone $"{name} {d.Path}"

                        if d.Type <> PropertyType.Enum then
                            Expect.isEmpty d.Allowed $"{name} {d.Path}"
                        elif d.Writable then
                            Expect.isNonEmpty d.Allowed $"{name} {d.Path}"

                    if e.Ref.Kind <> ObjectKind.Model then
                        Expect.isFalse (descriptor db (ObjectRef.format e.Ref) "name").Writable name

            testCase "current values, as get -p shows them"
            <| fun () ->
                let db = richDatabase ()
                let d path = (descriptor db "[Total]" path).Value
                Expect.equal (d "formatString") (PropertyValue.String "#,0") ""
                Expect.equal (d "isHidden") (PropertyValue.Bool false) ""
                Expect.equal (d "kpi") PropertyValue.Null "absent, but creatable"
                Expect.equal (d "kpi.targetExpression") PropertyValue.Null ""
                Expect.equal (descriptor db "Sales[Amount]" "dataType").Value (PropertyValue.String "decimal") ""
                Expect.equal (descriptor db "partition:Sales/Sales" "source").Value PropertyValue.Present ""
                Expect.isTrue (descriptor db "partition:Sales/Sales" "source.expression").Writable ""
                Expect.equal (descriptor db "Sales" "annotations.<name>").Type PropertyType.String "any new annotation"

            testCase "every writable property, set to another value, shows up in the diff under its path"
            <| fun () ->
                // The catalog's paths must be get -p's paths: otherwise `set` changes something diff
                // can't see, and the change is never saved.
                let failures = ResizeArray<string>()
                let mutable checkedCount = 0
                let db0 = richDatabase ()

                for e in Inventory.objects db0 do
                    let selector = ObjectRef.format e.Ref

                    for d in catalog db0 e do
                        let candidates =
                            match d.Type, d.Value with
                            | _ when not d.Writable || d.Path.EndsWith "<name>" -> []
                            | PropertyType.Object, _ -> []
                            | (PropertyType.String | PropertyType.Expression), v ->
                                [ "en-GB"; "tess"; "1" ] |> List.filter (fun c -> v <> PropertyValue.String c)
                            | PropertyType.Bool, PropertyValue.Bool b -> [ (if b then "false" else "true") ]
                            | PropertyType.Bool, _ -> [ "true" ]
                            | PropertyType.Int, PropertyValue.Int n -> [ string (n + 1L); "2" ]
                            | PropertyType.Int, _ -> [ "7" ]
                            | PropertyType.Number, _ -> [ "0.5"; "0.25" ]
                            | PropertyType.Enum, v -> d.Allowed |> List.filter (fun a -> v <> PropertyValue.String a)
                            | PropertyType.Reference, v ->
                                Inventory.objects db0
                                |> List.filter (fun o ->
                                    Some o.Ref.Kind = d.RefersTo && v <> PropertyValue.Reference o.Ref)
                                // Own-table columns first: sortByColumn and levels need them.
                                |> List.sortBy (fun o -> Inventory.ObjectEntry.table o <> Inventory.ObjectEntry.table e)
                                |> List.map (fun o -> ObjectRef.format o.Ref)

                        // TOM masks connection strings, and normalizes them.
                        let opaque = d.Path = "connectionString"

                        let candidates =
                            if d.Path.EndsWith "linguisticMetadata.content" then
                                [ "<Linguistics />" ]
                            else
                                candidates

                        let rec attempt tried =
                            function
                            | [] when not (List.isEmpty candidates) ->
                                let why = String.concat "; " tried
                                failures.Add $"{selector} {d.Path}: no value showed up in the diff ({why})"
                            | [] -> ()
                            | c :: rest ->
                                let db = richDatabase ()

                                match run db selector [ p d.Path c ] with
                                | Error err -> attempt (TessError.message err :: tried) rest
                                | Ok a ->
                                    // A reference is a group in TOM's JSON when it names its table too
                                    // (defaultMeasure.table, defaultMeasure.measure).
                                    let found =
                                        changes a
                                        |> List.filter (fun (path, _, _) ->
                                            path = d.Path
                                            || (d.Type = PropertyType.Reference && path.StartsWith(d.Path + ".")))

                                    let seen = changes a |> List.map (fun (path, _, _) -> path)

                                    match found with
                                    // TOM leaves default values out: try another value.
                                    | [] when seen.IsEmpty -> attempt ($"{c}: no change" :: tried) rest
                                    | [] -> failures.Add $"{selector} {d.Path}={c}: not in the diff (saw {seen})"
                                    | [ (_, _, after) ] when
                                        d.Type <> PropertyType.Reference
                                        && not opaque
                                        && after <> Some c
                                        && after <> None
                                        ->
                                        failures.Add $"{selector} {d.Path}={c}: diff shows {after}"
                                    | _ when opaque -> ()
                                    | _ ->
                                        // And the catalog reads it back the same way.
                                        let now = (descriptor db selector d.Path).Value

                                        match d.Type, now with
                                        | PropertyType.Reference, PropertyValue.Reference r when ObjectRef.format r = c ->
                                            ()
                                        | PropertyType.Reference, other ->
                                            failures.Add $"{selector} {d.Path}: reads back {other}"
                                        | _, PropertyValue.String s when s = c -> ()
                                        | _, PropertyValue.Bool b when (if b then "true" else "false") = c -> ()
                                        | _, PropertyValue.Int n when string n = c -> ()
                                        | _, PropertyValue.Number n when n = float c -> ()
                                        | _, other -> failures.Add $"{selector} {d.Path}={c}: reads back {other}"

                        if not candidates.IsEmpty then
                            checkedCount <- checkedCount + 1

                        attempt [] (List.truncate 6 candidates)

                        // Every enum member offered must read back as itself (or be refused by TOM).
                        if d.Type = PropertyType.Enum then
                            for c in candidates do
                                let db = richDatabase ()

                                if Result.isOk (run db selector [ p d.Path c ]) then
                                    match (descriptor db selector d.Path).Value with
                                    | PropertyValue.String s when s = c -> ()
                                    | other -> failures.Add $"{selector} {d.Path}={c}: reads back {other}"

                Expect.isEmpty failures (String.concat "\n" failures)
                Expect.isGreaterThan checkedCount 300 $"checked {checkedCount} properties"
        ]
    ]
