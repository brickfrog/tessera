module Tessera.Tests.ValidateTests

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Validate
open Tessera.Tests.Fixtures

/// (rule, severity, ref, property, line) for each issue: everything but the message.
let private summary (issues: Issue list) =
    issues
    |> List.map (fun i -> i.Rule, Severity.token i.Severity, ObjectRef.format i.Ref, i.Property, i.Line)

let private ofRule rule (issues: Issue list) =
    issues |> List.filter (fun i -> i.Rule = rule)

/// The sample model with one change applied.
let private sampleWith (change: Database -> unit) =
    let db = sampleDatabase ()
    change db
    db

let private sales (db: Database) = db.Model.Tables.["Sales"]
let private date (db: Database) = db.Model.Tables.["Date"]

let private addMeasure (table: Table) name expression =
    table.Measures.Add(new Measure(Name = name, Expression = expression))

let private calculatedTable (db: Database) name expression =
    let t = new Table(Name = name)

    t.Partitions.Add(new Partition(Name = name, Source = new CalculatedPartitionSource(Expression = expression)))

    db.Model.Tables.Add t
    t

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

[<Tests>]
let tests =
    testList "Validate" [
        testCase "the sample model has no issues"
        <| fun () -> Expect.isEmpty (validate (sampleDatabase ()) |> summary) ""

        testCase "every rule id is kebab-case and documented once"
        <| fun () ->
            let ids = rules |> List.map fst
            Expect.equal ids (List.distinct ids) ""

            for id in ids do
                Expect.isMatch id "^[a-z]+(-[a-z]+)*$" id

        testList "unresolved-reference" [
            testCase "qualified and quoted references are errors; unqualified ones are warnings"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        addMeasure (sales db) "Broken" "'Nope'[X]\n+ Sales[Gone]\n+ [Missing]\n+ COUNTROWS('Ghost')")

                let issues = validate db |> ofRule "unresolved-reference"

                Expect.equal
                    (summary issues)
                    [
                        "unresolved-reference", "error", "measure:Sales/Broken", Some "expression", Some 1
                        "unresolved-reference", "error", "measure:Sales/Broken", Some "expression", Some 2
                        "unresolved-reference", "warning", "measure:Sales/Broken", Some "expression", Some 3
                        "unresolved-reference", "error", "measure:Sales/Broken", Some "expression", Some 4
                    ]
                    ""

                let messages = issues |> List.map (fun i -> i.Message)
                Expect.equal messages.[0] "'Nope'[X] does not exist: there is no table 'Nope'" ""
                Expect.equal messages.[1] "Sales[Gone] does not exist: 'Sales' has no column or measure [Gone]" ""
                Expect.stringStarts messages.[2] "[Missing] does not exist" ""

            testCase "references in RLS filters and format strings are checked too"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        db.Model.Roles.["Readers"].TablePermissions.Add(
                            new TablePermission(Table = sales db, FilterExpression = "'Sales'[Nope] > 0")
                        )

                        (sales db).Measures.["Total"].FormatStringDefinition <-
                            new FormatStringDefinition(Expression = "IF('Date'[Nope] > 1, \"0\")"))

                Expect.equal
                    (validate db |> summary)
                    [
                        "unresolved-reference",
                        "error",
                        "measure:Sales/Total",
                        Some "formatStringDefinition.expression",
                        Some 1
                        "unresolved-reference",
                        "error",
                        "role:Readers",
                        Some "tablePermissions.Sales.filterExpression",
                        Some 1
                    ]
                    ""

            testCase "VAR tables, local columns and UDF parameters are not reported"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        addMeasure
                            (sales db)
                            "Locals"
                            "VAR t = ADDCOLUMNS(Sales, \"Twice\", [Amount] * 2) RETURN SUMX(t, [Twice] + t[Amount])"

                        db.Model.Functions.Add(
                            new Function(Name = "Plus", Expression = "(a: INT64, b: INT64) => a + b")
                        ))

                Expect.isEmpty (validate db |> summary) ""
        ]

        testList "circular-reference" [
            let cycles db =
                validate db
                |> ofRule "circular-reference"
                |> List.map (fun i -> ObjectRef.format i.Ref, i.Line, i.Message)

            testCase "a two-measure loop is reported once, from its first member"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        addMeasure (sales db) "A" "1 +\n[B]"
                        addMeasure (sales db) "B" "[A] + [Total]")

                Expect.equal
                    (cycles db)
                    [
                        "measure:Sales/A", Some 2, "measure:Sales/A -> measure:Sales/B -> measure:Sales/A"
                    ]
                    ""

            testCase "a measure that refers to itself"
            <| fun () ->
                let db = sampleWith (fun db -> addMeasure (sales db) "Self" "[Self] + 1")

                Expect.equal (cycles db) [ "measure:Sales/Self", Some 1, "measure:Sales/Self -> measure:Sales/Self" ] ""

            testCase "a loop through a calculated column, and the rest of a tangled group"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        (sales db).Columns.Add(new CalculatedColumn(Name = "Calc", Expression = "[M1]"))
                        addMeasure (sales db) "M1" "SUM(Sales[Calc]) + [M2]"
                        addMeasure (sales db) "M2" "[M1]")

                Expect.equal
                    (cycles db)
                    [
                        "column:Sales/Calc",
                        Some 1,
                        "column:Sales/Calc -> measure:Sales/M1 -> column:Sales/Calc (also in this loop: measure:Sales/M2)"
                    ]
                    ""

            testCase "a loop through a calculated table's columns"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        let t = calculatedTable db "Summary" "ROW(\"N\", [Uses Summary])"

                        t.Columns.Add(
                            new CalculatedTableColumn(Name = "N", SourceColumn = "[N]", DataType = DataType.Int64)
                        )

                        addMeasure (sales db) "Uses Summary" "SUM(Summary[N])")

                Expect.equal
                    (cycles db |> List.map (fun (_, _, m) -> m))
                    [
                        "measure:Sales/Uses Summary -> column:Summary/N -> table:Summary -> measure:Sales/Uses Summary"
                    ]
                    ""

            testCase "recursive functions"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        db.Model.Functions.Add(new Function(Name = "Ping", Expression = "(n: INT64) => Pong(n)"))
                        db.Model.Functions.Add(new Function(Name = "Pong", Expression = "(n: INT64) => Ping(n)")))

                Expect.equal
                    (cycles db |> List.map (fun (_, _, m) -> m))
                    [ "function:Ping -> function:Pong -> function:Ping" ]
                    ""

            testCase "not loops: a format string using its own measure, a calculated table's own columns, detail rows"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        (sales db).Measures.["Total"].FormatStringDefinition <-
                            new FormatStringDefinition(Expression = "IF([Total] > 1, \"0\", \"0.0\")")

                        let t =
                            calculatedTable db "Dates" "ADDCOLUMNS(CALENDAR(1, 2), \"Year\", YEAR([Date]))"

                        t.Columns.Add(
                            new CalculatedTableColumn(
                                Name = "Date",
                                SourceColumn = "[Date]",
                                DataType = DataType.DateTime
                            )
                        )

                        t.Measures.Add(new Measure(Name = "Days", Expression = "COUNTROWS(Dates)"))

                        (sales db).DefaultDetailRowsDefinition <-
                            new DetailRowsDefinition(Expression = "SELECTCOLUMNS(Sales, \"T\", [Total])")

                        (sales db).Measures.["Total"].Expression <- "SUM(Sales[Amount]) + COUNTROWS(Sales)")

                Expect.isEmpty (cycles db) ""

            testCase "a long chain neither overflows the stack nor counts as a loop"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        for i in 1..5000 do
                            addMeasure (sales db) $"C{i}" $"[C{i + 1}]"

                        addMeasure (sales db) "C5001" "1")

                Expect.isEmpty (cycles db) ""
        ]

        testList "invalid-token" [
            let tokens expression =
                let db = sampleWith (fun db -> addMeasure (sales db) "Bad" expression)

                validate db |> ofRule "invalid-token" |> List.map (fun i -> i.Line, i.Message)

            testCase "unterminated string"
            <| fun () -> Expect.equal (tokens "1 +\n\"open") [ Some 2, "unterminated string: \"open" ] ""

            testCase "unterminated date literal"
            <| fun () ->
                Expect.equal (tokens "DT\"2024-01-01") [ Some 1, "unterminated date literal: DT\"2024-01-01" ] ""

            testCase "unterminated names, which can't span lines"
            <| fun () ->
                Expect.equal
                    (tokens "'Sales\n[Amount\n")
                    [
                        Some 1, "unterminated quoted name (names can't span lines): 'Sales"
                        Some 2, "unterminated [name] (names can't span lines): [Amount"
                    ]
                    ""

            testCase "unterminated comment, and characters DAX doesn't use"
            <| fun () ->
                Expect.equal
                    (tokens "1 ? 2\n/* never closed")
                    [ Some 1, "unexpected character '?'"; Some 2, "unterminated /* comment" ]
                    ""

            testCase "valid DAX with strings, comments and escapes is fine"
            <| fun () ->
                Expect.isEmpty
                    (tokens
                        "-- a comment with ' and [\n\"a \"\"quoted\"\" [x]\" & 'It''s'[A]]b] /* ok */ + DT\"2024-01-01\"")
                    ""
        ]

        testCase "empty-expression"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    addMeasure (sales db) "Blank" "  "
                    (sales db).Columns.Add(new CalculatedColumn(Name = "Nothing", Expression = ""))

                    db.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.Add(
                        new CalculationItem(Name = "Empty", Expression = "\n", Ordinal = 2)
                    )

                    db.Model.Functions.Add(new Function(Name = "Void", Expression = ""))
                    calculatedTable db "Hollow" " " |> ignore)

            Expect.equal
                (validate db |> ofRule "empty-expression" |> summary)
                [
                    "empty-expression", "warning", "column:Sales/Nothing", Some "expression", None
                    "empty-expression", "warning", "measure:Sales/Blank", Some "expression", None
                    "empty-expression", "warning", "calcitem:Time Intelligence/Empty", Some "expression", None
                    "empty-expression", "warning", "table:Hollow", Some "expression", None
                    "empty-expression", "warning", "function:Void", Some "expression", None
                ]
                ""

        testList "duplicate-name" [
            let dups db =
                validate db
                |> ofRule "duplicate-name"
                |> List.map (fun i -> ObjectRef.format i.Ref, i.Message)

            testCase "a column, a measure and a hierarchy may not share a name (any case) in one table"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        addMeasure (sales db) "amount" "1"
                        (date db).Hierarchies.Add(new Hierarchy(Name = "YEAR")))

                Expect.equal
                    (dups db)
                    [
                        "measure:Sales/amount", "name [amount] is also used by column:Sales/Amount in the same table"
                        "hierarchy:Date/YEAR", "name [YEAR] is also used by column:Date/Year in the same table"
                    ]
                    ""

            testCase "measure names are unique across the model"
            <| fun () ->
                let db = sampleWith (fun db -> addMeasure (date db) "TOTAL" "1")

                Expect.equal
                    (dups db)
                    [
                        "measure:Date/TOTAL",
                        "measure name [TOTAL] is also used by measure:Sales/Total; measure names must be unique in the model"
                    ]
                    ""

            testCase
                "columns and hierarchies may repeat names across tables, and so may a measure and another table's column"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        (date db)
                            .Columns
                            .Add(new DataColumn(Name = "Amount", DataType = DataType.Decimal, SourceColumn = "A"))

                        (sales db).Hierarchies.Add(new Hierarchy(Name = "Calendar"))
                        addMeasure (date db) "Year Count" "1"
                        addMeasure (date db) "DateKey Count" "1"
                        addMeasure (sales db) "Month" "1")

                Expect.isEmpty (dups db) ""

            testCase "the collisions survive saving and loading TMDL, so they can occur in files"
            <| fun () ->
                use t = new TempDir()
                let db = sampleWith (fun db -> addMeasure (sales db) "Amount" "1")
                TmdlSerializer.SerializeDatabaseToFolder(db, t.Path)
                let loaded = TmdlSerializer.DeserializeDatabaseFromFolder t.Path

                Expect.equal (dups loaded |> List.map fst) [ "measure:Sales/Amount" ] ""
        ]

        testList "relationships" [
            testCase "relationship-datatype-mismatch"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        db.Model.Relationships.Add(
                            new SingleColumnRelationship(
                                Name = "Sales-Month",
                                FromColumn = (sales db).Columns.["Amount"],
                                ToColumn = (date db).Columns.["Month"],
                                IsActive = false
                            )
                        ))

                let issues = validate db

                Expect.equal
                    (summary issues)
                    [
                        "relationship-datatype-mismatch", "warning", "relationship:Sales-Month", None, None
                    ]
                    ""

                Expect.equal issues.Head.Message "'Sales'[Amount] is Decimal but 'Date'[Month] is String" ""

            testCase "relationship-self-join"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        db.Model.Relationships.Add(
                            new SingleColumnRelationship(
                                Name = "Self",
                                FromColumn = (date db).Columns.["Year"],
                                ToColumn = (date db).Columns.["DateKey"],
                                IsActive = false
                            )
                        ))

                Expect.equal
                    (validate db |> summary)
                    [ "relationship-self-join", "error", "relationship:Self", None, None ]
                    ""
        ]

        testList "invalid-sort-by" [
            let sortIssues db =
                validate db
                |> ofRule "invalid-sort-by"
                |> List.map (fun i -> ObjectRef.format i.Ref, i.Message)

            testCase "sorted by itself"
            <| fun () ->
                let db =
                    sampleWith (fun db -> (date db).Columns.["Month"].SortByColumn <- (date db).Columns.["Month"])

                Expect.equal (sortIssues db) [ "column:Date/Month", "sorted by itself" ] ""

            testCase "sorted by a column that has its own sort-by column, including a loop"
            <| fun () ->
                let db =
                    sampleWith (fun db ->
                        let c = (date db).Columns
                        c.["Month"].SortByColumn <- c.["Year"]
                        c.["Year"].SortByColumn <- c.["DateKey"]
                        (sales db).Columns.["Amount"].SortByColumn <- (sales db).Columns.["DateKey"]
                        (sales db).Columns.["DateKey"].SortByColumn <- (sales db).Columns.["Amount"])

                Expect.equal
                    (sortIssues db |> List.map fst)
                    [ "column:Sales/Amount"; "column:Sales/DateKey"; "column:Date/Month" ]
                    ""

                Expect.stringContains
                    (sortIssues db |> List.last |> snd)
                    "sorted by [Year], which is itself sorted by [DateKey]"
                    ""

            testCase "an ordinary sort-by column is fine"
            <| fun () ->
                let db =
                    sampleWith (fun db -> (date db).Columns.["Month"].SortByColumn <- (date db).Columns.["DateKey"])

                Expect.isEmpty (validate db) ""
        ]

        testCase "tom-validation: a link to an object that can't be found"
        <| fun () ->
            // .bim files name columns; TOM leaves a name it can't resolve unset, and its own
            // validation reports it.
            let json =
                JsonSerializer
                    .SerializeDatabase(sampleDatabase ())
                    .Replace("\"toColumn\": \"DateKey\"", "\"toColumn\": \"Nope\"")

            let db = JsonSerializer.DeserializeDatabase json
            let issues = validate db

            Expect.equal (summary issues) [ "tom-validation", "error", "relationship:Sales-Date", None, None ] ""
            Expect.stringContains issues.Head.Message "ToColumn" ""

        testCase "issues come in model order"
        <| fun () ->
            let db =
                sampleWith (fun db ->
                    db.Model.Functions.Add(new Function(Name = "Void", Expression = ""))
                    addMeasure (date db) "Ghost" "[Nowhere]"
                    addMeasure (sales db) "Blank" "")

            Expect.equal
                (validate db |> List.map (fun i -> ObjectRef.format i.Ref))
                [ "measure:Sales/Blank"; "measure:Date/Ghost"; "function:Void" ]
                ""

        match samplePath with
        | None -> ()
        | Some path ->
            testCase "the real sample model (TESSERA_SAMPLE_MODEL) has no errors"
            <| fun () ->
                match ModelIO.loadPath path with
                | Error e -> failtestf "%s" (TessError.message e)
                | Ok(_, db) ->
                    let errors =
                        validate db
                        |> List.filter (fun i -> i.Severity = Severity.Error)
                        |> List.map (fun i -> ObjectRef.format i.Ref, i.Message)

                    Expect.isEmpty errors ""
    ]
