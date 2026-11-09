module Tessera.Tests.BpaTests

open System
open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.BpaRules
open Tessera.Tests.Fixtures

/// Rule JSON in TE's format, from (field, value) pairs.
let ruleJson (rules: (string * obj) list list) =
    JsonSerializer.Serialize(rules |> List.map dict)

let private rule (id: string) (scope: string) (expression: string) : (string * obj) list = [
    "ID", box id
    "Name", box $"Rule {id}"
    "Severity", box 2
    "Scope", box scope
    "Expression", box expression
]

let private parseOk json =
    match parse BuiltIn json with
    | Ok rules -> rules
    | Error e -> failtestf "parse failed: %s" e

let private one scope expression =
    parseOk (ruleJson [ rule "T" scope expression ])

/// The sample model plus objects that break each built-in rule at least once.
let bpaDatabase () =
    let db = sampleDatabase ()
    let m = db.Model
    let sales = m.Tables.["Sales"]
    let date = m.Tables.["Date"]

    let add (t: Table) (c: Column) =
        t.Columns.Add c
        c

    let price =
        add sales (new DataColumn(Name = "Price", DataType = DataType.Double, SourceColumn = "Price"))

    add sales (new DataColumn(Name = "Unused", DataType = DataType.String, SourceColumn = "U", IsHidden = true))
    |> ignore

    let productKey =
        add sales (new DataColumn(Name = "ProductKey", DataType = DataType.Int64, SourceColumn = "PK"))

    add sales (new CalculatedColumn(Name = "Doubled", DataType = DataType.Decimal, Expression = "Sales[Amount] * 2"))
    |> ignore

    add date (new DataColumn(Name = "Date", DataType = DataType.DateTime, SourceColumn = "Date"))
    |> ignore

    let measure name expr =
        sales.Measures.Add(new Measure(Name = name, Expression = expr, FormatString = "0"))

    sales.Measures.Add(new Measure(Name = "No Format", Expression = "1"))
    measure "Ratio" "[Total] / SUM(Sales[Amount])"
    measure "Per Ten" "[Total] / 10"
    measure "Safe" "IFERROR(1, 0)"
    measure "Unqualified" "SUM([Amount])"
    measure "Qualified" "Sales[Total] + 1"
    measure "Debug" "EVALUATEANDLOG([Total])"
    measure "Todo" "1 // TODO: finish\n+ 2"
    measure "Not a todo" "\"TODO\""
    measure "Empty" ""
    measure "Spaced " "1"

    let kpi = new Measure(Name = "With KPI", Expression = "[Total]", FormatString = "0")
    kpi.KPI <- new KPI(StatusExpression = "IF([Amount] > 0, 1, 0)", TargetExpression = "1")
    sales.Measures.Add kpi

    let product = new Table(Name = "Product")

    let key =
        add product (new DataColumn(Name = "ProductKey", DataType = DataType.String, SourceColumn = "PK"))

    product.Partitions.Add(new Partition(Name = "Product", Source = new MPartitionSource(Expression = "1")))
    m.Tables.Add product

    m.Relationships.Add(
        new SingleColumnRelationship(
            Name = "Sales-Product",
            FromColumn = productKey,
            ToColumn = key,
            FromCardinality = RelationshipEndCardinality.Many,
            ToCardinality = RelationshipEndCardinality.Many,
            CrossFilteringBehavior = CrossFilteringBehavior.BothDirections
        )
    )

    let local = new Table(Name = "LocalDateTable_1", IsHidden = true)

    local.Partitions.Add(
        new Partition(Name = "LocalDateTable_1", Source = new CalculatedPartitionSource(Expression = "CALENDARAUTO()"))
    )

    local.Columns.Add(new CalculatedTableColumn(Name = "Date", SourceColumn = "[Date]", DataType = DataType.DateTime))
    m.Tables.Add local

    let empty =
        new Table(Name = "Empty Group", CalculationGroup = new CalculationGroup())

    empty.Columns.Add(new DataColumn(Name = "Name", DataType = DataType.String, SourceColumn = "Name"))
    empty.Partitions.Add(new Partition(Name = "Empty Group", Source = new CalculationGroupSource()))
    m.Tables.Add empty

    let readers = m.Roles.["Readers"]
    readers.TablePermissions.Add(new TablePermission(Table = sales, FilterExpression = "Sales[Price] > 0"))
    m.Cultures.["da-DK"].ObjectTranslations.SetTranslation(price, TranslatedProperty.Caption, "Pris")
    db

/// Run one rule; the refs it finds.
let private hitsOn (db: Database) (scope: string) (expression: string) =
    match Bpa.analyze db (one scope expression) with
    | [ r ] when r.Status = Bpa.Evaluated -> r.Findings |> List.map (fun f -> ObjectRef.format f.Ref)
    | [ r ] -> failtestf "rule did not run: %A" r.Status
    | rs -> failtestf "expected one result, got %d" rs.Length

let private statusOf (db: Database) (rules: Rule list) =
    Bpa.analyze db rules |> List.map (fun r -> r.Rule.Id, r.Status)

[<Tests>]
let loaderTests =
    testList "BPA rules" [
        testCase "TE rule JSON: fields, defaults, case-insensitive names"
        <| fun () ->
            let json =
                """[ { "id": "R1", "expression": "IsHidden", "scope": "Measure",
                       "Category": "Cat", "FixExpression": "IsHidden = false", "CompatibilityLevel": 1200,
                       "Source": "ignored", "LearnMoreUrl": "https://example.com" } ]"""

            match parseOk json with
            | [ r ] ->
                Expect.equal r.Id "R1" ""
                Expect.equal r.Name "R1" "Name defaults to the ID"
                Expect.equal r.Severity 1 "Severity defaults to 1"
                Expect.equal r.Category "Cat" ""
                Expect.equal r.Scopes [ Scope.Measure ] ""
                Expect.equal r.FixExpression (Some "IsHidden = false") ""
                Expect.equal r.CompatibilityLevel 1200 ""
                Expect.equal r.LearnMoreUrl (Some "https://example.com") ""
            | rs -> failtestf "%A" rs

        testCase "comments and trailing commas are accepted"
        <| fun () ->
            let rules =
                parseOk
                    """[ // a rule
                         { "ID": "R", "Scope": "Model", "Expression": "true", }, ]"""

            Expect.equal rules.Length 1 ""

        testCase "scope names: TE spellings, plurals, legacy names, flags"
        <| fun () ->
            let scopes text =
                match parseOk (ruleJson [ rule "R" text "true" ]) with
                | [ r ] -> r.Scopes, r.UnknownScopes
                | _ -> failtest "one rule"

            Expect.equal
                (scopes "Measures, Calculated Columns, Hierarchies")
                ([ Scope.Measure; Scope.CalculatedColumn; Scope.Hierarchy ], [])
                ""

            Expect.equal
                (scopes "Column")
                ([ Scope.DataColumn; Scope.CalculatedColumn; Scope.CalculatedTableColumn ], [])
                "legacy Column"

            Expect.equal (scopes "DataSource") ([ Scope.ProviderDataSource ], []) "legacy DataSource"
            Expect.equal (scopes "Function") ([ Scope.UserDefinedFunction ], []) ""
            Expect.equal (scopes "kpi,calendar") ([ Scope.KPI; Scope.Calendar ], []) "case-insensitive"
            Expect.equal (scopes "Measure, Gizmo") ([ Scope.Measure ], [ "Gizmo" ]) "unknown scopes are kept"

            match parseOk """[ { "ID": "R", "Scope": 4100, "Expression": "true" } ]""" with
            | [ r ] -> Expect.equal r.Scopes [ Scope.Measure; Scope.CalculatedTable ] "flags value"
            | _ -> failtest "one rule"

        testCase "broken rule files are errors"
        <| fun () ->
            let fails json =
                match parse BuiltIn json with
                | Error e -> e
                | Ok _ -> failtestf "expected an error for %s" json

            Expect.stringContains (fails "{}") "array" ""
            Expect.stringContains (fails "[ 1 ]") "expected an object" ""
            Expect.stringContains (fails """[ { "Scope": "Model", "Expression": "x" } ]""") "\"ID\"" ""
            Expect.stringContains (fails """[ { "ID": "A", "Scope": "Model" } ]""") "\"Expression\"" ""
            Expect.stringContains (fails """[ { "ID": "A", "Expression": "x" } ]""") "\"Scope\"" ""

            Expect.stringContains
                (fails """[ { "ID": "A", "Scope": "Model", "Expression": "x", "Severity": "high" } ]""")
                "Severity"
                ""

            fails "[ { " |> ignore

        testCase "rule files: missing and invalid files are typed errors"
        <| fun () ->
            use t = new TempDir()
            let path = t.Combine "rules.json"
            write path "not json"

            match loadFile path with
            | Error(InvalidRules(p, _)) -> Expect.equal p path ""
            | r -> failtestf "%A" r

            match loadFile (t.Combine "nope.json") with
            | Error(PathNotFound _) -> ()
            | r -> failtestf "%A" r

        testCase "later rules replace earlier ones with the same ID"
        <| fun () ->
            let a = one "Model" "true" |> List.map (fun r -> { r with Name = "first" })

            let b =
                one "Model" "false" |> List.map (fun r -> { r with Id = "t"; Name = "second" })

            let c = parseOk (ruleJson [ rule "Other" "Model" "true" ])

            match merge [ a @ c; b ] with
            | [ x; y ] ->
                Expect.equal x.Name "second" ""
                Expect.equal y.Id "Other" ""
            | rs -> failtestf "%A" rs

        testCase "ignore lists"
        <| fun () ->
            Expect.equal (parseIgnoreList """{"RuleIDs":["A","B"]}""") [ "A"; "B" ] ""
            Expect.equal (parseIgnoreList """["A"]""") [ "A" ] ""
            Expect.equal (parseIgnoreList "garbage") [] ""

        testCase "built-in rules load, have unique IDs, and all compile"
        <| fun () ->
            let rules = builtIn.Value
            Expect.isGreaterThanOrEqual rules.Length 15 ""
            Expect.equal (rules |> List.distinctBy (fun r -> r.Id.ToUpperInvariant())).Length rules.Length "unique"

            for r in rules do
                Expect.equal (Bpa.compile r) (Ok()) r.Id
                Expect.isSome r.Description r.Id
    ]

[<Tests>]
let dialectTests =
    testList "BPA expression dialect" [
        let translate = Bpa.Dialect.translate [ typeof<Text.RegularExpressions.Regex> ]

        testCase "outerIt and current become parent and root"
        <| fun () ->
            Expect.equal (translate "Any(OuterIt.Name = current.Name)") "Any(parent.Name = root.Name)" ""
            Expect.equal (translate "x.outerIt") "x.outerIt" "members are left alone"

        testCase "type names are matched case-insensitively"
        <| fun () -> Expect.equal (translate "RegEx.IsMatch(Name, \"a\")") "Regex.IsMatch(Name, \"a\")" ""

        testCase "strings: backslashes are literal, \"\" is a quote; 'x' is a char"
        <| fun () ->
            Expect.equal (translate "\"\\s\"\"\"") "\"\\\\s\\\"\"" ""
            Expect.equal (translate "'a' + 'bc'") "'a' + \"bc\"" ""

        testCase "DAX token names become their codes, only in Tokenize rules"
        <| fun () ->
            Expect.equal
                (translate "Tokenize().Any(Type = DIV and Next.Type <> COMMA)")
                "Tokenize().Any(Type = 411 and Next.Type <> 407)"
                ""

            Expect.equal (translate "Name = DIV") "Name = DIV" ""
            Expect.equal (translate "Tokenize().Any(Type = DIV AND TRUE)") "Tokenize().Any(Type = 411 AND TRUE)" ""
    ]

[<Tests>]
let evaluatorTests =
    testList "BPA evaluator" [
        let db = lazy (bpaDatabase ())
        let hits scope expr = hitsOn db.Value scope expr

        testCase "names, visibility and data types"
        <| fun () ->
            Expect.equal (hits "Table" "Name = \"Sales\"") [ "table:Sales" ] ""
            Expect.equal (hits "Measure" "IsHidden") [ "measure:Sales/Total (hidden)" ] ""

            Expect.contains (hits "DataColumn" "not IsVisible") "column:Sales/DateKey" ""
            Expect.contains (hits "DataColumn" "DataType = \"Double\"") "column:Sales/Price" ""
            Expect.equal (hits "DataColumn" "DataType == DataType.Double") [ "column:Sales/Price" ] "enum member"
            Expect.equal (hits "DataColumn" "DataType.ToString() == \"Double\"") [ "column:Sales/Price" ] ""

            Expect.equal
                (hits "Table" "ObjectTypeName == \"Calculated Table\"")
                []
                "Table scope excludes calculated tables"

            Expect.equal
                (hits "CalculatedTable" "ObjectTypeName == \"Calculated Table\"")
                [ "table:LocalDateTable_1" ]
                ""

            Expect.equal (hits "CalculationGroup" "true") [ "table:Time Intelligence"; "table:Empty Group" ] ""
            Expect.contains (hits "Table" "ObjectTypeName == \"Table (Import)\"") "table:Sales" ""

        testCase "null strings are safe to call methods on"
        <| fun () ->
            Expect.contains (hits "Measure" "not FormatString.Contains(\"#\")") "measure:Sales/No Format" ""
            Expect.contains (hits "Measure" "string.IsNullOrEmpty(Description)") "measure:Sales/No Format" ""
            Expect.contains (hits "Measure" "string.IsNullOrWhitespace(DisplayFolder)") "measure:Sales/Total" ""
            Expect.contains (hits "DataColumn" "SortByColumn = null and Table.Name = \"Date\"") "column:Date/Year" ""

        testCase "ReferencedBy and DependsOn come from DAX references"
        <| fun () ->
            Expect.contains (hits "Measure" "ReferencedBy.Count > 0") "measure:Sales/Total" ""
            Expect.contains (hits "Measure" "ReferencedBy.Count = 0") "measure:Sales/Ratio" ""
            Expect.contains (hits "Measure" "ReferencedBy.Measures.Any(Name = \"Ratio\")") "measure:Sales/Total" ""
            Expect.contains (hits "DataColumn" "ReferencedBy.Roles.Any(Name = \"Readers\")") "column:Sales/Price" ""
            Expect.contains (hits "DataColumn" "ReferencedBy.Any(ObjectType = \"KPI\")") "column:Sales/Amount" ""

            Expect.equal
                (hits "Measure" "DependsOn.Any(Key.ObjectType = \"Column\" and Value.Any(not FullyQualified))")
                [ "measure:Sales/Unqualified" ]
                ""

            Expect.equal
                (hits "Measure" "DependsOn.Measures.Any(Name = \"Total\") and DependsOn.Count = 1")
                [
                    "measure:Sales/Total (hidden)"
                    "measure:Sales/Per Ten"
                    "measure:Sales/Qualified"
                    "measure:Sales/Debug"
                    "measure:Sales/With KPI"
                ]
                "KPI expressions belong to the KPI, not the measure"

            Expect.equal (hits "KPI" "DependsOn.Columns.Any(Name = \"Amount\")") [ "measure:Sales/With KPI" ] ""
            Expect.equal (hits "TablePermission" "DependsOn.Columns.Any(Name = \"Price\")") [ "role:Readers" ] ""

        testCase "structural links are separate from ReferencedBy"
        <| fun () ->
            let unreferenced = hits "DataColumn" "ReferencedBy.Count = 0"
            Expect.contains unreferenced "column:Date/Year" "hierarchy levels aren't DAX references"
            Expect.contains unreferenced "column:Sales/ProductKey" "relationships aren't DAX references"
            Expect.contains (hits "DataColumn" "UsedInRelationships.Any()") "column:Date/DateKey" ""
            Expect.contains (hits "DataColumn" "UsedInHierarchies.Any(Name = \"Calendar\")") "column:Date/Month" ""
            Expect.equal (hits "Table" "UsedInRelationships.Count() = 2") [ "table:Sales" ] ""

        testCase "a calculated table's unqualified [X] is not its own column"
        <| fun () -> Expect.equal (hits "CalculatedTableColumn" "ReferencedBy.Count > 0") [] ""

        testCase "outerIt, current and Model"
        <| fun () ->
            Expect.equal
                (hits "DataColumn" "Model.Relationships.Any(FromColumn = outerIt and FromCardinality == \"Many\")")
                [ "column:Sales/DateKey"; "column:Sales/ProductKey" ]
                ""

            Expect.equal
                (hits "Measure" "Model.AllMeasures.Any(it <> current and Expression = current.Expression)")
                [
                    "measure:Sales/Total (hidden)"
                    "measure:Sales/No Format"
                    "measure:Sales/Spaced "
                    "measure:Sales/With KPI"
                ]
                ""

        testCase "Tokenize"
        <| fun () ->
            Expect.equal (hits "Measure" "Tokenize().Any(Type = IFERROR)") [ "measure:Sales/Safe" ] ""

            Expect.equal
                (hits "Measure" "Tokenize().Any(Type = DIV and Next.Type <> INTEGER_LITERAL)")
                [ "measure:Sales/Ratio" ]
                ""

            Expect.equal
                (hits
                    "Measure"
                    "Tokenize(false).Count(Type = SINGLE_LINE_COMMENT) = 0 and Tokenize().Any(Type = SINGLE_LINE_COMMENT)")
                [ "measure:Sales/Todo" ]
                "Tokenize(false) drops comments"

        testCase "annotations, translations, perspectives, security"
        <| fun () ->
            Expect.equal (hits "Model" "HasAnnotation(\"nope\")") [] ""
            Expect.equal (hits "DataColumn" "TranslatedNames[\"da-DK\"] = \"Pris\"") [ "column:Sales/Price" ] ""

            Expect.equal
                (hits "DataColumn" "Model.Cultures.Any(outerIt.TranslatedNames[it] = \"Pris\")")
                [ "column:Sales/Price" ]
                ""

            Expect.equal
                (hits "Perspective" "not Model.Tables.Any(InPerspective[outerIt])")
                [ "perspective:Sales View" ]
                ""

            Expect.equal (hits "Table" "RowLevelSecurity.Any(it <> null)") [ "table:Sales" ] ""
            Expect.equal (hits "ModelRole" "RowLevelSecurity[\"Sales\"].Contains(\"Price\")") [ "role:Readers" ] ""

        testCase "DAX names"
        <| fun () ->
            Expect.equal (hits "Measure" "DaxObjectFullName = \"[Total]\"") [ "measure:Sales/Total" ] ""
            Expect.equal (hits "DataColumn" "DaxObjectFullName = \"'Sales'[Amount]\"") [ "column:Sales/Amount" ] ""
            Expect.equal (hits "Table" "DaxObjectName = \"'Date'\"") [ "table:Date" ] ""

            Expect.equal
                (hits "Relationship" "Name = \"'Sales'[DateKey] --> 'Date'[DateKey]\"")
                [ "relationship:Sales-Date" ]
                "TE names relationships by their columns"

        testCase "other scopes"
        <| fun () ->
            Expect.equal
                (hits "CalculationItem" "Expression.Contains(\"TOTALYTD\")")
                [ "calcitem:Time Intelligence/YTD" ]
                ""

            Expect.equal (hits "UserDefinedFunction" "Name = \"Double\"") [ "function:Double" ] ""
            Expect.equal (hits "NamedExpression" "Kind == \"M\"") [ "expression:ServerName" ] ""

            Expect.equal
                (hits "Partition" "SourceType == PartitionSourceType.Calculated")
                [ "partition:LocalDateTable_1/LocalDateTable_1" ]
                ""

            Expect.equal (hits "Level" "Ordinal = 1") [ "level:Date/Calendar/Month" ] ""
            Expect.equal (hits "Culture" "true") [ "culture:da-DK" ] ""
    ]

[<Tests>]
let statusTests =
    testList "BPA rule status" [
        let db = lazy (bpaDatabase ())

        testCase "an expression that doesn't compile is unsupported, and the others still run"
        <| fun () ->
            let rules =
                parseOk (
                    ruleJson [
                        rule "BAD" "Measure" "NoSuchProperty = 1"
                        rule "SYNTAX" "Measure" "Name = "
                        rule "NOTBOOL" "Measure" "Name"
                        rule "GOOD" "Measure" "IsHidden"
                    ]
                )

            match statusOf db.Value rules with
            | [ "BAD", Bpa.Unsupported e1
                "SYNTAX", Bpa.Unsupported _
                "NOTBOOL", Bpa.Unsupported _
                "GOOD", Bpa.Evaluated ] ->
                Expect.stringContains e1 "NoSuchProperty" ""
                Expect.stringStarts e1 "Measure:" "names the scope"
            | s -> failtestf "%A" s

        testCase "unknown scopes are unsupported"
        <| fun () ->
            match statusOf db.Value (parseOk (ruleJson [ rule "U" "Measure, Gizmo" "true" ])) with
            | [ _, Bpa.Unsupported e ] -> Expect.stringContains e "Gizmo" ""
            | s -> failtestf "%A" s

        testCase "a runtime error fails the rule without stopping the run"
        <| fun () ->
            let rules =
                parseOk (ruleJson [ rule "BOOM" "Measure" "Name.Substring(100) = \"\""; rule "OK" "Model" "true" ])

            match statusOf db.Value rules with
            | [ "BOOM", Bpa.Failed e; "OK", Bpa.Evaluated ] -> Expect.stringStarts e "Measure:" ""
            | s -> failtestf "%A" s

        testCase "rules for a newer compatibility level are skipped"
        <| fun () ->
            let rules =
                one "Model" "true" |> List.map (fun r -> { r with CompatibilityLevel = 9999 })

            Expect.equal (statusOf db.Value rules) [ "T", Bpa.Skipped 9999 ] ""
    ]

[<Tests>]
let ignoreTests =
    testList "BPA ignore annotations" [
        testCase "an object's annotation suppresses its findings for that rule"
        <| fun () ->
            let db = bpaDatabase ()
            let m = db.Model.Tables.["Sales"].Measures.["No Format"]

            m.Annotations.Add(
                new Annotation(Name = "BestPracticeAnalyzer_IgnoreRules", Value = """{"RuleIDs":["t","other"]}""")
            )

            match Bpa.analyze db (one "Measure" "Name = \"No Format\" or Name = \"Ratio\"") with
            | [ r ] ->
                Expect.equal (r.Findings |> List.map (fun f -> ObjectRef.format f.Ref)) [ "measure:Sales/Ratio" ] ""
                Expect.equal (r.Ignored |> List.map (fun f -> ObjectRef.format f.Ref)) [ "measure:Sales/No Format" ] ""
            | rs -> failtestf "%A" rs

        testCase "the model's annotation (or TE's old misspelling) turns a rule off"
        <| fun () ->
            let db = bpaDatabase ()

            db.Model.Annotations.Add(
                new Annotation(Name = "BestPractizeAnalyzer_IgnoreRules", Value = """{"RuleIDs":["T"]}""")
            )

            Expect.equal (statusOf db (one "Model" "true")) [ "T", Bpa.Disabled ] ""
    ]

[<Tests>]
let builtInTests =
    testList "BPA built-in rules" [
        testCase "every built-in rule finds something in the BPA fixture"
        <| fun () ->
            let results = Bpa.analyze (bpaDatabase ()) builtIn.Value

            for r in results do
                Expect.equal r.Status Bpa.Evaluated r.Rule.Id
                Expect.isNonEmpty r.Findings r.Rule.Id

        testCase "built-in rules don't fire on objects that follow them"
        <| fun () ->
            let findings =
                Bpa.analyze (bpaDatabase ()) builtIn.Value
                |> List.collect (fun r -> r.Findings |> List.map (fun f -> r.Rule.Id, ObjectRef.format f.Ref))

            for unexpected in
                [
                    "TESS_DIVIDE", "measure:Sales/Per Ten"
                    "TESS_TODO_COMMENT", "measure:Sales/Not a todo"
                    "TESS_MEASURE_FORMAT_STRING", "measure:Sales/Total"
                    "TESS_UNUSED_HIDDEN_COLUMN", "column:Sales/DateKey"
                    "TESS_QUALIFY_COLUMNS", "measure:Sales/Total"
                ] do
                Expect.isFalse (List.contains unexpected findings) $"%A{unexpected}"
    ]

[<Tests>]
let sourceTests =
    testList "BPA rule sources" [
        testCase "rules embedded in the model, and external rule files it lists"
        <| fun () ->
            use t = new TempDir()
            let db = bpaDatabase ()
            write (t.Combine "extra.json") (ruleJson [ rule "EXTRA" "Model" "true" ])

            db.Model.Annotations.Add(
                new Annotation(Name = "BestPracticeAnalyzer", Value = ruleJson [ rule "EMBEDDED" "Model" "true" ])
            )

            db.Model.Annotations.Add(
                new Annotation(
                    Name = "BestPracticeAnalyzer_ExternalRuleFiles",
                    Value = """["extra.json", "missing.json", "https://example.com/rules.json"]"""
                )
            )

            let bim = t.Combine "model.bim"
            ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false db bim |> ignore

            let source, loaded =
                match ModelIO.loadPath bim with
                | Ok x -> x
                | Error e -> failtest (TessError.message e)

            let opts = {
                Bpa.RuleOptions.defaults with
                    BuiltIn = false
            }

            match Bpa.collectRules source loaded opts with
            | Ok c ->
                Expect.equal (c.Effective |> List.map (fun r -> r.Id)) [ "EXTRA"; "EMBEDDED" ] ""

                Expect.equal
                    (c.Effective |> List.map (fun r -> r.Source))
                    [ ExternalRuleFile(t.Combine "extra.json"); ModelAnnotation ]
                    ""

                Expect.equal c.Warnings.Length 1 "a missing file"
                Expect.equal c.Notes.Length 1 "a URL, not fetched"
            | Error e -> failtest (TessError.message e)

            match Bpa.collectRules source loaded { opts with ModelRules = false } with
            | Ok c -> Expect.isEmpty c.Effective ""
            | Error e -> failtest (TessError.message e)
    ]

/// Optional: run real rule files (e.g. TabularEditor/BestPracticeRules) against a model. Set
/// TESSERA_BPA_RULES to one or more rule files (separated by the path separator); the model is
/// TESSERA_SAMPLE_MODEL if set, else the BPA fixture.
[<Tests>]
let compatibilityTests =
    let files =
        match Environment.GetEnvironmentVariable "TESSERA_BPA_RULES" with
        | null
        | "" -> []
        | v ->
            v.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray

    let isFailed =
        function
        | Bpa.Failed _ -> true
        | _ -> false

    let isUnsupported =
        function
        | Bpa.Unsupported _ -> true
        | _ -> false

    let isSkipped =
        function
        | Bpa.Skipped _
        | Bpa.Disabled -> true
        | _ -> false

    let run (file: string) () =
        let db =
            match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
            | null
            | "" -> bpaDatabase ()
            | path ->
                match ModelIO.loadPath path with
                | Ok(_, db) -> db
                | Error e -> failtest (TessError.message e)

        let rules =
            match loadFile file with
            | Ok rules -> rules
            | Error e -> failtest (TessError.message e)

        let results = Bpa.analyze db rules

        let count p =
            results |> List.filter (fun r -> p r.Status) |> List.length

        let problems =
            results
            |> List.choose (fun r ->
                match r.Status with
                | Bpa.Unsupported e
                | Bpa.Failed e -> Some $"  {r.Rule.Id}: {e}"
                | _ -> None)

        eprintfn
            "%s: %d rules, %d supported, %d unsupported, %d failed, %d skipped\n%s"
            (Path.GetFileName file)
            rules.Length
            (count (fun s -> s = Bpa.Evaluated))
            (count isUnsupported)
            (count isFailed)
            (count isSkipped)
            (String.concat "\n" problems)

        Expect.equal (count isFailed) 0 "no rule fails while running"

    testList
        "BPA compatibility (TESSERA_BPA_RULES)"
        (if files.IsEmpty then
             [
                 testCase "not configured" (fun () -> skiptest "TESSERA_BPA_RULES is not set")
             ]
         else
             [ for file in files -> testCase (Path.GetFileName file) (run file) ])
