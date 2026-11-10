module Tessera.Tests.EmbedTests

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.DaxAst
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

// ---- Reading an embedded table back ---------------------------------------------------------------

/// What an embedded table's expression holds, read with DaxParser and nothing else: the declared
/// columns, and the rows with each constant as a value of its column's type (which is what the
/// DATATABLE documentation says the engine does with it).
let readTable (expression: string) : (string * DaxType) list * DaxValue array list =
    let inv = CultureInfo.InvariantCulture

    let rec strip (e: Expr) =
        match e.Kind with
        | Paren inner -> strip inner
        | _ -> e

    let typeOf (word: string) =
        match DaxType.all |> List.tryFind (fun t -> DaxType.name t = word) with
        | Some t -> t
        | None -> failtestf "DATATABLE type %s" word

    let rec constant (dataType: DaxType) (negative: bool) (e: Expr) : DaxValue =
        let number (text: string) =
            let text = if negative then "-" + text else text

            match dataType with
            | DaxType.Integer -> DaxValue.Integer(Int64.Parse(text, NumberStyles.AllowLeadingSign, inv))
            | DaxType.Decimal -> DaxValue.Decimal(Double.Parse(text, NumberStyles.Float, inv))
            | DaxType.Currency ->
                match DaxValue.tryCurrency (Decimal.Parse(text, NumberStyles.Float, inv)) with
                | Some c -> c
                | None -> failtestf "currency constant %s" text
            | other -> failtestf "a number in a %A column" other

        match (strip e).Kind, dataType with
        | Call(name, []), _ when name.Name.ToUpperInvariant() = "BLANK" -> DaxValue.Blank
        | Unary(UnaryOp.Minus, inner), _ -> constant dataType (not negative) inner
        | IntegerLiteral(_, text), _ -> number text
        | NumberLiteral(_, text), _ -> number text
        | StringLiteral s, DaxType.String -> DaxValue.String s
        | BooleanLiteral b, DaxType.Boolean -> DaxValue.Boolean b
        | DateLiteral text, DaxType.DateTime ->
            match tryDate text with
            | Some d -> DaxValue.ofDateTime d
            | None -> failtestf "date literal %s" text
        | kind, _ -> failtestf "not a DATATABLE constant for %A: %A" dataType kind

    let dataTable (e: Expr) =
        match (strip e).Kind with
        | Call(name, args) when name.Name.ToUpperInvariant() = "DATATABLE" ->
            let declarations, data = List.splitAt (args.Length - 1) args

            let columns =
                declarations
                |> List.chunkBySize 2
                |> List.map (function
                    | [ { Kind = StringLiteral columnName }; Keyword word ] -> columnName, typeOf word
                    | other -> failtestf "DATATABLE column declaration: %A" other)

            let rows =
                match data with
                | [ { Kind = TableConstructor rows } ] ->
                    rows
                    |> List.map (fun row ->
                        match row.Kind with
                        | TableConstructor cells when cells.Length = columns.Length ->
                            List.map2 (fun (_, t) cell -> constant t false cell) columns cells
                            |> Array.ofList
                        | other -> failtestf "DATATABLE row: %A" other)
                | other -> failtestf "DATATABLE data: %A" other

            columns, rows
        | other -> failtestf "expected DATATABLE, found %A" other

    match DaxParser.parseExpression expression with
    | Error errors -> failtestf "the expression doesn't parse: %A\n%s" errors expression
    | Ok e ->
        match (strip e).Kind with
        | Call(name, [ inner; condition ]) when name.Name.ToUpperInvariant() = "FILTER" ->
            match (strip condition).Kind with
            | Call(f, []) when f.Name.ToUpperInvariant() = "FALSE" -> fst (dataTable inner), []
            | other -> failtestf "FILTER condition: %A" other
        | _ -> dataTable e

/// Check that an embedded model holds exactly the store's rows, table by table, and return how
/// many tables were compared.
let expectHoldsStore (original: Database) (store: DataStore) (embedded: FixtureEmbed.Embedded) =
    let compared =
        Fixture.dataTables original
        |> List.map (fun table ->
            let after = embedded.Database.Model.Tables.[table.Name]
            Expect.isTrue (Fixture.isComputed after) $"{table.Name} is a calculated table"

            let source =
                match after.Partitions |> Seq.exactlyOne |> _.Source with
                | :? CalculatedPartitionSource as s -> s.Expression
                | other -> failtestf "%s: partition source %A" table.Name other

            let columns, rows = readTable source
            let expected = Fixture.dataColumns table |> List.map (fun (c, t) -> c.Name, t)

            if expected.IsEmpty then
                Expect.equal columns [ FixtureEmbed.PlaceholderColumn, DaxType.Integer ] table.Name
                Expect.isEmpty rows table.Name
            else
                Expect.equal columns expected $"{table.Name}: columns"

                let stored =
                    match store.TryTable table.Name with
                    | None -> []
                    | Some t -> [
                        for row in 0 .. t.RowCount - 1 ->
                            expected
                            |> List.map (fun (name, _) ->
                                match t.TryColumn name with
                                | Some c -> c.[row]
                                | None -> DaxValue.Blank)
                            |> Array.ofList
                      ]

                Expect.equal rows.Length stored.Length $"{table.Name}: rows"

                for n, (a, b) in List.indexed (List.zip rows stored) do
                    Expect.equal a b $"{table.Name}: row {n}"

            // Every column the model declared is still there, with its type.
            for column, _ in Fixture.dataColumns table do
                let now = after.Columns.[column.Name]
                Expect.equal now.DataType column.DataType $"{table.Name}[{column.Name}]: type"
                Expect.equal now.Type ColumnType.CalculatedTableColumn $"{table.Name}[{column.Name}]: kind"

            table.Name)

    compared.Length

// ---- A fixture with every awkward value -----------------------------------------------------------

let private productCsv =
    "ProductKey,Name,Price,Weight,Active,Launched\n"
    + "1,Bike,499.95,12.5,true,2020-03-01\n"
    + "-2,\"He said \"\"hi\"\", then\nleft\",-0.0001,-2.5e-3,false,2021-11-15T08:30:00\n"
    + "3,\"\",922337203685477.5807,1e21,1,1899-12-31\n"
    + "9223372036854775807,Ærø ☃,,1e-7,,\n"
    + "-9223372036854775808,trailing  ,0,0.1,,2024-02-29T23:59:59\n"
    + "6,,12,123456789.123456789,true,1900-03-01T00:00:01\n"

let private customerCsv = "Code,Name\nC1,Ann\nC2,Bob\n"

let private salesCsv =
    "OrderId,ProductKey,CustomerCode,Amount,When\n"
    + "100,1,C1,499.95,2020-05-01\n"
    + "101,-2,c2,49,2020-05-02\n"
    + "102,77,C1,10,2020-05-02\n"

let private fixture (files: (string * string) list) =
    let t = new TempDir()

    for name, text in files do
        File.WriteAllText(t.Combine name, text, UTF8Encoding false)

    t

let private awkward () =
    fixture [
        "Product.csv", productCsv
        "Customer.csv", customerCsv
        "Sales.csv", salesCsv
    ]

let private load (db: Database) (directory: string) =
    match Fixture.loadAndCheck db directory with
    | Ok(loaded, issues) ->
        Expect.isFalse (Fixture.hasErrors issues) $"%A{issues}"
        loaded
    | Error e -> failtest (TessError.message e)

let private embedOk (db: Database) (store: DataStore) =
    match FixtureEmbed.embed db store with
    | Ok e -> e
    | Error e -> failtest (TessError.message e)

let private errors (db: Database) =
    Validate.validate db
    |> List.filter (fun i -> i.Severity = Validate.Severity.Error)

[<Tests>]
let embedTests =
    testList "FixtureEmbed" [
        testCase "every loaded table becomes a calculated table holding exactly its rows"
        <| fun () ->
            let db = FixtureTests.model ()
            use dir = awkward ()
            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store
            Expect.equal (expectHoldsStore db loaded.Store embedded) 3 "Product, Customer, Sales"

            Expect.equal
                (embedded.Tables |> List.map (fun t -> t.Table, t.Rows, t.Columns))
                [ "Product", 6, 6; "Customer", 2, 2; "Sales", 3, 5 ]
                ""

        testCase "the rest of the model is as it was, and nothing needs a data source"
        <| fun () ->
            let db = FixtureTests.model ()

            db.Model.Expressions.Add(
                new NamedExpression(Name = "Server", Kind = ExpressionKind.M, Expression = "\"localhost\"")
            )

            db.Model.Tables.["Product"].Columns.["Name"].IsHidden <- true
            db.Model.Tables.["Product"].Columns.["Price"].FormatString <- "#,0.00"

            db.Model.Tables.["Product"].Measures.Add(
                new Measure(Name = "Avg Price", Expression = "AVERAGE ( Product[Price] )")
            )

            use dir = awkward ()
            let loaded = load db dir.Path
            let before = errors db
            let embedded = embedOk db loaded.Store
            let m = embedded.Database.Model

            Expect.equal embedded.Database.Name db.Name ""
            Expect.equal (m.Tables |> Seq.map _.Name |> List.ofSeq) (db.Model.Tables |> Seq.map _.Name |> List.ofSeq) ""
            Expect.equal m.Relationships.Count db.Model.Relationships.Count "relationships"

            for r in m.Relationships do
                let r = r :?> SingleColumnRelationship
                Expect.isNotNull r.FromColumn r.Name
                Expect.isNotNull r.ToColumn r.Name

            let product = m.Tables.["Product"]
            Expect.equal product.Columns.["Margin"].Type ColumnType.Calculated "a calculated column stays one"
            Expect.isTrue product.Columns.["Name"].IsHidden "column properties are kept"
            Expect.equal product.Columns.["Price"].FormatString "#,0.00" ""
            Expect.isTrue m.Tables.["Customer"].Columns.["Code"].IsKey "the key is kept"
            Expect.isTrue (product.Measures.ContainsName "Avg Price") "measures are kept"

            match m.Tables.["Calendar"].Partitions.[0].Source with
            | :? CalculatedPartitionSource as s -> Expect.stringContains s.Expression "CALENDAR" "untouched"
            | other -> failtestf "%A" other

            Expect.isNotNull m.Tables.["Time"].CalculationGroup "the calculation group is untouched"
            Expect.equal m.Expressions.Count 0 "no shared expressions"
            Expect.equal m.DataSources.Count 0 ""
            Expect.exists embedded.Warnings (fun w -> w.Contains "1 shared expression removed") ""

            for t in m.Tables do
                for p in t.Partitions do
                    Expect.isTrue
                        (p.SourceType = PartitionSourceType.Calculated
                         || p.SourceType = PartitionSourceType.CalculationGroup)
                        $"{t.Name}: {p.SourceType}"

            Expect.isEmpty (Fixture.dataTables embedded.Database) "nothing left for a fixture to supply"

            Expect.equal
                (errors embedded.Database |> List.map _.Rule)
                (before |> List.map _.Rule)
                "no new validation errors"

            // The model given is not changed.
            Expect.equal db.Model.Expressions.Count 1 ""
            Expect.equal db.Model.Tables.["Product"].Partitions.[0].SourceType PartitionSourceType.M ""

        testCase "values a constant can't hold exactly are warned about"
        <| fun () ->
            let db = FixtureTests.model ()

            use dir =
                fixture [
                    "Product.csv",
                    "ProductKey,Name,Price,Weight,Active,Launched\n"
                    + "1,a,922337203685477.5807,1,true,2024-01-31T13:05:00.25\n"
                    + "2,b,1.5,2,true,2024-01-31T13:05:00\n"
                ]

            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store

            Expect.exists
                embedded.Warnings
                (fun w -> w.Contains "15 significant digits" && w.Contains "'Product'[Price]")
                ""

            Expect.exists
                embedded.Warnings
                (fun w -> w.Contains "whole seconds" && w.Contains "2024-01-31T13:05:00.25")
                ""

            Expect.equal
                (embedded.Warnings |> List.filter (fun w -> w.Contains "Product")).Length
                2
                "only those two values"

            let _, rows =
                readTable (embedded.Tables |> List.find (fun t -> t.Table = "Product")).Expression

            Expect.equal rows.[0].[5] (DaxValue.ofDateTime (DateTime(2024, 1, 31, 13, 5, 0))) "the fraction is dropped"

        testCase "a table with no rows, or no file, keeps its columns and types"
        <| fun () ->
            let db = FixtureTests.model ()
            use dir = fixture [ "Customer.csv", "Code,Name\n" ]
            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store
            expectHoldsStore db loaded.Store embedded |> ignore

            for name in [ "Customer"; "Sales" ] do
                let expression = (embedded.Tables |> List.find (fun t -> t.Table = name)).Expression
                Expect.stringStarts expression "FILTER (" name
                Expect.stringContains expression "FALSE ()" name
                let columns, rows = readTable expression
                Expect.isEmpty rows name
                Expect.isNonEmpty columns name

        testCase "a table with no data columns gets a hidden placeholder column"
        <| fun () ->
            let db = FixtureTests.model ()
            let measures = new Table(Name = "KPIs")
            measures.Measures.Add(new Measure(Name = "One", Expression = "1"))

            measures.Partitions.Add(
                new Partition(Name = "KPIs", Source = new MPartitionSource(Expression = "let x = 1 in x"))
            )

            db.Model.Tables.Add measures
            use dir = awkward ()
            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store
            expectHoldsStore db loaded.Store embedded |> ignore
            let table = embedded.Database.Model.Tables.["KPIs"]
            Expect.equal table.Columns.Count 1 ""
            Expect.isTrue table.Columns.[FixtureEmbed.PlaceholderColumn].IsHidden ""
            Expect.isTrue (table.Measures.ContainsName "One") ""
            Expect.exists embedded.Warnings (fun w -> w.Contains "'KPIs' has no data columns") ""

        testCase "the expression is plain DATATABLE: names and strings quoted, types declared"
        <| fun () ->
            let db = new Database(Name = "D", ID = "D", CompatibilityLevel = 1702)
            db.Model <- new Model(Name = "Model")
            let t = new Table(Name = "Odd \"name\"")
            t.Columns.Add(new DataColumn(Name = "A \"quoted\" col", DataType = DataType.String, SourceColumn = "a"))
            t.Columns.Add(new DataColumn(Name = "N", DataType = DataType.Int64, SourceColumn = "n"))
            t.Partitions.Add(new Partition(Name = "p", Source = new MPartitionSource(Expression = "let x = 1 in x")))
            db.Model.Tables.Add t

            use dir =
                fixture [
                    FixtureName.encode t.Name + ".csv", "\"A \"\"quoted\"\" col\",N\n\"say \"\"x\"\"\",-5\n,\n"
                ]

            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store

            Expect.equal
                embedded.Tables.Head.Expression
                ("DATATABLE (\n"
                 + "    \"A \"\"quoted\"\" col\", STRING,\n"
                 + "    \"N\", INTEGER,\n"
                 + "    {\n"
                 + "        { \"say \"\"x\"\"\", -5 },\n"
                 + "        { BLANK (), BLANK () }\n"
                 + "    }\n"
                 + ")")
                ""

            expectHoldsStore db loaded.Store embedded |> ignore

        testCase "a double is written in plain digits that read back as the same double"
        <| fun () ->
            let cases = [
                0.0, "0"
                1.0, "1"
                -2.5e-3, "-0.0025"
                1e21, "1000000000000000000000"
                1e-7, "0.0000001"
                1.5e300, "15" + String('0', 299)
                123456789.12345679, "123456789.12345679"
                0.1, "0.1"
            ]

            for value, text in cases do
                Expect.equal (FixtureEmbed.doubleLiteral value) text (string value)

            let random = Random 7

            for _ in 1..20000 do
                let value = BitConverter.Int64BitsToDouble(random.NextInt64())

                if Double.IsFinite value then
                    let text = FixtureEmbed.doubleLiteral value
                    Expect.isFalse (text.Contains 'E' || text.Contains 'e') text
                    Expect.equal (Double.Parse(text, CultureInfo.InvariantCulture)) value text

        testCase "the embedded model saves and loads back the same, in every format"
        <| fun () ->
            let db = FixtureTests.model ()
            use dir = awkward ()
            let loaded = load db dir.Path
            let embedded = embedOk db loaded.Store
            use out = new TempDir()

            for format, name in
                [
                    ModelIO.BimFile, "m.bim"
                    ModelIO.TmdlFolder, "tmdl"
                    ModelIO.PbipFolder, "M.SemanticModel"
                ] do
                let destination = out.Combine name

                match ModelIO.save format ModelIO.SaveOptions.defaults false embedded.Database destination with
                | Error e -> failtest (TessError.message e)
                | Ok _ ->
                    match ModelIO.loadPath destination with
                    | Error e -> failtest (TessError.message e)
                    | Ok(_, back) ->
                        // (TMDL gives the fixture's calculation item an ordinal: nothing to do with embedding.)
                        let changes =
                            Diff.compare Diff.Options.none embedded.Database back
                            |> List.filter (fun c -> not (c.Label.StartsWith "calcitem:"))

                        Expect.isEmpty changes $"{name}: %A{changes}"

                        expectHoldsStore db loaded.Store { embedded with Database = back } |> ignore
    ]

// ---- tess data embed ------------------------------------------------------------------------------

let private setup () =
    let t = new TempDir()
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase(FixtureTests.model ()))
    let data = t.Combine "data"
    Directory.CreateDirectory data |> ignore
    File.WriteAllText(Path.Combine(data, "Product.csv"), productCsv, UTF8Encoding false)
    File.WriteAllText(Path.Combine(data, "Customer.csv"), customerCsv, UTF8Encoding false)
    File.WriteAllText(Path.Combine(data, "Sales.csv"), salesCsv, UTF8Encoding false)
    t, path, data

[<Tests>]
let commandTests =
    testList "CLI data embed" [
        testCase "embed writes the model with its data, and says what it did"
        <| fun () ->
            let t, model, data = setup ()
            use _ = t
            let out = t.Combine "embedded.bim"
            let r = tess [ "data"; "embed"; model; data; out ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (r.Out.Split('\n') |> Array.filter ((<>) ""))
                [|
                    $"Wrote bim  {out}  (3 tables now hold their fixture rows)"
                    "  Product   6 rows"
                    "  Customer  2 rows"
                    "  Sales     3 rows"
                |]
                ""

            Expect.stringContains r.Err "warning: 'Product'[Price] holds 922337203685477.5807" ""

            match ModelIO.loadPath out with
            | Error e -> failtest (TessError.message e)
            | Ok(_, back) ->
                let original = FixtureTests.model ()
                let loaded = load original data

                expectHoldsStore original loaded.Store {
                    Database = back
                    Tables = []
                    Warnings = []
                }
                |> ignore

            // The written model is a model like any other.
            Expect.equal
                (tess [ "validate"; out ]).Code
                (tess [ "validate"; model ]).Code
                "validates as the original does"

        testCase "embed --json lists the tables, the files and the warnings"
        <| fun () ->
            let t, model, data = setup ()
            use _ = t
            let out = t.Combine "tmdl-out"
            let r = tess [ "data"; "embed"; model; data; out; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "data embed" ""
            Expect.equal (root.GetProperty("directory").GetString()) (Path.GetFullPath data) ""
            Expect.equal (root.GetProperty("output").GetProperty("format").GetString()) "tmdl" ""
            Expect.isGreaterThan (root.GetProperty("output").GetProperty("files").GetArrayLength()) 0 ""

            Expect.equal
                [
                    for x in root.GetProperty("tables").EnumerateArray() ->
                        x.GetProperty("table").GetString(),
                        x.GetProperty("rows").GetInt32(),
                        x.GetProperty("columns").GetInt32()
                ]
                [ "Product", 6, 6; "Customer", 2, 2; "Sales", 3, 5 ]
                ""

            Expect.equal (root.GetProperty("warnings").GetArrayLength()) 1 ""
            Expect.equal r.Err "" "warnings are in the JSON"

        testCase "embed refuses to overwrite without --force"
        <| fun () ->
            let t, model, data = setup ()
            use _ = t
            let out = t.Combine "embedded.bim"
            File.WriteAllText(out, "mine")
            let r = tess [ "data"; "embed"; model; data; out ]
            Expect.equal r.Code ExitCode.output r.Out
            Expect.equal (File.ReadAllText out) "mine" "untouched"
            let forced = tess [ "data"; "embed"; model; data; out; "--force" ]
            Expect.equal forced.Code ExitCode.ok forced.Err
            Expect.isOk (ModelIO.loadPath out) ""

        testCase "embed refuses a fixture with errors: it would deploy other data"
        <| fun () ->
            let t, model, data = setup ()
            use _ = t
            File.WriteAllText(Path.Combine(data, "Customer.csv"), "Code,Name\nC1,Ann\nC1,Again\n")
            let out = t.Combine "embedded.bim"
            let r = tess [ "data"; "embed"; model; data; out; "--json" ]
            Expect.equal r.Code ExitCode.input r.Out
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "invalid-fixture" ""

            Expect.stringContains
                (doc.RootElement.GetProperty("error").GetProperty("message").GetString())
                "tess data check"
                ""

            Expect.isFalse (File.Exists out) "nothing written"

        testCase "embed: a missing model or directory, and a bad format, are reported as elsewhere"
        <| fun () ->
            let t, model, data = setup ()
            use _ = t
            let out = t.Combine "o.bim"
            Expect.equal (tess [ "data"; "embed"; t.Combine "nope.bim"; data; out ]).Code ExitCode.input ""
            Expect.equal (tess [ "data"; "embed"; model; t.Combine "nope"; out ]).Code ExitCode.input ""
            Expect.equal (tess [ "data"; "embed"; model; data; out; "--format"; "xlsx" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "data"; "embed"; model; data ]).Code ExitCode.usage "three arguments"
            Expect.isFalse (File.Exists out) ""
    ]
