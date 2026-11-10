/// `tess test`, through the real CLI entry point: suites run offline over fixture data (the
/// harness fails a test if the command asks for a connection), and on a fake server.
module Tessera.Tests.TestCommandTests

open System
open System.IO
open System.Text.Json
open System.Xml.Linq
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.EngineHelpers
open Tessera.Tests.XmlaFakes
open Tessera.Cli.Common

/// The star model with a measure the local engine refuses.
let private model = {
    star with
        Measures = star.Measures @ [ "Sales", "Label", "FORMAT ( [Amount], \"0\" )" ]
}

/// A test model as model.bim in a temp folder, with its data as a fixture directory `data`.
let private setupWith (m: TestModel) =
    let t = new TempDir()
    let db, store = build m
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    let dir = t.Combine "data"
    Directory.CreateDirectory dir |> ignore

    for table in store.Tables do
        use writer =
            new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

        Fixture.writeCsv writer table

    t, path

let private setup () = setupWith model

/// Write a suite to the model's `.tess-tests`.
let private suite (t: TempDir) (name: string) (yaml: string) =
    let path = t.Combine(".tess-tests", name)
    write path yaml
    path

/// A suite over the fixture next to the model, with these tests.
let private tests (t: TempDir) (yaml: string) =
    suite t "s.test.yaml" ("suite: S\ndata: ../data\ntests:\n" + yaml)

/// (name, outcome) of every test in a JSON report.
let private outcomes (r: Run) =
    use doc = json r

    [
        for s in doc.RootElement.GetProperty("suites").EnumerateArray() do
            for t in s.GetProperty("tests").EnumerateArray() ->
                t.GetProperty("name").GetString(), t.GetProperty("outcome").GetString()
    ]

/// The JSON of one test in a report.
let private testJson (r: Run) (name: string) (f: JsonElement -> 'T) : 'T =
    use doc = json r

    let found =
        doc.RootElement.GetProperty("suites").EnumerateArray()
        |> Seq.collect (fun s -> s.GetProperty("tests").EnumerateArray())
        |> Seq.tryFind (fun t -> t.GetProperty("name").GetString() = name)

    match found with
    | Some t -> f t
    | None -> failtestf "no test '%s' in:\n%s" name r.Out

let private message (r: Run) (name: string) =
    testJson r name (fun t -> t.GetProperty("message").GetString())

let private differences (r: Run) (name: string) =
    testJson r name (fun t -> [ for d in t.GetProperty("differences").EnumerateArray() -> d.GetString() ])

let private summary (r: Run) =
    use doc = json r
    let s = doc.RootElement.GetProperty "testSummary"

    [ "passed"; "failed"; "errored"; "unsupported"; "skipped"; "total" ]
    |> List.map (fun k -> s.GetProperty(k).GetInt32())

let private codes (r: Run) =
    use doc = json r

    [
        for f in doc.RootElement.GetProperty("findings").EnumerateArray() -> f.GetProperty("code").GetString()
    ]

let private lines (s: string) =
    s.Split('\n') |> Array.filter ((<>) "") |> List.ofArray

/// A fake server holding the star model, answering queries with the local engine over its data.
let private engineServer () =
    let db, store = build star

    let prepared =
        match Engine.prepare db store with
        | Ok p -> p
        | Error e -> failtestf "%s" (TessError.message e)

    let server = FakeServer()
    server.Add db

    server.OnQuery <-
        fun dax ->
            match Engine.evaluateQuery prepared dax with
            | Ok results -> results
            | Error e -> raise (XmlaQueryError(TessError.message e))

    server

let private remote = $"xmla:{workspace};Test"

[<Tests>]
let tests' =
    testList "CLI test" [
        testCase "an end-to-end example: every kind of test on a fixture model" (fun () ->
            let t, path = setup ()
            use _ = t
            write (t.Combine(".tess-tests", "by-country.csv")) "Customer[Country],Amount\nUK,140\nUS,485\n"

            write
                (t.Combine(".tess-tests", "by-color.json"))
                """[ { "Color": "Red", "Units": 12 }, { "Color": "Black", "Units": 1 }, { "Color": null, "Units": 4 }, { "Color": "Blue", "Units": null } ]"""

            tests
                t
                """  - name: total amount
    measure: Amount
    expect: 625
  - name: amount in 2024
    measure: Amount
    filters:
      Date[Year]: 2024
    expect: { value: 525, tolerance: 0.001 }
  - name: orders by year
    matrix:
      - { year: 2023, orders: 1 }
      - { year: 2024, orders: 6 }
    measure: Orders
    filters:
      Date[Year]: "{year}"
    expect: "{orders}"
  - name: average price is sane
    expression: DIVIDE ( [Amount], [Units] )
    expect: { gt: 30, lte: 40 }
  - name: units never exceed amount
    assert: "[Units] <= [Amount]"
  - name: no sales without a customer
    query: EVALUATE FILTER ( Sales, ISBLANK ( RELATED ( Customer[Name] ) ) )
    expect: { empty: true }
  - name: amount by country
    query: |
      EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] )
    expect:
      rows: 2
      columns: [Country, Amount]
      table:
        - { Country: US, Amount: 485 }
        - { Country: UK, Amount: 140 }
  - name: amount by country, from a file
    query: |
      EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] ) ORDER BY Customer[Country]
    expect: { file: by-country.csv }
  - name: units by color, from a JSON file
    query: |
      EVALUATE ADDCOLUMNS ( VALUES ( Product[Color] ), "Units", [Units] )
    expect: { file: by-color.json }
  - name: a one-row query
    query: EVALUATE ROW ( "Units", [Units], "Orders", [Orders], "First", MIN ( 'Date'[Date] ), "Name", "x" )
    expect:
      Units: 17
      Orders: { gte: 1 }
      First: 2023-12-31
      Name: x
  - name: a missing measure is an error
    measure: Nope
    expect: { error: dax-invalid }
  - name: an error raised by the measure
    expression: ERROR ( "too many" )
    expect: { error: { code: dax-error, message: too many } }
"""
            |> ignore

            let r = tess [ "test"; "run"; path; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.isTrue (outcomes r |> List.forall (fun (_, o) -> o = "passed")) r.Out
            Expect.equal (summary r) [ 13; 0; 0; 0; 0; 13 ] "summary"
            Expect.equal (codes r) [] "no findings"

            let text = tess [ "test"; "run"; path ]
            Expect.equal text.Code ExitCode.ok text.Err
            let out = lines text.Out
            Expect.equal out.[1] "  pass         total amount" "a line per test"
            Expect.equal out.[3] "  pass         orders by year [year=2023, orders=1]" "matrix names"
            Expect.stringStarts (List.last out) "13 tests: 13 passed (" "summary")

        testCase "failures say what differs, value by value" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: wrong total
    measure: Amount
    expect: 600
  - name: outside the tolerance
    measure: Amount
    expect: { value: 600, tolerance: 0.01, tolerance_abs: 5 }
  - name: inside the tolerance
    measure: Amount
    expect: { value: 620, tolerance: 0.01 }
  - name: inside the absolute tolerance
    measure: Amount
    tolerance_abs: 5
    expect: 620
  - name: blank is not zero
    expression: BLANK ()
    expect: 0
  - name: bounds
    measure: Units
    expect: { gt: 17, lt: 10 }
  - name: a false assertion
    assert: "[Units] > [Amount]"
  - name: a blank assertion
    assert: BLANK ()
  - name: wrong table
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] )
    expect:
      table:
        - { Country: US, Amount: 485 }
        - { Country: UK, Amount: 141 }
        - { Country: FR, Amount: 1 }
  - name: wrong order
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] ) ORDER BY [Amount]
    expect:
      table:
        - { Country: US, Amount: 485 }
        - { Country: UK, Amount: 140 }
  - name: order ignored when asked
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] ) ORDER BY [Amount]
    expect:
      ordered: false
      table:
        - { Country: US, Amount: 485 }
        - { Country: UK, Amount: 140 }
  - name: shape
    query: EVALUATE VALUES ( Customer[Country] )
    expect: { rows: { gte: 3 }, empty: true, columns: [Country, Amount] }
  - name: several rows
    query: EVALUATE VALUES ( Customer[Country] )
    expect: { Country: US }
  - name: missing column
    query: EVALUATE ROW ( "a", 1 )
    expect: { b: 1 }
  - name: no error
    measure: Amount
    expect: { error: true }
  - name: another error
    measure: Nope
    expect: { error: dax-error }
  - name: a text is not a number
    expression: '"625"'
    expect: 625
"""
            |> ignore

            let r = tess [ "test"; "run"; path; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out

            Expect.equal
                (outcomes r |> List.filter (fun (_, o) -> o <> "failed") |> List.map fst)
                [
                    "inside the tolerance"
                    "inside the absolute tolerance"
                    "order ignored when asked"
                ]
                "everything else fails"

            Expect.equal (message r "wrong total") "[Value] is 625, expected 600" "value"

            Expect.equal
                (message r "outside the tolerance")
                "[Value] is 625, expected 600 (tolerance 0.01 or tolerance_abs 5)"
                "tolerance"

            Expect.equal (message r "blank is not zero") "[Value] is BLANK, expected 0" "blank"

            Expect.equal
                (differences r "bounds")
                [
                    "[Value] is 17, expected more than 17"
                    "[Value] is 17, expected less than 10"
                ]
                "bounds"

            Expect.equal (message r "a false assertion") "the assertion is FALSE, not TRUE" "assert"
            Expect.equal (message r "a blank assertion") "the assertion is BLANK, not TRUE" "assert blank"
            let table = differences r "wrong table"
            Expect.contains table "2 rows, expected 3" "row count"

            Expect.isTrue
                (table
                 |> List.exists (fun d -> d.Contains "[Amount]: 140 (int64), expected 141 (int64)"))
                (string table)

            Expect.isTrue (table |> List.exists (fun d -> d.Contains "is missing: [\"FR\", 1]")) (string table)
            Expect.isNonEmpty (differences r "wrong order") "ORDER BY makes the order count"

            Expect.equal
                (differences r "shape")
                [
                    "there is no column Amount (the result has Customer[Country])"
                    "the row count is 2, expected 3 or more"
                    "2 rows, expected none"
                ]
                "shape"

            Expect.stringContains
                (message r "several rows")
                "the result has 2 rows; a column expectation needs exactly one"
                "rows"

            Expect.equal (message r "missing column") "there is no column b (the result has [a])" "column"
            Expect.equal (message r "no error") "expected an error, but the query returned 1 row(s)" "no error"
            Expect.stringStarts (message r "another error") "expected the error 'dax-error', got 'dax-invalid'" "code"
            Expect.equal (message r "a text is not a number") "[Value] is \"625\", expected 625" "text"

            // Each failure is a finding; the text output shows the differences under the test.
            Expect.isTrue (codes r |> List.forall ((=) "TEST_FAIL")) "codes"
            let text = tess [ "test"; "run"; path; "--filter"; "wrong total" ]
            Expect.equal text.Code ExitCode.findings text.Out

            Expect.equal
                (lines text.Out |> List.skip 1 |> List.take 2)
                [ "  FAIL         wrong total"; "               [Value] is 625, expected 600" ]
                "text")

        testCase "errors, unsupported, pending: each its own outcome" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: passes
    measure: Amount
    expect: 625
  - name: errors
    measure: Nope
    expect: 1
  - name: syntax
    query: EVALUATE (
    expect: { rows: 1 }
  - name: two results
    query: EVALUATE Sales EVALUATE Sales
    expect: { rows: 7 }
  - name: unsupported
    measure: Label
    expect: "625"
  - name: unsupported even when an error is expected
    measure: Label
    expect: { error: true }
  - name: a refusal can be expected by its code
    measure: Label
    expect: { error: not-supported }
  - name: pending
    measure: Amount
    pending: true
  - name: skipped
    measure: Amount
    skip: not yet
    expect: 1
"""
            |> ignore

            let r = tess [ "test"; "run"; path; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out

            Expect.equal
                (outcomes r |> List.map snd)
                [
                    "passed"
                    "error"
                    "error"
                    "error"
                    "unsupported"
                    "unsupported"
                    "passed"
                    "skipped"
                    "skipped"
                ]
                "outcomes"

            Expect.equal (summary r) [ 2; 0; 3; 2; 2; 9 ] "summary"
            Expect.equal (testJson r "errors" (fun t -> t.GetProperty("code").GetString())) "dax-invalid" "error code"
            Expect.equal (testJson r "syntax" (fun t -> t.GetProperty("code").GetString())) "dax-syntax" "syntax"
            Expect.stringContains (message r "two results") "2 EVALUATE statements" "one EVALUATE"
            Expect.stringContains (message r "unsupported") "FORMAT" "the function is named"
            Expect.equal (message r "pending") "pending" "pending"
            Expect.equal (message r "skipped") "not yet" "skip reason"

            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "test run" "command"
            Expect.equal (root.GetProperty("source").GetString()) path "source"
            Expect.equal (root.GetProperty("summary").GetProperty("errors").GetInt32()) 3 "error findings"
            Expect.equal (root.GetProperty("summary").GetProperty("warnings").GetInt32()) 2 "warning findings"

            let finding =
                root.GetProperty("findings").EnumerateArray()
                |> Seq.find (fun f -> f.GetProperty("code").GetString() = "TEST_UNSUPPORTED")

            Expect.equal (finding.GetProperty("severity").GetString()) "warning" "unsupported is a warning"
            Expect.equal (finding.GetProperty("objectType").GetString()) "Test" "objectType"
            Expect.equal (finding.GetProperty("object").GetString()) "unsupported" "object"
            Expect.equal (finding.GetProperty("source").GetString()) "test" "source")

        testCase "unsupported tests don't fail a run by default, do with --strict, and never count as tested" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: passes
    measure: Amount
    tags: [core]
    expect: 625
  - name: unsupported
    measure: Label
    tags: [labels]
    expect: "625"
  - name: pending
    measure: Units
    tags: [labels]
    pending: true
"""
            |> ignore

            let r = tess [ "test"; "run"; path ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.stringContains r.Out "  unsupported  unsupported" "shown"
            Expect.stringContains r.Out "3 tests: 1 passed, 1 unsupported, 1 skipped (" "counted"

            Expect.equal (tess [ "test"; "run"; path; "--strict" ]).Code ExitCode.findings "--strict"

            Expect.equal
                (tess [ "test"; "run"; path; "--fail-on"; "warning" ]).Code
                ExitCode.findings
                "--fail-on warning"

            Expect.equal
                (tess [ "test"; "run"; path; "--fail-on"; "info" ]).Code
                ExitCode.usage
                "--fail-on takes two levels"

            // Nothing passed and nothing failed: the run tested nothing, which is not a pass.
            let none = tess [ "test"; "run"; path; "--tag"; "labels" ]
            Expect.equal none.Code ExitCode.findings none.Out
            Expect.stringContains none.Err "no test was run to a verdict" "says so"
            let noneJson = tess [ "test"; "run"; path; "--tag"; "labels"; "--json" ]
            Expect.equal (codes noneJson) [ "TEST_UNSUPPORTED"; "TEST_NONE_RAN" ] "as a finding"

            let nothing = tess [ "test"; "run"; path; "--filter"; "no such test" ]
            Expect.equal nothing.Code ExitCode.findings "a filter that matches nothing")

        testCase "--filter, --tag and --fail-fast" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: amount
    measure: Amount
    tags: [money]
    expect: 625
  - name: wrong units
    measure: Units
    tags: [counts]
    expect: 1
  - name: orders
    measure: Orders
    tags: [counts, Money]
    expect: 7
"""
            |> ignore

            let names (args: string list) =
                outcomes (tess ([ "test"; "run"; path; "--json" ] @ args))

            Expect.equal (names [ "--filter"; "AMOUNT" ] |> List.map fst) [ "amount" ] "by name, ignoring case"

            Expect.equal
                (names [ "--filter"; "money" ] |> List.map fst)
                [ "amount"; "orders" ]
                "--filter also matches tags"

            Expect.equal (names [ "--tag"; "counts" ] |> List.map fst) [ "wrong units"; "orders" ] "--tag"

            Expect.equal
                (names [ "--tag"; "counts"; "--filter"; "o,x" ] |> List.map fst)
                [ "wrong units"; "orders" ]
                "both; lists"

            Expect.equal
                (names [ "--fail-fast" ])
                [ "amount", "passed"; "wrong units", "failed"; "orders", "skipped" ]
                "--fail-fast skips what follows")

        testCase "inline tables stand in for fixture files" (fun () ->
            let t, path = setup ()
            use _ = t

            suite
                t
                "inline.test.yaml"
                """tables:
  Sales:
    - { Id: 1, ProductKey: 1, CustomerKey: 1, OrderDate: 2024-01-01, ShipDate: 2024-01-01, Qty: 2, Price: 10 }
    - { Id: 2, ProductKey: 2, CustomerKey: 1, OrderDate: 2024-01-01, ShipDate: 2024-01-01, Qty: 1, Price: 5 }
  Product:
    - { ProductKey: 1, Name: Bike, Color: Red }
    - { ProductKey: 2, Name: Lock, Color: Black }
tests:
  - name: only the inline rows
    measure: Amount
    expect: 25
  - name: by color
    query: EVALUATE SUMMARIZECOLUMNS ( Product[Color], "Amount", [Amount] )
    expect:
      table:
        - { Color: Red, Amount: 20 }
        - { Color: Black, Amount: 5 }
  - name: a table of the test's own
    measure: Amount
    tables:
      Sales:
        - { Id: 1, ProductKey: 1, Qty: 3, Price: 7 }
    expect: 21
  - name: over the fixture directory
    data: ../data
    tables:
      Sales:
        - { Id: 1, ProductKey: 1, CustomerKey: 2, Qty: 1, Price: 1 }
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount] )
    expect:
      table:
        - { Country: UK, Amount: 1 }
"""
            |> ignore

            let r = tess [ "test"; "run"; path; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.equal (summary r) [ 4; 0; 0; 0; 0; 4 ] r.Out
            // Nothing is left behind in the temp folder.
            Expect.isEmpty
                (Directory.GetDirectories(Path.GetTempPath(), "tessera-test-*"))
                "temporary fixtures are deleted")

        testCase "fixture problems stop the run, naming the suite's line for inline rows" (fun () ->
            let t, path = setup ()
            use _ = t

            let file =
                suite
                    t
                    "bad.test.yaml"
                    """tables:
  Product:
    - { ProductKey: 1, Name: Bike }
    - { ProductKey: one, Name: Lock }
tests:
  - name: t
    measure: Amount
    expect: 1
"""

            let r = tess [ "test"; "run"; path; file; "--json" ]
            Expect.equal r.Code ExitCode.input r.Out
            use doc = json r
            let error = doc.RootElement.GetProperty "error"
            Expect.equal (error.GetProperty("code").GetString()) "invalid-fixture" "code"

            Expect.stringContains
                (error.GetProperty("message").GetString())
                $"{file}:2 (table 'Product', row 2)"
                "where"

            let unknown =
                suite
                    t
                    "unknown.test.yaml"
                    "tables:\n  Nope:\n    - { a: 1 }\ntests:\n  - name: t\n    measure: Amount\n    expect: 1\n"

            let r = tess [ "test"; "run"; path; unknown ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err $"{unknown}:2 (table 'Nope')" "an unknown table"

            // A fixture directory with errors, or that isn't there.
            write (t.Combine("broken", "Product.csv")) "ProductKey,Name\nx,Bike\n"
            let broken = tests t "  - name: t\n    measure: Amount\n    expect: 1\n"
            let r = tess [ "test"; "run"; path; broken; "--data"; t.Combine "broken"; "--json" ]
            Expect.equal r.Code ExitCode.input r.Out
            Expect.stringContains r.Out "fixture-has-errors" "fixture errors"
            let r = tess [ "test"; "run"; path; broken; "--data"; t.Combine "nowhere" ]
            Expect.equal r.Code ExitCode.input r.Err)

        testCase "--data replaces the suites' data; a suite with no data at all is an error per test" (fun () ->
            let t, path = setup ()
            use _ = t

            let file =
                suite t "nodata.test.yaml" "tests:\n  - name: t\n    measure: Amount\n    expect: 625\n"

            let r = tess [ "test"; "run"; path; file; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out
            Expect.equal (outcomes r) [ "t", "error" ] "no data"
            Expect.stringContains (message r "t") "no fixture data" "says what to do"

            let r = tess [ "test"; "run"; path; file; "--data"; t.Combine "data"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out

            // Another fixture: the same suite, other numbers.
            write (t.Combine("other", "Sales.csv")) "Id,Qty,Price\n1,2,3\n"
            let other = tests t "  - name: t\n    measure: Amount\n    expect: 6\n"

            Expect.equal
                (tess [ "test"; "run"; path; other; "--data"; t.Combine "other" ]).Code
                ExitCode.ok
                "--data wins over the suite's")

        testCase "an invalid suite stops everything, with file and line" (fun () ->
            let t, path = setup ()
            use _ = t
            tests t "  - name: fine\n    measure: Amount\n    expect: 625\n" |> ignore

            let broken =
                suite t "broken.test.yaml" "tests:\n  - name: t\n    measure: Amount\n    expcet: 625\n"

            let r = tess [ "test"; "run"; path ]
            Expect.equal r.Code ExitCode.findings r.Out

            Expect.stringContains
                r.Out
                $"{broken}:4:5: unknown key 'expcet' in a test (did you mean 'expect'?)"
                "where"

            Expect.stringContains r.Out "1 suite is not valid: nothing was run." "nothing ran"
            Expect.isFalse (r.Out.Contains "pass ") "the valid suite isn't run either"

            let j = tess [ "test"; "run"; path; "--json"; "--ci"; "github" ]
            Expect.equal j.Code ExitCode.findings j.Out
            Expect.equal (codes j) [ "TEST_SUITE_INVALID" ] "finding"
            use doc = json j
            let invalid = doc.RootElement.GetProperty("invalidSuites").[0]
            Expect.equal (invalid.GetProperty("file").GetString()) broken "file"
            Expect.stringContains (invalid.GetProperty("validationErrors").[0].GetString()) ":4:5: unknown key" "error"
            Expect.equal (doc.RootElement.GetProperty("testSummary").GetProperty("invalidSuites").GetInt32()) 1 "count"
            Expect.stringStarts j.Err "::error title=TEST_SUITE_INVALID::" "annotation")

        testCase "where suites are looked for; none is an error" (fun () ->
            let t, path = setup ()
            use _ = t
            let none = tess [ "test"; "run"; path; "--json" ]
            Expect.equal none.Code ExitCode.input none.Out
            Expect.stringContains none.Out "no-test-suites" "no suites"
            Expect.stringContains none.Out (t.Combine ".tess-tests") "says where it looked"

            let missing = tess [ "test"; "run"; path; t.Combine "nope.test.yaml" ]
            Expect.equal missing.Code ExitCode.input missing.Err

            Directory.CreateDirectory(t.Combine "empty") |> ignore

            Expect.equal
                (tess [ "test"; "run"; path; t.Combine "empty" ]).Code
                ExitCode.input
                "a directory with no suites"

            // Suites elsewhere: a directory (searched recursively) and a file, relative data included.
            write
                (t.Combine("suites", "deep", "a.test.yaml"))
                "data: ../../data\ntests:\n  - name: a\n    measure: Amount\n    expect: 625\n"

            write
                (t.Combine("suites", "b.test.json"))
                """{ "data": "../data", "tests": [ { "name": "b", "measure": "Units", "expect": 17 } ] }"""

            let r = tess [ "test"; "run"; path; t.Combine "suites"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.equal (outcomes r) [ "b", "passed"; "a", "passed" ] "both ran"
            Expect.equal (tess [ "test"; "run" ]).Code ExitCode.usage "no model")

        testCase "--ci, --trx and --junit" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: passes
    measure: Amount
    expect: 625
  - name: fails
    measure: Units
    expect: 1
  - name: errors
    measure: Nope
    expect: 1
  - name: unsupported
    measure: Label
    expect: "1"
  - name: pending
    measure: Units
    pending: true
"""
            |> ignore

            let trx = t.Combine("out", "results.trx")
            let junit = t.Combine "results.xml"

            let r =
                tess [ "test"; "run"; path; "--ci"; "github"; "--trx"; trx; "--junit"; junit ]

            Expect.equal r.Code ExitCode.findings r.Out

            Expect.equal
                (lines r.Err |> List.map (fun l -> l.Substring(0, l.IndexOf("::", 2))))
                [
                    "::error title=TEST_FAIL"
                    "::error title=TEST_ERROR"
                    "::warning title=TEST_UNSUPPORTED"
                ]
                "annotations"

            let azdo = tess [ "test"; "run"; path; "--ci"; "azdo" ]

            Expect.stringContains
                azdo.Err
                "##vso[task.logissue type=error;code=TEST_FAIL][Value] is 17, expected 1"
                "azdo"

            Expect.equal (tess [ "test"; "run"; path; "--ci"; "jenkins" ]).Code ExitCode.usage "unknown --ci"

            let doc = XDocument.Load trx
            let ns = XNamespace.Get "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"

            Expect.equal
                [
                    for e in doc.Descendants(ns + "UnitTestResult") ->
                        e.Attribute(XName.Get "testName").Value, e.Attribute(XName.Get "outcome").Value
                ]
                [
                    "passes", "Passed"
                    "fails", "Failed"
                    "errors", "Error"
                    "unsupported", "NotExecuted"
                    "pending", "NotExecuted"
                ]
                "TRX results"

            let counters = doc.Descendants(ns + "Counters") |> Seq.head
            Expect.equal (counters.Attribute(XName.Get "total").Value) "5" "total"
            Expect.equal (counters.Attribute(XName.Get "failed").Value) "1" "failed"

            Expect.equal
                ((doc.Descendants(ns + "ResultSummary") |> Seq.head).Attribute(XName.Get "outcome").Value)
                "Failed"
                "outcome"

            Expect.stringContains
                ((doc.Descendants(ns + "Message") |> Seq.head).Value)
                "[Value] is 17, expected 1"
                "message"

            // Every result points at a definition.
            let definitions =
                doc.Descendants(ns + "UnitTest")
                |> Seq.map (fun e -> e.Attribute(XName.Get "id").Value)
                |> Set.ofSeq

            Expect.equal definitions.Count 5 "definitions"

            Expect.isTrue
                (doc.Descendants(ns + "UnitTestResult")
                 |> Seq.forall (fun e -> definitions.Contains(e.Attribute(XName.Get "testId").Value)))
                "ids"

            let j = XDocument.Load junit
            let root = j.Root
            Expect.equal root.Name.LocalName "testsuites" "root"

            Expect.equal
                [
                    for a in [ "tests"; "failures"; "errors"; "skipped" ] -> root.Attribute(XName.Get a).Value
                ]
                [ "5"; "1"; "1"; "2" ]
                "JUnit counts"

            Expect.equal
                [
                    for c in j.Descendants(XName.Get "testcase") ->
                        c.Elements() |> Seq.map (fun e -> e.Name.LocalName) |> Seq.tryHead
                ]
                [ None; Some "failure"; Some "error"; Some "skipped"; Some "skipped" ]
                "JUnit cases"

            // A report replaces a report, but never another kind of file without --force.
            Expect.equal
                (tess [ "test"; "run"; path; "--trx"; trx; "--junit"; junit ]).Code
                ExitCode.findings
                "written again"

            let precious = tess [ "test"; "run"; path; "--trx"; path ]
            Expect.equal precious.Code ExitCode.output precious.Err
            Expect.stringContains (File.ReadAllText path) "\"model\"" "the model is untouched"
            let other = t.Combine "notes.txt"
            File.WriteAllText(other, "notes")
            Expect.equal (tess [ "test"; "run"; path; "--junit"; other ]).Code ExitCode.output "refused"
            Expect.equal (tess [ "test"; "run"; path; "--junit"; other; "--force" ]).Code ExitCode.findings "--force"
            Expect.stringContains (File.ReadAllText other) "<testsuites" "replaced"

            // With --strict an unsupported test is a failure in the reports too.
            tess [ "test"; "run"; path; "--strict"; "--trx"; trx ] |> ignore

            Expect.equal
                (XDocument.Load(trx).Descendants(ns + "UnitTestResult")
                 |> Seq.map (fun e -> e.Attribute(XName.Get "outcome").Value)
                 |> Seq.item 3)
                "Failed"
                "strict")

        testCase "snapshots: a dry run, a recording, a regression, a refresh" (fun () ->
            let t, path = setup ()
            use _ = t

            let file =
                tests
                    t
                    """  - name: total
    measure: Amount
    expect: { snapshot: true }
  - name: by country
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Amount", [Amount], "Share", DIVIDE ( [Amount], 625 ) )
    expect: { snapshot: true }
  - name: by year, ordered
    query: EVALUATE SUMMARIZECOLUMNS ( 'Date'[Year], "Orders", [Orders] ) ORDER BY 'Date'[Year] DESC
    expect: { snapshot: true }
  - name: not a snapshot
    measure: Units
    expect: 17
  - name: cannot be recorded
    measure: Label
    expect: { snapshot: true }
"""

            let snapshots = t.Combine(".tess-tests", "s.snapshot.json")

            // Nothing recorded yet: the tests fail, saying how to record.
            let before = tess [ "test"; "run"; path; "--json" ]
            Expect.equal before.Code ExitCode.findings before.Out
            Expect.stringContains (message before "total") "tess test snapshot" "how to record"

            // A dry run by default.
            let dry = tess [ "test"; "snapshot"; path ]
            Expect.equal dry.Code ExitCode.findings dry.Out
            Expect.isFalse (File.Exists snapshots) "a dry run writes nothing"

            Expect.equal
                (lines dry.Out |> List.take 6)
                [
                    snapshots
                    "  + total"
                    "  + by country"
                    "  + by year, ordered"
                    "  ! cannot be recorded"
                    "      not recorded (unsupported): Not supported by the local DAX engine: the function FORMAT (measure 'Sales'[Label] 1:1)"
                ]
                "what would change"

            Expect.stringContains dry.Out "Dry run: 3 snapshots would change" "dry run"
            Expect.stringContains dry.Err "1 snapshot could not be recorded" "the one that can't"

            let saved = tess [ "test"; "snapshot"; path; "--save" ]
            Expect.equal saved.Code ExitCode.findings "still one that can't be recorded"
            let text = File.ReadAllText snapshots
            Expect.isFalse (text.Contains "\r") "LF line ends"
            Expect.stringContains text "\"by country\": {" "by name"
            Expect.isFalse (text.Contains "cannot be recorded") "an unsupported test is never recorded"
            // Unordered rows are sorted; ordered ones keep their order.
            Expect.isLessThan (text.IndexOf "[\"UK\"") (text.IndexOf "[\"US\"") "sorted"
            Expect.isLessThan (text.IndexOf "[2024,6]") (text.IndexOf "[2023,1]") "ORDER BY kept"

            // Recording again changes nothing, byte for byte.
            let again = tess [ "test"; "snapshot"; path; "--save"; "--json" ]
            use doc = json again

            Expect.equal
                (doc.RootElement.GetProperty("files").[0].GetProperty("changed").GetBoolean())
                false
                "unchanged"

            Expect.equal (doc.RootElement.GetProperty("summary").GetProperty("unchanged").GetInt32()) 3 "unchanged"
            Expect.equal (File.ReadAllText snapshots) text "the same bytes"

            // Now the tests pass (the one that can't be recorded is unsupported, a warning).
            let run = tess [ "test"; "run"; path; "--json" ]
            Expect.equal run.Code ExitCode.ok run.Out
            Expect.equal (summary run) [ 4; 0; 0; 1; 0; 5 ] "summary"

            // The data changes: the snapshot tests fail with the difference, and `snapshot` shows it.
            File.AppendAllText(t.Combine("data", "Sales.csv"), "8,1,2,2024-02-01,2024-02-01,1,5\n")
            let regression = tess [ "test"; "run"; path; "--json" ]
            Expect.equal regression.Code ExitCode.findings regression.Out
            Expect.equal (message regression "total") "row 0, [Value]: 630 (int64), expected 625 (int64)" "difference"

            Expect.equal
                (outcomes regression |> List.map snd)
                [ "failed"; "failed"; "failed"; "failed"; "unsupported" ]
                "outcomes"

            let changed = tess [ "test"; "snapshot"; path; "--filter"; "total" ]
            Expect.equal changed.Code ExitCode.ok changed.Out

            Expect.equal
                (lines changed.Out |> List.take 3)
                [
                    snapshots
                    "  ~ total"
                    "      row 0, [Value]: 630 (int64), expected 625 (int64)"
                ]
                "changed"

            Expect.equal (File.ReadAllText snapshots) text "still a dry run"

            // Refresh one; the others stay as recorded.
            Expect.equal (tess [ "test"; "snapshot"; path; "--filter"; "total"; "--save" ]).Code ExitCode.ok "saved"
            let after = tess [ "test"; "run"; path; "--json" ]

            Expect.equal
                (outcomes after |> List.map snd)
                [ "passed"; "failed"; "failed"; "failed"; "unsupported" ]
                "only the refreshed one"

            // A test that is gone loses its snapshot.
            File.WriteAllText(
                file,
                "data: ../data\ntests:\n  - name: total\n    measure: Amount\n    expect: { snapshot: true }\n"
            )

            let removed = tess [ "test"; "snapshot"; path; "--save" ]
            Expect.equal removed.Code ExitCode.ok removed.Out
            Expect.stringContains removed.Out "  - by country" "removed"
            Expect.isFalse ((File.ReadAllText snapshots).Contains "by country") "gone from the file"

            // A snapshot file that can't be read stops the run.
            File.WriteAllText(snapshots, "{ nope")
            Expect.equal (tess [ "test"; "run"; path ]).Code ExitCode.input "unreadable")

        testCase "a snapshot with a tolerance compares numbers loosely" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                """  - name: exact
    measure: Amount
    expect: { snapshot: true }
  - name: loose
    measure: Amount
    tolerance: 0.01
    expect: { snapshot: true }
"""
            |> ignore

            Expect.equal (tess [ "test"; "snapshot"; path; "--save" ]).Code ExitCode.ok "recorded"
            File.AppendAllText(t.Combine("data", "Sales.csv"), "8,1,2,2024-02-01,2024-02-01,1,5\n")
            let r = tess [ "test"; "run"; path; "--json" ]
            Expect.equal (outcomes r) [ "exact", "failed"; "loose", "passed" ] "tolerance")

        testCase "init writes a suite that reads back, and never overwrites" (fun () ->
            let odd = {
                star with
                    Measures =
                        star.Measures
                        @ [
                            "Sales", "Margin [%]: #1", "1"
                            "Sales", "\"quoted\" 'name'", "2"
                            "Sales", "  spaced & - yes", "3"
                            "Sales", "Año {x}", "4"
                        ]
            }

            let t, path = setupWith odd
            use _ = t
            let r = tess [ "test"; "init"; path; "--data"; t.Combine "data" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let file = t.Combine(".tess-tests", "measures.test.yaml")
            Expect.equal r.Out $"Wrote {file}: 7 tests.\n" "says what it wrote"

            let suite =
                match TestSuite.load file with
                | Ok s -> s
                | Error issues -> failtestf "%A" issues

            Expect.equal suite.Name "Test measures" "name"
            Expect.equal suite.Data (Some(t.Combine "data")) "data, relative to the suite"

            Expect.equal
                (suite.Tests |> List.map _.Name)
                [
                    "Amount"
                    "Units"
                    "Orders"
                    "Margin [%]: #1"
                    "\"quoted\" 'name'"
                    "  spaced & - yes"
                    "Año {x}"
                ]
                "a test per measure, whatever its name"

            Expect.equal suite.Tests.[3].Body (TestBody.Scalar "[Margin [%]]: #1]") "the measure's reference"
            Expect.isTrue (suite.Tests |> List.forall (fun t -> t.Skip = Some "pending")) "pending"

            // Pending tests are not a pass: the scaffold fails until someone fills it in.
            let pending = tess [ "test"; "run"; path ]
            Expect.equal pending.Code ExitCode.findings pending.Out
            Expect.stringContains pending.Out "7 tests: 7 skipped" "all pending"

            let before = File.ReadAllText file
            let again = tess [ "test"; "init"; path; "--json" ]
            Expect.equal again.Code ExitCode.output again.Out
            Expect.stringContains again.Out "never overwrites" "refuses"
            Expect.equal (File.ReadAllText file) before "untouched"

            // --snapshot: tests to record instead; the odd names work end to end.
            let other = t.Combine("elsewhere", "snap.test.yaml")

            let s =
                tess [
                    "test"
                    "init"
                    path
                    "-o"
                    other
                    "--snapshot"
                    "--data"
                    t.Combine "data"
                    "--json"
                ]

            Expect.equal s.Code ExitCode.ok s.Out
            Expect.equal (tess [ "test"; "snapshot"; path; other; "--save" ]).Code ExitCode.ok "recorded"
            let run = tess [ "test"; "run"; path; other; "--json" ]
            Expect.equal run.Code ExitCode.ok run.Out
            Expect.equal (summary run) [ 7; 0; 0; 0; 0; 7 ] "all pass"

            Expect.equal
                (tess [ "test"; "init"; path; "-o"; t.Combine "x.yaml" ]).Code
                ExitCode.usage
                "the name must be a suite's")

        testCase "list checks suites without a model" (fun () ->
            let t, _ = setup ()
            use _ = t

            let file =
                tests
                    t
                    "  - name: a\n    measure: Amount\n    tags: [x]\n    expect: 1\n  - name: b\n    measure: Units\n    pending: true\n"

            let r = tess [ "test"; "list"; file ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (lines r.Out) [ $"S  ({file})"; "  a  [x]"; "  b  (pending)" ] "listed"
            let j = tess [ "test"; "list"; t.Combine ".tess-tests"; "--json" ]
            use doc = json j
            let first = doc.RootElement.GetProperty("suites").[0].GetProperty("tests").[0]

            Expect.equal
                (first.GetProperty("query").GetString())
                "EVALUATE ROW ( \"Value\",\n[Amount]\n)"
                "the query it runs as"

            File.WriteAllText(file, "tests: 1\n")
            Expect.equal (tess [ "test"; "list"; file ]).Code ExitCode.findings "an invalid suite")

        testCase "the same suite against a server model" (fun () ->
            let t, _ = setup ()
            use _ = t
            let server = engineServer ()

            let file =
                tests
                    t
                    """  - name: total
    measure: Amount
    expect: 625
  - name: filtered
    measure: Amount
    filters:
      Date[Year]: 2024
    expect: 525
  - name: wrong
    measure: Units
    expect: 1
  - name: a server error can be expected
    measure: Nope
    expect: { error: query-failed }
  - name: a server error
    measure: Nope
    expect: 1
  - name: own data
    measure: Amount
    tables:
      Sales:
        - { Id: 1, Qty: 1, Price: 1 }
    expect: 1
"""

            let r = tessOn server tokenEnv [ "test"; "run"; remote; file; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out

            Expect.equal
                (outcomes r |> List.map snd)
                [ "passed"; "passed"; "failed"; "passed"; "error"; "skipped" ]
                "outcomes"

            Expect.equal
                (testJson r "a server error" (fun t -> t.GetProperty("code").GetString()))
                "query-failed"
                "code"

            Expect.stringContains (message r "own data") "a server model is tested on the data it holds" "why skipped"
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("source").GetString()) $"xmla:{workspace};Test" "source"

            // One connection for the whole run; the queries are the ones a local run evaluates.
            Expect.equal server.Requests.Length 1 "one connection"
            Expect.equal server.Closed 1 "closed"

            Expect.equal
                (server.Queries
                 |> List.map (fun (db, dax, limit) -> db, dax, limit)
                 |> List.item 1)
                ("Test",
                 "EVALUATE ROW ( \"Value\",\nCALCULATE (\n[Amount]\n, TREATAS ( { 2024 }, Date[Year] ) )\n)",
                 None)
                "the query sent"

            Expect.equal server.Queries.Length 5 "the skipped test is not sent"

            let usage =
                tessOn server tokenEnv [ "test"; "run"; remote; file; "--data"; t.Combine "data" ]

            Expect.equal usage.Code ExitCode.usage "--data is for model files"

            // A server that can't be reached stops the run with the connection's exit code.
            let down = FakeServer()
            down.OpenError <- Some(exn "No such host is known")
            let r = tessOn down tokenEnv [ "test"; "run"; remote; file ]
            Expect.equal r.Code ExitCode.remote r.Err

            // An invalid suite is reported before any connection is made.
            let broken = suite t "broken.test.yaml" "tests: 1\n"
            let quiet = FakeServer()
            let r = tessOn quiet tokenEnv [ "test"; "run"; remote; broken ]
            Expect.equal r.Code ExitCode.findings r.Out
            Expect.isEmpty quiet.Requests "no connection")

        testCase "compare: two versions of a model, and model files against the deployed model" (fun () ->
            let t, path = setupWith star
            use _ = t

            let changed = {
                star with
                    Measures = [
                        "Sales", "Amount", "SUMX ( Sales, Sales[Qty] * Sales[Price] ) + 0.001"
                        "Sales", "Units", "SUM ( Sales[Qty] ) + 0"
                        "Sales", "Orders", "COUNTROWS ( Sales ) * 1.0"
                    ]
            }

            let db, _ = build changed
            let other = t.Combine "changed.bim"
            File.WriteAllText(other, JsonSerializer.SerializeDatabase db)

            let file =
                tests
                    t
                    """  - name: amount
    measure: Amount
    expect: 1
  - name: units
    measure: Units
    expect: 1
  - name: orders
    measure: Orders
    expect: 1
  - name: by country
    query: EVALUATE SUMMARIZECOLUMNS ( Customer[Country], "Units", [Units] )
    expect: { rows: 1 }
  - name: fails on both
    measure: Nope
    expect: { error: true }
  - name: pending
    measure: Amount
    pending: true
"""

            // The tests' own expectations (all wrong here) play no part: only the two results do.
            let r = tess [ "test"; "compare"; path; "--against"; other; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out

            Expect.equal
                (outcomes r |> List.map snd)
                [ "failed"; "passed"; "passed"; "passed"; "passed"; "skipped" ]
                "only the changed value differs; a type alone is not a difference"

            Expect.equal (message r "amount") "row 0, [Value]: 625.001 (double), expected 625 (int64)" "difference"
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("command").GetString()) "test compare" "command"
            Expect.equal (doc.RootElement.GetProperty("against").GetString()) other "against"

            let types =
                tess [ "test"; "compare"; path; "--against"; other; "--types"; "--json" ]

            Expect.equal (outcomes types |> List.map snd |> List.take 3) [ "failed"; "passed"; "failed" ] "--types"
            Expect.isTrue (differences types "orders" |> List.exists (fun d -> d.Contains "(type only)")) "type only"

            let loose =
                tess [ "test"; "compare"; path; "--against"; other; "--tolerance"; "0.001" ]

            Expect.equal loose.Code ExitCode.ok loose.Out

            Expect.equal
                (tess [ "test"; "compare"; path; "--against"; other; "--tolerance-abs"; "0.01" ]).Code
                ExitCode.ok
                "absolute"

            Expect.equal
                (tess [ "test"; "compare"; path; "--against"; other; "--tolerance"; "-1" ]).Code
                ExitCode.usage
                "negative"

            Expect.equal (tess [ "test"; "compare"; path ]).Code ExitCode.usage "--against is needed"

            // The other side over another fixture.
            write (t.Combine("other", "Sales.csv")) "Id,Qty,Price\n1,2,3\n"

            let data =
                tess [
                    "test"
                    "compare"
                    path
                    "--against"
                    path
                    "--against-data"
                    t.Combine "other"
                    "--json"
                ]

            Expect.equal (message data "units") "row 0, [Value]: 2 (int64), expected 17 (int64)" "--against-data"

            // Model files against the deployed model: the local engine against the server.
            let server = engineServer ()

            let same =
                tessOn server tokenEnv [ "test"; "compare"; path; "--against"; remote; file; "--json" ]

            Expect.equal same.Code ExitCode.ok same.Out
            Expect.equal (summary same) [ 5; 0; 0; 0; 1; 6 ] "the same everywhere"
            Expect.equal server.Requests.Length 1 "one connection"

            server.OnQuery <-
                fun dax ->
                    if dax.Contains "[Units]" then
                        raise (XmlaQueryError "boom")
                    else
                        [
                            {
                                Columns = [
                                    {
                                        Name = "[Value]"
                                        Type = QueryType.Int64
                                    }
                                ]
                                Rows = [ [| QueryValue.Int64 1L |] ]
                                Truncated = false
                            }
                        ]

            let differs =
                tessOn server tokenEnv [ "test"; "compare"; path; "--against"; remote; file; "--json" ]

            Expect.equal differs.Code ExitCode.findings differs.Out

            Expect.equal
                (message differs "amount")
                "row 0, [Value]: 1 (int64), expected 625 (int64)"
                "the server's value"

            Expect.stringContains
                (message differs "units")
                "fails on the model compared against but not on the model: query-failed: Query failed: boom"
                "one side fails"

            Expect.stringContains
                (message differs "fails on both")
                "fails on the model but not on the one compared against"
                "the other way")

        testCase "compare treats an unsupported side as unsupported, not as a difference" (fun () ->
            let t, path = setup ()
            use _ = t

            tests
                t
                "  - name: label\n    measure: Label\n    expect: 1\n  - name: amount\n    measure: Amount\n    expect: 1\n"
            |> ignore

            let r = tess [ "test"; "compare"; path; "--against"; path; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            Expect.equal (outcomes r) [ "label", "unsupported"; "amount", "passed" ] "outcomes"

            Expect.equal
                (tess [ "test"; "compare"; path; "--against"; path; "--strict" ]).Code
                ExitCode.findings
                "--strict")
    ]
