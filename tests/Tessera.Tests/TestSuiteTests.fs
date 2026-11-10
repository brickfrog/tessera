/// The `tess test` suite format: reading YAML and JSON suites, with the line of every problem,
/// and the DAX query each test compiles to.
module Tessera.Tests.TestSuiteTests

open System
open System.IO
open Expecto
open Tessera
open Tessera.Tests.Fixtures

let private file = Path.Combine(Path.GetTempPath(), "tessera-tests", "suite.test.yaml")
let private jsonFile = Path.Combine(Path.GetTempPath(), "tessera-tests", "suite.test.json")

let private parse (yaml: string) = TestSuite.parse file yaml

let private good (yaml: string) =
    match parse yaml with
    | Ok suite -> suite
    | Error issues -> failtestf "expected a valid suite, got:\n%s" (issues |> List.map SuiteIssue.format |> String.concat "\n")

/// The problems of a suite, as (line, message).
let private bad (yaml: string) =
    match parse yaml with
    | Ok _ -> failtest "expected the suite to be refused"
    | Error issues -> issues |> List.map (fun i -> i.Line, i.Message)

let private one (yaml: string) =
    match bad yaml with
    | [ issue ] -> issue
    | issues -> failtestf "expected one problem, got %A" issues

let private only (suite: TestSuiteFile) =
    match suite.Tests with
    | [ t ] -> t
    | tests -> failtestf "expected one test, got %d" tests.Length

let private value (t: TestCase) =
    match t.Expect.Values with
    | [ "Value", e ] -> e
    | other -> failtestf "expected one Value expectation, got %A" other

let private compiled (yaml: string) =
    (good yaml).Tests |> List.map TestRun.compile

[<Tests>]
let tests =
    testList "test suites" [
        testCase "a suite in the Tabular Editor CLI's format" (fun () ->
            let suite =
                good
                    """suite: Revenue regression
description: Guards the core revenue measures
connection:
  server: MyWorkspace
  database: Sales Model
defaults:
  tolerance: 0.01
  tags: [revenue]
tests:
  - name: FY25 revenue matches finance sign-off
    query: |
      EVALUATE ROW("Revenue", CALCULATE([Total Revenue], 'Date'[FY] = "FY25"))
    expect:
      Revenue:
        value: 12500000
        tolerance: 0.001
  - name: No orphan sales rows
    query: EVALUATE ROW("Orphans", 0)
    tags: [integrity]
    expect:
      Orphans: 0
      rows: 1
      empty: false
      columns: [Orphans]
"""

            Expect.equal suite.Name "Revenue regression" "name"
            Expect.equal suite.Description (Some "Guards the core revenue measures") "description"
            Expect.equal (suite.Tests |> List.map _.Name) [ "FY25 revenue matches finance sign-off"; "No orphan sales rows" ] "tests"
            let first = suite.Tests.[0]
            Expect.equal first.Tags [ "revenue" ] "default tags"
            Expect.equal first.Line 10 "the test's line"

            match first.Body with
            | TestBody.Query q -> Expect.stringContains q "CALCULATE([Total Revenue]" "query"
            | other -> failtestf "expected a query, got %A" other

            let name, revenue = first.Expect.Values.[0]
            Expect.equal name "Revenue" "column"
            Expect.equal revenue.Value (Some(SuiteValue.Int 12500000L)) "value"
            Expect.equal revenue.Tolerance { Relative = Some 0.001; Absolute = None } "the column's tolerance"

            let second = suite.Tests.[1]
            Expect.equal second.Tags [ "revenue"; "integrity" ] "tags add to the defaults"
            Expect.equal (snd second.Expect.Values.[0]).Tolerance { Relative = Some 0.01; Absolute = None } "inherited"
            Expect.equal second.Expect.Rows (Some(RowsExpect.Exactly 1)) "rows"
            Expect.equal second.Expect.Empty (Some false) "empty"
            Expect.equal second.Expect.Columns (Some [ "Orphans" ]) "columns"

            // te's `connection` is read and set aside, with a note saying so.
            Expect.equal (suite.Notes |> List.map _.Line) [ 3 ] "note"
            Expect.stringContains suite.Notes.[0].Message "connection" "note")

        testCase "the shorthands: measure, expression, assert, pending" (fun () ->
            let suite =
                good
                    """tests:
  - name: a measure
    measure: Total [EUR]
    expect: 12.5
  - name: an expression
    expression: "[A] + [B]"
    expect: { gte: 0, lt: 10, tolerance_abs: 0.5 }
  - name: an assertion
    assert: "[A] = [B]"
  - name: to be written
    measure: X
    pending: true
  - name: blank
    measure: X
    expect: null
  - name: skipped
    measure: X
    skip: waiting for the new fixture
    expect: 1
"""

            Expect.equal suite.Name "suite" "a suite without a name is named after its file"
            let t = suite.Tests
            Expect.equal t.[0].Body (TestBody.Scalar "[Total [EUR]]]") "a measure name is escaped"
            Expect.equal (value t.[0]).Value (Some(SuiteValue.Float 12.5)) "value"
            Expect.equal t.[1].Body (TestBody.Scalar "[A] + [B]") "expression"
            Expect.equal (value t.[1]).Bounds.Gte (Some 0.0) "gte"
            Expect.equal (value t.[1]).Bounds.Lt (Some 10.0) "lt"
            Expect.equal (value t.[1]).Tolerance { Relative = None; Absolute = Some 0.5 } "tolerance_abs"
            Expect.equal t.[2].Body (TestBody.Assert "[A] = [B]") "assert"
            Expect.equal t.[3].Skip (Some "pending") "pending"
            Expect.equal (value t.[4]).Value (Some SuiteValue.Null) "null expects BLANK"
            Expect.equal t.[5].Skip (Some "waiting for the new fixture") "skip with a reason")

        testCase "values keep their YAML types: only unquoted scalars are numbers, booleans, dates" (fun () ->
            let suite =
                good
                    """tests:
  - name: t
    query: EVALUATE x
    expect:
      table:
        - { a: 12, b: "12", c: 1.5, d: true, e: 2024-01-31, f: "2024-01-31", g: ~, h: text, i: 1e3, j: 2024-01-31T13:05:00 }
"""

            match (only suite).Expect.Table with
            | Some(TableExpect.Rows(columns, [ row ])) ->
                Expect.equal columns [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] "columns"

                Expect.equal
                    row
                    [
                        SuiteValue.Int 12L
                        SuiteValue.Text "12"
                        SuiteValue.Float 1.5
                        SuiteValue.Bool true
                        SuiteValue.Date(DateTime(2024, 1, 31))
                        SuiteValue.Text "2024-01-31"
                        SuiteValue.Null
                        SuiteValue.Text "text"
                        SuiteValue.Float 1000.0
                        SuiteValue.Date(DateTime(2024, 1, 31, 13, 5, 0))
                    ]
                    "values"
            | other -> failtestf "expected one row, got %A" other)

        testCase "fixture data, filters and the expected table's file" (fun () ->
            use t = new TempDir()
            write (t.Combine "expected.csv") "a\n1\n"
            write (t.Combine("queries", "q.dax")) "EVALUATE Sales"

            let suite =
                match
                    TestSuite.parse
                        (t.Combine "s.test.yaml")
                        """data: ../fixtures
tables:
  Sales:
    - { Id: 1, Amount: 2.5, When: 2024-01-31, Note: "a \"b\"" }
  Date: []
defaults:
  filters:
    Date[Year]: 2024
tests:
  - name: file
    query_file: queries/q.dax
    filters:
      "'Product'[Color]": [Red, "Blue"]
      Sales[When]: { dax: "DATE ( 2024, 1, 31 )" }
    tables:
      date:
        - { Year: 2024 }
    data: own
    expect: { file: expected.csv, ordered: true }
"""
                with
                | Ok s -> s
                | Error issues -> failtestf "%A" issues

            Expect.equal suite.Data (Some(Path.GetFullPath(t.Combine("..", "fixtures")))) "suite data, relative to the suite"
            let test = only suite
            Expect.equal test.Data (Some(t.Combine "own")) "the test's own data"
            Expect.equal test.Body (TestBody.Query "EVALUATE Sales") "query_file is read"
            Expect.equal (test.Tables |> List.map _.Table) [ "Sales"; "date" ] "the test's table replaces the suite's Date"
            Expect.equal test.Tables.[0].Line 3 "table line"

            Expect.equal
                test.Tables.[0].Json
                "[\n{ \"Id\": 1, \"Amount\": 2.5, \"When\": \"2024-01-31\", \"Note\": \"a \\u0022b\\u0022\" }\n]\n"
                "rows as fixture JSON"

            Expect.equal
                test.Filters
                [
                    {
                        Column = "Date[Year]"
                        Values = [ FilterValue.Literal(SuiteValue.Int 2024L) ]
                    }
                    {
                        Column = "'Product'[Color]"
                        Values = [ FilterValue.Literal(SuiteValue.Text "Red"); FilterValue.Literal(SuiteValue.Text "Blue") ]
                    }
                    {
                        Column = "Sales[When]"
                        Values = [ FilterValue.Dax "DATE ( 2024, 1, 31 )" ]
                    }
                ]
                "filters: the defaults', then the test's"

            Expect.equal test.Expect.Table (Some(TableExpect.File(t.Combine "expected.csv"))) "file"
            Expect.equal test.Expect.Ordered (Some true) "ordered")

        testList "problems carry their line" [
            let cases = [
                "an unknown key, with a hint",
                """tests:
  - name: t
    measure: X
    expcet: 1
""",
                4,
                "unknown key 'expcet' in a test"

                "a hint for a misspelt key",
                """tests:
  - name: t
    queryFile: x.dax
    expect: 1
""",
                3,
                "did you mean 'query_file'?"

                "an unknown top-level key",
                """suite: x
test:
  - name: t
""",
                2,
                "unknown key 'test' in a suite"

                "no expect",
                """tests:

  - name: t
    measure: X
""",
                3,
                "has no `expect`"

                "nothing to evaluate",
                """tests:
  - name: t
    expect: 1
""",
                2,
                "needs one of query, query_file, measure, expression, assert"

                "two things to evaluate",
                """tests:
  - name: t
    measure: X
    query: EVALUATE x
    expect: 1
""",
                4,
                "more than one of"

                "no name",
                """tests:
  - measure: X
    expect: 1
""",
                2,
                "a test needs a name"

                "an expectation that asserts nothing",
                """tests:
  - name: t
    query: EVALUATE x
    expect: {}
""",
                4,
                "expect has no assertion"

                "an empty expected value",
                """tests:
  - name: t
    measure: X
    expect:
""",
                4,
                "write null to expect BLANK"

                "an error and a value",
                """tests:
  - name: t
    query: EVALUATE x
    expect:
      error: true
      rows: 1
""",
                5,
                "can't expect anything else"

                "an assert with an expect",
                """tests:
  - name: t
    assert: "1 = 1"
    expect: true
""",
                4,
                "an `assert` test has no `expect`"

                "a filter that isn't on a column",
                """tests:
  - name: t
    measure: X
    filters:
      Year: 2024
    expect: 1
""",
                5,
                "a filter is on a column"

                "a filter with no values",
                """tests:
  - name: t
    measure: X
    filters:
      Date[Year]: []
    expect: 1
""",
                5,
                "has no values"

                "a negative tolerance",
                """tests:
  - name: t
    measure: X
    tolerance: -1
    expect: 1
""",
                4,
                "tolerance must be 0 or more"

                "a missing query file",
                """tests:
  - name: t
    query_file: nope.dax
    expect: { rows: 1 }
""",
                3,
                "does not exist"

                "a missing expected file",
                """tests:
  - name: t
    query: EVALUATE x
    expect: { file: nope.csv }
""",
                4,
                "does not exist"

                "rows that aren't a count",
                """tests:
  - name: t
    query: EVALUATE x
    expect:
      rows: many
""",
                5,
                "rows must be a whole number"

                "tests that aren't a list",
                """tests:
  name: t
""",
                2,
                "tests must be a list"

                "no tests",
                "suite: x\n",
                1,
                "a suite needs `tests`"

                "an empty list of tests",
                "suite: x\ntests: []\n",
                2,
                "the suite has no tests"

                "YAML that doesn't parse",
                """tests:
  - name: t
    query: "EVALUATE x
    expect: 1
  - name: u
""",
                3,
                "not valid YAML"

                "a key twice",
                """tests:
  - name: t
    measure: X
    measure: Y
    expect: 1
""",
                4,
                "not valid YAML"

                "an empty file", "", 1, "the file is empty"
            ]

            for (name, yaml, line, message) in cases ->
                testCase name (fun () ->
                    let actualLine, actualMessage = one yaml
                    Expect.stringContains actualMessage message "message"
                    Expect.equal actualLine line $"line ({actualMessage})")
        ]

        testCase "every test's problem is reported, and names must be unique ignoring case" (fun () ->
            let problems =
                bad
                    """tests:
  - name: a
    measure: X
  - name: b
    measure: X
    expect: 1
  - name: c
    wrong: 1
  - name: B
    measure: X
    expect: 1
"""

            Expect.equal (problems |> List.map fst) [ 2; 8; 9 ] "lines"
            Expect.stringContains (snd problems.[2]) "more than one test named 'B'" "duplicate")

        testCase "a problem is formatted file:line:column" (fun () ->
            match parse "tests:\n  - name: t\n    nope: 1\n" with
            | Error [ issue ] ->
                Expect.equal (issue.Line, issue.Column) (3, 5) "position"
                Expect.stringStarts (SuiteIssue.format issue) $"{file}:3:5: unknown key 'nope'" "format"
            | other -> failtestf "%A" other)

        testList "matrix" [
            testCase "a mapping is the cross product, and placeholders keep the values' types" (fun () ->
                let suite =
                    good
                        """tests:
  - name: sales
    matrix:
      year: [2023, 2024]
      color: [Red, "Blue"]
    measure: Amount
    filters:
      Date[Year]: "{year}"
      Product[Color]: "{color}"
    expect: { gte: 0 }
"""

                Expect.equal
                    (suite.Tests |> List.map _.Name)
                    [
                        "sales [year=2023, color=Red]"
                        "sales [year=2023, color=Blue]"
                        "sales [year=2024, color=Red]"
                        "sales [year=2024, color=Blue]"
                    ]
                    "names"

                Expect.equal
                    (suite.Tests.[2].Filters |> List.map _.Values)
                    [
                        [ FilterValue.Literal(SuiteValue.Int 2024L) ]
                        [ FilterValue.Literal(SuiteValue.Text "Red") ]
                    ]
                    "a value that is one placeholder takes the variable's value and type"

                Expect.equal suite.Tests.[2].Matrix [ "year", SuiteValue.Int 2024L; "color", SuiteValue.Text "Red" ] "matrix values")

            testCase "a list of mappings is those combinations, with expected values among them" (fun () ->
                let suite =
                    good
                        """tests:
  - name: "sales in {year}"
    matrix:
      - { year: 2023, total: 100 }
      - { year: 2024, total: 525.5 }
    query: |
      EVALUATE ROW ( "Total", CALCULATE ( [Amount], 'Date'[Year] = {year} ), "Set", { 1, 2 }, "Other", "{nope}" )
    expect:
      Total: "{total}"
"""

                Expect.equal (suite.Tests |> List.map _.Name) [ "sales in 2023"; "sales in 2024" ] "a name with a placeholder is not suffixed"

                Expect.equal
                    (suite.Tests |> List.map (fun t -> (snd t.Expect.Values.[0]).Value))
                    [ Some(SuiteValue.Int 100L); Some(SuiteValue.Float 525.5) ]
                    "expected values"

                match suite.Tests.[1].Body with
                | TestBody.Query q ->
                    Expect.stringContains q "'Date'[Year] = 2024 )" "substituted in the query"
                    Expect.stringContains q "{ 1, 2 }" "a table constructor is left alone"
                    Expect.stringContains q "\"{nope}\"" "an undeclared name is left alone"
                | other -> failtestf "%A" other)

            testCase "matrix problems" (fun () ->
                let line, message =
                    one "tests:\n  - name: t\n    measure: X\n    matrix:\n      year: []\n    expect: 1\n"

                Expect.equal line 5 "line"
                Expect.stringContains message "has no values" "message"

                let _, message =
                    one "tests:\n  - name: t\n    measure: X\n    matrix:\n      \"the year\": [1]\n    expect: 1\n"

                Expect.stringContains message "letters, digits and _" "name"

                // The expansion's names must still be unique.
                let problems =
                    bad "tests:\n  - name: \"t\"\n    measure: X\n    matrix:\n      a: [1, 1]\n    expect: 1\n"

                Expect.stringContains (snd problems.[0]) "more than one test named 't [a=1]'" "duplicates")
        ]

        testList "JSON suites" [
            testCase "the same suite as JSON, with tabs, comments, escapes and a trailing comma" (fun () ->
                let json =
                    "{\n\t\"suite\": \"J\\u00e9r\\u00f4me \\ud83d\\ude00\",\n\t// a comment\n\t\"tests\": [\n\t\t{ \"name\": \"t\", \"measure\": \"X\", \"expect\": { \"value\": 1.50, \"tolerance\": 1e-3 }, \"tags\": [\"a\"], },\n\t\t{ \"name\": \"u\", \"measure\": \"X\", \"expect\": null }\n\t]\n}\n"

                match TestSuite.parse jsonFile json with
                | Ok suite ->
                    Expect.equal suite.Name "Jérôme \U0001F600" "escapes"
                    Expect.equal (value suite.Tests.[0]).Value (Some(SuiteValue.Float 1.5)) "number"
                    Expect.equal (value suite.Tests.[0]).Tolerance.Relative (Some 0.001) "tolerance"
                    Expect.equal suite.Tests.[0].Line 5 "line"
                    Expect.equal (value suite.Tests.[1]).Value (Some SuiteValue.Null) "null expects BLANK"
                | Error issues -> failtestf "%A" issues)

            testCase "problems in JSON carry their line" (fun () ->
                let position (json: string) =
                    match TestSuite.parse jsonFile json with
                    | Error [ issue ] -> issue.Line, issue.Message
                    | other -> failtestf "expected one problem, got %A" other

                let line, message =
                    position "{\n  \"tests\": [\n    { \"name\": \"t\",\n      \"mesure\": \"X\", \"expect\": 1 }\n  ]\n}\n"

                Expect.equal line 4 "an unknown key"
                Expect.stringContains message "unknown key 'mesure'" "message"

                let line, message = position "{\n  \"tests\": [\n    { \"name\": }\n  ]\n}\n"
                Expect.equal line 3 "a syntax error"
                Expect.stringContains message "not valid JSON" "message"

                let line, message =
                    position "{\n  \"tests\": [ { \"name\": \"t\", \"measure\": \"X\",\n \"measure\": \"Y\", \"expect\": 1 } ]\n}\n"

                Expect.equal line 3 "a key twice"
                Expect.stringContains message "twice" "message")
        ]

        testList "the query a test runs as" [
            testCase "a measure is a one-row query" (fun () ->
                Expect.equal
                    (compiled "tests:\n  - name: t\n    measure: Amount\n    expect: 1\n")
                    [ "EVALUATE ROW ( \"Value\",\n[Amount]\n)" ]
                    "query")

            testCase "filters are TREATAS filters of a CALCULATE, with values as DAX literals" (fun () ->
                let yaml =
                        """tests:
  - name: t
    expression: "[Amount] -- a comment"
    filters:
      Date[Year]: [2023, 2024]
      "'Product'[Name]": 'the "big" one'
      T[Flag]: true
      T[When]: 2024-01-31
      T[Ratio]: 2.0
      T[None]: null
      T[Calc]: { dax: "1 + 1" }
    expect: 1
"""

                Expect.equal
                    (compiled yaml)
                    [
                        "EVALUATE ROW ( \"Value\",\nCALCULATE (\n[Amount] -- a comment\n, "
                        + "TREATAS ( { 2023, 2024 }, Date[Year] ), "
                        + "TREATAS ( { \"the \"\"big\"\" one\" }, 'Product'[Name] ), "
                        + "TREATAS ( { TRUE }, T[Flag] ), "
                        + "TREATAS ( { dt\"2024-01-31\" }, T[When] ), "
                        + "TREATAS ( { 2.0 }, T[Ratio] ), "
                        + "TREATAS ( { BLANK () }, T[None] ), "
                        + "TREATAS ( { ( 1 + 1 ) }, T[Calc] ) )\n)"
                    ]
                    "query")

            testCase "a query's EVALUATE goes inside CALCULATETABLE; DEFINE and ORDER BY stay" (fun () ->
                let yaml =
                        """tests:
  - name: t
    query: "DEFINE MEASURE Sales[M] = 1 EVALUATE VALUES ( Product[Color] ) ORDER BY Product[Color] DESC"
    filters:
      Date[Year]: 2024
    expect: { rows: 1 }
"""

                Expect.equal
                    (compiled yaml)
                    [
                        "DEFINE MEASURE Sales[M] = 1 EVALUATE CALCULATETABLE (\nVALUES ( Product[Color] )\n, "
                        + "TREATAS ( { 2024 }, Date[Year] ) ) ORDER BY Product[Color] DESC"
                    ]
                    "query")

            testCase "a query without filters, or that doesn't parse, is run as written" (fun () ->
                Expect.equal
                    (compiled
                        "tests:\n  - name: t\n    query: EVALUATE (\n    filters:\n      Date[Year]: 1\n    expect: { error: true }\n  - name: u\n    query: \"EVALUATE  Sales\"\n    expect: { rows: 1 }\n")
                    [ "EVALUATE ("; "EVALUATE  Sales" ]
                    "queries")
        ]

        testCase "suite files are found by name, recursively, in a stable order" (fun () ->
            use t = new TempDir()
            write (t.Combine("b", "x.test.yaml")) ""
            write (t.Combine("a", "deep", "y.TEST.YML")) ""
            write (t.Combine("a", "z.test.json")) ""
            write (t.Combine("a", "notes.yaml")) ""
            write (t.Combine("a", "z.snapshot.json")) ""

            Expect.equal
                (TestSuite.discover [ t.Path ])
                (Ok [ t.Combine("a", "deep", "y.TEST.YML"); t.Combine("a", "z.test.json"); t.Combine("b", "x.test.yaml") ])
                "found"

            // A file given by name is a suite whatever it is called.
            Expect.equal (TestSuite.discover [ t.Combine("a", "notes.yaml") ]) (Ok [ t.Combine("a", "notes.yaml") ]) "a file"
            Expect.equal (TestSuite.discover [ t.Combine "nope" ]) (Error(t.Combine "nope")) "missing"
            Expect.equal (TestRun.Snapshots.fileOf (t.Combine("b", "x.test.yaml"))) (t.Combine("b", "x.snapshot.json")) "snapshot file")
    ]
