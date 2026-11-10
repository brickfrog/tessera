/// Running the tests of `tess test` suites (TestSuite.fs).
///
/// Every test compiles to one DAX query (`compile`), so the same suite runs on the local engine
/// over fixture data and on a server: a `Target` is whatever runs a query. `run` judges each
/// result against the test's expectations; `compare` runs each query on two targets and diffs
/// the results; `Snapshots` records results next to a suite.
module Tessera.TestRun

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera

[<RequireQualifiedAccess>]
type TestOutcome =
    | Passed
    /// The expectation doesn't hold: a summary, and the differences line by line.
    | Failed of message: string * details: string list
    /// The query failed (and the test doesn't expect it to), or the test can't be run as written.
    | Errored of code: string * message: string
    /// The local engine refused: it doesn't implement something the test uses. Not a verdict.
    | Unsupported of message: string
    /// Not run: pending, skipped, or not runnable on this target.
    | Skipped of reason: string

module TestOutcome =
    let token =
        function
        | TestOutcome.Passed -> "passed"
        | TestOutcome.Failed _ -> "failed"
        | TestOutcome.Errored _ -> "error"
        | TestOutcome.Unsupported _ -> "unsupported"
        | TestOutcome.Skipped _ -> "skipped"

    /// Whether the test was run to a verdict: it passed or it failed.
    let isVerdict =
        function
        | TestOutcome.Passed
        | TestOutcome.Failed _ -> true
        | _ -> false

type TestResult = {
    Test: TestCase
    Outcome: TestOutcome
    /// The DAX query the test was run as.
    Query: string
    Duration: TimeSpan
}

type SuiteResult = {
    Suite: TestSuiteFile
    Results: TestResult list
}

/// Why a target won't run a test.
[<RequireQualifiedAccess>]
type Refusal =
    | Skip of reason: string
    | Error of code: string * message: string

/// Something that runs DAX queries: the local engine over fixture data, or a server.
type Target = {
    /// Checked before a test runs.
    Refuses: TestSuiteFile -> TestCase -> Refusal option
    /// Run a query for a test. An error that isn't the query's own (a fixture that can't be
    /// loaded, a lost connection) stops the whole run.
    Execute: TestSuiteFile -> TestCase -> string -> Result<QueryResult list, TessError>
}

// ---- Compiling a test to a query -------------------------------------------------------------------

let private filterDax (f: TestFilter) =
    let values =
        f.Values
        |> List.map (function
            | FilterValue.Literal v -> SuiteValue.daxLiteral v
            | FilterValue.Dax dax -> "( " + dax + " )")
        |> String.concat ", "

    $"TREATAS ( {{ {values} }}, {f.Column} )"

/// The DAX query a test runs as.
///
/// - `measure` / `expression` / `assert`: `EVALUATE ROW ( "Value", <expression> )`, the expression
///   inside `CALCULATE ( ..., <filters> )` when the test has filters.
/// - `query`: as written; with filters, each EVALUATE's table expression goes inside
///   `CALCULATETABLE ( ..., <filters> )` (DEFINE and ORDER BY stay where they are).
/// - A filter is `TREATAS ( { values }, column )`.
///
/// The expression sits on lines of its own, so a trailing `--` comment can't swallow the rest.
let compile (t: TestCase) : string =
    let filters = t.Filters |> List.map filterDax |> String.concat ", "

    match t.Body with
    | TestBody.Scalar e
    | TestBody.Assert e ->
        let value =
            if t.Filters.IsEmpty then
                e
            else
                "CALCULATE (\n" + e + "\n, " + filters + " )"

        "EVALUATE ROW ( \"Value\",\n" + value + "\n)"
    | TestBody.Query q when t.Filters.IsEmpty -> q
    | TestBody.Query q ->
        match DaxParser.parseQuery q with
        // It will fail with its syntax error when run.
        | Error _ -> q
        | Ok query ->
            query.Evaluates
            |> List.map (fun e -> e.Table.Span)
            |> List.sortByDescending (fun s -> s.Start)
            |> List.fold
                (fun (text: string) s ->
                    text.Substring(0, s.Start)
                    + "CALCULATETABLE (\n"
                    + text.Substring(s.Start, s.Length)
                    + "\n, "
                    + filters
                    + " )"
                    + text.Substring(s.End))
                q

/// Whether a query's (first) EVALUATE has an ORDER BY, so its row order is defined.
let private hasOrderBy (dax: string) =
    match DaxParser.parseQuery dax with
    | Ok { Evaluates = e :: _ } -> not e.OrderBy.IsEmpty
    | _ -> false

// ---- Expectations ----------------------------------------------------------------------------------

let private inv = CultureInfo.InvariantCulture

let private show (v: QueryValue) =
    match v with
    | QueryValue.Blank -> "BLANK"
    | QueryValue.String s -> "\"" + s + "\""
    | v -> QueryValue.text v

/// The result column a name in a suite means: the name itself, else ignoring case, else as a
/// measure-style column (`Revenue` is `[Revenue]`), else the one column of any table with that
/// name (`Country` is `Customer[Country]` when no other column ends in `[Country]`).
let resolveColumn (columns: QueryColumn list) (wanted: string) : QueryColumn option =
    let ignoreCase (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let bracketed = "[" + wanted + "]"

    match columns |> List.tryFind (fun c -> c.Name = wanted) with
    | Some c -> Some c
    | None ->
        match columns |> List.tryFind (fun c -> ignoreCase c.Name wanted) with
        | Some c -> Some c
        | None ->
            match columns |> List.tryFind (fun c -> ignoreCase c.Name bracketed) with
            | Some c -> Some c
            | None ->
                match
                    columns
                    |> List.filter (fun c -> c.Name.EndsWith(bracketed, StringComparison.OrdinalIgnoreCase))
                with
                | [ one ] -> Some one
                | _ -> None

let private compareOptions (tolerance: SuiteTolerance) (ordered: bool) : CompareOptions =
    if tolerance.IsSet then
        {
            Ordered = ordered
            RelativeTolerance = defaultArg tolerance.Relative 0.0
            AbsoluteTolerance = defaultArg tolerance.Absolute 0.0
            IgnoreTypesOf = Set.empty
        }
    else
        {
            CompareOptions.defaults with
                Ordered = ordered
        }

/// Whole and fixed decimal numbers as doubles. `QueryResult.compare` gives only doubles a
/// tolerance; a suite's tolerance is for every number.
let private loosen (r: QueryResult) : QueryResult =
    let value =
        function
        | QueryValue.Int64 i -> QueryValue.Double(double i)
        | QueryValue.Decimal d -> QueryValue.Double(double d)
        | v -> v

    {
        r with
            Columns =
                r.Columns
                |> List.map (fun c ->
                    match c.Type with
                    | QueryType.Int64
                    | QueryType.Decimal -> { c with Type = QueryType.Double }
                    | _ -> c)
            Rows = r.Rows |> List.map (Array.map value)
    }

/// A value written in a suite as the result value it stands for, given the type of the column it
/// is compared with: text is a date for a date column; the rest is what it is.
let private expectedValue (actual: QueryType option) (v: SuiteValue) : QueryValue =
    match v with
    | SuiteValue.Null -> QueryValue.Blank
    | SuiteValue.Bool b -> QueryValue.Boolean b
    | SuiteValue.Int i -> QueryValue.Int64 i
    | SuiteValue.Float f -> QueryValue.Double f
    | SuiteValue.Date d -> QueryValue.DateTime d
    | SuiteValue.Text s ->
        match actual, SuiteValue.tryDate s with
        | Some QueryType.DateTime, Some d -> QueryValue.DateTime d
        | _ -> QueryValue.String s

let private typeOfValue (v: QueryValue) =
    QueryValue.typeOf v |> Option.defaultValue QueryType.Variant

/// The differences worth failing for, as text. Type-only differences count only when `types`.
let private differences (types: bool) (c: Comparison) : string list =
    c.Differences
    |> List.filter (fun d -> types || not (ResultDifference.isTypeOnly d))
    |> List.map ResultDifference.describe

/// Compare, with a suite's tolerance applied to every kind of number.
let private compareResults
    (tolerance: SuiteTolerance)
    (ordered: bool)
    (types: bool)
    (expected: QueryResult)
    (actual: QueryResult)
    : string list =
    let expected, actual =
        if tolerance.IsSet then
            loosen expected, loosen actual
        else
            expected, actual

    QueryResult.compare (compareOptions tolerance ordered) expected actual
    |> differences (types && not tolerance.IsSet)

let private asNumber (v: QueryValue) : double option =
    match v with
    | QueryValue.Int64 i -> Some(double i)
    | QueryValue.Double d -> Some d
    | QueryValue.Decimal d -> Some(double d)
    | _ -> None

let private checkBounds (what: string) (b: Bounds) (value: double) : string list = [
    let number (d: double) = d.ToString("R", inv)

    match b.Gt with
    | Some limit when not (value > limit) -> $"{what} is {number value}, expected more than {number limit}"
    | _ -> ()

    match b.Gte with
    | Some limit when not (value >= limit) -> $"{what} is {number value}, expected {number limit} or more"
    | _ -> ()

    match b.Lt with
    | Some limit when not (value < limit) -> $"{what} is {number value}, expected less than {number limit}"
    | _ -> ()

    match b.Lte with
    | Some limit when not (value <= limit) -> $"{what} is {number value}, expected {number limit} or less"
    | _ -> ()
]

let private toleranceText (t: SuiteTolerance) =
    match t.Relative, t.Absolute with
    | None, None -> ""
    | Some r, None -> $" (tolerance {r.ToString(inv)})"
    | None, Some a -> $" (tolerance_abs {a.ToString(inv)})"
    | Some r, Some a -> $" (tolerance {r.ToString(inv)} or tolerance_abs {a.ToString(inv)})"

/// One column of a one-row result against its expectation.
let private checkValue (column: QueryColumn) (actual: QueryValue) (e: ValueExpect) : string list = [
    match e.Value with
    | Some wanted ->
        let expected = expectedValue (QueryValue.typeOf actual) wanted

        let one (v: QueryValue) = {
            Columns = [ { Name = column.Name; Type = typeOfValue v } ]
            Rows = [ [| v |] ]
            Truncated = false
        }

        if not (compareResults e.Tolerance true false (one expected) (one actual)).IsEmpty then
            $"{column.Name} is {show actual}, expected {show expected}{toleranceText e.Tolerance}"
    | None -> ()

    if not e.Bounds.IsEmpty then
        match asNumber actual with
        | Some n -> yield! checkBounds column.Name e.Bounds n
        | None -> $"{column.Name} is {show actual}, expected a number"
]

/// The text of one CSV field as the value it stands for in a column of this type.
let private csvValue (columnType: QueryType option) (text: string) (quoted: bool) : QueryValue =
    if text = "" && not quoted then
        QueryValue.Blank
    else
        match columnType with
        | Some QueryType.String -> QueryValue.String text
        | Some QueryType.DateTime ->
            match SuiteValue.tryDate text with
            | Some d -> QueryValue.DateTime d
            | None -> QueryValue.String text
        | _ when quoted -> QueryValue.String text
        | _ -> expectedValue columnType (SuiteValue.ofPlain text)

/// An expected table as a result to compare with the actual one: columns matched to the actual
/// columns by name (`resolveColumn`), values read for the actual columns' types.
let private expectedTable (table: TableExpect) (actual: QueryResult) : Result<QueryResult, string> =
    let column (name: string) =
        match resolveColumn actual.Columns name with
        | Some c -> c.Name, Some c.Type
        | None -> name, None

    let build (names: string list) (rows: (QueryType option -> QueryValue) list list) =
        // No columns named (an empty `table: []`): no rows of the actual columns.
        if names.IsEmpty && rows.IsEmpty then
            { actual with Rows = [] }
        else
            let columns = names |> List.map column

            {
                Columns =
                    columns
                    |> List.map (fun (name, t) -> {
                        Name = name
                        Type = defaultArg t QueryType.Variant
                    })
                Rows =
                    rows
                    |> List.map (fun row -> List.map2 (fun (_, t) cell -> cell t) columns row |> Array.ofList)
                Truncated = false
            }

    match table with
    | TableExpect.Rows(names, rows) ->
        Ok(build names (rows |> List.map (List.map (fun v -> fun t -> expectedValue t v))))
    | TableExpect.File path when path.EndsWith(".csv", StringComparison.OrdinalIgnoreCase) ->
        try
            use reader = new StreamReader(path, UTF8Encoding(false, true), true)
            let csv = CsvReader(reader)

            let rec rows (acc: (string * bool) list list) =
                match csv.Read() with
                | Error(line, reason) -> Error $"{path}:{line}: {reason}"
                | Ok false -> Ok(List.rev acc)
                | Ok true when csv.IsBlankLine && acc.IsEmpty -> rows acc
                | Ok true ->
                    rows (
                        [
                            for i in 0 .. csv.FieldCount - 1 -> csv.FieldString i, csv.IsQuoted i
                        ]
                        :: acc
                    )

            match rows [] with
            | Error e -> Error e
            | Ok [] -> Error $"{path}: the file has no header row"
            | Ok(header :: data) ->
                let names = header |> List.map fst
                // In a one-column file an empty line is a BLANK row; elsewhere it is skipped.
                let data =
                    data
                    |> List.filter (fun row -> names.Length = 1 || row <> [ "", false ])

                match data |> List.tryFindIndex (fun row -> row.Length <> names.Length) with
                | Some i -> Error $"{path}: row {i + 1} has {data.[i].Length} fields, the header has {names.Length}"
                | None ->
                    Ok(
                        build
                            names
                            (data
                             |> List.map (List.map (fun (text, quoted) -> fun t -> csvValue t text quoted)))
                    )
        with
        | :? IOException
        | :? UnauthorizedAccessException
        | :? DecoderFallbackException as e -> Error $"can't read {path}: {e.Message}"
    | TableExpect.File path ->
        try
            let options =
                JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

            match JsonNode.Parse(File.ReadAllText path, Nullable(), options) with
            | :? JsonObject as o ->
                // A result as `tess query --json` writes one.
                QueryResult.ofJson o
                |> Result.mapError (fun e -> $"{path}: {e}")
                |> Result.map (fun r -> {
                    r with
                        Columns = r.Columns |> List.map (fun c -> { c with Name = fst (column c.Name) })
                })
            | :? JsonArray as rows ->
                let objects = rows |> Seq.map (fun r -> r :?> JsonObject) |> List.ofSeq

                let names =
                    objects |> List.collect (fun o -> [ for kv in o -> kv.Key ]) |> List.distinct

                let cell (o: JsonObject) (name: string) : QueryType option -> QueryValue =
                    let value =
                        match o.[name] with
                        | null -> SuiteValue.Null
                        | node ->
                            match node.GetValueKind() with
                            | JsonValueKind.String -> SuiteValue.Text(node.GetValue<string>())
                            | JsonValueKind.True -> SuiteValue.Bool true
                            | JsonValueKind.False -> SuiteValue.Bool false
                            | JsonValueKind.Number -> SuiteValue.ofPlain (node.ToJsonString())
                            | _ -> failwith $"the value of \"{name}\" must be a string, a number, true, false or null"

                    fun t -> expectedValue t value

                Ok(build names (objects |> List.map (fun o -> names |> List.map (cell o))))
            | _ -> Error $"{path}: expected an array of row objects, or a result object with \"columns\" and \"rows\""
        with
        | :? JsonException
        | :? InvalidCastException
        | :? IOException
        | :? UnauthorizedAccessException
        | Failure _ as e -> Error $"{path}: {e.Message}"

/// Judge a result against a test's expectations (all but `error` and `snapshot`, which the caller
/// handles): the list of what doesn't hold, or why the expectation can't be checked.
let private check (t: TestCase) (ordered: bool) (actual: QueryResult) : Result<string list, string> =
    let e = t.Expect

    let table =
        match e.Table with
        | Some table ->
            expectedTable table actual
            |> Result.map (fun expected -> compareResults t.Tolerance ordered false expected actual)
        | None -> Ok []

    match table with
    | Error problem -> Error problem
    | Ok tableDifferences ->
        Ok [
            match e.Columns with
            | Some names ->
                for name in names do
                    if (resolveColumn actual.Columns name).IsNone then
                        let have = actual.Columns |> List.map _.Name |> String.concat ", "
                        $"there is no column {name} (the result has {have})"
            | None -> ()

            match e.Rows with
            | Some(RowsExpect.Exactly n) when actual.Rows.Length <> n -> $"{actual.Rows.Length} rows, expected {n}"
            | Some(RowsExpect.Within limits) -> yield! checkBounds "the row count" limits (double actual.Rows.Length)
            | _ -> ()

            match e.Empty with
            | Some true when not actual.Rows.IsEmpty -> $"{actual.Rows.Length} rows, expected none"
            | Some false when actual.Rows.IsEmpty -> "no rows, expected at least one"
            | _ -> ()

            if not e.Values.IsEmpty then
                match actual.Rows with
                | [ row ] ->
                    for (name, expected) in e.Values do
                        match resolveColumn actual.Columns name with
                        | Some column ->
                            let index = actual.Columns |> List.findIndex (fun c -> c.Name = column.Name)
                            yield! checkValue column row.[index] expected
                        | None ->
                            let have = actual.Columns |> List.map _.Name |> String.concat ", "
                            $"there is no column {name} (the result has {have})"
                | rows ->
                    $"the result has {rows.Length} rows; a column expectation needs exactly one (use `table` for more)"

            yield! tableDifferences
        ]

// ---- Snapshots -------------------------------------------------------------------------------------

/// Results recorded next to a suite, one per test with `snapshot: true`:
/// `<suite>.snapshot.json` for `<suite>.test.yaml`.
module Snapshots =

    /// The snapshot file of a suite file.
    let fileOf (suiteFile: string) : string =
        TestSuite.stem suiteFile + ".snapshot.json"

    /// The rows in one order whatever order they came in: by their text, then their types.
    let sortRows (r: QueryResult) : QueryResult =
        let key (row: QueryValue array) =
            row
            |> Array.map (fun v ->
                let tag =
                    match QueryValue.typeOf v with
                    | Some t -> QueryType.token t
                    | None -> ""

                QueryValue.text v + "\u0001" + tag)
            |> String.concat "\u0000"

        {
            r with
                Rows = r.Rows |> List.sortWith (fun a b -> String.CompareOrdinal(key a, key b))
        }

    let private compact =
        JsonSerializerOptions(Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

    /// The file's text: tests in ordinal order of their names, a row per line, LF line ends.
    /// Nothing in it depends on when or where it was written.
    let render (snapshots: (string * QueryResult) list) : string =
        let sb = StringBuilder()
        let line (s: string) = sb.Append(s).Append('\n') |> ignore
        let text (s: string) = JsonSerializer.Serialize(s, compact)

        let entries =
            snapshots |> List.sortWith (fun (a, _) (b, _) -> String.CompareOrdinal(a, b))

        line "{"
        line "  \"version\": 1,"
        line (if entries.IsEmpty then "  \"snapshots\": {}" else "  \"snapshots\": {")

        entries
        |> List.iteri (fun i (name, r) ->
            line $"    {text name}: {{"
            line "      \"columns\": ["

            r.Columns
            |> List.iteri (fun j c ->
                let comma = if j < r.Columns.Length - 1 then "," else ""
                line $"        {{ \"name\": {text c.Name}, \"type\": {text (QueryType.token c.Type)} }}{comma}")

            line "      ],"
            line (if r.Rows.IsEmpty then "      \"rows\": []" else "      \"rows\": [")

            r.Rows
            |> List.iteri (fun j row ->
                let comma = if j < r.Rows.Length - 1 then "," else ""
                let values = JsonArray(row |> Array.map QueryValue.toJson)
                line $"        {values.ToJsonString compact}{comma}")

            if not r.Rows.IsEmpty then
                line "      ]"

            line (if i < entries.Length - 1 then "    }," else "    }"))

        if not entries.IsEmpty then
            line "  }"

        line "}"
        sb.ToString()

    /// Read a snapshot file's text.
    let parse (text: string) : Result<Map<string, QueryResult>, string> =
        try
            match JsonNode.Parse text with
            | :? JsonObject as root ->
                match root.["snapshots"] with
                | :? JsonObject as snapshots ->
                    let entries = [ for kv in snapshots -> kv.Key, QueryResult.ofJson kv.Value ]

                    match
                        entries
                        |> List.tryPick (function
                            | name, Error e -> Some $"snapshot \"{name}\": {e}"
                            | _ -> None)
                    with
                    | Some e -> Error e
                    | None ->
                        Ok(
                            entries
                            |> List.choose (function
                                | name, Ok r -> Some(name, r)
                                | _ -> None)
                            |> Map.ofList
                        )
                | _ -> Error "expected an object with \"snapshots\""
            | _ -> Error "expected an object with \"snapshots\""
        with :? JsonException as e ->
            Error e.Message

    /// A suite's recorded snapshots; none if it has no snapshot file.
    let load (suiteFile: string) : Result<Map<string, QueryResult>, TessError> =
        let path = fileOf suiteFile

        if not (File.Exists path) then
            Ok Map.empty
        else
            try
                parse (File.ReadAllText path)
                |> Result.mapError (fun e -> LoadFailed(path, e))
            with
            | :? IOException
            | :? UnauthorizedAccessException as e -> Error(LoadFailed(path, e.Message))

// ---- The local target ------------------------------------------------------------------------------

/// The fixture a test evaluates over: `--data`, else the test's own directory, else the suite's;
/// with the test's inline tables on top.
let private fixtureOf (cliData: string option) (suite: TestSuiteFile) (t: TestCase) =
    let directory =
        match t.Data with
        | Some own -> Some own
        | None ->
            match cliData with
            | Some given -> Some(Path.GetFullPath given)
            | None -> suite.Data

    directory, t.Tables

let private isDataFile (path: string) =
    let extension = Path.GetExtension(path).ToLowerInvariant()
    extension = ".csv" || extension = ".json"

/// Load a fixture directory with inline tables on top, through the same loader and checks as a
/// plain directory: the directory's files (but those of tables given inline) and one JSON file
/// per inline table are put in a temporary directory, which is loaded and deleted.
let private loadFixture
    (db: Database)
    (suite: TestSuiteFile)
    (directory: string option)
    (tables: InlineTable list)
    : Result<DataStore, TessError> =
    let usable (loaded: Fixture.Loaded, issues: FixtureIssue list) (describe: FixtureIssue list -> TessError) =
        match issues |> List.filter (fun i -> i.Severity = Validate.Severity.Error) with
        | [] -> Ok loaded.Store
        | errors -> Error(describe errors)

    match directory, tables with
    | Some dir, [] ->
        Fixture.loadAndCheck db dir
        |> Result.bind (fun (loaded, issues) ->
            usable (loaded, issues) (fun errors -> FixtureHasErrors(loaded.Directory, errors.Length)))
    | _ ->
        let temp =
            Path.Combine(Path.GetTempPath(), "tessera-test-" + Guid.NewGuid().ToString("N"))

        let inlineFile (t: InlineTable) =
            Path.Combine(temp, FixtureName.encode t.Table + ".json")

        try
            try
                Directory.CreateDirectory temp |> ignore

                let copied =
                    match directory with
                    | None -> Ok()
                    | Some dir when File.Exists dir ->
                        Error(InvalidFixture(dir, "it is a file; a fixture is a directory of <Table>.csv or <Table>.json files"))
                    | Some dir when not (Directory.Exists dir) -> Error(PathNotFound dir)
                    | Some dir ->
                        for file in Directory.EnumerateFiles dir do
                            let stem = Path.GetFileNameWithoutExtension file

                            if
                                isDataFile file
                                && not (tables |> List.exists (fun t -> FixtureName.matches t.Table stem))
                            then
                                File.Copy(file, Path.Combine(temp, Path.GetFileName file))

                        Ok()

                match copied with
                | Error e -> Error e
                | Ok() ->
                    for t in tables do
                        File.WriteAllText(inlineFile t, t.Json, UTF8Encoding false)

                    Fixture.loadAndCheck db temp
                    |> Result.bind (fun (loaded, issues) ->
                        usable (loaded, issues) (fun errors ->
                            // Say where in the suite the problem is, not where in a file that is gone.
                            let first = errors.Head

                            let from =
                                tables
                                |> List.tryFind (fun t ->
                                    match first.File with
                                    | Some f -> String.Equals(Path.GetFullPath f, inlineFile t, StringComparison.OrdinalIgnoreCase)
                                    | None -> false)

                            match from with
                            | Some t ->
                                // Row r of the table is line r + 1 of its JSON file.
                                let row =
                                    match first.Line with
                                    | Some line when line > 1 -> $", row {line - 1}"
                                    | _ -> ""

                                InvalidFixture($"{suite.File}:{t.Line} (table '{t.Table}'{row})", first.Message)
                            | None ->
                                let where =
                                    match first.File with
                                    | Some f -> Path.GetFileName f + ": "
                                    | None -> ""

                                InvalidFixture($"{suite.File} (with its inline tables)", where + first.Message)))
            with
            | :? IOException
            | :? UnauthorizedAccessException as e -> Error(InvalidFixture(suite.File, e.Message))
        finally
            try
                if Directory.Exists temp then
                    Directory.Delete(temp, true)
            with _ ->
                ()

/// The local engine over fixture data, for model files. `cliData` is `--data`: it replaces the
/// suites' own `data` directory (not a test's). Each distinct fixture is loaded and prepared once.
let localTarget (db: Database) (cliData: string option) : Target =
    let prepared = Dictionary<string, Result<Engine.Prepared, TessError>>()

    let prepare (suite: TestSuiteFile) (t: TestCase) =
        let directory, tables = fixtureOf cliData suite t

        let key =
            (defaultArg directory "")
            + "\u0000"
            + (tables |> List.map (fun t -> t.Table + "\u0001" + t.Json) |> String.concat "\u0000")

        match prepared.TryGetValue key with
        | true, p -> p
        | _ ->
            let p =
                loadFixture db suite directory tables |> Result.bind (Engine.prepare db)

            prepared.[key] <- p
            p

    {
        Refuses =
            fun suite t ->
                match fixtureOf cliData suite t with
                | None, [] ->
                    Some(
                        Refusal.Error(
                            "no-data",
                            "no fixture data to evaluate over: set `data:` (a fixture directory) or `tables:` in the suite, or pass --data <directory>"
                        )
                    )
                | _ -> None
        Execute = fun suite t dax -> prepare suite t |> Result.bind (fun p -> Engine.evaluateQuery p dax)
    }

/// Why a test can't run on a server model: it brings its own data.
let remoteRefusal (t: TestCase) : Refusal option =
    if t.Data.IsSome || not t.Tables.IsEmpty then
        Some(Refusal.Skip "it has its own fixture data (`data` or `tables`); a server model is tested on the data it holds")
    else
        None

// ---- Running ---------------------------------------------------------------------------------------

type RunOptions = {
    /// Which tests to run; the others are left out of the results altogether.
    Select: TestCase -> bool
    /// Stop at the first failure or error: the tests after it are skipped.
    FailFast: bool
}

/// The query's own errors: what a test can expect, and what makes a test (not the run) fail.
let private isQueryError (e: TessError) =
    match e with
    | DaxSyntax _
    | DaxInvalid _
    | DaxEvaluation _
    | DaxNotSupported _
    | QueryFailed _ -> true
    | _ -> false

let private matchesError (wanted: ErrorExpect) (e: TessError) : string option =
    let code = TessError.code e
    let message = TessError.message e

    match wanted.Code, wanted.Message with
    | Some c, _ when not (String.Equals(c, code, StringComparison.OrdinalIgnoreCase)) ->
        Some $"expected the error '{c}', got '{code}': {message}"
    | _, Some m when not (message.Contains(m, StringComparison.OrdinalIgnoreCase)) ->
        Some $"expected an error saying \"{m}\", got: {message}"
    | _ -> None

let private failed (problems: string list) =
    match problems with
    | [] -> TestOutcome.Passed
    | [ one ] -> TestOutcome.Failed(one, [])
    | many -> TestOutcome.Failed($"{many.Length} differences", many)

/// One result set, or why the query isn't one a test takes.
let private single (results: QueryResult list) : Result<QueryResult, TestOutcome> =
    match results with
    | [ one ] -> Ok one
    | many ->
        Error(
            TestOutcome.Errored(
                "test-query",
                $"the query has {many.Length} EVALUATE statements; a test takes a query with one"
            )
        )

/// A snapshot test's result as it is recorded: rows sorted unless their order is defined.
let private snapshotOf (t: TestCase) (dax: string) (r: QueryResult) : bool * QueryResult =
    let ordered = defaultArg t.Expect.Ordered (hasOrderBy dax)
    ordered, (if ordered then r else Snapshots.sortRows r)

/// The outcome of a query that ran, against the test's expectations.
let private judge (snapshots: Map<string, QueryResult>) (t: TestCase) (dax: string) (results: QueryResult list) =
    match single results with
    | Error outcome -> outcome
    | Ok actual ->
        match t.Body, t.Expect with
        | _, { Error = Some _ } ->
            TestOutcome.Failed($"expected an error, but the query returned {actual.Rows.Length} row(s)", [])
        | TestBody.Assert _, _ ->
            match actual.Rows with
            | [ [| QueryValue.Boolean true |] ] -> TestOutcome.Passed
            | [ [| value |] ] -> TestOutcome.Failed($"the assertion is {show value}, not TRUE", [])
            | _ -> TestOutcome.Errored("test-query", "the assertion did not return one value")
        | _, { Snapshot = true } ->
            let ordered, actual = snapshotOf t dax actual

            match snapshots.TryFind t.Name with
            | None ->
                TestOutcome.Failed("no snapshot is recorded for this test (record it with `tess test snapshot <model> --save`)", [])
            | Some expected -> failed (compareResults t.Tolerance ordered true expected actual)
        | _ ->
            let ordered = defaultArg t.Expect.Ordered (hasOrderBy dax)

            match check t ordered actual with
            | Ok problems -> failed problems
            | Error problem -> TestOutcome.Errored("test-expectation", problem)

/// Run a query for a test. `Error` is an error that stops the run.
let private execute
    (target: Target)
    (suite: TestSuiteFile)
    (t: TestCase)
    (dax: string)
    : Result<Result<QueryResult list, TessError>, TessError> =
    match target.Execute suite t dax with
    | Ok results -> Ok(Ok results)
    | Error e when isQueryError e -> Ok(Error e)
    | Error e -> Error e

/// The outcome of a query that failed with an error of its own.
let private judgeError (t: TestCase) (e: TessError) : TestOutcome =
    match t.Expect.Error, e with
    // A refusal is only what a test expects when it says so.
    | Some { Code = Some code }, DaxNotSupported _ when String.Equals(code, "not-supported", StringComparison.OrdinalIgnoreCase) ->
        TestOutcome.Passed
    | _, DaxNotSupported _ -> TestOutcome.Unsupported(TessError.message e)
    | Some wanted, _ ->
        match matchesError wanted e with
        | None -> TestOutcome.Passed
        | Some problem -> TestOutcome.Failed(problem, [])
    | None, _ -> TestOutcome.Errored(TessError.code e, TessError.message e)

let private timed (t: TestCase) (dax: string) (f: unit -> Result<TestOutcome, TessError>) =
    let clock = Stopwatch.StartNew()

    f ()
    |> Result.map (fun outcome -> {
        Test = t
        Outcome = outcome
        Query = dax
        Duration = clock.Elapsed
    })

/// Run every selected test of the suites with `one`, which gives a test's outcome (`Error` stops
/// the run). Tests that are pending, or that the targets refuse, never reach `one`.
let private runAll
    (targets: Target list)
    (options: RunOptions)
    (suites: TestSuiteFile list)
    (one: TestSuiteFile -> TestCase -> string -> Result<TestOutcome, TessError>)
    : Result<SuiteResult list, TessError> =
    let mutable stopped = false
    let mutable error = None
    let done' = ResizeArray<SuiteResult>()

    for suite in suites do
        if error.IsNone then
            let results = ResizeArray<TestResult>()

            for t in suite.Tests |> List.filter options.Select do
                if error.IsNone then
                    let dax = compile t

                    let outcome =
                        timed t dax (fun () ->
                            match t.Skip, targets |> List.tryPick (fun target -> target.Refuses suite t) with
                            | Some reason, _ -> Ok(TestOutcome.Skipped reason)
                            | None, _ when stopped -> Ok(TestOutcome.Skipped "not run: an earlier test failed (--fail-fast)")
                            | None, Some(Refusal.Skip reason) -> Ok(TestOutcome.Skipped reason)
                            | None, Some(Refusal.Error(code, message)) -> Ok(TestOutcome.Errored(code, message))
                            | None, None -> one suite t dax)

                    match outcome with
                    | Error e -> error <- Some e
                    | Ok result ->
                        results.Add result

                        match result.Outcome with
                        | TestOutcome.Failed _
                        | TestOutcome.Errored _ when options.FailFast -> stopped <- true
                        | _ -> ()

            done'.Add {
                Suite = suite
                Results = List.ofSeq results
            }

    match error with
    | Some e -> Error e
    | None -> Ok(List.ofSeq done')

/// Run the suites' tests on a target and judge each against its expectations.
///
/// `Error` is what stops a run: a fixture that can't be loaded, a snapshot file that can't be
/// read, a server that can't be reached. A query's own failure is that test's outcome.
let run (target: Target) (options: RunOptions) (suites: TestSuiteFile list) : Result<SuiteResult list, TessError> =
    let snapshots = Dictionary<string, Result<Map<string, QueryResult>, TessError>>()

    let snapshotsOf (suite: TestSuiteFile) =
        match snapshots.TryGetValue suite.File with
        | true, s -> s
        | _ ->
            let s = Snapshots.load suite.File
            snapshots.[suite.File] <- s
            s

    runAll [ target ] options suites (fun suite t dax ->
        let recorded =
            if t.Expect.Snapshot then
                snapshotsOf suite
            else
                Ok Map.empty

        recorded
        |> Result.bind (fun recorded ->
            execute target suite t dax
            |> Result.map (function
                | Ok results -> judge recorded t dax results
                | Error e -> judgeError t e)))

/// Run each test's query on two targets and compare the results: `a` is the reference ("expected"),
/// `b` the one checked against it. The tests' own expectations aren't used, except that a test
/// expecting an error passes when both sides fail. `tolerance`, when set, replaces the tests' own;
/// `types` makes a difference of type alone (int64 1 against double 1.0) a difference.
let compare
    (a: Target)
    (b: Target)
    (tolerance: SuiteTolerance option)
    (types: bool)
    (options: RunOptions)
    (suites: TestSuiteFile list)
    : Result<SuiteResult list, TessError> =
    runAll [ a; b ] options suites (fun suite t dax ->
        execute a suite t dax
        |> Result.bind (fun left -> execute b suite t dax |> Result.map (fun right -> left, right))
        |> Result.map (fun (left, right) ->
            let describe (e: TessError) =
                $"{TessError.code e}: {TessError.message e}"

            match left, right with
            | Error(DaxNotSupported _ as e), _
            | _, Error(DaxNotSupported _ as e) -> TestOutcome.Unsupported(TessError.message e)
            | Error _, Error _ when t.Expect.Error.IsSome -> TestOutcome.Passed
            | Error ea, Error eb ->
                TestOutcome.Errored("both-failed", $"the query failed on both sides. Model: {describe ea}. Against: {describe eb}")
            | Error e, Ok _ ->
                TestOutcome.Failed($"the query fails on the model but not on the one compared against: {describe e}", [])
            | Ok _, Error e ->
                TestOutcome.Failed($"the query fails on the model compared against but not on the model: {describe e}", [])
            | Ok left, Ok right ->
                match single left, single right with
                | Error outcome, _
                | _, Error outcome -> outcome
                | Ok expected, Ok actual ->
                    let ordered = defaultArg t.Expect.Ordered (hasOrderBy dax)
                    failed (compareResults (defaultArg tolerance t.Tolerance) ordered types expected actual)))

// ---- Recording snapshots ---------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type SnapshotChange =
    /// Recorded for the first time.
    | Added
    /// The result is the one recorded.
    | Unchanged
    /// The result differs from the one recorded: how.
    | Changed of differences: string list
    /// Recorded, but no test with `snapshot: true` has this name any more.
    | Removed
    /// The test couldn't be evaluated, so nothing is recorded for it (what was recorded stays).
    | NotRecorded of outcome: TestOutcome

type SuiteSnapshots = {
    Suite: TestSuiteFile
    /// The snapshot file (whether or not it exists yet).
    File: string
    /// Test name and what recording does to its snapshot, in suite order; removals last.
    Changes: (string * SnapshotChange) list
    /// The file's new text; None when it would be the same as now (or there is nothing to record
    /// and no file).
    NewText: string option
}

/// Evaluate the selected snapshot tests and work out what recording them changes. Nothing is
/// written. A snapshot whose test wasn't selected stays as it is; one whose test is gone (or is no
/// longer a snapshot test) is removed.
let snapshot (target: Target) (options: RunOptions) (suites: TestSuiteFile list) : Result<SuiteSnapshots list, TessError> =
    let isSnapshot (t: TestCase) = t.Expect.Snapshot && t.Skip.IsNone
    let suites = suites |> List.filter (fun s -> s.Tests |> List.exists _.Expect.Snapshot)
    let fresh = Dictionary<string, QueryResult>()

    let options = {
        options with
            Select = fun t -> isSnapshot t && options.Select t
            FailFast = false
    }

    runAll [ target ] options suites (fun suite t dax ->
        execute target suite t dax
        |> Result.map (function
            | Error e -> judgeError { t with Expect = { t.Expect with Error = None } } e
            | Ok results ->
                match single results with
                | Error outcome -> outcome
                | Ok actual ->
                    fresh.[suite.File + "\u0000" + t.Name] <- snd (snapshotOf t dax actual)
                    TestOutcome.Passed))
    |> Result.bind (fun ran ->
        let rec each (acc: SuiteSnapshots list) (rest: SuiteResult list) =
            match rest with
            | [] -> Ok(List.rev acc)
            | r :: tail ->
                match Snapshots.load r.Suite.File with
                | Error e -> Error e
                | Ok recorded ->
                    let file = Snapshots.fileOf r.Suite.File

                    let changes =
                        r.Results
                        |> List.map (fun result ->
                            let name = result.Test.Name

                            match fresh.TryGetValue(r.Suite.File + "\u0000" + name), recorded.TryFind name with
                            | (false, _), _ -> name, SnapshotChange.NotRecorded result.Outcome
                            | (true, _), None -> name, SnapshotChange.Added
                            | (true, actual), Some expected ->
                                let ordered, _ = snapshotOf result.Test result.Query actual

                                match
                                    QueryResult.compare
                                        {
                                            CompareOptions.exact with
                                                Ordered = ordered
                                        }
                                        expected
                                        actual
                                    |> differences true
                                with
                                | [] -> name, SnapshotChange.Unchanged
                                | changed -> name, SnapshotChange.Changed changed)

                    let current =
                        r.Suite.Tests |> List.filter _.Expect.Snapshot |> List.map _.Name |> Set.ofList

                    let removed =
                        recorded
                        |> Map.toList
                        |> List.map fst
                        |> List.filter (fun name -> not (current.Contains name))

                    let entries =
                        recorded
                        |> Map.toList
                        |> List.filter (fun (name, _) -> current.Contains name)
                        |> List.map (fun (name, old) ->
                            match fresh.TryGetValue(r.Suite.File + "\u0000" + name) with
                            | true, actual -> name, actual
                            | _ -> name, old)

                    let added =
                        changes
                        |> List.choose (fun (name, change) ->
                            match change with
                            | SnapshotChange.Added -> Some(name, fresh.[r.Suite.File + "\u0000" + name])
                            | _ -> None)

                    let text = Snapshots.render (entries @ added)

                    let existing =
                        if File.Exists file then
                            Some(File.ReadAllText(file).Replace("\r\n", "\n"))
                        else
                            None

                    let newText =
                        match existing with
                        | Some old when old = text -> None
                        | None when entries.IsEmpty && added.IsEmpty -> None
                        | _ -> Some text

                    each
                        ({
                            Suite = r.Suite
                            File = file
                            Changes = changes @ (removed |> List.map (fun name -> name, SnapshotChange.Removed))
                            NewText = newText
                         }
                         :: acc)
                        tail

        each [] ran)
