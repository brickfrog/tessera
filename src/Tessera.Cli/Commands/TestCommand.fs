/// `tess test`: DAX assertion tests for a model: offline on model files with fixture data (the
/// local engine), or against a server model (`xmla:`). Suites are YAML or JSON files (TestSuite.fs);
/// TestRun.fs runs them; this file finds the suites, picks the target and renders.
module Tessera.Cli.TestCommand

open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Xml.Linq
open Argu
open Tessera
open Tessera.TestRun
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "DAX tests for a model, from .tess-tests/*.test.yaml suites: run them offline over fixture data or on a server model (run), scaffold (init), record snapshots (snapshot), compare two models (compare)."

/// The directory suites live in by default, next to the model.
let suiteDirectoryName = ".tess-tests"

type RunArgs =
    | [<MainCommand>] Target of string list
    | Data of directory: string
    | Filter of text: string
    | Tag of tag: string
    | Fail_Fast
    | Fail_On of level: string
    | Strict
    | Ci of system: string
    | Trx of file: string
    | Junit of file: string
    | Force
    | Watch

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model (model files, or xmla:<server>;<database>), then suite files or directories. "
                + $"Default: {suiteDirectoryName} next to the model, else in the current directory."
            | Data _ ->
                "the fixture directory to evaluate over (see `tess data`), instead of the suites' own `data`. Model files only."
            | Filter _ ->
                "only tests whose name contains this text, or that have this tag (repeatable or comma-separated)."
            | Tag _ -> "only tests with this tag (repeatable or comma-separated)."
            | Fail_Fast -> "stop at the first failure or error; the tests after it are skipped."
            | Fail_On _ ->
                "exit 1 for findings this severe or more: error (failures and errors; the default) or warning "
                + "(also tests the local engine doesn't support)."
            | Strict -> "the same as --fail-on warning: an unsupported test fails the run."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."
            | Trx _ -> "also write the results as a Visual Studio test results (TRX) file."
            | Junit _ -> "also write the results as a JUnit XML file."
            | Force -> "let --trx / --junit replace a file that isn't a test report."
            | Watch -> Tessera.Cli.Watch.usage

type InitArgs =
    | [<MainCommand; ExactlyOnce>] Model of model: string
    | [<AltCommandLine("-o")>] Output of file: string
    | Data of directory: string
    | Snapshot

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "the model (model files, or xmla:<server>;<database>)."
            | Output _ ->
                $"the suite file to write. Default: {suiteDirectoryName}/measures.test.yaml next to the model."
            | Data _ -> "the fixture directory the suite's tests evaluate over: written as the suite's `data`."
            | Snapshot ->
                "write snapshot tests (`snapshot: true`) instead of pending ones: record them with `tess test snapshot`."

type SnapshotArgs =
    | [<MainCommand>] Target of string list
    | Data of directory: string
    | Filter of text: string
    | Tag of tag: string
    | Save

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, then suite files or directories (as for `run`)."
            | Data _ -> "the fixture directory to evaluate over, instead of the suites' own `data`."
            | Filter _ -> "only tests whose name contains this text, or that have this tag."
            | Tag _ -> "only tests with this tag."
            | Save -> "write the snapshot files. Without it, only show what would change."

type CompareArgs =
    | [<MainCommand>] Target of string list
    | Against of model: string
    | Data of directory: string
    | Against_Data of directory: string
    | Tolerance of fraction: float
    | Tolerance_Abs of amount: float
    | Types
    | Filter of text: string
    | Tag of tag: string
    | Fail_Fast
    | Fail_On of level: string
    | Strict
    | Ci of system: string
    | Trx of file: string
    | Junit of file: string
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, then suite files or directories (as for `run`)."
            | Against _ ->
                "the model to compare with: model files (another version of the model) or xmla:<server>;<database>."
            | Data _ -> "the fixture directory for model files, instead of the suites' own `data`."
            | Against_Data _ ->
                "the fixture directory for --against, when it is model files. Default: the same as the model's."
            | Tolerance _ -> "relative tolerance for numbers (0.001 is 0.1%), instead of the tests' own."
            | Tolerance_Abs _ -> "absolute tolerance for numbers, instead of the tests' own."
            | Types -> "also report values that differ in type only (a whole number 1 against a decimal 1.0)."
            | Filter _ -> "only tests whose name contains this text, or that have this tag."
            | Tag _ -> "only tests with this tag."
            | Fail_Fast -> "stop at the first difference or error."
            | Fail_On _ -> "exit 1 for findings this severe or more: error (the default) or warning."
            | Strict -> "the same as --fail-on warning."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."
            | Trx _ -> "also write the results as a TRX file."
            | Junit _ -> "also write the results as a JUnit XML file."
            | Force -> "let --trx / --junit replace a file that isn't a test report."

type ListArgs =
    | [<MainCommand>] Paths of string list
    | Filter of text: string
    | Tag of tag: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Paths _ -> $"suite files or directories. Default: {suiteDirectoryName} in the current directory."
            | Filter _ -> "only tests whose name contains this text, or that have this tag."
            | Tag _ -> "only tests with this tag."

type Args =
    | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<RunArgs>
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitArgs>
    | [<CliPrefix(CliPrefix.None)>] Snapshot of ParseResults<SnapshotArgs>
    | [<CliPrefix(CliPrefix.None)>] Compare of ParseResults<CompareArgs>
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Run _ ->
                "run the suites' tests against the model: offline over fixture data, or on a server model "
                + "(exit 1: a test failed, or nothing was tested)."
            | Init _ -> "write a suite with one test per measure, to fill in (never overwrites a file)."
            | Snapshot _ ->
                "record the results of the suites' snapshot tests next to the suites (dry run: shows what would change; --save writes)."
            | Compare _ ->
                "run the suites' queries on the model and on --against (another version, or the deployed model) "
                + "and report the differences (exit 1: differences)."
            | List _ -> "check the suites and list their tests, without a model."

// ---- Finding the suites ----------------------------------------------------------------------------

/// Where a model's suites are by default: beside a model file (or a .pbip project's files), and
/// beside a model folder (inside it for a folder that holds a .pbip).
let private suiteDirectory (model: string) : string =
    let here = Path.Combine(Directory.GetCurrentDirectory(), suiteDirectoryName)

    if XmlaAddress.isRemote model then
        here
    else
        let full =
            Path.GetFullPath(model).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

        let folder =
            if Directory.Exists full then
                if Directory.EnumerateFiles(full, "*.pbip") |> Seq.isEmpty then
                    Path.GetDirectoryName full
                else
                    full
            else
                Path.GetDirectoryName full

        match folder with
        | null
        | "" -> here
        | folder -> Path.Combine(folder, suiteDirectoryName)

type private Suites = {
    Valid: TestSuiteFile list
    Invalid: (string * SuiteIssue list) list
}

/// The suites for a command: the paths given, or the default directory (next to the model, else
/// the current directory's). Finding none is an error: a run that tests nothing must not pass.
let private findSuites (model: string option) (paths: string list) : Result<Suites, TessError> =
    let files =
        match paths with
        | [] ->
            let here = Path.Combine(Directory.GetCurrentDirectory(), suiteDirectoryName)

            let candidates =
                match model with
                | Some m -> [ suiteDirectory m; here ] |> List.distinct
                | None -> [ here ]

            match candidates |> List.tryFind Directory.Exists with
            | None -> Error(NoTestSuites(String.concat " or " candidates))
            | Some directory ->
                match TestSuite.discover [ directory ] with
                | Ok [] -> Error(NoTestSuites directory)
                | Ok files -> Ok files
                | Error missing -> Error(PathNotFound missing)
        | given ->
            match TestSuite.discover given with
            | Ok [] -> Error(NoTestSuites(String.concat ", " given))
            | Ok files -> Ok files
            | Error missing -> Error(PathNotFound missing)

    files
    |> Result.map (fun files ->
        let loaded = files |> List.map (fun f -> f, TestSuite.load f)

        {
            Valid =
                loaded
                |> List.choose (function
                    | _, Ok s -> Some s
                    | _ -> None)
            Invalid =
                loaded
                |> List.choose (function
                    | f, Error issues -> Some(f, issues)
                    | _ -> None)
        })

let private select (filters: string list) (tags: string list) (t: TestCase) =
    let same (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let hasTag (tag: string) = t.Tags |> List.exists (same tag)

    (filters.IsEmpty
     || filters
        |> List.exists (fun f -> t.Name.Contains(f, StringComparison.OrdinalIgnoreCase) || hasTag f))
    && (tags.IsEmpty || tags |> List.exists hasTag)

// ---- Targets ---------------------------------------------------------------------------------------

/// Run `f` with what evaluates queries for a model: the local engine for model files, one open
/// connection for a server model.
let private withTarget
    (ctx: Ctx)
    (model: string)
    (data: string option)
    (f: string -> Target -> Result<'T, TessError>)
    : Result<'T, TessError> =
    if XmlaAddress.isRemote model then
        XmlaAddress.parse model
        |> Result.bind (fun address ->
            Remote.withConnection (Source.access ctx) address (fun c ->
                Remote.resolveDatabase c
                |> Result.bind (fun d ->
                    let database = XmlaAddress.withDatabase d.Name address

                    f (XmlaAddress.display database) {
                        Refuses =
                            fun _ t ->
                                match remoteRefusal t with
                                | Some refusal -> Some refusal
                                | None ->
                                    let given = TestRun.remoteProperties t |> List.map fst

                                    match
                                        address.Properties
                                        |> List.tryFind (fun (k, _) ->
                                            given
                                            |> List.exists (fun g ->
                                                String.Equals(g, k, StringComparison.OrdinalIgnoreCase)))
                                    with
                                    | Some(key, _) ->
                                        Some(
                                            Refusal.Error(
                                                "duplicate-property",
                                                $"the address already sets {key}=, and the test's roles or identity set it too"
                                            )
                                        )
                                    | None -> None
                        // A test with roles or an identity has a connection of its own, with them.
                        Execute =
                            fun _ t dax ->
                                match TestRun.remoteProperties t with
                                | [] -> Remote.queryOn c d.Name dax None
                                | extra ->
                                    Remote.withConnection
                                        (Source.access ctx)
                                        {
                                            database with
                                                Properties = database.Properties @ extra
                                        }
                                        (fun own -> Remote.queryOn own d.Name dax None)
                    })))
    else
        ModelIO.loadPath model
        |> Result.bind (fun (source, db) -> f source.Path (localTarget db data))

// ---- Results ---------------------------------------------------------------------------------------

type private Counts = {
    Passed: int
    Failed: int
    Errored: int
    Unsupported: int
    Skipped: int
} with

    member c.Total = c.Passed + c.Failed + c.Errored + c.Unsupported + c.Skipped

let private count (results: TestResult list) =
    let n (f: TestOutcome -> bool) =
        results |> List.filter (fun r -> f r.Outcome) |> List.length

    {
        Passed = n _.IsPassed
        Failed = n _.IsFailed
        Errored = n _.IsErrored
        Unsupported = n _.IsUnsupported
        Skipped = n _.IsSkipped
    }

type private Report = {
    /// "test run" or "test compare".
    Command: string
    Source: string
    Against: string option
    Suites: SuiteResult list
    Invalid: (string * SuiteIssue list) list
    Elapsed: TimeSpan
}

type private ReportOptions = {
    FailOnWarning: bool
    Ci: Findings.Ci
    Trx: string option
    Junit: string option
    Force: bool
}

let private finding
    (severity: Findings.Severity)
    (code: string)
    (objectType: string)
    (name: string)
    (message: string)
    : Findings.Finding = {
    Severity = severity
    Source = "test"
    Code = code
    Message = message
    Object = name
    ObjectType = objectType
    ObjectPath = None
    Position = None
    RuleName = None
    Category = None
    Fixable = false
}

/// Up to this many difference lines are shown per test in text and in findings; JSON has all.
let private shownDetails = 20

let private detailText (details: string list) =
    let shown = details |> List.truncate shownDetails

    if details.Length > shownDetails then
        shown @ [ $"... and {details.Length - shownDetails} more" ]
    else
        shown

let private findingsOf (r: Report) : Findings.Finding list = [
    for (file, issues) in r.Invalid do
        for i in issues ->
            finding
                Findings.Severity.Error
                "TEST_SUITE_INVALID"
                "TestSuite"
                (Path.GetFileName file)
                (SuiteIssue.format i)

    for s in r.Suites do
        for t in s.Results do
            match t.Outcome with
            | TestOutcome.Failed(message, details) ->
                yield
                    finding
                        Findings.Severity.Error
                        "TEST_FAIL"
                        "Test"
                        t.Test.Name
                        (String.concat "\n" (message :: detailText details))
            | TestOutcome.Errored(code, message) ->
                yield finding Findings.Severity.Error "TEST_ERROR" "Test" t.Test.Name $"{code}: {message}"
            | TestOutcome.Unsupported message ->
                yield finding Findings.Severity.Warning "TEST_UNSUPPORTED" "Test" t.Test.Name message
            | TestOutcome.Passed
            | TestOutcome.Skipped _ -> ()

    // A run that reached no verdict tested nothing: that is never a pass.
    if
        r.Invalid.IsEmpty
        && not (
            r.Suites
            |> List.exists (fun s -> s.Results |> List.exists (fun t -> TestOutcome.isVerdict t.Outcome))
        )
    then
        yield
            finding
                Findings.Severity.Error
                "TEST_NONE_RAN"
                "TestSuite"
                "(run)"
                "no test was run to a verdict: none passed and none failed (they were filtered out, pending, skipped or unsupported)"
]

let private resultJson (t: TestResult) : JsonNode =
    obj [
        yield "name", str t.Test.Name
        yield "outcome", str (TestOutcome.token t.Outcome)
        yield "line", num t.Test.Line
        yield "tags", arr (t.Test.Tags |> List.map str)
        yield "durationMs", num (int t.Duration.TotalMilliseconds)

        match t.Outcome with
        | TestOutcome.Passed -> ()
        | TestOutcome.Failed(message, details) ->
            yield "message", str message
            yield "differences", arr (details |> List.map str)
        | TestOutcome.Errored(code, message) ->
            yield "code", str code
            yield "message", str message
        | TestOutcome.Unsupported message
        | TestOutcome.Skipped message -> yield "message", str message

        yield "query", str t.Query
    ]

let private countsJson (c: Counts) (extra: (string * JsonNode) list) : JsonNode =
    obj [
        yield "passed", num c.Passed
        yield "failed", num c.Failed
        yield "errored", num c.Errored
        yield "unsupported", num c.Unsupported
        yield "skipped", num c.Skipped
        yield! extra
        yield "total", num c.Total
    ]

let private reportJson (r: Report) (findings: Findings.Finding list) : JsonNode =
    let all = r.Suites |> List.collect _.Results

    Findings.envelope r.Command r.Elapsed findings [
        yield "source", str r.Source

        match r.Against with
        | Some against -> yield "against", str against
        | None -> ()

        yield
            "suites",
            arr [
                for s in r.Suites ->
                    obj [
                        "name", str s.Suite.Name
                        "file", str s.Suite.File
                        "summary", countsJson (count s.Results) []
                        "tests", arr (s.Results |> List.map resultJson)
                    ]
            ]

        yield
            "invalidSuites",
            arr [
                for (file, issues) in r.Invalid ->
                    obj [
                        "file", str file
                        "validationErrors", arr (issues |> List.map (fun i -> str (SuiteIssue.format i)))
                    ]
            ]

        yield
            "testSummary",
            countsJson (count all) [
                "invalidSuites", num r.Invalid.Length
                "durationMs", num (int r.Elapsed.TotalMilliseconds)
            ]
    ]

let private plural (n: int) (one: string) (many: string) =
    if n = 1 then $"1 {one}" else $"{n} {many}"

let private summaryLine (c: Counts) (elapsed: TimeSpan) =
    let parts = [
        if c.Passed > 0 then
            $"{c.Passed} passed"
        if c.Failed > 0 then
            $"{c.Failed} failed"
        if c.Errored > 0 then
            plural c.Errored "error" "errors"
        if c.Unsupported > 0 then
            $"{c.Unsupported} unsupported"
        if c.Skipped > 0 then
            $"{c.Skipped} skipped"
    ]

    let seconds =
        elapsed.TotalSeconds.ToString("0.0", Globalization.CultureInfo.InvariantCulture)

    let tests = plural c.Total "test" "tests"

    if parts.IsEmpty then
        $"{tests} ({seconds} s)"
    else
        let list = String.concat ", " parts
        $"{tests}: {list} ({seconds} s)"

let private renderText (out: TextWriter) (r: Report) =
    for (file, issues) in r.Invalid do
        out.WriteLine($"{file}: not a valid suite")

        for i in issues do
            out.WriteLine($"  {SuiteIssue.format i}")

        out.WriteLine()

    for s in r.Suites do
        out.WriteLine($"{s.Suite.Name}  ({s.Suite.File})")

        for t in s.Results do
            let label, lines =
                match t.Outcome with
                | TestOutcome.Passed -> "pass", []
                | TestOutcome.Failed(message, details) -> "FAIL", message :: detailText details
                | TestOutcome.Errored(code, message) -> "ERROR", [ $"{code}: {message}" ]
                | TestOutcome.Unsupported message -> "unsupported", [ message ]
                | TestOutcome.Skipped reason -> "skip", [ reason ]

            out.WriteLine($"  {label, -11}  {t.Test.Name}")

            for line in lines do
                for part in line.Split('\n') do
                    out.WriteLine($"               {part.TrimEnd('\r')}")

        out.WriteLine()

    if r.Invalid.IsEmpty then
        out.WriteLine(summaryLine (count (r.Suites |> List.collect _.Results)) r.Elapsed)
    else
        out.WriteLine((plural r.Invalid.Length "suite is" "suites are" + " not valid: nothing was run."))

// ---- TRX and JUnit ---------------------------------------------------------------------------------

/// A GUID that depends only on the text: a test keeps its id from run to run.
let private stableGuid (text: string) =
    Guid(MD5.HashData(Encoding.UTF8.GetBytes text))

let private outcomeMessage (o: TestOutcome) =
    match o with
    | TestOutcome.Passed -> None
    | TestOutcome.Failed(message, details) -> Some(String.concat "\n" (message :: details))
    | TestOutcome.Errored(code, message) -> Some $"{code}: {message}"
    | TestOutcome.Unsupported message -> Some message
    | TestOutcome.Skipped reason -> Some reason

/// Characters XML 1.0 can't hold (control characters, mostly) as U+FFFD.
let private xmlSafe (s: string) =
    String(
        s.ToCharArray()
        |> Array.map (fun c ->
            if c = '\t' || c = '\n' || c = '\r' || (c >= ' ' && c <> '￾' && c <> '￿') then
                c
            else
                '�')
    )

/// Whether an unsupported test counts as a failure in the report files (--strict).
let private trx (r: Report) (strict: bool) (finished: DateTime) : XDocument =
    let ns = XNamespace.Get "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"
    let x (name: string) = ns + name
    let attr (name: string) (value: obj) = XAttribute(XName.Get name, value)
    let iso (d: DateTime) = d.ToString("o")
    let started = finished - r.Elapsed
    let listId = "8c84fa94-04c1-424b-9868-57a2d4851a1d"
    let unitTestType = "13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b"

    let tests = [
        for s in r.Suites do
            for t in s.Results ->
                let key = s.Suite.File + "\u0000" + t.Test.Name
                s, t, stableGuid ("test:" + key), stableGuid ("execution:" + key)
    ]

    let outcomeOf (o: TestOutcome) =
        match o with
        | TestOutcome.Passed -> "Passed"
        | TestOutcome.Failed _ -> "Failed"
        | TestOutcome.Errored _ -> "Error"
        | TestOutcome.Unsupported _ when strict -> "Failed"
        | TestOutcome.Unsupported _
        | TestOutcome.Skipped _ -> "NotExecuted"

    let all = tests |> List.map (fun (_, t, _, _) -> outcomeOf t.Outcome)

    let n (outcome: string) =
        all |> List.filter ((=) outcome) |> List.length

    let bad = n "Failed" + n "Error" > 0 || not r.Invalid.IsEmpty

    XDocument(
        XElement(
            x "TestRun",
            attr "id" (stableGuid ("run:" + r.Source + iso finished)),
            attr "name" $"tess {r.Command}: {r.Source}",
            XElement(x "Times", attr "creation" (iso started), attr "start" (iso started), attr "finish" (iso finished)),
            XElement(
                x "ResultSummary",
                attr "outcome" (if bad then "Failed" else "Completed"),
                XElement(
                    x "Counters",
                    attr "total" all.Length,
                    attr "executed" (all.Length - n "NotExecuted"),
                    attr "passed" (n "Passed"),
                    attr "failed" (n "Failed"),
                    attr "error" (n "Error"),
                    attr "notExecuted" (n "NotExecuted")
                ),
                XElement(
                    x "RunInfos",
                    [
                        for (file, issues) in r.Invalid do
                            for i in issues ->
                                XElement(
                                    x "RunInfo",
                                    attr "computerName" "tess",
                                    attr "outcome" "Error",
                                    attr "timestamp" (iso finished),
                                    XElement(x "Text", xmlSafe $"{file}: {SuiteIssue.format i}")
                                )
                    ]
                )
            ),
            XElement(
                x "TestDefinitions",
                [
                    for (s, t, testId, executionId) in tests ->
                        XElement(
                            x "UnitTest",
                            attr "name" (xmlSafe t.Test.Name),
                            attr "storage" s.Suite.File,
                            attr "id" testId,
                            XElement(x "Execution", attr "id" executionId),
                            XElement(
                                x "TestMethod",
                                attr "codeBase" s.Suite.File,
                                attr "adapterTypeName" "executor://tess/test",
                                attr "className" (xmlSafe s.Suite.Name),
                                attr "name" (xmlSafe t.Test.Name)
                            )
                        )
                ]
            ),
            XElement(x "TestLists", XElement(x "TestList", attr "name" "Results Not in a List", attr "id" listId)),
            XElement(
                x "TestEntries",
                [
                    for (_, _, testId, executionId) in tests ->
                        XElement(
                            x "TestEntry",
                            attr "testId" testId,
                            attr "executionId" executionId,
                            attr "testListId" listId
                        )
                ]
            ),
            XElement(
                x "Results",
                [
                    for (_, t, testId, executionId) in tests ->
                        XElement(
                            x "UnitTestResult",
                            attr "executionId" executionId,
                            attr "testId" testId,
                            attr "testName" (xmlSafe t.Test.Name),
                            attr "computerName" "tess",
                            attr "duration" (t.Duration.ToString "c"),
                            attr "startTime" (iso started),
                            attr "endTime" (iso finished),
                            attr "testType" unitTestType,
                            attr "outcome" (outcomeOf t.Outcome),
                            attr "testListId" listId,
                            [
                                match outcomeMessage t.Outcome with
                                | None -> ()
                                | Some message when outcomeOf t.Outcome = "NotExecuted" ->
                                    XElement(x "Output", XElement(x "StdOut", xmlSafe message))
                                | Some message ->
                                    XElement(
                                        x "Output",
                                        XElement(x "ErrorInfo", XElement(x "Message", xmlSafe message))
                                    )
                            ]
                        )
                ]
            )
        )
    )

let private junit (r: Report) (strict: bool) : XDocument =
    let attr (name: string) (value: obj) = XAttribute(XName.Get name, value)

    let seconds (d: TimeSpan) =
        d.TotalSeconds.ToString("0.000", Globalization.CultureInfo.InvariantCulture)

    let failures (results: TestResult list) =
        results
        |> List.filter (fun t -> t.Outcome.IsFailed || (strict && t.Outcome.IsUnsupported))
        |> List.length

    let errors (results: TestResult list) =
        results |> List.filter _.Outcome.IsErrored |> List.length

    let skipped (results: TestResult list) =
        results
        |> List.filter (fun t -> t.Outcome.IsSkipped || (not strict && t.Outcome.IsUnsupported))
        |> List.length

    let all = r.Suites |> List.collect _.Results

    XDocument(
        XElement(
            XName.Get "testsuites",
            attr "name" $"tess {r.Command}",
            attr "tests" all.Length,
            attr "failures" (failures all),
            attr "errors" (errors all + r.Invalid.Length),
            attr "skipped" (skipped all),
            attr "time" (seconds r.Elapsed),
            [
                for (file, issues) in r.Invalid ->
                    XElement(
                        XName.Get "testsuite",
                        attr "name" (Path.GetFileName file),
                        attr "file" file,
                        attr "tests" 1,
                        attr "failures" 0,
                        attr "errors" 1,
                        attr "skipped" 0,
                        XElement(
                            XName.Get "testcase",
                            attr "name" "the suite is valid",
                            attr "classname" (Path.GetFileName file),
                            XElement(
                                XName.Get "error",
                                attr "type" "TEST_SUITE_INVALID",
                                attr "message" "the suite is not valid",
                                xmlSafe (issues |> List.map SuiteIssue.format |> String.concat "\n")
                            )
                        )
                    )

                for s in r.Suites ->
                    XElement(
                        XName.Get "testsuite",
                        attr "name" (xmlSafe s.Suite.Name),
                        attr "file" s.Suite.File,
                        attr "tests" s.Results.Length,
                        attr "failures" (failures s.Results),
                        attr "errors" (errors s.Results),
                        attr "skipped" (skipped s.Results),
                        attr "time" (seconds (s.Results |> List.sumBy _.Duration.TotalSeconds |> TimeSpan.FromSeconds)),
                        [
                            for t in s.Results ->
                                XElement(
                                    XName.Get "testcase",
                                    attr "name" (xmlSafe t.Test.Name),
                                    attr "classname" (xmlSafe s.Suite.Name),
                                    attr "time" (seconds t.Duration),
                                    [
                                        match t.Outcome with
                                        | TestOutcome.Passed -> ()
                                        | TestOutcome.Failed(message, details) ->
                                            XElement(
                                                XName.Get "failure",
                                                attr "type" "TEST_FAIL",
                                                attr "message" (xmlSafe message),
                                                xmlSafe (String.concat "\n" (message :: details))
                                            )
                                        | TestOutcome.Errored(code, message) ->
                                            XElement(
                                                XName.Get "error",
                                                attr "type" code,
                                                attr "message" (xmlSafe message),
                                                xmlSafe message
                                            )
                                        | TestOutcome.Unsupported message when strict ->
                                            XElement(
                                                XName.Get "failure",
                                                attr "type" "TEST_UNSUPPORTED",
                                                attr "message" (xmlSafe message),
                                                xmlSafe message
                                            )
                                        | TestOutcome.Unsupported message
                                        | TestOutcome.Skipped message ->
                                            XElement(XName.Get "skipped", attr "message" (xmlSafe message))
                                    ]
                                )
                        ]
                    )
            ]
        )
    )

/// Write a report file. A file that is already there is replaced only if it is a test report
/// (or with --force): a mistyped path must not cost someone their model.
let private checkReport (path: string) (force: bool) : Result<unit, TessError> =
    let full = Path.GetFullPath path

    let isReport () =
        use reader = new StreamReader(full)
        let buffer = Array.zeroCreate<char> 1024
        let read = reader.Read(buffer, 0, buffer.Length)
        let head = String(buffer, 0, read)
        head.Contains "<TestRun" || head.Contains "<testsuites"

    try
        if Directory.Exists full then
            Error(SaveFailed(full, "it is a directory"))
        elif File.Exists full && not force && FileInfo(full).Length > 0L && not (isReport ()) then
            Error(DestinationNotEmpty full)
        else
            Ok()
    with
    | :? IOException
    | :? UnauthorizedAccessException as e -> Error(SaveFailed(full, e.Message))

let private writeReport (path: string) (force: bool) (document: XDocument) : Result<unit, TessError> =
    let full = Path.GetFullPath path

    checkReport path force
    |> Result.bind (fun () ->
        try
            match Path.GetDirectoryName full with
            | null
            | "" -> ()
            | directory -> Directory.CreateDirectory directory |> ignore

            document.Save full
            Ok()
        with
        | :? IOException
        | :? UnauthorizedAccessException as e -> Error(SaveFailed(full, e.Message)))

/// The report files a run was asked for can be written: checked before anything runs.
let private checkReports (options: ReportOptions) : Result<unit, TessError> =
    [ options.Trx; options.Junit ]
    |> List.choose id
    |> List.fold
        (fun (state: Result<unit, TessError>) path -> state |> Result.bind (fun () -> checkReport path options.Force))
        (Ok())

/// Render a report everywhere it was asked for, and say how the run went.
let private finish (ctx: Ctx) (options: ReportOptions) (r: Report) : Outcome =
    let findings = findingsOf r
    let strict = options.FailOnWarning

    let written =
        [
            options.Trx
            |> Option.map (fun path -> fun () -> writeReport path options.Force (trx r strict DateTime.UtcNow))
            options.Junit
            |> Option.map (fun path -> fun () -> writeReport path options.Force (junit r strict))
        ]
        |> List.choose id
        |> List.fold (fun (state: Result<unit, TessError>) write -> state |> Result.bind write) (Ok())

    match written with
    // One document on stdout: the error, not a report and then an error.
    | Error e -> Failed e
    | Ok() ->
        if ctx.Json then
            writeJson ctx.Out (reportJson r findings)
        else
            renderText ctx.Out r

        Findings.annotate options.Ci ctx.Err findings

        let fails (f: Findings.Finding) =
            f.Severity = Findings.Severity.Error
            || (strict && f.Severity = Findings.Severity.Warning)

        if findings |> List.exists fails then
            if not ctx.Json then
                for f in findings do
                    if f.Code = "TEST_NONE_RAN" then
                        ctx.Err.WriteLine($"tess: {f.Message}")

            DoneWith ExitCode.findings
        else
            Done

/// What `run` and `compare` share on the command line.
type private Shared = {
    Paths: string list
    Filters: string list
    Tags: string list
    FailFast: bool
    FailOn: string option
    Strict: bool
    Ci: string option
    Trx: string option
    Junit: string option
    Force: bool
}

let private reportOptions (s: Shared) : Result<ReportOptions, string> =
    let failOn =
        match s.FailOn |> Option.map (fun l -> l.Trim().ToLowerInvariant()) with
        | None -> Ok s.Strict
        | Some "error" -> Ok s.Strict
        | Some "warning" -> Ok true
        | Some other -> Error $"--fail-on takes error or warning, not '{other}'"

    let ci =
        match s.Ci with
        | None -> Ok Findings.NoCi
        | Some name ->
            match Findings.Ci.tryParse name with
            | Some ci -> Ok ci
            | None -> Error $"unknown --ci '{name}' (known: {Findings.Ci.names})"

    match failOn, ci with
    | Error e, _
    | _, Error e -> Error e
    | Ok failOnWarning, Ok ci ->
        Ok {
            FailOnWarning = failOnWarning
            Ci = ci
            Trx = s.Trx
            Junit = s.Junit
            Force = s.Force
        }

/// Find and read the suites, then run them with `evaluate`; a suite that isn't valid stops
/// everything before a model is loaded or a connection made.
let private execute
    (ctx: Ctx)
    (command: string)
    (model: string)
    (shared: Shared)
    (evaluate: RunOptions -> TestSuiteFile list -> Result<string * string option * SuiteResult list, TessError>)
    : Outcome =
    match reportOptions shared with
    | Error message -> UsageError message
    | Ok options ->
        let clock = Stopwatch.StartNew()

        match
            checkReports options
            |> Result.bind (fun () -> findSuites (Some model) shared.Paths)
        with
        | Error e -> Failed e
        | Ok suites when not suites.Invalid.IsEmpty ->
            finish ctx options {
                Command = command
                Source = model
                Against = None
                Suites = []
                Invalid = suites.Invalid
                Elapsed = clock.Elapsed
            }
        | Ok suites ->
            if not ctx.Json then
                for s in suites.Valid do
                    for note in s.Notes do
                        ctx.Err.WriteLine($"note: {SuiteIssue.format note}")

            let run = {
                Select = select shared.Filters shared.Tags
                FailFast = shared.FailFast
            }

            match evaluate run suites.Valid with
            | Error e -> Failed e
            | Ok(source, against, results) ->
                finish ctx options {
                    Command = command
                    Source = source
                    Against = against
                    Suites = results |> List.filter (fun s -> not s.Results.IsEmpty)
                    Invalid = []
                    Elapsed = clock.Elapsed
                }

// ---- run -------------------------------------------------------------------------------------------

let private runTests (ctx: Ctx) (args: ParseResults<RunArgs>) =
    match args.GetResults RunArgs.Target |> List.concat with
    | [] -> UsageError "expected a model: model files, or xmla:<server>;<database>"
    | model :: _ when XmlaAddress.isRemote model && args.Contains RunArgs.Data ->
        UsageError "--data is for model files: a server model is tested on the data it holds"
    | model :: paths ->
        let data = args.TryGetResult RunArgs.Data

        execute
            ctx
            "test run"
            model
            {
                Paths = paths
                Filters = parseList (args.GetResults RunArgs.Filter)
                Tags = parseList (args.GetResults RunArgs.Tag)
                FailFast = args.Contains RunArgs.Fail_Fast
                FailOn = args.TryGetResult RunArgs.Fail_On
                Strict = args.Contains RunArgs.Strict
                Ci = args.TryGetResult RunArgs.Ci
                Trx = args.TryGetResult RunArgs.Trx
                Junit = args.TryGetResult RunArgs.Junit
                Force = args.Contains RunArgs.Force
            }
            (fun options suites ->
                withTarget ctx model data (fun source target ->
                    TestRun.run target options suites
                    |> Result.map (fun results -> source, None, results)))

// ---- compare ---------------------------------------------------------------------------------------

let private compare (ctx: Ctx) (args: ParseResults<CompareArgs>) =
    let negative (value: float option) =
        match value with
        | Some v -> v < 0.0 || Double.IsNaN v
        | None -> false

    match args.GetResults CompareArgs.Target |> List.concat, args.TryGetResult CompareArgs.Against with
    | [], _ -> UsageError "expected a model: model files, or xmla:<server>;<database>"
    | _, None -> UsageError "expected --against <model>: the model to compare with"
    | _ when
        negative (args.TryGetResult CompareArgs.Tolerance)
        || negative (args.TryGetResult CompareArgs.Tolerance_Abs)
        ->
        UsageError "--tolerance and --tolerance-abs must be 0 or more"
    | model :: paths, Some against ->
        let data = args.TryGetResult CompareArgs.Data

        let againstData =
            match args.TryGetResult CompareArgs.Against_Data with
            | Some d -> Some d
            | None -> data

        let tolerance =
            match args.TryGetResult CompareArgs.Tolerance, args.TryGetResult CompareArgs.Tolerance_Abs with
            | None, None -> None
            | relative, absolute ->
                Some {
                    Relative = relative
                    Absolute = absolute
                }

        execute
            ctx
            "test compare"
            model
            {
                Paths = paths
                Filters = parseList (args.GetResults CompareArgs.Filter)
                Tags = parseList (args.GetResults CompareArgs.Tag)
                FailFast = args.Contains CompareArgs.Fail_Fast
                FailOn = args.TryGetResult CompareArgs.Fail_On
                Strict = args.Contains CompareArgs.Strict
                Ci = args.TryGetResult CompareArgs.Ci
                Trx = args.TryGetResult CompareArgs.Trx
                Junit = args.TryGetResult CompareArgs.Junit
                Force = args.Contains CompareArgs.Force
            }
            (fun options suites ->
                withTarget ctx model data (fun source a ->
                    withTarget ctx against againstData (fun other b ->
                        TestRun.compare a b tolerance (args.Contains CompareArgs.Types) options suites
                        |> Result.map (fun results -> source, Some other, results))))

// ---- snapshot --------------------------------------------------------------------------------------

let private snapshot (ctx: Ctx) (args: ParseResults<SnapshotArgs>) =
    match args.GetResults SnapshotArgs.Target |> List.concat with
    | [] -> UsageError "expected a model: model files, or xmla:<server>;<database>"
    | model :: _ when XmlaAddress.isRemote model && args.Contains SnapshotArgs.Data ->
        UsageError "--data is for model files: a server model is tested on the data it holds"
    | model :: paths ->
        let save = args.Contains SnapshotArgs.Save

        let options = {
            Select =
                select (parseList (args.GetResults SnapshotArgs.Filter)) (parseList (args.GetResults SnapshotArgs.Tag))
            FailFast = false
        }

        match findSuites (Some model) paths with
        | Error e -> Failed e
        | Ok suites when not suites.Invalid.IsEmpty ->
            finish
                ctx
                {
                    FailOnWarning = false
                    Ci = Findings.NoCi
                    Trx = None
                    Junit = None
                    Force = false
                }
                {
                    Command = "test snapshot"
                    Source = model
                    Against = None
                    Suites = []
                    Invalid = suites.Invalid
                    Elapsed = TimeSpan.Zero
                }
        | Ok suites ->
            let recorded =
                withTarget ctx model (args.TryGetResult SnapshotArgs.Data) (fun source target ->
                    TestRun.snapshot target options suites.Valid
                    |> Result.map (fun result -> source, result))

            let write (s: SuiteSnapshots) : Result<unit, TessError> =
                match s.NewText with
                | Some text when save ->
                    try
                        File.WriteAllText(s.File, text, UTF8Encoding false)
                        Ok()
                    with
                    | :? IOException
                    | :? UnauthorizedAccessException as e -> Error(SaveFailed(s.File, e.Message))
                | _ -> Ok()

            match recorded with
            | Error e -> Failed e
            | Ok(source, result) ->
                match
                    result
                    |> List.fold
                        (fun (state: Result<unit, TessError>) s -> state |> Result.bind (fun () -> write s))
                        (Ok())
                with
                | Error e -> Failed e
                | Ok() ->
                    let changes = result |> List.collect (fun s -> s.Changes |> List.map snd)
                    let n (f: SnapshotChange -> bool) = changes |> List.filter f |> List.length
                    let notRecorded = n _.IsNotRecorded
                    let changed = n (fun c -> c.IsAdded || c.IsChanged || c.IsRemoved)

                    let token (c: SnapshotChange) =
                        match c with
                        | SnapshotChange.Added -> "added"
                        | SnapshotChange.Unchanged -> "unchanged"
                        | SnapshotChange.Changed _ -> "changed"
                        | SnapshotChange.Removed -> "removed"
                        | SnapshotChange.NotRecorded _ -> "notRecorded"

                    if ctx.Json then
                        writeJson
                            ctx.Out
                            (obj [
                                "command", str "test snapshot"
                                "source", str source
                                "saved", bool save
                                "files",
                                arr [
                                    for s in result ->
                                        obj [
                                            "suite", str s.Suite.File
                                            "file", str s.File
                                            "changed", bool s.NewText.IsSome
                                            "snapshots",
                                            arr [
                                                for (name, change) in s.Changes ->
                                                    obj [
                                                        yield "name", str name
                                                        yield "change", str (token change)

                                                        match change with
                                                        | SnapshotChange.Changed differences ->
                                                            yield "differences", arr (differences |> List.map str)
                                                        | SnapshotChange.NotRecorded outcome ->
                                                            yield "outcome", str (TestOutcome.token outcome)

                                                            match outcomeMessage outcome with
                                                            | Some message -> yield "message", str message
                                                            | None -> ()
                                                        | _ -> ()
                                                    ]
                                            ]
                                        ]
                                ]
                                "summary",
                                obj [
                                    "added", num (n _.IsAdded)
                                    "changed", num (n _.IsChanged)
                                    "removed", num (n _.IsRemoved)
                                    "unchanged", num (n _.IsUnchanged)
                                    "notRecorded", num notRecorded
                                ]
                            ])
                    else
                        for s in result do
                            ctx.Out.WriteLine s.File

                            for (name, change) in s.Changes do
                                let mark, lines =
                                    match change with
                                    | SnapshotChange.Added -> "+", []
                                    | SnapshotChange.Unchanged -> "=", []
                                    | SnapshotChange.Changed differences -> "~", detailText differences
                                    | SnapshotChange.Removed -> "-", [ "no snapshot test has this name any more" ]
                                    | SnapshotChange.NotRecorded outcome ->
                                        "!",
                                        [
                                            "not recorded ("
                                            + TestOutcome.token outcome
                                            + "): "
                                            + defaultArg (outcomeMessage outcome) ""
                                        ]

                                ctx.Out.WriteLine($"  {mark} {name}")

                                for line in lines do
                                    ctx.Out.WriteLine($"      {line}")

                            ctx.Out.WriteLine()

                        if result.IsEmpty then
                            ctx.Out.WriteLine "No snapshot tests (tests with `snapshot: true`) in these suites."
                        elif changed = 0 then
                            ctx.Out.WriteLine "The snapshots are up to date."
                        elif save then
                            ctx.Out.WriteLine(("Recorded: " + plural changed "snapshot" "snapshots" + " changed."))
                        else
                            ctx.Out.WriteLine(
                                "Dry run: "
                                + plural changed "snapshot" "snapshots"
                                + " would change (a difference reads: now, expected what is recorded). Use --save to record."
                            )

                        if notRecorded > 0 then
                            ctx.Err.WriteLine(
                                "tess: "
                                + plural notRecorded "snapshot" "snapshots"
                                + " could not be recorded (see above)."
                            )

                    if notRecorded > 0 then DoneWith ExitCode.findings else Done

// ---- init ------------------------------------------------------------------------------------------

/// Text as a YAML double-quoted scalar (JSON's escapes are YAML's).
let private yamlText (s: string) =
    JsonSerializer.Serialize(s, jsonOptions)

/// A suite with a test per measure, as YAML text.
let private scaffold
    (db: Microsoft.AnalysisServices.Tabular.Database)
    (data: string option)
    (snapshots: bool)
    : string * int =
    let sb = StringBuilder()
    let line (s: string) = sb.Append(s).Append('\n') |> ignore

    let measures = [
        for t in db.Model.Tables do
            for m in t.Measures -> t.Name, m.Name
    ]

    line "# Tests for the model's measures, written by `tess test init`."

    if snapshots then
        line "# Each test compares the measure with its recorded value: record them with"
        line "#   tess test snapshot <model> --save"
    else
        line "# Each test is pending: set what the measure must be in `expect`, then remove `pending`."

    line "# The format is described in tessera's README, under \"Testing a model\"."
    line ("suite: " + yamlText (db.Name + " measures"))

    match data with
    | Some d -> line $"data: {yamlText d}"
    | None ->
        line
            "# data: ../fixtures        # the fixture directory to evaluate over (see `tess data init`), or pass --data"

    if measures.IsEmpty then line "tests: []" else line "tests:"

    for (table, name) in measures do
        line ""
        line $"  - name: {yamlText name}"

        line
            $"    description: {yamlText (
                                    ObjectRef.format {
                                        Kind = ObjectKind.Measure
                                        Path = [ table; name ]
                                    }
                                )}"

        line $"    measure: {yamlText name}"

        if snapshots then
            line "    expect: { snapshot: true }"
        else
            line "    pending: true"
            line "    expect: 0"

    sb.ToString(), measures.Length

let private init (ctx: Ctx) (args: ParseResults<InitArgs>) =
    let model = args.GetResult InitArgs.Model

    let output =
        match args.TryGetResult InitArgs.Output with
        | Some path -> Path.GetFullPath path
        | None -> Path.Combine(suiteDirectory model, "measures.test.yaml")

    if
        not (TestSuite.isSuiteFile output)
        || output.EndsWith(".json", StringComparison.OrdinalIgnoreCase)
    then
        UsageError "--output must be a .test.yaml (or .test.yml) file: that is how suites are found"
    elif File.Exists output || Directory.Exists output then
        Failed(SaveFailed(output, "it already exists, and `test init` never overwrites a suite: give another --output"))
    else
        match Source.load ctx model with
        | Error e -> Failed e
        | Ok(_, db) ->
            // The suite refers to its data relative to itself, so the two can move together.
            let data =
                args.TryGetResult InitArgs.Data
                |> Option.map (fun d ->
                    Path.GetRelativePath(Path.GetDirectoryName output, Path.GetFullPath d).Replace('\\', '/'))

            let text, tests = scaffold db data (args.Contains InitArgs.Snapshot)

            if tests = 0 then
                Failed(SaveFailed(output, "the model has no measures to write tests for"))
            else

                try
                    Directory.CreateDirectory(Path.GetDirectoryName output) |> ignore
                    // CreateNew: never over a file that appeared in the meantime.
                    use stream = new FileStream(output, FileMode.CreateNew, FileAccess.Write)
                    use writer = new StreamWriter(stream, UTF8Encoding false)
                    writer.Write text
                    writer.Flush()

                    if ctx.Json then
                        writeJson ctx.Out (obj [ "command", str "test init"; "file", str output; "tests", num tests ])
                    else
                        ctx.Out.WriteLine(("Wrote " + output + ": " + plural tests "test" "tests" + "."))

                    Done
                with
                | :? IOException
                | :? UnauthorizedAccessException as e -> Failed(SaveFailed(output, e.Message))

// ---- list ------------------------------------------------------------------------------------------

let private list (ctx: Ctx) (args: ParseResults<ListArgs>) =
    let chosen =
        select (parseList (args.GetResults ListArgs.Filter)) (parseList (args.GetResults ListArgs.Tag))

    match findSuites None (args.GetResults ListArgs.Paths |> List.concat) with
    | Error e -> Failed e
    | Ok suites ->
        let kind (t: TestCase) =
            match t.Body with
            | TestBody.Query _ -> "query"
            | TestBody.Scalar _ -> "value"
            | TestBody.Assert _ -> "assert"

        if ctx.Json then
            writeJson
                ctx.Out
                (obj [
                    "command", str "test list"
                    "suites",
                    arr [
                        for s in suites.Valid ->
                            obj [
                                "name", str s.Name
                                "file", str s.File
                                "tests",
                                arr [
                                    for t in s.Tests |> List.filter chosen ->
                                        obj [
                                            "name", str t.Name
                                            "line", num t.Line
                                            "kind", str (kind t)
                                            "tags", arr (t.Tags |> List.map str)
                                            "skip", optStr t.Skip
                                            "query", str (TestRun.compile t)
                                        ]
                                ]
                            ]
                    ]
                    "invalidSuites",
                    arr [
                        for (file, issues) in suites.Invalid ->
                            obj [
                                "file", str file
                                "validationErrors", arr (issues |> List.map (fun i -> str (SuiteIssue.format i)))
                            ]
                    ]
                ])
        else
            for (file, issues) in suites.Invalid do
                ctx.Out.WriteLine($"{file}: not a valid suite")

                for i in issues do
                    ctx.Out.WriteLine($"  {SuiteIssue.format i}")

                ctx.Out.WriteLine()

            for s in suites.Valid do
                ctx.Out.WriteLine($"{s.Name}  ({s.File})")

                for t in s.Tests |> List.filter chosen do
                    let tags =
                        if t.Tags.IsEmpty then
                            ""
                        else
                            "  [" + String.concat ", " t.Tags + "]"

                    let skip =
                        match t.Skip with
                        | Some reason -> $"  ({reason})"
                        | None -> ""

                    ctx.Out.WriteLine($"  {t.Name}{tags}{skip}")

                ctx.Out.WriteLine()

        if suites.Invalid.IsEmpty then
            Done
        else
            DoneWith ExitCode.findings

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.GetSubCommand() with
    | Run a -> runTests ctx a
    | Init a -> init ctx a
    | Snapshot a -> snapshot ctx a
    | Compare a -> compare ctx a
    | List a -> list ctx a
