module Tessera.Cli.BpaCommand

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json.Nodes
open System.Xml.Linq
open Argu
open Tessera
open Tessera.BpaRules
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "check the model against Best Practice Analyzer rules (exit 1: findings)."

type Args =
    | [<MainCommand; ExactlyOnce>] Model of path: string
    | [<AltCommandLine("-r")>] Rules of file: string
    | No_Default_Rules
    | No_Model_Rules
    | Min_Severity of level: int
    | [<AltCommandLine("-c")>] Category of category: string
    | List_Rules
    | Trx of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."
            | Rules _ -> "a Tabular Editor BPA rule file (JSON) to run as well (repeatable)."
            | No_Default_Rules -> "don't run tessera's built-in rules."
            | No_Model_Rules -> "ignore rules stored in the model's annotations."
            | Min_Severity _ -> "only run rules of at least this severity (1 info, 2 warning, 3 error)."
            | Category _ -> "only run rules in this category (repeatable or comma-separated)."
            | List_Rules -> "list the rules that would run, and whether each is supported, instead of running them."
            | Trx _ -> "also write the results as a VSTest .trx file (one test per rule), e.g. for Azure DevOps."

let private severityName (s: int) =
    if s >= 3 then "error"
    elif s = 2 then "warning"
    else "info"

let private scopesText (r: Rule) =
    (r.Scopes |> List.map Scope.name) @ r.UnknownScopes |> String.concat ", "

/// Unsupported or failed rules: (rule, stage, error).
let private problems (results: Bpa.RuleResult list) =
    results
    |> List.choose (fun r ->
        match r.Status with
        | Bpa.Unsupported e -> Some(r.Rule, "compile", e)
        | Bpa.Failed e -> Some(r.Rule, "evaluate", e)
        | _ -> None)

let private findingJson (f: Bpa.Finding) =
    obj [
        "ruleId", str f.Rule.Id
        "ruleName", str f.Rule.Name
        "category", str f.Rule.Category
        "severity", num f.Rule.Severity
        "ref", str (ObjectRef.format f.Ref)
        "kind", str (ObjectKind.token f.Ref.Kind)
        "object", str f.ObjectName
        "objectType", str f.ObjectType
    ]

let private ruleJson (r: Rule) (support: Result<unit, string>) =
    obj [
        "id", str r.Id
        "name", str r.Name
        "category", str r.Category
        "severity", num r.Severity
        "scope", str (scopesText r)
        "expression", str r.Expression
        "description", optStr r.Description
        "source", str (RuleSource.describe r.Source)
        "supported", bool (Result.isOk support)
        "error",
        (match support with
         | Error e -> str e
         | Ok() -> null)
    ]

type private Summary = {
    Rules: int
    Evaluated: int
    Findings: Bpa.Finding list
    Ignored: int
    Unsupported: int
    Skipped: int
}

let private summarize (results: Bpa.RuleResult list) = {
    Rules = results.Length
    Evaluated = results |> List.filter (fun r -> r.Status = Bpa.Evaluated) |> List.length
    Findings =
        results
        |> List.collect (fun r -> r.Findings)
        |> List.sortBy (fun f -> -f.Rule.Severity)
    Ignored = results |> List.sumBy (fun r -> r.Ignored.Length)
    Unsupported = (problems results).Length
    Skipped =
        results
        |> List.filter (fun r ->
            match r.Status with
            | Bpa.Skipped _
            | Bpa.Disabled -> true
            | _ -> false)
        |> List.length
}

let private countSeverity (s: Summary) (pred: int -> bool) =
    s.Findings |> List.filter (fun f -> pred f.Rule.Severity) |> List.length

let private resultsJson (results: Bpa.RuleResult list) (warnings: string list) =
    let s = summarize results

    obj [
        "findings", arr (s.Findings |> List.map findingJson)
        "ignored", arr (results |> List.collect (fun r -> r.Ignored) |> List.map findingJson)
        "unsupported",
        arr [
            for rule, stage, error in problems results ->
                obj [
                    "ruleId", str rule.Id
                    "ruleName", str rule.Name
                    "stage", str stage
                    "error", str error
                ]
        ]
        "skipped",
        arr [
            for r in results do
                match r.Status with
                | Bpa.Skipped level ->
                    obj [
                        "ruleId", str r.Rule.Id
                        "ruleName", str r.Rule.Name
                        "reason", str $"needs compatibility level {level}"
                    ]
                | Bpa.Disabled ->
                    obj [
                        "ruleId", str r.Rule.Id
                        "ruleName", str r.Rule.Name
                        "reason", str "ignored by the model's annotation"
                    ]
                | _ -> ()
        ]
        "warnings", arr (warnings |> List.map str)
        "summary",
        obj [
            "rules", num s.Rules
            "evaluated", num s.Evaluated
            "findings", num s.Findings.Length
            "errors", num (countSeverity s (fun x -> x >= 3))
            "warnings", num (countSeverity s (fun x -> x = 2))
            "info", num (countSeverity s (fun x -> x <= 1))
            "ignored", num s.Ignored
            "unsupported", num s.Unsupported
            "skipped", num s.Skipped
        ]
    ]

let private plural (n: int) (word: string) =
    if n = 1 then $"{n} {word}" else $"{n} {word}s"

/// severity  ref  ruleId  rule name, then a summary line. Problems with rules go to stderr.
let private resultsText (ctx: Ctx) (results: Bpa.RuleResult list) (warnings: string list) =
    for w in warnings do
        ctx.Err.WriteLine $"tess: bpa: warning: {w}"

    for rule, stage, error in problems results do
        let what = if stage = "compile" then "not supported" else "failed"
        ctx.Err.WriteLine $"tess: bpa: rule {rule.Id} ({rule.Name}) {what}: {error}"

    let s = summarize results

    let width =
        s.Findings
        |> List.map (fun f -> (ObjectRef.format f.Ref).Length)
        |> List.fold max 0

    for f in s.Findings do
        let r = (ObjectRef.format f.Ref).PadRight width
        ctx.Out.WriteLine $"{(severityName f.Rule.Severity).PadRight 7}  {r}  {f.Rule.Id}  {f.Rule.Name}"

    let bySeverity =
        [
            countSeverity s (fun x -> x >= 3), "error"
            countSeverity s (fun x -> x = 2), "warning"
            countSeverity s (fun x -> x <= 1), "info"
        ]
        |> List.filter (fun (n, _) -> n > 0)
        |> List.map (fun (n, w) -> if w = "info" then $"{n} info" else plural n w)
        |> String.concat ", "

    let parts = [
        if s.Findings.IsEmpty then
            "No findings"
        else
            let findings = plural s.Findings.Length "finding"
            $"{findings} ({bySeverity})"
        let checkedRules = plural s.Evaluated "rule"
        $"{checkedRules} checked"

        if s.Ignored > 0 then
            $"{s.Ignored} ignored by annotations"

        if s.Unsupported > 0 then
            let unsupported = plural s.Unsupported "rule"
            $"{unsupported} unsupported"

        if s.Skipped > 0 then
            $"{s.Skipped} skipped"
    ]

    ctx.Out.WriteLine(String.concat "; " parts)

let private rulesText (out: TextWriter) (rules: (Rule * Result<unit, string>) list) =
    let width = rules |> List.map (fun (r, _) -> r.Id.Length) |> List.fold max 0

    for r, support in rules do
        let status =
            match support with
            | Ok() -> ""
            | Error e -> $"  [unsupported: {e}]"

        out.WriteLine
            $"{r.Id.PadRight width}  {r.Severity}  {r.Category}  {r.Name}  ({scopesText r}; {RuleSource.describe r.Source}){status}"

/// A stable GUID for a string, so a rule keeps its test id across runs.
let private guidOf (s: string) =
    Guid(MD5.HashData(Encoding.UTF8.GetBytes s))

/// The results as a VSTest results file: one test per rule.
let trx (modelPath: string) (started: DateTime) (results: Bpa.RuleResult list) =
    let ns = XNamespace.Get "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"

    let el (name: string) (content: obj list) =
        XElement(ns + name, Array.ofList content)

    let at (name: string) (value: obj) =
        XAttribute(XName.Get name, value) :> obj

    let listId = "8c84fa94-04c1-424b-9868-57a2d4851a1d"
    let unitTestType = "13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b"
    let time (d: DateTime) = d.ToString("o")
    let finished = DateTime.Now

    let outcome (r: Bpa.RuleResult) =
        match r.Status with
        | Bpa.Evaluated when r.Findings.IsEmpty -> "Passed", None
        | Bpa.Evaluated ->
            let objects =
                r.Findings
                |> List.map (fun f -> $"{f.ObjectName} ({f.ObjectType})")
                |> String.concat "\n"

            let count = plural r.Findings.Length "object"
            "Failed", Some($"{count} in violation of rule \"{r.Rule.Name}\"", objects)
        | Bpa.Unsupported e -> "Inconclusive", Some($"Rule not supported: {e}", "")
        | Bpa.Failed e -> "Inconclusive", Some($"Rule failed: {e}", "")
        | Bpa.Skipped level -> "NotExecuted", Some($"Rule needs compatibility level {level}", "")
        | Bpa.Disabled -> "NotExecuted", Some("Rule ignored by the model's annotation", "")

    let rows =
        results
        |> List.map (fun r ->
            let testId = guidOf ("test:" + r.Rule.Id)
            let executionId = guidOf ("execution:" + r.Rule.Id + time started)
            r, testId, executionId, outcome r)

    let count o =
        rows |> List.filter (fun (_, _, _, (x, _)) -> x = o) |> List.length

    let failed = count "Failed"

    let doc: XDocument =
        XDocument(
            XDeclaration("1.0", "utf-8", null),
            box (
                el "TestRun" [
                    at "id" (Guid.NewGuid())
                    at "name" $"tess bpa {Path.GetFileName modelPath}"
                    el "Times" [
                        at "creation" (time started)
                        at "queuing" (time started)
                        at "start" (time started)
                        at "finish" (time finished)
                    ]
                    el "Results" [
                        for r, testId, executionId, (o, info) in rows ->
                            el "UnitTestResult" [
                                at "executionId" executionId
                                at "testId" testId
                                at "testName" $"{r.Rule.Id}: {r.Rule.Name}"
                                at "computerName" Environment.MachineName
                                at "duration" "00:00:00"
                                at "startTime" (time started)
                                at "endTime" (time finished)
                                at "testType" unitTestType
                                at "outcome" o
                                at "testListId" listId
                                match info with
                                | Some(message, details) ->
                                    el "Output" [
                                        el "ErrorInfo" [ el "Message" [ message ]; el "StackTrace" [ details ] ]
                                    ]
                                | None -> ()
                            ]
                    ]
                    el "TestDefinitions" [
                        for r, testId, executionId, _ in rows ->
                            el "UnitTest" [
                                at "name" $"{r.Rule.Id}: {r.Rule.Name}"
                                at "storage" modelPath
                                at "id" testId
                                el "Execution" [ at "id" executionId ]
                                el "TestMethod" [
                                    at "codeBase" modelPath
                                    at "adapterTypeName" "executor://tessera/bpa"
                                    at "className" (if r.Rule.Category = "" then "BPA" else r.Rule.Category)
                                    at "name" r.Rule.Id
                                ]
                            ]
                    ]
                    el "TestEntries" [
                        for _, testId, executionId, _ in rows ->
                            el "TestEntry" [ at "testId" testId; at "executionId" executionId; at "testListId" listId ]
                    ]
                    el "TestLists" [
                        el "TestList" [ at "name" "Results Not in a List"; at "id" listId ]
                        el "TestList" [
                            at "name" "All Loaded Results"
                            at "id" "19431567-8539-422a-85d7-44ee4e166bda"
                        ]
                    ]
                    el "ResultSummary" [
                        at "outcome" (if failed > 0 then "Failed" else "Completed")
                        el "Counters" [
                            at "total" rows.Length
                            at "executed" (rows.Length - count "NotExecuted")
                            at "passed" (count "Passed")
                            at "failed" failed
                            at "error" 0
                            at "timeout" 0
                            at "aborted" 0
                            at "inconclusive" (count "Inconclusive")
                            at "passedButRunAborted" 0
                            at "notRunnable" 0
                            at "notExecuted" (count "NotExecuted")
                            at "disconnected" 0
                            at "warning" 0
                            at "completed" 0
                            at "inProgress" 0
                            at "pending" 0
                        ]
                    ]
                ]
            )
        )

    doc.Declaration.ToString() + "\n" + doc.ToString()

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let started = DateTime.Now
    let modelPath = args.GetResult Args.Model
    let categories = parseList (args.GetResults Args.Category) |> Set.ofList
    let minSeverity = args.TryGetResult Args.Min_Severity |> Option.defaultValue 0

    let opts: Bpa.RuleOptions = {
        BuiltIn = not (args.Contains Args.No_Default_Rules)
        Files = args.GetResults Args.Rules
        ModelRules = not (args.Contains Args.No_Model_Rules)
    }

    let selected (r: Rule) =
        r.Severity >= minSeverity
        && (categories.IsEmpty
            || categories
               |> Set.exists (fun c -> String.Equals(c, r.Category, StringComparison.OrdinalIgnoreCase)))

    match
        ModelIO.loadPath modelPath
        |> Result.bind (fun (source, db) -> Bpa.collectRules source db opts |> Result.map (fun x -> db, x))
    with
    | Error e -> Failed e
    | Ok(db, (rules, warnings)) ->
        let rules = rules |> List.filter selected

        if args.Contains Args.List_Rules then
            let checkedRules = rules |> List.map (fun r -> r, Bpa.compile r)

            if ctx.Json then
                writeJson ctx.Out (arr [ for r, support in checkedRules -> ruleJson r support ])
            else
                rulesText ctx.Out checkedRules

            Done
        else
            let results = Bpa.analyze db rules

            let written =
                match args.TryGetResult Args.Trx with
                | None -> Ok()
                | Some path ->
                    try
                        File.WriteAllText(path, trx modelPath started results)
                        Ok()
                    with e ->
                        Error(SaveFailed(path, e.Message))

            match written with
            | Error e -> Failed e
            | Ok() ->
                if ctx.Json then
                    writeJson ctx.Out (resultsJson results warnings)
                else
                    resultsText ctx results warnings

                if results |> List.exists (fun r -> not r.Findings.IsEmpty) then
                    DoneWith ExitCode.findings
                else
                    Done
