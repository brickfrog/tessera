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
    | [<MainCommand>] Model of path: string
    | [<AltCommandLine("-r")>] Rules of file: string
    | [<CustomCommandLine("--no-builtin"); AltCommandLine("--no-default-rules")>] No_Default_Rules
    | No_Model_Rules
    | Min_Severity of level: int
    | [<AltCommandLine("-c")>] Category of category: string
    | List_Rules
    | Trx of path: string
    | Sarif of file: string
    | Fail_On of level: string
    | Ci of system: string
    | Builtin_Only
    | No_Config
    | Allow_External_Rules
    | Path of pattern: string
    | [<CliPrefix(CliPrefix.None); CustomCommandLine("rules")>] Rules_Cmd of ParseResults<BpaRulesCommand.Args>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."
            | Rules _ -> BpaRulesCommand.SourceUsage.rules
            | No_Default_Rules -> BpaRulesCommand.SourceUsage.noBuiltin
            | No_Model_Rules -> BpaRulesCommand.SourceUsage.noModelRules
            | Min_Severity _ -> "only run rules of at least this severity (1 info, 2 warning, 3 error)."
            | Category _ -> "only run rules in this category (repeatable or comma-separated)."
            | List_Rules -> "list the rules that would run, and whether each is supported, instead of running them."
            | Trx _ -> "also write the results as a VSTest .trx file (one test per rule), e.g. for Azure DevOps."
            | Sarif _ -> Tessera.Cli.Sarif.usage
            | Fail_On _ -> "exit 1 if any finding is this severe or more: error, warning or info (default: error)."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."
            | Builtin_Only -> BpaRulesCommand.SourceUsage.builtinOnly
            | No_Config -> BpaRulesCommand.SourceUsage.noConfig
            | Allow_External_Rules -> BpaRulesCommand.SourceUsage.allowExternal
            | Path _ ->
                "only report findings on objects whose reference matches: a glob (`measure:Sales/*`, `*:Sales/*`) "
                + "or a prefix (`column:`); repeatable or comma-separated."
            | Rules_Cmd _ -> "list, show, add or remove rules, and ignore rules on objects: `tess bpa rules --help`."

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

let private severityOf (s: int) =
    if s >= 3 then Findings.Severity.Error
    elif s = 2 then Findings.Severity.Warning
    else Findings.Severity.Info

/// A BPA finding in the shared envelope (see Findings.fs): code = rule ID, message = rule name.
let toFinding (f: Bpa.Finding) : Findings.Finding = {
    Findings.about f.Ref (severityOf f.Rule.Severity) "bpa" f.Rule.Id f.Rule.Name with
        Object = f.ObjectName
        RuleName = Some f.Rule.Name
        Category = Option.ofObj f.Rule.Category |> Option.filter (String.IsNullOrWhiteSpace >> not)
}

let private findingJson (f: Bpa.Finding) = Findings.findingJson (toFinding f)

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

let private resultsJson
    (duration: TimeSpan)
    (model: string)
    (results: Bpa.RuleResult list)
    (collected: Bpa.CollectedRules)
    =
    let s = summarize results

    Findings.envelope "bpa" duration (s.Findings |> List.map toFinding) [
        "model", str model
        "rulesEvaluated", num s.Evaluated
        "violations", num s.Findings.Length
        "ignored", arr (results |> List.collect (fun r -> r.Ignored) |> List.map findingJson)
        "ruleErrors",
        arr [
            for rule, stage, error in problems results ->
                obj [
                    "ruleId", str rule.Id
                    "ruleName", str rule.Name
                    "stage", str stage
                    "error", str error
                ]
        ]
        "skippedRules",
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
        yield! BpaRulesCommand.collectedFields collected
        "rules",
        obj [
            "total", num s.Rules
            "evaluated", num s.Evaluated
            "ignoredFindings", num s.Ignored
            "errors", num s.Unsupported
            "skipped", num s.Skipped
        ]
    ]

let private plural (n: int) (word: string) =
    if n = 1 then $"{n} {word}" else $"{n} {word}s"

/// severity  ref  ruleId  rule name, then a summary line. Problems with rules go to stderr.
let private resultsText (ctx: Ctx) (results: Bpa.RuleResult list) =
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

/// The results as SARIF: the rules that ran, every finding, the findings ignored by annotations
/// as suppressed results, and the rules that couldn't run as notifications.
let sarif (modelPath: string) (source: ModelSource) (results: Bpa.RuleResult list) =
    let index = SourceLocations.build source

    let rules = [
        for r in results ->
            let rule = r.Rule

            {
                Sarif.Rule.Id = rule.Id
                Sarif.Rule.ShortDescription = rule.Name
                Sarif.Rule.FullDescription =
                    rule.Description |> Option.filter (String.IsNullOrWhiteSpace >> not)
                Sarif.Rule.Level = Some(severityOf rule.Severity)
                Sarif.Rule.HelpUri = Sarif.helpUri rule.LearnMoreUrl
                Sarif.Rule.Category = Option.ofObj rule.Category
            }
    ]

    let findings = (summarize results).Findings |> List.map toFinding

    let ignored =
        results
        |> List.collect (fun r -> r.Ignored)
        |> List.map toFinding
        |> Sarif.modelItems index
        |> List.map (fun i -> {
            i with
                Suppressed = Some "ignored by the object's BestPracticeAnalyzer_IgnoreRules annotation"
        })

    let notifications = [
        for rule, stage, error in problems results ->
            {
                Sarif.Notification.RuleId = rule.Id
                Sarif.Notification.Level = Findings.Severity.Warning
                Sarif.Notification.Message =
                    if stage = "compile" then
                        $"rule not supported: {error}"
                    else
                        $"rule failed: {error}"
            }
    ]

    Sarif.log (Sarif.rootFor modelPath) rules (Sarif.modelItems index findings @ ignored) notifications

/// A --path pattern: a glob (* and ?) over the whole reference, or else a prefix of it.
/// Case-insensitive, like names in Tabular.
let private pathMatcher (pattern: string) : ObjectRef -> bool =
    if pattern.Contains '*' || pattern.Contains '?' then
        let rx =
            Text.RegularExpressions.Regex(
                "^"
                + Text.RegularExpressions.Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".")
                + "$",
                Text.RegularExpressions.RegexOptions.IgnoreCase
                ||| Text.RegularExpressions.RegexOptions.CultureInvariant
            )

        fun r -> rx.IsMatch(ObjectRef.format r)
    else
        fun r -> (ObjectRef.format r).StartsWith(pattern, StringComparison.OrdinalIgnoreCase)

let private analyze (ctx: Ctx) (args: ParseResults<Args>) (modelPath: string) =
    let started = DateTime.Now
    let clock = Diagnostics.Stopwatch.StartNew()
    let categories = parseList (args.GetResults Args.Category) |> Set.ofList
    let minSeverity = args.TryGetResult Args.Min_Severity |> Option.defaultValue 0

    let paths = parseList (args.GetResults Args.Path) |> List.map pathMatcher

    let inPaths (f: Bpa.Finding) =
        paths.IsEmpty || paths |> List.exists (fun m -> m f.Ref)

    let opts =
        BpaRulesCommand.ruleOptions ctx {
            Rules = args.GetResults Args.Rules
            NoBuiltin = args.Contains Args.No_Default_Rules
            BuiltinOnly = args.Contains Args.Builtin_Only
            NoModelRules = args.Contains Args.No_Model_Rules
            NoConfig = args.Contains Args.No_Config
            AllowExternal = args.Contains Args.Allow_External_Rules
        }

    let selected (r: Rule) =
        r.Severity >= minSeverity
        && (categories.IsEmpty
            || categories
               |> Set.exists (fun c -> String.Equals(c, r.Category, StringComparison.OrdinalIgnoreCase)))

    let failOn =
        match
            args.TryGetResult Args.Fail_On
            |> Option.map (fun s -> s.Trim().ToLowerInvariant())
        with
        | None
        | Some "error" -> Ok 3
        | Some "warning" -> Ok 2
        | Some "info" -> Ok 1
        | Some other -> Error $"--fail-on: unknown severity '{other}' (known: error, warning, info)"

    let ci =
        match args.TryGetResult Args.Ci with
        | None -> Ok Findings.NoCi
        | Some c ->
            Findings.Ci.tryParse c
            |> Option.map Ok
            |> Option.defaultValue (Error $"--ci: unknown CI system '{c}' (known: {Findings.Ci.names})")

    match failOn, ci, opts with
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> UsageError message
    | Ok failOn, Ok ci, Ok opts ->

        match
            Source.load ctx modelPath
            |> Result.bind (fun (source, db) -> Bpa.collectRules source db opts |> Result.map (fun x -> (source, db), x))
        with
        | Error e -> Failed e
        | Ok((source, db), collected) ->
            BpaRulesCommand.reportCollected ctx "bpa" collected
            let rules = collected.Effective |> List.filter selected

            if args.Contains Args.List_Rules then
                let checkedRules = rules |> List.map (fun r -> r, Bpa.compile r)

                if ctx.Json then
                    writeJson ctx.Out (arr [ for r, support in checkedRules -> ruleJson r support ])
                else
                    rulesText ctx.Out checkedRules

                Done
            else
                let results =
                    Bpa.analyze db rules
                    |> List.map (fun r -> {
                        r with
                            Findings = r.Findings |> List.filter inPaths
                            Ignored = r.Ignored |> List.filter inPaths
                    })

                let written =
                    match args.TryGetResult Args.Trx with
                    | None -> Ok()
                    | Some path ->
                        try
                            File.WriteAllText(path, trx modelPath started results)
                            Ok()
                        with e ->
                            Error(SaveFailed(path, e.Message))

                let written =
                    written
                    |> Result.bind (fun () ->
                        match args.TryGetResult Args.Sarif with
                        | None -> Ok()
                        | Some path -> Sarif.write path (sarif modelPath source results))

                match written with
                | Error e -> Failed e
                | Ok() ->
                    if ctx.Json then
                        writeJson ctx.Out (resultsJson clock.Elapsed modelPath results collected)
                    else
                        resultsText ctx results

                    let findings =
                        results
                        |> List.collect (fun r -> r.Findings)
                        |> List.sortBy (fun f -> -f.Rule.Severity)

                    Findings.annotate ci ctx.Err (findings |> List.map toFinding)

                    if findings |> List.exists (fun f -> f.Rule.Severity >= failOn) then
                        DoneWith ExitCode.findings
                    else
                        Done

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetResult Args.Rules_Cmd, args.GetResults Args.Model with
    | Some sub, [] when args.GetAllResults().Length = 1 -> BpaRulesCommand.run ctx sub
    | Some _, _ ->
        UsageError
            "`tess bpa rules` takes its own options after the subcommand, e.g. `tess bpa rules list <model> --rules x.json`"
    | None, [] -> UsageError(args.Parser.PrintUsage(message = "missing the model path"))
    | None, [ modelPath ] -> analyze ctx args modelPath
    | None, _ -> UsageError "bpa takes one model path"
