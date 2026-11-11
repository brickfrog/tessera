module Tessera.Cli.LintCommand

open System
open System.IO
open Argu
open Tessera
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "check DAX against code actions (unused variables, unqualified columns, IFERROR...) (exit 1: issues); --fix rewrites them."

type Args =
    | [<MainCommand; ExactlyOnce>] Model of model: string
    | Rule of rules: string
    | Min_Severity of level: string
    | Fail_On of level: string
    | Ci of system: string
    | Fix
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only
    | Sarif of file: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path."
            | Rule _ ->
                let ids = Lint.rules |> List.map (fun r -> r.Id) |> String.concat ", "
                $"only these rules (repeatable or comma-separated): {ids}."
            | Min_Severity _ ->
                "only show (or with --fix, only fix) findings this severe or more: error, warning or info (default: info)."
            | Fail_On _ ->
                "exit 1 if any finding is this severe or more, shown or not: error, warning or info (default: error)."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."
            | Fix -> "rewrite the DAX to fix what can be fixed safely (a dry run unless --save or --save-to)."
            | Save -> "with --fix: " + Mutation.Usage.save
            | Save_To _ -> "with --fix: " + Mutation.Usage.saveTo
            | Force -> "with --fix: " + Mutation.Usage.force
            | Stat -> "with --fix: " + Mutation.Usage.stat
            | Name_Only -> "with --fix: " + Mutation.Usage.nameOnly
            | Sarif _ -> Tessera.Cli.Sarif.usage

/// A lint finding in the shared envelope (see Findings.fs).
let toFinding (f: Lint.Finding) : Findings.Finding = {
    ValidateCommand.toFinding (Lint.Finding.toIssue f) with
        Source = "lint"
        RuleName = Some f.Rule.Name
        Category = Some f.Rule.Category
        Fixable = Lint.Finding.fixable f
}

/// A lint rule, for SARIF.
let sarifRule (r: Lint.Rule) : Sarif.Rule = {
    Id = r.Id
    ShortDescription = r.Name
    FullDescription = Some r.Description
    Level =
        Some(
            match r.Severity with
            | Validate.Severity.Error -> Findings.Severity.Error
            | Validate.Severity.Warning -> Findings.Severity.Warning
            | Validate.Severity.Info -> Findings.Severity.Info
        )
    HelpUri = None
    Category = Some r.Category
}

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

/// The aligned validate-style report, plus how many findings --fix would fix.
let lintText (out: TextWriter) (findings: Lint.Finding list) =
    ValidateCommand.validateText out (findings |> List.map Lint.Finding.toIssue)

    match findings |> List.filter Lint.Finding.fixable |> List.length with
    | 0 -> ()
    | n -> out.WriteLine($"{n} can be fixed with --fix.")

/// The findings envelope, plus "fixable": how many findings --fix would fix.
let lintJson (duration: TimeSpan) (findings: Lint.Finding list) =
    Findings.envelope "lint" duration (findings |> List.map toFinding) [
        "fixable", num (findings |> List.filter Lint.Finding.fixable |> List.length)
    ]

/// What --fix did, for the editing pipeline's notes and warnings.
let private effect (result: Lint.FixResult) : Tessera.Mutation.Effect =
    let fixedCount = result.Fixes |> List.sumBy (fun f -> f.Fixed |> List.sumBy snd)

    let byRule =
        result.Fixes
        |> List.collect (fun f -> f.Fixed)
        |> List.groupBy fst
        |> List.map (fun (id, xs) -> id, xs |> List.sumBy snd)
        |> List.sortBy (fun (id, _) -> Lint.rules |> List.findIndex (fun r -> r.Id = id))
        |> List.map (fun (id, n) -> $"{id}: {n}")
        |> String.concat ", "

    {
        Tessera.Mutation.Effect.none with
            Notes = [
                if result.Fixes.IsEmpty then
                    "Nothing to fix."
                else
                    $"""Fixed {plural fixedCount "finding"} in {plural result.Fixes.Length "expression"} ({byRule})."""
            ]
            Warnings = [
                match result.Remaining.Length with
                | 0 -> ()
                | n -> $"""{plural n "finding"} can't be fixed automatically; `tess lint` lists them"""

                for s in result.Unsettled do
                    $"{ObjectRef.format s.Owner.Ref} {s.Property}: the fixes didn't settle, so it was left unchanged"

                if
                    result.Fixes
                    |> List.exists (fun f -> f.Fixed |> List.exists (fun (id, _) -> id = "DI014"))
                then
                    "DI014: DIVIDE returns the alternate result for a zero or blank divisor, where / gave infinity, "
                    + "and doesn't catch errors raised by its operands; leave it out with --rule if that matters"
            ]
    }

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath = args.GetResult Args.Model
    let ruleIds = parseList (args.GetResults Args.Rule)
    let known = Lint.rules |> List.map (fun r -> r.Id) |> String.concat ", "
    let fixing = args.Contains Args.Fix

    let selected =
        match ruleIds |> List.tryFind (fun id -> (Lint.tryRule id).IsNone) with
        | Some bad -> Error $"unknown rule '{bad}' (known: {known})"
        | None -> Ok(ruleIds |> List.choose Lint.tryRule |> List.map (fun r -> r.Id) |> Set.ofList)

    let options =
        selected
        |> Result.bind (fun selected ->
            ValidateCommand.severityArg "--min-severity" (args.TryGetResult Args.Min_Severity) Validate.Severity.Info
            |> Result.bind (fun minimum ->
                ValidateCommand.severityArg "--fail-on" (args.TryGetResult Args.Fail_On) Validate.Severity.Error
                |> Result.bind (fun failOn ->
                    ValidateCommand.ciArg (args.TryGetResult Args.Ci)
                    |> Result.map (fun ci -> selected, minimum, failOn, ci))))

    let isSelected (selected: Set<string>) (f: Lint.Finding) =
        selected.IsEmpty || selected.Contains f.Rule.Id

    let editSwitches =
        [
            args.Contains Args.Save
            args.Contains Args.Save_To
            args.Contains Args.Force
            args.Contains Args.Stat
            args.Contains Args.Name_Only
        ]
        |> List.exists id

    match options with
    | Error message -> UsageError message
    | Ok _ when not fixing && editSwitches -> UsageError "--save, --save-to, --force, --stat and --name-only need --fix"
    | Ok _ when fixing && (args.Contains Args.Fail_On || args.Contains Args.Ci) ->
        UsageError "--fail-on and --ci report findings; they don't combine with --fix"
    | Ok _ when fixing && args.Contains Args.Sarif ->
        UsageError "--sarif reports findings; it doesn't combine with --fix"
    | Ok(selected, minimum, _, _) when fixing ->
        let opts =
            Mutation.options
                (args.Contains Args.Save)
                (args.TryGetResult Args.Save_To)
                (args.Contains Args.Force)
                (args.Contains Args.Stat)
                (args.Contains Args.Name_Only)

        match opts with
        | Error message -> UsageError message
        | Ok opts ->
            Mutation.run ctx "lint" opts modelPath (fun db ->
                let result =
                    Lint.fix db (fun f -> isSelected selected f && Validate.Severity.atLeast minimum f.Rule.Severity)

                Lint.apply result
                Ok(effect result))
    | Ok(selected, minimum, failOn, ci) ->
        let clock = Diagnostics.Stopwatch.StartNew()

        let linted =
            Source.load ctx modelPath
            |> Result.bind (fun (source, db) ->
                let findings = Lint.lint db |> List.filter (isSelected selected)

                let shown =
                    findings
                    |> List.filter (fun f -> Validate.Severity.atLeast minimum f.Rule.Severity)

                match args.TryGetResult Args.Sarif with
                | Some path ->
                    let ruleSet =
                        Lint.rules
                        |> List.filter (fun r -> selected.IsEmpty || selected.Contains r.Id)
                        |> List.map sarifRule

                    ValidateCommand.writeSarif path modelPath source ruleSet (shown |> List.map toFinding)
                    |> Result.map (fun () -> findings, shown)
                | None -> Ok(findings, shown))

        match linted with
        | Error e -> Failed e
        | Ok(findings, shown) ->

            if ctx.Json then
                writeJson ctx.Out (lintJson clock.Elapsed shown)
            else
                lintText ctx.Out shown

            Findings.annotate ci ctx.Err (shown |> List.map toFinding)

            // --fail-on looks at every selected finding, so hiding warnings can't turn a failing check green.
            if
                findings
                |> List.exists (fun f -> Validate.Severity.atLeast failOn f.Rule.Severity)
            then
                DoneWith ExitCode.findings
            else
                Done
