module Tessera.Cli.ValidateCommand

open System
open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Validate
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "check for broken references, circular references and other structural problems (exit 1: issues)."

type Args =
    | [<MainCommand; ExactlyOnce>] Model of model: string
    | Min_Severity of level: string
    | Fail_On of level: string
    | Rule of rules: string
    | Ci of system: string
    | No_Group

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path."
            | Min_Severity _ -> "only show issues this severe or more: error, warning or info (default: info)."
            | Fail_On _ ->
                "exit 1 if any issue is this severe or more, shown or not: error, warning or info (default: error)."
            | Rule _ ->
                let ids = rules |> List.map fst |> String.concat ", "
                $"only these rules (repeatable or comma-separated): {ids}."
            | Ci _ -> $"also write CI annotations to stderr: {Findings.Ci.names}."
            | No_Group -> "list every issue on its own, instead of folding issues with one root cause into the first."

/// "2 errors, 1 warning"; zero counts are left out.
let private summaryText (issues: Issue list) =
    let count s =
        issues |> List.filter (fun i -> i.Severity = s) |> List.length

    let parts = [
        for s in Severity.all do
            match count s with
            | 0 -> ()
            | 1 -> $"1 {Severity.token s}"
            | n -> $"{n} {Severity.token s}s"
    ]

    if parts.IsEmpty then
        "No issues."
    else
        String.concat ", " parts

let private location (i: Issue) =
    match i.Property, i.Line with
    | Some p, Some l -> $"{p}:{l}"
    | Some p, None -> p
    | None, _ -> ""

/// An issue as a finding in the shared envelope (see Findings.fs).
let toFinding (i: Issue) : Findings.Finding = {
    Findings.about
        i.Ref
        (match i.Severity with
         | Severity.Error -> Findings.Severity.Error
         | Severity.Warning -> Findings.Severity.Warning
         | Severity.Info -> Findings.Severity.Info)
        "validate"
        i.Rule
        i.Message with
        Position =
            match i.Property, i.Line with
            | Some p, Some l ->
                Some {
                    Property = p
                    Line = l
                    Column = i.Column
                }
            | _ -> None
}

/// The findings envelope, plus "valid": no errors among the issues.
let validateJson (duration: TimeSpan) (issues: Issue list) =
    let findings = issues |> List.map toFinding

    Findings.envelope "validate" duration findings [
        "valid", bool (issues |> List.forall (fun i -> i.Severity <> Severity.Error))
    ]

/// severity  ref  property:line  message  [rule], aligned; then a summary line.
let validateText (out: TextWriter) (issues: Issue list) =
    let width f =
        issues |> List.map (fun i -> String.length (f i)) |> List.fold max 0

    let severityWidth = width (fun i -> Severity.token i.Severity)
    let refWidth = width (fun i -> ObjectRef.format i.Ref)
    let locationWidth = width location

    for i in issues do
        let columns = [
            (Severity.token i.Severity).PadRight severityWidth
            (ObjectRef.format i.Ref).PadRight refWidth
            if locationWidth > 0 then
                (location i).PadRight locationWidth
            i.Message
            $"[{i.Rule}]"
        ]

        out.WriteLine(String.concat "  " columns)

    out.WriteLine(summaryText issues)

/// "+2 more for 'Date'[DateKey]" after a root issue; nothing for others.
let private groupSuffix =
    function
    | Root(cause, dependents) -> Some $"+{dependents.Length} more for {cause}"
    | Alone
    | Dependent _ -> None

/// Like validateText, but a group of issues with one root cause shows as its first issue, with a count.
let validateTextGrouped (out: TextWriter) (grouped: (Issue * Grouping) list) =
    let shown =
        grouped
        |> List.filter (fun (_, g) ->
            match g with
            | Dependent _ -> false
            | _ -> true)

    let issues = shown |> List.map fst

    let width f =
        issues |> List.map (fun i -> String.length (f i)) |> List.fold max 0

    let severityWidth = width (fun i -> Severity.token i.Severity)
    let refWidth = width (fun i -> ObjectRef.format i.Ref)
    let locationWidth = width location

    for i, g in shown do
        let columns = [
            (Severity.token i.Severity).PadRight severityWidth
            (ObjectRef.format i.Ref).PadRight refWidth
            if locationWidth > 0 then
                (location i).PadRight locationWidth
            i.Message
            $"[{i.Rule}]"
            yield! Option.toList (groupSuffix g)
        ]

        out.WriteLine(String.concat "  " columns)

    let folded = grouped.Length - shown.Length
    let all = grouped |> List.map fst

    if folded = 0 then
        out.WriteLine(summaryText all)
    else
        out.WriteLine($"{summaryText all} ({folded} grouped under their root cause; --no-group lists them)")

/// The findings envelope with root-cause grouping: every issue stays a finding; a root gets
/// "dependents" (the other findings' objects) and each of those gets "rootCause" (the root's object).
let validateJsonGrouped (duration: TimeSpan) (grouped: (Issue * Grouping) list) =
    let path (i: Issue) = ObjectRef.format i.Ref

    let keys =
        function
        | Alone -> []
        | Root(_, dependents) -> [
            "dependents", arr (dependents |> List.map path |> List.distinct |> List.map str)
          ]
        | Dependent(_, root) -> [ "rootCause", str (path root) ]

    Findings.envelopeWith "validate" duration (grouped |> List.map (fun (i, g) -> toFinding i, keys g)) [
        "valid", bool (grouped |> List.forall (fun (i, _) -> i.Severity <> Severity.Error))
    ]

/// CI annotations for grouped issues: one per root, with the count in its message.
let private groupedAnnotations (grouped: (Issue * Grouping) list) = [
    for i, g in grouped do
        match g, groupSuffix g with
        | Dependent _, _ -> ()
        | _, Some suffix -> {
            toFinding i with
                Message = $"{i.Message} ({suffix})"
          }
        | _, None -> toFinding i
]

let private severityArg (name: string) (value: string option) (fallback: Severity) =
    match value with
    | None -> Ok fallback
    | Some v ->
        match Severity.tryParse v with
        | Some s -> Ok s
        | None -> Error $"{name}: unknown severity '{v}' (known: error, warning, info)"

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath = args.GetResult Args.Model
    let selected = parseList (args.GetResults Args.Rule)
    let known = rules |> List.map fst
    let knownList = String.concat ", " known

    let options =
        match selected |> List.tryFind (fun r -> not (List.contains r known)) with
        | Some bad -> Error $"unknown rule '{bad}' (known: {knownList})"
        | None ->
            severityArg "--min-severity" (args.TryGetResult Args.Min_Severity) Severity.Info
            |> Result.bind (fun minimum ->
                severityArg "--fail-on" (args.TryGetResult Args.Fail_On) Severity.Error
                |> Result.map (fun failOn -> minimum, failOn))

    let ci =
        match args.TryGetResult Args.Ci with
        | None -> Ok Findings.NoCi
        | Some c ->
            Findings.Ci.tryParse c
            |> Option.map Ok
            |> Option.defaultValue (Error $"--ci: unknown CI system '{c}' (known: {Findings.Ci.names})")

    match options, ci with
    | Error message, _
    | _, Error message -> UsageError message
    | Ok(minimum, failOn), Ok ci ->
        let clock = Diagnostics.Stopwatch.StartNew()

        match ModelIO.loadPath modelPath with
        | Error e -> Failed e
        | Ok(_, db) ->
            let g = Deps.build db

            let issues =
                validateGraph g db
                |> List.filter (fun i -> selected.IsEmpty || List.contains i.Rule selected)

            let shown = issues |> List.filter (fun i -> Severity.atLeast minimum i.Severity)

            if args.Contains Args.No_Group then
                if ctx.Json then
                    writeJson ctx.Out (validateJson clock.Elapsed shown)
                else
                    validateText ctx.Out shown

                Findings.annotate ci ctx.Err (shown |> List.map toFinding)
            else
                let grouped = group g shown

                if ctx.Json then
                    writeJson ctx.Out (validateJsonGrouped clock.Elapsed grouped)
                else
                    validateTextGrouped ctx.Out grouped

                Findings.annotate ci ctx.Err (groupedAnnotations grouped)

            // --fail-on looks at every selected issue, so hiding warnings can't turn a failing check green.
            if issues |> List.exists (fun i -> Severity.atLeast failOn i.Severity) then
                DoneWith ExitCode.findings
            else
                Done
