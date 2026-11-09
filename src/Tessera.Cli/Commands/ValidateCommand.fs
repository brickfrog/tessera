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

/// A --min-severity / --fail-on value (shared with `lint`).
let severityArg (name: string) (value: string option) (fallback: Severity) =
    match value with
    | None -> Ok fallback
    | Some v ->
        match Severity.tryParse v with
        | Some s -> Ok s
        | None -> Error $"{name}: unknown severity '{v}' (known: error, warning, info)"

/// A --ci value (shared with `lint`).
let ciArg (value: string option) =
    match value with
    | None -> Ok Findings.NoCi
    | Some c ->
        Findings.Ci.tryParse c
        |> Option.map Ok
        |> Option.defaultValue (Error $"--ci: unknown CI system '{c}' (known: {Findings.Ci.names})")

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

    let ci = ciArg (args.TryGetResult Args.Ci)

    match options, ci with
    | Error message, _
    | _, Error message -> UsageError message
    | Ok(minimum, failOn), Ok ci ->
        let clock = Diagnostics.Stopwatch.StartNew()

        match ModelIO.loadPath modelPath with
        | Error e -> Failed e
        | Ok(_, db) ->
            let issues =
                validate db
                |> List.filter (fun i -> selected.IsEmpty || List.contains i.Rule selected)

            let shown = issues |> List.filter (fun i -> Severity.atLeast minimum i.Severity)

            if ctx.Json then
                writeJson ctx.Out (validateJson clock.Elapsed shown)
            else
                validateText ctx.Out shown

            Findings.annotate ci ctx.Err (shown |> List.map toFinding)

            // --fail-on looks at every selected issue, so hiding warnings can't turn a failing check green.
            if issues |> List.exists (fun i -> Severity.atLeast failOn i.Severity) then
                DoneWith ExitCode.findings
            else
                Done
