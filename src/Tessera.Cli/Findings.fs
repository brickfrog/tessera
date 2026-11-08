/// The findings envelope shared by `validate` and `bpa` (and later `test`, `schemacheck`): the same
/// shape the Tabular Editor CLI uses, so tooling written for one reads the other.
///   { command, durationMs, summary {errors, warnings, info, total}, findings [...], ...extra }
/// Optional finding keys are left out rather than written as null.
module Tessera.Cli.Findings

open System
open System.IO
open System.Text.Json.Nodes
open Tessera
open Tessera.Cli.Output

[<RequireQualifiedAccess>]
type Severity =
    | Error
    | Warning
    | Info

module Severity =
    let token =
        function
        | Severity.Error -> "error"
        | Severity.Warning -> "warning"
        | Severity.Info -> "info"

/// Where in an expression a finding is.
type Position = {
    /// Property path, as `get -p` takes it (e.g. "expression").
    Property: string
    Line: int
    Column: int option
}

type Finding = {
    Severity: Severity
    /// The command that produced it: "validate", "bpa", ...
    Source: string
    /// Stable identifier: a validate rule id or a BPA rule ID.
    Code: string
    Message: string
    /// The object's display name.
    Object: string
    /// Closed vocabulary (see objectType).
    ObjectType: string
    /// The object's tessera reference (pastes into any command).
    ObjectPath: string option
    Position: Position option
    RuleName: string option
    Category: string option
    /// Whether tess can fix it automatically (no command applies fixes yet).
    Fixable: bool
}

/// PascalCase object types, one per ObjectKind.
let objectType = ObjectKind.typeName

/// A finding about an object, with its name, type and path filled in from the reference.
let about (r: ObjectRef) severity source code message = {
    Severity = severity
    Source = source
    Code = code
    Message = message
    Object = ObjectRef.name r
    ObjectType = objectType r.Kind
    ObjectPath = Some(ObjectRef.format r)
    Position = None
    RuleName = None
    Category = None
    Fixable = false
}

type Summary = {
    Errors: int
    Warnings: int
    Info: int
    Total: int
}

let summarize (findings: Finding list) =
    let count s =
        findings |> List.filter (fun f -> f.Severity = s) |> List.length

    {
        Errors = count Severity.Error
        Warnings = count Severity.Warning
        Info = count Severity.Info
        Total = findings.Length
    }

/// "2 errors, 1 warning, 3 info"; zero counts left out; "" when there are none.
let summaryText (s: Summary) =
    [
        s.Errors, "error", "errors"
        s.Warnings, "warning", "warnings"
        s.Info, "info", "info"
    ]
    |> List.filter (fun (n, _, _) -> n > 0)
    |> List.map (fun (n, one, many) -> if n = 1 then $"1 {one}" else $"{n} {many}")
    |> String.concat ", "

let summaryJson (s: Summary) =
    obj [
        "errors", num s.Errors
        "warnings", num s.Warnings
        "info", num s.Info
        "total", num s.Total
    ]

let findingJson (f: Finding) : JsonNode =
    obj [
        yield "severity", str (Severity.token f.Severity)
        yield "source", str f.Source
        yield "code", str f.Code
        yield "message", str f.Message
        yield "object", str f.Object
        yield "objectType", str f.ObjectType

        match f.ObjectPath with
        | Some p -> yield "objectPath", str p
        | None -> ()

        match f.Position with
        | Some p ->
            yield
                "expressionPosition",
                obj [
                    yield "property", str p.Property
                    yield "lineNumber", num p.Line

                    match p.Column with
                    | Some c -> yield "column", num c
                    | None -> ()
                ]
        | None -> ()

        match f.RuleName with
        | Some n -> yield "ruleName", str n
        | None -> ()

        match f.Category with
        | Some c -> yield "category", str c
        | None -> ()

        yield "fixable", bool f.Fixable
    ]

/// The full envelope. `extra` adds command-specific top-level keys (e.g. "valid").
let envelope (command: string) (duration: TimeSpan) (findings: Finding list) (extra: (string * JsonNode) list) =
    obj [
        yield "command", str command
        yield "durationMs", num (int duration.TotalMilliseconds)
        yield "summary", summaryJson (summarize findings)
        yield "findings", arr (findings |> List.map findingJson)
        yield! extra
    ]

// ---- CI annotations ---------------------------------------------------------------------------

type Ci =
    | NoCi
    | GitHub
    | AzureDevOps

module Ci =
    let tryParse (s: string) =
        match s.Trim().ToLowerInvariant() with
        | "github"
        | "gh" -> Some GitHub
        | "azdo"
        | "azure-devops"
        | "vsts" -> Some AzureDevOps
        | "none" -> Some NoCi
        | _ -> None

    let names = "github (gh), azdo (azure-devops, vsts), none"

/// GitHub workflow commands: % CR LF must be escaped in the message, and also , : in properties.
let private escapeGitHub (s: string) =
    s.Replace("%", "%25").Replace("\r", "%0D").Replace("\n", "%0A")

let private escapeGitHubProperty (s: string) =
    (escapeGitHub s).Replace(":", "%3A").Replace(",", "%2C")

/// Azure DevOps logging commands: one line; ; and ] end a property.
let private oneLine (s: string) =
    s.Replace("\r\n", " ").Replace("\n", " ").Replace("\r", " ")

let private escapeAzdoProperty (s: string) =
    (oneLine s).Replace(";", "%3B").Replace("]", "%5D")

let private located (f: Finding) =
    match f.ObjectPath with
    | Some p -> $"{p}: {f.Message}"
    | None -> f.Message

/// One annotation line per finding, for the CI system's log (write to stderr). Info findings are
/// notices on GitHub and plain lines on Azure DevOps, so info-only runs stay green there.
let annotate (ci: Ci) (out: TextWriter) (findings: Finding list) =
    for f in findings do
        match ci with
        | NoCi -> ()
        | GitHub ->
            let level =
                match f.Severity with
                | Severity.Error -> "error"
                | Severity.Warning -> "warning"
                | Severity.Info -> "notice"

            out.WriteLine($"::{level} title={escapeGitHubProperty f.Code}::{escapeGitHub (located f)}")
        | AzureDevOps ->
            match f.Severity with
            | Severity.Info -> out.WriteLine($"{f.Code}: {oneLine (located f)}")
            | s ->
                let kind = if s = Severity.Error then "error" else "warning"

                out.WriteLine($"##vso[task.logissue type={kind};code={escapeAzdoProperty f.Code}]{oneLine (located f)}")
