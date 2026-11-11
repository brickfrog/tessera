/// Findings as a SARIF 2.1.0 log (Static Analysis Results Interchange Format, OASIS), for code
/// scanning UIs such as GitHub's. One run, one result per finding, with the rules that ran.
///
/// - Locations: the file and line where the object is declared (TMDL file, or the line of its
///   "name" in a .bim), as a URI relative to the repository root (`uriBaseId` SRCROOT); plus a
///   logical location with the object's tessera reference. Where the declaration can't be found,
///   the nearest enclosing object's, else line 1 of the model file, with `exactLocation: false`.
/// - Deterministic: no timestamps, no machine names, no absolute paths for files under the root;
///   results in the command's own (stable) order.
/// - Self-contained: `helpUri` only where a rule already carries a URL (a BPA rule's `LearnMoreUrl`). Nothing in
///   the log needs fetching to make sense.
module Tessera.Cli.Sarif

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open Tessera
open Tessera.Cli.Output

/// The `$schema` the log names: the OASIS 2.1.0 (errata 01) schema's own id.
let schemaUri =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"

/// A rule (SARIF `reportingDescriptor`).
type Rule = {
    Id: string
    /// One line: the rule's name, or its description when it has no name.
    ShortDescription: string
    FullDescription: string option
    /// The severity it reports at, when it has one fixed severity.
    Level: Findings.Severity option
    HelpUri: string option
    Category: string option
}

module Rule =
    let simple id text = {
        Id = id
        ShortDescription = text
        FullDescription = None
        Level = None
        HelpUri = None
        Category = None
    }

/// A finding and where it is: a file location and whether it is the object's own place
/// (false: an enclosing object's, or just the file).
type Item = {
    Finding: Findings.Finding
    Location: (SourceLocations.SourceLocation * bool) option
    /// Suppressed (e.g. a BPA finding ignored by the object's annotation): the justification.
    Suppressed: string option
}

/// What the run reports about its own configuration (e.g. a BPA rule that couldn't be compiled).
type Notification = {
    RuleId: string
    Level: Findings.Severity
    Message: string
}

let private level =
    function
    | Findings.Severity.Error -> "error"
    | Findings.Severity.Warning -> "warning"
    | Findings.Severity.Info -> "note"

let private text (s: string) = obj [ "text", str s ]

/// The repository root for relative URIs: the nearest directory at or above `path` holding `.git`
/// (a directory, or a file in a worktree), else None.
let repositoryRoot (path: string) : string option =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then
            None
        elif
            Directory.Exists(Path.Combine(dir.FullName, ".git"))
            || File.Exists(Path.Combine(dir.FullName, ".git"))
        then
            Some dir.FullName
        else
            up dir.Parent

    let full = Path.GetFullPath path

    let start =
        if Directory.Exists full then
            DirectoryInfo full
        else
            (FileInfo full).Directory

    up start

/// A file as SARIF wants it: relative to `root` with '/' and each segment percent-encoded (and
/// `uriBaseId` SRCROOT), or, outside the root, an absolute file URI.
let artifactLocation (root: string) (file: string) : JsonNode =
    let relative = Path.GetRelativePath(root, Path.GetFullPath file)

    if relative = ".." || relative.StartsWith(".." + string Path.DirectorySeparatorChar) || Path.IsPathRooted relative then
        obj [ "uri", str (Uri(Path.GetFullPath file).AbsoluteUri) ]
    else
        let segments =
            relative.Split([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |])
            |> Array.map Uri.EscapeDataString

        obj [ "uri", str (String.Join("/", segments)); "uriBaseId", str "SRCROOT" ]

let private ruleJson (r: Rule) : JsonNode =
    let full = r.FullDescription |> Option.defaultValue r.ShortDescription

    obj [
        yield "id", str r.Id
        yield "shortDescription", text r.ShortDescription
        yield "fullDescription", text full

        match r.HelpUri with
        | Some u -> yield "helpUri", str u
        | None -> ()

        yield "help", text full

        match r.Level with
        | Some l -> yield "defaultConfiguration", obj [ "level", str (level l) ]
        | None -> ()

        match r.Category with
        | Some c when not (String.IsNullOrWhiteSpace c) -> yield "properties", obj [ "tags", arr [ str c ] ]
        | _ -> ()
    ]

let private sha256 (s: string) =
    Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes s)).ToLowerInvariant()

/// A BPA rule's URL is a help URI only if it is an absolute http(s) URL.
let helpUri (url: string option) =
    url
    |> Option.filter (fun u ->
        match Uri.TryCreate(u, UriKind.Absolute) with
        | true, uri -> uri.Scheme = Uri.UriSchemeHttps || uri.Scheme = Uri.UriSchemeHttp
        | _ -> false)

/// The SARIF log. `rules` are the rules that ran, in order; a finding whose code isn't among them
/// gets a rule named after its code. `root` is the directory URIs are relative to.
let log (root: string) (rules: Rule list) (items: Item list) (notifications: Notification list) : JsonNode =
    let rules =
        let known = rules |> List.map _.Id |> Set.ofList

        let extra =
            items
            |> List.map _.Finding.Code
            |> List.distinct
            |> List.filter (fun c -> not (known.Contains c))
            |> List.map (fun c -> Rule.simple c c)

        (rules |> List.distinctBy _.Id) @ extra

    let ruleIndex = rules |> List.mapi (fun i r -> r.Id, i) |> Map.ofList

    // Stable identity for a finding across runs: rule, object, property, and which occurrence of
    // those it is. Not the message (it may carry positions) nor the line (it moves).
    let fingerprints =
        items
        |> List.mapFold
            (fun (seen: Map<string, int>) item ->
                let f = item.Finding

                let key =
                    String.concat
                        "\u0000"
                        [
                            f.Code
                            f.ObjectType
                            f.Object
                            f.ObjectPath |> Option.defaultValue ""
                            f.Position |> Option.map _.Property |> Option.defaultValue ""
                        ]

                let n = seen |> Map.tryFind key |> Option.defaultValue 0
                sha256 $"{key}\u0000{n}", Map.add key (n + 1) seen)
            Map.empty
        |> fst

    let resultJson (item: Item) (fingerprint: string) : JsonNode =
        let f = item.Finding

        let message =
            match f.ObjectPath with
            | Some p -> $"{p}: {f.Message}"
            | None -> $"{f.ObjectType} '{f.Object}': {f.Message}"

        let logical =
            obj [
                yield "name", str f.Object

                match f.ObjectPath with
                | Some p -> yield "fullyQualifiedName", str p
                | None -> ()
            ]

        let physical =
            item.Location
            |> Option.map (fun (loc, _) ->
                obj [
                    "artifactLocation", artifactLocation root loc.File
                    "region", obj [ "startLine", num (loc.Line |> Option.defaultValue 1) ]
                ])

        obj [
            yield "ruleId", str f.Code
            yield "ruleIndex", num ruleIndex.[f.Code]
            yield "level", str (level f.Severity)
            yield "message", text message
            yield
                "locations",
                arr [
                    obj [
                        match physical with
                        | Some p -> yield "physicalLocation", p
                        | None -> ()
                        yield "logicalLocations", arr [ logical ]
                    ]
                ]
            yield "partialFingerprints", obj [ "tesseraFinding/v1", str fingerprint ]

            match item.Suppressed with
            | Some why -> yield "suppressions", arr [ obj [ "kind", str "inSource"; "justification", str why ] ]
            | None -> ()

            yield
                "properties",
                obj [
                    yield "objectType", str f.ObjectType

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

                    match item.Location with
                    | Some(_, exact) -> yield "exactLocation", bool exact
                    | None -> ()

                    yield "fixable", bool f.Fixable
                ]
        ]

    let invocation =
        obj [
            yield "executionSuccessful", bool true

            if not notifications.IsEmpty then
                yield
                    "toolConfigurationNotifications",
                    arr [
                        for n in notifications ->
                            obj [
                                "descriptor", obj [ "id", str n.RuleId ]
                                "level", str (level n.Level)
                                "message", text n.Message
                            ]
                    ]
        ]

    obj [
        "$schema", str schemaUri
        "version", str "2.1.0"
        "runs",
        arr [
            obj [
                "tool",
                obj [
                    "driver",
                    obj [
                        "name", str "tessera"
                        "semanticVersion", str (Common.version ())
                        "rules", arr (rules |> List.map ruleJson)
                    ]
                ]
                "originalUriBaseIds",
                obj [
                    "SRCROOT",
                    obj [
                        "description",
                        text "The repository root: the nearest folder above the model holding .git, else the current folder."
                    ]
                ]
                "invocations", arr [ invocation ]
                "results", arr (List.map2 resultJson items fingerprints)
            ]
        ]
    ]

/// Items for findings about model objects: each placed by its object reference.
let modelItems (index: SourceLocations.Index) (findings: Findings.Finding list) : Item list = [
    for f in findings ->
        let location =
            f.ObjectPath
            |> Option.bind (fun p ->
                match ObjectRef.parse p with
                | Ok r -> SourceLocations.locate index r
                | Error _ -> None)
            |> Option.orElse (index.MainFile |> Option.map (fun file -> { SourceLocations.File = file; Line = None }, false))

        {
            Finding = f
            Location = location
            Suppressed = None
        }
]

let private options =
    JsonSerializerOptions(WriteIndented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping, NewLine = "\n")

/// The log as text: indented, LF line endings, ending with a newline.
let render (node: JsonNode) = node.ToJsonString options + "\n"

/// The root URIs are made relative to, for a model at `modelPath`.
let rootFor (modelPath: string) =
    if XmlaAddress.isRemote modelPath then
        Directory.GetCurrentDirectory()
    else
        repositoryRoot modelPath |> Option.defaultWith Directory.GetCurrentDirectory

/// Write the log (replacing the file, as --trx does).
let write (path: string) (node: JsonNode) : Result<unit, TessError> =
    try
        File.WriteAllText(path, render node, UTF8Encoding false)
        Ok()
    with
    | :? IOException
    | :? UnauthorizedAccessException as e -> Error(SaveFailed(path, e.Message))

/// The usage line every command's --sarif shares.
let usage =
    "also write the findings as a SARIF 2.1.0 file (e.g. for GitHub code scanning; paths relative to the repository root)."
