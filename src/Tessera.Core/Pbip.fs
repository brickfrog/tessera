/// Power BI Project (PBIP) folder conventions.
/// File shapes follow Power BI Desktop's output (checked against TabularEditor/spaceparts-analytics):
///   X.pbip                          { artifacts: [ { report: { path: "X.Report" } } ] }
///   X.Report/definition.pbir        { datasetReference: { byPath: { path: "../X.SemanticModel" } } }
///   X.SemanticModel/definition.pbism, .platform, and either definition/ (TMDL) or model.bim
module Tessera.Pbip

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes

let pbismFileName = "definition.pbism"
let platformFileName = ".platform"
let pbirFileName = "definition.pbir"
let definitionFolderName = "definition"
let modelBimFileName = "model.bim"
let semanticModelSuffix = ".SemanticModel"

let pbismSchema =
    "https://developer.microsoft.com/json-schemas/fabric/item/semanticModel/definitionProperties/1.0.0/schema.json"

let platformSchema =
    "https://developer.microsoft.com/json-schemas/fabric/gitIntegration/platformProperties/2.0.0/schema.json"

/// definition.pbism version written for new TMDL-based models (current Power BI Desktop output).
let pbismVersion = "4.2"

/// definition.pbism versions below this predate TMDL support.
let minTmdlPbismVersion = Version(4, 0)

let private documentOptions =
    JsonDocumentOptions(AllowTrailingCommas = true, CommentHandling = JsonCommentHandling.Skip)

let private nodeOptions = JsonNodeOptions(PropertyNameCaseInsensitive = false)

let private writeOptions = JsonSerializerOptions(WriteIndented = true)

let private utf8NoBom = UTF8Encoding(false)

let private readJson (path: string) : Result<JsonNode, string> =
    try
        match JsonNode.Parse(File.ReadAllText path, nodeOptions, documentOptions) with
        | null -> Error "file is empty"
        | node -> Ok node
    with
    | :? JsonException as e -> Error $"invalid JSON: {e.Message}"
    | :? IOException as e -> Error e.Message

let private tryString (node: JsonNode) (path: string list) =
    let rec go (n: JsonNode) =
        function
        | [] ->
            match n with
            | :? JsonValue as v ->
                match v.TryGetValue<string>() with
                | true, s -> Some s
                | _ -> None
            | _ -> None
        | key :: rest ->
            match n with
            | :? JsonObject as o ->
                let mutable child: JsonNode = null

                if o.TryGetPropertyValue(key, &child) && not (isNull child) then
                    go child rest
                else
                    None
            | _ -> None

    go node path

/// PBIP paths can be written with Windows separators.
let private resolveRelative (baseDir: string) (relative: string) =
    let normalized =
        relative.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar)

    Path.GetFullPath(Path.Combine(baseDir, normalized))

/// The semantic model folder a report folder points at, via definition.pbir byPath.
let semanticModelOfReport (reportDir: string) : Result<string, TessError> =
    let pbir = Path.Combine(reportDir, pbirFileName)

    if not (File.Exists pbir) then
        Error(InvalidPbip(reportDir, $"missing {pbirFileName}"))
    else
        match readJson pbir with
        | Error e -> Error(InvalidPbip(pbir, e))
        | Ok json ->
            match tryString json [ "datasetReference"; "byPath"; "path" ] with
            | Some rel -> Ok(resolveRelative reportDir rel)
            | None ->
                match json.["datasetReference"] with
                | :? JsonObject as o when o.ContainsKey "byConnection" -> Error(RemoteModelReference reportDir)
                | _ -> Error(InvalidPbip(pbir, "no datasetReference.byPath.path"))

/// Semantic model folders reachable from a .pbip file (through its report artifacts).
let semanticModelsOfPbip (pbipFile: string) : Result<string list, TessError> =
    match readJson pbipFile with
    | Error e -> Error(InvalidPbip(pbipFile, e))
    | Ok json ->
        let baseDir = Path.GetDirectoryName pbipFile

        let reportDirs =
            match json.["artifacts"] with
            | :? JsonArray as artifacts ->
                artifacts
                |> Seq.choose (fun a ->
                    match a with
                    | null -> None
                    | a -> tryString a [ "report"; "path" ])
                |> Seq.map (resolveRelative baseDir)
                |> List.ofSeq
            | _ -> []

        if reportDirs.IsEmpty then
            Error(InvalidPbip(pbipFile, "no report artifacts listed"))
        else
            let results = reportDirs |> List.map semanticModelOfReport

            match
                results
                |> List.tryPick (function
                    | Error e -> Some e
                    | Ok _ -> None)
            with
            | Some e -> Error e
            | None ->
                results
                |> List.choose (function
                    | Ok d -> Some d
                    | Error _ -> None)
                |> List.distinct
                |> Ok

let isSemanticModelFolder (dir: string) =
    File.Exists(Path.Combine(dir, pbismFileName))

/// Display name for a semantic model folder: "Sales.SemanticModel" -> "Sales".
let displayNameOf (semanticModelDir: string) =
    let name = DirectoryInfo(semanticModelDir).Name

    if name.EndsWith(semanticModelSuffix, StringComparison.OrdinalIgnoreCase) then
        name.Substring(0, name.Length - semanticModelSuffix.Length)
    else
        name

let private writeJson (path: string) (node: JsonNode) eol =
    let text = node.ToJsonString(writeOptions).Replace("\r\n", "\n")
    let text = if eol = "\n" then text else text.Replace("\n", eol)
    File.WriteAllText(path, text + eol, utf8NoBom)

let private newPbism () =
    JsonObject(
        [
            Collections.Generic.KeyValuePair<string, JsonNode>("$schema", JsonValue.Create pbismSchema)
            Collections.Generic.KeyValuePair<string, JsonNode>("version", JsonValue.Create pbismVersion)
            Collections.Generic.KeyValuePair<string, JsonNode>("settings", JsonObject())
        ]
    )

let private newPlatform (displayName: string) (logicalId: Guid) =
    let metadata = JsonObject()
    metadata.["type"] <- JsonValue.Create "SemanticModel"
    metadata.["displayName"] <- JsonValue.Create displayName
    let config = JsonObject()
    config.["version"] <- JsonValue.Create "2.0"
    config.["logicalId"] <- JsonValue.Create(logicalId.ToString())
    let root = JsonObject()
    root.["$schema"] <- JsonValue.Create platformSchema
    root.["metadata"] <- metadata
    root.["config"] <- config
    root

/// Make sure a semantic model folder has the PBIP support files for a TMDL definition.
/// Existing files are preserved (a .platform keeps its logicalId); an existing definition.pbism
/// whose version predates TMDL is bumped, keeping its other settings.
let ensureSupportFiles (semanticModelDir: string) (eol: string) (newId: unit -> Guid) : Result<unit, TessError> =
    let pbism = Path.Combine(semanticModelDir, pbismFileName)
    let platform = Path.Combine(semanticModelDir, platformFileName)

    try
        let pbismResult =
            if not (File.Exists pbism) then
                writeJson pbism (newPbism ()) eol
                Ok()
            else
                match readJson pbism with
                | Error e -> Error(InvalidPbip(pbism, e))
                | Ok json ->
                    let tooOld =
                        match tryString json [ "version" ] |> Option.map Version.TryParse with
                        | Some(true, v) -> v < minTmdlPbismVersion
                        | _ -> false

                    match json with
                    | :? JsonObject as o when tooOld ->
                        o.["version"] <- JsonValue.Create pbismVersion
                        writeJson pbism o eol
                    | _ -> ()

                    Ok()

        pbismResult
        |> Result.map (fun () ->
            if not (File.Exists platform) then
                writeJson platform (newPlatform (displayNameOf semanticModelDir) (newId ())) eol)
    with
    | :? IOException
    | :? UnauthorizedAccessException as e -> Error(SaveFailed(semanticModelDir, e.Message))
