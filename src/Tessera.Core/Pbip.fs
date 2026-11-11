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

/// The text of a support file: two-space indented, non-ASCII written as is (as Desktop writes it;
/// System.Text.Json's default writer \u-escapes it), the given line ending, one after the last line.
let jsonText (node: JsonNode) (eol: string) =
    FolderFormat.JsonText.write eol node + eol

let private writeJson (path: string) (node: JsonNode) eol =
    File.WriteAllText(path, jsonText node eol, utf8NoBom)

let private newPbism () =
    JsonObject(
        [
            Collections.Generic.KeyValuePair<string, JsonNode>("$schema", JsonValue.Create pbismSchema)
            Collections.Generic.KeyValuePair<string, JsonNode>("version", JsonValue.Create pbismVersion)
            Collections.Generic.KeyValuePair<string, JsonNode>("settings", JsonObject())
        ]
    )

/// What a .platform says about its item.
type Item = {
    DisplayName: string
    Description: string option
}

/// The item a semantic model folder's .platform describes, if it has a readable one with a
/// displayName.
let readItem (semanticModelDir: string) : Item option =
    let platform = Path.Combine(semanticModelDir, platformFileName)

    if File.Exists platform then
        match readJson platform with
        | Ok json ->
            tryString json [ "metadata"; "displayName" ]
            |> Option.filter (String.IsNullOrWhiteSpace >> not)
            |> Option.map (fun name -> {
                DisplayName = name
                Description =
                    tryString json [ "metadata"; "description" ]
                    |> Option.filter (String.IsNullOrEmpty >> not)
            })
        | Error _ -> None
    else
        None

let private newPlatform (item: Item) (logicalId: Guid) =
    let metadata = JsonObject()
    metadata.["type"] <- JsonValue.Create "SemanticModel"
    metadata.["displayName"] <- JsonValue.Create item.DisplayName

    match item.Description with
    | Some d -> metadata.["description"] <- JsonValue.Create d
    | None -> ()

    let config = JsonObject()
    config.["version"] <- JsonValue.Create "2.0"
    config.["logicalId"] <- JsonValue.Create(logicalId.ToString())
    let root = JsonObject()
    root.["$schema"] <- JsonValue.Create platformSchema
    root.["metadata"] <- metadata
    root.["config"] <- config
    root

/// The text of a new .platform for an item.
let platformText (item: Item) (logicalId: Guid) (eol: string) =
    jsonText (newPlatform item logicalId) eol

/// The text of a new definition.pbism.
let pbismText (eol: string) = jsonText (newPbism ()) eol

/// Make sure a semantic model folder has its PBIP support files: what's missing is added, what's
/// there is kept ("merge, never clobber": a .platform keeps its displayName, description and
/// logicalId). The one change to an existing file: a definition.pbism whose version predates TMDL
/// is bumped (Power BI can't open the TMDL otherwise), keeping its other settings. A new .platform
/// describes `item` (by default, the folder's name).
let ensureSupportFilesFor
    (semanticModelDir: string)
    (item: Item option)
    (eol: string)
    (newId: unit -> Guid)
    : Result<unit, TessError> =
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
                let item =
                    item
                    |> Option.defaultValue {
                        DisplayName = displayNameOf semanticModelDir
                        Description = None
                    }

                writeJson platform (newPlatform item (newId ())) eol)
    with
    | :? IOException
    | :? UnauthorizedAccessException as e -> Error(SaveFailed(semanticModelDir, e.Message))

/// ensureSupportFilesFor a TMDL definition, naming a new .platform's item after the folder.
let ensureSupportFiles (semanticModelDir: string) (eol: string) (newId: unit -> Guid) : Result<unit, TessError> =
    ensureSupportFilesFor semanticModelDir None eol newId
