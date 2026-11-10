namespace Tessera

open System
open System.IO

type ModelFormat =
    | Bim
    | Tmdl
    /// A database on a server (`xmla:<server>;<database>`). Core never loads these itself: only a
    /// command given a connection does (Tessera.Xmla).
    | Xmla

/// A located, loadable model: on disk, or (Format = Xmla) on a server.
type ModelSource = {
    Format: ModelFormat
    /// The .bim file, or the TMDL root folder (the one holding database.tmdl / model.tmdl).
    /// For a remote model, its address as `xmla:<server>;<database>`: not a file path.
    Path: string
    /// The enclosing PBIP "X.SemanticModel" folder, when the model lives in one.
    SemanticModelFolder: string option
}

module ModelFormat =
    let token =
        function
        | Bim -> "bim"
        | Tmdl -> "tmdl"
        | Xmla -> "xmla"

module ModelSource =

    let private tmdlRootMarkers = [ "database.tmdl"; "model.tmdl"; "database.tmd"; "model.tmd" ]

    let isTmdlRoot (dir: string) =
        Directory.Exists dir
        && tmdlRootMarkers |> List.exists (fun f -> File.Exists(Path.Combine(dir, f)))

    let private hasExt (ext: string) (path: string) =
        String.Equals(Path.GetExtension path, ext, StringComparison.OrdinalIgnoreCase)

    /// The PBIP semantic model folder enclosing a model location, if any.
    let private enclosingSemanticModel (dir: string) =
        if Pbip.isSemanticModelFolder dir then Some dir else None

    let private bim (file: string) =
        Ok {
            Format = Bim
            Path = file
            SemanticModelFolder = enclosingSemanticModel (Path.GetDirectoryName file)
        }

    let private tmdl (root: string) =
        Ok {
            Format = Tmdl
            Path = root
            SemanticModelFolder =
                match Directory.GetParent root with
                | null -> None
                | parent -> enclosingSemanticModel parent.FullName
        }

    /// Walk up from a .tmdl file's folder to the TMDL root (as TE2 does, bounded).
    let private findTmdlRoot (start: string) =
        let rec go (dir: DirectoryInfo) depth =
            if isNull dir || depth > 6 then None
            elif isTmdlRoot dir.FullName then Some dir.FullName
            else go dir.Parent (depth + 1)

        go (DirectoryInfo start) 0

    let private fromSemanticModelFolder (dir: string) : Result<ModelSource, TessError> =
        let definition = Path.Combine(dir, Pbip.definitionFolderName)
        let modelBim = Path.Combine(dir, Pbip.modelBimFileName)

        match isTmdlRoot definition, File.Exists modelBim with
        | true, true -> Error(AmbiguousModel(dir, [ definition; modelBim ]))
        | true, false -> tmdl definition
        | false, true -> bim modelBim
        | false, false ->
            Error(
                InvalidPbip(
                    dir,
                    $"has {Pbip.pbismFileName} but neither a TMDL {Pbip.definitionFolderName}/ nor {Pbip.modelBimFileName}"
                )
            )

    let private fromSemanticModelFolders (origin: string) (dirs: string list) =
        match dirs with
        | [ one ] -> fromSemanticModelFolder one
        | [] -> Error(InvalidPbip(origin, "references no semantic model"))
        | many -> Error(AmbiguousModel(origin, many))

    let private fromPbip (file: string) =
        Pbip.semanticModelsOfPbip file |> Result.bind (fromSemanticModelFolders file)

    let private fromReportFolder (dir: string) =
        Pbip.semanticModelOfReport dir
        |> Result.bind (fun sm ->
            if Directory.Exists sm then
                fromSemanticModelFolder sm
            else
                Error(InvalidPbip(dir, $"referenced semantic model folder does not exist: {sm}")))

    /// Look one level down: a PBIP project root holding one .pbip / one .SemanticModel.
    let private fromProjectRoot (dir: string) =
        let semanticModels =
            Directory.GetDirectories dir
            |> Array.filter Pbip.isSemanticModelFolder
            |> Array.sort
            |> List.ofArray

        let pbips = Directory.GetFiles(dir, "*.pbip") |> Array.sort |> List.ofArray

        let bims = Directory.GetFiles(dir, "*.bim") |> Array.sort |> List.ofArray

        match semanticModels, pbips, bims with
        | [ sm ], _, [] -> fromSemanticModelFolder sm
        | [], [ pbip ], [] -> fromPbip pbip
        | [], [], [ b ] -> bim b
        | [], [], [] ->
            Error(
                UnrecognizedModel(
                    dir,
                    "no database.tmdl/model.tmdl, definition.pbism, .pbip, .bim or *.SemanticModel folder found"
                )
            )
        | sms, _, bs -> Error(AmbiguousModel(dir, sms @ bs))

    let private resolveLocal (input: string) : Result<ModelSource, TessError> =
        if String.IsNullOrWhiteSpace input then
            Error(PathNotFound "\"\" (an empty path)")
        else

            let full = Path.GetFullPath input

            if File.Exists full then
                let dir = Path.GetDirectoryName full

                if hasExt ".bim" full then
                    bim full
                elif hasExt ".pbip" full then
                    fromPbip full
                elif hasExt ".pbism" full then
                    fromSemanticModelFolder dir
                elif hasExt ".pbir" full then
                    fromReportFolder dir
                elif hasExt ".tmdl" full || hasExt ".tmd" full then
                    match findTmdlRoot dir with
                    | Some root -> tmdl root
                    | None ->
                        Error(UnrecognizedModel(full, "no database.tmdl or model.tmdl in this or any parent folder"))
                else
                    Error(UnrecognizedModel(full, "unsupported file type"))
            elif Directory.Exists full then
                if Pbip.isSemanticModelFolder full then
                    fromSemanticModelFolder full
                elif isTmdlRoot full then
                    tmdl full
                elif File.Exists(Path.Combine(full, Pbip.pbirFileName)) then
                    fromReportFolder full
                else
                    fromProjectRoot full
            else
                Error(PathNotFound full)

    /// Work out what model a user-supplied path refers to. Accepts:
    /// a .bim file; a TMDL folder (or any .tmdl file inside one); a PBIP "X.SemanticModel" folder or
    /// its definition.pbism; a .pbip file; a "X.Report" folder or its definition.pbir; or a project
    /// folder containing exactly one of those.
    /// A remote model (`xmla:...`) is refused here: only commands that open a connection load one,
    /// and they do so before coming here.
    let resolve (input: string) : Result<ModelSource, TessError> =
        if XmlaAddress.isRemote input then
            Error(
                RemoteSourceUnsupported(
                    input,
                    "this command works on model files only (copy the model first: `tess convert <xmla:...> <folder>`)"
                )
            )
        else
            resolveLocal input

    /// The source describing a remote model (nothing is contacted).
    let remote (address: XmlaAddress) : ModelSource = {
        Format = Xmla
        Path = XmlaAddress.display address
        SemanticModelFolder = None
    }
