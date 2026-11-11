/// Loading and saving models via Microsoft's TOM serializers.
module Tessera.ModelIO

open System
open System.IO
open System.Text
open Microsoft.AnalysisServices.Tabular
open Microsoft.AnalysisServices.Tabular.Serialization
open Microsoft.AnalysisServices.Tabular.Extensions
open Microsoft.AnalysisServices.Tabular.Tmdl

type Eol =
    | Lf
    | CrLf

module Eol =
    let text =
        function
        | Lf -> "\n"
        | CrLf -> "\r\n"

    let token =
        function
        | Lf -> "lf"
        | CrLf -> "crlf"

    /// Count line breaks by kind; returns (crlf, lf-only).
    let private count (text: string) =
        let mutable crlf = 0
        let mutable lf = 0

        for i in 0 .. text.Length - 1 do
            if text.[i] = '\n' then
                if i > 0 && text.[i - 1] = '\r' then
                    crlf <- crlf + 1
                else
                    lf <- lf + 1

        crlf, lf

    let private verdict (crlf, lf) =
        if crlf = 0 && lf = 0 then None
        elif crlf > lf then Some CrLf
        else Some Lf

    /// The line ending a text mostly uses; None if it has no line breaks.
    let ofText (text: string) = verdict (count text)

    /// Model files worth sampling at a path: a .bim file, or the .tmdl/.bim files under a folder
    /// (for a PBIP folder, its definition/; for a database.json folder, its .json files). Bounded so
    /// huge models stay cheap.
    let private sampleFiles (path: string) =
        if File.Exists path then
            [ path ]
        elif Directory.Exists path then
            let definition = Path.Combine(path, Pbip.definitionFolderName)
            let root = if Directory.Exists definition then definition else path

            [
                "*.tmdl"
                "*.bim"
                if FolderFormat.isFolder root then
                    "*.json"
            ]
            |> Seq.collect (fun pattern -> Directory.EnumerateFiles(root, pattern, SearchOption.AllDirectories))
            |> Seq.truncate 25
            |> List.ofSeq
        else
            []

    let private head (file: string) =
        try
            use reader = new StreamReader(file)
            let buffer = Array.zeroCreate<char> 65536
            let n = reader.ReadBlock(buffer, 0, buffer.Length)
            String(buffer, 0, n)
        with _ ->
            ""

    /// The line ending the model files at a path mostly use, if there are any.
    let detectAt (path: string) =
        sampleFiles path
        |> List.map (head >> count)
        |> List.fold (fun (a, b) (c, d) -> a + c, b + d) (0, 0)
        |> verdict

    /// For "auto": match what's already at the destination (so overwrites don't churn every line in
    /// git), else what the source uses (a Windows user's CRLF model stays CRLF), else LF.
    let resolveAuto (source: string) (destination: string) =
        detectAt destination
        |> Option.orElse (detectAt source)
        |> Option.defaultValue Lf

type SaveOptions = {
    Eol: Eol
    /// Keep restricted information (e.g. data source passwords). Off by default.
    IncludeSensitive: bool
    /// TMDL indentation: None = tabs (TMDL default), Some n = n spaces.
    IndentSpaces: int option
}

module SaveOptions =
    let defaults = {
        Eol = Lf
        IncludeSensitive = false
        IndentSpaces = None
    }

/// Where (and how) to write a model.
type OutputFormat =
    | BimFile
    | TmdlFolder
    /// A PBIP "X.SemanticModel" folder: TMDL under definition/ plus definition.pbism and .platform.
    | PbipFolder
    /// Tabular Editor's "save to folder" layout: database.json plus a file per object (FolderFormat).
    | DatabaseJsonFolder

module OutputFormat =
    let token =
        function
        | BimFile -> "bim"
        | TmdlFolder -> "tmdl"
        | PbipFolder -> "pbip"
        | DatabaseJsonFolder -> "database.json"

    /// Every format, in the order the CLI lists them.
    let all = [ BimFile; TmdlFolder; PbipFolder; DatabaseJsonFolder ]

    /// The format names `--format` / `--serialization` accept (a fixed list, for completions).
    let tokens = all |> List.map token

    let tryParse (s: string) =
        let s = s.Trim().ToLowerInvariant()
        all |> List.tryFind (fun f -> token f = s)

    /// Guess from the destination path: *.bim -> BIM, *.SemanticModel -> PBIP, .../database.json -> the
    /// database.json folder; an existing folder keeps its format (database.json, PBIP); otherwise a
    /// TMDL folder.
    let infer (destination: string) =
        let trimmed = destination.TrimEnd('/', '\\')

        if trimmed.EndsWith(".bim", StringComparison.OrdinalIgnoreCase) then
            BimFile
        elif trimmed.EndsWith(Pbip.semanticModelSuffix, StringComparison.OrdinalIgnoreCase) then
            PbipFolder
        elif
            String.Equals(
                Path.GetFileName trimmed,
                FolderFormat.databaseJsonFileName,
                StringComparison.OrdinalIgnoreCase
            )
            || FolderFormat.isFolder trimmed
        then
            DatabaseJsonFolder
        elif Directory.Exists trimmed && Pbip.isSemanticModelFolder trimmed then
            PbipFolder
        else
            TmdlFolder

/// What a save produced.
type SavedModel = {
    Format: OutputFormat
    Path: string
    Eol: Eol
    Files: string list
}

let private utf8NoBom = UTF8Encoding(false)

let private catchIo (path: string) (f: unit -> 'a) : Result<'a, TessError> =
    try
        Ok(f ())
    with e ->
        Error(SaveFailed(path, e.Message))

/// Compatibility levels Analysis Services itself uses; anything else (e.g. 1702) is Power BI only.
let private analysisServicesLevels = set [ 1200; 1400; 1500; 1600 ]

/// TOM's .bim reader validates properties against a compatibility mode and rejects Power BI-only
/// ones (e.g. relatedColumnDetails on field parameters) in Analysis Services mode. Guess the mode
/// up front from the JSON, the same signals TE2 uses (PowerBiCompatibilityModeHelper).
let private guessBimMode (json: string) =
    try
        use doc = Text.Json.JsonDocument.Parse(json)
        let root = doc.RootElement

        let prop (e: Text.Json.JsonElement) (name: string) =
            match e.ValueKind with
            | Text.Json.JsonValueKind.Object ->
                match e.TryGetProperty name with
                | true, v -> Some v
                | _ -> None
            | _ -> None

        let explicitMode =
            prop root "compatibilityMode"
            |> Option.filter (fun v -> v.ValueKind = Text.Json.JsonValueKind.String)
            |> Option.bind (fun v ->
                match Enum.TryParse<Microsoft.AnalysisServices.CompatibilityMode>(v.GetString(), true) with
                | true, m when m <> Microsoft.AnalysisServices.CompatibilityMode.Unknown -> Some m
                | _ -> None)

        let level =
            prop root "compatibilityLevel"
            |> Option.filter (fun v -> v.ValueKind = Text.Json.JsonValueKind.Number)
            |> Option.map (fun v -> v.GetInt32())

        let v3 =
            prop root "model"
            |> Option.bind (fun m -> prop m "defaultPowerBIDataSourceVersion")
            |> Option.exists (fun v -> v.ValueKind = Text.Json.JsonValueKind.String && v.GetString() = "powerBI_V3")

        match explicitMode with
        | Some m -> m
        | None when
            v3
            || (level |> Option.exists (fun l -> not (analysisServicesLevels.Contains l)))
            ->
            Microsoft.AnalysisServices.CompatibilityMode.PowerBI
        | None -> Microsoft.AnalysisServices.CompatibilityMode.AnalysisServices
    with _ ->
        Microsoft.AnalysisServices.CompatibilityMode.AnalysisServices

let private deserializeBim (json: string) =
    let attempt mode =
        JsonSerializer.DeserializeDatabase(json, DeserializeOptions(), mode)

    match guessBimMode json with
    | Microsoft.AnalysisServices.CompatibilityMode.PowerBI as mode -> attempt mode
    | mode ->
        // The heuristic can miss Power BI-only properties at standard levels; retry once.
        try
            attempt mode
        with e when e.Message.Contains("Unrecognized JSON property") ->
            attempt Microsoft.AnalysisServices.CompatibilityMode.PowerBI

/// A deep copy of the database. TOM's Database.Clone() resets an inferred column data type
/// (isDataTypeInferred: true, as .bim files from Visual Studio / SSDT carry for calculated columns)
/// to Unknown, so comparing a clone with the original would report a dataType change on every such
/// column. Copy those data types back.
let clone (db: Database) : Database =
    let copy = db.Clone()

    if not (isNull db.Model) then
        for t in db.Model.Tables do
            match copy.Model.Tables.Find t.Name with
            | null -> ()
            | ct ->
                for c in t.Columns do
                    match ct.Columns.Find c.Name with
                    | null -> ()
                    | cc when cc.DataType = DataType.Unknown && c.DataType <> DataType.Unknown ->
                        let inferred =
                            match c with
                            | :? CalculatedColumn as x -> Some x.IsDataTypeInferred
                            | :? CalculatedTableColumn as x -> Some x.IsDataTypeInferred
                            | _ -> None

                        cc.DataType <- c.DataType

                        match cc, inferred with
                        | (:? CalculatedColumn as x), Some i -> x.IsDataTypeInferred <- i
                        | (:? CalculatedTableColumn as x), Some i -> x.IsDataTypeInferred <- i
                        | _ -> ()
                    | _ -> ()

    copy

/// The mode a database effectively runs in. TMDL loads report Unknown; infer it the way TE2 does.
/// Deliberately not written back to the Database: TOM would then emit "compatibilityMode: powerBI"
/// into database.tmdl, which Power BI Desktop doesn't write (pure diff noise in PBIP repos).
let effectiveCompatibilityMode (db: Database) =
    if db.CompatibilityMode <> Microsoft.AnalysisServices.CompatibilityMode.Unknown then
        db.CompatibilityMode
    else
        let pbiOnly =
            not (analysisServicesLevels.Contains db.CompatibilityLevel)
            || (not (isNull db.Model)
                && (db.Model.DefaultPowerBIDataSourceVersion = PowerBIDataSourceVersion.PowerBI_V3
                    || db.Model.Tables
                       |> Seq.exists (fun t -> t.Columns |> Seq.exists (fun c -> not (isNull c.RelatedColumnDetails)))))

        if pbiOnly then
            Microsoft.AnalysisServices.CompatibilityMode.PowerBI
        else
            Microsoft.AnalysisServices.CompatibilityMode.AnalysisServices

let load (source: ModelSource) : Result<Database, TessError> =
    try
        match source.Format with
        | Bim -> Ok(deserializeBim (File.ReadAllText source.Path))
        | Tmdl -> Ok(TmdlSerializer.DeserializeDatabaseFromFolder source.Path)
        | DatabaseJson ->
            match FolderFormat.combine source.Path with
            | Ok json -> Ok(deserializeBim json)
            | Error reason -> Error(LoadFailed(source.Path, reason))
        | Xmla ->
            Error(RemoteSourceUnsupported(source.Path, "a remote model is loaded over a connection, not from files"))
    with e ->
        Error(LoadFailed(source.Path, e.Message))

/// Read a model from .bim (TMSL database) JSON text. Throws on JSON that isn't a model.
let parseBim (json: string) : Database = deserializeBim json

/// Resolve and load in one step.
let loadPath (path: string) : Result<ModelSource * Database, TessError> =
    ModelSource.resolve path
    |> Result.bind (fun src -> load src |> Result.map (fun db -> src, db))

let private jsonOptions (opts: SaveOptions) =
    SerializeOptions(
        IgnoreInferredObjects = true,
        IgnoreInferredProperties = true,
        IgnoreTimestamps = true,
        SplitMultilineStrings = true,
        IncludeRestrictedInformation = opts.IncludeSensitive
    )

/// TMDL databases (as written by Power BI Desktop) are often unnamed, and TOM then writes
/// "name": null as the root's first property, which its own .bim reader rejects. Patch just that
/// token with the model's name; touching the TOM object instead trips its ID validation.
let private nullRootName = "{\n  \"name\": null,"

let private serializeNamed (db: Database) (options: SerializeOptions) =
    let json = JsonSerializer.SerializeDatabase(db, options).Replace("\r\n", "\n")

    if json.StartsWith(nullRootName, StringComparison.Ordinal) then
        let fallback =
            match db.Model with
            | null -> "SemanticModel"
            | m when String.IsNullOrEmpty m.Name -> "SemanticModel"
            | m -> m.Name

        let encoded =
            System.Text.Json.JsonSerializer.Serialize(
                fallback,
                System.Text.Json.JsonSerializerOptions(
                    Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
                )
            )

        "{\n  \"name\": " + encoded + "," + json.Substring(nullRootName.Length)
    elif String.IsNullOrEmpty db.Name then
        // TOM changed its output shape; refuse rather than silently write an unloadable .bim.
        failwith "cannot name an unnamed database in .bim output: unexpected TOM JSON layout"
    else
        json

/// The .bim JSON text for a database, with the requested line endings.
let toBimText (opts: SaveOptions) (db: Database) =
    let json = serializeNamed db (jsonOptions opts)

    match opts.Eol with
    | Lf -> json
    | CrLf -> json.Replace("\n", "\r\n")

let private tmdlOptions (opts: SaveOptions) =
    let formatting =
        MetadataFormattingOptionsBuilder(MetadataSerializationStyle.Tmdl)
            .WithBaseIndentationLevel(0)
            .WithCasingStyle(TmdlCasingStyle.CamelCase)
            .WithEncoding(utf8NoBom)
            .WithNewLineStyle(
                match opts.Eol with
                | Lf -> NewLineStyle.UnixStyle
                | CrLf -> NewLineStyle.WindowsStyle
            )

    let formatting =
        match opts.IndentSpaces with
        | Some n when n > 0 -> formatting.WithSpacesIndentationMode(n)
        | _ -> formatting.WithTabsIndentationMode()

    let builder =
        MetadataSerializationOptionsBuilder(MetadataSerializationStyle.Tmdl)
            .WithFormattingOptions(formatting.GetOptions())
            .WithExpressionTrimStyle(TmdlExpressionTrimStyle.NoTrim)
            .WithMetadataOrderHints()

    let builder =
        if opts.IncludeSensitive then
            builder.WithRestrictedInformation()
        else
            builder.WithoutRestrictedInformation()

    builder.GetOptions()

let private isNonEmpty (path: string) =
    File.Exists path
    || (Directory.Exists path
        && not (Seq.isEmpty (Directory.EnumerateFileSystemEntries path)))

let private filesUnder (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.sort
    |> List.ofArray

/// Write bytes unless the file already holds exactly them (so a no-op save changes nothing on disk).
let private writeIfChanged (path: string) (bytes: byte array) =
    let unchanged =
        File.Exists path
        && Array.length bytes = int (FileInfo(path).Length)
        && File.ReadAllBytes path = bytes

    if not unchanged then
        match Path.GetDirectoryName path with
        | null
        | "" -> ()
        | dir -> Directory.CreateDirectory dir |> ignore

        File.WriteAllBytes(path, bytes)

let private saveBim (opts: SaveOptions) (db: Database) (path: string) =
    catchIo path (fun () ->
        writeIfChanged path (utf8NoBom.GetBytes(toBimText opts db + Eol.text opts.Eol))

        {
            Format = BimFile
            Path = path
            Eol = opts.Eol
            Files = [ path ]
        })

let private isTmdlFile (relative: string) =
    relative.EndsWith(".tmdl", StringComparison.OrdinalIgnoreCase)

/// The files (relative paths, '/'-separated) of a folder that a save owns (`isManaged`) but no
/// longer writes, compared by exact case: on a case-insensitive file system, tables/Sales.tmdl is
/// stale when the new output has tables/SALES.tmdl, even though both name the same file.
let staleFiles (isManaged: string -> bool) (existing: string list) (written: string list) =
    let keep = Collections.Generic.HashSet<string>(written, StringComparer.Ordinal)

    existing
    |> List.filter (fun f -> isManaged f && not (keep.Contains f))
    |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))

/// staleFiles for TMDL: any .tmdl file.
let staleTmdlFiles (existing: string list) (written: string list) = staleFiles isTmdlFile existing written

let private relativeFiles (dir: string) =
    if Directory.Exists dir then
        Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> Path.GetRelativePath(dir, f).Replace('\\', '/'))
        |> List.ofArray
    else
        []

/// Write a folder's files (relative '/'-separated paths): stale files the save owns are deleted
/// first, by exact case (so a case-only rename on a case-insensitive file system doesn't delete the
/// file it just wrote), with the folders they leave empty; then whatever changed is written. Files
/// the save doesn't own are left alone.
let private writeFolder (dir: string) (isManaged: string -> bool) (files: (string * byte array) list) =
    catchIo dir (fun () ->
        let local (relative: string) =
            Path.Combine(dir, relative.Replace('/', Path.DirectorySeparatorChar))

        for stale in staleFiles isManaged (relativeFiles dir) (List.map fst files) do
            let path = local stale
            File.Delete path

            // Folders the deletion left empty go too, up to (not including) the root.
            let root = Path.GetFullPath dir
            let mutable folder = Path.GetDirectoryName(Path.GetFullPath path)

            while not (isNull folder)
                  && folder.Length > root.TrimEnd(Path.DirectorySeparatorChar).Length
                  && Directory.Exists folder
                  && Seq.isEmpty (Directory.EnumerateFileSystemEntries folder) do
                Directory.Delete folder
                folder <- Path.GetDirectoryName folder

        Directory.CreateDirectory dir |> ignore

        for relative, bytes in files do
            writeIfChanged (local relative) bytes)

/// The TMDL files for a database, as (relative path, bytes), in TOM's document order: exactly what
/// TOM's folder serializer writes, with file names made portable. TOM writes `tables/CON.tmdl`
/// (a device name on Windows) and, for tables whose names differ only by case, files that collide on
/// Windows and macOS; those get `_CON.tmdl` and `sales~2.tmdl` (TMDL readers take object names from
/// the files' content, never from their names). Computed in memory, so nothing collides on disk first.
let tmdlFiles (opts: SaveOptions) (db: Database) : (string * byte array) list =
    let context =
        MetadataSerializationContext.Create(MetadataSerializationStyle.Tmdl, db, tmdlOptions opts)

    let documents = [
        for d in context ->
            let logical = d.LogicalPath

            let relative =
                if logical.StartsWith("./", StringComparison.Ordinal) then
                    logical.Substring 2
                else
                    logical

            use stream = new MemoryStream()
            d.WriteTo(stream)
            relative.Split('/') |> Array.map FolderFormat.SafeName.portable, stream.ToArray()
    ]

    // Unique file names per folder, the first of each clash keeping its name.
    let byFolder =
        documents
        |> List.map (fun (segments, _) -> String.Join("/", segments.[.. segments.Length - 2]))
        |> List.distinct

    let names = Collections.Generic.Dictionary<string * int, string>()

    for folder in byFolder do
        let members =
            documents
            |> List.indexed
            |> List.filter (fun (_, (segments, _)) -> String.Join("/", segments.[.. segments.Length - 2]) = folder)

        let unique =
            members
            |> List.map (fun (_, (segments, _)) -> segments.[segments.Length - 1])
            |> FolderFormat.SafeName.uniqueIn ".tmdl"

        for (i, _), name in List.zip members unique do
            names.[(folder, i)] <- name

    documents
    |> List.mapi (fun i (segments, bytes) ->
        let folder = String.Join("/", segments.[.. segments.Length - 2])
        let file = names.[(folder, i)] + ".tmdl"
        (if folder = "" then file else folder + "/" + file), bytes)

/// Write TMDL into a folder (see writeFolder: stale .tmdl files removed, other files left alone).
let private writeTmdl (opts: SaveOptions) (db: Database) (dir: string) =
    try
        Ok(tmdlFiles opts db)
    with e ->
        Error(SaveFailed(dir, e.Message))
    |> Result.bind (writeFolder dir isTmdlFile)

let private saveTmdl opts db dir =
    writeTmdl opts db dir
    |> Result.map (fun () -> {
        Format = TmdlFolder
        Path = dir
        Eol = opts.Eol
        Files = filesUnder dir
    })

/// The folder options a model asks for: its TabularEditor_SerializeOptions annotation, or TE's
/// default folder levels when it has none (and whether it had one).
let private folderOptionsOf (db: Database) : Result<FolderFormat.FolderOptions * bool, string> =
    let annotation =
        match db.Model with
        | null -> None
        | m -> Option.ofObj (m.Annotations.Find FolderFormat.FolderOptions.annotationName)

    match annotation with
    | None -> Ok(FolderFormat.FolderOptions.defaultFolder, false)
    | Some a ->
        FolderFormat.FolderOptions.parse a.Value
        |> Result.map (fun o -> o, true)
        |> Result.mapError (fun e ->
            $"the model's {FolderFormat.FolderOptions.annotationName} annotation can't be read: {e}")

/// The database.json folder's files for a database. The model's TabularEditor_SerializeOptions
/// annotation decides the layout; a model without one gets TE's default folder levels, and the
/// annotation is written into database.json (as TE does), so later saves by any tool keep the layout.
/// The annotation's own IncludeSensitive is not honoured: --include-sensitive decides.
let databaseJsonFiles (opts: SaveOptions) (db: Database) : Result<FolderFormat.FolderFile list, string> =
    folderOptionsOf db
    |> Result.bind (fun (folder, present) ->
        let needsCopy =
            not (isNull db.Model)
            && (not present
                || folder.LocalTranslations
                || folder.LocalPerspectives
                || folder.LocalRelationships)

        let target = if needsCopy then clone db else db

        if needsCopy && not present then
            target.Model.Annotations.Add(
                Annotation(
                    Name = FolderFormat.FolderOptions.annotationName,
                    Value = FolderFormat.FolderOptions.toJson folder
                )
            )

        FolderFormat.Local.apply folder (effectiveCompatibilityMode db) target
        |> Result.bind (fun () ->
            let options =
                SerializeOptions(
                    IgnoreInferredObjects = folder.IgnoreInferredObjects,
                    IgnoreInferredProperties = folder.IgnoreInferredProperties,
                    IgnoreTimestamps = folder.IgnoreTimestamps,
                    SplitMultilineStrings = folder.SplitMultilineStrings,
                    IncludeRestrictedInformation = opts.IncludeSensitive
                )

            FolderFormat.split folder (Eol.text opts.Eol) (serializeNamed target options)))

let private saveDatabaseJson (opts: SaveOptions) (db: Database) (dir: string) =
    (try
        databaseJsonFiles opts db
     with e ->
         Error e.Message)
    |> Result.mapError (fun e -> SaveFailed(dir, e))
    |> Result.bind (fun files ->
        writeFolder dir FolderFormat.isManagedFile [ for f in files -> f.Path, utf8NoBom.GetBytes f.Text ])
    |> Result.map (fun () -> {
        Format = DatabaseJsonFolder
        Path = dir
        Eol = opts.Eol
        Files = filesUnder dir
    })

/// The item name for a new PBIP semantic model folder: the source's .platform displayName, else the
/// database's name (unless it's empty or a GUID, as Power BI Desktop's are), else the model's.
let private itemName (item: Pbip.Item option) (db: Database) =
    let usable (s: string) =
        not (String.IsNullOrWhiteSpace s) && not (fst (Guid.TryParse s))

    match item with
    | Some i -> i.DisplayName
    | None when usable db.Name -> db.Name
    | None when not (isNull db.Model) && usable db.Model.Name -> db.Model.Name
    | None -> "SemanticModel"

/// The semantic model folder a PBIP save writes: the destination itself when it's one (named
/// X.SemanticModel, or holding a definition.pbism); else, for a project folder, the one
/// X.SemanticModel in it, or a new one named after the item.
let pbipTarget (item: Pbip.Item option) (db: Database) (destination: string) : Result<string, TessError> =
    let destination = Path.GetFullPath destination

    let trimmed =
        destination.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

    if
        trimmed.EndsWith(Pbip.semanticModelSuffix, StringComparison.OrdinalIgnoreCase)
        || Pbip.isSemanticModelFolder destination
    then
        Ok destination
    else
        let existing =
            if Directory.Exists destination then
                Directory.GetDirectories destination
                |> Array.filter (fun d ->
                    d.EndsWith(Pbip.semanticModelSuffix, StringComparison.OrdinalIgnoreCase)
                    && Pbip.isSemanticModelFolder d)
                |> Array.sortWith (fun a b -> String.CompareOrdinal(a, b))
                |> List.ofArray
            else
                []

        match existing with
        | [ one ] -> Ok one
        | [] ->
            let name = FolderFormat.SafeName.escape true (itemName item db)
            Ok(Path.Combine(destination, name + Pbip.semanticModelSuffix))
        | many -> Error(AmbiguousModel(destination, many))

/// Write a PBIP semantic model folder, merging into what's there: the model goes where the folder
/// keeps it (TMDL in definition/, or model.bim for a .bim-based project), missing support files are
/// added, and existing ones are kept (see Pbip.ensureSupportFilesFor).
let private savePbip (opts: SaveOptions) (item: Pbip.Item option) (db: Database) (dir: string) =
    let definition = Path.Combine(dir, Pbip.definitionFolderName)
    let modelBim = Path.Combine(dir, Pbip.modelBimFileName)
    let bimBased = File.Exists modelBim && not (Directory.Exists definition)

    catchIo dir (fun () -> Directory.CreateDirectory dir |> ignore)
    |> Result.bind (fun () ->
        if bimBased then
            saveBim opts db modelBim |> Result.map ignore
        else
            writeTmdl opts db definition
            // A folder with both definition/ and model.bim is ambiguous to Power BI (tess refuses to
            // load it); the TMDL just written wins.
            |> Result.bind (fun () ->
                catchIo modelBim (fun () ->
                    if File.Exists modelBim then
                        File.Delete modelBim)))
    |> Result.bind (fun () -> Pbip.ensureSupportFilesFor dir item (not bimBased) (Eol.text opts.Eol) Guid.NewGuid)
    |> Result.map (fun () -> {
        Format = PbipFolder
        Path = dir
        Eol = opts.Eol
        Files = filesUnder dir
    })

/// The item (displayName, description) of the PBIP a model was loaded from, if any.
let sourceItem (source: ModelSource) =
    source.SemanticModelFolder |> Option.bind Pbip.readItem

/// Write a model. Refuses to touch a non-empty destination unless force is set. For PBIP, `item`
/// names a new semantic model folder and its .platform (see pbipTarget); pass the source's
/// (sourceItem) to carry its displayName and description over.
let saveItem
    (item: Pbip.Item option)
    (format: OutputFormat)
    (opts: SaveOptions)
    (force: bool)
    (db: Database)
    (destination: string)
    =
    let destination = Path.GetFullPath destination

    // A database.json path names its folder.
    let destination =
        if
            format = DatabaseJsonFolder
            && String.Equals(
                Path.GetFileName destination,
                FolderFormat.databaseJsonFileName,
                StringComparison.OrdinalIgnoreCase
            )
        then
            Path.GetDirectoryName destination
        else
            destination

    let target =
        match format with
        | PbipFolder when not (File.Exists destination) -> pbipTarget item db destination
        | _ -> Ok destination

    match target with
    | Error e -> Error e
    | Ok destination ->
        if isNonEmpty destination && not force then
            Error(DestinationNotEmpty destination)
        elif format <> BimFile && File.Exists destination then
            Error(SaveFailed(destination, "destination is a file, but the output format writes a folder"))
        elif format = BimFile && Directory.Exists destination then
            Error(SaveFailed(destination, "destination is a folder, but the output format writes a .bim file"))
        else
            match format with
            | BimFile -> saveBim opts db destination
            | TmdlFolder -> saveTmdl opts db destination
            | PbipFolder -> savePbip opts item db destination
            | DatabaseJsonFolder -> saveDatabaseJson opts db destination

/// saveItem with no source item.
let save (format: OutputFormat) (opts: SaveOptions) (force: bool) (db: Database) (destination: string) =
    saveItem None format opts force db destination

/// Where an in-place save of a loaded model goes: back to its own format and location (a .bim inside
/// a PBIP folder stays a .bim).
let inPlace (source: ModelSource) =
    match source.Format, source.SemanticModelFolder with
    | Bim, _ -> BimFile, source.Path
    | Tmdl, Some folder -> PbipFolder, folder
    | Tmdl, None -> TmdlFolder, source.Path
    | DatabaseJson, _ -> DatabaseJsonFolder, source.Path
    | Xmla, _ -> invalidOp "a remote model has no file location to save to"

/// Save a model back where it came from: same format and line endings, restricted information kept
/// (it's the user's own model, not an export).
let saveBack (source: ModelSource) (db: Database) =
    let format, destination = inPlace source

    let opts = {
        SaveOptions.defaults with
            Eol = Eol.resolveAuto source.Path destination
            IncludeSensitive = true
    }

    save format opts true db destination

/// Save a model somewhere else, like `convert` (format inferred from the path, restricted information
/// stripped, a non-empty destination only with force).
let saveTo (source: ModelSource) (force: bool) (db: Database) (destination: string) =
    let opts = {
        SaveOptions.defaults with
            Eol = Eol.resolveAuto source.Path destination
    }

    saveItem (sourceItem source) (OutputFormat.infer destination) opts force db destination
