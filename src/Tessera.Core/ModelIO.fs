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
    /// (for a PBIP folder, its definition/). Bounded so huge models stay cheap.
    let private sampleFiles (path: string) =
        if File.Exists path then
            [ path ]
        elif Directory.Exists path then
            let definition = Path.Combine(path, Pbip.definitionFolderName)
            let root = if Directory.Exists definition then definition else path

            [ "*.tmdl"; "*.bim" ]
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

module OutputFormat =
    let token =
        function
        | BimFile -> "bim"
        | TmdlFolder -> "tmdl"
        | PbipFolder -> "pbip"

    let tryParse (s: string) =
        match s.Trim().ToLowerInvariant() with
        | "bim" -> Some BimFile
        | "tmdl" -> Some TmdlFolder
        | "pbip" -> Some PbipFolder
        | _ -> None

    /// Guess from the destination path: *.bim -> BIM, *.SemanticModel -> PBIP, otherwise a TMDL folder.
    let infer (destination: string) =
        let trimmed = destination.TrimEnd('/', '\\')

        if trimmed.EndsWith(".bim", StringComparison.OrdinalIgnoreCase) then
            BimFile
        elif trimmed.EndsWith(Pbip.semanticModelSuffix, StringComparison.OrdinalIgnoreCase) then
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

let private saveBim (opts: SaveOptions) (db: Database) (path: string) =
    catchIo path (fun () ->
        match Path.GetDirectoryName path with
        | null
        | "" -> ()
        | dir -> Directory.CreateDirectory dir |> ignore

        File.WriteAllText(path, toBimText opts db + Eol.text opts.Eol, utf8NoBom)

        {
            Format = BimFile
            Path = path
            Eol = opts.Eol
            Files = [ path ]
        })

let private isTmdlFile (relative: string) =
    relative.EndsWith(".tmdl", StringComparison.OrdinalIgnoreCase)

/// The .tmdl files (relative paths, '/'-separated) of a folder that a new serialization doesn't
/// write, compared by exact case: on a case-insensitive file system, tables/Sales.tmdl is stale
/// when the new output has tables/SALES.tmdl, even though both name the same file.
let staleTmdlFiles (existing: string list) (written: string list) =
    let keep = Collections.Generic.HashSet<string>(written, StringComparer.Ordinal)

    existing
    |> List.filter (fun f -> isTmdlFile f && not (keep.Contains f))
    |> List.sort

let private relativeFiles (dir: string) =
    if Directory.Exists dir then
        Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> Path.GetRelativePath(dir, f).Replace('\\', '/'))
        |> List.ofArray
    else
        []

/// Write TMDL into a folder the way TOM's folder serializer does (stale .tmdl files removed, with
/// folders they leave empty; other files left alone), but safe for case-only renames on
/// case-insensitive file systems. TOM writes SALES.tmdl (which lands on the existing Sales.tmdl
/// there) and then deletes Sales.tmdl as stale, losing the table. So serialize into a staging
/// folder, delete stale files first (by exact case), then copy in what changed.
let private writeTmdl (opts: SaveOptions) (db: Database) (dir: string) =
    catchIo dir (fun () ->
        let staging =
            Path.Combine(Path.GetTempPath(), "tessera-tmdl-" + Guid.NewGuid().ToString("N"))

        try
            TmdlSerializer.SerializeDatabaseToFolder(db, staging, tmdlOptions opts)
            let written = relativeFiles staging

            let local (relative: string) =
                Path.Combine(dir, relative.Replace('/', Path.DirectorySeparatorChar))

            for stale in staleTmdlFiles (relativeFiles dir) written do
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

            for relative in written do
                let source =
                    Path.Combine(staging, relative.Replace('/', Path.DirectorySeparatorChar))

                let target = local relative
                let bytes = File.ReadAllBytes source

                let unchanged =
                    File.Exists target
                    && Array.length bytes = int (FileInfo(target).Length)
                    && File.ReadAllBytes target = bytes

                if not unchanged then
                    Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                    File.WriteAllBytes(target, bytes)
        finally
            try
                if Directory.Exists staging then
                    Directory.Delete(staging, true)
            with _ ->
                ())

let private saveTmdl opts db dir =
    writeTmdl opts db dir
    |> Result.map (fun () -> {
        Format = TmdlFolder
        Path = dir
        Eol = opts.Eol
        Files = filesUnder dir
    })

let private savePbip (opts: SaveOptions) (db: Database) (dir: string) =
    let definition = Path.Combine(dir, Pbip.definitionFolderName)
    let staleBim = Path.Combine(dir, Pbip.modelBimFileName)

    catchIo dir (fun () -> Directory.CreateDirectory dir |> ignore)
    |> Result.bind (fun () -> writeTmdl opts db definition)
    // A folder with both definition/ and model.bim is ambiguous to Power BI; TMDL wins.
    |> Result.bind (fun () ->
        catchIo staleBim (fun () ->
            if File.Exists staleBim then
                File.Delete staleBim))
    |> Result.bind (fun () -> Pbip.ensureSupportFiles dir (Eol.text opts.Eol) Guid.NewGuid)
    |> Result.map (fun () -> {
        Format = PbipFolder
        Path = dir
        Eol = opts.Eol
        Files = filesUnder dir
    })

/// Write a model. Refuses to touch a non-empty destination unless force is set.
let save (format: OutputFormat) (opts: SaveOptions) (force: bool) (db: Database) (destination: string) =
    let destination = Path.GetFullPath destination

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
        | PbipFolder -> savePbip opts db destination

/// Where an in-place save of a loaded model goes: back to its own format and location (a .bim inside
/// a PBIP folder stays a .bim).
let inPlace (source: ModelSource) =
    match source.Format, source.SemanticModelFolder with
    | Bim, _ -> BimFile, source.Path
    | Tmdl, Some folder -> PbipFolder, folder
    | Tmdl, None -> TmdlFolder, source.Path
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

    save (OutputFormat.infer destination) opts force db destination
