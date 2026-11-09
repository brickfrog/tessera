/// Field references in Power BI reports (PBIR), kept in step with renames and moves in the model.
///
/// A PBIR report (X.Report/definition/**/*.json) names model fields structurally, in semantic query
/// expressions:
///   { "Column":  { "Expression": { "SourceRef": { "Entity": "Sales" } }, "Property": "Amount" } }
///   { "Measure": { "Expression": { "SourceRef": { "Source": "s" } }, "Property": "Total" } }
///     with "From": [ { "Name": "s", "Entity": "Sales", "Type": 0 } ] in the enclosing query or filter
///   { "Hierarchy": { "Expression": <SourceRef>, "Hierarchy": "Calendar" } }
///   { "HierarchyLevel": { "Expression": { "Hierarchy": ... }, "Level": "Year" } }
///   { "PropertyVariationSource": { "Expression": <SourceRef>, "Name": "Variation", "Property": "Date" } }
/// Only those names are rewritten, in place: every other byte of the file stays. Not rewritten:
///   - queryRef, nativeQueryRef and selector "metadata": identifiers local to a visual (Power BI
///     itself leaves queryRef as it was after a rename, and visual calculations refer to fields by
///     their nativeQueryRef), so they keep working as they are;
///   - report-level measures (SourceRef "Schema": "extension", reportExtensions.json), free text,
///     and literal values (filters and slicer selections store data values).
module Tessera.Report

open System
open System.IO
open System.Text
open System.Text.Json

/// A change in the model that reports must follow.
type Change =
    | RenameTable of oldName: string * newName: string
    | RenameColumn of table: string * oldName: string * newName: string
    | RenameMeasure of table: string * oldName: string * newName: string
    | RenameHierarchy of table: string * oldName: string * newName: string
    | RenameLevel of table: string * hierarchy: string * oldName: string * newName: string
    /// A measure moved from one table to another (it keeps its name).
    | MoveMeasure of measure: string * fromTable: string * toTable: string

module Change =
    /// The name the change takes away: what a report file must mention to be affected.
    let oldName =
        function
        | RenameTable(o, _)
        | RenameColumn(_, o, _)
        | RenameMeasure(_, o, _)
        | RenameHierarchy(_, o, _)
        | RenameLevel(_, _, o, _) -> o
        | MoveMeasure(m, _, _) -> m

    /// What reports must follow when an object is renamed or moved from `oldRef` to `newRef`.
    let ofMove (oldRef: ObjectRef) (newRef: ObjectRef) =
        match oldRef.Kind, oldRef.Path, newRef.Path with
        | _, _, _ when oldRef.Kind <> newRef.Kind -> None
        | ObjectKind.Table, [ o ], [ n ] -> Some(RenameTable(o, n))
        | ObjectKind.Column, [ t; o ], [ t2; n ] when t = t2 -> Some(RenameColumn(t, o, n))
        | ObjectKind.Measure, [ t; o ], [ t2; n ] when t = t2 -> Some(RenameMeasure(t, o, n))
        | ObjectKind.Measure, [ t; m ], [ t2; m2 ] when m = m2 -> Some(MoveMeasure(m, t, t2))
        | ObjectKind.Hierarchy, [ t; o ], [ t2; n ] when t = t2 -> Some(RenameHierarchy(t, o, n))
        | ObjectKind.Level, [ t; h; o ], [ t2; h2; n ] when t = t2 && h = h2 -> Some(RenameLevel(t, h, o, n))
        | _ -> None

/// One report file rewritten.
type FileEdit = {
    /// The file's full path.
    Path: string
    /// The path as shown to the user: relative to the folder holding the report.
    Display: string
    Before: string
    After: string
    /// The file starts with a UTF-8 byte order mark (kept on write).
    Bom: bool
    /// How many names were rewritten.
    References: int
}

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// A JSON value, with the byte span (quotes included) of every string, for in-place edits.
type private Node =
    | Obj of (string * Node) list
    | Arr of Node list
    | Str of value: string * start: int * length: int
    | Other

let rec private parseValue (r: byref<Utf8JsonReader>) : Node =
    match r.TokenType with
    | JsonTokenType.StartObject ->
        let props = ResizeArray<string * Node>()
        let mutable go = r.Read()

        while go && r.TokenType <> JsonTokenType.EndObject do
            let name = r.GetString()
            r.Read() |> ignore
            props.Add(name, parseValue &r)
            go <- r.Read()

        Obj(List.ofSeq props)
    | JsonTokenType.StartArray ->
        let items = ResizeArray<Node>()
        let mutable go = r.Read()

        while go && r.TokenType <> JsonTokenType.EndArray do
            items.Add(parseValue &r)
            go <- r.Read()

        Arr(List.ofSeq items)
    | JsonTokenType.String -> Str(r.GetString(), int r.TokenStartIndex, r.ValueSpan.Length + 2)
    | _ -> Other

let private readerOptions =
    JsonReaderOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

let private parse (bytes: byte array) =
    let mutable r = Utf8JsonReader(ReadOnlySpan bytes, readerOptions)

    if r.Read() then parseValue &r else Other

let private prop (name: string) (n: Node) =
    match n with
    | Obj props -> props |> List.tryPick (fun (k, v) -> if k = name then Some v else None)
    | _ -> None

let private str (name: string) (n: Node) =
    match prop name n with
    | Some(Str(v, s, l)) -> Some(v, s, l)
    | _ -> None

/// Where a field's table comes from.
type private Entity = {
    Name: string
    /// The "Entity" string of a SourceRef written in place (None: through a From alias).
    Standalone: (int * int) option
    /// A report-level (extension) entity, not the model's table.
    Extension: bool
}

let private isExtension (n: Node) =
    str "Schema" n |> Option.exists (fun (v, _, _) -> eqCi v "extension")

/// The entity of an expression container that is a SourceRef, standalone or through an alias.
let private entityOf (aliases: Map<string, Entity>) (expression: Node option) =
    match expression |> Option.bind (prop "SourceRef") with
    | Some sourceRef ->
        match str "Entity" sourceRef, str "Source" sourceRef with
        | Some(name, s, l), _ ->
            Some {
                Name = name
                Standalone = Some(s, l)
                Extension = isExtension sourceRef
            }
        | None, Some(alias, _, _) -> Map.tryFind alias aliases
        | None, None -> None
    | None -> None

type private Found = {
    Replacements: ResizeArray<int * int * string>
    /// References the change applies to that can't be rewritten in place (a moved measure named
    /// through a From alias that other fields share).
    mutable Skipped: int
}

/// Collect the rewrites a change needs in a parsed file.
let private collect (change: Change) (root: Node) =
    let found = {
        Replacements = ResizeArray()
        Skipped = 0
    }

    let replace (s, l) (value: string) (current: string) =
        if current <> value then
            found.Replacements.Add(s, l, value)

    let modelEntity (e: Entity option) (table: string) =
        e |> Option.exists (fun e -> not e.Extension && eqCi e.Name table)

    let rec visit (aliases: Map<string, Entity>) (n: Node) =
        match n with
        | Obj props ->
            // A query or filter: its From names the sources its expressions refer to by alias.
            let aliases =
                match prop "From" n with
                | Some(Arr sources) ->
                    sources
                    |> List.fold
                        (fun (m: Map<string, Entity>) source ->
                            match str "Name" source, str "Entity" source with
                            | Some(alias, _, _), Some(entity, s, l) ->
                                match change with
                                | RenameTable(o, nw) when eqCi entity o && not (isExtension source) ->
                                    replace (s, l) nw entity
                                | _ -> ()

                                m.Add(
                                    alias,
                                    {
                                        Name = entity
                                        Standalone = None
                                        Extension = isExtension source
                                    }
                                )
                            | _ -> m)
                        aliases
                | _ -> aliases

            for key, value in props do
                let expression = prop "Expression" value

                match key, change with
                | "SourceRef", RenameTable(o, nw) ->
                    match str "Entity" value with
                    | Some(entity, s, l) when eqCi entity o && not (isExtension value) -> replace (s, l) nw entity
                    | _ -> ()
                | ("Column" | "PropertyVariationSource"), RenameColumn(t, o, nw) ->
                    match str "Property" value with
                    | Some(p, s, l) when eqCi p o && modelEntity (entityOf aliases expression) t -> replace (s, l) nw p
                    | _ -> ()
                | "Measure", RenameMeasure(t, o, nw) ->
                    match str "Property" value with
                    | Some(p, s, l) when eqCi p o && modelEntity (entityOf aliases expression) t -> replace (s, l) nw p
                    | _ -> ()
                | "Measure", MoveMeasure(m, from, target) ->
                    match str "Property" value, entityOf aliases expression with
                    | Some(p, _, _), Some e when eqCi p m && not e.Extension && eqCi e.Name from ->
                        match e.Standalone with
                        | Some span -> replace span target e.Name
                        | None -> found.Skipped <- found.Skipped + 1
                    | _ -> ()
                | "Hierarchy", RenameHierarchy(t, o, nw) ->
                    match str "Hierarchy" value with
                    | Some(h, s, l) when eqCi h o && modelEntity (entityOf aliases expression) t -> replace (s, l) nw h
                    | _ -> ()
                | "HierarchyLevel", RenameLevel(t, h, o, nw) ->
                    let hierarchy = expression |> Option.bind (prop "Hierarchy")

                    match str "Level" value, hierarchy |> Option.bind (str "Hierarchy") with
                    | Some(level, s, l), Some(name, _, _) when
                        eqCi level o
                        && eqCi name h
                        && modelEntity (entityOf aliases (hierarchy |> Option.bind (prop "Expression"))) t
                        ->
                        replace (s, l) nw level
                    | _ -> ()
                | _ -> ()

                visit aliases value
        | Arr items -> items |> List.iter (visit aliases)
        | _ -> ()

    visit Map.empty root
    found

let private relaxed =
    JsonSerializerOptions(Encoder = Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let private bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]

let private hasBom (bytes: byte array) =
    bytes.Length >= 3
    && bytes.[0] = bom.[0]
    && bytes.[1] = bom.[1]
    && bytes.[2] = bom.[2]

/// Rewrite one report file's JSON for a change: the new text, how many names were rewritten, and
/// how many matching references had to be left alone. Fails on invalid JSON.
let rewriteBytes (change: Change) (bytes: byte array) =
    let body = if hasBom bytes then bytes.[3..] else bytes
    let found = collect change (parse body)

    let edited =
        found.Replacements
        |> Seq.sortByDescending (fun (s, _, _) -> s)
        |> Seq.fold
            (fun (b: byte array) (s, l, value) ->
                let encoded = Encoding.UTF8.GetBytes(JsonSerializer.Serialize(value, relaxed))
                Array.concat [ b.[.. s - 1]; encoded; b.[s + l ..] ])
            body

    Encoding.UTF8.GetString body, Encoding.UTF8.GetString edited, found.Replacements.Count, found.Skipped

/// The same, for text (no byte order mark).
let rewrite (change: Change) (json: string) =
    let _, after, n, skipped = rewriteBytes change (Encoding.UTF8.GetBytes json)
    after, n, skipped

let private fullPath (p: string) =
    Path.GetFullPath(p).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

/// Report folders next to a semantic model folder whose definition.pbir points at it (byPath).
let reportsOf (semanticModelFolder: string) =
    let model = fullPath semanticModelFolder

    match Path.GetDirectoryName model with
    | null -> []
    | parent when Directory.Exists parent ->
        Directory.GetDirectories parent
        |> Array.sort
        |> Array.filter (fun d ->
            File.Exists(Path.Combine(d, Pbip.pbirFileName))
            && match Pbip.semanticModelOfReport d with
               | Ok sm -> String.Equals(fullPath sm, model, StringComparison.OrdinalIgnoreCase)
               | Error _ -> false)
        |> List.ofArray
    | _ -> []

let private display (reportDir: string) (file: string) =
    let parent = Path.GetDirectoryName(fullPath reportDir)
    Path.GetRelativePath(parent, file).Replace('\\', '/')

let private mentions (name: string) (file: string) =
    try
        File.ReadAllText(file).Contains(name, StringComparison.OrdinalIgnoreCase)
    with _ ->
        false

/// The edits a change needs in these report folders, and warnings about what can't be rewritten.
let plan (reportDirs: string list) (change: Change) : FileEdit list * string list =
    let name = Change.oldName change
    let edits = ResizeArray<FileEdit>()
    let warnings = ResizeArray<string>()

    for dir in reportDirs do
        let definition = Path.Combine(dir, Pbip.definitionFolderName)
        let legacy = Path.Combine(dir, "report.json")

        if Directory.Exists definition then
            let files =
                Directory.GetFiles(definition, "*.json", SearchOption.AllDirectories)
                |> Array.sort
                |> List.ofArray

            let skipped = ResizeArray<string>()
            let unreadable = ResizeArray<string>()

            for file in files do
                let shown = display dir file

                if Path.GetFileName file = "reportExtensions.json" then
                    if mentions name file then
                        warnings.Add $"report-level measures in {shown} mention '{name}' and are not updated"
                else
                    try
                        let bytes = File.ReadAllBytes file
                        let before, after, n, notRewritten = rewriteBytes change bytes

                        if notRewritten > 0 then
                            skipped.Add shown

                        if n > 0 then
                            edits.Add {
                                Path = file
                                Display = shown
                                Before = before
                                After = after
                                Bom = hasBom bytes
                                References = n
                            }
                    with :? JsonException ->
                        if mentions name file then
                            unreadable.Add shown

            if skipped.Count > 0 then
                warnings.Add(
                    $"report filters that name '{name}' through a shared source alias are not updated: "
                    + String.concat ", " skipped
                )

            if unreadable.Count > 0 then
                warnings.Add(
                    $"report files that mention '{name}' are not valid JSON and are not updated: "
                    + String.concat ", " unreadable
                )
        elif File.Exists legacy && mentions name legacy then
            warnings.Add(
                $"{display dir legacy} (PBIR-Legacy format, which Power BI doesn't support editing outside "
                + $"Power BI Desktop) mentions '{name}' and is not updated; save the report in PBIR format to "
                + "have tess update it"
            )

    List.ofSeq edits, List.ofSeq warnings

let private utf8 (withBom: bool) = UTF8Encoding(withBom)

/// Write planned edits, refusing files changed since they were planned.
let write (edits: FileEdit list) : Result<unit, TessError> =
    try
        for e in edits do
            let current = File.ReadAllBytes e.Path
            let body = if hasBom current then current.[3..] else current

            if Encoding.UTF8.GetString body <> e.Before then
                failwith $"{e.Display} changed since the edit was planned"

        for e in edits do
            File.WriteAllText(e.Path, e.After, utf8 e.Bom)

        Ok()
    with e ->
        Error(SaveFailed(String.concat ", " (edits |> List.map (fun e -> e.Path)), e.Message))
