/// Tabular Editor's "save to folder" model layout (TE2, TE3 and pbi-tools call it the JSON folder
/// format): a `database.json` holding the TOM JSON of the database, with collections split out into
/// one file per object, as the model's `TabularEditor_SerializeOptions` annotation says.
///
///   database.json
///   dataSources/<name>.json  expressions/<name>.json  functions/<name>.json
///   relationships/<name>.json  perspectives/<name>.json  cultures/<name>.json  roles/<name>.json
///   tables/<table>/<table>.json
///   tables/<table>/{columns,measures,hierarchies,partitions,annotations,calculationItems}/<name>.json
///
/// Reading and writing work on JSON text: ModelIO turns a Database into TOM's .bim JSON and back.
module Tessera.FolderFormat

// The layout, the file naming, the serialize options and the "local" annotations (translations,
// perspectives and relationships stored on the objects themselves) are ported from Tabular Editor 2,
// TOMWrapper/Serialization (SplitModelSerializer.cs, SerializeOptions.cs, Serializer.cs and the
// *AnnotationSerializer.cs files) and TOMWrapper/AnnotationHelper.cs.
// Copyright (c) 2017 Daniel Otykier. MIT License: https://github.com/TabularEditor/TabularEditor
// Where tessera departs from TE2 (to give the same bytes on every OS, and never lose data), the
// comment says so.

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular

let databaseJsonFileName = "database.json"

/// A folder in this format: one holding a database.json.
let isFolder (dir: string) =
    Directory.Exists dir && File.Exists(Path.Combine(dir, databaseJsonFileName))

/// File names that work on Windows, macOS and Linux alike.
module SafeName =
    /// What Windows refuses in a file name (Path.GetInvalidFileNameChars on Windows). TE2 asks the
    /// running OS, so on Linux it writes `a:b.json`, which a Windows checkout can't hold; this set is
    /// fixed, so every OS writes the same names.
    let private invalidChars =
        HashSet<char>([ '"'; '<'; '>'; '|'; ':'; '*'; '?'; '\\'; '/' ] @ [ for c in 0..31 -> char c ])

    let isInvalidChar (c: char) = invalidChars.Contains c

    /// Windows device names. TE2 knows CON..LPT9; Windows also reserves COM0/LPT0 and the
    /// superscript-digit forms.
    let private reservedNames =
        let numbered = [
            for prefix in [ "COM"; "LPT" ] do
                for suffix in [ for d in 0..9 -> string d ] @ [ "\u00B9"; "\u00B2"; "\u00B3" ] do
                    yield prefix + suffix
        ]

        HashSet<string>([ "CON"; "PRN"; "AUX"; "NUL" ] @ numbered, StringComparer.OrdinalIgnoreCase)

    /// A device name, alone or with an extension (Windows treats `NUL.txt` and `nul.a.b` as NUL;
    /// TE2 checks the whole name only).
    let isReserved (name: string) =
        let stem =
            match name.IndexOf '.' with
            | -1 -> name
            | i -> name.Substring(0, i)

        reservedNames.Contains(stem.TrimEnd ' ')

    let private hex (c: char) =
        "%" + (int c).ToString("x2", CultureInfo.InvariantCulture)

    /// TE2's Sanitize: Windows-invalid characters become %xx (lowercase hex), a literal "%20" or
    /// "%5F" gets its % escaped, a device name gets a "_" prefix (and a name that already looks
    /// like one, "_CON", gets "%5F"). A folder name can't end in a space, nor (tessera) a dot.
    let escape (isFolder: bool) (name: string) =
        let sb = StringBuilder()

        for i in 0 .. name.Length - 1 do
            let c = name.[i]

            let escapedPercent =
                i < name.Length - 2
                && (let s = name.Substring(i, 3)

                    s.Equals("%20", StringComparison.OrdinalIgnoreCase)
                    || s.Equals("%5F", StringComparison.OrdinalIgnoreCase))

            if invalidChars.Contains c || escapedPercent then
                sb.Append(hex c) |> ignore
            else
                sb.Append c |> ignore

        let mutable result = sb.ToString()

        if result.Length = 0 then
            result <- "_"

        if isFolder && (result.EndsWith " " || result.EndsWith ".") then
            result <- result.Substring(0, result.Length - 1) + hex result.[result.Length - 1]

        if result.StartsWith "_" && isReserved (result.Substring 1) then
            result <- "%5F" + result.Substring 1

        if isReserved result then
            result <- "_" + result

        result

    /// The key two names collide under on a case-insensitive (Windows, macOS) or
    /// normalization-insensitive (macOS) file system.
    let collisionKey (fileName: string) =
        fileName.Normalize(NormalizationForm.FormC).ToUpperInvariant()

    /// Make sibling names unique under collisionKey, keeping the first of each clash as it is and
    /// giving the others "~2", "~3", ... before the extension. Deterministic: the same names in the
    /// same order give the same result on every OS. (TE2 lets such files overwrite each other.)
    let uniqueIn (extension: string) (names: string list) =
        let taken = HashSet<string>(StringComparer.Ordinal)

        names
        |> List.map (fun name ->
            if taken.Add(collisionKey (name + extension)) then
                name
            else
                let mutable i = 2

                while not (taken.Add(collisionKey ($"{name}~{i}" + extension))) do
                    i <- i + 1

                $"{name}~{i}")

    /// A file or folder name every OS can hold.
    let isPortable (name: string) =
        name.Length > 0
        && name <> "."
        && name <> ".."
        && not (name |> Seq.exists invalidChars.Contains)
        && not (isReserved name)
        && not (name.EndsWith " " || name.EndsWith ".")

    /// A file name TOM chose (TMDL), made portable: TOM escapes invalid characters itself but writes
    /// device names (tables/CON.tmdl) as they are.
    let portable (name: string) =
        let escaped =
            name
            |> String.collect (fun c -> if invalidChars.Contains c then hex c else string c)

        if isReserved escaped then "_" + escaped else escaped

/// JSON text the way TOM (and TE2, both through Json.NET) writes it: two-space indentation,
/// `"key": value`, and strings escaped only where JSON requires. Values read from a document are
/// written back token for token, so numbers and escapes never change.
module JsonText =
    /// Json.NET's default string escaping.
    let quote (s: string) =
        let sb = StringBuilder(s.Length + 2)
        sb.Append '"' |> ignore

        for c in s do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | '\n' -> sb.Append "\\n" |> ignore
            | '\r' -> sb.Append "\\r" |> ignore
            | '\t' -> sb.Append "\\t" |> ignore
            | '\b' -> sb.Append "\\b" |> ignore
            | '\f' -> sb.Append "\\f" |> ignore
            | c when c < ' ' || c = '\u0085' || c = ' ' || c = ' ' ->
                sb.Append("\\u").Append((int c).ToString("x4", CultureInfo.InvariantCulture))
                |> ignore
            | c -> sb.Append c |> ignore

        sb.Append('"').ToString()

    let private scalar (v: JsonValue) =
        match v.TryGetValue<JsonElement>() with
        | true, e -> e.GetRawText()
        | _ ->
            match v.GetValueKind() with
            | JsonValueKind.String -> quote (v.GetValue<string>())
            | _ -> v.ToJsonString()

    let rec private writeNode (sb: StringBuilder) (eol: string) (depth: int) (node: JsonNode) =
        let pad (d: int) = sb.Append(' ', 2 * d) |> ignore

        match node with
        | null -> sb.Append "null" |> ignore
        | :? JsonObject as o when o.Count = 0 -> sb.Append "{}" |> ignore
        | :? JsonObject as o ->
            sb.Append('{').Append(eol) |> ignore
            let mutable first = true

            for KeyValue(k, v) in o do
                if not first then
                    sb.Append(',').Append(eol) |> ignore

                first <- false
                pad (depth + 1)
                sb.Append(quote k).Append(": ") |> ignore
                writeNode sb eol (depth + 1) v

            sb.Append(eol) |> ignore
            pad depth
            sb.Append '}' |> ignore
        | :? JsonArray as a when a.Count = 0 -> sb.Append "[]" |> ignore
        | :? JsonArray as a ->
            sb.Append('[').Append(eol) |> ignore

            for i in 0 .. a.Count - 1 do
                if i > 0 then
                    sb.Append(',').Append(eol) |> ignore

                pad (depth + 1)
                writeNode sb eol (depth + 1) a.[i]

            sb.Append(eol) |> ignore
            pad depth
            sb.Append ']' |> ignore
        | :? JsonValue as v -> sb.Append(scalar v) |> ignore
        | other -> sb.Append(other.ToJsonString()) |> ignore

    /// Indented JSON with the given line ending, and no line ending after the last line (as TE2).
    let write (eol: string) (node: JsonNode) =
        let sb = StringBuilder()
        writeNode sb eol 0 node
        sb.ToString()

    /// One line, no spaces, as Json.NET's Formatting.None (TE2's annotation values).
    let rec compact (node: JsonNode) =
        match node with
        | null -> "null"
        | :? JsonObject as o ->
            "{"
            + String.Join(",", [ for KeyValue(k, v) in o -> quote k + ":" + compact v ])
            + "}"
        | :? JsonArray as a -> "[" + String.Join(",", [ for v in a -> compact v ]) + "]"
        | :? JsonValue as v -> scalar v
        | other -> other.ToJsonString()

    let private documentOptions =
        JsonDocumentOptions(AllowTrailingCommas = true, CommentHandling = JsonCommentHandling.Skip)

    let parse (text: string) : JsonNode =
        JsonNode.Parse(text, Nullable(), documentOptions)

/// The `TabularEditor_SerializeOptions` annotation: how a model is split into files.
type FolderOptions = {
    IgnoreInferredObjects: bool
    IgnoreInferredProperties: bool
    IgnoreTimestamps: bool
    SplitMultilineStrings: bool
    IgnoreLineageTags: bool
    IgnorePrivacySettings: bool
    IgnoreIncrementalRefreshPartitions: bool
    PrefixFilenames: bool
    LocalTranslations: bool
    LocalPerspectives: bool
    LocalRelationships: bool
    SortArrays: bool
    /// The split levels, e.g. "Tables", "Tables/Measures" (TE's names, in the annotation's order).
    Levels: string list
}

module FolderOptions =
    let annotationName = "TabularEditor_SerializeOptions"

    /// Every level TE2 and TE3 know. Unknown levels in an annotation are kept (and their objects
    /// stay inline in their parent's file).
    let knownLevels = [
        "Data Sources"
        "Functions"
        "Shared Expressions"
        "Perspectives"
        "Relationships"
        "Roles"
        "Tables"
        "Tables/Columns"
        "Tables/Hierarchies"
        "Tables/Measures"
        "Tables/Partitions"
        "Tables/Calculation Items"
        "Tables/Annotations"
        "Translations"
    ]

    /// SerializeOptions' field defaults, with no levels: what TE2 uses for a key the annotation
    /// leaves out.
    let empty = {
        IgnoreInferredObjects = true
        IgnoreInferredProperties = true
        IgnoreTimestamps = true
        SplitMultilineStrings = true
        IgnoreLineageTags = false
        IgnorePrivacySettings = false
        IgnoreIncrementalRefreshPartitions = false
        PrefixFilenames = false
        LocalTranslations = false
        LocalPerspectives = false
        LocalRelationships = false
        SortArrays = false
        Levels = []
    }

    /// SerializeOptions.DefaultFolder: what TE (and tessera) use for a model saved to a folder the
    /// first time. It includes TE3's per-file UDFs (3.27.0) and calculation items (3.25.5).
    let defaultFolder = {
        empty with
            Levels = [
                "Data Sources"
                "Functions"
                "Shared Expressions"
                "Perspectives"
                "Relationships"
                "Roles"
                "Tables"
                "Tables/Columns"
                "Tables/Hierarchies"
                "Tables/Measures"
                "Tables/Partitions"
                "Tables/Calculation Items"
                "Translations"
            ]
    }

    let has (level: string) (o: FolderOptions) = List.contains level o.Levels

    /// Read the annotation's JSON. Keys match case-insensitively, as Json.NET reads them; keys
    /// that only matter elsewhere (TmdlOptions, IncludeSensitive, AlsoSaveAsBim) are ignored.
    let parse (json: string) : Result<FolderOptions, string> =
        try
            match JsonText.parse json with
            | :? JsonObject as o ->
                let find (key: string) =
                    o
                    |> Seq.tryFind (fun kv -> String.Equals(kv.Key, key, StringComparison.OrdinalIgnoreCase))
                    |> Option.map (fun kv -> kv.Value)

                let flag (key: string) (fallback: bool) =
                    match find key with
                    | Some(:? JsonValue as v) when v.GetValueKind() = JsonValueKind.True -> true
                    | Some(:? JsonValue as v) when v.GetValueKind() = JsonValueKind.False -> false
                    | None -> fallback
                    | Some _ -> failwith $"{key} is not true or false"

                let levels =
                    match find "Levels" with
                    | None -> []
                    | Some(:? JsonArray as a) ->
                        [
                            for v in a ->
                                match v with
                                | :? JsonValue as s when s.GetValueKind() = JsonValueKind.String ->
                                    // TE normalizes an old misspelling of this level the same way.
                                    s.GetValue<string>().Replace("Tables/CalculationItems", "Tables/Calculation Items")
                                | _ -> failwith "Levels holds something other than names"
                        ]
                        |> List.distinct
                    | Some _ -> failwith "Levels is not a list"

                let e = empty

                Ok {
                    IgnoreInferredObjects = flag "IgnoreInferredObjects" e.IgnoreInferredObjects
                    IgnoreInferredProperties = flag "IgnoreInferredProperties" e.IgnoreInferredProperties
                    IgnoreTimestamps = flag "IgnoreTimestamps" e.IgnoreTimestamps
                    SplitMultilineStrings = flag "SplitMultilineStrings" e.SplitMultilineStrings
                    IgnoreLineageTags = flag "IgnoreLineageTags" e.IgnoreLineageTags
                    IgnorePrivacySettings = flag "IgnorePrivacySettings" e.IgnorePrivacySettings
                    IgnoreIncrementalRefreshPartitions =
                        flag "IgnoreIncrementalRefreshPartitions" e.IgnoreIncrementalRefreshPartitions
                    PrefixFilenames = flag "PrefixFilenames" e.PrefixFilenames
                    LocalTranslations = flag "LocalTranslations" e.LocalTranslations
                    LocalPerspectives = flag "LocalPerspectives" e.LocalPerspectives
                    LocalRelationships = flag "LocalRelationships" e.LocalRelationships
                    SortArrays = flag "SortArrays" e.SortArrays
                    Levels = levels
                }
            | _ -> Error "not a JSON object"
        with
        | :? JsonException as e -> Error $"not valid JSON: {e.Message}"
        | e -> Error e.Message

    /// The annotation's JSON, keys in the order TE2's Json.NET writes them (optional keys only
    /// when set, as TE2's ShouldSerialize methods do).
    let toJson (o: FolderOptions) =
        let b (v: bool) = if v then "true" else "false"

        let parts = [
            "\"IgnoreInferredObjects\":" + b o.IgnoreInferredObjects
            "\"IgnoreInferredProperties\":" + b o.IgnoreInferredProperties
            "\"IgnoreTimestamps\":" + b o.IgnoreTimestamps
            if o.IgnoreLineageTags then
                "\"IgnoreLineageTags\":true"
            "\"SplitMultilineStrings\":" + b o.SplitMultilineStrings
            if o.IgnorePrivacySettings then
                "\"IgnorePrivacySettings\":true"
            if o.IgnoreIncrementalRefreshPartitions then
                "\"IgnoreIncrementalRefreshPartitions\":true"
            "\"PrefixFilenames\":" + b o.PrefixFilenames
            "\"LocalTranslations\":" + b o.LocalTranslations
            "\"LocalPerspectives\":" + b o.LocalPerspectives
            "\"SortArrays\":" + b o.SortArrays
            "\"LocalRelationships\":" + b o.LocalRelationships
            "\"Levels\":[" + String.Join(",", o.Levels |> List.map JsonText.quote) + "]"
        ]

        "{" + String.Join(",", parts) + "}"

/// TE's annotation names (AnnotationHelper.cs).
module Annotations =
    let translatedNames = "TabularEditor_TranslatedNames"
    let translatedDescriptions = "TabularEditor_TranslatedDescriptions"
    let translatedDisplayFolders = "TabularEditor_TranslatedDisplayFolders"
    let inPerspective = "TabularEditor_InPerspective"
    let rls = "TabularEditor_RLS"
    let ols = "TabularEditor_OLS"
    let relationships = "TabularEditor_Relationships"
    let cultures = "TabularEditor_Cultures"
    let perspectives = "TabularEditor_Perspectives"

    /// Cleared from tables and everything under them when a folder is loaded (as TE does).
    let objectLevel = [
        translatedNames
        translatedDescriptions
        translatedDisplayFolders
        inPerspective
        rls
        ols
        relationships
    ]

// ---- JSON helpers ---------------------------------------------------------------------------------

let private str (node: JsonNode) =
    match node with
    | :? JsonValue as v when v.GetValueKind() = JsonValueKind.String -> Some(v.GetValue<string>())
    | _ -> None

let private nameOf (node: JsonNode) =
    match node with
    | :? JsonObject as o -> str o.["name"]
    | _ -> None

let private objects (o: JsonObject) (arrayName: string) =
    match o.[arrayName] with
    | :? JsonArray as a -> [
        for n in a do
            match n with
            | :? JsonObject as x -> x
            | _ -> ()
      ]
    | _ -> []

/// The items of an array, detached so they can be moved elsewhere.
let private detach (a: JsonArray) =
    let items = [ for n in a -> n ]
    a.Clear()
    items

/// Remove an array (by dotted path, e.g. "calculationGroup.calculationItems") and return its items.
let private popArray (o: JsonObject) (path: string) : JsonNode list option =
    let parts = path.Split '.'

    let parent =
        parts.[0 .. parts.Length - 2]
        |> Array.fold
            (fun (acc: JsonObject option) key ->
                acc
                |> Option.bind (fun x ->
                    match x.[key] with
                    | :? JsonObject as child -> Some child
                    | _ -> None))
            (Some o)

    let last = parts.[parts.Length - 1]

    match parent with
    | Some p ->
        match p.[last] with
        | :? JsonArray as a ->
            p.Remove last |> ignore
            Some(detach a)
        | _ -> None
    | None -> None

let private getOrCreateArray (o: JsonObject) (name: string) =
    match o.[name] with
    | :? JsonArray as a -> a
    | _ ->
        let a = JsonArray()
        o.[name] <- a
        a

/// The value of a TE annotation on an object (a multi-line value is a JSON array of lines), and
/// remove it if asked.
let private annotation (o: JsonObject) (name: string) (remove: bool) =
    match o.["annotations"] with
    | :? JsonArray as anns ->
        anns
        |> Seq.tryFind (fun a -> nameOf a = Some name)
        |> Option.bind (fun a ->
            let value =
                match (a :?> JsonObject).["value"] with
                | :? JsonArray as lines -> Some(String.Join("\n", lines |> Seq.map (str >> Option.defaultValue "")))
                | v -> str v

            if remove then
                anns.Remove a |> ignore

            value)
    | _ -> None

let private dropEmptyAnnotations (o: JsonObject) =
    match o.["annotations"] with
    | :? JsonArray as a when a.Count = 0 -> o.Remove "annotations" |> ignore
    | _ -> ()

// ---- Writing ----------------------------------------------------------------------------------------

/// One file of a folder: its '/'-separated path relative to the folder, and its text.
type FolderFile = { Path: string; Text: string }

/// The top-level folders the format writes; stale .json files are only ever removed from these.
let managedFolders = [
    "tables"
    "relationships"
    "perspectives"
    "cultures"
    "dataSources"
    "expressions"
    "functions"
    "roles"
]

/// Whether a file (relative, '/'-separated) is one a save of this format owns: database.json and
/// .json files under the format's folders. Compared by exact case.
let isManagedFile (relative: string) =
    relative = databaseJsonFileName
    || (relative.EndsWith(".json", StringComparison.Ordinal)
        && managedFolders
           |> List.exists (fun f -> relative.StartsWith(f + "/", StringComparison.Ordinal)))

/// Sort the arrays TE sorts when SortArrays is set (Serializer.SortModelArrays): perspectives (unless
/// they get their own files) and their members, and roles with their table and column permissions.
/// Ordinal, where TE2 sorts by the current culture. (TE2 never calls this code; TE3's option does
/// the same sort, as far as its documentation says.)
let private has level o = FolderOptions.has level o

let private sortArrays (o: FolderOptions) (model: JsonObject) =
    let sort (parent: JsonObject) (name: string) =
        match parent.[name] with
        | :? JsonArray as a ->
            let sorted =
                detach a
                |> List.sortWith (fun x y ->
                    String.CompareOrdinal(nameOf x |> Option.defaultValue "", nameOf y |> Option.defaultValue ""))

            for n in sorted do
                a.Add n

            Some a
        | _ -> None

    let objectsOf (a: JsonArray option) =
        a
        |> Option.map (fun a -> [
            for n in a do
                match n with
                | :? JsonObject as x -> x
                | _ -> ()
        ])
        |> Option.defaultValue []

    if not o.LocalPerspectives then
        let perspectives =
            if has "Perspectives" o then
                match model.["perspectives"] with
                | :? JsonArray as a -> Some a
                | _ -> None
            else
                sort model "perspectives"

        for p in objectsOf perspectives do
            for t in objectsOf (sort p "tables") do
                sort t "columns" |> ignore
                sort t "measures" |> ignore
                sort t "hierarchies" |> ignore

    let roles =
        if has "Roles" o then
            match model.["roles"] with
            | :? JsonArray as a -> Some a
            | _ -> None
        else
            sort model "roles"

    for r in objectsOf roles do
        for tp in objectsOf (sort r "tablePermissions") do
            sort tp "columnPermissions" |> ignore

/// Remove every property with this name, anywhere.
let rec private removeProperty (name: string) (node: JsonNode) =
    match node with
    | :? JsonObject as o ->
        o.Remove name |> ignore

        for KeyValue(_, v) in List.ofSeq o do
            removeProperty name v
    | :? JsonArray as a ->
        for v in a do
            removeProperty name v
    | _ -> ()

let rec private incrementalRefreshPartitions (node: JsonNode) =
    match node with
    | :? JsonObject as o -> [
        for KeyValue(k, v) in o do
            match v with
            | :? JsonArray as parts when k = "partitions" ->
                for p in parts do
                    match p with
                    | :? JsonObject as po when
                        str po.["mode"] = Some "import"
                        && (match po.["source"] with
                            | :? JsonObject as s -> str s.["type"] = Some "policyRange"
                            | _ -> false)
                        ->
                        yield nameOf po |> Option.defaultValue "?"
                    | _ -> ()
            | v -> yield! incrementalRefreshPartitions v
      ]
    | :? JsonArray as a -> [
        for v in a do
            yield! incrementalRefreshPartitions v
      ]
    | _ -> []

/// Split TOM's .bim JSON for a database into the folder's files, as the options say. The JSON
/// must already carry the options annotation and any "local" annotations (see Local.apply).
/// Files come back sorted by path.
let split (o: FolderOptions) (eol: string) (bimJson: string) : Result<FolderFile list, string> =
    try
        let root = JsonText.parse bimJson :?> JsonObject
        let files = ResizeArray<FolderFile>()

        let write (path: string) (node: JsonNode) =
            files.Add {
                Path = path
                Text = JsonText.write eol node
            }

        let named (what: string) (node: JsonNode) =
            match nameOf node with
            | Some n -> n
            | None -> failwith $"a {what} has no name"

        let prefixed (i: int) (name: string) =
            if o.PrefixFilenames then
                (i.ToString("D3", CultureInfo.InvariantCulture)) + " " + name
            else
                name

        let outArray (folder: string) (arrayName: string) (items: JsonNode list) =
            let names =
                items
                |> List.mapi (fun i item -> prefixed i (SafeName.escape false (named arrayName item)))
                |> SafeName.uniqueIn ".json"

            for name, item in List.zip names items do
                write $"{folder}{arrayName}/{name}.json" item

        if o.IgnoreLineageTags then
            removeProperty "lineageTag" root

        if o.IgnorePrivacySettings then
            removeProperty "PrivacySetting" root

        if o.IgnoreIncrementalRefreshPartitions then
            match incrementalRefreshPartitions root with
            | [] -> ()
            | names ->
                // TE2 drops these partitions and TE3 swaps in a placeholder; either loses the model's
                // incremental refresh partitions, so tessera refuses instead.
                let list = String.Join(", ", names)

                failwith
                    $"the model's {FolderOptions.annotationName} sets IgnoreIncrementalRefreshPartitions, which would drop its incremental refresh partitions ({list}); tessera won't write that"

        match root.["model"] with
        | :? JsonObject as model ->
            if o.SortArrays then
                sortArrays o model

            let level (name: string) (array: string) (also: bool) =
                if has name o || also then popArray model array else None

            let dataSources = level "Data Sources" "dataSources" false
            let expressions = level "Shared Expressions" "expressions" false
            let functions = level "Functions" "functions" false
            let tables = level "Tables" "tables" false
            let relationships = level "Relationships" "relationships" o.LocalRelationships

            let cultures =
                if o.LocalTranslations then
                    // The translations are on the objects now. A culture with nothing else (just a
                    // name) is in the TabularEditor_Cultures annotation; one with more (linguistic
                    // metadata, annotations) stays a culture, without its translations. TE2 drops
                    // those cultures, and their linguistic metadata with them.
                    match model.["cultures"] with
                    | :? JsonArray as a ->
                        for c in a do
                            match c with
                            | :? JsonObject as co -> co.Remove "translations" |> ignore
                            | _ -> ()

                        let bare =
                            a
                            |> Seq.forall (fun c ->
                                match c with
                                | :? JsonObject as co -> co.Count = 1 && co.ContainsKey "name"
                                | _ -> false)

                        let listed = annotation model Annotations.cultures false |> Option.isSome

                        if bare && listed then
                            popArray model "cultures" |> ignore
                            None
                        elif has "Translations" o then
                            popArray model "cultures"
                        else
                            None
                    | _ -> None
                else
                    level "Translations" "cultures" false

            let perspectives = level "Perspectives" "perspectives" o.LocalPerspectives
            let roles = level "Roles" "roles" false

            write databaseJsonFileName root

            match relationships with
            | Some items when not o.LocalRelationships -> outArray "" "relationships" items
            | _ -> ()

            match perspectives with
            | Some items when not o.LocalPerspectives -> outArray "" "perspectives" items
            | _ -> ()

            cultures |> Option.iter (outArray "" "cultures")
            dataSources |> Option.iter (outArray "" "dataSources")
            expressions |> Option.iter (outArray "" "expressions")
            functions |> Option.iter (outArray "" "functions")
            roles |> Option.iter (outArray "" "roles")

            match tables with
            | Some items ->
                let tableNames = items |> List.map (named "table")

                let folders =
                    tableNames
                    |> List.mapi (fun i n -> prefixed i (SafeName.escape true n))
                    |> SafeName.uniqueIn ""

                for (folder, name), item in List.zip (List.zip folders tableNames) items do
                    let t = item :?> JsonObject

                    let pop (lvl: string) (path: string) =
                        if has lvl o then popArray t path else None

                    let columns = pop "Tables/Columns" "columns"
                    let partitions = pop "Tables/Partitions" "partitions"
                    let measures = pop "Tables/Measures" "measures"
                    let hierarchies = pop "Tables/Hierarchies" "hierarchies"
                    let annotations = pop "Tables/Annotations" "annotations"

                    let calculationItems =
                        pop "Tables/Calculation Items" "calculationGroup.calculationItems"

                    let tablePath = $"tables/{folder}/"
                    write $"{tablePath}{SafeName.escape false name}.json" t
                    measures |> Option.iter (outArray tablePath "measures")
                    columns |> Option.iter (outArray tablePath "columns")
                    hierarchies |> Option.iter (outArray tablePath "hierarchies")
                    partitions |> Option.iter (outArray tablePath "partitions")
                    annotations |> Option.iter (outArray tablePath "annotations")
                    calculationItems |> Option.iter (outArray tablePath "calculationItems")
            | None -> ()
        | _ -> write databaseJsonFileName root

        Ok(
            files
            |> Seq.sortWith (fun a b -> String.CompareOrdinal(a.Path, b.Path))
            |> List.ofSeq
        )
    with e ->
        Error e.Message

// ---- Reading ----------------------------------------------------------------------------------------

/// The order files are read in: names with a number prefix ("000 Sales", as PrefixFilenames writes)
/// by that number, then the rest, ordinally. TE2 sorts by the current culture, which puts "1000 x"
/// before "101 x" and so loses the order of a folder with more than 1000 objects.
let fileOrder (a: string) (b: string) =
    let key (name: string) =
        let digits = name |> Seq.takeWhile Char.IsAsciiDigit |> Seq.length

        if digits > 0 && digits < name.Length && name.[digits] = ' ' then
            let number = name.Substring(0, digits).TrimStart '0'
            0, number.Length, number, name
        else
            1, 0, "", name

    let ga, la, na, ra = key a
    let gb, lb, nb, rb = key b

    match compare ga gb with
    | 0 ->
        match compare la lb with
        | 0 ->
            match String.CompareOrdinal(na, nb) with
            | 0 -> String.CompareOrdinal(ra, rb)
            | n -> n
        | n -> n
    | n -> n

let private ordinal (paths: string array) =
    paths
    |> Array.sortWith (fun a b -> fileOrder (Path.GetFileName a) (Path.GetFileName b))

let private jsonFiles (dir: string) =
    Directory.GetFiles dir
    |> Array.filter (fun f -> f.EndsWith(".json", StringComparison.OrdinalIgnoreCase))
    |> ordinal

let private parseFile (path: string) : JsonObject =
    let node =
        try
            JsonText.parse (File.ReadAllText path)
        with :? JsonException as e ->
            failwith $"{path}: not valid JSON ({e.Message})"

    match node with
    | :? JsonObject as o -> o
    | _ -> failwith $"{path}: not a JSON object"

/// Add the objects in <dir>/<array name>/*.json (in file name order) to the array at a dotted path.
let private inArray (dir: string) (path: string) (target: JsonObject) =
    let parts = path.Split '.'
    let arrayName = parts.[parts.Length - 1]
    let folder = Path.Combine(dir, arrayName)

    if Directory.Exists folder then
        let items = jsonFiles folder |> Array.map parseFile

        let parent =
            parts.[0 .. parts.Length - 2]
            |> Array.fold
                (fun (o: JsonObject) key ->
                    match o.[key] with
                    | :? JsonObject as child -> child
                    | _ -> failwith $"{folder} holds {arrayName}, but {key} is missing from its parent's file")
                target

        let array = getOrCreateArray parent arrayName

        for item in items do
            array.Add item

/// Find or add the object called `name` in an array.
let private getOrCreateIn (o: JsonObject) (arrayName: string) (name: string) =
    let array = getOrCreateArray o arrayName

    match array |> Seq.tryFind (fun n -> nameOf n = Some name) with
    | Some(:? JsonObject as found) -> found
    | _ ->
        let created = JsonObject()
        created.["name"] <- JsonValue.Create name
        array.Add created
        created

let private stringSet (json: string option) =
    match json with
    | None -> HashSet<string>(StringComparer.OrdinalIgnoreCase)
    | Some text ->
        match JsonText.parse text with
        | :? JsonArray as a -> HashSet<string>(a |> Seq.choose str, StringComparer.OrdinalIgnoreCase)
        | _ -> failwith $"not a list of names: {text}"

/// TE's annotation-stored translations, perspectives and relationships, put back where TOM keeps them
/// (SplitModelSerializer.ResolveAnnotations).
let private resolveAnnotations (model: JsonObject) =
    // Relationships, from each table.
    let fromTables = [
        for t in objects model "tables" do
            match annotation t Annotations.relationships true with
            | Some json ->
                match JsonText.parse json with
                | :? JsonArray as a -> yield! detach a
                | _ -> failwith $"table {nameOf t}: {Annotations.relationships} is not a list"
            | None -> ()
    ]

    if not fromTables.IsEmpty then
        // TE2 replaces the model's relationships with these; tessera adds them.
        let rels = getOrCreateArray model "relationships"

        for r in fromTables do
            rels.Add r

    // Perspectives (without members).
    match annotation model Annotations.perspectives true with
    | Some json ->
        match JsonText.parse json with
        | :? JsonArray as a ->
            for p in a do
                match p with
                | :? JsonObject as sp ->
                    let field (key: string) =
                        sp
                        |> Seq.tryFind (fun kv -> String.Equals(kv.Key, key, StringComparison.OrdinalIgnoreCase))
                        |> Option.map (fun kv -> kv.Value)

                    let name =
                        field "Name"
                        |> Option.bind str
                        |> Option.defaultWith (fun () -> failwith "a perspective has no name")

                    let target = getOrCreateIn model "perspectives" name

                    match field "Description" |> Option.bind str with
                    | Some d -> target.["description"] <- JsonValue.Create d
                    | None -> ()

                    match field "Annotations" with
                    | Some(:? JsonObject as anns) when anns.Count > 0 ->
                        let target = getOrCreateArray target "annotations"

                        for KeyValue(k, v) in anns do
                            let a = JsonObject()
                            a.["name"] <- JsonValue.Create k
                            a.["value"] <- JsonValue.Create(str v |> Option.defaultValue "")
                            target.Add a
                    | _ -> ()

                    match field "ExtendedProperties" with
                    | Some(:? JsonObject as props) when props.Count > 0 ->
                        let target = getOrCreateArray target "extendedProperties"

                        for KeyValue(k, v) in props do
                            let text = str v |> Option.defaultValue ""
                            let e = JsonObject()
                            e.["name"] <- JsonValue.Create k

                            match
                                (try
                                    Some(JsonText.parse text)
                                 with _ ->
                                     None)
                            with
                            | Some parsed when not (isNull parsed) ->
                                e.["type"] <- JsonValue.Create "json"
                                e.["value"] <- parsed
                            | _ -> e.["value"] <- JsonValue.Create text

                            target.Add e
                    | _ -> ()
                | _ -> ()
        | _ -> failwith $"{Annotations.perspectives} is not a list"
    | None -> ()

    // Cultures (without translations).
    match annotation model Annotations.cultures true with
    | Some json ->
        for c in stringSet (Some json) |> Seq.sortWith (fun a b -> String.CompareOrdinal(a, b)) do
            getOrCreateIn model "cultures" c |> ignore
    | None -> ()

    // Perspective membership.
    let perspectives = objects model "perspectives"

    for t in objects model "tables" do
        let inPerspectives = stringSet (annotation t Annotations.inPerspective true)
        let tableName = nameOf t |> Option.defaultValue ""

        let members = [
            for p in perspectives do
                match nameOf p with
                | Some pn when inPerspectives.Contains pn ->
                    let pt = JsonObject()
                    pt.["name"] <- JsonValue.Create tableName
                    (getOrCreateArray p "tables").Add pt
                    pn, pt
                | _ -> ()
        ]

        if not members.IsEmpty then
            for collection in [ "measures"; "columns"; "hierarchies" ] do
                for item in objects t collection do
                    let objPerspectives = stringSet (annotation item Annotations.inPerspective true)

                    for pn, pt in members do
                        if objPerspectives.Contains pn then
                            let po = JsonObject()
                            po.["name"] <- item.["name"].DeepClone()
                            (getOrCreateArray pt collection).Add po

    // Translations.
    if model.ContainsKey "cultures" then
        let modelTranslation (culture: string) =
            let c = getOrCreateIn model "cultures" culture

            let translations =
                match c.["translations"] with
                | :? JsonObject as t -> t
                | _ ->
                    let t = JsonObject()
                    c.["translations"] <- t
                    t

            match translations.["model"] with
            | :? JsonObject as m -> m
            | _ ->
                let m = JsonObject()
                m.["name"] <- JsonValue.Create(nameOf model |> Option.defaultValue "Model")
                translations.["model"] <- m
                m

        let apply (o: JsonObject) (target: string -> JsonObject) =
            for annotationName, property in
                [
                    Annotations.translatedNames, "translatedCaption"
                    Annotations.translatedDescriptions, "translatedDescription"
                    Annotations.translatedDisplayFolders, "translatedDisplayFolder"
                ] do
                match annotation o annotationName true with
                | Some json ->
                    let pairs =
                        match JsonText.parse json with
                        // An old TE2 build wrote [{"Key": ..., "Value": ...}] instead of an object.
                        | :? JsonArray as a -> [
                            for kv in a do
                                match kv with
                                | :? JsonObject as p -> str p.["Key"] |> Option.defaultValue "", str p.["Value"]
                                | _ -> ()
                          ]
                        | :? JsonObject as m -> [ for KeyValue(k, v) in m -> k, str v ]
                        | _ -> failwith $"{annotationName} is not an object"

                    for culture, value in pairs do
                        match value with
                        | Some v -> (target culture).[property] <- JsonValue.Create v
                        | None -> ()
                | None -> ()

        let name (o: JsonObject) = nameOf o |> Option.defaultValue ""

        apply model modelTranslation

        for p in objects model "perspectives" do
            apply p (fun c -> getOrCreateIn (modelTranslation c) "perspectives" (name p))

        for t in objects model "tables" do
            let tableT c =
                getOrCreateIn (modelTranslation c) "tables" (name t)

            apply t tableT

            for m in objects t "measures" do
                apply m (fun c -> getOrCreateIn (tableT c) "measures" (name m))

            for col in objects t "columns" do
                apply col (fun c -> getOrCreateIn (tableT c) "columns" (name col))

            for h in objects t "hierarchies" do
                let hierarchyT c =
                    getOrCreateIn (tableT c) "hierarchies" (name h)

                apply h hierarchyT

                for l in objects h "levels" do
                    apply l (fun c -> getOrCreateIn (hierarchyT c) "levels" (name l))

/// Remove TE's object-level annotations that loading leaves (TE's ClearTabularEditorAnnotations), e.g.
/// perspective membership of a table that's in no perspective.
let rec private clearObjectAnnotations (o: JsonObject) =
    match o.["annotations"] with
    | :? JsonArray as anns ->
        for a in List.ofSeq anns do
            match nameOf a with
            | Some n when List.contains n Annotations.objectLevel -> anns.Remove a |> ignore
            | _ -> ()

        dropEmptyAnnotations o
    | _ -> ()

    for KeyValue(k, v) in List.ofSeq o do
        match v with
        | :? JsonArray as a when k <> "annotations" ->
            for item in a do
                match item with
                | :? JsonObject as child -> clearObjectAnnotations child
                | _ -> ()
        | :? JsonObject as child -> clearObjectAnnotations child
        | _ -> ()

/// Read a folder back into one TOM JSON (.bim) text. Files are read in fileOrder (TE2 uses the
/// current culture's order, and the file system's order for table folders).
let combine (dir: string) : Result<string, string> =
    try
        let databaseJson = Path.Combine(dir, databaseJsonFileName)

        if not (File.Exists databaseJson) then
            failwith $"no {databaseJsonFileName} in {dir}"

        let root = parseFile databaseJson

        match root.["model"] with
        | :? JsonObject as model ->
            inArray dir "dataSources" model
            inArray dir "expressions" model
            inArray dir "functions" model

            let tablesDir = Path.Combine(dir, "tables")

            if Directory.Exists tablesDir then
                let tables = getOrCreateArray model "tables"

                for tableDir in Directory.GetDirectories tablesDir |> ordinal do
                    match jsonFiles tableDir with
                    | [| file |] ->
                        let table = parseFile file
                        inArray tableDir "columns" table
                        inArray tableDir "partitions" table
                        inArray tableDir "measures" table
                        inArray tableDir "hierarchies" table
                        inArray tableDir "annotations" table
                        inArray tableDir "calculationGroup.calculationItems" table
                        tables.Add table
                    | _ -> failwith $"folder '{tableDir}' is expected to contain exactly one .json file"

            inArray dir "relationships" model
            inArray dir "cultures" model
            inArray dir "perspectives" model
            inArray dir "roles" model
            resolveAnnotations model

            for t in objects model "tables" do
                clearObjectAnnotations t

            dropEmptyAnnotations model
        | _ -> failwith $"{databaseJson} has no model"

        Ok(JsonText.write "\n" root)
    with e ->
        Error e.Message

// ---- Local translations, perspectives and relationships --------------------------------------------

/// Store translations, perspective membership and relationships as annotations on the objects, as the
/// Local* options ask (TranslationAnnotationSerializer, PerspectiveAnnotationSerializer,
/// RelationshipAnnotationSerializer). Changes `db`: pass a copy. Refuses what TE's annotations can't
/// hold, rather than write a folder that loses it.
module Local =
    let private setIn (find: string -> Annotation) (add: Annotation -> unit) (name: string) (value: string) =
        match find name with
        | null -> add (Annotation(Name = name, Value = value))
        | a -> a.Value <- value

    let private ordinalSort (xs: string seq) =
        xs |> Seq.sortWith (fun a b -> String.CompareOrdinal(a, b)) |> List.ofSeq

    let private stringArray (xs: string list) =
        "[" + String.Join(",", xs |> List.map JsonText.quote) + "]"

    /// Set an annotation on the objects TE's local annotations go on.
    let private setterOf (o: MetadataObject) : (string -> string -> unit) option =
        match o with
        | :? Model as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Table as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Column as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Measure as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Hierarchy as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Level as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | :? Perspective as x -> Some(setIn x.Annotations.Find x.Annotations.Add)
        | _ -> None

    let private describe (o: MetadataObject) =
        match o with
        | :? NamedMetadataObject as n -> $"{o.ObjectType} '{n.Name}'"
        | _ -> string o.ObjectType

    let private translations (model: Model) =
        let byObject =
            Dictionary<MetadataObject, Dictionary<TranslatedProperty, ResizeArray<string * string>>>(
                HashIdentity.Reference
            )

        let refused = ResizeArray<string>()

        for culture in model.Cultures do
            for t in culture.ObjectTranslations do
                if (setterOf t.Object).IsNone then
                    refused.Add(describe t.Object)
                elif not (String.IsNullOrEmpty t.Value) then
                    let props =
                        match byObject.TryGetValue t.Object with
                        | true, p -> p
                        | _ ->
                            let p = Dictionary()
                            byObject.[t.Object] <- p
                            p

                    let values =
                        match props.TryGetValue t.Property with
                        | true, v -> v
                        | _ ->
                            let v = ResizeArray()
                            props.[t.Property] <- v
                            v

                    values.Add(culture.Name, t.Value)

        if refused.Count > 0 then
            let list = String.Join(", ", refused |> Seq.distinct)

            Error
                $"LocalTranslations can't hold translations of {list} (TE stores them on models, tables, columns, measures, hierarchies, levels and perspectives only)"
        else
            for KeyValue(o, props) in byObject do
                let set = (setterOf o).Value

                for property, name in
                    [
                        TranslatedProperty.Caption, Annotations.translatedNames
                        TranslatedProperty.Description, Annotations.translatedDescriptions
                        TranslatedProperty.DisplayFolder, Annotations.translatedDisplayFolders
                    ] do
                    match props.TryGetValue property with
                    | true, values ->
                        let pairs =
                            values
                            |> Seq.sortWith (fun (a, _) (b, _) -> String.CompareOrdinal(a, b))
                            |> Seq.map (fun (c, v) -> JsonText.quote c + ":" + JsonText.quote v)

                        set name ("{" + String.Join(",", pairs) + "}")
                    | _ -> ()

            let hasContent (c: Culture) =
                not (isNull c.LinguisticMetadata) || c.Annotations.Count > 0

            if model.Cultures.Count > 0 && not (model.Cultures |> Seq.exists hasContent) then
                (setterOf model).Value
                    Annotations.cultures
                    (stringArray (ordinalSort [ for c in model.Cultures -> c.Name ]))

            Ok()

    let private perspectives (model: Model) =
        let lossy = [
            for p in model.Perspectives do
                if p.ExtendedProperties.Count > 0 then
                    $"perspective '{p.Name}' has extended properties"

                for pt in p.PerspectiveTables do
                    if pt.Annotations.Count > 0 || pt.ExtendedProperties.Count > 0 then
                        $"perspective '{p.Name}' has annotations or extended properties on table '{pt.Name}'"

                    for pc in pt.PerspectiveColumns do
                        if pc.Annotations.Count > 0 then
                            $"perspective '{p.Name}' has annotations on column '{pt.Name}'[{pc.Name}]"

                    for pm in pt.PerspectiveMeasures do
                        if pm.Annotations.Count > 0 then
                            $"perspective '{p.Name}' has annotations on measure [{pm.Name}]"

                    for ph in pt.PerspectiveHierarchies do
                        if ph.Annotations.Count > 0 then
                            $"perspective '{p.Name}' has annotations on hierarchy '{pt.Name}'[{ph.Name}]"
        ]

        match lossy with
        | first :: _ -> Error $"LocalPerspectives can't store this model's perspectives as annotations: {first}"
        | [] ->
            let membership =
                Dictionary<MetadataObject, ResizeArray<string>>(HashIdentity.Reference)

            let add (o: MetadataObject) (perspective: string) =
                match membership.TryGetValue o with
                | true, l -> l.Add perspective
                | _ -> membership.[o] <- ResizeArray [ perspective ]

            for p in model.Perspectives do
                for pt in p.PerspectiveTables do
                    add pt.Table p.Name

                    for pc in pt.PerspectiveColumns do
                        add pc.Column p.Name

                    for pm in pt.PerspectiveMeasures do
                        add pm.Measure p.Name

                    for ph in pt.PerspectiveHierarchies do
                        add ph.Hierarchy p.Name

            for KeyValue(o, names) in membership do
                (setterOf o).Value Annotations.inPerspective (stringArray (ordinalSort names))

            let serialized =
                model.Perspectives
                |> Seq.sortWith (fun a b -> String.CompareOrdinal(a.Name, b.Name))
                |> Seq.map (fun p ->
                    let anns =
                        p.Annotations
                        |> Seq.map (fun a -> JsonText.quote a.Name + ":" + JsonText.quote a.Value)

                    let description =
                        if isNull p.Description then
                            "null"
                        else
                            JsonText.quote p.Description

                    "{\"Name\":"
                    + JsonText.quote p.Name
                    + ",\"Description\":"
                    + description
                    + ",\"Annotations\":{"
                    + String.Join(",", anns)
                    + "}}")

            if model.Perspectives.Count > 0 then
                (setterOf model).Value Annotations.perspectives ("[" + String.Join(",", serialized) + "]")

            Ok()

    let private daxName (c: Column) =
        "'" + c.Table.Name.Replace("'", "''") + "'[" + c.Name.Replace("]", "]]") + "]"

    /// The names TOM gives relationships that have none: "Relationship", "Relationship 1", ...
    let private generatedName =
        Text.RegularExpressions.Regex(
            @"^Relationship( [1-9][0-9]*)?$",
            Text.RegularExpressions.RegexOptions.CultureInvariant
        )

    let private relationships (db: Database) (mode: Microsoft.AnalysisServices.CompatibilityMode) =
        let model = db.Model

        let incomplete =
            model.Relationships
            |> Seq.tryPick (fun r ->
                match r with
                | :? SingleColumnRelationship as s when isNull s.FromColumn || isNull s.ToColumn -> Some r.Name
                | :? SingleColumnRelationship -> None
                | other -> Some other.Name)

        match incomplete with
        | Some name -> Error $"LocalRelationships: relationship '{name}' is incomplete (no from or to column)"
        | None ->
            let hasVariations =
                model.Tables
                |> Seq.exists (fun t ->
                    t.Columns
                    |> Seq.exists (fun c -> c.Variations |> Seq.exists (fun v -> not (isNull v.Relationship))))

            let options =
                SerializeOptions(IgnoreInferredObjects = true, IgnoreInferredProperties = true, IgnoreTimestamps = true)

            for t in model.Tables do
                let rels =
                    model.Relationships
                    |> Seq.cast<SingleColumnRelationship>
                    |> Seq.filter (fun r -> obj.ReferenceEquals(r.FromTable, t))
                    |> Seq.sortWith (fun a b ->
                        match String.CompareOrdinal(a.FromColumn.Name, b.FromColumn.Name) with
                        | 0 -> String.CompareOrdinal(daxName a.ToColumn, daxName b.ToColumn)
                        | n -> n)

                let array = JsonArray()

                for r in rels do
                    let json =
                        JsonText.parse (JsonSerializer.SerializeObject(r, options, db.CompatibilityLevel, mode))
                        :?> JsonObject

                    // TE2 drops every name here (unless a column variation refers to one), so a
                    // load gives them TOM's defaults: a model's own names (Power BI Desktop's GUIDs)
                    // would be lost. tessera drops only names TOM gives back on load anyway.
                    if not hasVariations && generatedName.IsMatch r.Name then
                        json.Remove "name" |> ignore

                    array.Add json

                (setterOf t).Value Annotations.relationships (JsonText.write "\n" array)

            Ok()

    /// Add the annotations the options ask for. Translations first, then perspectives (whose
    /// annotation carries the perspectives' own translation annotations), then relationships, the
    /// order TE2 uses.
    let apply (o: FolderOptions) (mode: Microsoft.AnalysisServices.CompatibilityMode) (db: Database) =
        let step (wanted: bool) (f: unit -> Result<unit, string>) (r: Result<unit, string>) =
            r |> Result.bind (fun () -> if wanted then f () else Ok())

        if isNull db.Model then
            Ok()
        else
            Ok()
            |> step o.LocalTranslations (fun () -> translations db.Model)
            |> step o.LocalPerspectives (fun () -> perspectives db.Model)
            |> step o.LocalRelationships (fun () -> relationships db mode)
