/// Per-object serialization, shared by `get`, `find` and `diff`.
module Tessera.Serialize

open System
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Microsoft.AnalysisServices.Tabular.Serialization
open Microsoft.AnalysisServices.Tabular.Extensions

let private jsonOptions (children: bool) =
    SerializeOptions(
        IgnoreChildrenExceptAnnotations = not children,
        IgnoreInferredObjects = true,
        IgnoreInferredProperties = true,
        IgnoreTimestamps = true,
        SplitMultilineStrings = false,
        // Never print passwords and the like from `get`/`find`/`diff`.
        IncludeRestrictedInformation = false
    )

/// TOM's JSON for one object. Without children, child collections are left out but annotations kept.
/// The level and mode must be passed explicitly: the defaults (AS mode) reject Power BI-only
/// properties such as relatedColumnDetails, even when serializing an unrelated object.
let objectJson (db: Database) (children: bool) (o: MetadataObject) =
    JsonSerializer
        .SerializeObject(o, jsonOptions children, db.CompatibilityLevel, ModelIO.effectiveCompatibilityMode db)
        .Replace("\r\n", "\n")

/// TMDL for one object, as it would appear in its file (unqualified, tabs, LF).
let objectTmdl (children: bool) (o: MetadataObject) =
    let formatting =
        MetadataFormattingOptionsBuilder(MetadataSerializationStyle.Tmdl)
            .WithNewLineStyle(NewLineStyle.UnixStyle)
            .WithTabsIndentationMode()
            .GetOptions()

    let builder =
        MetadataSerializationOptionsBuilder(MetadataSerializationStyle.Tmdl)
            .WithFormattingOptions(formatting)
            .WithoutRestrictedInformation()

    let builder =
        if children then
            builder.WithChildrenMetadata()
        else
            builder.WithoutChildrenMetadata()

    TmdlSerializer.SerializeObject(o, builder.GetOptions(), false).Replace("\r\n", "\n").TrimEnd()
    + "\n"

let private scalar (v: JsonValue) =
    match v.GetValueKind() with
    | JsonValueKind.String -> v.GetValue<string>()
    | _ -> v.ToJsonString()

/// Arrays of {name, ...} objects (annotations, extendedProperties, ...) are keyed by name.
let private namedItems (a: JsonArray) =
    let names =
        a
        |> Seq.map (fun item ->
            match item with
            | :? JsonObject as o ->
                match o.["name"] with
                | :? JsonValue as v when v.GetValueKind() = JsonValueKind.String -> Some(v.GetValue<string>(), o)
                | _ -> None
            | _ -> None)
        |> List.ofSeq

    if not names.IsEmpty && names |> List.forall Option.isSome then
        let items = names |> List.choose id

        if items |> List.map fst |> List.distinct |> List.length = items.Length then
            Some items
        else
            None
    else
        None

/// Flatten an object's JSON into (path, value) leaves. Paths are dot-separated property names
/// ("source.expression"); named array items use their name ("annotations.PBI_FormatHint"), and an
/// item that is just {name, value} collapses to that value; other arrays use [index].
let flatten (json: string) : (string * string) list =
    let out = ResizeArray<string * string>()

    let join prefix key =
        if prefix = "" then key else prefix + "." + key

    let rec walk (prefix: string) (node: JsonNode) =
        match node with
        | null -> out.Add(prefix, "null")
        | :? JsonObject as o ->
            for KeyValue(k, v) in o do
                walk (join prefix k) v
        | :? JsonArray as a ->
            match namedItems a with
            | Some items ->
                for name, item in items do
                    let path = join prefix name

                    match item.["value"] with
                    | :? JsonValue as v when item.Count = 2 -> out.Add(path, scalar v)
                    | _ ->
                        for KeyValue(k, v) in item do
                            if k <> "name" then
                                walk (join path k) v
            | None -> a |> Seq.iteri (fun i v -> walk $"{prefix}[{i}]" v)
        | :? JsonValue as v -> out.Add(prefix, scalar v)
        | other -> out.Add(prefix, other.ToJsonString())

    walk "" (JsonNode.Parse json)
    List.ofSeq out

/// Flattened properties of an entry, without children. The model entry also carries the
/// database's compatibilityLevel, so level changes show up in diffs.
let properties (db: Database) (e: Inventory.ObjectEntry) =
    let own = flatten (objectJson db false e.Object)

    if e.Ref.Kind = ObjectKind.Model then
        ("compatibilityLevel", string db.CompatibilityLevel) :: own
    else
        own

/// Values at a path: the exact leaf, or every leaf below it ("source" -> "source.type", ...).
let select (path: string) (props: (string * string) list) =
    match props |> List.filter (fun (p, _) -> p = path) with
    | [] ->
        props
        |> List.filter (fun (p, _) ->
            p.StartsWith(path + ".", StringComparison.Ordinal)
            || p.StartsWith(path + "[", StringComparison.Ordinal))
    | exact -> exact
