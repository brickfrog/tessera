/// Turning core results into text or JSON. Nothing in Tessera.Core writes output itself.
module Tessera.Cli.Render

open System.IO
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open Tessera
open Tessera.Inventory

let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let private str (s: string) : JsonNode = JsonValue.Create s
let private num (n: int) : JsonNode = JsonValue.Create n
let private bool (b: bool) : JsonNode = JsonValue.Create b

let private optStr =
    function
    | Some s -> str s
    | None -> null

let private obj (props: (string * JsonNode) list) : JsonNode =
    let o = JsonObject()

    for k, v in props do
        o.[k] <- v

    o

let private arr (items: JsonNode seq) : JsonNode = JsonArray(Array.ofSeq items)

let writeJson (out: TextWriter) (node: JsonNode) =
    out.WriteLine(node.ToJsonString jsonOptions)

let sourceJson (s: ModelSource) =
    obj [
        "format", str (ModelFormat.token s.Format)
        "path", str s.Path
        "semanticModelFolder", optStr s.SemanticModelFolder
    ]

let errorJson (code: string) (message: string) =
    obj [ "error", obj [ "code", str code; "message", str message ] ]

let versionJson (v: string) = obj [ "version", str v ]

let tessErrorJson (e: TessError) =
    errorJson (TessError.code e) (TessError.message e)

// ---- info ------------------------------------------------------------------

let infoJson (i: ModelInfo) =
    obj [
        "name", str i.Name
        "compatibilityLevel", num i.CompatibilityLevel
        "compatibilityMode", str i.CompatibilityMode
        "culture", optStr i.Culture
        "source", sourceJson i.Source
        "calculationGroups", num i.CalculationGroups
        "counts", obj [ for k, n in i.Counts -> ObjectKind.token k, num n ]
    ]

let private noun (kind: ObjectKind) n =
    let singular, plural =
        match kind with
        | ObjectKind.Hierarchy -> "hierarchy", "hierarchies"
        | ObjectKind.CalculationItem -> "calculation item", "calculation items"
        | ObjectKind.DataSource -> "data source", "data sources"
        | k -> ObjectKind.token k, ObjectKind.token k + "s"

    if n = 1 then singular else plural

let infoText (out: TextWriter) (i: ModelInfo) =
    let row (label: string) (value: string) = out.WriteLine($"{label, -15} {value}")

    let where =
        match i.Source.SemanticModelFolder with
        | Some sm -> $"{ModelFormat.token i.Source.Format} in PBIP semantic model {sm}"
        | None -> ModelFormat.token i.Source.Format

    row "Name" (if i.Name = "" then "(unnamed)" else i.Name)
    row "Format" where
    row "Path" i.Source.Path

    row
        "Compatibility"
        (if i.CompatibilityMode = "Unknown" then
             string i.CompatibilityLevel
         else
             $"{i.CompatibilityLevel} ({i.CompatibilityMode})")

    i.Culture |> Option.iter (row "Culture")
    out.WriteLine()

    for kind, n in i.Counts do
        if n > 0 then
            let extra =
                if kind = ObjectKind.Table && i.CalculationGroups > 0 then
                    let groups =
                        if i.CalculationGroups = 1 then
                            "calculation group"
                        else
                            "calculation groups"

                    $"  (incl. {i.CalculationGroups} {groups})"
                else
                    ""

            out.WriteLine($"{n, 6}  {noun kind n}{extra}")

// ---- list ------------------------------------------------------------------

let listJson (entries: ObjectEntry list) =
    arr [
        for e in entries ->
            obj [
                "ref", str (ObjectRef.format e.Ref)
                "kind", str (ObjectKind.token e.Ref.Kind)
                "name", str (ObjectEntry.name e)
                "table", optStr (ObjectEntry.table e |> Option.filter (fun _ -> e.Ref.Kind <> ObjectKind.Table))
                "hidden",
                (match e.IsHidden with
                 | Some h -> bool h
                 | None -> null)
                "detail", optStr e.Detail
            ]
    ]

/// One reference per line (pipe-friendly), with hidden flag and detail after two spaces.
let listText (out: TextWriter) (entries: ObjectEntry list) =
    for e in entries do
        let extras = [
            if e.IsHidden = Some true then
                "(hidden)"
            yield! Option.toList e.Detail
        ]

        match extras with
        | [] -> out.WriteLine(ObjectRef.format e.Ref)
        | xs -> out.WriteLine(ObjectRef.format e.Ref + "  " + String.concat "  " xs)

// ---- get -------------------------------------------------------------------

let private refFields (e: ObjectEntry) = [
    "ref", str (ObjectRef.format e.Ref)
    "kind", str (ObjectKind.token e.Ref.Kind)
]

let getJson (e: ObjectEntry) (objectJson: string) =
    obj (refFields e @ [ "object", JsonNode.Parse objectJson ])

let propertiesJson (e: ObjectEntry) (leaves: (string * string) list) =
    obj (refFields e @ [ "properties", obj [ for p, v in leaves -> p, str v ] ])

// ---- find ------------------------------------------------------------------

let findJson (hits: Find.Hit list) =
    arr [
        for h in hits ->
            obj [
                "ref", str (ObjectRef.format h.Ref)
                "kind", str (ObjectKind.token h.Ref.Kind)
                "property", str h.Path
                "line", num h.Line
                "text", str h.Text
            ]
    ]

/// ref  property[:line]  text: the line number only when the value has several lines.
let findText (out: TextWriter) (hits: Find.Hit list) =
    let multiLine =
        hits
        |> List.filter (fun h -> h.Line > 1)
        |> List.map (fun h -> h.Ref, h.Path)
        |> Set.ofList

    for h in hits do
        let where =
            if h.Line > 1 || multiLine.Contains(h.Ref, h.Path) then
                $"{h.Path}:{h.Line}"
            else
                h.Path

        out.WriteLine($"{ObjectRef.format h.Ref}  {where}  {h.Text}")

// ---- convert -----------------------------------------------------------------

let convertJson (source: ModelSource) (saved: ModelIO.SavedModel) =
    obj [
        "source", sourceJson source
        "output",
        obj [
            "format", str (ModelIO.OutputFormat.token saved.Format)
            "path", str saved.Path
            "eol", str (ModelIO.Eol.token saved.Eol)
            "files", arr [ for f in saved.Files -> str f ]
        ]
    ]

let convertText (out: TextWriter) (source: ModelSource) (saved: ModelIO.SavedModel) =
    let n = saved.Files.Length
    let files = if n = 1 then "1 file" else $"{n} files"
    out.WriteLine($"Read  {ModelFormat.token source.Format}  {source.Path}")

    out.WriteLine(
        $"Wrote {ModelIO.OutputFormat.token saved.Format}  {saved.Path}  ({files}, {ModelIO.Eol.token saved.Eol})"
    )
