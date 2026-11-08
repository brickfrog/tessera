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

// ---- diff ------------------------------------------------------------------

let private changeToken =
    function
    | Diff.Added -> "added"
    | Diff.Removed -> "removed"
    | Diff.Renamed -> "renamed"
    | Diff.Modified -> "modified"

let diffJson (changes: Diff.Change list) =
    let s = Diff.summarize changes
    let refOpt = Option.map ObjectRef.format >> optStr

    obj [
        "summary",
        obj [
            "added", num s.Added
            "removed", num s.Removed
            "renamed", num s.Renamed
            "modified", num s.Modified
        ]
        "changes",
        arr [
            for c in changes ->
                obj [
                    "change", str (changeToken c.Kind)
                    "label", str c.Label
                    "before", refOpt c.Before
                    "after", refOpt c.After
                    "properties",
                    arr [
                        for p in c.Properties ->
                            obj [ "path", str p.Path; "before", optStr p.Before; "after", optStr p.After ]
                    ]
                ]
        ]
    ]

let private quote (v: string option) =
    match v with
    | None -> "(none)"
    | Some s -> JsonSerializer.Serialize(s, jsonOptions |> fun o -> JsonSerializerOptions(o, WriteIndented = false))

/// Changed lines with this many unchanged lines of context around them.
let private context = 2

let private lineDiffText (out: TextWriter) (before: string) (after: string) =
    let ops = Diff.LineDiff.diff (before.Split '\n') (after.Split '\n') |> Array.ofList

    let changed i =
        match ops.[i] with
        | Diff.LineDiff.Same _ -> false
        | _ -> true

    let near i =
        seq { max 0 (i - context) .. min (ops.Length - 1) (i + context) }
        |> Seq.exists changed

    let mutable gap = false

    for i in 0 .. ops.Length - 1 do
        if near i then
            if gap then
                out.WriteLine("        …")

            gap <- false

            match ops.[i] with
            | Diff.LineDiff.Same l -> out.WriteLine($"        {l}")
            | Diff.LineDiff.Removed l -> out.WriteLine($"      - {l}")
            | Diff.LineDiff.Added l -> out.WriteLine($"      + {l}")
        else
            gap <- true

let diffText (out: TextWriter) (changes: Diff.Change list) =
    for c in changes do
        let symbol =
            match c.Kind with
            | Diff.Added -> "+"
            | Diff.Removed -> "-"
            | Diff.Renamed -> ">"
            | Diff.Modified -> "~"

        out.WriteLine($"{symbol} {c.Label}")

        for p in c.Properties do
            let multiLine (v: string option) =
                v |> Option.exists (fun s -> s.Contains '\n')

            if multiLine p.Before || multiLine p.After then
                out.WriteLine($"    {p.Path}:")
                lineDiffText out (defaultArg p.Before "") (defaultArg p.After "")
            else
                out.WriteLine($"    {p.Path}: {quote p.Before} -> {quote p.After}")

    let s = Diff.summarize changes

    if changes.IsEmpty then
        out.WriteLine("No differences.")
    else
        let parts =
            [
                s.Added, "added"
                s.Removed, "removed"
                s.Renamed, "renamed"
                s.Modified, "modified"
            ]
            |> List.filter (fun (n, _) -> n > 0)
            |> List.map (fun (n, w) -> $"{n} {w}")

        out.WriteLine()
        out.WriteLine(String.concat ", " parts)

// ---- deps ------------------------------------------------------------------

let private targetText =
    function
    | Deps.Resolved r -> ObjectRef.format r
    | Deps.Unresolved text -> $"? {text}"

/// expression:2,5  sortByColumn
let private viaText (via: (string * int option) list) =
    via
    |> List.groupBy fst
    |> List.map (fun (property, xs) ->
        match xs |> List.choose snd |> List.distinct with
        | [] -> property
        | lines -> property + ":" + (lines |> List.map string |> String.concat ","))
    |> String.concat "  "

let rec private nodeJson (n: Deps.Node) : JsonNode =
    obj [
        match n.Target with
        | Deps.Resolved r -> yield "ref", str (ObjectRef.format r)
        | Deps.Unresolved text -> yield "unresolved", str text

        yield
            "via",
            arr [
                for property, line in n.Via ->
                    obj [
                        "property", str property
                        "line",
                        (match line with
                         | Some l -> num l
                         | None -> null)
                    ]
            ]
        if n.Cycle then
            yield "cycle", bool true

        if not n.Children.IsEmpty then
            yield "children", arr (n.Children |> List.map nodeJson)
    ]

let private directionKey =
    function
    | Deps.DependsOn -> "dependsOn"
    | Deps.ReferencedBy -> "referencedBy"

let depsJson (root: ObjectRef) (trees: (Deps.Direction * Deps.Node list) list) =
    obj [
        yield "ref", str (ObjectRef.format root)

        for direction, nodes in trees do
            yield directionKey direction, arr (nodes |> List.map nodeJson)
    ]

let depsText (out: TextWriter) (root: ObjectRef) (trees: (Deps.Direction * Deps.Node list) list) =
    out.WriteLine(ObjectRef.format root)

    let rec write (indent: string) (nodes: Deps.Node list) =
        let width =
            nodes |> List.map (fun n -> (targetText n.Target).Length) |> List.fold max 0

        for n in nodes do
            let label = targetText n.Target
            let cycle = if n.Cycle then "  (circular)" else ""
            out.WriteLine($"{indent}{label.PadRight width}  {viaText n.Via}{cycle}")
            write (indent + "    ") n.Children

    for direction, nodes in trees do
        let heading =
            match direction with
            | Deps.DependsOn -> "depends on"
            | Deps.ReferencedBy -> "referenced by"

        out.WriteLine($"  {heading}")

        if nodes.IsEmpty then
            out.WriteLine("    (nothing)")
        else
            write "    " nodes

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
