/// Where a model object is declared in its files: the TMDL file and line of `measure 'X'`, or the
/// line of `"name": "X"` in a .bim. Used by reports that point at source (SARIF). It reads the
/// files as text and never fails: what it can't place, it doesn't place.
module Tessera.SourceLocations

open System
open System.IO
open System.Text
open System.Text.Json

/// A place in a model file. `File` is a full path.
type SourceLocation = { File: string; Line: int option }

/// One step of an object's address: its kind and its name, lowercased (Tabular names are
/// case-insensitive).
type private Step = ObjectKind * string

type Index = {
    /// The file to point at when nothing better is known (model.bim, or model.tmdl / database.tmdl).
    MainFile: string option
    /// Declarations found exactly once, by their address.
    Declarations: Map<Step list, SourceLocation>
}

let empty = {
    MainFile = None
    Declarations = Map.empty
}

let private lower (s: string) = s.ToLowerInvariant()

/// The address of an object: the object and the named objects it lives in. A calculation item
/// lives in its table (the calculation group between them has no name).
let private address (r: ObjectRef) : Step list option =
    let step k n = k, lower n

    match r.Kind, r.Path with
    | ObjectKind.Model, _ -> Some [ step ObjectKind.Model "" ]
    | ObjectKind.Level, [ t; h; l ] ->
        Some [
            step ObjectKind.Table t
            step ObjectKind.Hierarchy h
            step ObjectKind.Level l
        ]
    | kind, [ t; n ] -> Some [ step ObjectKind.Table t; step kind n ]
    | kind, [ n ] -> Some [ step kind n ]
    | _ -> None

/// Keep entries seen exactly once: a name declared twice can't be placed.
let private unique (entries: (Step list * SourceLocation) seq) =
    entries
    |> Seq.groupBy fst
    |> Seq.choose (fun (key, xs) ->
        match List.ofSeq xs with
        | [ (_, loc) ] -> Some(key, loc)
        | _ -> None)
    |> Map.ofSeq

// ---- TMDL ---------------------------------------------------------------------------------------

let private tmdlKeywords =
    dict [
        "model", ObjectKind.Model
        "table", ObjectKind.Table
        "column", ObjectKind.Column
        "measure", ObjectKind.Measure
        "hierarchy", ObjectKind.Hierarchy
        "level", ObjectKind.Level
        "partition", ObjectKind.Partition
        "calculationItem", ObjectKind.CalculationItem
        "calendar", ObjectKind.Calendar
        "relationship", ObjectKind.Relationship
        "role", ObjectKind.Role
        "perspective", ObjectKind.Perspective
        "culture", ObjectKind.Culture
        "expression", ObjectKind.Expression
        "function", ObjectKind.Function
        "dataSource", ObjectKind.DataSource
    ]

/// The name after a TMDL keyword: 'quoted' (with '' for a quote) or bare, up to ` =` or the end.
let private tmdlName (rest: string) =
    let rest = rest.TrimStart()

    if rest.StartsWith "'" then
        let name = StringBuilder()
        let mutable i = 1
        let mutable closed = false

        while not closed && i < rest.Length do
            if rest.[i] = '\'' then
                if i + 1 < rest.Length && rest.[i + 1] = '\'' then
                    name.Append '\'' |> ignore
                    i <- i + 2
                else
                    closed <- true
            else
                name.Append rest.[i] |> ignore
                i <- i + 1

        if closed then Some(name.ToString()) else None
    else
        let name =
            match rest.IndexOf '=' with
            | -1 -> rest
            | i -> rest.Substring(0, i)

        match name.Trim() with
        | "" -> None
        | n -> Some n

/// Declarations in one TMDL file, nested by indentation. A `calculationGroup` has no name, so its
/// items belong to the table; anything else that isn't a declaration (a property, an expression
/// line) still opens a level, so a keyword-like line inside an expression never looks like a
/// child of the table.
let private tmdlDeclarations (file: string) : (Step list * SourceLocation) list =
    let lines = File.ReadAllLines file
    // (indent, step if a named declaration)
    let stack = Collections.Generic.Stack<int * Step option>()
    let found = ResizeArray()

    for i in 0 .. lines.Length - 1 do
        let line = lines.[i]
        let text = line.TrimStart()

        if text <> "" && not (text.StartsWith "///") then
            let indent = line.Length - text.Length

            while stack.Count > 0 && fst (stack.Peek()) >= indent do
                stack.Pop() |> ignore

            let keyword, rest =
                match text.IndexOf ' ' with
                | -1 -> text, ""
                | j -> text.Substring(0, j), text.Substring(j + 1)

            let step =
                match tmdlKeywords.TryGetValue keyword with
                | true, kind ->
                    tmdlName rest
                    |> Option.map (fun name -> kind, (if kind = ObjectKind.Model then "" else lower name))
                | _ -> None

            let transparent = keyword = "calculationGroup"

            match step with
            | Some s ->
                let parents = stack |> Seq.rev |> Seq.choose snd |> List.ofSeq

                // Only a declaration under named declarations alone (not inside a property or an
                // expression) is one.
                let underProperty = stack |> Seq.exists (fun (_, s) -> s.IsNone)

                if not underProperty then
                    found.Add(parents @ [ s ], { File = file; Line = Some(i + 1) })

                stack.Push(indent, Some s)
            | None when transparent && not (stack |> Seq.exists (fun (_, s) -> s.IsNone)) ->
                // Push nothing: its children are the table's.
                ()
            | None -> stack.Push(indent, None)

    List.ofSeq found

let private tmdlIndex (root: string) =
    let files =
        Directory.EnumerateFiles(root, "*.tmdl", SearchOption.AllDirectories)
        |> Seq.sort
        |> List.ofSeq

    let main =
        [ "model.tmdl"; "database.tmdl" ]
        |> List.map (fun f -> Path.Combine(root, f))
        |> List.tryFind File.Exists

    {
        MainFile = main
        Declarations = files |> Seq.collect tmdlDeclarations |> unique
    }

// ---- .bim ---------------------------------------------------------------------------------------

let private bimCollections =
    dict [
        "tables", ObjectKind.Table
        "columns", ObjectKind.Column
        "measures", ObjectKind.Measure
        "hierarchies", ObjectKind.Hierarchy
        "levels", ObjectKind.Level
        "partitions", ObjectKind.Partition
        "calculationItems", ObjectKind.CalculationItem
        "calendars", ObjectKind.Calendar
        "relationships", ObjectKind.Relationship
        "roles", ObjectKind.Role
        "perspectives", ObjectKind.Perspective
        "cultures", ObjectKind.Culture
        "expressions", ObjectKind.Expression
        "functions", ObjectKind.Function
        "dataSources", ObjectKind.DataSource
    ]

/// An open JSON array or object while walking a .bim.
type private Container(isObject: bool, holder: string, kind: ObjectKind option, steps: Step list) =
    member _.IsObject = isObject
    /// The property holding it ("" at the root).
    member _.Holder = holder
    /// For an element of a known collection (`tables`, `measures`...), its kind.
    member _.Kind = kind
    /// The named objects it is inside, and itself once its name has been read.
    member val Steps = steps with get, set
    member val Named = false with get, set

/// The line of each object's `"name"` in a .bim: walks the JSON with a reader, tracking which
/// named objects it is inside (`tables[...]`, then `measures[...]`, ...).
let private bimDeclarations (file: string) : (Step list * SourceLocation) list =
    let bytes = File.ReadAllBytes file

    // 1-based line of a byte offset.
    let lineStarts =
        let starts = ResizeArray [ 0 ]

        for i in 0 .. bytes.Length - 1 do
            if bytes.[i] = byte '\n' then
                starts.Add(i + 1)

        starts.ToArray()

    // The reader doesn't take a byte order mark; offsets are then relative to what follows it.
    let skip =
        if
            bytes.Length >= 3
            && bytes.[0] = 0xEFuy
            && bytes.[1] = 0xBBuy
            && bytes.[2] = 0xBFuy
        then
            3
        else
            0

    let lineOf (offset: int64) =
        let i = Array.BinarySearch(lineStarts, int offset + skip)
        if i >= 0 then i + 1 else ~~~i

    let found = ResizeArray()

    let options =
        JsonReaderOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

    let mutable reader =
        Utf8JsonReader(ReadOnlySpan(bytes, skip, bytes.Length - skip), options)

    let containers = Collections.Generic.Stack<Container>()
    // The property whose value comes next, in the innermost object.
    let mutable property: string = null

    let inherited () =
        containers
        |> Seq.tryPick (fun c -> if c.IsObject then Some c.Steps else None)
        |> Option.defaultValue []

    while reader.Read() do
        match reader.TokenType with
        | JsonTokenType.PropertyName -> property <- reader.GetString()
        | JsonTokenType.StartArray ->
            let holder = if isNull property then "" else property
            containers.Push(Container(false, holder, None, inherited ()))
            property <- null
        | JsonTokenType.StartObject ->
            let kind =
                if containers.Count > 0 && not (containers.Peek().IsObject) then
                    match bimCollections.TryGetValue(containers.Peek().Holder) with
                    | true, k -> Some k
                    | _ -> None
                else
                    None

            // The top-level "model" object (the .bim root holds it).
            if property = "model" && containers.Count = 1 then
                found.Add(
                    [ ObjectKind.Model, "" ],
                    {
                        File = file
                        Line = Some(lineOf reader.TokenStartIndex)
                    }
                )

            containers.Push(Container(true, (if isNull property then "" else property), kind, inherited ()))
            property <- null
        | JsonTokenType.EndArray
        | JsonTokenType.EndObject ->
            containers.Pop() |> ignore
            property <- null
        | JsonTokenType.String when property = "name" && containers.Count > 0 && containers.Peek().IsObject ->
            let current = containers.Peek()

            match current.Kind with
            | Some k when not current.Named ->
                current.Steps <- current.Steps @ [ k, lower (reader.GetString()) ]
                current.Named <- true

                found.Add(
                    current.Steps,
                    {
                        File = file
                        Line = Some(lineOf reader.TokenStartIndex)
                    }
                )
            | _ -> ()

            property <- null
        | _ -> property <- null

    List.ofSeq found

let private bimIndex (file: string) = {
    MainFile = Some file
    Declarations = bimDeclarations file |> unique
}

// ---- building and looking up --------------------------------------------------------------------

/// An index of where a model's objects are declared. Remote models (and anything that can't be
/// read) give an empty index: no file, no lines.
let build (source: ModelSource) : Index =
    try
        match source.Format with
        | ModelFormat.Bim when File.Exists source.Path -> bimIndex source.Path
        | ModelFormat.Tmdl when Directory.Exists source.Path -> tmdlIndex source.Path
        | _ -> empty
    with
    | :? IOException
    | :? UnauthorizedAccessException
    | :? JsonException -> empty

/// Where an object is declared, if known exactly.
let tryFind (index: Index) (r: ObjectRef) : SourceLocation option =
    address r |> Option.bind (fun a -> Map.tryFind a index.Declarations)

/// The best place to point at for an object: its declaration; else the nearest enclosing object
/// whose declaration is known (a level's hierarchy, a column's table); else the main file with no
/// line. `exact` says whether it is the object's own declaration.
let locate (index: Index) (r: ObjectRef) : (SourceLocation * bool) option =
    match tryFind index r with
    | Some loc -> Some(loc, true)
    | None ->
        let rec enclosing (steps: Step list) =
            match steps with
            | []
            | [ _ ] -> None
            | _ ->
                let parent = List.take (steps.Length - 1) steps

                match Map.tryFind parent index.Declarations with
                | Some loc -> Some loc
                | None -> enclosing parent

        let fromParent = address r |> Option.bind enclosing

        match fromParent with
        | Some loc -> Some(loc, false)
        | None -> index.MainFile |> Option.map (fun f -> { File = f; Line = None }, false)
