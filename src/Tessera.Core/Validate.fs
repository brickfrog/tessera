/// Structural checks: things that are wrong in a model regardless of style, such as DAX that names
/// nothing, circular references, and names Tabular requires to be unique. Each issue carries a stable
/// rule id. Style rules belong to the best-practice analyzer, not here.
module Tessera.Validate

open System
open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

/// Qualified access: `Error` would otherwise shadow Result.Error.
[<RequireQualifiedAccess>]
type Severity =
    | Error
    | Warning
    | Info

module Severity =
    /// Most severe first.
    let all = [ Severity.Error; Severity.Warning; Severity.Info ]

    let token =
        function
        | Severity.Error -> "error"
        | Severity.Warning -> "warning"
        | Severity.Info -> "info"

    let tryParse (s: string) =
        let s = s.Trim().ToLowerInvariant()
        all |> List.tryFind (fun x -> token x = s)

    /// Higher is more severe.
    let rank =
        function
        | Severity.Error -> 2
        | Severity.Warning -> 1
        | Severity.Info -> 0

    /// Whether `s` is `threshold` or more severe.
    let atLeast (threshold: Severity) (s: Severity) = rank s >= rank threshold

type Issue = {
    /// Stable kebab-case rule id, e.g. "unresolved-reference".
    Rule: string
    Severity: Severity
    /// The object the issue is about.
    Ref: ObjectRef
    /// The property it's in, using the same paths as `get -p` and `deps`, e.g. "expression".
    Property: string option
    /// 1-based line in that property's DAX.
    Line: int option
    /// 1-based column on that line, where the position is known exactly.
    Column: int option
    Message: string
}

/// Every rule id, with a one-line description, in the order the checks run.
let rules = [
    "unresolved-reference",
    "a DAX reference names no table, column or measure (unqualified [X] is a warning: it may be a column of a table expression)"
    "circular-reference", "measures, calculated columns, calculated tables or functions that depend on themselves"
    "invalid-token", "an unterminated string, quoted name, [name] or comment, or a character DAX doesn't use"
    "empty-expression", "a measure, calculated column, calculated table, calculation item or function with no DAX"
    "duplicate-name",
    "a column, measure and hierarchy that share a name in one table, or measures that share a name across tables"
    "relationship-datatype-mismatch", "a relationship between columns of different data types"
    "relationship-self-join", "a relationship from a table to itself"
    "invalid-sort-by", "a column sorted by itself, or by a column that is itself sorted by another"
    "tom-validation", "TOM's own validation, e.g. a property that refers to an object that can't be found"
]

let private issue rule severity (r: ObjectRef) property line message = {
    Rule = rule
    Severity = severity
    Ref = r
    Property = property
    Line = line
    Column = None
    Message = message
}

let private sameName (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// 'Table'[Name]
let private columnText (c: Column) =
    quoteTable c.Table.Name + bracket c.Name

// --- unresolved-reference ------------------------------------------------------------------------

let private unresolvedReferences (g: Deps.Graph) =
    let daxOf =
        Deps.daxSources g.Entries
        |> List.map (fun s -> (s.Owner.Ref, s.Property), s.Dax)
        |> List.distinctBy fst
        |> dict

    let columnOf (r: Deps.Reference) =
        match r.Start, daxOf.TryGetValue((r.From, r.Via)) with
        | Some start, (true, dax) -> Some(columnOf dax start)
        | _ -> None

    let tableExists name =
        g.Entries
        |> List.exists (fun e ->
            (e.Ref.Kind = ObjectKind.Table || e.Ref.Kind = ObjectKind.Calendar)
            && sameName (ObjectEntry.name e) name)

    [
        for r in Deps.unresolved g do
            match r.Target with
            | Deps.Resolved _ -> ()
            | Deps.Unresolved text ->
                let at severity message = {
                    issue "unresolved-reference" severity r.From (Some r.Via) r.Line message with
                        Column = columnOf r
                }

                match tokenize text |> significant with
                | [ t; n ] when Token.isName t && n.Kind = TokenKind.Bracketed ->
                    if tableExists t.Text then
                        at
                            Severity.Error
                            $"{text} does not exist: {quoteTable t.Text} has no column or measure {bracket n.Text}"
                    else
                        at Severity.Error $"{text} does not exist: there is no table {quoteTable t.Text}"
                | [ n ] when n.Kind = TokenKind.Bracketed ->
                    // The resolver can't see columns of table expressions (e.g. a table-valued UDF
                    // parameter, or a column of a variable's table), so this may be a false alarm.
                    let detail =
                        match r.From.Kind, ObjectRef.tableName r.From with
                        | (ObjectKind.Measure | ObjectKind.Column | ObjectKind.CalculationItem | ObjectKind.Table),
                          Some table -> $"{quoteTable table} has no such column, and there is no such measure"
                        | _ -> "there is no such measure"

                    at Severity.Warning $"{text} does not exist: {detail}"
                | _ -> at Severity.Error $"{text} does not exist"
    ]

// --- circular-reference --------------------------------------------------------------------------

/// Strongly connected components (Tarjan), iteratively so long chains can't overflow the stack.
/// Components come out in reverse topological order; members in discovery order.
let private stronglyConnected (nodes: ObjectRef list) (successors: ObjectRef -> ObjectRef list) =
    let index = Dictionary<ObjectRef, int>()
    let low = Dictionary<ObjectRef, int>()
    let onStack = HashSet<ObjectRef>()
    let stack = Stack<ObjectRef>()
    let work = Stack<ObjectRef * ObjectRef list>()
    let components = ResizeArray<ObjectRef list>()
    let mutable counter = 0

    let visit v =
        index.[v] <- counter
        low.[v] <- counter
        counter <- counter + 1
        stack.Push v
        onStack.Add v |> ignore
        work.Push(v, successors v)

    for root in nodes do
        if not (index.ContainsKey root) then
            visit root

            while work.Count > 0 do
                match work.Pop() with
                | v, w :: rest ->
                    work.Push(v, rest)

                    if not (index.ContainsKey w) then
                        visit w
                    elif onStack.Contains w then
                        low.[v] <- min low.[v] index.[w]
                | v, [] ->
                    if work.Count > 0 then
                        let parent, _ = work.Peek()
                        low.[parent] <- min low.[parent] low.[v]

                    if low.[v] = index.[v] then
                        let members = ResizeArray<ObjectRef>()
                        let mutable fin = false

                        while not fin do
                            let w = stack.Pop()
                            onStack.Remove w |> ignore
                            members.Add w
                            fin <- (w = v)

                        components.Add(members |> Seq.rev |> List.ofSeq)

    List.ofSeq components

/// The shortest path from `start` back to itself, staying inside `within` (breadth-first).
let private shortestCycle (successors: ObjectRef -> ObjectRef list) (within: HashSet<ObjectRef>) start =
    let parent = Dictionary<ObjectRef, ObjectRef>()
    let queue = Queue<ObjectRef>([ start ])
    let mutable last = None

    while last.IsNone && queue.Count > 0 do
        let v = queue.Dequeue()

        for w in successors v do
            if last.IsNone && within.Contains w then
                if w = start then
                    last <- Some v
                elif w <> start && not (parent.ContainsKey w) then
                    parent.[w] <- v
                    queue.Enqueue w

    let rec back (v: ObjectRef) acc =
        if v = start then
            start :: acc
        else
            back parent.[v] (v :: acc)

    last |> Option.map (fun v -> back v [ start ])

let private circularReferences (g: Deps.Graph) =
    let order = g.Entries |> List.mapi (fun i e -> e.Ref, i) |> dict

    let computed (r: ObjectRef) =
        match r.Kind with
        | ObjectKind.Measure
        | ObjectKind.Column
        | ObjectKind.Table
        | ObjectKind.CalculationItem
        | ObjectKind.Function -> true
        | _ -> false

    // Only the main DAX expression takes part in evaluation order. Format strings may refer to their
    // own measure, and detail rows / calculation group expressions are attributed to the table.
    let daxEdges =
        g.References
        |> List.choose (fun r ->
            match r.Target with
            | Deps.Resolved target when r.Via = "expression" && computed r.From ->
                // A calculated table's [X] resolves against its own columns, but those come from the
                // expression itself (e.g. CALENDAR's [Date]); it can't depend on its own output.
                let ownColumn =
                    r.From.Kind = ObjectKind.Table
                    && target.Kind = ObjectKind.Column
                    && ObjectRef.tableName target = ObjectRef.tableName r.From

                if ownColumn then None else Some(r.From, (target, r.Line))
            | _ -> None)

    // A calculated table's columns are produced by the table's expression.
    let tableColumnEdges =
        g.Entries
        |> List.choose (fun e ->
            match e.Object with
            | :? CalculatedTableColumn as c -> Some(e.Ref, (ObjectRef.table c.Table.Name, None))
            | _ -> None)

    let edges =
        (daxEdges @ tableColumnEdges)
        |> List.groupBy fst
        |> List.map (fun (from, xs) -> from, xs |> List.map snd |> List.distinctBy fst)
        |> dict

    let edgesOf r =
        match edges.TryGetValue r with
        | true, xs -> xs
        | _ -> []

    let successors r = edgesOf r |> List.map fst
    let nodes = g.Entries |> List.map (fun e -> e.Ref) |> List.filter computed

    [
        for scc in stronglyConnected nodes successors do
            let isCycle =
                match scc with
                | [ single ] -> successors single |> List.contains single
                | _ -> true

            if isCycle then
                let start = scc |> List.minBy (fun r -> order.[r])
                let within = HashSet<ObjectRef>(scc)

                match shortestCycle successors within start with
                | None -> ()
                | Some path ->
                    let line =
                        match path with
                        | a :: b :: _ -> edgesOf a |> List.tryFind (fun (t, _) -> t = b) |> Option.bind snd
                        | _ -> None

                    let chain = path |> List.map ObjectRef.format |> String.concat " -> "

                    let others =
                        scc
                        |> List.filter (fun r -> not (List.contains r path))
                        |> List.sortBy (fun r -> order.[r])

                    let message =
                        match others with
                        | [] -> chain
                        | more ->
                            let list = more |> List.map ObjectRef.format |> String.concat ", "
                            $"{chain} (also in this loop: {list})"

                    issue "circular-reference" Severity.Error start (Some "expression") line message
    ]

// --- invalid-token -------------------------------------------------------------------------------

let private snippet (text: string) =
    let firstLine = text.Split([| '\r'; '\n' |]).[0]

    if firstLine.Length > 30 then
        firstLine.Substring(0, 30) + "..."
    else
        firstLine

let private invalidTokens (g: Deps.Graph) = [
    for s in Deps.daxSources g.Entries do
        for t in tokenize s.Dax do
            let problem =
                match t.Kind with
                | TokenKind.Invalid ->
                    match t.Text with
                    | x when x.StartsWith "\"" -> Some $"unterminated string: {snippet x}"
                    | x when x.StartsWith "'" -> Some $"unterminated quoted name (names can't span lines): {snippet x}"
                    | x when x.StartsWith "[" -> Some $"unterminated [name] (names can't span lines): {snippet x}"
                    | x when x.StartsWith("DT", StringComparison.OrdinalIgnoreCase) ->
                        Some $"unterminated date literal: {snippet x}"
                    | x -> Some $"unexpected character '{x}'"
                | TokenKind.Comment when t.Text.StartsWith "/*" && (t.Length < 4 || not (t.Text.EndsWith "*/")) ->
                    Some "unterminated /* comment"
                | _ -> None

            match problem with
            | Some message -> {
                issue "invalid-token" Severity.Error s.Owner.Ref (Some s.Property) (Some(lineOf s.Dax t.Start)) message with
                    Column = Some(columnOf s.Dax t.Start)
              }
            | None -> ()
]

// --- empty-expression ----------------------------------------------------------------------------

let private emptyExpressions (g: Deps.Graph) =
    let blank (s: string) = String.IsNullOrWhiteSpace s

    let tableRef (t: Table) =
        g.Entries
        |> List.tryFind (fun e -> obj.ReferenceEquals(e.Object, t))
        |> Option.map (fun e -> e.Ref)

    [
        for e in g.Entries do
            let empty =
                match e.Object with
                | :? Measure as m when blank m.Expression -> Some(e.Ref, "measure")
                | :? CalculatedColumn as c when blank c.Expression -> Some(e.Ref, "calculated column")
                | :? CalculationItem as ci when blank ci.Expression -> Some(e.Ref, "calculation item")
                | :? Function as f when blank f.Expression -> Some(e.Ref, "function")
                | :? Partition as p ->
                    match p.Source with
                    | :? CalculatedPartitionSource as cps when blank cps.Expression ->
                        tableRef p.Table |> Option.map (fun r -> r, "calculated table")
                    | _ -> None
                | _ -> None

            match empty with
            | Some(r, what) ->
                issue "empty-expression" Severity.Warning r (Some "expression") None $"{what} has no DAX expression"
            | None -> ()
    ]

// --- duplicate-name ------------------------------------------------------------------------------

/// Names the collections don't already keep unique (TOM rejects duplicate tables, and duplicate
/// columns or measures within one table, on load).
let private duplicateNames (g: Deps.Graph) =
    let key (e: ObjectEntry) = (ObjectEntry.name e).ToUpperInvariant()

    let collisions (entries: ObjectEntry list) =
        entries
        |> List.groupBy key
        |> List.collect (fun (_, group) ->
            match group with
            | first :: rest -> rest |> List.map (fun e -> first, e)
            | [] -> [])

    let tableMembers =
        g.Entries
        |> List.filter (fun e ->
            match e.Ref.Kind with
            | ObjectKind.Column
            | ObjectKind.Measure
            | ObjectKind.Hierarchy -> true
            | _ -> false)

    [
        for _, members in tableMembers |> List.groupBy ObjectEntry.table do
            for first, dup in collisions members do
                issue
                    "duplicate-name"
                    Severity.Error
                    dup.Ref
                    None
                    None
                    $"name {bracket (ObjectEntry.name dup)} is also used by {ObjectRef.format first.Ref} in the same table"

        let measures =
            tableMembers |> List.filter (fun e -> e.Ref.Kind = ObjectKind.Measure)

        for first, dup in collisions measures do
            issue
                "duplicate-name"
                Severity.Error
                dup.Ref
                None
                None
                $"measure name {bracket (ObjectEntry.name dup)} is also used by {ObjectRef.format first.Ref}; measure names must be unique in the model"
    ]

// --- relationships -------------------------------------------------------------------------------

let private relationships (g: Deps.Graph) =
    let untyped =
        function
        | DataType.Automatic
        | DataType.Unknown
        | DataType.Variant -> true
        | _ -> false

    [
        for e in g.Entries do
            match e.Object with
            | :? SingleColumnRelationship as r when not (isNull r.FromColumn) && not (isNull r.ToColumn) ->
                let a, b = r.FromColumn, r.ToColumn

                if a.DataType <> b.DataType && not (untyped a.DataType) && not (untyped b.DataType) then
                    issue
                        "relationship-datatype-mismatch"
                        Severity.Warning
                        e.Ref
                        None
                        None
                        $"{columnText a} is {a.DataType} but {columnText b} is {b.DataType}"

                if obj.ReferenceEquals(a.Table, b.Table) then
                    issue
                        "relationship-self-join"
                        Severity.Error
                        e.Ref
                        None
                        None
                        $"{columnText a} -> {columnText b} relates {quoteTable a.Table.Name} to itself; tabular models don't allow self-joins"
            | _ -> ()
    ]

// --- invalid-sort-by -----------------------------------------------------------------------------

/// TOM accepts these, but Analysis Services and Power BI don't. (A sort-by column in another table
/// can't be written to .bim or TMDL: it comes back unresolved, which tom-validation reports.)
let private sortBy (g: Deps.Graph) = [
    for e in g.Entries do
        match e.Object with
        | :? Column as c when not (isNull c.SortByColumn) ->
            let s = c.SortByColumn

            let message =
                if obj.ReferenceEquals(s, c) then
                    Some "sorted by itself"
                elif not (isNull s.SortByColumn) then
                    Some
                        $"sorted by {bracket s.Name}, which is itself sorted by {bracket s.SortByColumn.Name}; a sort-by column can't have its own sort-by column"
                else
                    None

            match message with
            | Some m -> issue "invalid-sort-by" Severity.Warning e.Ref (Some "sortByColumn") None m
            | None -> ()
        | _ -> ()
]

// --- tom-validation ------------------------------------------------------------------------------

let private tomValidation (db: Database) (g: Deps.Graph) =
    let byObject = Dictionary<MetadataObject, ObjectRef>(HashIdentity.Reference)

    for e in g.Entries do
        byObject.[e.Object] <- e.Ref

    /// The nearest addressable object: the source itself, else its closest addressable parent.
    let rec owner (o: MetadataObject) =
        match o with
        | null -> ObjectRef.model
        | o ->
            match byObject.TryGetValue o with
            | true, r -> r
            | _ -> owner o.Parent

    try
        [
            for e in db.Model.Validate().Errors do
                issue "tom-validation" Severity.Error (owner e.Source) None None e.Message
        ]
    with ex -> [
        issue
            "tom-validation"
            Severity.Warning
            ObjectRef.model
            None
            None
            $"TOM's validation could not run: {ex.Message}"
    ]

/// Every issue in the model, in model-tree order.
let validate (db: Database) : Issue list =
    let g = Deps.build db
    let order = g.Entries |> List.mapi (fun i e -> e.Ref, i) |> dict

    let position (i: Issue) =
        match order.TryGetValue i.Ref with
        | true, n -> n
        | _ -> Int32.MaxValue

    [
        yield! unresolvedReferences g
        yield! circularReferences g
        yield! invalidTokens g
        yield! emptyExpressions g
        yield! duplicateNames g
        yield! relationships g
        yield! sortBy g
        yield! tomValidation db g
    ]
    // Stable: issues on one object keep rule order.
    |> List.sortBy position
