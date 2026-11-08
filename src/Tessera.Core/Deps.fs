/// Which objects refer to which: DAX references plus structural links (sort-by columns,
/// hierarchy levels, relationships). Resolution follows TE2's FormulaFixup, plus VAR/parameter
/// shadowing and locally defined column names.
module Tessera.Deps

open System
open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

type Target =
    | Resolved of ObjectRef
    /// A reference that names no object in the model; Text is the reference as written.
    | Unresolved of text: string

type Reference = {
    From: ObjectRef
    Target: Target
    /// Where the reference lives: a DAX property path ("expression",
    /// "formatStringDefinition.expression", "tablePermissions.Sales.filterExpression") or a
    /// structural property ("sortByColumn", "column", "fromColumn", "toColumn").
    Via: string
    /// 1-based line in the DAX, for DAX references.
    Line: int option
    /// How the reference is written in the source (for DAX), or the property value.
    Text: string
    /// Offset of the reference's first character in the DAX (None for structural links).
    Start: int option
    /// Length of the reference in the DAX, so Text = dax.Substring(Start, Length).
    Length: int option
}

/// A DAX expression held by an object.
type DaxSource = {
    Owner: ObjectEntry
    Property: string
    Dax: string
    /// The table unqualified [Column] references resolve against.
    ContextTable: Table option
}

let private nonEmpty (s: string) = not (String.IsNullOrWhiteSpace s)

/// Every DAX expression in the model, attributed to the object a user would think owns it.
let daxSources (entries: ObjectEntry list) : DaxSource list =
    let src owner property dax ctx =
        if nonEmpty dax then
            Some {
                Owner = owner
                Property = property
                Dax = dax
                ContextTable = ctx
            }
        else
            None

    let tableEntries =
        entries
        |> List.choose (fun e ->
            match e.Object with
            | :? Table as t -> Some(t.Name, e)
            | _ -> None)
        |> dict

    [
        for e in entries do
            match e.Object with
            | :? Measure as m ->
                src e "expression" m.Expression (Some m.Table)

                if not (isNull m.FormatStringDefinition) then
                    src e "formatStringDefinition.expression" m.FormatStringDefinition.Expression (Some m.Table)

                if not (isNull m.DetailRowsDefinition) then
                    src e "detailRowsDefinition.expression" m.DetailRowsDefinition.Expression (Some m.Table)

                if not (isNull m.KPI) then
                    src e "kpi.statusExpression" m.KPI.StatusExpression (Some m.Table)
                    src e "kpi.targetExpression" m.KPI.TargetExpression (Some m.Table)
                    src e "kpi.trendExpression" m.KPI.TrendExpression (Some m.Table)
            | :? CalculatedColumn as c -> src e "expression" c.Expression (Some c.Table)
            | :? Table as t ->
                if not (isNull t.DefaultDetailRowsDefinition) then
                    src e "defaultDetailRowsDefinition.expression" t.DefaultDetailRowsDefinition.Expression (Some t)

                if not (isNull t.CalculationGroup) then
                    let cg = t.CalculationGroup

                    for name, x in
                        [
                            "calculationGroup.noSelectionExpression", cg.NoSelectionExpression
                            "calculationGroup.multipleOrEmptySelectionExpression", cg.MultipleOrEmptySelectionExpression
                        ] do
                        if not (isNull x) then
                            src e (name + ".expression") x.Expression (Some t)

                            if not (isNull x.FormatStringDefinition) then
                                src
                                    e
                                    (name + ".formatStringDefinition.expression")
                                    x.FormatStringDefinition.Expression
                                    (Some t)
            | :? Partition as p ->
                match p.Source with
                // A calculated table's DAX: attributed to the table, which is what people mean.
                | :? CalculatedPartitionSource as cps ->
                    match tableEntries.TryGetValue p.Table.Name with
                    | true, te -> src te "expression" cps.Expression (Some p.Table)
                    | _ -> None
                | _ -> None
            | :? CalculationItem as ci ->
                let table = ci.CalculationGroup.Table
                src e "expression" ci.Expression (Some table)

                if not (isNull ci.FormatStringDefinition) then
                    src e "formatStringDefinition.expression" ci.FormatStringDefinition.Expression (Some table)
            | :? ModelRole as r ->
                for tp in r.TablePermissions do
                    src e $"tablePermissions.{tp.Table.Name}.filterExpression" tp.FilterExpression (Some tp.Table)
            | :? Function as f -> src e "expression" f.Expression None
            | _ -> ()
    ]
    |> List.choose id

/// Name lookups, case-insensitive like Tabular itself.
type private Index(entries: ObjectEntry list) =
    let ci = StringComparer.OrdinalIgnoreCase
    let tables = Dictionary<string, ObjectEntry>(ci)
    let calendars = Dictionary<string, ObjectEntry>(ci)
    let functions = Dictionary<string, ObjectEntry>(ci)
    let measures = Dictionary<string, ObjectEntry>(ci)
    let tableMembers = Dictionary<string, ObjectEntry>(ci)
    let byObject = Dictionary<MetadataObject, ObjectEntry>(HashIdentity.Reference)
    let memberKey (table: string) (name: string) = table + "\u0000" + name

    do
        for e in entries do
            byObject.[e.Object] <- e

            match e.Object with
            | :? Table as t -> tables.[t.Name] <- e
            | :? Calendar as c -> calendars.[c.Name] <- e
            | :? Function as f -> functions.[f.Name] <- e
            | :? Measure as m ->
                measures.[m.Name] <- e
                // Columns win over measures for Table[Name]; add measures only where free.
                tableMembers.TryAdd(memberKey m.Table.Name m.Name, e) |> ignore
            | :? Column as c -> tableMembers.[memberKey c.Table.Name c.Name] <- e
            | _ -> ()

    member _.Table name =
        match tables.TryGetValue name with
        | true, e -> Some e
        | _ ->
            match calendars.TryGetValue name with
            | true, e -> Some e
            | _ -> None

    member _.Function name =
        match functions.TryGetValue name with
        | true, e -> Some e
        | _ -> None

    member _.Member (table: string) (name: string) =
        match tableMembers.TryGetValue(memberKey table name) with
        | true, e -> Some e
        | _ -> None

    member _.Measure name =
        match measures.TryGetValue name with
        | true, e -> Some e
        | _ -> None

    member _.Entry(o: MetadataObject) =
        match o with
        | null -> None
        | o ->
            match byObject.TryGetValue o with
            | true, e -> Some e
            | _ -> None

/// Names an expression defines for itself: VAR names, UDF parameters, and string literals (which
/// name columns made by ADDCOLUMNS, SELECTCOLUMNS, SUMMARIZE...). These shadow model objects.
let private locals (isFunction: bool) (tokens: Token array) =
    let names = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let columns = HashSet<string>(StringComparer.OrdinalIgnoreCase)

    let lambda =
        tokens
        |> Array.tryFindIndex (fun t -> t.Kind = TokenKind.Operator && t.Text = "=>")

    for i in 0 .. tokens.Length - 1 do
        let t = tokens.[i]
        let next = if i + 1 < tokens.Length then Some tokens.[i + 1] else None

        match t.Kind, next with
        | TokenKind.Identifier, Some n when
            t.Text.Equals("VAR", StringComparison.OrdinalIgnoreCase)
            && n.Kind = TokenKind.Identifier
            ->
            names.Add n.Text |> ignore
        // UDF parameter list: (name: TYPE, ...) before the first =>
        | TokenKind.Identifier, Some n when
            isFunction
            && n.Kind = TokenKind.Operator
            && n.Text = ":"
            && lambda |> Option.exists (fun l -> i < l)
            ->
            names.Add t.Text |> ignore
        | TokenKind.String, _ -> columns.Add t.Text |> ignore
        | _ -> ()

    names, columns

let private resolveSource (index: Index) (s: DaxSource) : Reference list =
    let tokens = tokenize s.Dax |> significant |> Array.ofList
    let names, localColumns = locals (s.Owner.Ref.Kind = ObjectKind.Function) tokens

    let reference (target: Target) (first: Token) (last: Token) =
        let length = last.Start + last.Length - first.Start

        {
            From = s.Owner.Ref
            Target = target
            Via = s.Property
            Line = Some(lineOf s.Dax first.Start)
            Text = s.Dax.Substring(first.Start, length)
            Start = Some first.Start
            Length = Some length
        }

    let resolved (e: ObjectEntry) = Resolved e.Ref

    let isOpenParen (t: Token) =
        t.Kind = TokenKind.Punctuation && t.Text = "("

    let out = ResizeArray<Reference>()
    let mutable i = 0

    while i < tokens.Length do
        let t = tokens.[i]
        let next = if i + 1 < tokens.Length then Some tokens.[i + 1] else None

        match t.Kind, next with
        // Table[Name]: a column, else a measure, in that table.
        | (TokenKind.Identifier | TokenKind.QuotedTable), Some n when n.Kind = TokenKind.Bracketed ->
            // ''[Col] names a column of an anonymous table (e.g. rows a function returns): local.
            let isLocal =
                (t.Kind = TokenKind.Identifier && names.Contains t.Text)
                || (t.Kind = TokenKind.QuotedTable && t.Text = "")

            if not isLocal then
                let target =
                    match index.Member t.Text n.Text with
                    | Some e -> resolved e
                    | None -> Unresolved(s.Dax.Substring(t.Start, n.Start + n.Length - t.Start))

                out.Add(reference target t n)

            i <- i + 1
        // Name( : a function call; only user-defined functions are model objects.
        | TokenKind.Identifier, Some n when isOpenParen n ->
            index.Function t.Text
            |> Option.iter (fun f -> out.Add(reference (resolved f) t t))
        // A bare name: a table (or calendar), unless the expression defines it.
        | TokenKind.Identifier, _ ->
            if not (names.Contains t.Text) then
                index.Table t.Text |> Option.iter (fun e -> out.Add(reference (resolved e) t t))
        // 'Quoted' is always meant as a table.
        | TokenKind.QuotedTable, _ ->
            let target =
                match index.Table t.Text with
                | Some e -> resolved e
                | None -> Unresolved(s.Dax.Substring(t.Start, t.Length))

            out.Add(reference target t t)
        // [Name]: a column of the context table, else a measure there, else a measure anywhere.
        | TokenKind.Bracketed, _ ->
            if not (localColumns.Contains t.Text) then
                let inContext =
                    s.ContextTable |> Option.bind (fun table -> index.Member table.Name t.Text)

                let target =
                    match inContext |> Option.orElse (index.Measure t.Text) with
                    | Some e -> resolved e
                    | None -> Unresolved(s.Dax.Substring(t.Start, t.Length))

                out.Add(reference target t t)
        | _ -> ()

        i <- i + 1

    List.ofSeq out

let private structural (index: Index) (entries: ObjectEntry list) : Reference list =
    let link (from: ObjectEntry) via (target: MetadataObject) (text: string) =
        index.Entry target
        |> Option.map (fun t -> {
            From = from.Ref
            Target = Resolved t.Ref
            Via = via
            Line = None
            Text = text
            Start = None
            Length = None
        })

    [
        for e in entries do
            match e.Object with
            | :? Column as c when not (isNull c.SortByColumn) ->
                link e "sortByColumn" c.SortByColumn c.SortByColumn.Name
            | :? Level as l when not (isNull l.Column) -> link e "column" l.Column l.Column.Name
            | :? SingleColumnRelationship as r ->
                if not (isNull r.FromColumn) then
                    link e "fromColumn" r.FromColumn $"{r.FromTable.Name}[{r.FromColumn.Name}]"

                if not (isNull r.ToColumn) then
                    link e "toColumn" r.ToColumn $"{r.ToTable.Name}[{r.ToColumn.Name}]"
            | _ -> ()
    ]
    |> List.choose id

type Graph = {
    Entries: ObjectEntry list
    References: Reference list
}

let build (db: Database) : Graph =
    let entries = objects db
    let index = Index entries

    {
        Entries = entries
        References =
            (daxSources entries |> List.collect (resolveSource index))
            @ structural index entries
    }

/// What an object refers to (resolved or not), in source order.
let dependsOn (g: Graph) (r: ObjectRef) =
    g.References |> List.filter (fun x -> x.From = r)

/// What refers to an object.
let referencedBy (g: Graph) (r: ObjectRef) =
    g.References |> List.filter (fun x -> x.Target = Resolved r)

/// References that name no object.
let unresolved (g: Graph) =
    g.References
    |> List.filter (fun x ->
        match x.Target with
        | Unresolved _ -> true
        | Resolved _ -> false)

type Direction =
    | DependsOn
    | ReferencedBy

/// One neighbour in a dependency tree: every place the link occurs, and (to the requested depth)
/// the neighbour's own neighbours.
type Node = {
    Target: Target
    /// (property, line) for each occurrence of this link.
    Via: (string * int option) list
    Children: Node list
    /// The target is already on the path from the root (a circular reference); not expanded.
    Cycle: bool
}

/// The neighbours of an object, grouped by target, expanded to `depth` levels (0 = unlimited).
let tree (direction: Direction) (depth: int) (g: Graph) (root: ObjectRef) : Node list =
    let rec expand (path: Set<ObjectRef>) (level: int) (r: ObjectRef) =
        let links =
            match direction with
            | DependsOn -> dependsOn g r |> List.map (fun x -> x.Target, (x.Via, x.Line))
            | ReferencedBy -> referencedBy g r |> List.map (fun x -> Resolved x.From, (x.Via, x.Line))

        links
        |> List.groupBy fst
        |> List.map (fun (target, occurrences) ->
            let via = occurrences |> List.map snd |> List.distinct

            match target with
            | Resolved t when path.Contains t -> {
                Target = target
                Via = via
                Children = []
                Cycle = true
              }
            | Resolved t when depth = 0 || level < depth -> {
                Target = target
                Via = via
                Children = expand (path.Add t) (level + 1) t
                Cycle = false
              }
            | _ -> {
                Target = target
                Via = via
                Children = []
                Cycle = false
              })

    expand (Set.singleton root) 1 root
