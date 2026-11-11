namespace Tessera.DaxEngine

// Row-level security: which rows of the model members of some roles can see.
//
// A role's row filter is a DAX expression on one table, "a DAX formula that evaluates to a
// TRUE/FALSE value, defines the rows that can be queried by members of that particular role. Rows
// not included in the DAX formula can't be queried" (Microsoft Learn, "Roles in Analysis Services
// tabular models"). The engine applies roles by building a copy of the prepared model in which the
// hidden rows don't exist (see `Engine.asRoles`), because that is what the sources describe:
// "When a security filter is active, DAX cannot remove it ... Any customer from outside Europe
// disappears as if they did not exist. In other words, when ALL ( Customer ) is executed, ALL
// removes all the filters from the Customer table except for the security filters" (SQLBI,
// "Computing accurate percentages with row-level security in Power BI"), and the filter is not one
// a query can see: with a role filtering Customer, `ISCROSSFILTERED ( Customer )` is still FALSE
// where the query doesn't filter Customer (SQLBI, "Implement non-visual totals with Power BI
// security roles": "any calculation that doesn't filter any Customer will always display the
// total of all the customers"). So a role's filters are not filters in the filter context, and
// `Context.root` doesn't carry them: a filter there would be removed by ALL and replaced by
// CALCULATE.
//
// What this file settles:
//   - which tables a role's filters reach, from the model alone (`reach`): over active
//     relationships only ("RLS filters only propagate through active relationships": Microsoft
//     Learn, "Row-level security (RLS) guidance in Power BI Desktop"), from the one side to the
//     many side, and from the many side to the one side too where the relationship's
//     `SecurityFilteringBehavior` is BothDirections ("By default, row-level security filtering uses
//     single-directional filters, whether the relationships are set to single direction or
//     bi-directional": Microsoft Learn, "Row-level security (RLS) with Power BI");
//   - what refuses a role from the model alone (`problems`): `--coverage` reports it, and
//     `Engine.asRoles` refuses with it;
//   - the rows each table keeps (`visibleRows`): each filter evaluated in a row context on every
//     row of its table, over the model as processed (no role applied), then carried over the
//     relationships, then the roles combined.
//
// Roles combine as the sources say: "A user can belong to multiple roles, and the roles are
// additive. For example, if a user belongs to both the "Sales" and "Marketing" roles, they can see
// data for both these roles" (Microsoft Learn, "Row-level security (RLS) with Power BI"). That can
// be read two ways when the roles filter different tables: a row is visible if one role lets it
// through (each role's filters carried over the relationships, then the union), or each table's
// filters are first combined (a table is unfiltered unless every role filters it) and then
// carried. The two agree when the roles filter the same tables; where they don't, the engine
// refuses.

open System
open System.Collections.Generic
open Tessera
open Tessera.DaxAst

module Security =

    type private Database = Microsoft.AnalysisServices.Tabular.Database
    type private ModelRole = Microsoft.AnalysisServices.Tabular.ModelRole
    type private TomTable = Microsoft.AnalysisServices.Tabular.Table
    type private SingleColumnRelationship = Microsoft.AnalysisServices.Tabular.SingleColumnRelationship
    type private RelationshipEndCardinality = Microsoft.AnalysisServices.Tabular.RelationshipEndCardinality
    type private SecurityFilteringBehavior = Microsoft.AnalysisServices.Tabular.SecurityFilteringBehavior
    type private ModelPermission = Microsoft.AnalysisServices.Tabular.ModelPermission
    type private MetadataPermission = Microsoft.AnalysisServices.Tabular.MetadataPermission

    let private ci = StringComparer.OrdinalIgnoreCase

    let private eq (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    /// An active relationship a role's filter crosses: from the one side to the many side, and
    /// back when `Both` (its security filter goes both ways).
    type Edge = {
        Name: string
        Many: string
        ManyColumn: string
        One: string
        OneColumn: string
        Both: bool
    }

    /// The tables the role filters on `filtered` reach, the relationships they cross, and what the
    /// engine refuses about how they cross them.
    type Reach = {
        Tables: string list
        Edges: Edge list
        Problems: string list
    }

    let private between (s: SingleColumnRelationship) =
        $"'{s.FromTable.Name}'[{s.FromColumn.Name}] -> '{s.ToTable.Name}'[{s.ToColumn.Name}]"

    /// Where the filters on these tables go (see the file's header).
    let reach (db: Database) (filtered: string seq) : Reach =
        let reached = HashSet<string>(filtered, ci)
        let problems = ResizeArray<string>()
        let used = Dictionary<SingleColumnRelationship, Edge>(HashIdentity.Reference)

        let relationships =
            db.Model.Relationships
            |> Seq.choose (fun r ->
                match r with
                | :? SingleColumnRelationship as s when
                    s.IsActive
                    && not (isNull s.FromColumn)
                    && not (isNull s.ToColumn)
                    && not (isNull s.FromTable)
                    && not (isNull s.ToTable)
                    ->
                    Some s
                | _ -> None)
            |> List.ofSeq

        let mutable changed = true

        while changed do
            changed <- false

            for s in relationships do
                let touches = reached.Contains s.FromTable.Name || reached.Contains s.ToTable.Name

                if touches && not (used.ContainsKey s) then
                    let ends =
                        match s.FromCardinality, s.ToCardinality with
                        | RelationshipEndCardinality.Many, RelationshipEndCardinality.One ->
                            Some(s.FromColumn, s.ToColumn)
                        | RelationshipEndCardinality.One, RelationshipEndCardinality.Many ->
                            Some(s.ToColumn, s.FromColumn)
                        | RelationshipEndCardinality.One, RelationshipEndCardinality.One ->
                            problems.Add
                                $"a role's filter that reaches the one-to-one relationship {between s} (how it crosses one isn't documented)"

                            None
                        | RelationshipEndCardinality.Many, RelationshipEndCardinality.Many ->
                            problems.Add $"a role's filter that reaches the many-to-many relationship {between s}"
                            None
                        | _ -> None

                    match ends with
                    | None -> ()
                    | Some(many, one) ->
                        let both =
                            match s.SecurityFilteringBehavior with
                            | SecurityFilteringBehavior.OneDirection -> Some false
                            | SecurityFilteringBehavior.BothDirections -> Some true
                            | other ->
                                problems.Add
                                    $"a role's filter that reaches the relationship {between s}, whose security filtering behavior is {other}"

                                None

                        match both with
                        | None -> ()
                        | Some both ->
                            let edge = {
                                Name = (if isNull s.Name then "" else s.Name)
                                Many = many.Table.Name
                                ManyColumn = many.Name
                                One = one.Table.Name
                                OneColumn = one.Name
                                Both = both
                            }

                            let crosses = reached.Contains edge.One || (both && reached.Contains edge.Many)

                            if crosses then
                                used.[s] <- edge
                                reached.Add edge.One |> ignore
                                reached.Add edge.Many |> ignore
                                changed <- true

        // On a tree of relationships every table is reached one way; with a loop, a filter could
        // reach a table along two paths, and which rows that leaves isn't documented.
        let parent = Dictionary<string, string>(ci)

        let rec find (x: string) =
            match parent.TryGetValue x with
            | true, p when not (eq p x) ->
                let root = find p
                parent.[x] <- root
                root
            | _ -> x

        for t in reached do
            parent.[t] <- t

        for edge in used.Values do
            let a = find edge.Many
            let b = find edge.One

            if eq a b then
                problems.Add
                    $"role filters that reach '{edge.Many}' and '{edge.One}' along more than one path of relationships"
            else
                parent.[a] <- b

        {
            Tables = reached |> Seq.sortWith (fun a b -> String.CompareOrdinal(a, b)) |> List.ofSeq
            Edges = List.ofSeq used.Values
            Problems = problems |> Seq.distinct |> List.ofSeq
        }

    // ---- What a filter expression may read ---------------------------------------------------------

    /// Functions whose arguments are all values: a column of the filtered table inside them is the
    /// value of the row being filtered.
    let private scalarFunctions =
        HashSet<string>(
            [
                "IF"
                "IF.EAGER"
                "SWITCH"
                "AND"
                "OR"
                "NOT"
                "TRUE"
                "FALSE"
                "BLANK"
                "ISBLANK"
                "COALESCE"
                "IFERROR"
                "ISERROR"
                "ISNUMBER"
                "ISTEXT"
                "ISLOGICAL"
                "UPPER"
                "LOWER"
                "LEFT"
                "RIGHT"
                "MID"
                "LEN"
                "TRIM"
                "SEARCH"
                "FIND"
                "SUBSTITUTE"
                "CONTAINSSTRING"
                "CONTAINSSTRINGEXACT"
                "EXACT"
                "CONCATENATE"
                "VALUE"
                "INT"
                "ROUND"
                "ABS"
                "YEAR"
                "MONTH"
                "DAY"
                "DATE"
                "WEEKDAY"
                "USERNAME"
                "USERPRINCIPALNAME"
                "USEROBJECTID"
                "USERCULTURE"
                "CUSTOMDATA"
                "PATHCONTAINS"
            ],
            ci
        )

    [<RequireQualifiedAccess>]
    type private Position =
        /// Evaluated as a value in the row context of the row being filtered.
        | Value
        /// Anything else: an argument a function may read as a column or a table.
        | Other

    /// What a filter expression on `table` reads that the engine won't evaluate it with: its own
    /// table other than as the row being filtered (whether that sees the table with the role
    /// applied isn't documented), another table the roles' filters reach (likewise), a measure or a
    /// user-defined function (whose reads can't be told here), USERELATIONSHIP and CROSSFILTER.
    /// Anything else is read as processed: it is the same with or without the role.
    let expressionProblems (db: Database) (table: string) (reached: string seq) (expr: Expr) : string list =
        let reached = HashSet<string>(reached, ci)
        let problems = ResizeArray<string>()

        let tables = HashSet<string>(ci)
        let measures = HashSet<string>(ci)
        let functions = HashSet<string>(ci)
        let ownColumns = HashSet<string>(ci)

        for t in db.Model.Tables do
            tables.Add t.Name |> ignore

            for m in t.Measures do
                measures.Add m.Name |> ignore

            if eq t.Name table then
                for c in t.Columns do
                    ownColumns.Add c.Name |> ignore

        for f in db.Model.Functions do
            functions.Add f.Name |> ignore

        let own =
            $"a role's filter on '{table}' that reads '{table}' other than as the row being filtered"

        let readsTable (name: string) =
            if eq name table then
                problems.Add own
            elif reached.Contains name then
                problems.Add $"a role's filter on '{table}' that reads '{name}', which the roles' filters also reach"

        let rec walk (vars: Set<string>) (pos: Position) (e: Expr) =
            Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

            match e.Kind with
            | Qualified(t, name) ->
                if eq t.Name table then
                    if not (ownColumns.Contains name) then
                        if measures.Contains name then
                            problems.Add $"a role's filter that refers to the measure [{name}]"
                    elif pos <> Position.Value then
                        problems.Add own
                else
                    readsTable t.Name

                    match db.Model.Tables |> Seq.tryFind (fun x -> eq x.Name t.Name) with
                    | Some x when x.Measures.ContainsName name ->
                        problems.Add $"a role's filter that refers to the measure [{name}]"
                    | _ -> ()
            | Bracketed name ->
                if ownColumns.Contains name then
                    if pos <> Position.Value then
                        problems.Add own
                elif measures.Contains name then
                    problems.Add $"a role's filter that refers to the measure [{name}]"
            | QuotedTable name -> readsTable name
            | Identifier name when not (vars.Contains(name.ToUpperInvariant())) && tables.Contains name ->
                readsTable name
            | Call(name, args) ->
                let upper = name.Name.ToUpperInvariant()

                if functions.Contains name.Name then
                    problems.Add $"a role's filter that calls the user-defined function {name.Name}"

                if upper = "USERELATIONSHIP" || upper = "CROSSFILTER" then
                    problems.Add $"{upper} in a role's filter"

                let n = args.Length

                args
                |> List.iteri (fun i a ->
                    let p =
                        if scalarFunctions.Contains upper then
                            pos
                        else
                            match upper with
                            // LOOKUPVALUE ( result, search, value, ..., [alternate] ) and
                            // CONTAINS ( table, column, value, ... ): the values are values.
                            | "LOOKUPVALUE" when (i >= 2 && i % 2 = 0) || (n % 2 = 0 && i = n - 1) -> pos
                            | "CONTAINS" when i >= 2 && i % 2 = 0 -> pos
                            | _ -> Position.Other

                    walk vars p a)
            | Binary(BinaryOp.In, l, r) ->
                walk vars pos l
                walk vars Position.Other r
            | VarBlock(decls, body) ->
                let mutable scope = vars

                for d in decls do
                    walk scope pos d.Value
                    scope <- scope.Add(d.Name.Name.ToUpperInvariant())

                walk scope pos body
            | _ -> List.iter (walk vars pos) (children e)

        walk Set.empty Position.Value expr
        problems |> Seq.distinct |> List.ofSeq

    let private permissionText (p: ModelPermission) =
        match p with
        | ModelPermission.None -> "None"
        | ModelPermission.Read -> "Read"
        | ModelPermission.ReadRefresh -> "ReadRefresh"
        | ModelPermission.Refresh -> "Refresh"
        | ModelPermission.Administrator -> "Administrator"
        | other -> string other

    /// Whether members of a role may query data: "Read" and "Read and Process" filter rows,
    /// "Administrator" sees everything, "None" and "Process" can't query (Microsoft Learn, "Roles in
    /// Analysis Services tabular models").
    let canRead (r: ModelRole) =
        match r.ModelPermission with
        | ModelPermission.Read
        | ModelPermission.ReadRefresh
        | ModelPermission.Administrator -> true
        | _ -> false

    /// The role's row filters: (table, expression text), those with an expression.
    let filtersOf (r: ModelRole) : (TomTable * string) list = [
        if r.ModelPermission <> ModelPermission.Administrator then
            for tp in r.TablePermissions do
                if not (isNull tp.Table) && not (String.IsNullOrWhiteSpace tp.FilterExpression) then
                    tp.Table, tp.FilterExpression
    ]

    /// What a role uses that the engine refuses, from the model alone: object-level security, a
    /// filter on a calculation group, how its filters cross the relationships, and what each filter
    /// reads (`reached`: the tables the filters of every role in use reach). A filter that doesn't
    /// parse is left to the evaluator, which reports its syntax error.
    let roleProblems (db: Database) (reached: string seq) (r: ModelRole) : string list = [
        for tp in r.TablePermissions do
            if not (isNull tp.Table) then
                if tp.MetadataPermission = MetadataPermission.None then
                    $"object-level security (role '{r.Name}' hides table '{tp.Table.Name}')"

                for cp in tp.ColumnPermissions do
                    if cp.MetadataPermission = MetadataPermission.None && not (isNull cp.Column) then
                        $"object-level security (role '{r.Name}' hides column '{tp.Table.Name}'[{cp.Column.Name}])"

        for (t, text) in filtersOf r do
            if not (isNull t.CalculationGroup) then
                $"a role's filter on the calculation group '{t.Name}'"

            match DaxParser.parseExpression text with
            | Ok e -> yield! expressionProblems db t.Name reached e
            | Error _ -> ()
    ]

    /// Everything that refuses these roles together, from the model alone.
    let problems (db: Database) (roles: ModelRole list) : string list =
        let readers = roles |> List.filter canRead

        if
            readers
            |> List.exists (fun r -> r.ModelPermission = ModelPermission.Administrator)
        then
            []
        else
            let filtered =
                readers
                |> List.collect filtersOf
                |> List.map (fun (t, _) -> t.Name)
                |> List.distinct

            let reached = reach db filtered

            reached.Problems @ (readers |> List.collect (roleProblems db reached.Tables))
            |> List.distinct

    /// The permission as the model spells it, for messages.
    let permission (r: ModelRole) = permissionText r.ModelPermission

    // ---- The rows each table keeps -----------------------------------------------------------------

    let private refuse (what: string) : 'a =
        raise (EngineException(DaxNotSupported(what, "")))

    /// Per table id: the stored rows the filters keep, or null for a table they don't filter.
    type private Kept = Dictionary<int, bool array>

    /// One role filter's verdict on each stored row of its table, the filter evaluated in a row
    /// context on that row over the model as processed. Its value must be TRUE or FALSE.
    let private evaluateFilter
        (ev: IEval)
        (model: Model)
        (identity: Identity option)
        (t: BTable)
        (source: SourceText)
        (expr: Expr)
        : bool array =
        let ctx = {
            Context.root model null source false with
                Identity = identity
        }

        let n = t.RowCount

        let rows =
            Shadows.notAnIteration
                "the row context of a role's filter"
                (Tables.ofBase model.DefaultConfig t (Array.init n id))

        Array.init n (fun row ->
            match ev.Scalar(Context.pushRow ctx rows row, expr) with
            | DaxValue.Boolean b -> b
            | v ->
                let kind =
                    match DaxValue.typeOf v with
                    | Some ty -> DaxType.name ty
                    | None -> "BLANK"

                Fail.notSupported ctx expr.Span $"a role's filter whose value is {kind}, not TRUE or FALSE")

    /// Carry the filters in `kept` over the relationships until nothing changes. The relationships
    /// form a tree (`reach` refuses a loop), so this is how a filter on one table reaches each other
    /// table: by its one path.
    let private propagate (edges: (BRel * bool) list) (kept: Kept) : unit =
        let mutable changed = true

        let all (t: BTable) = Array.create t.RowCount true

        while changed do
            changed <- false

            for (r, both) in edges do
                let one = r.To.Table
                let many = r.From.Table
                let map = r.ToRowOfFromRow

                match kept.TryGetValue one.Id with
                | true, oneRows ->
                    if r.OrphanFromRows.Length > 0 then
                        refuse
                            $"a role's filter on '{one.Name}' with rows of '{many.Name}' whose key has no match there (whether the role lets the blank row's rows through isn't documented)"

                    let current =
                        match kept.TryGetValue many.Id with
                        | true, rows -> rows
                        | _ -> null

                    let next =
                        Array.init many.RowCount (fun row -> (isNull current || current.[row]) && oneRows.[map.[row]])

                    if isNull current || next <> current then
                        kept.[many.Id] <- next
                        changed <- true
                | _ -> ()

                if both then
                    match kept.TryGetValue many.Id with
                    | true, manyRows ->
                        if r.OrphanFromRows.Length > 0 then
                            refuse
                                $"a role's filter on '{many.Name}' crossing to '{one.Name}', where rows of '{many.Name}' have no match (whether the blank row is let through isn't documented)"

                        let referred = Array.zeroCreate<bool> one.RowCount

                        for row in 0 .. many.RowCount - 1 do
                            if manyRows.[row] then
                                referred.[map.[row]] <- true

                        if Array.forall id manyRows && not (Array.forall id referred) then
                            refuse
                                $"a role's filter on '{many.Name}' that keeps all of its rows, crossing to '{one.Name}' in both directions (whether the rows nothing refers to stay visible isn't documented)"

                        let current =
                            match kept.TryGetValue one.Id with
                            | true, rows -> rows
                            | _ -> all one

                        let next = Array.init one.RowCount (fun row -> current.[row] && referred.[row])

                        if not (kept.ContainsKey one.Id) || next <> current then
                            kept.[one.Id] <- next
                            changed <- true
                    | _ -> ()

    /// The rows these roles let their members see, by table: only the tables where some row is
    /// hidden, each with its visible stored rows in order. Raises `EngineException` for what isn't
    /// supported (`problems` first, then what only the data tells). `roles` are the model's own
    /// objects, each one that can read; a role with Administrator permission sees everything.
    let visibleRows
        (ev: IEval)
        (db: Database)
        (model: Model)
        (identity: Identity option)
        (roles: ModelRole list)
        : Dictionary<string, int array> =
        match problems db roles with
        | first :: _ -> refuse first
        | [] -> ()

        let result = Dictionary<string, int array>(ci)

        if
            not (
                roles
                |> List.exists (fun r -> r.ModelPermission = ModelPermission.Administrator)
            )
        then
            let tableOf (name: string) =
                match model.TableByName.TryGetValue name with
                | true, t when t.Gate.IsNone -> t
                | true, t -> refuse $"a role's filter on '{name}': {t.Gate.Value}"
                | _ -> refuse $"a role's filter on '{name}', which the engine has no table for"

            let filtered =
                roles
                |> List.collect filtersOf
                |> List.map (fun (t, _) -> t.Name)
                |> List.distinct

            let reached = reach db filtered

            let edges =
                reached.Edges
                |> List.map (fun e ->
                    let found =
                        model.Rels
                        |> Array.tryFind (fun r ->
                            r.IsActive
                            && not r.Limited
                            && eq r.From.Table.Name e.Many
                            && eq r.From.Name e.ManyColumn
                            && eq r.To.Table.Name e.One
                            && eq r.To.Name e.OneColumn)

                    match found with
                    | Some r ->
                        r.Check()
                        r, e.Both
                    | None ->
                        refuse
                            $"a role's filter that crosses the relationship '{e.Many}'[{e.ManyColumn}] -> '{e.One}'[{e.OneColumn}], which the engine doesn't have")

            // Each filter's own verdict, by role.
            let own =
                roles
                |> List.map (fun r ->
                    let kept = Kept()

                    for (t, text) in filtersOf r do
                        let bt = tableOf t.Name

                        let source = {
                            Label = $"role '{r.Name}' filter on '{t.Name}'"
                            Text = text
                        }

                        let expr =
                            match DaxParser.parseExpression text with
                            | Ok e -> e
                            | Error errors ->
                                let e = errors.Head
                                raise (EngineException(DaxSyntax(source.Label, e.Line, e.Column, e.Message)))

                        let verdict = evaluateFilter ev model identity bt source expr

                        // Two filters of one role on one table: a row must pass both (TOM allows one
                        // per table, so this doesn't happen in a valid model).
                        match kept.TryGetValue bt.Id with
                        | true, before -> kept.[bt.Id] <- Array.map2 (&&) before verdict
                        | _ -> kept.[bt.Id] <- verdict

                    kept)

            let union (sets: Kept list) (t: BTable) : bool array =
                let each = sets |> List.map (fun k -> if k.ContainsKey t.Id then k.[t.Id] else null)

                if each |> List.exists isNull then
                    null
                else
                    each |> List.reduce (Array.map2 (||))

            // One reading: each role's filters carried, then their union.
            let perRole =
                own
                |> List.map (fun k ->
                    let copy = Kept(k)
                    propagate edges copy
                    copy)

            // The other: the filters on each table combined, then carried.
            let combined = Kept()

            for t in model.Tables do
                match union own t with
                | null -> ()
                | rows -> combined.[t.Id] <- rows

            propagate edges combined

            for t in model.Tables do
                let a = union perRole t

                let b =
                    match combined.TryGetValue t.Id with
                    | true, rows -> rows
                    | _ -> null

                let allOf (rows: bool array) = isNull rows || Array.forall id rows

                let same = (allOf a && allOf b) || (not (isNull a) && not (isNull b) && a = b)

                if not same then
                    let names = roles |> List.map (fun r -> $"'{r.Name}'") |> String.concat ", "

                    refuse
                        $"the roles {names} together: their filters reach '{t.Name}' through different tables, and the sources don't say whether a row one role lets through and another hides that way is visible"

                if not (allOf a) then
                    result.[t.Name] <- [|
                        for row in 0 .. a.Length - 1 do
                            if a.[row] then
                                row
                    |]

        result

    // ---- USERELATIONSHIP and CROSSFILTER under a role ------------------------------------------------

    /// Where a query's evaluation may reach USERELATIONSHIP or CROSSFILTER: the query's expressions,
    /// the measures and functions they name (the model's and the query's), and every calculation
    /// item. "The UseRelationship() and CrossFilter() functions may not be used when querying
    /// 'Sales' because it is constrained by row-level security" (the error SQLBI quotes in "DAX
    /// limitations with inactive relationships and row-level security (RLS)"): exactly when isn't
    /// spelled out, so under a role that filters anything the engine refuses both.
    let relationshipFunction (model: Model) (defs: QueryDefs) (exprs: Expr list) : string option =
        let seen = HashSet<obj>(HashIdentity.Reference)
        let mutable found = None

        let measure (name: string) =
            match (if isNull defs then None else Some defs) with
            | Some d when d.Measures.ContainsKey name -> Some d.Measures.[name]
            | _ ->
                match model.Measures.TryGetValue name with
                | true, m -> Some m
                | _ -> None

        let func (name: string) =
            match (if isNull defs then None else Some defs) with
            | Some d when d.Functions.ContainsKey name -> Some d.Functions.[name]
            | _ ->
                match model.Functions.TryGetValue name with
                | true, f -> Some f
                | _ -> None

        let rec walk (where: string) (e: Expr) =
            Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

            if found.IsNone then
                match e.Kind with
                | Call(name, _) ->
                    let upper = name.Name.ToUpperInvariant()

                    if upper = "USERELATIONSHIP" || upper = "CROSSFILTER" then
                        found <- Some $"{upper} ({where}) under a role's row-level security"

                    match func name.Name with
                    | Some f when seen.Add f ->
                        try
                            walk f.Source.Label f.Def.Body
                        with EngineException _ ->
                            ()
                    | _ -> ()
                | Bracketed name
                | Qualified(_, name) ->
                    match measure name with
                    | Some m when seen.Add m ->
                        try
                            walk m.Source.Label m.Expr
                        with EngineException _ ->
                            ()
                    | _ -> ()
                | _ -> ()

                List.iter (walk where) (children e)

        for e in exprs do
            walk "the query" e

        for g in model.CalcGroups do
            for item in g.Items do
                try
                    walk item.Source.Label item.Expr
                with EngineException _ ->
                    ()

        found
