namespace Tessera.DaxEngine.Functions

// Functions that build tables.

open System
open System.Collections.Generic
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module TableFunctions =

    let private literalName (ctx: Ctx) (e: Expr) : string =
        match (Fn.unparen e).Kind with
        | StringLiteral s -> s
        | _ -> Fail.invalid ctx e.Span "a column name must be a text in quotes"

    let private isName (e: Expr) =
        match (Fn.unparen e).Kind with
        | StringLiteral _ -> true
        | _ -> false

    /// An expression evaluated for every row of a table: its declared type and its values.
    let private extensionColumn (ev: IEval) (ctx: Ctx) (table: TableVal) (expr: Expr) : DaxType * DaxValue array =
        let values =
            Array.init table.Count (fun row -> ev.Scalar(Context.pushRow ctx table row, expr))

        // The static type needs a row context to name columns by, not a row.
        let staticType = ev.TypeOf(Context.pushRow ctx table 0, expr)
        Typing.column staticType values

    let private checkNewNames (ctx: Ctx) (span: Span) (fn: string) (existing: ColInfo array) (added: string list) =
        let all =
            (existing |> Array.map Tables.displayName |> List.ofArray)
            @ (added |> List.map (fun n -> $"[{n}]"))

        match
            all
            |> List.countBy (fun n -> n.ToUpperInvariant())
            |> List.tryFind (fun (_, n) -> n > 1)
        with
        | Some(name, _) -> Fail.invalid ctx span $"{fn}: more than one column would be named {name}"
        | None -> ()

    let private pairs (ctx: Ctx) (span: Span) (fn: string) (args: Expr list) : (string * Expr) list =
        if args.Length % 2 <> 0 then
            Fail.invalid ctx span $"{fn} takes its new columns as pairs of a name and an expression"

        args |> List.chunkBySize 2 |> List.map (fun p -> literalName ctx p.[0], p.[1])

    /// The columns to group a table by: columns of the table itself, or for the rows of a model
    /// table, columns of the tables related to it ("in the table (or in a related table)").
    ///
    /// `[Name]` is a column the table expression added ("SUMMARIZE has the option of grouping by
    /// columns computed in the query": dax.guide/summarize; "GROUPBY is useful to group by columns
    /// with no lineage": dax.guide/groupby).
    let private groupColumn (ev: IEval) (ctx: Ctx) (fn: string) (table: TableVal) (e: Expr) : ColInfo * DaxValue array =
        let added =
            match (Fn.unparen e).Kind with
            | Bracketed name ->
                table.Cols
                |> Array.tryFindIndex (fun c ->
                    isNull c.Table
                    && String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
            | _ -> None

        match added with
        | Some i -> table.Cols.[i], table.Column i
        | None ->

            let c = ev.Column(ctx, e)

            match Tables.columnIndexOfLineage table c with
            | i when i >= 0 -> table.Cols.[i], table.Column i
            | _ ->
                if
                    not (isNull table.BaseTable)
                    && table.Config.IsExpanded(table.BaseTable, c.Table)
                then
                    if not table.IsPureBase then
                        Fail.notSupported
                            ctx
                            e.Span
                            $"{fn} by a column of a related table, over a table built on '{table.BaseTable.Name}' with columns added"

                    let map = table.Config.RowMap(table.BaseTable, c.Table)
                    Tables.modelColumn c, Array.init table.Count (fun row -> c.ValueAt map.[table.BaseRows.[row]])
                else
                    Fail.invalid
                        ctx
                        e.Span
                        $"{fn}: '{c.Table.Name}'[{c.Name}] is not a column of the table, or of a table it has a many-to-one relationship to"

    /// Group rows by key values: each group's key, and the positions of its rows, in order of
    /// first appearance.
    let private groupRows
        (model: Model)
        (keys: DaxValue array array)
        (count: int)
        : (DaxValue array * int array) array =
        let groups =
            Dictionary<DaxValue array, ResizeArray<int>>(
                ValueArrayComparer(model.Comparer) :> IEqualityComparer<DaxValue array>
            )

        let order = ResizeArray<DaxValue array>()

        for row in 0 .. count - 1 do
            let key = keys |> Array.map (fun k -> k.[row])

            match groups.TryGetValue key with
            | true, list -> list.Add row
            | _ ->
                let list = ResizeArray<int>()
                list.Add row
                groups.[key] <- list
                order.Add key

        order.ToArray() |> Array.map (fun key -> key, groups.[key].ToArray())

    let private unsupportedHelpers = [
        "ROLLUP"
        "ROLLUPGROUP"
        "ROLLUPADDISSUBTOTAL"
        "ROLLUPISSUBTOTAL"
        "IGNORE"
        "NONVISUAL"
        "ISSUBTOTAL"
    ]

    let private checkHelpers (fn: string) (args: Expr list) : string option =
        args
        |> List.tryPick (fun a ->
            match (Fn.unparen a).Kind with
            | Call(name, _) when unsupportedHelpers |> List.contains (name.Name.ToUpperInvariant()) ->
                Some $"{name.Name.ToUpperInvariant()} in {fn}"
            | _ -> None)

    /// SUMMARIZE and GROUPBY need a group-by column after the table.
    let private checkGrouped (fn: string) (args: Expr list) : string option =
        match args with
        | [ _ ] -> Some $"{fn} without a group-by column"
        | _ :: second :: _ when isName second -> Some $"{fn} without a group-by column"
        | _ -> None

    /// See `Sorting.check`.
    let checkSortable (ctx: Ctx) (span: Span) (what: string) (values: DaxValue seq) = Sorting.check ctx span what values

    let private descending (e: Expr) : bool option =
        match Fn.keyword e, (Fn.unparen e).Kind with
        | Some "DESC", _ -> Some true
        | Some "ASC", _ -> Some false
        | _, (IntegerLiteral(0L, _) | BooleanLiteral false) -> Some true
        | _, (IntegerLiteral(1L, _) | BooleanLiteral true) -> Some false
        | _ -> None

    let private datatableType (ctx: Ctx) (e: Expr) : DaxType =
        match Fn.keyword e with
        | Some "INTEGER" -> DaxType.Integer
        | Some "DOUBLE" -> DaxType.Decimal
        | Some "STRING" -> DaxType.String
        | Some "BOOLEAN" -> DaxType.Boolean
        | Some "CURRENCY" -> DaxType.Currency
        | Some "DATETIME" -> DaxType.DateTime
        | _ ->
            Fail.invalid ctx e.Span "a DATATABLE column type is INTEGER, DOUBLE, STRING, BOOLEAN, CURRENCY or DATETIME"

    let private setOperation (keepWhenFound: bool) (name: string) =
        // "Duplicate rows are retained"; "Columns are compared based on positioning, and data
        // comparison with no type coercion"; "The column names will match the column names in
        // tableExpression1" (Microsoft Learn, INTERSECT and EXCEPT).
        Fn.table name "Table manipulation" (2, 2) (fun ev ctx call ->
            let a = ev.Table(ctx, call.Args.[0])
            let b = ev.Table(ctx, call.Args.[1])

            if a.Cols.Length <> b.Cols.Length then
                Fail.invalid ctx call.Span $"the two tables of {name} must have the same number of columns"

            let rows =
                HashSet<DaxValue array>(ValueArrayComparer(ctx.Model.Comparer) :> IEqualityComparer<DaxValue array>)

            for row in 0 .. b.Count - 1 do
                rows.Add(b.Row row) |> ignore

            Tables.select a [|
                for row in 0 .. a.Count - 1 do
                    if rows.Contains(a.Row row) = keepWhenFound then
                        row
            |])

    /// NATURALINNERJOIN and NATURALLEFTOUTERJOIN of tables whose columns have no data lineage
    /// (DATATABLE, table constructors, columns computed by ADDCOLUMNS or SELECTCOLUMNS): "Tables
    /// are joined on common columns (by name) in the two tables. If the two tables have no common
    /// column names, an error is returned. Columns being joined on must have the same data type in
    /// both tables"; "Strict comparison semantics are used during join" (Microsoft Learn,
    /// NATURALINNERJOIN). "When a row of the left table matches more than one row of the right
    /// table, the result has one row for each match. A row of the left table without a match is
    /// returned once, with blank values in the columns of the right table" (dax.guide/
    /// naturalleftouterjoin). Tables with model columns are joined over their expanded tables and
    /// by lineage, which is not implemented; nor is a join on a key that is BLANK in some row (if
    /// BLANK matches BLANK is not documented).
    let private naturalJoin (outer: bool) (name: string) =
        Fn.table name "Table manipulation" (2, 2) (fun ev ctx call ->
            let left = ev.Table(ctx, call.Args.[0])
            let right = ev.Table(ctx, call.Args.[1])

            if
                Array.append left.Cols right.Cols
                |> Array.exists (fun c -> not (isNull c.Lineage) || not (isNull c.Table))
            then
                Fail.notSupported ctx call.Span $"{name} of tables with columns of the model (joins by data lineage)"

            let named (t: TableVal) (n: string) =
                t.Cols
                |> Array.tryFindIndex (fun c -> String.Equals(c.Name, n, StringComparison.OrdinalIgnoreCase))

            let common =
                left.Cols
                |> Array.indexed
                |> Array.choose (fun (i, c) -> named right c.Name |> Option.map (fun j -> i, j))

            if common.Length = 0 then
                Fail.invalid ctx call.Span $"the two tables of {name} have no column in common"

            for i, j in common do
                let a, b = left.Cols.[i], right.Cols.[j]

                if a.Type = DaxType.Variant || b.Type = DaxType.Variant then
                    Fail.notSupported ctx call.Span $"{name} on a column whose values are of different data types"

                if a.Type <> b.Type then
                    Fail.invalid
                        ctx
                        call.Span
                        $"the column [{a.Name}] has a different data type in the two tables of {name}, so they can't be joined on it"

            let keyOf (t: TableVal) (cols: int array) (row: int) =
                let key = cols |> Array.map (fun c -> t.Get(c, row))

                if key |> Array.exists DaxValue.isBlank then
                    Fail.notSupported ctx call.Span $"{name} on a key that is BLANK in some row"

                key

            let rightOnly =
                [| 0 .. right.Cols.Length - 1 |]
                |> Array.filter (fun j -> not (common |> Array.exists (fun (_, x) -> x = j)))

            let byKey =
                Dictionary<DaxValue array, ResizeArray<int>>(
                    ValueArrayComparer(ctx.Model.Comparer) :> IEqualityComparer<DaxValue array>
                )

            for row in 0 .. right.Count - 1 do
                let key = keyOf right (common |> Array.map snd) row

                match byKey.TryGetValue key with
                | true, rows -> rows.Add row
                | _ -> byKey.[key] <- ResizeArray [ row ]

            let rows = ResizeArray<DaxValue array>()

            for row in 0 .. left.Count - 1 do
                let mine = left.Row row

                match byKey.TryGetValue(keyOf left (common |> Array.map fst) row) with
                | true, matches ->
                    for m in matches do
                        rows.Add(Array.append mine (rightOnly |> Array.map (fun j -> right.Get(j, m))))
                | _ ->
                    if outer then
                        rows.Add(Array.append mine (rightOnly |> Array.map (fun _ -> DaxValue.Blank)))

            Tables.ofRows (Array.append left.Cols (rightOnly |> Array.map (fun j -> right.Cols.[j]))) rows)

    let private generate (all: bool) (name: string) =
        Fn.table name "Table manipulation" (2, 2) (fun ev ctx call ->
            let left = ev.Table(ctx, call.Args.[0])

            let parts =
                Array.init left.Count (fun row -> ev.Table(Context.pushRow ctx left row, call.Args.[1]))

            // The second table's columns, when no row produced one: from a row of BLANKs.
            let shape =
                if parts.Length > 0 then
                    parts.[0]
                else
                    let blanks =
                        Tables.ofRows (left.Cols |> Array.map (fun c -> { c with Lineage = c.Lineage })) [
                            Array.create left.Cols.Length DaxValue.Blank
                        ]

                    try
                        ev.Table(Context.pushRow ctx blanks 0, call.Args.[1])
                    with DaxRuntimeException _ ->
                        Fail.notSupported
                            ctx
                            call.Span
                            $"{name} over an empty table whose second table can't be evaluated for a BLANK row"

            checkNewNames ctx call.Span name (Array.append left.Cols shape.Cols) []

            let rows = ResizeArray<DaxValue array>()

            for row in 0 .. left.Count - 1 do
                let l = left.Row row
                let part = parts.[row]

                if part.Count = 0 then
                    // GENERATEALL: "the current row from table1 will be included in the results and
                    // columns corresponding to table2 will have null values" (Microsoft Learn).
                    if all then
                        rows.Add(Array.append l (Array.create shape.Cols.Length DaxValue.Blank))
                else
                    for r in 0 .. part.Count - 1 do
                        rows.Add(Array.append l (part.Row r))

            Tables.ofRows (Array.append left.Cols shape.Cols) rows)

    // ---- SUMMARIZECOLUMNS -------------------------------------------------------------------------
    //
    // SUMMARIZECOLUMNS ( group-by column | ROLLUPADDISSUBTOTAL ( ... ), ..., filter table, ...,
    // name, expression | IGNORE ( expression ), ... ). Sources: Microsoft Learn SUMMARIZECOLUMNS
    // (quoted as "Learn" below), dax.guide/summarizecolumns, SQLBI "Introducing SUMMARIZECOLUMNS"
    // and "Understanding DAX auto-exist".
    //
    // - Group-by columns of one table give the combinations that exist in it ("auto-existed (same
    //   table)"); different tables are cross-joined (Learn).
    // - A filter restricts the group-by columns of the tables it has columns of, and the
    //   expressions; it does not reach group-by columns over a relationship: "GroupBy columns
    //   within SummarizeColumns will no longer be filtered by columns from external tables, even if
    //   a relationship exists between them" (Learn, "SelfValue semantics"), and for a filter
    //   argument "the filter expression is a no-op and the groupBy columns are not impacted". This
    //   holds for the filter arguments and for the filter context the function is called in.
    // - A filter argument replaces the outer filters on its columns; under KEEPFILTERS it is added
    //   to them ("normal filters (which override existing filters) and filters with KeepFilters
    //   specified": Learn).
    // - A model table as a filter has the columns of its expanded table, so it pairs the group-by
    //   columns of the tables it relates to: SUMMARIZECOLUMNS ( a, b, Sales ) is SUMMARIZE ( Sales,
    //   a, b ) (SQLBI, "Introducing SUMMARIZECOLUMNS").
    // - "Only rows for which at least one of the supplied expressions return a non-blank value are
    //   included"; "Rows for which all expressions not using IGNORE return BLANK/NULL will be
    //   excluded" (Learn).
    // - ROLLUPADDISSUBTOTAL adds subtotal rows: the rolled-up columns are BLANK there, the named
    //   column is TRUE, and the expressions are evaluated without the grouping (the filters stay).
    //   Several columns in one call roll up from the last to the first; several calls roll up
    //   independently; ROLLUPGROUP rolls its columns up together. The result has the group-by
    //   columns, then the subtotal columns, then the expressions (the result tables of Learn and
    //   of dax.guide/rollupaddissubtotal).

    type private ScArgument =
        | GroupBy of BColumn
        /// The levels of one ROLLUPADDISSUBTOTAL: the columns of each, and its subtotal column's name.
        | RollUp of (BColumn array * string) list
        | FilterTable of Expr * keep: bool

    let private callTo (name: string) (e: Expr) : Expr list option =
        match (Fn.unparen e).Kind with
        | Call(n, args) when n.Name.ToUpperInvariant() = name -> Some args
        | _ -> None

    let private checkSummarizeColumns (args: Expr list) : string option =
        let helper (names: string list) (a: Expr) =
            match (Fn.unparen a).Kind with
            | Call(name, _) when names |> List.contains (name.Name.ToUpperInvariant()) ->
                Some $"{name.Name.ToUpperInvariant()} in SUMMARIZECOLUMNS"
            | _ -> None

        let rollupFilters (a: Expr) =
            match callTo "ROLLUPADDISSUBTOTAL" a with
            | Some inner ->
                // column | ROLLUPGROUP ( ... ), name, repeated: anything else is a level filter.
                let rec plain (items: Expr list) =
                    match items with
                    | [] -> true
                    | c :: n :: tail when isName n && not (isName c) ->
                        (match (Fn.unparen c).Kind with
                         | Qualified _ -> true
                         | Call(g, _) -> g.Name.ToUpperInvariant() = "ROLLUPGROUP"
                         | _ -> false)
                        && plain tail
                    | _ -> false

                if plain inner then
                    None
                else
                    Some "ROLLUPADDISSUBTOTAL with a filter for the grand total or for a level"
            | None -> None

        args
        |> List.tryPick (fun a ->
            helper [ "ROLLUP"; "ROLLUPGROUP"; "ROLLUPISSUBTOTAL"; "NONVISUAL"; "ISSUBTOTAL" ] a
            |> Option.orElseWith (fun () -> rollupFilters a))

    /// A helper that only means something as an argument of another function.
    let private onlyInside (name: string) (host: string) : DaxFunction = {
        Name = name
        Family = "Table manipulation"
        MinArgs = 0
        MaxArgs = -1
        Returns = Returns.Modifier
        Check = fun _ -> None
        Type = Fn.typeUnknown
        Scalar = fun _ ctx call -> Fail.invalid ctx call.Span $"{name} can only be used as an argument of {host}"
        Table = fun _ ctx call -> Fail.invalid ctx call.Span $"{name} can only be used as an argument of {host}"
    }

    /// The rows of SUMMARIZECOLUMNS for one set of group-by columns: each row's group values (in
    /// the order of `groupCols`) and the values of the expressions.
    let private summarizeGroups
        (ev: IEval)
        (ctx: Ctx)
        (call: Call)
        (filtered: FilterCtx)
        (groupCols: BColumn array)
        (exprs: (Expr * bool) array)
        : (DaxValue array * DaxValue array) array =
        let cfg = filtered.Config
        let all = filtered.Filters
        let groupTables = groupCols |> Array.map _.Table |> Array.distinctBy _.Id

        let touches (f: Filter) (t: BTable) =
            match f with
            | OnColumn(c, _) -> obj.ReferenceEquals(c.Table, t)
            | Tuples(cols, _) -> cols |> Array.exists (fun c -> obj.ReferenceEquals(c.Table, t))
            | Rows(x, _, _) -> cfg.IsExpanded(x, t)

        let covers (f: Filter) (c: BColumn) =
            match f with
            | OnColumn(x, _) -> obj.ReferenceEquals(x, c)
            | Tuples(cols, _) -> cols |> Array.exists (fun x -> obj.ReferenceEquals(x, c))
            | Rows(x, _, _) -> cfg.IsExpanded(x, c.Table)

        // Group-by tables that one filter has columns of are grouped together: the filter pairs
        // their values.
        let leader = Dictionary<int, int>()

        for t in groupTables do
            leader.[t.Id] <- t.Id

        let rec find (id: int) =
            if leader.[id] = id then id else find leader.[id]

        for f in all do
            let touched = groupTables |> Array.filter (touches f)

            for t in touched do
                leader.[find t.Id] <- find touched.[0].Id

        let clusters = groupTables |> Array.groupBy (fun t -> find t.Id) |> Array.map snd

        // Per cluster: its group-by columns, the columns its filter will be on (the group-by
        // columns and the filtered columns of its tables: "SUMMARIZECOLUMNS always combines all
        // the filters on the same table into a single filter"), and the existing combinations.
        let perCluster =
            clusters
            |> Array.map (fun tables ->
                let parts =
                    tables
                    |> Array.map (fun t ->
                        let mine = groupCols |> Array.filter (fun c -> obj.ReferenceEquals(c.Table, t))

                        let extra =
                            t.Columns
                            |> Array.filter (fun c ->
                                not (mine |> Array.exists (fun m -> obj.ReferenceEquals(m, c)))
                                && all |> List.exists (fun f -> covers f c))

                        mine, Array.append mine extra)

                let mine = parts |> Array.collect fst
                let cols = parts |> Array.collect snd

                // The candidates: for each, the row of every table of the cluster.
                let count, rowOf =
                    if tables.Length = 1 then
                        // The table's own rows under its own filters, the blank row included.
                        let rows = (Filters.ownRows filtered tables.[0]).Rows
                        rows.Length, (fun (_: BTable) (p: int) -> rows.[p])
                    else
                        let source =
                            all
                            |> List.tryPick (fun f ->
                                match f with
                                | Rows(x, _, _) when tables |> Array.forall (fun t -> cfg.IsExpanded(x, t)) -> Some x
                                | _ -> None)

                        match source with
                        | None ->
                            Fail.notSupported
                                ctx
                                call.Span
                                "a SUMMARIZECOLUMNS filter over columns of more than one of the group-by tables that is not the rows of a table related to them all"
                        | Some x ->
                            // The rows of the filtering table that the filters on these tables
                            // leave: each is a combination of rows of the group-by tables.
                            let relevant = all |> List.filter (fun f -> tables |> Array.exists (touches f))

                            let rows = (Filters.visible (Filters.build cfg relevant) x).Rows
                            let maps = tables |> Array.map (fun t -> t.Id, cfg.RowMap(x, t)) |> dict
                            rows.Length, (fun (t: BTable) (p: int) -> maps.[t.Id].[rows.[p]])

                let keys =
                    mine
                    |> Array.map (fun c -> Array.init count (fun p -> c.ValueAt(rowOf c.Table p)))

                let groups =
                    groupRows ctx.Model keys count
                    |> Array.map (fun (key, positions) ->
                        let tuples =
                            HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                        for p in positions do
                            tuples.Add(cols |> Array.map (fun c -> c.IdAt(rowOf c.Table p))) |> ignore

                        let filter =
                            if cols.Length = 1 then
                                OnColumn(cols.[0], RowSet.OfSeq(cols.[0].IdCount, tuples |> Seq.map (fun t -> t.[0])))
                            else
                                Tuples(cols, tuples)

                        key, filter)

                mine, cols, groups)

        // The filters no longer filter those columns themselves: the combined filter does.
        let merged = perCluster |> Array.collect (fun (_, cols, _) -> cols)

        let baseCtx =
            if merged.Length = 0 then
                filtered
            else
                Filters.remove filtered (fun c -> merged |> Array.exists (fun m -> obj.ReferenceEquals(m, c)))

        // Clusters are cross-joined.
        let combos =
            perCluster
            |> Array.fold
                (fun (acc: (DaxValue array * Filter list) list) (_, _, groups) -> [
                    for (key, fs) in acc do
                        for (k, f) in groups -> Array.append key k, f :: fs
                ])
                [ [||], [] ]
            |> Array.ofList

        let order = perCluster |> Array.collect (fun (mine, _, _) -> mine)

        let position =
            groupCols
            |> Array.map (fun c -> order |> Array.findIndex (fun o -> obj.ReferenceEquals(o, c)))

        let counted = [|
            for i in 0 .. exprs.Length - 1 do
                if not (snd exprs.[i]) then
                    i
        |]

        combos
        |> Array.choose (fun (key, fs) ->
            let inner =
                Context.withFilter ctx (Filters.mark (Filters.add baseCtx fs true) (Grouped groupCols))

            let values = exprs |> Array.map (fun (expr, _) -> ev.Scalar(inner, expr))

            // With no expression that counts, every combination is a row.
            let blank =
                counted.Length > 0
                && counted |> Array.forall (fun i -> DaxValue.isBlank values.[i])

            if blank && groupCols.Length = 0 then
                Fail.notSupported
                    ctx
                    call.Span
                    "a SUMMARIZECOLUMNS total row (no group-by column) whose expressions are all BLANK (whether it is returned is not documented)"

            if blank then
                None
            else
                Some(position |> Array.map (fun p -> key.[p]), values))

    let private summarizeColumns (ev: IEval) (ctx: Ctx) (call: Call) : TableVal =
        if not ctx.Rows.IsEmpty then
            // "contextual SummarizeColumns ... allows SummarizeColumns to be evaluated in any
            // context transition" (Learn): under the filters a transition leaves. What a row
            // context that was not turned into filters means to it is not said.
            Fail.notSupported
                ctx
                call.Span
                "SUMMARIZECOLUMNS inside a row context (it is supported under a filter context: in a measure, or inside CALCULATE or CALCULATETABLE)"

        let before = call.Args |> List.takeWhile (fun a -> not (isName a))

        let added =
            pairs ctx call.Span "SUMMARIZECOLUMNS" (call.Args |> List.skip before.Length)
            |> List.map (fun (name, expr) ->
                match callTo "IGNORE" expr with
                | Some [ inner ] -> name, inner, true
                | Some _ -> Fail.invalid ctx expr.Span "IGNORE takes one expression"
                | None -> name, expr, false)

        let rollup (a: Expr) (items: Expr list) : ScArgument =
            let rec read (items: Expr list) =
                match items with
                | [] -> []
                | c :: n :: tail when isName n ->
                    let cols =
                        match callTo "ROLLUPGROUP" c with
                        | Some grouped -> grouped |> List.map (fun g -> ev.Column(ctx, g)) |> Array.ofList
                        | None -> [| ev.Column(ctx, c) |]

                    (cols, literalName ctx n) :: read tail
                | _ ->
                    Fail.notSupported ctx a.Span "ROLLUPADDISSUBTOTAL with a filter for the grand total or for a level"

            match read items with
            | [] -> Fail.invalid ctx a.Span "ROLLUPADDISSUBTOTAL takes a column and the name of its subtotal column"
            | levels -> RollUp levels

        let arguments =
            before
            |> List.map (fun a ->
                match callTo "ROLLUPADDISSUBTOTAL" a, callTo "KEEPFILTERS" a with
                | Some items, _ -> rollup a items
                | None, Some [ inner ] -> FilterTable(inner, true)
                | None, Some _ -> Fail.invalid ctx a.Span "KEEPFILTERS takes one argument"
                | None, None ->
                    match (Fn.unparen a).Kind with
                    | Qualified _ when (ev.TryColumn(ctx, a)).IsSome -> GroupBy (ev.TryColumn(ctx, a)).Value
                    | _ -> FilterTable(a, false))

        // Every group-by column, in the order the query names them.
        let groupCols =
            arguments
            |> List.collect (fun a ->
                match a with
                | GroupBy c -> [ c ]
                | RollUp levels -> levels |> List.collect (fun (cols, _) -> List.ofArray cols)
                | FilterTable _ -> [])
            |> Array.ofList

        if (groupCols |> Array.distinctBy _.Id).Length <> groupCols.Length then
            // "A column cannot be specified more than once" (Learn).
            Fail.invalid
                ctx
                call.Span
                "SUMMARIZECOLUMNS: a column cannot be specified more than once as a group-by column"

        let rollups =
            arguments
            |> List.choose (fun a ->
                match a with
                | RollUp levels -> Some levels
                | _ -> None)

        let flagNames = rollups |> List.collect (List.map snd)

        checkNewNames
            ctx
            call.Span
            "SUMMARIZECOLUMNS"
            (groupCols |> Array.map Tables.modelColumn)
            (flagNames @ (added |> List.map (fun (name, _, _) -> name)))

        // The filter tables, each evaluated on its own in the context of the call.
        let filters =
            arguments
            |> List.choose (fun a ->
                match a with
                | FilterTable(e, keep) ->
                    if not (ev.IsTable(ctx, e)) then
                        Fail.invalid ctx e.Span "a filter argument of SUMMARIZECOLUMNS is a table expression"

                    Some(Filters.ofTable ctx e.Span (ev.Table(ctx, e)), keep)
                | _ -> None)

        if filters |> List.exists snd && filters |> List.exists (fun (_, keep) -> not keep) then
            // "SummarizeColumns filters with keepfilters behavior and overridefilters behavior are
            // mixed within one cluster, which is not allowed" (Learn): which mixes are one cluster
            // is not spelled out, so every mix is refused.
            Fail.notSupported
                ctx
                call.Span
                "SUMMARIZECOLUMNS with some filters under KEEPFILTERS and some not (use KEEPFILTERS on all of them or on none)"

        let filtered =
            Filters.add
                (Filters.add ctx.Filter (filters |> List.filter (fun (_, keep) -> not keep) |> List.collect fst) false)
                (filters |> List.filter snd |> List.collect fst)
                true

        let exprs =
            added |> List.map (fun (_, expr, ignored) -> expr, ignored) |> Array.ofList

        // The states of the rollups: for each ROLLUPADDISSUBTOTAL, how many of its levels are
        // still grouped (all of them: a detail row; none: its grand total).
        let states =
            rollups
            |> List.fold
                (fun (acc: int list list) levels -> [
                    for state in acc do
                        for kept in levels.Length .. -1 .. 0 -> state @ [ kept ]
                ])
                [ [] ]

        let rows = ResizeArray<DaxValue array * DaxValue array * DaxValue array>()

        for state in states do
            // Which columns are grouped in this state, and the subtotal columns' values.
            let rolled = HashSet<int>()
            let flags = ResizeArray<DaxValue>()

            List.iter2
                (fun (levels: (BColumn array * string) list) (kept: int) ->
                    levels
                    |> List.iteri (fun i (cols, _) ->
                        flags.Add(DaxValue.Boolean(i >= kept))

                        if i >= kept then
                            for c in cols do
                                rolled.Add c.Id |> ignore))
                rollups
                state

            let active = groupCols |> Array.filter (fun c -> not (rolled.Contains c.Id))
            let flags = flags.ToArray()

            for (key, values) in summarizeGroups ev ctx call filtered active exprs do
                let mutable next = 0

                let full =
                    groupCols
                    |> Array.map (fun c ->
                        if rolled.Contains c.Id then
                            DaxValue.Blank
                        else
                            next <- next + 1
                            key.[next - 1])

                rows.Add((full, flags, values))

        let rows = rows.ToArray()

        let groupData =
            groupCols
            |> Array.mapi (fun i _ -> rows |> Array.map (fun (key, _, _) -> key.[i]))

        let flagData =
            flagNames
            |> List.mapi (fun i _ -> rows |> Array.map (fun (_, flags, _) -> flags.[i]))
            |> Array.ofList

        let extensions =
            added
            |> List.mapi (fun i (name, expr, _) ->
                let t, values =
                    Typing.column (ev.TypeOf(ctx, expr)) (rows |> Array.map (fun (_, _, v) -> v.[i]))

                Tables.extension name t, values)
            |> Array.ofList

        Tables.ofColumns
            (Array.concat [
                groupCols |> Array.map Tables.modelColumn
                flagNames
                |> List.map (fun n -> Tables.extension n DaxType.Boolean)
                |> Array.ofList
                extensions |> Array.map fst
            ])
            (Array.concat [ groupData; flagData; extensions |> Array.map snd ])
            rows.Length


    let functions: DaxFunction list = [
        // Each new column is evaluated in a row context on the table; the filter context is the
        // caller's (dax.guide/addcolumns).
        Fn.table "ADDCOLUMNS" "Table manipulation" (3, -1) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let added = pairs ctx call.Span "ADDCOLUMNS" call.Args.Tail
            checkNewNames ctx call.Span "ADDCOLUMNS" table.Cols (added |> List.map fst)

            let columns =
                added
                |> List.map (fun (name, expr) ->
                    let t, values = extensionColumn ev ctx table expr
                    Tables.extension name t, values)
                |> Array.ofList

            Tables.addColumns table (columns |> Array.map fst) (columns |> Array.map snd))

        // A column that is a plain column reference keeps its data lineage, under its new name; an
        // expression has none (SQLBI, "Understanding data lineage in DAX").
        Fn.table "SELECTCOLUMNS" "Table manipulation" (2, -1) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])

            let rec read (items: Expr list) : (string option * Expr) list =
                match items with
                | [] -> []
                | name :: expr :: tail when isName name -> (Some(literalName ctx name), expr) :: read tail
                | expr :: tail -> (None, expr) :: read tail

            let columns =
                read call.Args.Tail
                |> List.map (fun (name, expr) ->
                    let at = Context.pushRow ctx table 0
                    let t, values = extensionColumn ev ctx table expr

                    let source =
                        match (Fn.unparen expr).Kind with
                        | Qualified _ -> ev.TryColumn(at, expr)
                        | _ -> None

                    match name, source with
                    | Some n, Some c ->
                        {
                            Name = n
                            Table = null
                            Lineage = c
                            Type = t
                        },
                        values
                    | Some n, None -> Tables.extension n t, values
                    | None, Some c -> { Tables.modelColumn c with Type = t }, values
                    | None, None ->
                        Fail.invalid
                            ctx
                            expr.Span
                            "a SELECTCOLUMNS column that is not a column reference needs a name")
                |> Array.ofList

            checkNewNames ctx call.Span "SELECTCOLUMNS" (columns |> Array.map fst) []
            Tables.ofColumns (columns |> Array.map fst) (columns |> Array.map snd) table.Count)

        Fn.table "ROW" "Table manipulation" (2, -1) (fun ev ctx call ->
            let columns =
                pairs ctx call.Span "ROW" call.Args
                |> List.map (fun (name, expr) ->
                    let t, values = Typing.column (ev.TypeOf(ctx, expr)) [| ev.Scalar(ctx, expr) |]
                    Tables.extension name t, values)
                |> Array.ofList

            checkNewNames ctx call.Span "ROW" (columns |> Array.map fst) []
            Tables.ofColumns (columns |> Array.map fst) (columns |> Array.map snd) 1)

        // DATATABLE ( name, type, ..., { { value, ... }, ... } ): constants only.
        Fn.table "DATATABLE" "Table manipulation" (3, -1) (fun ev ctx call ->
            let args = Array.ofList call.Args

            if args.Length % 2 <> 1 then
                Fail.invalid ctx call.Span "DATATABLE takes pairs of a name and a type, then the rows"

            let cols = [|
                for i in 0..2 .. args.Length - 3 -> literalName ctx args.[i], datatableType ctx args.[i + 1]
            |]

            let rows =
                match args.[args.Length - 1].Kind with
                | TableConstructor rows ->
                    rows
                    |> List.map (fun r ->
                        match r.Kind with
                        | TableConstructor cells when cells.Length = cols.Length ->
                            cells
                            |> List.mapi (fun c cell ->
                                let constant =
                                    match (Fn.unparen cell).Kind with
                                    | IntegerLiteral _
                                    | NumberLiteral _
                                    | StringLiteral _
                                    | BooleanLiteral _
                                    | DateLiteral _
                                    | Unary(UnaryOp.Minus,
                                            {
                                                Kind = (IntegerLiteral _ | NumberLiteral _)
                                            })
                                    | Call({ Name = ("BLANK" | "blank" | "Blank") }, []) -> true
                                    | _ -> false

                                if not constant then
                                    Fail.invalid ctx cell.Span "DATATABLE takes constants only"

                                Fail.dax ctx cell.Span (DaxValue.convertTo (snd cols.[c]) (ev.Scalar(ctx, cell))))
                            |> Array.ofList
                        | _ -> Fail.invalid ctx r.Span $"each DATATABLE row is {{ ... }} with {cols.Length} value(s)")
                | _ ->
                    Fail.invalid ctx args.[args.Length - 1].Span "the last argument of DATATABLE is { { ... }, ... }"

            Tables.ofRows (cols |> Array.map (fun (n, t) -> Tables.extension n t)) rows)

        // "If there is a tie ... at the N-th row of the table, then all tied rows are returned";
        // "If N_Value is 0 (zero) or less than 0, then TOPN returns an empty table"; order
        // "0/FALSE/DESC – descending; 1/TRUE/ASC – ascending", descending by default
        // (dax.guide/topn). The result's own order is not defined.
        Fn.table "TOPN" "Table manipulation" (2, -1) (fun ev ctx call ->
            let n =
                match ev.Scalar(ctx, call.Args.[0]) with
                | DaxValue.Integer i -> i
                | DaxValue.Blank -> 0L
                | _ -> Fail.notSupported ctx call.Args.[0].Span "a TOPN row count that is not a whole number"

            let table = ev.Table(ctx, call.Args.[1])

            let rec read (items: Expr list) : (Expr * bool) list =
                match items with
                | [] -> []
                | key :: order :: tail when (descending order).IsSome || Fn.isOmitted order ->
                    (key, descending order |> Option.defaultValue true) :: read tail
                | key :: tail -> (key, true) :: read tail

            let keys = read (call.Args |> List.skip 2) |> Array.ofList

            if n <= 0L then
                Tables.select table [||]
            elif int64 table.Count <= n then
                table
            elif keys.Length = 0 then
                Fail.notSupported
                    ctx
                    call.Span
                    "TOPN without an order-by expression, on a table with more rows than asked for"
            else
                let values =
                    Array.init table.Count (fun row ->
                        let inner = Context.pushRow ctx table row
                        keys |> Array.map (fun (k, _) -> ev.Scalar(inner, k)))

                for k in 0 .. keys.Length - 1 do
                    checkSortable ctx call.Span "TOPN" (values |> Seq.map (fun v -> v.[k]))

                let compare (a: int) (b: int) =
                    let mutable c = 0
                    let mutable i = 0

                    while c = 0 && i < keys.Length do
                        c <- DaxValue.sortCompare ctx.Model.Collation values.[a].[i] values.[b].[i]

                        if snd keys.[i] then
                            c <- -c

                        i <- i + 1

                    c

                let sorted = Array.init table.Count id |> Array.sortWith compare
                let last = sorted.[int n - 1]
                let keep = sorted |> Array.filter (fun row -> compare row last <= 0) |> Array.sort
                Tables.select table keep)

        generate false "GENERATE"
        generate true "GENERATEALL"

        // "Column names from table arguments must all be different in all tables or an error is
        // returned" (Microsoft Learn, CROSSJOIN).
        Fn.table "CROSSJOIN" "Table manipulation" (2, -1) (fun ev ctx call ->
            let tables = call.Args |> List.map (fun a -> ev.Table(ctx, a))
            let cols = tables |> List.map _.Cols |> Array.concat
            checkNewNames ctx call.Span "CROSSJOIN" cols []

            let bare = cols |> Array.countBy (fun c -> c.Name.ToUpperInvariant())

            if bare |> Array.exists (fun (_, n) -> n > 1) then
                Fail.notSupported
                    ctx
                    call.Span
                    "CROSSJOIN of tables with columns of the same name in different tables (whether that is an error is not documented)"

            let rows =
                tables
                |> List.fold
                    (fun (acc: DaxValue array list) t -> [
                        for left in acc do
                            for row in 0 .. t.Count - 1 -> Array.append left (t.Row row)
                    ])
                    [ [||] ]

            Tables.ofRows cols rows)

        // "Columns are combined by position"; "The column names in the return table will match
        // the column names in tableExpression1"; "Duplicate rows are retained" (Microsoft Learn,
        // UNION). A column keeps its lineage when every table gives it the same one.
        Fn.table "UNION" "Table manipulation" (2, -1) (fun ev ctx call ->
            let tables = call.Args |> List.map (fun a -> ev.Table(ctx, a))
            let first = tables.Head

            if tables |> List.exists (fun t -> t.Cols.Length <> first.Cols.Length) then
                Fail.invalid ctx call.Span "the tables of UNION must have the same number of columns"

            let columns =
                first.Cols
                |> Array.mapi (fun c info ->
                    let values = tables |> List.map (fun t -> t.Column c) |> Array.concat

                    let declared =
                        tables
                        |> List.fold
                            (fun t table -> Typing.unify t (Typing.ofColumn table.Cols.[c].Type))
                            StaticType.Blank

                    let t, values =
                        match Typing.column declared values with
                        | DaxType.Variant, _ when Typing.ofValues values = Some DaxType.Variant ->
                            Fail.notSupported ctx call.Span "UNION of columns of different data types"
                        | typed -> typed

                    let sameLineage =
                        tables
                        |> List.forall (fun table -> obj.ReferenceEquals(table.Cols.[c].Lineage, info.Lineage))

                    {
                        info with
                            Type = t
                            Lineage = (if sameLineage then info.Lineage else null)
                    },
                    values)

            Tables.ofColumns (columns |> Array.map fst) (columns |> Array.map snd) (tables |> List.sumBy _.Count))

        naturalJoin false "NATURALINNERJOIN"
        naturalJoin true "NATURALLEFTOUTERJOIN"
        setOperation true "INTERSECT"
        setOperation false "EXCEPT"

        // SUMMARIZE ( table, group-by column, ..., name, expression, ... ): one row per existing
        // combination. An expression is evaluated in a row context on the group's key AND in a
        // filter context made of all the rows of the group, which for the rows of a model table
        // is a filter on its expanded table ("The cluster header contains Product[Color], but the
        // cluster filter contains all the columns in the expanded table Sales": SQLBI, "All the
        // secrets of SUMMARIZE"). ROLLUP and its relatives are not supported.
        Fn.table "SUMMARIZE" "Table manipulation" (2, -1) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let rest = call.Args.Tail
            let groupArgs = rest |> List.takeWhile (fun a -> not (isName a))
            let added = pairs ctx call.Span "SUMMARIZE" (rest |> List.skip groupArgs.Length)

            if groupArgs.IsEmpty then
                Fail.notSupported ctx call.Span "SUMMARIZE without a group-by column"

            let groupCols =
                groupArgs |> List.map (groupColumn ev ctx "SUMMARIZE" table) |> Array.ofList

            let groups = groupRows ctx.Model (groupCols |> Array.map snd) table.Count
            let headerCols = groupCols |> Array.map fst
            checkNewNames ctx call.Span "SUMMARIZE" headerCols (added |> List.map fst)
            let header = Tables.ofRows headerCols (groups |> Array.map fst)

            let extensions =
                added
                |> List.map (fun (name, expr) ->
                    let values =
                        groups
                        |> Array.mapi (fun g (_, rows) ->
                            let cluster = Tables.select table rows

                            let inner =
                                Context.withFilters
                                    (Context.pushRow ctx header g)
                                    (Filters.ofTable ctx call.Args.[0].Span cluster)
                                    false

                            ev.Scalar(inner, expr))

                    let t, values = Typing.column (ev.TypeOf(Context.pushRow ctx header 0, expr)) values
                    Tables.extension name t, values)
                |> Array.ofList

            Tables.addColumns header (extensions |> Array.map fst) (extensions |> Array.map snd))
        |> Fn.withCheck (fun args -> checkHelpers "SUMMARIZE" args |> Option.orElse (checkGrouped "SUMMARIZE" args))

        // SUMMARIZECOLUMNS: see `summarizeColumns`.
        Fn.table "SUMMARIZECOLUMNS" "Table manipulation" (1, -1) summarizeColumns
        |> Fn.withCheck checkSummarizeColumns

        onlyInside "ROLLUPADDISSUBTOTAL" "SUMMARIZECOLUMNS"
        onlyInside "ROLLUPGROUP" "ROLLUPADDISSUBTOTAL"
        onlyInside "IGNORE" "SUMMARIZECOLUMNS"
        onlyInside "NONVISUAL" "SUMMARIZECOLUMNS"

        // GROUPBY ( table, group-by column, ..., name, X-aggregation over CURRENTGROUP () ): "an
        // implied CALCULATE is not performed, and the group isn't placed into the filter context"
        // (Microsoft Learn, GROUPBY).
        Fn.table "GROUPBY" "Table manipulation" (1, -1) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let rest = call.Args.Tail
            let groupArgs = rest |> List.takeWhile (fun a -> not (isName a))
            let added = pairs ctx call.Span "GROUPBY" (rest |> List.skip groupArgs.Length)

            if groupArgs.IsEmpty then
                Fail.notSupported ctx call.Span "GROUPBY without a group-by column"

            let groupCols =
                groupArgs |> List.map (groupColumn ev ctx "GROUPBY" table) |> Array.ofList

            let groups = groupRows ctx.Model (groupCols |> Array.map snd) table.Count
            let headerCols = groupCols |> Array.map fst
            checkNewNames ctx call.Span "GROUPBY" headerCols (added |> List.map fst)
            let header = Tables.ofRows headerCols (groups |> Array.map fst)

            let extensions =
                added
                |> List.map (fun (name, expr) ->
                    let values =
                        groups
                        |> Array.map (fun (_, rows) ->
                            ev.Scalar(
                                {
                                    ctx with
                                        Group = Tables.select table rows
                                },
                                expr
                            ))

                    let t, values =
                        Typing.column
                            (ev.TypeOf(
                                {
                                    ctx with
                                        Group = Tables.select table [||]
                                },
                                expr
                            ))
                            values

                    Tables.extension name t, values)
                |> Array.ofList

            Tables.addColumns header (extensions |> Array.map fst) (extensions |> Array.map snd))
        |> Fn.withCheck (fun args ->
            // "CURRENTGROUP ... is only supported as the first argument to one of the following
            // aggregation functions: AVERAGEX, COUNTAX, COUNTX, GEOMEANX, MAXX, MINX, PRODUCTX,
            // STDEVX.S, STDEVX.P, SUMX, VARX.S, VARX.P" (Microsoft Learn, CURRENTGROUP).
            let allowed = [ "AVERAGEX"; "COUNTAX"; "COUNTX"; "MAXX"; "MINX"; "PRODUCTX"; "SUMX" ]

            let rest =
                args
                |> List.skip (min 1 args.Length)
                |> List.skipWhile (fun a -> not (isName a))

            let columns () =
                rest
                |> List.chunkBySize 2
                |> List.tryPick (fun p ->
                    match p with
                    | [ _; expr ] ->
                        match (Fn.unparen expr).Kind with
                        | Call(fn, first :: _) when allowed |> List.contains (fn.Name.ToUpperInvariant()) ->
                            match (Fn.unparen first).Kind with
                            | Call(g, []) when g.Name.ToUpperInvariant() = "CURRENTGROUP" -> None
                            | _ -> Some "a GROUPBY column that is not an X aggregation over CURRENTGROUP ()"
                        | _ -> Some "a GROUPBY column that is not an X aggregation over CURRENTGROUP ()"
                    | _ -> None)

            checkGrouped "GROUPBY" args |> Option.orElseWith columns)

        Fn.table "CURRENTGROUP" "Table manipulation" (0, 0) (fun _ ctx call ->
            if isNull ctx.Group then
                Fail.invalid ctx call.Span "CURRENTGROUP can only be used in a column of GROUPBY"
            else
                ctx.Group)
    ]
