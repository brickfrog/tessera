namespace Tessera.DaxEngine.Functions

// CALCULATE and everything that reads or changes the filter context, and the functions that follow
// relationships.

open System
open System.Collections.Generic
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Filter =

    let private sameTable (ctx: Ctx) (span: Span) (name: string) (cols: BColumn array) : BTable =
        let t = cols.[0].Table

        if cols |> Array.exists (fun c -> not (obj.ReferenceEquals(c.Table, t))) then
            Fail.invalid ctx span $"all the columns given to {name} must be of one table"

        t

    /// The existing combinations of some columns of a table in a set of its rows.
    let private combinations (cols: BColumn array) (rows: int array) : TableVal =
        let infos = cols |> Array.map Tables.modelColumn

        if cols.Length = 1 then
            let c = cols.[0]
            let size = c.Table.Size
            let ids = Filters.idsIn c (RowSet.OfSeq(size, rows)) true
            Tables.ofColumns infos [| ids |> Array.map c.ValueOfId |] ids.Length
        else
            let seen =
                HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

            let keep = ResizeArray<int array>()

            for row in rows do
                let key = cols |> Array.map (fun c -> c.IdAt row)

                if seen.Add key then
                    keep.Add key

            let data =
                cols
                |> Array.mapi (fun i c -> Array.init keep.Count (fun r -> c.ValueOfId keep.[r].[i]))

            Tables.ofColumns infos data keep.Count

    /// Every row of a table: `blankRow` adds the blank row when the table has one.
    let private allRows (t: BTable) (blankRow: bool) : int array =
        Array.init (if blankRow then t.Size else t.RowCount) id

    /// ALL / ALLNOBLANKROW as a table: "ALL includes in the result the additional blank row
    /// generated for invalid relationships" (dax.guide/all); "ALLNOBLANKROW does not return the
    /// blank row" (SQLBI, "Blank row in DAX").
    let private allTable (name: string) (blankRow: bool) (ev: IEval) (ctx: Ctx) (call: Call) : TableVal =
        match call.Args with
        | [] ->
            Fail.invalid ctx call.Span $"{name} () with no argument can only be used as a filter argument of CALCULATE"
        | [ single ] when (ev.TryModelTable(ctx, single)).IsSome ->
            let t = (ev.TryModelTable(ctx, single)).Value
            Tables.ofBase ctx.Filter.Config t (allRows t blankRow)
        | args ->
            let cols = args |> List.map (fun a -> ev.Column(ctx, a)) |> Array.ofList
            let t = sameTable ctx call.Span name cols
            combinations cols (allRows t blankRow)

    /// What a filter-removing modifier removes, as a test on columns.
    let private removal (ev: IEval) (ctx: Ctx) (config: RelConfig) (call: Call) : BColumn -> bool =
        let name = call.Name.Name.ToUpperInvariant()

        let expandedOf (t: BTable) =
            // The columns a table modifier removes depend on which relationships are in force;
            // when the same CALCULATE changes those for this table, which set applies isn't
            // documented.
            let before = Filters.expandedColumns ctx.Filter.Config t
            let after = Filters.expandedColumns config t

            if
                before.Length <> after.Length
                || Array.exists2 (fun a b -> not (obj.ReferenceEquals(a, b))) before after
            then
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name} ( {t.Name} ) together with a relationship modifier that changes the tables related to it"

            fun (c: BColumn) -> config.IsExpanded(t, c.Table)

        match name, call.Args with
        | ("ALL" | "REMOVEFILTERS"), [] -> fun _ -> true
        | "ALLEXCEPT", table :: kept ->
            let t =
                match ev.TryModelTable(ctx, table) with
                | Some t -> t
                | None -> Fail.invalid ctx table.Span "the first argument of ALLEXCEPT is a table"

            let inTable = expandedOf t

            // "keeping only the filters in the columns specified": a column, or a whole table of
            // the expanded table (dax.guide/allexcept).
            let keeps =
                kept
                |> List.map (fun k ->
                    match ev.TryModelTable(ctx, k) with
                    | Some kt -> fun (c: BColumn) -> obj.ReferenceEquals(c.Table, kt)
                    | None ->
                        let kc = ev.Column(ctx, k)

                        if not (inTable kc) then
                            Fail.invalid
                                ctx
                                k.Span
                                $"'{kc.Table.Name}'[{kc.Name}] is not a column of '{t.Name}' or of a table related to it"

                        fun (c: BColumn) -> obj.ReferenceEquals(c, kc))

            fun c -> inTable c && not (keeps |> List.exists (fun k -> k c))
        | "ALLCROSSFILTERED", [ table ] ->
            let t =
                match ev.TryModelTable(ctx, table) with
                | Some t -> t
                | None -> Fail.invalid ctx table.Span "the argument of ALLCROSSFILTERED is a table"

            // It also removes the filters "on columns and tables that are cross-filtering the table
            // argument because of ... bidirectional cross-filters" (dax.guide/allcrossfiltered):
            // which ones exactly isn't spelled out, so only models without them are answered.
            if ctx.Model.Rels |> Array.exists config.FiltersBothWays then
                Fail.notSupported ctx call.Span "ALLCROSSFILTERED in a model with a relationship that filters both ways"

            expandedOf t
        | _, [ single ] when (ev.TryModelTable(ctx, single)).IsSome ->
            // "ALL ( Sales ) ... also removes filters from the derived columns of Sales, which
            // include Product[Color]" (SQLBI, "Filtering tables").
            expandedOf (ev.TryModelTable(ctx, single)).Value
        | _, args ->
            let cols = args |> List.map (fun a -> ev.Column(ctx, a)) |> Array.ofList
            sameTable ctx call.Span name cols |> ignore
            fun c -> cols |> Array.exists (fun x -> obj.ReferenceEquals(x, c))

    /// ALLEXCEPT as a table: "all the unique combinations of the columns in the table specified
    /// in the first argument that are not listed in the following arguments"
    /// (dax.guide/allexcept), with no filter applied. Whether the blank row of an invalid
    /// relationship is among them isn't stated, so a table that has one is refused; so is a table
    /// as a kept argument, a kept column of a related table, and listing every column.
    let private allExceptTable (ev: IEval) (ctx: Ctx) (call: Call) : TableVal =
        let t =
            match ev.TryModelTable(ctx, call.Args.Head) with
            | Some t -> t
            | None -> Fail.invalid ctx call.Args.Head.Span "the first argument of ALLEXCEPT is a table"

        let kept =
            call.Args.Tail
            |> List.map (fun k ->
                if (ev.TryModelTable(ctx, k)).IsSome then
                    Fail.notSupported ctx k.Span "ALLEXCEPT as a table with a table among the kept arguments"

                let kc = ev.Column(ctx, k)

                if not (ctx.Filter.Config.IsExpanded(t, kc.Table)) then
                    Fail.invalid
                        ctx
                        k.Span
                        $"'{kc.Table.Name}'[{kc.Name}] is not a column of '{t.Name}' or of a table related to it"

                // Whether the result then has the columns of the related tables (the expanded
                // table's) is not documented.
                if not (obj.ReferenceEquals(kc.Table, t)) then
                    Fail.notSupported
                        ctx
                        k.Span
                        $"ALLEXCEPT as a table keeping a column of '{kc.Table.Name}', a table related to '{t.Name}'"

                kc)

        let rest =
            t.Columns
            |> Array.filter (fun c -> not (kept |> List.exists (fun k -> obj.ReferenceEquals(k, c))))

        if rest.Length = 0 then
            Fail.notSupported ctx call.Span "ALLEXCEPT as a table that lists every column of the table"

        if t.HasBlankRow then
            Fail.notSupported
                ctx
                call.Span
                $"ALLEXCEPT as a table over '{t.Name}', which has a blank row for an invalid relationship (whether it is returned isn't documented)"

        combinations rest (allRows t false)

    let private relationship (ev: IEval) (ctx: Ctx) (call: Call) : BRel =
        let a = ev.Column(ctx, call.Args.[0])
        let b = ev.Column(ctx, call.Args.[1])

        let found =
            ctx.Model.Rels
            |> Array.tryFind (fun r ->
                (obj.ReferenceEquals(r.From, a) && obj.ReferenceEquals(r.To, b))
                || (obj.ReferenceEquals(r.From, b) && obj.ReferenceEquals(r.To, a)))

        match found with
        | Some r -> r
        | None ->
            Fail.invalid
                ctx
                call.Span
                $"there is no relationship between '{a.Table.Name}'[{a.Name}] and '{b.Table.Name}'[{b.Name}]"

    /// The relationships in force after a USERELATIONSHIP or CROSSFILTER.
    let private reconfigure (ev: IEval) (ctx: Ctx) (config: RelConfig) (call: Call) : RelConfig =
        let r = relationship ev ctx call
        let active = Array.copy config.Active
        let both = Array.copy config.Both

        match call.Name.Name.ToUpperInvariant() with
        | "USERELATIONSHIP" ->
            // "activates a disabled relationship, deactivating possible conflicting relationships"
            // (dax.guide/userelationship). The ones between the same two tables are deactivated
            // here; any other conflict is refused below.
            for other in ctx.Model.Rels do
                let sameTables =
                    (obj.ReferenceEquals(other.From.Table, r.From.Table)
                     && obj.ReferenceEquals(other.To.Table, r.To.Table))
                    || (obj.ReferenceEquals(other.From.Table, r.To.Table)
                        && obj.ReferenceEquals(other.To.Table, r.From.Table))

                if sameTables then
                    active.[other.Id] <- false

            active.[r.Id] <- true
        | _ ->
            let direction =
                match Fn.keyword call.Args.[2], (Fn.unparen call.Args.[2]).Kind with
                | Some k, _ -> k
                | None, IntegerLiteral(0L, _) -> "NONE"
                | None, IntegerLiteral(1L, _) -> "ONEWAY"
                | None, IntegerLiteral(2L, _) -> "BOTH"
                | _ -> ""

            match direction with
            | "NONE" -> active.[r.Id] <- false
            | "ONEWAY" -> both.[r.Id] <- false
            | "BOTH" -> both.[r.Id] <- true
            | _ -> Fail.notSupported ctx call.Args.[2].Span "a CROSSFILTER direction other than NONE, ONEWAY or BOTH"

        let next = ctx.Model.Config(active, both)

        match next.Ambiguity with
        | Some tables ->
            Fail.notSupported
                ctx
                call.Span
                $"relationships that give more than one filter path between {tables} (the engine's priority rules are not implemented)"
        | None -> next

    let private isCallTo (names: string list) (e: Expr) : Call option =
        match (Fn.unparen e).Kind with
        | Call(name, args) when names |> List.contains (name.Name.ToUpperInvariant()) ->
            Some {
                Name = name
                Args = args
                Span = e.Span
            }
        | _ -> None

    /// The model columns a predicate reads (measures are an error in one).
    let private predicateColumns (ev: IEval) (ctx: Ctx) (e: Expr) : BColumn list =
        let found = ResizeArray<BColumn>()

        let rec walk (scope: Set<string>) (e: Expr) =
            match e.Kind with
            | Qualified(table, name) when not (scope.Contains(table.Name.ToUpperInvariant())) || table.Quoted ->
                match ev.TryColumn(ctx, e) with
                | Some c ->
                    if not (found.Exists(fun x -> obj.ReferenceEquals(x, c))) then
                        found.Add c
                | None ->
                    // "A function 'PLACEHOLDER' has been used in a True/False expression that is
                    // used as a table filter expression" is what the engine says of a measure here
                    // (SQLBI, "Solving errors in CALCULATE filter arguments").
                    Fail.invalid
                        ctx
                        e.Span
                        $"a measure ({name}) can't be used in a True/False filter argument of CALCULATE"
            | Bracketed name ->
                Fail.notSupported
                    ctx
                    e.Span
                    $"[{name}] without its table in a True/False filter argument of CALCULATE (write Table[{name}])"
            | Call(fn, _) when [ "CALCULATE"; "CALCULATETABLE" ] |> List.contains (fn.Name.ToUpperInvariant()) ->
                Fail.invalid
                    ctx
                    e.Span
                    $"A function '{fn.Name.ToUpperInvariant()}' has been used in a True/False expression that is used as a table filter expression. This is not allowed."
            | VarBlock(vars, body) ->
                let mutable inner = scope

                for v in vars do
                    walk inner v.Value
                    inner <- inner.Add(v.Name.Name.ToUpperInvariant())

                walk inner body
            | _ -> List.iter (walk scope) (children e)

        walk Set.empty e
        List.ofSeq found

    /// A True/False filter argument as the table it stands for: "table[column] = 10" is "FILTER (
    /// ALL ( table[column] ), table[column] = 10 )" (dax.guide/calculate), and a predicate over
    /// several columns of one table is FILTER over ALL of those columns (SQLBI, "Specifying
    /// multiple filter conditions in CALCULATE").
    let private predicateFilter (ev: IEval) (ctx: Ctx) (e: Expr) : Filter list =
        let cols = predicateColumns ev ctx e |> Array.ofList

        if cols.Length = 0 then
            Fail.notSupported ctx e.Span "a True/False filter argument of CALCULATE that names no column"

        if
            cols
            |> Array.exists (fun c -> not (obj.ReferenceEquals(c.Table, cols.[0].Table)))
        then
            Fail.invalid
                ctx
                e.Span
                "The expression contains columns from multiple tables, but only columns from a single table can be used in a True/False expression that is used as a table filter expression."

        let t = cols.[0].Table
        let all = combinations cols (allRows t true)

        let keep = [|
            for row in 0 .. all.Count - 1 do
                let v = ev.Scalar(Context.pushRow ctx all row, e)

                if Fail.dax ctx e.Span (DaxValue.toBoolean v) then
                    row
        |]

        Filters.ofTable ctx e.Span (Tables.select all keep)

    /// CALCULATE's context: dax.guide/calculate, steps 1 to 5.
    let private calculateContext (ev: IEval) (ctx: Ctx) (call: Call) : Ctx =
        let args = call.Args.Tail

        if not (isNull ctx.Group) then
            Fail.invalid ctx call.Span "CALCULATE can't be used inside GROUPBY"

        let removers = [ "ALL"; "REMOVEFILTERS"; "ALLEXCEPT"; "ALLCROSSFILTERED"; "ALLNOBLANKROW" ]

        let relationshipModifiers = [ "USERELATIONSHIP"; "CROSSFILTER" ]

        // 1. The filter arguments are evaluated in the original context, each on its own.
        let filters =
            args
            |> List.choose (fun a ->
                if (isCallTo removers a).IsSome || (isCallTo relationshipModifiers a).IsSome then
                    None
                else
                    let inner, keep =
                        match isCallTo [ "KEEPFILTERS" ] a with
                        | Some k -> k.Args.[0], true
                        | None -> a, false

                    if (isCallTo [ "KEEPFILTERS" ] inner).IsSome then
                        Fail.notSupported ctx inner.Span "KEEPFILTERS inside KEEPFILTERS"

                    let filter =
                        if ev.IsTable(ctx, inner) then
                            Filters.ofTable ctx inner.Span (ev.Table(ctx, inner))
                        else
                            predicateFilter ev ctx inner

                    Some(filter, keep))

        // 2 and 3. The row contexts become filters; none is left.
        let moved = Context.transition ctx call.Span

        // 4. The modifiers: first the relationships, then the filters to remove.
        let config =
            args
            |> List.choose (isCallTo relationshipModifiers)
            |> List.fold (fun cfg m -> reconfigure ev ctx cfg m) moved.Filter.Config

        let mutable fc = Filters.withConfig moved.Filter config

        for m in args |> List.choose (isCallTo removers) do
            fc <- Filters.remove fc (removal ev ctx config m)

        // A filter on the date column of a date table first removes the filters on that table.
        fc <- DateTable.onFilterArguments ctx call.Span fc filters

        // 5. The filters: each replaces the filters on its columns, unless under KEEPFILTERS.
        fc <- Filters.add fc (filters |> List.filter (fun (_, keep) -> not keep) |> List.collect fst) false
        fc <- Filters.add fc (filters |> List.filter snd |> List.collect fst) true
        { moved with Filter = fc }

    let private checkCalculate (args: Expr list) : string option =
        args
        |> List.tryPick (fun a ->
            match (Fn.unparen a).Kind with
            | Call(fn, _) when fn.Name.ToUpperInvariant() = "ALLSELECTED" -> Some "ALLSELECTED"
            | _ -> None)

    /// The visible values of a column: VALUES counts the blank row, DISTINCT doesn't ("DISTINCT
    /// never returns the blank row": SQLBI, "Blank row in DAX").
    let private columnValues (ctx: Ctx) (c: BColumn) (blankRow: bool) : TableVal =
        let ids = Filters.idsIn c (Filters.visible ctx.Filter c.Table) blankRow
        Tables.ofColumns [| Tables.modelColumn c |] [| ids |> Array.map c.ValueOfId |] ids.Length

    let private countValues (ctx: Ctx) (c: BColumn) : int =
        (Filters.idsIn c (Filters.visible ctx.Filter c.Table) true).Length

    /// Whether any filter is on this column, and whether one of them is a table filter (which
    /// the definitions of ISFILTERED and HASONEFILTER don't cover).
    let private directFilters (ctx: Ctx) (c: BColumn) : Filter list =
        ctx.Filter.Filters
        |> List.filter (fun f ->
            match f with
            | OnColumn(x, _) -> obj.ReferenceEquals(x, c)
            | Tuples(cols, _) -> cols |> Array.exists (fun x -> obj.ReferenceEquals(x, c))
            | Rows(t, _, cfg) -> cfg.IsExpanded(t, c.Table))

    let private isFiltered (ctx: Ctx) (span: Span) (name: string) (c: BColumn) : bool =
        let filters = directFilters ctx c

        if
            filters
            |> List.exists (fun f ->
                match f with
                | Rows _ -> true
                | _ -> false)
        then
            Fail.notSupported
                ctx
                span
                $"{name} of a column under a table filter (whether a table filter counts as filtering each of its columns is not documented)"

        not filters.IsEmpty

    let functions: DaxFunction list = [
        Fn.scalar "CALCULATE" "Filter" (1, -1) (Fn.typeOfArg 0) (fun ev ctx call ->
            ev.Scalar(calculateContext ev ctx call, call.Args.[0]))
        |> Fn.withCheck checkCalculate

        Fn.table "CALCULATETABLE" "Filter" (1, -1) (fun ev ctx call ->
            ev.Table(calculateContext ev ctx call, call.Args.[0]))
        |> Fn.withCheck checkCalculate

        {
            Fn.modifier "KEEPFILTERS" "Filter" (1, 1) with
                Table =
                    fun _ ctx call ->
                        Fail.notSupported
                            ctx
                            call.Span
                            "KEEPFILTERS on the table of an iterator (it is supported on a filter argument of CALCULATE)"
        }
        Fn.modifier "USERELATIONSHIP" "Filter" (2, 2)
        Fn.modifier "CROSSFILTER" "Filter" (3, 3)
        |> Fn.withCheck (fun args ->
            match List.tryItem 2 args with
            | Some direction ->
                match Fn.keyword direction, (Fn.unparen direction).Kind with
                | Some("NONE" | "ONEWAY" | "BOTH"), _
                | None, IntegerLiteral((0L | 1L | 2L), _) -> None
                | _ -> Some "a CROSSFILTER direction other than NONE, ONEWAY or BOTH"
            | None -> None)
        Fn.modifier "REMOVEFILTERS" "Filter" (0, -1)
        Fn.modifier "ALLCROSSFILTERED" "Filter" (1, 1)

        {
            Fn.modifier "ALLEXCEPT" "Filter" (2, -1) with
                Table = allExceptTable
        }

        Fn.table "ALL" "Filter" (0, -1) (allTable "ALL" true)
        Fn.table "ALLNOBLANKROW" "Filter" (1, -1) (allTable "ALLNOBLANKROW" false)

        // FILTER: the rows of the table for which the condition is true, in a row context each.
        Fn.table "FILTER" "Filter" (2, 2) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let cond = call.Args.[1]

            let keep = [|
                for row in 0 .. table.Count - 1 do
                    let v = ev.Scalar(Context.pushRow ctx table row, cond)

                    if Fail.dax ctx cond.Span (DaxValue.toBoolean v) then
                        row
            |]

            if keep.Length = table.Count then
                table
            else
                Tables.select table keep)

        // VALUES ( column ): the visible values, with the blank row's BLANK. VALUES ( table ): "all
        // rows in the specified table plus a blank row, if there is a violation of referential
        // integrity" (Microsoft Learn, VALUES); duplicate rows stay.
        Fn.table "VALUES" "Filter" (1, 1) (fun ev ctx call ->
            match ev.TryModelTable(ctx, call.Args.[0]) with
            | Some t -> Tables.ofBase ctx.Filter.Config t (Filters.visible ctx.Filter t).Rows
            | None ->
                match ev.TryColumn(ctx, call.Args.[0]) with
                | Some c -> columnValues ctx c true
                | None -> Fail.invalid ctx call.Args.[0].Span "VALUES takes a column or a table name")

        // DISTINCT ( column ): the visible values without the blank row. DISTINCT ( table ): its
        // distinct rows.
        Fn.table "DISTINCT" "Filter" (1, 1) (fun ev ctx call ->
            match ev.TryColumn(ctx, call.Args.[0]) with
            | Some c -> columnValues ctx c false
            | None -> Tables.distinct ctx.Model (ev.Table(ctx, call.Args.[0])))

        // "COUNTROWS ( VALUES ( table[column] ) ) = 1" (dax.guide/countrows).
        Fn.scalar "HASONEVALUE" "Filter" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            DaxValue.Boolean(countValues ctx (ev.Column(ctx, call.Args.[0])) = 1))

        // "IF ( HASONEVALUE ( <columnName> ), VALUES ( <columnName> ), <alternateResult> )"
        // (dax.guide/selectedvalue).
        Fn.scalar
            "SELECTEDVALUE"
            "Filter"
            (1, 2)
            (fun ev ctx args ->
                match args with
                | [ c; alt ] -> Typing.unify (ev.TypeOf(ctx, c)) (ev.TypeOf(ctx, alt))
                | c :: _ -> ev.TypeOf(ctx, c)
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                let c = ev.Column(ctx, call.Args.[0])
                let ids = Filters.idsIn c (Filters.visible ctx.Filter c.Table) true

                if ids.Length = 1 then
                    c.ValueOfId ids.[0]
                else
                    match List.tryItem 1 call.Args with
                    | Some alt -> ev.Scalar(ctx, alt)
                    | None -> DaxValue.Blank)

        // "TRUE when ColumnName is being filtered directly, or when any column of TableName is
        // being filtered directly ... It does not check the columns of related tables"
        // (dax.guide/isfiltered).
        Fn.scalar "ISFILTERED" "Filter" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            match ev.TryModelTable(ctx, call.Args.[0]) with
            | Some t -> DaxValue.Boolean(t.Columns |> Array.exists (isFiltered ctx call.Span "ISFILTERED"))
            | None -> DaxValue.Boolean(isFiltered ctx call.Span "ISFILTERED" (ev.Column(ctx, call.Args.[0]))))

        // "TRUE when any column of the table specified or of a related table is being filtered"
        // (dax.guide/iscrossfiltered). A filter that reaches the table makes it TRUE; a filter on a
        // connected table that doesn't reach it (the many side of a one-way relationship) is the
        // case the definition leaves open.
        Fn.scalar "ISCROSSFILTERED" "Filter" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let t =
                match ev.TryModelTable(ctx, call.Args.[0]) with
                | Some t -> t
                | None -> (ev.Column(ctx, call.Args.[0])).Table

            if not (Filters.visible ctx.Filter t).IsAll then
                DaxValue.Boolean true
            else
                let cfg = ctx.Filter.Config
                let connected = HashSet<int>()

                let rec reach (x: BTable) =
                    if connected.Add x.Id then
                        for r in cfg.ManyToOne x do
                            reach r.To.Table

                        for r in cfg.OneToMany x do
                            reach r.From.Table

                reach t

                let touches (c: BColumn) = connected.Contains c.Table.Id

                let near =
                    ctx.Filter.Filters
                    |> List.exists (fun f ->
                        match f with
                        | OnColumn(c, _) -> touches c
                        | Tuples(cols, _) -> cols |> Array.exists touches
                        | Rows(x, _, _) -> connected.Contains x.Id)

                if near then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"ISCROSSFILTERED of '{t.Name}' while a table related to it is filtered but does not filter it"

                DaxValue.Boolean false)

        // "ISINSCOPE checks whether there is a filter placed on the column derived either from
        // context transition or from a group by column placed by SUMMARIZECOLUMNS"
        // (dax.guide/isinscope); "at the grand total ... Brand neither has one value, nor is it in
        // scope" (SQLBI, "Distinguishing HASONEVALUE from ISINSCOPE"). A filter that CALCULATE
        // puts on the column, or removes from it, ends that.
        Fn.scalar "ISINSCOPE" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let c = ev.Column(ctx, call.Args.[0])

            if not ctx.Rows.IsEmpty then
                Fail.notSupported
                    ctx
                    call.Span
                    "ISINSCOPE inside a row context (whether a column being iterated is in scope before a context transition is not documented)"

            let has (cols: BColumn array) =
                cols |> Array.exists (fun x -> obj.ReferenceEquals(x, c))

            let rec inScope (marks: ScopeMark list) =
                match marks with
                | [] -> false
                | Grouped cols :: rest -> has cols || inScope rest
                | Transitioned(own, related) :: rest ->
                    if has own then
                        true
                    elif related c then
                        Fail.notSupported
                            ctx
                            call.Span
                            $"ISINSCOPE of '{c.Table.Name}'[{c.Name}] after a context transition over a table related to '{c.Table.Name}' (whether the columns of related tables are then in scope is not documented)"
                    else
                        inScope rest
                | Unfiltered gone :: rest -> not (gone c) && inScope rest

            DaxValue.Boolean(inScope ctx.Filter.Scope))

        // "COUNTROWS ( FILTERS ( <ColumnName> ) ) = 1": "the number of directly filtered values"
        // (dax.guide/hasonefilter). Answered when the column has one filter of its own.
        Fn.scalar "HASONEFILTER" "Filter" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let c = ev.Column(ctx, call.Args.[0])

            match directFilters ctx c with
            | [ OnColumn(_, ids) ] -> DaxValue.Boolean(ids.Count = 1)
            | [] -> DaxValue.Boolean(Filters.idsIn c (RowSet.All c.Table.Size) true |> Array.length = 1)
            | _ -> Fail.notSupported ctx call.Span "HASONEFILTER of a column filtered together with other columns")

        // "TREATAS assigns the data lineage of the columns returned by the expression"; "For each
        // column, filters out any values that are not present in its respective output column"
        // (dax.guide/treatas).
        Fn.table "TREATAS" "Filter" (2, -1) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let cols = call.Args.Tail |> List.map (fun a -> ev.Column(ctx, a)) |> Array.ofList

            if cols.Length <> table.Cols.Length then
                Fail.invalid
                    ctx
                    call.Span
                    $"TREATAS was given {cols.Length} column(s) for a table of {table.Cols.Length} column(s)"

            let data = Array.init cols.Length table.Column

            let present (c: BColumn) (v: DaxValue) =
                match v, DaxValue.typeOf v with
                | DaxValue.Blank, _ -> c.Data.BlankId >= 0 || c.Table.HasBlankRow
                | _, Some t when
                    c.Data.DataType <> DaxType.Variant
                    && t <> c.Data.DataType
                    && not (DaxType.isNumeric t && DaxType.isNumeric c.Data.DataType)
                    ->
                    Fail.notSupported
                        ctx
                        call.Span
                        $"TREATAS of {DaxType.name t} values as the {DaxType.name c.Data.DataType} column '{c.Table.Name}'[{c.Name}]"
                | _ -> c.FindId v >= 0

            let keep = [|
                for row in 0 .. table.Count - 1 do
                    if cols |> Array.mapi (fun i c -> present c data.[i].[row]) |> Array.forall id then
                        row
            |]

            // The values as the column holds them (its type, its casing).
            let converted =
                cols
                |> Array.mapi (fun i c -> keep |> Array.map (fun row -> c.ValueOfId(c.FindId data.[i].[row])))

            Tables.ofColumns (cols |> Array.map Tables.modelColumn) converted keep.Length)

        // "follows a chain of one or more many-to-one relationships to fetch the value"; it "needs
        // a row context" (dax.guide/related). A row with no match is on the blank row: BLANK.
        Fn.scalar "RELATED" "Relationship" (1, 1) (Fn.typeOfArg 0) (fun ev ctx call ->
            let c = ev.Column(ctx, call.Args.[0])

            let fromBase =
                ctx.Rows
                |> List.tryPick (fun rc ->
                    let t = rc.Table
                    // The expanded table is fixed when the table is evaluated: "Date[Month] contained
                    // in SalesB is related to Sales[DueDate], not to Sales[Date]" for a table
                    // evaluated under USERELATIONSHIP (SQLBI, "Expanded tables in DAX"). So the row
                    // follows the relationships in force then, not the ones in force now.
                    let config = if isNull t.Config then ctx.Filter.Config else t.Config

                    if not (isNull t.BaseTable) && config.IsExpanded(t.BaseTable, c.Table) then
                        if not t.IsPureBase then
                            Fail.notSupported
                                ctx
                                call.Span
                                $"RELATED in a row context on a table built on '{t.BaseTable.Name}' with columns added"

                        let map = config.RowMap(t.BaseTable, c.Table)
                        Some(c.ValueAt map.[t.BaseRows.[rc.Row]])
                    else
                        None)

            match fromBase with
            | Some v -> v
            | None ->
                if ctx.CallerRows then
                    Fail.notSupported ctx call.Span "RELATED inside a function that was called from a row context"

                Fail.invalid
                    ctx
                    call.Span
                    $"RELATED ( '{c.Table.Name}'[{c.Name}] ) needs a row context on a table with a many-to-one relationship to '{c.Table.Name}'")

        // "a shortcut for CALCULATETABLE function with no additional filters, accepting only a
        // table reference" (dax.guide/relatedtable).
        Fn.table "RELATEDTABLE" "Relationship" (1, 1) (fun ev ctx call ->
            match ev.TryModelTable(ctx, call.Args.[0]) with
            | Some _ -> ev.Table(Context.transition ctx call.Span, call.Args.[0])
            | None -> Fail.invalid ctx call.Args.[0].Span "RELATEDTABLE takes a table name")

        // "If no match satisfies all the search values, the function returns BLANK or alternateResult
        // (if specified)"; "if result_columnName returns different values, the function returns an
        // error or alternateResult (if specified)" (Microsoft Learn, LOOKUPVALUE).
        // dax.guide/lookupvalue gives the expansion this follows: the search values first, then
        // CALCULATE ( DISTINCT ( result ), FILTER ( ALLNOBLANKROW ( search ), search == value ), ...,
        // ALL ( table of result ) ), with its context transition. One distinct value is the
        // result; none is BLANK (the alternate result when given); several are the alternate
        // result, or an error without one.
        Fn.scalar "LOOKUPVALUE" "Relationship" (3, -1) (Fn.typeOfArg 0) (fun ev ctx call ->
            let result = ev.Column(ctx, call.Args.[0])
            let rest = call.Args.Tail

            let pairs, alternate =
                let rec read (items: Expr list) (acc: (Expr * Expr) list) =
                    match items with
                    | c :: v :: tail -> read tail ((c, v) :: acc)
                    | [ alt ] -> List.rev acc, Some alt
                    | _ -> List.rev acc, None

                read rest []

            let searches =
                pairs
                |> List.map (fun (c, v) ->
                    let column = ev.Column(ctx, c)

                    if not (ctx.Filter.Config.IsExpanded(result.Table, column.Table)) then
                        Fail.invalid
                            ctx
                            c.Span
                            $"'{column.Table.Name}'[{column.Name}] is not a column of '{result.Table.Name}' or of a table related to it"

                    column, ev.Scalar(ctx, v), v.Span)

            let filters =
                searches
                |> List.map (fun (column, value, span) ->
                    // `==` between a value and a column it can't be compared with is an error.
                    match column.Data.Values |> Array.tryFind (fun x -> not (DaxValue.isBlank x)) with
                    | Some sample when not (DaxValue.isBlank value) ->
                        Fail.dax ctx span (DaxValue.strictEquals ctx.Model.Collation value sample)
                        |> ignore
                    | _ -> ()

                    let id =
                        match value with
                        // ALLNOBLANKROW: a BLANK matches a stored BLANK only.
                        | DaxValue.Blank -> column.Data.BlankId
                        | v -> column.FindId v

                    if id < 0 then
                        OnColumn(column, RowSet.Empty column.IdCount)
                    else
                        OnColumn(column, RowSet.One(column.IdCount, id)))

            let moved = Context.transition ctx call.Span
            let cfg = moved.Filter.Config

            let cleared =
                Filters.remove moved.Filter (fun c -> cfg.IsExpanded(result.Table, c.Table))

            let fc = Filters.add cleared filters false
            let ids = Filters.idsIn result (Filters.visible fc result.Table) false

            match ids.Length, alternate with
            | 1, _ -> result.ValueOfId ids.[0]
            | 0, None -> DaxValue.Blank
            | _, Some alt -> ev.Scalar(ctx, alt)
            | _, None ->
                Fail.runtime ctx call.Span "A table of multiple values was supplied where a single value was expected.")

        // "Returns the value in the column prior to the specified number of table scans (default
        // is 1)" (dax.guide/earlier).
        Fn.scalar "EARLIER" "Filter" (1, 2) (Fn.typeOfArg 0) (fun ev ctx call ->
            let c = ev.Column(ctx, call.Args.[0])

            let n =
                match List.tryItem 1 call.Args with
                | None -> 1
                | Some a ->
                    match ev.Scalar(ctx, a) with
                    | DaxValue.Integer i when i >= 1L && i < 1000L -> int i
                    | _ -> Fail.invalid ctx a.Span "the second argument of EARLIER is a positive whole number"

            match Context.tryColumnValue ctx c n with
            | ValueSome v -> v
            | ValueNone ->
                if ctx.CallerRows then
                    Fail.notSupported ctx call.Span "EARLIER inside a function that was called from a row context"

                Fail.invalid
                    ctx
                    call.Span
                    $"EARLIER refers to an earlier row context of '{c.Table.Name}'[{c.Name}] which doesn't exist")

        // "EARLIEST retrieves the first ever row context" (dax.guide/earlier).
        Fn.scalar "EARLIEST" "Filter" (1, 1) (Fn.typeOfArg 0) (fun ev ctx call ->
            let c = ev.Column(ctx, call.Args.[0])

            let outermost =
                ctx.Rows
                |> List.rev
                |> List.tryPick (fun rc ->
                    match Tables.columnIndexOfLineage rc.Table c with
                    | -1 -> None
                    | i -> Some(rc.Table.Get(i, rc.Row)))

            match outermost with
            | Some v -> v
            | None -> Fail.invalid ctx call.Span $"EARLIEST: there is no row context of '{c.Table.Name}'[{c.Name}]")
    ]
