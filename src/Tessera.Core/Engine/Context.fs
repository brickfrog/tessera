namespace Tessera.DaxEngine

// Table values, the filter context and the row context: everything a function needs to read or
// change the evaluation context.
//
// The model of a filter context, from SQLBI's "Expanded tables in DAX", "Filtering tables" and
// dax.guide/calculate:
//   - A filter is a set of columns with the combinations of values allowed in them. A row of a
//     table is visible when, for every filter, the row of its EXPANDED table (the table joined to
//     every table it reaches over many-to-one relationships) has an allowed combination in the
//     filter's columns that the expanded table has.
//   - A table used as a filter filters every column of its expanded table ("When filtering
//     tables, DAX always places the filter over the expanded version of the table").
//   - A new filter replaces the old filters on its columns; what an old filter says about its
//     other columns stays, without the pairing with the replaced column ("the Year column of the
//     existing filter is maintained": SQLBI, "Context transition in DAX explained visually").
//   - A relationship that filters both ways is not expansion: the one-side table is restricted
//     to the rows the visible many-side rows refer to ("bidirectional filtering is executed by
//     injecting a direct table filter": SQLBI, "Filtering tables").

open System
open System.Collections.Generic
open Tessera
open Tessera.DaxAst

module Tables =

    /// The name a column has in a query result: `Table[Column]`, or `[Name]` for an extension column.
    let displayName (c: ColInfo) =
        if isNull c.Table then
            $"[{c.Name}]"
        else
            $"{c.Table}[{c.Name}]"

    let modelColumn (c: BColumn) : ColInfo = {
        Name = c.Name
        Table = c.Table.Name
        Lineage = c
        Type = c.DataType
    }

    let extension (name: string) (t: DaxType) : ColInfo = {
        Name = name
        Table = null
        Lineage = null
        Type = t
    }

    /// Rows of a model table (row `RowCount` is the blank row).
    let ofBase (config: RelConfig) (table: BTable) (rows: int array) : TableVal =
        TableVal(
            table.Columns |> Array.map modelColumn,
            rows.Length,
            table,
            rows,
            Array.zeroCreate table.Columns.Length,
            config
        )

    let ofColumns (cols: ColInfo array) (data: DaxValue array array) (count: int) : TableVal =
        TableVal(cols, count, null, null, data, null)

    /// From rows of values.
    let ofRows (cols: ColInfo array) (rows: DaxValue array seq) : TableVal =
        let rows = Array.ofSeq rows

        let data =
            Array.init cols.Length (fun c -> Array.init rows.Length (fun r -> rows.[r].[c]))

        TableVal(cols, rows.Length, null, null, data, null)

    let empty (cols: ColInfo array) : TableVal = ofRows cols []

    /// The rows at these positions, in this order.
    let select (t: TableVal) (positions: int array) : TableVal =
        TableVal(
            t.Cols,
            positions.Length,
            t.BaseTable,
            (if isNull t.BaseRows then
                 null
             else
                 positions |> Array.map (fun p -> t.BaseRows.[p])),
            t.Data
            |> Array.map (fun d ->
                if isNull d then
                    null
                else
                    positions |> Array.map (fun p -> d.[p])),
            t.Config
        )

    /// The same table with columns added.
    let addColumns (t: TableVal) (cols: ColInfo array) (data: DaxValue array array) : TableVal =
        TableVal(Array.append t.Cols cols, t.Count, t.BaseTable, t.BaseRows, Array.append t.Data data, t.Config)

    /// The same rows as plain values: no longer the rows of a model table (lineage stays).
    let materialize (t: TableVal) : TableVal =
        if isNull t.BaseTable then
            t
        else
            TableVal(t.Cols, t.Count, null, null, Array.init t.Cols.Length t.Column, null)

    /// These columns of the table, as plain values.
    let project (t: TableVal) (cols: int array) : TableVal =
        TableVal(cols |> Array.map (fun c -> t.Cols.[c]), t.Count, null, null, cols |> Array.map t.Column, null)

    /// The positions of the first row of every group of equal rows (grouping equality: BLANK is
    /// its own value, text by the collation).
    let distinctPositions (model: Model) (t: TableVal) : int array =
        let seen =
            HashSet<DaxValue array>(ValueArrayComparer(model.Comparer) :> IEqualityComparer<DaxValue array>)

        let keep = ResizeArray<int>()

        for row in 0 .. t.Count - 1 do
            if seen.Add(t.Row row) then
                keep.Add row

        keep.ToArray()

    let distinct (model: Model) (t: TableVal) : TableVal =
        let positions = distinctPositions model t
        if positions.Length = t.Count then t else select t positions

    let columnIndexOfLineage (t: TableVal) (c: BColumn) : int =
        t.Cols
        |> Array.tryFindIndex (fun i -> obj.ReferenceEquals(i.Lineage, c))
        |> Option.defaultValue -1

module Filters =

    let private gate (what: string) : 'a =
        raise (EngineException(DaxNotSupported(what, "")))

    /// Every column of a table's expanded table.
    let expandedColumns (config: RelConfig) (table: BTable) : BColumn array =
        config.Expanded table |> Array.collect (fun (x, _) -> x.Columns)

    /// A set of rows closed under "equal in every column": a filter is on values, not on row
    /// numbers ("if you use context transition in a table that contains duplicated rows, the
    /// filter context generated by CALCULATE includes all the duplicates": SQLBI, "Understanding
    /// context transition").
    let closeRows (table: BTable) (rows: int array) : RowSet =
        let size = table.Size

        let plain () =
            if rows.Length = 1 then
                RowSet.One(size, rows.[0])
            else
                RowSet.OfSeq(size, rows)

        let computing = table.Columns |> Array.filter _.Computing
        let key = table.UniqueColumn

        // The object whose expression is evaluated now. If it is a column of this table, it isn't
        // part of the row yet; any other column of the table being computed is (it is computed
        // because something it needs is), so the transition needs it.
        let innermost =
            if table.Model.Computing.Count > 0 then
                table.Model.Computing.Peek()
            else
                null

        let own (c: BColumn) = obj.ReferenceEquals(c, innermost)

        match key with
        | Some _ when not (computing |> Array.exists _.ChangesRelationships) -> plain ()
        | _ ->
            // Without the key the engine knows (`BTable.UniqueColumn`) the row is identified by
            // all of its columns, so a calculated column that gets here depends on every other
            // column of the table. Two such columns depend on each other: "CALCULATE in a
            // calculated column performs a context transition and makes that column dependent on
            // all the columns in the table. If two such columns exist, they depend on each other"
            // (SQLBI, "Avoiding circular dependency errors in DAX"). That is settled when nothing
            // in the columns narrows the transition's filter and no key could apply; otherwise
            // (ALLEXCEPT keeping the key, a column marked as the key, USERELATIONSHIP with a key)
            // the cycle may not be there, and it is refused.
            // Also unsettled: a key column being computed itself, and a transition from outside
            // the table's own columns (a calculated table, another table's column), whose
            // expression's traits aren't known here.
            let open' =
                key.IsSome
                || table.MarkedKey.IsSome
                || not (computing |> Array.exists own)
                || computing |> Array.exists _.MayNarrow
                || table.Model.Rels
                   |> Array.exists (fun r ->
                       not r.Limited
                       && ((obj.ReferenceEquals(r.To.Table, table) && r.To.Computing)
                           || (r.OneToOne && obj.ReferenceEquals(r.From.Table, table) && r.From.Computing)))

            let names (cols: BColumn seq) =
                cols |> Seq.map (fun c -> $"[{c.Name}]") |> String.concat " and "

            let cycle (cols: BColumn seq) : 'a =
                if open' then
                    let e =
                        DaxNotSupported(
                            $"context transitions in the calculated columns {names cols} of '{table.Name}', which depend on each other through it unless a key or ALLEXCEPT / REMOVEFILTERS keeps the dependency to one column (whether one does here isn't decided)",
                            ""
                        )

                    // Not a circular dependency for sure: the columns are refused, not invalid.
                    for c in cols do
                        if not c.IsReady then
                            c.Failure <- Some e

                    raise (EngineException e)
                else
                    raise (
                        EngineException(
                            DaxInvalid(
                                $"circular dependency: the calculated columns {names cols} of '{table.Name}' each use a context transition, and the table has no key (a column on the one side of a relationship) to identify a row by",
                                ""
                            )
                        )
                    )

            if computing |> Array.exists (fun c -> not (own c)) then
                cycle computing

            for c in table.Columns do
                if not c.IsReady && not c.Computing then
                    try
                        c.Data |> ignore
                    with EngineException(DaxInvalid(message, _)) when
                        message.StartsWith("circular dependency", StringComparison.Ordinal) && open' ->
                        cycle (Seq.append computing [ c ])

            match table.Duplicates with
            | null -> plain ()
            | dups ->
                let n = table.RowCount

                RowSet.OfSeq(
                    size,
                    seq {
                        for r in rows do
                            if r < n && not (isNull dups.[r]) then
                                yield! dups.[r]
                            else
                                yield r
                    }
                )

    /// A table filter written out as a filter on columns: the combinations its rows have in the
    /// columns of its expanded table that `keep` accepts.
    let private spellOut (table: BTable) (rows: RowSet) (config: RelConfig) (keep: BColumn -> bool) : Filter list =
        let cols =
            config.Expanded table
            |> Array.collect (fun (x, _) ->
                x.Columns
                |> Array.filter keep
                |> Array.map (fun c -> c, config.RowMap(table, x)))

        if cols.Length = 0 then
            []
        elif rows.Count = 1 then
            let row = rows.Rows.[0]

            [ for (c, map) in cols -> OnColumn(c, RowSet.One(c.IdCount, c.IdAt map.[row])) ]
        else
            let tuples =
                HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

            for row in rows.Rows do
                tuples.Add(cols |> Array.map (fun (c, map) -> c.IdAt map.[row])) |> ignore

            [ Tuples(cols |> Array.map fst, tuples) ]

    /// The filter without the columns `drop` names (a filter with no column left is gone).
    let private without (drop: BColumn -> bool) (f: Filter) : Filter list =
        match f with
        | OnColumn(c, _) -> if drop c then [] else [ f ]
        | Tuples(cols, tuples) ->
            if not (cols |> Array.exists drop) then
                [ f ]
            else
                // A transition filter spelled out while a calculated column of its table was being
                // computed leaves out the columns not computed yet (the `Rows` case below); that is
                // the same filter while the table's unique column stays in it, or they would go too.
                for c in cols do
                    if drop c then
                        match c.Table.UniqueColumn with
                        | Some u when
                            obj.ReferenceEquals(u, c)
                            && c.Table.Columns |> Array.exists _.Computing
                            && c.Table.Columns
                               |> Array.exists (fun x ->
                                   not x.IsReady
                                   && not (drop x)
                                   && not (cols |> Array.exists (fun y -> obj.ReferenceEquals(x, y))))
                            ->
                            raise (
                                EngineException(
                                    DaxNotSupported(
                                        $"removing the filter on the unique column of '{c.Table.Name}' while one of its calculated columns is being computed",
                                        ""
                                    )
                                )
                            )
                        | _ -> ()

                let keep = [|
                    for i in 0 .. cols.Length - 1 do
                        if not (drop cols.[i]) then
                            i
                |]

                if keep.Length = 0 then
                    []
                else
                    let projected =
                        HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                    for t in tuples do
                        projected.Add(keep |> Array.map (fun i -> t.[i])) |> ignore

                    if keep.Length = 1 then
                        let c = cols.[keep.[0]]
                        [ OnColumn(c, RowSet.OfSeq(c.IdCount, projected |> Seq.map (fun t -> t.[0]))) ]
                    else
                        [ Tuples(keep |> Array.map (fun i -> cols.[i]), projected) ]
        | Rows(table, rows, cfg) ->
            let all = expandedColumns cfg table

            if not (all |> Array.exists drop) then
                [ f ]
            elif all |> Array.forall drop then
                []
            else
                // While a calculated column of the table is being computed (a context transition
                // in its expression), the table's calculated columns not computed yet have no
                // values to spell out, and computing them may need the one being computed. While
                // the table's unique column stays, the rows kept are the same without them, so
                // they are left out. Without the unique column they could change the rows, and
                // nothing says what the engine does.
                let pending (c: BColumn) =
                    not c.IsReady && obj.ReferenceEquals(c.Table, table) && not (drop c)

                if
                    table.Columns |> Array.exists _.Computing
                    && table.Columns |> Array.exists pending
                then
                    match table.UniqueColumn with
                    | Some u when not (drop u) -> spellOut table rows cfg (fun c -> not (drop c) && not (pending c))
                    // Without a key, `closeRows` computed every column but the ones being
                    // computed, which aren't part of the row yet ("In the absence of Test2, Test1
                    // does not reference columns other than the ones already present in the
                    // table": SQLBI, "Understanding circular dependencies").
                    | None when table.Columns |> Array.forall (fun c -> not (pending c) || c.Computing) ->
                        spellOut table rows cfg (fun c -> not (drop c) && not (pending c))
                    | _ ->
                        raise (
                            EngineException(
                                DaxNotSupported(
                                    $"removing the filter on the unique column of '{table.Name}' from a context transition while one of its calculated columns is being computed",
                                    ""
                                )
                            )
                        )
                else
                    spellOut table rows cfg (fun c -> not (drop c))

    /// The filter on the columns `keep` names only (a filter with none of them is gone).
    let project (keep: BColumn -> bool) (f: Filter) : Filter list = without (fun c -> not (keep c)) f

    /// A filter context with these filters under these relationships. A table filter evaluated
    /// under other relationships keeps the columns it had then ("table expansion ... happens when
    /// you define a table": SQLBI, "Expanded tables in DAX"), so it is spelled out by column.
    let buildWith (scope: ScopeMark list) (config: RelConfig) (filters: Filter list) : FilterCtx =
        let filters =
            filters
            |> List.collect (fun f ->
                match f with
                | Rows(table, rows, cfg) when not (obj.ReferenceEquals(cfg, config)) ->
                    // While a calculated column of the table is computed, the columns that aren't
                    // part of the row yet stay out, as in `without`: the one being computed, and
                    // with a key the ones not computed yet.
                    let computing = table.Columns |> Array.exists _.Computing

                    let left (c: BColumn) =
                        computing
                        && obj.ReferenceEquals(c.Table, table)
                        && not c.IsReady
                        && (c.Computing || table.UniqueColumn.IsSome)

                    spellOut table rows cfg (fun c -> not (left c))
                | f -> [ f ])

        FilterCtx(filters, config, scope)

    let build (config: RelConfig) (filters: Filter list) : FilterCtx = buildWith [] config filters

    /// The same filters, with a mark added for ISINSCOPE (see `ScopeMark`).
    let mark (fc: FilterCtx) (m: ScopeMark) : FilterCtx =
        FilterCtx(fc.Filters, fc.Config, m :: fc.Scope)

    /// The columns a filter is on, as a test.
    let private covers (f: Filter) : BColumn -> bool =
        match f with
        | OnColumn(c, _) -> fun x -> obj.ReferenceEquals(x, c)
        | Tuples(cols, _) -> fun x -> Array.exists (fun c -> obj.ReferenceEquals(x, c)) cols
        | Rows(table, _, cfg) -> fun x -> cfg.IsExpanded(table, x.Table)

    /// The context without any filter on the columns `drop` names.
    let remove (fc: FilterCtx) (drop: BColumn -> bool) : FilterCtx =
        buildWith (Unfiltered drop :: fc.Scope) fc.Config (fc.Filters |> List.collect (without drop))

    /// The context with filters added. Unless `keep` (KEEPFILTERS), each new filter first removes
    /// the context's filters on its columns; the new filters do not remove one another.
    let add (fc: FilterCtx) (filters: Filter list) (keep: bool) : FilterCtx =
        if filters.IsEmpty then
            fc
        else
            // A filter on a column a calendar tags clears others first (Calendars.fs).
            let fc = CalendarHooks.beforeAdd fc filters keep

            let old, scope =
                if keep then
                    fc.Filters, fc.Scope
                else
                    let tests = filters |> List.map covers
                    let drop (c: BColumn) = tests |> List.exists (fun t -> t c)
                    fc.Filters |> List.collect (without drop), Unfiltered drop :: fc.Scope

            buildWith scope fc.Config (old @ filters)

    let withConfig (fc: FilterCtx) (config: RelConfig) : FilterCtx =
        if obj.ReferenceEquals(fc.Config, config) then
            fc
        else
            buildWith fc.Scope config fc.Filters

    // ---- Which rows are visible ------------------------------------------------------------------

    /// The rows of a table that have one of these ids in a column.
    let private rowsWithIds (t: BTable) (c: BColumn) (ids: RowSet) (current: RowSet) : RowSet =
        let size = t.Size

        if (not current.IsAll && current.Count <= 256) || ids.Count > 64 then
            current.Where(fun row -> ids.Contains(c.IdAt row))
        else
            let stored = c.Data.DistinctCount
            let data = c.Data

            let rows =
                seq {
                    for id in ids.Rows do
                        if id < stored then
                            yield! data.RowsOfId id

                        if id = c.BlankId && t.HasBlankRow then
                            yield t.RowCount
                }

            current.Intersect(RowSet.OfSeq(size, rows))

    let rec private vis (fc: FilterCtx) (t: BTable) (via: BRel) : RowSet =
        let key = t.Id * (t.Model.Rels.Length + 1) + (if isNull via then 0 else via.Id + 1)

        match fc.Visible.TryGetValue key with
        | true, set -> set
        | _ ->
            if not (fc.InProgress.Add key) then
                gate $"filters that reach table '{t.Name}' again over relationships that filter both ways"

            let set = compute fc t via
            fc.InProgress.Remove key |> ignore
            fc.Visible.[key] <- set
            set

    and private compute (fc: FilterCtx) (t: BTable) (via: BRel) : RowSet =
        let cfg = fc.Config
        let size = t.Size
        let mutable set = RowSet.All size

        // Tables that are only reached through the relationship we came over: their filters are
        // the caller's business.
        let beyondVia (x: BTable) =
            not (isNull via)
            && obj.ReferenceEquals(via.From.Table, t)
            && cfg.IsExpanded(via.To.Table, x)

        // Filters on this table's own columns first, then (below) the tables it relates to, and
        // only then the filters that pair columns of several tables: those test row by row, so
        // they come last, on the rows that are left.
        let mutable paired = false

        let applyFilters () =
            for f in fc.Filters do
                match f with
                | OnColumn(c, ids) ->
                    if not paired && obj.ReferenceEquals(c.Table, t) then
                        set <- rowsWithIds t c ids set
                | Tuples(cols, tuples) ->
                    let here = [|
                        for i in 0 .. cols.Length - 1 do
                            if cfg.IsExpanded(t, cols.[i].Table) then
                                i
                    |]

                    if here.Length > 0 then
                        let tables = here |> Array.map (fun i -> cols.[i].Table) |> Array.distinctBy _.Id

                        let apply =
                            if tables.Length = 1 then
                                // One table's columns: that table applies them, and its rows filter ours.
                                obj.ReferenceEquals(tables.[0], t)
                            else
                                not (tables |> Array.forall beyondVia)

                        if apply && paired = (tables.Length > 1) then
                            let projected =
                                if here.Length = cols.Length then
                                    tuples
                                else
                                    let p =
                                        HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                                    for tuple in tuples do
                                        p.Add(here |> Array.map (fun i -> tuple.[i])) |> ignore

                                    p

                            let columns = here |> Array.map (fun i -> cols.[i])

                            let maps =
                                columns
                                |> Array.map (fun c ->
                                    if obj.ReferenceEquals(c.Table, t) then
                                        null
                                    else
                                        cfg.RowMap(t, c.Table))

                            let buffer = Array.zeroCreate<int> columns.Length

                            set <-
                                set.Where(fun row ->
                                    for i in 0 .. columns.Length - 1 do
                                        let r = if isNull maps.[i] then row else maps.[i].[row]
                                        buffer.[i] <- columns.[i].IdAt r

                                    projected.Contains buffer)
                | Rows(x, rows, _) ->
                    if obj.ReferenceEquals(x, t) then
                        if not paired then
                            set <- set.Intersect rows
                    elif cfg.IsExpanded(t, x) then
                        // x filters us through the relationships below.
                        ()
                    elif cfg.IsExpanded(x, t) then
                        // We are part of x's expanded table: our rows that its rows refer to.
                        let map = cfg.RowMap(x, t)

                        let image =
                            RowSet.OfSeq(
                                size,
                                rows.Rows |> Seq.map (fun r -> map.[r]) |> Seq.filter (fun r -> r < size)
                            )

                        if not paired then
                            set <- set.Intersect image
                    elif paired then
                        // What the two expanded tables share. If that hangs off one table, that table
                        // carries the filter to us; otherwise the combinations matter.
                        let shared =
                            cfg.Expanded t |> Array.map fst |> Array.filter (fun y -> cfg.IsExpanded(x, y))

                        let top =
                            shared
                            |> Array.filter (fun y ->
                                not (
                                    shared
                                    |> Array.exists (fun z -> not (obj.ReferenceEquals(z, y)) && cfg.IsExpanded(z, y))
                                ))

                        if top.Length > 1 && not (top |> Array.forall beyondVia) then
                            let fromX = top |> Array.map (fun y -> cfg.RowMap(x, y))
                            let fromT = top |> Array.map (fun y -> cfg.RowMap(t, y))

                            let combos =
                                HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                            for r in rows.Rows do
                                combos.Add(fromX |> Array.map (fun m -> m.[r])) |> ignore

                            let buffer = Array.zeroCreate<int> top.Length

                            set <-
                                set.Where(fun row ->
                                    for i in 0 .. top.Length - 1 do
                                        buffer.[i] <- fromT.[i].[row]

                                    combos.Contains buffer)

        applyFilters ()

        // The one-side tables we reach filter us.
        for r in cfg.ManyToOne t do
            // A calculated table's expression may read the tables related to it, as a date table
            // usually does ("CALENDAR (MINX (Sales, [Date]), MAXX (Forecast, [Date]))": Microsoft
            // Learn, CALENDAR). While it is being computed, and nothing at all is filtered, it can't
            // restrict anything, and asking it for its rows would be a circular dependency that
            // isn't one. Under any filter the table is asked as before, and that is circular: what
            // a filter would do through a table that doesn't exist yet can't be told.
            if
                not (obj.ReferenceEquals(r, via))
                && not r.Limited
                && not (r.To.Table.Computing && fc.Filters.IsEmpty)
            then
                let d = r.To.Table
                let dv = vis fc d r

                if not dv.IsAll then
                    let map = r.ToRowOfFromRow
                    let stored = t.RowCount
                    let dBlank = d.RowCount

                    let related (row: int) =
                        if row >= stored then dBlank else map.[row]

                    if (not set.IsAll && set.Count <= 256) || dv.Count > 64 then
                        set <- set.Where(fun row -> dv.Contains(related row))
                    else
                        let rows =
                            seq {
                                for dRow in dv.Rows do
                                    yield! r.FromRowsOfToRow dRow

                                    if dRow = dBlank && t.HasBlankRow then
                                        yield stored
                            }

                        set <- set.Intersect(RowSet.OfSeq(size, rows))

        paired <- true
        applyFilters ()

        // A many-side table that filters both ways restricts us to the rows it refers to.
        for r in cfg.OneToMany t do
            if not (obj.ReferenceEquals(r, via)) && not r.Limited && cfg.FiltersBothWays r then
                let m = r.From.Table
                let mv = vis fc m r

                if not mv.IsAll then
                    let map = r.ToRowOfFromRow
                    let stored = m.RowCount

                    let referred =
                        RowSet.OfSeq(
                            size,
                            mv.Rows
                            |> Seq.map (fun row -> if row >= stored then t.RowCount else map.[row])
                            |> Seq.filter (fun row -> row < size)
                        )

                    if mv.Count = m.Size && referred.Count < size then
                        // No source says whether a filter that keeps every row still hides the
                        // one-side rows nothing refers to.
                        gate
                            $"a filter on '{m.Name}' that keeps all of its rows, crossing the both-directions relationship to '{t.Name}'"

                    set <- set.Intersect referred

        // A limited relationship filters by values: "Table joins are achieved by using INNER JOIN
        // semantics" (Microsoft Learn, "Model relationships in Power BI Desktop"). With nothing
        // filtering the other side nothing is filtered here, so a total includes the rows that
        // match nothing ("the total shown for Weak Amount is different than the sum of individual
        // brands": SQLBI, "Strong and weak relationships in Power BI").
        let limited (r: BRel) (source: BColumn) (target: BColumn) =
            let s = source.Table
            let sv = vis fc s r

            if not sv.IsAll then
                if sv.Count = s.Size then
                    // As for a relationship that filters both ways: no source says whether a filter
                    // that keeps every row still hides the rows that match nothing.
                    gate
                        $"a filter on '{s.Name}' that keeps all of its rows, crossing the many-to-many relationship {r.Describe}"

                let stored = s.RowCount
                let data = source.Data
                let marks = Array.zeroCreate<bool>(data.DistinctCount + 1)
                let mutable blank = false

                for row in sv.Rows do
                    if row >= stored then
                        blank <- true
                    else
                        let id = data.ValueIds.[row]

                        if id = data.BlankId then
                            blank <- true
                        else
                            marks.[id] <- true

                if blank && (target.Data.BlankId >= 0 || t.HasBlankRow) then
                    gate
                        $"a BLANK of '{s.Name}'[{source.Name}] crossing the many-to-many relationship {r.Describe} to the BLANK of '{t.Name}'[{target.Name}] (whether they match isn't documented)"

                let ids =
                    r.MatchIds(
                        source,
                        seq {
                            for id in 0 .. marks.Length - 1 do
                                if marks.[id] then
                                    id
                        }
                    )

                set <- rowsWithIds t target (RowSet.OfSeq(target.IdCount, ids)) set

        for r in cfg.ManyToOne t do
            if r.Limited && not (obj.ReferenceEquals(r, via)) then
                limited r r.To r.From

        for r in cfg.OneToMany t do
            if r.Limited && not (obj.ReferenceEquals(r, via)) && cfg.Both.[r.Id] then
                limited r r.From r.To

        set

    /// The rows of a table the filter context lets through, the blank row (row `RowCount`)
    /// included when it has one. `IsAll` when nothing filters the table.
    let visible (fc: FilterCtx) (t: BTable) : RowSet = vis fc t null

    /// The visible stored rows: what a table reference is ("when you reference a table the blank
    /// row is never returned": SQLBI, "Blank row in DAX").
    let visibleStored (fc: FilterCtx) (t: BTable) : int array =
        let v = visible fc t
        let n = t.RowCount

        if v.IsAll then
            Array.init n id
        else
            let rows = v.Rows

            if rows.Length > 0 && rows.[rows.Length - 1] >= n then
                Array.sub rows 0 (rows.Length - 1)
            else
                rows

    /// The rows of a table under its own filters only (those on its columns), without what other
    /// tables' filters do to it over relationships. What SUMMARIZECOLUMNS groups by.
    let ownRows (fc: FilterCtx) (t: BTable) : RowSet =
        let own =
            fc.Filters
            |> List.collect (fun f ->
                match f with
                | OnColumn(c, _) when obj.ReferenceEquals(c.Table, t) -> [ f ]
                | OnColumn _ -> []
                | Tuples(cols, tuples) ->
                    let here = [|
                        for i in 0 .. cols.Length - 1 do
                            if obj.ReferenceEquals(cols.[i].Table, t) then
                                i
                    |]

                    if here.Length = 0 then
                        []
                    else
                        let p =
                            HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                        for tuple in tuples do
                            p.Add(here |> Array.map (fun i -> tuple.[i])) |> ignore

                        [ Tuples(here |> Array.map (fun i -> cols.[i]), p) ]
                | Rows(x, rows, cfg) ->
                    if obj.ReferenceEquals(x, t) then
                        [ f ]
                    elif cfg.IsExpanded(x, t) then
                        let map = cfg.RowMap(x, t)
                        let size = t.Size

                        [
                            Rows(
                                t,
                                RowSet.OfSeq(
                                    size,
                                    rows.Rows |> Seq.map (fun r -> map.[r]) |> Seq.filter (fun r -> r < size)
                                ),
                                cfg
                            )
                        ]
                    else
                        [])

        // Under a configuration with no relationships, only the table's own filters act.
        let none =
            t.Model.Config(Array.zeroCreate t.Model.Rels.Length, Array.zeroCreate t.Model.Rels.Length)

        let own =
            own
            |> List.map (fun f ->
                match f with
                | Rows(x, rows, _) -> Rows(x, rows, none)
                | f -> f)

        visible (FilterCtx(own, none)) t

    /// The ids a column has in a set of rows of its table, ascending. `blankRow`: count the blank
    /// row's BLANK (VALUES does, DISTINCT doesn't).
    let idsIn (c: BColumn) (rows: RowSet) (blankRow: bool) : int array =
        let t = c.Table
        let stored = c.Data.DistinctCount
        let n = t.RowCount
        let marks = Array.zeroCreate<bool>(stored + 1)

        if rows.IsAll then
            for id in 0 .. stored - 1 do
                marks.[id] <- true

            if blankRow && t.HasBlankRow then
                marks.[c.BlankId] <- true
        else
            let ids = c.Data.ValueIds

            for row in rows.Rows do
                if row < n then
                    marks.[ids.[row]] <- true
                elif blankRow then
                    marks.[c.BlankId] <- true

        [|
            for id in 0..stored do
                if marks.[id] then
                    id
        |]

    /// The filters a table value is when used as a filter: its columns with lineage, with the
    /// combinations of values its rows have. The rows of a model table filter its expanded table.
    let ofTable (ctx: Ctx) (span: Span) (t: TableVal) : Filter list =
        if t.IsPureBase then
            [ Rows(t.BaseTable, closeRows t.BaseTable t.BaseRows, t.Config) ]
        else
            if not (isNull t.BaseTable) && (t.Config.Expanded t.BaseTable).Length > 1 then
                Fail.notSupported
                    ctx
                    span
                    $"a table built on '{t.BaseTable.Name}' with columns added, used as a filter (whether it still filters the related tables is not documented)"

            let cols = [|
                for i in 0 .. t.Cols.Length - 1 do
                    if not (isNull t.Cols.[i].Lineage) then
                        i
            |]

            let lineage = cols |> Array.map (fun i -> t.Cols.[i].Lineage)

            if (lineage |> Array.distinctBy _.Id).Length <> lineage.Length then
                Fail.notSupported ctx span "a filter table with two columns of the same model column"

            if cols.Length = 0 then
                []
            elif cols.Length = 1 then
                let c = lineage.[0]
                let values = t.Column cols.[0]

                [
                    OnColumn(c, RowSet.OfSeq(c.IdCount, values |> Seq.map c.FindId |> Seq.filter (fun id -> id >= 0)))
                ]
            else
                let tuples =
                    HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                let data = cols |> Array.map t.Column

                for row in 0 .. t.Count - 1 do
                    let tuple = Array.init cols.Length (fun i -> lineage.[i].FindId data.[i].[row])

                    if tuple |> Array.forall (fun id -> id >= 0) then
                        tuples.Add tuple |> ignore

                [ Tuples(lineage, tuples) ]

/// The date-table rule: a filter that CALCULATE puts on the date column of a date table also
/// removes the filters on every other column of that table.
///
///   - "When you apply the Mark as Date Table setting to a table, the DAX engine automatically adds
///     an ALL function over the same table in each CALCULATE statement where a filter [is applied]
///     over the column used in that setting"; "this ALL statement is automatically applied when you
///     apply a filter over a column of Date type that is the primary key in a relationship,
///     regardless of the presence of the Mark as Date Table setting" (SQLBI, "Time Intelligence in
///     Power BI Desktop").
///   - "the DAX engine makes an assumption when two tables are related through a column of Date
///     data type: When a filter is applied on the key of the relationship ... the new filter
///     overrides any other filter on the Date table. It basically applies a REMOVEFILTERS ( Date )
///     ... This behavior occurs automatically only when the relationship is based on a column of
///     Date data type"; "the additional REMOVEFILTERS is applied only when you apply a filter on the
///     Date[Date] column. If you move the filter to any other column, then REMOVEFILTERS is not
///     applied" (SQLBI, "Mark as date table").
///
/// What the sources settle is a filter argument of CALCULATE on that one column. Where they are
/// silent (a context transition that filters the date column, KEEPFILTERS, a filter that pairs the
/// date column with others, a key that only an inactive relationship uses) the engine refuses,
/// but only when the rule could change the result: when no other column of the table is filtered,
/// both readings give the same context (and, for a context transition, see `onTransition`).
module DateTable =

    type Standing =
        | No
        | Sure
        /// It may be one: why that isn't settled.
        | Unsure of string

    /// Whether filtering this column is filtering the date column of a date table.
    let standing (config: RelConfig) (c: BColumn) : Standing =
        if c.DataType <> DaxType.DateTime then
            No
        elif c.DateKey = 2 then
            Sure
        else
            let keyOf =
                c.Table.Model.Rels
                |> Array.filter (fun r -> obj.ReferenceEquals(r.To, c) && not r.Limited)

            if
                keyOf
                |> Array.exists (fun r -> config.Active.[r.Id] && r.From.DataType = DaxType.DateTime)
            then
                Sure
            elif keyOf.Length > 0 then
                Unsure "is the key of a relationship that is not active, or not from a date column"
            elif
                c.Table.Model.Rels
                |> Array.exists (fun r -> r.OneToOne && obj.ReferenceEquals(r.From, c))
            then
                Unsure "is the key of a one-to-one relationship"
            elif c.DateKey = 1 then
                Unsure "is the key of a table that is not marked as a date table"
            else
                No

    /// The columns a filter is on.
    let private covers (f: Filter) : BColumn -> bool =
        match f with
        | OnColumn(c, _) -> fun x -> obj.ReferenceEquals(x, c)
        | Tuples(cols, _) -> fun x -> Array.exists (fun c -> obj.ReferenceEquals(x, c)) cols
        | Rows(table, _, cfg) -> fun x -> cfg.IsExpanded(table, x.Table)

    /// Whether the context filters a column of the table (or of a table it expands to) that
    /// `replaced` doesn't name: what an ALL over the table would remove beyond those.
    let private filtersOthers (fc: FilterCtx) (table: BTable) (replaced: BColumn -> bool) : bool =
        let inTable (x: BColumn) =
            fc.Config.IsExpanded(table, x.Table) && not (replaced x)

        fc.Filters
        |> List.exists (fun f ->
            match f with
            | OnColumn(x, _) -> inTable x
            | Tuples(cols, _) -> cols |> Array.exists inTable
            | Rows(t, _, cfg) -> Filters.expandedColumns cfg t |> Array.exists inTable)

    let private describe (c: BColumn) = $"'{c.Table.Name}'[{c.Name}]"

    let private unsettled (ctx: Ctx) (span: Span) (c: BColumn) (what: string) : 'a =
        Fail.notSupported
            ctx
            span
            $"{what} while other columns of '{c.Table.Name}' are filtered (a filter argument of CALCULATE on the date column of a date table removes those filters; whether this does too is not documented)"

    /// The context CALCULATE's filter arguments are added to: `fc` without the filters on the date
    /// tables whose date column one of them filters. `filters`: each argument's filters, and
    /// whether it is under KEEPFILTERS. The arguments themselves are not touched: a filter on
    /// another column of the date table in the same CALCULATE stays, as with the ALL ( Calendar )
    /// SQLBI writes out next to DATESYTD.
    let onFilterArguments (ctx: Ctx) (span: Span) (fc: FilterCtx) (filters: (Filter list * bool) list) : FilterCtx =
        let cfg = fc.Config
        let cleared = ResizeArray<BTable>()

        let replacedBy (keep: bool) (f: Filter) : BColumn -> bool =
            if keep then (fun _ -> false) else covers f

        for (fs, keep) in filters do
            for f in fs do
                match f with
                | OnColumn(c, _) ->
                    match standing cfg c with
                    | No -> ()
                    | Sure when not keep ->
                        if not (cleared.Exists(fun t -> obj.ReferenceEquals(t, c.Table))) then
                            cleared.Add c.Table
                    | Sure ->
                        if filtersOthers fc c.Table (replacedBy keep f) then
                            unsettled ctx span c $"KEEPFILTERS on a filter of the date column {describe c}"
                    | Unsure why ->
                        if filtersOthers fc c.Table (replacedBy keep f) then
                            unsettled ctx span c $"a filter on {describe c}, which {why},"
                | Tuples(cols, _) ->
                    for c in cols do
                        if standing cfg c <> No && filtersOthers fc c.Table (replacedBy keep f) then
                            unsettled ctx span c $"a filter on the date column {describe c} together with other columns"
                | Rows _ ->
                    // A table filter replaces the filters on every column of its expanded table.
                    ()

        let mutable result = fc

        for t in cleared do
            result <- Filters.remove result (fun x -> cfg.IsExpanded(t, x.Table))

        result

    /// A context transition is about to add these filters (one row context's). One on the date
    /// column of a date table is refused when the rule could change anything. It can't when every
    /// filter the rule would remove is on the date table's own columns alone, and the row's date is
    /// visible once the filters are added without the rule: the date column is a key, so the table
    /// is then that one row either way (the other filters on it all keep that row), and nothing
    /// else was filtered by what the rule removes. That is grouping by Date[Date] under a filter
    /// on the year. A filter that also restricts another table (a table the date table expands to,
    /// a filter pairing a Date column with another table's) is still refused.
    let onTransition (ctx: Ctx) (span: Span) (fc: FilterCtx) (filters: Filter list) : unit =
        let tests = filters |> List.map covers
        let replaced (x: BColumn) = tests |> List.exists (fun t -> t x)
        let after = lazy (Filters.add fc filters false)

        // Every filter on the expanded date table is on that table's own columns only.
        let confined (t: BTable) =
            let cfg = fc.Config
            let own (x: BColumn) = obj.ReferenceEquals(x.Table, t)
            let touches (x: BColumn) = cfg.IsExpanded(t, x.Table)

            fc.Filters
            |> List.forall (fun f ->
                match f with
                | OnColumn(x, _) -> not (touches x) || own x
                | Tuples(cols, _) -> not (Array.exists touches cols) || Array.forall own cols
                | Rows(r, _, cfg) ->
                    let cols = Filters.expandedColumns cfg r
                    not (Array.exists touches cols) || Array.forall own cols)

        let keepsRow (c: BColumn) (values: RowSet) =
            values.Count = 1
            && confined c.Table
            && (let id = values.Rows.[0]

                Filters.idsIn c (Filters.visible after.Value c.Table) false |> Array.contains id)

        let check (c: BColumn) (values: RowSet option) =
            if
                standing fc.Config c <> No
                && filtersOthers fc c.Table replaced
                && not (values |> Option.exists (keepsRow c))
            then
                unsettled ctx span c $"a context transition on the date column {describe c}"

        for f in filters do
            match f with
            | OnColumn(c, values) -> check c (Some values)
            | Tuples(cols, _) -> cols |> Array.iter (fun c -> check c None)
            | Rows _ -> ()

module Context =

    let root (model: Model) (query: QueryDefs) (source: SourceText) (inQuery: bool) : Ctx = {
        Model = model
        Query = query
        Filter = FilterCtx([], model.DefaultConfig)
        Rows = []
        Scope = {
            Vars = Map.empty
            Source = source
            InQuery = inQuery
        }
        Measures = []
        Functions = []
        Group = null
        CallerRows = false
        Identity = None
        Items = ItemCtx.none
    }

    /// A row context on top of the others.
    let pushRow (ctx: Ctx) (table: TableVal) (row: int) : Ctx = {
        ctx with
            Rows = { Table = table; Row = row } :: ctx.Rows
    }

    let withFilter (ctx: Ctx) (fc: FilterCtx) : Ctx = { ctx with Filter = fc }

    /// The context with filters added to its filter context (`keep`: as KEEPFILTERS does).
    let withFilters (ctx: Ctx) (filters: Filter list) (keep: bool) : Ctx = {
        ctx with
            Filter = Filters.add ctx.Filter filters keep
    }

    /// The filters one row context becomes: every column being iterated, at the row's value.
    let rowFilters (ctx: Ctx) (span: Span) (rc: RowCtx) : Filter list =
        let t = rc.Table

        if t.IsPureBase then
            // "all the columns of the expanded table will be part of the newly-created filter
            // context" (SQLBI, "Understanding context transition in DAX").
            [
                Rows(t.BaseTable, Filters.closeRows t.BaseTable [| t.BaseRows.[rc.Row] |], t.Config)
            ]
        else
            if not (isNull t.BaseTable) && (t.Config.Expanded t.BaseTable).Length > 1 then
                Fail.notSupported
                    ctx
                    span
                    $"context transition over a table built on '{t.BaseTable.Name}' with columns added (whether the related tables are filtered is not documented)"

            [
                for i in 0 .. t.Cols.Length - 1 do
                    let c = t.Cols.[i].Lineage

                    if not (isNull c) then
                        match c.FindId(t.Get(i, rc.Row)) with
                        | -1 -> OnColumn(c, RowSet.Empty c.IdCount)
                        | id -> OnColumn(c, RowSet.One(c.IdCount, id))
            ]

    /// Context transition: every row context becomes filters, outermost first, each replacing the
    /// filters on its columns; no row context is left (dax.guide/calculate, steps 2 and 3).
    let transition (ctx: Ctx) (span: Span) : Ctx =
        if ctx.CallerRows then
            Fail.notSupported
                ctx
                span
                "a context transition (CALCULATE, a measure) inside a function that was called from a row context (whether the function sees the caller's row context is not documented)"

        match ctx.Rows with
        | [] -> ctx
        | rows ->
            let mutable fc = ctx.Filter

            for rc in List.rev rows do
                // "Being an iterator, SUMX generates a shadow filter context that contains the
                // Sales table" (SQLBI, "The definitive guide to ALLSELECTED"): it stays when the
                // row context becomes filters.
                fc <- Filters.mark fc (Shadow rc.Table)
                let filters = rowFilters ctx span rc
                DateTable.onTransition ctx span fc filters
                fc <- Filters.add fc filters false
                let t = rc.Table

                let scope =
                    if t.IsPureBase then
                        let b = t.BaseTable
                        let cfg = t.Config

                        Transitioned(
                            b.Columns,
                            fun c -> not (obj.ReferenceEquals(c.Table, b)) && cfg.IsExpanded(b, c.Table)
                        )
                    else
                        Transitioned(
                            t.Cols |> Array.map _.Lineage |> Array.filter (fun c -> not (isNull c)),
                            fun _ -> false
                        )

                fc <- Filters.mark fc scope

            { ctx with Filter = fc; Rows = [] }

    /// The value a row context has for a model column: the innermost row context that has it,
    /// skipping `skip` of them (EARLIER).
    let tryColumnValue (ctx: Ctx) (c: BColumn) (skip: int) : DaxValue voption =
        let mutable left = skip
        let mutable result = ValueNone
        let mutable rows = ctx.Rows

        while result.IsNone && not rows.IsEmpty do
            let rc = rows.Head
            let i = Tables.columnIndexOfLineage rc.Table c

            if i >= 0 then
                if left = 0 then
                    result <- ValueSome(rc.Table.Get(i, rc.Row))
                else
                    left <- left - 1

            rows <- rows.Tail

        result

/// Row contexts that are not an iterator's, and the rows of SUMMARIZECOLUMNS, for ALLSELECTED: an
/// iterator's table is a shadow filter context (`ScopeMark.Shadow`); a row context that no source
/// describes as an iteration (a calculated column's, the rows ORDER BY sorts) is marked here, and
/// ALLSELECTED refuses under one.
module Shadows =

    let private notIterations =
        System.Runtime.CompilerServices.ConditionalWeakTable<TableVal, string>()

    /// The same table, as a value whose row contexts are not an iteration (`what` says whose they are).
    let notAnIteration (what: string) (t: TableVal) : TableVal =
        let copy = TableVal(t.Cols, t.Count, t.BaseTable, t.BaseRows, t.Data, t.Config)
        notIterations.AddOrUpdate(copy, what)
        copy

    /// Why a row context on this table is not an iteration, if it isn't.
    let whyNot (t: TableVal) : string option =
        match notIterations.TryGetValue t with
        | true, what -> Some what
        | _ -> None

    /// SUMMARIZE's extension columns are evaluated in a row context on each group. When the table
    /// summarized has no columns but the group-by columns, the groups are its rows, and Microsoft
    /// Learn's ALLSELECTED example (SUMMARIZE over a CROSSJOIN of the group-by columns) shows them
    /// restored as a shadow filter context. Over a wider table, whether the shadow holds the table
    /// or the groups is not documented.
    let summarized (table: TableVal) (groups: TableVal) : TableVal =
        let lineage (t: TableVal) =
            t.Cols |> Array.map (fun c -> c.Lineage)

        let same =
            lineage table |> Array.forall (fun c -> not (isNull c))
            && lineage groups |> Array.forall (fun c -> not (isNull c))
            && (lineage table |> Array.distinctBy _.Id |> Array.map _.Id |> Array.sort) =
                (lineage groups |> Array.map _.Id |> Array.sort)

        // The rows of a model table with related tables: its shadow would reach them, the groups'
        // wouldn't, so which one applies matters and is not documented.
        if
            same
            && not (table.IsPureBase && (table.Config.Expanded table.BaseTable).Length > 1)
        then
            groups
        else
            notAnIteration
                "a SUMMARIZE extension column over a table with columns that are not group-by columns (whether its shadow filter context holds the table or the groups is not documented)"
                groups

    /// A SUMMARIZECOLUMNS row: `ctx` is the row's context, `before` what the groups were computed
    /// under, `keys` the filters that hold the row's group.
    let inGroup (ctx: Ctx) (before: FilterCtx) (groups: BColumn array) (keys: Filter list) : Ctx =
        let fc = ctx.Filter

        let m = {
            Before = before.Filters
            Row = fc.Filters
            Groups = groups
            Keys = keys
        }

        Context.withFilter ctx (Filters.mark fc (Selection m))
