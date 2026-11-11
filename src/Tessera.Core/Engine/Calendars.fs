namespace Tessera.DaxEngine

// Calendar-based time intelligence: the rules that hold wherever a model has calendars, and what
// the time intelligence functions (Functions/TimeIntelligence.fs) read from a calendar.
//
// Context clearing. "When context is set on a column or its associated sort by column that is
// tagged in a calendar, prior filter context is cleared on: All category dependencies of X ... All
// category dependents of both X and its dependencies"; "Context clearing happens on columns tagged
// in a calendar or associated sort-by columns regardless if the context is set using time
// intelligence functions or otherwise"; "If there are multiple calendars defined on the same table,
// these processes are completed for every calendar defined on the table" (Microsoft Learn,
// "Implement time-based calculations in Power BI", Context clearing). So a filter argument of
// CALCULATE (and CALCULATETABLE) first removes the filters on the columns, of any calendar of the
// table, in the categories `Category.cleared` gives (`onFilterArguments`). Every other way a filter
// is set clears nothing, and is refused where clearing would change the visible rows (`beforeAdd`,
// `onGroup`), because no source says whether it clears:
//
//   - a context transition (the date-table rule, the classic counterpart, is refused there too),
//     KEEPFILTERS, the groups and filter arguments of SUMMARIZECOLUMNS, the filters FIRSTNONBLANK,
//     LOOKUPVALUE and ALLSELECTED set;
//   - the result of a calendar function (a table of a calendar's columns) while time-related
//     columns of ANOTHER calendar of the table are filtered: "Most time intelligence functions,
//     except for DATEADD and SAMEPERIODLASTYEAR, will clear context on all time-related columns",
//     across calendars, but the same table written out by hand need not;
//   - a filter on a table whose calendars use a category no page describes (the semesters), or
//     give one column two categories.
//
// A plain filter on a time-related column clears nothing: Learn's rule is about categories.
//
// The date-table rule (`DateTable` in Context.fs) does not apply to a calendar function's result:
// "With a calendar, CALCULATE removes only the filters on the columns of the calendar. The filters
// on the other columns of the date table stay ... With the date column of a table marked as a date
// table, CALCULATE removes all the filters on the date table" (dax.guide/dateswtd and the other
// functions).

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Tessera
open Tessera.DaxAst

module Calendars =

    let private refuse (what: string) : 'a =
        raise (EngineException(DaxNotSupported(what, "")))

    let describe (c: BColumn) = $"'{c.Table.Name}'[{c.Name}]"

    let private has (cols: BColumn array) (c: BColumn) =
        cols |> Array.exists (fun x -> obj.ReferenceEquals(x, c))

    /// The columns a filter puts a new filter on. A table filter replaces the filters on every
    /// column of its expanded table, so nothing of a table it reaches is left to clear.
    let private setColumns (f: Filter) : BColumn list =
        match f with
        | OnColumn(c, _) -> [ c ]
        | Tuples(cols, _) -> List.ofArray cols
        | Rows _ -> []

    let private covers (f: Filter) (x: BColumn) =
        match f with
        | OnColumn(c, _) -> obj.ReferenceEquals(x, c)
        | Tuples(cols, _) -> has cols x
        | Rows(t, _, cfg) -> cfg.IsExpanded(t, x.Table)

    let private touches (f: Filter) (drop: BColumn -> bool) =
        match f with
        | OnColumn(c, _) -> drop c
        | Tuples(cols, _) -> Array.exists drop cols
        | Rows(t, _, cfg) -> Filters.expandedColumns cfg t |> Array.exists drop

    /// The calendar whose functions return a table of exactly this filter's columns.
    let shapeOf (set: CalendarSet) (f: Filter) : BCalendar option =
        match f with
        | Tuples(cols, _) when cols.Length > 0 ->
            match set.ByTable.TryGetValue cols.[0].Table with
            | true, cals ->
                cals
                |> List.tryFind (fun cal ->
                    cal.ResultColumns.Length = cols.Length
                    && cal.ResultColumns |> Array.forall (has cols))
            | _ -> None
        | _ -> None

    /// Whether removing the filters on the columns `drop` names, before adding `filters`, can
    /// change what is visible. It can't when no filter is on those columns, or when every such
    /// filter is on one table's own columns and that table shows the same rows either way (the
    /// filters it would remove keep every row the new ones leave).
    let private matters (fc: FilterCtx) (filters: Filter list) (keep: bool) (drop: BColumn -> bool) : bool =
        let affected = fc.Filters |> List.filter (fun f -> touches f drop)

        if affected.IsEmpty then
            false
        else
            let cfg = fc.Config

            let ownTable (f: Filter) : BTable option =
                let one (cols: BColumn array) (t: BTable) =
                    if cols |> Array.forall (fun c -> obj.ReferenceEquals(c.Table, t)) then
                        Some t
                    else
                        None

                match f with
                | OnColumn(c, _) -> Some c.Table
                | Tuples(cols, _) -> one cols cols.[0].Table
                | Rows(t, _, cfg) -> one (Filters.expandedColumns cfg t) t

            let tables = affected |> List.map ownTable

            if tables |> List.exists Option.isNone then
                true
            else
                let replaced (x: BColumn) =
                    not keep && filters |> List.exists (fun f -> covers f x)

                let before =
                    fc.Filters |> List.collect (Filters.project (fun x -> not (replaced x)))

                let kept = Filters.build cfg (before @ filters)

                let cleared =
                    Filters.build cfg ((before |> List.collect (Filters.project (fun x -> not (drop x)))) @ filters)

                tables
                |> List.choose id
                |> List.distinct
                |> List.exists (fun t -> (Filters.visible kept t).Rows <> (Filters.visible cleared t).Rows)

    /// What adding these filters (as KEEPFILTERS does, or replacing the filters on their columns)
    /// would clear under the calendar rule: the columns to clear, and the time-related columns of
    /// other calendars a calendar's result would also clear (`check`).
    let private clearing
        (set: CalendarSet)
        (filters: Filter list)
        (keep: bool)
        : (BColumn -> bool) * (BColumn -> bool) =
        let tagged =
            filters
            |> List.collect setColumns
            |> List.filter (fun c -> set.CategoryOf.ContainsKey c || set.TableProblem.ContainsKey c.Table)

        for c in tagged do
            match set.TableProblem.TryGetValue c.Table with
            | true, why -> refuse $"a filter on {describe c}, a column of a calendar's table: {why}"
            | _ -> ()

        let drop = HashSet<BColumn>(HashIdentity.Reference)
        let check = HashSet<BColumn>(HashIdentity.Reference)

        for (t, cols) in tagged |> List.groupBy (fun c -> c.Table) do
            let categories =
                cols
                |> List.fold (fun acc c -> Set.union acc (Category.cleared set.CategoryOf.[c])) Set.empty

            for c in set.ColumnsIn(t, categories) do
                drop.Add c |> ignore

            for f in filters do
                match shapeOf set f with
                | Some cal when obj.ReferenceEquals(cal.Table, t) ->
                    for other in set.ByTable.[t] do
                        for c in other.TimeRelated do
                            if not (has cal.ResultColumns c) then
                                check.Add c |> ignore
                | _ -> ()

        let replaced (x: BColumn) =
            not keep && filters |> List.exists (fun f -> covers f x)

        (fun x -> drop.Contains x && not (replaced x)), (fun x -> check.Contains x && not (replaced x))

    /// Every addition of filters other than CALCULATE's filter arguments (which `onFilterArguments`
    /// clears first): a context transition, KEEPFILTERS, a SUMMARIZECOLUMNS group or filter
    /// argument, FIRSTNONBLANK's and the other functions' own filters. Whether calendar context
    /// clearing applies there is not documented, so nothing is cleared and the addition is
    /// refused where clearing would change the visible rows.
    let private beforeAdd (fc: FilterCtx) (filters: Filter list) (keep: bool) : FilterCtx =
        match CalendarRegistry.tryGet fc.Config.Model with
        | None -> fc
        | Some set ->
            let dropping, checking = clearing set filters keep

            if matters fc filters keep (fun x -> dropping x || checking x) then
                refuse
                    "a filter on a column a calendar tags, other than a CALCULATE filter argument (a context transition, KEEPFILTERS, SUMMARIZECOLUMNS, an iterator's own filter), while other columns the calendars tag are filtered (whether calendar context clearing applies to it is not documented)"

            fc

    /// CALCULATE's filter arguments: the context without the filters they clear (see the top of
    /// the file). The ones under KEEPFILTERS clear nothing (`beforeAdd` refuses where that
    /// matters).
    let onFilterArguments (fc: FilterCtx) (filters: (Filter list * bool) list) : FilterCtx =
        match CalendarRegistry.tryGet fc.Config.Model with
        | None -> fc
        | Some set ->
            let replacing =
                filters |> List.filter (fun (_, keep) -> not keep) |> List.collect fst

            if replacing.IsEmpty then
                fc
            else
                let dropping, checking = clearing set replacing false

                if matters fc replacing false checking then
                    refuse
                        "a calendar's columns as a filter while time-related columns of another calendar of the table are filtered (whether those filters are cleared is not documented)"

                if fc.Filters |> List.exists (fun f -> touches f dropping) then
                    Filters.remove fc dropping
                else
                    fc

    /// A SUMMARIZECOLUMNS row: its groups' filters (`keys`, on the group-by columns and on the
    /// other filtered columns of their tables, combined) added to `fc`. Whether a group-by column
    /// that a calendar tags clears the other columns' filters is not documented: refused where it
    /// would change the visible rows (grouping by the date under a filter on its week is fine; by
    /// the year under a filter on the month is not).
    let onGroup (fc: FilterCtx) (groupCols: BColumn array) (keys: Filter list) : unit =
        match CalendarRegistry.tryGet fc.Config.Model with
        | None -> ()
        | Some set ->
            let tagged = groupCols |> Array.filter (fun c -> set.CategoryOf.ContainsKey c)

            if tagged.Length > 0 then
                let drop = HashSet<BColumn>(HashIdentity.Reference)

                for (t, cols) in tagged |> Array.groupBy (fun c -> c.Table) do
                    let categories =
                        cols
                        |> Array.fold (fun acc c -> Set.union acc (Category.cleared set.CategoryOf.[c])) Set.empty

                    for c in set.ColumnsIn(t, categories) do
                        if not (has groupCols c) then
                            drop.Add c |> ignore

                if drop.Count > 0 then
                    let off (x: BColumn) = not (drop.Contains x)
                    let cfg = fc.Config
                    let kept = Filters.build cfg (fc.Filters @ keys)

                    let cleared =
                        Filters.build cfg (List.collect (Filters.project off) (fc.Filters @ keys))

                    let tables = drop |> Seq.map (fun c -> c.Table) |> Seq.distinct

                    if
                        tables
                        |> Seq.exists (fun t -> (Filters.visible kept t).Rows <> (Filters.visible cleared t).Rows)
                    then
                        refuse
                            "a SUMMARIZECOLUMNS group-by column a calendar tags, with a filter on another column the calendars tag whose filter it would clear (whether calendar context clearing applies to the groups is not documented)"

    /// Puts the clearing rule in force: called when a model with calendars is bound.
    let install () = CalendarHooks.beforeAdd <- beforeAdd

    /// CALCULATE's filter arguments without the calendar functions' results, for the date-table
    /// rule, which doesn't apply to them (see the top of the file).
    let forDateTable (model: Model) (filters: (Filter list * bool) list) : (Filter list * bool) list =
        match CalendarRegistry.tryGet model with
        | None -> filters
        | Some set ->
            filters
            |> List.map (fun (fs, keep) -> fs |> List.filter (fun f -> (shapeOf set f).IsNone), keep)

    // ---- A calendar as an argument ---------------------------------------------------------------

    /// The calendar an argument names: `'Fiscal'` or `Fiscal`, or a parameter of a user-defined
    /// function the caller gave a calendar.
    let rec tryResolve (ctx: Ctx) (e: Expr) : BCalendar option =
        match CalendarRegistry.tryGet ctx.Model with
        | None -> None
        | Some set ->
            let byName (n: string) =
                match set.ByName.TryGetValue n with
                | true, cal ->
                    if ctx.Model.TableByName.ContainsKey n then
                        Fail.notSupported ctx e.Span $"'{n}', the name of both a table and a calendar"

                    Some cal
                | _ -> None

            match (Fn.unparen e).Kind with
            | QuotedTable n -> byName n
            | Identifier n ->
                match ctx.Scope.Vars.TryFind(n.ToUpperInvariant()) with
                | Some(ExprParam(arg, scope)) -> tryResolve { ctx with Scope = scope } arg
                | Some _ -> None
                | None -> byName n
            | _ -> None

    // ---- What a calendar's table holds -----------------------------------------------------------

    /// The rows of a calendar's table in the order of its dates, and its periods.
    type Data = {
        Calendar: BCalendar
        DateColumn: BColumn
        /// The table's rows (not the blank row), by date.
        Rows: int array
        /// Each position's date (a serial number).
        Days: float array
        /// The position of each row of the table, -1 for none.
        PositionOf: int array
        /// Per period: each position's period, numbered from 0 in the order of the dates, or why
        /// the calendar can't say.
        Periods: Dictionary<CalendarPeriod, Result<int array, string>>
    }

    type private Built = { Result: Result<Data, string> }

    let private datas = ConditionalWeakTable<BCalendar, Built>()

    /// The value a column has in a row, as its sort-by column orders it.
    let private sortKey (sortBy: BColumn -> BColumn option) (c: BColumn) (row: int) : DaxValue =
        match sortBy c with
        | Some s -> s.ValueAt row
        | None -> c.ValueAt row

    let private build (sortBy: BColumn -> BColumn option) (cal: BCalendar) : Result<Data, string> =
        let t = cal.Table
        let collation = t.Model.Collation

        match cal.Problem, cal.Group Category.Date with
        | Some why, _ -> Error why
        | None, None ->
            Error
                $"the calendar '{cal.Name}' doesn't tag the Date category (the functions are implemented over the dates of the calendar)"
        | None, Some g ->
            let dc = g.Primary
            let n = t.RowCount
            let days = Array.zeroCreate<float> n
            let mutable problem = None

            if dc.DataType <> DaxType.DateTime then
                problem <- Some $"its Date column {describe dc} is not of dates"

            for r in 0 .. n - 1 do
                match dc.ValueAt r with
                | DaxValue.DateTime s when s = Math.Floor s -> days.[r] <- s
                | DaxValue.DateTime _ -> problem <- Some $"its Date column {describe dc} has a time of day"
                | _ -> problem <- Some $"its Date column {describe dc} has a BLANK"

            let rows = Array.init n id |> Array.sortBy (fun r -> days.[r])

            for i in 1 .. n - 1 do
                if days.[rows.[i]] = days.[rows.[i - 1]] then
                    problem <- Some $"its Date column {describe dc} has the same date twice"

            // "a column associated with a category doesn't have blank values" (Learn, Offline
            // validations): a calendar that breaks it is not one the sources describe.
            for grp in cal.Groups do
                for c in grp.Primary :: grp.Associated do
                    for r in 0 .. n - 1 do
                        match c.ValueAt r with
                        | DaxValue.Blank when problem.IsNone -> problem <- Some $"its column {describe c} has a BLANK"
                        | _ -> ()

            match problem with
            | Some why -> Error $"the calendar '{cal.Name}': {why}"
            | None ->
                let positionOf = Array.create (t.Size + 1) -1

                rows |> Array.iteri (fun i r -> positionOf.[r] <- i)

                let periods = Dictionary<CalendarPeriod, Result<int array, string>>()

                for p in
                    [
                        CalendarPeriod.Week
                        CalendarPeriod.Month
                        CalendarPeriod.Quarter
                        CalendarPeriod.Year
                    ] do
                    let available =
                        Category.routes p |> List.filter (fun route -> route |> List.forall cal.Has)

                    let ofRoute (route: Category list) : Result<int array, string> =
                        let cols = route |> List.map (fun c -> (cal.Group c).Value.Primary) |> Array.ofList
                        let key (r: int) = cols |> Array.map (fun c -> c.IdAt r)
                        let index = Array.zeroCreate<int> n

                        let seen =
                            HashSet<int array>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

                        let mutable current = -1
                        let mutable last: int array = null
                        let mutable error = None

                        for i in 0 .. n - 1 do
                            let k = key rows.[i]

                            if isNull last || k <> last then
                                if not (seen.Add k) && error.IsNone then
                                    error <-
                                        Some
                                            $"the dates of one {Category.periodName p} of the calendar '{cal.Name}' are not next to one another"

                                // Each period after the one before it in the order of its columns:
                                // "the primary columns are used for sorting" (Learn).
                                if not (isNull last) && error.IsNone then
                                    let before = rows.[i - 1]

                                    let order =
                                        cols
                                        |> Array.fold
                                            (fun acc c ->
                                                if acc <> 0 then
                                                    acc
                                                else
                                                    DaxValue.sortCompare
                                                        collation
                                                        (sortKey sortBy c before)
                                                        (sortKey sortBy c rows.[i]))
                                            0

                                    if order >= 0 then
                                        error <-
                                            Some
                                                $"the {Category.periodName p}s of the calendar '{cal.Name}' don't sort in the order of their dates"

                                current <- current + 1
                                last <- k

                            index.[i] <- current

                        match error with
                        | Some e -> Error e
                        | None -> Ok index

                    let result =
                        match available with
                        | [] ->
                            Error
                                $"the calendar '{cal.Name}' doesn't tag the categories that define a {Category.periodName p}"
                        | first :: others ->
                            match ofRoute first with
                            | Error e -> Error e
                            | Ok index ->
                                let disagree =
                                    others
                                    |> List.exists (fun r ->
                                        match ofRoute r with
                                        | Ok other -> other <> index
                                        | Error _ -> true)

                                if disagree then
                                    Error
                                        $"the columns of the calendar '{cal.Name}' that define a {Category.periodName p} give different {Category.periodName p}s"
                                else
                                    Ok index

                    periods.[p] <- result

                Ok {
                    Calendar = cal
                    DateColumn = dc
                    Rows = rows
                    Days = rows |> Array.map (fun r -> days.[r])
                    PositionOf = positionOf
                    Periods = periods
                }

    /// The sort-by column of each column, as the model was bound.
    let private sortBys = ConditionalWeakTable<Model, Dictionary<BColumn, BColumn>>()

    let registerSortBy (model: Model) (map: Dictionary<BColumn, BColumn>) = sortBys.AddOrUpdate(model, map)

    let private sortByOf (model: Model) (c: BColumn) : BColumn option =
        match sortBys.TryGetValue model with
        | true, m ->
            match m.TryGetValue c with
            | true, s -> Some s
            | _ -> None
        | _ -> None

    let data (ctx: Ctx) (span: Span) (name: string) (cal: BCalendar) : Data =
        let built =
            datas.GetValue(
                cal,
                ConditionalWeakTable<BCalendar, Built>.CreateValueCallback(fun (c: BCalendar) -> {
                    Result = build (sortByOf c.Table.Model) c
                })
            )

        match built.Result with
        | Ok d -> d
        | Error why -> Fail.notSupported ctx span $"{name} over {why}"
