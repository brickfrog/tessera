namespace Tessera.DaxEngine.Functions

// Time intelligence over a date column (the "classic" functions) and over a calendar
// (calendar-based time intelligence: see "Calendars" below, and Calendars.fs for the filter
// clearing). Each classic function is implemented to the definition DAX Guide publishes for it:
//
//   - The dates argument. "A reference to a date/time column. Only in this case a context
//     transition applies because the <Dates> column reference is replaced by CALCULATETABLE (
//     DISTINCT ( <Dates> ) )"; or "a table expression that returns a single column of date/time
//     values" (dax.guide, every function here). The True/False form is refused.
//   - "The result of time intelligence functions has the same data lineage as the date column or
//     table provided as an argument" and "the result table includes only dates that exist in the
//     dates column" (dax.guide): a result is always some of the values of that model column, so it
//     filters the column when used as a CALCULATE filter, and the date-table rule (`DateTable` in
//     Context.fs) then applies to it like to any filter on that column.
//   - STARTOF* / ENDOF*: the expansion dax.guide gives (the earliest / latest date "in the entire
//     <Dates> column devoid of all filters" of the period of the first / last visible date, then
//     CALCULATETABLE ( VALUES ( column ), column = that date )).
//   - DATESMTD / QTD / YTD: the dates of the whole column in the period of the last visible date,
//     up to it (SQLBI's expansion; see `toDate`). TOTAL*: CALCULATE ( <Expression>, DATES*TD (
//     <Dates> ) [, <Filter>] ).
//     CLOSINGBALANCE*: CALCULATE ( <Expression>, ENDOF* ( <Dates> ) [, <Filter>] ).
//     OPENINGBALANCE*: CALCULATE ( <Expression>, CALCULATETABLE ( ENDOF* ( <Dates> ), PREVIOUS* (
//     <Dates> ) ) [, <Filter>] ). These three are evaluated as exactly that expression.
//   - SAMEPERIODLASTYEAR: DATEADD ( <Dates>, -1, YEAR ). PREVIOUSDAY / NEXTDAY: DATEADD ( FIRSTDATE
//     / LASTDATE ( <Dates> ), -1 / 1, DAY ). PREVIOUS / NEXT MONTH, QUARTER, YEAR: PARALLELPERIOD (
//     FIRSTDATE / LASTDATE ( <Dates> ), -1 / 1, ... ).
//   - DATEADD and PARALLELPERIOD: the algorithms of SQLBI's "Differences between DATEADD and
//     PARALLELPERIOD in DAX", with the month-end behaviour Microsoft Learn documents (see `shift`).
//   - DATESINPERIOD: dax.guide's rule (a boundary date that is not in the result) with the
//     boundary its tables show for a date column (see `period`).
//
// Where the sources stop, the function refuses: see each `Fail.notSupported`.

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module TimeIntelligence =

    let private family = "Time intelligence"

    // ---- Days ------------------------------------------------------------------------------------
    //
    // A date is its day number: the serial number of a date at midnight (days since 1899-12-30).

    let private epoch = DateTime(1899, 12, 30)
    let private dayOf (d: DateTime) : int = int (d.Date - epoch).TotalDays
    let private dateOf (day: int) : DateTime = epoch.AddDays(float day)

    /// The days the functions work on: far enough from both ends of the calendar that no period
    /// of a date falls off it.
    let private firstDay = dayOf (DateTime(3, 1, 1))
    let private lastDay = dayOf (DateTime(9990, 12, 31))

    /// Months are numbered year * 12 + month - 1.
    let private monthOf (day: int) : int =
        let d = dateOf day
        d.Year * 12 + d.Month - 1

    let private firstMonth = monthOf firstDay
    let private lastMonth = monthOf lastDay

    /// The first day of a month, or of the first / last month the functions work on when it is
    /// outside them (no date of a column is beyond those).
    let private monthStart (month: int) : int =
        let m = max firstMonth (min lastMonth month)
        dayOf (DateTime(m / 12, m % 12 + 1, 1))

    let private monthEnd (month: int) : int =
        let m = max firstMonth (min lastMonth month)
        dayOf (DateTime(m / 12, m % 12 + 1, 1).AddMonths(1).AddDays(-1.0))

    let private inRange (month: int) =
        month >= firstMonth && month <= lastMonth

    // ---- The dates column ------------------------------------------------------------------------

    /// What a date column holds, whatever the filter context.
    type private Universe = {
        /// Every date of the column, ascending.
        Days: int array
        Set: HashSet<int>
        /// A value has a time of day, or is outside the years 3 to 9990.
        Odd: bool
        StoresBlank: bool
        /// Every row has a different date and no day is missing between the first and the last:
        /// what the engine's own message calls a column that is unique and has no gaps.
        WellFormed: bool
    }

    let private universes = ConditionalWeakTable<BColumn, Universe>()

    let private buildUniverse (c: BColumn) : Universe =
        let data = c.Data
        let days = ResizeArray<int>()
        let mutable odd = false
        let mutable blank = false

        for v in data.Values do
            match v with
            | DaxValue.Blank -> blank <- true
            | DaxValue.DateTime s when s = Math.Floor s && s >= float firstDay && s <= float lastDay -> days.Add(int s)
            | _ -> odd <- true

        let days = days.ToArray()
        Array.sortInPlace days

        {
            Days = days
            Set = HashSet<int>(days)
            Odd = odd
            StoresBlank = blank
            WellFormed =
                data.IsUnique
                && not blank
                && (days.Length = 0 || days.[days.Length - 1] - days.[0] + 1 = days.Length)
        }

    let private describe (c: BColumn) = $"'{c.Table.Name}'[{c.Name}]"

    let private universeOf (ctx: Ctx) (span: Span) (name: string) (c: BColumn) : Universe =
        if c.DataType <> DaxType.DateTime then
            Fail.invalid ctx span $"{name} needs a column of dates: {describe c} is {DaxType.name c.DataType}"

        let u =
            universes.GetValue(c, ConditionalWeakTable<BColumn, Universe>.CreateValueCallback buildUniverse)

        if u.Odd then
            // "In case the column also contains a time part, no time should be used" (dax.guide).
            Fail.notSupported
                ctx
                span
                $"{name} over {describe c}, which has dates with a time of day (or outside the years 3 to 9990)"

        u

    /// The dates of the column from `lo` to `hi`, both included.
    let private range (u: Universe) (lo: int) (hi: int) : int array =
        if hi < lo || u.Days.Length = 0 then
            [||]
        else
            let find (day: int) =
                let i = Array.BinarySearch(u.Days, day)
                if i >= 0 then i else ~~~i

            let a = find lo
            let b = find (if hi = Int32.MaxValue then hi else hi + 1)
            if b > a then Array.sub u.Days a (b - a) else [||]

    /// These days, without the ones the column doesn't have: ascending, each once.
    let private inColumn (u: Universe) (days: int seq) : int array =
        days |> Seq.filter u.Set.Contains |> Seq.distinct |> Seq.sort |> Array.ofSeq

    /// Dates as a table with the column's lineage.
    let private tableOf (c: BColumn) (days: int array) : TableVal =
        Tables.ofColumns
            [| Tables.modelColumn c |]
            [| days |> Array.map (fun d -> DaxValue.DateTime(float d)) |]
            days.Length

    // ---- The dates argument ----------------------------------------------------------------------

    type private Dates = {
        Column: BColumn
        U: Universe
        /// The selected dates, ascending.
        Days: int array
        /// The selection also has a BLANK (a column that stores one).
        Blank: bool
        /// The caller's context after context transition: where the CALCULATETABLE that ends the
        /// definition of STARTOF* / ENDOF* evaluates.
        After: Lazy<Ctx>
    }

    let private read (ev: IEval) (ctx: Ctx) (name: string) (arg: Expr) : Dates =
        if (Calendars.tryResolve ctx arg).IsSome then
            Fail.notSupported
                ctx
                arg.Span
                $"{name} over a calendar (calendar-based time intelligence: not implemented for {name})"

        match ev.TryColumn(ctx, arg) with
        | Some c ->
            let u = universeOf ctx arg.Span name c
            // CALCULATETABLE ( DISTINCT ( column ) ): context transition, no blank row.
            let moved = Context.transition ctx arg.Span
            let ids = Filters.idsIn c (Filters.visible moved.Filter c.Table) false
            let days = ResizeArray<int>()
            let mutable blank = false

            for id in ids do
                match c.ValueOfId id with
                | DaxValue.DateTime s -> days.Add(int s)
                | _ -> blank <- true

            let days = days.ToArray()
            Array.sortInPlace days

            {
                Column = c
                U = u
                Days = days
                Blank = blank
                After = lazy moved
            }
        | None when ev.IsTable(ctx, arg) ->
            let t = ev.Table(ctx, arg)

            if t.Cols.Length <> 1 then
                Fail.invalid ctx arg.Span $"the dates argument of {name} is a column of dates, or a table of one column"

            let c = t.Cols.[0].Lineage

            if isNull c then
                // "The result table includes only dates that exist in the dates column": a table
                // that is not of a model column has no such column.
                Fail.notSupported
                    ctx
                    arg.Span
                    $"{name} of a table of dates that are not the values of a model column (the result is defined by the dates of that column)"

            let u = universeOf ctx arg.Span name c
            let days = ResizeArray<int>()
            let mutable blank = false

            for v in t.Column 0 do
                match v with
                | DaxValue.Blank -> blank <- true
                | DaxValue.DateTime s when s = Math.Floor s && u.Set.Contains(int s) -> days.Add(int s)
                | _ ->
                    Fail.notSupported ctx arg.Span $"{name} of a table with values that are not dates of {describe c}"

            let days = days.ToArray()
            Array.sortInPlace days

            if (Array.distinct days).Length <> days.Length then
                Fail.notSupported ctx arg.Span $"{name} of a table that has the same date more than once"

            {
                Column = c
                U = u
                Days = days
                Blank = blank
                After = lazy (Context.transition ctx arg.Span)
            }
        | None ->
            Fail.notSupported
                ctx
                arg.Span
                $"a dates argument of {name} that is neither a date column nor a table of dates (a True/False expression, or a calendar)"

    let private noBlank (ctx: Ctx) (span: Span) (name: string) (d: Dates) : unit =
        if d.Blank then
            Fail.notSupported ctx span $"{name} over a selection of dates that has a BLANK"

    // ---- Periods ---------------------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type private Period =
        | Month
        | Quarter
        /// A year that ends on this month and day (12, 31 unless a year end date is given).
        | Year of endMonth: int * endDay: int

    let private calendarYear = Period.Year(12, 31)

    /// The first and the last day of the period a day is in.
    let private bounds (p: Period) (day: int) : int * int =
        let d = dateOf day

        match p with
        | Period.Month ->
            let first = DateTime(d.Year, d.Month, 1)
            dayOf first, dayOf (first.AddMonths(1).AddDays(-1.0))
        | Period.Quarter ->
            let first = DateTime(d.Year, (d.Month - 1) / 3 * 3 + 1, 1)
            dayOf first, dayOf (first.AddMonths(3).AddDays(-1.0))
        | Period.Year(m, dd) ->
            let thisYear = DateTime(d.Year, m, dd)

            let last =
                if d <= thisYear then
                    thisYear
                else
                    DateTime(d.Year + 1, m, dd)

            dayOf (DateTime(last.Year - 1, m, dd).AddDays(1.0)), dayOf last

    let private daysInMonth = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

    /// The year end date argument. "A literal string with a date that defines the year-end date"
    /// (Microsoft Learn, DATESYTD); "The year part of the date is ignored, so "2001-06-30" and
    /// "06-30" give the same result" (dax.guide/closingbalanceyear). It is read "in the same locale
    /// as the locale of the client where the workbook was created ... Depending on locale, the
    /// format might be something like "m-dd" or "dd-m"" (Microsoft Learn, DATESYTD), and "it is
    /// recommended you specify year_end_date using "month/day" ... to ensure the string resolves to
    /// a date" (Microsoft Learn, TOTALYTD). So what is taken is a whole ISO date, or month then day
    /// where the day can't be a month ("6/30": the documented form; a day-first locale can't read
    /// it as day 6 of month 30, and "if conversion using the locale and date/time settings of the
    /// model fails, DATEVALUE will attempt to use other date formats": Microsoft Learn, DATEVALUE).
    let private yearEnd (ctx: Ctx) (arg: Expr option) : Period =
        match arg with
        | None -> calendarYear
        | Some e when Fn.isOmitted e -> calendarYear
        | Some e ->
            let text =
                match (Fn.unparen e).Kind with
                | StringLiteral s -> s
                | _ -> Fail.notSupported ctx e.Span "a year end date that is not a text literal"

            let iso = Regex.Match(text, @"^\s*\d{4}-(\d{1,2})-(\d{1,2})\s*$")
            let two = Regex.Match(text, @"^\s*(\d{1,2})\s*[/\-.]\s*(\d{1,2})\s*$")

            let month, day =
                if iso.Success then
                    int iso.Groups.[1].Value, int iso.Groups.[2].Value
                elif two.Success then
                    let a = int two.Groups.[1].Value
                    let b = int two.Groups.[2].Value

                    if a = b || (a <= 12 && b > 12) then
                        a, b
                    else
                        Fail.notSupported
                            ctx
                            e.Span
                            $"the year end date \"{text}\" (which number is the month depends on the model's culture; write the month first with a day above 12, as in \"6/30\", or a whole date, as in \"2000-06-30\")"
                else
                    Fail.notSupported
                        ctx
                        e.Span
                        $"the year end date \"{text}\" (only numeric month and day are read: \"6/30\", \"06-30\", \"2000-06-30\")"

            if month < 1 || month > 12 || day < 1 || day > daysInMonth.[month - 1] then
                Fail.notSupported ctx e.Span $"the year end date \"{text}\", which is not a day of every year"

            if month = 2 then
                // Whether a year that ends on February 28 ends on the 29th in a leap year is not
                // documented.
                Fail.notSupported ctx e.Span "a year end date in February"

            Period.Year(month, day)

    let private isTextLiteral (e: Expr) =
        match (Fn.unparen e).Kind with
        | StringLiteral _ -> true
        | _ -> false

    // ---- FIRSTDATE, LASTDATE, STARTOF*, ENDOF* ---------------------------------------------------

    /// "Returns first non blank date" (dax.guide/firstdate): the BLANK a column may store is not a
    /// date.
    let private firstOrLast (isLast: bool) (d: Dates) : int voption =
        if d.Days.Length = 0 then ValueNone
        elif isLast then ValueSome d.Days.[d.Days.Length - 1]
        else ValueSome d.Days.[0]

    /// The value of the column's rows visible once CALCULATETABLE has put `column = day` on the
    /// context: the last step of the definition of STARTOF* and ENDOF*.
    let private valuesAt (span: Span) (d: Dates) (day: int) : int array =
        let c = d.Column
        let after = d.After.Value

        let filter =
            OnColumn(c, RowSet.One(c.IdCount, c.FindId(DaxValue.DateTime(float day))))

        let fc = DateTable.onFilterArguments after span after.Filter [ [ filter ], false ]

        let fc = Filters.add fc [ filter ] false

        Filters.idsIn c (Filters.visible fc c.Table) true
        |> Array.choose (fun id ->
            match c.ValueOfId id with
            | DaxValue.DateTime s -> Some(int s)
            | _ -> None)

    /// STARTOF* / ENDOF*: "filters <Dates> into a 1-row, 1-column table that shows only the
    /// earliest [latest] date, in the entire <Dates> column devoid of all filters, that is in the
    /// same year and month as the earliest [latest] visible date in <Dates>" (dax.guide).
    let private edge (isEnd: bool) (p: Period) (ctx: Ctx) (span: Span) (name: string) (d: Dates) : int array =
        match firstOrLast isEnd d with
        | ValueNone ->
            // The definition then looks for the dates of the period of BLANK, December 1899 (the
            // year and month of BLANK), and ends in `column = BLANK`. With no date of 1899 and no
            // BLANK in the column that is an empty table: "evaluates Expression with an empty
            // filter on the dates" (dax.guide/openingbalancemonth, of the first month).
            let has1899 =
                (range d.U (dayOf (DateTime(1899, 1, 1))) (dayOf (DateTime(1899, 12, 31)))).Length > 0

            if d.U.StoresBlank || d.Blank || d.Column.Table.HasBlankRow || has1899 then
                Fail.notSupported
                    ctx
                    span
                    $"{name} with no date selected, over a column that has a BLANK (or the blank row) or dates of 1899"

            [||]
        | ValueSome anchor ->
            let lo, hi = bounds p anchor
            let period = range d.U lo hi
            valuesAt span d (if isEnd then period.[period.Length - 1] else period.[0])

    // ---- DATESBETWEEN, DATESINPERIOD -------------------------------------------------------------

    let private columnOnly (ev: IEval) (ctx: Ctx) (name: string) (arg: Expr) : BColumn * Universe =
        if (Calendars.tryResolve ctx arg).IsSome then
            Fail.notSupported
                ctx
                arg.Span
                $"{name} over a calendar (calendar-based time intelligence: not implemented for {name})"

        match ev.TryColumn(ctx, arg) with
        | Some c -> c, universeOf ctx arg.Span name c
        | None ->
            // "The Dates argument can be a reference to a date column or a calendar reference"
            // (dax.guide/datesbetween).
            Fail.notSupported ctx arg.Span $"{name} over anything but a date column (a table of dates, or a calendar)"

    let private interval (ctx: Ctx) (name: string) (e: Expr) : string =
        match Fn.keyword e with
        | Some("DAY" | "MONTH" | "QUARTER" | "YEAR" as k) -> k
        | Some "WEEK" -> Fail.notSupported ctx e.Span $"{name} by WEEK (calendar-based time intelligence)"
        | _ -> Fail.invalid ctx e.Span $"the interval of {name} is one of DAY, MONTH, QUARTER, YEAR"

    let private count (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : int =
        match ev.Scalar(ctx, e) with
        | DaxValue.Integer i when abs i <= 1000000L -> int i
        | _ ->
            Fail.notSupported
                ctx
                e.Span
                $"a number of intervals of {name} that is not a whole number (or is beyond a million)"

    let private monthsOf (unit: string) =
        match unit with
        | "MONTH" -> 1
        | "QUARTER" -> 3
        | _ -> 12

    /// DATESINPERIOD: "starts on the start date. It moves back or forward by the number of
    /// intervals. This gives a boundary date. The result has the dates between the start date and
    /// the boundary date. The boundary date is not in the result" (dax.guide/datesinperiod).
    ///
    /// The boundary, for a date column, from the "classic 'Date'[Date]" rows of that page's tables:
    /// the same day of the other month (2008-01-15 back a month, a quarter, a year: from
    /// 2007-12-16, 2007-10-16, 2007-01-16), but the LAST day of the other month when the start is
    /// the last day of its month (2008-02-29 back a month: from 2008-02-01, back a quarter: from
    /// 2007-12-01; 2007-02-28 forward a month: to 2007-03-30, forward a year: to 2008-02-28). A
    /// start that is not a month end and whose day the other month doesn't have (March 30 back a
    /// month) is in none of the tables: refused.
    let private period (ctx: Ctx) (span: Span) (u: Universe) (start: int) (n: int) (unit: string) : int array =
        if n = 0 then
            // "returns an empty table because we request 0 days" (dax.guide/datesinperiod).
            [||]
        elif unit = "DAY" then
            if n > 0 then
                range u start (start + n - 1)
            else
                range u (start + n + 1) start
        else
            let month = monthOf start
            let target = month + n * monthsOf unit

            if not (inRange target) then
                (if n > 0 then
                     range u start Int32.MaxValue
                 else
                     range u Int32.MinValue start)
            else
                let offset = start - monthStart month
                let isMonthEnd = start = monthEnd month
                let length = monthEnd target - monthStart target + 1

                let boundary =
                    if isMonthEnd then
                        monthEnd target
                    elif offset < length then
                        monthStart target + offset
                    else
                        Fail.notSupported
                            ctx
                            span
                            "DATESINPERIOD from a day of the month that the month it moves to doesn't have (and that is not a month end)"

                if n > 0 then
                    range u start (boundary - 1)
                else
                    range u (boundary + 1) start

    // ---- DATEADD ---------------------------------------------------------------------------------

    let private isContiguous (days: int array) =
        days.Length < 2 || days.[days.Length - 1] - days.[0] + 1 = days.Length

    /// DATEADD (SQLBI, "Differences between DATEADD and PARALLELPERIOD in DAX"):
    ///
    ///   "If the interval is DAY, every Date[Date] value is shifted by the number of intervals
    ///   specified. ... If the interval is MONTH, QUARTER, or YEAR, then for each month: If all the
    ///   days of the month are selected, then the entire month is shifted by the number of
    ///   intervals specified. If not all the days of the month are selected, then each day is
    ///   shifted by the number of intervals specified."
    ///
    /// "The notion of 'month selected' is based on the dates available in the underlying date
    /// table" (dax.guide/dateadd). A day the other month doesn't have becomes its last day ("The
    /// last day of the shifted month is returned if the day does not exist in the shifted month":
    /// SQLBI; "In classic time intelligence, the same shift [February 29, 2008 back one year]
    /// returns Feb 28 2007": Microsoft Learn, SAMEPERIODLASTYEAR). And "when the selection includes
    /// the last two days of month, DATEADD will use "extension" semantics and will include the days
    /// until the end of month. For example, when Feb 27 and 28 of 2013 are included in the selection
    /// and a month is added, DATEADD will return March 27 to 31. This behavior only happens when
    /// last two days of month are included in the selection" (Microsoft Learn, DATEADD).
    ///
    /// The last day of a month selected without the day before it, moving to a longer month, is
    /// the one case those leave open (DAX Guide's EXTENDING, for calendars, extends from the last
    /// day alone): refused.
    ///
    /// A selection with gaps: "If the date column syntax is used and the dates in the current
    /// context do not form a contiguous interval, the function returns an error" (Microsoft Learn,
    /// DATEADD). The engine's message, as the Power BI community quotes it ("SAMEPERIODLASTYEAR
    /// expects a contiguous selection when the date column is not unique, has gaps or it contains
    /// time portion", community.fabric.microsoft.com/t5/Desktop/td-p/3217642), puts a condition on
    /// that. So on a column that is not unique or has gaps it is an error by both; on a column that
    /// is unique and complete, where SQLBI's "for each month" would apply, the two disagree: refused.
    let private shift (ctx: Ctx) (span: Span) (name: string) (d: Dates) (n: int) (unit: string) : int array =
        noBlank ctx span name d
        let u = d.U
        let s = d.Days

        if s.Length = 0 then
            [||]
        else
            if not (isContiguous s) then
                if u.WellFormed then
                    Fail.notSupported
                        ctx
                        span
                        $"{name} of a selection of dates with gaps (Microsoft Learn: \"if the dates in the current context do not form a contiguous interval, the function returns an error\"; the engine's own message only expects a contiguous selection of a column that is not unique or has gaps)"

                if (range u s.[0] s.[s.Length - 1]).Length = s.Length then
                    Fail.notSupported
                        ctx
                        span
                        $"{name} over {describe d.Column}, which has gaps, with a selection that skips only days the column doesn't have (whether that selection counts as contiguous is not documented)"

                Fail.runtime
                    ctx
                    span
                    $"Function '{name}' expects a contiguous selection when the date column is not unique, has gaps or it contains time portion."

            if unit = "DAY" then
                inColumn u (s |> Seq.map (fun day -> day + n))
            else
                let k = n * monthsOf unit
                let result = ResizeArray<int>()

                for (month, selected) in s |> Array.groupBy monthOf do
                    let target = month + k

                    if inRange target then
                        let first = monthStart month
                        let last = monthEnd month
                        let targetFirst = monthStart target
                        let targetLast = monthEnd target

                        if selected.Length = (range u first last).Length then
                            result.AddRange(range u targetFirst targetLast)
                        else
                            let length = last - first + 1
                            let targetLength = targetLast - targetFirst + 1
                            let hasLast = Array.contains last selected
                            let hasDayBefore = Array.contains (last - 1) selected

                            if hasLast && not hasDayBefore && targetLength > length then
                                Fail.notSupported
                                    ctx
                                    span
                                    $"{name} of the last day of a month, selected without the day before it, to a longer month (whether the result runs to the end of that month is not documented)"

                            for day in selected do
                                result.Add(targetFirst + min (day - first) (targetLength - 1))

                            if hasLast && hasDayBefore then
                                for offset in (min length targetLength) - 1 .. targetLength - 1 do
                                    result.Add(targetFirst + offset)

                inColumn u result

    // ---- PARALLELPERIOD --------------------------------------------------------------------------

    /// PARALLELPERIOD: "For each interval with at least one value in Date[Date], the function
    /// returns the entire interval shifted by the number of specified intervals" (SQLBI); "shifts
    /// the first date and the last date the specified number of intervals, and then returns all
    /// contiguous dates between the two shifted dates ... any partial months in the result are also
    /// filled out to complete the entire interval" (Microsoft Learn). The two agree when the
    /// intervals that have a selected date are next to one another; when one in between has none,
    /// the first leaves it out and the second includes it: refused.
    let private parallelPeriod (ctx: Ctx) (span: Span) (name: string) (d: Dates) (n: int) (unit: string) : int array =
        noBlank ctx span name d
        let u = d.U
        let s = d.Days

        if s.Length = 0 then
            [||]
        else
            if not u.WellFormed && not (isContiguous s) then
                Fail.notSupported
                    ctx
                    span
                    $"{name} of a selection with gaps over {describe d.Column}, which is not unique or has gaps (DATEADD is an error there; whether {name} is too is not documented)"

            let size = monthsOf unit
            let intervals = s |> Array.map (fun day -> monthOf day / size) |> Array.distinct
            let first = intervals.[0]
            let last = intervals.[intervals.Length - 1]

            if last - first + 1 <> intervals.Length then
                Fail.notSupported
                    ctx
                    span
                    $"{name} of a selection that skips a whole {unit.ToLowerInvariant()} (Microsoft's definition returns every date between the first and the last shifted {unit.ToLowerInvariant()}, SQLBI's only the ones that have a selected date)"

            let lo = (first + n) * size
            let hi = (last + n) * size + size - 1

            if hi < firstMonth || lo > lastMonth then
                [||]
            else
                range u (monthStart lo) (monthEnd hi)

    /// PREVIOUS* / NEXT* MONTH, QUARTER, YEAR: the whole period before the one of the first date,
    /// or after the one of the last ("PARALLELPERIOD ( FIRSTDATE ( <Dates> ), -1, MONTH )":
    /// dax.guide/previousmonth). With a year end date the years are the ones that end on it.
    let private neighbour (isNext: bool) (p: Period) (d: Dates) : int array =
        match firstOrLast isNext d with
        | ValueNone -> [||]
        | ValueSome anchor ->
            let lo, hi = bounds p anchor
            let lo, hi = bounds p (if isNext then hi + 1 else lo - 1)
            range d.U lo hi

    // ---- Argument checks (what can be told from the trees) ---------------------------------------

    /// A dates argument that can only be a True/False expression or a value.
    let private checkDates (index: int) (args: Expr list) : string option =
        match List.tryItem index args with
        | Some e ->
            match (Fn.unparen e).Kind with
            | Binary _
            | Unary _
            | IntegerLiteral _
            | NumberLiteral _
            | StringLiteral _
            | BooleanLiteral _
            | DateLiteral _ ->
                Some "a True/False expression (or a value) as the dates argument of a time intelligence function"
            | _ -> None
        | None -> None

    let private checkInterval (index: int) (args: Expr list) : string option =
        match List.tryItem index args |> Option.bind Fn.keyword with
        | Some "WEEK" -> Some "the WEEK interval (calendar-based time intelligence)"
        | _ -> None

    let private checkYearEnd (index: int) (args: Expr list) : string option =
        match List.tryItem index args with
        | Some e when not (Fn.isOmitted e) && not (isTextLiteral e) -> Some "a year end date that is not a text literal"
        | _ -> None

    let private checkExtra (index: int) (what: string) (args: Expr list) : string option =
        if args.Length > index then Some what else None

    let private checks (rules: (Expr list -> string option) list) : Expr list -> string option =
        fun args -> rules |> List.tryPick (fun rule -> rule args)

    // ---- The functions that are a CALCULATE ------------------------------------------------------

    let private callOf (name: string) (span: Span) (args: Expr list) : Expr = {
        Kind = Call({ Name = name; Span = span }, args)
        Span = span
    }

    /// ( <Expression>, <Dates> [, <Filter>] [, <YearEndDate>] ): "If you do not use Filter, you can
    /// write YearEndDate as the third argument" (dax.guide/closingbalanceyear).
    let private balanceArguments (withYearEnd: bool) (call: Call) : Expr * Expr * Expr list * Expr list =
        let given (e: Expr) = if Fn.isOmitted e then [] else [ e ]

        match call.Args with
        | [ e; d ] -> e, d, [], []
        | [ e; d; third ] when withYearEnd && isTextLiteral third -> e, d, [], [ third ]
        | [ e; d; f ] -> e, d, given f, []
        | [ e; d; f; y ] -> e, d, given f, given y
        | _ -> invalidOp "the argument count is checked before the call"

    let private checkBalance (withYearEnd: bool) : Expr list -> string option =
        checks [
            checkDates 1
            if withYearEnd then
                checkYearEnd 3
        ]

    /// A function that is CALCULATE ( <Expression>, <the dates filter> [, <Filter>] ), evaluated as
    /// that expression. `datesFilter`: the filter argument, from the dates argument and the year
    /// end date (if any).
    let private asCalculate
        (name: string)
        (withYearEnd: bool)
        (datesFilter: Span -> Expr -> Expr list -> Expr)
        : DaxFunction =
        Fn.scalar name family (2, (if withYearEnd then 4 else 3)) (Fn.typeOfArg 0) (fun ev ctx call ->
            let expression, dates, filter, yearEndDate = balanceArguments withYearEnd call

            ev.Scalar(
                ctx,
                callOf "CALCULATE" call.Span (expression :: datesFilter call.Span dates yearEndDate :: filter)
            ))
        |> Fn.withCheck (checkBalance withYearEnd)

    // ---- Calendars (calendar-based time intelligence) ----------------------------------------
    //
    // With a calendar the functions work on the rows of the calendar's table, in the order of the
    // dates the calendar tags as Date, and on the periods its categories define (`Calendars.Data`).
    // "With a calendar, the result has the primary column of each calendar category and the
    // time-related columns of the calendar. Also in this case, a context transition applies"
    // (dax.guide/datesmtd). The result is of the whole table: "Calendar-based time intelligence
    // functions don't have such a requirement [a complete date column]. Instead, they operate on
    // the dates as-is" (Microsoft Learn, Sparse dates). Refused: a calendar without the Date
    // category, FIRSTDATE, LASTDATE, DATEADD, SAMEPERIODLASTYEAR, PARALLELPERIOD and DATESINPERIOD
    // over a calendar (the "same distance from parent" shifts and their Extension, Truncation and
    // EndBehavior arguments).

    /// What a calendar function sees of its calendar's table.
    type private Seen = {
        D: Calendars.Data
        /// The visible positions (see `Calendars.Data`), ascending.
        Positions: int array
        /// The categories of the filtered columns some calendar tags.
        Filtered: Set<Category>
        /// A filter is on the column tagged as Date, or on the whole table.
        DateFiltered: bool
        /// A time-related column is filtered.
        TimeRelatedFiltered: bool
    }

    let private calendarSet (ctx: Ctx) : CalendarSet =
        match CalendarRegistry.tryGet ctx.Model with
        | Some s -> s
        | None -> invalidOp "a calendar was resolved in a model without calendars"

    /// The rows of the calendar's table visible after context transition.
    let private seen (ctx: Ctx) (span: Span) (name: string) (cal: BCalendar) : Seen =
        let d = Calendars.data ctx span name cal
        let set = calendarSet ctx
        let fc = (Context.transition ctx span).Filter
        let t = cal.Table
        let visible = Filters.visible fc t

        let onTable (f: Filter) =
            match f with
            | OnColumn(c, _) -> obj.ReferenceEquals(c.Table, t)
            | Tuples(cols, _) -> cols |> Array.exists (fun c -> obj.ReferenceEquals(c.Table, t))
            | Rows(r, _, cfg) -> cfg.IsExpanded(r, t)

        let own = fc.Filters |> List.filter onTable

        if (Filters.visible (Filters.build fc.Config own) t).Rows <> visible.Rows then
            // Which columns count as filtered when other tables cross-filter the calendar's table
            // is not documented.
            Fail.notSupported
                ctx
                span
                $"{name} over the calendar '{cal.Name}' while filters on other tables cross-filter '{t.Name}'"

        let mutable filtered = Set.empty
        let mutable dateFiltered = false
        let mutable timeRelated = false

        let column (c: BColumn) =
            if obj.ReferenceEquals(c, d.DateColumn) then
                dateFiltered <- true

            match set.CategoryOf.TryGetValue c with
            | true, Category.Date -> dateFiltered <- true
            | true, cat -> filtered <- filtered.Add cat
            | _ ->
                if set.TimeRelated.Contains c then
                    timeRelated <- true

        for f in own do
            match f with
            | OnColumn(c, _) -> column c
            | Tuples(cols, _) -> cols |> Array.iter column
            | Rows _ -> dateFiltered <- true

        {
            D = d
            Positions =
                visible.Rows
                |> Array.choose (fun r ->
                    let p = if r < d.PositionOf.Length then d.PositionOf.[r] else -1
                    if p >= 0 then Some p else None)
                |> Array.sort
            Filtered = filtered
            DateFiltered = dateFiltered
            TimeRelatedFiltered = timeRelated
        }

    /// Each position's period. "TOTALWTD expects specific categories to be present in the
    /// calendar. If they aren't, an error is returned" (Microsoft Learn, "Work with calendars");
    /// "Power BI needs to be able to "walk-up" from the level the calculation is performed on all
    /// the way to an individual year" (the same page).
    let private periodsOf (ctx: Ctx) (span: Span) (name: string) (d: Calendars.Data) (p: CalendarPeriod) : int array =
        let cal = d.Calendar

        if not (Category.routes p |> List.exists (List.forall cal.Has)) then
            Fail.invalid
                ctx
                span
                $"{name}: the calendar '{cal.Name}' doesn't tag the categories that define a {Category.periodName p}"

        match d.Periods.[p] with
        | Ok index -> index
        | Error why -> Fail.notSupported ctx span $"{name} over {why}"

    /// The first and the last position of the period of a position.
    let private extent (index: int array) (pos: int) : int * int =
        let mutable lo = pos
        let mutable hi = pos

        while lo > 0 && index.[lo - 1] = index.[pos] do
            lo <- lo - 1

        while hi < index.Length - 1 && index.[hi + 1] = index.[pos] do
            hi <- hi + 1

        lo, hi

    /// The positions of a period by its number (empty when there is none).
    let private periodNumbered (index: int array) (n: int) : int * int =
        match Array.tryFindIndex (fun i -> i = n) index with
        | Some pos -> extent index pos
        | None -> 0, -1

    /// The calendar's columns for the rows at these positions.
    let private calendarTable (d: Calendars.Data) (lo: int) (hi: int) : TableVal =
        let cols = d.Calendar.ResultColumns
        let count = max 0 (hi - lo + 1)

        Tables.ofColumns
            (cols |> Array.map Tables.modelColumn)
            (cols
             |> Array.map (fun c -> Array.init count (fun i -> c.ValueAt d.Rows.[lo + i])))
            count

    let private noYearEnd (ctx: Ctx) (name: string) (call: Call) =
        if call.Args.Length > 1 then
            // "YearEndDate is valid only with a date column. With a calendar, DATESYTD returns an
            // error if you use YearEndDate. The calendar defines the year" (dax.guide/datesytd).
            Fail.invalid ctx call.Args.[1].Span $"{name}: a year end date can't be given with a calendar"

    /// DATESWTD / MTD / QTD / YTD over a calendar (dax.guide/datesmtd, datesqtd, datesytd,
    /// dateswtd): from the first date of the period of the last visible date to
    ///
    ///   - "If the filter context filters the column tagged as Day, the result ends with the last
    ///     visible day.
    ///   - If the filter context filters a column of another category, for example a week or a
    ///     month column, the result ends with the last day of the last visible period of that
    ///     category. The result does not go past the end of the month.
    ///   - If the filter context filters only columns that no calendar tags, for example the day
    ///     of the week, the result has all the days of the month that includes the last visible
    ///     date."
    ///   - "If the filter context filters a column that another calendar tags as Week, a calendar
    ///     without the Week category returns an error."
    ///
    /// With columns of several periods filtered, the period is the one inside all the others (a
    /// month and its year: the month); periods that don't nest (weeks across months) are refused,
    /// as are filters on the day categories other than Date (which no example shows) and on
    /// time-related columns (which no calendar tags with a category).
    let private toDateCalendar (p: CalendarPeriod) (name: string) (ctx: Ctx) (call: Call) (cal: BCalendar) : TableVal =
        noYearEnd ctx name call
        let span = call.Span
        let s = seen ctx span name cal
        let d = s.D
        let index = periodsOf ctx span name d p

        if s.Positions.Length = 0 then
            Fail.notSupported ctx span $"{name} over a calendar when no date is visible (not documented)"

        let last = s.Positions.[s.Positions.Length - 1]
        let first, periodEnd = extent index last

        if
            s.Filtered.Contains Category.Week
            && not (Category.routes CalendarPeriod.Week |> List.exists (List.forall cal.Has))
        then
            Fail.runtime
                ctx
                span
                $"{name}: a column another calendar tags as Week is filtered, and the calendar '{cal.Name}' has no Week category."

        let finish =
            if s.DateFiltered then
                last
            else
                let days = s.Filtered |> Set.filter (fun c -> (Category.periodOf c).IsNone)

                if not days.IsEmpty then
                    Fail.notSupported
                        ctx
                        span
                        $"{name} over a calendar while a column of the category {Category.name days.MinimumElement} is filtered (where the result ends is not documented)"

                if s.TimeRelatedFiltered then
                    Fail.notSupported
                        ctx
                        span
                        $"{name} over a calendar while a time-related column is filtered (where the result ends is not documented)"

                let kinds = s.Filtered |> Seq.choose Category.periodOf |> Seq.distinct |> List.ofSeq

                for k in kinds do
                    if not (Category.routes k |> List.exists (List.forall cal.Has)) then
                        Fail.notSupported
                            ctx
                            span
                            $"{name} over the calendar '{cal.Name}' while a column of a {Category.periodName k} it doesn't define is filtered (not documented)"

                match kinds with
                | [] -> periodEnd
                | _ ->
                    let indexes = kinds |> List.map (fun k -> k, periodsOf ctx span name d k)

                    // g nests in k when two positions in one g-period are always in one k-period.
                    let nests (g: int array) (k: int array) =
                        seq { 1 .. g.Length - 1 }
                        |> Seq.forall (fun i -> g.[i] <> g.[i - 1] || k.[i] = k.[i - 1])

                    match
                        indexes
                        |> List.tryFind (fun (_, g) -> indexes |> List.forall (fun (_, k) -> nests g k))
                    with
                    | Some(_, g) -> min periodEnd (snd (extent g last))
                    | None ->
                        Fail.notSupported
                            ctx
                            span
                            $"{name} over a calendar while columns of periods that don't nest are filtered (where the result ends is not documented)"

        calendarTable d first finish

    /// STARTOF* / ENDOF* over a calendar: the first date of the period of the first visible date,
    /// or the last date of the period of the last one ("CLOSINGBALANCEWEEK uses the last date of
    /// the week that contains the last date in the filter context": dax.guide/closingbalanceweek).
    /// With nothing visible the table is empty: "If there is no date before the current year ...
    /// OPENINGBALANCEYEAR evaluates Expression with an empty filter on the dates"
    /// (dax.guide/openingbalanceyear), whose definition ends in ENDOFYEAR of nothing.
    let private edgeCalendar (isEnd: bool) (p: CalendarPeriod) (name: string) (ctx: Ctx) (call: Call) (cal: BCalendar) =
        noYearEnd ctx name call
        let s = seen ctx call.Span name cal
        let index = periodsOf ctx call.Span name s.D p

        if s.Positions.Length = 0 then
            calendarTable s.D 0 -1
        else
            let anchor =
                if isEnd then
                    s.Positions.[s.Positions.Length - 1]
                else
                    s.Positions.[0]

            let lo, hi = extent index anchor
            let at = if isEnd then hi else lo
            calendarTable s.D at at

    /// PREVIOUS* / NEXT* over a calendar: "all dates from the previous week, based on the first
    /// date in the current context" (Microsoft Learn, PREVIOUSWEEK), and the period after the one
    /// of the last date for NEXT*. "OPENINGBALANCEWEEK uses the last date of the week before the
    /// week that contains the first date in the filter context" (dax.guide/openingbalanceweek).
    let private neighbourCalendar
        (isNext: bool)
        (p: CalendarPeriod)
        (name: string)
        (ctx: Ctx)
        (call: Call)
        (cal: BCalendar)
        : TableVal =
        noYearEnd ctx name call
        let s = seen ctx call.Span name cal
        let index = periodsOf ctx call.Span name s.D p

        if s.Positions.Length = 0 then
            calendarTable s.D 0 -1
        else
            let anchor =
                if isNext then
                    s.Positions.[s.Positions.Length - 1]
                else
                    s.Positions.[0]

            let lo, hi = periodNumbered index (index.[anchor] + (if isNext then 1 else -1))
            calendarTable s.D lo hi

    /// PREVIOUSDAY / NEXTDAY over a calendar: the date before the first visible date, or after the
    /// last, among the calendar's dates: "you can now use PREVIOUSDAY with a calendar based on a
    /// table that doesn't have entries for the weekend to jump from Monday straight to Friday"
    /// (Microsoft Learn, Sparse dates).
    let private dayCalendar (isNext: bool) (name: string) (ctx: Ctx) (call: Call) (cal: BCalendar) : TableVal =
        let s = seen ctx call.Span name cal

        if s.Positions.Length = 0 then
            calendarTable s.D 0 -1
        else
            let at =
                if isNext then
                    s.Positions.[s.Positions.Length - 1] + 1
                else
                    s.Positions.[0] - 1

            if at < 0 || at >= s.D.Rows.Length then
                calendarTable s.D 0 -1
            else
                calendarTable s.D at at

    /// The implementation of a table function over a calendar, by the function's name.
    let private overCalendar (name: string) : (Ctx -> Call -> BCalendar -> TableVal) option =
        match name with
        | "DATESWTD" -> Some(toDateCalendar CalendarPeriod.Week name)
        | "DATESMTD" -> Some(toDateCalendar CalendarPeriod.Month name)
        | "DATESQTD" -> Some(toDateCalendar CalendarPeriod.Quarter name)
        | "DATESYTD" -> Some(toDateCalendar CalendarPeriod.Year name)
        | "STARTOFWEEK" -> Some(edgeCalendar false CalendarPeriod.Week name)
        | "STARTOFMONTH" -> Some(edgeCalendar false CalendarPeriod.Month name)
        | "STARTOFQUARTER" -> Some(edgeCalendar false CalendarPeriod.Quarter name)
        | "STARTOFYEAR" -> Some(edgeCalendar false CalendarPeriod.Year name)
        | "ENDOFWEEK" -> Some(edgeCalendar true CalendarPeriod.Week name)
        | "ENDOFMONTH" -> Some(edgeCalendar true CalendarPeriod.Month name)
        | "ENDOFQUARTER" -> Some(edgeCalendar true CalendarPeriod.Quarter name)
        | "ENDOFYEAR" -> Some(edgeCalendar true CalendarPeriod.Year name)
        | "PREVIOUSWEEK" -> Some(neighbourCalendar false CalendarPeriod.Week name)
        | "PREVIOUSMONTH" -> Some(neighbourCalendar false CalendarPeriod.Month name)
        | "PREVIOUSQUARTER" -> Some(neighbourCalendar false CalendarPeriod.Quarter name)
        | "PREVIOUSYEAR" -> Some(neighbourCalendar false CalendarPeriod.Year name)
        | "NEXTWEEK" -> Some(neighbourCalendar true CalendarPeriod.Week name)
        | "NEXTMONTH" -> Some(neighbourCalendar true CalendarPeriod.Month name)
        | "NEXTQUARTER" -> Some(neighbourCalendar true CalendarPeriod.Quarter name)
        | "NEXTYEAR" -> Some(neighbourCalendar true CalendarPeriod.Year name)
        | "PREVIOUSDAY" -> Some(dayCalendar false name)
        | "NEXTDAY" -> Some(dayCalendar true name)
        | _ -> None

    /// DATESBETWEEN over a calendar: "If StartDate is BLANK ... With a calendar, it starts from the
    /// first value in the column tagged as Day"; "With a calendar, StartDate and EndDate must have
    /// the same data type and the same format as the column tagged as Day. If a value does not
    /// exist in that column, DATESBETWEEN uses it as BLANK" (dax.guide/datesbetween).
    let private datesBetweenCalendar (ev: IEval) (ctx: Ctx) (call: Call) (cal: BCalendar) : TableVal =
        let d = Calendars.data ctx call.Span "DATESBETWEEN" cal

        let position (e: Expr) (whenMissing: int) =
            match ev.Scalar(ctx, e) with
            | DaxValue.Blank -> whenMissing
            | DaxValue.DateTime s ->
                match Array.BinarySearch(d.Days, s) with
                | i when i >= 0 -> i
                | _ -> whenMissing
            | _ ->
                Fail.notSupported
                    ctx
                    e.Span
                    "a boundary of DATESBETWEEN over a calendar that is not a date (it must have the type of the Date column)"

        let lo = position call.Args.[1] 0
        let hi = position call.Args.[2] (d.Rows.Length - 1)
        calendarTable d lo hi

    /// A week function given a date column: "Week functions only work with calendar based time
    /// intelligence" (Microsoft Learn, TOTALWTD and the others); "A date column is not valid and
    /// returns an error" (dax.guide/closingbalanceweek).
    let private weekNeedsCalendar (ctx: Ctx) (call: Call) : 'a =
        Fail.invalid
            ctx
            call.Args.[0].Span
            $"{call.Name.Name.ToUpperInvariant()} works only with a calendar (calendar-based time intelligence)"

    // ---- Registration ----------------------------------------------------------------------------

    let private datesTable
        (name: string)
        (maxArgs: int)
        (check: Expr list -> string option)
        (impl: IEval -> Ctx -> Call -> Dates -> int array)
        : DaxFunction =
        Fn.table name family (1, maxArgs) (fun ev ctx call ->
            match Calendars.tryResolve ctx call.Args.[0], overCalendar name with
            | Some cal, Some over -> over ctx call cal
            | _ ->
                let d = read ev ctx name call.Args.[0]
                tableOf d.Column (impl ev ctx call d))
        |> Fn.withCheck (checks [ checkDates 0; check ])

    let private none: Expr list -> string option = fun _ -> None

    /// FIRSTDATE / LASTDATE. With no date selected the value is BLANK; whether the table is then
    /// empty or one row of BLANK is not documented, so the table is refused.
    let private firstLast (name: string) (isLast: bool) : DaxFunction =
        let table =
            Fn.table name family (1, 1) (fun ev ctx call ->
                let d = read ev ctx name call.Args.[0]

                match firstOrLast isLast d with
                | ValueSome day -> tableOf d.Column [| day |]
                | ValueNone ->
                    Fail.notSupported
                        ctx
                        call.Span
                        $"{name} as a table when no date is selected (whether it is empty or a row of BLANK is not documented; as a value it is BLANK)")

        {
            table with
                Check = checkDates 0
                Type = fun _ _ _ -> StaticType.Known DaxType.DateTime
                Scalar =
                    fun ev ctx call ->
                        match firstOrLast isLast (read ev ctx name call.Args.[0]) with
                        | ValueSome day -> DaxValue.DateTime(float day)
                        | ValueNone -> DaxValue.Blank
        }

    /// DATESMTD / QTD / YTD: the dates of the whole column in the period of the last visible date,
    /// up to that date. "DATESYTD is the very same as the following table expression: VAR
    /// LastVisibleDate = MAX ( 'Date'[Date] ) RETURN FILTER ( ALL ( 'Date'[Date] ), 'Date'[Date] >=
    /// DATE ( YEAR ( LastVisibleDate ), 1, 1 ) && 'Date'[Date] <= LastVisibleDate )" (SQLBI, "Mark
    /// as date table"). DAX Guide's form, DATESBETWEEN ( <Dates>, STARTOFYEAR ( LASTDATE ( <Dates>
    /// ) ), LASTDATE ( <Dates> ) ), is the same dates whenever STARTOFYEAR finds its date; when it
    /// doesn't, the two differ, and the function refuses.
    let private toDate (name: string) (p: Call -> Ctx -> Period) (maxArgs: int) : DaxFunction =
        datesTable name maxArgs (checkYearEnd 1) (fun _ ctx call d ->
            match firstOrLast true d with
            | ValueNone ->
                // Both forms then have BLANK boundaries: an empty table by the first (no date is
                // <= BLANK), every date of the column by the second.
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name} with no date selected (by SQLBI's definition the result is empty, by DAX Guide's it is DATESBETWEEN with two BLANK boundaries, every date of the column)"
            | ValueSome last ->
                let period = p call ctx
                let lo, _ = bounds period last

                // The first date of the period as DAX Guide's form finds it: STARTOF* ( LASTDATE (
                // <Dates> ) ), which other filters can leave empty (the DATESBETWEEN of that form
                // then starts from the first date of the column, SQLBI's from the period's).
                let only = {
                    d with
                        Days = [| last |]
                        Blank = false
                }

                match edge false period ctx call.Span name only with
                | [||] ->
                    Fail.notSupported
                        ctx
                        call.Span
                        $"{name} when other filters hide the first date of the period (STARTOF* is then empty: DAX Guide's definition starts from the first date of the column, SQLBI's from the first of the period)"
                | _ -> range d.U lo last)

    let private yearEndAt (index: int) : Call -> Ctx -> Period =
        fun call ctx -> yearEnd ctx (List.tryItem index call.Args)

    let private always (p: Period) : Call -> Ctx -> Period = fun _ _ -> p

    let functions: DaxFunction list = [
        firstLast "FIRSTDATE" false
        firstLast "LASTDATE" true

        datesTable "STARTOFMONTH" 1 none (fun _ ctx call d -> edge false Period.Month ctx call.Span "STARTOFMONTH" d)
        datesTable "STARTOFQUARTER" 1 none (fun _ ctx call d ->
            edge false Period.Quarter ctx call.Span "STARTOFQUARTER" d)
        datesTable "STARTOFYEAR" 2 (checkYearEnd 1) (fun _ ctx call d ->
            edge false (yearEndAt 1 call ctx) ctx call.Span "STARTOFYEAR" d)
        datesTable "ENDOFMONTH" 1 none (fun _ ctx call d -> edge true Period.Month ctx call.Span "ENDOFMONTH" d)
        datesTable "ENDOFQUARTER" 1 none (fun _ ctx call d -> edge true Period.Quarter ctx call.Span "ENDOFQUARTER" d)
        datesTable "ENDOFYEAR" 2 (checkYearEnd 1) (fun _ ctx call d ->
            edge true (yearEndAt 1 call ctx) ctx call.Span "ENDOFYEAR" d)

        toDate "DATESMTD" (always Period.Month) 1
        toDate "DATESQTD" (always Period.Quarter) 1
        toDate "DATESYTD" (yearEndAt 1) 2

        // "If StartDate is BLANK, DATESBETWEEN starts from the first date in the Dates column"; the
        // same for the end; "the dates used as StartDate and EndDate are inclusive"; "if StartDate
        // is later than EndDate, the result is an empty table" (dax.guide/datesbetween). The dates
        // are those of the whole column, whatever is filtered: "when a report filters the measure by
        // the month of June 2020, the DATESBETWEEN function returns a date range from July 1, 2017
        // until June 30, 2020" (Microsoft Learn, of DATESBETWEEN ( 'Date'[Date], BLANK (), MAX (
        // 'Date'[Date] ) )).
        Fn.table "DATESBETWEEN" family (3, 3) (fun ev ctx call ->
            match Calendars.tryResolve ctx call.Args.[0] with
            | Some cal -> datesBetweenCalendar ev ctx call cal
            | None ->
                let c, u = columnOnly ev ctx "DATESBETWEEN" call.Args.[0]

                let boundary (e: Expr) (whenBlank: float) =
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Blank -> whenBlank
                    | DaxValue.DateTime s -> s
                    | _ ->
                        Fail.notSupported
                            ctx
                            e.Span
                            "a boundary of DATESBETWEEN that is not a date (how a number or a text is read as one is not implemented)"

                let lo = boundary call.Args.[1] Double.NegativeInfinity
                let hi = boundary call.Args.[2] Double.PositiveInfinity
                tableOf c (u.Days |> Array.filter (fun day -> float day >= lo && float day <= hi)))
        |> Fn.withCheck (checkDates 0)

        Fn.table "DATESINPERIOD" family (4, 5) (fun ev ctx call ->
            let c, u = columnOnly ev ctx "DATESINPERIOD" call.Args.[0]
            let unit = interval ctx "DATESINPERIOD" call.Args.[3]
            let n = count ev ctx "DATESINPERIOD" call.Args.[2]

            let start =
                match ev.Scalar(ctx, call.Args.[1]) with
                | DaxValue.DateTime s when s = Math.Floor s && s >= float firstDay && s <= float lastDay -> int s
                | DaxValue.Blank ->
                    // "A BLANK value provided as StartDate uses MIN ( Dates )" (dax.guide): of the
                    // filtered column or of the whole one, it doesn't say.
                    Fail.notSupported ctx call.Args.[1].Span "DATESINPERIOD with a BLANK start date"
                | _ ->
                    Fail.notSupported
                        ctx
                        call.Args.[1].Span
                        "a start date of DATESINPERIOD that is not a date, or has a time of day"

            tableOf c (period ctx call.Span u start n unit))
        |> Fn.withCheck (
            checks [
                checkDates 0
                checkInterval 3
                checkExtra 4 "DATESINPERIOD with an EndBehavior argument (calendar-based time intelligence)"
            ]
        )

        datesTable
            "DATEADD"
            5
            (checks [
                checkInterval 2
                checkExtra 3 "DATEADD with the Extension or Truncation arguments (calendar-based time intelligence)"
            ])
            (fun ev ctx call d ->
                let unit = interval ctx "DATEADD" call.Args.[2]
                shift ctx call.Span "DATEADD" d (count ev ctx "DATEADD" call.Args.[1]) unit)

        // "Internally SAMEPERIODLASTYEAR corresponds to the following call of DATEADD: DATEADD (
        // <Dates>, -1, YEAR )" (dax.guide/sameperiodlastyear).
        datesTable "SAMEPERIODLASTYEAR" 1 none (fun _ ctx call d ->
            shift ctx call.Span "SAMEPERIODLASTYEAR" d -1 "YEAR")

        datesTable "PARALLELPERIOD" 3 (checkInterval 2) (fun ev ctx call d ->
            match interval ctx "PARALLELPERIOD" call.Args.[2] with
            | "DAY" ->
                Fail.invalid ctx call.Args.[2].Span "the interval of PARALLELPERIOD is one of MONTH, QUARTER, YEAR"
            | unit ->
                parallelPeriod ctx call.Span "PARALLELPERIOD" d (count ev ctx "PARALLELPERIOD" call.Args.[1]) unit)

        // "DATEADD ( FIRSTDATE ( <Dates> ), -1, DAY )" (dax.guide/previousday); "DATEADD ( LASTDATE (
        // <Dates> ), 1, DAY )" (dax.guide/nextday).
        datesTable "PREVIOUSDAY" 1 none (fun _ _ _ d ->
            match firstOrLast false d with
            | ValueSome day -> inColumn d.U [ day - 1 ]
            | ValueNone -> [||])
        datesTable "NEXTDAY" 1 none (fun _ _ _ d ->
            match firstOrLast true d with
            | ValueSome day -> inColumn d.U [ day + 1 ]
            | ValueNone -> [||])

        datesTable "PREVIOUSMONTH" 1 none (fun _ _ _ d -> neighbour false Period.Month d)
        datesTable "PREVIOUSQUARTER" 1 none (fun _ _ _ d -> neighbour false Period.Quarter d)
        datesTable "PREVIOUSYEAR" 2 (checkYearEnd 1) (fun _ ctx call d -> neighbour false (yearEndAt 1 call ctx) d)
        datesTable "NEXTMONTH" 1 none (fun _ _ _ d -> neighbour true Period.Month d)
        datesTable "NEXTQUARTER" 1 none (fun _ _ _ d -> neighbour true Period.Quarter d)
        datesTable "NEXTYEAR" 2 (checkYearEnd 1) (fun _ ctx call d -> neighbour true (yearEndAt 1 call ctx) d)

        // The week functions, which take only a calendar (see `weekNeedsCalendar`).
        datesTable "DATESWTD" 1 none (fun _ ctx call _ -> weekNeedsCalendar ctx call)
        datesTable "STARTOFWEEK" 1 none (fun _ ctx call _ -> weekNeedsCalendar ctx call)
        datesTable "ENDOFWEEK" 1 none (fun _ ctx call _ -> weekNeedsCalendar ctx call)
        datesTable "PREVIOUSWEEK" 1 none (fun _ ctx call _ -> weekNeedsCalendar ctx call)
        datesTable "NEXTWEEK" 1 none (fun _ ctx call _ -> weekNeedsCalendar ctx call)

        // "TOTALWTD(<expression>,<calendar>[,<filter>])": "the expression evaluated for the dates in
        // the current week-to-date" (Microsoft Learn, TOTALWTD), as TOTALMTD is with DATESMTD.
        asCalculate "TOTALWTD" false (fun span dates _ -> callOf "DATESWTD" span [ dates ])

        // "The syntax CLOSINGBALANCEWEEK ( <Expression>, <Calendar> [, <Filter>] ) corresponds to
        // CALCULATE ( <Expression>, ENDOFWEEK ( <Calendar> ) [, <Filter>] )"
        // (dax.guide/closingbalanceweek).
        asCalculate "CLOSINGBALANCEWEEK" false (fun span dates _ -> callOf "ENDOFWEEK" span [ dates ])

        // "OPENINGBALANCEWEEK evaluates Expression at the last date of the previous week"
        // (dax.guide/openingbalanceweek): CALCULATETABLE ( ENDOFWEEK, PREVIOUSWEEK ), as for the
        // month ("With a calendar reference ... CALCULATETABLE ( ENDOFMONTH ( <Calendar> ),
        // PREVIOUSMONTH ( <Calendar> ) )": dax.guide/openingbalancemonth).
        asCalculate "OPENINGBALANCEWEEK" false (fun span dates _ ->
            callOf "CALCULATETABLE" span [ callOf "ENDOFWEEK" span [ dates ]; callOf "PREVIOUSWEEK" span [ dates ] ])

        // "CALCULATE ( <Expression>, DATESYTD ( <Dates> [, <YearEndDate>] ) [, <Filter>] )"
        // (dax.guide/totalytd), and the same for the month and the quarter.
        asCalculate "TOTALMTD" false (fun span dates _ -> callOf "DATESMTD" span [ dates ])
        asCalculate "TOTALQTD" false (fun span dates _ -> callOf "DATESQTD" span [ dates ])
        asCalculate "TOTALYTD" true (fun span dates yearEndDate -> callOf "DATESYTD" span (dates :: yearEndDate))

        // "CALCULATE ( <Expression>, ENDOFMONTH ( <Dates> ) [, <Filter>] )"
        // (dax.guide/closingbalancemonth).
        asCalculate "CLOSINGBALANCEMONTH" false (fun span dates _ -> callOf "ENDOFMONTH" span [ dates ])
        asCalculate "CLOSINGBALANCEQUARTER" false (fun span dates _ -> callOf "ENDOFQUARTER" span [ dates ])
        asCalculate "CLOSINGBALANCEYEAR" true (fun span dates yearEndDate ->
            callOf "ENDOFYEAR" span (dates :: yearEndDate))

        // "CALCULATE ( <Expression>, CALCULATETABLE ( ENDOFMONTH ( <Dates> ), PREVIOUSMONTH (
        // <Dates> ) ) [, <Filter>] )" (dax.guide/openingbalancemonth).
        asCalculate "OPENINGBALANCEMONTH" false (fun span dates _ ->
            callOf "CALCULATETABLE" span [ callOf "ENDOFMONTH" span [ dates ]; callOf "PREVIOUSMONTH" span [ dates ] ])
        asCalculate "OPENINGBALANCEQUARTER" false (fun span dates _ ->
            callOf "CALCULATETABLE" span [
                callOf "ENDOFQUARTER" span [ dates ]
                callOf "PREVIOUSQUARTER" span [ dates ]
            ])
        asCalculate "OPENINGBALANCEYEAR" true (fun span dates yearEndDate ->
            callOf "CALCULATETABLE" span [
                callOf "ENDOFYEAR" span (dates :: yearEndDate)
                callOf "PREVIOUSYEAR" span (dates :: yearEndDate)
            ])
    ]
