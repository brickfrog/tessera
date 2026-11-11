namespace Tessera.DaxEngine

// Calendars (calendar-based time intelligence): what a model's calendars tag, and the lattice of
// their categories. The rules that use them are in Calendars.fs; the functions in
// Functions/TimeIntelligence.fs.
//
// "Calendars are metadata definitions added to a table to indicate which columns from that table
// represent what attributes of time" (Microsoft Learn, "Implement time-based calculations in Power
// BI", Calendar-based time intelligence). Each category has "a primary column and can have zero or
// more associated columns"; a column in a group with no category is "time-related"; "The columns
// specified in the sort-by-column property are automatically considered as associated columns"
// (SQLBI, "Introducing calendar-based time intelligence in DAX").

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

/// The categories of Microsoft Learn's table "Available column categories" (TOM's `TimeUnit`
/// without the semester ones, which no page describes).
[<RequireQualifiedAccess>]
type Category =
    | Year
    | QuarterOfYear
    | MonthOfQuarter
    | WeekOfMonth
    | DayOfWeek
    | Quarter
    | MonthOfYear
    | WeekOfQuarter
    | DayOfMonth
    | Month
    | WeekOfYear
    | DayOfQuarter
    | Week
    | DayOfYear
    | Date

/// The four periods the time intelligence functions work in.
[<RequireQualifiedAccess>]
type CalendarPeriod =
    | Week
    | Month
    | Quarter
    | Year

module Category =

    let all = [
        Category.Year
        Category.QuarterOfYear
        Category.MonthOfQuarter
        Category.WeekOfMonth
        Category.DayOfWeek
        Category.Quarter
        Category.MonthOfYear
        Category.WeekOfQuarter
        Category.DayOfMonth
        Category.Month
        Category.WeekOfYear
        Category.DayOfQuarter
        Category.Week
        Category.DayOfYear
        Category.Date
    ]

    let name (c: Category) =
        match c with
        | Category.Year -> "Year"
        | Category.QuarterOfYear -> "Quarter of Year"
        | Category.MonthOfQuarter -> "Month of Quarter"
        | Category.WeekOfMonth -> "Week of Month"
        | Category.DayOfWeek -> "Day of Week"
        | Category.Quarter -> "Quarter"
        | Category.MonthOfYear -> "Month of Year"
        | Category.WeekOfQuarter -> "Week of Quarter"
        | Category.DayOfMonth -> "Day of Month"
        | Category.Month -> "Month"
        | Category.WeekOfYear -> "Week of Year"
        | Category.DayOfQuarter -> "Day of Quarter"
        | Category.Week -> "Week"
        | Category.DayOfYear -> "Day of Year"
        | Category.Date -> "Date"

    /// The arrows of Microsoft Learn's lattice diagram ("Lattice structure diagram of all calendar
    /// categories", section "How context clearing works"): each category points at its
    /// dependencies. "For example, "Month" is dependent on "Year", "Quarter of Year", "Month Of
    /// Quarter", "Quarter", and "Month of Year"" (the same page), which these arrows give.
    let parents (c: Category) : Category list =
        match c with
        | Category.Quarter -> [ Category.Year; Category.QuarterOfYear ]
        | Category.MonthOfYear -> [ Category.QuarterOfYear; Category.MonthOfQuarter ]
        | Category.WeekOfQuarter -> [ Category.MonthOfQuarter; Category.WeekOfMonth ]
        | Category.DayOfMonth -> [ Category.WeekOfMonth; Category.DayOfWeek ]
        | Category.Month -> [ Category.Quarter; Category.MonthOfYear ]
        | Category.WeekOfYear -> [ Category.MonthOfYear; Category.WeekOfQuarter ]
        | Category.DayOfQuarter -> [ Category.WeekOfQuarter; Category.DayOfMonth ]
        | Category.Week -> [ Category.Month; Category.WeekOfYear ]
        | Category.DayOfYear -> [ Category.WeekOfYear; Category.DayOfQuarter ]
        | Category.Date -> [ Category.Week; Category.DayOfYear ]
        | _ -> []

    /// Every category a category depends on, directly or not ("all categories above X").
    let dependencies (c: Category) : Set<Category> =
        let rec go (seen: Set<Category>) (todo: Category list) =
            match todo with
            | [] -> seen
            | x :: rest ->
                let next = parents x |> List.filter (fun p -> not (seen.Contains p))
                go (Set.union seen (Set.ofList next)) (next @ rest)

        go Set.empty [ c ]

    /// Every category that depends on this one ("all categories below X").
    let dependents (c: Category) : Set<Category> =
        all |> List.filter (fun d -> (dependencies d).Contains c) |> Set.ofList

    /// "When context is set on a column or its associated sort by column that is tagged in a
    /// calendar, prior filter context is cleared on: All category dependencies of X ... All
    /// category dependents of both X and its dependencies" (Microsoft Learn, "How context
    /// clearing works"). X itself is included: each category "represents all columns (primary and
    /// associated) tagged to that category". Learn's example: from Quarter this is Year, Quarter
    /// of Year, Quarter, Month of Year, Month, Week of Year, Week, Day of Year and Date.
    let cleared (c: Category) : Set<Category> =
        let up = Set.add c (dependencies c)
        up |> Seq.fold (fun acc x -> Set.union acc (dependents x)) up

    let periodName (p: CalendarPeriod) =
        match p with
        | CalendarPeriod.Week -> "week"
        | CalendarPeriod.Month -> "month"
        | CalendarPeriod.Quarter -> "quarter"
        | CalendarPeriod.Year -> "year"

    /// The sets of categories that identify a period, from the top down: "make sure to assign at
    /// least a primary column to one of the following sets of categories: Week; Week of Year, Year;
    /// Week of Quarter, Quarter; Week of Quarter, Quarter of Year, Year; Week of Month, Month; Week
    /// of Month, Month of Year, Year; Week of Month, Month of Quarter, Quarter; Week of Month, Month
    /// of Quarter, Quarter of Year, Year" (Microsoft Learn, "Period Uniqueness"), and the same
    /// paths through the lattice for the other periods.
    let routes (p: CalendarPeriod) : Category list list =
        match p with
        | CalendarPeriod.Year -> [ [ Category.Year ] ]
        | CalendarPeriod.Quarter -> [ [ Category.Quarter ]; [ Category.Year; Category.QuarterOfYear ] ]
        | CalendarPeriod.Month -> [
            [ Category.Month ]
            [ Category.Year; Category.MonthOfYear ]
            [ Category.Quarter; Category.MonthOfQuarter ]
            [ Category.Year; Category.QuarterOfYear; Category.MonthOfQuarter ]
          ]
        | CalendarPeriod.Week -> [
            [ Category.Week ]
            [ Category.Year; Category.WeekOfYear ]
            [ Category.Quarter; Category.WeekOfQuarter ]
            [ Category.Year; Category.QuarterOfYear; Category.WeekOfQuarter ]
            [ Category.Month; Category.WeekOfMonth ]
            [ Category.Year; Category.MonthOfYear; Category.WeekOfMonth ]
            [ Category.Quarter; Category.MonthOfQuarter; Category.WeekOfMonth ]
            [
                Category.Year
                Category.QuarterOfYear
                Category.MonthOfQuarter
                Category.WeekOfMonth
            ]
          ]

    /// The period a category is about, if it is one of the four's.
    let periodOf (c: Category) : CalendarPeriod option =
        match c with
        | Category.Year -> Some CalendarPeriod.Year
        | Category.Quarter
        | Category.QuarterOfYear -> Some CalendarPeriod.Quarter
        | Category.Month
        | Category.MonthOfYear
        | Category.MonthOfQuarter -> Some CalendarPeriod.Month
        | Category.Week
        | Category.WeekOfYear
        | Category.WeekOfQuarter
        | Category.WeekOfMonth -> Some CalendarPeriod.Week
        | _ -> None

    /// The order of a calendar's primary columns in the tables its functions return: the order
    /// dax.guide's examples show (DATESWTD: Date, the week of the year, the week, the month of the
    /// year, the month, the quarter of the year, the quarter, the year; DATESBETWEEN: Date, the
    /// month of the year, the month, the quarter, the year).
    let resultOrder = [
        Category.Date
        Category.DayOfWeek
        Category.DayOfMonth
        Category.DayOfQuarter
        Category.DayOfYear
        Category.WeekOfMonth
        Category.WeekOfQuarter
        Category.WeekOfYear
        Category.Week
        Category.MonthOfQuarter
        Category.MonthOfYear
        Category.Month
        Category.QuarterOfYear
        Category.Quarter
        Category.Year
    ]

/// One category of a calendar: its primary column and the columns associated with it (the sort-by
/// columns of both included).
type CalendarGroup = {
    Category: Category
    Primary: BColumn
    Associated: BColumn list
}

/// A calendar of the model.
[<Sealed; AllowNullLiteral>]
type BCalendar(name: string, table: BTable, groups: CalendarGroup list, timeRelated: BColumn list) =
    member _.Name = name
    member _.Table = table
    member _.Groups = groups
    /// Columns in the calendar with no category, with their sort-by columns.
    member _.TimeRelated = timeRelated
    /// Why the calendar can't be used, when it can't.
    member val Problem: string option = None with get, set

    member _.Group(c: Category) : CalendarGroup option =
        groups |> List.tryFind (fun g -> g.Category = c)

    member _.Has(c: Category) =
        groups |> List.exists (fun g -> g.Category = c)

    /// The columns its functions return: "A table containing the primary column of each calendar
    /// category and the time-related columns" (dax.guide/dateswtd).
    member val ResultColumns: BColumn array =
        [|
            for c in Category.resultOrder do
                match groups |> List.tryFind (fun g -> g.Category = c) with
                | Some g -> g.Primary
                | None -> ()
            yield! timeRelated
        |]

/// The calendars of a model, by name and by table.
[<Sealed>]
type CalendarSet() =
    member val ByName = Dictionary<string, BCalendar>(StringComparer.OrdinalIgnoreCase) with get
    member val ByTable = Dictionary<BTable, BCalendar list>(HashIdentity.Reference) with get
    /// The category of each column some calendar tags (primary, associated or sort-by), across
    /// the calendars: "Columns must have a consistent category across calendars" (Learn).
    member val CategoryOf = Dictionary<BColumn, Category>(HashIdentity.Reference) with get
    /// The columns some calendar has as time-related.
    member val TimeRelated = HashSet<BColumn>(HashIdentity.Reference) with get
    /// Tables whose calendars can't be used, and why: every filter on a column they tag is then
    /// refused.
    member val TableProblem = Dictionary<BTable, string>(HashIdentity.Reference) with get

    /// Whether a calendar tags the column, in a category or as time-related.
    member this.Tags(c: BColumn) =
        this.CategoryOf.ContainsKey c || this.TimeRelated.Contains c

    /// The columns of the table's calendars in these categories.
    member this.ColumnsIn(table: BTable, categories: Set<Category>) : HashSet<BColumn> =
        let out = HashSet<BColumn>(HashIdentity.Reference)

        for KeyValue(c, cat) in this.CategoryOf do
            if obj.ReferenceEquals(c.Table, table) && categories.Contains cat then
                out.Add c |> ignore

        out

module CalendarRegistry =

    let private sets = ConditionalWeakTable<Model, CalendarSet>()

    let register (model: Model) (set: CalendarSet) = sets.AddOrUpdate(model, set)

    /// The model's calendars, if it has any.
    let tryGet (model: Model) : CalendarSet option =
        match sets.TryGetValue model with
        | true, s when s.ByName.Count > 0 -> Some s
        | _ -> None

/// Where Context.fs's `Filters.add` asks the calendar rules (Calendars.fs, compiled after it) what
/// a new filter clears: set by `Calendars.install`. The arguments are the context, the filters
/// being added and whether they are added as KEEPFILTERS does; the result is the context to add
/// them to.
module CalendarHooks =
    let mutable beforeAdd: FilterCtx -> Filter list -> bool -> FilterCtx =
        fun fc _ _ -> fc

    /// Called by `Context.transition` before a row context's filters are added (`beforeAdd` then
    /// runs too): a context transition is refused where calendar context clearing would change it.
    let mutable onTransition: FilterCtx -> Filter list -> unit = fun _ _ -> ()
