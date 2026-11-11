/// Calendar-based time intelligence in the local DAX engine. Every expected value is worked out by
/// hand from the rules the sources state (cited in Engine/Calendars.fs and
/// Functions/TimeIntelligence.fs), on this data:
///
/// 'Date': every day from 2024-12-01 to 2025-02-28 (31 + 31 + 28 days). Year, YearMonth (202412),
/// MonthOfYear (12), YearQuarter (20244), WeekKey (the yyyymmdd of the Sunday that starts the
/// week: weeks run Sunday to Saturday), DayOfWeek ("Sun"...), IsWorkingDay.
/// Sales: one row per day, Amount = the day of the month (so a month of 31 days sums to 496, of 28
/// to 406), related to 'Date'[Date].
///
/// Calendars: Gregorian (Year, Quarter = YearQuarter, Month = YearMonth, Month of Year =
/// MonthOfYear, Date) and Weekly (Week = WeekKey, Date). DayOfWeek and IsWorkingDay are tagged by
/// neither.
module Tessera.Tests.EngineCalendarTests

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private days =
    [ 0..89 ] |> List.map (fun i -> DateTime(2024, 12, 1).AddDays(float i))

let private weekStart (d: DateTime) = d.AddDays(-(float (int d.DayOfWeek)))

let private dateTable =
    table "Date" [
        "Date", DataType.DateTime
        "Year", DataType.Int64
        "YearMonth", DataType.Int64
        "MonthOfYear", DataType.Int64
        "YearQuarter", DataType.Int64
        "WeekKey", DataType.Int64
        "DayOfWeek", DataType.String
        "IsWorkingDay", DataType.Boolean
        "FiscalMonth", DataType.Int64
    ] [
        for d in days ->
            let w = weekStart d

            [
                box d
                box d.Year
                box (d.Year * 100 + d.Month)
                box d.Month
                box (d.Year * 10 + (d.Month - 1) / 3 + 1)
                box (w.Year * 10000 + w.Month * 100 + w.Day)
                box (d.DayOfWeek.ToString().Substring(0, 3))
                box (d.DayOfWeek <> DayOfWeek.Saturday && d.DayOfWeek <> DayOfWeek.Sunday)
                box (d.Year * 100 + d.Month)
            ]
    ]

let private model = {
    Tables = [
        dateTable
        table "Sales" [ "Date", DataType.DateTime; "Amount", DataType.Int64 ] [ for d in days -> [ box d; box d.Day ] ]
    ]
    Rels = [ rel ("Sales", "Date") ("Date", "Date") ]
    Measures = [ "Sales", "Total", "SUM ( Sales[Amount] )" ]
    Functions = []
}

let private addCalendar
    (db: Database)
    (tableName: string)
    (name: string)
    (groups: (TimeUnit * string * string list) list)
    (related: string list)
    =
    let t = db.Model.Tables.[tableName]
    let cal = new Calendar(Name = name)

    for (u, primary, associated) in groups do
        let a = new TimeUnitColumnAssociation(u, PrimaryColumn = t.Columns.[primary])

        for c in associated do
            a.AssociatedColumns.Add(t.Columns.[c])

        cal.CalendarColumnGroups.Add a

    if not related.IsEmpty then
        let r = new TimeRelatedColumnGroup()

        for c in related do
            r.Columns.Add(t.Columns.[c])

        cal.CalendarColumnGroups.Add r

    t.Calendars.Add cal

let private gregorian = [
    TimeUnit.Year, "Year", []
    TimeUnit.Quarter, "YearQuarter", []
    TimeUnit.Month, "YearMonth", []
    TimeUnit.MonthOfYear, "MonthOfYear", []
    TimeUnit.Date, "Date", []
]

let private weekly = [ TimeUnit.Week, "WeekKey", []; TimeUnit.Date, "Date", [] ]

let private prepareWith (configure: Database -> unit) : Engine.Prepared =
    let db, store = build model
    configure db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private standard =
    lazy
        (prepareWith (fun db ->
            addCalendar db "Date" "Gregorian" gregorian []
            addCalendar db "Date" "Weekly" weekly []))

let private p () = standard.Value

let private date (y: int) (m: int) (d: int) = DateTime(y, m, d)

let private code (p: Engine.Prepared) (dax: string) =
    match Engine.evaluateQuery p dax with
    | Ok _ -> failtestf "expected an error\n%s" dax
    | Error e -> TessError.code e

/// The first and last date and the number of rows of a table expression of the Date table.
let private span (p: Engine.Prepared) (filter: string) (table: string) =
    rows
        p
        $"EVALUATE CALCULATETABLE ( ROW ( \"first\", MINX ( {table}, 'Date'[Date] ), \"last\", MAXX ( {table}, 'Date'[Date] ), \"n\", COUNTROWS ( {table} ) ), {filter} )"

let private spanIs (first: DateTime) (last: DateTime) (n: int) (filter: string) (table: string) =
    Expect.equal (span (p ()) filter table) (expected [ [ box first; box last; box n ] ]) $"{table} under {filter}"

let private emptyUnder (filter: string) (table: string) =
    Expect.equal
        (rows (p ()) $"EVALUATE CALCULATETABLE ( ROW ( \"n\", COUNTROWS ( {table} ) ), {filter} )")
        (expected [ [ null ] ])
        $"{table} under {filter}"

[<Tests>]
let tests =
    testList "Engine calendars" [
        testList "the tables the functions return" [
            test "the primary column of each category, in dax.guide's order" {
                let r =
                    query (p ()) "EVALUATE CALCULATETABLE ( DATESMTD ( Gregorian ), 'Date'[Date] = dt\"2025-01-02\" )"

                Expect.equal
                    (r.Columns |> List.map _.Name)
                    [
                        "Date[Date]"
                        "Date[MonthOfYear]"
                        "Date[YearMonth]"
                        "Date[YearQuarter]"
                        "Date[Year]"
                    ]
                    "columns"

                Expect.equal
                    (r.Rows |> List.map List.ofArray)
                    (expected [
                        [ box (date 2025 1 1); box 1; box 202501; box 20251; box 2025 ]
                        [ box (date 2025 1 2); box 1; box 202501; box 20251; box 2025 ]
                    ])
                    "rows"
            }

            test "DATESMTD / QTD / YTD / WTD end with the last visible day when the date is filtered" {
                let f = "'Date'[Date] = dt\"2025-01-15\""
                spanIs (date 2025 1 1) (date 2025 1 15) 15 f "DATESMTD ( Gregorian )"
                spanIs (date 2025 1 1) (date 2025 1 15) 15 f "DATESQTD ( Gregorian )"
                spanIs (date 2025 1 1) (date 2025 1 15) 15 f "DATESYTD ( Gregorian )"
                // 2025-01-15 is a Wednesday; its week starts on Sunday 12.
                spanIs (date 2025 1 12) (date 2025 1 15) 4 f "DATESWTD ( Weekly )"
            }

            test "with only columns no calendar tags filtered, the whole period of the last visible date" {
                // The last Monday is February 24; its week runs to March 1, after the data.
                let f = "'Date'[DayOfWeek] = \"Mon\""
                spanIs (date 2025 2 1) (date 2025 2 28) 28 f "DATESMTD ( Gregorian )"
                spanIs (date 2025 2 23) (date 2025 2 28) 6 f "DATESWTD ( Weekly )"
                spanIs (date 2025 1 1) (date 2025 2 28) 59 f "DATESYTD ( Gregorian )"
            }

            test "with a month filtered, to the end of the last visible month, not past the period" {
                spanIs
                    (date 2025 1 1)
                    (date 2025 2 28)
                    59
                    "'Date'[YearMonth] IN { 202501, 202502 }"
                    "DATESYTD ( Gregorian )"

                spanIs (date 2025 1 1) (date 2025 1 31) 31 "'Date'[MonthOfYear] = 1" "DATESQTD ( Gregorian )"
                // A year filtered: the whole year, but DATESMTD stops at the end of the month.
                spanIs (date 2024 12 1) (date 2024 12 31) 31 "'Date'[Year] = 2024" "DATESMTD ( Gregorian )"
            }

            test "STARTOF / ENDOF, PREVIOUS / NEXT" {
                let f = "'Date'[Date] = dt\"2025-01-15\""
                spanIs (date 2025 1 1) (date 2025 1 1) 1 f "STARTOFMONTH ( Gregorian )"
                spanIs (date 2025 1 31) (date 2025 1 31) 1 f "ENDOFMONTH ( Gregorian )"
                spanIs (date 2025 1 12) (date 2025 1 12) 1 f "STARTOFWEEK ( Weekly )"
                spanIs (date 2025 1 18) (date 2025 1 18) 1 f "ENDOFWEEK ( Weekly )"
                spanIs (date 2024 12 1) (date 2024 12 31) 31 f "PREVIOUSMONTH ( Gregorian )"
                spanIs (date 2025 2 1) (date 2025 2 28) 28 f "NEXTMONTH ( Gregorian )"
                spanIs (date 2025 1 5) (date 2025 1 11) 7 f "PREVIOUSWEEK ( Weekly )"
                spanIs (date 2025 1 19) (date 2025 1 25) 7 f "NEXTWEEK ( Weekly )"
                spanIs (date 2024 12 1) (date 2024 12 31) 31 f "PREVIOUSYEAR ( Gregorian )"
                spanIs (date 2025 1 14) (date 2025 1 14) 1 f "PREVIOUSDAY ( Gregorian )"
                spanIs (date 2025 1 16) (date 2025 1 16) 1 f "NEXTDAY ( Weekly )"
                // The first week of the data starts on Sunday, December 1.
                emptyUnder "'Date'[Date] = dt\"2024-12-03\"" "PREVIOUSWEEK ( Weekly )"
                emptyUnder "'Date'[YearMonth] = 202412" "PREVIOUSMONTH ( Gregorian )"
                emptyUnder "'Date'[Date] = dt\"2025-02-28\"" "NEXTDAY ( Gregorian )"
            }

            test "PREVIOUSDAY skips the days the table doesn't have" {
                // Microsoft Learn: with no rows for the weekend, PREVIOUSDAY jumps from Monday to
                // Friday. Here: the calendar's table without Saturdays and Sundays.
                let weekdays = {
                    dateTable with
                        Rows = dateTable.Rows |> List.filter (fun r -> r.[7] :?> bool)
                }

                let db, store =
                    build {
                        model with
                            Tables = [ weekdays; model.Tables.[1] ]
                    }

                addCalendar db "Date" "Gregorian" gregorian []

                let p =
                    match Engine.prepare db store with
                    | Ok p -> p
                    | Error e -> failtestf "%s" (TessError.message e)

                // Monday, January 13 2025.
                Expect.equal
                    (span p "'Date'[Date] = dt\"2025-01-13\"" "PREVIOUSDAY ( Gregorian )")
                    (expected [ [ box (date 2025 1 10); box (date 2025 1 10); box 1 ] ])
                    "Friday"
            }

            test "DATESBETWEEN: BLANK and a date the calendar doesn't have mean its first or last date" {
                spanIs
                    (date 2024 12 1)
                    (date 2024 12 5)
                    5
                    "ALL ( 'Date' )"
                    "DATESBETWEEN ( Gregorian, BLANK (), dt\"2024-12-05\" )"

                spanIs
                    (date 2024 12 1)
                    (date 2024 12 5)
                    5
                    "ALL ( 'Date' )"
                    "DATESBETWEEN ( Gregorian, dt\"2024-12-03\" + TIME ( 12, 0, 0 ), dt\"2024-12-05\" )"

                spanIs
                    (date 2024 12 1)
                    (date 2025 2 28)
                    90
                    "ALL ( 'Date' )"
                    "DATESBETWEEN ( Gregorian, dt\"2030-01-01\", dt\"2030-12-31\" )"

                emptyUnder "ALL ( 'Date' )" "DATESBETWEEN ( Gregorian, dt\"2025-01-05\", dt\"2025-01-01\" )"
            }
        ]

        testList "CALCULATE with a calendar function" [
            test "TOTALYTD by month" {
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"ytd\", TOTALYTD ( [Total], Gregorian ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [ [ box 202412; box 496 ]; [ box 202501; box 496 ]; [ box 202502; box 902 ] ])
                    "ytd"
            }

            test "the filters on columns no calendar tags stay" {
                // Sundays: December 1, 8, 15, 22, 29 (75); January 5, 12, 19, 26 (62); February 2,
                // 9, 16, 23 (50).
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], TREATAS ( { \"Sun\" }, 'Date'[DayOfWeek] ), \"ytd\", TOTALYTD ( [Total], Gregorian ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [ [ box 202412; box 75 ]; [ box 202501; box 62 ]; [ box 202502; box 112 ] ])
                    "ytd of Sundays"
            }

            test "TOTALWTD and TOTALMTD by day" {
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Date], TREATAS ( { 20250105 }, 'Date'[WeekKey] ), \"wtd\", TOTALWTD ( [Total], Weekly ) ) ORDER BY 'Date'[Date]")
                    (expected [ for d in 5..11 -> [ box (date 2025 1 d); box ((5 + d) * (d - 4) / 2) ] ])
                    "week of January 5"

                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Date], TREATAS ( { 202501 }, 'Date'[YearMonth] ), \"mtd\", TOTALMTD ( [Total], Gregorian ) ) ORDER BY 'Date'[Date]")
                    (expected [ for d in 1..31 -> [ box (date 2025 1 d); box (d * (d + 1) / 2) ] ])
                    "January"
            }

            test "the balances" {
                // OPENINGBALANCEMONTH: the last day of the month before (December 31: 31; January
                // 31: 31); none before December. CLOSINGBALANCEWEEK of January: the week of January
                // 31 ends on Saturday, February 1 (dax.guide: "This date can be after the end of
                // the month"). OPENINGBALANCEWEEK of January: January 1 is in the week from December
                // 29; the week before ends on December 28.
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"open\", OPENINGBALANCEMONTH ( [Total], Gregorian ), \"closeWeek\", CLOSINGBALANCEWEEK ( [Total], Weekly ), \"openWeek\", OPENINGBALANCEWEEK ( [Total], Weekly ), \"closeMonth\", CLOSINGBALANCEMONTH ( [Total], Gregorian ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [
                        [ box 202412; null; box 4; null; box 31 ]
                        [ box 202501; box 31; box 1; box 28; box 31 ]
                        [ box 202502; box 31; box 28; box 25; box 28 ]
                    ])
                    "balances"
            }
        ]

        testList "context clearing" [
            test "a filter on the year clears the month, not the month of the year" {
                // Year's dependents are Quarter, Month, Week and Date; Month of Year is not one.
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"y2025\", CALCULATE ( [Total], 'Date'[Year] = 2025 ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [ [ box 202412; box 902 ]; [ box 202501; box 902 ]; [ box 202502; box 902 ] ])
                    "by month"

                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[MonthOfYear], \"y2025\", CALCULATE ( [Total], 'Date'[Year] = 2025 ) ) ORDER BY 'Date'[MonthOfYear]")
                    (expected [ [ box 1; box 496 ]; [ box 2; box 406 ] ])
                    "by month of year"
            }

            test "a filter on a column no calendar tags clears nothing" {
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"mon\", CALCULATE ( [Total], 'Date'[DayOfWeek] = \"Mon\" ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [
                        [ box 202412; box (2 + 9 + 16 + 23 + 30) ]
                        [ box 202501; box (6 + 13 + 20 + 27) ]
                        [ box 202502; box (3 + 10 + 17 + 24) ]
                    ])
                    "Mondays"
            }

            test "KEEPFILTERS on a tagged column while others are filtered is refused" {
                Expect.equal
                    (code
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"k\", CALCULATE ( [Total], KEEPFILTERS ( 'Date'[Year] = 2025 ) ) )")
                    "not-supported"
                    "KEEPFILTERS"
            }

            test "grouping by a date under a filter on its week is fine" {
                // dax.guide/dateswtd: SUMMARIZECOLUMNS by date with TREATAS on the week. The group's
                // date is in the week, so clearing the week's filter or not shows the same rows.
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Date], TREATAS ( { 20250112 }, 'Date'[WeekKey] ), \"t\", [Total] ) ORDER BY 'Date'[Date]")
                    (expected [ for d in 12..18 -> [ box (date 2025 1 d); box d ] ])
                    "the week"
            }

            test "grouping by the year under a filter on the month is refused" {
                // SUMMARIZECOLUMNS combines the month's filter with the year's group; whether the
                // year, a group-by column, clears the month is not documented, and it would matter.
                Expect.equal
                    (code
                        (p ())
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Year], TREATAS ( { 202501 }, 'Date'[YearMonth] ), \"t\", [Total] )")
                    "not-supported"
                    "group by year"
            }

            test "a filter a context transition leaves is not cleared" {
                // The transition sets the date; the week's filter stays (whether it would be
                // cleared is not documented), and DATESMTD of a calendar without weeks then meets
                // both a filtered date and another calendar's week: refused.
                Expect.equal
                    (code
                        (p ())
                        "EVALUATE CALCULATETABLE ( ROW ( \"t\", SUMX ( VALUES ( 'Date'[Date] ), TOTALMTD ( [Total], Gregorian ) ) ), 'Date'[WeekKey] = 20250105 )")
                    "not-supported"
                    "week and date"
            }

            test "a context transition on the year under a filter on the month is refused" {
                // Whether clearing applies to a context transition is not documented.
                Expect.equal
                    (code
                        (p ())
                        "EVALUATE CALCULATETABLE ( ROW ( \"t\", SUMX ( VALUES ( 'Date'[Year] ), [Total] ) ), 'Date'[YearMonth] = 202501 )")
                    "not-supported"
                    "transition"

                // Iterating the dates of the month: each date is in the month either way.
                Expect.equal
                    (rows
                        (p ())
                        "EVALUATE CALCULATETABLE ( ROW ( \"t\", SUMX ( VALUES ( 'Date'[Date] ), [Total] ) ), 'Date'[YearMonth] = 202501 )")
                    (expected [ [ box 496 ] ])
                    "dates"
            }

            test "without calendars nothing changes" {
                let plain = prepare model

                Expect.equal
                    (rows
                        plain
                        "EVALUATE SUMMARIZECOLUMNS ( 'Date'[YearMonth], \"y2025\", CALCULATE ( [Total], 'Date'[Year] = 2025 ) ) ORDER BY 'Date'[YearMonth]")
                    (expected [ [ box 202501; box 496 ]; [ box 202502; box 406 ] ])
                    "classic"
            }
        ]

        testList "errors and refusals" [
            test "a week function needs a calendar with weeks" {
                Expect.equal
                    (code (p ()) "EVALUATE ROW ( \"x\", TOTALWTD ( [Total], 'Date'[Date] ) )")
                    "dax-invalid"
                    "date column"

                Expect.equal
                    (code (p ()) "EVALUATE ROW ( \"x\", TOTALWTD ( [Total], Gregorian ) )")
                    "dax-invalid"
                    "no week"

                Expect.equal
                    (code (p ()) "EVALUATE ROW ( \"x\", TOTALYTD ( [Total], Weekly ) )")
                    "dax-invalid"
                    "no year"
            }

            test "a year end date with a calendar is an error" {
                Expect.equal (code (p ()) "EVALUATE DATESYTD ( Gregorian, \"6/30\" )") "dax-invalid" "DATESYTD"

                Expect.equal
                    (code (p ()) "EVALUATE ROW ( \"x\", TOTALYTD ( [Total], Gregorian, \"6/30\" ) )")
                    "dax-invalid"
                    "TOTALYTD"
            }

            test "a filter on a column another calendar tags as a week is an error for a calendar without weeks" {
                Expect.equal
                    (code (p ()) "EVALUATE CALCULATETABLE ( DATESYTD ( Gregorian ), 'Date'[WeekKey] = 20250105 )")
                    "dax-error"
                    "week filter"
            }

            test "what is not implemented over a calendar is refused" {
                for f in
                    [
                        "DATEADD ( Gregorian, -1, MONTH )"
                        "SAMEPERIODLASTYEAR ( Gregorian )"
                        "PARALLELPERIOD ( Gregorian, -1, MONTH )"
                        "DATESINPERIOD ( Gregorian, dt\"2025-01-01\", 1, MONTH )"
                        "FIRSTDATE ( Gregorian )"
                    ] do
                    Expect.equal (code (p ()) $"EVALUATE {f}") "not-supported" f
            }

            test "a calendar named like a table is refused" {
                let p = prepareWith (fun db -> addCalendar db "Date" "Sales" gregorian [])

                Expect.equal
                    (code p "EVALUATE CALCULATETABLE ( DATESMTD ( Sales ), 'Date'[Date] = dt\"2025-01-02\" )")
                    "not-supported"
                    "Sales"
            }

            test "a calendar with a semester is refused wherever its columns are filtered" {
                let p =
                    prepareWith (fun db ->
                        addCalendar db "Date" "Odd" [
                            TimeUnit.SemesterOfYear, "MonthOfYear", []
                            TimeUnit.Date, "Date", []
                        ] [])

                Expect.equal
                    (code p "EVALUATE CALCULATETABLE ( VALUES ( 'Date'[Year] ), 'Date'[Date] = dt\"2025-01-02\" )")
                    "not-supported"
                    "semester"
            }

            test "a column only another calendar tags, filtered, is refused by the to-date functions" {
                let p =
                    prepareWith (fun db ->
                        addCalendar db "Date" "Gregorian" gregorian []

                        addCalendar db "Date" "Fiscal" [
                            TimeUnit.Year, "Year", []
                            TimeUnit.Month, "FiscalMonth", []
                            TimeUnit.Date, "Date", []
                        ] [])

                Expect.equal
                    (code p "EVALUATE CALCULATETABLE ( DATESYTD ( Fiscal ), 'Date'[YearMonth] = 202501 )")
                    "not-supported"
                    "Gregorian's month"

                Expect.equal
                    (span p "'Date'[FiscalMonth] = 202501" "DATESYTD ( Fiscal )")
                    (expected [ [ box (date 2025 1 1); box (date 2025 1 31); box 31 ] ])
                    "its own month"
            }

            test "a calendar of the date column of a date table alone is refused" {
                let p =
                    prepareWith (fun db -> addCalendar db "Date" "Days" [ TimeUnit.Date, "Date", [] ] [])

                Expect.equal
                    (code p "EVALUATE CALCULATETABLE ( PREVIOUSDAY ( Days ), 'Date'[Date] = dt\"2025-01-02\" )")
                    "not-supported"
                    "Days"
            }

            test "time-related columns of another calendar under a calendar's result are refused" {
                let p =
                    prepareWith (fun db ->
                        addCalendar db "Date" "Gregorian" gregorian []
                        addCalendar db "Date" "Working" [ TimeUnit.Date, "Date", [] ] [ "IsWorkingDay" ])

                Expect.equal
                    (code
                        p
                        "EVALUATE ROW ( \"x\", CALCULATE ( TOTALMTD ( [Total], Gregorian ), 'Date'[IsWorkingDay] = TRUE (), 'Date'[Date] = dt\"2025-01-15\" ) )")
                    "not-supported"
                    "time-related"
            }
        ]

        test "coverage accepts a calendar as the dates argument" {
            let db, _ = build model
            addCalendar db "Date" "Gregorian" gregorian []

            db.Model.Tables.["Sales"].Measures.Add(
                new Measure(
                    Name = "YTD",
                    Expression = "TOTALYTD ( [Total], Gregorian ) + TOTALWTD ( [Total], Gregorian )"
                )
            )

            let ytd =
                Engine.coverage db
                |> List.filter (fun (r, _) -> (ObjectRef.format r).Contains "YTD")

            Expect.equal ytd.Length 1 "the measure is in the report"
            Expect.isEmpty (snd ytd.Head) "nothing unsupported"
        }
    ]
