/// Time intelligence in the local DAX engine: results the sources publish (each with its source),
/// the edges worked out by hand from the rules they state, each function against its documented
/// expansion or an independent oracle on random selections of dates, the date-table rule, and
/// what the engine refuses.
///
/// Sources, by the short names used below:
///   SQLBI-DP   https://www.sqlbi.com/articles/differences-between-dateadd-and-parallelperiod-in-dax/
///   SQLBI-MDT  https://www.sqlbi.com/articles/mark-as-date-table/
///   SQLBI-TI   https://www.sqlbi.com/articles/time-intelligence-in-power-bi-desktop/
///   DG/<f>     https://dax.guide/<f>/
///   MS/<f>     https://learn.microsoft.com/en-us/dax/<f>-function-dax
module Tessera.Tests.EngineTimeIntelligenceTests

open System
open System.Text.RegularExpressions
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

// ---- Models --------------------------------------------------------------------------------------

let private day (y: int) (m: int) (d: int) = DateTime(y, m, d)

let private daysFrom (first: DateTime) (last: DateTime) : DateTime list = [
    for i in 0 .. int (last - first).TotalDays -> first.AddDays(float i)
]

/// A row of the Date table: Date, Year, MonthNo, Quarter (1 to 4), YearMonth (Year * 100 + MonthNo)
/// and FiscalYear (the year in which the year that ends on June 30 ends).
let private dateRow (x: DateTime) : obj list = [
    box x
    box x.Year
    box x.Month
    box ((x.Month - 1) / 3 + 1)
    box (x.Year * 100 + x.Month)
    box (if x.Month > 6 then x.Year + 1 else x.Year)
]

let private dateColumns = [
    "Date", DataType.DateTime
    "Year", DataType.Int64
    "MonthNo", DataType.Int64
    "Quarter", DataType.Int64
    "YearMonth", DataType.Int64
    "FiscalYear", DataType.Int64
]

/// The sales of the corpus model `f-calendar`. By month: 2023 Jan 30 (Jan 1: 10, Jan 31: 20),
/// Feb 70 (Feb 15: 30, Feb 28: 40), Mar 50 (Mar 31), Jun 60 (Jun 30), Jul 70 (Jul 1), Dec 80 (Dec
/// 31); 2024 Jan 300 (Jan 15: 100, Jan 31: 200), Feb 700 (Feb 10: 300, Feb 29: 400), Mar 500 (Mar
/// 10), May 600 (May 20), Jul 700 (Jul 1), Dec 800 (Dec 31). 2023: 360, 2024: 3600.
let private sales: (DateTime * decimal) list = [
    day 2023 1 1, 10M
    day 2023 1 31, 20M
    day 2023 2 15, 30M
    day 2023 2 28, 40M
    day 2023 3 31, 50M
    day 2023 6 30, 60M
    day 2023 7 1, 70M
    day 2023 12 31, 80M
    day 2024 1 15, 100M
    day 2024 1 31, 200M
    day 2024 2 10, 300M
    day 2024 2 29, 400M
    day 2024 3 10, 500M
    day 2024 5 20, 600M
    day 2024 7 1, 700M
    day 2024 12 31, 800M
]

let private salesTable =
    table "Sales" [ "Date", DataType.DateTime; "Amount", DataType.Decimal ] [
        for (d, amount) in sales -> [ box d; box amount ]
    ]

let private measures = [ "Sales", "Sales", "SUM ( Sales[Amount] )" ]

let private calendarModel (first: DateTime) (last: DateTime) : TestModel = {
    Tables = [
        table "Date" dateColumns (daysFrom first last |> List.map dateRow)
        salesTable
    ]
    Rels = [ rel ("Sales", "Date") ("Date", "Date") ]
    Measures = measures
    Functions = []
}

let private smallFirst = day 2023 1 1
let private smallLast = day 2024 12 31
let private allDays = daysFrom smallFirst smallLast

/// Every day of 2023 and 2024, and the sales above: Sales[Date] -> Date[Date].
let private small = lazy (prepare (calendarModel smallFirst smallLast))

/// Every day of 2006 to 2020 (the years of the published examples).
let private big = lazy (prepare (calendarModel (day 2006 1 1) (day 2020 12 31)))

/// The small calendar related to Sales over a whole-number key instead of the date.
let private keyed (configure: Database -> unit) : Engine.Prepared =
    let key (d: DateTime) =
        box (d.Year * 10000 + d.Month * 100 + d.Day)

    let model = {
        Tables = [
            table
                "Date"
                (("DateKey", DataType.Int64) :: dateColumns)
                (allDays |> List.map (fun d -> key d :: dateRow d))
            table "Sales" [ "DateKey", DataType.Int64; "Amount", DataType.Decimal ] [
                for (d, amount) in sales -> [ key d; box amount ]
            ]
        ]
        Rels = [ rel ("Sales", "DateKey") ("Date", "DateKey") ]
        Measures = measures
        Functions = []
    }

    let db, store = build model
    configure db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

// ---- Asking the engine ---------------------------------------------------------------------------

let private lit (d: DateTime) =
    d.ToString("'dt\"'yyyy-MM-dd'\"'", Globalization.CultureInfo.InvariantCulture)

/// The dates of a one-column table expression, ascending; or the error.
let private tryDates (p: Engine.Prepared) (tableExpr: string) : Result<DateTime list, TessError> =
    match Engine.evaluateQuery p ("EVALUATE " + tableExpr) with
    | Ok [ r ] ->
        r.Rows
        |> List.map (fun row ->
            match row.[0] with
            | QueryValue.DateTime d -> d
            | other -> failtestf "not a date: %A\n%s" other tableExpr)
        |> List.sort
        |> Ok
    | Ok other -> failtestf "expected one result, got %d\n%s" other.Length tableExpr
    | Error e -> Error e

let private dates (p: Engine.Prepared) (tableExpr: string) : DateTime list =
    match tryDates p tableExpr with
    | Ok d -> d
    | Error e -> failtestf "query failed: %s\n%s" (TessError.message e) tableExpr

/// The first date, the last date and the number of dates of a table of dates (as the sources'
/// tables show them).
let private extent (p: Engine.Prepared) (tableExpr: string) : (DateTime * DateTime * int) option =
    match dates p tableExpr with
    | [] -> None
    | d -> Some(List.head d, List.last d, d.Length)

let private isExtent (p: Engine.Prepared) (tableExpr: string) (first: DateTime) (last: DateTime) (count: int) =
    Expect.equal (extent p tableExpr) (Some(first, last, count)) tableExpr

let private isDays (p: Engine.Prepared) (tableExpr: string) (expected: DateTime list) =
    Expect.equal (dates p tableExpr) expected tableExpr

let private between (a: DateTime) (b: DateTime) =
    $"DATESBETWEEN ( 'Date'[Date], {lit a}, {lit b} )"

let private pick (days: DateTime list) =
    match days with
    | [] -> "FILTER ( ALL ( 'Date'[Date] ), FALSE )"
    | _ ->
        let values = days |> List.map lit |> String.concat ", "
        $"TREATAS ( {{ {values} }}, 'Date'[Date] )"

/// `tableExpr` with these dates selected.
let private under (selection: string) (tableExpr: string) =
    $"CALCULATETABLE ( {tableExpr}, {selection} )"

let private refused (p: Engine.Prepared) (dax: string) (mentions: string) =
    match Engine.evaluateQuery p dax with
    | Error(DaxNotSupported(what, _)) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported (%s), got %A\n%s" mentions other dax

let private value (p: Engine.Prepared) (expr: string) : QueryValue =
    match Engine.evaluateQuery p $"EVALUATE ROW ( \"r\", {expr} )" with
    | Ok [ r ] -> r.Rows.Head.[0]
    | Ok _ -> failtestf "expected one result\n%s" expr
    | Error e -> failtestf "query failed: %s\n%s" (TessError.message e) expr

let private money (p: Engine.Prepared) (expr: string) (expected: decimal) =
    Expect.equal (value p expr) (QueryValue.Decimal expected) expr

let private blank (p: Engine.Prepared) (expr: string) =
    Expect.equal (value p expr) QueryValue.Blank expr

// ---- The corpus, until DATE lands ---------------------------------------------------------------
//
// The corpus writes its dates as DATE ( y, m, d ), a function of another family that is not in
// this branch. Here every case of the area runs with DATE ( y, m, d ) spelled as the literal
// dt"y-m-d", which is the same date: nothing else of a case changes, and its expectation is the
// corpus's own.

let private dateCall =
    Regex(@"\bDATE\s*\(\s*(\d{4})\s*,\s*(\d{1,2})\s*,\s*(\d{1,2})\s*\)", RegexOptions.IgnoreCase)

let private withLiterals (query: string) : string =
    dateCall.Replace(
        query,
        fun (m: Match) -> lit (day (int m.Groups.[1].Value) (int m.Groups.[2].Value) (int m.Groups.[3].Value))
    )

let private engine: Conformance.Evaluate =
    fun db store dax -> Engine.prepare db store |> Result.bind (fun e -> Engine.evaluateQuery e dax)

let private areaCases =
    lazy
        (match Conformance.corpusDirectory with
         | None -> []
         | Some directory ->
             (Conformance.load directory).Cases
             |> List.filter (fun c -> c.Area = "time-intelligence"))

[<Tests>]
let corpusTests =
    testList "Engine time intelligence: the corpus with dates as literals" [
        testCase "the area has its cases"
        <| fun () ->
            match Conformance.corpusDirectory with
            | None -> skiptest "no conformance corpus here"
            | Some _ -> Expect.isGreaterThanOrEqual areaCases.Value.Length 44 "time-intelligence cases"

        for c in areaCases.Value do
            testCase c.Id
            <| fun () ->
                let query = withLiterals c.Query

                Expect.isFalse
                    (Regex.IsMatch(query, @"\bDATE\s*\(", RegexOptions.IgnoreCase))
                    $"a DATE call that is not three whole numbers is left in:\n{query}"

                match Conformance.run engine { c with Query = query } with
                | Conformance.Outcome.Pass -> ()
                | Conformance.Outcome.NotSupported reason -> failtest $"{c.Title}\n{query}\nnot supported: {reason}"
                | Conformance.Outcome.Error message -> failtest $"{c.Title}\n{query}\nerror: {message}"
                | Conformance.Outcome.Fail mismatch ->
                    failtest (String.concat "\n" (c.Title :: query :: Conformance.Mismatch.describe mismatch))
    ]

// ---- Published results ---------------------------------------------------------------------------

[<Tests>]
let publishedTests =
    testList "Engine time intelligence: published results" [
        // SQLBI-DP, "Understanding DATEADD": the three queries over February 2019 and their tables.
        testCase "DATEADD of February 2019 (SQLBI)"
        <| fun () ->
            let p = big.Value
            let february = between (day 2019 2 1) (day 2019 2 28)
            // "The result is still 28 days, from 1/31/2019 to 2/27/2019."
            isExtent p $"DATEADD ( {february}, -1, DAY )" (day 2019 1 31) (day 2019 2 27) 28
            // "The result is now 31 days, the number of days in January".
            isExtent p $"DATEADD ( {february}, -1, MONTH )" (day 2019 1 1) (day 2019 1 31) 31
            // "The result is 41 days: 31 days in January 2019, plus the first 10 days in February".
            let range = between (day 2019 2 1) (day 2019 3 10)
            isExtent p $"DATEADD ( {range}, -1, MONTH )" (day 2019 1 1) (day 2019 2 10) 41

        // SQLBI-DP, "Understanding PARALLELPERIOD": both tables, for the range and for "just two
        // days, one in each February and one in March: the result is the same".
        testCase "PARALLELPERIOD of February 1 to March 10, 2019 (SQLBI)"
        <| fun () ->
            let p = big.Value

            for selection in [ between (day 2019 2 1) (day 2019 3 10); pick [ day 2019 2 1; day 2019 3 10 ] ] do
                isExtent p $"PARALLELPERIOD ( {selection}, 0, YEAR )" (day 2019 1 1) (day 2019 12 31) 365
                isExtent p $"PARALLELPERIOD ( {selection}, 0, QUARTER )" (day 2019 1 1) (day 2019 3 31) 90
                isExtent p $"PARALLELPERIOD ( {selection}, 0, MONTH )" (day 2019 2 1) (day 2019 3 31) 59
                isExtent p $"PARALLELPERIOD ( {selection}, -1, YEAR )" (day 2018 1 1) (day 2018 12 31) 365
                isExtent p $"PARALLELPERIOD ( {selection}, -1, QUARTER )" (day 2018 10 1) (day 2018 12 31) 92
                isExtent p $"PARALLELPERIOD ( {selection}, -1, MONTH )" (day 2019 1 1) (day 2019 2 28) 59

        // SQLBI-DP, "Functions that use DATEADD or PARALLELPERIOD": "PREVIOUS* functions are based
        // on the first date in the input dates, whereas the NEXT* functions are based on the last
        // date", for the selection 2018-02-12 to 2019-03-10 (or just those two dates). Worked out
        // from that: the periods before 2018-02-12 and after 2019-03-10.
        testCase "PREVIOUS* and NEXT* of 2018-02-12 to 2019-03-10 (SQLBI)"
        <| fun () ->
            let p = big.Value

            for selection in
                [
                    between (day 2018 2 12) (day 2019 3 10)
                    pick [ day 2018 2 12; day 2019 3 10 ]
                ] do
                isExtent p $"PREVIOUSYEAR ( {selection} )" (day 2017 1 1) (day 2017 12 31) 365
                isExtent p $"PREVIOUSQUARTER ( {selection} )" (day 2017 10 1) (day 2017 12 31) 92
                isExtent p $"PREVIOUSMONTH ( {selection} )" (day 2018 1 1) (day 2018 1 31) 31
                isDays p $"PREVIOUSDAY ( {selection} )" [ day 2018 2 11 ]
                isExtent p $"NEXTYEAR ( {selection} )" (day 2020 1 1) (day 2020 12 31) 366
                isExtent p $"NEXTQUARTER ( {selection} )" (day 2019 4 1) (day 2019 6 30) 91
                isExtent p $"NEXTMONTH ( {selection} )" (day 2019 4 1) (day 2019 4 30) 30
                isDays p $"NEXTDAY ( {selection} )" [ day 2019 3 11 ]

        // DG/dateadd, examples: 2008-07-25..31 back a year is 2007-07-25..31; "DATEADD has a quite
        // complex logic to move months and quarters": 2008-02-25..29 forward a month is 2008-03-25
        // to 2008-03-31.
        testCase "DATEADD examples of DAX Guide"
        <| fun () ->
            let p = big.Value

            isDays
                p
                (under (between (day 2008 7 25) (day 2008 7 31)) "DATEADD ( 'Date'[Date], -1, YEAR )")
                (daysFrom (day 2007 7 25) (day 2007 7 31))

            isDays
                p
                (under (between (day 2008 2 25) (day 2008 2 29)) "DATEADD ( 'Date'[Date], +1, MONTH )")
                (daysFrom (day 2008 3 25) (day 2008 3 31))

        // MS/dateadd, "Special behavior when input is a date column": "when Feb 27 and 28 of 2013
        // are included in the selection and a month is added, DATEADD will return March 27 to 31
        // ... If only Feb 27 is selected, it will go to March 27."
        testCase "DATEADD extends from the last two days of a month (Microsoft Learn)"
        <| fun () ->
            let p = big.Value

            isDays
                p
                (under (pick [ day 2013 2 27; day 2013 2 28 ]) "DATEADD ( 'Date'[Date], 1, MONTH )")
                (daysFrom (day 2013 3 27) (day 2013 3 31))

            isDays p (under (pick [ day 2013 2 27 ]) "DATEADD ( 'Date'[Date], 1, MONTH )") [ day 2013 3 27 ]

        // MS/sameperiodlastyear, "Special behavior": "when Feb 27 and 28 of 2009 are included in the
        // selection, SAMEPERIODLASTYEAR will return Feb 27 to 29 of 2008 ... If only Feb 27 is
        // selected, it will go to Feb 27." And, of classic time intelligence: shifting Feb 29 2008
        // back one year "returns Feb 28 2007".
        testCase "SAMEPERIODLASTYEAR at the end of February (Microsoft Learn)"
        <| fun () ->
            let p = big.Value

            isDays p (under (pick [ day 2009 2 27; day 2009 2 28 ]) "SAMEPERIODLASTYEAR ( 'Date'[Date] )") [
                day 2008 2 27
                day 2008 2 28
                day 2008 2 29
            ]

            isDays p (under (pick [ day 2009 2 27 ]) "SAMEPERIODLASTYEAR ( 'Date'[Date] )") [ day 2008 2 27 ]
            isDays p (under (pick [ day 2008 2 29 ]) "SAMEPERIODLASTYEAR ( 'Date'[Date] )") [ day 2007 2 28 ]

        // MS/parallelperiod: "if you have a selection of dates that starts at June 10 and finishes
        // at June 21 of the same year, and you want to shift that selection forward by one month
        // then the PARALLELPERIOD function will return all dates from the next month (July 1 to
        // July 31); however, if DATEADD is used instead, then the result will include only dates
        // from July 10 to July 21."
        testCase "PARALLELPERIOD returns whole months where DATEADD doesn't (Microsoft Learn)"
        <| fun () ->
            let p = big.Value
            let june = between (day 2010 6 10) (day 2010 6 21)

            isDays p (under june "PARALLELPERIOD ( 'Date'[Date], 1, MONTH )") (daysFrom (day 2010 7 1) (day 2010 7 31))

            isDays p (under june "DATEADD ( 'Date'[Date], 1, MONTH )") (daysFrom (day 2010 7 10) (day 2010 7 21))

        // DG/datesinperiod: the first examples, and the rows "classic 'Date'[Date]" of its tables.
        testCase "DATESINPERIOD over a date column (DAX Guide)"
        <| fun () ->
            let p = big.Value

            let inPeriod (start: DateTime) (n: int) (unit: string) =
                $"DATESINPERIOD ( 'Date'[Date], {lit start}, {n}, {unit} )"

            // "returns 1 day", "returns 3 days, starting from August 15, 2008", "returns an empty
            // table because we request 0 days", "returns 2 days, the last one is August 15, 2008",
            // "returns an entire month (31 days), the last day is August 15, 2008".
            isDays p (inPeriod (day 2008 8 15) 1 "DAY") [ day 2008 8 15 ]
            isDays p (inPeriod (day 2008 8 15) 3 "DAY") (daysFrom (day 2008 8 15) (day 2008 8 17))
            isDays p (inPeriod (day 2008 8 15) 0 "DAY") []
            isDays p (inPeriod (day 2008 8 15) -2 "DAY") [ day 2008 8 14; day 2008 8 15 ]
            isExtent p (inPeriod (day 2008 8 15) -1 "MONTH") (day 2008 7 16) (day 2008 8 15) 31

            // "The following query uses February 29, 2008 as start date. It goes back one interval".
            isExtent p (inPeriod (day 2008 2 29) -1 "DAY") (day 2008 2 29) (day 2008 2 29) 1
            isExtent p (inPeriod (day 2008 2 29) -1 "MONTH") (day 2008 2 1) (day 2008 2 29) 29
            isExtent p (inPeriod (day 2008 2 29) -1 "QUARTER") (day 2007 12 1) (day 2008 2 29) 91
            isExtent p (inPeriod (day 2008 2 29) -1 "YEAR") (day 2007 3 1) (day 2008 2 29) 366

            // "March 31, 2008 is the end of a month and of a quarter."
            isExtent p (inPeriod (day 2008 3 31) -1 "MONTH") (day 2008 3 1) (day 2008 3 31) 31
            isExtent p (inPeriod (day 2008 3 31) -1 "QUARTER") (day 2008 1 1) (day 2008 3 31) 91
            isExtent p (inPeriod (day 2008 3 31) -1 "YEAR") (day 2007 4 1) (day 2008 3 31) 366

            // "With a positive count, the result starts on the start date" (from 2007-02-28).
            isExtent p (inPeriod (day 2007 2 28) 1 "DAY") (day 2007 2 28) (day 2007 2 28) 1
            isExtent p (inPeriod (day 2007 2 28) 1 "MONTH") (day 2007 2 28) (day 2007 3 30) 31
            isExtent p (inPeriod (day 2007 2 28) 1 "QUARTER") (day 2007 2 28) (day 2007 5 30) 92
            isExtent p (inPeriod (day 2007 2 28) 1 "YEAR") (day 2007 2 28) (day 2008 2 28) 366

            // "When the start date is in the middle of a month" (2008-01-15).
            isExtent p (inPeriod (day 2008 1 15) -1 "MONTH") (day 2007 12 16) (day 2008 1 15) 31
            isExtent p (inPeriod (day 2008 1 15) -1 "QUARTER") (day 2007 10 16) (day 2008 1 15) 92
            isExtent p (inPeriod (day 2008 1 15) -1 "YEAR") (day 2007 1 16) (day 2008 1 15) 365

            // "June 30, 2009 is the last day of fiscal year 2009": the classic rows.
            isExtent p (inPeriod (day 2009 6 30) -1 "YEAR") (day 2008 7 1) (day 2009 6 30) 365
            isExtent p (inPeriod (day 2009 6 30) -2 "YEAR") (day 2007 7 1) (day 2009 6 30) 731
            isExtent p (inPeriod (day 2008 6 30) 1 "YEAR") (day 2008 6 30) (day 2009 6 29) 365
            isExtent p (inPeriod (day 2009 3 31) -1 "YEAR") (day 2008 4 1) (day 2009 3 31) 365

        // DG/datesytd ("for 2007-05-12 returns 2007-01-01 to 2007-05-12"; "If the selection
        // contains larger periods, it returns the YTD using the last date": 2007-02-05..05-12),
        // DG/endofyear (2006-11-05..2007-05-12: "the end of the entire period", 2007-12-31),
        // DG/startofmonth (2007-05-12: 2007-05-01), DG/nextday (2008-08-15..20: 2008-08-21),
        // DG/previousmonth (2008-08-15..20: all of July 2008), DG/parallelperiod (2008-08-15, -1,
        // YEAR: all of 2007).
        testCase "examples of DAX Guide"
        <| fun () ->
            let p = big.Value

            isExtent p (under (pick [ day 2007 5 12 ]) "DATESYTD ( 'Date'[Date] )") (day 2007 1 1) (day 2007 5 12) 132

            isExtent
                p
                (under (between (day 2007 2 5) (day 2007 5 12)) "DATESYTD ( 'Date'[Date] )")
                (day 2007 1 1)
                (day 2007 5 12)
                132

            isDays p (under (between (day 2006 11 5) (day 2007 5 12)) "ENDOFYEAR ( 'Date'[Date] )") [ day 2007 12 31 ]

            isDays p (under (pick [ day 2007 5 12 ]) "STARTOFMONTH ( 'Date'[Date] )") [ day 2007 5 1 ]
            let august = between (day 2008 8 15) (day 2008 8 20)
            isDays p (under august "NEXTDAY ( 'Date'[Date] )") [ day 2008 8 21 ]
            isExtent p (under august "PREVIOUSMONTH ( 'Date'[Date] )") (day 2008 7 1) (day 2008 7 31) 31

            isExtent
                p
                (under (pick [ day 2008 8 15 ]) "PARALLELPERIOD ( 'Date'[Date], -1, YEAR )")
                (day 2007 1 1)
                (day 2007 12 31)
                365

        // MS/datesbetween: "if the Dates column starts from July 1, 2017, and the StartDate value
        // is July 1, 2016, the returned table starts from July 1, 2017"; and, of DATESBETWEEN (
        // 'Date'[Date], BLANK (), MAX ( 'Date'[Date] ) ): "when a report filters the measure by the
        // month of June 2020, the DATESBETWEEN function returns a date range from July 1, 2017
        // until June 30, 2020": it starts at the first date of the column whatever is filtered.
        testCase "DATESBETWEEN is over the whole column (Microsoft Learn)"
        <| fun () ->
            let p = small.Value

            isExtent
                p
                (under "'Date'[YearMonth] = 202406" "DATESBETWEEN ( 'Date'[Date], BLANK (), MAX ( 'Date'[Date] ) )")
                (day 2023 1 1)
                (day 2024 6 30)
                547

            isExtent p (between (day 2022 7 1) (day 2023 1 3)) (day 2023 1 1) (day 2023 1 3) 3
            isExtent p "DATESBETWEEN ( 'Date'[Date], dt\"2024-12-30\", BLANK () )" (day 2024 12 30) (day 2024 12 31) 2
    ]

// ---- Worked by hand ------------------------------------------------------------------------------

[<Tests>]
let edgeTests =
    testList "Engine time intelligence: edges" [
        // DG/dateadd: "The notion of 'month selected' is based on the dates available in the
        // underlying date table": a quarter and a year move month by month, so a whole quarter is
        // the whole quarter before, whatever the lengths (Q2 2024, 91 days, to Q1 2024, 91; the
        // year 2024, 366 days, to 2023, 365).
        testCase "DATEADD by QUARTER and YEAR moves whole months"
        <| fun () ->
            let p = small.Value

            isExtent
                p
                (under (between (day 2024 4 1) (day 2024 6 30)) "DATEADD ( 'Date'[Date], -1, QUARTER )")
                (day 2024 1 1)
                (day 2024 3 31)
                91

            isExtent
                p
                (under (between (day 2024 1 1) (day 2024 12 31)) "DATEADD ( 'Date'[Date], -1, YEAR )")
                (day 2023 1 1)
                (day 2023 12 31)
                365

        // SQLBI-DP: "The last day of the shifted month is returned if the day does not exist in the
        // shifted month": March 29 to 31, 2023 back a month are all February 28, and the last two
        // days are selected, so nothing is left to extend to.
        testCase "DATEADD to a shorter month ends on its last day"
        <| fun () ->
            let p = small.Value

            isDays p (under (between (day 2023 3 29) (day 2023 3 31)) "DATEADD ( 'Date'[Date], -1, MONTH )") [
                day 2023 2 28
            ]

            isDays p (under (pick [ day 2023 3 30 ]) "DATEADD ( 'Date'[Date], -1, MONTH )") [ day 2023 2 28 ]

        // "The result table includes only dates that exist in the dates column" (DG/dateadd): what
        // moves off either end of the Date table is dropped, what stays is kept.
        testCase "DATEADD keeps only the dates the table has"
        <| fun () ->
            let p = small.Value

            isDays p (under (between (day 2024 12 30) (day 2024 12 31)) "DATEADD ( 'Date'[Date], 1, DAY )") [
                day 2024 12 31
            ]

            isDays p (under (between (day 2023 1 1) (day 2023 2 28)) "SAMEPERIODLASTYEAR ( 'Date'[Date] )") []
            isDays p (under (pick [ day 2023 1 1 ]) "PREVIOUSDAY ( 'Date'[Date] )") []
            isDays p (under (pick [ day 2023 5 5 ]) "PREVIOUSYEAR ( 'Date'[Date] )") []
            isDays p (under (pick [ day 2024 11 5 ]) "NEXTQUARTER ( 'Date'[Date] )") []

        // A column reference is CALCULATETABLE ( DISTINCT ( column ) ): in a row context the row
        // becomes the filter (dax.guide, every function: "Only in this case a context transition
        // applies"). Iterating the Date table, each row sees its own date: the month to date of the
        // 10th has 10 dates. With the dates as a table expression there is no context transition:
        // VALUES sees the whole filter context, all of March 2024, and the month to date has 31.
        testCase "a column reference makes a context transition, a table expression doesn't"
        <| fun () ->
            let p = small.Value
            let march = "'Date'[YearMonth] = 202403"

            let perRow (datesArgument: string) =
                $"MAXX ( FILTER ( 'Date', 'Date'[Date] = dt\"2024-03-10\" ), COUNTROWS ( DATESMTD ( {datesArgument} ) ) )"

            let ofColumn = perRow "'Date'[Date]"
            let ofTable = perRow "VALUES ( 'Date'[Date] )"
            Expect.equal (value p $"CALCULATE ( {ofColumn}, {march} )") (QueryValue.Int64 10L) "column reference"
            Expect.equal (value p $"CALCULATE ( {ofTable}, {march} )") (QueryValue.Int64 31L) "table expression"

        // The same in a calculated-column style iteration: the year to date of each row's date.
        testCase "DATESYTD in a row context is the year to date of the row"
        <| fun () ->
            let p = small.Value

            returns
                p
                "EVALUATE SELECTCOLUMNS ( FILTER ( 'Date', 'Date'[Date] = dt\"2024-03-10\" || 'Date'[Date] = dt\"2023-02-01\" ), \"d\", 'Date'[Date], \"n\", COUNTROWS ( DATESYTD ( 'Date'[Date] ) ) )"
                [ [ day 2024 3 10; 70 ]; [ day 2023 2 1; 32 ] ]

        // "the same data lineage as the date column or table provided as an argument" (dax.guide):
        // the result is named after the column, and filters it.
        testCase "the result has the lineage of the dates column"
        <| fun () ->
            let p = small.Value

            for f in
                [
                    "DATESYTD ( 'Date'[Date] )"
                    "DATEADD ( 'Date'[Date], -1, MONTH )"
                    "PARALLELPERIOD ( 'Date'[Date], -1, MONTH )"
                    "LASTDATE ( 'Date'[Date] )"
                    "ENDOFMONTH ( 'Date'[Date] )"
                    "NEXTDAY ( 'Date'[Date] )"
                    "DATESINPERIOD ( 'Date'[Date], dt\"2024-03-10\", -1, MONTH )"
                    "DATESBETWEEN ( 'Date'[Date], BLANK (), BLANK () )"
                ] do
                let r = query p $"EVALUATE CALCULATETABLE ( {f}, 'Date'[YearMonth] = 202403 )"
                Expect.equal (r.Columns |> List.map _.Name) [ "Date[Date]" ] f
                Expect.equal (r.Columns |> List.map _.Type) [ QueryType.DateTime ] f

            // A table of the fact's dates filters the fact's column: the sales of the first sale
            // date of 2024 (January 15: 100).
            money p "CALCULATE ( CALCULATE ( [Sales], FIRSTDATE ( Sales[Date] ) ), 'Date'[Year] = 2024 )" 100M

            let r =
                query p "EVALUATE CALCULATETABLE ( LASTDATE ( Sales[Date] ), 'Date'[Year] = 2023 )"

            Expect.equal (r.Columns |> List.map _.Name) [ "Sales[Date]" ] "the fact's column"
            Expect.equal r.Rows.Head.[0] (QueryValue.DateTime(day 2023 12 31)) "the last sale of 2023"

        // FIRSTDATE and LASTDATE as values: "the returned value can be used whenever a date value is
        // required" (MS/firstdate); with nothing selected the value is BLANK.
        testCase "FIRSTDATE and LASTDATE as values"
        <| fun () ->
            let p = small.Value

            Expect.equal
                (value p "CALCULATE ( LASTDATE ( 'Date'[Date] ), 'Date'[YearMonth] = 202402 )")
                (QueryValue.DateTime(day 2024 2 29))
                "the last date of February 2024"

            blank p "CALCULATE ( FIRSTDATE ( 'Date'[Date] ), 'Date'[Year] = 1999 )"

            Expect.equal
                (value p "CALCULATE ( FIRSTDATE ( 'Date'[Date] ) + 0, 'Date'[Year] = 1999 ) = 0")
                (QueryValue.Boolean true)
                "BLANK in arithmetic"

        // DG/openingbalancemonth: "If the Dates column does not contain all the dates, for example
        // Sales[Order Date], this is the last date of the previous month that exists in the
        // column." With the sale dates of March 2024 selected, February's sale dates are the 10th
        // and the 29th: the last is the 29th, 400. March 2023 has one sale date; February 2023's end
        // on the 28th: 40. With no sale date selected there is no previous month: BLANK.
        testCase "OPENINGBALANCEMONTH over a fact column uses the dates the column has"
        <| fun () ->
            let p = small.Value

            let month (first: string) (last: string) =
                $"CALCULATE ( OPENINGBALANCEMONTH ( [Sales], Sales[Date] ), Sales[Date] >= dt\"{first}\" && Sales[Date] <= dt\"{last}\" )"

            money p (month "2024-03-01" "2024-03-31") 400M
            money p (month "2023-03-01" "2023-03-31") 40M
            blank p (month "2024-04-01" "2024-04-30")
            // The definition is CALCULATE ( ..., CALCULATETABLE ( ENDOFMONTH ( Sales[Date] ),
            // PREVIOUSMONTH ( Sales[Date] ) ) ): a filter on the Date table is not replaced by the
            // one PREVIOUSMONTH puts on Sales[Date], so under "March" no February sale is visible.
            blank p "CALCULATE ( OPENINGBALANCEMONTH ( [Sales], Sales[Date] ), 'Date'[YearMonth] = 202403 )"

        // The quarter and year forms, on the Date table. In May 2024: the quarter to date is April
        // 1 to May 31 (600); the previous quarter ended on March 31 (no sale: BLANK); the quarter
        // ends on June 30 (no sale); the year ends on December 31 (800); the previous year ended on
        // 2023-12-31 (80). With the year ending June 30: the year of May 2024 began on 2023-07-01
        // (70 + 80 + 300 + 700 + 500 + 600 = 2250), ends on 2024-06-30 (no sale), and the previous
        // one ended on 2023-06-30 (60).
        testCase "the quarter and year forms"
        <| fun () ->
            let p = small.Value

            let may (expr: string) =
                $"CALCULATE ( {expr}, 'Date'[YearMonth] = 202405 )"

            money p (may "TOTALQTD ( [Sales], 'Date'[Date] )") 600M
            money p (may "TOTALMTD ( [Sales], 'Date'[Date] )") 600M
            blank p (may "OPENINGBALANCEQUARTER ( [Sales], 'Date'[Date] )")
            blank p (may "CLOSINGBALANCEQUARTER ( [Sales], 'Date'[Date] )")
            money p (may "CLOSINGBALANCEYEAR ( [Sales], 'Date'[Date] )") 800M
            money p (may "OPENINGBALANCEYEAR ( [Sales], 'Date'[Date] )") 80M
            money p (may "TOTALYTD ( [Sales], 'Date'[Date], \"6/30\" )") 2250M
            blank p (may "CLOSINGBALANCEYEAR ( [Sales], 'Date'[Date], \"6/30\" )")
            money p (may "OPENINGBALANCEYEAR ( [Sales], 'Date'[Date], \"6/30\" )") 60M
            isDays p (under "'Date'[YearMonth] = 202405" "STARTOFYEAR ( 'Date'[Date], \"6/30\" )") [ day 2023 7 1 ]
            isDays p (under "'Date'[YearMonth] = 202405" "STARTOFQUARTER ( 'Date'[Date] )") [ day 2024 4 1 ]
            isDays p (under "'Date'[YearMonth] = 202405" "ENDOFQUARTER ( 'Date'[Date] )") [ day 2024 6 30 ]
            isDays p (under "'Date'[YearMonth] = 202405" "ENDOFMONTH ( 'Date'[Date] )") [ day 2024 5 31 ]

            isExtent
                p
                (under "'Date'[YearMonth] = 202405" "PREVIOUSYEAR ( 'Date'[Date], \"6/30\" )")
                (day 2023 1 1)
                (day 2023 6 30)
                181

            isExtent
                p
                (under "'Date'[YearMonth] = 202309" "NEXTYEAR ( 'Date'[Date], \"6/30\" )")
                (day 2024 7 1)
                (day 2024 12 31)
                184

        // The filter argument, and the year end after it: "TOTALYTD ( <Expression>, <Dates> [,
        // <Filter>] [, <YearEndDate>] ) corresponds to CALCULATE ( <Expression>, DATESYTD ( <Dates>
        // [, <YearEndDate>] ) [, <Filter>] )" (DG/totalytd). The filter is one more filter argument:
        // on another column of the Date table it stays (the date rule removes only what was in the
        // context before). Year to date at May 2024 restricted to quarter 1: 1500; to the months
        // of the year ending June 30 that are in the second half of 2023: 70 + 80 = 150.
        testCase "the filter argument of TOTALYTD"
        <| fun () ->
            let p = small.Value

            let may (expr: string) =
                $"CALCULATE ( {expr}, 'Date'[YearMonth] = 202405 )"

            money p (may "TOTALYTD ( [Sales], 'Date'[Date], 'Date'[Quarter] = 1 )") 1500M
            money p (may "TOTALYTD ( [Sales], 'Date'[Date], 'Date'[Year] = 2023, \"6/30\" )") 150M
            money p (may "TOTALYTD ( [Sales], 'Date'[Date], , \"06-30\" )") 2250M
            money p (may "TOTALYTD ( [Sales], 'Date'[Date], \"2001-06-30\" )") 2250M
            blank p (may "TOTALMTD ( [Sales], 'Date'[Date], Sales[Amount] > 1000 )")
    ]

// ---- The date-table rule -------------------------------------------------------------------------

[<Tests>]
let dateTableTests =
    testList "Engine time intelligence: the date-table rule" [
        // SQLBI-MDT: "When a filter is applied on the key of the relationship – Date[Date] in this
        // example – the new filter overrides any other filter on the Date table."
        testCase "a filter on the date key of a date relationship removes the other Date filters"
        <| fun () ->
            let p = small.Value

            let march (expr: string) =
                $"CALCULATE ( {expr}, 'Date'[YearMonth] = 202403 )"
            // January to March 2024: 300 + 700 + 500.
            money p (march "CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ) )") 1500M
            money p (march "CALCULATE ( [Sales], 'Date'[Date] = dt\"2023-01-31\" )") 20M
            money p (march "CALCULATE ( [Sales], TREATAS ( { dt\"2023-02-15\" }, 'Date'[Date] ) )") 30M

            money
                p
                (march "CALCULATE ( [Sales], FILTER ( ALL ( 'Date'[Date] ), 'Date'[Date] < dt\"2023-02-01\" ) )")
                30M

        // SQLBI-TI writes the rule as an ALL ( 'Calendar' ) next to the filter: "CALCULATE ( SUM (
        // Sales[SalesAmount] ), DATESYTD ( 'Calendar'[Date] ), ALL ( 'Calendar' ) )". So a filter on
        // another Date column in the SAME CALCULATE stays: the year to date at March 2024 that is
        // in month 2 is February, 700.
        testCase "filters of the same CALCULATE on other Date columns stay"
        <| fun () ->
            let p = small.Value

            money
                p
                "CALCULATE ( CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ), 'Date'[MonthNo] = 2 ), 'Date'[YearMonth] = 202403 )"
                700M

        // SQLBI-MDT: "the additional REMOVEFILTERS is applied only when you apply a filter on the
        // Date[Date] column. If you move the filter to any other column, then REMOVEFILTERS is not
        // applied": the Sales 2008 example. March 2024 and the year 2023 have no date in common;
        // March and the year 2024 are March.
        testCase "a filter on another Date column removes nothing else"
        <| fun () ->
            let p = small.Value
            blank p "CALCULATE ( CALCULATE ( [Sales], 'Date'[Year] = 2023 ), 'Date'[YearMonth] = 202403 )"
            money p "CALCULATE ( CALCULATE ( [Sales], 'Date'[Year] = 2024 ), 'Date'[YearMonth] = 202403 )" 500M
            // A date filter on the fact's own date column is not on the Date table: March stays.
            blank p "CALCULATE ( CALCULATE ( [Sales], Sales[Date] = dt\"2024-01-15\" ), 'Date'[YearMonth] = 202403 )"

        // SQLBI-MDT: with the relationship on a whole-number key "the Sales YTD measure does no
        // longer work": the filter on Date[Date] no longer removes the one on the month, and the
        // year to date of March is March. "You can obtain the same behavior ... by marking the
        // table as a date table."
        testCase "with a whole-number key the rule needs the table marked as a date table"
        <| fun () ->
            let ytd =
                "CALCULATE ( CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ) ), 'Date'[YearMonth] = 202403 )"

            // The same filter as plain DAX (SQLBI's expansion of DATESYTD).
            let plain =
                "CALCULATE ( CALCULATE ( [Sales], 'Date'[Date] >= dt\"2024-01-01\" && 'Date'[Date] <= dt\"2024-03-31\" ), 'Date'[YearMonth] = 202403 )"

            let unmarked = keyed ignore
            money unmarked plain 500M
            // The filter on the month stays, so DATESYTD's own two definitions part ways: SQLBI's
            // January 1 to March 31, and DG's dates of the year up to the last visible one, which
            // starts at March 1. Refused rather than picking one.
            refused unmarked $"EVALUATE ROW ( \"r\", {ytd} )" "hide the first date of the period"

            let marked =
                keyed (fun db ->
                    db.Model.Tables.["Date"].DataCategory <- "Time"
                    db.Model.Tables.["Date"].Columns.["Date"].IsKey <- true)

            money marked ytd 1500M
            money marked plain 1500M

        // What the sources don't settle is refused, but only where it would make a difference.
        testCase "a key column of a table that is not marked: refused when other Date columns are filtered"
        <| fun () ->
            let p = keyed (fun db -> db.Model.Tables.["Date"].Columns.["Date"].IsKey <- true)

            money p "CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ), 'Date'[Date] = dt\"2024-03-10\" )" 500M

            money
                p
                "CALCULATE ( CALCULATE ( [Sales], 'Date'[Date] = dt\"2024-03-10\" ), 'Date'[Date] > dt\"2024-01-01\" )"
                500M

            refused
                p
                "EVALUATE ROW ( \"r\", CALCULATE ( CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ) ), 'Date'[YearMonth] = 202403 ) )"
                "not marked as a date table"

        testCase "KEEPFILTERS on the date column: refused when other Date columns are filtered"
        <| fun () ->
            let p = small.Value

            money
                p
                "CALCULATE ( [Sales], KEEPFILTERS ( DATESBETWEEN ( 'Date'[Date], dt\"2024-01-01\", dt\"2024-01-31\" ) ) )"
                300M

            refused
                p
                "EVALUATE ROW ( \"r\", CALCULATE ( CALCULATE ( [Sales], KEEPFILTERS ( DATESYTD ( 'Date'[Date] ) ) ), 'Date'[YearMonth] = 202403 ) )"
                "KEEPFILTERS"

        testCase "a context transition on the date column: refused when the other Date filters hide the date"
        <| fun () ->
            let p = small.Value
            // Nothing else filters Date: the rule would change nothing. The sales by date add up.
            money p "SUMX ( VALUES ( 'Date'[Date] ), [Sales] )" 3960M
            // Rows of the whole Date table replace every Date filter anyway.
            money p "CALCULATE ( SUMX ( 'Date', [Sales] ), 'Date'[Year] = 2024 )" 3600M
            // A filter on the date column itself is replaced by the row's, rule or no rule.
            money p "CALCULATE ( SUMX ( VALUES ( 'Date'[Date] ), [Sales] ), 'Date'[Date] >= dt\"2024-01-01\" )" 3600M

            // A date that the other filters keep: the Date table is that one row, rule or no rule.
            // Grouping by date under a filter on the year adds up to the year, 3600.
            money p "CALCULATE ( SUMX ( VALUES ( 'Date'[Date] ), [Sales] ), 'Date'[Year] = 2024 )" 3600M

            money
                p
                "CALCULATE ( SUMX ( ADDCOLUMNS ( SUMMARIZE ( Sales, 'Date'[Date] ), \"s\", [Sales] ), [s] ), 'Date'[Year] = 2024 )"
                3600M

            // SUMMARIZE's own extension columns are computed under a filter on the group's Sales
            // rows, which reaches the Date table through the relationship: still refused.
            refused
                p
                "EVALUATE CALCULATETABLE ( SUMMARIZE ( Sales, 'Date'[Date], \"s\", [Sales] ), 'Date'[Year] = 2024 )"
                "context transition on the date column"

            // SUMMARIZECOLUMNS with the year as its own filter argument groups by date fine.
            match
                Engine.evaluateQuery
                    p
                    "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Date], TREATAS ( { 2024 }, 'Date'[Year] ), \"s\", [Sales] )"
            with
            | Ok [ r ] -> Expect.equal r.Rows.Length 8 "the 2024 dates with sales"
            | other -> failtestf "expected one result, got %A" other

            // A date the other filters hide: the rule would bring back its sales (all 3960), no rule
            // leaves it empty (3600).
            refused
                p
                "EVALUATE ROW ( \"r\", CALCULATE ( SUMX ( ALL ( 'Date'[Date] ), [Sales] ), 'Date'[Year] = 2024 ) )"
                "context transition on the date column"

            // A filter that pairs a Date column with a column of Sales: removing the Year half
            // would widen the amounts, keeping it doesn't.
            refused
                p
                "EVALUATE ROW ( \"r\", CALCULATE ( SUMX ( VALUES ( 'Date'[Date] ), [Sales] ), FILTER ( CROSSJOIN ( VALUES ( 'Date'[Year] ), VALUES ( Sales[Amount] ) ), 'Date'[Year] = 2024 && Sales[Amount] > 50 ) ) )"
                "context transition on the date column"

            // A snowflake, Date -> Month: the rule would also remove the filter on Month, which the
            // date row keeps either way. Per date, 24 months with the rule, 12 without.
            let snowflake =
                prepare {
                    calendarModel smallFirst smallLast with
                        Tables =
                            (calendarModel smallFirst smallLast).Tables
                            @ [
                                table "Month" [ "YearMonth", DataType.Int64; "Quarter", DataType.Int64 ] [
                                    for y in [ 2023; 2024 ] do
                                        for m in 1..12 -> [ box (y * 100 + m); box ((m - 1) / 3 + 1) ]
                                ]
                            ]
                        Rels = [
                            rel ("Sales", "Date") ("Date", "Date")
                            rel ("Date", "YearMonth") ("Month", "YearMonth")
                        ]
                }

            refused
                snowflake
                "EVALUATE ROW ( \"r\", CALCULATE ( SUMX ( VALUES ( 'Date'[Date] ), CALCULATE ( COUNTROWS ( 'Month' ) ) ), 'Month'[Quarter] IN { 1, 2 } ) )"
                "context transition on the date column"

        // The star model's Date[Date] is the key of an active and an inactive date relationship.
        testCase "the key of a relationship that is not in force: refused when it matters"
        <| fun () ->
            let model = {
                calendarModel smallFirst smallLast with
                    Rels = [ inactive (rel ("Sales", "Date") ("Date", "Date")) ]
            }

            let p = prepare model
            // No relationship in force: Sales is not filtered by Date at all.
            money p "CALCULATE ( [Sales], 'Date'[Date] = dt\"2024-03-10\" )" 3960M

            refused
                p
                "EVALUATE ROW ( \"r\", CALCULATE ( CALCULATE ( COUNTROWS ( 'Date' ), 'Date'[Date] = dt\"2024-03-10\" ), 'Date'[Year] = 2024 ) )"
                "not active"

            // With USERELATIONSHIP the relationship is in force, and the rule is sure.
            money
                p
                "CALCULATE ( CALCULATE ( [Sales], 'Date'[Date] >= dt\"2024-01-01\" && 'Date'[Date] <= dt\"2024-03-31\", USERELATIONSHIP ( Sales[Date], 'Date'[Date] ) ), 'Date'[YearMonth] = 202403 )"
                1500M
    ]

// ---- Refusals and errors -------------------------------------------------------------------------

/// A fact with several rows on a date and days without rows: what the engine's message calls a
/// date column that "is not unique" and "has gaps".
let private loose =
    lazy
        (prepare {
            Tables = [
                table "Log" [ "At", DataType.DateTime; "N", DataType.Int64 ] [
                    [ box (day 2024 1 30); box 1 ]
                    [ box (day 2024 1 31); box 2 ]
                    [ box (day 2024 1 31); box 3 ]
                    [ box (day 2024 2 1); box 4 ]
                    [ box (day 2024 2 15); box 5 ]
                    [ box (day 2024 3 1); box 6 ]
                    [ box (day 2024 3 31); box 7 ]
                ]
                table "Stamp" [ "At", DataType.DateTime; "Text", DataType.String ] [
                    [ box (DateTime(2024, 1, 31, 13, 5, 0)); box "a" ]
                    [ box (day 2024 2 1); box "b" ]
                ]
            ]
            Rels = []
            Measures = []
            Functions = []
        })

[<Tests>]
let refusalTests =
    testList "Engine time intelligence: refusals and errors" [
        testCase "calendar-based time intelligence"
        <| fun () ->
            let p = small.Value
            refused p "EVALUATE DATEADD ( 'Date'[Date], -1, WEEK )" "WEEK"
            refused p "EVALUATE DATEADD ( 'Date'[Date], -1, MONTH, EXTENDING )" "Extension"
            refused p "EVALUATE DATEADD ( 'Date'[Date], -1, MONTH, , ANCHORED )" "Truncation"
            refused p "EVALUATE DATESINPERIOD ( 'Date'[Date], dt\"2024-03-10\", -1, MONTH, ENDALIGNED )" "EndBehavior"
            refused p "EVALUATE DATESINPERIOD ( 'Date'[Date], dt\"2024-03-10\", -1, WEEK )" "WEEK"
            refused p "EVALUATE PARALLELPERIOD ( 'Date'[Date], -1, WEEK )" "WEEK"

            // "Week functions only work with calendar based time intelligence" (Microsoft Learn):
            // with a date column they are an error.
            for f in [ "DATESWTD"; "STARTOFWEEK" ] do
                match Engine.evaluateQuery p $"EVALUATE {f} ( 'Date'[Date] )" with
                | Error e -> Expect.equal (TessError.code e) "dax-invalid" f
                | Ok _ -> failtestf "%s of a date column should be an error" f

        testCase "dates arguments that are not a date column or a table of one"
        <| fun () ->
            let p = small.Value
            refused p "EVALUATE DATESYTD ( 'Date'[Date] > dt\"2024-01-01\" )" "True/False"
            refused p "EVALUATE DATESYTD ( { dt\"2024-03-10\" } )" "not the values of a model column"

            refused
                p
                "EVALUATE DATESBETWEEN ( VALUES ( 'Date'[Date] ), BLANK (), BLANK () )"
                "anything but a date column"

            match queryError p "EVALUATE DATESYTD ( 'Date'[Year] )" with
            | DaxInvalid(message, _) -> Expect.stringContains message "needs a column of dates" "not a date column"
            | other -> failtestf "expected dax-invalid, got %A" other

            match queryError p "EVALUATE DATESYTD ( 'Date' )" with
            | DaxInvalid(message, _) -> Expect.stringContains message "one column" "a whole table"
            | other -> failtestf "expected dax-invalid, got %A" other

            match queryError p "EVALUATE PARALLELPERIOD ( 'Date'[Date], -1, DAY )" with
            | DaxInvalid(message, _) -> Expect.stringContains message "MONTH, QUARTER, YEAR" "no DAY"
            | other -> failtestf "expected dax-invalid, got %A" other

        testCase "year end dates that are not read the same everywhere"
        <| fun () ->
            let p = small.Value

            let ytd (yearEnd: string) =
                $"EVALUATE DATESYTD ( 'Date'[Date], {yearEnd} )"

            refused p (ytd "\"6/7\"") "depends on the model's culture"
            // Day first: June 30 in a day-first culture, an error in a month-first one.
            refused p (ytd "\"30/6\"") "depends on the model's culture"
            refused p (ytd "\"Jun 30\"") "only numeric"
            refused p (ytd "\"2/28\"") "February"
            refused p (ytd "\"6/31\"") "not a day of every year"
            refused p (ytd "\"6\" & \"/30\"") "not a text literal"

            refused
                p
                "EVALUATE ROW ( \"r\", TOTALYTD ( [Sales], 'Date'[Date], ALL ( 'Date' ), [Sales] ) )"
                "not a text literal"

        // The sources don't say what these are.
        testCase "what the sources leave open"
        <| fun () ->
            let p = small.Value
            let empty = "'Date'[Year] = 1999"
            refused p $"EVALUATE CALCULATETABLE ( FIRSTDATE ( 'Date'[Date] ), {empty} )" "no date is selected"
            refused p $"EVALUATE CALCULATETABLE ( DATESYTD ( 'Date'[Date] ), {empty} )" "no date selected"

            refused
                p
                $"EVALUATE ROW ( \"r\", CALCULATE ( TOTALMTD ( [Sales], 'Date'[Date] ), {empty} ) )"
                "no date selected"

            // June 30 alone, forward to July: July 30, or July 30 and 31?
            refused
                p
                (under (pick [ day 2024 6 30 ]) "DATEADD ( 'Date'[Date], 1, MONTH )"
                 |> (+) "EVALUATE ")
                "last day of a month"

            // Microsoft Learn (DATEADD): "if the dates in the current context do not form a
            // contiguous interval, the function returns an error"; SQLBI-DP moves each month on its
            // own. February and April 1, 2024, without March.
            let gapped = pick [ day 2024 2 1; day 2024 4 1 ]

            for f in
                [
                    "DATEADD ( 'Date'[Date], -1, MONTH )"
                    "DATEADD ( 'Date'[Date], 1, DAY )"
                    "SAMEPERIODLASTYEAR ( 'Date'[Date] )"
                ] do
                refused p ("EVALUATE " + under gapped f) "selection of dates with gaps"

            // January and March, without February.
            refused
                p
                ("EVALUATE "
                 + under (pick [ day 2024 1 10; day 2024 3 10 ]) "PARALLELPERIOD ( 'Date'[Date], -1, MONTH )")
                "skips a whole month"

            refused p "EVALUATE DATESINPERIOD ( 'Date'[Date], dt\"2024-03-30\", -1, MONTH )" "doesn't have"
            refused p "EVALUATE DATESINPERIOD ( 'Date'[Date], BLANK (), -1, MONTH )" "BLANK start"
            refused p "EVALUATE DATESINPERIOD ( 'Date'[Date], dt\"2024-03-30\", 1.5, MONTH )" "whole number"
            refused p "EVALUATE DATEADD ( 'Date'[Date], BLANK (), MONTH )" "whole number"
            refused p "EVALUATE DATESBETWEEN ( 'Date'[Date], 45000, BLANK () )" "not a date"

        // With nothing selected the shifting functions have nothing to shift, and STARTOF* / ENDOF*
        // are "an empty filter on the dates" (DG/openingbalancemonth).
        testCase "no date selected: the functions that are then empty"
        <| fun () ->
            let p = small.Value
            let empty = "'Date'[Year] = 1999"

            for f in
                [
                    "DATEADD ( 'Date'[Date], -1, MONTH )"
                    "SAMEPERIODLASTYEAR ( 'Date'[Date] )"
                    "PARALLELPERIOD ( 'Date'[Date], 1, YEAR )"
                    "PREVIOUSDAY ( 'Date'[Date] )"
                    "NEXTMONTH ( 'Date'[Date] )"
                    "STARTOFYEAR ( 'Date'[Date] )"
                    "ENDOFMONTH ( 'Date'[Date] )"
                ] do
                isDays p (under empty f) []

            blank p $"CALCULATE ( CLOSINGBALANCEMONTH ( [Sales], 'Date'[Date] ), {empty} )"
            blank p $"CALCULATE ( OPENINGBALANCEYEAR ( [Sales], 'Date'[Date] ), {empty} )"

        // "Function 'DATEADD' expects a contiguous selection when the date column is not unique,
        // has gaps or it contains time portion." is the engine's own message (quoted in the Power
        // BI community threads about it). Log[At] repeats January 31 and misses most days.
        testCase "DATEADD over a column that is not unique or has gaps"
        <| fun () ->
            let p = loose.Value
            // A contiguous selection is fine: January 30 and 31 forward a day are January 31 and
            // February 1.
            Expect.equal
                (dates p "CALCULATETABLE ( DATEADD ( Log[At], 1, DAY ), Log[N] <= 3 )")
                [ day 2024 1 31; day 2024 2 1 ]
                "contiguous"

            // January 30 and February 1, without the 31st the column has.
            match queryError p "EVALUATE CALCULATETABLE ( DATEADD ( Log[At], 1, DAY ), Log[N] IN { 1, 4 } )" with
            | DaxEvaluation(message, _) ->
                Expect.equal
                    message
                    "Function 'DATEADD' expects a contiguous selection when the date column is not unique, has gaps or it contains time portion."
                    "the engine's message"
            | other -> failtestf "expected a DAX error, got %A" other

            match queryError p "EVALUATE CALCULATETABLE ( SAMEPERIODLASTYEAR ( Log[At] ), Log[N] IN { 1, 4 } )" with
            | DaxEvaluation(message, _) ->
                Expect.stringContains message "'SAMEPERIODLASTYEAR'" "named after the function"
            | other -> failtestf "expected a DAX error, got %A" other

            // February 1 and 15: the column has no date in between. Contiguous or not?
            refused
                p
                "EVALUATE CALCULATETABLE ( DATEADD ( Log[At], 1, DAY ), Log[N] IN { 4, 5 } )"
                "whether that selection counts as contiguous"

            refused
                p
                "EVALUATE CALCULATETABLE ( PARALLELPERIOD ( Log[At], 1, MONTH ), Log[N] IN { 1, 4 } )"
                "not unique or has gaps"

            // The functions that take one date, or a range of the column, are not affected.
            Expect.equal (dates p "PREVIOUSMONTH ( Log[At] )") [] "nothing before January"

            Expect.equal
                (dates p "CALCULATETABLE ( PREVIOUSMONTH ( Log[At] ), Log[N] >= 6 )")
                [ day 2024 2 1; day 2024 2 15 ]
                "the dates the column has in February"

            Expect.equal (dates p "DATESMTD ( Log[At] )") [ day 2024 3 1; day 2024 3 31 ] "March to date"
            // The "whole month" is the dates the column has (DG/dateadd): February 1 and 15 are
            // all of February in Log[At], so they move to all the column has of March.
            Expect.equal
                (dates p "CALCULATETABLE ( DATEADD ( Log[At], 1, MONTH ), Log[N] = 5 )")
                []
                "one of the two dates of February is a day, not the month: March 15, which the column doesn't have"

            Expect.equal
                (dates
                    p
                    "DATEADD ( FILTER ( ALL ( Log[At] ), Log[At] = dt\"2024-01-30\" || Log[At] = dt\"2024-01-31\" ), 1, MONTH )")
                [ day 2024 2 1; day 2024 2 15 ]
                "all of January to all of February"

        testCase "a column with a time of day"
        <| fun () ->
            let p = loose.Value
            refused p "EVALUATE LASTDATE ( Stamp[At] )" "time of day"
            refused p "EVALUATE DATESBETWEEN ( Stamp[At], BLANK (), BLANK () )" "time of day"
            refused p "EVALUATE DATESINPERIOD ( 'Log'[At], dt\"2024-03-30T10:00:00\", 1, DAY )" "time of day"
    ]

// ---- Each function against its definition, on random selections ---------------------------------

let private config = {
    FsCheckConfig.defaultConfig with
        maxTest = 120
}

let private monthStartOf (d: DateTime) = DateTime(d.Year, d.Month, 1)

let private monthEndOf (d: DateTime) =
    (monthStartOf d).AddMonths(1).AddDays(-1.0)

/// A selection of dates of the small calendar: nothing, single days, ranges, whole months, ends of
/// months, the edges of the table, or scattered days; often several of those together.
let private selectionOf (random: Random) : DateTime list =
    let anyDay () = allDays.[random.Next allDays.Length]

    let piece () =
        match random.Next 8 with
        | 0 -> [ anyDay () ]
        | 1 ->
            let a = anyDay ()
            daysFrom a (min smallLast (a.AddDays(float (random.Next 70))))
        | 2 ->
            let a = monthStartOf (anyDay ())
            daysFrom a (monthEndOf (a.AddMonths(random.Next 3)) |> min smallLast)
        | 3 ->
            // The last days of a month.
            let e = monthEndOf (anyDay ())
            daysFrom (e.AddDays(float -(random.Next 4))) e
        | 4 ->
            // A month end alone, or with the day before.
            let e = monthEndOf (anyDay ())
            if random.Next 2 = 0 then [ e ] else [ e.AddDays(-1.0); e ]
        | 5 -> [ for _ in 0 .. random.Next 6 -> anyDay () ]
        | 6 ->
            if random.Next 2 = 0 then
                daysFrom smallFirst (smallFirst.AddDays(float (random.Next 40)))
            else
                daysFrom (smallLast.AddDays(float -(random.Next 40))) smallLast
        | _ -> daysFrom (day 2024 2 (1 + random.Next 29)) (day 2024 3 (1 + random.Next 31))

    match random.Next 12 with
    | 0 -> []
    | n when n < 8 -> piece () |> List.distinct |> List.sort
    | _ -> piece () @ piece () |> List.distinct |> List.sort

let private inCalendar (days: DateTime seq) : DateTime list =
    days
    |> Seq.filter (fun d -> d >= smallFirst && d <= smallLast)
    |> Seq.distinct
    |> Seq.sort
    |> List.ofSeq

/// DATEADD by months as the sources state it (see `shift` in TimeIntelligence.fs for the quotes),
/// written with the calendar's own arithmetic: AddMonths goes to the same day, or to the last day
/// of a shorter month. None: the case the sources leave open.
let private dateAddOracle (selection: DateTime list) (months: int) : DateTime list option =
    let mutable open' = false

    let moved = [
        for (first, days) in selection |> List.groupBy monthStartOf do
            let last = monthEndOf first
            let target = first.AddMonths months

            if days.Length = DateTime.DaysInMonth(first.Year, first.Month) then
                yield! daysFrom target (monthEndOf target)
            else
                let hasLast = List.contains last days
                let hasDayBefore = List.contains (last.AddDays(-1.0)) days

                if hasLast && not hasDayBefore && monthEndOf(target).Day > last.Day then
                    open' <- true

                for d in days do
                    d.AddMonths months

                if hasLast && hasDayBefore then
                    yield! daysFrom (last.AddMonths months) (monthEndOf target)
    ]

    if open' then None else Some(inCalendar moved)

/// PARALLELPERIOD as Microsoft Learn states it: "shifts the first date and the last date the
/// specified number of intervals, and then returns all contiguous dates between the two shifted
/// dates", filled out "to complete the entire interval". None: a selection that skips an interval.
let private parallelOracle (selection: DateTime list) (n: int) (size: int) : DateTime list option =
    match selection with
    | [] -> Some []
    | _ ->
        let index (d: DateTime) = (d.Year * 12 + d.Month - 1) / size
        let used = selection |> List.map index |> List.distinct

        if List.max used - List.min used + 1 <> used.Length then
            None
        else
            let startOf (d: DateTime) =
                let m = (d.Month - 1) / size * size + 1
                DateTime(d.Year, m, 1)

            let first = startOf ((List.head selection).AddMonths(n * size))

            let last =
                (startOf ((List.last selection).AddMonths(n * size))).AddMonths(size).AddDays(-1.0)

            Some(allDays |> List.filter (fun d -> d >= first && d <= last))

/// Consecutive days: on the small calendar, which has every day, the selections DATEADD shifts.
let private isRun (days: DateTime list) =
    days |> List.pairwise |> List.forall (fun (a, b) -> b = a.AddDays 1.0)

let private refusedWithGaps (query: string) (result: Result<DateTime list, TessError>) =
    match result with
    | Error(DaxNotSupported(what, _)) -> Expect.stringContains what "selection of dates with gaps" query
    | other -> failtestf "expected the refusal of a selection with gaps, got %A\n%s" other query

let private unitOf (random: Random) =
    match random.Next 3 with
    | 0 -> "MONTH", 1
    | 1 -> "QUARTER", 3
    | _ -> "YEAR", 12

let private sameDates (p: Engine.Prepared) (seed: int) (a: string) (b: string) =
    Expect.equal (tryDates p a) (tryDates p b) $"seed {seed}:\n{a}\n{b}"

/// STARTOF* / ENDOF* as DAX Guide writes them out, with the Date table's own columns in place of
/// YEAR, MONTH and QUOTIENT (functions of another family): `keys` are the columns that say which
/// period a date is in.
let private edgeExpansion (isEnd: bool) (keys: string list) : string =
    let agg = if isEnd then "MAX" else "MIN"

    let keyVars =
        keys
        |> List.map (fun k -> $"VAR K{k} = MINX ( FILTER ( ALL ( 'Date' ), 'Date'[Date] = Visible ), 'Date'[{k}] )")
        |> String.concat " "

    let samePeriod =
        keys |> List.map (fun k -> $"'Date'[{k}] = K{k}") |> String.concat " && "

    $"VAR Visible = CALCULATE ( {agg} ( 'Date'[Date] ) ) {keyVars} VAR InPeriod = FILTER ( ALL ( 'Date' ), {samePeriod} ) VAR Edge = {agg}X ( InPeriod, 'Date'[Date] ) RETURN CALCULATETABLE ( VALUES ( 'Date'[Date] ), 'Date'[Date] = Edge )"

/// DATESYTD as SQLBI-MDT writes it ("DATESYTD is the very same as the following table expression"),
/// for any period: the dates of the whole column in the period of the last visible date, up to it.
let private toDateExpansion (keys: string list) : string =
    let keyVars =
        keys
        |> List.map (fun k -> $"VAR K{k} = MINX ( FILTER ( ALL ( 'Date' ), 'Date'[Date] = LastVisible ), 'Date'[{k}] )")
        |> String.concat " "

    let samePeriod =
        keys |> List.map (fun k -> $"'Date'[{k}] = K{k}") |> String.concat " && "

    $"VAR LastVisible = MAX ( 'Date'[Date] ) {keyVars} RETURN SELECTCOLUMNS ( FILTER ( ALL ( 'Date' ), {samePeriod} && 'Date'[Date] <= LastVisible ), \"Date\", 'Date'[Date] )"

[<Tests>]
let propertyTests =
    testList "Engine time intelligence: against the definitions" [
        testPropertyWithConfig config "DATEADD by DAY shifts every date"
        <| fun (seed: int) ->
            let random = Random(seed)
            let selection = selectionOf random
            let n = random.Next(-40, 41)
            let query = under (pick selection) $"DATEADD ( 'Date'[Date], {n}, DAY )"

            if isRun selection then
                Expect.equal
                    (dates small.Value query)
                    (inCalendar (selection |> List.map (fun d -> d.AddDays(float n))))
                    $"seed {seed}"
            else
                refusedWithGaps query (tryDates small.Value query)

        testPropertyWithConfig config "DATEADD by MONTH, QUARTER, YEAR: whole months, days, month ends"
        <| fun (seed: int) ->
            let random = Random(seed)
            let selection = selectionOf random
            let unit, months = unitOf random
            let n = random.Next(-3, 4)
            let query = under (pick selection) $"DATEADD ( 'Date'[Date], {n}, {unit} )"

            match dateAddOracle selection (n * months), tryDates small.Value query with
            | _, actual when not (isRun selection) -> refusedWithGaps query actual
            | Some expected, Ok actual -> Expect.equal actual expected $"seed {seed}: {query}"
            | None, Error(DaxNotSupported(what, _)) -> Expect.stringContains what "last day of a month" query
            | expected, actual -> failtestf "seed %d: expected %A, got %A\n%s" seed expected actual query

        testPropertyWithConfig config "PARALLELPERIOD: the first and last date shifted, whole intervals"
        <| fun (seed: int) ->
            let random = Random(seed)
            let selection = selectionOf random
            let unit, months = unitOf random
            let n = random.Next(-3, 4)
            let query = under (pick selection) $"PARALLELPERIOD ( 'Date'[Date], {n}, {unit} )"

            match parallelOracle selection n months, tryDates small.Value query with
            | Some expected, Ok actual -> Expect.equal actual expected $"seed {seed}: {query}"
            | None, Error(DaxNotSupported(what, _)) -> Expect.stringContains what "skips a whole" query
            | expected, actual -> failtestf "seed %d: expected %A, got %A\n%s" seed expected actual query

        // The boundary is the same day of the other month (AddMonths, for a day every month has),
        // and is not in the result (DG/datesinperiod).
        testPropertyWithConfig config "DATESINPERIOD from a day every month has"
        <| fun (seed: int) ->
            let random = Random(seed)

            let start =
                (monthStartOf allDays.[random.Next allDays.Length]).AddDays(float (random.Next 27))

            let unit, months = unitOf random
            let n = random.Next(-5, 6)
            let boundary = start.AddMonths(n * months)

            let expected =
                allDays
                |> List.filter (fun d ->
                    if n > 0 then d >= start && d < boundary
                    elif n < 0 then d > boundary && d <= start
                    else false)

            Expect.equal
                (dates small.Value $"DATESINPERIOD ( 'Date'[Date], {lit start}, {n}, {unit} )")
                expected
                $"seed {seed}"

            let days = random.Next(-50, 51)

            let expectedDays =
                allDays
                |> List.filter (fun d ->
                    if days > 0 then
                        d >= start && d < start.AddDays(float days)
                    elif days < 0 then
                        d > start.AddDays(float days) && d <= start
                    else
                        false)

            Expect.equal
                (dates small.Value $"DATESINPERIOD ( 'Date'[Date], {lit start}, {days}, DAY )")
                expectedDays
                $"seed {seed}"

        testPropertyWithConfig config "DATESBETWEEN is the dates of the column between the two, whatever is filtered"
        <| fun (seed: int) ->
            let random = Random(seed)
            let a = smallFirst.AddDays(float (random.Next(-30, 760)))
            let b = smallFirst.AddDays(float (random.Next(-30, 760)))
            let selection = selectionOf random

            sameDates
                small.Value
                seed
                (under (pick selection) (between a b))
                $"FILTER ( ALL ( 'Date'[Date] ), 'Date'[Date] >= {lit a} && 'Date'[Date] <= {lit b} )"

        testPropertyWithConfig config "FIRSTDATE and LASTDATE are MIN and MAX"
        <| fun (seed: int) ->
            let selection = selectionOf (Random(seed))
            let p = small.Value

            for (f, agg) in [ "FIRSTDATE", "MIN"; "LASTDATE", "MAX" ] do
                Expect.equal
                    (value p $"CALCULATE ( {f} ( 'Date'[Date] ), {pick selection} )")
                    (value p $"CALCULATE ( {agg} ( 'Date'[Date] ), {pick selection} )")
                    $"seed {seed}: {f}"

        testPropertyWithConfig config "STARTOF* and ENDOF* are DAX Guide's expansions"
        <| fun (seed: int) ->
            let selection = pick (selectionOf (Random(seed)))
            let p = small.Value

            for (name, keys, yearEnd) in
                [
                    "MONTH", [ "Year"; "MonthNo" ], ""
                    "QUARTER", [ "Year"; "Quarter" ], ""
                    "YEAR", [ "Year" ], ""
                    "YEAR", [ "FiscalYear" ], ", \"6/30\""
                ] do
                for (prefix, isEnd) in [ "STARTOF", false; "ENDOF", true ] do
                    sameDates
                        p
                        seed
                        (under selection $"{prefix}{name} ( 'Date'[Date]{yearEnd} )")
                        (under selection (edgeExpansion isEnd keys))

        testPropertyWithConfig config "DATESMTD, DATESQTD and DATESYTD are the period of the last date, up to it"
        <| fun (seed: int) ->
            let random = Random(seed)
            let days = selectionOf random

            let days =
                if days.IsEmpty then
                    [ allDays.[random.Next allDays.Length] ]
                else
                    days

            let selection = pick days
            let p = small.Value

            for (f, keys) in
                [
                    "DATESMTD ( 'Date'[Date] )", [ "Year"; "MonthNo" ]
                    "DATESQTD ( 'Date'[Date] )", [ "Year"; "Quarter" ]
                    "DATESYTD ( 'Date'[Date] )", [ "Year" ]
                    "DATESYTD ( 'Date'[Date], \"6/30\" )", [ "FiscalYear" ]
                    "DATESYTD ( 'Date'[Date], \"2000-06-30\" )", [ "FiscalYear" ]
                ] do
                sameDates p seed (under selection f) (under selection (toDateExpansion keys))

            // And DAX Guide's own form: DATESBETWEEN from STARTOF* ( LASTDATE ) to LASTDATE.
            for (f, start) in
                [
                    "DATESMTD ( 'Date'[Date] )", "STARTOFMONTH ( LASTDATE ( 'Date'[Date] ) )"
                    "DATESQTD ( 'Date'[Date] )", "STARTOFQUARTER ( LASTDATE ( 'Date'[Date] ) )"
                    "DATESYTD ( 'Date'[Date] )", "STARTOFYEAR ( LASTDATE ( 'Date'[Date] ) )"
                    "DATESYTD ( 'Date'[Date], \"6/30\" )", "STARTOFYEAR ( LASTDATE ( 'Date'[Date] ), \"6/30\" )"
                ] do
                sameDates
                    p
                    seed
                    (under selection f)
                    (under selection $"DATESBETWEEN ( 'Date'[Date], {start}, LASTDATE ( 'Date'[Date] ) )")

        testPropertyWithConfig config "the functions that are another function with fixed arguments"
        <| fun (seed: int) ->
            let random = Random(seed)
            let days = selectionOf random

            let days =
                if days.IsEmpty then
                    [ allDays.[random.Next allDays.Length] ]
                else
                    days

            let selection = pick days
            let p = small.Value

            for (f, expansion) in
                [
                    // DG/previousday, nextday, previousmonth ... nextyear.
                    "PREVIOUSDAY ( 'Date'[Date] )", "DATEADD ( FIRSTDATE ( 'Date'[Date] ), -1, DAY )"
                    "NEXTDAY ( 'Date'[Date] )", "DATEADD ( LASTDATE ( 'Date'[Date] ), 1, DAY )"
                    "PREVIOUSMONTH ( 'Date'[Date] )", "PARALLELPERIOD ( FIRSTDATE ( 'Date'[Date] ), -1, MONTH )"
                    "PREVIOUSQUARTER ( 'Date'[Date] )", "PARALLELPERIOD ( FIRSTDATE ( 'Date'[Date] ), -1, QUARTER )"
                    "PREVIOUSYEAR ( 'Date'[Date] )", "PARALLELPERIOD ( FIRSTDATE ( 'Date'[Date] ), -1, YEAR )"
                    "NEXTMONTH ( 'Date'[Date] )", "PARALLELPERIOD ( LASTDATE ( 'Date'[Date] ), 1, MONTH )"
                    "NEXTQUARTER ( 'Date'[Date] )", "PARALLELPERIOD ( LASTDATE ( 'Date'[Date] ), 1, QUARTER )"
                    "NEXTYEAR ( 'Date'[Date] )", "PARALLELPERIOD ( LASTDATE ( 'Date'[Date] ), 1, YEAR )"
                ] do
                sameDates p seed (under selection f) (under selection expansion)

        // DG/sameperiodlastyear: DATEADD ( dates, -1, YEAR ). Refused or wrong the same way, but
        // under its own name.
        testPropertyWithConfig config "SAMEPERIODLASTYEAR is DATEADD back a year"
        <| fun (seed: int) ->
            let random = Random(seed)
            let selection = pick (selectionOf random)
            let p = small.Value
            let query = under selection "SAMEPERIODLASTYEAR ( 'Date'[Date] )"

            match tryDates p query, tryDates p (under selection "DATEADD ( 'Date'[Date], -1, YEAR )") with
            | Ok a, Ok b -> Expect.equal a b $"seed {seed}: {query}"
            | Error(DaxNotSupported(a, _)), Error(DaxNotSupported(b, _)) ->
                Expect.equal a (b.Replace("DATEADD", "SAMEPERIODLASTYEAR")) $"seed {seed}: {query}"
            | a, b -> failtestf "seed %d: %A\n%A\n%s" seed a b query

        testPropertyWithConfig config "TOTAL*, OPENINGBALANCE* and CLOSINGBALANCE* are their CALCULATE"
        <| fun (seed: int) ->
            let random = Random(seed)
            let days = selectionOf random

            let days =
                if days.IsEmpty then
                    [ allDays.[random.Next allDays.Length] ]
                else
                    days

            let selection = pick days
            let p = small.Value

            let filter, filterArgument =
                match random.Next 3 with
                | 0 -> "", ""
                | 1 -> ", 'Date'[Quarter] <= 2", ", 'Date'[Quarter] <= 2"
                | _ -> ", Sales[Amount] >= 100", ", Sales[Amount] >= 100"

            for (f, expansion) in
                [
                    $"TOTALMTD ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], DATESMTD ( 'Date'[Date] ){filter} )"
                    $"TOTALQTD ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], DATESQTD ( 'Date'[Date] ){filter} )"
                    $"TOTALYTD ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], DATESYTD ( 'Date'[Date] ){filter} )"
                    "TOTALYTD ( [Sales], 'Date'[Date], \"6/30\" )",
                    "CALCULATE ( [Sales], DATESYTD ( 'Date'[Date], \"6/30\" ) )"
                    $"CLOSINGBALANCEMONTH ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], ENDOFMONTH ( 'Date'[Date] ){filter} )"
                    $"CLOSINGBALANCEQUARTER ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], ENDOFQUARTER ( 'Date'[Date] ){filter} )"
                    $"CLOSINGBALANCEYEAR ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], ENDOFYEAR ( 'Date'[Date] ){filter} )"
                    "CLOSINGBALANCEYEAR ( [Sales], 'Date'[Date], \"6/30\" )",
                    "CALCULATE ( [Sales], ENDOFYEAR ( 'Date'[Date], \"6/30\" ) )"
                    $"OPENINGBALANCEMONTH ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], CALCULATETABLE ( ENDOFMONTH ( 'Date'[Date] ), PREVIOUSMONTH ( 'Date'[Date] ) ){filter} )"
                    $"OPENINGBALANCEQUARTER ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], CALCULATETABLE ( ENDOFQUARTER ( 'Date'[Date] ), PREVIOUSQUARTER ( 'Date'[Date] ) ){filter} )"
                    $"OPENINGBALANCEYEAR ( [Sales], 'Date'[Date]{filterArgument} )",
                    $"CALCULATE ( [Sales], CALCULATETABLE ( ENDOFYEAR ( 'Date'[Date] ), PREVIOUSYEAR ( 'Date'[Date] ) ){filter} )"
                    "OPENINGBALANCEYEAR ( [Sales], 'Date'[Date], \"6/30\" )",
                    "CALCULATE ( [Sales], CALCULATETABLE ( ENDOFYEAR ( 'Date'[Date], \"6/30\" ), PREVIOUSYEAR ( 'Date'[Date], \"6/30\" ) ) )"
                ] do
                Expect.equal
                    (value p $"CALCULATE ( {f}, {selection} )")
                    (value p $"CALCULATE ( {expansion}, {selection} )")
                    $"seed {seed}:\n{f}\n{expansion}"

        // The opening balance, by its meaning: the sales of the last day before the period of the
        // first selected date. And the closing balance: of the last day of the period of the last.
        testPropertyWithConfig config "OPENINGBALANCEMONTH and CLOSINGBALANCEMONTH against the calendar"
        <| fun (seed: int) ->
            let random = Random(seed)
            let days = selectionOf random

            let days =
                if days.IsEmpty then
                    [ allDays.[random.Next allDays.Length] ]
                else
                    days

            let p = small.Value

            let salesOn (d: DateTime) =
                match sales |> List.filter (fun (x, _) -> x = d) with
                | [] -> QueryValue.Blank
                | rows -> QueryValue.Decimal(rows |> List.sumBy snd)

            Expect.equal
                (value p $"CALCULATE ( OPENINGBALANCEMONTH ( [Sales], 'Date'[Date] ), {pick days} )")
                (salesOn ((monthStartOf (List.head days)).AddDays(-1.0)))
                $"seed {seed}: opening"

            Expect.equal
                (value p $"CALCULATE ( CLOSINGBALANCEMONTH ( [Sales], 'Date'[Date] ), {pick days} )")
                (salesOn (monthEndOf (List.last days)))
                $"seed {seed}: closing"

        // SQLBI-TI: the engine "automatically adds an ALL function over the Calendar table":
        // CALCULATE ( ..., DATESYTD ( 'Calendar'[Date] ), ALL ( 'Calendar' ) ).
        testPropertyWithConfig config "a time intelligence filter is the same with ALL ( 'Date' ) written out"
        <| fun (seed: int) ->
            let random = Random(seed)
            let p = small.Value

            let outer =
                [
                    "'Date'[Year] = 2024"
                    "'Date'[Quarter] IN { 1, 3 }"
                    "'Date'[YearMonth] >= 202311 && 'Date'[YearMonth] <= 202402"
                    "'Date'[MonthNo] = 2"
                    "Sales[Amount] > 50"
                    "'Date'[FiscalYear] = 2024"
                ]
                |> List.filter (fun _ -> random.Next 3 = 0)

            let outer =
                if outer.IsEmpty then
                    [ "'Date'[YearMonth] = 202403" ]
                else
                    outer

            let f =
                [|
                    "DATESYTD ( 'Date'[Date] )"
                    "DATESMTD ( 'Date'[Date] )"
                    "SAMEPERIODLASTYEAR ( 'Date'[Date] )"
                    "PARALLELPERIOD ( LASTDATE ( 'Date'[Date] ), -1, QUARTER )"
                    "PREVIOUSMONTH ( 'Date'[Date] )"
                    "DATESINPERIOD ( 'Date'[Date], MAX ( 'Date'[Date] ), -1, YEAR )"
                    "'Date'[Date] = dt\"2023-06-30\""
                    "LASTDATE ( 'Date'[Date] )"
                |]
                    .[random.Next 8]

            let within (inner: string) =
                let filters = String.concat ", " outer
                $"EVALUATE ROW ( \"r\", CALCULATE ( {inner}, {filters} ) )"

            let a = Engine.evaluateQuery p (within $"CALCULATE ( [Sales], {f} )")

            let b =
                Engine.evaluateQuery p (within $"CALCULATE ( [Sales], {f}, ALL ( 'Date' ) )")

            match a, b with
            | Ok x, Ok y -> Expect.equal x y $"seed {seed}: {f} under {outer}"
            // The same refusal either way (an empty selection, a month end alone).
            | Error(DaxNotSupported(x, _)), Error(DaxNotSupported(y, _)) -> Expect.equal x y $"seed {seed}"
            | _ -> failtestf "seed %d: %A\n%A" seed a b
    ]
