/// The date and time functions of the local DAX engine. Expected values are the ones the pages
/// show (Microsoft Learn, dax.guide: named at each test) or are worked out by hand from the rules
/// they state. The clock is always pinned.
module Tessera.Tests.EngineDateTimeTests

open System
open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.DaxEngine.Functions
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.EngineHelpers
open Tessera.Cli.Common

/// 2024-03-05 14:30:15.250 local, two hours ahead of UTC.
let private pinned: Engine.Clock = {
    Now = DateTime(2024, 3, 5, 14, 30, 15, 250)
    UtcNow = DateTime(2024, 3, 5, 12, 30, 15, 250)
}

let private prepareAt (clock: Engine.Clock) (m: TestModel) : Engine.Prepared =
    let db, store = build m

    match
        Engine.prepareWith
            {
                Engine.Options.defaults with
                    Clock = Some clock
            }
            db
            store
    with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private prepared = lazy (prepareAt pinned star)
let private p () = prepared.Value

/// The star model with another culture.
let private preparedIn (culture: string) =
    let db, store = build star
    db.Model.Culture <- culture

    match
        Engine.prepareWith
            {
                Engine.Options.defaults with
                    Clock = Some pinned
            }
            db
            store
    with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private d (y: int) (m: int) (day: int) = box (DateTime(y, m, day))

let private dt (y: int) (m: int) (day: int) (h: int) (mi: int) (s: int) = box (DateTime(y, m, day, h, mi, s))

let private code (dax: string) = TessError.code (scalarError (p ()) dax)

/// A DAX error, the kind IFERROR catches.
let private isDaxError (dax: string) =
    Expect.equal (code dax) "dax-error" dax
    is (p ()) $"ISERROR ( {dax} )" true

/// Refused: named, and not something IFERROR turns into a value.
let private refused (dax: string) (mentions: string) =
    match scalarError (p ()) dax with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported, got %A\n%s" other dax

    Expect.equal (TessError.code (scalarError (p ()) $"ISERROR ( {dax} )")) "not-supported" "not caught by ISERROR"

/// The ten dates of dax.guide's WEEKDAY and WEEKNUM examples: January 1 to 10, 2005.
let private january2005 =
    [ 1..10 ] |> List.map (fun day -> $"DATE ( 2005, 1, {day} )")

let private setup (m: TestModel) =
    let t = new TempDir()
    let db, store = build m
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    let dir = t.Combine "data"
    Directory.CreateDirectory dir |> ignore

    for table in store.Tables do
        use writer =
            new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

        Fixture.writeCsv writer table

    t, path, dir

[<Tests>]
let tests =
    testList "Engine date and time" [
        testList "DATE" [
            // Microsoft Learn, DATE: the examples.
            testCase "the documented examples" (fun () ->
                is (p ()) "DATE ( 2009, 7, 8 )" (d 2009 7 8)
                is (p ()) "DATE ( 08, 1, 2 )" (d 2008 1 2)
                is (p ()) "DATE ( 50, 1, 2 )" (d 1950 1 2)
                is (p ()) "DATE ( 2008, 1, 2 )" (d 2008 1 2)
                is (p ()) "DATE ( 2008, 14, 2 )" (d 2009 2 2)
                is (p ()) "DATE ( 2008, 1, 35 )" (d 2008 2 4)
                is (p ()) "DATE ( 2008, 18, 1 )" (d 2009 6 1)
                is (p ()) "DATE ( 2008, 3, 32 )" (d 2008 4 1))
            // dax.guide, DATE: "Notes about the arguments", each with its example.
            testCase "the year, month and day rules" (fun () ->
                is (p ()) "DATE ( 98, 1, 2 )" (d 1998 1 2)
                is (p ()) "DATE ( 49, 1, 2 )" (d 2049 1 2)
                is (p ()) "DATE ( 100, 1, 2 )" (d 100 1 2)
                is (p ()) "DATE ( 1850, 1, 2 )" (d 1850 1 2)
                is (p ()) "DATE ( 9999, 12, 31 )" (d 9999 12 31)
                is (p ()) "DATE ( 2008, 0, 2 )" (d 2007 12 2)
                is (p ()) "DATE ( 2008, -3, 2 )" (d 2007 9 2)
                is (p ()) "DATE ( 2008, 1, -15 )" (d 2007 12 16)
                is (p ()) "DATE ( 2020, 10, 32 )" (d 2020 11 1)
                is (p ()) "DATE ( 2020, 20, 1 )" (d 2021 8 1)
                is (p ()) "DATE ( 2008, 1, 2.5 )" (d 2008 1 3)
                is (p ()) "DATE ( 2008, 1, 2.4 )" (d 2008 1 2)
                is (p ()) "DATE ( 2007.6, 1, 2 )" (d 2008 1 2)
                is (p ()) "DATE ( 2008, BLANK (), 2 )" (d 2007 12 2)
                is (p ()) "DATE ( BLANK (), 1, 2 )" (d 2000 1 2))
            // 1900 was not a leap year: DAX dates are a real calendar, not Excel's serial numbers.
            testCase "February 1900 has 28 days" (fun () ->
                is (p ()) "DATE ( 1900, 2, 29 )" (d 1900 3 1)
                is (p ()) "DATE ( 1900, 3, 1 ) - DATE ( 1900, 2, 28 ) = 1" true
                is (p ()) "DATE ( 1900, 3, 1 ) = 61" true
                is (p ()) "DATE ( 2000, 2, 29 )" (d 2000 2 29)
                is (p ()) "DATE ( 2100, 2, 29 )" (d 2100 3 1))
            testCase "errors" (fun () ->
                isDaxError "DATE ( 2008, 1, 0 )"
                isDaxError "DATE ( 2008, 1, BLANK () )"
                isDaxError "DATE ( 10000, 1, 1 )"
                isDaxError "DATE ( 9999, 12, 32 )"
                isDaxError "DATE ( 9999, 13, 1 )")
            testCase "what the sources leave open is refused" (fun () ->
                refused "DATE ( -50, 1, 2 )" "negative year"
                refused "DATE ( 2008, 1, -2.5 )" "halfway"
                refused "DATE ( 100, 1, -5 )" "before the year 100"
                refused "DATE ( \"2008\", 1, 1 )" "not a number"
                refused "DATE ( dt\"2008-01-01\", 1, 1 )" "not a number")
        ]

        testList "TIME" [
            // Microsoft Learn, TIME; dax.guide, TIME.
            testCase "the documented examples" (fun () ->
                is (p ()) "TIME ( 22, 45, 30 )" (dt 1899 12 30 22 45 30)
                is (p ()) "TIME ( 27, 0, 0 )" (dt 1899 12 30 3 0 0)
                is (p ()) "TIME ( 0, 750, 0 )" (dt 1899 12 30 12 30 0)
                is (p ()) "TIME ( 0, 2190, 0 )" (dt 1899 12 30 12 30 0)
                is (p ()) "TIME ( 0, 0, 2000 )" (dt 1899 12 30 0 33 20)
                is (p ()) "TIME ( 10, 90, 0 )" (dt 1899 12 30 11 30 0)
                is (p ()) "TIME ( 50, 0, 0 )" (dt 1899 12 30 2 0 0)
                is (p ()) "TIME ( 12, 0, 0 ) = 0.5" true
                is (p ()) "TIME ( 0, 0, 0 )" (d 1899 12 30)
                is (p ()) "TIME ( 32767, 32767, 32767 )" (dt 1899 12 30 10 13 7))
            testCase "errors and refusals" (fun () ->
                isDaxError "TIME ( 32768, 0, 0 )"
                isDaxError "TIME ( 0, 0, 40000 )"
                refused "TIME ( -1, 0, 0 )" "negative hour"
                refused "TIME ( 1.5, 0, 0 )" "not a whole number"
                refused "TIME ( 1, BLANK (), 0 )" "BLANK minute")
            testCase "a date plus a time" (fun () ->
                is (p ()) "DATE ( 2020, 10, 15 ) + TIME ( 22, 45, 30 )" (dt 2020 10 15 22 45 30))
        ]

        testList "text" [
            // dax.guide, DATEVALUE: the two examples.
            testCase "DATEVALUE" (fun () ->
                is (p ()) "DATEVALUE ( \"10/15/2020\" )" (d 2020 10 15)
                is (p ()) "DATEVALUE ( \"10-15-2020\" )" (d 2020 10 15)
                is (p ()) "DATEVALUE ( \"28/02/2020\" )" (d 2020 2 28)
                is (p ()) "DATEVALUE ( \"02/28/2020\" )" (d 2020 2 28)
                is (p ()) "DATEVALUE ( \"08/10/2020\" )" (d 2020 8 10)
                is (p ()) "DATEVALUE ( \"2020-02-06\" )" (d 2020 2 6)
                is (p ()) "DATEVALUE ( \"1/8/2009\" )" (d 2009 1 8)
                is (p ()) "DATEVALUE ( \"1/31/2009\" )" (d 2009 1 31)
                is (p ()) "DATEVALUE ( \"20/4/2018\" )" (d 2018 4 20)
                // "Time information in the date_text argument is ignored."
                is (p ()) "DATEVALUE ( \"2020-02-06 13:45:00\" )" (d 2020 2 6)
                is (p ()) "DATEVALUE ( \"March 3, 2008 3:00 PM\" )" (d 2008 3 3))
            testCase "TIMEVALUE" (fun () ->
                is (p ()) "TIMEVALUE ( \"20:45:30\" )" (dt 1899 12 30 20 45 30)
                is (p ()) "TIMEVALUE ( \"12:45:01\" )" (dt 1899 12 30 12 45 1)
                is (p ()) "TIMEVALUE ( \"4:48 PM\" )" (dt 1899 12 30 16 48 0)
                is (p ()) "TIMEVALUE ( \"12:05 AM\" )" (dt 1899 12 30 0 5 0)
                is (p ()) "TIMEVALUE ( \"12:05 PM\" )" (dt 1899 12 30 12 5 0)
                // "Any date information included in the time_text argument is ignored."
                is (p ()) "TIMEVALUE ( \"2020-02-06T13:45:10\" )" (dt 1899 12 30 13 45 10))
            // The examples of Microsoft Learn's pages for HOUR, MINUTE, SECOND, MONTH, DAY, WEEKNUM,
            // EOMONTH and EDATE.
            testCase "text arguments of the other functions" (fun () ->
                is (p ()) "HOUR ( \"March 3, 2008 3:00 PM\" )" 15
                is (p ()) "MINUTE ( \"March 23, 2008 1:45 PM\" )" 45
                is (p ()) "SECOND ( \"March 3, 2008 12:00:03\" )" 3
                is (p ()) "MONTH ( \"March 3, 2008 3:45 PM\" )" 3
                is (p ()) "DAY ( \"March 4 2007\" )" 4
                is (p ()) "WEEKNUM ( \"Feb 14, 2010\", 2 )" 7
                is (p ()) "EOMONTH ( \"March 3, 2008\", 1.5 )" (d 2008 5 31)
                is (p ()) "EDATE ( \"2009-01-29\", 1 )" (d 2009 2 28)
                is (p ()) "EDATE ( \"2009-01-30\", 1 )" (d 2009 2 28)
                is (p ()) "EDATE ( \"2009-01-31\", 1 )" (d 2009 2 28)
                is (p ()) "YEAR ( \"2024-01-31\" )" 2024
                is (p ()) "HOUR ( \"16:48:00\" )" 16)
            testCase "text this doesn't read is refused, not guessed" (fun () ->
                refused "DATEVALUE ( \"March 2007\" )" "not a date the local engine reads"
                refused "DATEVALUE ( \"1/8\" )" "not a date the local engine reads"
                refused "DATEVALUE ( \"1/8/09\" )" "not a date the local engine reads"
                refused "DATEVALUE ( \"Mar-3-2003\" )" "not a date the local engine reads"
                refused "DATEVALUE ( \"13/13/2020\" )" "not a date the local engine reads"
                refused "DATEVALUE ( \"2020-02-30\" )" "not a date the local engine reads"
                refused "TIMEVALUE ( \"12.45.01 AM\" )" "not a date the local engine reads"
                refused "TIMEVALUE ( \"15:45:01 AM\" )" "not a date the local engine reads"
                refused "TIMEVALUE ( \"25:00\" )" "not a date the local engine reads"
                refused "YEAR ( \"yesterday\" )" "YEAR of text"
                refused "DATEVALUE ( \"10:30\" )" "a time and no date"
                refused "TIMEVALUE ( \"2020-02-06\" )" "a date and no time"
                refused "YEAR ( \"10:30\" )" "a time and no date"
                refused "DATEVALUE ( BLANK () )" "BLANK"
                refused "DATEVALUE ( 5 )" "not text"
                refused "YEAR ( TRUE )" "TRUE or FALSE")
            testCase "only ISO text is read when the model's culture is not en-US" (fun () ->
                let fr = preparedIn "fr-FR"
                is fr "DATEVALUE ( \"2020-02-06\" )" (d 2020 2 6)
                is fr "HOUR ( \"2020-02-06T16:48:00\" )" 16
                is fr "HOUR ( \"16:48\" )" 16

                for dax in
                    [
                        "DATEVALUE ( \"10/15/2020\" )"
                        "DATEVALUE ( \"15/10/2020\" )"
                        "MONTH ( \"March 3, 2008\" )"
                        "HOUR ( \"4:48 PM\" )"
                    ] do
                    Expect.equal (TessError.code (scalarError fr dax)) "not-supported" dax)
            testCase "DateText.read" (fun () ->
                let read (text: string) =
                    DateText.read "en-US" text
                    |> Option.map (fun r -> r.Date |> ValueOption.toOption, r.Time |> ValueOption.toOption)

                Expect.equal (read " 2024-01-31 ") (Some(Some(DateTime(2024, 1, 31)), None)) "ISO date"

                Expect.equal
                    (read "2024-01-31T07:08")
                    (Some(Some(DateTime(2024, 1, 31)), Some(TimeSpan(7, 8, 0))))
                    "ISO date and time"

                Expect.equal (read "7:08:09") (Some(None, Some(TimeSpan(7, 8, 9)))) "a time"

                Expect.equal (read "jan 1 2007") (Some(Some(DateTime(2007, 1, 1)), None)) "month names ignore case"

                Expect.equal (read "2/29/2023") None "not a date either way round"
                Expect.equal (read "") None "empty"
                Expect.equal (read "2024-01-31T07:08:09.5") None "fractions of a second"
                Expect.equal (read "2024-01-31T07:08:09Z") None "time zones"
                Expect.equal (DateText.read "" "1/8/2009") None "no culture: ISO only"
                Expect.isSome (DateText.read "EN-us" "1/8/2009") "the culture's case doesn't matter")
        ]

        testList "parts" [
            testCase "YEAR, MONTH, DAY, QUARTER" (fun () ->
                is (p ()) "YEAR ( DATE ( 2008, 11, 20 ) )" 2008
                is (p ()) "MONTH ( DATE ( 2008, 11, 20 ) )" 11
                is (p ()) "DAY ( DATE ( 2008, 11, 20 ) )" 20
                is (p ()) "QUARTER ( DATE ( 2019, 2, 1 ) )" 1
                is (p ()) "QUARTER ( DATE ( 2018, 12, 31 ) )" 4
                is (p ()) "QUARTER ( DATE ( 2008, 3, 31 ) )" 1
                is (p ()) "QUARTER ( DATE ( 2008, 4, 1 ) )" 2
                is (p ()) "YEAR ( dt\"2020-10-15T22:45:30\" )" 2020
                is (p ()) "DAY ( dt\"2020-10-15T23:59:59\" )" 15
                is (p ()) "YEAR ( DATE ( 1850, 1, 2 ) )" 1850
                // A number is its serial: 45000 days after December 30, 1899 is March 15, 2023.
                is (p ()) "YEAR ( 45000 )" 2023
                is (p ()) "MONTH ( 45000 )" 3
                is (p ()) "DAY ( 45000.75 )" 15
                is (p ()) "DAY ( 61 )" 1)
            // Microsoft Learn, QUARTER: "If the input value is BLANK, the output value is also BLANK".
            testCase "BLANK" (fun () ->
                is (p ()) "QUARTER ( BLANK () )" null
                refused "YEAR ( BLANK () )" "YEAR of BLANK"
                refused "MONTH ( BLANK () )" "MONTH of BLANK"
                refused "DAY ( BLANK () )" "DAY of BLANK"
                refused "HOUR ( BLANK () )" "HOUR of BLANK"
                refused "WEEKDAY ( BLANK () )" "WEEKDAY of BLANK")
            testCase "HOUR, MINUTE, SECOND" (fun () ->
                is (p ()) "HOUR ( dt\"2020-10-15T22:45:30\" )" 22
                is (p ()) "MINUTE ( dt\"2020-10-15T22:45:30\" )" 45
                is (p ()) "SECOND ( dt\"2020-10-15T22:45:30\" )" 30
                is (p ()) "HOUR ( DATE ( 2020, 10, 15 ) )" 0
                is (p ()) "HOUR ( TIME ( 23, 59, 59 ) )" 23
                // Microsoft Learn, SECOND: "the value 25.56 ... 1:26:24 PM".
                is (p ()) "SECOND ( 25.56 )" 24
                is (p ()) "MINUTE ( 25.56 )" 26
                is (p ()) "HOUR ( 25.56 )" 13
                // Before the zero date the time still runs forwards.
                is (p ()) "HOUR ( dt\"1850-06-01T10:20:30\" )" 10
                is (p ()) "SECOND ( dt\"1850-06-01T10:20:30\" )" 30)
            testCase "what the sources leave open is refused" (fun () ->
                // Microsoft Learn's SECOND page dates 25.56 as January 25, 1900 (Excel's calendar);
                // counting from December 30, 1899 gives January 24.
                refused "DAY ( 25.56 )" "below 61"
                refused "YEAR ( 1 )" "below 61"
                refused "WEEKDAY ( 60 )" "below 61"
                refused "HOUR ( -0.25 )" "negative number"
                // NOW is 14:30:15.250: the same second, minute and hour rounded or cut off.
                is (p ()) "SECOND ( NOW () )" 15
                is (p ()) "MINUTE ( NOW () )" 30
                refused "SECOND ( dt\"2024-01-31T10:20:30\" + 0.5 / 86400 )" "fraction of a second"
                is (p ()) "MINUTE ( dt\"2024-01-31T10:20:30\" + 0.5 / 86400 )" 20
                refused "MINUTE ( dt\"2024-01-31T10:20:59\" + 0.5 / 86400 )" "fraction of a second"
                is (p ()) "HOUR ( dt\"2024-01-31T10:20:59\" + 0.5 / 86400 )" 10
                refused "HOUR ( dt\"2024-01-31T10:59:59\" + 0.5 / 86400 )" "fraction of a second"
                refused "DAY ( dt\"2024-01-31T23:59:59\" + 0.5 / 86400 )" "last second"
                refused "MONTH ( DATE ( 100, 1, 1 ) - 1 )" "before the year 100")
            testCase "YEAR of a date before the year 100 is an error" (fun () ->
                isDaxError "YEAR ( DATE ( 100, 1, 1 ) - 1 )")
        ]

        testList "weeks" [
            // dax.guide, WEEKDAY: the table of its example, January 1 to 10, 2005.
            testCase "WEEKDAY: the published table" (fun () ->
                let expected = [
                    [ 7; 7; 6; 5; 6; 5; 4; 3 ]
                    [ 1; 1; 7; 6; 7; 6; 5; 4 ]
                    [ 2; 2; 1; 0; 1; 7; 6; 5 ]
                    [ 3; 3; 2; 1; 2; 1; 7; 6 ]
                    [ 4; 4; 3; 2; 3; 2; 1; 7 ]
                    [ 5; 5; 4; 3; 4; 3; 2; 1 ]
                    [ 6; 6; 5; 4; 5; 4; 3; 2 ]
                    [ 7; 7; 6; 5; 6; 5; 4; 3 ]
                    [ 1; 1; 7; 6; 7; 6; 5; 4 ]
                    [ 2; 2; 1; 0; 1; 7; 6; 5 ]
                ]

                for date, row in List.zip january2005 expected do
                    let forms = [
                        $"WEEKDAY ( {date} )"
                        $"WEEKDAY ( {date}, 1 )"
                        $"WEEKDAY ( {date}, 2 )"
                        $"WEEKDAY ( {date}, 3 )"
                        $"WEEKDAY ( {date}, 11 )"
                        $"WEEKDAY ( {date}, 12 )"
                        $"WEEKDAY ( {date}, 13 )"
                        $"WEEKDAY ( {date}, 14 )"
                    ]

                    for dax, value in List.zip forms row do
                        is (p ()) dax value)
            // dax.guide's list of return types: 15 Friday, 16 Saturday, 17 Sunday first.
            testCase "WEEKDAY: the other return types" (fun () ->
                // January 7, 2005 was a Friday.
                is (p ()) "WEEKDAY ( DATE ( 2005, 1, 7 ), 15 )" 1
                is (p ()) "WEEKDAY ( DATE ( 2005, 1, 7 ), 16 )" 7
                is (p ()) "WEEKDAY ( DATE ( 2005, 1, 7 ), 17 )" 6
                is (p ()) "WEEKDAY ( DATE ( 2005, 1, 7 ), )" 6
                // Before 1900 too: July 4, 1776 was a Thursday.
                is (p ()) "WEEKDAY ( DATE ( 1776, 7, 4 ) )" 5)
            // dax.guide, WEEKNUM: the table of its example.
            testCase "WEEKNUM: the published table" (fun () ->
                let expected = [
                    [ 1; 1; 1; 1; 1; 53 ]
                    [ 2; 1; 1; 1; 1; 53 ]
                    [ 2; 2; 2; 1; 1; 1 ]
                    [ 2; 2; 2; 2; 1; 1 ]
                    [ 2; 2; 2; 2; 2; 1 ]
                    [ 2; 2; 2; 2; 2; 1 ]
                    [ 2; 2; 2; 2; 2; 1 ]
                    [ 2; 2; 2; 2; 2; 1 ]
                    [ 3; 2; 2; 2; 2; 1 ]
                    [ 3; 3; 3; 2; 2; 2 ]
                ]

                for date, row in List.zip january2005 expected do
                    let forms = [
                        $"WEEKNUM ( {date} )"
                        $"WEEKNUM ( {date}, 2 )"
                        $"WEEKNUM ( {date}, 11 )"
                        $"WEEKNUM ( {date}, 12 )"
                        $"WEEKNUM ( {date}, 13 )"
                        $"WEEKNUM ( {date}, 21 )"
                    ]

                    for dax, value in List.zip forms row do
                        is (p ()) dax value)
            testCase "WEEKNUM: year ends" (fun () ->
                // December 31, 2005 was a Saturday; January 1, 2005 too, so week 1 was one day long.
                is (p ()) "WEEKNUM ( DATE ( 2005, 12, 31 ) )" 53
                is (p ()) "WEEKNUM ( DATE ( 2005, 12, 31 ), 1 )" 53
                is (p ()) "WEEKNUM ( DATE ( 2005, 12, 31 ), 17 )" 53
                // With weeks from Saturday, January 1 began a whole week.
                is (p ()) "WEEKNUM ( DATE ( 2005, 12, 31 ), 16 )" 53
                is (p ()) "WEEKNUM ( DATE ( 2005, 12, 30 ), 16 )" 52
                // ISO: December 29, 2008 (a Monday) is in week 1 of 2009; January 1, 2010 (a Friday)
                // in week 53 of 2009; December 28 is always in the last week.
                is (p ()) "WEEKNUM ( DATE ( 2008, 12, 29 ), 21 )" 1
                is (p ()) "WEEKNUM ( DATE ( 2010, 1, 1 ), 21 )" 53
                is (p ()) "WEEKNUM ( DATE ( 2010, 1, 4 ), 21 )" 1
                is (p ()) "WEEKNUM ( DATE ( 2020, 12, 28 ), 21 )" 53)
            testCase "a return type the pages don't list is refused" (fun () ->
                refused "WEEKDAY ( DATE ( 2005, 1, 1 ), 4 )" "return type 4"
                refused "WEEKDAY ( DATE ( 2005, 1, 1 ), 21 )" "return type 21"
                refused "WEEKNUM ( DATE ( 2005, 1, 1 ), 3 )" "return type 3"
                refused "WEEKNUM ( DATE ( 2005, 1, 1 ), 1 + 9 )" "return type 10"
                refused "WEEKNUM ( DATE ( 2005, 1, 1 ), BLANK () )" "BLANK return type"
                refused "WEEKNUM ( DATE ( 2005, 1, 1 ), 1.5 )" "not a whole number")
        ]

        testList "EOMONTH and EDATE" [
            // dax.guide, EOMONTH: the table of its example, for the first days of January 2005.
            testCase "the published table" (fun () ->
                for day in 1..9 do
                    let date = $"DATE ( 2005, 1, {day} )"
                    is (p ()) $"EDATE ( {date}, +1 )" (d 2005 2 day)
                    is (p ()) $"EDATE ( {date}, -1 )" (d 2004 12 day)
                    is (p ()) $"EOMONTH ( {date}, 0 )" (d 2005 1 31)
                    is (p ()) $"EOMONTH ( {date}, +1 )" (d 2005 2 28)
                    is (p ()) $"EOMONTH ( {date}, -1 )" (d 2004 12 31)
                    is (p ()) $"EOMONTH ( {date}, -1 ) + 1" (d 2005 1 1)
                    is (p ()) $"EOMONTH ( {date}, -2 ) + 1" (d 2004 12 1))
            testCase "month ends and leap years" (fun () ->
                is (p ()) "EDATE ( DATE ( 2023, 1, 31 ), 1 )" (d 2023 2 28)
                is (p ()) "EDATE ( DATE ( 2024, 1, 31 ), 1 )" (d 2024 2 29)
                is (p ()) "EDATE ( DATE ( 2024, 2, 29 ), 12 )" (d 2025 2 28)
                is (p ()) "EDATE ( DATE ( 2024, 2, 29 ), -48 )" (d 2020 2 29)
                is (p ()) "EDATE ( DATE ( 2024, 3, 31 ), -1 )" (d 2024 2 29)
                is (p ()) "EDATE ( DATE ( 2024, 11, 30 ), 3 )" (d 2025 2 28)
                is (p ()) "EOMONTH ( DATE ( 2024, 2, 1 ), 0 )" (d 2024 2 29)
                is (p ()) "EOMONTH ( DATE ( 2024, 12, 15 ), 2 )" (d 2025 2 28)
                is (p ()) "EOMONTH ( DATE ( 2024, 1, 15 ), -13 )" (d 2022 12 31))
            // Microsoft Learn: EOMONTH's months are "rounded up or down to the nearest integer";
            // EDATE's: "If months is not an integer, it is truncated".
            testCase "months that are not whole" (fun () ->
                is (p ()) "EOMONTH ( DATE ( 2008, 3, 3 ), 1.5 )" (d 2008 5 31)
                is (p ()) "EOMONTH ( DATE ( 2008, 3, 3 ), 1.4 )" (d 2008 4 30)
                is (p ()) "EOMONTH ( DATE ( 2008, 3, 3 ), -1.4 )" (d 2008 2 29)
                is (p ()) "EOMONTH ( DATE ( 2008, 3, 3 ), -1.6 )" (d 2008 1 31)
                is (p ()) "EDATE ( DATE ( 2008, 3, 3 ), 1.9 )" (d 2008 4 3)
                is (p ()) "EDATE ( DATE ( 2008, 3, 3 ), -1.9 )" (d 2008 2 3)
                refused "EOMONTH ( DATE ( 2008, 3, 3 ), -1.5 )" "halfway")
            // Microsoft Learn, EOMONTH: "Dates before March 1st of 1900 and after December 31st of
            // 9999 are invalid."
            testCase "the range of dates" (fun () ->
                isDaxError "EOMONTH ( DATE ( 1900, 3, 15 ), -1 )"
                isDaxError "EOMONTH ( DATE ( 1900, 2, 15 ), 0 )"
                isDaxError "EOMONTH ( DATE ( 9999, 12, 1 ), 1 )"
                isDaxError "EDATE ( DATE ( 9999, 12, 1 ), 1 )"
                is (p ()) "EOMONTH ( DATE ( 1900, 3, 1 ), 0 )" (d 1900 3 31)
                is (p ()) "EOMONTH ( DATE ( 9999, 11, 1 ), 1 )" (d 9999 12 31)
                refused "EDATE ( DATE ( 1900, 3, 15 ), -1 )" "before March 1, 1900"
                refused "EDATE ( DATE ( 1850, 3, 15 ), 1 )" "before March 1, 1900")
            testCase "refusals" (fun () ->
                refused "EOMONTH ( BLANK (), 1 )" "BLANK date"
                refused "EDATE ( DATE ( 2024, 1, 1 ), BLANK () )" "BLANK number of months"
                refused "EOMONTH ( NOW (), 0 )" "time of day"
                refused "EDATE ( dt\"2024-01-31T10:00:00\", 1 )" "time of day")
            // A number is converted to a date (Microsoft Learn, EOMONTH).
            testCase "a number as the start date" (fun () -> is (p ()) "EOMONTH ( 45000, 0 )" (d 2023 3 31))
        ]

        testList "DATEDIFF" [
            // Microsoft Learn, DATEDIFF: the example.
            testCase "the documented example" (fun () ->
                let a, b = "DATE ( 2019, 07, 01 )", "DATE ( 2021, 12, 31 )"
                is (p ()) $"DATEDIFF ( {a}, {b}, YEAR )" 2
                is (p ()) $"DATEDIFF ( {a}, {b}, QUARTER )" 9
                is (p ()) $"DATEDIFF ( {a}, {b}, MONTH )" 29
                is (p ()) $"DATEDIFF ( {a}, {b}, WEEK )" 130
                is (p ()) $"DATEDIFF ( {a}, {b}, DAY )" 914)
            // dax.guide, DATEDIFF: the example, and the remarks.
            testCase "boundaries, not whole intervals" (fun () ->
                let a, b = "DATE ( 2011, 01, 01 )", "DATE ( 2012, 12, 15 )"
                is (p ()) $"DATEDIFF ( {a}, {b}, YEAR )" 1
                is (p ()) $"DATEDIFF ( {a}, {b}, QUARTER )" 7
                is (p ()) $"DATEDIFF ( {a}, {b}, MONTH )" 23
                is (p ()) $"DATEDIFF ( {a}, {b}, DAY )" 714
                is (p ()) "DATEDIFF ( DATE ( 2011, 12, 31 ), DATE ( 2012, 1, 1 ), YEAR )" 1
                is (p ()) "DATEDIFF ( DATE ( 2011, 1, 1 ), DATE ( 2011, 12, 31 ), YEAR )" 0
                is (p ()) "DATEDIFF ( DATE ( 2011, 12, 31 ), DATE ( 2012, 1, 1 ), QUARTER )" 1
                is (p ()) "DATEDIFF ( DATE ( 2011, 12, 31 ), DATE ( 2012, 1, 1 ), MONTH )" 1
                // "With WEEK, a week starts on Sunday": January 1, 2005 was a Saturday.
                is (p ()) "DATEDIFF ( DATE ( 2005, 1, 1 ), DATE ( 2005, 1, 2 ), WEEK )" 1
                is (p ()) "DATEDIFF ( DATE ( 2005, 1, 2 ), DATE ( 2005, 1, 8 ), WEEK )" 0
                is (p ()) "DATEDIFF ( DATE ( 2005, 1, 2 ), DATE ( 2005, 1, 9 ), WEEK )" 1)
            testCase "times" (fun () ->
                let a, b = "dt\"2024-01-31T23:59:59\"", "dt\"2024-02-01T00:00:01\""
                is (p ()) $"DATEDIFF ( {a}, {b}, SECOND )" 2
                is (p ()) $"DATEDIFF ( {a}, {b}, MINUTE )" 1
                is (p ()) $"DATEDIFF ( {a}, {b}, HOUR )" 1
                is (p ()) $"DATEDIFF ( {a}, {b}, DAY )" 1
                is (p ()) $"DATEDIFF ( {a}, {b}, MONTH )" 1
                is (p ()) "DATEDIFF ( dt\"2024-01-31T10:00:00\", dt\"2024-01-31T10:59:59\", HOUR )" 0
                is (p ()) "DATEDIFF ( dt\"2024-01-31T10:00:00\", dt\"2024-01-31T12:30:00\", MINUTE )" 150
                is (p ()) "DATEDIFF ( dt\"2024-01-31T00:00:00\", dt\"2024-01-31T23:59:59\", DAY )" 0
                is (p ()) "DATEDIFF ( DATE ( 2024, 1, 1 ), DATE ( 2024, 1, 2 ), SECOND )" 86400)
            // "The result is negative if Date1 is larger than Date2. If Date1 or Date2 is BLANK, the
            // result is BLANK" (dax.guide).
            testCase "negative and BLANK" (fun () ->
                is (p ()) "DATEDIFF ( DATE ( 2012, 1, 1 ), DATE ( 2011, 12, 31 ), DAY )" -1
                is (p ()) "DATEDIFF ( DATE ( 2012, 1, 1 ), DATE ( 2011, 12, 31 ), YEAR )" -1
                is (p ()) "DATEDIFF ( DATE ( 2021, 12, 31 ), DATE ( 2019, 7, 1 ), WEEK )" -130
                is (p ()) "DATEDIFF ( dt\"2024-02-01T00:00:01\", dt\"2024-01-31T23:59:59\", MINUTE )" -1
                is (p ()) "DATEDIFF ( BLANK (), DATE ( 2012, 1, 1 ), DAY )" null
                is (p ()) "DATEDIFF ( DATE ( 2012, 1, 1 ), BLANK (), DAY )" null)
            testCase "the interval is a keyword" (fun () ->
                is (p ()) "DATEDIFF ( DATE ( 2011, 1, 1 ), DATE ( 2012, 1, 1 ), year )" 1
                Expect.equal (code "DATEDIFF ( DATE ( 2011, 1, 1 ), DATE ( 2012, 1, 1 ), FORTNIGHT )") "dax-invalid" ""
                Expect.equal (code "DATEDIFF ( DATE ( 2011, 1, 1 ), DATE ( 2012, 1, 1 ), \"DAY\" )") "dax-invalid" "")
            testCase "in a row context, over columns" (fun () ->
                returns
                    (p ())
                    "EVALUATE SELECTCOLUMNS ( Sales, \"Id\", Sales[Id], \"Days\", DATEDIFF ( Sales[OrderDate], Sales[ShipDate], DAY ) )"
                    [ [ 1; 1 ]; [ 2; 31 ]; [ 3; 0 ]; [ 4; 0 ]; [ 5; 0 ]; [ 6; 1 ]; [ 7; 0 ] ])
        ]

        testList "YEARFRAC" [
            // dax.guide, YEARFRAC: the examples (0.9556, 0.9667, 0.9534, 0.9556; 1.96).
            testCase "the published examples" (fun () ->
                let a, b = "DATE ( 2011, 01, 01 )", "DATE ( 2011, 12, 15 )"
                is (p ()) $"YEARFRAC ( {a}, {b} )" (344.0 / 360.0)
                is (p ()) $"YEARFRAC ( {a}, {b}, 0 )" (344.0 / 360.0)
                is (p ()) $"YEARFRAC ( {a}, {b}, 2 )" (348.0 / 360.0)
                is (p ()) $"YEARFRAC ( {a}, {b}, 3 )" (348.0 / 365.0)
                is (p ()) $"YEARFRAC ( {a}, {b}, 4 )" (344.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2010, 01, 01 ), DATE ( 2011, 12, 15 ) )" (704.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 01, 01 ), DATE ( 2012, 12, 15 ) )" (704.0 / 360.0)
                // Microsoft Learn's second example: January 1 to March 1 is two 30-day months.
                is (p ()) "YEARFRAC ( \"Jan 1 2007\", \"Mar 1 2007\" )" (60.0 / 360.0))
            // The 30/360 conventions: a 31st counts as the 30th; in the US form the end date's only
            // when the start date's is the 30th or 31st.
            testCase "the 31st of a month" (fun () ->
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 31 ), DATE ( 2011, 3, 31 ), 0 )" (60.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 30 ), DATE ( 2011, 3, 31 ), 0 )" (60.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 29 ), DATE ( 2011, 3, 31 ), 0 )" (62.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 29 ), DATE ( 2011, 3, 31 ), 4 )" (61.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 31 ), DATE ( 2011, 3, 30 ), 4 )" (60.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 31 ), DATE ( 2011, 2, 28 ), 4 )" (28.0 / 360.0)
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2011, 1, 1 ) )" 0.0)
            // "All arguments are truncated to integers" (Microsoft Learn).
            testCase "truncation" (fun () ->
                is (p ()) "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2011, 12, 15 ), 3.9 )" (348.0 / 365.0)
                is (p ()) "YEARFRAC ( dt\"2011-01-01T18:00:00\", dt\"2011-12-15T06:00:00\", 3 )" (348.0 / 365.0))
            testCase "errors and refusals" (fun () ->
                isDaxError "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2011, 12, 15 ), 5 )"
                isDaxError "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2011, 12, 15 ), -1 )"
                refused "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2011, 12, 15 ), 1 )" "actual/actual"
                refused "YEARFRAC ( DATE ( 2011, 12, 15 ), DATE ( 2011, 1, 1 ) )" "start date after the end date"
                refused "YEARFRAC ( DATE ( 2011, 2, 28 ), DATE ( 2011, 12, 15 ) )" "last day of February"
                refused "YEARFRAC ( DATE ( 2011, 1, 1 ), DATE ( 2012, 2, 29 ), 0 )" "last day of February"
                refused "YEARFRAC ( BLANK (), DATE ( 2011, 12, 15 ) )" "BLANK date"
                refused "YEARFRAC ( DATE ( 1899, 1, 1 ), DATE ( 2011, 12, 15 ), 3 )" "before March 1, 1900")
        ]

        testList "NETWORKDAYS" [
            // dax.guide, NETWORKDAYS: May 2022 (22, 26, 21); Microsoft Learn's example (0).
            testCase "the published examples" (fun () ->
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 1 )" 22
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\" )" 22
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 11 )" 26
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 1, { dt\"2022-05-30\" } )" 21

                is (p ()) "NETWORKDAYS ( DATE ( 2022, 5, 28 ), DATE ( 2022, 5, 30 ), 1, { DATE ( 2022, 5, 30 ) } )" 0)
            // Microsoft Learn's list of weekend numbers. The week of Sunday May 1 to Saturday May 7,
            // 2022 has every day of the week once.
            testCase "every weekend number" (fun () ->
                for code in 1..7 do
                    is (p ()) $"NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-07\", {code} )" 5

                for code in 11..17 do
                    is (p ()) $"NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-07\", {code} )" 6

                // Which days: 2 is Sunday and Monday, 7 Friday and Saturday, 11 Sunday, 17 Saturday.
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-02\", 2 )" 0
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-02\", 3 )" 1
                is (p ()) "NETWORKDAYS ( dt\"2022-05-06\", dt\"2022-05-07\", 7 )" 0
                is (p ()) "NETWORKDAYS ( dt\"2022-05-06\", dt\"2022-05-07\", 6 )" 1
                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-01\", 11 )" 0
                is (p ()) "NETWORKDAYS ( dt\"2022-05-07\", dt\"2022-05-07\", 17 )" 0
                is (p ()) "NETWORKDAYS ( dt\"2022-05-07\", dt\"2022-05-07\", 16 )" 1
                is (p ()) "NETWORKDAYS ( dt\"2022-05-04\", dt\"2022-05-04\" )" 1)
            testCase "holidays" (fun () ->
                // A holiday at the weekend, outside the range, or listed twice changes nothing.
                is
                    (p ())
                    "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 1, { dt\"2022-05-30\", dt\"2022-05-30\", dt\"2022-05-29\", dt\"2022-06-01\" } )"
                    21

                is (p ()) "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", , { dt\"2022-05-30\" } )" 21
                // The holidays as a table of the model: the Date table's days in January 2024.
                is
                    (p ())
                    "NETWORKDAYS ( dt\"2024-01-01\", dt\"2024-01-05\", 1, CALCULATETABLE ( VALUES ( 'Date'[Date] ), 'Date'[Month] = \"Jan\" ) )"
                    3)
            // "If start_date and end_date both are BLANK, the output value is also BLANK. If either
            // ... is BLANK, [it] will be treated as Date(1899, 12, 30)" (Microsoft Learn): a
            // Saturday, so the two days to Monday January 1, 1900 hold one workday.
            testCase "BLANK dates" (fun () ->
                is (p ()) "NETWORKDAYS ( BLANK (), BLANK () )" null
                is (p ()) "NETWORKDAYS ( BLANK (), DATE ( 1900, 1, 1 ) )" 1
                is (p ()) "NETWORKDAYS ( BLANK (), DATE ( 1899, 12, 31 ) )" 0
                is (p ()) "NETWORKDAYS ( DATE ( 1899, 12, 25 ), BLANK () )" 5)
            testCase "refusals" (fun () ->
                refused "NETWORKDAYS ( dt\"2022-05-31\", dt\"2022-05-01\" )" "start date after the end date"
                refused "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 8 )" "weekend number 8"
                refused "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 4 + 4 )" "weekend number 8"
                refused "NETWORKDAYS ( \"2022-05-01\", dt\"2022-05-31\" )" "given as text"
                refused "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31T12:00:00\" )" "time of day"
                refused "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 1, { \"2022-05-30\" } )" "not a date value"
                refused "NETWORKDAYS ( dt\"2022-05-01\", dt\"2022-05-31\", 1, { BLANK () } )" "BLANK holiday"
                refused "NETWORKDAYS ( 44682, dt\"2022-05-31\" )" "not a date value")
        ]

        testList "the clock" [
            testCase "TODAY, NOW, UTCTODAY, UTCNOW return the pinned instant" (fun () ->
                is (p ()) "TODAY ()" (d 2024 3 5)
                is (p ()) "NOW ()" (box pinned.Now)
                is (p ()) "UTCTODAY ()" (d 2024 3 5)
                is (p ()) "UTCNOW ()" (box pinned.UtcNow)
                is (p ()) "YEAR ( TODAY () )" 2024
                is (p ()) "HOUR ( NOW () )" 14
                is (p ()) "HOUR ( UTCNOW () )" 12
                is (p ()) "NOW () = NOW ()" true
                Expect.equal (Engine.clock (p ())) pinned "Engine.clock")
            testCase "the UTC date can be another day" (fun () ->
                let late =
                    prepareAt
                        {
                            Now = DateTime(2024, 1, 1, 1, 0, 0)
                            UtcNow = DateTime(2023, 12, 31, 23, 0, 0)
                        }
                        star

                is late "TODAY ()" (d 2024 1 1)
                is late "UTCTODAY ()" (d 2023 12 31)
                is late "INT ( 1 ) + DATEDIFF ( UTCTODAY (), TODAY (), DAY )" 2)
            testCase "calculated tables and columns use the same clock" (fun () ->
                let m = {
                    star with
                        Tables =
                            (star.Tables
                             |> List.map (fun t ->
                                 if t.Name = "Product" then
                                     t |> withCalculated [ "Stamp", DataType.DateTime, "NOW ()" ]
                                 else
                                     t))
                            @ [
                                calculatedTable "Today" [ "Day", DataType.DateTime ] "ROW ( \"Day\", TODAY () )"
                            ]
                }

                let prepared = prepareAt pinned m
                Expect.isEmpty (Engine.failures prepared) "nothing failed"
                returns prepared "EVALUATE Today" [ [ d 2024 3 5 ] ]
                returns prepared "EVALUATE DISTINCT ( Product[Stamp] )" [ [ box pinned.Now ] ]
                is prepared "MAX ( Product[Stamp] ) = NOW ()" true)
            testCase "without a pinned clock it is the machine's, read once" (fun () ->
                let before = DateTime.Now.AddSeconds -1.0
                let prepared = prepare star
                let clock = Engine.clock prepared
                Expect.isTrue (clock.Now >= before && clock.Now <= DateTime.Now.AddSeconds 1.0) "local now"

                Expect.isTrue (abs (clock.UtcNow - DateTime.UtcNow).TotalSeconds < 5.0) "UTC now"

                Threading.Thread.Sleep 20
                is prepared "NOW ()" (box clock.Now))
            testCase "Clock.parse" (fun () ->
                let parse (text: string) =
                    match Engine.Clock.parse text with
                    | Ok c -> Some(c.Now, c.UtcNow)
                    | Error _ -> None

                let at = DateTime(2024, 3, 5, 14, 30, 0)
                Expect.equal (parse "2024-03-05") (Some(DateTime(2024, 3, 5), DateTime(2024, 3, 5))) "a date"
                Expect.equal (parse "2024-03-05T14:30") (Some(at, at)) "no seconds"
                Expect.equal (parse "2024-03-05 14:30:00") (Some(at, at)) "a space"
                Expect.equal (parse "2024-03-05T14:30:00Z") (Some(at, at)) "Z"
                Expect.equal (parse "2024-03-05T14:30:00+02:00") (Some(at, at.AddHours -2.0)) "ahead of UTC"
                Expect.equal (parse "2024-03-05T14:30:00-0530") (Some(at, at.AddHours 5.5)) "behind UTC"

                Expect.equal
                    (parse "2024-03-05T14:30:00.250+01:00")
                    (Some(at.AddMilliseconds 250.0, at.AddMilliseconds(250.0).AddHours -1.0))
                    "milliseconds"

                Expect.equal
                    (parse "2024-01-01T01:00:00+02:00")
                    (Some(DateTime(2024, 1, 1, 1, 0, 0), DateTime(2023, 12, 31, 23, 0, 0)))
                    "the UTC date is the day before"

                for bad in
                    [
                        ""
                        "now"
                        "5/3/2024"
                        "2024-03-05T25:00"
                        "2024-02-30"
                        "2024-03-05+02:00"
                        "2024-03-05T14:30:00+25:00"
                        "2024-03-05T14:30:00 Europe/Paris"
                    ] do
                    Expect.equal (parse bad) None bad)
        ]

        testList "CALENDAR" [
            // dax.guide, CALENDAR: the example (ten days to December 5, 2022).
            testCase "a date per day, both ends included" (fun () ->
                returnsInOrder
                    (p ())
                    "EVALUATE CALENDAR ( DATE ( 2022, 11, 26 ), DATE ( 2022, 12, 5 ) ) ORDER BY [Date] DESC"
                    [
                        [ d 2022 12 5 ]
                        [ d 2022 12 4 ]
                        [ d 2022 12 3 ]
                        [ d 2022 12 2 ]
                        [ d 2022 12 1 ]
                        [ d 2022 11 30 ]
                        [ d 2022 11 29 ]
                        [ d 2022 11 28 ]
                        [ d 2022 11 27 ]
                        [ d 2022 11 26 ]
                    ]

                let r =
                    query (p ()) "EVALUATE CALENDAR ( DATE ( 2024, 1, 1 ), DATE ( 2024, 1, 1 ) )"

                Expect.equal
                    (r.Columns |> List.map (fun c -> c.Name, c.Type))
                    [ "[Date]", QueryType.DateTime ]
                    "column"

                Expect.equal r.Rows.Length 1 "the same date at both ends is one row"
                is (p ()) "COUNTROWS ( CALENDAR ( DATE ( 2015, 1, 1 ), DATE ( 2021, 12, 31 ) ) )" 2557)
            // "CALENDAR ignores the time part of StartDate and EndDate ... the time of each value is
            // always midnight" (dax.guide).
            testCase "times are ignored" (fun () ->
                returns (p ()) "EVALUATE CALENDAR ( dt\"2024-01-30T18:00:00\", dt\"2024-01-31T06:00:00\" )" [
                    [ d 2024 1 30 ]
                    [ d 2024 1 31 ]
                ]

                returns (p ()) "EVALUATE CALENDAR ( TODAY (), NOW () )" [ [ d 2024 3 5 ] ])
            // "CALENDAR ( 1, 100 )" has the values of GENERATESERIES ( 1, 100, 1 ), as dates (dax.guide).
            testCase "numbers are serials" (fun () ->
                is (p ()) "COUNTROWS ( CALENDAR ( 1, 100 ) )" 100
                is (p ()) "MINX ( CALENDAR ( 1, 100 ), [Date] ) = 1" true
                is (p ()) "MAXX ( CALENDAR ( 1, 100 ), [Date] ) = 100" true)
            testCase "errors and refusals" (fun () ->
                isDaxError "COUNTROWS ( CALENDAR ( DATE ( 2024, 1, 2 ), DATE ( 2024, 1, 1 ) ) )"
                refused "COUNTROWS ( CALENDAR ( BLANK (), DATE ( 2024, 1, 1 ) ) )" "BLANK"
                refused "COUNTROWS ( CALENDAR ( \"2024-01-01\", DATE ( 2024, 1, 1 ) ) )" "not a date or a number")
            testCase "the usual date table" (fun () ->
                returns
                    (p ())
                    "EVALUATE ADDCOLUMNS ( CALENDAR ( DATE ( 2023, 12, 31 ), DATE ( 2024, 1, 1 ) ), \"Year\", YEAR ( [Date] ), \"Month\", MONTH ( [Date] ), \"Week\", WEEKNUM ( [Date], 21 ), \"Weekday\", WEEKDAY ( [Date], 2 ) )"
                    [ [ d 2023 12 31; 2023; 12; 52; 7 ]; [ d 2024 1 1; 2024; 1; 1; 1 ] ])
        ]

        testList "CALENDARAUTO" [
            // The star model's dates run from December 31, 2023 to February 1, 2024.
            testCase "whole years around the model's dates" (fun () ->
                is (p ()) "COUNTROWS ( CALENDARAUTO () )" 731
                is (p ()) "MINX ( CALENDARAUTO (), [Date] ) = DATE ( 2023, 1, 1 )" true
                is (p ()) "MAXX ( CALENDARAUTO (), [Date] ) = DATE ( 2024, 12, 31 )" true
                is (p ()) "COUNTROWS ( CALENDARAUTO ( 12 ) )" 731
                is (p ()) "COUNTROWS ( CALENDARAUTO ( BLANK () ) )" 731)
            // Microsoft Learn, CALENDARAUTO: with a fiscal year ending in March, the range starts on
            // the April 1 on or before the first date and ends on the March 31 on or after the last.
            testCase "fiscal years" (fun () ->
                is (p ()) "MINX ( CALENDARAUTO ( 3 ), [Date] ) = DATE ( 2023, 4, 1 )" true
                is (p ()) "MAXX ( CALENDARAUTO ( 3 ), [Date] ) = DATE ( 2024, 3, 31 )" true
                is (p ()) "COUNTROWS ( CALENDARAUTO ( 3 ) )" 366
                // Ending in January: February 1, 2024 opens the fiscal year that ends January 31, 2025.
                is (p ()) "MINX ( CALENDARAUTO ( 1 ), [Date] ) = DATE ( 2023, 2, 1 )" true
                is (p ()) "MAXX ( CALENDARAUTO ( 1 ), [Date] ) = DATE ( 2025, 1, 31 )" true
                // Ending in February of a leap year.
                is (p ()) "MAXX ( CALENDARAUTO ( 2 ), [Date] ) = DATE ( 2024, 2, 29 )" true
                is (p ()) "MINX ( CALENDARAUTO ( 2 ), [Date] ) = DATE ( 2023, 3, 1 )" true
                // "2.5 becomes 3" (dax.guide).
                is (p ()) "MINX ( CALENDARAUTO ( 2.5 ), [Date] ) = DATE ( 2023, 4, 1 )" true
                isDaxError "COUNTROWS ( CALENDARAUTO ( 0 ) )"
                isDaxError "COUNTROWS ( CALENDARAUTO ( 13 ) )")
            // Microsoft Learn's own example: dates from July 1, 2018 to June 30, 2019.
            testCase "the documented example" (fun () ->
                let m = {
                    Tables = [
                        table "T" [ "D", DataType.DateTime ] [ [ d 2018 7 1 ]; [ null ]; [ d 2019 6 30 ] ]
                        calculatedTable "CalendarAuto" [ "Date", DataType.DateTime ] "CALENDARAUTO ()"
                        calculatedTable "CalendarAuto3" [ "Date", DataType.DateTime ] "CALENDARAUTO ( 3 )"
                    ]
                    Rels = []
                    Measures = []
                    Functions = []
                }

                let prepared = prepareAt pinned m
                Expect.isEmpty (Engine.failures prepared) "nothing failed"
                is prepared "MIN ( 'CalendarAuto'[Date] ) = DATE ( 2018, 1, 1 )" true
                is prepared "MAX ( 'CalendarAuto'[Date] ) = DATE ( 2019, 12, 31 )" true
                is prepared "MIN ( 'CalendarAuto3'[Date] ) = DATE ( 2018, 4, 1 )" true
                is prepared "MAX ( 'CalendarAuto3'[Date] ) = DATE ( 2020, 3, 31 )" true
                // The calculated tables' own dates (to 2020) don't widen the range.
                is prepared "COUNTROWS ( CALENDARAUTO () )" 730)
            testCase "calculated columns and calculated tables are not scanned" (fun () ->
                let m = {
                    star with
                        Tables =
                            (star.Tables
                             |> List.map (fun t ->
                                 if t.Name = "Product" then
                                     t |> withCalculated [ "Launch", DataType.DateTime, "DATE ( 2031, 5, 5 )" ]
                                 else
                                     t))
                            @ [
                                calculatedTable
                                    "Far"
                                    [ "Date", DataType.DateTime ]
                                    "CALENDAR ( DATE ( 1990, 1, 1 ), DATE ( 1990, 1, 3 ) )"
                            ]
                }

                let prepared = prepareAt pinned m
                Expect.isEmpty (Engine.failures prepared) "nothing failed"
                is prepared "COUNTROWS ( CALENDARAUTO () )" 731
                is prepared "COUNTROWS ( Far )" 3)
            testCase "a model without dates is an error" (fun () ->
                let m = {
                    Tables = [ table "T" [ "N", DataType.Int64; "D", DataType.DateTime ] [ [ 1; null ] ] ]
                    Rels = []
                    Measures = []
                    Functions = []
                }

                let prepared = prepareAt pinned m

                Expect.equal
                    (TessError.code (scalarError prepared "COUNTROWS ( CALENDARAUTO () )"))
                    "dax-error"
                    "no dates")
        ]

        testList "types" [
            testCase "what each function returns" (fun () ->
                let r =
                    query
                        (p ())
                        "EVALUATE ROW ( \"date\", DATE ( 2024, 1, 31 ), \"time\", TIME ( 1, 2, 3 ), \"datevalue\", DATEVALUE ( \"2024-01-31\" ), \"timevalue\", TIMEVALUE ( \"01:02:03\" ), \"year\", YEAR ( TODAY () ), \"quarter\", QUARTER ( BLANK () ), \"hour\", HOUR ( 0.5 ), \"weekday\", WEEKDAY ( TODAY () ), \"weeknum\", WEEKNUM ( TODAY () ), \"eomonth\", EOMONTH ( TODAY (), 0 ), \"edate\", EDATE ( TODAY (), 1 ), \"datediff\", DATEDIFF ( BLANK (), TODAY (), DAY ), \"yearfrac\", YEARFRAC ( TODAY (), TODAY () ), \"networkdays\", NETWORKDAYS ( BLANK (), BLANK () ), \"today\", TODAY (), \"now\", NOW (), \"utctoday\", UTCTODAY (), \"utcnow\", UTCNOW () )"

                let dateTime, int64, double = QueryType.DateTime, QueryType.Int64, QueryType.Double

                Expect.equal
                    (r.Columns |> List.map (fun c -> c.Name, c.Type))
                    [
                        "[date]", dateTime
                        "[time]", dateTime
                        "[datevalue]", dateTime
                        "[timevalue]", dateTime
                        "[year]", int64
                        "[quarter]", int64
                        "[hour]", int64
                        "[weekday]", int64
                        "[weeknum]", int64
                        "[eomonth]", dateTime
                        "[edate]", dateTime
                        "[datediff]", int64
                        "[yearfrac]", double
                        "[networkdays]", int64
                        "[today]", dateTime
                        "[now]", dateTime
                        "[utctoday]", dateTime
                        "[utcnow]", dateTime
                    ]
                    "declared types")
            // The docs' tables of result types: a date plus or minus a number is a date, a date
            // minus a date too; a date compares with a number as its serial.
            testCase "dates in arithmetic and comparisons" (fun () ->
                is (p ()) "DATE ( 2020, 10, 15 ) + 1" (d 2020 10 16)
                is (p ()) "1 + DATE ( 2020, 10, 15 )" (d 2020 10 16)
                is (p ()) "DATE ( 2020, 10, 15 ) + 0.5" (dt 2020 10 15 12 0 0)
                is (p ()) "DATE ( 2020, 10, 15 ) - 15" (d 2020 9 30)
                is (p ()) "NOW () + 3.5" (box (pinned.Now.AddDays 3.5))
                is (p ()) "TODAY () - DATE ( 2024, 3, 1 ) = 4" true
                is (p ()) "DATE ( 2020, 10, 15 ) = 44119" true
                is (p ()) "DATE ( 2020, 10, 15 ) > 44118.5" true
                is (p ()) "DATE ( 1899, 12, 30 ) = BLANK ()" true
                is (p ()) "DATE ( 1899, 12, 29 ) < BLANK ()" true
                is (p ()) "DATE ( 2020, 10, 15 ) < TODAY ()" true
                is (p ()) "BLANK () + DATE ( 2020, 10, 15 )" (d 2020 10 15)

                Expect.equal
                    (code "DATE ( 2020, 10, 15 ) = \"2020-10-15\"")
                    "dax-error"
                    "a date and text don't compare")
        ]

        testList "coverage" [
            testCase "argument forms the engine refuses show without data" (fun () ->
                let m = {
                    star with
                        Measures =
                            star.Measures
                            @ [
                                "Sales", "Fine", "WEEKNUM ( MAX ( Sales[OrderDate] ), 21 ) + YEAR ( \"2024-01-31\" )"
                                "Sales", "Odd Week", "WEEKNUM ( MAX ( Sales[OrderDate] ), 5 )"
                                "Sales", "Odd Text", "MONTH ( \"last Tuesday\" )"
                                "Sales", "Actual", "YEARFRAC ( DATE ( 2020, 1, 1 ), TODAY (), 1 )"
                                "Sales", "Text Days", "NETWORKDAYS ( \"2024-01-01\", TODAY () )"
                                "Sales", "Odd Weekend", "NETWORKDAYS ( DATE ( 2020, 1, 1 ), TODAY (), 9 )"
                            ]
                }

                let db, _ = build m

                let of' (name: string) =
                    Engine.coverage db
                    |> List.find (fun (r, _) -> r.Kind = ObjectKind.Measure && List.last r.Path = name)
                    |> snd

                Expect.isEmpty (of' "Fine") "Fine"

                Expect.equal
                    (of' "Odd Week")
                    [ "WEEKNUM with return type 5 (not one of the documented ones)" ]
                    "Odd Week"

                Expect.equal (of' "Odd Text").Length 1 "Odd Text"
                Expect.stringContains (of' "Odd Text").Head "MONTH of text" "Odd Text"
                Expect.equal (of' "Actual") [ "YEARFRAC on the actual/actual basis (1)" ] "Actual"
                Expect.equal (of' "Text Days") [ "NETWORKDAYS with a date given as text" ] "Text Days"

                Expect.equal
                    (of' "Odd Weekend")
                    [ "NETWORKDAYS with weekend number 9 (not one of the documented ones)" ]
                    "Odd Weekend")
            testCase "every function of the family is registered once" (fun () ->
                let names =
                    Engine.functions
                    |> List.filter (fun (_, family) -> family = "Date and time")
                    |> List.map fst

                Expect.equal
                    names
                    [
                        "CALENDAR"
                        "CALENDARAUTO"
                        "DATE"
                        "DATEDIFF"
                        "DATEVALUE"
                        "DAY"
                        "EDATE"
                        "EOMONTH"
                        "HOUR"
                        "MINUTE"
                        "MONTH"
                        "NETWORKDAYS"
                        "NOW"
                        "QUARTER"
                        "SECOND"
                        "TIME"
                        "TIMEVALUE"
                        "TODAY"
                        "UTCNOW"
                        "UTCTODAY"
                        "WEEKDAY"
                        "WEEKNUM"
                        "YEAR"
                        "YEARFRAC"
                    ]
                    "the family")
        ]

        testList "CLI --now" [
            testCase "sets what TODAY, NOW and the UTC functions return" (fun () ->
                let t, model, dir = setup star
                use _ = t

                let run (now: string) =
                    let r =
                        tess [
                            "query"
                            model
                            "--data"
                            dir
                            "--now"
                            now
                            "--json"
                            "EVALUATE ROW ( \"today\", TODAY (), \"now\", NOW (), \"utctoday\", UTCTODAY (), \"utcnow\", UTCNOW () )"
                        ]

                    Expect.equal r.Code ExitCode.ok r.Err
                    use doc = json r

                    let row = doc.RootElement.GetProperty("results").[0].GetProperty("rows").[0]

                    [ for v in row.EnumerateArray() -> v.GetString() ]

                Expect.equal
                    (run "2024-03-05T14:30:00")
                    [
                        "2024-03-05T00:00:00"
                        "2024-03-05T14:30:00"
                        "2024-03-05T00:00:00"
                        "2024-03-05T14:30:00"
                    ]
                    "no offset: UTC is the same instant"

                Expect.equal
                    (run "2024-01-01T01:00:00+02:00")
                    [
                        "2024-01-01T00:00:00"
                        "2024-01-01T01:00:00"
                        "2023-12-31T00:00:00"
                        "2023-12-31T23:00:00"
                    ]
                    "with an offset"

                Expect.equal (run "2024-03-05").Head "2024-03-05T00:00:00" "a date alone")
            testCase "is refused where it means nothing" (fun () ->
                let t, model, dir = setup star
                use _ = t

                let bad =
                    tess [ "query"; model; "--data"; dir; "--now"; "tomorrow"; "EVALUATE Sales" ]

                Expect.equal bad.Code ExitCode.usage bad.Err
                Expect.stringContains bad.Err "ISO 8601" "says what it takes"

                let noData = tess [ "query"; model; "--now"; "2024-03-05"; "EVALUATE Sales" ]
                Expect.equal noData.Code ExitCode.usage noData.Err

                let coverage = tess [ "query"; model; "--coverage"; "--now"; "2024-03-05" ]
                Expect.equal coverage.Code ExitCode.usage coverage.Err

                let remote =
                    tess [ "query"; "xmla:localhost;db"; "--now"; "2024-03-05"; "EVALUATE Sales" ]

                Expect.equal remote.Code ExitCode.usage remote.Err)
        ]
    ]
