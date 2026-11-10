namespace Tessera.DaxEngine.Functions

open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// Text read as a date, a time, or both, by the date and time functions.
///
/// The engine reads text with "the locale and date/time settings of the model" (Microsoft Learn,
/// DATEVALUE; the pages of YEAR, MONTH, HOUR... say "of the client computer") and then tries other
/// formats, so most of what it accepts is not written down anywhere. This reads only the forms a
/// source shows a result for, and everything else is refused by the caller:
///
/// - any culture: ISO 8601 `yyyy-MM-dd` (dax.guide, DATEVALUE: `DATEVALUE ( "2020-02-06" )`), alone
///   or followed by a time after a `T` or a space, and a time alone, `h:mm` or `h:mm:ss` on the
///   24-hour clock ("any input text using colons as time separators will parse correctly":
///   Microsoft Learn, HOUR; `TIMEVALUE("20:45:30")`: Microsoft Learn, TIMEVALUE);
/// - a model whose culture is en-US, the culture of every example on those pages:
///   `m/d/yyyy` and `m-d-yyyy` (dax.guide, DATEVALUE: "10/15/2020", "10-15-2020"), read day first
///   when month first is not a date ("20/4/2018 may convert to April 20th": Microsoft Learn,
///   DATEVALUE; dax.guide: "28/02/2020"); `Month d, yyyy` and `Mon d yyyy` with English month names
///   ("March 3, 2008 3:00 PM": Microsoft Learn, HOUR; "Feb 14, 2010": WEEKNUM; "Jan 1 2007":
///   YEARFRAC); and `AM` / `PM` after a time.
///
/// Not read: a date without a year (the engine takes the current year), a month and a year alone
/// ("March 2007"), two-digit years, "Mar-3-2003", times written with dots, fractions of a second.
module DateText =

    /// What a text says: a date (at midnight), a time of day, or both.
    type Reading = {
        Date: DateTime voption
        Time: TimeSpan voption
    }

    let private options = RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase

    let private time = @"(?<h>\d{1,2}):(?<mi>\d{2})(?::(?<s>\d{2}))?(?:\s*(?<ap>AM|PM))?"

    let private iso =
        Regex(@"^(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})(?:(?:T|\s+)" + time + ")?$", options)

    let private numeric =
        Regex(@"^(?<a>\d{1,2})(?<sep>[/-])(?<b>\d{1,2})\k<sep>(?<y>\d{4})(?:\s+" + time + ")?$", options)

    let private named =
        Regex(@"^(?<mon>[A-Za-z]+)\s+(?<d>\d{1,2}),?\s+(?<y>\d{4})(?:\s+" + time + ")?$", options)

    let private timeOnly = Regex("^" + time + "$", options)

    let private months =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

        [
            "January"
            "February"
            "March"
            "April"
            "May"
            "June"
            "July"
            "August"
            "September"
            "October"
            "November"
            "December"
        ]
        |> List.iteri (fun i name ->
            d.[name] <- i + 1
            d.[name.Substring(0, 3)] <- i + 1)

        d

    let private date (year: int) (month: int) (day: int) : DateTime voption =
        if
            year >= 100
            && year <= 9999
            && month >= 1
            && month <= 12
            && day >= 1
            && day <= DateTime.DaysInMonth(year, month)
        then
            ValueSome(DateTime(year, month, day))
        else
            ValueNone

    /// The time of a match, if it has one: Ok None when the text has no time, Error when the time
    /// it has is not one this reads.
    let private timeOf (enUS: bool) (m: Match) : Result<TimeSpan voption, unit> =
        if not m.Groups.["h"].Success then
            Ok ValueNone
        else
            let hour = int m.Groups.["h"].Value
            let minute = int m.Groups.["mi"].Value
            let second = if m.Groups.["s"].Success then int m.Groups.["s"].Value else 0

            let hour =
                if not m.Groups.["ap"].Success then
                    (if hour <= 23 then hour else -1)
                elif not enUS || hour < 1 || hour > 12 then
                    -1
                elif m.Groups.["ap"].Value.ToUpperInvariant() = "PM" then
                    hour % 12 + 12
                else
                    hour % 12

            if hour < 0 || minute > 59 || second > 59 then
                Error()
            else
                Ok(ValueSome(TimeSpan(hour, minute, second)))

    /// Read a text under a model culture ("en-US", "fr-FR", ""). None for anything this doesn't
    /// read, which is not to say the engine wouldn't.
    let read (culture: string) (text: string) : Reading option =
        let enUS = String.Equals(culture, "en-US", StringComparison.OrdinalIgnoreCase)
        let t = (if isNull text then "" else text.Trim())
        let number (m: Match) (group: string) = int m.Groups.[group].Value

        let withTime (m: Match) (d: DateTime voption) =
            match d, timeOf enUS m with
            | ValueSome _, Ok time -> Some { Date = d; Time = time }
            | _ -> None

        let isoMatch = iso.Match t

        if isoMatch.Success then
            withTime isoMatch (date (number isoMatch "y") (number isoMatch "m") (number isoMatch "d"))
        else
            let timeMatch = timeOnly.Match t

            if timeMatch.Success then
                match timeOf enUS timeMatch with
                | Ok(ValueSome time) ->
                    Some {
                        Date = ValueNone
                        Time = ValueSome time
                    }
                | _ -> None
            elif not enUS then
                None
            else
                let numericMatch = numeric.Match t

                if numericMatch.Success then
                    let a = number numericMatch "a"
                    let b = number numericMatch "b"
                    let year = number numericMatch "y"

                    // Month first; day first only when month first is no date.
                    match date year a b with
                    | ValueSome d -> withTime numericMatch (ValueSome d)
                    | ValueNone -> withTime numericMatch (date year b a)
                else
                    let namedMatch = named.Match t

                    if namedMatch.Success then
                        match months.TryGetValue namedMatch.Groups.["mon"].Value with
                        | true, month ->
                            withTime namedMatch (date (number namedMatch "y") month (number namedMatch "d"))
                        | _ -> None
                    else
                        None

/// The date and time functions: DATE, TIME, DATEVALUE, TIMEVALUE, YEAR, QUARTER, MONTH, DAY, HOUR,
/// MINUTE, SECOND, WEEKDAY, WEEKNUM, EOMONTH, EDATE, DATEDIFF, YEARFRAC, NETWORKDAYS, TODAY, NOW,
/// UTCTODAY, UTCNOW, CALENDAR, CALENDARAUTO. (Time intelligence is another family.)
///
/// A date is a number of days since December 30, 1899, the time of day its fraction ("12:00 PM is
/// equivalent to 0.5": Microsoft Learn, TIME; `TIME ( 22, 45, 30 )` is `1899-12-30 22:45:30`:
/// dax.guide, TIME). The calendar is the real one on both sides of 1900: DAX is not Excel, whose
/// serial numbers have a February 29, 1900 that never was (`DATE ( -50, 1, 2 )` is January 2,
/// 1850: dax.guide, DATE). The one place the sources disagree is a plain NUMBER below 61 read as a
/// date, which Microsoft Learn's SECOND page dates as Excel does (25.56 is "January 25, 1900"), so
/// the calendar parts of such a number are refused.
///
/// Everything here works on whole milliseconds since the zero date, never on the float itself.
module DateAndTime =

    let private family = "Date and time"

    let private epoch = DateTime(1899, 12, 30)
    let private msPerDay = 86400000L

    let private floorDiv (a: int64) (b: int64) : int64 =
        let q = a / b
        if a % b <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q

    /// The day number of a date: days since the zero date.
    let private dayOfDate (d: DateTime) : int64 = int64 (d.Date - epoch).Days

    let private minDay = dayOfDate DateTime.MinValue
    let private maxDay = dayOfDate (DateTime(9999, 12, 31))
    /// March 1, 1900: where Microsoft Learn says valid dates begin (DATE, EOMONTH).
    let private march1900 = 61L
    let private year100 = dayOfDate (DateTime(100, 1, 1))

    let private outOfRange (ctx: Ctx) (span: Span) : 'a =
        Fail.runtime ctx span "A date is outside the range of dates (0001-01-01 to 9999-12-31)."

    /// A serial number as whole milliseconds since the zero date.
    let private moment (ctx: Ctx) (span: Span) (serial: float) : int64 =
        if Double.IsNaN serial || Double.IsInfinity serial then
            outOfRange ctx span

        let ms = Math.Round(serial * 86400000.0, MidpointRounding.AwayFromZero)

        if ms < float (minDay * msPerDay) || ms >= float ((maxDay + 1L) * msPerDay) then
            outOfRange ctx span

        int64 ms

    let private dayOf (ms: int64) : int64 = floorDiv ms msPerDay
    let private dateOfDay (day: int64) : DateTime = epoch.AddDays(float day)
    let private dateValue (day: int64) : DaxValue = DaxValue.DateTime(float day)

    /// Sunday is 0, Saturday 6. Day 0 (December 30, 1899) was a Saturday.
    let private dayOfWeek (day: int64) : int =
        int (((day + 6L) % 7L + 7L) % 7L)

    // ---- Arguments ----------------------------------------------------------------------------------

    /// What a date argument was before it was a date.
    [<RequireQualifiedAccess>]
    type private Origin =
        | Date
        | Number
        | Text of hasDate: bool * hasTime: bool

    type private DateArg =
        /// BLANK.
        | NoDate
        /// Milliseconds since the zero date.
        | At of ms: int64 * origin: Origin

    let private unreadable (name: string) =
        $"{name} of text that is not a date the local engine reads (it reads ISO 8601 text and, for a model whose culture is en-US, m/d/yyyy and \"Month d, yyyy\", with a time as h:mm or h:mm:ss)"

    let private readText (ctx: Ctx) (span: Span) (name: string) (text: string) : DateArg =
        match DateText.read ctx.Model.Culture text with
        | Some r ->
            let day =
                match r.Date with
                | ValueSome d -> dayOfDate d
                | ValueNone -> 0L

            let ms =
                match r.Time with
                | ValueSome t -> t.Ticks / TimeSpan.TicksPerMillisecond
                | ValueNone -> 0L

            At(day * msPerDay + ms, Origin.Text(r.Date.IsSome, r.Time.IsSome))
        | None -> Fail.notSupported ctx span (unreadable name)

    /// A date argument: a date, a number (the serial), or text.
    let private dateArg (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : DateArg =
        match ev.Scalar(ctx, e) with
        | DaxValue.Blank -> NoDate
        | DaxValue.DateTime s -> At(moment ctx e.Span s, Origin.Date)
        | DaxValue.Integer i -> At(moment ctx e.Span (float i), Origin.Number)
        | DaxValue.Decimal d -> At(moment ctx e.Span d, Origin.Number)
        | DaxValue.Currency c -> At(moment ctx e.Span (float c / 10000.0), Origin.Number)
        | DaxValue.String s -> readText ctx e.Span name s
        | DaxValue.Boolean _ -> Fail.notSupported ctx e.Span $"{name} of TRUE or FALSE"

    /// The calendar day of a date argument, for what needs its year, month, day or weekday.
    let private calendarDate (ctx: Ctx) (span: Span) (name: string) (ms: int64) (origin: Origin) : DateTime =
        match origin with
        | Origin.Number when ms < march1900 * msPerDay ->
            Fail.notSupported
                ctx
                span
                $"{name} of a number below 61 read as a date (Microsoft Learn dates such a number as Excel does, a day later than counting from December 30, 1899 gives)"
        | Origin.Text(false, _) -> Fail.notSupported ctx span $"{name} of text that has a time and no date"
        | _ -> ()

        let day = dayOf ms

        if ms - day * msPerDay > 86399000L then
            Fail.notSupported
                ctx
                span
                $"{name} of a time in the last second of a day (whether it rounds into the next day is not documented)"

        dateOfDay day

    /// The second of the day of a date argument, for HOUR, MINUTE and SECOND.
    let private secondOfDay (ctx: Ctx) (span: Span) (name: string) (ms: int64) (origin: Origin) : int =
        match origin with
        | Origin.Number when ms < 0L -> Fail.notSupported ctx span $"{name} of a negative number read as a time"
        | _ -> ()

        let inDay = ms - dayOf ms * msPerDay

        if inDay % 1000L <> 0L then
            Fail.notSupported
                ctx
                span
                $"{name} of a time with a fraction of a second (whether it is rounded or cut off is not documented)"

        int (inDay / 1000L)

    [<RequireQualifiedAccess>]
    type private Num =
        | Blank
        | Whole of int64
        | Real of float

    /// A numeric argument. `what` names it in a refusal: "a DATE year".
    let private numArg (ev: IEval) (ctx: Ctx) (what: string) (e: Expr) : Num =
        let real (d: float) =
            if Double.IsNaN d || Double.IsInfinity d || abs d >= 1e15 then
                Fail.notSupported ctx e.Span $"{what} that is not a finite number below 1e15"
            elif d = Math.Floor d then
                Num.Whole(int64 d)
            else
                Num.Real d

        match ev.Scalar(ctx, e) with
        | DaxValue.Blank -> Num.Blank
        | DaxValue.Integer i -> Num.Whole i
        | DaxValue.Decimal d -> real d
        | DaxValue.Currency c ->
            if c % 10000L = 0L then
                Num.Whole(c / 10000L)
            else
                real (float c / 10000.0)
        | _ -> Fail.notSupported ctx e.Span $"{what} that is not a number"

    /// Rounded to the nearest whole number; a half rounds up ("DATE ( 2008, 1, 2.5 ) returns
    /// January 3": dax.guide, DATE; EOMONTH's 1.5 "is rounded to 2": Microsoft Learn). No page says
    /// which way a negative half goes.
    let private nearest (ctx: Ctx) (span: Span) (what: string) (n: Num) : int64 voption =
        match n with
        | Num.Blank -> ValueNone
        | Num.Whole i -> ValueSome i
        | Num.Real r ->
            let floor = Math.Floor r

            if r - floor = 0.5 && r < 0.0 then
                Fail.notSupported
                    ctx
                    span
                    $"{what} that is a negative number exactly halfway between two whole numbers (which way it rounds is not documented)"

            ValueSome(int64 (if r - floor >= 0.5 then floor + 1.0 else floor))

    let private literalWhole (e: Expr) : int64 option =
        match (Fn.unparen e).Kind with
        | IntegerLiteral(value, _) -> Some value
        | _ -> None

    /// A check of the arguments that are text literals: can this file read them at all?
    let private textCheck (name: string) (positions: int list) (args: Expr list) : string option =
        positions
        |> List.tryPick (fun i ->
            match List.tryItem i args |> Option.map (fun e -> (Fn.unparen e).Kind) with
            | Some(StringLiteral text) when (DateText.read "en-US" text).IsNone -> Some(unreadable name)
            | _ -> None)

    let private firstOf (checks: (Expr list -> string option) list) (args: Expr list) : string option =
        checks |> List.tryPick (fun check -> check args)

    // ---- Parts of a date ----------------------------------------------------------------------------

    /// YEAR, MONTH, DAY, QUARTER.
    let private datePart (name: string) (blankIsBlank: bool) (pick: DateTime -> int) : DaxFunction =
        Fn.scalar name family (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let e = call.Args.[0]

            match dateArg ev ctx name e with
            | NoDate ->
                if blankIsBlank then
                    DaxValue.Blank
                else
                    Fail.notSupported ctx e.Span $"{name} of BLANK"
            | At(ms, origin) ->
                let d = calendarDate ctx e.Span name ms origin

                if d.Year < 100 then
                    // "The result is a date before the year 100. Other functions, for example YEAR
                    // and FORMAT, return an error with that date" (dax.guide, DATE).
                    if name = "YEAR" then
                        Fail.runtime ctx e.Span "YEAR: the date is before the year 100."
                    else
                        Fail.notSupported ctx e.Span $"{name} of a date before the year 100"

                DaxValue.Integer(int64 (pick d)))
        |> Fn.withCheck (textCheck name [ 0 ])

    /// HOUR, MINUTE, SECOND.
    let private timePart (name: string) (pick: int -> int) : DaxFunction =
        Fn.scalar name family (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let e = call.Args.[0]

            match dateArg ev ctx name e with
            | NoDate -> Fail.notSupported ctx e.Span $"{name} of BLANK"
            | At(ms, origin) -> DaxValue.Integer(int64 (pick (secondOfDay ctx e.Span name ms origin))))
        |> Fn.withCheck (textCheck name [ 0 ])

    /// The day a week begins on (Sunday 0) for a return type of WEEKDAY (other than 3) or of
    /// WEEKNUM's system 1.
    let private weekStart (returnType: int64) : int voption =
        match returnType with
        | 1L
        | 17L -> ValueSome 0
        | 2L
        | 11L -> ValueSome 1
        | 12L -> ValueSome 2
        | 13L -> ValueSome 3
        | 14L -> ValueSome 4
        | 15L -> ValueSome 5
        | 16L -> ValueSome 6
        | _ -> ValueNone

    let private weekdayTypes = set ([ 1L; 2L; 3L ] @ [ 11L..17L ])
    let private weeknumTypes = set ([ 1L; 2L; 21L ] @ [ 11L..17L ])

    let private returnTypeRefusal (name: string) (returnType: int64) =
        $"{name} with return type {returnType} (not one of the documented ones)"

    let private returnTypeCheck (name: string) (valid: Set<int64>) (args: Expr list) : string option =
        match List.tryItem 1 args |> Option.bind literalWhole with
        | Some t when not (valid.Contains t) -> Some(returnTypeRefusal name t)
        | _ -> None

    /// The optional return type of WEEKDAY and WEEKNUM: 1 when left out.
    let private returnType (ev: IEval) (ctx: Ctx) (name: string) (valid: Set<int64>) (args: Expr list) : int64 =
        match List.tryItem 1 args with
        | None -> 1L
        | Some e when Fn.isOmitted e -> 1L
        | Some e ->
            match numArg ev ctx $"a {name} return type" e with
            | Num.Whole t when valid.Contains t -> t
            | Num.Whole t -> Fail.notSupported ctx e.Span (returnTypeRefusal name t)
            | Num.Blank -> Fail.notSupported ctx e.Span $"{name} with a BLANK return type"
            | Num.Real _ -> Fail.notSupported ctx e.Span $"{name} with a return type that is not a whole number"

    /// The date of the one date argument of WEEKDAY and WEEKNUM.
    let private weekDate (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : DateTime =
        match dateArg ev ctx name e with
        | NoDate -> Fail.notSupported ctx e.Span $"{name} of BLANK"
        | At(ms, origin) ->
            let d = calendarDate ctx e.Span name ms origin

            if d.Year < 100 then
                Fail.notSupported ctx e.Span $"{name} of a date before the year 100"

            d

    // ---- Months -------------------------------------------------------------------------------------

    /// The start date of EOMONTH and EDATE: a whole day.
    let private startDay (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : DateTime =
        match dateArg ev ctx name e with
        | NoDate -> Fail.notSupported ctx e.Span $"{name} of a BLANK date"
        | At(ms, origin) ->
            let d = calendarDate ctx e.Span name ms origin

            if ms <> dayOf ms * msPerDay then
                Fail.notSupported
                    ctx
                    e.Span
                    $"{name} of a date with a time of day (whether the result keeps the time is not documented)"

            d

    /// The year and month `months` after a date's.
    let private addMonths (ctx: Ctx) (span: Span) (d: DateTime) (months: int64) : int * int =
        if abs months > 120000L then
            outOfRange ctx span

        let index = int64 d.Year * 12L + int64 (d.Month - 1) + months
        let year = floorDiv index 12L

        if year < 1L || year > 9999L then
            outOfRange ctx span

        int year, int (index - year * 12L) + 1

    // ---- YEARFRAC -----------------------------------------------------------------------------------

    let private isEndOfFebruary (d: DateTime) =
        d.Month = 2 && d.Day = DateTime.DaysInMonth(d.Year, 2)

    /// Days between two dates on a 30/360 basis, the day numbers already adjusted.
    let private days360 (a: DateTime) (dayA: int) (b: DateTime) (dayB: int) : int =
        (b.Year - a.Year) * 360 + (b.Month - a.Month) * 30 + (dayB - dayA)

    // ---- NETWORKDAYS --------------------------------------------------------------------------------

    /// The days of the week (Sunday 0) a weekend number makes the weekend.
    let private weekendDays (code: int64) : int list voption =
        if code >= 1L && code <= 7L then
            // 1: Saturday, Sunday; 2: Sunday, Monday; ... 7: Friday, Saturday.
            ValueSome [ int ((code + 5L) % 7L); int ((code + 6L) % 7L) ]
        elif code >= 11L && code <= 17L then
            // 11: Sunday only ... 17: Saturday only.
            ValueSome [ int (code - 11L) ]
        else
            ValueNone

    let private weekendRefusal (code: int64) =
        $"NETWORKDAYS with weekend number {code} (not one of the documented ones)"

    /// A date of NETWORKDAYS: "Do not specify dates as text" (Microsoft Learn).
    let private workDate (ev: IEval) (ctx: Ctx) (e: Expr) : int64 voption =
        match ev.Scalar(ctx, e) with
        | DaxValue.Blank -> ValueNone
        | DaxValue.DateTime s ->
            let ms = moment ctx e.Span s

            if ms <> dayOf ms * msPerDay then
                Fail.notSupported
                    ctx
                    e.Span
                    "NETWORKDAYS of a date with a time of day (how a part of a day counts is not documented)"

            ValueSome(dayOf ms)
        | DaxValue.String _ -> Fail.notSupported ctx e.Span "NETWORKDAYS with a date given as text"
        | _ -> Fail.notSupported ctx e.Span "NETWORKDAYS with a date that is not a date value"

    // ---- CALENDAR -----------------------------------------------------------------------------------

    /// "A table with a single column named "Date"" (Microsoft Learn, CALENDAR), a date per day.
    let private calendarTable (first: int64) (last: int64) : TableVal =
        let n = int (last - first + 1L)

        Tables.ofColumns
            [| Tables.extension "Date" DaxType.DateTime |]
            [| Array.init n (fun i -> dateValue (first + int64 i)) |]
            n

    /// "CALENDAR ignores the time part of StartDate and EndDate" (dax.guide). A number is its
    /// serial: `CALENDAR ( 1, 100 )` has the values of `GENERATESERIES ( 1, 100, 1 )`, as dates
    /// (dax.guide).
    let private calendarBound (ev: IEval) (ctx: Ctx) (which: string) (e: Expr) : int64 =
        match ev.Scalar(ctx, e) with
        | DaxValue.DateTime s -> dayOf (moment ctx e.Span s)
        | DaxValue.Integer i -> dayOf (moment ctx e.Span (float i))
        | DaxValue.Decimal d -> dayOf (moment ctx e.Span d)
        | DaxValue.Currency c -> dayOf (moment ctx e.Span (float c / 10000.0))
        | DaxValue.Blank -> Fail.notSupported ctx e.Span $"CALENDAR with a BLANK {which} date"
        | _ -> Fail.notSupported ctx e.Span $"CALENDAR with {which} date that is not a date or a number"

    let functions: DaxFunction list = [
        // ---- Making dates ---------------------------------------------------------------------------

        // The rules are dax.guide's (DATE), which gives a worked result for each: a year 0-49 has
        // 2000 added and 50-99 has 1900; 100-9999 is the year; 10000 or more is an error; months
        // past 12 and at or below 0 move the year; days past the month's end and below 0 move the
        // month; day 0 is an error; decimals round; "A BLANK argument is the same as 0". Microsoft
        // Learn agrees where it says anything, except on a negative year ("the function returns a
        // #VALUE! error"; dax.guide: 1900 is added), which is refused.
        Fn.scalar "DATE" family (3, 3) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let part (i: int) (what: string) =
                let e = call.Args.[i]

                nearest ctx e.Span what (numArg ev ctx what e)
                |> ValueOption.defaultValue 0L

            let year = part 0 "a DATE year"
            let month = part 1 "a DATE month"
            let day = part 2 "a DATE day"

            let year =
                if year < 0L then
                    Fail.notSupported
                        ctx
                        call.Args.[0].Span
                        "DATE with a negative year (Microsoft Learn says it is an error, dax.guide that 1900 is added)"
                elif year <= 49L then
                    year + 2000L
                elif year <= 99L then
                    year + 1900L
                elif year <= 9999L then
                    year
                else
                    Fail.runtime ctx call.Args.[0].Span "DATE: the year is greater than 9999."

            if day = 0L then
                Fail.runtime ctx call.Args.[2].Span "DATE: the day is 0."

            if abs month > 240000L || abs day > 8000000L then
                outOfRange ctx call.Span

            let index = year * 12L + (month - 1L)
            let y = floorDiv index 12L

            if y < 1L || y > 9999L then
                outOfRange ctx call.Span

            // Day 1 is the first of the month, 2 the day after; -15 is "the magnitude of Day plus
            // one day" back from it ("DATE ( 2008, 1, -15 ) returns December 16, 2007").
            let result =
                dayOfDate (DateTime(int y, int (index - y * 12L) + 1, 1)) + day - 1L

            if result > maxDay then
                outOfRange ctx call.Span

            if result < year100 then
                Fail.notSupported ctx call.Span "DATE that gives a date before the year 100"

            dateValue result)

        // "A number from 0 to 32767"; "Any value greater than 23 will be divided by 24 and the
        // remainder will be treated as the hour value"; minutes and seconds carry, and the result
        // never leaves the day: "TIME(0,2190,0) = TIME(0,750,0) = TIME(12,30,0)" (Microsoft Learn,
        // TIME). "The maximum number accepted in each argument is 32,767" (dax.guide).
        Fn.scalar "TIME" family (3, 3) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let part (i: int) (what: string) =
                let e = call.Args.[i]

                match numArg ev ctx $"a TIME {what}" e with
                | Num.Whole n when n >= 0L && n <= 32767L -> n
                | Num.Whole n when n > 32767L -> Fail.runtime ctx e.Span $"TIME: the {what} is greater than 32767."
                | Num.Whole _ -> Fail.notSupported ctx e.Span $"TIME with a negative {what}"
                | Num.Blank -> Fail.notSupported ctx e.Span $"TIME with a BLANK {what}"
                | Num.Real _ -> Fail.notSupported ctx e.Span $"TIME with a {what} that is not a whole number"

            let seconds =
                (part 0 "hour" * 3600L + part 1 "minute" * 60L + part 2 "second") % 86400L

            DaxValue.DateTime(float seconds / 86400.0))

        // "Time information in the date_text argument is ignored" (Microsoft Learn, DATEVALUE).
        Fn.scalar "DATEVALUE" family (1, 1) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let e = call.Args.[0]

            match ev.Scalar(ctx, e) with
            | DaxValue.String text ->
                match readText ctx e.Span "DATEVALUE" text with
                | At(ms, Origin.Text(true, _)) -> dateValue (dayOf ms)
                | _ -> Fail.notSupported ctx e.Span "DATEVALUE of text that has a time and no date"
            | DaxValue.Blank -> Fail.notSupported ctx e.Span "DATEVALUE of BLANK"
            | _ -> Fail.notSupported ctx e.Span "DATEVALUE of a value that is not text")
        |> Fn.withCheck (textCheck "DATEVALUE" [ 0 ])

        // "Any date information included in the time_text argument is ignored"; `TIMEVALUE (
        // "12:45:01" )` is `1899-12-30 12:45:01` (Microsoft Learn; dax.guide).
        Fn.scalar "TIMEVALUE" family (1, 1) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let e = call.Args.[0]

            match ev.Scalar(ctx, e) with
            | DaxValue.String text ->
                match readText ctx e.Span "TIMEVALUE" text with
                | At(ms, Origin.Text(_, true)) -> DaxValue.DateTime(float (ms - dayOf ms * msPerDay) / 86400000.0)
                | _ -> Fail.notSupported ctx e.Span "TIMEVALUE of text that has a date and no time"
            | DaxValue.Blank -> Fail.notSupported ctx e.Span "TIMEVALUE of BLANK"
            | _ -> Fail.notSupported ctx e.Span "TIMEVALUE of a value that is not text")
        |> Fn.withCheck (textCheck "TIMEVALUE" [ 0 ])

        // ---- Parts ----------------------------------------------------------------------------------

        // The pages say what these give for a date, a number (its serial: Microsoft Learn, SECOND)
        // and text. Only QUARTER's says what BLANK gives ("If the input value is BLANK, the output
        // value is also BLANK"), so the others refuse it.
        datePart "YEAR" false (fun d -> d.Year)
        datePart "MONTH" false (fun d -> d.Month)
        datePart "DAY" false (fun d -> d.Day)
        // "QUOTIENT ( MONTH ( <Date> ) + 2, 3 ) returns the same value" (dax.guide, QUARTER).
        datePart "QUARTER" true (fun d -> (d.Month + 2) / 3)
        timePart "HOUR" (fun s -> s / 3600)
        timePart "MINUTE" (fun s -> s % 3600 / 60)
        timePart "SECOND" (fun s -> s % 60)

        // Return types 1, 2 and 3 are Microsoft Learn's; 11 to 17 dax.guide's ("Numbers 1 (Monday)
        // through 7 (Sunday)" ... "1 (Sunday) through 7 (Saturday)"), with its table of results.
        Fn.scalar "WEEKDAY" family (1, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let d = weekDate ev ctx "WEEKDAY" call.Args.[0]
            let returnType = returnType ev ctx "WEEKDAY" weekdayTypes call.Args
            let weekday = dayOfWeek (dayOfDate d)

            match returnType, weekStart returnType with
            // "Week begins on Monday (0) and ends on Sunday (6)".
            | 3L, _ -> DaxValue.Integer(int64 ((weekday + 6) % 7))
            | _, ValueSome start -> DaxValue.Integer(int64 ((weekday - start + 7) % 7 + 1))
            | _ -> Fail.notSupported ctx call.Span (returnTypeRefusal "WEEKDAY" returnType))
        |> Fn.withCheck (firstOf [ textCheck "WEEKDAY" [ 0 ]; returnTypeCheck "WEEKDAY" weekdayTypes ])

        // "System 1 - The week containing January 1 is the first week of the year"; "System 2 - The
        // week containing the first Thursday of the year is the first week of the year ... the
        // methodology specified in ISO 8601"; the table of return types (Microsoft Learn, WEEKNUM).
        Fn.scalar "WEEKNUM" family (1, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let d = weekDate ev ctx "WEEKNUM" call.Args.[0]
            let returnType = returnType ev ctx "WEEKNUM" weeknumTypes call.Args

            match returnType, weekStart returnType with
            | 21L, _ -> DaxValue.Integer(int64 (ISOWeek.GetWeekOfYear d))
            | _, ValueSome start ->
                // The days of week 1 that fall in the year before.
                let before = (dayOfWeek (dayOfDate (DateTime(d.Year, 1, 1))) - start + 7) % 7
                DaxValue.Integer(int64 ((d.DayOfYear - 1 + before) / 7 + 1))
            | _ -> Fail.notSupported ctx call.Span (returnTypeRefusal "WEEKNUM" returnType))
        |> Fn.withCheck (firstOf [ textCheck "WEEKNUM" [ 0 ]; returnTypeCheck "WEEKNUM" weeknumTypes ])

        // ---- Months ---------------------------------------------------------------------------------

        // "If you enter a number that is not an integer, the number is rounded up or down to the
        // nearest integer"; "If start_date plus months yields an invalid date, EOMONTH returns an
        // error. Dates before March 1st of 1900 and after December 31st of 9999 are invalid"
        // (Microsoft Learn, EOMONTH).
        Fn.scalar "EOMONTH" family (2, 2) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let invalid () =
                Fail.runtime ctx call.Span "EOMONTH: dates before March 1, 1900 are not valid."

            let d = startDay ev ctx "EOMONTH" call.Args.[0]

            if dayOfDate d < march1900 then
                invalid ()

            let months =
                match
                    nearest
                        ctx
                        call.Args.[1].Span
                        "an EOMONTH number of months"
                        (numArg ev ctx "an EOMONTH number of months" call.Args.[1])
                with
                | ValueSome m -> m
                | ValueNone -> Fail.notSupported ctx call.Args.[1].Span "EOMONTH with a BLANK number of months"

            let year, month = addMonths ctx call.Span d months

            let result =
                dayOfDate (DateTime(year, month, DateTime.DaysInMonth(year, month)))

            if result < march1900 then
                invalid ()

            dateValue result)
        |> Fn.withCheck (textCheck "EOMONTH" [ 0 ])

        // "If months is not an integer, it is truncated"; "If the requested date is past the last
        // day of the corresponding month, then the last day of the month is returned" (Microsoft
        // Learn, EDATE). The page gives no first valid date as EOMONTH's does, so dates before
        // March 1900 are refused.
        Fn.scalar "EDATE" family (2, 2) (Fn.typeIs DaxType.DateTime) (fun ev ctx call ->
            let early () =
                Fail.notSupported ctx call.Span "EDATE of or to a date before March 1, 1900"

            let d = startDay ev ctx "EDATE" call.Args.[0]

            if dayOfDate d < march1900 then
                early ()

            let months =
                match numArg ev ctx "an EDATE number of months" call.Args.[1] with
                | Num.Whole m -> m
                | Num.Real r -> int64 (Math.Truncate r)
                | Num.Blank -> Fail.notSupported ctx call.Args.[1].Span "EDATE with a BLANK number of months"

            let year, month = addMonths ctx call.Span d months

            let result =
                dayOfDate (DateTime(year, month, min d.Day (DateTime.DaysInMonth(year, month))))

            if result < march1900 then
                early ()

            dateValue result)
        |> Fn.withCheck (textCheck "EDATE" [ 0 ])

        // ---- Differences ----------------------------------------------------------------------------

        // "DATEDIFF counts the interval boundaries between Date1 and Date2. It does not count
        // complete intervals"; "With WEEK, a week starts on Sunday"; "The result is negative if
        // Date1 is larger than Date2. If Date1 or Date2 is BLANK, the result is BLANK"; an error
        // for a later first date was the engine before 2018 (dax.guide, DATEDIFF).
        Fn.scalar "DATEDIFF" family (3, 3) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let interval =
                match Fn.keyword call.Args.[2] with
                | Some("SECOND" | "MINUTE" | "HOUR" | "DAY" | "WEEK" | "MONTH" | "QUARTER" | "YEAR" as k) -> k
                | _ ->
                    Fail.invalid
                        ctx
                        call.Args.[2].Span
                        "the interval of DATEDIFF is SECOND, MINUTE, HOUR, DAY, WEEK, MONTH, QUARTER or YEAR"

            let first = dateArg ev ctx "DATEDIFF" call.Args.[0]
            let second = dateArg ev ctx "DATEDIFF" call.Args.[1]

            match first, second with
            | NoDate, _
            | _, NoDate -> DaxValue.Blank
            | At(a, originA), At(b, originB) ->
                let boundaries (unit: int64) = floorDiv b unit - floorDiv a unit

                let calendar (index: DateTime -> int64) =
                    index (calendarDate ctx call.Args.[1].Span "DATEDIFF" b originB)
                    - index (calendarDate ctx call.Args.[0].Span "DATEDIFF" a originA)

                let result =
                    match interval with
                    | "SECOND" ->
                        if a % 1000L <> 0L || b % 1000L <> 0L then
                            Fail.notSupported
                                ctx
                                call.Span
                                "DATEDIFF in seconds of a time with a fraction of a second (whether it is rounded or cut off is not documented)"

                        boundaries 1000L
                    | "MINUTE" -> boundaries 60000L
                    | "HOUR" -> boundaries 3600000L
                    | "DAY" -> boundaries msPerDay
                    // Day 1 (December 31, 1899) was a Sunday.
                    | "WEEK" -> calendar (fun d -> floorDiv (dayOfDate d - 1L) 7L)
                    | "MONTH" -> calendar (fun d -> int64 d.Year * 12L + int64 d.Month)
                    | "QUARTER" -> calendar (fun d -> int64 d.Year * 4L + int64 ((d.Month + 2) / 3))
                    | _ -> calendar (fun d -> int64 d.Year)

                DaxValue.Integer result)
        |> Fn.withCheck (textCheck "DATEDIFF" [ 0; 1 ])

        // "The fraction of the year represented by the number of whole days between two dates";
        // basis 0 US (NASD) 30/360 (the default), 1 actual/actual, 2 actual/360, 3 actual/365, 4
        // European 30/360; "All arguments are truncated to integers"; "If basis < 0 or if basis >
        // 4, YEARFRAC returns an error" (Microsoft Learn, YEARFRAC). The pages name the bases and
        // don't define them. 30/360 is the public convention: a day 31 counts as 30 (in the US
        // form the end date's only when the start date's is 30 or 31); the US form's rules for the
        // last day of February differ between its descriptions, so those dates are refused, and so
        // is actual/actual, which has no one definition.
        Fn.scalar "YEARFRAC" family (2, 3) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let basis =
                match List.tryItem 2 call.Args with
                | None -> 0L
                | Some e when Fn.isOmitted e -> 0L
                | Some e ->
                    match numArg ev ctx "a YEARFRAC basis" e with
                    | Num.Whole b -> b
                    | Num.Real r -> int64 (Math.Truncate r)
                    | Num.Blank -> Fail.notSupported ctx e.Span "YEARFRAC with a BLANK basis"

            if basis < 0L || basis > 4L then
                Fail.runtime ctx call.Span "YEARFRAC: the basis is not between 0 and 4."

            if basis = 1L then
                Fail.notSupported
                    ctx
                    call.Span
                    "YEARFRAC on the actual/actual basis (1): how it counts the days of a year is not documented"

            let date (e: Expr) =
                match dateArg ev ctx "YEARFRAC" e with
                | NoDate -> Fail.notSupported ctx e.Span "YEARFRAC of a BLANK date"
                | At(ms, origin) ->
                    let d = calendarDate ctx e.Span "YEARFRAC" ms origin

                    if dayOfDate d < march1900 then
                        Fail.notSupported ctx e.Span "YEARFRAC of a date before March 1, 1900"

                    d

            let a = date call.Args.[0]
            let b = date call.Args.[1]

            if a > b then
                Fail.notSupported
                    ctx
                    call.Span
                    "YEARFRAC with a start date after the end date (what it returns is not documented)"

            let actual = float (dayOfDate b - dayOfDate a)

            match basis with
            | 0L ->
                if isEndOfFebruary a || isEndOfFebruary b then
                    Fail.notSupported
                        ctx
                        call.Span
                        "YEARFRAC on the US 30/360 basis with a date on the last day of February (the convention's rule for it is not documented)"

                let dayA = min a.Day 30
                let dayB = if b.Day = 31 && dayA = 30 then 30 else b.Day
                DaxValue.Decimal(float (days360 a dayA b dayB) / 360.0)
            | 2L -> DaxValue.Decimal(actual / 360.0)
            | 3L -> DaxValue.Decimal(actual / 365.0)
            | _ -> DaxValue.Decimal(float (days360 a (min a.Day 30) b (min b.Day 30)) / 360.0))
        |> Fn.withCheck (
            firstOf [
                textCheck "YEARFRAC" [ 0; 1 ]
                fun args ->
                    match List.tryItem 2 args |> Option.bind literalWhole with
                    | Some 1L -> Some "YEARFRAC on the actual/actual basis (1)"
                    | _ -> None
            ]
        )

        // "The number of whole workdays between two dates (inclusive)"; the weekend numbers 1-7 and
        // 11-17; "If start_date and end_date both are BLANK, the output value is also BLANK. If
        // either start_date or end_date is BLANK, the BLANK start_date or end_date will be treated
        // as Date(1899, 12, 30)"; "Do not specify dates as text" (Microsoft Learn, NETWORKDAYS).
        // The start "can be earlier than, the same as, or later than the end_date", but the page
        // doesn't say what a later start returns, so that is refused.
        Fn.scalar "NETWORKDAYS" family (2, 4) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let args = Array.ofList call.Args
            let first = workDate ev ctx args.[0]
            let last = workDate ev ctx args.[1]

            let weekend =
                if args.Length < 3 || Fn.isOmitted args.[2] then
                    [ 6; 0 ]
                else
                    match numArg ev ctx "a NETWORKDAYS weekend number" args.[2] with
                    | Num.Whole code ->
                        match weekendDays code with
                        | ValueSome days -> days
                        | ValueNone -> Fail.notSupported ctx args.[2].Span (weekendRefusal code)
                    | Num.Blank -> Fail.notSupported ctx args.[2].Span "NETWORKDAYS with a BLANK weekend number"
                    | Num.Real _ ->
                        Fail.notSupported ctx args.[2].Span "NETWORKDAYS with a weekend number that is not whole"

            let holidays = HashSet<int64>()

            if args.Length = 4 && not (Fn.isOmitted args.[3]) then
                let table = ev.Table(ctx, args.[3])

                if table.Cols.Length <> 1 then
                    Fail.invalid ctx args.[3].Span "the holidays of NETWORKDAYS are a table of one column"

                for v in table.Column 0 do
                    match v with
                    | DaxValue.DateTime s ->
                        let ms = moment ctx args.[3].Span s

                        if ms <> dayOf ms * msPerDay then
                            Fail.notSupported ctx args.[3].Span "NETWORKDAYS with a holiday that has a time of day"

                        holidays.Add(dayOf ms) |> ignore
                    | DaxValue.Blank -> Fail.notSupported ctx args.[3].Span "NETWORKDAYS with a BLANK holiday"
                    | _ -> Fail.notSupported ctx args.[3].Span "NETWORKDAYS with a holiday that is not a date value"

            match first, last with
            | ValueNone, ValueNone -> DaxValue.Blank
            | _ ->
                let first = first |> ValueOption.defaultValue 0L
                let last = last |> ValueOption.defaultValue 0L

                if first > last then
                    Fail.notSupported
                        ctx
                        call.Span
                        "NETWORKDAYS with a start date after the end date (what it returns is not documented)"

                let isWeekend = Array.init 7 (fun d -> List.contains d weekend)
                let mutable count = 0L

                for day in first..last do
                    if not isWeekend.[dayOfWeek day] && not (holidays.Contains day) then
                        count <- count + 1L

                DaxValue.Integer count)
        |> Fn.withCheck (fun args ->
            let text =
                [ 0; 1 ]
                |> List.exists (fun i ->
                    match List.tryItem i args |> Option.map (fun e -> (Fn.unparen e).Kind) with
                    | Some(StringLiteral _) -> true
                    | _ -> false)

            if text then
                Some "NETWORKDAYS with a date given as text"
            else
                match List.tryItem 2 args |> Option.bind literalWhole with
                | Some code when (weekendDays code).IsNone -> Some(weekendRefusal code)
                | _ -> None)

        // ---- The clock ------------------------------------------------------------------------------

        // The clock is fixed when the model is prepared (`Engine.prepareWith`), so every use in
        // one evaluation, and in the calculated tables a query reads, is the same instant.
        // "TODAY returns the time value 12:00:00 AM (midnight) for all dates" (Microsoft Learn).
        Fn.scalar "TODAY" family (0, 0) (Fn.typeIs DaxType.DateTime) (fun _ ctx _ ->
            DaxValue.ofDateTime ctx.Model.Now.Date)
        Fn.scalar "NOW" family (0, 0) (Fn.typeIs DaxType.DateTime) (fun _ ctx _ -> DaxValue.ofDateTime ctx.Model.Now)
        // Midnight: "The time returned is always 12:00:00 AM" (dax.guide, UTCTODAY). Microsoft
        // Learn's page says "12:00:00 PM", and its own example of FORMAT ( UTCTODAY (), "General
        // Date" ) prints the date alone, "2/2/2018", as General Date does at midnight.
        Fn.scalar "UTCTODAY" family (0, 0) (Fn.typeIs DaxType.DateTime) (fun _ ctx _ ->
            DaxValue.ofDateTime ctx.Model.UtcNow.Date)
        Fn.scalar "UTCNOW" family (0, 0) (Fn.typeIs DaxType.DateTime) (fun _ ctx _ ->
            DaxValue.ofDateTime ctx.Model.UtcNow)

        // ---- Calendars ------------------------------------------------------------------------------

        // "The range of dates is from the specified start date to the specified end date, inclusive
        // of those two dates"; "An error is returned if start_date is greater than end_date"
        // (Microsoft Learn, CALENDAR).
        Fn.table "CALENDAR" family (2, 2) (fun ev ctx call ->
            let first = calendarBound ev ctx "a start" call.Args.[0]
            let last = calendarBound ev ctx "an end" call.Args.[1]

            if first > last then
                Fail.runtime ctx call.Span "CALENDAR: the start date is after the end date."

            calendarTable first last)

        // "The earliest date in the model which is not in a calculated column or calculated table
        // is taken as the MinDate", the latest as the MaxDate; "The date range returned is dates
        // between the beginning of the fiscal year associated with MinDate and the end of the
        // fiscal year associated with MaxDate"; "An error is returned if the model does not contain
        // any datetime values which are not in calculated columns or calculated tables" (Microsoft
        // Learn, CALENDARAUTO). With dates from July 1, 2018 to June 30, 2019, CALENDARAUTO ( 3 )
        // runs from April 1, 2018 to March 31, 2020 (the same page). The month "rounds ... to the
        // nearest integer"; "If FiscalYearEndMonth is BLANK, CALENDARAUTO uses 12. If the value is
        // not between 1 and 12, CALENDARAUTO returns an error" (dax.guide). Left out, the engine
        // takes it from "the calendar table template for the current user, if present; otherwise
        // 12": there is no such template here.
        Fn.table "CALENDARAUTO" family (0, 1) (fun ev ctx call ->
            let fiscalEnd =
                match call.Args with
                | [] -> 12
                | e :: _ when Fn.isOmitted e -> 12
                | e :: _ ->
                    let what = "a CALENDARAUTO fiscal year end month"

                    match nearest ctx e.Span what (numArg ev ctx what e) with
                    | ValueNone -> 12
                    | ValueSome m when m >= 1L && m <= 12L -> int m
                    | ValueSome _ ->
                        Fail.runtime ctx e.Span "CALENDARAUTO: the fiscal year end month is not between 1 and 12."

            let mutable earliest = Int64.MaxValue
            let mutable latest = Int64.MinValue

            for t in ctx.Model.Tables do
                if t.Expression.IsNone then
                    for c in t.Columns do
                        if c.Expression.IsNone && c.DataType = DaxType.DateTime then
                            for v in c.Data.Values do
                                match v with
                                | DaxValue.DateTime s ->
                                    let day = dayOf (moment ctx call.Span s)
                                    earliest <- min earliest day
                                    latest <- max latest day
                                | _ -> ()

            if earliest > latest then
                Fail.runtime
                    ctx
                    call.Span
                    "CALENDARAUTO: the model has no dates outside calculated columns and calculated tables."

            let minDate = dateOfDay earliest
            let maxDate = dateOfDay latest
            // The fiscal year that ends in month 3 begins on April 1.
            let startMonth = fiscalEnd % 12 + 1

            let startYear =
                if minDate.Month >= startMonth then
                    minDate.Year
                else
                    minDate.Year - 1

            let endYear =
                if maxDate.Month <= fiscalEnd then
                    maxDate.Year
                else
                    maxDate.Year + 1

            if startYear < 1 || endYear > 9999 then
                outOfRange ctx call.Span

            calendarTable
                (dayOfDate (DateTime(startYear, startMonth, 1)))
                (dayOfDate (DateTime(endYear, fiscalEnd, DateTime.DaysInMonth(endYear, fiscalEnd)))))
    ]
