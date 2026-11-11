namespace Tessera

open System
open System.Globalization
open System.Numerics

// DAX scalar values and their semantics, for the local DAX engine.
//
// Sources: Microsoft Learn "Data types in Power BI Desktop" (the implicit-conversion tables, the BLANK
// table, case and trailing-space handling), "DAX operators" (comparison rules, `==`), the CONVERT,
// CURRENCY and MAX function pages, and SQLBI "Understanding numeric data type conversions in DAX" and
// "Handling BLANK in DAX" and "Rounding errors with different data types in DAX". Where those
// disagree or say nothing, the operation either returns `Unsettled` (the engine then refuses) or
// makes a choice that is marked ASSUMPTION next to the one function that implements it.

/// The data type of a DAX scalar value or of a column.
[<RequireQualifiedAccess>]
type DaxType =
    /// Whole number: a 64-bit integer (TOM `DataType.Int64`).
    | Integer
    /// Decimal number: a 64-bit float (TOM `DataType.Double`).
    | Decimal
    /// Fixed decimal number: a 64-bit integer count of 1/10000ths (TOM `DataType.Decimal`).
    | Currency
    | String
    | Boolean
    /// A date and time, stored as days since 1899-12-30 in a 64-bit float.
    | DateTime
    /// Not one type: an expression whose value can be of different types (a measure returning a
    /// number or a string). Never the type of a value, only of a column or an expression.
    | Variant

module DaxType =

    /// The name DAX itself uses, in CONVERT and DATATABLE.
    let name =
        function
        | DaxType.Integer -> "INTEGER"
        | DaxType.Decimal -> "DOUBLE"
        | DaxType.Currency -> "CURRENCY"
        | DaxType.String -> "STRING"
        | DaxType.Boolean -> "BOOLEAN"
        | DaxType.DateTime -> "DATETIME"
        | DaxType.Variant -> "VARIANT"

    /// Stable lower-case token (JSON output).
    let token =
        function
        | DaxType.Integer -> "integer"
        | DaxType.Decimal -> "decimal"
        | DaxType.Currency -> "currency"
        | DaxType.String -> "string"
        | DaxType.Boolean -> "boolean"
        | DaxType.DateTime -> "dateTime"
        | DaxType.Variant -> "variant"

    let all = [
        DaxType.Integer
        DaxType.Decimal
        DaxType.Currency
        DaxType.String
        DaxType.Boolean
        DaxType.DateTime
        DaxType.Variant
    ]

    /// A model column's type. Mind TOM's names: `DataType.Decimal` is the fixed decimal (Currency)
    /// and `DataType.Double` is the "Decimal number". None for types a column can't hold values of
    /// here (Binary, Automatic, Unknown).
    let ofTom (t: Microsoft.AnalysisServices.Tabular.DataType) =
        match t with
        | Microsoft.AnalysisServices.Tabular.DataType.Int64 -> Some DaxType.Integer
        | Microsoft.AnalysisServices.Tabular.DataType.Double -> Some DaxType.Decimal
        | Microsoft.AnalysisServices.Tabular.DataType.Decimal -> Some DaxType.Currency
        | Microsoft.AnalysisServices.Tabular.DataType.String -> Some DaxType.String
        | Microsoft.AnalysisServices.Tabular.DataType.Boolean -> Some DaxType.Boolean
        | Microsoft.AnalysisServices.Tabular.DataType.DateTime -> Some DaxType.DateTime
        | Microsoft.AnalysisServices.Tabular.DataType.Variant -> Some DaxType.Variant
        | _ -> None

    /// Integer, Decimal, Currency and DateTime are one family for comparison and arithmetic.
    let isNumeric =
        function
        | DaxType.Integer
        | DaxType.Decimal
        | DaxType.Currency
        | DaxType.DateTime -> true
        | _ -> false

/// A DAX scalar value.
///
/// `Currency` holds the scaled integer the engine stores (value x 10000), so every Currency is a
/// valid fixed decimal and its range is exactly the engine's; build one with `DaxValue.currency`
/// and read it with `DaxValue.currencyValue`. `DateTime` holds the serial number the engine stores
/// (days since 1899-12-30, the time as the fraction), so date arithmetic is the float arithmetic DAX
/// does; convert with `DaxValue.ofDateTime` / `DaxValue.toDateTime`.
///
/// Equality on this type is exact (same case, same payload, strings by code unit; NaN equals NaN
/// and -0.0 equals 0.0), which is what tests and dictionaries of constants want. DAX's own notions
/// of equality (`=`, `==`, grouping) are the functions of the `DaxValue` module.
[<RequireQualifiedAccess; CustomEquality; NoComparison>]
type DaxValue =
    | Blank
    | Integer of int64
    | Decimal of float
    | Currency of scaled: int64
    | String of string
    | Boolean of bool
    | DateTime of serial: float

    override this.Equals(other: obj) =
        match other with
        | :? DaxValue as o ->
            match this, o with
            | Blank, Blank -> true
            | Integer a, Integer b -> a = b
            | Decimal a, Decimal b -> a.Equals b
            | Currency a, Currency b -> a = b
            | String a, String b -> String.Equals(a, b, StringComparison.Ordinal)
            | Boolean a, Boolean b -> a = b
            | DateTime a, DateTime b -> a.Equals b
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Blank -> 0
        | Integer a -> HashCode.Combine(1, a)
        | Decimal a -> HashCode.Combine(2, a)
        | Currency a -> HashCode.Combine(3, a)
        | String a -> HashCode.Combine(4, String.GetHashCode(a, StringComparison.Ordinal))
        | Boolean a -> HashCode.Combine(5, a)
        | DateTime a -> HashCode.Combine(6, a)

/// Why an operation on values has no result. The evaluator turns these into query errors.
type DaxError =
    /// A comparison between families DAX doesn't compare (text with a number, a Boolean with text...).
    | IncomparableTypes of left: DaxType * right: DaxType
    /// A value that can't be converted to the type an operation or CONVERT needs.
    | CannotConvert of value: string * source: DaxType * target: DaxType
    /// A result outside the range of its type (Integer, Currency or DateTime).
    | Overflow of operation: string * resultType: DaxType
    /// Something the published sources leave open (they disagree, or say nothing). Not an error DAX
    /// raises: the engine refuses to answer (`not-supported`), and IFERROR must not catch it.
    | Unsettled of what: string

module DaxError =

    let private typeLabel =
        function
        | DaxType.Integer -> "Integer"
        | DaxType.Decimal -> "Number"
        | DaxType.Currency -> "Currency"
        | DaxType.String -> "Text"
        | DaxType.Boolean -> "True/False"
        | DaxType.DateTime -> "Date"
        | DaxType.Variant -> "Variant"

    /// A message in the engine's own wording, where it has one.
    let message =
        function
        | IncomparableTypes(l, r) ->
            $"DAX comparison operations do not support comparing values of type {typeLabel l} with values of type {typeLabel r}. "
            + "Consider using the VALUE or FORMAT function to convert one of the values."
        | CannotConvert(value, source, target) ->
            $"Cannot convert value '{value}' of type {typeLabel source} to type {typeLabel target}."
        | Overflow(operation, resultType) ->
            $"The result of {operation} is out of range for type {typeLabel resultType}."
        | Unsettled what -> $"Not supported by the local DAX engine: {what}"

/// How strings compare. One object answers equality, hashing and ordering, so the three always
/// agree. Trailing spaces never count: the engine drops them when it stores text, and `"a " = "a"`.
///
/// The engine's default is case-insensitive. ASSUMPTION: accents, kana and width count. The docs
/// fetched state only the case-insensitivity; a tabular model takes a Windows collation from its
/// culture, and whether that one ignores accents was not found.
///
/// The default here compares UTF-16 code units after upper-casing, not linguistically, so results
/// are the same on every OS and ICU version. So "é" and "e" are different values, and ORDER BY on
/// text is code-unit order: right for ASCII letters and digits ("apple" < "Banana" < "cherry"), but
/// not a culture's alphabetical order for other text ("Zebra" sorts before "Ärger"). Pass another
/// `Collation` (see `Collation.ofCulture`) to change it.
type Collation = {
    Name: string
    /// Must implement Equals, GetHashCode and Compare consistently.
    Comparer: StringComparer
}

module Collation =

    /// The default: case-insensitive, by code unit (see `Collation`).
    let ordinalIgnoreCase = {
        Name = "ordinal-ignore-case"
        Comparer = StringComparer.OrdinalIgnoreCase
    }

    /// Case-sensitive, by code unit: what a model with a `_CS` or binary collation does.
    let ordinal = {
        Name = "ordinal"
        Comparer = StringComparer.Ordinal
    }

    /// Linguistic comparison for a culture ("" is the invariant culture). Results depend on the
    /// platform's ICU/NLS data, and ignorable characters (e.g. a soft hyphen) compare as absent.
    let ofCulture (culture: CultureInfo) (ignoreCase: bool) = {
        Name =
            (if culture.Name = "" then "invariant" else culture.Name)
            + (if ignoreCase then "-ci" else "-cs")
        Comparer =
            StringComparer.Create(
                culture,
                (if ignoreCase then
                     CompareOptions.IgnoreCase
                 else
                     CompareOptions.None)
            )
    }

    /// The text without the trailing spaces the engine doesn't keep (U+0020 only).
    let trimTrailing (s: string) =
        if s.Length > 0 && s.[s.Length - 1] = ' ' then
            s.TrimEnd ' '
        else
            s

    let equals (c: Collation) (a: string) (b: string) =
        c.Comparer.Equals(trimTrailing a, trimTrailing b)

    let hash (c: Collation) (a: string) = c.Comparer.GetHashCode(trimTrailing a)

    let compare (c: Collation) (a: string) (b: string) =
        sign (c.Comparer.Compare(trimTrailing a, trimTrailing b))

/// Equality and ordering of values for grouping and sorting (VALUES, SUMMARIZE, ORDER BY): total,
/// never fails, and Compare = 0 exactly when Equals. See `DaxValue.sortCompare`.
[<Sealed>]
type DaxValueComparer(collation: Collation, sortCompare: Collation -> DaxValue -> DaxValue -> int) =
    member _.Collation = collation

    interface System.Collections.Generic.IEqualityComparer<DaxValue> with
        member _.Equals(a, b) = sortCompare collation a b = 0

        member _.GetHashCode v =
            match v with
            | DaxValue.String s -> HashCode.Combine(4, Collation.hash collation s)
            // -0.0 and 0.0 are one value; so is every NaN.
            | DaxValue.Decimal d when d = 0.0 -> HashCode.Combine(2, 0.0)
            | DaxValue.DateTime d when d = 0.0 -> HashCode.Combine(6, 0.0)
            | v -> v.GetHashCode()

    interface System.Collections.Generic.IComparer<DaxValue> with
        member _.Compare(a, b) = sortCompare collation a b

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DaxValue =

    // ---- Currency ---------------------------------------------------------------------------------

    /// A Currency holds its value times this.
    [<Literal>]
    let CurrencyScale = 10000L

    /// The value of a Currency's scaled integer.
    let currencyValue (scaled: int64) : decimal = decimal scaled / 10000m

    /// The whole numbers the documentation gives as outside the Integer range and as synonyms of
    /// null ("must be whole numbers between -9,223,372,036,854,775,807 (-2^63+1) and
    /// 9,223,372,036,854,775,806 (2^63-2)"; "The values in the following list are synonyms to the
    /// previous mentioned null value: 9223372036854775807, -9223372036854775808": Microsoft Learn,
    /// "Data types supported in Analysis Services tabular models"). The Currency range excludes
    /// the same two scaled values. What an operation that lands on one gives is not documented.
    let private isNullSynonym (i: int64) =
        i = Int64.MaxValue || i = Int64.MinValue

    /// The exact value of a finite double times `scale`, rounded half away from zero.
    let private exactRound (x: float) (scale: int) : BigInteger =
        let bits = BitConverter.DoubleToInt64Bits x
        let exponent = int ((bits >>> 52) &&& 0x7FFL)
        let fraction = bits &&& 0xFFFFFFFFFFFFFL

        let mantissa, e =
            if exponent = 0 then
                fraction, -1074
            else
                (fraction ||| 0x10000000000000L), exponent - 1075

        let m = BigInteger mantissa * BigInteger scale

        let magnitude =
            if e >= 0 then
                m <<< e
            else
                let q = m >>> -e
                let rest = m - (q <<< -e)

                if rest >= (BigInteger.One <<< (-e - 1)) then
                    q + BigInteger.One
                else
                    q

        if bits < 0L then -magnitude else magnitude

    let private int64Min = BigInteger Int64.MinValue
    let private int64Max = BigInteger Int64.MaxValue

    /// A decimal number times `scale` (1 for an Integer, 10000 for a Currency) as a whole int64,
    /// a half rounding away from zero.
    ///
    /// Sources. To an Integer: "A value that ends in .5 is rounded away from zero. For example,
    /// CONVERT ( 2.5, INTEGER ) returns 3 and CONVERT ( -2.5, INTEGER ) returns -3"
    /// (dax.guide/convert). To a Currency: "rounds up the 5th significant decimal ... Rounding up
    /// occurs if the 5th significant decimal is equal or larger than 5" (Microsoft Learn,
    /// CURRENCY): a rule on the digits, so the same for a negative number; SQLBI ("Rounding errors
    /// with different data types in DAX") publishes the tie 1.2345 / 2 as 0.6173. The data types
    /// page's "truncating" is about what the type can store, not about this conversion.
    ///
    /// What no source says is which number is rounded: the double's exact binary value, the double
    /// multiplied by the scale, or the number as it reads in 15 significant digits (ROUND ( 2.15,
    /// 1 ) is documented as 2.2, though the double 2.15 is below the tie). They differ only for a
    /// double within a rounding error of a tie; then the answer is `Unsettled`.
    let private roundFloat (op: string) (target: DaxType) (scale: int) (x: float) : Result<int64, DaxError> =
        if Double.IsNaN x || Double.IsInfinity x then
            Error(Overflow(op, target))
        else
            let exact = exactRound x scale

            if exact > int64Max || exact < int64Min then
                Error(Overflow(op, target))
            else
                let exact = int64 exact
                let scaled = Math.Round(x * float scale, MidpointRounding.AwayFromZero)

                let asDecimal =
                    if abs x < 1e19 then
                        Math.Round(decimal x * decimal scale, 0, MidpointRounding.AwayFromZero)
                    else
                        decimal exact

                if isNullSynonym exact then
                    Error(
                        Unsettled
                            $"{op} to the largest or smallest 64-bit value, which the documentation gives as a synonym of null"
                    )
                elif scaled = float exact && asDecimal = decimal exact then
                    Ok exact
                else
                    Error(
                        Unsettled
                            $"{op} of a decimal number within a rounding error of halfway between two results (whether the binary value or the number as written is rounded is not documented)"
                    )

    let private decimalToScaled (d: decimal) =
        // Half away from zero: see `roundFloat`.
        if d > 922337203685477.5807m || d < -922337203685477.5808m then
            None
        else
            let r = Math.Round(d * 10000m, 0, MidpointRounding.AwayFromZero)

            if r > 9223372036854775807m || r < -9223372036854775808m then
                None
            else
                Some(Decimal.ToInt64 r)

    /// A Currency from a decimal number, rounded to four decimals. None when out of range.
    let tryCurrency (d: decimal) =
        decimalToScaled d |> Option.map DaxValue.Currency

    /// A Currency from a decimal number, rounded to four decimals.
    let currency (d: decimal) : Result<DaxValue, DaxError> =
        match decimalToScaled d with
        | Some s -> Ok(DaxValue.Currency s)
        | None -> Error(Overflow("the conversion", DaxType.Currency))

    // ---- DateTime ---------------------------------------------------------------------------------

    let private epochTicks = DateTime(1899, 12, 30).Ticks

    /// The serial number of a date and time: days since 1899-12-30, the time of day as the fraction.
    /// Linear on both sides of the epoch (unlike OLE Automation dates, which mirror the fraction for
    /// negative serials).
    let serialOfDateTime (dt: DateTime) : float =
        let ticks = dt.Ticks - epochTicks
        let days = Math.DivRem(ticks, TimeSpan.TicksPerDay)
        let struct (whole, rest) = days
        float whole + float rest / float TimeSpan.TicksPerDay

    let ofDateTime (dt: DateTime) = DaxValue.DateTime(serialOfDateTime dt)

    /// The date and time of a serial number, to the nearest millisecond (a float can't hold a tick
    /// exactly this far from the epoch). None outside 0001-01-01..9999-12-31.
    let dateTimeOfSerial (serial: float) : DateTime option =
        if Double.IsNaN serial || Double.IsInfinity serial then
            None
        else
            let ms = Math.Round(serial * 86400000.0, MidpointRounding.AwayFromZero)
            let min = float (DateTime.MinValue.Ticks - epochTicks) / 10000.0
            let max = float (DateTime.MaxValue.Ticks - epochTicks) / 10000.0

            if ms < min || ms > max then
                None
            else
                Some(DateTime(epochTicks + int64 ms * TimeSpan.TicksPerMillisecond))

    let toDateTime =
        function
        | DaxValue.DateTime serial -> dateTimeOfSerial serial
        | _ -> None

    // ---- Types ------------------------------------------------------------------------------------

    /// The value's type; None for BLANK, which has none.
    let typeOf =
        function
        | DaxValue.Blank -> None
        | DaxValue.Integer _ -> Some DaxType.Integer
        | DaxValue.Decimal _ -> Some DaxType.Decimal
        | DaxValue.Currency _ -> Some DaxType.Currency
        | DaxValue.String _ -> Some DaxType.String
        | DaxValue.Boolean _ -> Some DaxType.Boolean
        | DaxValue.DateTime _ -> Some DaxType.DateTime

    let isBlank =
        function
        | DaxValue.Blank -> true
        | _ -> false

    /// Whether a value can sit in a column of this type: BLANK always, anything in a Variant column.
    let fits (t: DaxType) (v: DaxValue) =
        match typeOf v with
        | None -> true
        | Some vt -> t = DaxType.Variant || vt = t

    // ---- Text -------------------------------------------------------------------------------------

    let private inv = CultureInfo.InvariantCulture

    /// A number as DAX shows it when it becomes text: up to 15 significant digits, no trailing zeros.
    let private floatText (d: float) =
        if Double.IsNaN d then "NaN"
        elif Double.IsPositiveInfinity d then "Infinity"
        elif Double.IsNegativeInfinity d then "-Infinity"
        elif d = 0.0 then "0"
        else d.ToString("G15", inv)

    let private dateText (serial: float) =
        // ASSUMPTION: the engine formats dates with the model's culture ("General Date"); this is
        // the en-US form, with the time left out at midnight and the date left out for a time alone.
        // Two published results agree with the en-US form: FORMAT ( UTCTODAY (), "General Date" )
        // is "2/2/2018" and FORMAT ( UTCNOW (), "General Date" ) is "2/2/2018 4:48:08 AM"
        // (Microsoft Learn, UTCTODAY and UTCNOW). What another culture gives is still open, so
        // the local engine refuses a date turned into text (`Convert.toText`).
        match dateTimeOfSerial serial with
        | None -> floatText serial
        | Some dt ->
            if dt.TimeOfDay = TimeSpan.Zero then
                dt.ToString("M/d/yyyy", inv)
            elif serial >= 0.0 && serial < 1.0 then
                dt.ToString("h:mm:ss tt", inv)
            else
                dt.ToString("M/d/yyyy h:mm:ss tt", inv)

    /// The value as text, as `&` and CONVERT(..., STRING) produce it. BLANK is "".
    let toText =
        function
        | DaxValue.Blank -> ""
        | DaxValue.Integer i -> i.ToString inv
        | DaxValue.Decimal d -> floatText d
        | DaxValue.Currency c -> (currencyValue c).ToString("0.####", inv)
        | DaxValue.String s -> s
        | DaxValue.Boolean true -> "True"
        | DaxValue.Boolean false -> "False"
        | DaxValue.DateTime s -> dateText s

    /// A number written as text: digits, a sign, a decimal point, an exponent. Never "NaN" or
    /// "Infinity", which .NET would accept. An integer if it reads as one and fits.
    let private parseNumber (s: string) : DaxValue option =
        let t = s.Trim()

        let numeric =
            t.Length > 0
            && t
               |> Seq.forall (fun ch ->
                   Char.IsAsciiDigit ch || ch = '+' || ch = '-' || ch = '.' || ch = 'e' || ch = 'E')

        if not numeric then
            None
        else
            match Int64.TryParse(t, NumberStyles.AllowLeadingSign, inv) with
            | true, i -> Some(DaxValue.Integer i)
            | _ ->
                match Double.TryParse(t, NumberStyles.Float, inv) with
                | true, d when not (Double.IsInfinity d) -> Some(DaxValue.Decimal d)
                | _ -> None

    let private isoFormats = [|
        "yyyy-MM-dd"
        "yyyy-MM-dd'T'HH:mm"
        "yyyy-MM-dd'T'HH:mm:ss"
        "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF"
        "yyyy-MM-dd HH:mm"
        "yyyy-MM-dd HH:mm:ss"
        "yyyy-MM-dd HH:mm:ss.FFFFFFF"
    |]

    let private timeFormats = [| "hh\\:mm"; "hh\\:mm\\:ss"; "hh\\:mm\\:ss\\.FFFFFFF" |]

    /// A plain `yyyy-MM-dd` date: the common case, read without going through the list of formats.
    let private plainDate (t: ReadOnlySpan<char>) : float voption =
        if t.Length <> 10 || t.[4] <> '-' || t.[7] <> '-' then
            ValueNone
        else
            let mutable ok = true

            for i in 0..9 do
                ok <- ok && (i = 4 || i = 7 || Char.IsAsciiDigit t.[i])

            if not ok then
                ValueNone
            else
                let digit (c: char) = int c - int '0'
                let year = digit t.[0] * 1000 + digit t.[1] * 100 + digit t.[2] * 10 + digit t.[3]
                let month = digit t.[5] * 10 + digit t.[6]
                let day = digit t.[8] * 10 + digit t.[9]

                if
                    year >= 1
                    && month >= 1
                    && month <= 12
                    && day >= 1
                    && day <= DateTime.DaysInMonth(year, month)
                then
                    ValueSome(serialOfDateTime (DateTime(year, month, day)))
                else
                    ValueNone

    /// The serial number of ISO 8601 text; see `parseIsoDateTime`.
    let parseIsoSerial (text: ReadOnlySpan<char>) : float voption =
        let t = text.Trim()

        let t =
            if t.Length > 11 && (t.[t.Length - 1] = 'Z' || t.[t.Length - 1] = 'z') then
                t.Slice(0, t.Length - 1)
            else
                t

        let mutable dt = DateTime.MinValue
        let plain = plainDate t

        if plain.IsSome then
            plain
        elif t.Length = 10 && t.[4] = '-' && t.[7] = '-' then
            ValueNone
        elif DateTime.TryParseExact(t, isoFormats, inv, DateTimeStyles.None, &dt) then
            ValueSome(serialOfDateTime dt)
        else
            let mutable ts = TimeSpan.Zero

            if
                TimeSpan.TryParseExact(t, timeFormats, inv, &ts)
                && ts >= TimeSpan.Zero
                && ts < TimeSpan.FromDays 1.0
            then
                ValueSome(float ts.Ticks / float TimeSpan.TicksPerDay)
            else
                ValueNone

    /// An ISO 8601 date, date and time (`T` or a space between; an optional trailing `Z`, which is
    /// dropped: values are not shifted between time zones) or time of day (which lands on
    /// 1899-12-30, as the engine stores a time). None for anything else, including UTC offsets.
    let parseIsoDateTime (s: string) : DaxValue option =
        match parseIsoSerial (s.AsSpan()) with
        | ValueSome serial -> Some(DaxValue.DateTime serial)
        | ValueNone -> None

    /// A date written as text. ASSUMPTION: the engine reads the model culture's date formats; this
    /// reads ISO 8601 and then whatever .NET's invariant culture reads ("1/31/2024", "Jan 1 2009").
    /// The first half is documented: "DATEVALUE uses the locale and date/time settings of the model
    /// ... If conversion using the locale and date/time settings of the model fails, DATEVALUE will
    /// attempt to use other date formats" (Microsoft Learn, DATEVALUE). Which other formats is not,
    /// so the second half stays a guess: the local engine doesn't use this for its date functions
    /// (they read only the forms a source shows a result for: `DateText` in
    /// Engine/Functions/DateTime.fs) and refuses text read as a date in arithmetic.
    let private parseDate (s: string) : DaxValue option =
        match parseIsoDateTime s with
        | Some v -> Some v
        | None ->
            match DateTime.TryParse(s, inv, DateTimeStyles.AllowWhiteSpaces) with
            | true, dt -> Some(ofDateTime dt)
            | _ -> None

    // ---- Numbers ----------------------------------------------------------------------------------

    /// A value as an operand of arithmetic.
    [<Struct>]
    type private Num =
        | NInt of i: int64
        | NCur of c: int64
        | NReal of r: float
        | NDate of d: float

    let private numType =
        function
        | NInt _ -> DaxType.Integer
        | NCur _ -> DaxType.Currency
        | NReal _ -> DaxType.Decimal
        | NDate _ -> DaxType.DateTime

    let private numFloat =
        function
        | NInt i -> float i
        | NCur c -> float c / 10000.0
        | NReal r -> r
        | NDate d -> d

    let private ofNum =
        function
        | NInt i -> DaxValue.Integer i
        | NCur c -> DaxValue.Currency c
        | NReal r -> DaxValue.Decimal r
        | NDate d -> DaxValue.DateTime d

    /// Text that is not a plain number but is made of what a culture writes numbers with: digits
    /// with thousands separators, a currency symbol, a percent sign, parentheses. "the string is
    /// automatically converted into a corresponding number, as long as the string is a valid
    /// representation of a number" (SQLBI, "Understanding numeric data type conversions in DAX"):
    /// which of these forms are valid is not documented.
    let isCultureNumber (s: string) =
        let t = s.Trim()

        // A sign only at either end: "2024-01-31" is a date, not a number.
        let inner = if t.Length > 2 then t.Substring(1, t.Length - 2) else ""

        t |> Seq.exists Char.IsAsciiDigit
        && not (inner.Contains '-' || inner.Contains '+')
        && t
           |> Seq.forall (fun ch ->
               Char.IsAsciiDigit ch
               || ch = ' '
               || ch = '\u00A0'
               || ch = ','
               || ch = '.'
               || ch = '+'
               || ch = '-'
               || ch = '('
               || ch = ')'
               || ch = '%'
               || ch = '\''
               || Char.GetUnicodeCategory ch = UnicodeCategory.CurrencySymbol)

    let private cultureNumber (s: string) =
        Unsettled
            $"the text \"{s}\" read as a number (which separators and symbols the engine accepts in a number depends on the model's culture and is not documented)"

    /// The operand of an arithmetic operator: TRUE is 1 and FALSE 0; text is read as a number, or
    /// failing that as a date ("22" + 22 = 44). None for BLANK, which each operator treats its own way.
    let private toNum (target: DaxType) (v: DaxValue) : Result<Num voption, DaxError> =
        match v with
        | DaxValue.Blank -> Ok ValueNone
        | DaxValue.Integer i -> Ok(ValueSome(NInt i))
        | DaxValue.Decimal d -> Ok(ValueSome(NReal d))
        | DaxValue.Currency c -> Ok(ValueSome(NCur c))
        | DaxValue.DateTime d -> Ok(ValueSome(NDate d))
        | DaxValue.Boolean b -> Ok(ValueSome(NInt(if b then 1L else 0L)))
        | DaxValue.String s ->
            // "the string is automatically converted into a corresponding number, as long as the
            // string is a valid representation of a number" (SQLBI, "Understanding numeric data
            // type conversions in DAX"): digits with a sign and a decimal point are; whether
            // spaces around them, thousands separators or a currency symbol are is not said.
            let decorated =
                s.Length > 0
                && s |> Seq.exists Char.IsAsciiDigit
                && s
                   |> Seq.forall (fun ch ->
                       Char.IsAsciiDigit ch
                       || Char.IsWhiteSpace ch
                       || Char.GetUnicodeCategory ch = UnicodeCategory.CurrencySymbol
                       || "+-.,eE()%".Contains ch)

            match parseNumber s with
            | Some _ when s <> s.Trim() ->
                Error(
                    Unsettled
                        $"the text \"{s}\" read as a number (whether spaces around a number are accepted is not documented)"
                )
            | Some(DaxValue.Integer i) -> Ok(ValueSome(NInt i))
            | Some(DaxValue.Decimal d) -> Ok(ValueSome(NReal d))
            | _ when isCultureNumber s -> Error(cultureNumber s)
            | _ ->
                match parseDate s with
                | Some(DaxValue.DateTime d) -> Ok(ValueSome(NDate d))
                | _ when decorated -> Error(cultureNumber s)
                | _ -> Error(CannotConvert(s, DaxType.String, target))

    let private scaledOfInt (i: int64) =
        let wide = Int128.op_Implicit i * Int128.op_Implicit 10000L

        if
            wide > Int128.op_Implicit Int64.MaxValue
            || wide < Int128.op_Implicit Int64.MinValue
        then
            None
        else
            Some(Int128.op_Explicit wide: int64)

    /// Compare a/10000 with d exactly (d finite or infinite, not NaN).
    let private compareScaledFloat (scaled: int64) (d: float) =
        let approx = float scaled / 10000.0

        if Double.IsInfinity d then
            (if d > 0.0 then -1 else 1)
        elif abs (approx - d) > 1e-6 * max 1.0 (max (abs approx) (abs d)) then
            compare approx d
        else
            // Close: decide exactly. d = m * 2^e; compare scaled * 2^-e with m * 10000 (or the mirror).
            let bits = BitConverter.DoubleToInt64Bits d
            let exponent = int ((bits >>> 52) &&& 0x7FFL)
            let fraction = bits &&& 0xFFFFFFFFFFFFFL

            let mantissa, e =
                if exponent = 0 then
                    fraction, -1074
                else
                    (fraction ||| 0x10000000000000L), exponent - 1075

            let m = BigInteger(if bits < 0L then -mantissa else mantissa)
            let left = BigInteger scaled
            let right = m * BigInteger 10000

            if e >= 0 then
                BigInteger.Compare(left, right <<< e)
            else
                BigInteger.Compare(left <<< -e, right)

    /// Compare an integer with a float exactly (no rounding of the integer to 53 bits).
    let private compareIntFloat (i: int64) (d: float) =
        if d >= 9223372036854775808.0 then
            -1
        elif d < -9223372036854775808.0 then
            1
        else
            let whole = Math.Truncate d
            let w = int64 whole

            if i <> w then compare i w
            elif d > whole then -1
            elif d < whole then 1
            else 0

    /// Exact order of two numbers, whatever their types. NaN is below every number and equal to itself.
    let private compareNum (a: Num) (b: Num) : int =
        let floats (x: float) (y: float) = x.CompareTo y

        let withFloat (n: Num) (d: float) =
            if Double.IsNaN d then
                1
            else
                match n with
                | NInt i -> compareIntFloat i d
                | NCur c -> compareScaledFloat c d
                | NReal r
                | NDate r -> floats r d

        match a, b with
        | NInt x, NInt y -> compare x y
        | NCur x, NCur y -> compare x y
        | NInt x, NCur y -> Int128.Sign(Int128.op_Implicit x * Int128.op_Implicit 10000L - Int128.op_Implicit y)
        | NCur x, NInt y -> Int128.Sign(Int128.op_Implicit x - Int128.op_Implicit y * Int128.op_Implicit 10000L)
        | (NReal x | NDate x), (NReal y | NDate y) -> floats x y
        | n, (NReal d | NDate d) -> withFloat n d
        | (NReal d | NDate d), n -> -(withFloat n d)

    let private asNum =
        function
        | DaxValue.Integer i -> ValueSome(NInt i)
        | DaxValue.Decimal d -> ValueSome(NReal d)
        | DaxValue.Currency c -> ValueSome(NCur c)
        | DaxValue.DateTime d -> ValueSome(NDate d)
        | _ -> ValueNone

    // ---- Comparison -------------------------------------------------------------------------------

    /// The order of two numbers as the comparison operators see it: "DAX does implicit conversions
    /// for numeric or date/time types as the following table describes" (Microsoft Learn, "Data
    /// types in Power BI Desktop", Comparison operators): two Integers compare as Integers, two
    /// Currencies or a Currency with an Integer as Currencies, and everything else (a Decimal or a
    /// date with anything) as REAL, that is after both sides became 64-bit floats. So
    /// 9007199254740993 = 9007199254740992.0 (the Integer has no exact float), and CURRENCY ( 0.1 )
    /// = 0.1. NaN is below every number and equal to itself.
    let private operatorCompareNum (a: Num) (b: Num) : Result<int, DaxError> =
        match a, b with
        | NInt x, NInt y -> Ok(compare x y)
        | NCur x, NCur y -> Ok(compare x y)
        | NInt i, NCur c
        | NCur c, NInt i ->
            match scaledOfInt i with
            | Some s ->
                match a with
                | NInt _ -> Ok(compare s c)
                | _ -> Ok(compare c s)
            | None ->
                Error(
                    Unsettled
                        "a comparison of a fixed decimal number with a whole number too large to be one (the comparison is made as fixed decimal numbers; what the conversion does out of range is not documented)"
                )
        | _ -> Ok((numFloat a).CompareTo(numFloat b))

    /// The order the comparison operators (`<`, `<=`, `>`, `>=`, and `=`/`<>` through `equals`) see.
    ///
    /// - BLANK is the other side's zero: 0, "", FALSE or 1899-12-30. BLANK equals BLANK.
    /// - Integer, Decimal, Currency and DateTime compare as numbers, converted as the
    ///   documentation's comparison table says (see `operatorCompareNum`).
    /// - Text compares with the collation (case-insensitive by default), ignoring trailing spaces.
    /// - FALSE is less than TRUE.
    /// - Any other mix is an error, as in the current engine ("... do not support comparing values
    ///   of type Text with values of type Integer"). The data types page still describes the older
    ///   behaviour, where Boolean > text > number; that ranking is what `sortCompare` uses.
    ///
    /// Not transitive: BLANK = 0 and BLANK = "", yet 0 and "" don't compare; and two different
    /// Integers above 2^53 can both equal one Decimal.
    let compare (collation: Collation) (a: DaxValue) (b: DaxValue) : Result<int, DaxError> =
        let zeroOf =
            function
            | DaxValue.Integer _ -> DaxValue.Integer 0L
            | DaxValue.Decimal _ -> DaxValue.Decimal 0.0
            | DaxValue.Currency _ -> DaxValue.Currency 0L
            | DaxValue.DateTime _ -> DaxValue.DateTime 0.0
            | DaxValue.String _ -> DaxValue.String ""
            | DaxValue.Boolean _ -> DaxValue.Boolean false
            | DaxValue.Blank -> DaxValue.Blank

        let a, b =
            match a, b with
            | DaxValue.Blank, other -> zeroOf other, other
            | other, DaxValue.Blank -> other, zeroOf other
            | _ -> a, b

        match a, b with
        | DaxValue.Blank, _
        | _, DaxValue.Blank -> Ok 0
        | DaxValue.String x, DaxValue.String y -> Ok(Collation.compare collation x y)
        | DaxValue.Boolean x, DaxValue.Boolean y -> Ok(Operators.compare x y)
        | _ ->
            match asNum a, asNum b with
            | ValueSome x, ValueSome y -> operatorCompareNum x y
            | _ ->
                match typeOf a, typeOf b with
                | Some l, Some r -> Error(IncomparableTypes(l, r))
                // Unreachable: BLANKs were replaced above.
                | _ -> Ok 0

    /// Whether comparing every value of a column of type `column` with `value` can be answered by
    /// looking up the one column value that `value` converts to. Not when the comparison is made
    /// as floats (see `operatorCompareNum`) and the numbers are large enough for several column
    /// values to become the same float: Integers from 2^53, Currencies from 2^39.
    let comparesOneToOne (column: DaxType) (value: DaxValue) : bool =
        match column, value with
        | DaxType.Integer, (DaxValue.Decimal d | DaxValue.DateTime d) -> abs d < 9007199254740992.0
        | DaxType.Currency, (DaxValue.Decimal d | DaxValue.DateTime d) -> abs d < 549755813888.0
        | _ -> true

    /// `=`: BLANK equals 0, "", FALSE and 1899-12-30 (see `compare`).
    let equals (collation: Collation) (a: DaxValue) (b: DaxValue) : Result<bool, DaxError> =
        compare collation a b |> Result.map (fun c -> c = 0)

    /// `==`: like `=`, except that BLANK equals only BLANK.
    let strictEquals (collation: Collation) (a: DaxValue) (b: DaxValue) : Result<bool, DaxError> =
        match a, b with
        | DaxValue.Blank, DaxValue.Blank -> Ok true
        | DaxValue.Blank, _
        | _, DaxValue.Blank -> Ok false
        | _ -> equals collation a b

    let private rank =
        function
        | DaxValue.Blank -> 0
        | DaxValue.Integer _
        | DaxValue.Decimal _
        | DaxValue.Currency _
        | DaxValue.DateTime _ -> 1
        | DaxValue.String _ -> 2
        | DaxValue.Boolean _ -> 3

    let private typeOrder =
        function
        | DaxValue.Integer _ -> 0
        | DaxValue.Currency _ -> 1
        | DaxValue.Decimal _ -> 2
        | DaxValue.DateTime _ -> 3
        | _ -> 4

    /// The total order for ORDER BY, TOPN, RANKX and MIN/MAX over mixed types, and the equality
    /// behind grouping (VALUES, DISTINCT, SUMMARIZE): never fails, and 0 exactly for values that are
    /// one group.
    ///
    /// - BLANK first; it is its own group, apart from 0, "" and FALSE. ASSUMPTION: BLANK sorts before
    ///   negative numbers too (the ORDER BY page doesn't say).
    /// - Then numbers and dates together, by value; then text, by the collation; then FALSE, TRUE
    ///   ("Boolean values [are] greater than string values, and string values greater than numeric or
    ///   date/time values").
    /// - Numbers are ordered by their exact values here (no conversion to floats, unlike the
    ///   comparison operators), so the order is total and consistent.
    /// - Equal numbers of different types (Integer 1, Currency 1, Decimal 1.0) are different groups,
    ///   ordered Integer, Currency, Decimal, DateTime. A column holds one type, so this only shows in
    ///   Variant columns; convert first to group across types.
    /// - Text that differs only in case (or trailing spaces) is one group.
    let sortCompare (collation: Collation) (a: DaxValue) (b: DaxValue) : int =
        match Operators.compare (rank a) (rank b) with
        | 0 ->
            match a, b with
            | DaxValue.String x, DaxValue.String y -> Collation.compare collation x y
            | DaxValue.Boolean x, DaxValue.Boolean y -> Operators.compare x y
            | _ ->
                match asNum a, asNum b with
                | ValueSome x, ValueSome y ->
                    match compareNum x y with
                    | 0 -> Operators.compare (typeOrder a) (typeOrder b)
                    | c -> c
                | _ -> 0
        | c -> c

    /// Whether two values are one group (see `sortCompare`).
    let groupEquals (collation: Collation) (a: DaxValue) (b: DaxValue) = sortCompare collation a b = 0

    /// Equality, hashing and ordering for dictionaries and sorts, all agreeing with `sortCompare`.
    /// Keep the first value seen for a group to show VertiPaq's "first casing wins".
    let comparer (collation: Collation) =
        DaxValueComparer(collation, sortCompare)

    // ---- Arithmetic -------------------------------------------------------------------------------

    let private overflow op t = Error(Overflow(op, t))

    let private fitsInt64 (wide: Int128) =
        wide <= Int128.op_Implicit Int64.MaxValue
        && wide >= Int128.op_Implicit Int64.MinValue

    /// A whole-number operation with a null synonym (see `isNullSynonym`) as an operand or the
    /// result: an operand may already be BLANK to the engine.
    let private atTheLimits (op: string) =
        Error(
            Unsettled
                $"{op} with 9223372036854775807 or -9223372036854775808 as an operand or as the result (values the documentation puts outside the range of whole numbers)"
        )

    /// The 64-bit result of a whole-number operation (an Integer, or the scaled integer of a
    /// Currency). With `wrap`, a result out of range keeps its low 64 bits: "there is an overflow
    /// condition, but there is no overflow error and the result is the least significant
    /// representable digits. ... CONVERT ( 5E18, INTEGER ) + CONVERT ( 5E18, INTEGER ) ... the
    /// outcome of the expression is a negative number: -8,446,744,073,709,551,616", and a Currency
    /// sum is "subject to the behavior of integers in case of overflow" (SQLBI, "Rounding errors
    /// with different data types in DAX"). An aggregation's total is checked by the aggregation.
    let private wholeResult (op: string) (wide: Int128) : Result<int64, DaxError> =
        let r: int64 = Int128.op_Explicit wide

        if isNullSynonym r then atTheLimits op else Ok r

    let private tooLargeForCurrency (op: string) =
        Unsettled
            $"{op} of a fixed decimal number and a whole number too large to be one (what the conversion does out of range is not documented)"

    let private operands (a: DaxValue) (b: DaxValue) =
        match toNum DaxType.Decimal a with
        | Error e -> Error e
        | Ok x -> toNum DaxType.Decimal b |> Result.map (fun y -> x, y)

    let private addOrSubtract (op: string) (subtract: bool) (a: DaxValue) (b: DaxValue) =
        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, ValueNone) -> Ok DaxValue.Blank
        | Ok(x, y) ->
            // BLANK + 5 = 5, BLANK - 4 = -4: next to a value, BLANK is the Integer 0.
            let x = x |> ValueOption.defaultValue (NInt 0L)
            let y = y |> ValueOption.defaultValue (NInt 0L)

            let f =
                if subtract then
                    numFloat x - numFloat y
                else
                    numFloat x + numFloat y

            // The result type, from the docs' tables. Addition: a date wins, then REAL, then CURRENCY.
            // Subtraction: the same, except that only a date on the left gives a date (number - date
            // is REAL).
            let resultType =
                match numType x, numType y with
                | DaxType.DateTime, _ -> DaxType.DateTime
                | _, DaxType.DateTime -> if subtract then DaxType.Decimal else DaxType.DateTime
                | DaxType.Decimal, _
                | _, DaxType.Decimal -> DaxType.Decimal
                | DaxType.Currency, _
                | _, DaxType.Currency -> DaxType.Currency
                | _ -> DaxType.Integer

            let whole (l: int64) (r: int64) =
                let l, r = Int128.op_Implicit l, Int128.op_Implicit r
                wholeResult op (if subtract then l - r else l + r)

            let scaled (n: Num) =
                match n with
                | NInt i -> scaledOfInt i
                | NCur c -> Some c
                | NReal _
                | NDate _ -> None

            match resultType with
            | DaxType.DateTime -> Ok(DaxValue.DateTime f)
            | DaxType.Decimal -> Ok(DaxValue.Decimal f)
            | DaxType.Currency ->
                match scaled x, scaled y with
                | Some l, Some r -> whole l r |> Result.map DaxValue.Currency
                | _ -> Error(tooLargeForCurrency op)
            | _ ->
                match x, y with
                | NInt l, NInt r when isNullSynonym l || isNullSynonym r -> atTheLimits op
                | NInt l, NInt r -> whole l r |> Result.map DaxValue.Integer
                | _ -> Ok(DaxValue.Decimal f)

    /// `+`. BLANK + BLANK is BLANK; otherwise BLANK is 0. Result type: DateTime if either side is a
    /// date, else Decimal if either is, else Currency if either is, else Integer. TRUE is 1; text is
    /// read as a number or a date. An Integer or Currency result out of range wraps around (see
    /// `wholeResult`).
    let add (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> = addOrSubtract "addition" false a b

    /// `-`. As `add`, except that a number minus a date is a Decimal (date - anything is a date).
    let subtract (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> = addOrSubtract "subtraction" true a b

    let private twoTo53 = 9007199254740992.0
    let private twoTo63 = 9223372036854775808.0

    /// The Currency result of a Currency multiplied or divided by a Decimal. `scaled` is the
    /// operation on the Currency's scaled integer, `plain` the operation on its value.
    ///
    /// "A multiplication between a currency and a floating point produces an overflow error if the
    /// result cannot be represented as a currency value"; "the result might be rounded to the
    /// closest value that can be represented in a currency data type": CONVERT ( 1.2345, CURRENCY )
    /// * CONVERT ( 0.1, DOUBLE ) is 0.1235 and CONVERT ( 1.2345, CURRENCY ) / CONVERT ( 2, DOUBLE )
    /// is 0.6173 (SQLBI, "Rounding errors with different data types in DAX"). Both are ties that
    /// round away from zero, and the second shows that it is not the float's exact binary value
    /// that is rounded (1.2345 / 2 as a float is below 0.61725). Whether the engine rounds the
    /// scaled float or the number as it reads in 15 digits is not said: where the two differ, and
    /// from 2^53 on (where a float no longer holds every scaled value, and the article's own large
    /// example follows from neither), the answer is `Unsettled`.
    let private currencyFromFloats (op: string) (c: int64) (scaled: float) (plain: float) =
        if Double.IsNaN scaled || abs scaled >= twoTo63 then
            overflow op DaxType.Currency
        elif abs (float c) >= twoTo53 || abs scaled >= twoTo53 then
            Error(
                Unsettled
                    $"{op} of a fixed decimal number and a decimal number where the fixed decimal number has more than 15 digits"
            )
        else
            let a = Math.Round(scaled, MidpointRounding.AwayFromZero)

            match decimalToScaled (decimal plain) with
            | Some b when float b = a && not (isNullSynonym b) -> Ok(DaxValue.Currency b)
            | _ ->
                Error(
                    Unsettled
                        $"{op} of a fixed decimal number and a decimal number whose result is within a rounding error of halfway between two fixed decimal numbers"
                )

    /// `*`. BLANK times anything is BLANK. Result type: Integer x Integer is Integer; Currency x
    /// Currency is Decimal; Currency with an Integer or a Decimal is Currency; the rest is Decimal
    /// (SQLBI, "Understanding numeric data type conversions in DAX", and the Multiplication table
    /// of Microsoft Learn's data types page, which agree).
    ///
    /// Overflow (SQLBI, "Rounding errors..."): Integer x Integer wraps around; Currency x Decimal
    /// is an error; Currency x Integer "never produces an overflow error, but it might result in an
    /// inaccurate number", which says too little to compute it: `Unsettled`.
    ///
    /// A date: the Microsoft table gives Integer x Date/time = INTEGER, Currency x Date/time = REAL
    /// and REAL x Date/time = REAL, and has no row for a date on the left. So a number times a date
    /// follows the table, except an Integer times a date with a time of day (the table makes the
    /// result an Integer, and nothing says how the fraction becomes one); a date times anything is
    /// `Unsettled`.
    let multiply (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        let op = "multiplication"

        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _)
        | Ok(_, ValueNone) -> Ok DaxValue.Blank
        | Ok(ValueSome x, ValueSome y) ->
            match x, y with
            | NDate _, _ ->
                Error(
                    Unsettled
                        "a date multiplied by a number (the documentation's table of result types has no row for a date on the left of *)"
                )
            | NInt i, NDate d ->
                if d = Math.Truncate d && abs d < twoTo53 then
                    wholeResult op (Int128.op_Implicit i * Int128.op_Implicit (int64 d))
                    |> Result.map DaxValue.Integer
                else
                    Error(
                        Unsettled
                            "a whole number multiplied by a date with a time of day (the result is a whole number; how the fraction of a day becomes one is not documented)"
                    )
            | (NCur _ | NReal _), NDate _ -> Ok(DaxValue.Decimal(numFloat x * numFloat y))
            | NInt l, NInt r when isNullSynonym l || isNullSynonym r -> atTheLimits op
            | NInt l, NInt r ->
                wholeResult op (Int128.op_Implicit l * Int128.op_Implicit r)
                |> Result.map DaxValue.Integer
            | NCur c, NInt i
            | NInt i, NCur c ->
                let wide = Int128.op_Implicit c * Int128.op_Implicit i

                if fitsInt64 wide then
                    wholeResult op wide |> Result.map DaxValue.Currency
                else
                    Error(
                        Unsettled
                            "a fixed decimal number times a whole number beyond the range of a fixed decimal number (no error, but \"an inaccurate number\": which one is not documented)"
                    )
            | NCur _, NCur _ -> Ok(DaxValue.Decimal(numFloat x * numFloat y))
            | NCur c, NReal r
            | NReal r, NCur c -> currencyFromFloats op c (float c * r) (float c / 10000.0 * r)
            | _ -> Ok(DaxValue.Decimal(numFloat x * numFloat y))

    /// `/`. BLANK / anything is BLANK; anything / BLANK divides by zero. Division by zero is not an
    /// error: 5/0 is Infinity, 0/0 is NaN (use DIVIDE to avoid them). Result type: "The result is
    /// always decimal, unless a currency is divided by an integer or by a decimal; in this case,
    /// the result is currency" (SQLBI, "Understanding numeric data type conversions in DAX").
    /// Microsoft's table has CURRENCY in the cell of an Integer divided by a Currency, but the
    /// sentence under it says "if a division operation combines an integer with a currency value,
    /// DAX converts both values to real numbers, and the result is also a real number", SQLBI's
    /// table says Decimal, and SQLBI publishes CONVERT ( 1.2345, DOUBLE ) / CONVERT ( 2, CURRENCY )
    /// as 0.61725, which no Currency holds: a Decimal.
    ///
    /// A Currency divided by zero is a Decimal infinity or NaN, since a Currency can't hold one.
    let divide (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        let op = "division"

        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _) -> Ok DaxValue.Blank
        | Ok(ValueSome x, y) ->
            let y = y |> ValueOption.defaultValue (NReal 0.0)

            match x, y with
            | NCur c, NInt i when i <> 0L ->
                // The scaled integer divided exactly, a half rounding away from zero ("CONVERT (
                // 1.2345, CURRENCY ) / CONVERT ( 2, INTEGER ) -- Result is 0.6173": SQLBI).
                let num = BigInteger c
                let den = BigInteger i
                let negative = (num.Sign < 0) <> (den.Sign < 0)

                let magnitude =
                    (BigInteger.Abs num * BigInteger 2 + BigInteger.Abs den)
                    / (BigInteger.Abs den * BigInteger 2)

                let exact = if negative then -magnitude else magnitude
                let viaFloat = Math.Round(float c / float i, MidpointRounding.AwayFromZero)

                if exact > int64Max || exact < int64Min then
                    overflow op DaxType.Currency
                elif float (int64 exact) = viaFloat && not (isNullSynonym (int64 exact)) then
                    Ok(DaxValue.Currency(int64 exact))
                else
                    Error(
                        Unsettled
                            "a fixed decimal number of more than 15 digits divided by a whole number (whether the division is exact or made on floats is not documented)"
                    )
            | NCur c, NReal r when r <> 0.0 && not (Double.IsNaN r) ->
                let q = float c / r

                if Double.IsInfinity q then
                    Ok(DaxValue.Decimal q)
                else
                    currencyFromFloats op c q (float c / 10000.0 / r)
            | _ -> Ok(DaxValue.Decimal(numFloat x / numFloat y))

    /// `^`. BLANK ^ anything is BLANK; anything ^ BLANK is 1 (the exponent is 0). The result is a
    /// Decimal: `^` "is syntactic sugar for the POWER function, which is used underneath"
    /// (dax.guide, Exponentiation), and POWER returns "A decimal number" (Microsoft Learn, POWER).
    /// What the pages don't say is `Unsettled`: zero to the power of zero, and a result that is
    /// not a finite number (a negative base with a fractional exponent, zero to a negative power,
    /// a result too large).
    let power (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _) -> Ok DaxValue.Blank
        | Ok(ValueSome x, y) ->
            let x = numFloat x
            let y = y |> ValueOption.map numFloat |> ValueOption.defaultValue 0.0
            let r = Math.Pow(x, y)

            if Double.IsNaN x || Double.IsInfinity x || Double.IsNaN y || Double.IsInfinity y then
                Ok(DaxValue.Decimal r)
            elif x = 0.0 && y = 0.0 then
                Error(Unsettled "zero to the power of zero")
            elif Double.IsNaN r || Double.IsInfinity r then
                Error(
                    Unsettled
                        "a power whose result is not a finite number (a negative base with a fractional exponent, zero to a negative power, or a result too large)"
                )
            else
                Ok(DaxValue.Decimal r)

    /// Unary `-`. Keeps the operand's type ("this operator doesn't change the data type"); BLANK
    /// stays BLANK; TRUE becomes -1.
    let negate (a: DaxValue) : Result<DaxValue, DaxError> =
        match toNum DaxType.Decimal a with
        | Error e -> Error e
        | Ok ValueNone -> Ok DaxValue.Blank
        | Ok(ValueSome n) ->
            match n with
            | NInt i when isNullSynonym i -> atTheLimits "negation"
            | NInt i -> wholeResult "negation" (-(Int128.op_Implicit i)) |> Result.map DaxValue.Integer
            | NCur c -> wholeResult "negation" (-(Int128.op_Implicit c)) |> Result.map DaxValue.Currency
            | NReal r -> Ok(DaxValue.Decimal(-r))
            | NDate d -> Ok(DaxValue.DateTime(-d))

    /// `&`. Both sides as text (see `toText`); a BLANK side is "". Two BLANKs give BLANK: the `&`
    /// operator "works as the CONCATENATE function" (dax.guide, Concatenation), for which "A BLANK
    /// argument is an empty string. If both arguments are BLANK, the result is BLANK"
    /// (dax.guide/concatenate).
    let concat (a: DaxValue) (b: DaxValue) : DaxValue =
        match a, b with
        | DaxValue.Blank, DaxValue.Blank -> DaxValue.Blank
        | _ -> DaxValue.String(toText a + toText b)

    // ---- Conversion -------------------------------------------------------------------------------

    /// A value as a condition (IF, AND, FILTER): BLANK is FALSE, a number is TRUE unless 0, text
    /// must read "true" or "false".
    let toBoolean (v: DaxValue) : Result<bool, DaxError> =
        match v with
        | DaxValue.Blank -> Ok false
        | DaxValue.Boolean b -> Ok b
        | DaxValue.Integer i -> Ok(i <> 0L)
        | DaxValue.Currency c -> Ok(c <> 0L)
        | DaxValue.Decimal d
        | DaxValue.DateTime d -> Ok(d <> 0.0)
        | DaxValue.String s ->
            match s.Trim().ToUpperInvariant() with
            | "TRUE" -> Ok true
            | "FALSE" -> Ok false
            | _ -> Error(CannotConvert(s, DaxType.String, DaxType.Boolean))

    /// What `&&` and `||` refuse: Microsoft's table of BLANK in operations gives BLANK for BLANK AND
    /// BLANK and BLANK OR BLANK, SQLBI says a logical expression is never BLANK.
    let bothBlank =
        "&& or || with BLANK on both sides (BLANK according to Microsoft's table of BLANK in operations, FALSE according to SQLBI)"

    /// `&&`. ASSUMPTION: always TRUE or FALSE, BLANK counting as FALSE (the current engine, per
    /// SQLBI; the docs' table still shows BLANK AND BLANK = BLANK, so that one case is `Unsettled`).
    let logicalAnd (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match toBoolean a, toBoolean b with
        | _ when isBlank a && isBlank b -> Error(Unsettled bothBlank)
        | Ok x, Ok y -> Ok(DaxValue.Boolean(x && y))
        | Error e, _
        | _, Error e -> Error e

    /// `||`. See `logicalAnd`.
    let logicalOr (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match toBoolean a, toBoolean b with
        | _ when isBlank a && isBlank b -> Error(Unsettled bothBlank)
        | Ok x, Ok y -> Ok(DaxValue.Boolean(x || y))
        | Error e, _
        | _, Error e -> Error e

    /// NOT. NOT of BLANK is `Unsettled`: TRUE if the BLANK counts as FALSE, as in a condition;
    /// BLANK if it passes through, as in arithmetic. No source shows it.
    let logicalNot (a: DaxValue) : Result<DaxValue, DaxError> =
        if isBlank a then
            Error(Unsettled "NOT of BLANK (TRUE or BLANK: no source shows which)")
        else
            toBoolean a |> Result.map (fun x -> DaxValue.Boolean(not x))

    /// What storing a value in a typed column does, and the conversions behind CONVERT (whose own
    /// rules, such as BLANK to BOOLEAN, are the CONVERT function's).
    ///
    /// - BLANK converts to BLANK; converting to Variant changes nothing.
    /// - To Integer: rounds, a half away from zero. To Currency: rounds to four decimals, a half
    ///   away from zero (see `roundFloat` for the sources and for what is `Unsettled`).
    /// - To Boolean: a number is TRUE unless 0; text must read "true" or "false".
    /// - To DateTime: a number is the serial; text is read as a date (see `parseIsoDateTime`).
    /// - From text to a number: digits, sign, decimal point and exponent, in the invariant culture.
    ///   Text with thousands separators or a currency symbol is `Unsettled` (see `isCultureNumber`).
    /// - To String: see `toText`.
    let convertTo (target: DaxType) (v: DaxValue) : Result<DaxValue, DaxError> =
        let fail () =
            match typeOf v with
            | Some source -> Error(CannotConvert(toText v, source, target))
            | None -> Ok DaxValue.Blank

        let number (s: string) =
            match parseNumber s with
            | Some n -> Ok n
            | None when isCultureNumber s -> Error(cultureNumber s)
            | None -> Error(CannotConvert(s, DaxType.String, target))

        let toInteger (d: float) =
            roundFloat "the conversion" DaxType.Integer 1 d |> Result.map DaxValue.Integer

        let toCurrency (d: float) =
            roundFloat "the conversion" DaxType.Currency 10000 d
            |> Result.map DaxValue.Currency

        match target, v with
        | _, DaxValue.Blank -> Ok DaxValue.Blank
        | DaxType.Variant, _ -> Ok v
        | DaxType.String, _ -> Ok(DaxValue.String(toText v))
        | DaxType.Integer, DaxValue.Integer _ -> Ok v
        | DaxType.Integer, DaxValue.Boolean b -> Ok(DaxValue.Integer(if b then 1L else 0L))
        | DaxType.Integer, DaxValue.Currency c ->
            Ok(DaxValue.Integer(Decimal.ToInt64(Math.Round(decimal c / 10000m, 0, MidpointRounding.AwayFromZero))))
        | DaxType.Integer, (DaxValue.Decimal d | DaxValue.DateTime d) -> toInteger d
        | DaxType.Integer, DaxValue.String s ->
            match number s with
            | Ok(DaxValue.Decimal d) -> toInteger d
            | other -> other
        | DaxType.Decimal, DaxValue.Decimal _ -> Ok v
        | DaxType.Decimal, DaxValue.Integer i -> Ok(DaxValue.Decimal(float i))
        | DaxType.Decimal, DaxValue.Currency c -> Ok(DaxValue.Decimal(float c / 10000.0))
        | DaxType.Decimal, DaxValue.DateTime d -> Ok(DaxValue.Decimal d)
        | DaxType.Decimal, DaxValue.Boolean b -> Ok(DaxValue.Decimal(if b then 1.0 else 0.0))
        | DaxType.Decimal, DaxValue.String s ->
            match number s with
            | Ok(DaxValue.Integer i) -> Ok(DaxValue.Decimal(float i))
            | other -> other
        | DaxType.Currency, DaxValue.Currency _ -> Ok v
        | DaxType.Currency, DaxValue.Integer i ->
            match scaledOfInt i with
            | Some s -> Ok(DaxValue.Currency s)
            | None -> overflow "the conversion" DaxType.Currency
        | DaxType.Currency, (DaxValue.Decimal d | DaxValue.DateTime d) -> toCurrency d
        | DaxType.Currency, DaxValue.Boolean b -> Ok(DaxValue.Currency(if b then 10000L else 0L))
        | DaxType.Currency, DaxValue.String s ->
            // Through System.Decimal where it reads as one, so "0.00005" isn't rounded twice.
            match Decimal.TryParse(s.Trim(), NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint, inv) with
            | true, d -> currency d
            | _ ->
                match number s with
                | Ok(DaxValue.Integer i) ->
                    match scaledOfInt i with
                    | Some x -> Ok(DaxValue.Currency x)
                    | None -> overflow "the conversion" DaxType.Currency
                | Ok(DaxValue.Decimal d) -> toCurrency d
                | other -> other
        | DaxType.Boolean, _ -> toBoolean v |> Result.map DaxValue.Boolean
        | DaxType.DateTime, DaxValue.DateTime _ -> Ok v
        | DaxType.DateTime, DaxValue.Integer i -> Ok(DaxValue.DateTime(float i))
        | DaxType.DateTime, DaxValue.Decimal d -> Ok(DaxValue.DateTime d)
        | DaxType.DateTime, DaxValue.Currency c -> Ok(DaxValue.DateTime(float c / 10000.0))
        | DaxType.DateTime, DaxValue.Boolean b -> Ok(DaxValue.DateTime(if b then 1.0 else 0.0))
        | DaxType.DateTime, DaxValue.String s ->
            match parseDate s with
            | Some d -> Ok d
            | None -> fail ()
