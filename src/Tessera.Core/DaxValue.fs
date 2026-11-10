namespace Tessera

open System
open System.Globalization
open System.Numerics

// DAX scalar values and their semantics, for the local DAX engine.
//
// Sources: Microsoft Learn "Data types in Power BI Desktop" (the implicit-conversion tables, the BLANK
// table, case and trailing-space handling), "DAX operators" (comparison rules, `==`), the CONVERT,
// CURRENCY and MAX function pages, and SQLBI "Understanding numeric data type conversions in DAX" and
// "Handling BLANK in DAX". Where those disagree or say nothing, the choice made here is marked
// ASSUMPTION next to the one function that implements it.

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

    /// Round half away from zero to a whole int64; None when not finite or out of range.
    let private roundToInt64 (x: float) =
        if Double.IsNaN x || Double.IsInfinity x then
            None
        else
            let r = Math.Round(x, MidpointRounding.AwayFromZero)
            // 2^63 is exactly representable; int64 r is defined for [-2^63, 2^63).
            if r >= 9223372036854775808.0 || r < -9223372036854775808.0 then
                None
            else
                Some(int64 r)

    let private decimalToScaled (d: decimal) =
        // ASSUMPTION: rounding to the fourth decimal is half away from zero. The CURRENCY page says
        // the fifth decimal "rounds up ... if equal or larger than 5"; the data types page says
        // "truncating". Rounding is what CURRENCY() and CONVERT() are documented to do.
        if d > 922337203685477.5807m || d < -922337203685477.5808m then
            None
        else
            let r = Math.Round(d * 10000m, 0, MidpointRounding.AwayFromZero)

            if r > 9223372036854775807m || r < -9223372036854775808m then
                None
            else
                Some(Decimal.ToInt64 r)

    let private floatToScaled (x: float) =
        if Double.IsNaN x || Double.IsInfinity x || abs x >= 1e15 then
            None
        else
            // Through System.Decimal, so 1.00005 rounds as the decimal number it prints as.
            decimalToScaled (decimal x)

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
            match parseNumber s with
            | Some(DaxValue.Integer i) -> Ok(ValueSome(NInt i))
            | Some(DaxValue.Decimal d) -> Ok(ValueSome(NReal d))
            | _ ->
                match parseDate s with
                | Some(DaxValue.DateTime d) -> Ok(ValueSome(NDate d))
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

    /// The order the comparison operators (`<`, `<=`, `>`, `>=`, and `=`/`<>` through `equals`) see.
    ///
    /// - BLANK is the other side's zero: 0, "", FALSE or 1899-12-30. BLANK equals BLANK.
    /// - Integer, Decimal, Currency and DateTime compare as numbers, whatever the mix.
    /// - Text compares with the collation (case-insensitive by default), ignoring trailing spaces.
    /// - FALSE is less than TRUE.
    /// - Any other mix is an error, as in the current engine ("... do not support comparing values
    ///   of type Text with values of type Integer"). The data types page still describes the older
    ///   behaviour, where Boolean > text > number; that ranking is what `sortCompare` uses.
    ///
    /// ASSUMPTION: numbers compare exactly (an Integer above 2^53 is not rounded to a float first).
    /// Not transitive across BLANK: BLANK = 0 and BLANK = "", yet 0 and "" don't compare.
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
            | ValueSome x, ValueSome y -> Ok(compareNum x y)
            | _ ->
                match typeOf a, typeOf b with
                | Some l, Some r -> Error(IncomparableTypes(l, r))
                // Unreachable: BLANKs were replaced above.
                | _ -> Ok 0

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

    let private checkedInt op (wide: Int128) =
        if
            wide > Int128.op_Implicit Int64.MaxValue
            || wide < Int128.op_Implicit Int64.MinValue
        then
            overflow op DaxType.Integer
        else
            Ok(DaxValue.Integer(Int128.op_Explicit wide: int64))

    let private checkedCurrency op (wide: Int128) =
        if
            wide > Int128.op_Implicit Int64.MaxValue
            || wide < Int128.op_Implicit Int64.MinValue
        then
            overflow op DaxType.Currency
        else
            Ok(DaxValue.Currency(Int128.op_Explicit wide: int64))

    let private currencyOfFloat op (x: float) =
        match floatToScaled x with
        | Some s -> Ok(DaxValue.Currency s)
        | None -> overflow op DaxType.Currency

    /// A number's scaled-integer form, for Currency arithmetic with an Integer.
    let private wideScaled =
        function
        | NInt i -> Int128.op_Implicit i * Int128.op_Implicit 10000L
        | NCur c -> Int128.op_Implicit c
        | NReal _
        | NDate _ -> Int128.Zero

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

            match resultType with
            | DaxType.DateTime -> Ok(DaxValue.DateTime f)
            | DaxType.Decimal -> Ok(DaxValue.Decimal f)
            | DaxType.Currency ->
                let l, r = wideScaled x, wideScaled y
                checkedCurrency op (if subtract then l - r else l + r)
            | _ ->
                match x, y with
                | NInt l, NInt r ->
                    let l, r = Int128.op_Implicit l, Int128.op_Implicit r
                    // ASSUMPTION: Integer overflow is an error rather than wrapping or widening.
                    checkedInt op (if subtract then l - r else l + r)
                | _ -> Ok(DaxValue.Decimal f)

    /// `+`. BLANK + BLANK is BLANK; otherwise BLANK is 0. Result type: DateTime if either side is a
    /// date, else Decimal if either is, else Currency if either is, else Integer. TRUE is 1; text is
    /// read as a number or a date.
    let add (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> = addOrSubtract "addition" false a b

    /// `-`. As `add`, except that a number minus a date is a Decimal (date - anything is a date).
    let subtract (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> = addOrSubtract "subtraction" true a b

    /// `*`. BLANK times anything is BLANK. Result type: Integer x Integer is Integer; Currency x
    /// Currency is Decimal; Currency with anything else is Currency; the rest is Decimal.
    /// ASSUMPTION: a date multiplies as a Decimal (the docs' table gives Integer x Date = Integer and
    /// has no Date row; a serial with a time of day can't be an Integer).
    let multiply (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        let op = "multiplication"

        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _)
        | Ok(_, ValueNone) -> Ok DaxValue.Blank
        | Ok(ValueSome x, ValueSome y) ->
            match x, y with
            | NInt l, NInt r -> checkedInt op (Int128.op_Implicit l * Int128.op_Implicit r)
            | NCur c, NInt i
            | NInt i, NCur c -> checkedCurrency op (Int128.op_Implicit c * Int128.op_Implicit i)
            | NCur _, NCur _ -> Ok(DaxValue.Decimal(numFloat x * numFloat y))
            | NCur _, _
            | _, NCur _ -> currencyOfFloat op (numFloat x * numFloat y)
            | _ -> Ok(DaxValue.Decimal(numFloat x * numFloat y))

    /// `/`. BLANK / anything is BLANK; anything / BLANK divides by zero. Division by zero is not an
    /// error: 5/0 is Infinity, 0/0 is NaN (use DIVIDE to avoid them). Result type: Decimal, except
    /// Currency / Integer and Currency / Decimal, which are Currency.
    /// ASSUMPTION: where the docs' table and SQLBI disagree (Integer / Currency) this follows SQLBI
    /// and the docs' own example sentence: Decimal. A Currency division by zero is a Decimal
    /// infinity or NaN, since a Currency can't hold one.
    let divide (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        let op = "division"

        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _) -> Ok DaxValue.Blank
        | Ok(ValueSome x, y) ->
            let y = y |> ValueOption.defaultValue (NReal 0.0)

            match x, y with
            | NCur c, NInt i when i <> 0L ->
                match decimalToScaled (currencyValue c / decimal i) with
                | Some s -> Ok(DaxValue.Currency s)
                | None -> overflow op DaxType.Currency
            | NCur _, NReal r when r <> 0.0 && not (Double.IsNaN r) ->
                let q = numFloat x / r

                if Double.IsInfinity q then
                    Ok(DaxValue.Decimal q)
                else
                    currencyOfFloat op q
            | _ -> Ok(DaxValue.Decimal(numFloat x / numFloat y))

    /// `^`. BLANK ^ anything is BLANK; anything ^ BLANK is 1 (the exponent is 0).
    /// ASSUMPTION: the result is always a Decimal (the docs give no table for `^`).
    let power (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match operands a b with
        | Error e -> Error e
        | Ok(ValueNone, _) -> Ok DaxValue.Blank
        | Ok(ValueSome x, y) ->
            let y = y |> ValueOption.map numFloat |> ValueOption.defaultValue 0.0
            Ok(DaxValue.Decimal(Math.Pow(numFloat x, y)))

    /// Unary `-`. Keeps the operand's type ("this operator doesn't change the data type"); BLANK
    /// stays BLANK; TRUE becomes -1.
    let negate (a: DaxValue) : Result<DaxValue, DaxError> =
        match toNum DaxType.Decimal a with
        | Error e -> Error e
        | Ok ValueNone -> Ok DaxValue.Blank
        | Ok(ValueSome n) ->
            match n with
            | NInt i -> checkedInt "negation" (-(Int128.op_Implicit i))
            | NCur c -> checkedCurrency "negation" (-(Int128.op_Implicit c))
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

    /// `&&`. ASSUMPTION: always TRUE or FALSE, BLANK counting as FALSE (the current engine, per
    /// SQLBI; the docs' table still shows BLANK AND BLANK = BLANK).
    let logicalAnd (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match toBoolean a, toBoolean b with
        | Ok x, Ok y -> Ok(DaxValue.Boolean(x && y))
        | Error e, _
        | _, Error e -> Error e

    /// `||`. See `logicalAnd`.
    let logicalOr (a: DaxValue) (b: DaxValue) : Result<DaxValue, DaxError> =
        match toBoolean a, toBoolean b with
        | Ok x, Ok y -> Ok(DaxValue.Boolean(x || y))
        | Error e, _
        | _, Error e -> Error e

    /// NOT. NOT BLANK is TRUE.
    let logicalNot (a: DaxValue) : Result<DaxValue, DaxError> =
        toBoolean a |> Result.map (fun x -> DaxValue.Boolean(not x))

    /// CONVERT(value, type), and what storing a value in a typed column does.
    ///
    /// - BLANK converts to BLANK; converting to Variant changes nothing.
    /// - To Integer: rounds (ASSUMPTION: half away from zero). To Currency: rounds to four decimals.
    /// - To Boolean: a number is TRUE unless 0; text must read "true" or "false".
    /// - To DateTime: a number is the serial; text is read as a date (see `parseIsoDateTime`).
    /// - From text to a number: digits, sign, decimal point and exponent, in the invariant culture.
    ///   ASSUMPTION: no thousands separators or currency symbols, which the engine reads per culture.
    /// - To String: see `toText`.
    let convertTo (target: DaxType) (v: DaxValue) : Result<DaxValue, DaxError> =
        let fail () =
            match typeOf v with
            | Some source -> Error(CannotConvert(toText v, source, target))
            | None -> Ok DaxValue.Blank

        let number (s: string) =
            match parseNumber s with
            | Some n -> Ok n
            | None -> Error(CannotConvert(s, DaxType.String, target))

        match target, v with
        | _, DaxValue.Blank -> Ok DaxValue.Blank
        | DaxType.Variant, _ -> Ok v
        | DaxType.String, _ -> Ok(DaxValue.String(toText v))
        | DaxType.Integer, DaxValue.Integer _ -> Ok v
        | DaxType.Integer, DaxValue.Boolean b -> Ok(DaxValue.Integer(if b then 1L else 0L))
        | DaxType.Integer, DaxValue.Currency c ->
            Ok(DaxValue.Integer(Decimal.ToInt64(Math.Round(decimal c / 10000m, 0, MidpointRounding.AwayFromZero))))
        | DaxType.Integer, (DaxValue.Decimal d | DaxValue.DateTime d) ->
            match roundToInt64 d with
            | Some i -> Ok(DaxValue.Integer i)
            | None -> overflow "the conversion" DaxType.Integer
        | DaxType.Integer, DaxValue.String s ->
            match number s with
            | Ok(DaxValue.Decimal d) ->
                match roundToInt64 d with
                | Some i -> Ok(DaxValue.Integer i)
                | None -> overflow "the conversion" DaxType.Integer
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
        | DaxType.Currency, (DaxValue.Decimal d | DaxValue.DateTime d) -> currencyOfFloat "the conversion" d
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
                | Ok(DaxValue.Decimal d) -> currencyOfFloat "the conversion" d
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
