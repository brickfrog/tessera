namespace Tessera.DaxEngine.Functions

// Math functions. (DIVIDE is in Logical.fs; CONVERT, CURRENCY and VALUE in Conversion.fs.)
//
// Sources: the function pages of dax.guide and of the DAX reference on Microsoft Learn, quoted at
// each function. Three things hold throughout:
//
// - An argument that is text is read as a number when it is one (`Coerce.number`); a date or a
//   Boolean argument is refused unless the function's page says what it does with one; so is a
//   BLANK argument (only INT and CEILING document theirs).
// - A decimal number is a binary float, and the pages give results for numbers as they are
//   written: ROUND ( 2.15, 1 ) is 2.2 although the float 2.15 is below the tie. No page says how:
//   the number as it reads in 15 significant digits, and the float scaled by the power of ten (or
//   divided by the multiple), both give the published results. Each rounding function computes
//   both and refuses where they differ (a number within a rounding error of a boundary, such as
//   0.1 + 0.2 for ROUNDUP to one decimal).
// - Where the two sites give different result types, the function says which it follows.

open System
open System.Globalization
open System.Numerics
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Math =

    let private inv = CultureInfo.InvariantCulture
    let private twoTo53 = 9007199254740992.0

    let private arg (ev: IEval) (ctx: Ctx) (call: Call) (i: int) : DaxValue =
        Coerce.number ctx call.Args.[i].Span (call.Name.Name.ToUpperInvariant()) (ev.Scalar(ctx, call.Args.[i]))

    let private argNotBlank (ev: IEval) (ctx: Ctx) (call: Call) (i: int) : DaxValue =
        Coerce.numberNotBlank ctx call.Args.[i].Span (call.Name.Name.ToUpperInvariant()) (ev.Scalar(ctx, call.Args.[i]))

    /// A finite float argument.
    let private floatArg (ev: IEval) (ctx: Ctx) (call: Call) (i: int) : float =
        let x = Coerce.toFloat (argNotBlank ev ctx call i)

        if Double.IsNaN x || Double.IsInfinity x then
            Fail.notSupported
                ctx
                call.Args.[i].Span
                $"{call.Name.Name.ToUpperInvariant()} of a number that is not finite"

        x

    /// A finite result, or a refusal: the pages don't say what a result too large is.
    let private finite (ctx: Ctx) (call: Call) (r: float) : DaxValue =
        if Double.IsNaN r || Double.IsInfinity r then
            Fail.notSupported ctx call.Span $"{call.Name.Name.ToUpperInvariant()} whose result is not a finite number"

        DaxValue.Decimal r

    /// The number as it reads in decimal: exactly for an Integer or a Currency, in 15 significant
    /// digits for a Decimal.
    let private reading (ctx: Ctx) (span: Span) (name: string) (v: DaxValue) : decimal =
        match v with
        | DaxValue.Integer i -> decimal i
        | DaxValue.Currency c -> DaxValue.currencyValue c
        | DaxValue.Decimal d when not (Double.IsNaN d) && abs d < 1e15 -> decimal d
        | _ ->
            Fail.notSupported
                ctx
                span
                $"{name} of a number that is not finite or has more than 15 digits before the decimal point"

    /// The nearest float to a decimal number.
    let private ofDecimal (d: decimal) : float = Double.Parse(d.ToString inv, inv)

    let private isFloat (v: DaxValue) =
        match v with
        | DaxValue.Decimal _ -> true
        | _ -> false

    [<RequireQualifiedAccess>]
    type private Mode =
        /// To the nearest, a half away from zero.
        | Nearest
        /// Away from zero.
        | Up
        /// Toward zero.
        | Down
        /// Toward positive infinity.
        | Ceiling
        /// Toward negative infinity.
        | Floor

    let private roundDecimal (mode: Mode) (q: decimal) : decimal =
        match mode with
        | Mode.Nearest -> Math.Round(q, 0, MidpointRounding.AwayFromZero)
        | Mode.Up -> if q >= 0m then Math.Ceiling q else Math.Floor q
        | Mode.Down -> Math.Truncate q
        | Mode.Ceiling -> Math.Ceiling q
        | Mode.Floor -> Math.Floor q

    let private roundFloat (mode: Mode) (q: float) : float =
        match mode with
        | Mode.Nearest -> Math.Round(q, MidpointRounding.AwayFromZero)
        | Mode.Up -> if q >= 0.0 then Math.Ceiling q else Math.Floor q
        | Mode.Down -> Math.Truncate q
        | Mode.Ceiling -> Math.Ceiling q
        | Mode.Floor -> Math.Floor q

    let private nearBoundary (ctx: Ctx) (span: Span) (name: string) : 'a =
        Fail.notSupported
            ctx
            span
            $"{name} of a decimal number within a rounding error of a boundary between two results (whether the binary value or the number as written is rounded is not documented)"

    /// ROUND, ROUNDUP, ROUNDDOWN and TRUNC: the number at a number of decimal digits.
    let private atDigits
        (ctx: Ctx)
        (call: Call)
        (name: string)
        (mode: Mode)
        (v: DaxValue)
        (digits: DaxValue)
        : DaxValue =
        let digits =
            match digits with
            | DaxValue.Integer i when i >= -15L && i <= 15L -> int i
            | _ ->
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name} to a number of digits that is not a whole number from -15 to 15"

        let d = reading ctx call.Span name v
        let power = pown 10m (abs digits)

        try
            let scaled = if digits >= 0 then d * power else d / power
            let whole = roundDecimal mode scaled

            match v with
            | DaxValue.Decimal x ->
                let viaFloat =
                    roundFloat mode (if digits >= 0 then x * float power else x / float power)

                if viaFloat <> float whole then
                    nearBoundary ctx call.Span name
            | _ -> ()

            if abs whole >= 9007199254740992m then
                Fail.notSupported ctx call.Span $"{name} of a number with more than 15 digits"

            DaxValue.Decimal(
                if digits >= 0 then
                    float whole / float power
                else
                    float whole * float power
            )
        with :? OverflowException ->
            Fail.notSupported ctx call.Span $"{name} of a number with more than 28 digits at that number of decimals"

    /// How many times the multiple goes into the number, rounded: what MROUND, CEILING, FLOOR and
    /// ISO.CEILING share. Returns the count and the multiple, as decimal numbers.
    let private countOfMultiple
        (ctx: Ctx)
        (call: Call)
        (name: string)
        (mode: Mode)
        (number: DaxValue)
        (multiple: DaxValue)
        : decimal * decimal =
        let n = reading ctx call.Args.[0].Span name number
        let m = reading ctx call.Span name multiple

        try
            let q = n / m
            let count = roundDecimal mode q

            if isFloat number || isFloat multiple then
                let viaFloat = roundFloat mode (Coerce.toFloat number / Coerce.toFloat multiple)

                if viaFloat <> float count then
                    nearBoundary ctx call.Span name

            count, m
        with :? OverflowException ->
            Fail.notSupported ctx call.Span $"{name} of numbers too far apart in size"

    /// A whole decimal number as an Integer.
    let private integerOf (ctx: Ctx) (call: Call) (name: string) (d: decimal) : DaxValue =
        if d <> Math.Truncate d || abs d >= 9223372036854775807m then
            Fail.notSupported ctx call.Span $"{name} whose result is not a whole number in the range of one"

        DaxValue.Integer(Decimal.ToInt64 d)

    /// A decimal number as a Currency, when it is one exactly.
    let private currencyOf (ctx: Ctx) (call: Call) (name: string) (d: decimal) : DaxValue =
        match DaxValue.tryCurrency d with
        | Some(DaxValue.Currency c) when DaxValue.currencyValue c = d -> DaxValue.Currency c
        | _ ->
            Fail.notSupported
                ctx
                call.Span
                $"{name} whose result has more than four decimals, as a fixed decimal number"

    let private sign (v: DaxValue) : int =
        match v with
        | DaxValue.Integer i -> sign i
        | DaxValue.Currency c -> sign c
        | DaxValue.Decimal d -> sign d
        | _ -> 0

    /// The result type CEILING documents: "The return data type is usually the same as the data
    /// type of the Significance argument, with these exceptions: If the number argument type is
    /// Currency, the return type is Currency. ... If the significance argument type is
    /// non-numeric, the return type is Decimal" (dax.guide/ceiling).
    let private ceilingResult
        (ctx: Ctx)
        (call: Call)
        (name: string)
        (number: DaxValue)
        (significance: DaxValue)
        (significanceWasText: bool)
        (result: decimal)
        : DaxValue =
        match number, significance with
        | DaxValue.Currency _, _ -> currencyOf ctx call name result
        | _ when significanceWasText -> DaxValue.Decimal(ofDecimal result)
        | _, DaxValue.Currency _ -> currencyOf ctx call name result
        | _, DaxValue.Integer _ -> integerOf ctx call name result
        | _ -> DaxValue.Decimal(ofDecimal result)

    let private isText (v: DaxValue) =
        match v with
        | DaxValue.String _ -> true
        | _ -> false

    /// A whole number below 2^53 from an argument of GCD or LCM: "If a value is not an integer, it
    /// is truncated"; "If any argument is less than zero, GCD returns the #NUM! error value"; "If
    /// a parameter to GCD is >=2^53, GCD returns the #NUM! error value" (Microsoft Learn, GCD).
    let private wholeArg (ev: IEval) (ctx: Ctx) (call: Call) (i: int) : BigInteger =
        let name = call.Name.Name.ToUpperInvariant()

        let truncated =
            match argNotBlank ev ctx call i with
            | DaxValue.Integer n -> float n
            | DaxValue.Currency c -> Math.Truncate(float (c / 10000L))
            | v ->
                let x = Coerce.toFloat v

                if Double.IsNaN x then
                    Fail.notSupported ctx call.Args.[i].Span $"{name} of a number that is not finite"

                Math.Truncate x

        if truncated < 0.0 then
            Fail.runtime
                ctx
                call.Args.[i].Span
                $"An argument of function '{name}' has the wrong data type or has an invalid value."

        if truncated >= twoTo53 then
            Fail.runtime
                ctx
                call.Args.[i].Span
                $"An argument of function '{name}' has the wrong data type or has an invalid value."

        BigInteger truncated

    let private unseeded (name: string) =
        Fn.withCheck (fun _ -> Some $"{name}: its result is random, so no answer of this engine could be the server's")

    let functions: DaxFunction list = [
        // "The absolute value of a number with the same data type and without its sign"; a result
        // of "one these types: currency, decimal, integer" (dax.guide/abs). Microsoft Learn says
        // "A decimal number" for every argument; this follows dax.guide, which is the more
        // specific.
        Fn.scalar
            "ABS"
            "Math"
            (1, 1)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Known(DaxType.Integer | DaxType.Decimal | DaxType.Currency) as t -> t
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                match argNotBlank ev ctx call 0 with
                | DaxValue.Integer i when i < 0L -> Fail.dax ctx call.Span (DaxValue.negate (DaxValue.Integer i))
                | DaxValue.Currency c when c < 0L -> Fail.dax ctx call.Span (DaxValue.negate (DaxValue.Currency c))
                | DaxValue.Decimal d -> DaxValue.Decimal(abs d)
                | v -> v)

        // "A whole number. The possible Return values are 1, 0, and -1" (Microsoft Learn, SIGN);
        // "It does not work with infinite numbers" (dax.guide/sign).
        Fn.scalar "SIGN" "Math" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            match argNotBlank ev ctx call 0 with
            | DaxValue.Decimal d when Double.IsNaN d || Double.IsInfinity d ->
                Fail.notSupported ctx call.Span "SIGN of a number that is not finite"
            | v -> DaxValue.Integer(int64 (sign v)))

        // "A decimal number"; ties "are broken by rounding half away from zero (also known as
        // commercial rounding)": ROUND(1.15, 1) is 1.2 and ROUND(-1.15, 1) is -1.2; ROUND(21.5,
        // -1) is 20 (Microsoft Learn, ROUND).
        Fn.scalar "ROUND" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            atDigits ctx call "ROUND" Mode.Nearest (argNotBlank ev ctx call 0) (argNotBlank ev ctx call 1))

        // "ROUNDUP behaves like ROUND, except that it always rounds a number up": away from zero
        // ("To always round up (away from zero), use the ROUNDUP function": Microsoft Learn,
        // ROUND). ROUNDUP(PI(), 4) is 3.1416; ROUNDUP(13.25, -1) is 20.
        Fn.scalar "ROUNDUP" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            atDigits ctx call "ROUNDUP" Mode.Up (argNotBlank ev ctx call 0) (argNotBlank ev ctx call 1))

        // "To always round down (toward zero), use the ROUNDDOWN function" (Microsoft Learn,
        // ROUND). ROUNDDOWN(3.14159, 3) is 3.141; ROUNDDOWN(31415.92654, -2) is 31400.
        Fn.scalar "ROUNDDOWN" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            atDigits ctx call "ROUNDDOWN" Mode.Down (argNotBlank ev ctx call 0) (argNotBlank ev ctx call 1))

        // "TRUNC removes the fractional part of the number": TRUNC(-8.9) is -8; the number of
        // digits is "the precision of the truncation; if omitted, 0" (Microsoft Learn, TRUNC).
        // The result type: Microsoft Learn says "A whole number", dax.guide "A single decimal
        // value", and dax.guide/int that "Other rounding functions (such as TRUNC) return a
        // Decimal data type": a Decimal.
        Fn.scalar "TRUNC" "Math" (1, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let digits =
                match List.tryItem 1 call.Args with
                | Some d when not (Fn.isOmitted d) -> argNotBlank ev ctx call 1
                | _ -> DaxValue.Integer 0L

            atDigits ctx call "TRUNC" Mode.Down (argNotBlank ev ctx call 0) digits)

        // "INT rounds <Number> towards −∞"; "If <Number> is an Integer or a Decimal data type,
        // then the result is an Integer data type. If <Number> is a Currency, then the result is
        // also a Currency data type"; INT ( TRUE ) is 1, INT ( "1950.00" ) is 1950 and INT (
        // "1E6" ) is 1,000,000 (dax.guide/int). "INT ( BLANK () )" is a "Blank value" (SQLBI,
        // "Handling BLANK in DAX").
        Fn.scalar
            "INT"
            "Math"
            (1, 1)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Known DaxType.Currency -> StaticType.Known DaxType.Currency
                | StaticType.Known(DaxType.Integer | DaxType.Decimal | DaxType.Boolean | DaxType.String) ->
                    StaticType.Known DaxType.Integer
                | StaticType.Blank -> StaticType.Blank
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                let v =
                    match ev.Scalar(ctx, call.Args.[0]) with
                    | DaxValue.Boolean b -> DaxValue.Integer(if b then 1L else 0L)
                    | v -> Coerce.number ctx call.Args.[0].Span "INT" v

                match v with
                | DaxValue.Decimal d ->
                    let f = Math.Floor d

                    if Double.IsNaN f || f >= 9223372036854775808.0 || f <= -9223372036854775808.0 then
                        Fail.runtime ctx call.Span (DaxError.message (Overflow("INT", DaxType.Integer)))

                    DaxValue.Integer(int64 f)
                | DaxValue.Currency c ->
                    let whole = (if c >= 0L then c / 10000L else -((-c + 9999L) / 10000L))
                    DaxValue.Currency(whole * 10000L)
                | v -> v)

        // "MROUND rounds up, away from zero, if the remainder of dividing number by the specified
        // multiple is greater than or equal to half the value of multiple"; MROUND(-10, -3) is
        // -9; MROUND(5, -2) "returns an error, because the numbers have different signs"
        // (Microsoft Learn, MROUND). "when a decimal value is provided to the Multiple argument,
        // the rounding direction is undefined for midpoint numbers. For example, MROUND ( 6.05,
        // 0.1 ) returns 6.0 while MROUND ( 7.05, 0.1 ) returns 7.1" (dax.guide/mround): a
        // midpoint with a multiple that is not a whole number is refused. A multiple of zero is
        // not documented.
        Fn.scalar "MROUND" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let number = argNotBlank ev ctx call 0
            let multiple = argNotBlank ev ctx call 1

            match sign number, sign multiple with
            | _, 0 -> Fail.notSupported ctx call.Span "MROUND to a multiple of zero"
            | 0, 1 -> DaxValue.Decimal 0.0
            | 0, _ -> Fail.notSupported ctx call.Span "MROUND of zero to a negative multiple"
            | a, b when a <> b ->
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'MROUND' has the wrong data type or has an invalid value."
            | _ ->
                let n = reading ctx call.Args.[0].Span "MROUND" number
                let m = reading ctx call.Span "MROUND" multiple

                let midpoint =
                    try
                        let q = n / m
                        abs (q - Math.Truncate q) = 0.5m
                    with :? OverflowException ->
                        false

                if midpoint && m <> Math.Truncate m then
                    Fail.notSupported
                        ctx
                        call.Span
                        "MROUND of a number exactly halfway between two multiples of a multiple that is not a whole number (the direction is undefined)"

                let count, m = countOfMultiple ctx call "MROUND" Mode.Nearest number multiple
                DaxValue.Decimal(ofDecimal (count * m)))

        // "When using a positive multiple of significance, both CEILING and ISO.CEILING round
        // negative numbers upward (toward positive infinity). When using a negative multiple of
        // significance, CEILING rounds negative numbers downward (toward negative infinity)"
        // (Microsoft Learn, CEILING). "If Significance is 0 or BLANK, CEILING returns 0. If
        // Number is BLANK, CEILING returns BLANK. If Number is positive and Significance is
        // negative, CEILING returns an error" (dax.guide/ceiling). The type is `ceilingResult`'s.
        Fn.scalar "CEILING" "Math" (2, 2) Fn.typeUnknown (fun ev ctx call ->
            let rawSignificance = ev.Scalar(ctx, call.Args.[1])
            let number = arg ev ctx call 0
            let significance = Coerce.number ctx call.Args.[1].Span "CEILING" rawSignificance

            match number with
            | DaxValue.Blank -> DaxValue.Blank
            | _ ->
                if DaxValue.isBlank significance || sign significance = 0 then
                    // "returns 0": of which type is not said.
                    match number, significance with
                    | DaxValue.Currency _, _ -> DaxValue.Currency 0L
                    | _, DaxValue.Integer _ -> DaxValue.Integer 0L
                    | _, DaxValue.Decimal _ when not (isText rawSignificance) -> DaxValue.Decimal 0.0
                    | _ ->
                        Fail.notSupported
                            ctx
                            call.Span
                            "CEILING with a significance that is BLANK or text for zero (the type of the zero it returns is not documented)"
                elif sign number > 0 && sign significance < 0 then
                    Fail.runtime
                        ctx
                        call.Span
                        "An argument of function 'CEILING' has the wrong data type or has an invalid value."
                else
                    // The count rounds up for a positive significance (toward positive infinity)
                    // and for a negative one (the product is then toward negative infinity).
                    let count, m = countOfMultiple ctx call "CEILING" Mode.Ceiling number significance
                    ceilingResult ctx call "CEILING" number significance (isText rawSignificance) (count * m))

        // "ISO.CEILING rounds negative numbers upward (toward positive infinity)" whatever the
        // sign of the significance; "If the unit of significance is not specified, the number is
        // rounded up to the nearest integer"; "If the optional argument is not included the
        // result is of integer type" (Microsoft Learn, ISO.CEILING). With a significance, the
        // page says both "A number, of the same type as the number argument" and "The result type
        // is usually the same type of the significance": where those two differ the call is
        // refused. Nothing is said of BLANK or of a significance of zero.
        Fn.scalar "ISO.CEILING" "Math" (1, 2) Fn.typeUnknown (fun ev ctx call ->
            let number = argNotBlank ev ctx call 0

            match List.tryItem 1 call.Args with
            | Some s when not (Fn.isOmitted s) ->
                let raw = ev.Scalar(ctx, s)
                let significance = Coerce.numberNotBlank ctx s.Span "ISO.CEILING" raw

                if sign significance = 0 then
                    Fail.notSupported ctx call.Span "ISO.CEILING with a significance of zero"

                let sameType =
                    match number, significance with
                    | DaxValue.Currency _, _ -> true
                    | _ when isText raw -> false
                    | DaxValue.Integer _, DaxValue.Integer _
                    | DaxValue.Decimal _, DaxValue.Decimal _ -> true
                    | _ -> false

                if not sameType then
                    Fail.notSupported
                        ctx
                        call.Span
                        "ISO.CEILING of a number and a significance of different data types (the page gives the result both the number's type and the significance's)"

                let positive =
                    if sign significance < 0 then
                        Fail.dax ctx call.Span (DaxValue.negate significance)
                    else
                        significance

                let count, m = countOfMultiple ctx call "ISO.CEILING" Mode.Ceiling number positive
                ceilingResult ctx call "ISO.CEILING" number significance false (count * m)
            | _ ->
                let count, _ =
                    countOfMultiple ctx call "ISO.CEILING" Mode.Ceiling number (DaxValue.Integer 1L)

                match number with
                | DaxValue.Currency _ -> currencyOf ctx call "ISO.CEILING" count
                | _ -> integerOf ctx call "ISO.CEILING" count)

        // "The arguments number and significance must either both be positive, or both be
        // negative"; "If number and significance have different signs, FLOOR returns the #NUM!
        // error value"; "a value is rounded down when adjusted away from zero" [toward zero];
        // FLOOR(10.8423, .1) is 10.8 (Microsoft Learn, FLOOR). "The functions always returns a
        // decimal data type, regardless of the arguments" (dax.guide/floor). A significance of
        // zero is not documented.
        Fn.scalar "FLOOR" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let number = argNotBlank ev ctx call 0
            let significance = argNotBlank ev ctx call 1

            match sign number, sign significance with
            | _, 0 -> Fail.notSupported ctx call.Span "FLOOR with a significance of zero"
            | 0, _ -> DaxValue.Decimal 0.0
            | a, b when a <> b ->
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'FLOOR' has the wrong data type or has an invalid value."
            | _ ->
                // Same signs: the count is positive, and rounding it down is toward zero.
                let count, m = countOfMultiple ctx call "FLOOR" Mode.Floor number significance
                DaxValue.Decimal(ofDecimal (count * m)))

        // "The MOD function can be expressed in terms of the INT function: MOD(n, d) = n -
        // d*INT(n/d)"; "the sign is always the same as the sign of the divisor"; "If the divisor
        // is 0 (zero), MOD returns an error" (Microsoft Learn, MOD). The result type: Microsoft
        // Learn says "A whole number", dax.guide "A single value of any type". Whole numbers
        // give a whole number (both agree); decimal numbers give the identity's value as a
        // decimal number; a Currency is refused (the identity's own rounding of n/d to four
        // decimals could change the result, and nothing says whether the engine's does).
        Fn.scalar
            "MOD"
            "Math"
            (2, 2)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]), ev.TypeOf(ctx, args.[1]) with
                | StaticType.Known DaxType.Integer, StaticType.Known DaxType.Integer ->
                    StaticType.Known DaxType.Integer
                | StaticType.Known(DaxType.Integer | DaxType.Decimal),
                  StaticType.Known(DaxType.Integer | DaxType.Decimal) -> StaticType.Known DaxType.Decimal
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                match argNotBlank ev ctx call 0, argNotBlank ev ctx call 1 with
                | _, d when sign d = 0 && not (isFloat d && Double.IsNaN(Coerce.toFloat d)) ->
                    Fail.runtime ctx call.Span "MOD: division by zero."
                | DaxValue.Integer n, DaxValue.Integer d ->
                    if d = -1L then
                        DaxValue.Integer 0L
                    else
                        let r = n % d
                        DaxValue.Integer(if r <> 0L && ((r < 0L) <> (d < 0L)) then r + d else r)
                | (DaxValue.Currency _, _)
                | (_, DaxValue.Currency _) -> Fail.notSupported ctx call.Span "MOD of a fixed decimal number"
                | n, d ->
                    let n = Coerce.toFloat n
                    let d = Coerce.toFloat d

                    if Double.IsNaN n || Double.IsInfinity n || Double.IsNaN d || Double.IsInfinity d then
                        Fail.notSupported ctx call.Span "MOD of a number that is not finite"

                    let quotient = Math.Floor(n / d)

                    if abs quotient >= twoTo53 then
                        Fail.notSupported ctx call.Span "MOD of numbers whose quotient has more than 15 digits"

                    finite ctx call (n - d * quotient))

        // "Returns only the integer portion of the division result" (dax.guide/quotient): toward
        // zero. "A whole number"; QUOTIENT(10/2, 2) is 2; a zero denominator is an error ("if the
        // column that you reference contains a 0 (zero), an error is returned": Microsoft Learn,
        // QUOTIENT).
        Fn.scalar "QUOTIENT" "Math" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let n = argNotBlank ev ctx call 0
            let d = argNotBlank ev ctx call 1

            if sign d = 0 && not (isFloat d && Double.IsNaN(Coerce.toFloat d)) then
                Fail.runtime ctx call.Span "QUOTIENT: division by zero."

            let scaled (v: DaxValue) =
                match v with
                | DaxValue.Integer i -> ValueSome(BigInteger i * BigInteger 10000)
                | DaxValue.Currency c -> ValueSome(BigInteger c)
                | _ -> ValueNone

            let whole =
                match scaled n, scaled d with
                | ValueSome a, ValueSome b -> BigInteger.Divide(a, b)
                | _ ->
                    let q = Math.Truncate(Coerce.toFloat n / Coerce.toFloat d)

                    if Double.IsNaN q || abs q >= twoTo53 then
                        Fail.notSupported
                            ctx
                            call.Span
                            "QUOTIENT of decimal numbers whose quotient is not finite or has more than 15 digits"

                    BigInteger q

            if whole >= BigInteger Int64.MaxValue || whole <= BigInteger Int64.MinValue then
                Fail.notSupported ctx call.Span "QUOTIENT beyond the range of a whole number"

            DaxValue.Integer(int64 whole))

        // "If the number is negative, the SQRT function returns an error"; "A decimal number"
        // (Microsoft Learn, SQRT).
        Fn.scalar "SQRT" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x < 0.0 then
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'SQRT' has the wrong data type or has an invalid value."

            DaxValue.Decimal(sqrt x))

        // "Returns the square root of (number * pi)" (Microsoft Learn, SQRTPI). A negative number
        // is not documented.
        Fn.scalar "SQRTPI" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x < 0.0 then
                Fail.notSupported ctx call.Span "SQRTPI of a negative number"

            DaxValue.Decimal(sqrt (x * Math.PI)))

        // "A decimal number"; "The exponentiation operator ^ returns the same result as the POWER
        // function" (dax.guide/power): `DaxValue.power`, with its BLANK rules.
        Fn.scalar
            "POWER"
            "Math"
            (2, 2)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Blank -> StaticType.Blank
                | _ -> StaticType.Known DaxType.Decimal)
            (fun ev ctx call ->
                let operand (i: int) =
                    match ev.Scalar(ctx, call.Args.[i]) with
                    | DaxValue.Boolean b -> DaxValue.Integer(if b then 1L else 0L)
                    | v -> Coerce.number ctx call.Args.[i].Span "POWER" v

                let b = operand 0
                let e = operand 1
                Fail.dax ctx call.Span (DaxValue.power b e))

        // "Returns e raised to the power of a given number"; "A decimal number" (Microsoft Learn,
        // EXP).
        Fn.scalar "EXP" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            finite ctx call (exp (floatArg ev ctx call 0)))

        // "If the number is less than or equal to zero, the function generates an error"
        // (dax.guide/ln).
        Fn.scalar "LN" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x <= 0.0 then
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'LN' has the wrong data type or has an invalid value."

            DaxValue.Decimal(log x))

        // "The base of the logarithm. If omitted, the base is 10"; LOG(100,10), LOG(100) and
        // LOG10(100) are 2 (Microsoft Learn, LOG). The pages ask for a "positive number" without
        // saying what another gives: refused, as is a base that is not positive or is 1.
        Fn.scalar "LOG" "Math" (1, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            let b =
                match List.tryItem 1 call.Args with
                | Some e when not (Fn.isOmitted e) -> floatArg ev ctx call 1
                | _ -> 10.0

            if x <= 0.0 then
                Fail.notSupported ctx call.Span "LOG of a number that is not positive"

            if b <= 0.0 || b = 1.0 then
                Fail.notSupported ctx call.Span "LOG to a base that is not positive or is 1"

            finite ctx call (if b = 10.0 then Math.Log10 x else Math.Log(x) / Math.Log(b)))

        Fn.scalar "LOG10" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x <= 0.0 then
                Fail.notSupported ctx call.Span "LOG10 of a number that is not positive"

            DaxValue.Decimal(Math.Log10 x))

        // "A decimal number with the value of Pi, 3.14159265358979, accurate to 15 digits"
        // (Microsoft Learn, PI).
        Fn.scalar "PI" "Math" (0, 0) (Fn.typeIs DaxType.Decimal) (fun _ _ _ -> DaxValue.Decimal Math.PI)

        // "A decimal number"; FACT of 0 to 5 is 1, 1, 2, 6, 24, 120 and of 170 is
        // 7.257415615308E+306; "If the result is too large, an error is returned" (Microsoft
        // Learn, FACT); "The largest factorial that can be returned by FACT is 170!"
        // (dax.guide/fact). "If the number is not an integer, it is truncated and an error is
        // returned" says two things at once, and a negative number is not mentioned: refused.
        Fn.scalar "FACT" "Math" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x <> Math.Truncate x then
                Fail.notSupported ctx call.Span "FACT of a number that is not whole"

            if x < 0.0 then
                Fail.notSupported ctx call.Span "FACT of a negative number"

            if x > 170.0 then
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'FACT' has the wrong data type or the result is too large or too small."

            let mutable product = BigInteger.One

            for i in 2 .. int x do
                product <- product * BigInteger i

            DaxValue.Decimal(float product))

        // "The greatest common divisor of two integers": GCD(24, 36) is 12 (Microsoft Learn,
        // GCD). The arguments are `wholeArg`'s.
        Fn.scalar "GCD" "Math" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let a = wholeArg ev ctx call 0
            let b = wholeArg ev ctx call 1
            DaxValue.Integer(int64 (BigInteger.GreatestCommonDivisor(a, b))))

        // "the least common multiple of integers": LCM(24, 36) is 72; "If LCM(a,b) >=2^53, LCM
        // returns the #NUM! error value" (Microsoft Learn, LCM). With a zero the pages say
        // nothing.
        Fn.scalar "LCM" "Math" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let a = wholeArg ev ctx call 0
            let b = wholeArg ev ctx call 1

            if a.IsZero || b.IsZero then
                Fail.notSupported ctx call.Span "LCM of zero"

            let lcm = a / BigInteger.GreatestCommonDivisor(a, b) * b

            if lcm >= BigInteger twoTo53 then
                Fail.runtime
                    ctx
                    call.Span
                    "An argument of function 'LCM' has the wrong data type or the result is too large or too small."

            DaxValue.Integer(int64 lcm))

        // "Returns number rounded up to the nearest even integer"; "Regardless of the sign of
        // number, a value is rounded up when adjusted away from zero": EVEN(1.5) is 2, EVEN(3) is
        // 4, EVEN(2) is 2, EVEN(-1) is -2 (Microsoft Learn, EVEN). "A single integer value"
        // (dax.guide/even).
        Fn.scalar "EVEN" "Math" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let x = floatArg ev ctx call 0
            let m = Math.Ceiling(abs x)

            if m >= twoTo53 then
                Fail.notSupported ctx call.Span "EVEN of a number with more than 15 digits"

            let m = int64 m
            let m = if m % 2L <> 0L then m + 1L else m
            DaxValue.Integer(if x < 0.0 then -m else m))

        // ODD(1.5) is 3, ODD(3) is 3, ODD(2) is 3, ODD(-1) is -1, ODD(-2) is -3 (Microsoft Learn,
        // ODD). Zero has no side to round away to, and the page doesn't give it.
        Fn.scalar "ODD" "Math" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let x = floatArg ev ctx call 0

            if x = 0.0 then
                Fail.notSupported ctx call.Span "ODD of zero"

            let m = Math.Ceiling(abs x)

            if m >= twoTo53 then
                Fail.notSupported ctx call.Span "ODD of a number with more than 15 digits"

            let m = int64 m
            let m = if m % 2L = 0L then m + 1L else m
            DaxValue.Integer(if x < 0.0 then -m else m))

        Fn.scalar "RAND" "Math" (0, 0) (Fn.typeIs DaxType.Decimal) (fun _ ctx call ->
            Fail.notSupported ctx call.Span "RAND: its result is random")
        |> unseeded "RAND"

        Fn.scalar "RANDBETWEEN" "Math" (2, 2) (Fn.typeIs DaxType.Integer) (fun _ ctx call ->
            Fail.notSupported ctx call.Span "RANDBETWEEN: its result is random")
        |> unseeded "RANDBETWEEN"
    ]
