namespace Tessera.DaxEngine.Functions

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// The few math functions the engine has so far: ABS, INT, MOD, ROUND. (DIVIDE is in Logical.fs.)
module Math =

    let private number (ctx: Ctx) (e: Expr) (name: string) (v: DaxValue) : DaxValue =
        match v with
        | DaxValue.Integer _
        | DaxValue.Decimal _
        | DaxValue.Currency _ -> v
        | DaxValue.Blank -> Fail.notSupported ctx e.Span $"{name} of BLANK"
        | _ -> Fail.notSupported ctx e.Span $"{name} of a value that is not a number"

    let private asFloat (v: DaxValue) =
        match v with
        | DaxValue.Integer i -> float i
        | DaxValue.Decimal d -> d
        | DaxValue.Currency c -> float c / 10000.0
        | _ -> nan

    let functions: DaxFunction list = [
        // "The absolute value of a number with the same data type" (dax.guide/abs).
        Fn.scalar "ABS" "Math" (1, 1) (Fn.typeOfArg 0) (fun ev ctx call ->
            match number ctx call.Args.[0] "ABS" (ev.Scalar(ctx, call.Args.[0])) with
            | DaxValue.Integer i when i < 0L -> Fail.dax ctx call.Span (DaxValue.negate (DaxValue.Integer i))
            | DaxValue.Currency c when c < 0L -> Fail.dax ctx call.Span (DaxValue.negate (DaxValue.Currency c))
            | DaxValue.Decimal d -> DaxValue.Decimal(abs d)
            | v -> v)

        // "A single decimal value"; digits > 0 are decimal places, 0 the nearest integer, < 0 to
        // the left of the decimal point (dax.guide/round). The number is rounded as it reads in
        // decimal: Microsoft Learn's example rounds 2.15 to 2.2, though the float 2.15 is just
        // below it.
        Fn.scalar "ROUND" "Math" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let v = number ctx call.Args.[0] "ROUND" (ev.Scalar(ctx, call.Args.[0]))

            let digits =
                match number ctx call.Args.[1] "ROUND to a number of digits" (ev.Scalar(ctx, call.Args.[1])) with
                | DaxValue.Integer i when i >= -20L && i <= 20L -> int i
                | _ ->
                    Fail.notSupported ctx call.Args.[1].Span "ROUND to a number of digits that is not a small integer"

            let x = asFloat v

            if Double.IsNaN x || Double.IsInfinity x || abs x >= 7.9e27 then
                Fail.notSupported ctx call.Span "ROUND of a number that is not finite or is beyond 28 digits"

            let d =
                match v with
                | DaxValue.Currency c -> DaxValue.currencyValue c
                | DaxValue.Integer i -> decimal i
                | _ -> decimal x

            // A positive number exactly halfway rounds up ("rounds 2.15 up, to one decimal place.
            // The expected result is 2.2": Microsoft Learn, ROUND). The pages say nothing of a
            // negative number halfway.
            let scaled =
                if digits >= 0 then
                    d * pown 10m digits
                else
                    d / pown 10m (-digits)

            if d < 0m && abs (scaled - Math.Truncate scaled) = 0.5m then
                Fail.notSupported ctx call.Span "ROUND of a negative number exactly halfway between two results"

            let rounded =
                if digits >= 0 then
                    Math.Round(d, digits, MidpointRounding.AwayFromZero)
                else
                    let scale = pown 10m (-digits)
                    Math.Round(d / scale, 0, MidpointRounding.AwayFromZero) * scale

            DaxValue.Decimal(float rounded))

        // "INT rounds <Number> towards −∞"; an Integer for an Integer or a Decimal, a Currency for
        // a Currency (dax.guide/int).
        Fn.scalar
            "INT"
            "Math"
            (1, 1)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Known DaxType.Currency -> StaticType.Known DaxType.Currency
                | StaticType.Known(DaxType.Integer | DaxType.Decimal) -> StaticType.Known DaxType.Integer
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                match number ctx call.Args.[0] "INT" (ev.Scalar(ctx, call.Args.[0])) with
                | DaxValue.Decimal d ->
                    let f = Math.Floor d

                    if Double.IsNaN f || f >= 9223372036854775808.0 || f < -9223372036854775808.0 then
                        Fail.runtime ctx call.Span (DaxError.message (Overflow("INT", DaxType.Integer)))

                    DaxValue.Integer(int64 f)
                | DaxValue.Currency c ->
                    let whole = (if c >= 0L then c / 10000L else -((-c + 9999L) / 10000L))
                    DaxValue.Currency(whole * 10000L)
                | v -> v)

        // "The result always has the same sign as the divisor"; "MOD(n, d) = n - d*INT(n/d)"; "If
        // the divisor is 0 (zero), MOD returns an error" (Microsoft Learn, MOD). Whole numbers
        // only: the pages give no result type for the others.
        Fn.scalar "MOD" "Math" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            match ev.Scalar(ctx, call.Args.[0]), ev.Scalar(ctx, call.Args.[1]) with
            | DaxValue.Integer _, DaxValue.Integer 0L -> Fail.runtime ctx call.Span "MOD: division by zero."
            | DaxValue.Integer n, DaxValue.Integer d ->
                let r = n % d
                DaxValue.Integer(if r <> 0L && ((r < 0L) <> (d < 0L)) then r + d else r)
            | _ -> Fail.notSupported ctx call.Span "MOD of values that are not whole numbers")
    ]
