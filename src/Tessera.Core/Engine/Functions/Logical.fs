namespace Tessera.DaxEngine.Functions

// Logical and information functions, and the handful of scalar functions tests need.

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Logical =

    let private toBool (ctx: Ctx) (e: Expr) (v: DaxValue) =
        Fail.dax ctx e.Span (DaxValue.toBoolean v)

    let private unifyAll (ev: IEval) (ctx: Ctx) (args: Expr list) =
        args
        |> List.fold
            (fun t a ->
                Typing.unify
                    t
                    (if Fn.isOmitted a then
                         StaticType.Blank
                     else
                         ev.TypeOf(ctx, a)))
            StaticType.Blank

    /// Only the branch that is taken is evaluated; its value becomes a value of the type the
    /// branches share when both are numbers (see `Typing.unify`).
    let private ifImpl (ev: IEval) (ctx: Ctx) (call: Call) : DaxValue =
        let cond = toBool ctx call.Args.[0] (ev.Scalar(ctx, call.Args.[0]))
        let branches = call.Args.Tail

        let chosen =
            if cond then
                Some branches.[0]
            else
                List.tryItem 1 branches |> Option.filter (fun e -> not (Fn.isOmitted e))

        match chosen with
        // "If omitted, a BLANK is returned" (dax.guide/if).
        | None -> DaxValue.Blank
        | Some e -> Typing.coerce (unifyAll ev ctx branches) (ev.Scalar(ctx, e))

    let private ifType: IEval -> Ctx -> Expr list -> StaticType =
        fun ev ctx args -> unifyAll ev ctx args.Tail

    /// SWITCH ( expression, value, result, ..., [else] ).
    let private switchParts (args: Expr list) : (Expr * Expr) list * Expr option =
        let rec read (items: Expr list) (pairs: (Expr * Expr) list) =
            match items with
            | value :: result :: tail -> read tail ((value, result) :: pairs)
            | [ otherwise ] -> List.rev pairs, Some otherwise
            | _ -> List.rev pairs, None

        read args.Tail []

    let functions: DaxFunction list = [
        Fn.scalar "IF" "Logical" (2, 3) ifType ifImpl
        Fn.scalar "IF.EAGER" "Logical" (2, 3) ifType ifImpl

        // "SWITCH compares Expression with each Value like the == operator, not like the =
        // operator. BLANK does not match 0 or an empty string" (dax.guide/switch). Values are tried
        // in order and only the matching result is evaluated.
        Fn.scalar
            "SWITCH"
            "Logical"
            (3, -1)
            (fun ev ctx args ->
                let pairs, otherwise = switchParts args
                unifyAll ev ctx ((pairs |> List.map snd) @ Option.toList otherwise))
            (fun ev ctx call ->
                let pairs, otherwise = switchParts call.Args
                let subject = ev.Scalar(ctx, call.Args.[0])
                let results = (pairs |> List.map snd) @ Option.toList otherwise

                let hit =
                    pairs
                    |> List.tryFind (fun (value, _) ->
                        Fail.dax
                            ctx
                            value.Span
                            (DaxValue.strictEquals ctx.Model.Collation subject (ev.Scalar(ctx, value))))

                match hit, otherwise with
                | Some(_, result), _ -> Typing.coerce (unifyAll ev ctx results) (ev.Scalar(ctx, result))
                | None, Some e -> Typing.coerce (unifyAll ev ctx results) (ev.Scalar(ctx, e))
                | None, None -> DaxValue.Blank)

        Fn.scalar "AND" "Logical" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            if not (toBool ctx call.Args.[0] (ev.Scalar(ctx, call.Args.[0]))) then
                DaxValue.Boolean false
            else
                DaxValue.Boolean(toBool ctx call.Args.[1] (ev.Scalar(ctx, call.Args.[1]))))

        Fn.scalar "OR" "Logical" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            if toBool ctx call.Args.[0] (ev.Scalar(ctx, call.Args.[0])) then
                DaxValue.Boolean true
            else
                DaxValue.Boolean(toBool ctx call.Args.[1] (ev.Scalar(ctx, call.Args.[1]))))

        Fn.scalar "NOT" "Logical" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            Fail.dax ctx call.Span (DaxValue.logicalNot (ev.Scalar(ctx, call.Args.[0]))))

        Fn.scalar "TRUE" "Logical" (0, 0) (Fn.typeIs DaxType.Boolean) (fun _ _ _ -> DaxValue.Boolean true)
        Fn.scalar "FALSE" "Logical" (0, 0) (Fn.typeIs DaxType.Boolean) (fun _ _ _ -> DaxValue.Boolean false)
        Fn.scalar "BLANK" "Logical" (0, 0) (fun _ _ _ -> StaticType.Blank) (fun _ _ _ -> DaxValue.Blank)

        // "COALESCE returns the first argument that is not BLANK ... An empty string and 0 are not
        // BLANK"; "The data type of the result can be different from the data type of the returned
        // argument" (dax.guide/coalesce). Arguments after the first non-blank are not evaluated.
        Fn.scalar "COALESCE" "Logical" (2, -1) unifyAll (fun ev ctx call ->
            let t = unifyAll ev ctx call.Args

            call.Args
            |> List.tryPick (fun a ->
                match ev.Scalar(ctx, a) with
                | DaxValue.Blank -> None
                | v -> Some(Typing.coerce t v))
            |> Option.defaultValue DaxValue.Blank)

        // An empty string is not BLANK (dax.guide/isblank: "same result as <expression> == BLANK()").
        Fn.scalar "ISBLANK" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            DaxValue.Boolean(DaxValue.isBlank (ev.Scalar(ctx, call.Args.[0]))))

        // "A single decimal value": the division, or the alternate result (BLANK by default) "in
        // case there is a division by zero" (dax.guide/divide). A BLANK denominator is zero; a
        // BLANK numerator gives BLANK, as with `/`.
        Fn.scalar
            "DIVIDE"
            "Math"
            (2, 3)
            (fun ev ctx args ->
                match List.tryItem 2 args with
                | Some alt when not (Fn.isOmitted alt) ->
                    Typing.unify (StaticType.Known DaxType.Decimal) (ev.TypeOf(ctx, alt))
                | _ -> StaticType.Known DaxType.Decimal)
            (fun ev ctx call ->
                let number (e: Expr) =
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Blank -> DaxValue.Blank
                    | DaxValue.DateTime _ -> Fail.notSupported ctx e.Span "DIVIDE of dates"
                    | v -> Fail.dax ctx e.Span (DaxValue.convertTo DaxType.Decimal v)

                let n = number call.Args.[0]
                let d = number call.Args.[1]

                match d with
                | DaxValue.Blank
                | DaxValue.Decimal 0.0 ->
                    match List.tryItem 2 call.Args with
                    | Some alt when not (Fn.isOmitted alt) ->
                        Typing.coerce
                            (Typing.unify (StaticType.Known DaxType.Decimal) (ev.TypeOf(ctx, alt)))
                            (ev.Scalar(ctx, alt))
                    | _ -> DaxValue.Blank
                | _ -> Fail.dax ctx call.Span (DaxValue.divide n d))

        // "IFERROR ( A, B ) corresponds to IF ( ISERROR ( A ), B, A )" (dax.guide/iferror). Only
        // DAX errors are caught; something this engine doesn't support stays an error.
        Fn.scalar
            "IFERROR"
            "Logical"
            (2, 2)
            (fun ev ctx args -> Typing.unify (ev.TypeOf(ctx, args.[0])) (ev.TypeOf(ctx, args.[1])))
            (fun ev ctx call ->
                try
                    ev.Scalar(ctx, call.Args.[0])
                with DaxRuntimeException _ ->
                    ev.Scalar(ctx, call.Args.[1]))

        Fn.scalar "ISERROR" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            try
                ev.Scalar(ctx, call.Args.[0]) |> ignore
                DaxValue.Boolean false
            with DaxRuntimeException _ ->
                DaxValue.Boolean true)

        // "stops the execution raising an error with the description provided" (dax.guide/error).
        Fn.scalar "ERROR" "Logical" (1, 1) (fun _ _ _ -> StaticType.Blank) (fun ev ctx call ->
            Fail.runtime ctx call.Span (DaxValue.toText (ev.Scalar(ctx, call.Args.[0]))))

        // CONTAINSROW ( table, value, ... ): what IN runs ("The IN operator internally executes
        // CONTAINSROW"), with strict comparison (dax.guide/containsrow).
        Fn.scalar "CONTAINSROW" "Information" (2, -1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let values = call.Args.Tail |> List.map (fun a -> ev.Scalar(ctx, a)) |> Array.ofList

            if table.Cols.Length <> values.Length then
                Fail.invalid
                    ctx
                    call.Span
                    $"CONTAINSROW was given {values.Length} value(s) for a table of {table.Cols.Length} column(s)"

            let data = Array.init values.Length table.Column

            let found =
                seq { 0 .. table.Count - 1 }
                |> Seq.exists (fun row ->
                    values
                    |> Array.mapi (fun c v ->
                        Fail.dax ctx call.Span (DaxValue.strictEquals ctx.Model.Collation v data.[c].[row]))
                    |> Array.forall id)

            DaxValue.Boolean found)
    ]

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

        // "A BLANK argument is an empty string. If both arguments are BLANK, the result is BLANK"
        // (dax.guide/concatenate). The page shows TRUE as "TRUE" where other sources give "True",
        // so Booleans are refused (see `Convert.toText`).
        Fn.scalar "CONCATENATE" "Text" (2, 2) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let a = ev.Scalar(ctx, call.Args.[0])
            let b = ev.Scalar(ctx, call.Args.[1])

            match a, b with
            | DaxValue.Blank, DaxValue.Blank -> DaxValue.Blank
            | _ -> DaxValue.String(Convert.toText ctx call.Span a + Convert.toText ctx call.Span b))
    ]
