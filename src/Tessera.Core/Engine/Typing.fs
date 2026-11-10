namespace Tessera.DaxEngine

open Tessera
open Tessera.DaxAst

/// Static data types of expressions (see `StaticType`), and the type of a column of values.
module Typing =

    let private known (t: DaxType) = StaticType.Known t

    /// The numeric type that holds values of both, as arithmetic between them gives it; a date with
    /// a number is a Decimal ("COALESCE ( DATE ( 2020, 1, 1 ), 1 ) returns the decimal number
    /// 43831": dax.guide/coalesce). None for types that have no common one.
    let private common (x: DaxType) (y: DaxType) : DaxType option =
        if x = y then
            Some x
        else
            match x, y with
            | (DaxType.Integer, DaxType.Decimal)
            | (DaxType.Decimal, DaxType.Integer)
            | (DaxType.Currency, DaxType.Decimal)
            | (DaxType.Decimal, DaxType.Currency) -> Some DaxType.Decimal
            | (DaxType.Integer, DaxType.Currency)
            | (DaxType.Currency, DaxType.Integer) -> Some DaxType.Currency
            | (DaxType.DateTime, (DaxType.Integer | DaxType.Decimal | DaxType.Currency))
            | ((DaxType.Integer | DaxType.Decimal | DaxType.Currency), DaxType.DateTime) -> Some DaxType.Decimal
            | _ -> None

    /// The type of an expression that is one of two others, as IF, SWITCH and COALESCE have it:
    /// "IF can return a variant data type if value_if_true and value_if_false are of different
    /// data types, but the function attempts to return a single data type if both ... are of
    /// numeric data types" (Microsoft Learn, IF; the same text on dax.guide/if.eager).
    let unify (a: StaticType) (b: StaticType) : StaticType =
        match a, b with
        | StaticType.Blank, t
        | t, StaticType.Blank -> t
        | StaticType.Variant, _
        | _, StaticType.Variant -> StaticType.Variant
        | StaticType.Unknown, _
        | _, StaticType.Unknown -> StaticType.Unknown
        | StaticType.Known x, StaticType.Known y ->
            match common x y with
            | Some t -> known t
            | None -> StaticType.Variant

    /// The static type of `l op r`, following `DaxValue`'s arithmetic. Text operands are read as
    /// numbers or dates when evaluated, so their result type isn't known before.
    let binary (op: BinaryOp) (l: StaticType) (r: StaticType) : StaticType =
        let numeric (t: StaticType) =
            match t with
            | StaticType.Known DaxType.Boolean -> known DaxType.Integer
            | StaticType.Known DaxType.String
            | StaticType.Variant -> StaticType.Unknown
            | t -> t

        let l = numeric l
        let r = numeric r

        match op with
        | BinaryOp.And
        | BinaryOp.Or
        | BinaryOp.Equal
        | BinaryOp.StrictEqual
        | BinaryOp.NotEqual
        | BinaryOp.Less
        | BinaryOp.LessOrEqual
        | BinaryOp.Greater
        | BinaryOp.GreaterOrEqual
        | BinaryOp.In -> known DaxType.Boolean
        | BinaryOp.Concat -> known DaxType.String
        | BinaryOp.Power ->
            if l = StaticType.Blank then
                StaticType.Blank
            else
                known DaxType.Decimal
        | BinaryOp.Add
        | BinaryOp.Subtract ->
            match l, r with
            | StaticType.Unknown, _
            | _, StaticType.Unknown -> StaticType.Unknown
            | StaticType.Blank, StaticType.Blank -> StaticType.Blank
            // Next to a value, BLANK is the Integer 0.
            | StaticType.Blank, StaticType.Known DaxType.DateTime when op = BinaryOp.Subtract -> known DaxType.Decimal
            | StaticType.Blank, t
            | t, StaticType.Blank -> t
            | StaticType.Known x, StaticType.Known y ->
                match x, y with
                | DaxType.DateTime, _ -> known DaxType.DateTime
                | _, DaxType.DateTime ->
                    if op = BinaryOp.Subtract then
                        known DaxType.Decimal
                    else
                        known DaxType.DateTime
                | DaxType.Decimal, _
                | _, DaxType.Decimal -> known DaxType.Decimal
                | DaxType.Currency, _
                | _, DaxType.Currency -> known DaxType.Currency
                | _ -> known DaxType.Integer
            | _ -> StaticType.Unknown
        | BinaryOp.Multiply ->
            match l, r with
            | StaticType.Blank, _
            | _, StaticType.Blank -> StaticType.Blank
            | StaticType.Known x, StaticType.Known y ->
                match x, y with
                | DaxType.Integer, DaxType.Integer -> known DaxType.Integer
                | DaxType.Currency, DaxType.Currency -> known DaxType.Decimal
                | DaxType.Currency, _
                | _, DaxType.Currency -> known DaxType.Currency
                | _ -> known DaxType.Decimal
            | _ -> StaticType.Unknown
        | BinaryOp.Divide ->
            match l, r with
            | StaticType.Blank, _ -> StaticType.Blank
            // Currency / whole number is a Currency, but a division by zero or BLANK is a Decimal
            // infinity or NaN: the values decide.
            | StaticType.Known DaxType.Currency, _
            | StaticType.Unknown, _ -> StaticType.Unknown
            | _ -> known DaxType.Decimal

    /// A value as a value of a numeric type it widens to (an Integer branch of an IF whose other
    /// branch is a Decimal is a Decimal). Anything else is left as it is.
    let widen (target: DaxType) (v: DaxValue) : DaxValue =
        match target, DaxValue.typeOf v with
        | DaxType.Decimal, Some(DaxType.Integer | DaxType.Currency | DaxType.DateTime)
        | DaxType.Currency, Some DaxType.Integer ->
            match DaxValue.convertTo target v with
            | Ok c -> c
            | Error _ -> v
        | _ -> v

    /// `widen` to a statically known type; nothing changes for the other static types.
    let coerce (target: StaticType) (v: DaxValue) : DaxValue =
        match target with
        | StaticType.Known t -> widen t v
        | _ -> v

    /// The type of a column from its values (BLANKs aside): the one type they share, the numeric
    /// type that holds them all, else Variant. None when every value is BLANK.
    let ofValues (values: DaxValue seq) : DaxType option =
        let mutable t = None

        for v in values do
            match DaxValue.typeOf v, t with
            | None, _ -> ()
            | Some vt, None -> t <- Some vt
            | Some _, Some DaxType.Variant -> ()
            | Some vt, Some current -> t <- Some(common current vt |> Option.defaultValue DaxType.Variant)

        t

    /// A column's declared type, and its values as that type.
    ///
    /// - statically Variant: Variant, as the engine declares it;
    /// - statically a type the values fit (after widening): that type, also when every row is BLANK;
    /// - otherwise the type of the values. A column of BLANKs with no static type is a Variant.
    let column (staticType: StaticType) (values: DaxValue array) : DaxType * DaxValue array =
        let dynamic = ofValues values

        let declared =
            match staticType, dynamic with
            | StaticType.Variant, _ -> DaxType.Variant
            | StaticType.Known s, None -> s
            | StaticType.Known s, Some d when common s d = Some s -> s
            | _, Some d -> d
            | _, None -> DaxType.Variant

        if declared = DaxType.Variant then
            declared, values
        else
            declared, values |> Array.map (widen declared)

    /// The static type of a column of a table value: its declared type.
    let ofColumn (t: DaxType) : StaticType =
        if t = DaxType.Variant then
            StaticType.Variant
        else
            StaticType.Known t

/// Conversions whose result the sources leave open: refused rather than guessed.
module Convert =

    /// A value as text, as `&` and CONCATENATEX need it. A date's text depends on the model's
    /// culture, and the sources disagree on a Boolean's ("True" or "TRUE"), so those are refused.
    let toText (ctx: Ctx) (span: Span) (v: DaxValue) : string =
        match v with
        | DaxValue.DateTime _ ->
            Fail.notSupported
                ctx
                span
                "a date converted to text (the format depends on the model's culture; FORMAT is not implemented)"
        | DaxValue.Boolean _ -> Fail.notSupported ctx span "TRUE or FALSE converted to text"
        | _ -> DaxValue.toText v

    /// An operand of arithmetic. Text that reads as a number is that number; text that would only
    /// read as a date is refused (how the engine reads one depends on the model's culture).
    let checkOperand (ctx: Ctx) (span: Span) (v: DaxValue) : unit =
        match v with
        | DaxValue.String _ ->
            match DaxValue.convertTo DaxType.Decimal v, DaxValue.add (DaxValue.Integer 0L) v with
            | Error _, Ok _ ->
                Fail.notSupported
                    ctx
                    span
                    "text read as a date in arithmetic (how a date is written depends on the model's culture)"
            | _ -> ()
        | _ -> ()

/// `&&`, `||`, AND and OR.
module Logic =

    /// `left && right` or `left || right`, the left side already evaluated. The right side is not
    /// evaluated when the left decides, except to tell whether both sides are BLANK: Microsoft's
    /// table of BLANK in operations gives BLANK for that, SQLBI says a logical expression is
    /// never BLANK, so it is refused (see `DaxValue.bothBlank`).
    let connective (ev: IEval) (ctx: Ctx) (isAnd: bool) (span: Span) (left: DaxValue) (right: Expr) : DaxValue =
        let toBool (v: DaxValue) =
            Fail.dax ctx span (DaxValue.toBoolean v)

        if DaxValue.isBlank left then
            let r =
                try
                    ev.Scalar(ctx, right)
                with DaxRuntimeException _ when isAnd ->
                    // BLANK && <an error>: FALSE if the right side is never looked at.
                    Fail.notSupported
                        ctx
                        span
                        "BLANK && an expression that raises an error (whether the right side is evaluated is not documented)"

            if DaxValue.isBlank r then
                Fail.notSupported ctx span DaxValue.bothBlank
            elif isAnd then
                // Evaluated only to rule out BLANK; it must still be a condition.
                toBool r |> ignore
                DaxValue.Boolean false
            else
                DaxValue.Boolean(toBool r)
        elif isAnd then
            if not (toBool left) then
                DaxValue.Boolean false
            else
                DaxValue.Boolean(toBool (ev.Scalar(ctx, right)))
        elif toBool left then
            DaxValue.Boolean true
        else
            DaxValue.Boolean(toBool (ev.Scalar(ctx, right)))

/// Sorting where the sources leave the order open.
module Sorting =

    /// Refuse to sort by values of different types, or by a key that is BLANK in some rows and
    /// zero, negative, empty or FALSE in others: where BLANK sorts among those is not documented.
    let check (ctx: Ctx) (span: Span) (what: string) (values: DaxValue seq) : unit =
        let values = List.ofSeq values
        let blank = values |> List.exists DaxValue.isBlank

        let families =
            values
            |> List.choose (fun v ->
                match v with
                | DaxValue.Blank -> None
                | DaxValue.String _ -> Some 1
                | DaxValue.Boolean _ -> Some 2
                | _ -> Some 0)
            |> List.distinct

        if families.Length > 1 then
            Fail.notSupported ctx span $"{what} by values of different data types"

        if blank then
            let low =
                values
                |> List.exists (fun v ->
                    not (DaxValue.isBlank v)
                    && (match DaxValue.compare ctx.Model.Collation v DaxValue.Blank with
                        | Ok c -> c <= 0
                        | Error _ -> false))

            if low then
                Fail.notSupported
                    ctx
                    span
                    $"{what} by a key that is BLANK in some rows and zero, negative, empty or FALSE in others (where BLANK sorts among those is not documented)"
