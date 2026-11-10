namespace Tessera.DaxEngine

open Tessera
open Tessera.DaxAst

/// Static data types of expressions, and the type of a column of values.
///
/// A static type is `Some t` (always a `t`, or BLANK), `Some Variant` (not known without
/// evaluating) or None (always BLANK: it has no type and takes the other side's).
module Typing =

    let private numericOf =
        function
        | Some DaxType.Boolean -> Some DaxType.Integer
        | t -> t

    /// The one type that holds values of both types, as IF, SWITCH and COALESCE pick it: "IF can
    /// return a variant data type if value_if_true and value_if_false are of different data types,
    /// but the function attempts to return a single data type if both ... are of numeric data
    /// types" (Microsoft Learn, IF; the same text on dax.guide/if.eager). The numeric type is the
    /// one arithmetic between the two gives; a date with a number is a Decimal ("COALESCE ( DATE (
    /// 2020, 1, 1 ), 1 ) returns the decimal number 43831": dax.guide/coalesce).
    let unify (a: DaxType option) (b: DaxType option) : DaxType option =
        match a, b with
        | None, t
        | t, None -> t
        | Some DaxType.Variant, _
        | _, Some DaxType.Variant -> Some DaxType.Variant
        | Some x, Some y when x = y -> Some x
        | Some x, Some y ->
            match x, y with
            | (DaxType.Integer, DaxType.Decimal)
            | (DaxType.Decimal, DaxType.Integer)
            | (DaxType.Currency, DaxType.Decimal)
            | (DaxType.Decimal, DaxType.Currency) -> Some DaxType.Decimal
            | (DaxType.Integer, DaxType.Currency)
            | (DaxType.Currency, DaxType.Integer) -> Some DaxType.Currency
            | (DaxType.DateTime, (DaxType.Integer | DaxType.Decimal | DaxType.Currency))
            | ((DaxType.Integer | DaxType.Decimal | DaxType.Currency), DaxType.DateTime) -> Some DaxType.Decimal
            | _ -> Some DaxType.Variant

    /// The static type of `l op r`, following `DaxValue`'s arithmetic. Text operands are read as
    /// numbers or dates when evaluated, so their result type isn't known before.
    let binary (op: BinaryOp) (l: DaxType option) (r: DaxType option) : DaxType option =
        let l = numericOf l
        let r = numericOf r

        let unknown (t: DaxType option) =
            t = Some DaxType.Variant || t = Some DaxType.String

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
        | BinaryOp.In -> Some DaxType.Boolean
        | BinaryOp.Concat -> Some DaxType.String
        | BinaryOp.Power -> if l.IsNone then None else Some DaxType.Decimal
        | _ when unknown l || unknown r -> Some DaxType.Variant
        | BinaryOp.Add
        | BinaryOp.Subtract ->
            match l, r with
            | None, None -> None
            // Next to a value, BLANK is the Integer 0.
            | None, t ->
                (if op = BinaryOp.Subtract && t = Some DaxType.DateTime then
                     Some DaxType.Decimal
                 else
                     t)
            | t, None -> t
            | Some DaxType.DateTime, _ -> Some DaxType.DateTime
            | _, Some DaxType.DateTime ->
                if op = BinaryOp.Subtract then
                    Some DaxType.Decimal
                else
                    Some DaxType.DateTime
            | Some DaxType.Decimal, _
            | _, Some DaxType.Decimal -> Some DaxType.Decimal
            | Some DaxType.Currency, _
            | _, Some DaxType.Currency -> Some DaxType.Currency
            | _ -> Some DaxType.Integer
        | BinaryOp.Multiply ->
            match l, r with
            | None, _
            | _, None -> None
            | Some DaxType.Integer, Some DaxType.Integer -> Some DaxType.Integer
            | Some DaxType.Currency, Some DaxType.Currency -> Some DaxType.Decimal
            | Some DaxType.Currency, _
            | _, Some DaxType.Currency -> Some DaxType.Currency
            | _ -> Some DaxType.Decimal
        | BinaryOp.Divide ->
            match l, r with
            | None, _ -> None
            // A division by zero or BLANK is a Decimal infinity or NaN whatever the left side.
            | Some DaxType.Currency, _ -> Some DaxType.Variant
            | _ -> Some DaxType.Decimal

    /// A value as a value of a statically known numeric type (an Integer branch of an IF whose
    /// other branch is a Decimal is a Decimal). Anything else is left as it is.
    let coerce (target: DaxType option) (v: DaxValue) : DaxValue =
        match target, DaxValue.typeOf v with
        | Some t, Some vt when t <> vt && t <> DaxType.Variant ->
            match t, vt with
            | DaxType.Decimal, (DaxType.Integer | DaxType.Currency | DaxType.DateTime)
            | DaxType.Currency, DaxType.Integer ->
                match DaxValue.convertTo t v with
                | Ok c -> c
                | Error _ -> v
            | _ -> v
        | _ -> v

    /// The type of a column from its values (BLANKs aside): the one type they share, the numeric
    /// type that holds them all, else Variant. None when every value is BLANK.
    let ofValues (values: DaxValue seq) : DaxType option =
        let mutable t = None

        for v in values do
            match DaxValue.typeOf v with
            | Some vt -> t <- unify t (Some vt)
            | None -> ()

        t

    /// A column's declared type and its values as that type: the static type where it is known and
    /// the values fit it, otherwise the type of the values. A column that is BLANK in every row
    /// and has no static type is declared Variant.
    let column (staticType: DaxType option) (values: DaxValue array) : DaxType * DaxValue array =
        let dynamic = ofValues values

        let declared =
            match staticType, dynamic with
            | Some s, None when s <> DaxType.Variant -> s
            | Some s, Some d when s <> DaxType.Variant && unify (Some s) (Some d) = Some s -> s
            | _, Some d -> d
            | _, None -> DaxType.Variant

        if declared = DaxType.Variant then
            declared, values
        else
            declared, values |> Array.map (coerce (Some declared))
