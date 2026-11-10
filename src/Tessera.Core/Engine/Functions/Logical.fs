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

    /// The value of the branch IF or SWITCH takes (None: the branch was left out, "If omitted, a
    /// BLANK is returned": dax.guide/if), as a value of the type the branches share.
    ///
    /// A BLANK that is written as one (BLANK (), a branch left out) has no type of its own and is
    /// cast to the type of the other branches, and "When <expression> is of Boolean data type, IF
    /// is of Boolean data type and an implicit cast is applied to BLANK which converts to FALSE";
    /// a Boolean expression that happens to be BLANK is not cast ("there is no cast, hence BLANK
    /// is preserved"): Jeffrey Wang, in SQLBI's "BLANK handling in DAX", whose table gives FALSE
    /// for IF ( 1 = 1, BLANK (), TRUE ).
    let private branchValue
        (ev: IEval)
        (ctx: Ctx)
        (span: Span)
        (branches: Expr list)
        (chosen: Expr option)
        (given: DaxValue option)
        : DaxValue =
        let typeOfBranch (e: Expr) =
            if Fn.isOmitted e then
                StaticType.Blank
            else
                ev.TypeOf(ctx, e)

        let t = unifyAll ev ctx branches

        let value, written =
            match chosen, given with
            | None, _ -> DaxValue.Blank, true
            | Some e, Some v -> v, typeOfBranch e = StaticType.Blank
            | Some e, None -> ev.Scalar(ctx, e), typeOfBranch e = StaticType.Blank

        if not (DaxValue.isBlank value && written) then
            Typing.coerce t value
        else
            match t with
            | StaticType.Known DaxType.Boolean -> DaxValue.Boolean false
            | StaticType.Unknown ->
                // The other branches decide, and their types aren't known without their values.
                let others =
                    branches
                    |> List.filter (fun b -> not (Fn.isOmitted b))
                    |> List.map (fun b -> b, typeOfBranch b)
                    |> List.filter (fun (_, bt) -> bt <> StaticType.Blank)

                let knownOther =
                    others
                    |> List.exists (fun (_, bt) ->
                        match bt with
                        | StaticType.Known k -> k <> DaxType.Boolean
                        | _ -> false)

                if knownOther then
                    DaxValue.Blank
                else
                    let unsure () =
                        Fail.notSupported
                            ctx
                            span
                            "a BLANK branch of IF or SWITCH where the data type of the other branches can't be told (the BLANK becomes FALSE when they are True/False)"

                    let dynamic =
                        others
                        |> List.filter (fun (_, bt) -> bt = StaticType.Unknown)
                        |> List.map (fun (b, _) ->
                            try
                                DaxValue.typeOf (ev.Scalar(ctx, b))
                            with DaxRuntimeException _ ->
                                None)

                    if dynamic |> List.exists (fun d -> d.IsSome && d <> Some DaxType.Boolean) then
                        DaxValue.Blank
                    elif dynamic |> List.forall (fun d -> d = Some DaxType.Boolean) then
                        DaxValue.Boolean false
                    else
                        unsure ()
            | _ -> DaxValue.Blank

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

        branchValue ev ctx call.Span branches chosen None

    /// IF.EAGER "uses an eager execution plan which always executes the branch expressions
    /// regardless of the condition expression" (Microsoft Learn, IF.EAGER): an error in the
    /// branch that isn't taken is raised.
    let private ifEagerImpl (ev: IEval) (ctx: Ctx) (call: Call) : DaxValue =
        let branches = call.Args.Tail

        let values =
            branches
            |> List.map (fun b -> if Fn.isOmitted b then None else Some(ev.Scalar(ctx, b)))

        let cond = toBool ctx call.Args.[0] (ev.Scalar(ctx, call.Args.[0]))
        let index = if cond then 0 else 1

        match List.tryItem index branches with
        | Some b when not (Fn.isOmitted b) -> branchValue ev ctx call.Span branches (Some b) values.[index]
        | _ -> branchValue ev ctx call.Span branches None None

    /// "ISERROR also returns TRUE when the value is positive infinity, negative infinity or NaN
    /// (not a number). For example, 1 / 0, -1 / 0 and 0 / 0 return these values. IFERROR has the
    /// same behavior" (dax.guide/iserror).
    let private notFinite (v: DaxValue) =
        match v with
        | DaxValue.Decimal d -> Double.IsNaN d || Double.IsInfinity d
        | _ -> false

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
        Fn.scalar "IF.EAGER" "Logical" (2, 3) ifType ifEagerImpl

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

                // "SWITCH can be rewritten as nested IF" (the same note in SQLBI's article).
                match hit, otherwise with
                | Some(_, result), _ -> branchValue ev ctx call.Span results (Some result) None
                | None, Some e -> branchValue ev ctx call.Span results (Some e) None
                | None, None -> branchValue ev ctx call.Span results None None)

        Fn.scalar "AND" "Logical" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            Logic.connective ev ctx true call.Args.[1].Span (ev.Scalar(ctx, call.Args.[0])) call.Args.[1])

        Fn.scalar "OR" "Logical" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            Logic.connective ev ctx false call.Args.[1].Span (ev.Scalar(ctx, call.Args.[0])) call.Args.[1])

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

        // The division, or the alternate result (BLANK by default) "in case there is a division by
        // zero" (dax.guide/divide). A BLANK denominator is zero; a BLANK numerator gives BLANK, as
        // with `/`. The result type is that of `/`: "The division (/) operator displays the same
        // behavior as the DIVIDE function. The result is always decimal, unless a currency is
        // divided by an integer or by a decimal; in this case, the result is currency" (SQLBI,
        // "Understanding numeric data type conversions in DAX", whose table is headed DIVIDE and
        // /). The function pages' "A decimal number" doesn't single out the Currency case.
        Fn.scalar
            "DIVIDE"
            "Math"
            (2, 3)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                // A Currency numerator: a Currency or a Decimal, as the values decide.
                | StaticType.Known DaxType.Currency
                | StaticType.Variant
                | StaticType.Unknown -> StaticType.Unknown
                | _ ->
                    match List.tryItem 2 args with
                    | Some alt when not (Fn.isOmitted alt) ->
                        Typing.unify (StaticType.Known DaxType.Decimal) (ev.TypeOf(ctx, alt))
                    | _ -> StaticType.Known DaxType.Decimal)
            (fun ev ctx call ->
                let number (e: Expr) =
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Blank -> DaxValue.Blank
                    | DaxValue.DateTime _ -> Fail.notSupported ctx e.Span "DIVIDE of dates"
                    | DaxValue.Currency _ as v -> v
                    | v -> Fail.dax ctx e.Span (DaxValue.convertTo DaxType.Decimal v)

                let n = number call.Args.[0]
                let d = number call.Args.[1]

                match d with
                | DaxValue.Blank
                | DaxValue.Currency 0L
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
                let t = Typing.unify (ev.TypeOf(ctx, call.Args.[0])) (ev.TypeOf(ctx, call.Args.[1]))

                let value =
                    try
                        Some(ev.Scalar(ctx, call.Args.[0]))
                    with DaxRuntimeException _ ->
                        None

                match value with
                | Some v when not (notFinite v) -> Typing.coerce t v
                | _ -> Typing.coerce t (ev.Scalar(ctx, call.Args.[1])))

        // ISERROR counts the infinities and NaN as errors (see `notFinite`).
        Fn.scalar "ISERROR" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            try
                DaxValue.Boolean(notFinite (ev.Scalar(ctx, call.Args.[0])))
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
