namespace Tessera.DaxEngine.Functions

// Statistical functions: MEDIAN, PERCENTILE.INC / .EXC, STDEV.S / .P, VAR.S / .P, GEOMEAN (each
// with its X form), AVERAGEA, MINA, MAXA and RANK.EQ.
//
// Sources: the function pages of dax.guide and of Microsoft Learn, quoted at each function. The
// column forms are the X forms over the column's table ("MEDIAN ( table[column] ) corresponds to
// the following MEDIANX call: MEDIANX ( table, table[column] )": dax.guide/median, and the same on
// the pages of STDEV.S, VAR.P and GEOMEAN), except in what they do with BLANK.
//
// Whole and decimal numbers only: the pages say "Only numeric data types are supported. Logical
// values, dates, and text columns are not supported" (MEDIAN) or that they are "ignored" (STDEV,
// GEOMEAN) without agreeing on which, and none gives the result type for fixed decimal numbers.

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Statistical =

    let private twoTo53 = 9007199254740992.0

    /// The values of a model column in the rows the filter context shows.
    let private columnValues (ev: IEval) (ctx: Ctx) (arg: Expr) : BColumn * DaxValue array =
        let c = ev.Column(ctx, arg)
        let rows = Filters.visibleStored ctx.Filter c.Table
        let data = c.Data
        let ids = data.ValueIds
        let values = data.Values
        c, [| for row in rows -> values.[ids.[row]] |]

    let private iterate (ev: IEval) (ctx: Ctx) (call: Call) : DaxValue array =
        let table = ev.Table(ctx, call.Args.[0])
        let expr = call.Args.[1]

        [|
            for row in 0 .. table.Count - 1 -> ev.Scalar(Context.pushRow ctx table row, expr)
        |]

    let private columnOnly (name: string) (args: Expr list) =
        match List.tryHead args with
        | Some a ->
            match (Fn.unparen a).Kind with
            | Qualified _
            | Bracketed _
            | Identifier _ -> None
            | _ -> Some $"{name} with an argument that is not a column reference"
        | None -> None

    /// The numbers among the values, as floats, and how many values are BLANK. Anything else is
    /// refused, and so is a whole number a float can't hold.
    let private numbers (ctx: Ctx) (span: Span) (name: string) (values: DaxValue array) : float array * bool * int =
        let out = ResizeArray<float>()
        let mutable blanks = 0
        let mutable whole = true

        for v in values do
            match v with
            | DaxValue.Blank -> blanks <- blanks + 1
            | DaxValue.Integer i ->
                if abs (float i) >= twoTo53 then
                    Fail.notSupported ctx span $"{name} of whole numbers of more than 15 digits"

                out.Add(float i)
            | DaxValue.Decimal d ->
                if Double.IsNaN d || Double.IsInfinity d then
                    Fail.notSupported ctx span $"{name} of numbers that are not finite"

                whole <- false
                out.Add d
            | DaxValue.Currency _ ->
                Fail.notSupported ctx span $"{name} of fixed decimal numbers (the result type is not documented)"
            | DaxValue.String _ -> Fail.notSupported ctx span $"{name} of text"
            | DaxValue.Boolean _ -> Fail.notSupported ctx span $"{name} of TRUE or FALSE"
            | DaxValue.DateTime _ -> Fail.notSupported ctx span $"{name} of dates"

        out.ToArray(), whole, blanks

    [<RequireQualifiedAccess>]
    type private Rank =
        /// "For PERCENTILE.INC the calculated rank is K*(N-1)+1" (dax.guide/percentile-inc).
        | Inclusive
        /// "For PERCENTILE.EXC the calculated rank is K*(N+1)".
        | Exclusive

    /// "Both functions rank the N values from 1 (lowest) to N (highest), then determine the
    /// possibly-non-integer calculated rank for the specified percentage argument K ..., and
    /// finally use linear interpolation between the closest integer-rank values of the data
    /// array" (dax.guide/percentile-inc). "MEDIAN corresponds to PERCENTILE.INC with k=0.50".
    ///
    /// `blanks` BLANKs take part (the X forms: "PERCENTILEX.INC considers the blank values
    /// returned by Expression"; MEDIANX of { BLANK(), 2, 4, 4, 4, 5, 5, 7, 9 } is 4): they rank
    /// below the numbers when those are all positive, and the answer must not rest on one of
    /// them; otherwise the call is refused, as the pages don't say what a BLANK is worth.
    ///
    /// The result type: "When Expression returns an Integer, the resulting data type of MEDIANX
    /// is Variant (Integer for non-ties, Decimal for ties)" (dax.guide/medianx). So a median of
    /// whole numbers is that whole number when the rank is whole, and an interpolated value is a
    /// decimal number; a percentile of whole numbers at a whole rank is refused (only the median's
    /// type is documented).
    let private percentile
        (ctx: Ctx)
        (span: Span)
        (name: string)
        (rank: Rank)
        (isMedian: bool)
        (k: float)
        (values: float array, whole: bool, blanks: int)
        : DaxValue =
        if values.Length = 0 then
            // "If column is empty, BLANK() is returned"; "The result is blank in case there are no
            // rows in the table returning a non-blank value".
            DaxValue.Blank
        else
            let sorted = Array.sort values

            if blanks > 0 && sorted.[0] <= 0.0 then
                Fail.notSupported
                    ctx
                    span
                    $"{name} over BLANKs together with numbers that are not all positive (where a BLANK ranks among them is not documented)"

            let n = sorted.Length + blanks

            if Double.IsNaN k || k < 0.0 || k > 1.0 then
                // "If k is nonnumeric or outside the range 0 to 1, an error is returned".
                Fail.runtime ctx span $"The percentile value of function '{name}' must be in the range 0..1."

            let position, exact =
                match rank with
                | Rank.Inclusive -> k * float (n - 1) + 1.0, decimal k * decimal (n - 1) + 1m
                | Rank.Exclusive -> k * float (n + 1), decimal k * decimal (n + 1)

            if position < 1.0 || position > float n then
                // "Desired percentile value in the interval [1/(n+1),1-1/(n+1)]"; "If it cannot
                // interpolate for the k percentile specified, an error is returned".
                Fail.runtime ctx span $"The percentile value of function '{name}' is out of the range the data allows."

            let low = Math.Floor position
            let fraction = position - low

            if (fraction = 0.0) <> (exact = Math.Truncate exact) then
                Fail.notSupported ctx span $"{name} at a percentile within a rounding error of a whole rank"

            let at (rank1: int) =
                if rank1 <= blanks then
                    Fail.notSupported
                        ctx
                        span
                        $"{name} whose result rests on a BLANK (what a BLANK is worth there is not documented)"

                sorted.[rank1 - blanks - 1]

            if fraction = 0.0 then
                let v = at (int low)

                if not whole then
                    DaxValue.Decimal v
                elif isMedian then
                    DaxValue.Integer(int64 v)
                else
                    Fail.notSupported
                        ctx
                        span
                        $"{name} of whole numbers at a percentile that is exactly one of them (whether the result is a whole or a decimal number is not documented)"
            else
                let a = at (int low)
                let b = at (int low + 1)
                DaxValue.Decimal(a + fraction * (b - a))

    let private ignoringBlanks (values: float array, whole: bool, _: int) = values, whole, 0

    let private kArg (ev: IEval) (ctx: Ctx) (call: Call) (i: int) (name: string) : float =
        Coerce.toFloat (
            Coerce.numberNotBlank ctx call.Args.[i].Span $"{name} at a percentile" (ev.Scalar(ctx, call.Args.[i]))
        )

    /// Variance: "∑(x - x̃)² / n" for a population and "∑(x - x̃)² / (n-1)" for a sample, "where x̃
    /// is the average value of x" (Microsoft Learn, VAR.P and VAR.S); the standard deviations are
    /// their square roots ("√[∑(x - x̃)²/(n-1)]": STDEV.S). "Blank rows are filtered out ... and
    /// not considered in the calculations. An error is returned if columnName contains less than
    /// 2 non-blank rows": that is the sample's division by zero for one row. The population pages
    /// carry the same sentence, though one value has a variance of zero, and no page says what no
    /// rows at all give: both are refused.
    let private variance
        (ctx: Ctx)
        (span: Span)
        (name: string)
        (sample: bool)
        (root: bool)
        (values: float array, _: bool, _: int)
        : DaxValue =
        let n = values.Length

        if n = 0 then
            Fail.notSupported ctx span $"{name} of no values"
        elif n = 1 && sample then
            Fail.runtime ctx span $"The function {name} needs at least two values that are not BLANK."
        elif n = 1 then
            Fail.notSupported ctx span $"{name} of a single value (the page says an error; the formula gives zero)"
        else
            let mean = Array.sum values / float n
            let squares = values |> Array.sumBy (fun x -> (x - mean) * (x - mean))
            let v = squares / float (if sample then n - 1 else n)
            let r = if root then sqrt v else v

            if Double.IsNaN r || Double.IsInfinity r then
                Fail.notSupported ctx span $"{name} whose result is not a finite number"

            DaxValue.Decimal r

    /// "Return a decimal number with the geometric mean"; "Only the numbers in the column are
    /// counted. Blanks, logical values, and text are ignored" (Microsoft Learn, GEOMEAN). The
    /// geometric mean of { 2, 4, 4, 4, 5, 5, 7, 9 } is 4.60 (dax.guide/geomean). Numbers that are
    /// not positive, and no numbers at all, are not documented.
    let private geomean (ctx: Ctx) (span: Span) (name: string) (values: float array, _: bool, _: int) : DaxValue =
        if values.Length = 0 then
            Fail.notSupported ctx span $"{name} of no values"

        if values |> Array.exists (fun x -> x <= 0.0) then
            Fail.notSupported ctx span $"{name} of numbers that are not all positive"

        DaxValue.Decimal(exp ((values |> Array.sumBy log) / float values.Length))

    let private variantOf (ev: IEval) (ctx: Ctx) (arg: Expr) =
        match ev.TypeOf(ctx, arg) with
        | StaticType.Known DaxType.Decimal -> StaticType.Known DaxType.Decimal
        | StaticType.Blank -> StaticType.Blank
        | _ -> StaticType.Unknown

    let private ofColumn
        name
        family
        (minArgs, maxArgs)
        typeRule
        (impl: Ctx -> Call -> IEval -> float array * bool * int -> DaxValue)
        =
        Fn.scalar name family (minArgs, maxArgs) typeRule (fun ev ctx call ->
            let _, values = columnValues ev ctx call.Args.[0]
            impl ctx call ev (numbers ctx call.Span name values |> ignoringBlanks))
        |> Fn.withCheck (columnOnly name)

    let private ofTable
        name
        family
        (minArgs, maxArgs)
        typeRule
        (impl: Ctx -> Call -> IEval -> float array * bool * int -> DaxValue)
        =
        Fn.scalar name family (minArgs, maxArgs) typeRule (fun ev ctx call ->
            impl ctx call ev (numbers ctx call.Span name (iterate ev ctx call)))

    let private order (ctx: Ctx) (e: Expr) : bool =
        // TRUE: descending. "0/FALSE/DESC – descending; 1/TRUE/ASC – ascending. If omitted, the
        // default is descending" (dax.guide/rank-eq).
        match Fn.keyword e with
        | Some "DESC" -> true
        | Some "ASC" -> false
        | _ ->
            match (Fn.unparen e).Kind with
            | IntegerLiteral(0L, _)
            | BooleanLiteral false -> true
            | IntegerLiteral(1L, _)
            | BooleanLiteral true -> false
            | _ -> Fail.notSupported ctx e.Span "an order that is not ASC, DESC, TRUE, FALSE, 1 or 0"

    let private decimalType = Fn.typeIs DaxType.Decimal

    /// "The function MINA corresponds to MIN used with a single column as an argument for all
    /// the data types except Boolean. When MINA operates with a Boolean data type, it consider
    /// TRUE as 1 and FALSE as 0" (dax.guide/mina, and the same of MAXA). Numbers and dates only:
    /// for text the two sites differ (Microsoft Learn: "If you want to compare text values, use
    /// the MIN function"), and for a column with no value too ("The result is blank": dax.guide;
    /// "MINA returns 0 (zero)": Microsoft Learn).
    let private minMaxA (name: string) (wantMax: bool) =
        Fn.scalar
            name
            "Aggregation"
            (1, 1)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Known DaxType.Boolean -> StaticType.Known DaxType.Integer
                | StaticType.Known(DaxType.Integer | DaxType.Decimal | DaxType.Currency | DaxType.DateTime) as t -> t
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                let c, values = columnValues ev ctx call.Args.[0]
                let present = values |> Array.filter (fun v -> not (DaxValue.isBlank v))

                if present.Length = 0 then
                    Fail.notSupported ctx call.Span $"{name} of a column with no value (BLANK or 0: the pages differ)"

                match c.Data.DataType with
                | DaxType.Integer
                | DaxType.Decimal
                | DaxType.Currency
                | DaxType.DateTime -> Aggregate.extreme ctx call.Span name wantMax present
                | DaxType.Boolean ->
                    let any = present |> Array.exists (fun v -> v = DaxValue.Boolean wantMax)
                    DaxValue.Integer(if any = wantMax then 1L else 0L)
                | t -> Fail.notSupported ctx call.Span $"{name} of a {DaxType.name t} column")
        |> Fn.withCheck (columnOnly name)

    let functions: DaxFunction list = [
        // "Blanks are ignored. ... The result is blank in case there are no rows in the table
        // with a non-blank value" (dax.guide/median).
        ofColumn "MEDIAN" "Statistical" (1, 1) (fun ev ctx args -> variantOf ev ctx args.[0]) (fun ctx call _ values ->
            percentile ctx call.Span "MEDIAN" Rank.Inclusive true 0.5 values)

        // "Expressions returning blanks are relevant to MEDIANX, whereas they are ignored by
        // MEDIAN" (dax.guide/medianx).
        ofTable "MEDIANX" "Statistical" (2, 2) (fun ev ctx args -> variantOf ev ctx args.[1]) (fun ctx call _ values ->
            percentile ctx call.Span "MEDIANX" Rank.Inclusive true 0.5 values)

        ofColumn
            "PERCENTILE.INC"
            "Statistical"
            (2, 2)
            (fun ev ctx args -> variantOf ev ctx args.[0])
            (fun ctx call ev values ->
                percentile
                    ctx
                    call.Span
                    "PERCENTILE.INC"
                    Rank.Inclusive
                    false
                    (kArg ev ctx call 1 "PERCENTILE.INC")
                    values)

        ofColumn
            "PERCENTILE.EXC"
            "Statistical"
            (2, 2)
            (fun ev ctx args -> variantOf ev ctx args.[0])
            (fun ctx call ev values ->
                percentile
                    ctx
                    call.Span
                    "PERCENTILE.EXC"
                    Rank.Exclusive
                    false
                    (kArg ev ctx call 1 "PERCENTILE.EXC")
                    values)

        ofTable
            "PERCENTILEX.INC"
            "Statistical"
            (3, 3)
            (fun ev ctx args -> variantOf ev ctx args.[1])
            (fun ctx call ev values ->
                percentile
                    ctx
                    call.Span
                    "PERCENTILEX.INC"
                    Rank.Inclusive
                    false
                    (kArg ev ctx call 2 "PERCENTILEX.INC")
                    values)

        ofTable
            "PERCENTILEX.EXC"
            "Statistical"
            (3, 3)
            (fun ev ctx args -> variantOf ev ctx args.[1])
            (fun ctx call ev values ->
                percentile
                    ctx
                    call.Span
                    "PERCENTILEX.EXC"
                    Rank.Exclusive
                    false
                    (kArg ev ctx call 2 "PERCENTILEX.EXC")
                    values)

        ofColumn "STDEV.S" "Statistical" (1, 1) decimalType (fun ctx call _ v ->
            variance ctx call.Span "STDEV.S" true true v)
        ofColumn "STDEV.P" "Statistical" (1, 1) decimalType (fun ctx call _ v ->
            variance ctx call.Span "STDEV.P" false true v)
        ofColumn "VAR.S" "Statistical" (1, 1) decimalType (fun ctx call _ v ->
            variance ctx call.Span "VAR.S" true false v)
        ofColumn "VAR.P" "Statistical" (1, 1) decimalType (fun ctx call _ v ->
            variance ctx call.Span "VAR.P" false false v)

        ofTable "STDEVX.S" "Statistical" (2, 2) decimalType (fun ctx call _ v ->
            variance ctx call.Span "STDEVX.S" true true (ignoringBlanks v))

        ofTable "STDEVX.P" "Statistical" (2, 2) decimalType (fun ctx call _ v ->
            variance ctx call.Span "STDEVX.P" false true (ignoringBlanks v))

        ofTable "VARX.S" "Statistical" (2, 2) decimalType (fun ctx call _ v ->
            variance ctx call.Span "VARX.S" true false (ignoringBlanks v))

        ofTable "VARX.P" "Statistical" (2, 2) decimalType (fun ctx call _ v ->
            variance ctx call.Span "VARX.P" false false (ignoringBlanks v))

        ofColumn "GEOMEAN" "Statistical" (1, 1) decimalType (fun ctx call _ v -> geomean ctx call.Span "GEOMEAN" v)
        ofTable "GEOMEANX" "Statistical" (2, 2) decimalType (fun ctx call _ v ->
            geomean ctx call.Span "GEOMEANX" (ignoringBlanks v))

        // "AVERAGEA manages a Boolean data type as an integer, where FALSE is 0 and TRUE is 1.
        // AVERAGEA always considers a string as 0, regardless of the content of the string. ...
        // For arguments that are not Boolean, the AVERAGEA function internally executes AVERAGEX"
        // (dax.guide/averagea). "Whenever there are no rows to aggregate, the function returns a
        // blank" (Microsoft Learn). Whether a BLANK text or Boolean counts as a row is not said.
        Fn.scalar
            "AVERAGEA"
            "Aggregation"
            (1, 1)
            (fun ev ctx args ->
                match ev.TypeOf(ctx, args.[0]) with
                | StaticType.Known DaxType.Currency -> StaticType.Known DaxType.Currency
                | StaticType.Known(DaxType.Integer | DaxType.Decimal | DaxType.Boolean | DaxType.String) ->
                    StaticType.Known DaxType.Decimal
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                let c, values = columnValues ev ctx call.Args.[0]

                match c.Data.DataType with
                | DaxType.Integer
                | DaxType.Decimal
                | DaxType.Currency -> Aggregate.average ctx call.Span "AVERAGEA" values
                | DaxType.Boolean
                | DaxType.String ->
                    if values.Length = 0 then
                        DaxValue.Blank
                    elif values |> Array.exists DaxValue.isBlank then
                        Fail.notSupported
                            ctx
                            call.Span
                            "AVERAGEA of a text or TRUE/FALSE column with BLANKs (whether a BLANK counts is not documented)"
                    else
                        let ones =
                            values
                            |> Array.sumBy (fun v ->
                                match v with
                                | DaxValue.Boolean true -> 1.0
                                | _ -> 0.0)

                        DaxValue.Decimal(ones / float values.Length)
                | t -> Fail.notSupported ctx call.Span $"AVERAGEA of a {DaxType.name t} column")
        |> Fn.withCheck (columnOnly "AVERAGEA")

        minMaxA "MINA" false
        minMaxA "MAXA" true

        // RANK.EQ ( value, table[column], order ) "correspond[s] to" RANKX ( SELECTCOLUMNS (
        // table, table[column] ), table[column], value, order ) (dax.guide/rank-eq): one more than
        // the number of rows whose value is greater (descending, the default) or smaller. "If
        // value is not in columnName or value is a blank, then RANK.EQ returns a blank value.
        // Duplicate values of value receive the same rank value" (Microsoft Learn, RANK.EQ).
        // Numbers only, and no BLANK in the column: how a BLANK ranks is not documented.
        Fn.scalar "RANK.EQ" "Statistical" (2, 3) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let value = ev.Scalar(ctx, call.Args.[0])
            let c, values = columnValues ev ctx call.Args.[1]

            let descending =
                match List.tryItem 2 call.Args with
                | Some o when not (Fn.isOmitted o) -> order ctx o
                | _ -> true

            match c.Data.DataType with
            | DaxType.Integer
            | DaxType.Decimal
            | DaxType.Currency -> ()
            | t -> Fail.notSupported ctx call.Span $"RANK.EQ in a {DaxType.name t} column"

            let blankRowShown =
                c.Table.HasBlankRow
                && (Filters.visible ctx.Filter c.Table).Contains c.Table.RowCount

            if blankRowShown || values |> Array.exists DaxValue.isBlank then
                Fail.notSupported ctx call.Span "RANK.EQ in a column with BLANKs (how a BLANK ranks is not documented)"

            match value with
            | DaxValue.Blank -> DaxValue.Blank
            | DaxValue.Integer _
            | DaxValue.Decimal _
            | DaxValue.Currency _ ->
                let coll = ctx.Model.Collation
                let mutable found = false
                let mutable ahead = 0L

                for v in values do
                    let cmp = Fail.dax ctx call.Span (DaxValue.compare coll v value)

                    if cmp = 0 then
                        found <- true
                    elif (descending && cmp > 0) || (not descending && cmp < 0) then
                        ahead <- ahead + 1L

                if found then
                    DaxValue.Integer(ahead + 1L)
                else
                    DaxValue.Blank
            | _ -> Fail.notSupported ctx call.Args.[0].Span "RANK.EQ of a value that is not a number")
        |> Fn.withCheck (fun args ->
            match args with
            | _ :: column :: _ -> columnOnly "RANK.EQ" [ column ]
            | _ -> None)
    ]
