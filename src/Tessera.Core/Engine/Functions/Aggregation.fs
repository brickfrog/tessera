namespace Tessera.DaxEngine.Functions

// Aggregations over a column (SUM, AVERAGE, MIN, MAX, COUNT...) and over a table with an
// expression (the X functions). The column forms are the X forms over the table of the column:
// "SUM ( table[column] ) corresponds to SUMX ( table, table[column] )" (dax.guide/sum, and the same
// sentence on the pages of AVERAGE, MIN, MAX, COUNT and COUNTA).

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// Folding values the way the aggregations do.
module Aggregate =

    let private typeName (v: DaxValue) =
        match DaxValue.typeOf v with
        | Some t -> DaxType.name t
        | None -> "BLANK"

    /// SUMX: BLANKs are skipped; no value at all is BLANK. Numbers only: what a sum of text, dates
    /// or Booleans is isn't documented, so it is refused.
    let sum (ctx: Ctx) (span: Span) (name: string) (values: DaxValue seq) : DaxValue =
        let mutable total = DaxValue.Blank

        for v in values do
            match v with
            | DaxValue.Blank -> ()
            | DaxValue.Integer _
            | DaxValue.Decimal _
            | DaxValue.Currency _ -> total <- Fail.dax ctx span (DaxValue.add total v)
            | _ -> Fail.notSupported ctx span $"{name} of {typeName v} values"

        total

    /// AVERAGEX: "ignores the blank values ... A value of zero is included ... When Table has no
    /// rows, or Expression returns BLANK for all the rows, the function returns BLANK"
    /// (dax.guide/averagex). The result is a Currency for Currency values, else a Decimal.
    let average (ctx: Ctx) (span: Span) (name: string) (values: DaxValue seq) : DaxValue =
        let mutable total = DaxValue.Blank
        let mutable n = 0L
        let mutable currency = true

        for v in values do
            match v with
            | DaxValue.Blank -> ()
            | DaxValue.Integer _
            | DaxValue.Decimal _
            | DaxValue.Currency _ ->
                (match v with
                 | DaxValue.Currency _ -> ()
                 | _ -> currency <- false)

                total <- Fail.dax ctx span (DaxValue.add total v)
                n <- n + 1L
            | _ -> Fail.notSupported ctx span $"{name} of {typeName v} values"

        if n = 0L then
            DaxValue.Blank
        elif currency then
            Fail.dax ctx span (DaxValue.divide total (DaxValue.Integer n))
        else
            let total = Fail.dax ctx span (DaxValue.convertTo DaxType.Decimal total)
            Fail.dax ctx span (DaxValue.divide total (DaxValue.Integer n))

    /// MINX / MAXX: BLANKs are skipped ("MINX ignores it": dax.guide/min); numbers and dates
    /// compare as numbers, text by the collation. Booleans, and a mix of text with numbers, are
    /// refused (the pages say MIN "ignores logical values" without saying what a column of them
    /// gives).
    let extreme (ctx: Ctx) (span: Span) (name: string) (wantMax: bool) (values: DaxValue seq) : DaxValue =
        let mutable best = DaxValue.Blank

        for v in values do
            match v with
            | DaxValue.Blank -> ()
            | DaxValue.Boolean _ -> Fail.notSupported ctx span $"{name} of BOOLEAN values"
            | _ ->
                if DaxValue.isBlank best then
                    best <- v
                else
                    let textA =
                        match best with
                        | DaxValue.String _ -> true
                        | _ -> false

                    let textB =
                        match v with
                        | DaxValue.String _ -> true
                        | _ -> false

                    if textA <> textB then
                        Fail.notSupported ctx span $"{name} of text mixed with numbers"

                    let c = Fail.dax ctx span (DaxValue.compare ctx.Model.Collation v best)

                    if (wantMax && c > 0) || (not wantMax && c < 0) then
                        best <- v

        best

    /// COUNTX / COUNTAX: the values that are not BLANK ("An empty string is a value"); none is
    /// BLANK, not 0. COUNTX "cannot" count Booleans (dax.guide/countx).
    let count (ctx: Ctx) (span: Span) (name: string) (booleans: bool) (values: DaxValue seq) : DaxValue =
        let mutable n = 0L

        for v in values do
            match v with
            | DaxValue.Blank -> ()
            | DaxValue.Boolean _ when not booleans ->
                Fail.runtime ctx span $"The function {name} cannot work with values of type Boolean."
            | _ -> n <- n + 1L

        if n = 0L then DaxValue.Blank else DaxValue.Integer n

module Aggregation =

    /// The values of a model column in the rows the filter context shows (the blank row has no
    /// value to aggregate: it isn't a row of the table).
    let private columnValues (ev: IEval) (ctx: Ctx) (call: Call) : BColumn * DaxValue seq =
        let c = ev.Column(ctx, call.Args.[0])
        let rows = Filters.visibleStored ctx.Filter c.Table
        let data = c.Data
        let ids = data.ValueIds
        let values = data.Values
        c, rows |> Seq.map (fun row -> values.[ids.[row]])

    /// How often each value id occurs in the visible rows: aggregating per distinct value is
    /// faster than per row.
    let private columnCounts (ev: IEval) (ctx: Ctx) (call: Call) : BColumn * int array =
        let c = ev.Column(ctx, call.Args.[0])
        let rows = Filters.visibleStored ctx.Filter c.Table
        let ids = c.Data.ValueIds
        let counts = Array.zeroCreate<int> c.Data.DistinctCount

        for row in rows do
            counts.[ids.[row]] <- counts.[ids.[row]] + 1

        c, counts

    let private iterate (ev: IEval) (ctx: Ctx) (call: Call) : DaxValue seq =
        let table = ev.Table(ctx, call.Args.[0])
        let expr = call.Args.[1]

        seq {
            for row in 0 .. table.Count - 1 do
                yield ev.Scalar(Context.pushRow ctx table row, expr)
        }

    let private columnOnly (name: string) (args: Expr list) =
        match args with
        | [ a ] ->
            match (Fn.unparen a).Kind with
            | Qualified _
            | Bracketed _
            | Identifier _ -> None
            | _ -> Some $"{name} with an argument that is not a column reference"
        | _ -> None

    let private numericOf (ev: IEval) (ctx: Ctx) (arg: Expr) =
        match ev.TypeOf(ctx, arg) with
        | StaticType.Known(DaxType.Integer | DaxType.Decimal | DaxType.Currency) as t -> t
        | StaticType.Blank -> StaticType.Blank
        | _ -> StaticType.Unknown

    let private averageType (ev: IEval) (ctx: Ctx) (arg: Expr) =
        match ev.TypeOf(ctx, arg) with
        | StaticType.Known DaxType.Currency -> StaticType.Known DaxType.Currency
        | StaticType.Known(DaxType.Integer | DaxType.Decimal) -> StaticType.Known DaxType.Decimal
        | StaticType.Blank -> StaticType.Blank
        | _ -> StaticType.Unknown

    let private sumColumn (ev: IEval) (ctx: Ctx) (call: Call) : DaxValue =
        let c, counts = columnCounts ev ctx call
        let values = c.Data.Values

        match c.Data.DataType with
        | DaxType.Decimal ->
            // Row order doesn't matter to the engine either; summing per value keeps it fast.
            let mutable total = 0.0
            let mutable any = false

            for id in 0 .. counts.Length - 1 do
                if counts.[id] > 0 then
                    match values.[id] with
                    | DaxValue.Decimal d ->
                        total <- total + d * float counts.[id]
                        any <- true
                    | _ -> ()

            if any then DaxValue.Decimal total else DaxValue.Blank
        | DaxType.Integer
        | DaxType.Currency ->
            let mutable total = Int128.Zero
            let mutable any = false

            for id in 0 .. counts.Length - 1 do
                if counts.[id] > 0 then
                    match values.[id] with
                    | DaxValue.Integer i
                    | DaxValue.Currency i ->
                        total <- total + Int128.op_Implicit i * Int128.op_Implicit counts.[id]
                        any <- true
                    | _ -> ()

            if not any then
                DaxValue.Blank
            elif
                total > Int128.op_Implicit Int64.MaxValue
                || total < Int128.op_Implicit Int64.MinValue
            then
                Fail.runtime ctx call.Span (DaxError.message (Overflow("SUM", c.Data.DataType)))
            elif c.Data.DataType = DaxType.Integer then
                DaxValue.Integer(Int128.op_Explicit total: int64)
            else
                DaxValue.Currency(Int128.op_Explicit total: int64)
        | t -> Fail.notSupported ctx call.Span $"SUM of a {DaxType.name t} column"

    let private twoScalars (ev: IEval) (ctx: Ctx) (call: Call) (wantMax: bool) : DaxValue =
        // dax.guide/min: SWITCH ( TRUE, v1 = v2, IF ( ISBLANK ( v1 ), v2, v1 ), v1 < v2, v1, v2 ),
        // and the same with > for MAX: here BLANK counts, as the other side's zero.
        let v1 = ev.Scalar(ctx, call.Args.[0])
        let v2 = ev.Scalar(ctx, call.Args.[1])
        let c = Fail.dax ctx call.Span (DaxValue.compare ctx.Model.Collation v1 v2)

        if c = 0 then (if DaxValue.isBlank v1 then v2 else v1)
        elif (wantMax && c > 0) || (not wantMax && c < 0) then v1
        else v2

    let private minMax (name: string) (wantMax: bool) =
        Fn.scalar
            name
            "Aggregation"
            (1, 2)
            (fun ev ctx args ->
                match args with
                | [ a ] -> ev.TypeOf(ctx, a)
                | [ a; b ] -> Typing.unify (ev.TypeOf(ctx, a)) (ev.TypeOf(ctx, b))
                | _ -> StaticType.Unknown)
            (fun ev ctx call ->
                if call.Args.Length = 2 then
                    twoScalars ev ctx call wantMax
                else
                    let c, counts = columnCounts ev ctx call
                    let values = c.Data.Values

                    Aggregate.extreme
                        ctx
                        call.Span
                        name
                        wantMax
                        (seq {
                            for id in 0 .. counts.Length - 1 do
                                if counts.[id] > 0 then
                                    yield values.[id]
                        }))
        |> Fn.withCheck (fun args -> if args.Length = 1 then columnOnly name args else None)

    let private order (ctx: Ctx) (e: Expr) : bool =
        // TRUE: descending.
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

    let functions: DaxFunction list = [
        Fn.scalar "SUM" "Aggregation" (1, 1) (fun ev ctx args -> numericOf ev ctx args.[0]) sumColumn
        |> Fn.withCheck (columnOnly "SUM")

        Fn.scalar "AVERAGE" "Aggregation" (1, 1) (fun ev ctx args -> averageType ev ctx args.[0]) (fun ev ctx call ->
            let c, values = columnValues ev ctx call

            match c.Data.DataType with
            | DaxType.Integer
            | DaxType.Decimal
            | DaxType.Currency -> Aggregate.average ctx call.Span "AVERAGE" values
            | DaxType.String ->
                // "With a column of type String, AVERAGE returns an error" (dax.guide/average).
                Fail.runtime ctx call.Span "The function AVERAGE cannot work with values of type String."
            | t -> Fail.notSupported ctx call.Span $"AVERAGE of a {DaxType.name t} column")
        |> Fn.withCheck (columnOnly "AVERAGE")

        minMax "MIN" false
        minMax "MAX" true

        Fn.scalar "COUNT" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let c, counts = columnCounts ev ctx call

            if c.Data.DataType = DaxType.Boolean then
                // "COUNT returns an error on a Boolean column" (dax.guide/count).
                Fail.runtime ctx call.Span "The function COUNT cannot work with values of type Boolean."

            let blank = c.Data.BlankId
            let mutable n = 0L

            for id in 0 .. counts.Length - 1 do
                if id <> blank then
                    n <- n + int64 counts.[id]

            if n = 0L then DaxValue.Blank else DaxValue.Integer n)
        |> Fn.withCheck (columnOnly "COUNT")

        Fn.scalar "COUNTA" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let c, counts = columnCounts ev ctx call
            let blank = c.Data.BlankId
            let mutable n = 0L

            for id in 0 .. counts.Length - 1 do
                if id <> blank then
                    n <- n + int64 counts.[id]

            if n = 0L then DaxValue.Blank else DaxValue.Integer n)
        |> Fn.withCheck (columnOnly "COUNTA")

        // "Any empty string is considered as a blank for COUNTBLANK purposes ... The function never
        // returns 0" (dax.guide/countblank).
        Fn.scalar "COUNTBLANK" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let c, counts = columnCounts ev ctx call
            let values = c.Data.Values
            let mutable n = 0L

            for id in 0 .. counts.Length - 1 do
                match values.[id] with
                | DaxValue.Blank
                | DaxValue.String "" -> n <- n + int64 counts.[id]
                | _ -> ()

            if n = 0L then DaxValue.Blank else DaxValue.Integer n)
        |> Fn.withCheck (columnOnly "COUNTBLANK")

        // "If the table has no rows, COUNTROWS returns BLANK, not 0" (dax.guide/countrows).
        Fn.scalar "COUNTROWS" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let n =
                match ev.TryModelTable(ctx, call.Args.[0]) with
                | Some t -> (Filters.visibleStored ctx.Filter t).Length
                | None -> ev.Table(ctx, call.Args.[0]).Count

            if n = 0 then DaxValue.Blank else DaxValue.Integer(int64 n))

        // "DISTINCTCOUNT ( table[column] ) corresponds to COUNTROWS ( DISTINCT ( table[column] ) )"
        // and "counts the BLANK value" (dax.guide/distinctcount, Microsoft Learn DISTINCTCOUNT).
        Fn.scalar "DISTINCTCOUNT" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let _, counts = columnCounts ev ctx call
            let n = counts |> Array.sumBy (fun k -> if k > 0 then 1L else 0L)
            if n = 0L then DaxValue.Blank else DaxValue.Integer n)
        |> Fn.withCheck (columnOnly "DISTINCTCOUNT")

        Fn.scalar "DISTINCTCOUNTNOBLANK" "Aggregation" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let c, counts = columnCounts ev ctx call
            let blank = c.Data.BlankId
            let mutable n = 0L

            for id in 0 .. counts.Length - 1 do
                if counts.[id] > 0 && id <> blank then
                    n <- n + 1L

            if n = 0L then DaxValue.Blank else DaxValue.Integer n)
        |> Fn.withCheck (columnOnly "DISTINCTCOUNTNOBLANK")

        Fn.scalar "SUMX" "Aggregation" (2, 2) (fun ev ctx args -> numericOf ev ctx args.[1]) (fun ev ctx call ->
            Aggregate.sum ctx call.Span "SUMX" (iterate ev ctx call))

        Fn.scalar "AVERAGEX" "Aggregation" (2, 2) (fun ev ctx args -> averageType ev ctx args.[1]) (fun ev ctx call ->
            Aggregate.average ctx call.Span "AVERAGEX" (iterate ev ctx call))

        Fn.scalar "MINX" "Aggregation" (2, 2) (Fn.typeOfArg 1) (fun ev ctx call ->
            Aggregate.extreme ctx call.Span "MINX" false (iterate ev ctx call))

        Fn.scalar "MAXX" "Aggregation" (2, 2) (Fn.typeOfArg 1) (fun ev ctx call ->
            Aggregate.extreme ctx call.Span "MAXX" true (iterate ev ctx call))

        Fn.scalar "COUNTX" "Aggregation" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            Aggregate.count ctx call.Span "COUNTX" false (iterate ev ctx call))

        Fn.scalar "COUNTAX" "Aggregation" (2, 2) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            Aggregate.count ctx call.Span "COUNTAX" true (iterate ev ctx call))

        // "PRODUCTX ignores blank expressions"; the result is "a single decimal value"
        // (dax.guide/productx).
        Fn.scalar "PRODUCTX" "Aggregation" (2, 2) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let mutable product = 1.0
            let mutable any = false

            for v in iterate ev ctx call do
                match v with
                | DaxValue.Blank -> ()
                | DaxValue.Integer _
                | DaxValue.Decimal _
                | DaxValue.Currency _ ->
                    match Fail.dax ctx call.Span (DaxValue.convertTo DaxType.Decimal v) with
                    | DaxValue.Decimal d ->
                        product <- product * d
                        any <- true
                    | _ -> ()
                | _ -> Fail.notSupported ctx call.Span "PRODUCTX of values that are not numbers"

            if any then DaxValue.Decimal product else DaxValue.Blank)

        // "A BLANK value of Expression becomes an empty string, but CONCATENATEX still adds the
        // delimiter"; "The result is BLANK when Table has no rows"; "Delimiter is not evaluated in
        // the row context of Table" (dax.guide/concatenatex). Without an order-by the order is not
        // defined by the engine, so that form is refused.
        Fn.scalar "CONCATENATEX" "Aggregation" (2, -1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])

            let delimiter =
                match List.tryItem 2 call.Args with
                | Some d when not (Fn.isOmitted d) -> DaxValue.toText (ev.Scalar(ctx, d))
                | _ -> ""

            // Order-by expressions, each optionally followed by its order.
            let keys =
                let rest = call.Args |> List.skip (min 3 call.Args.Length)

                let rec read (items: Expr list) =
                    match items with
                    | [] -> []
                    | key :: next :: tail when
                        (Fn.keyword next).IsSome
                        || (match (Fn.unparen next).Kind with
                            | IntegerLiteral _
                            | BooleanLiteral _ -> true
                            | _ -> false)
                        ->
                        (key, order ctx next) :: read tail
                    | key :: tail -> (key, false) :: read tail

                read rest

            if table.Count = 0 then
                DaxValue.Blank
            elif keys.IsEmpty && table.Count > 1 then
                Fail.notSupported
                    ctx
                    call.Span
                    "CONCATENATEX without an order-by expression over more than one row (the engine doesn't define the order)"
            else
                let rows =
                    Array.init table.Count (fun row ->
                        let inner = Context.pushRow ctx table row
                        let text = DaxValue.toText (ev.Scalar(inner, call.Args.[1]))
                        let sortBy = keys |> List.map (fun (k, _) -> ev.Scalar(inner, k)) |> Array.ofList
                        text, sortBy)

                let descending = keys |> List.map snd |> Array.ofList

                let compare (_, a: DaxValue array) (_, b: DaxValue array) =
                    let mutable c = 0
                    let mutable i = 0

                    while c = 0 && i < a.Length do
                        c <- DaxValue.sortCompare ctx.Model.Collation a.[i] b.[i]

                        if descending.[i] then
                            c <- -c

                        i <- i + 1

                    c

                let sorted = List.sortWith compare (List.ofArray rows)

                // Rows that tie on every key have no defined order among them.
                let ties =
                    sorted
                    |> List.pairwise
                    |> List.exists (fun (a, b) -> compare a b = 0 && fst a <> fst b)

                if ties then
                    Fail.notSupported
                        ctx
                        call.Span
                        "CONCATENATEX over rows that tie on the order-by expressions with different texts"

                DaxValue.String(String.Join(delimiter, sorted |> List.map fst)))

        // dax.guide/rankx and Microsoft Learn RANKX: the expression is evaluated for each row of
        // the table; the value (the expression again when omitted) in the context of the call;
        // "If expression or value evaluates to BLANK it is treated as a 0 (zero) for all expressions
        // that result in a number, or as an empty text"; order DESC and ties SKIP by default.
        Fn.scalar "RANKX" "Aggregation" (2, 5) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            let arg i =
                match List.tryItem i call.Args with
                | Some a when not (Fn.isOmitted a) -> Some a
                | _ -> None

            let table = ev.Table(ctx, call.Args.[0])
            let expr = call.Args.[1]

            let descending =
                match arg 3 with
                | None -> true
                | Some o -> order ctx o

            let dense =
                match arg 4 with
                | None -> false
                | Some t ->
                    match Fn.keyword t, (Fn.unparen t).Kind with
                    | Some "SKIP", _ -> false
                    | Some "DENSE", _ -> true
                    | _, StringLiteral s when s.ToUpperInvariant() = "SKIP" -> false
                    | _, StringLiteral s when s.ToUpperInvariant() = "DENSE" -> true
                    | _ -> Fail.invalid ctx t.Span "the ties argument of RANKX is SKIP or DENSE"

            let values =
                Array.init table.Count (fun row -> ev.Scalar(Context.pushRow ctx table row, expr))

            let value =
                match arg 2 with
                | Some v -> ev.Scalar(ctx, v)
                | None -> ev.Scalar(ctx, expr)

            let all = Array.append values [| value |]

            let text =
                all
                |> Array.exists (fun v ->
                    match v with
                    | DaxValue.String _ -> true
                    | _ -> false)

            let numbers =
                all
                |> Array.exists (fun v ->
                    match v with
                    | DaxValue.String _
                    | DaxValue.Blank -> false
                    | _ -> true)

            if text && numbers then
                Fail.notSupported ctx call.Span "RANKX over text mixed with numbers"

            if
                all
                |> Array.exists (fun v ->
                    match v with
                    | DaxValue.Boolean _ -> true
                    | _ -> false)
            then
                Fail.notSupported ctx call.Span "RANKX over BOOLEAN values"

            let zero = if text then DaxValue.String "" else DaxValue.Integer 0L
            let norm (v: DaxValue) = if DaxValue.isBlank v then zero else v
            let coll = ctx.Model.Collation
            let value = norm value

            let ahead (v: DaxValue) =
                let c = Fail.dax ctx call.Span (DaxValue.compare coll (norm v) value)
                if descending then c > 0 else c < 0

            let before = values |> Array.filter ahead

            let rank =
                if dense then
                    let distinct = ResizeArray<DaxValue>()

                    for v in before do
                        let v = norm v

                        if not (distinct.Exists(fun d -> Fail.dax ctx call.Span (DaxValue.compare coll d v) = 0)) then
                            distinct.Add v

                    distinct.Count + 1
                else
                    before.Length + 1

            DaxValue.Integer(int64 rank))
    ]
