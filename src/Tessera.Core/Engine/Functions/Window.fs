namespace Tessera.DaxEngine.Functions

// The window functions: INDEX, OFFSET, WINDOW, RANK and ROWNUMBER, with their ORDERBY,
// PARTITIONBY and MATCHBY clauses.
//
// Sources: Microsoft Learn's page for each function and "Understanding ORDERBY, PARTITIONBY, and
// MATCHBY functions" (learn.microsoft.com/dax/best-practices/dax-understand-orderby); dax.guide's
// pages; SQLBI, "Introducing window functions in DAX", "Introducing the RANK window function in
// DAX" and "Understanding apply semantics for window functions in DAX".
//
// How a call is evaluated:
//
//   1. The relation is evaluated, partitioned by the PARTITIONBY columns and each partition sorted
//      by the ORDERBY keys.
//   2. The current row is found by "apply semantics" (SQLBI): every model column of the relation is
//      matched with its value in the row context ("If multiple nested row contexts exist within the
//      same column, the engine uses the innermost row context"), or, if no row context has it, with
//      its VALUES in the filter context; "The full CROSSJOIN of all the possible values of all
//      columns is generated (there is no auto-exist)". The relation's rows that match are the
//      current rows (the apply table), and the function's results for each are put together:
//      "DISTINCT ( SELECTCOLUMNS ( GENERATE ( ApplyTable, <Result of Window Function> ), <All
//      columns in the Source table> ) )".
//   3. The sources differ on which columns take part in the matching: SQLBI says every model
//      column of the relation (the MATCHBY columns when given); Learn says the same for OFFSET and
//      WINDOW but only the PARTITIONBY (and MATCHBY) columns for INDEX and for a WINDOW whose ends
//      are both ABS, and the ORDERBY, PARTITIONBY and MATCHBY columns for RANK and ROWNUMBER;
//      dax.guide says the ORDERBY and PARTITIONBY columns for OFFSET; with MATCHBY, Learn says the
//      MATCHBY and PARTITIONBY columns. Each reading is evaluated, and the call is refused when
//      they don't agree.
//
// What is refused rather than guessed:
//   - an omitted relation whose ORDERBY and PARTITIONBY arguments are not columns of one table
//     (otherwise it is ALLSELECTED of them: see `defaultRelation`), the reset argument and an
//     axis (visual calculations only);
//   - rows that tie on every ORDERBY key, where the result depends on their order: the engine
//     "will add as many columns as needed to ORDERBY in order to ensure each row can be uniquely
//     identified", but "The way INDEX chooses the columns to add to the ORDERBY section ... is not
//     documented" (SQLBI). A result that is the same whichever way the ties are broken is given;
//   - an omitted ORDERBY over several columns: it "automatically contains all columns from the
//     relation that aren't in PARTITIONBY", but "no guarantee is given as to what order in which
//     they'll be added" (Learn). Every order is tried, and the call is refused if they disagree;
//   - a column that two nested row contexts both have: Learn says "If there is more than one
//     corresponding outer column, an error is returned", SQLBI that the innermost one is used;
//   - BLANK and values of different types in an ORDERBY key, as `Sorting.check` refuses them
//     (BLANKS FIRST and LAST place BLANK themselves);
//   - RELATED and related tables' columns in PARTITIONBY and MATCHBY, ORDERBY columns that aren't
//     the relation's.
//
// Documented errors: a relation whose rows the ORDERBY and PARTITIONBY columns can't tell apart
// and no other columns can ("OFFSET's Relation parameter may have duplicate rows, which isn't
// allowed", Learn), which for a model table means one without a key or unique column ("MATCHBY is
// required here because the relation, FactInternetSales, doesn't contain any explicit keys or
// unique columns"); MATCHBY and PARTITIONBY columns that don't identify the rows; a relation with
// no model column for OFFSET and WINDOW. Whether IFERROR catches these is not documented; they are
// raised as DAX errors.

open System
open System.Collections.Generic
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Window =

    // ---- Arguments ---------------------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type private Blanks =
        | Default
        | First
        | Last

    /// The kind of a trailing argument, in the order the syntax gives them:
    /// `[relation][, orderBy][, blanks][, partitionBy][, matchBy][, reset]`. Learn's own examples
    /// leave out the ones between (`OFFSET ( -1, vRelation, ORDERBY ( ... ), PARTITIONBY ( ... ) )`),
    /// so an argument is told by what it is, not by where it is.
    [<RequireQualifiedAccess>]
    type private Slot =
        | Relation of Expr
        | Axis
        | OrderBy of Expr list
        | Blanks of Blanks
        | PartitionBy of Expr list
        | MatchBy of Expr list
        | Reset

    let private slotIndex (s: Slot) =
        match s with
        | Slot.Relation _
        | Slot.Axis -> 0
        | Slot.OrderBy _ -> 1
        | Slot.Blanks _ -> 2
        | Slot.PartitionBy _ -> 3
        | Slot.MatchBy _ -> 4
        | Slot.Reset -> 5

    let private callNamed (name: string) (e: Expr) : Expr list option =
        match (Fn.unparen e).Kind with
        | Call(n, args) when String.Equals(n.Name, name, StringComparison.OrdinalIgnoreCase) -> Some args
        | _ -> None

    let private classify (e: Expr) : Slot option =
        if Fn.isOmitted e then
            None
        else
            match callNamed "ORDERBY" e, callNamed "PARTITIONBY" e, callNamed "MATCHBY" e with
            | Some args, _, _ -> Some(Slot.OrderBy args)
            | _, Some args, _ -> Some(Slot.PartitionBy args)
            | _, _, Some args -> Some(Slot.MatchBy args)
            | _ ->
                match Fn.keyword e, (Fn.unparen e).Kind with
                | Some("DEFAULT" | "KEEP"), _ -> Some(Slot.Blanks Blanks.Default)
                | Some "FIRST", _ -> Some(Slot.Blanks Blanks.First)
                | Some "LAST", _ -> Some(Slot.Blanks Blanks.Last)
                | Some("NONE" | "LOWESTPARENT" | "HIGHESTPARENT"), _ -> Some Slot.Reset
                | Some("ROWS" | "COLUMNS" | "ROWS COLUMNS" | "COLUMNS ROWS"), _ -> Some Slot.Axis
                | _, IntegerLiteral _
                | _, Unary(UnaryOp.Minus, { Kind = IntegerLiteral _ }) -> Some Slot.Reset
                | _ -> Some(Slot.Relation e)

    type private Kind =
        | Index
        | Offset
        | Window of fromAbs: bool * toAbs: bool
        | Rank of dense: bool
        | RowNumber

    /// The arguments before the common ones, and the common ones.
    type private Parsed = {
        Kind: Kind
        /// INDEX's position, OFFSET's delta, WINDOW's from and to.
        Numbers: Expr list
        Trailing: Expr list
    }

    let private isType (e: Expr) =
        match Fn.keyword e with
        | Some("ABS" | "REL") -> true
        | _ -> false

    let private isAbs (e: Expr option) =
        match e |> Option.bind Fn.keyword with
        | Some "ABS" -> true
        | _ -> false

    /// Splits the arguments of a window function; Error is invalid DAX (with where).
    let private parse (name: string) (args: Expr list) : Result<Parsed, Span option * string> =
        match name, args with
        | "INDEX", position :: rest ->
            Ok {
                Kind = Index
                Numbers = [ position ]
                Trailing = rest
            }
        | "OFFSET", delta :: rest ->
            Ok {
                Kind = Offset
                Numbers = [ delta ]
                Trailing = rest
            }
        | "WINDOW", from :: rest ->
            // WINDOW ( from[, from_type], to[, to_type], ... ): a type is ABS or REL (REL when left
            // out).
            let fromType, rest =
                match rest with
                | t :: tail when isType t || Fn.isOmitted t -> Some t, tail
                | _ -> None, rest

            match rest with
            | [] -> Error(None, "WINDOW needs a from and a to")
            | toArg :: rest ->
                let toType, rest =
                    match rest with
                    | t :: tail when isType t || Fn.isOmitted t -> Some t, tail
                    | _ -> None, rest

                Ok {
                    Kind = Window(isAbs fromType, isAbs toType)
                    Numbers = [ from; toArg ]
                    Trailing = rest
                }
        | "RANK", first :: rest when
            Fn.isOmitted first
            || Fn.keyword first = Some "DENSE"
            || Fn.keyword first = Some "SKIP"
            ->
            Ok {
                Kind = Rank(Fn.keyword first = Some "DENSE")
                Numbers = []
                Trailing = rest
            }
        | "RANK", _ ->
            Ok {
                Kind = Rank false
                Numbers = []
                Trailing = args
            }
        | "ROWNUMBER", _ ->
            Ok {
                Kind = RowNumber
                Numbers = []
                Trailing = args
            }
        | _ -> Error(None, $"{name} needs more arguments")

    type private Clauses = {
        Relation: Expr option
        OrderBy: Expr list option
        Blanks: Blanks
        PartitionBy: Expr list
        MatchBy: Expr list option
        /// A reset argument or an axis: visual calculations only.
        Visual: bool
    }

    let private clauses (name: string) (trailing: Expr list) : Result<Clauses, Span option * string> =
        let mutable last = -1
        let mutable error = None

        let mutable c = {
            Relation = None
            OrderBy = None
            Blanks = Blanks.Default
            PartitionBy = []
            MatchBy = None
            Visual = false
        }

        for e in trailing do
            match classify e with
            | None -> last <- last + 1
            | Some s ->
                let i = slotIndex s

                if i <= last && error.IsNone then
                    error <- Some(Some e.Span, $"this argument of {name} is out of place")

                last <- max last i

                match s with
                | Slot.Relation r -> c <- { c with Relation = Some r }
                | Slot.Axis
                | Slot.Reset -> c <- { c with Visual = true }
                | Slot.OrderBy keys -> c <- { c with OrderBy = Some keys }
                | Slot.Blanks b -> c <- { c with Blanks = b }
                | Slot.PartitionBy cols -> c <- { c with PartitionBy = cols }
                | Slot.MatchBy cols -> c <- { c with MatchBy = Some cols }

        match error with
        | Some e -> Error e
        | None -> Ok c

    /// One ORDERBY key: the expression, whether descending, and its own BLANKS handling.
    type private Key = {
        Expr: Expr
        Descending: bool
        Blanks: Blanks option
    }

    /// "OrderDirection [BlankHandling]": ASC or DESC (also 1/TRUE and 0/FALSE), then optionally
    /// BLANKS DEFAULT, BLANKS FIRST or BLANKS LAST (Learn, ORDERBY).
    let private order (e: Expr) : Result<bool * Blanks option, string> option =
        match Fn.keyword e, (Fn.unparen e).Kind with
        | _, (IntegerLiteral(0L, _) | BooleanLiteral false) -> Some(Ok(true, None))
        | _, (IntegerLiteral(1L, _) | BooleanLiteral true) -> Some(Ok(false, None))
        | Some k, _ ->
            let words = k.Split(' ') |> List.ofArray

            let direction =
                match words with
                | "ASC" :: _ -> Some false
                | "DESC" :: _ -> Some true
                | _ -> None

            match direction with
            | None -> None
            | Some d ->
                match List.tail words with
                | [] -> Some(Ok(d, None))
                | [ "BLANKS"; "DEFAULT" ] -> Some(Ok(d, Some Blanks.Default))
                | [ "BLANKS"; "FIRST" ] -> Some(Ok(d, Some Blanks.First))
                | [ "BLANKS"; "LAST" ] -> Some(Ok(d, Some Blanks.Last))
                | _ -> Some(Error $"an ORDERBY order is ASC or DESC, then BLANKS DEFAULT, FIRST or LAST: not {k}")
        | _ -> None

    let private keys (args: Expr list) : Result<Key list, string> =
        let rec read (items: Expr list) =
            match items with
            | [] -> Ok []
            | e :: o :: tail when Fn.isOmitted o ->
                read tail
                |> Result.map (fun rest ->
                    {
                        Expr = e
                        Descending = false
                        Blanks = None
                    }
                    :: rest)
            | e :: o :: tail when (order o).IsSome ->
                match (order o).Value with
                | Error m -> Error m
                | Ok(d, b) ->
                    read tail
                    |> Result.map (fun rest -> { Expr = e; Descending = d; Blanks = b } :: rest)
            | e :: tail ->
                read tail
                |> Result.map (fun rest ->
                    {
                        Expr = e
                        Descending = false
                        Blanks = None
                    }
                    :: rest)

        read args

    // ---- The relation an omitted one stands for ----------------------------------------------------

    /// An omitted relation. "If omitted: ... Defaults to ALLSELECTED() of all columns in orderBy
    /// and partitionBy" (Learn), and "All orderBy and partitionBy expressions must be fully
    /// qualified column names and come from a single table": the relation is
    /// `ALLSELECTED ( <those columns> )`, evaluated as ALLSELECTED is (which refuses what it
    /// can't settle). Other ORDERBY or PARTITIONBY arguments are refused: what the server reports
    /// for them is not documented.
    let private defaultRelation (ev: IEval) (ctx: Ctx) (span: Span) (name: string) (columns: Expr list) : TableVal =
        let cols =
            columns
            |> List.map (fun e ->
                match ev.TryColumn(ctx, e) with
                | Some c -> c
                | None ->
                    Fail.notSupported
                        ctx
                        e.Span
                        $"{name} without its relation argument and with an ORDERBY or PARTITIONBY argument that is not a column")

        match cols |> List.distinctBy (fun c -> c.Table.Id) with
        | [ _ ] -> ()
        | _ ->
            Fail.notSupported
                ctx
                span
                $"{name} without its relation argument and with ORDERBY and PARTITIONBY columns of more than one table"

        let allSelected: Expr = {
            Kind = Call({ Name = "ALLSELECTED"; Span = span }, columns)
            Span = span
        }

        ev.Table(ctx, allSelected)

    /// What `Check` refuses from the arguments alone.
    let private check (name: string) (args: Expr list) : string option =
        match parse name args with
        | Error _ -> None
        | Ok p ->
            match clauses name p.Trailing with
            | Error _ -> None
            | Ok c ->
                let related (cols: Expr list) =
                    cols |> List.exists (fun e -> (callNamed "RELATED" e).IsSome)

                if c.Visual then
                    Some $"{name} with a reset or axis argument (visual calculations only)"
                elif c.OrderBy = Some [] then
                    Some $"{name} with an empty ORDERBY ()"
                elif related c.PartitionBy || related (c.MatchBy |> Option.defaultValue []) then
                    Some $"RELATED in the PARTITIONBY or MATCHBY of {name}"
                else
                    None

    // ---- Evaluation ----------------------------------------------------------------------------------

    /// A whole number from a position, delta or bound; None for BLANK.
    let private whole (ctx: Ctx) (span: Span) (name: string) (v: DaxValue) : int64 option =
        let fractional () =
            Fail.notSupported ctx span $"a {name} that is not a whole number"

        match v with
        | DaxValue.Blank -> None
        | DaxValue.Integer i -> Some i
        | DaxValue.Decimal f when Double.IsFinite f && System.Math.Floor f = f && abs f < 1e15 -> Some(int64 f)
        | DaxValue.Currency c when c % 10000L = 0L -> Some(c / 10000L)
        | DaxValue.Decimal _
        | DaxValue.Currency _ -> fractional ()
        | _ -> Fail.notSupported ctx span $"a {name} that is not a number"

    let private sameColumn (a: ColInfo) (table: string) (name: string) =
        not (isNull a.Table)
        && String.Equals(a.Table, table, StringComparison.OrdinalIgnoreCase)
        && String.Equals(a.Name, name, StringComparison.OrdinalIgnoreCase)

    /// The relation's column a column reference names, if it is one: `T[c]` by its table and name,
    /// `[c]` by its name.
    let private relationColumn (ev: IEval) (ctx: Ctx) (r: TableVal) (what: string) (e: Expr) : int option =
        let e = Fn.unparen e

        match e.Kind with
        | Qualified(t, n) ->
            match r.Cols |> Array.tryFindIndex (fun c -> sameColumn c t.Name n) with
            | Some i -> Some i
            | None ->
                match ev.TryColumn(ctx, e) with
                | Some _ ->
                    Fail.notSupported
                        ctx
                        e.Span
                        $"a {what} column that is not a column of the relation ('{t.Name}'[{n}]: a related table's column, or one the relation names differently)"
                | None -> None
        | Bracketed n ->
            match
                r.Cols
                |> Array.indexed
                |> Array.filter (fun (_, c) -> String.Equals(c.Name, n, StringComparison.OrdinalIgnoreCase))
            with
            | [| i, _ |] -> Some i
            | [||] ->
                // Not the relation's: a measure, or a column of an outer row context (which every row
                // of the relation would share).
                match ev.TryColumn(ctx, e) with
                | Some _ ->
                    Fail.notSupported
                        ctx
                        e.Span
                        $"a {what} [{n}] that is not a column of the relation but of an outer row context"
                | None -> None
            | _ -> Fail.invalid ctx e.Span $"[{n}] names more than one column of the relation"
        | _ -> None

    let private columnsOf (ev: IEval) (ctx: Ctx) (r: TableVal) (what: string) (cols: Expr list) : int array =
        cols
        |> List.map (fun e ->
            match relationColumn ev ctx r what e with
            | Some i -> i
            | None -> Fail.invalid ctx e.Span $"{what} takes columns of the relation")
        |> Array.ofList

    /// How the possible results of one way of reading the call came out.
    type private Outcome =
        | Rows of int array
        | Value of DaxValue
        | Refused of string

    /// One partition, sorted: its rows in order, and for each position the first and last
    /// positions of the rows that tie with it on every key, and how many distinct key values come
    /// before it.
    type private Sorted = {
        Rows: int array
        GroupStart: int array
        GroupEnd: int array
        DenseRank: int array
    }

    let private sortPartition (rows: int array) (compare: int -> int -> int) : Sorted =
        let sorted = rows |> Array.sortWith compare
        let n = sorted.Length
        let start = Array.zeroCreate n
        let finish = Array.zeroCreate n
        let dense = Array.zeroCreate n
        let mutable groups = 0

        for i in 0 .. n - 1 do
            if i > 0 && compare sorted.[i - 1] sorted.[i] = 0 then
                start.[i] <- start.[i - 1]
                dense.[i] <- dense.[i - 1]
            else
                start.[i] <- i
                dense.[i] <- groups
                groups <- groups + 1

        for i in n - 1 .. -1 .. 0 do
            finish.[i] <-
                if i < n - 1 && start.[i + 1] = start.[i] then
                    finish.[i + 1]
                else
                    i

        {
            Rows = sorted
            GroupStart = start
            GroupEnd = finish
            DenseRank = dense
        }

    let private ties =
        "rows that tie on every ORDERBY key, where which comes first decides the result (the columns the engine adds to ORDERBY to tell them apart are not documented)"

    let private evaluate (ev: IEval) (ctx: Ctx) (call: Call) (name: string) : Choice<TableVal, DaxValue> =
        let invalid (span: Span option) (m: string) =
            Fail.invalid ctx (defaultArg span call.Span) m

        let p =
            match parse name call.Args with
            | Ok p -> p
            | Error(span, m) -> invalid span m

        let c =
            match clauses name p.Trailing with
            | Ok c -> c
            | Error(span, m) -> invalid span m

        match check name call.Args with
        | Some why -> Fail.notSupported ctx call.Span why
        | None -> ()

        // The numbers are evaluated once, in the context of the call.
        let numbers =
            p.Numbers
            |> List.map (fun e ->
                let what =
                    match p.Kind with
                    | Index -> "INDEX position"
                    | Offset -> "OFFSET delta"
                    | _ -> "WINDOW bound"

                whole ctx e.Span what (ev.Scalar(ctx, e)))

        let r =
            match c.Relation with
            | Some rel -> ev.Table(ctx, rel)
            | None ->
                if c.OrderBy.IsNone then
                    Fail.invalid ctx call.Span $"{name} needs ORDERBY when its relation is left out"

                let keyExprs =
                    match c.OrderBy with
                    | Some args ->
                        match keys args with
                        | Ok k -> k |> List.map _.Expr
                        | Error m -> Fail.invalid ctx call.Span m
                    | None -> []

                defaultRelation ev ctx call.Span name (keyExprs @ c.PartitionBy)

        let n = r.Count
        let partition = columnsOf ev ctx r "PARTITIONBY" c.PartitionBy

        let matchBy =
            c.MatchBy
            |> Option.map (fun cols ->
                let idx = columnsOf ev ctx r "MATCHBY" cols

                for i, e in Array.zip idx (Array.ofList cols) do
                    if isNull r.Cols.[i].Lineage then
                        Fail.invalid ctx e.Span "MATCHBY takes model columns, not columns a table function added"

                idx)

        let lineage = [|
            for i in 0 .. r.Cols.Length - 1 do
                if not (isNull r.Cols.[i].Lineage) then
                    i
        |]

        // In a calculated column of the relation's table, the columns not computed yet (the one
        // being computed among them) can't be read. They are left out of matching and of telling
        // rows apart when a computed unique column still identifies every row.
        let pending = lineage |> Array.filter (fun i -> not r.Cols.[i].Lineage.IsReady)

        if pending.Length > 0 then
            let identified =
                lineage
                |> Array.exists (fun i ->
                    let col = r.Cols.[i].Lineage

                    col.IsReady
                    && (col.IsDeclaredUnique || (col.Data.IsUnique && col.Data.BlankId < 0)))

            if not identified then
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name} in a calculated column of the table of its relation, where no computed column identifies the rows"

        let readable (i: int) = not (Array.contains i pending)

        match p.Kind with
        | Offset
        | Window _ when lineage.Length = 0 ->
            // "If all of relation's columns were added by DAX table functions, an error is returned."
            Fail.runtime ctx call.Span $"{name}: every column of the relation was added by a table function"
        | _ -> ()

        // ---- The keys -----------------------------------------------------------------------------
        let given =
            match c.OrderBy with
            | Some args ->
                match keys args with
                | Ok k -> Some k
                | Error m -> invalid None m
            | None -> None

        // A key is a column of the relation, or an expression evaluated in a row context of it.
        let keyValues (k: Key) : DaxValue array =
            match relationColumn ev ctx r "ORDERBY" k.Expr with
            | Some i -> Array.init n (fun row -> r.Get(i, row))
            | None -> Array.init n (fun row -> ev.Scalar(Context.pushRow ctx r row, k.Expr))

        let keyed: (Key * DaxValue array) array =
            match given with
            | Some ks -> ks |> List.map (fun k -> k, keyValues k) |> Array.ofList
            | None ->
                // "Defaults to ordering by every column in relation that is not already specified in
                // partitionBy" (Learn).
                if
                    r.Cols
                    |> Array.exists (fun col -> not (isNull col.Lineage) && col.Lineage.Computing)
                then
                    // "If OFFSET is used within a calculated column defined on the same table as
                    // relation, and orderBy is omitted, an error is returned."
                    Fail.runtime
                        ctx
                        call.Span
                        $"{name} without ORDERBY in a calculated column of the table of its relation"

                [|
                    for i in 0 .. r.Cols.Length - 1 do
                        if not (Array.contains i partition) then
                            {
                                Expr = { Kind = Omitted; Span = call.Span }
                                Descending = false
                                Blanks = None
                            },
                            Array.init n (fun row -> r.Get(i, row))
                |]

        // ---- Partitions ---------------------------------------------------------------------------
        let comparer = ctx.Model.Comparer

        let parts =
            let groups =
                Dictionary<DaxValue array, ResizeArray<int>>(ValueArrayComparer(comparer) :> IEqualityComparer<_>)

            let order = ResizeArray<DaxValue array>()

            for row in 0 .. n - 1 do
                let key = partition |> Array.map (fun i -> r.Get(i, row))

                match groups.TryGetValue key with
                | true, list -> list.Add row
                | _ ->
                    let list = ResizeArray<int>()
                    list.Add row
                    groups.[key] <- list
                    order.Add key

            order.ToArray() |> Array.map (fun k -> groups.[k].ToArray())

        let partOf = Array.zeroCreate<int> n

        parts
        |> Array.iteri (fun pi rows ->
            for row in rows do
                partOf.[row] <- pi)

        let blanksOf (k: Key) = defaultArg k.Blanks c.Blanks

        // Sorting where the sources leave it open is refused, partition by partition.
        for (k, values) in keyed do
            for rows in parts do
                let vs = rows |> Array.map (fun row -> values.[row])

                match blanksOf k with
                | Blanks.Default -> Sorting.check ctx call.Span name vs
                | _ -> Sorting.check ctx call.Span name (vs |> Array.filter (fun v -> not (DaxValue.isBlank v)))

                let numericTypes =
                    vs
                    |> Array.choose (fun v ->
                        match v with
                        | DaxValue.Integer _
                        | DaxValue.Decimal _
                        | DaxValue.Currency _
                        | DaxValue.DateTime _ -> DaxValue.typeOf v
                        | _ -> None)
                    |> Array.distinct

                if numericTypes.Length > 1 then
                    Fail.notSupported ctx call.Span $"{name} ordering by numbers of different data types"

        let coll = ctx.Model.Collation

        let compareKey (k: Key, values: DaxValue array) (a: int) (b: int) =
            let va = values.[a]
            let vb = values.[b]

            match blanksOf k, DaxValue.isBlank va, DaxValue.isBlank vb with
            | _, true, true -> 0
            | Blanks.First, true, false -> -1
            | Blanks.First, false, true -> 1
            | Blanks.Last, true, false -> 1
            | Blanks.Last, false, true -> -1
            | _ ->
                let c = DaxValue.sortCompare coll va vb
                if k.Descending then -c else c

        let comparerOf (order: (Key * DaxValue array) array) (a: int) (b: int) =
            let mutable c = 0
            let mutable i = 0

            while c = 0 && i < order.Length do
                c <- compareKey order.[i] a b
                i <- i + 1

            c

        // The orders to try: the keys as given, or every order of the default columns.
        let orders: (Key * DaxValue array) array list =
            match given with
            | Some _ -> [ keyed ]
            | None ->
                let rec permutations (items: 'a list) : 'a list list =
                    match items with
                    | [] -> [ [] ]
                    | _ ->
                        items
                        |> List.indexed
                        |> List.collect (fun (i, x) ->
                            let others =
                                items |> List.indexed |> List.filter (fun (j, _) -> j <> i) |> List.map snd

                            permutations others |> List.map (fun p -> x :: p))

                if keyed.Length > 4 then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"{name} without ORDERBY over more than four columns (the order in which the engine adds them is not documented)"

                permutations (List.ofArray keyed) |> List.map Array.ofList

        // ---- Rows that can't be told apart -------------------------------------------------------
        let sameRow (cols: int array) (a: int) (b: int) =
            cols
            |> Array.forall (fun i -> DaxValue.groupEquals coll (r.Get(i, a)) (r.Get(i, b)))

        let allCols = Array.init r.Cols.Length id |> Array.filter readable
        let firstOrder = List.head orders

        let sortedOnce =
            parts |> Array.map (fun rows -> sortPartition rows (comparerOf firstOrder))

        let tiedPairs = [
            for s in sortedOnce do
                for i in 1 .. s.Rows.Length - 1 do
                    if s.GroupStart.[i] <> i then
                        for j in s.GroupStart.[i] .. i - 1 do
                            s.Rows.[j], s.Rows.[i]
        ]

        let duplicate () =
            Fail.runtime ctx call.Span $"{name}'s Relation parameter may have duplicate rows, which isn't allowed"

        match matchBy with
        | Some m ->
            // "If MATCHBY is specified, the columns in MATCHBY and PARTITIONBY are used to uniquely
            // identify each row. If that's not possible, an error is returned." (Learn)
            let ids = Array.append m partition |> Array.distinct

            let seen =
                HashSet<DaxValue array>(ValueArrayComparer(comparer) :> IEqualityComparer<_>)

            for row in 0 .. n - 1 do
                if not (seen.Add(ids |> Array.map (fun i -> r.Get(i, row)))) then
                    Fail.runtime
                        ctx
                        call.Span
                        $"{name}: the MATCHBY and PARTITIONBY columns don't identify every row of the relation"
        | None ->
            let isModelTable =
                not (isNull r.BaseTable)
                && r.Cols
                   |> Array.forall (fun col ->
                       isNull col.Lineage || obj.ReferenceEquals(col.Lineage.Table, r.BaseTable))

            // A model table's rows are told apart by "explicit keys or unique columns" (Learn);
            // "When the source table is a reference to a model table, unique rows must be guaranteed
            // through a primary key" (SQLBI). A column the model declares the key or unique does;
            // one whose values just happen to be unique may or may not.
            let has (test: BColumn -> bool) =
                isModelTable
                && r.Cols |> Array.exists (fun col -> not (isNull col.Lineage) && test col.Lineage)

            let unique = has _.IsDeclaredUnique

            // Values that are unique in this data (not `BTable.UniqueColumn`, which is a
            // relationship's key: the rule for context transitions in calculated columns).
            let uniqueByData =
                has (fun col -> col.IsReady && col.Data.IsUnique && col.Data.BlankId < 0)

            match p.Kind with
            | Rank _ -> ()
            | _ when isModelTable && not unique && tiedPairs.IsEmpty ->
                // Learn's example fails because the table "doesn't contain any explicit keys or
                // unique columns", where the ORDERBY and PARTITIONBY columns certainly tie; whether
                // it also fails when they happen not to is not said.
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name} over a model table without a key or unique column and without MATCHBY (Learn: such a relation \"may have duplicate rows\")"
            | _ when tiedPairs.IsEmpty -> ()
            | _ ->
                if isModelTable then
                    if not unique then
                        if uniqueByData then
                            Fail.notSupported
                                ctx
                                call.Span
                                $"{name} over a model table whose only unique column is not declared its key or unique (whether the engine uses it to tell rows apart is not documented)"
                        else
                            duplicate ()
                else
                    for (a, b) in tiedPairs do
                        if sameRow allCols a b then
                            duplicate ()
                        elif sameRow (lineage |> Array.filter readable) a b then
                            Fail.notSupported
                                ctx
                                call.Span
                                $"{name} over rows that differ only in columns a table function added (whether those can tell rows apart is not documented)"

        // ---- The current rows ----------------------------------------------------------------------
        // Per model column of the relation: the value a row context gives it, or the ids VALUES has
        // in the filter context.
        let matchers = Dictionary<int, int -> bool>()

        let matcher (i: int) : int -> bool =
            match matchers.TryGetValue i with
            | true, m -> m
            | _ ->
                let m = r.Cols.[i].Lineage

                let found = [
                    for rc in ctx.Rows do
                        let hits =
                            rc.Table.Cols
                            |> Array.indexed
                            |> Array.filter (fun (_, col) -> obj.ReferenceEquals(col.Lineage, m))

                        if hits.Length > 0 then
                            rc, hits
                ]

                let test =
                    match found with
                    | [ rc, [| j, _ |] ] ->
                        let v = rc.Table.Get(j, rc.Row)
                        fun row -> DaxValue.groupEquals coll (r.Get(i, row)) v
                    | [] ->
                        if ctx.CallerRows then
                            Fail.notSupported
                                ctx
                                call.Span
                                $"{name} inside a function that was called from a row context (whether the function sees the caller's row context is not documented)"

                        let ids = HashSet<int>(Filters.idsIn m (Filters.visible ctx.Filter m.Table) true)
                        fun row -> ids.Contains(m.FindId(r.Get(i, row)))
                    | [ _ ] ->
                        Fail.notSupported
                            ctx
                            call.Span
                            $"{name}: a row context with two columns of '{m.Table.Name}'[{m.Name}]"
                    | _ ->
                        Fail.notSupported
                            ctx
                            call.Span
                            $"{name} inside nested row contexts that both have '{m.Table.Name}'[{m.Name}] (Learn says that is an error, SQLBI that the innermost one is used)"

                matchers.[i] <- test
                test

        let orderColumns =
            match given with
            | Some ks ->
                ks
                |> List.choose (fun k -> relationColumn ev ctx r "ORDERBY" k.Expr)
                |> Array.ofList
            | None -> allCols |> Array.filter (fun i -> not (Array.contains i partition))

        let withLineage (cols: int array) =
            cols
            |> Array.filter (fun i -> not (isNull r.Cols.[i].Lineage) && readable i)
            |> Array.distinct
            |> Array.sort

        // The readings of which columns identify the current row (see the header).
        let readings =
            let all = withLineage allCols

            // dax.guide's reading for OFFSET and Learn's for RANK and ROWNUMBER take the ORDERBY and
            // PARTITIONBY columns; where those are no model column at all, SQLBI's examples rule it
            // out: OFFSET ( -1, SourceTable, ORDERBY ( [PrevAmount] ) ) finds one current row from
            // the row context ("apply semantics", example 2), and RANK ( DENSE, SourceTable, ORDERBY
            // ( [@Amt] ) ) ranks each month ("Introducing the RANK window function").
            let orderAndPartition = withLineage (Array.append orderColumns partition)

            let named =
                if orderAndPartition.Length = 0 then
                    all
                else
                    orderAndPartition

            match matchBy with
            | Some m -> [ withLineage m; withLineage (Array.append m partition) ]
            | None ->
                match p.Kind with
                | Offset -> [ all; named ]
                | Window(true, true) -> [ all; withLineage partition ]
                | Window _ -> [ all ]
                | Index -> [ all; withLineage partition ]
                | Rank _
                | RowNumber -> [ all; named ]
            |> List.distinct

        let applyRows (cols: int array) : int array =
            let tests = cols |> Array.map matcher

            Array.init n id
            |> Array.filter (fun row -> tests |> Array.forall (fun t -> t row))

        // ---- One reading under one order ---------------------------------------------------------
        let outcome (sorted: Sorted array) (current: int array) : Outcome =
            let pos = Array.zeroCreate<int> n

            for s in sorted do
                s.Rows |> Array.iteri (fun i row -> pos.[row] <- i)

            let single (s: Sorted) (i: int) =
                s.GroupStart.[i] = i && s.GroupEnd.[i] = i

            let result = HashSet<int>()
            let mutable refused = None
            let refuse () = refused <- Some ties

            let range (s: Sorted) (first: int) (last: int) =
                let first = max first 0
                let last = min last (s.Rows.Length - 1)

                if first <= last then
                    if s.GroupStart.[first] <> first || s.GroupEnd.[last] <> last then
                        refuse ()
                    else
                        for i in first..last do
                            result.Add s.Rows.[i] |> ignore

            let absolute (len: int) (v: int64) =
                if v > 0L then v - 1L
                elif v = 0L then 0L
                else int64 len + v

            let clampInt (v: int64) =
                int (max -1L (min v (int64 Int32.MaxValue / 2L)))

            let currentParts = current |> Array.map (fun row -> partOf.[row]) |> Array.distinct

            match p.Kind with
            | Index ->
                match numbers with
                | [ None ]
                | [ Some 0L ] -> ()
                | [ Some k ] ->
                    for pi in currentParts do
                        let s = sorted.[pi]
                        let i = if k > 0L then k - 1L else int64 s.Rows.Length + k

                        if i >= 0L && i < int64 s.Rows.Length then
                            if single s (int i) then
                                result.Add s.Rows.[int i] |> ignore
                            else
                                refuse ()
                | _ -> ()
            | Offset ->
                match numbers with
                | [ None ] -> refused <- Some "an OFFSET delta that is BLANK"
                | [ Some d ] ->
                    for row in current do
                        let s = sorted.[partOf.[row]]
                        let len = int64 s.Rows.Length
                        let i = pos.[row]
                        let lo = int64 s.GroupStart.[i] + d
                        let hi = int64 s.GroupEnd.[i] + d

                        if hi < 0L || lo >= len then
                            ()
                        elif lo = hi && single s (int lo) then
                            result.Add s.Rows.[int lo] |> ignore
                        else
                            refuse ()
                | _ -> ()
            | Window(fromAbs, toAbs) ->
                match numbers with
                | [ Some f; Some t ] ->
                    if fromAbs && toAbs then
                        for pi in currentParts do
                            let s = sorted.[pi]
                            let len = s.Rows.Length
                            range s (clampInt (absolute len f)) (clampInt (absolute len t))
                    else
                        for row in current do
                            let s = sorted.[partOf.[row]]
                            let len = s.Rows.Length
                            let i = pos.[row]

                            if not (single s i) then
                                refuse ()
                            else
                                let bound (abs: bool) (v: int64) =
                                    if abs then absolute len v else int64 i + v

                                let first = bound fromAbs f
                                let last = bound toAbs t

                                // "If the beginning of the window is before the first row, it's
                                // set to the first row. Similarly, if the end of the window is
                                // after the last row of the partition, it's set to the last row."
                                if last >= 0L && first < int64 len then
                                    range s (clampInt first) (int (min last (int64 len - 1L)))
                | _ -> refused <- Some "a WINDOW bound that is BLANK"
            | RowNumber
            | Rank _ -> ()

            match refused with
            | Some why -> Refused why
            | None ->
                match p.Kind with
                | RowNumber ->
                    match current with
                    | [||] -> Value DaxValue.Blank
                    | [| row |] ->
                        let s = sorted.[partOf.[row]]
                        let i = pos.[row]

                        if single s i then
                            Value(DaxValue.Integer(int64 i + 1L))
                        else
                            Refused ties
                    | _ ->
                        Refused "ROWNUMBER with more than one current row (the sources don't say what it returns then)"
                | Rank dense ->
                    // Rows that tie on every key have the same rank; "RANK returns a blank value for
                    // total rows" (Learn): where the current rows are different rows, BLANK.
                    let ranks =
                        current
                        |> Array.map (fun row ->
                            let pi = partOf.[row]
                            let s = sorted.[pi]
                            let i = pos.[row]
                            pi, s.GroupStart.[i], (if dense then s.DenseRank.[i] + 1 else s.GroupStart.[i] + 1))
                        |> Array.distinct

                    match ranks with
                    | [||] -> Value DaxValue.Blank
                    | [| _, _, rank |] -> Value(DaxValue.Integer(int64 rank))
                    | _ -> Value DaxValue.Blank
                | _ -> Rows(result |> Seq.sort |> Array.ofSeq)

        let outcomes = [
            for o in orders do
                let sorted =
                    if obj.ReferenceEquals(o, firstOrder) then
                        sortedOnce
                    else
                        parts |> Array.map (fun rows -> sortPartition rows (comparerOf o))

                for cols in readings do
                    outcome sorted (applyRows cols)
        ]

        match
            outcomes
            |> List.tryPick (fun o ->
                match o with
                | Refused why -> Some why
                | _ -> None)
        with
        | Some why -> Fail.notSupported ctx call.Span $"{name}: {why}"
        | None -> ()

        let same (a: Outcome) (b: Outcome) =
            match a, b with
            | Rows x, Rows y -> x = y
            | Value x, Value y -> DaxValue.isBlank x = DaxValue.isBlank y && DaxValue.groupEquals coll x y
            | _ -> false

        let first = List.head outcomes

        if not (outcomes |> List.forall (same first)) then
            Fail.notSupported
                ctx
                call.Span
                (if orders.Length > 1 then
                     $"{name} without ORDERBY, where the order in which the engine adds the relation's columns decides the result (Learn: \"no guarantee is given as to what order in which they'll be added\")"
                 else
                     $"{name} where the sources disagree on which columns identify the current row, and the readings give different results")

        match first with
        | Rows rows -> Choice1Of2(Tables.select r rows)
        | Value v -> Choice2Of2 v
        | Refused why -> Fail.notSupported ctx call.Span why

    let private tableFunction (name: string) (minArgs: int) =
        Fn.table name "Filter" (minArgs, -1) (fun ev ctx call ->
            match evaluate ev ctx call name with
            | Choice1Of2 t -> t
            | Choice2Of2 _ -> Fail.invalid ctx call.Span $"{name} returns a table")
        |> Fn.withCheck (check name)

    let private scalarFunction (name: string) =
        Fn.scalar name "Filter" (0, -1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            match evaluate ev ctx call name with
            | Choice2Of2 v -> v
            | Choice1Of2 _ -> Fail.invalid ctx call.Span $"{name} returns a single value")
        |> Fn.withCheck (check name)

    /// ORDERBY, PARTITIONBY and MATCHBY: "This function can only be used within a window function
    /// expression" (Learn).
    let private clause (name: string) : DaxFunction =
        let fail (ctx: Ctx) (call: Call) : 'a =
            Fail.invalid
                ctx
                call.Span
                $"{name} can only be used within a window function (INDEX, OFFSET, WINDOW, RANK, ROWNUMBER)"

        {
            Fn.scalar name "Filter" (0, -1) Fn.typeUnknown (fun _ ctx call -> fail ctx call) with
                Table = fun _ ctx call -> fail ctx call
        }

    let functions: DaxFunction list = [
        tableFunction "INDEX" 1
        tableFunction "OFFSET" 1
        tableFunction "WINDOW" 2
        scalarFunction "RANK"
        scalarFunction "ROWNUMBER"
        clause "ORDERBY"
        clause "PARTITIONBY"
        clause "MATCHBY"
    ]
