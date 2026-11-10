namespace Tessera.DaxEngine

// The local DAX engine's vocabulary: the bound model, sets of rows, table values with data lineage,
// the filter context, the evaluation context, and the API a function is written against.
//
// ---- Writing a function --------------------------------------------------------------------------
//
// Every DAX function is a `DaxFunction` record registered by name in a family file under
// `Functions/` and listed in `Registry.fs`. Adding a family is: a new file with a
// `let functions : DaxFunction list`, one `<Compile>` line, one line in `Registry.all`. The
// evaluator core is not touched.
//
// A function receives its arguments UNEVALUATED (`Call.Args`, the parser's trees) together with the
// evaluation context, and evaluates what it needs through `IEval`. So an iterator or a
// CALCULATE-like function is an ordinary entry.
//
// A simple scalar function (both arguments evaluated in the caller's context):
//
//     Fn.scalar "MOD" "Math" (2, 2) Fn.typeUnknown (fun ev ctx call ->
//         let n = ev.Scalar(ctx, call.Args.[0])
//         let d = ev.Scalar(ctx, call.Args.[1])
//         ...)
//
// A function that manipulates the context (an iterator: a row context per row of its table):
//
//     Fn.scalar "SUMX" "Aggregation" (2, 2) Fn.typeUnknown (fun ev ctx call ->
//         let table = ev.Table(ctx, call.Args.[0])
//         let mutable total = DaxValue.Blank
//         for row in 0 .. table.Count - 1 do
//             let inner = Context.pushRow ctx table row          // the new row context
//             total <- Fail.dax ctx call.Span (DaxValue.add total (ev.Scalar(inner, call.Args.[1])))
//         total)
//
// and one that changes the filter context (what CALCULATE does, reduced to one table filter):
//
//     let filter = Filters.ofTable ctx call.Args.[1].Span (ev.Table(ctx, call.Args.[1]))
//     let inner = Context.withFilters (Context.transition ctx call.Span) [ filter ] false
//     ev.Scalar(inner, call.Args.[0])
//
// Rules for a function author:
//   - NEVER return an approximate answer. An argument form, a data type or a model shape the
//     function doesn't implement exactly is `Fail.notSupported ctx span "what"`.
//   - A DAX error (the kind IFERROR catches: a failed conversion, ERROR()) is `Fail.runtime`; DAX
//     that is wrong (wrong argument kind, unknown name) is `Fail.invalid`. Results of `DaxValue`
//     operations go through `Fail.dax`.
//   - `Check` says, from the argument trees alone, which forms are not supported. `Engine.coverage`
//     runs it without data and the evaluator runs it before every call. Put every refusal that
//     can be told from the arguments there, so coverage reports it; a refusal that needs values
//     or the context stays in the implementation (and coverage can't see it).
//   - Text and order have open questions the engine refuses rather than guesses: turn a value
//     into text with `Convert.toText` (never `DaxValue.toText`: a date's text depends on the
//     model's culture), check operands of arithmetic with `Convert.checkOperand`, and before
//     sorting by values call `Sorting.check` (where BLANK sorts against zero and negatives is
//     not documented).
//   - Names are registered once: `Registry.byName` throws if two entries share a name. Before
//     adding a family, see which of its functions another file already has (`Engine.functions`).
//   - `Type` is the static result type where the function has one whatever its data (COUNTROWS is
//     always an Integer). It types result columns that are BLANK in every row.

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open Tessera
open Tessera.DaxAst

/// A text that DAX was parsed from: spans are offsets into it.
type SourceText = { Label: string; Text: string }

module SourceText =
    let query (text: string) = { Label = "query"; Text = text }

    /// "query 2:14", "measure 'Sales'[Total] 1:5".
    let where (s: SourceText) (span: Span) =
        let upto = min (max span.Start 0) s.Text.Length
        let mutable line = 1
        let mutable col = 1

        for i in 0 .. upto - 1 do
            if s.Text.[i] = '\n' then
                line <- line + 1
                col <- 1
            else
                col <- col + 1

        $"{s.Label} {line}:{col}"

/// Not supported, or invalid DAX: never caught by IFERROR.
exception EngineException of TessError

/// A DAX error raised during evaluation: IFERROR and ISERROR catch it.
exception DaxRuntimeException of text: string * at: string

/// A set of row numbers (or of value ids) below `Size`. `IsAll` means "not restricted": it is
/// only ever produced where no filter applied, so it also tells whether anything filtered.
[<Sealed; AllowNullLiteral>]
type RowSet private (size: int, isAll: bool, rows: int array, bits: uint64 array, count: int) =
    let mutable rows = rows
    let mutable bits = bits

    static let extract (size: int) (bits: uint64 array) (count: int) =
        let out = Array.zeroCreate<int> count
        let mutable n = 0

        for w in 0 .. bits.Length - 1 do
            let mutable word = bits.[w]

            while word <> 0UL do
                let bit = BitOperations.TrailingZeroCount word
                let i = (w <<< 6) + bit

                if i < size then
                    out.[n] <- i
                    n <- n + 1

                word <- word &&& (word - 1UL)

        out

    member _.Size = size
    member _.IsAll = isAll
    member _.Count = count
    member _.IsEmpty = count = 0

    /// The members in ascending order. Do not modify.
    member _.Rows: int array =
        if isNull rows then
            rows <-
                if isAll then
                    Array.init size id
                else
                    extract size bits count

        rows

    member this.Bits: uint64 array =
        if isNull bits then
            let b = Array.zeroCreate<uint64>((size + 63) >>> 6)

            if isAll then
                for i in 0 .. size - 1 do
                    b.[i >>> 6] <- b.[i >>> 6] ||| (1UL <<< (i &&& 63))
            else
                for i in rows do
                    b.[i >>> 6] <- b.[i >>> 6] ||| (1UL <<< (i &&& 63))

            bits <- b

        bits

    member this.Contains(i: int) : bool =
        if isAll then
            i >= 0 && i < size
        elif not (isNull bits) then
            i >= 0 && i < size && ((bits.[i >>> 6] >>> (i &&& 63)) &&& 1UL) <> 0UL
        elif rows.Length <= 8 then
            Array.IndexOf(rows, i) >= 0
        else
            let b = this.Bits
            i >= 0 && i < size && ((b.[i >>> 6] >>> (i &&& 63)) &&& 1UL) <> 0UL

    static member All(size: int) = RowSet(size, true, null, null, size)
    static member Empty(size: int) = RowSet(size, false, [||], null, 0)

    /// From members in ascending order without repeats (the array is kept).
    static member OfSorted(size: int, rows: int array) =
        RowSet(size, false, rows, null, rows.Length)

    static member OfBits(size: int, bits: uint64 array) =
        let mutable n = 0

        for w in bits do
            n <- n + BitOperations.PopCount w

        RowSet(size, false, null, bits, n)

    /// From members in any order, repeats allowed.
    static member OfSeq(size: int, members: int seq) =
        let b = Array.zeroCreate<uint64>((size + 63) >>> 6)

        for i in members do
            b.[i >>> 6] <- b.[i >>> 6] ||| (1UL <<< (i &&& 63))

        RowSet.OfBits(size, b)

    static member One(size: int, row: int) = RowSet(size, false, [| row |], null, 1)

    /// The members that satisfy `keep`. Never `IsAll`.
    member this.Where(keep: int -> bool) : RowSet =
        if isAll then
            let b = Array.zeroCreate<uint64>((size + 63) >>> 6)

            for i in 0 .. size - 1 do
                if keep i then
                    b.[i >>> 6] <- b.[i >>> 6] ||| (1UL <<< (i &&& 63))

            RowSet.OfBits(size, b)
        else
            RowSet.OfSorted(size, this.Rows |> Array.filter keep)

    member this.Intersect(other: RowSet) : RowSet =
        if isAll then
            other
        elif other.IsAll then
            this
        elif count <= other.Count then
            if count > 64 && other.Count > 64 then
                let a = this.Bits
                let b = other.Bits
                RowSet.OfBits(size, Array.init a.Length (fun i -> a.[i] &&& b.[i]))
            else
                RowSet.OfSorted(size, this.Rows |> Array.filter other.Contains)
        else
            other.Intersect this

type IntArrayComparer() =
    static member val Instance = IntArrayComparer()

    interface IEqualityComparer<int array> with
        member _.Equals(a, b) =
            a.Length = b.Length
            && (let mutable same = true
                let mutable i = 0

                while same && i < a.Length do
                    same <- a.[i] = b.[i]
                    i <- i + 1

                same)

        member _.GetHashCode a =
            let mutable h = 17

            for x in a do
                h <- h * 31 + x

            h

type ValueArrayComparer(comparer: IEqualityComparer<DaxValue>) =
    interface IEqualityComparer<DaxValue array> with
        member _.Equals(a, b) =
            a.Length = b.Length
            && (let mutable same = true
                let mutable i = 0

                while same && i < a.Length do
                    same <- comparer.Equals(a.[i], b.[i])
                    i <- i + 1

                same)

        member _.GetHashCode a =
            let mutable h = 17

            for x in a do
                h <- h * 31 + comparer.GetHashCode x

            h

/// The static type of a scalar expression: what can be said of its type without evaluating it.
[<RequireQualifiedAccess>]
type StaticType =
    /// Always BLANK (BLANK (), an omitted argument): no type of its own; it takes the type of
    /// whatever it is combined with.
    | Blank
    /// Always a value of this type, or BLANK.
    | Known of DaxType
    /// Known to be of different types in different cases (an IF with a text and a number branch):
    /// the engine declares such a column Variant.
    | Variant
    /// Not known without evaluating: the values decide.
    | Unknown

// ---- The bound model ---------------------------------------------------------------------------

/// A column of a model table, bound to its data. A calculated column has no data until `prepare`
/// has computed it; reading one that is being computed is a circular dependency.
[<Sealed; AllowNullLiteral>]
type BColumn(id: int, table: BTable, name: string, dataType: DaxType) =
    let mutable store: StoreColumn = Unchecked.defaultof<StoreColumn>
    let mutable blankId = -1

    member _.Id = id
    member _.Table: BTable = table
    member _.Name = name
    member val DataType = dataType with get, set
    /// The DAX expression of a calculated column, with where it is written; null for a data column.
    member val Expression: SourceText option = None with get, set
    member val Failure: TessError option = None with get, set
    member val Computing = false with get, set
    /// For a column of a calculated table: the column of the expression's result it holds.
    member val SourceName: string = null with get, set
    member _.IsReady = not (obj.ReferenceEquals(store, null))

    member this.SetData(column: StoreColumn) =
        blankId <-
            if column.BlankId >= 0 then
                column.BlankId
            else
                column.DistinctCount

        store <- column

    /// The stored values. Computes a calculated column that is still pending (during `prepare`).
    member this.Data: StoreColumn =
        if obj.ReferenceEquals(store, null) then
            table.Model.ForceColumn this

        store

    /// The id BLANK has in this column: the stored one, or one past the stored ids when no row is
    /// BLANK (the blank row's value still needs an id).
    member this.BlankId: int =
        if obj.ReferenceEquals(store, null) then
            table.Model.ForceColumn this

        blankId

    /// Ids run from 0 to IdCount - 1 (the last may be the virtual BLANK).
    member this.IdCount: int = this.Data.DistinctCount + 1

    member this.IdAt(row: int) : int =
        let d = this.Data
        if row >= d.RowCount then blankId else d.ValueIds.[row]

    member this.ValueOfId(id: int) : DaxValue =
        let v = this.Data.Values
        if id < v.Length then v.[id] else DaxValue.Blank

    /// The value of a row; the blank row (row = RowCount) is BLANK.
    member this.ValueAt(row: int) : DaxValue =
        let d = this.Data

        if row >= d.RowCount then
            DaxValue.Blank
        else
            d.Values.[d.ValueIds.[row]]

    /// The id of a value, or -1 when no row has it. A number of another numeric type matches when it
    /// converts to the column's type without change.
    member this.FindId(value: DaxValue) : int =
        match value with
        | DaxValue.Blank -> this.BlankId
        | _ ->
            let d = this.Data

            match DaxValue.typeOf value with
            | Some t when t <> d.DataType && d.DataType <> DaxType.Variant && DaxType.isNumeric t ->
                if DaxType.isNumeric d.DataType then
                    match DaxValue.convertTo d.DataType value with
                    | Ok converted ->
                        match DaxValue.compare d.Collation converted value with
                        | Ok 0 -> d.TryFindId converted
                        | Error(Unsettled what) -> raise (EngineException(DaxNotSupported(what, "")))
                        | _ -> -1
                    | _ -> -1
                else
                    -1
            | _ -> d.TryFindId value

/// A model table bound to its data.
and [<Sealed; AllowNullLiteral>] BTable(id: int, model: Model, name: string) =
    let mutable rowCount = -1
    let mutable blankRow = 0 // 0 unknown, 1 no, 2 yes
    let mutable unique: BColumn option voption = ValueNone
    let mutable duplicates: (int * int array array) voption = ValueNone

    member _.Id = id
    member _.Model: Model = model
    member _.Name = name
    member val Columns: BColumn array = [||] with get, set
    member val ColumnByName = Dictionary<string, BColumn>(StringComparer.OrdinalIgnoreCase) with get
    /// The DAX expression of a calculated table; None for a table that has data.
    member val Expression: SourceText option = None with get, set
    member val Failure: TessError option = None with get, set
    member val Computing = false with get, set
    /// Why queries can't use this table (a relationship or model feature the engine doesn't do).
    member val Gate: string option = None with get, set
    member _.IsReady = rowCount >= 0
    member _.SetRowCount(n: int) = rowCount <- n

    /// The stored rows (without the blank row).
    member this.RowCount: int =
        if rowCount < 0 then
            model.ForceTable this

        rowCount

    member _.BlankRowKnown = blankRow <> 0
    member _.SetBlankRow(has: bool) = blankRow <- (if has then 2 else 1)

    /// Whether the table has the blank row: it is on the one side of a relationship whose many
    /// side has keys with no match. The blank row is row number `RowCount`.
    member this.HasBlankRow: bool =
        if blankRow = 0 then
            model.ForceBlankRow this

        blankRow = 2

    /// Rows including the blank row.
    member this.Size: int = this.RowCount + (if this.HasBlankRow then 1 else 0)

    /// A column with a different value in every row, if there is one: a row is then its value there.
    member this.UniqueColumn: BColumn option =
        match unique with
        | ValueSome u -> u
        | ValueNone ->
            let u =
                this.Columns
                |> Array.tryFind (fun c -> c.IsReady && c.Data.IsUnique && c.Data.BlankId < 0)

            if this.Columns |> Array.forall _.IsReady then
                unique <- ValueSome u

            u

    /// For a table with rows that repeat in every column: for each row, the rows equal to it
    /// (itself included), or null for a row that is alone. Null when no row repeats. During
    /// `prepare` this is over the columns computed so far.
    member this.Duplicates: int array array =
        let ready = this.Columns |> Array.filter _.IsReady

        match duplicates with
        | ValueSome(stamp, d) when stamp = ready.Length -> d
        | _ ->
            let n = this.RowCount
            let cols = ready |> Array.map (fun c -> c.Data.ValueIds)

            let groups =
                Dictionary<int array, ResizeArray<int>>(IntArrayComparer.Instance :> IEqualityComparer<int array>)

            for row in 0 .. n - 1 do
                let key = cols |> Array.map (fun ids -> ids.[row])

                match groups.TryGetValue key with
                | true, list -> list.Add row
                | _ ->
                    let list = ResizeArray<int>()
                    list.Add row
                    groups.[key] <- list

            let result =
                if groups.Count = n then
                    null
                else
                    let per = Array.zeroCreate<int array> n

                    for kv in groups do
                        if kv.Value.Count > 1 then
                            let rows = kv.Value.ToArray()

                            for r in rows do
                                per.[r] <- rows

                    per

            duplicates <- ValueSome(ready.Length, result)
            result

/// A single-column relationship: `From` is the many side, `To` the one side.
and [<Sealed; AllowNullLiteral>] BRel(id: int, name: string, from: BColumn, toColumn: BColumn, both: bool, active: bool)
    =
    let mutable toRow: int array = null
    let mutable orphans: int array = null
    let mutable fromIdOfToId: int array = null

    member _.Id = id
    member _.Name = name
    member _.From: BColumn = from
    member _.To: BColumn = toColumn
    member _.Both = both
    member _.IsActive = active

    member private this.Build() =
        let f = from.Data
        let t = toColumn.Data
        let toRows = toColumn.Table.RowCount

        if not t.IsUnique || t.BlankId >= 0 then
            raise (
                EngineException(
                    DaxNotSupported(
                        $"relationship '{name}': its one side '{toColumn.Table.Name}'[{toColumn.Name}] has values that repeat or are BLANK",
                        ""
                    )
                )
            )

        let convert (source: StoreColumn) (target: StoreColumn) (v: DaxValue) =
            if DaxValue.isBlank v then
                // A BLANK key matches only a stored BLANK on the other side.
                target.BlankId
            elif source.DataType = target.DataType then
                target.TryFindId v
            else
                match DaxValue.convertTo target.DataType v with
                | Ok c -> target.TryFindId c
                | Error _ -> -1

        let toIdOfFromId = f.Values |> Array.map (convert f t)

        let firstRow =
            toIdOfFromId
            |> Array.map (fun toId -> if toId < 0 then toRows else t.RowsOfId(toId).[0])

        let ids = f.ValueIds
        let map = Array.init ids.Length (fun row -> firstRow.[ids.[row]])
        let orphanRows = ResizeArray<int>()

        for row in 0 .. map.Length - 1 do
            if map.[row] = toRows then
                orphanRows.Add row

        fromIdOfToId <- t.Values |> Array.map (convert t f)
        orphans <- orphanRows.ToArray()
        toRow <- map

    /// For each stored row of the From table: its row of the To table, or the To table's
    /// `RowCount` (its blank row) when the key has no match.
    member this.ToRowOfFromRow: int array =
        if isNull toRow then
            this.Build()

        toRow

    /// The From rows whose key has no match: the rows of the To table's blank row.
    member this.OrphanFromRows: int array =
        if isNull toRow then
            this.Build()

        orphans

    /// The stored From rows that relate to a row of the To table (its blank row included).
    member this.FromRowsOfToRow(row: int) : ArraySegment<int> =
        if isNull toRow then
            this.Build()

        if row >= toColumn.Table.RowCount then
            ArraySegment<int>(orphans)
        else
            match fromIdOfToId.[toColumn.Data.ValueIds.[row]] with
            | -1 -> ArraySegment<int>.Empty
            | id -> from.Data.RowsOfId id

/// Which relationships are in force and which way they filter: the model's own set-up, or that
/// changed by USERELATIONSHIP and CROSSFILTER. Everything derived from it is cached here.
and [<Sealed; AllowNullLiteral>] RelConfig(model: Model, active: bool array, both: bool array) =
    let manyToOne =
        lazy
            (model.Tables
             |> Array.map (fun t ->
                 model.Rels
                 |> Array.filter (fun r -> active.[r.Id] && obj.ReferenceEquals(r.From.Table, t))))

    let oneToMany =
        lazy
            (model.Tables
             |> Array.map (fun t ->
                 model.Rels
                 |> Array.filter (fun r -> active.[r.Id] && obj.ReferenceEquals(r.To.Table, t))))

    /// Per table: the tables of its expanded table (itself first), each with the relationships
    /// that lead to it.
    let expanded =
        lazy
            (model.Tables
             |> Array.map (fun t ->
                 let found = ResizeArray<BTable * BRel list>()
                 let mutable ambiguous = None

                 let rec walk (x: BTable) (path: BRel list) (depth: int) =
                     if
                         found.Exists(fun (y, _) -> obj.ReferenceEquals(x, y))
                         || depth > model.Tables.Length
                     then
                         if ambiguous.IsNone then
                             ambiguous <- Some x.Name
                     else
                         found.Add((x, List.rev path))

                         for r in manyToOne.Value.[x.Id] do
                             walk r.To.Table (r :: path) (depth + 1)

                 walk t [] 0
                 found.ToArray(), ambiguous))

    let isExpanded =
        lazy
            (expanded.Value
             |> Array.map (fun (entries, _) ->
                 let flags = Array.zeroCreate<bool> model.Tables.Length

                 for (x, _) in entries do
                     flags.[x.Id] <- true

                 flags))

    let rowMaps = ConcurrentDictionary<struct (int * int), int array>()

    member _.Model = model
    member _.Active = active
    member _.Both = both

    member val Key =
        String(
            Array.init active.Length (fun i ->
                if not active.[i] then '0'
                elif both.[i] then '2'
                else '1')
        )

    /// The active relationships in which the table is the many side.
    member _.ManyToOne(table: BTable) : BRel array = manyToOne.Value.[table.Id]
    /// The active relationships in which the table is the one side.
    member _.OneToMany(table: BTable) : BRel array = oneToMany.Value.[table.Id]
    member _.FiltersBothWays(r: BRel) : bool = active.[r.Id] && both.[r.Id]

    /// The tables of a table's expanded table: itself, then every table reached over many-to-one
    /// relationships (SQLBI, "Expanded tables in DAX").
    member _.Expanded(table: BTable) : (BTable * BRel list) array = fst expanded.Value.[table.Id]

    /// A table that two chains of many-to-one relationships lead to from `table`, if any.
    member _.AmbiguousFrom(table: BTable) : string option = snd expanded.Value.[table.Id]

    member _.IsExpanded(table: BTable, other: BTable) : bool = isExpanded.Value.[table.Id].[other.Id]

    /// Two tables that filters can travel between along more than one path of active
    /// relationships, if there are any: the engine has rules to pick one, this one refuses.
    member val Ambiguity: string option =
        let edges (t: BTable) =
            model.Rels
            |> Array.collect (fun r ->
                if not active.[r.Id] then
                    [||]
                elif obj.ReferenceEquals(r.To.Table, t) then
                    [| r, r.From.Table |]
                elif obj.ReferenceEquals(r.From.Table, t) && both.[r.Id] then
                    [| r, r.To.Table |]
                else
                    [||])

        model.Tables
        |> Array.tryPick (fun start ->
            let seen = HashSet<int>()
            seen.Add start.Id |> ignore
            let mutable found = None

            let rec walk (t: BTable) (arrivedBy: BRel) =
                for (r, next) in edges t do
                    if found.IsNone && not (obj.ReferenceEquals(r, arrivedBy)) then
                        if seen.Add next.Id then
                            walk next r
                        else
                            found <- Some $"'{start.Name}' and '{next.Name}'"

            walk start null
            found) with get

    /// For each row of `table` (its blank row last): the row of `other` in its expanded table.
    member this.RowMap(table: BTable, other: BTable) : int array =
        rowMaps.GetOrAdd(
            struct (table.Id, other.Id),
            fun _ ->
                let path =
                    this.Expanded table
                    |> Array.find (fun (x, _) -> obj.ReferenceEquals(x, other))
                    |> snd

                let mutable map = Array.init (table.RowCount + 1) id

                for r in path do
                    let step = r.ToRowOfFromRow
                    let fromRows = r.From.Table.RowCount
                    let toBlank = r.To.Table.RowCount
                    map <- map |> Array.map (fun row -> if row >= fromRows then toBlank else step.[row])

                map
        )

and [<Sealed; AllowNullLiteral>] BMeasure(name: string, table: BTable, source: SourceText, parsed: Lazy<Expr>) =
    member _.Name = name
    member _.Table = table
    member _.Source = source
    /// The parsed expression; raises `EngineException` for one that doesn't parse.
    member _.Expr: Expr = parsed.Value

and [<Sealed; AllowNullLiteral>] BFunction(name: string, source: SourceText, parsed: Lazy<FunctionDef>) =
    member _.Name = name
    member _.Source = source
    member _.Def: FunctionDef = parsed.Value

and [<Sealed; AllowNullLiteral>] Model(collation: Collation) =
    let configs = ConcurrentDictionary<string, RelConfig>()
    let mutable defaultConfig: RelConfig = null

    member _.Collation = collation
    member val Comparer = DaxValue.comparer collation
    member val Tables: BTable array = [||] with get, set
    member val TableByName = Dictionary<string, BTable>(StringComparer.OrdinalIgnoreCase) with get
    member val Rels: BRel array = [||] with get, set
    member val Measures = Dictionary<string, BMeasure>(StringComparer.OrdinalIgnoreCase) with get
    member val Functions = Dictionary<string, BFunction>(StringComparer.OrdinalIgnoreCase) with get
    /// The static type of each measure, once worked out (a measure's type is its own business).
    member val MeasureTypes = ConcurrentDictionary<BMeasure, StaticType>(HashIdentity.Reference) with get
    member val ColumnCount = 0 with get, set
    /// Set by `prepare`: compute a pending calculated column or table, or raise.
    member val ForceColumn: BColumn -> unit = ignore with get, set
    member val ForceTable: BTable -> unit = ignore with get, set
    member val ForceBlankRow: BTable -> unit = ignore with get, set

    member this.Config(active: bool array, both: bool array) : RelConfig =
        let c = RelConfig(this, active, both)
        configs.GetOrAdd(c.Key, c)

    member this.DefaultConfig: RelConfig =
        if isNull defaultConfig then
            defaultConfig <-
                this.Config(this.Rels |> Array.map (fun r -> r.IsActive), this.Rels |> Array.map (fun r -> r.Both))

        defaultConfig

// ---- Table values ------------------------------------------------------------------------------

/// A column of a table value.
type ColInfo = {
    /// The column's own name, without a table: `Amount`, or the name an extension column was given.
    Name: string
    /// The model table of a column that is shown as `Table[Name]`; null for an extension column,
    /// shown as `[Name]`.
    Table: string
    /// Data lineage: the model column this column's values are values of, or null. A column with
    /// lineage filters that model column when the table is used as a filter.
    Lineage: BColumn
    /// The declared type; Variant when it is whatever the values are.
    Type: DaxType
}

/// A table as a value: columns and rows. A table that came from a model table keeps the row of
/// that table each of its rows is (`BaseTable`, `BaseRows`), and reads the model's columns from
/// there (`Data.[c]` is null for those).
[<Sealed; AllowNullLiteral>]
type TableVal
    (
        cols: ColInfo array,
        count: int,
        baseTable: BTable,
        baseRows: int array,
        data: DaxValue array array,
        config: RelConfig
    ) =
    member _.Cols = cols
    member _.Count = count
    /// The model table whose rows these are, or null.
    member _.BaseTable = baseTable
    /// Per row, the row of `BaseTable` (its `RowCount` for the blank row).
    member _.BaseRows = baseRows
    member _.Data = data
    /// The relationships in force when the table was evaluated: its expanded table is fixed then.
    member _.Config = config

    member _.Get(col: int, row: int) : DaxValue =
        let d = data.[col]

        if isNull d then
            cols.[col].Lineage.ValueAt(baseRows.[row])
        else
            d.[row]

    /// Nothing but the rows of a model table, with all of its columns.
    member _.IsPureBase =
        not (isNull baseTable)
        && cols.Length = baseTable.Columns.Length
        && data |> Array.forall isNull

    member this.Column(col: int) : DaxValue array =
        let d = data.[col]

        if isNull d then
            let c = cols.[col].Lineage
            Array.init count (fun row -> c.ValueAt(baseRows.[row]))
        else
            d

    member this.Row(row: int) : DaxValue array =
        Array.init cols.Length (fun c -> this.Get(c, row))

[<RequireQualifiedAccess>]
type Value =
    | Scalar of DaxValue
    | Table of TableVal

// ---- Contexts ----------------------------------------------------------------------------------

/// One filter of a filter context.
type Filter =
    /// One column restricted to a set of its value ids.
    | OnColumn of BColumn * RowSet
    /// Several columns restricted to combinations of value ids (an "arbitrarily shaped" filter).
    | Tuples of BColumn array * HashSet<int array>
    /// A model table restricted to a set of its rows; it filters every column of the table's
    /// EXPANDED table, as the relationships were when the table was evaluated. Inside a
    /// `FilterCtx` the configuration is always the context's own (see `Filters.build`).
    | Rows of BTable * RowSet * RelConfig

/// How a column came to be filtered, as ISINSCOPE asks: "whether there is a filter placed on the
/// column derived either from context transition or from a group by column placed by
/// SUMMARIZECOLUMNS" (dax.guide/isinscope). A filter context keeps these marks, newest first.
type ScopeMark =
    /// Group-by columns of the SUMMARIZECOLUMNS row being computed.
    | Grouped of BColumn array
    /// A context transition: the columns of the table that was iterated, and a test for the
    /// columns of the tables related to it (which the transition also filters; whether those
    /// count as in scope is not documented).
    | Transitioned of own: BColumn array * related: (BColumn -> bool)
    /// The filters on these columns were removed or replaced.
    | Unfiltered of (BColumn -> bool)

/// A filter context: filters, and the relationships in force. Immutable; the caches belong to one
/// evaluation (one thread).
[<Sealed; AllowNullLiteral>]
type FilterCtx(filters: Filter list, config: RelConfig, scope: ScopeMark list) =
    new(filters: Filter list, config: RelConfig) = FilterCtx(filters, config, [])
    member _.Filters = filters
    member _.Config = config
    /// See `ScopeMark`. Part of the context: a measure's value may depend on it.
    member _.Scope = scope
    member val Visible = Dictionary<int, RowSet>() with get
    member val InProgress = HashSet<int>() with get
    member val Measures = Dictionary<obj, DaxValue>(HashIdentity.Reference) with get

/// One level of row context: a row of a table.
type RowCtx = { Table: TableVal; Row: int }

/// What a name is bound to: a VAR or a VAL parameter (a value, computed once, on first use), or an
/// EXPR parameter (an expression evaluated wherever it is used).
type Binding =
    | Val of Lazy<Value> * isTable: bool * staticType: Lazy<StaticType>
    | ExprParam of Expr * Scope

/// The lexical side of a context: which names are in scope, and which text the code is from.
and Scope = {
    Vars: Map<string, Binding>
    Source: SourceText
    /// Code written in the query (or defined by it): it sees the query's measures and functions.
    InQuery: bool
}

/// What a query's DEFINE section adds.
[<Sealed; AllowNullLiteral>]
type QueryDefs() =
    member val Measures = Dictionary<string, BMeasure>(StringComparer.OrdinalIgnoreCase) with get
    member val MeasureTypes = Dictionary<BMeasure, StaticType>(HashIdentity.Reference) with get
    member val Functions = Dictionary<string, BFunction>(StringComparer.OrdinalIgnoreCase) with get

/// The evaluation context.
type Ctx = {
    Model: Model
    Query: QueryDefs
    Filter: FilterCtx
    /// The row contexts, innermost first.
    Rows: RowCtx list
    Scope: Scope
    /// The measures being evaluated, innermost first (to detect a measure that refers to itself).
    Measures: BMeasure list
    Functions: BFunction list
    /// The rows of the current group, inside GROUPBY; null elsewhere.
    Group: TableVal
    /// Inside a user-defined function that was called from a row context. The function's body is
    /// evaluated without that row context; whether the engine would let it be seen is not
    /// documented, so anything that would depend on it is refused (see `Context.transition`).
    CallerRows: bool
}

type Call = {
    Name: Ident
    Args: Expr list
    /// The whole call.
    Span: Span
}

/// What a function evaluates its arguments with.
type IEval =
    /// A scalar expression's value. A table expression of one column and at most one row converts.
    abstract Scalar: Ctx * Expr -> DaxValue
    abstract Table: Ctx * Expr -> TableVal
    /// A scalar or a table, whichever the expression is.
    abstract Value: Ctx * Expr -> Value
    abstract IsTable: Ctx * Expr -> bool
    /// The static type of a scalar expression.
    abstract TypeOf: Ctx * Expr -> StaticType
    /// The model column an argument names (`T[c]`), or an error.
    abstract Column: Ctx * Expr -> BColumn
    /// The model column an argument names, if it is a column reference.
    abstract TryColumn: Ctx * Expr -> BColumn option
    /// The model table an argument names, if it is a table reference (not a variable).
    abstract TryModelTable: Ctx * Expr -> BTable option
    /// A scalar from an already computed table (one column, at most one row).
    abstract ScalarOfTable: Ctx * Span * TableVal -> DaxValue

[<RequireQualifiedAccess>]
type Returns =
    | Scalar
    | Table
    /// Only meaningful as a CALCULATE argument (USERELATIONSHIP, CROSSFILTER, KEEPFILTERS...).
    | Modifier

type DaxFunction = {
    Name: string
    /// The family it is documented under ("Aggregation", "Filter", "Logical"...).
    Family: string
    MinArgs: int
    /// -1: no limit.
    MaxArgs: int
    Returns: Returns
    /// Which argument forms are not supported, told from the trees alone.
    Check: Expr list -> string option
    Type: IEval -> Ctx -> Expr list -> StaticType
    Scalar: IEval -> Ctx -> Call -> DaxValue
    Table: IEval -> Ctx -> Call -> TableVal
}

module Fail =
    let where (ctx: Ctx) (span: Span) = SourceText.where ctx.Scope.Source span

    /// Something the engine doesn't implement: it refuses.
    let notSupported (ctx: Ctx) (span: Span) (what: string) : 'a =
        raise (EngineException(DaxNotSupported(what, where ctx span)))

    /// DAX that is wrong.
    let invalid (ctx: Ctx) (span: Span) (message: string) : 'a =
        raise (EngineException(DaxInvalid(message, where ctx span)))

    /// A DAX error (IFERROR catches it).
    let runtime (ctx: Ctx) (span: Span) (message: string) : 'a =
        raise (DaxRuntimeException(message, where ctx span))

    let dax (ctx: Ctx) (span: Span) (r: Result<'a, DaxError>) : 'a =
        match r with
        | Ok v -> v
        // What the sources leave open is refused, not raised as a DAX error (IFERROR must not
        // turn it into a value).
        | Error(Unsettled what) -> notSupported ctx span what
        | Error e -> runtime ctx span (DaxError.message e)

module Fn =
    let typeUnknown: IEval -> Ctx -> Expr list -> StaticType =
        fun _ _ _ -> StaticType.Unknown

    let typeIs (t: DaxType) : IEval -> Ctx -> Expr list -> StaticType = fun _ _ _ -> StaticType.Known t

    /// The type of the n-th argument.
    let typeOfArg (n: int) : IEval -> Ctx -> Expr list -> StaticType =
        fun ev ctx args ->
            match List.tryItem n args with
            | Some a -> ev.TypeOf(ctx, a)
            | None -> StaticType.Unknown

    let private noScalar (name: string) : IEval -> Ctx -> Call -> DaxValue =
        fun _ ctx call -> Fail.invalid ctx call.Span $"{name} returns a table, where a single value is needed"

    let private noTable (name: string) : IEval -> Ctx -> Call -> TableVal =
        fun _ ctx call -> Fail.invalid ctx call.Span $"{name} returns a single value, where a table is needed"

    let scalar name family (minArgs, maxArgs) typeRule (impl: IEval -> Ctx -> Call -> DaxValue) : DaxFunction = {
        Name = name
        Family = family
        MinArgs = minArgs
        MaxArgs = maxArgs
        Returns = Returns.Scalar
        Check = fun _ -> None
        Type = typeRule
        Scalar = impl
        Table = noTable name
    }

    let table name family (minArgs, maxArgs) (impl: IEval -> Ctx -> Call -> TableVal) : DaxFunction = {
        Name = name
        Family = family
        MinArgs = minArgs
        MaxArgs = maxArgs
        Returns = Returns.Table
        Check = fun _ -> None
        Type = typeUnknown
        Scalar =
            fun ev ctx call ->
                // A table of one column and at most one row is a value.
                ev.ScalarOfTable(ctx, call.Span, impl ev ctx call)
        Table = impl
    }

    /// A function that only means something as an argument of CALCULATE.
    let modifier name family (minArgs, maxArgs) : DaxFunction = {
        Name = name
        Family = family
        MinArgs = minArgs
        MaxArgs = maxArgs
        Returns = Returns.Modifier
        Check = fun _ -> None
        Type = typeUnknown
        Scalar =
            fun _ ctx call -> Fail.invalid ctx call.Span $"{name} can only be used as a filter argument of CALCULATE"
        Table =
            fun _ ctx call -> Fail.invalid ctx call.Span $"{name} can only be used as a filter argument of CALCULATE"
    }

    let withCheck (check: Expr list -> string option) (f: DaxFunction) = { f with Check = check }

    /// The expression without the parentheses around it.
    let rec unparen (e: Expr) : Expr =
        match e.Kind with
        | Paren inner -> unparen inner
        | _ -> e

    /// The upper-case keyword an argument reads as (`DESC`, `ASC`), if it is a bare word.
    let keyword (e: Expr) : string option =
        match unparen e with
        | Keyword k -> Some k
        | _ -> None

    let isOmitted (e: Expr) =
        match e.Kind with
        | Omitted -> true
        | _ -> false
