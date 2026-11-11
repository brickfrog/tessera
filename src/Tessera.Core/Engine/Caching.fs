namespace Tessera.DaxEngine

// Caches that outlive one filter context: what makes an iterator's context transitions affordable.
//
// A `FilterCtx` already caches, for itself, the rows each table shows (`Visible`) and the value of
// each plain measure (`Measures`). But an iterator builds a new filter context per row, so those
// start empty every time, and the same work (the rows a date table shows under a filter that every
// row shares, the same measure under the same filters for every row of an outer iteration) is
// done again per row. The caches here are keyed on what the result is computed from, compared by
// content, so two filter contexts built separately share them:
//
//   - `VisCache` (one per `RelConfig`, so per model and relationship set-up): the rows of a table
//     under the filters that can reach it. The key is the table, the relationship it is reached
//     over, and the filters of the context that touch a table it depends on (`Deps`: the tables
//     its rows are computed from, a superset). Not used while a calculated column or table is
//     being computed (the rows then depend on what is computed so far), nor for a set-up whose
//     relationships let a table's rows depend on themselves (then the order of evaluation decides
//     what is refused). Bounded by size; cleared when full.
//   - `PredicateKey` (in `QueryDefs.Predicates`, so for one query): the filter a True/False filter
//     argument of CALCULATE stands for, when the predicate reads nothing but its own columns,
//     constants, variables and pure functions (`closedVariables` in Functions/Filter.fs). Keyed on
//     the expression, the model, the columns and the values of the variables it reads.
//   - `MeasureKey` (in `QueryDefs.Results`, so for one query): the value of a measure reference,
//     keyed on the measure, the model, the relationships, every filter of the context, and the
//     measures and functions being evaluated (which decide what is a circular dependency). Only
//     for a measure that is `ScopeBlind`: neither it nor anything it uses reads the context's
//     `ScopeMark`s (ALLSELECTED, ISFILTERED, ISCROSSFILTERED, HASONEFILTER, ISINSCOPE), which two
//     contexts with the same filters need not share; only without calculation groups, outside a
//     user-defined function called from a row context, and while nothing is being computed.
//
// Everything else a result reads is fixed for the model or the query: the data, the clock, the
// identity, the query's definitions, roles (a role's model is another model).

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open Tessera
open Tessera.DaxAst

module FilterKey =

    let private tupleHashes = ConditionalWeakTable<HashSet<int array>, obj>()

    let private isIntArraySet (s: HashSet<int array>) =
        obj.ReferenceEquals(s.Comparer, IntArrayComparer.Instance)

    /// Order-independent: the sum of the members' hashes.
    let private tuplesHash (s: HashSet<int array>) : int =
        match tupleHashes.TryGetValue s with
        | true, h -> unbox<int> h
        | _ ->
            let h =
                if isIntArraySet s then
                    let cmp = IntArrayComparer.Instance :> IEqualityComparer<int array>
                    let mutable h = s.Count

                    for t in s do
                        h <- h + cmp.GetHashCode t

                    h
                else
                    RuntimeHelpers.GetHashCode s

            tupleHashes.AddOrUpdate(s, box h)
            h

    let private sameTuples (a: HashSet<int array>) (b: HashSet<int array>) =
        obj.ReferenceEquals(a, b)
        || (isIntArraySet a
            && isIntArraySet b
            && a.Count = b.Count
            && tuplesHash a = tuplesHash b
            && a.SetEquals b)

    let private sameColumns (a: BColumn array) (b: BColumn array) =
        a.Length = b.Length
        && (let mutable same = true
            let mutable i = 0

            while same && i < a.Length do
                same <- obj.ReferenceEquals(a.[i], b.[i])
                i <- i + 1

            same)

    let hash (f: Filter) : int =
        match f with
        | OnColumn(c, ids) -> HashCode.Combine(1, RuntimeHelpers.GetHashCode c, ids.ContentHash)
        | Tuples(cols, tuples) ->
            let mutable h = 2

            for c in cols do
                h <- h * 31 + RuntimeHelpers.GetHashCode c

            HashCode.Combine(h, tuplesHash tuples)
        | Rows(t, rows, cfg) ->
            HashCode.Combine(3, RuntimeHelpers.GetHashCode t, rows.ContentHash, RuntimeHelpers.GetHashCode cfg)

    /// The same filter: the same columns (or table and relationships), allowing the same values.
    let same (a: Filter) (b: Filter) : bool =
        match a, b with
        | OnColumn(c, x), OnColumn(d, y) -> obj.ReferenceEquals(c, d) && x.SameAs y
        | Tuples(c, x), Tuples(d, y) -> sameColumns c d && sameTuples x y
        | Rows(t, x, cfg), Rows(u, y, cfg') -> obj.ReferenceEquals(t, u) && obj.ReferenceEquals(cfg, cfg') && x.SameAs y
        | _ -> false

    let hashAll (filters: Filter array) : int =
        let mutable h = filters.Length

        for f in filters do
            h <- h * 31 + hash f

        h

    let sameAll (a: Filter array) (b: Filter array) : bool =
        a.Length = b.Length
        && (let mutable ok = true
            let mutable i = 0

            while ok && i < a.Length do
                ok <- obj.ReferenceEquals(a.[i], b.[i]) || same a.[i] b.[i]
                i <- i + 1

            ok)

    /// The tables a filter is on: a table filter is on its whole expanded table.
    let touches (config: RelConfig) (f: Filter) (test: BTable -> bool) : bool =
        match f with
        | OnColumn(c, _) -> test c.Table
        | Tuples(cols, _) -> cols |> Array.exists (fun c -> test c.Table)
        | Rows(t, _, cfg) ->
            test t
            || (config.Expanded t |> Array.exists (fun (x, _) -> test x))
            || (not (obj.ReferenceEquals(cfg, config))
                && cfg.Expanded t |> Array.exists (fun (x, _) -> test x))

/// The key of a table's visible rows in `VisCache`.
[<Sealed; AllowNullLiteral>]
type VisKey(table: int, via: int, filters: Filter array) =
    let hash = HashCode.Combine(table, via, FilterKey.hashAll filters)
    member _.Table = table
    member _.Via = via
    member _.Filters = filters
    override _.GetHashCode() = hash

    override this.Equals(o: obj) =
        match o with
        | :? VisKey as k ->
            obj.ReferenceEquals(this, k)
            || (hash = k.GetHashCode()
                && table = k.Table
                && via = k.Via
                && FilterKey.sameAll filters k.Filters)
        | _ -> false

/// The visible rows of tables under filters, for one relationship set-up (see the top of the file).
[<Sealed; AllowNullLiteral>]
type VisCache(config: RelConfig) =
    static let limit = 64L * 1024L * 1024L
    let entries = ConcurrentDictionary<VisKey, RowSet>()
    let mutable bytes = 0L

    let model = config.Model

    /// Per table, the tables whose filters and rows its rows are computed from (`Filters.compute`
    /// in Context.fs): itself, the tables it reaches over many-to-one relationships, over
    /// relationships that filter both ways and over limited ones, and their expanded tables.
    let deps =
        lazy
            (model.Tables
             |> Array.map (fun t ->
                 let seen = Array.zeroCreate<bool> model.Tables.Length
                 let stack = Stack<BTable>()

                 let visit (x: BTable) =
                     if not seen.[x.Id] then
                         seen.[x.Id] <- true
                         stack.Push x

                 visit t

                 while stack.Count > 0 do
                     let x = stack.Pop()

                     for (y, _) in config.Expanded x do
                         visit y

                     for r in config.ManyToOne x do
                         visit r.To.Table

                     for r in config.OneToMany x do
                         if config.FiltersBothWays r || r.Limited || config.Both.[r.Id] then
                             visit r.From.Table

                 seen))

    /// Whether a table's rows can never be asked for while they are being computed: the calls
    /// `Filters.compute` makes (table, relationship come over) have no cycle. Where they do,
    /// whether a filter is refused depends on which table was asked first, so nothing is shared.
    let acyclic =
        lazy
            (let n = model.Rels.Length + 1
             let state = Array.zeroCreate<byte>(model.Tables.Length * n)
             let mutable cyclic = false

             let next (t: BTable) (via: BRel) = [
                 for r in config.ManyToOne t do
                     if not (obj.ReferenceEquals(r, via)) then
                         yield r.To.Table, r

                 for r in config.OneToMany t do
                     if
                         not (obj.ReferenceEquals(r, via))
                         && (config.FiltersBothWays r || (r.Limited && config.Both.[r.Id]))
                     then
                         yield r.From.Table, r
             ]

             let rec walk (t: BTable) (via: BRel) =
                 let key = t.Id * n + (if isNull via then 0 else via.Id + 1)

                 match state.[key] with
                 | 1uy -> cyclic <- true
                 | 2uy -> ()
                 | _ ->
                     state.[key] <- 1uy

                     for (x, r) in next t via do
                         if not cyclic then
                             walk x r

                     state.[key] <- 2uy

             for t in model.Tables do
                 if not cyclic then
                     walk t null

             not cyclic)

    /// The cache of a relationship set-up.
    static member Of(config: RelConfig) : VisCache =
        match config.VisCache with
        | :? VisCache as c -> c
        | _ ->
            Interlocked.CompareExchange(&config.VisCache, box (VisCache config), null)
            |> ignore

            config.VisCache :?> VisCache

    /// Whether the cache may be used now.
    member _.Usable: bool = model.Computing.Count = 0 && acyclic.Value

    member _.Key(filters: Filter list, t: BTable, via: BRel) : VisKey =
        let d = deps.Value.[t.Id]
        let test (x: BTable) = x.Id < d.Length && d.[x.Id]

        let relevant =
            filters
            |> List.filter (fun f -> FilterKey.touches config f test)
            |> Array.ofList

        VisKey(t.Id, (if isNull via then -1 else via.Id), relevant)

    member _.TryGet(key: VisKey, rows: byref<RowSet>) : bool = entries.TryGetValue(key, &rows)

    member _.Add(key: VisKey, rows: RowSet) =
        let size =
            128L
            + 16L * int64 key.Filters.Length
            + (if rows.IsAll then
                   0L
               else
                   int64 ((rows.Size + 63) / 64) * 8L + int64 rows.Count * 4L)

        if Interlocked.Add(&bytes, size) > limit then
            entries.Clear()
            Interlocked.Exchange(&bytes, size) |> ignore

        entries.[key] <- rows

    /// How many entries there are (for tests).
    member _.Count = entries.Count

/// Values as keys: the same case and the same bits (so 0.0 and -0.0 differ, as they can in DAX).
module ValueKey =
    let same (a: DaxValue) (b: DaxValue) : bool =
        match a, b with
        | DaxValue.Blank, DaxValue.Blank -> true
        | DaxValue.Integer x, DaxValue.Integer y -> x = y
        | DaxValue.Decimal x, DaxValue.Decimal y -> BitConverter.DoubleToInt64Bits x = BitConverter.DoubleToInt64Bits y
        | DaxValue.Currency x, DaxValue.Currency y -> x = y
        | DaxValue.String x, DaxValue.String y -> String.Equals(x, y, StringComparison.Ordinal)
        | DaxValue.Boolean x, DaxValue.Boolean y -> x = y
        | DaxValue.DateTime x, DaxValue.DateTime y ->
            BitConverter.DoubleToInt64Bits x = BitConverter.DoubleToInt64Bits y
        | _ -> false

    let hash (v: DaxValue) : int =
        match v with
        | DaxValue.Blank -> 1
        | DaxValue.Integer x -> HashCode.Combine(2, x)
        | DaxValue.Decimal x -> HashCode.Combine(3, BitConverter.DoubleToInt64Bits x)
        | DaxValue.Currency x -> HashCode.Combine(4, x)
        | DaxValue.String x -> HashCode.Combine(5, StringComparer.Ordinal.GetHashCode x)
        | DaxValue.Boolean x -> HashCode.Combine(6, x)
        | DaxValue.DateTime x -> HashCode.Combine(7, BitConverter.DoubleToInt64Bits x)

/// The key of a True/False filter argument's filter in `QueryDefs.Predicates`: the expression, the
/// model, the columns it filters, and the values of the variables it reads (see `predicateFilter` in Functions/Filter.fs).
[<Sealed; AllowNullLiteral>]
type PredicateKey(expr: Expr, model: Model, columns: BColumn array, values: DaxValue array) =
    let hash =
        let mutable h =
            HashCode.Combine(RuntimeHelpers.GetHashCode expr, RuntimeHelpers.GetHashCode model)

        for c in columns do
            h <- h * 31 + RuntimeHelpers.GetHashCode c

        for v in values do
            h <- h * 31 + ValueKey.hash v

        h

    member _.Expr = expr
    member _.Model = model
    member _.Columns = columns
    member _.Values = values
    override _.GetHashCode() = hash

    override this.Equals(o: obj) =
        match o with
        | :? PredicateKey as k ->
            obj.ReferenceEquals(this, k)
            || (hash = k.GetHashCode()
                && obj.ReferenceEquals(expr, k.Expr)
                && obj.ReferenceEquals(model, k.Model)
                && columns.Length = k.Columns.Length
                && Array.forall2 (fun (a: BColumn) b -> obj.ReferenceEquals(a, b)) columns k.Columns
                && values.Length = k.Values.Length
                && Array.forall2 ValueKey.same values k.Values)
        | _ -> false

/// The most entries a per-query cache (`QueryDefs.Predicates`, `QueryDefs.Results`) keeps: it is
/// emptied when full.
module QueryCache =
    let limit = 200_000

/// The key of a measure's value in `QueryDefs.Results` (see the top of the file).
[<Sealed; AllowNullLiteral>]
type MeasureKey
    (
        measure: BMeasure,
        model: Model,
        config: RelConfig,
        filters: Filter array,
        measures: BMeasure list,
        functions: BFunction list
    ) =
    let hash =
        let mutable h =
            HashCode.Combine(
                RuntimeHelpers.GetHashCode measure,
                RuntimeHelpers.GetHashCode model,
                RuntimeHelpers.GetHashCode config,
                FilterKey.hashAll filters
            )

        for m in measures do
            h <- h * 31 + RuntimeHelpers.GetHashCode m

        for f in functions do
            h <- h * 31 + RuntimeHelpers.GetHashCode f

        h

    static let rec sameList (a: 'a list) (b: 'a list) =
        match a, b with
        | [], [] -> true
        | x :: a, y :: b -> obj.ReferenceEquals(x, y) && sameList a b
        | _ -> false

    member _.Measure = measure
    member _.Model = model
    member _.Config = config
    member _.Filters = filters
    member _.Measures = measures
    member _.Functions = functions
    override _.GetHashCode() = hash

    override this.Equals(o: obj) =
        match o with
        | :? MeasureKey as k ->
            obj.ReferenceEquals(this, k)
            || (hash = k.GetHashCode()
                && obj.ReferenceEquals(measure, k.Measure)
                && obj.ReferenceEquals(model, k.Model)
                && obj.ReferenceEquals(config, k.Config)
                && sameList measures k.Measures
                && sameList functions k.Functions
                && FilterKey.sameAll filters k.Filters)
        | _ -> false

module MeasureCache =

    /// The functions that read a filter context's `ScopeMark`s (Functions/Filter.fs).
    let private readsScope =
        HashSet<string>(
            [ "ALLSELECTED"; "ISFILTERED"; "ISCROSSFILTERED"; "HASONEFILTER"; "ISINSCOPE" ],
            StringComparer.OrdinalIgnoreCase
        )

    let private isInternal (name: string) =
        name.StartsWith("_", StringComparison.Ordinal)
        && name.EndsWith(" FormatString", StringComparison.OrdinalIgnoreCase)

    /// Whether nothing the measure evaluates, through the measures and user-defined functions it
    /// names, reads the scope marks. Names are resolved loosely (any measure of the model or the
    /// query with the name counts), which can only find more.
    let scopeBlind (model: Model) (query: QueryDefs) (m: BMeasure) : bool =
        let known =
            match model.Measures.TryGetValue m.Name with
            | true, x when obj.ReferenceEquals(x, m) -> true
            | _ ->
                match query.Measures.TryGetValue m.Name with
                | true, x -> obj.ReferenceEquals(x, m)
                | _ -> false

        if not known then
            false
        else
            match query.ScopeBlind.TryGetValue m with
            | true, b -> b
            | _ ->
                let seenMeasures = HashSet<BMeasure>(HashIdentity.Reference)
                let seenFunctions = HashSet<BFunction>(HashIdentity.Reference)
                let mutable blind = true

                let rec refs (names: NameRef list) =
                    for r in names do
                        if blind then
                            match r.Kind with
                            | NameKind.Function ->
                                if readsScope.Contains r.Name then
                                    blind <- false
                                else
                                    for fs in [ query.Functions; model.Functions ] do
                                        match fs.TryGetValue r.Name with
                                        | true, f when seenFunctions.Add f ->
                                            refs (Tessera.DaxAst.functionReferences f.Def)
                                        | _ -> ()
                            | NameKind.Bracketed
                            | NameKind.Qualified ->
                                if isInternal r.Name then
                                    blind <- false
                                else
                                    for ms in [ query.Measures; model.Measures ] do
                                        match ms.TryGetValue r.Name with
                                        | true, x when seenMeasures.Add x -> refs (Tessera.DaxAst.references x.Expr)
                                        | _ -> ()
                            | _ -> ()

                seenMeasures.Add m |> ignore

                try
                    refs (Tessera.DaxAst.references m.Expr)
                with _ ->
                    // A measure that doesn't parse: its evaluation is the error.
                    blind <- false

                query.ScopeBlind.[m] <- blind
                blind
