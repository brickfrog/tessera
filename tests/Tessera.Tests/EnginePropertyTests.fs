/// Properties of the local DAX engine: identities DAX guarantees, and the filter context checked
/// against its definition on random small models.
module Tessera.Tests.EnginePropertyTests

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

let private config = {
    FsCheckConfig.defaultConfig with
        maxTest = 150
}

// ---- Identities on the star model, under random filters -----------------------------------------

/// Filter tables, usable as CALCULATE arguments and as SUMMARIZECOLUMNS filter tables.
let private pool = [|
    "TREATAS ( { \"Red\" }, Product[Color] )"
    "TREATAS ( { \"Red\", \"Blue\" }, Product[Color] )"
    "TREATAS ( { \"Safety\" }, Product[Category] )"
    "TREATAS ( { \"US\" }, Customer[Country] )"
    "TREATAS ( { 2024 }, 'Date'[Year] )"
    "TREATAS ( { \"Jan\", \"Feb\" }, 'Date'[Month] )"
    "FILTER ( ALL ( Sales[Qty] ), Sales[Qty] > 1 )"
    "FILTER ( ALL ( Product[Color] ), ISBLANK ( Product[Color] ) )"
    "FILTER ( ALL ( Product[Name] ), Product[Name] <> \"Bike\" )"
|]

/// Up to three of the pool's filters, chosen by a seed, as ", f1, f2".
let private filters (seed: int) : string =
    let random = Random(seed)
    let n = random.Next 4

    [ for _ in 1..n -> pool.[random.Next pool.Length] ]
    |> List.distinct
    |> List.map (fun f -> ", " + f)
    |> String.concat ""

let private same (seed: int) (a: string) (b: string) =
    let p = starPrepared.Value
    Expect.equal (scalar p a) (scalar p b) $"seed {seed}:\n{a}\n{b}"

// ---- The filter context against its definition ---------------------------------------------------
//
// Random small models and random filters, and for each table the number of rows the DEFINITION of
// a filter context gives (the header of Engine/Context.fs, from SQLBI's "Expanded tables in DAX"):
//
//   - a filter is a set of columns with the combinations of values allowed in them;
//   - a table's EXPANDED rows are its rows joined to the rows they relate to over many-to-one
//     relationships, with BLANK where a key has no match (that table then has a blank row);
//   - a row is visible when, for every filter, its expanded row has an allowed combination in the
//     filter's columns that the expanded table has;
//   - a table used as a filter is a filter on every column of its expanded table;
//   - removing a column from a filter leaves the combinations of its other columns;
//   - a relationship that filters both ways shows, on the one side, only the rows that visible
//     many-side rows refer to, once something filters the many side.
//
// The oracle below is that and nothing else: no indexes, no propagation order, no shortcuts.

/// A value: None is BLANK.
type private V = string option

type private Tab = {
    Name: string
    Cols: string list
    Rows: V list list
}

type private Link = {
    From: string * string
    To: string * string
}

type private World = {
    Tables: Tab list
    Links: Link list
    /// F[K2] <-> D2[K] filters both ways.
    Both: bool
}

/// A filter: columns (table, column) and the allowed combinations.
type private Def = {
    Cols: (string * string) list
    Tuples: Set<V list>
}

type private Spec =
    | Treatas of (string * string) list * V list list
    | Above of int

type private Inner =
    | Nothing
    | RemoveA
    | ReplaceA of string
    | KeepA of string
    | AllD1
    | AllF

type private Scenario = {
    World: World
    Outer: Spec list
    Inner: Inner
}

module private Oracle =
    let tab (w: World) (name: string) =
        w.Tables |> List.find (fun t -> t.Name = name)

    let has (w: World) (name: string) =
        w.Tables |> List.exists (fun t -> t.Name = name)

    let cell (t: Tab) (col: string) (row: V list) : V = row.[List.findIndex ((=) col) t.Cols]

    let linksFrom (w: World) (name: string) =
        w.Links |> List.filter (fun l -> fst l.From = name)

    /// The stored row of the To table a key relates to, if any.
    let related (w: World) (l: Link) (key: V) : V list option =
        let target = tab w (fst l.To)

        match key with
        | None -> None
        | Some _ -> target.Rows |> List.tryFind (fun r -> cell target (snd l.To) r = key)

    /// A table has the blank row when a stored row of a table that relates to it has no match.
    let blankRow (w: World) (name: string) : bool =
        w.Links
        |> List.exists (fun l ->
            fst l.To = name
            && (let source = tab w (fst l.From)

                source.Rows
                |> List.exists (fun r -> (related w l (cell source (snd l.From) r)).IsNone)))

    let blank (t: Tab) : V list = t.Cols |> List.map (fun _ -> None)

    /// The rows of a table, the blank row last when it has one.
    let rows (w: World) (name: string) : V list list =
        let t = tab w name
        t.Rows @ (if blankRow w name then [ blank t ] else [])

    /// The tables of a table's expanded table.
    let rec expandedTables (w: World) (name: string) : string list =
        name
        :: (linksFrom w name |> List.collect (fun l -> expandedTables w (fst l.To)))

    /// A row with the rows it relates to: every column of the expanded table.
    let rec expand (w: World) (name: string) (row: V list) : Map<string * string, V> =
        let t = tab w name
        let own = t.Cols |> List.map (fun c -> (name, c), cell t c row)

        let reached =
            linksFrom w name
            |> List.collect (fun l ->
                let target = tab w (fst l.To)

                let next =
                    match related w l (cell t (snd l.From) row) with
                    | Some r -> r
                    | None -> blank target

                expand w target.Name next |> Map.toList)

        Map.ofList (own @ reached)

    /// The rows of a table that every filter lets through.
    let passing (w: World) (filters: Def list) (name: string) : V list list =
        let tables = expandedTables w name

        rows w name
        |> List.filter (fun row ->
            let e = expand w name row

            filters
            |> List.forall (fun f ->
                let here = [
                    for i, (t, _) in List.indexed f.Cols do
                        if List.contains t tables then
                            i
                ]

                here.IsEmpty
                || (let mine = here |> List.map (fun i -> e.[f.Cols.[i]])

                    f.Tuples
                    |> Set.exists (fun tuple -> (here |> List.map (fun i -> tuple.[i])) = mine))))

    /// `passing`, and for D2 under a both-directions relationship: only the rows a visible row of F
    /// refers to, once a filter is on something of F's that is not D2's.
    let visible (w: World) (filters: Def list) (name: string) : V list list =
        let own = passing w filters name

        let reachesF =
            filters
            |> List.exists (fun f ->
                f.Cols
                |> List.exists (fun (t, _) -> t <> "D2" && List.contains t (expandedTables w "F")))

        if name = "D2" && w.Both && reachesF then
            let d2 = tab w "D2"

            let referred =
                passing w filters "F"
                |> List.map (fun r -> let e = expand w "F" r in d2.Cols |> List.map (fun c -> e.[("D2", c)]))

            own |> List.filter (fun r -> List.contains r referred)
        else
            own

    let present (w: World) ((t, c): string * string) (v: V) =
        rows w t |> List.exists (fun r -> cell (tab w t) c r = v)

    /// An outer filter as its definition.
    let define (w: World) (spec: Spec) : Def =
        match spec with
        | Treatas(cols, tuples) ->
            // TREATAS "filters out any values that are not present in its respective output column".
            {
                Cols = cols
                Tuples =
                    tuples
                    |> List.filter (fun tuple -> List.forall2 (present w) cols tuple)
                    |> Set.ofList
            }
        | Above threshold ->
            // FILTER ( F, F[X] > n ): rows of F, so a filter on every column of F's expanded table.
            let f = tab w "F"

            let cols =
                expandedTables w "F"
                |> List.collect (fun t -> (tab w t).Cols |> List.map (fun c -> t, c))

            {
                Cols = cols
                Tuples =
                    f.Rows
                    |> List.filter (fun r -> int (cell f "X" r).Value > threshold)
                    |> List.map (fun r -> let e = expand w "F" r in cols |> List.map (fun c -> e.[c]))
                    |> Set.ofList
            }

    let without (drop: string * string -> bool) (filters: Def list) : Def list =
        filters
        |> List.choose (fun f ->
            let keep = [
                for i, c in List.indexed f.Cols do
                    if not (drop c) then
                        i
            ]

            if keep.IsEmpty then
                None
            else
                Some {
                    Cols = keep |> List.map (fun i -> f.Cols.[i])
                    Tuples = f.Tuples |> Set.map (fun t -> keep |> List.map (fun i -> t.[i]))
                })

    /// The filters in force inside CALCULATE ( CALCULATE ( ..., inner ), outer ... ).
    let filters (s: Scenario) : Def list =
        let w = s.World
        let outer = s.Outer |> List.map (define w)
        let a = "D1", "A"

        let only (v: string) = {
            Cols = [ a ]
            Tuples = set [ [ Some v ] ]
        }

        match s.Inner with
        | Nothing -> outer
        | RemoveA -> without ((=) a) outer
        | ReplaceA v -> without ((=) a) outer @ [ only v ]
        | KeepA v -> outer @ [ only v ]
        | AllD1 -> let tables = expandedTables w "D1" in without (fun (t, _) -> List.contains t tables) outer
        | AllF -> let tables = expandedTables w "F" in without (fun (t, _) -> List.contains t tables) outer

let private scenario (seed: int) : Scenario =
    let r = Random(seed)
    let pick (items: 'a array) = items.[r.Next items.Length]

    let some (n: int) (make: unit -> 'a) =
        [ for _ in 1 .. 1 + r.Next n -> make () ] |> List.distinct

    let letters = [| "a"; "b"; "c" |]
    let marks = [| "x"; "y" |]
    let kinds = [| "p"; "q" |]
    let v (x: obj) : V = Some(string x)
    // 0: one fact; 1: a second fact sharing both dimensions; 2: D1 relates on to G.
    let shape = r.Next 3
    let n1 = 1 + r.Next 4
    let nG = 1 + r.Next 2

    let d1 = {
        Name = "D1"
        Cols = [ "K"; "A"; "B" ] @ (if shape = 2 then [ "GK" ] else [])
        Rows = [
            for k in 1..n1 ->
                [ v k; v (pick letters); v (1 + r.Next 2) ]
                @ (if shape = 2 then [ v (1 + r.Next nG) ] else [])
        ]
    }

    let d2 = {
        Name = "D2"
        Cols = [ "K"; "C" ]
        Rows = [ for k in 1 .. 1 + r.Next 3 -> [ v k; v (pick marks) ] ]
    }

    let key1 () : V =
        // With G behind D1, every key matches: a blank row at the start of a chain is refused.
        if shape = 2 then v (1 + r.Next n1)
        elif r.Next 6 = 0 then None
        else v (1 + r.Next 5)

    let f = {
        Name = "F"
        Cols = [ "Id"; "K1"; "K2"; "X" ]
        Rows = [
            for id in 1 .. (if shape = 1 then 3 + r.Next 6 else r.Next 9) ->
                [ v id; key1 (); v (1 + r.Next 3); v (r.Next 4) ]
        ]
    }

    let f2 = {
        Name = "F2"
        Cols = [ "Id"; "K1"; "K2"; "Y" ]
        Rows = [
            for id in 1 .. 3 + r.Next 6 -> [ v id; key1 (); v (1 + r.Next 3); v (r.Next 4) ]
        ]
    }

    let g = {
        Name = "G"
        Cols = [ "K"; "E" ]
        Rows = [ for k in 1..nG -> [ v k; v (pick kinds) ] ]
    }

    let world = {
        Tables =
            [ d1; d2; f ]
            @ (if shape = 1 then [ f2 ] else [])
            @ (if shape = 2 then [ g ] else [])
        Links =
            [ { From = "F", "K1"; To = "D1", "K" }; { From = "F", "K2"; To = "D2", "K" } ]
            @ (if shape = 1 then
                   [ { From = "F2", "K1"; To = "D1", "K" }; { From = "F2", "K2"; To = "D2", "K" } ]
               else
                   [])
            @ (if shape = 2 then
                   [ { From = "D1", "GK"; To = "G", "K" } ]
               else
                   [])
        // Two facts over two dimensions with a both-directions relationship give two filter paths.
        Both = shape <> 1 && r.Next 2 = 0
    }

    {
        World = world
        Outer =
            // With two facts, always a table filter on the first: what it does to the second is the
            // case where the PAIRS of dimension rows matter.
            (if shape = 1 then [ Above(r.Next 3) ] else [])
            @ [
                for _ in 1 .. r.Next 4 ->
                    match r.Next(if shape = 2 then 6 else 5) with
                    | 0 ->
                        Treatas([ "D1", "A" ], some 2 (fun () -> [ (if r.Next 4 = 0 then None else v (pick letters)) ]))
                    | 1 -> Treatas([ "D1", "A"; "D2", "C" ], some 3 (fun () -> [ v (pick letters); v (pick marks) ]))
                    | 2 -> Treatas([ "F", "X" ], some 2 (fun () -> [ v (r.Next 4) ]))
                    | 3 -> Above(r.Next 3)
                    | 4 -> Treatas([ "D1", "A"; "D1", "B" ], some 3 (fun () -> [ v (pick letters); v (1 + r.Next 2) ]))
                    | _ -> Treatas([ "G", "E" ], some 2 (fun () -> [ v (pick kinds) ]))
            ]
        Inner =
            match r.Next 8 with
            | 0 -> RemoveA
            | 1 -> ReplaceA(pick letters)
            | 2 -> KeepA(pick letters)
            | 3 -> AllD1
            | 4 -> AllF
            | _ -> Nothing
    }

/// Columns whose values are whole numbers; the others are text.
let private whole = set [ "K"; "B"; "GK"; "Id"; "K1"; "K2"; "X"; "Y" ]

let private literal (col: string) (value: V) : string =
    match value with
    | None -> "BLANK ()"
    | Some s -> if whole.Contains col then s else "\"" + s + "\""

let private outerDax (spec: Spec) : string =
    match spec with
    | Above t -> $"FILTER ( F, F[X] > {t} )"
    | Treatas(cols, tuples) ->
        let row (tuple: V list) =
            match List.map2 (fun (_, c) value -> literal c value) cols tuple with
            | [ one ] -> one
            | several -> "( " + String.concat ", " several + " )"

        let targets = cols |> List.map (fun (t, c) -> $"{t}[{c}]") |> String.concat ", "

        "TREATAS ( { "
        + (tuples |> List.map row |> String.concat ", ")
        + " }, "
        + targets
        + " )"

let private innerDax (inner: Inner) : string =
    match inner with
    | Nothing -> ""
    | RemoveA -> ", ALL ( D1[A] )"
    | ReplaceA v -> $", D1[A] = \"{v}\""
    | KeepA v -> $", KEEPFILTERS ( D1[A] = \"{v}\" )"
    | AllD1 -> ", ALL ( D1 )"
    | AllF -> ", ALL ( F )"

let private model (w: World) : TestModel = {
    Tables = [
        for t in w.Tables ->
            table t.Name [
                for c in t.Cols -> c, (if whole.Contains c then DataType.Int64 else DataType.String)
            ] [
                for row in t.Rows ->
                    [
                        for c, value in List.zip t.Cols row ->
                            match value with
                            | None -> null
                            | Some s -> if whole.Contains c then box (int s) else box s
                    ]
            ]
    ]
    Rels = [
        for l in w.Links ->
            if w.Both && l.From = ("F", "K2") then
                bothWays (rel l.From l.To)
            else
                rel l.From l.To
    ]
    Measures = []
    Functions = []
}

let private count (n: int) : QueryValue =
    if n = 0 then
        QueryValue.Blank
    else
        QueryValue.Int64(int64 n)

[<Tests>]
let tests =
    testList "Engine properties" [
        testPropertyWithConfig config "SUMX ( T, T[c] ) = SUM ( T[c] ) under any filters"
        <| fun (seed: int) ->
            let f = filters seed
            same seed $"CALCULATE ( SUMX ( Sales, Sales[Qty] ){f} )" $"CALCULATE ( SUM ( Sales[Qty] ){f} )"
            same seed $"CALCULATE ( MAXX ( Sales, Sales[Price] ){f} )" $"CALCULATE ( MAX ( Sales[Price] ){f} )"
            same seed $"CALCULATE ( COUNTX ( Sales, Sales[Id] ){f} )" $"CALCULATE ( COUNT ( Sales[Id] ){f} )"

        testPropertyWithConfig config "CALCULATE ( m, ALL ( T ) ) does not depend on the filters on T's expanded table"
        <| fun (seed: int) ->
            let f = filters seed
            same seed $"CALCULATE ( CALCULATE ( [Amount], ALL ( Sales ) ){f} )" "625"
            same seed $"CALCULATE ( CALCULATE ( [Amount], REMOVEFILTERS () ){f} )" "625"

        testPropertyWithConfig
            config
            "COUNTROWS ( DISTINCT ( c ) ) = DISTINCTCOUNT ( c ), and VALUES adds the blank row when it is visible"
        <| fun (seed: int) ->
            let f = filters seed

            // Where the table has no blank row, or it is hidden, DISTINCTCOUNT is COUNTROWS ( DISTINCT ).
            same
                seed
                $"CALCULATE ( COUNTROWS ( DISTINCT ( Customer[Name] ) ){f} )"
                $"CALCULATE ( DISTINCTCOUNT ( Customer[Name] ){f} )"

            same
                seed
                $"CALCULATE ( COUNTROWS ( DISTINCT ( Sales[ProductKey] ) ){f} )"
                $"CALCULATE ( DISTINCTCOUNT ( Sales[ProductKey] ){f} )"

            // On Product it is that too, or refused: exactly when the blank row is visible.
            let p = starPrepared.Value

            let blankRowShown =
                scalar p $"CALCULATE ( COUNTROWS ( VALUES ( Product ) ) - COUNTROWS ( Product ) + 0{f} )"

            match Engine.evaluateScalar p $"CALCULATE ( DISTINCTCOUNT ( Product[Color] ){f} )" with
            | Ok v ->
                Expect.equal blankRowShown (QueryValue.Int64 0L) $"seed {seed}: answered, so the blank row is hidden"
                Expect.equal v (scalar p $"CALCULATE ( COUNTROWS ( DISTINCT ( Product[Color] ) ){f} )") $"seed {seed}"
            | Error(DaxNotSupported _) ->
                Expect.equal blankRowShown (QueryValue.Int64 1L) $"seed {seed}: refused, so the blank row is visible"
            | Error e -> failtestf "seed %d: %s" seed (TessError.message e)

            same
                seed
                $"CALCULATE ( COUNTROWS ( VALUES ( Product[Color] ) ) - COUNTROWS ( DISTINCT ( Product[Color] ) ) + 0{f} )"
                $"CALCULATE ( COUNTROWS ( VALUES ( Product ) ) - COUNTROWS ( Product ) + 0{f} )"

            same
                seed
                $"CALCULATE ( HASONEVALUE ( Product[Color] ){f} )"
                $"CALCULATE ( COUNTROWS ( VALUES ( Product[Color] ) ) = 1{f} )"

        testPropertyWithConfig config "an additive measure over the groups of SUMMARIZECOLUMNS adds up to its total"
        <| fun (seed: int) ->
            let f = filters seed

            same
                seed
                $"SUMX ( SUMMARIZECOLUMNS ( Product[Color], Customer[Country]{f}, \"A\", [Amount] ), [A] )"
                $"CALCULATE ( [Amount]{f} )"

            same
                seed
                $"SUMX ( SUMMARIZECOLUMNS ( 'Date'[Year], 'Date'[Month]{f}, \"U\", [Units] ), [U] )"
                $"CALCULATE ( [Units]{f} )"

        testPropertyWithConfig config "an additive measure over the rows of a dimension adds up to its total"
        <| fun (seed: int) ->
            let f = filters seed
            same seed $"CALCULATE ( SUMX ( VALUES ( Product ), [Amount] ){f} )" $"CALCULATE ( [Amount]{f} )"
            same seed $"CALCULATE ( SUMX ( VALUES ( Customer[Name] ), [Amount] ){f} )" $"CALCULATE ( [Amount]{f} )"

            same
                seed
                $"CALCULATE ( SUMX ( SUMMARIZE ( Sales, Product[Color], 'Date'[Year] ), [Amount] ){f} )"
                $"CALCULATE ( [Amount]{f} )"

        testPropertyWithConfig
            { config with maxTest = 1500 }
            "the rows a filter context shows are the rows its definition gives"
        <| fun (seed: int) ->
            let s = scenario seed
            let w = s.World
            let p = prepare (model w)
            let filters = Oracle.filters s
            let outer = s.Outer |> List.map (fun f -> ", " + outerDax f) |> String.concat ""

            for t in w.Tables do
                // A fact table by its stored rows, a dimension with its blank row.
                let expression, expected =
                    if t.Name.StartsWith "F" then
                        $"COUNTROWS ( {t.Name} )",
                        (Oracle.visible w filters t.Name |> List.filter (fun r -> List.contains r t.Rows)).Length
                    else
                        $"COUNTROWS ( VALUES ( {t.Name} ) )", (Oracle.visible w filters t.Name).Length

                let query = $"CALCULATE ( CALCULATE ( {expression}{innerDax s.Inner} ){outer} )"

                match Engine.evaluateScalar p query with
                | Ok v -> Expect.equal v (count expected) $"seed {seed}: {t.Name}\n{query}\n%A{s}"
                // The one shape the engine refuses here: a both-directions filter that keeps all rows.
                | Error(DaxNotSupported(what, _)) when w.Both && what.Contains "keeps all of its rows" -> ()
                | Error e -> failtestf "seed %d: %s: %s\n%s\n%A" seed t.Name (TessError.message e) query s

        // SUMMARIZECOLUMNS ( D1[A], "n", COUNTROWS ( F ) ) under the scenario's outer filters (in
        // CALCULATETABLE, or as its own filter arguments): a row per value of D1[A], the blank row's
        // BLANK included, for which the count is not BLANK, and that count is the one the
        // definition gives with D1[A] = the value as one more filter. A filter on D1[A] together
        // with other columns is left out: whether the group-by value replaces or narrows it is not
        // what this checks.
        testPropertyWithConfig
            { config with maxTest = 600 }
            "SUMMARIZECOLUMNS groups and counts are the ones the definition of the filter context gives"
        <| fun (seed: int) ->
            let s = { scenario seed with Inner = Nothing }
            let w = s.World

            let mixed =
                s.Outer
                |> List.exists (fun f ->
                    match f with
                    | Treatas(cols, _) -> cols.Length > 1 && List.contains ("D1", "A") cols
                    | Above _ -> false)

            if not mixed then
                let p = prepare (model w)
                let filters = Oracle.filters s
                let d1 = Oracle.tab w "D1"
                let f = Oracle.tab w "F"

                let expected =
                    Oracle.rows w "D1"
                    |> List.map (Oracle.cell d1 "A")
                    |> List.distinct
                    |> List.choose (fun a ->
                        let group = {
                            Cols = [ "D1", "A" ]
                            Tuples = set [ [ a ] ]
                        }

                        let n =
                            (Oracle.visible w (filters @ [ group ]) "F"
                             |> List.filter (fun r -> List.contains r f.Rows))
                                .Length

                        if n = 0 then None else Some(a, n))
                    |> List.sort

                let outer = s.Outer |> List.map (fun f -> ", " + outerDax f) |> String.concat ""

                for query in
                    [
                        $"EVALUATE CALCULATETABLE ( SUMMARIZECOLUMNS ( D1[A], \"n\", COUNTROWS ( F ) ){outer} )"
                        $"EVALUATE SUMMARIZECOLUMNS ( D1[A]{outer}, \"n\", COUNTROWS ( F ) )"
                    ] do
                    match Engine.evaluateQuery p query with
                    | Ok(r :: _) ->
                        let got =
                            r.Rows
                            |> List.map (fun (row: QueryValue array) ->
                                let a =
                                    match row.[0] with
                                    | QueryValue.String x -> Some x
                                    | QueryValue.Blank -> None
                                    | other -> failtestf "seed %d: %A" seed other

                                let n =
                                    match row.[1] with
                                    | QueryValue.Int64 n -> int n
                                    | other -> failtestf "seed %d: %A" seed other

                                a, n)
                            |> List.sort

                        Expect.equal got expected $"seed {seed}\n{query}\n%A{s}"
                    | Ok [] -> failtest "no result"
                    | Error(DaxNotSupported(what, _)) when w.Both && what.Contains "keeps all of its rows" -> ()
                    | Error e -> failtestf "seed %d: %s\n%s\n%A" seed (TessError.message e) query s

        // The property above is only worth what its scenarios reach: check that they do reach the
        // cases they are for, with results that the filters actually decide.
        testCase "the scenarios cover two facts, a chain, both directions and inner CALCULATEs"
        <| fun () ->
            let scenarios = [ for seed in 0..1499 -> scenario seed ]

            /// How many scenarios satisfy `kind` and show some, but not all, rows of `table`.
            let decided (kind: Scenario -> bool) (table: string) =
                scenarios
                |> List.filter (fun s ->
                    kind s
                    && Oracle.has s.World table
                    && (let all = (Oracle.rows s.World table).Length
                        let shown = (Oracle.visible s.World (Oracle.filters s) table).Length
                        shown > 0 && shown < all))
                |> List.length

            let tableFilter (s: Scenario) =
                s.Outer
                |> List.exists (fun f ->
                    match f with
                    | Above _ -> true
                    | _ -> false)

            let inner (s: Scenario) = s.Inner <> Nothing

            Expect.isGreaterThan (decided tableFilter "F2") 100 "a table filter on F deciding the rows of F2"

            // ... and in some of them only the pairs of dimension rows give the answer: filtering
            // each dimension on its own would show more rows of F2.
            let pairsMatter =
                scenarios
                |> List.filter (fun s ->
                    Oracle.has s.World "F2"
                    && (let w = s.World
                        let filters = Oracle.filters s

                        let part (table: string) (row: V list) =
                            let e = Oracle.expand w "F2" row
                            (Oracle.tab w table).Cols |> List.map (fun c -> e.[(table, c)])

                        let d1 = Oracle.visible w filters "D1"
                        let d2 = Oracle.visible w filters "D2"

                        let separately =
                            (Oracle.tab w "F2").Rows
                            |> List.filter (fun r -> List.contains (part "D1" r) d1 && List.contains (part "D2" r) d2)

                        (Oracle.visible w filters "F2").Length < separately.Length))
                |> List.length

            Expect.isGreaterThan pairsMatter 25 "scenarios where the pairs matter"
            Expect.isGreaterThan (decided (fun _ -> true) "G") 30 "the far end of a chain"
            Expect.isGreaterThan (decided (fun s -> s.World.Both) "D2") 30 "a both-directions relationship"
            Expect.isGreaterThan (decided inner "D1") 60 "an inner CALCULATE"
            Expect.isGreaterThan (decided inner "F") 60 "an inner CALCULATE, on the fact"

            // And the engine answers nearly all of them rather than refusing.
            let refused =
                scenarios
                |> List.filter (fun s ->
                    let p = prepare (model s.World)
                    let outer = s.Outer |> List.map (fun f -> ", " + outerDax f) |> String.concat ""

                    match
                        Engine.evaluateScalar
                            p
                            $"CALCULATE ( CALCULATE ( COUNTROWS ( VALUES ( D2 ) ){innerDax s.Inner} ){outer} )"
                    with
                    | Error _ -> true
                    | Ok _ -> false)
                |> List.length

            Expect.isLessThan refused 150 "refused scenarios, of 1500"
    ]
