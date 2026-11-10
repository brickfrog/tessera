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

/// A random model F -> D1, F -> D2 (optionally filtering both ways), random filters, and the
/// counts the DEFINITION gives: a row of F is visible when its expanded row (F joined to D1 and D2,
/// BLANK where there is no match) satisfies every filter; a row of D1 when it satisfies every
/// filter's columns that D1 has, a table filter on F counting as a filter on the D1 rows its rows
/// refer to; with a both-directions relationship, a row of D2 also only when a visible row of F
/// refers to it.
type private Scenario = {
    D1: (int * string * int) list
    D2: (int * string) list
    F: (int * int option * int * int) list
    Both: bool
    Filters: Filter list
}

and private Filter =
    | OnA of string option list
    | OnAC of (string * string) list
    | OnX of int list
    | Above of int
    | OnAB of (string * int) list

let private scenario (seed: int) : Scenario =
    let r = Random(seed)
    let pick (items: 'a array) = items.[r.Next items.Length]

    let some (n: int) (make: unit -> 'a) =
        [ for _ in 1 .. 1 + r.Next n -> make () ] |> List.distinct

    let letters = [| "a"; "b"; "c" |]
    let marks = [| "x"; "y" |]

    {
        D1 = [ for k in 1 .. 1 + r.Next 4 -> k, pick letters, 1 + r.Next 2 ]
        D2 = [ for k in 1 .. 1 + r.Next 3 -> k, pick marks ]
        F = [
            for id in 1 .. r.Next 9 -> id, (if r.Next 6 = 0 then None else Some(1 + r.Next 5)), 1 + r.Next 3, r.Next 4
        ]
        Both = r.Next 2 = 0
        Filters = [
            for _ in 1 .. r.Next 4 ->
                match r.Next 5 with
                | 0 -> OnA(some 2 (fun () -> if r.Next 4 = 0 then None else Some(pick letters)))
                | 1 -> OnAC(some 3 (fun () -> pick letters, pick marks))
                | 2 -> OnX(some 2 (fun () -> r.Next 4))
                | 3 -> Above(r.Next 3)
                | _ -> OnAB(some 3 (fun () -> pick letters, 1 + r.Next 2))
        ]
    }

let private text (s: string) = "\"" + s + "\""

let private dax (f: Filter) : string =
    let rows (items: string list) = "{ " + String.concat ", " items + " }"

    match f with
    | OnA values ->
        let items =
            values
            |> List.map (fun v ->
                match v with
                | Some s -> text s
                | None -> "BLANK ()")

        $"TREATAS ( {rows items}, D1[A] )"
    | OnAC pairs ->
        let items = pairs |> List.map (fun (a, c) -> $"( {text a}, {text c} )")
        $"TREATAS ( {rows items}, D1[A], D2[C] )"
    | OnX values -> $"TREATAS ( {rows (values |> List.map string)}, F[X] )"
    | Above t -> $"FILTER ( F, F[X] > {t} )"
    | OnAB pairs ->
        let items = pairs |> List.map (fun (a, b) -> $"( {text a}, {b} )")
        $"TREATAS ( {rows items}, D1[A], D1[B] )"

let private model (s: Scenario) : TestModel = {
    Tables = [
        table "D1" [ "K", DataType.Int64; "A", DataType.String; "B", DataType.Int64 ] [
            for (k, a, b) in s.D1 -> [ k; a; b ]
        ]
        table "D2" [ "K", DataType.Int64; "C", DataType.String ] [ for (k, c) in s.D2 -> [ k; c ] ]
        table "F" [
            "Id", DataType.Int64
            "K1", DataType.Int64
            "K2", DataType.Int64
            "X", DataType.Int64
        ] [
            for (id, k1, k2, x) in s.F ->
                [
                    box id
                    (match k1 with
                     | Some k -> box k
                     | None -> null)
                    box k2
                    box x
                ]
        ]
    ]
    Rels = [
        rel ("F", "K1") ("D1", "K")
        (if s.Both then
             bothWays (rel ("F", "K2") ("D2", "K"))
         else
             rel ("F", "K2") ("D2", "K"))
    ]
    Measures = []
    Functions = []
}

/// The counts by definition: COUNTROWS ( F ), COUNTROWS ( VALUES ( D1 ) ), COUNTROWS ( VALUES ( D2 ) ).
let private byDefinition (s: Scenario) : int * int * int =
    // The expanded F: each row with its D1 row's A and B and its D2 row's C (None: no match).
    let expanded = [
        for (id, k1, k2, x) in s.F ->
            let d1 =
                k1 |> Option.bind (fun k -> s.D1 |> List.tryFind (fun (key, _, _) -> key = k))

            let d2 = s.D2 |> List.tryFind (fun (key, _) -> key = k2)
            id, d1, d2, x
    ]

    // TREATAS "filters out any values that are not present in its respective output column"
    // (dax.guide/treatas): a row with a value its column doesn't have is not part of the filter.
    let hasA (v: string) =
        s.D1 |> List.exists (fun (_, a, _) -> a = v)

    let hasB (v: int) =
        s.D1 |> List.exists (fun (_, _, b) -> b = v)

    let hasC (v: string) =
        s.D2 |> List.exists (fun (_, c) -> c = v)

    let d1BlankRow = expanded |> List.exists (fun (_, d1, _, _) -> d1.IsNone)

    let filters =
        s.Filters
        |> List.map (fun f ->
            match f with
            | OnA values ->
                OnA(
                    values
                    |> List.filter (fun v ->
                        match v with
                        | Some a -> hasA a
                        | None -> d1BlankRow)
                )
            | OnAC pairs -> OnAC(pairs |> List.filter (fun (a, c) -> hasA a && hasC c))
            | OnAB pairs -> OnAB(pairs |> List.filter (fun (a, b) -> hasA a && hasB b))
            | f -> f)

    let s = { s with Filters = filters }
    let a (d1: (int * string * int) option) = d1 |> Option.map (fun (_, a, _) -> a)
    let b (d1: (int * string * int) option) = d1 |> Option.map (fun (_, _, b) -> b)
    let c (d2: (int * string) option) = d2 |> Option.map snd

    let visibleF =
        expanded
        |> List.filter (fun (_, d1, d2, x) ->
            s.Filters
            |> List.forall (fun f ->
                match f with
                | OnA values -> values |> List.contains (a d1)
                | OnAC pairs -> pairs |> List.exists (fun (pa, pc) -> Some pa = a d1 && Some pc = c d2)
                | OnX values -> values |> List.contains x
                | Above t -> x > t
                | OnAB pairs -> pairs |> List.exists (fun (pa, pb) -> Some pa = a d1 && Some pb = b d1)))

    // D1's rows, the blank row (None) among them when some row of F has no match.
    let d1Rows =
        (s.D1 |> List.map Some)
        @ (if expanded |> List.exists (fun (_, d1, _, _) -> d1.IsNone) then
               [ None ]
           else
               [])

    let visibleD1 =
        d1Rows
        |> List.filter (fun row ->
            s.Filters
            |> List.forall (fun f ->
                match f with
                | OnA values -> values |> List.contains (a row)
                | OnAC pairs -> pairs |> List.exists (fun (pa, _) -> Some pa = a row)
                | OnX _ -> true
                // A table filter on F is a filter on its expanded table: the D1 rows its rows have.
                | Above t -> expanded |> List.exists (fun (_, d1, _, x) -> x > t && d1 = row)
                | OnAB pairs -> pairs |> List.exists (fun (pa, pb) -> Some pa = a row && Some pb = b row)))

    let d2Rows =
        (s.D2 |> List.map Some)
        @ (if expanded |> List.exists (fun (_, _, d2, _) -> d2.IsNone) then
               [ None ]
           else
               [])

    let visibleD2 =
        d2Rows
        |> List.filter (fun row ->
            let own =
                s.Filters
                |> List.forall (fun f ->
                    match f with
                    | OnAC pairs -> pairs |> List.exists (fun (_, pc) -> Some pc = c row)
                    | Above t -> expanded |> List.exists (fun (_, _, d2, x) -> x > t && d2 = row)
                    | _ -> true)

            // Both directions: only the D2 rows a visible F row refers to, once anything filters F.
            let referred =
                not s.Both
                || s.Filters.IsEmpty
                || visibleF |> List.exists (fun (_, _, d2, _) -> d2 = row)

            own && referred)

    visibleF.Length, visibleD1.Length, visibleD2.Length

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

            same
                seed
                $"CALCULATE ( COUNTROWS ( DISTINCT ( Product[Color] ) ){f} )"
                $"CALCULATE ( DISTINCTCOUNT ( Product[Color] ){f} )"

            same
                seed
                $"CALCULATE ( COUNTROWS ( VALUES ( Product[Color] ) ) - DISTINCTCOUNT ( Product[Color] ) + 0{f} )"
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
            { config with maxTest = 400 }
            "the rows a filter context shows are the rows its definition gives"
        <| fun (seed: int) ->
            let s = scenario seed
            let p = prepare (model s)
            let args = s.Filters |> List.map (fun f -> ", " + dax f) |> String.concat ""
            let expectedF, expectedD1, expectedD2 = byDefinition s

            let check (name: string) (expression: string) (expected: int) =
                let query = $"CALCULATE ( {expression}{args} )"

                match Engine.evaluateScalar p query with
                | Ok v -> Expect.equal v (count expected) $"seed {seed}: {name}\n{query}\n%A{s}"
                // The one shape the engine refuses here: a both-directions filter that keeps all rows.
                | Error(DaxNotSupported(what, _)) when s.Both && what.Contains "keeps all of its rows" -> ()
                | Error e -> failtestf "seed %d: %s: %s\n%s" seed name (TessError.message e) query

            check "F" "COUNTROWS ( F )" expectedF
            check "D1" "COUNTROWS ( VALUES ( D1 ) )" expectedD1
            check "D2" "COUNTROWS ( VALUES ( D2 ) )" expectedD2
    ]
