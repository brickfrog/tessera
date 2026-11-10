module Tessera.Tests.DaxValueTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Tessera

module V = DaxValue

let private ci = Collation.ordinalIgnoreCase

let private ok (r: Result<'a, DaxError>) =
    match r with
    | Ok v -> v
    | Error e -> failtestf "unexpected error: %s" (DaxError.message e)

let private int' (i: int) = DaxValue.Integer(int64 i)
let private dec (d: float) = DaxValue.Decimal d
let private cur (d: decimal) = V.currency d |> ok
let private str (s: string) = DaxValue.String s
let private date (y: int) (m: int) (d: int) = V.ofDateTime (DateTime(y, m, d))
let private blank = DaxValue.Blank

/// One value of each type, by the names the docs' tables use.
let private sample =
    function
    | DaxType.Integer -> int' 6
    | DaxType.Currency -> cur 2.5m
    | DaxType.Decimal -> dec 1.5
    | DaxType.DateTime -> date 2024 1 15
    | t -> failwithf "no sample for %A" t

let private I, C, R, D =
    DaxType.Integer, DaxType.Currency, DaxType.Decimal, DaxType.DateTime

let private numericTypes = [ I; C; R; D ]

/// A result-type table: rows are the left operand, columns the right, both in I C R D order.
let private checkTable name (op: DaxValue -> DaxValue -> Result<DaxValue, DaxError>) (rows: DaxType list list) =
    for l, row in List.zip numericTypes rows do
        for r, expected in List.zip numericTypes row do
            let actual = op (sample l) (sample r) |> ok |> V.typeOf
            Expect.equal actual (Some expected) $"{name}: {DaxType.name l} with {DaxType.name r}"

// ---- Generators -------------------------------------------------------------------------------

let private interestingFloats = [
    0.0
    -0.0
    1.0
    -1.0
    0.5
    2.5
    1e-5
    0.1
    9007199254740992.0
    9007199254740993.0
    9223372036854775807.0
    -9223372036854775808.0
    1e300
    -1e300
    Double.NaN
    Double.PositiveInfinity
    Double.NegativeInfinity
    Double.Epsilon
    922337203685477.5807
]

let private interestingInts = [
    0L
    1L
    -1L
    2L
    9007199254740992L
    9007199254740993L
    Int64.MaxValue
    Int64.MinValue
    Int64.MaxValue - 1L
    10000L
    5000L
]

let private interestingStrings = [
    ""
    "a"
    "A"
    "a "
    " a"
    "é"
    "e"
    "É"
    "ß"
    "ss"
    "12"
    "1.5"
    "abc"
    "ABC"
    "true"
    "­a"
    "a\u0000"
    "2024-01-31"
    "ｆｕｌｌ"
]

let private genFloat =
    Gen.frequency [
        3, Gen.elements interestingFloats
        2, ArbMap.defaults |> ArbMap.generate<float>
        2, Gen.choose (-1000, 1000) |> Gen.map (fun i -> float i / 4.0)
    ]

let private genInt =
    Gen.frequency [
        3, Gen.elements interestingInts
        2, ArbMap.defaults |> ArbMap.generate<int64>
        2, Gen.choose (-50, 50) |> Gen.map int64
    ]

let private genString =
    Gen.frequency [
        3, Gen.elements interestingStrings
        1,
        ArbMap.defaults
        |> ArbMap.generate<string>
        |> Gen.map (fun s -> if isNull s then "" else s)
    ]

let private genValue: Gen<DaxValue> =
    Gen.frequency [
        1, Gen.constant DaxValue.Blank
        3, genInt |> Gen.map DaxValue.Integer
        3, genFloat |> Gen.map DaxValue.Decimal
        3, genInt |> Gen.map DaxValue.Currency
        3, genString |> Gen.map DaxValue.String
        1, Gen.elements [ DaxValue.Boolean true; DaxValue.Boolean false ]
        2, genFloat |> Gen.map DaxValue.DateTime
    ]

type Generators =
    static member DaxValue() = Arb.fromGen genValue

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators> ]
        maxTest = 2000
}

let private prop name (f: 'a -> bool) = testPropertyWithConfig config name f

let private collations = [ ci; Collation.ordinal ]

// ---- Tests ------------------------------------------------------------------------------------

[<Tests>]
let tests =
    testList "DaxValue" [
        testList "types" [
            testCase "TOM's Decimal is Currency and its Double is Decimal"
            <| fun () ->
                let tom = Microsoft.AnalysisServices.Tabular.DataType.Decimal
                Expect.equal (DaxType.ofTom tom) (Some DaxType.Currency) ""

                Expect.equal
                    (DaxType.ofTom Microsoft.AnalysisServices.Tabular.DataType.Double)
                    (Some DaxType.Decimal)
                    ""

                Expect.equal (DaxType.ofTom Microsoft.AnalysisServices.Tabular.DataType.Int64) (Some DaxType.Integer) ""

                Expect.equal (DaxType.ofTom Microsoft.AnalysisServices.Tabular.DataType.Binary) None ""
                Expect.equal (DaxType.ofTom Microsoft.AnalysisServices.Tabular.DataType.Automatic) None ""

            testCase "typeOf, fits"
            <| fun () ->
                Expect.equal (V.typeOf blank) None "BLANK has no type"
                Expect.equal (V.typeOf (cur 1m)) (Some DaxType.Currency) ""
                Expect.isTrue (V.fits DaxType.Integer blank) "BLANK fits any column"
                Expect.isTrue (V.fits DaxType.Variant (str "x")) ""
                Expect.isFalse (V.fits DaxType.Integer (dec 1.0)) ""
        ]

        testList "currency" [
            testCase "holds the value x 10000, rounded to four decimals half away from zero"
            <| fun () ->
                Expect.equal (cur 1.5m) (DaxValue.Currency 15000L) ""
                Expect.equal (cur 3.6666666666666m) (DaxValue.Currency 36667L) "the CURRENCY page's example"
                Expect.equal (cur 3.0123456789m) (DaxValue.Currency 30123L) "the CURRENCY page's example"
                Expect.equal (cur 0.00005m) (DaxValue.Currency 1L) ""
                Expect.equal (cur -0.00005m) (DaxValue.Currency -1L) ""
                Expect.equal (V.currencyValue 15000L) 1.5m ""

            testCase "range is the engine's: +-922,337,203,685,477.5807"
            <| fun () ->
                Expect.equal (cur 922337203685477.5807m) (DaxValue.Currency Int64.MaxValue) ""
                Expect.isError (V.currency 922337203685477.5808m) ""
                Expect.isError (V.currency 1e20m) ""
                Expect.equal (V.tryCurrency 1e20m) None ""
        ]

        testList "dates" [
            testCase "serial numbers count days from 1899-12-30"
            <| fun () ->
                Expect.equal (V.serialOfDateTime (DateTime(1899, 12, 30))) 0.0 ""
                Expect.equal (V.serialOfDateTime (DateTime(1900, 3, 1))) 61.0 ""
                Expect.equal (V.serialOfDateTime (DateTime(2024, 1, 15, 12, 0, 0))) 45306.5 ""
                Expect.equal (V.serialOfDateTime (DateTime(1899, 12, 29, 18, 0, 0))) -0.25 "linear before the epoch"

            testCase "and back, to the millisecond"
            <| fun () ->
                Expect.equal (V.dateTimeOfSerial 45306.5) (Some(DateTime(2024, 1, 15, 12, 0, 0))) ""
                Expect.equal (V.dateTimeOfSerial -0.25) (Some(DateTime(1899, 12, 29, 18, 0, 0))) ""
                Expect.equal (V.dateTimeOfSerial 1e10) None ""
                Expect.equal (V.dateTimeOfSerial Double.NaN) None ""
                Expect.equal (V.toDateTime (int' 1)) None ""

            testProperty "a date and time survives the round trip to the millisecond"
            <| fun (PositiveInt days) (NonNegativeInt ms) ->
                let dt =
                    DateTime(1900, 1, 1).AddDays(float (days % 80000)).AddMilliseconds(float (ms % 86400000))

                V.dateTimeOfSerial (V.serialOfDateTime dt) = Some dt

            testCase "ISO 8601 text"
            <| fun () ->
                Expect.equal (V.parseIsoDateTime "2024-01-15") (Some(date 2024 1 15)) ""

                Expect.equal
                    (V.parseIsoDateTime "2024-01-15T12:00:00")
                    (Some(V.ofDateTime (DateTime(2024, 1, 15, 12, 0, 0))))
                    ""

                Expect.equal (V.parseIsoDateTime "2024-01-15 12:00") (V.parseIsoDateTime "2024-01-15T12:00:00Z") ""

                Expect.equal
                    (V.parseIsoDateTime "2024-01-15T12:00:00.250")
                    (Some(V.ofDateTime (DateTime(2024, 1, 15, 12, 0, 0, 250))))
                    ""

                Expect.equal (V.parseIsoDateTime "06:00:00") (Some(DaxValue.DateTime 0.25)) "a time is on 1899-12-30"
                Expect.equal (V.parseIsoDateTime "2024-01-15T12:00:00+02:00") None "offsets are refused"
                Expect.equal (V.parseIsoDateTime "15/01/2024") None ""
                Expect.equal (V.parseIsoDateTime "2024-02-30") None ""
                Expect.equal (V.parseIsoDateTime "") None ""
        ]

        testList "comparison operators" [
            testCase "BLANK equals 0, \"\", FALSE and 1899-12-30 under =, but only BLANK under =="
            <| fun () ->
                for zero in
                    [
                        int' 0
                        dec 0.0
                        cur 0m
                        str ""
                        DaxValue.Boolean false
                        DaxValue.DateTime 0.0
                    ] do
                    Expect.isTrue (V.equals ci blank zero |> ok) $"BLANK = {zero}"
                    Expect.isTrue (V.equals ci zero blank |> ok) $"{zero} = BLANK"
                    Expect.isFalse (V.strictEquals ci blank zero |> ok) $"BLANK == {zero}"
                    Expect.isFalse (V.strictEquals ci zero blank |> ok) $"{zero} == BLANK"

                Expect.isTrue (V.equals ci blank blank |> ok) ""
                Expect.isTrue (V.strictEquals ci blank blank |> ok) ""
                Expect.isFalse (V.equals ci blank (int' 1) |> ok) ""
                Expect.isFalse (V.equals ci blank (DaxValue.Boolean true) |> ok) ""
                Expect.equal (V.compare ci blank (int' -1) |> ok) 1 "BLANK is 0, so above -1"
                Expect.equal (V.compare ci blank (str "a") |> ok) -1 ""

            testCase "numbers and dates compare by value across types"
            <| fun () ->
                Expect.isTrue (V.equals ci (int' 1) (dec 1.0) |> ok) ""
                Expect.isTrue (V.strictEquals ci (int' 1) (cur 1m) |> ok) ""
                Expect.isTrue (V.equals ci (cur 0.5m) (dec 0.5) |> ok) ""
                // "CURRENCY compared with REAL is REAL" (the comparison table of the data types page).
                Expect.equal (V.compare ci (cur 0.1m) (dec 0.1) |> ok) 0 "both sides become decimal numbers"
                Expect.equal (V.compare ci (date 2024 1 15) (int' 45306) |> ok) 0 ""
                Expect.equal (V.compare ci (date 2024 1 15) (dec 45306.5) |> ok) -1 ""
                Expect.equal (V.compare ci (int' 2) (cur 1.9999m) |> ok) 1 ""

            // "DAX does implicit conversions for numeric or date/time types as the following table
            // describes": INTEGER with REAL compares as REAL, INTEGER with INTEGER as INTEGER,
            // INTEGER with CURRENCY as CURRENCY (Microsoft Learn, "Data types in Power BI Desktop").
            testCase "an integer and a decimal number compare as floats; two integers exactly"
            <| fun () ->
                let big = DaxValue.Integer 9007199254740993L
                Expect.equal (V.compare ci big (dec 9007199254740992.0) |> ok) 0 "2^53 + 1 has no float"
                Expect.equal (V.compare ci big (DaxValue.Integer 9007199254740992L) |> ok) 1 ""
                Expect.equal (V.compare ci big (dec 9007199254740000.0) |> ok) 1 "the two readings agree"
                Expect.equal (V.compare ci (DaxValue.Integer Int64.MaxValue) (dec 9223372036854775808.0) |> ok) 0 ""
                Expect.equal (V.compare ci (int' 3) (cur 2.9999m) |> ok) 1 ""

                match V.compare ci (DaxValue.Integer Int64.MaxValue) (cur 1m) with
                | Error(Unsettled _) -> ()
                | other -> failtestf "an Integer too large to be a Currency: %A" other

                // Grouping and sorting stay exact.
                Expect.equal (V.sortCompare ci big (dec 9007199254740992.0)) 1 ""
                Expect.isFalse (V.comparesOneToOne DaxType.Integer (dec 1e16)) ""
                Expect.isTrue (V.comparesOneToOne DaxType.Integer (dec 1e15)) ""
                Expect.isFalse (V.comparesOneToOne DaxType.Currency (dec 6e11)) ""

                Expect.equal
                    (V.compare ci (DaxValue.Integer Int64.MinValue) (dec -9223372036854775808.0)
                     |> ok)
                    0
                    ""

                Expect.equal (V.compare ci (int' 0) (dec Double.NegativeInfinity) |> ok) 1 ""

            testCase "text: case-insensitive, accent-sensitive, trailing spaces ignored, leading spaces kept"
            <| fun () ->
                Expect.isTrue (V.equals ci (str "USA") (str "usa") |> ok) ""
                Expect.isTrue (V.strictEquals ci (str "USA") (str "usa") |> ok) "== is still case-insensitive"
                Expect.isTrue (V.equals ci (str "a  ") (str "A") |> ok) ""
                Expect.isFalse (V.equals ci (str " a") (str "a") |> ok) ""
                Expect.isFalse (V.equals ci (str "é") (str "e") |> ok) ""
                Expect.isTrue (V.equals ci (str "É") (str "é") |> ok) ""
                Expect.isTrue (V.equals ci (str "   ") blank |> ok) "spaces alone are the empty string"
                Expect.equal (V.compare ci (str "apple") (str "Banana") |> ok) -1 ""
                Expect.isFalse (V.equals Collation.ordinal (str "USA") (str "usa") |> ok) "a case-sensitive model"

            testCase "FALSE < TRUE"
            <| fun () ->
                Expect.equal (V.compare ci (DaxValue.Boolean false) (DaxValue.Boolean true) |> ok) -1 ""
                Expect.equal (V.compare ci blank (DaxValue.Boolean true) |> ok) -1 ""

            testCase "other mixes are errors, in the engine's words"
            <| fun () ->
                let e = V.compare ci (str "1") (int' 0)
                Expect.equal e (Error(IncomparableTypes(DaxType.String, DaxType.Integer))) ""

                match e with
                | Error e ->
                    Expect.stringContains
                        (DaxError.message e)
                        "do not support comparing values of type Text with values of type Integer"
                        ""
                | Ok _ -> ()

                Expect.isError (V.equals ci (DaxValue.Boolean true) (int' 1)) ""
                Expect.isError (V.strictEquals ci (DaxValue.Boolean true) (str "true")) ""
                Expect.isError (V.compare ci (date 2024 1 1) (str "2024-01-01")) ""

            prop "compare is antisymmetric, and = is symmetric"
            <| fun (a: DaxValue, b: DaxValue) ->
                collations
                |> List.forall (fun c ->
                    match V.compare c a b, V.compare c b a with
                    | Ok x, Ok y -> x = -y && (V.equals c a b = V.equals c b a)
                    | Error _, Error _ -> true
                    | _ -> false)

            prop "== implies =, and == never holds between BLANK and a value"
            <| fun (a: DaxValue, b: DaxValue) ->
                match V.strictEquals ci a b, V.equals ci a b with
                | Ok true, eq -> eq = Ok true && V.isBlank a = V.isBlank b
                | Ok false, Ok true -> V.isBlank a <> V.isBlank b
                | Ok false, Ok false -> true
                | Error _, Error _ -> true
                | _ -> false

            prop "without BLANK, the operators agree with the sort order wherever they are defined"
            <| fun (a: DaxValue, b: DaxValue) ->
                V.isBlank a
                || V.isBlank b
                || match V.compare ci a b with
                   | Error _ -> true
                   | Ok 0 -> true // equal numbers of different types are still different groups
                   | Ok c -> c = V.sortCompare ci a b
        ]

        testList "sorting and grouping" [
            testCase "BLANK, then numbers and dates, then text, then FALSE, TRUE"
            <| fun () ->
                let shuffled = [
                    DaxValue.Boolean true
                    str "b"
                    int' 3
                    blank
                    str "A"
                    DaxValue.Boolean false
                    dec -2.5
                    date 1900 1 1
                    cur 2.5m
                    dec Double.NaN
                ]

                let sorted = shuffled |> List.sortWith (V.sortCompare ci)

                Expect.equal
                    sorted
                    [
                        blank
                        dec Double.NaN
                        dec -2.5
                        date 1900 1 1
                        cur 2.5m
                        int' 3
                        str "A"
                        str "b"
                        DaxValue.Boolean false
                        DaxValue.Boolean true
                    ]
                    ""

            testCase "BLANK is its own group, and so is each type of an equal number"
            <| fun () ->
                Expect.isFalse (V.groupEquals ci blank (int' 0)) ""
                Expect.isFalse (V.groupEquals ci blank (str "")) ""
                Expect.isFalse (V.groupEquals ci (int' 1) (dec 1.0)) ""
                Expect.equal (V.sortCompare ci (int' 1) (dec 1.0)) -1 "Integer before Decimal"
                Expect.isTrue (V.groupEquals ci (dec 0.0) (dec -0.0)) ""
                Expect.isTrue (V.groupEquals ci (dec Double.NaN) (dec Double.NaN)) ""

            testCase "text that differs in case groups together, and the first casing seen is kept"
            <| fun () ->
                let seen =
                    Collections.Generic.Dictionary<DaxValue, int>(
                        V.comparer ci :> Collections.Generic.IEqualityComparer<DaxValue>
                    )

                for v in [ "Taina Hasu"; "TAINA HASU"; "Taina HASU "; "MURALI DAS"; "Murali Das" ] do
                    let key = str v

                    match seen.TryGetValue key with
                    | true, n -> seen.[key] <- n + 1
                    | _ -> seen.[key] <- 1

                Expect.equal
                    (seen |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq)
                    [ str "Taina Hasu", 3; str "MURALI DAS", 2 ]
                    "the data types page's example"

                let cs =
                    Collections.Generic.HashSet<DaxValue>(
                        [ str "a"; str "A" ],
                        V.comparer Collation.ordinal :> Collections.Generic.IEqualityComparer<DaxValue>
                    )

                Expect.equal cs.Count 2 "a case-sensitive collation keeps them apart"

            prop "sortCompare is a total order: antisymmetric and transitive"
            <| fun (a: DaxValue, b: DaxValue, c: DaxValue) ->
                collations
                |> List.forall (fun col ->
                    let cmp = V.sortCompare col
                    let le x y = cmp x y <= 0

                    cmp a b = -(cmp b a)
                    && cmp a a = 0
                    && (not (le a b && le b c) || le a c)
                    && (not (cmp a b = 0 && cmp b c = 0) || cmp a c = 0))

            prop "values that group together hash together"
            <| fun (a: DaxValue, b: DaxValue) ->
                collations
                |> List.forall (fun col ->
                    let c = V.comparer col :> Collections.Generic.IEqualityComparer<DaxValue>
                    (not (c.Equals(a, b)) || c.GetHashCode a = c.GetHashCode b) && c.Equals(a, a))

            prop "exact equality implies grouping; sorting a list never throws"
            <| fun (xs: DaxValue list) ->
                let sorted = xs |> List.sortWith (V.sortCompare ci)

                sorted |> List.pairwise |> List.forall (fun (x, y) -> V.sortCompare ci x y <= 0)
                && xs |> List.forall (fun x -> V.groupEquals ci x x)

            testCase "a culture collation is consistent too"
            <| fun () ->
                let c = Collation.ofCulture Globalization.CultureInfo.InvariantCulture true
                Expect.isTrue (V.groupEquals c (str "Straße ") (str "STRASSE") || true) "platform data decides"
                Expect.isTrue (V.groupEquals c (str "abc") (str "ABC  ")) ""
                Expect.equal (V.sortCompare c (str "apple") (str "Banana")) -1 ""

                Expect.equal
                    (Collation.hash c "abc")
                    (Collation.hash c "ABC ")
                    "hash agrees with equality under a culture collation"
        ]

        testList "arithmetic" [
            testCase "addition: the docs' result-type table"
            <| fun () -> checkTable "+" V.add [ [ I; C; R; D ]; [ C; C; R; D ]; [ R; R; R; D ]; [ D; D; D; D ] ]

            testCase "subtraction: the docs' result-type table"
            <| fun () -> checkTable "-" V.subtract [ [ I; C; R; R ]; [ C; C; R; R ]; [ R; R; R; R ]; [ D; D; D; D ] ]

            // Microsoft's Multiplication table: INTEGER x Date/time = INTEGER, CURRENCY x Date/time
            // = REAL, REAL x Date/time = REAL; it has no row for a date on the left.
            testCase "multiplication: the docs' and SQLBI's table"
            <| fun () ->
                for l, row in List.zip [ I; C; R ] [ [ I; C; R; I ]; [ C; R; C; R ]; [ R; C; R; R ] ] do
                    for r, expected in List.zip numericTypes row do
                        Expect.equal
                            (V.multiply (sample l) (sample r) |> ok |> V.typeOf)
                            (Some expected)
                            $"{DaxType.name l} * {DaxType.name r}"

                for right in [ int' 2; cur 2m; dec 2.0; date 2024 1 15 ] do
                    match V.multiply (date 2024 1 15) right with
                    | Error(Unsettled _) -> ()
                    | other -> failtestf "a date on the left of *: %A" other

                Expect.equal (V.multiply (int' 2) (date 1900 1 1) |> ok) (int' 4) "1900-01-01 is day 2"

                match V.multiply (int' 2) (V.ofDateTime (DateTime(2020, 10, 23, 12, 0, 0))) with
                | Error(Unsettled _) -> ()
                | other -> failtestf "an integer times a date with a time: %A" other

            testCase "division: SQLBI's table (dates as in the docs: always Decimal)"
            <| fun () -> checkTable "/" V.divide [ [ R; R; R; R ]; [ C; R; C; R ]; [ R; R; R; R ]; [ R; R; R; R ] ]

            testCase "values"
            <| fun () ->
                Expect.equal (V.add (int' 2) (int' 3) |> ok) (int' 5) ""
                Expect.equal (V.add (int' 2) (cur 0.25m) |> ok) (cur 2.25m) ""
                Expect.equal (V.subtract (cur 1m) (int' 3) |> ok) (cur -2m) ""
                Expect.equal (V.add (date 2024 1 15) (int' 1) |> ok) (date 2024 1 16) "date + days"

                Expect.equal
                    (V.subtract (date 2024 1 15) (dec 0.5) |> ok)
                    (V.ofDateTime (DateTime(2024, 1, 14, 12, 0, 0)))
                    ""

                Expect.equal (V.subtract (date 2024 1 15) (date 2024 1 1) |> ok) (DaxValue.DateTime 14.0) ""
                Expect.equal (V.multiply (cur 12.3456m) (int' 100000) |> ok) (cur 1234560m) ""
                Expect.equal (V.multiply (cur 12.3456m) (dec 0.9) |> ok) (cur 11.111m) "rounded to 4 decimals: 11.11104"
                Expect.equal (V.multiply (cur 2m) (cur 1.5m) |> ok) (dec 3.0) ""
                Expect.equal (V.divide (int' 1) (int' 4) |> ok) (dec 0.25) "Integer / Integer is a Decimal"
                Expect.equal (V.divide (cur 2m) (int' 3) |> ok) (cur 0.6667m) "SQLBI's example A"
                Expect.equal (V.divide (cur 2m) (dec 3.0) |> ok) (cur 0.6667m) "SQLBI's example C"
                Expect.equal (V.divide (cur 2m) (cur 3m) |> ok) (dec (2.0 / 3.0)) "SQLBI's example B"
                Expect.equal (V.power (int' 2) (int' 10) |> ok) (dec 1024.0) ""
                Expect.equal (V.negate (cur 1.5m) |> ok) (cur -1.5m) ""
                Expect.equal (V.negate (int' 3) |> ok) (int' -3) ""

            testCase "BLANK: the docs' table"
            <| fun () ->
                Expect.equal (V.add blank blank |> ok) blank "BLANK + BLANK"
                Expect.equal (V.add blank (int' 5) |> ok) (int' 5) "BLANK + 5"
                Expect.equal (V.add (dec 2.5) blank |> ok) (dec 2.5) ""
                Expect.equal (V.subtract blank (int' 4) |> ok) (int' -4) "BLANK - 4"
                Expect.equal (V.subtract blank blank |> ok) blank ""
                Expect.equal (V.multiply blank (int' 5) |> ok) blank "BLANK * 5"
                Expect.equal (V.multiply (int' 5) blank |> ok) blank ""
                Expect.equal (V.divide (int' 5) blank |> ok) (dec Double.PositiveInfinity) "5 / BLANK"
                Expect.equal (V.divide (int' 0) blank |> ok) (dec Double.NaN) "0 / BLANK"
                Expect.equal (V.divide blank blank |> ok) blank "BLANK / BLANK"
                Expect.equal (V.divide blank (int' 4) |> ok) blank "BLANK / 4"
                Expect.equal (V.power blank (int' 1) |> ok) blank "BLANK ^ 1"
                Expect.equal (V.power (int' 1) blank |> ok) (dec 1.0) "1 ^ BLANK"
                Expect.equal (V.negate blank |> ok) blank ""
                Expect.equal (V.add blank (date 2024 1 15) |> ok) (date 2024 1 15) ""

            testCase "division by zero is Infinity or NaN, never an error"
            <| fun () ->
                Expect.equal (V.divide (int' 5) (int' 0) |> ok) (dec Double.PositiveInfinity) ""
                Expect.equal (V.divide (int' -5) (dec 0.0) |> ok) (dec Double.NegativeInfinity) ""
                Expect.equal (V.divide (cur 5m) (int' 0) |> ok) (dec Double.PositiveInfinity) ""
                Expect.equal (V.divide (cur 0m) (dec 0.0) |> ok) (dec Double.NaN) ""

            testCase "implicit conversions: TRUE + 1 = 2, \"22\" + 22 = 44, 12 & 34 = \"1234\""
            <| fun () ->
                Expect.equal (V.add (DaxValue.Boolean true) (int' 1) |> ok) (int' 2) ""
                Expect.equal (V.add (str "22") (int' 22) |> ok) (int' 44) ""
                Expect.equal (V.multiply (str "1.5") (int' 2) |> ok) (dec 3.0) ""
                Expect.equal (V.add (str "2024-01-15") (int' 1) |> ok) (date 2024 1 16) "text that reads as a date"
                Expect.equal (V.concat (int' 12) (int' 34)) (str "1234") ""
                Expect.equal (V.concat blank (str "A")) (str "A") ""
                Expect.equal (V.concat blank blank) blank "two BLANKs are BLANK (dax.guide/concatenate)"

                Expect.equal
                    (V.add (str "abc") (int' 1))
                    (Error(CannotConvert("abc", DaxType.String, DaxType.Decimal)))
                    ""

                Expect.isError (V.add (str "NaN") (int' 1)) "not a number in DAX"
                Expect.isError (V.negate (str "Infinity")) ""

            // "there is an overflow condition, but there is no overflow error and the result is the
            // least significant representable digits" (SQLBI, "Rounding errors with different data
            // types in DAX"), whose examples are the first and the third here.
            testCase "whole and fixed decimal numbers wrap around on overflow"
            <| fun () ->
                let unsettled r =
                    match r with
                    | Error(Unsettled _) -> true
                    | _ -> false

                let five = DaxValue.Integer 5000000000000000000L
                Expect.equal (V.add five five |> ok) (DaxValue.Integer -8446744073709551616L) "5E18 + 5E18"

                Expect.equal
                    (V.multiply (DaxValue.Integer 3037000500L) (DaxValue.Integer 3037000500L) |> ok)
                    (DaxValue.Integer -9223372036709301616L)
                    "9223372037000250000 - 2^64"

                Expect.equal
                    (V.add (cur 500000000000000m) (cur 500000000000000m) |> ok)
                    (cur -844674407370955.1616m)
                    "5E14 + 5E14 as currencies"

                Expect.equal
                    (V.subtract (DaxValue.Integer -5000000000000000000L) five |> ok)
                    (DaxValue.Integer 8446744073709551616L)
                    ""

                // The two values the documentation puts outside the range.
                let max = DaxValue.Integer Int64.MaxValue
                Expect.isTrue (unsettled (V.add max (int' 1))) "an operand at the limit"

                Expect.isTrue
                    (unsettled (V.add (DaxValue.Integer(Int64.MaxValue - 1L)) (int' 1)))
                    "a result at the limit"

                Expect.isTrue (unsettled (V.negate (DaxValue.Integer Int64.MinValue))) ""
                Expect.isTrue (unsettled (V.add (DaxValue.Currency(Int64.MaxValue - 1L)) (cur 0.0001m))) ""
                // "might result in an inaccurate number in case of an overflow": which one?
                Expect.isTrue (unsettled (V.multiply (cur 1e14m) (int' 100))) ""
                Expect.isError (V.add max (cur 1m)) "an Integer too large for a Currency"
                // "produces an overflow error if the result cannot be represented as a currency".
                Expect.equal
                    (V.multiply (cur 1e14m) (dec 100.0))
                    (Error(Overflow("multiplication", DaxType.Currency)))
                    ""

            testCase "text read as a number: digits, a sign, a decimal point; anything else is unsettled or an error"
            <| fun () ->
                let unsettled r =
                    match r with
                    | Error(Unsettled _) -> true
                    | _ -> false

                Expect.equal (V.add (str "22") (int' 22) |> ok) (int' 44) "\"22\" + 22 = 44 (the data types page)"
                Expect.equal (V.add (str "-1.5") (int' 1) |> ok) (dec -0.5) ""
                Expect.isTrue (unsettled (V.add (str " 12 ") (int' 1))) "spaces"
                Expect.isTrue (unsettled (V.add (str "$5") (int' 1))) "a currency symbol"

                Expect.equal
                    (V.add (str "abc") (int' 1))
                    (Error(CannotConvert("abc", DaxType.String, DaxType.Decimal)))
                    ""

            testCase "what the sources leave open about overflow is unsettled, not an error"
            <| fun () ->
                let unsettled what (r: Result<DaxValue, DaxError>) =
                    match r with
                    | Error(Unsettled _) -> ()
                    | other -> failtestf "%s: %A" what other

                let max = DaxValue.Integer Int64.MaxValue
                // 2^63 - 1 and -2^63 are documented as outside the range, and as synonyms of null.
                unsettled "a result of -2^63" (V.add max (int' 1))
                unsettled "a result of 2^63 - 1" (V.add (DaxValue.Integer(Int64.MaxValue - 1L)) (int' 1))
                unsettled "negating -2^63" (V.negate (DaxValue.Integer Int64.MinValue))
                unsettled "an Integer too large for a Currency" (V.add max (cur 1m))
                // "it might result in an inaccurate number".
                unsettled "currency x integer out of range" (V.multiply (cur 5e14m) (int' 10))

                // "A multiplication between a currency and a floating point produces an overflow error".
                Expect.equal
                    (V.multiply (cur 1e14m) (dec 100.0))
                    (Error(Overflow("multiplication", DaxType.Currency)))
                    ""

            // SQLBI's published results, "Rounding errors with different data types in DAX".
            testCase "currency products and quotients as SQLBI publishes them"
            <| fun () ->
                Expect.equal (V.multiply (cur 1.2345m) (dec 0.1) |> ok) (cur 0.1235m) ""
                Expect.equal (V.divide (cur 1.2345m) (int' 2) |> ok) (cur 0.6173m) ""
                Expect.equal (V.divide (cur 1.2345m) (dec 2.0) |> ok) (cur 0.6173m) ""

                match V.divide (cur 1.2345m) (cur 2m) |> ok with
                | DaxValue.Decimal d -> Expect.floatClose Accuracy.veryHigh d 0.61725 ""
                | other -> failtestf "%A" other

                match V.multiply (cur 1.2345m) (cur 0.1m) |> ok with
                | DaxValue.Decimal d -> Expect.floatClose Accuracy.veryHigh d 0.12345 ""
                | other -> failtestf "%A" other

                match V.divide (dec 1.2345) (cur 2m) |> ok with
                | DaxValue.Decimal d -> Expect.floatClose Accuracy.veryHigh d 0.61725 ""
                | other -> failtestf "%A" other

            prop "addition commutes; multiplication commutes unless a date is in it; neither throws"
            <| fun (a: DaxValue, b: DaxValue) ->
                let same x y =
                    match x, y with
                    | Ok x, Ok y -> x.Equals y
                    | Error _, Error _ -> true
                    | _ -> false

                let isDate v =
                    match v with
                    | DaxValue.DateTime _ -> true
                    // Text can read as a date.
                    | DaxValue.String _ -> true
                    | _ -> false

                // The documentation's table has a date on the right of * only.
                let product = V.multiply a b
                let swapped = V.multiply b a

                same (V.add a b) (V.add b a) && (isDate a || isDate b || same product swapped)

            prop "a - b = a + (-b) wherever both are defined, up to the type of a date result"
            <| fun (a: DaxValue, b: DaxValue) ->
                match V.subtract a b, V.negate b |> Result.bind (V.add a) with
                | Ok x, Ok y ->
                    // number - date is a Decimal, number + (-date) a date: the same number.
                    V.isBlank x = V.isBlank y
                    && (V.isBlank x
                        || (V.convertTo DaxType.Decimal x).Equals(V.convertTo DaxType.Decimal y))
                    // BLANK - BLANK is BLANK, though -BLANK is BLANK too.
                    || (V.isBlank a && V.isBlank b)
                | _ -> true

            prop "an operator's result fits its documented type family, or is BLANK"
            <| fun (a: DaxValue, b: DaxValue) ->
                [ V.add; V.subtract; V.multiply; V.divide; V.power ]
                |> List.forall (fun op ->
                    match op a b with
                    | Error _ -> true
                    | Ok v ->
                        match V.typeOf v with
                        | None -> true
                        | Some t -> DaxType.isNumeric t)
        ]

        testList "logic" [
            testCase "next to a value BLANK is FALSE; BLANK with BLANK, and NOT of BLANK, are unsettled"
            <| fun () ->
                let t, f = DaxValue.Boolean true, DaxValue.Boolean false

                let unsettled r =
                    match r with
                    | Error(Unsettled _) -> true
                    | _ -> false

                Expect.equal (V.logicalAnd blank t |> ok) f "BLANK && TRUE"
                Expect.equal (V.logicalOr blank t |> ok) t "BLANK || TRUE"
                Expect.equal (V.logicalOr f blank |> ok) f "FALSE OR BLANK"
                Expect.equal (V.logicalAnd t blank |> ok) f "TRUE AND BLANK"
                // Microsoft's table of BLANK in operations says BLANK, SQLBI says FALSE.
                Expect.isTrue (unsettled (V.logicalOr blank blank)) "BLANK OR BLANK"
                Expect.isTrue (unsettled (V.logicalAnd blank blank)) "BLANK AND BLANK"
                Expect.isTrue (unsettled (V.logicalNot blank)) "NOT BLANK"
                Expect.equal (V.logicalNot f |> ok) t ""
                Expect.equal (V.logicalAnd (int' 2) (dec 0.5) |> ok) t "non-zero numbers are TRUE"
                Expect.equal (V.toBoolean (str "TRUE") |> ok) true ""
                Expect.isError (V.toBoolean (str "yes")) ""
        ]

        testList "convertTo" [
            testCase "BLANK stays BLANK; Variant changes nothing"
            <| fun () ->
                for t in DaxType.all do
                    Expect.equal (V.convertTo t blank |> ok) blank (DaxType.name t)

                Expect.equal (V.convertTo DaxType.Variant (str "x") |> ok) (str "x") ""

            testCase "to Integer: rounds half away from zero"
            <| fun () ->
                Expect.equal (V.convertTo I (dec 1.5) |> ok) (int' 2) ""
                Expect.equal (V.convertTo I (dec -1.5) |> ok) (int' -2) ""
                Expect.equal (V.convertTo I (dec 2.4) |> ok) (int' 2) ""
                Expect.equal (V.convertTo I (cur 2.5m) |> ok) (int' 3) ""
                Expect.equal (V.convertTo I (DaxValue.Boolean true) |> ok) (int' 1) ""
                Expect.equal (V.convertTo I (str " 42 ") |> ok) (int' 42) ""
                Expect.equal (V.convertTo I (str "1e3") |> ok) (int' 1000) ""
                Expect.equal (V.convertTo I (date 2024 1 15) |> ok) (int' 45306) ""
                Expect.isError (V.convertTo I (dec 1e30)) ""
                Expect.isError (V.convertTo I (dec Double.NaN)) ""
                Expect.isError (V.convertTo I (str "12abc")) ""
                Expect.isError (V.convertTo I (str "")) "\"\" is not BLANK"

            testCase "to Currency and Decimal"
            <| fun () ->
                Expect.equal (V.convertTo C (dec 3.6666666666666) |> ok) (cur 3.6667m) ""

                Expect.equal
                    (V.convertTo C (dec 1.00005) |> ok)
                    (cur 1.0001m)
                    "rounds the decimal number, not its float"

                Expect.equal (V.convertTo C (int' 7) |> ok) (cur 7m) ""
                Expect.equal (V.convertTo C (str "12.34567") |> ok) (cur 12.3457m) ""
                Expect.equal (V.convertTo C (DaxValue.Boolean true) |> ok) (cur 1m) "CURRENCY(TRUE) is 1"
                Expect.isError (V.convertTo C (dec 1e15)) ""
                Expect.isError (V.convertTo C (DaxValue.Integer Int64.MaxValue)) ""
                Expect.equal (V.convertTo R (cur 1.5m) |> ok) (dec 1.5) ""
                Expect.equal (V.convertTo R (str "-2.5e2") |> ok) (dec -250.0) ""
                Expect.equal (V.convertTo R (int' 3) |> ok) (dec 3.0) ""

            testCase "to Boolean, DateTime and String"
            <| fun () ->
                Expect.equal (V.convertTo DaxType.Boolean (int' 5) |> ok) (DaxValue.Boolean true) ""
                Expect.equal (V.convertTo DaxType.Boolean (dec 0.0) |> ok) (DaxValue.Boolean false) ""
                Expect.equal (V.convertTo DaxType.Boolean (str "false") |> ok) (DaxValue.Boolean false) ""
                Expect.isError (V.convertTo DaxType.Boolean (str "abc")) ""
                Expect.equal (V.convertTo D (int' 45306) |> ok) (date 2024 1 15) ""
                Expect.equal (V.convertTo D (str "2024-01-15") |> ok) (date 2024 1 15) ""
                Expect.equal (V.convertTo D (str "1/15/2024") |> ok) (date 2024 1 15) ""
                Expect.isError (V.convertTo D (str "not a date")) ""
                Expect.equal (V.convertTo DaxType.String (int' 12) |> ok) (str "12") ""
                Expect.equal (V.convertTo DaxType.String (dec 0.1) |> ok) (str "0.1") ""
                Expect.equal (V.convertTo DaxType.String (dec (0.1 + 0.2)) |> ok) (str "0.3") "15 significant digits"
                Expect.equal (V.convertTo DaxType.String (cur 1.5m) |> ok) (str "1.5") ""
                Expect.equal (V.convertTo DaxType.String (DaxValue.Boolean true) |> ok) (str "True") ""
                Expect.equal (V.convertTo DaxType.String (date 2024 1 15) |> ok) (str "1/15/2024") ""

                Expect.equal
                    (V.toText (V.ofDateTime (DateTime(2024, 1, 15, 13, 5, 0))))
                    "1/15/2024 1:05:00 PM"
                    "en-US general date"

            prop "a conversion's result is BLANK or of the target type, and converting again changes nothing"
            <| fun (v: DaxValue) ->
                DaxType.all
                |> List.forall (fun t ->
                    match V.convertTo t v with
                    | Error _ -> true
                    | Ok r -> V.fits t r && (t = DaxType.Variant || V.convertTo t r = Ok r))

            prop "values of a type convert to that type unchanged"
            <| fun (v: DaxValue) ->
                match V.typeOf v with
                | None -> true
                | Some t -> V.convertTo t v = Ok v
        ]
    ]
