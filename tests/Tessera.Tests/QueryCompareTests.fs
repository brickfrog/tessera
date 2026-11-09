module Tessera.Tests.QueryCompareTests

open System
open System.Text.Json.Nodes
open Expecto
open Tessera

let private col (name: string) (t: QueryType) : QueryColumn = { Name = name; Type = t }

let private result (columns: QueryColumn list) (rows: QueryValue list list) : QueryResult = {
    Columns = columns
    Rows = rows |> List.map Array.ofList
    Truncated = false
}

let private i (n: int) = QueryValue.Int64(int64 n)
let private d (x: float) = QueryValue.Double x
let private m (x: decimal) = QueryValue.Decimal x
let private s (x: string) = QueryValue.String x

let private diff (options: CompareOptions) (expected: QueryResult) (actual: QueryResult) =
    (QueryResult.compare options expected actual).Differences

let private unordered = CompareOptions.defaults

let private ordered = {
    CompareOptions.defaults with
        Ordered = true
}

let private parse (json: string) =
    QueryResult.ofJson (JsonNode.Parse json)

let private every: QueryResult = {
    Columns = [
        col "T[s]" QueryType.String
        col "[i]" QueryType.Int64
        col "[d]" QueryType.Double
        col "[m]" QueryType.Decimal
        col "[t]" QueryType.DateTime
        col "[b]" QueryType.Boolean
        col "[x]" QueryType.Binary
    ]
    Rows = [
        [|
            s "a \"b\""
            QueryValue.Int64 9007199254740993L
            d 0.1
            m 922337203685477.5807M
            QueryValue.DateTime(DateTime(2024, 1, 31, 13, 5, 0).AddMilliseconds 250.0)
            QueryValue.Boolean true
            QueryValue.Binary [| 1uy; 2uy |]
        |]
        [|
            s ""
            QueryValue.Int64 Int64.MinValue
            d 1e21
            m -0.0001M
            QueryValue.DateTime(DateTime(1899, 12, 30))
            QueryValue.Boolean false
            QueryValue.Binary [||]
        |]
        Array.create 7 QueryValue.Blank
        [|
            s "x"
            i 0
            d Double.NaN
            m 12M
            QueryValue.DateTime(DateTime(2024, 2, 29))
            QueryValue.Boolean true
            QueryValue.Blank
        |]
        [|
            s "y"
            i 1
            d Double.NegativeInfinity
            m 0M
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
        |]
    ]
    Truncated = true
}

[<Tests>]
let jsonTests =
    testList "QueryResult.ofJson" [
        testCase "reads back what toJson writes, value for value"
        <| fun () ->
            // Through text, as a file would be.
            let text = (QueryResult.toJson every).ToJsonString()

            match parse text with
            | Error e -> failtest e
            | Ok back ->
                Expect.equal back.Columns every.Columns "columns"
                Expect.equal back.Truncated true "truncated"

                let exact = {
                    CompareOptions.exact with
                        Ordered = true
                }

                Expect.isEmpty (diff exact { every with Truncated = false } { back with Truncated = false }) "values"
                // compare treats NaN as equal to NaN and has a tolerance; check the raw values too.
                Expect.equal (back.Rows.[0].[1]) (QueryValue.Int64 9007199254740993L) "an int64 above 2^53 is exact"
                Expect.equal (back.Rows.[0].[3]) (m 922337203685477.5807M) "a decimal keeps every digit"

        testCase "rowCount and truncated are optional; a wrong rowCount is an error"
        <| fun () ->
            Expect.isOk (parse """{ "columns": [ { "name": "[a]", "type": "int64" } ], "rows": [ [1], [null] ] }""") ""

            Expect.isOk
                (parse """{ "columns": [ { "name": "[a]", "type": "int64" } ], "rows": [ [1] ], "rowCount": 1 }""")
                ""

            Expect.isError
                (parse """{ "columns": [ { "name": "[a]", "type": "int64" } ], "rows": [ [1] ], "rowCount": 2 }""")
                ""

        testCase "a value of the wrong kind for its column is an error that says where"
        <| fun () ->
            let bad (rows: string) (expectedPart: string) =
                let json =
                    """{ "columns": [ { "name": "[a]", "type": "int64" }, { "name": "[b]", "type": "dateTime" }, { "name": "[c]", "type": "decimal" } ], "rows": """
                    + rows
                    + " }"

                match parse json with
                | Ok _ -> failtestf "%s should not parse" rows
                | Error e -> Expect.stringContains e expectedPart rows

            bad """[ [1.0, null, null] ]""" "row 0, [a]"
            bad """[ [1, null, null], ["1", null, null] ]""" "row 1, [a]"
            bad """[ [1, "31/01/2024", null] ]""" "[b]"
            bad """[ [1, null, "1.5"] ]""" "[c]"
            bad """[ [1, null] ]""" "2 values for 3 columns"
            bad """[ 1 ]""" "row 0 must be an array"

        testCase "an unknown column type is an error that lists the types"
        <| fun () ->
            match parse """{ "columns": [ { "name": "[a]", "type": "integer" } ], "rows": [] }""" with
            | Ok _ -> failtest "should not parse"
            | Error e -> Expect.stringContains e "int64" e

        testCase "in a variant column the spelling decides; an object names the type"
        <| fun () ->
            let json =
                """{ "columns": [ { "name": "[v]", "type": "variant" } ],
                     "rows": [ [1], [1.0], ["x"], [true], [null],
                               [ { "type": "decimal", "value": 1.5 } ],
                               [ { "type": "dateTime", "value": "2024-01-31T00:00:00" } ],
                               [ { "type": "double", "value": "NaN" } ] ] }"""

            match parse json with
            | Error e -> failtest e
            | Ok r ->
                let values = r.Rows |> List.map (fun row -> row.[0])
                Expect.equal values.[0] (i 1) ""
                Expect.equal values.[1] (d 1.0) ""
                Expect.equal values.[2] (s "x") ""
                Expect.equal values.[3] (QueryValue.Boolean true) ""
                Expect.equal values.[4] QueryValue.Blank ""
                Expect.equal values.[5] (m 1.5M) ""
                Expect.equal values.[6] (QueryValue.DateTime(DateTime(2024, 1, 31))) ""

                match values.[7] with
                | QueryValue.Double x -> Expect.isTrue (Double.IsNaN x) ""
                | other -> failtestf "%A" other

        testCase "a typed object is accepted in a typed column, and must be of a real type"
        <| fun () ->
            Expect.isOk
                (parse
                    """{ "columns": [ { "name": "[a]", "type": "double" } ], "rows": [ [ { "type": "int64", "value": 1 } ] ] }""")
                ""

            Expect.isError
                (parse
                    """{ "columns": [ { "name": "[a]", "type": "variant" } ], "rows": [ [ { "type": "variant", "value": 1 } ] ] }""")
                ""

            Expect.isError
                (parse """{ "columns": [ { "name": "[a]", "type": "variant" } ], "rows": [ [ { "value": 1 } ] ] }""")
                ""

        testCase "listOfJson reads the results array and says which result is wrong"
        <| fun () ->
            let good = """{ "columns": [], "rows": [] }"""

            Expect.equal
                (QueryResult.listOfJson (JsonNode.Parse $"[{good},{good}]")
                 |> Result.map List.length)
                (Ok 2)
                ""

            match QueryResult.listOfJson (JsonNode.Parse $"[{good},{{}}]") with
            | Error e -> Expect.stringContains e "result 1" e
            | Ok _ -> failtest "should not parse"

            Expect.isError (QueryResult.listOfJson (JsonNode.Parse good)) "not an array"
    ]

let private two =
    result [ col "T[k]" QueryType.String; col "[v]" QueryType.Int64 ] [ [ s "a"; i 1 ]; [ s "b"; i 2 ]; [ s "c"; i 3 ] ]

[<Tests>]
let compareTests =
    testList "QueryResult.compare" [
        testCase "a result equals itself, ordered or not"
        <| fun () ->
            let every = { every with Truncated = false }
            Expect.isEmpty (diff unordered every every) ""
            Expect.isEmpty (diff ordered every every) ""
            Expect.isTrue (QueryResult.compare unordered every every).IsMatch ""

        testCase "unordered: the same rows in another order match; ordered: they don't"
        <| fun () ->
            let shuffled = { two with Rows = List.rev two.Rows }
            Expect.isEmpty (diff unordered two shuffled) ""
            Expect.isNonEmpty (diff ordered two shuffled) ""

        testCase "rows are a multiset: a duplicate counts"
        <| fun () ->
            let once = result [ col "[v]" QueryType.Int64 ] [ [ i 1 ]; [ i 2 ] ]
            let twice = result [ col "[v]" QueryType.Int64 ] [ [ i 1 ]; [ i 1 ]; [ i 2 ] ]

            Expect.equal
                (diff unordered twice once)
                [ ResultDifference.RowCount(3, 2); ResultDifference.MissingRow(1, [| i 1 |]) ]
                ""

            Expect.equal
                (diff unordered once twice)
                [
                    ResultDifference.RowCount(2, 3)
                    ResultDifference.UnexpectedRow(1, [| i 1 |])
                ]
                ""

        testCase "columns are matched by name, in any order, ignoring case"
        <| fun () ->
            let swapped =
                result [ col "[V]" QueryType.Int64; col "t[K]" QueryType.String ] [
                    [ i 2; s "b" ]
                    [ i 1; s "a" ]
                    [ i 3; s "c" ]
                ]

            Expect.isEmpty (diff unordered two swapped) ""

        testCase "an exact name wins over one that differs in case"
        <| fun () ->
            let expected =
                result [ col "[a]" QueryType.Int64; col "[A]" QueryType.Int64 ] [ [ i 1; i 2 ] ]

            let actual =
                result [ col "[A]" QueryType.Int64; col "[a]" QueryType.Int64 ] [ [ i 2; i 1 ] ]

            Expect.isEmpty (diff unordered expected actual) ""

        testCase "missing and unexpected columns are reported, and the rest still compared"
        <| fun () ->
            let actual =
                result [ col "T[k]" QueryType.String; col "[w]" QueryType.Int64 ] [
                    [ s "a"; i 1 ]
                    [ s "b"; i 2 ]
                    [ s "z"; i 3 ]
                ]

            Expect.equal
                (diff ordered two actual)
                [
                    ResultDifference.MissingColumn "[v]"
                    ResultDifference.UnexpectedColumn "[w]"
                    ResultDifference.Value(2, 2, "T[k]", s "c", s "z")
                ]
                ""

        testCase "a different value is a value difference, paired with the closest row"
        <| fun () ->
            let actual = {
                two with
                    Rows = [ [| s "c"; i 3 |]; [| s "b"; i 20 |]; [| s "a"; i 1 |] ]
            }

            Expect.equal (diff unordered two actual) [ ResultDifference.Value(1, 1, "[v]", i 2, i 20) ] ""
            Expect.isFalse (QueryResult.compare unordered two actual).ValuesMatch ""

        testCase "rows that share nothing are missing and unexpected, not changed in every value"
        <| fun () ->
            let actual = {
                two with
                    Rows = [ [| s "a"; i 1 |]; [| s "b"; i 2 |]; [| s "x"; i 9 |] ]
            }

            Expect.equal
                (diff unordered two actual)
                [
                    ResultDifference.MissingRow(2, [| s "c"; i 3 |])
                    ResultDifference.UnexpectedRow(2, [| s "x"; i 9 |])
                ]
                ""

        testCase "one column, same count: the leftovers are paired"
        <| fun () ->
            let expected = result [ col "[v]" QueryType.Int64 ] [ [ i 1 ]; [ i 5 ] ]
            let actual = result [ col "[v]" QueryType.Int64 ] [ [ i 6 ]; [ i 1 ] ]
            Expect.equal (diff unordered expected actual) [ ResultDifference.Value(1, 0, "[v]", i 5, i 6) ] ""

        testCase "the same number in another type is a type difference, apart from value differences"
        <| fun () ->
            let expected =
                result [
                    col "[a]" QueryType.Int64
                    col "[b]" QueryType.Decimal
                    col "[c]" QueryType.Double
                ] [ [ i 1; m 2.5M; d 3.0 ] ]

            let actual =
                result [
                    col "[a]" QueryType.Double
                    col "[b]" QueryType.Double
                    col "[c]" QueryType.Int64
                ] [ [ d 1.0; d 2.5; i 3 ] ]

            let c = QueryResult.compare unordered expected actual

            Expect.equal
                c.Differences
                [
                    ResultDifference.ColumnType("[a]", QueryType.Int64, QueryType.Double)
                    ResultDifference.ColumnType("[b]", QueryType.Decimal, QueryType.Double)
                    ResultDifference.ColumnType("[c]", QueryType.Double, QueryType.Int64)
                    ResultDifference.ValueType(0, 0, "[a]", i 1, d 1.0)
                    ResultDifference.ValueType(0, 0, "[b]", m 2.5M, d 2.5)
                    ResultDifference.ValueType(0, 0, "[c]", d 3.0, i 3)
                ]
                ""

            Expect.isFalse c.IsMatch ""
            Expect.isTrue c.ValuesMatch "only types differ"

            let wrong =
                QueryResult.compare unordered expected {
                    actual with
                        Rows = [ [| d 1.5; d 2.5; i 3 |] ]
                }

            Expect.isFalse wrong.ValuesMatch "1 is not 1.5"

        testCase "IgnoreTypesOf leaves a column's types unchecked but not its values"
        <| fun () ->
            let expected =
                result [ col "[a]" QueryType.Variant ] [ [ i 1 ]; [ QueryValue.Blank ] ]

            let actual (v: QueryValue) =
                result [ col "[a]" QueryType.Decimal ] [ [ v ]; [ QueryValue.Blank ] ]

            let loose = {
                unordered with
                    IgnoreTypesOf = set [ "[a]" ]
            }

            Expect.isEmpty (diff loose expected (actual (m 1M))) ""
            Expect.isNonEmpty (diff unordered expected (actual (m 1M))) ""
            Expect.equal (diff loose expected (actual (m 2M))) [ ResultDifference.Value(0, 0, "[a]", i 1, m 2M) ] ""

        testCase "int64 and decimal compare exactly; doubles within the tolerance"
        <| fun () ->
            let one (t: QueryType) (v: QueryValue) = result [ col "[a]" t ] [ [ v ] ]

            let same (t: QueryType) (a: QueryValue) (b: QueryValue) =
                (QueryResult.compare unordered (one t a) (one t b)).IsMatch

            Expect.isTrue (same QueryType.Double (d 0.3) (d (0.1 + 0.2))) "0.1 + 0.2"
            Expect.isFalse (same QueryType.Double (d 0.3) (d 0.3000001)) ""
            Expect.isTrue (same QueryType.Double (d 0.0) (d 1e-15)) "absolute tolerance near zero"
            Expect.isTrue (same QueryType.Double (d Double.NaN) (d Double.NaN)) "NaN is NaN"
            Expect.isTrue (same QueryType.Double (d Double.PositiveInfinity) (d Double.PositiveInfinity)) ""
            Expect.isFalse (same QueryType.Double (d Double.PositiveInfinity) (d Double.NegativeInfinity)) ""
            Expect.isFalse (same QueryType.Double (d Double.PositiveInfinity) (d Double.MaxValue)) ""
            Expect.isFalse (same QueryType.Decimal (m 1.0000M) (m 1.0001M)) "a decimal is exact"
            Expect.isTrue (same QueryType.Decimal (m 1.0M) (m 1.0000M)) "scale doesn't matter"

            Expect.isFalse
                (same QueryType.Int64 (QueryValue.Int64 9007199254740993L) (QueryValue.Int64 9007199254740992L))
                "an int64 is exact above 2^53"

            let exact (a: float) (b: float) =
                (QueryResult.compare CompareOptions.exact (one QueryType.Double (d a)) (one QueryType.Double (d b)))
                    .IsMatch

            Expect.isFalse (exact 0.3 (0.1 + 0.2)) "no tolerance when asked"
            Expect.isTrue (exact 0.5 0.5) ""

        testCase "the tolerance is configurable"
        <| fun () ->
            let one (v: float) =
                result [ col "[a]" QueryType.Double ] [ [ d v ] ]

            let loose = {
                unordered with
                    RelativeTolerance = 0.01
            }

            Expect.isEmpty (diff loose (one 100.0) (one 100.5)) ""
            Expect.isNonEmpty (diff loose (one 100.0) (one 102.0)) ""

        testCase "unordered rows that are equal only within the tolerance still pair up"
        <| fun () ->
            let expected =
                result [ col "[k]" QueryType.String; col "[a]" QueryType.Double ] [ [ s "x"; d 0.3 ]; [ s "y"; d 0.7 ] ]

            let actual =
                result [ col "[k]" QueryType.String; col "[a]" QueryType.Double ] [
                    [ s "y"; d (0.1 + 0.6) ]
                    [ s "x"; d (0.1 + 0.2) ]
                ]

            Expect.isEmpty (diff unordered expected actual) ""

        testCase "BLANK equals only BLANK: not 0, not the empty string, not FALSE"
        <| fun () ->
            let one (t: QueryType) (v: QueryValue) = result [ col "[a]" t ] [ [ v ] ]

            for t, v in
                [
                    QueryType.Int64, i 0
                    QueryType.String, s ""
                    QueryType.Boolean, QueryValue.Boolean false
                    QueryType.Double, d 0.0
                ] do
                Expect.equal
                    (diff unordered (one t QueryValue.Blank) (one t v))
                    [ ResultDifference.Value(0, 0, "[a]", QueryValue.Blank, v) ]
                    (string t)

        testCase "text compares by code unit: case and trailing spaces count"
        <| fun () ->
            let one (v: string) =
                result [ col "[a]" QueryType.String ] [ [ s v ] ]

            Expect.isNonEmpty (diff unordered (one "a") (one "A")) ""
            Expect.isNonEmpty (diff unordered (one "a") (one "a ")) ""
            Expect.isNonEmpty (diff unordered (one "1") (result [ col "[a]" QueryType.String ] [ [ i 1 ] ])) ""

        testCase "ordered: a row count difference reports the rows beyond the shorter side"
        <| fun () ->
            let shorter = {
                two with
                    Rows = List.truncate 2 two.Rows
            }

            Expect.equal
                (diff ordered two shorter)
                [
                    ResultDifference.RowCount(3, 2)
                    ResultDifference.MissingRow(2, [| s "c"; i 3 |])
                ]
                ""

            Expect.equal
                (diff ordered shorter two)
                [
                    ResultDifference.RowCount(2, 3)
                    ResultDifference.UnexpectedRow(2, [| s "c"; i 3 |])
                ]
                ""

        testCase "a truncated actual result is a difference"
        <| fun () -> Expect.equal (diff unordered two { two with Truncated = true }) [ ResultDifference.Truncated ] ""

        testCase "empty results match; an empty one against rows doesn't"
        <| fun () ->
            let empty = { two with Rows = [] }
            Expect.isEmpty (diff unordered empty empty) ""
            Expect.equal (List.head (diff unordered empty two)) (ResultDifference.RowCount(0, 3)) ""

        testCase "many rows compare quickly when they agree"
        <| fun () ->
            let rows = [ for n in 1..50000 -> [| i n; d (float n / 7.0) |] ]

            let big: QueryResult = {
                Columns = [ col "[n]" QueryType.Int64; col "[x]" QueryType.Double ]
                Rows = rows
                Truncated = false
            }

            Expect.isEmpty (diff unordered big { big with Rows = List.rev rows }) ""

        testCase "compareAll needs the same number of result sets"
        <| fun () ->
            Expect.isOk (QueryResult.compareAll unordered [ two; two ] [ two; two ]) ""
            Expect.isError (QueryResult.compareAll unordered [ two ] [ two; two ]) ""

        testCase "every difference has a one-line description"
        <| fun () ->
            let all = [
                ResultDifference.MissingColumn "[a]"
                ResultDifference.UnexpectedColumn "[b]"
                ResultDifference.ColumnType("[a]", QueryType.Int64, QueryType.Double)
                ResultDifference.RowCount(1, 2)
                ResultDifference.Value(0, 0, "[a]", QueryValue.Blank, s "")
                ResultDifference.ValueType(0, 0, "[a]", i 1, d 1.0)
                ResultDifference.MissingRow(0, [| i 1; s "x" |])
                ResultDifference.UnexpectedRow(0, [| QueryValue.Blank |])
                ResultDifference.Truncated
            ]

            for x in all do
                let text = ResultDifference.describe x
                Expect.isFalse (text.Contains '\n') text
                Expect.isNonEmpty text ""

            Expect.equal
                (ResultDifference.describe all.[4])
                "row 0, [a]: \"\" (string), expected BLANK"
                "BLANK and the empty string read differently"

            Expect.equal
                (ResultDifference.describe all.[5])
                "row 0, [a]: 1.0 (double), expected 1 (int64) (type only)"
                ""
    ]
