module Tessera.Tests.QueryResultTests

open System
open System.Globalization
open System.Text.Json
open Expecto
open FsCheck
open Tessera

let private jsonText (v: QueryValue) =
    match QueryValue.toJson v with
    | null -> "null"
    | node -> node.ToJsonString()

let private sample: QueryResult = {
    Columns = [
        {
            Name = "Sales[Product]"
            Type = QueryType.String
        }
        {
            Name = "[Qty]"
            Type = QueryType.Int64
        }
        {
            Name = "[Amount]"
            Type = QueryType.Decimal
        }
        {
            Name = "[Ratio]"
            Type = QueryType.Double
        }
        {
            Name = "[When]"
            Type = QueryType.DateTime
        }
        {
            Name = "[Flag]"
            Type = QueryType.Boolean
        }
        {
            Name = "[Any]"
            Type = QueryType.Variant
        }
    ]
    Rows = [
        [|
            QueryValue.String "Bike, \"red\""
            QueryValue.Int64 3L
            QueryValue.Decimal 1234.5000M
            QueryValue.Double 0.1
            QueryValue.DateTime(DateTime(2024, 1, 31))
            QueryValue.Boolean true
            QueryValue.String ""
        |]
        [|
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
            QueryValue.Blank
        |]
    ]
    Truncated = false
}

[<Tests>]
let tests =
    testList "QueryResult" [
        testCase "canonical JSON values"
        <| fun () ->
            let cases = [
                QueryValue.Blank, "null"
                QueryValue.String "", "\"\""
                QueryValue.String "a\"b", "\"a\\u0022b\""
                QueryValue.Boolean true, "true"
                QueryValue.Int64 0L, "0"
                QueryValue.Int64 Int64.MaxValue, "9223372036854775807"
                QueryValue.Int64 -42L, "-42"
                QueryValue.Double 1.0, "1.0"
                QueryValue.Double 0.1, "0.1"
                QueryValue.Double -0.0, "0.0"
                QueryValue.Double 1e21, "1E+21"
                QueryValue.Double 1.5e-7, "1.5E-07"
                QueryValue.Double(1.0 / 3.0), "0.3333333333333333"
                QueryValue.Double nan, "\"NaN\""
                QueryValue.Double infinity, "\"Infinity\""
                QueryValue.Double -infinity, "\"-Infinity\""
                QueryValue.Decimal 1234.5000M, "1234.5"
                QueryValue.Decimal 12M, "12.0"
                QueryValue.Decimal 0.0001M, "0.0001"
                QueryValue.Decimal -922337203685477.5808M, "-922337203685477.5808"
                QueryValue.Decimal 0M, "0.0"
                QueryValue.DateTime(DateTime(2024, 1, 31)), "\"2024-01-31T00:00:00\""
                QueryValue.DateTime(DateTime(1899, 12, 30, 13, 5, 9)), "\"1899-12-30T13:05:09\""
                QueryValue.DateTime(DateTime(2024, 1, 31, 1, 2, 3, 450)), "\"2024-01-31T01:02:03.45\""
                QueryValue.Binary [| 1uy; 2uy; 255uy |], "\"AQL/\""
            ]

            for value, expected in cases do
                Expect.equal (jsonText value) expected (sprintf "%A" value)

        testCase "a result set's JSON shape"
        <| fun () ->
            use doc = JsonDocument.Parse((QueryResult.toJson sample).ToJsonString())
            let root = doc.RootElement

            Expect.equal
                [ for p in root.EnumerateObject() -> p.Name ]
                [ "columns"; "rows"; "rowCount"; "truncated" ]
                "exactly these keys, in this order"

            Expect.equal
                [
                    for c in root.GetProperty("columns").EnumerateArray() ->
                        c.GetProperty("name").GetString(), c.GetProperty("type").GetString()
                ]
                [
                    "Sales[Product]", "string"
                    "[Qty]", "int64"
                    "[Amount]", "decimal"
                    "[Ratio]", "double"
                    "[When]", "dateTime"
                    "[Flag]", "boolean"
                    "[Any]", "variant"
                ]
                ""

            Expect.equal (root.GetProperty("rowCount").GetInt32()) 2 ""
            Expect.isFalse (root.GetProperty("truncated").GetBoolean()) ""

            Expect.equal
                (root.GetProperty("rows").[0].GetRawText())
                ("[\"Bike, "
                 + "\\"
                 + "u0022red"
                 + "\\"
                 + "u0022\",3,1234.5,0.1,\"2024-01-31T00:00:00\",true,\"\"]")
                ""

            Expect.equal (root.GetProperty("rows").[1].GetRawText()) "[null,null,null,null,null,null,null]" ""
            // The currency value survives exactly for a reader that asks for a decimal.
            Expect.equal (root.GetProperty("rows").[0].[2].GetDecimal()) 1234.5M ""

        testCase "type tokens round-trip"
        <| fun () ->
            for t in QueryType.all do
                Expect.equal (QueryType.tryParse (QueryType.token t)) (Some t) ""

            Expect.equal (QueryType.tryParse "Int64") None "tokens are case-sensitive"

        testCase "CSV: quoting, BLANK empty, empty string quoted"
        <| fun () ->
            Expect.equal
                (QueryResult.toCsv "\n" sample)
                ("Sales[Product],[Qty],[Amount],[Ratio],[When],[Flag],[Any]\n"
                 + "\"Bike, \"\"red\"\"\",3,1234.5,0.1,2024-01-31T00:00:00,TRUE,\"\"\n"
                 + ",,,,,,\n")
                ""

            Expect.equal
                (QueryResult.toCsv "\r\n" { sample with Rows = [] })
                "Sales[Product],[Qty],[Amount],[Ratio],[When],[Flag],[Any]\r\n"
                ""

        testCase "text table: numbers right-aligned, line breaks escaped, no trailing spaces"
        <| fun () ->
            let r: QueryResult = {
                Columns = [
                    {
                        Name = "Name"
                        Type = QueryType.String
                    }
                    { Name = "[N]"; Type = QueryType.Int64 }
                ]
                Rows = [
                    [| QueryValue.String "a\nb"; QueryValue.Int64 5L |]
                    [| QueryValue.String "long name"; QueryValue.Int64 12345L |]
                    [| QueryValue.Blank; QueryValue.Blank |]
                ]
                Truncated = false
            }

            Expect.equal
                (QueryResult.toTable r)
                [
                    "Name       [N]"
                    "---------  -----"
                    "a\\nb           5"
                    "long name  12345"
                    ""
                ]
                ""

        testCase "values don't depend on the current culture"
        <| fun () ->
            let before = CultureInfo.CurrentCulture

            try
                CultureInfo.CurrentCulture <- CultureInfo "da-DK"
                Expect.equal (jsonText (QueryValue.Double 1234.5)) "1234.5" ""
                Expect.equal (jsonText (QueryValue.Decimal 1234.5M)) "1234.5" ""

                Expect.equal
                    (QueryValue.text (QueryValue.DateTime(DateTime(2024, 3, 1, 14, 0, 0))))
                    "2024-03-01T14:00:00"
                    ""
            finally
                CultureInfo.CurrentCulture <- before

        testProperty "a double's JSON is a number that reads back to the same double, and is never an integer literal"
        <| fun (d: NormalFloat) ->
            let d = d.Get
            let text = jsonText (QueryValue.Double d)
            use doc = JsonDocument.Parse text

            doc.RootElement.ValueKind = JsonValueKind.Number
            && doc.RootElement.GetDouble() = d
            && (text.Contains '.' || text.Contains 'E')

        testProperty "a decimal's JSON reads back exactly and always has a decimal point"
        <| fun (d: decimal) ->
            let text = jsonText (QueryValue.Decimal d)
            use doc = JsonDocument.Parse text
            doc.RootElement.GetDecimal() = d && text.Contains '.' && not (text.Contains 'E')

        testProperty "an int64's JSON reads back exactly"
        <| fun (i: int64) ->
            use doc = JsonDocument.Parse(jsonText (QueryValue.Int64 i))
            doc.RootElement.GetInt64() = i

        testProperty "a date/time's text reads back to the same instant"
        <| fun (d: DateTime) ->
            let text = QueryValue.dateTimeText d

            DateTime.Parse(text, CultureInfo.InvariantCulture, DateTimeStyles.None).Ticks = d.Ticks
            && not (text.EndsWith "Z")
    ]
