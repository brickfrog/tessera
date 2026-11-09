namespace Tessera

open System
open System.Globalization
open System.Text
open System.Text.Json.Nodes

// The result of a DAX query, and its one canonical text and JSON encoding.
//
// Everything that runs a DAX query produces this shape: `tess query` against a server (Tessera.Xmla)
// and a local engine evaluating the same query offline. The two are compared value by value, so the
// encoding is exact and has one spelling per value. It depends on nothing but the base library.

/// The data type of a result column.
[<RequireQualifiedAccess>]
type QueryType =
    | String
    | Int64
    /// IEEE 754 double (DAX "Decimal number").
    | Double
    /// Fixed decimal with four decimal places (DAX "Currency" / "Fixed decimal number").
    | Decimal
    | DateTime
    | Boolean
    | Binary
    /// The column's values don't share one type (DAX "Variant"); each value carries its own.
    | Variant

/// One value of a result.
[<RequireQualifiedAccess>]
type QueryValue =
    /// DAX BLANK. Distinct from an empty string.
    | Blank
    | String of string
    | Int64 of int64
    | Double of double
    | Decimal of decimal
    /// A date/time with no time zone (DAX has none).
    | DateTime of DateTime
    | Boolean of bool
    | Binary of byte array

type QueryColumn = {
    /// The column's name exactly as the engine returns it, e.g. `Sales[Amount]` or `[Total]`.
    Name: string
    Type: QueryType
}

/// One result set: what one EVALUATE statement returns.
type QueryResult = {
    Columns: QueryColumn list
    /// Rows in the order returned. Without ORDER BY that order is not defined: compare such results
    /// as multisets.
    Rows: QueryValue array list
    /// True when there were more rows than the limit asked for (Rows holds the first ones).
    Truncated: bool
}

module QueryType =
    let token =
        function
        | QueryType.String -> "string"
        | QueryType.Int64 -> "int64"
        | QueryType.Double -> "double"
        | QueryType.Decimal -> "decimal"
        | QueryType.DateTime -> "dateTime"
        | QueryType.Boolean -> "boolean"
        | QueryType.Binary -> "binary"
        | QueryType.Variant -> "variant"

    let all = [
        QueryType.String
        QueryType.Int64
        QueryType.Double
        QueryType.Decimal
        QueryType.DateTime
        QueryType.Boolean
        QueryType.Binary
        QueryType.Variant
    ]

    let tryParse (s: string) =
        all |> List.tryFind (fun t -> token t = s)

module QueryValue =
    let private inv = CultureInfo.InvariantCulture

    /// The type a value has by itself (None for BLANK, which fits any column).
    let typeOf =
        function
        | QueryValue.Blank -> None
        | QueryValue.String _ -> Some QueryType.String
        | QueryValue.Int64 _ -> Some QueryType.Int64
        | QueryValue.Double _ -> Some QueryType.Double
        | QueryValue.Decimal _ -> Some QueryType.Decimal
        | QueryValue.DateTime _ -> Some QueryType.DateTime
        | QueryValue.Boolean _ -> Some QueryType.Boolean
        | QueryValue.Binary _ -> Some QueryType.Binary

    /// A double's canonical text: shortest round-trip digits, always with a '.' or an exponent (so
    /// it can't be read as an integer), "0.0" for negative zero. NaN and the infinities, which JSON
    /// numbers can't express, are "NaN", "Infinity" and "-Infinity".
    let doubleText (d: double) =
        if Double.IsNaN d then
            "NaN"
        elif Double.IsPositiveInfinity d then
            "Infinity"
        elif Double.IsNegativeInfinity d then
            "-Infinity"
        elif d = 0.0 then
            "0.0"
        else
            let s = d.ToString("R", inv)

            if s.Contains '.' || s.Contains 'E' || s.Contains 'e' then
                s
            else
                s + ".0"

    /// A decimal's canonical text: exact digits, no exponent, trailing zeros removed, but always at
    /// least one decimal place ("12.0", "0.5", "-3.1415").
    let decimalText (d: decimal) =
        let s = d.ToString("0.0###########################", inv)
        if s = "-0.0" then "0.0" else s

    /// A date/time's canonical text: ISO 8601 without a zone, `yyyy-MM-ddTHH:mm:ss`, with a fraction
    /// (trailing zeros removed) only when there is one.
    let dateTimeText (d: DateTime) =
        let whole = d.ToString("yyyy-MM-dd'T'HH:mm:ss", inv)
        let fraction = d.Ticks % TimeSpan.TicksPerSecond

        if fraction = 0L then
            whole
        else
            whole + "." + fraction.ToString("0000000", inv).TrimEnd '0'

    /// A value as plain text (tables and CSV): BLANK is empty, booleans are TRUE/FALSE, binary is
    /// base64, the rest as in JSON.
    let text =
        function
        | QueryValue.Blank -> ""
        | QueryValue.String s -> s
        | QueryValue.Int64 i -> i.ToString inv
        | QueryValue.Double d -> doubleText d
        | QueryValue.Decimal d -> decimalText d
        | QueryValue.DateTime d -> dateTimeText d
        | QueryValue.Boolean true -> "TRUE"
        | QueryValue.Boolean false -> "FALSE"
        | QueryValue.Binary b -> Convert.ToBase64String b

    /// A number exactly as written (System.Text.Json keeps a parsed number's text when writing).
    let private rawNumber (text: string) : JsonNode = JsonNode.Parse text

    /// A value as JSON:
    ///   BLANK -> null; string -> string; boolean -> true/false;
    ///   int64 -> an integer number; double and decimal -> a number with a '.' or exponent
    ///   (see doubleText, decimalText), except NaN/Infinity/-Infinity -> those strings;
    ///   dateTime -> a string (see dateTimeText); binary -> a base64 string.
    let toJson (v: QueryValue) : JsonNode =
        match v with
        | QueryValue.Blank -> null
        | QueryValue.String s -> JsonValue.Create s
        | QueryValue.Boolean b -> JsonValue.Create b
        | QueryValue.Int64 i -> rawNumber (i.ToString inv)
        | QueryValue.Double d when Double.IsFinite d -> rawNumber (doubleText d)
        | QueryValue.Double d -> JsonValue.Create(doubleText d)
        | QueryValue.Decimal d -> rawNumber (decimalText d)
        | QueryValue.DateTime d -> JsonValue.Create(dateTimeText d)
        | QueryValue.Binary b -> JsonValue.Create(Convert.ToBase64String b)

module QueryResult =

    /// One result set as JSON:
    ///   { "columns": [ { "name": "Sales[Amount]", "type": "decimal" }, ... ],
    ///     "rows": [ [ 12.5, "x", null ], ... ],      // one array per row, values in column order
    ///     "rowCount": 2,                             // rows present here
    ///     "truncated": false }                       // true: the limit cut the result short
    let toJson (r: QueryResult) : JsonNode =
        let o = JsonObject()

        o.["columns"] <-
            JsonArray(
                [|
                    for c in r.Columns ->
                        let col = JsonObject()
                        col.["name"] <- JsonValue.Create c.Name
                        col.["type"] <- JsonValue.Create(QueryType.token c.Type)
                        col :> JsonNode
                |]
            )

        o.["rows"] <-
            JsonArray(
                [|
                    for row in r.Rows -> JsonArray(row |> Array.map QueryValue.toJson) :> JsonNode
                |]
            )

        o.["rowCount"] <- JsonValue.Create r.Rows.Length
        o.["truncated"] <- JsonValue.Create r.Truncated
        o

    /// The "results" array of a query: one entry per EVALUATE, in order.
    let listToJson (results: QueryResult list) : JsonNode =
        JsonArray(results |> List.map toJson |> Array.ofList)

    let private csvField (s: string) =
        if
            s.IndexOfAny [| ','; '"'; '\n'; '\r' |] >= 0
            || s.StartsWith ' '
            || s.EndsWith ' '
        then
            "\"" + s.Replace("\"", "\"\"") + "\""
        else
            s

    /// One result set as CSV (RFC 4180): a header row, then the rows, each ended by `eol`. BLANK is
    /// an empty field; an empty string is `""`, so the two stay distinct.
    let toCsv (eol: string) (r: QueryResult) : string =
        let sb = StringBuilder()

        let line (fields: string seq) =
            sb.Append(String.Join(",", fields)).Append eol |> ignore

        line (r.Columns |> List.map (fun c -> csvField c.Name))

        for row in r.Rows do
            line (
                row
                |> Array.map (fun v ->
                    match v with
                    | QueryValue.String "" -> "\"\""
                    | v -> csvField (QueryValue.text v))
            )

        sb.ToString()

    let private isNumeric =
        function
        | QueryValue.Int64 _
        | QueryValue.Double _
        | QueryValue.Decimal _ -> true
        | _ -> false

    /// One result set as an aligned text table (numbers right-aligned, BLANK shown empty; line
    /// breaks inside a value shown as \n).
    let toTable (r: QueryResult) : string list =
        let cell (v: QueryValue) =
            (QueryValue.text v).Replace("\r\n", "\\n").Replace("\n", "\\n").Replace("\r", "\\n")

        let headers = r.Columns |> List.map _.Name |> Array.ofList
        let rows = r.Rows |> List.map (Array.map (fun v -> cell v, isNumeric v))

        let widths =
            headers
            |> Array.mapi (fun i h ->
                rows
                |> List.fold (fun w row -> if i < row.Length then max w (fst row.[i]).Length else w) h.Length)

        let pad (i: int) (text: string) (right: bool) =
            if right then
                text.PadLeft widths.[i]
            else
                text.PadRight widths.[i]

        let join (cells: string array) = (String.Join("  ", cells)).TrimEnd()

        [
            yield join (headers |> Array.mapi (fun i h -> pad i h false))
            yield join (widths |> Array.map (fun w -> String('-', w)))
            for row in rows -> join (row |> Array.mapi (fun i (text, numeric) -> pad i text numeric))
        ]
