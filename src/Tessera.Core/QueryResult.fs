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

    let private isoFormats = [|
        "yyyy-MM-dd'T'HH:mm:ss"
        "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF"
        "yyyy-MM-dd'T'HH:mm"
        "yyyy-MM-dd"
    |]

    let private kindName (node: JsonNode) =
        match node with
        | null -> "null"
        | node ->
            match node.GetValueKind() with
            | Text.Json.JsonValueKind.String -> "a string"
            | Text.Json.JsonValueKind.Number -> "a number"
            | Text.Json.JsonValueKind.True
            | Text.Json.JsonValueKind.False -> "a boolean"
            | Text.Json.JsonValueKind.Array -> "an array"
            | _ -> "an object"

    let private isKind (kind: Text.Json.JsonValueKind) (node: JsonNode) =
        not (isNull node) && node.GetValueKind() = kind

    let private isBool (node: JsonNode) =
        isKind Text.Json.JsonValueKind.True node
        || isKind Text.Json.JsonValueKind.False node

    /// Read a value written by `toJson` back, as a value of a column of type `t`. Numbers are read
    /// from their text, so a decimal keeps every digit and an int64 above 2^53 is exact.
    ///
    /// In a `variant` column a value's type is not in its spelling, so: a string is text, `true` /
    /// `false` a boolean, a number without `.` or exponent an int64, any other number a double.
    /// Anything else (a decimal, a date, a binary, the text "NaN") is written as
    /// `{ "type": "decimal", "value": 12.5 }`, which is accepted in every column.
    let rec ofJson (t: QueryType) (node: JsonNode) : Result<QueryValue, string> =
        let wrong (expected: string) =
            Error $"expected {expected} for a {QueryType.token t} value, found {kindName node}"

        let raw () = node.ToJsonString()

        if isNull node then
            Ok QueryValue.Blank
        elif isKind Text.Json.JsonValueKind.Object node then
            let o = node.AsObject()

            match o.["type"] with
            | typeNode when
                isKind Text.Json.JsonValueKind.String typeNode
                && o.ContainsKey "value"
                && o.Count = 2
                ->
                match QueryType.tryParse (typeNode.GetValue<string>()) with
                | Some QueryType.Variant
                | None -> Error $"""unknown value type '{typeNode.GetValue<string>()}' in {raw ()}"""
                | Some inner -> ofJson inner o.["value"]
            | _ -> Error $"""a value written as an object must be {{ "type": ..., "value": ... }}, found {raw ()}"""
        else
            match t with
            | QueryType.String ->
                if isKind Text.Json.JsonValueKind.String node then
                    Ok(QueryValue.String(node.GetValue<string>()))
                else
                    wrong "a string"
            | QueryType.Boolean ->
                if isBool node then
                    Ok(QueryValue.Boolean(node.GetValue<bool>()))
                else
                    wrong "true or false"
            | QueryType.Int64 ->
                if isKind Text.Json.JsonValueKind.Number node then
                    match Int64.TryParse(raw (), NumberStyles.AllowLeadingSign, inv) with
                    | true, i -> Ok(QueryValue.Int64 i)
                    | _ -> Error $"{raw ()} is not an int64 (an integer without a decimal point)"
                else
                    wrong "an integer"
            | QueryType.Double ->
                if isKind Text.Json.JsonValueKind.Number node then
                    Ok(QueryValue.Double(Double.Parse(raw (), NumberStyles.Float, inv)))
                elif isKind Text.Json.JsonValueKind.String node then
                    match node.GetValue<string>() with
                    | "NaN" -> Ok(QueryValue.Double Double.NaN)
                    | "Infinity" -> Ok(QueryValue.Double Double.PositiveInfinity)
                    | "-Infinity" -> Ok(QueryValue.Double Double.NegativeInfinity)
                    | other -> Error $"\"{other}\" is not a double (a number, or \"NaN\", \"Infinity\", \"-Infinity\")"
                else
                    wrong "a number"
            | QueryType.Decimal ->
                if isKind Text.Json.JsonValueKind.Number node then
                    match Decimal.TryParse(raw (), NumberStyles.Float, inv) with
                    | true, d -> Ok(QueryValue.Decimal d)
                    | _ -> Error $"{raw ()} is out of range for a decimal"
                else
                    wrong "a number"
            | QueryType.DateTime ->
                if isKind Text.Json.JsonValueKind.String node then
                    let text = node.GetValue<string>()

                    match DateTime.TryParseExact(text, isoFormats, inv, DateTimeStyles.None) with
                    | true, d -> Ok(QueryValue.DateTime d)
                    | _ -> Error $"\"{text}\" is not a date/time (ISO 8601 without a zone: 2024-01-31T00:00:00)"
                else
                    wrong "a date/time string"
            | QueryType.Binary ->
                if isKind Text.Json.JsonValueKind.String node then
                    try
                        Ok(QueryValue.Binary(Convert.FromBase64String(node.GetValue<string>())))
                    with :? FormatException ->
                        Error "a binary value must be a base64 string"
                else
                    wrong "a base64 string"
            | QueryType.Variant ->
                if isKind Text.Json.JsonValueKind.String node then
                    ofJson QueryType.String node
                elif isBool node then
                    ofJson QueryType.Boolean node
                elif isKind Text.Json.JsonValueKind.Number node then
                    let text = raw ()

                    if text.IndexOfAny [| '.'; 'e'; 'E' |] < 0 then
                        ofJson QueryType.Int64 node
                    else
                        ofJson QueryType.Double node
                else
                    wrong "a string, a number or a boolean"

/// What `QueryResult.compare` treats as equal.
type CompareOptions = {
    /// Row order matters (the query has an ORDER BY). Otherwise rows are compared as a multiset.
    Ordered: bool
    /// Two doubles a and b are equal when |a - b| <= AbsoluteTolerance, or
    /// |a - b| <= RelativeTolerance x max(|a|, |b|). Only doubles (and a double against another
    /// number type) get a tolerance: int64 and decimal values compare exactly.
    RelativeTolerance: float
    AbsoluteTolerance: float
    /// Columns (expected names) whose types are not checked: neither the declared type nor a
    /// number's own type. Their values still are.
    IgnoreTypesOf: Set<string>
}

module CompareOptions =
    /// Unordered; doubles equal to about 9 significant digits.
    let defaults = {
        Ordered = false
        RelativeTolerance = 1e-9
        AbsoluteTolerance = 1e-12
        IgnoreTypesOf = Set.empty
    }

    /// Unordered; doubles must be the same number.
    let exact = {
        defaults with
            RelativeTolerance = 0.0
            AbsoluteTolerance = 0.0
    }

/// One way an actual result differs from the expected one. Row numbers are 0-based.
[<RequireQualifiedAccess>]
type ResultDifference =
    /// An expected column the actual result doesn't have.
    | MissingColumn of name: string
    /// A column of the actual result that wasn't expected.
    | UnexpectedColumn of name: string
    /// The column is there, declared with another type. A difference of type, not of value.
    | ColumnType of column: string * expected: QueryType * actual: QueryType
    | RowCount of expected: int * actual: int
    /// Another value. `row` is the expected row; `actualRow` the actual row it was paired with.
    | Value of row: int * actualRow: int * column: string * expected: QueryValue * actual: QueryValue
    /// The same number in another number type (int64 1, double 1.0). A difference of type, not of value.
    | ValueType of row: int * actualRow: int * column: string * expected: QueryValue * actual: QueryValue
    /// An expected row with no counterpart.
    | MissingRow of row: int * values: QueryValue array
    /// An actual row with no counterpart.
    | UnexpectedRow of actualRow: int * values: QueryValue array
    /// The actual result was cut short by a row limit: what is missing can't be told.
    | Truncated

module ResultDifference =

    /// A difference of type only: the values agree.
    let isTypeOnly =
        function
        | ResultDifference.ColumnType _
        | ResultDifference.ValueType _ -> true
        | _ -> false

    let private show (v: QueryValue) =
        match v with
        | QueryValue.Blank -> "BLANK"
        | QueryValue.String s -> "\"" + s + "\""
        | v -> QueryValue.text v

    let private typed (v: QueryValue) =
        match QueryValue.typeOf v with
        | Some t -> $"{show v} ({QueryType.token t})"
        | None -> show v

    let private showRow (values: QueryValue array) =
        "[" + (values |> Array.map show |> String.concat ", ") + "]"

    /// One line of plain text.
    let describe =
        function
        | ResultDifference.MissingColumn name -> $"column {name} is missing"
        | ResultDifference.UnexpectedColumn name -> $"column {name} was not expected"
        | ResultDifference.ColumnType(column, expected, actual) ->
            $"column {column} is {QueryType.token actual}, expected {QueryType.token expected}"
        | ResultDifference.RowCount(expected, actual) -> $"{actual} rows, expected {expected}"
        | ResultDifference.Value(row, _, column, expected, actual) ->
            $"row {row}, {column}: {typed actual}, expected {typed expected}"
        | ResultDifference.ValueType(row, _, column, expected, actual) ->
            $"row {row}, {column}: {typed actual}, expected {typed expected} (type only)"
        | ResultDifference.MissingRow(row, values) -> $"expected row {row} is missing: {showRow values}"
        | ResultDifference.UnexpectedRow(row, values) -> $"row {row} was not expected: {showRow values}"
        | ResultDifference.Truncated -> "the result was cut short by a row limit"

/// The outcome of `QueryResult.compare`: no differences means the results are the same.
type Comparison = {
    Differences: ResultDifference list
} with

    /// Nothing differs, types included.
    member this.IsMatch = this.Differences.IsEmpty

    /// Every value agrees; at most types differ (see `ResultDifference.isTypeOnly`).
    member this.ValuesMatch = this.Differences |> List.forall ResultDifference.isTypeOnly

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

    /// Read a result set written by `toJson` back. `rowCount` and `truncated` may be left out; a
    /// `rowCount` that isn't the number of rows is an error. See `QueryValue.ofJson` for values.
    let ofJson (node: JsonNode) : Result<QueryResult, string> =
        let isKind (kind: Text.Json.JsonValueKind) (n: JsonNode) =
            not (isNull n) && n.GetValueKind() = kind

        if not (isKind Text.Json.JsonValueKind.Object node) then
            Error "a result must be an object with \"columns\" and \"rows\""
        else
            let o = node.AsObject()
            let columnsNode = o.["columns"]
            let rowsNode = o.["rows"]

            if not (isKind Text.Json.JsonValueKind.Array columnsNode) then
                Error "\"columns\" must be an array of { \"name\", \"type\" }"
            elif not (isKind Text.Json.JsonValueKind.Array rowsNode) then
                Error "\"rows\" must be an array of arrays"
            else
                let columns =
                    columnsNode.AsArray()
                    |> Seq.mapi (fun i c ->
                        if not (isKind Text.Json.JsonValueKind.Object c) then
                            Error $"column {i} must be {{ \"name\", \"type\" }}"
                        else
                            let c = c.AsObject()

                            match c.["name"], c.["type"] with
                            | name, typeNode when
                                isKind Text.Json.JsonValueKind.String name
                                && isKind Text.Json.JsonValueKind.String typeNode
                                ->
                                match QueryType.tryParse (typeNode.GetValue<string>()) with
                                | Some t ->
                                    Ok {
                                        Name = name.GetValue<string>()
                                        Type = t
                                    }
                                | None ->
                                    let known = QueryType.all |> List.map QueryType.token |> String.concat ", "

                                    Error
                                        $"column {i} ({name.GetValue<string>()}): unknown type '{typeNode.GetValue<string>()}' (known: {known})"
                            | _ -> Error $"column {i} needs a \"name\" and a \"type\", both strings")
                    |> List.ofSeq

                match
                    columns
                    |> List.tryPick (function
                        | Error e -> Some e
                        | Ok _ -> None)
                with
                | Some e -> Error e
                | None ->
                    let columns =
                        columns
                        |> List.choose (function
                            | Ok c -> Some c
                            | Error _ -> None)

                    let types = columns |> List.map _.Type |> Array.ofList
                    let rows = ResizeArray<QueryValue array>()
                    let mutable error = None
                    let mutable r = 0

                    for rowNode in rowsNode.AsArray() do
                        if error.IsNone then
                            if not (isKind Text.Json.JsonValueKind.Array rowNode) then
                                error <- Some $"row {r} must be an array"
                            else
                                let cells = rowNode.AsArray()

                                if cells.Count <> types.Length then
                                    error <- Some $"row {r} has {cells.Count} values for {types.Length} columns"
                                else
                                    let row = Array.create types.Length QueryValue.Blank

                                    for i in 0 .. types.Length - 1 do
                                        match QueryValue.ofJson types.[i] cells.[i] with
                                        | Ok v -> row.[i] <- v
                                        | Error e ->
                                            if error.IsNone then
                                                error <- Some $"row {r}, {columns.[i].Name}: {e}"

                                    rows.Add row

                        r <- r + 1

                    let truncated =
                        match o.["truncated"] with
                        | null -> Ok false
                        | t when
                            t.GetValueKind() = Text.Json.JsonValueKind.True
                            || t.GetValueKind() = Text.Json.JsonValueKind.False
                            ->
                            Ok(t.GetValue<bool>())
                        | _ -> Error "\"truncated\" must be true or false"

                    let rowCount =
                        match o.["rowCount"] with
                        | null -> Ok()
                        | n when isKind Text.Json.JsonValueKind.Number n && n.ToJsonString() = string rows.Count -> Ok()
                        | n -> Error $"\"rowCount\" is {n.ToJsonString()} but there are {rows.Count} rows"

                    match error, truncated, rowCount with
                    | Some e, _, _ -> Error e
                    | _, Error e, _
                    | _, _, Error e -> Error e
                    | None, Ok truncated, Ok() ->
                        Ok {
                            Columns = columns
                            Rows = List.ofSeq rows
                            Truncated = truncated
                        }

    /// The "results" array of a query, read back.
    let listOfJson (node: JsonNode) : Result<QueryResult list, string> =
        if isNull node || node.GetValueKind() <> Text.Json.JsonValueKind.Array then
            Error "results must be an array, one entry per EVALUATE"
        else
            let results = node.AsArray() |> Seq.map ofJson |> List.ofSeq

            match
                results
                |> List.indexed
                |> List.tryPick (function
                    | i, Error e -> Some $"result {i}: {e}"
                    | _ -> None)
            with
            | Some e -> Error e
            | None ->
                Ok(
                    results
                    |> List.choose (function
                        | Ok r -> Some r
                        | Error _ -> None)
                )

    // ---- Comparison -------------------------------------------------------------------------------

    [<Struct>]
    type private Cell =
        | Same
        | TypeDiffers
        | Differs

    let private doublesEqual (o: CompareOptions) (a: double) (b: double) =
        if Double.IsNaN a || Double.IsNaN b then
            Double.IsNaN a && Double.IsNaN b
        elif Double.IsInfinity a || Double.IsInfinity b then
            a = b
        else
            let d = abs (a - b)
            d <= o.AbsoluteTolerance || d <= o.RelativeTolerance * max (abs a) (abs b)

    let private compareCell (o: CompareOptions) (expected: QueryValue) (actual: QueryValue) : Cell =
        let ofBool b = if b then Same else Differs
        let typeOnly b = if b then TypeDiffers else Differs

        match expected, actual with
        | QueryValue.Blank, QueryValue.Blank -> Same
        | QueryValue.String a, QueryValue.String b -> ofBool (String.Equals(a, b, StringComparison.Ordinal))
        | QueryValue.Int64 a, QueryValue.Int64 b -> ofBool (a = b)
        | QueryValue.Decimal a, QueryValue.Decimal b -> ofBool (a = b)
        | QueryValue.Double a, QueryValue.Double b -> ofBool (doublesEqual o a b)
        | QueryValue.DateTime a, QueryValue.DateTime b -> ofBool (a = b)
        | QueryValue.Boolean a, QueryValue.Boolean b -> ofBool (a = b)
        | QueryValue.Binary a, QueryValue.Binary b -> ofBool (a.AsSpan().SequenceEqual(b.AsSpan()))
        // The same number in two number types.
        | QueryValue.Int64 a, QueryValue.Decimal b
        | QueryValue.Decimal b, QueryValue.Int64 a -> typeOnly (decimal a = b)
        | QueryValue.Double d, QueryValue.Int64 i
        | QueryValue.Int64 i, QueryValue.Double d -> typeOnly (doublesEqual o d (double i))
        | QueryValue.Double d, QueryValue.Decimal m
        | QueryValue.Decimal m, QueryValue.Double d -> typeOnly (doublesEqual o d (double m))
        | _ -> Differs

    /// A row's exact spelling, types included: equal keys are equal rows.
    let private rowKey (row: QueryValue array) =
        let sb = StringBuilder()

        for v in row do
            let tag =
                match QueryValue.typeOf v with
                | Some t -> QueryType.token t
                | None -> "blank"

            let text = QueryValue.text v

            sb.Append(tag).Append(':').Append(text.Length).Append(':').Append(text).Append('|')
            |> ignore

        sb.ToString()

    /// Leftover rows beyond this many (expected x actual) are reported as missing and unexpected
    /// without looking for near matches, which is quadratic.
    let private pairingLimit = 4_000_000L

    /// Compare an actual result with the expected one.
    ///
    /// - Columns are matched by name (exactly, else ignoring case: DAX names are case-insensitive),
    ///   so their order doesn't matter. Columns only one side has are reported and left out of the
    ///   row comparison.
    /// - Rows are compared in order when `Ordered`, otherwise as a multiset: each expected row is
    ///   paired with an actual row that equals it, then with one that equals it but for number
    ///   types, then with the one that differs in the fewest values; rows left over are reported
    ///   whole.
    /// - Values: text by code unit; int64 and decimal exactly; doubles within the tolerance;
    ///   BLANK only equals BLANK (never 0 or ""). The same number in two number types is a
    ///   `ValueType` difference, and a column declared with another type a `ColumnType` one: both
    ///   are differences of type, kept apart from differences of value (`Comparison.ValuesMatch`).
    let compare (options: CompareOptions) (expected: QueryResult) (actual: QueryResult) : Comparison =
        let differences = ResizeArray<ResultDifference>()
        let actualColumns = Array.ofList actual.Columns
        let used = Array.create actualColumns.Length false

        let find (comparison: StringComparison) (name: string) =
            let mutable found = -1

            for i in 0 .. actualColumns.Length - 1 do
                if
                    found < 0
                    && not used.[i]
                    && String.Equals(actualColumns.[i].Name, name, comparison)
                then
                    found <- i

            if found >= 0 then
                used.[found] <- true

            found

        // (expected index, actual index, name, check types)
        let pairs = ResizeArray<struct (int * int * string * bool)>()

        let exact =
            expected.Columns |> List.map (fun c -> find StringComparison.Ordinal c.Name)

        List.zip expected.Columns exact
        |> List.iteri (fun i (c, exactIndex) ->
            let index =
                if exactIndex >= 0 then
                    exactIndex
                else
                    find StringComparison.OrdinalIgnoreCase c.Name

            if index < 0 then
                differences.Add(ResultDifference.MissingColumn c.Name)
            else
                let checkTypes = not (options.IgnoreTypesOf.Contains c.Name)

                if checkTypes && c.Type <> actualColumns.[index].Type then
                    differences.Add(ResultDifference.ColumnType(c.Name, c.Type, actualColumns.[index].Type))

                pairs.Add(struct (i, index, c.Name, checkTypes)))

        for i in 0 .. actualColumns.Length - 1 do
            if not used.[i] then
                differences.Add(ResultDifference.UnexpectedColumn actualColumns.[i].Name)

        if actual.Truncated then
            differences.Add ResultDifference.Truncated

        let pairs = pairs.ToArray()
        let names = pairs |> Array.map (fun struct (_, _, name, _) -> name)
        let checkTypes = pairs |> Array.map (fun struct (_, _, _, check) -> check)

        let project (pick: struct (int * int * string * bool) -> int) (rows: QueryValue array list) =
            rows
            |> List.map (fun row ->
                pairs
                |> Array.map (fun p ->
                    let i = pick p
                    if i < row.Length then row.[i] else QueryValue.Blank))
            |> Array.ofList

        let expectedRows = project (fun struct (e, _, _, _) -> e) expected.Rows
        let actualRows = project (fun struct (_, a, _, _) -> a) actual.Rows

        if expectedRows.Length <> actualRows.Length then
            differences.Add(ResultDifference.RowCount(expectedRows.Length, actualRows.Length))

        let cell (column: int) (e: QueryValue) (a: QueryValue) =
            match compareCell options e a with
            | TypeDiffers when not checkTypes.[column] -> Same
            | c -> c

        /// How many values of two rows differ, and how many only in type.
        let score (e: QueryValue array) (a: QueryValue array) =
            let mutable differ = 0
            let mutable types = 0

            for c in 0 .. e.Length - 1 do
                match cell c e.[c] a.[c] with
                | Same -> ()
                | TypeDiffers -> types <- types + 1
                | Differs -> differ <- differ + 1

            struct (differ, types)

        let reportPair (er: int) (ar: int) =
            let e, a = expectedRows.[er], actualRows.[ar]

            for c in 0 .. e.Length - 1 do
                match cell c e.[c] a.[c] with
                | Same -> ()
                | TypeDiffers -> differences.Add(ResultDifference.ValueType(er, ar, names.[c], e.[c], a.[c]))
                | Differs -> differences.Add(ResultDifference.Value(er, ar, names.[c], e.[c], a.[c]))

        if options.Ordered then
            for r in 0 .. min expectedRows.Length actualRows.Length - 1 do
                reportPair r r

            for r in actualRows.Length .. expectedRows.Length - 1 do
                differences.Add(ResultDifference.MissingRow(r, expectedRows.[r]))

            for r in expectedRows.Length .. actualRows.Length - 1 do
                differences.Add(ResultDifference.UnexpectedRow(r, actualRows.[r]))
        else
            // 1. Rows spelled the same, in linear time.
            let byKey = Collections.Generic.Dictionary<string, Collections.Generic.Queue<int>>()

            actualRows
            |> Array.iteri (fun i row ->
                let key = rowKey row

                match byKey.TryGetValue key with
                | true, q -> q.Enqueue i
                | _ ->
                    let q = Collections.Generic.Queue<int>()
                    q.Enqueue i
                    byKey.[key] <- q)

            let actualTaken = Array.create actualRows.Length false
            let expectedLeft = ResizeArray<int>()

            expectedRows
            |> Array.iteri (fun i row ->
                match byKey.TryGetValue(rowKey row) with
                | true, q when q.Count > 0 -> actualTaken.[q.Dequeue()] <- true
                | _ -> expectedLeft.Add i)

            let actualLeft = ResizeArray<int>()

            for i in 0 .. actualRows.Length - 1 do
                if not actualTaken.[i] then
                    actualLeft.Add i

            if int64 expectedLeft.Count * int64 actualLeft.Count <= pairingLimit then
                // 2. Equal within the tolerance; 3. equal but for number types; 4. the closest row.
                let take (accept: struct (int * int) -> bool) (best: bool) =
                    let remaining = ResizeArray<int>()

                    for er in expectedLeft do
                        let mutable chosen = -1
                        let mutable chosenScore = struct (Int32.MaxValue, Int32.MaxValue)

                        for k in 0 .. actualLeft.Count - 1 do
                            if chosen < 0 || best then
                                let s = score expectedRows.[er] actualRows.[actualLeft.[k]]

                                if accept s && (chosen < 0 || s < chosenScore) then
                                    chosen <- k
                                    chosenScore <- s

                        if chosen >= 0 then
                            reportPair er actualLeft.[chosen]
                            actualLeft.RemoveAt chosen
                        else
                            remaining.Add er

                    expectedLeft.Clear()
                    expectedLeft.AddRange remaining

                take (fun struct (differ, types) -> differ = 0 && types = 0) false
                take (fun struct (differ, _) -> differ = 0) false
                // A row that shares nothing with any other is missing, not "changed in every value".
                take (fun struct (differ, _) -> names.Length > 0 && differ < names.Length) true

                // What is left pairs up one to one only when the counts agree and there is one column,
                // where "expected 5, found 6" says more than a missing and an unexpected row.
                if names.Length = 1 && expectedLeft.Count = actualLeft.Count then
                    take (fun _ -> true) true

            for er in expectedLeft do
                differences.Add(ResultDifference.MissingRow(er, expectedRows.[er]))

            for ar in actualLeft do
                differences.Add(ResultDifference.UnexpectedRow(ar, actualRows.[ar]))

        { Differences = List.ofSeq differences }

    /// Compare the results of a whole query, one `Comparison` per expected result. A missing or
    /// extra result set is a `RowCount`-free mismatch of its own: `Error` says how many there were.
    let compareAll
        (options: CompareOptions)
        (expected: QueryResult list)
        (actual: QueryResult list)
        : Result<Comparison list, string> =
        if expected.Length <> actual.Length then
            Error $"{actual.Length} result sets, expected {expected.Length}"
        else
            Ok(List.map2 (compare options) expected actual)

/// One cell's format string, and its value formatted with it.
type FormattedCell = {
    FormatString: string
    /// The value as FORMAT ( <value>, <format string> ) writes it; BLANK is "".
    Text: string
}

/// A result with the format strings of its cells, where the local engine can tell them exactly
/// (`tess query --formatted`). A server's DAX query result carries no format strings.
type FormattedResult = {
    Result: QueryResult
    /// Parallel to `Result.Rows`: per row, per column, the cell's format string and its formatted
    /// value, or None where the cell has no format string.
    Cells: FormattedCell option array list
}

module FormattedResult =

    /// The first `n` rows, marked truncated if there were more.
    let truncate (n: int) (r: FormattedResult) : FormattedResult =
        if r.Result.Rows.Length > n then
            {
                Result = {
                    r.Result with
                        Rows = List.truncate n r.Result.Rows
                        Truncated = true
                }
                Cells = List.truncate n r.Cells
            }
        else
            r

    /// The format strings of column `c`, one per row.
    let private formatStrings (r: FormattedResult) (c: int) : string option list =
        r.Cells |> List.map (fun row -> row.[c] |> Option.map _.FormatString)

    /// As `QueryResult.toJson`, and also:
    ///   - on each column with a format string: "formatString" when every row has the same one,
    ///     otherwise "formatStrings", one per row (null for a row whose cell has none);
    ///   - "formatted": parallel to "rows", each cell's value formatted with its format string, or
    ///     null where it has none.
    let toJson (r: FormattedResult) : JsonNode =
        let o = (QueryResult.toJson r.Result).AsObject()
        let columns = o.["columns"].AsArray()

        for c in 0 .. columns.Count - 1 do
            let col = columns.[c].AsObject()
            let fs = formatStrings r c

            match List.distinct fs with
            | [ Some f ] -> col.["formatString"] <- JsonValue.Create f
            | distinct when distinct |> List.exists Option.isSome ->
                col.["formatStrings"] <-
                    JsonArray(
                        fs
                        |> List.map (fun f ->
                            match f with
                            | Some f -> JsonValue.Create f :> JsonNode
                            | None -> null)
                        |> Array.ofList
                    )
            | _ -> ()

        let formatted =
            JsonArray(
                [|
                    for row in r.Cells ->
                        JsonArray(
                            row
                            |> Array.map (fun cell ->
                                match cell with
                                | Some cell -> JsonValue.Create cell.Text :> JsonNode
                                | None -> null)
                        )
                        :> JsonNode
                |]
            )

        // After "rows", before "rowCount".
        let rowCount = o.["rowCount"]
        let truncated = o.["truncated"]
        o.Remove "rowCount" |> ignore
        o.Remove "truncated" |> ignore
        o.["formatted"] <- formatted
        o.["rowCount"] <- rowCount
        o.["truncated"] <- truncated
        o

    /// The "results" array of a query: one entry per EVALUATE, in order.
    let listToJson (results: FormattedResult list) : JsonNode =
        JsonArray(results |> List.map toJson |> Array.ofList)

    /// The result with every cell that has a format string replaced by its formatted text (a
    /// string), for the text and CSV output.
    let private shown (r: FormattedResult) : QueryResult = {
        r.Result with
            Rows =
                List.map2
                    (fun (values: QueryValue array) (cells: FormattedCell option array) ->
                        Array.map2
                            (fun v cell ->
                                match v, cell with
                                | QueryValue.Blank, _ -> QueryValue.Blank
                                | _, Some cell -> QueryValue.String cell.Text
                                | _, None -> v)
                            values
                            cells)
                    r.Result.Rows
                    r.Cells
    }

    /// As `QueryResult.toCsv`, with formatted values where cells have a format string.
    let toCsv (eol: string) (r: FormattedResult) : string = QueryResult.toCsv eol (shown r)

    /// As `QueryResult.toTable`, with formatted values where cells have a format string.
    let toTable (r: FormattedResult) : string list = QueryResult.toTable (shown r)
