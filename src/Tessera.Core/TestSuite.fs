namespace Tessera

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open YamlDotNet.Core
open YamlDotNet.RepresentationModel

// Test suites for `tess test`: DAX assertion tests for a model, written in YAML (or JSON).
//
// The format follows the Tabular Editor CLI's `.te-tests/*.test.yaml` where that is sensible (suite,
// description, defaults, tests[] with name / query / query_file / tags / tolerance / tolerance_abs /
// matrix / expect), so a suite written for one runs on the other, and adds what testing offline
// needs: fixture data (`data`, inline `tables`), `filters`, `measure` / `expression` / `assert`
// shorthands, expected tables and snapshots. The README's "Testing a model" section is the reference.
//
// This file only reads suites: nothing here evaluates DAX. Every problem carries the line it is on.

/// A value written in a suite: an expected value, a filter value, a matrix value.
[<RequireQualifiedAccess>]
type SuiteValue =
    | Null
    | Bool of bool
    | Int of int64
    | Float of double
    | Text of string
    /// An unquoted ISO date or date-time in YAML (`2024-01-31`, `2024-01-31T13:05:00`).
    | Date of DateTime

module SuiteValue =
    let private inv = CultureInfo.InvariantCulture

    let dateText (d: DateTime) =
        if d.TimeOfDay = TimeSpan.Zero then
            d.ToString("yyyy-MM-dd", inv)
        elif d.Millisecond = 0 && d.Ticks % TimeSpan.TicksPerSecond = 0L then
            d.ToString("yyyy-MM-dd'T'HH:mm:ss", inv)
        else
            d.ToString("yyyy-MM-dd'T'HH:mm:ss.FFFFFFF", inv)

    /// The value as a DAX literal: `42`, `1.5`, `"it's ""x"""`, `TRUE`, `BLANK ()`, `dt"2024-01-31"`.
    let daxLiteral (v: SuiteValue) : string =
        match v with
        | SuiteValue.Null -> "BLANK ()"
        | SuiteValue.Bool true -> "TRUE"
        | SuiteValue.Bool false -> "FALSE"
        | SuiteValue.Int i -> i.ToString inv
        | SuiteValue.Float f ->
            let text = f.ToString("R", inv)

            // DAX reads `1E+20` as a number too, but a bare `5` would be a whole number.
            if text.Contains '.' || text.Contains 'E' || text.Contains 'e' then
                text
            else
                text + ".0"
        | SuiteValue.Text s -> "\"" + s.Replace("\"", "\"\"") + "\""
        | SuiteValue.Date d -> "dt\"" + dateText d + "\""

    /// The value as plain text: what a `{placeholder}` inside a longer text becomes, and what a
    /// matrix test's name shows.
    let text (v: SuiteValue) : string =
        match v with
        | SuiteValue.Null -> "BLANK"
        | SuiteValue.Bool true -> "TRUE"
        | SuiteValue.Bool false -> "FALSE"
        | SuiteValue.Int i -> i.ToString inv
        | SuiteValue.Float f -> f.ToString("R", inv)
        | SuiteValue.Text s -> s
        | SuiteValue.Date d -> dateText d

    /// The value as JSON text (a date as an ISO string): how inline fixture rows reach the loader.
    let json (v: SuiteValue) : string =
        match v with
        | SuiteValue.Null -> "null"
        | SuiteValue.Bool true -> "true"
        | SuiteValue.Bool false -> "false"
        | SuiteValue.Int i -> i.ToString inv
        | SuiteValue.Float f when Double.IsFinite f -> f.ToString("R", inv)
        | SuiteValue.Float f -> JsonSerializer.Serialize(f.ToString("R", inv))
        | SuiteValue.Text s -> JsonSerializer.Serialize s
        | SuiteValue.Date d -> JsonSerializer.Serialize(dateText d)

    let private intPattern = Regex(@"^[-+]?[0-9]+$", RegexOptions.CultureInvariant)

    let private floatPattern =
        Regex(@"^[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?$", RegexOptions.CultureInvariant)

    let private datePattern =
        Regex(@"^\d{4}-\d{2}-\d{2}([Tt ]\d{2}:\d{2}(:\d{2}(\.\d{1,7})?)?)?$", RegexOptions.CultureInvariant)

    let private dateFormats = [|
        "yyyy-MM-dd"
        "yyyy-MM-dd'T'HH:mm"
        "yyyy-MM-dd'T'HH:mm:ss"
        "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF"
    |]

    /// An ISO date or date-time (`T`, `t` or a space between them), if the text is one.
    let tryDate (s: string) : DateTime option =
        if datePattern.IsMatch s then
            let normal = s.Replace(' ', 'T').Replace('t', 'T')

            match DateTime.TryParseExact(normal, dateFormats, inv, DateTimeStyles.None) with
            | true, d -> Some d
            | _ -> None
        else
            None

    /// What an unquoted YAML scalar means (the YAML 1.2 core schema, plus ISO dates): `null` / `~` /
    /// nothing, `true` / `false`, a whole number, a decimal number, a date; anything else is text.
    let ofPlain (s: string) : SuiteValue =
        match s with
        | ""
        | "~"
        | "null"
        | "Null"
        | "NULL" -> SuiteValue.Null
        | "true"
        | "True"
        | "TRUE" -> SuiteValue.Bool true
        | "false"
        | "False"
        | "FALSE" -> SuiteValue.Bool false
        | _ when intPattern.IsMatch s ->
            match Int64.TryParse(s, NumberStyles.AllowLeadingSign, inv) with
            | true, i -> SuiteValue.Int i
            | _ -> SuiteValue.Float(Double.Parse(s, NumberStyles.Float, inv))
        | _ when floatPattern.IsMatch s -> SuiteValue.Float(Double.Parse(s, NumberStyles.Float, inv))
        | _ ->
            match tryDate s with
            | Some d -> SuiteValue.Date d
            | None -> SuiteValue.Text s

/// A problem in a suite file. Lines and columns are 1-based; 0 when the problem is the file itself.
type SuiteIssue = {
    File: string
    Line: int
    Column: int
    Message: string
}

module SuiteIssue =
    /// "path:line:column: message" (just "path: message" for a problem with the file itself).
    let format (i: SuiteIssue) =
        if i.Line > 0 then
            $"{i.File}:{i.Line}:{i.Column}: {i.Message}"
        else
            $"{i.File}: {i.Message}"

/// One value of a filter: a literal, or a DAX scalar expression (`{ dax: "DATE ( 2024, 1, 31 )" }`).
[<RequireQualifiedAccess>]
type FilterValue =
    | Literal of SuiteValue
    | Dax of string

/// A filter of a test's filter context: a column restricted to these values.
type TestFilter = {
    /// The column as DAX: `'Date'[Year]`.
    Column: string
    Values: FilterValue list
}

/// Rows for one table, given in the suite instead of a fixture file.
type InlineTable = {
    Table: string
    /// The rows as a JSON array of row objects: the fixture loader's JSON format.
    Json: string
    Line: int
}

/// Limits on a number. All that are set must hold.
type Bounds = {
    Gt: double option
    Gte: double option
    Lt: double option
    Lte: double option
} with

    member b.IsEmpty = b.Gt.IsNone && b.Gte.IsNone && b.Lt.IsNone && b.Lte.IsNone

/// How close two numbers must be: within `Relative` (a fraction: 0.01 is 1%) or within `Absolute`.
/// With neither set, numbers compare as `QueryResult.compare` does by default.
type SuiteTolerance = {
    Relative: float option
    Absolute: float option
} with

    member t.IsSet = t.Relative.IsSome || t.Absolute.IsSome

/// What one column of a one-row result must be.
type ValueExpect = {
    /// The exact value (within the tolerance, for numbers), when one is given.
    Value: SuiteValue option
    Bounds: Bounds
    Tolerance: SuiteTolerance
    Line: int
}

[<RequireQualifiedAccess>]
type RowsExpect =
    | Exactly of int
    | Within of Bounds

/// The error a test expects.
type ErrorExpect = {
    /// The error's code (`dax-error`, `dax-invalid`, `dax-syntax`, `not-supported`, `query-failed`).
    Code: string option
    /// Text the error's message must contain (ignoring case).
    Message: string option
}

[<RequireQualifiedAccess>]
type TableExpect =
    /// Rows given in the suite: the columns named, and each row's values in that order.
    | Rows of columns: string list * rows: SuiteValue list list
    /// A CSV or JSON file (full path).
    | File of path: string

type Expect = {
    /// Column name to what it must be; needs a result of exactly one row.
    Values: (string * ValueExpect) list
    Rows: RowsExpect option
    Empty: bool option
    /// Columns the result must have.
    Columns: string list option
    Error: ErrorExpect option
    Table: TableExpect option
    /// Whether row order matters for `Table` and `Snapshot`. None: only if the query has an ORDER BY.
    Ordered: bool option
    /// The expected result is the one recorded by `tess test snapshot`.
    Snapshot: bool
}

/// What a test evaluates.
[<RequireQualifiedAccess>]
type TestBody =
    /// A full DAX query (one EVALUATE).
    | Query of dax: string
    /// A scalar expression (or `[Measure]`), evaluated as the one column `Value`.
    | Scalar of dax: string
    /// A scalar expression that must be TRUE.
    | Assert of dax: string

type TestCase = {
    /// Unique in its suite (ignoring case). A matrix test's name carries its values.
    Name: string
    Description: string option
    Tags: string list
    Line: int
    /// The test's own fixture directory (full path), overriding the suite's.
    Data: string option
    /// Inline rows: the suite's and the test's own, the test's winning for the same table.
    Tables: InlineTable list
    Filters: TestFilter list
    Body: TestBody
    Expect: Expect
    /// For expected tables and snapshots (column expectations carry their own).
    Tolerance: SuiteTolerance
    /// Some reason: the test isn't run (`pending: true`, `skip: <why>`).
    Skip: string option
    /// The matrix values this test was expanded with.
    Matrix: (string * SuiteValue) list
}

type TestSuiteFile = {
    /// Full path of the suite file.
    File: string
    Name: string
    Description: string option
    /// The suite's fixture directory (full path).
    Data: string option
    Tests: TestCase list
    /// Things read and set aside (te's `connection:`), for whoever wants to say so.
    Notes: SuiteIssue list
}

module TestSuite =

    /// The names suite files have, for discovery: `*.test.yaml`, `*.test.yml`, `*.test.json`.
    let extensions = [ ".test.yaml"; ".test.yml"; ".test.json" ]

    let isSuiteFile (path: string) =
        extensions
        |> List.exists (fun e -> path.EndsWith(e, StringComparison.OrdinalIgnoreCase))

    /// A suite file's path without `.test.yaml` (or the other extensions).
    let stem (path: string) =
        match
            extensions
            |> List.tryFind (fun e -> path.EndsWith(e, StringComparison.OrdinalIgnoreCase))
        with
        | Some e -> path.Substring(0, path.Length - e.Length)
        | None -> Path.ChangeExtension(path, null)

    /// The suite files under these paths (a file is itself; a directory is searched recursively),
    /// in ordinal order of their full paths, without duplicates. `Error` is a path that doesn't exist.
    let discover (paths: string list) : Result<string list, string> =
        let missing =
            paths |> List.tryFind (fun p -> not (File.Exists p) && not (Directory.Exists p))

        match missing with
        | Some p -> Error(Path.GetFullPath p)
        | None ->
            paths
            |> List.collect (fun p ->
                let full = Path.GetFullPath p

                if File.Exists full then
                    [ full ]
                else
                    Directory.EnumerateFiles(full, "*", SearchOption.AllDirectories)
                    |> Seq.filter isSuiteFile
                    |> List.ofSeq)
            |> List.distinct
            |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))
            |> Ok

    // ---- The document tree: YAML and JSON read into one shape, every node with its position -------

    type private Node = {
        Kind: NodeKind
        Line: int
        Column: int
    }

    and private NodeKind =
        /// `raw` is the text as written (without quotes); `plain` whether it was unquoted.
        | Scalar of value: SuiteValue * raw: string * plain: bool
        | Sequence of Node list
        | Mapping of (string * Node * Node) list

    exception private Problem of line: int * column: int * message: string

    let private fail (n: Node) (message: string) : 'T =
        raise (Problem(n.Line, n.Column, message))

    let rec private ofYaml (n: YamlNode) : Node =
        let line = int n.Start.Line
        let column = int n.Start.Column

        let at kind = {
            Kind = kind
            Line = line
            Column = column
        }

        match n with
        | :? YamlScalarNode as s ->
            let raw = if isNull s.Value then "" else s.Value
            let plain = s.Style = ScalarStyle.Plain

            at (
                Scalar(
                    (if plain then
                         SuiteValue.ofPlain raw
                     else
                         SuiteValue.Text raw),
                    raw,
                    plain
                )
            )
        | :? YamlSequenceNode as s -> at (Sequence [ for c in s.Children -> ofYaml c ])
        | :? YamlMappingNode as m ->
            at (
                Mapping [
                    for kv in m.Children do
                        let key = ofYaml kv.Key

                        match key.Kind with
                        | Scalar(_, raw, _) -> raw, key, ofYaml kv.Value
                        | _ -> fail key "a key must be text"
                ]
            )
        | _ -> raise (Problem(line, column, "this kind of YAML node is not supported"))

    let private readYaml (text: string) : Node =
        let stream = YamlStream()

        try
            stream.Load(new StringReader(text))
        with :? YamlException as e ->
            // "(Line: 3, Col: 5, Idx: 20) - (Line: ...): message": the position is given apart.
            let message =
                match e.InnerException with
                | null ->
                    Regex.Replace(
                        e.Message,
                        @"^\(Line: \d+, Col: \d+, Idx: \d+\) - \(Line: \d+, Col: \d+, Idx: \d+\): ",
                        ""
                    )
                | inner -> inner.Message

            raise (Problem(int e.Start.Line, int e.Start.Column, $"not valid YAML: {message}"))

        match stream.Documents.Count with
        | 0 -> raise (Problem(1, 1, "the file is empty"))
        | 1 -> ofYaml stream.Documents.[0].RootNode
        | _ -> raise (Problem(1, 1, "the file holds more than one YAML document (`---`); a suite is one"))

    type private Frame = {
        IsMap: bool
        Line: int
        Column: int
        Items: ResizeArray<Node>
        Pairs: ResizeArray<string * Node * Node>
        mutable Key: (string * Node) option
    }

    let private readJson (text: string) : Node =
        let bytes = Encoding.UTF8.GetBytes(text.TrimStart('﻿'))
        let lineStarts = ResizeArray<int>([ 0 ])

        for i in 0 .. bytes.Length - 1 do
            if bytes.[i] = 10uy then
                lineStarts.Add(i + 1)

        let position (offset: int64) =
            let offset = int offset
            let mutable lo = 0
            let mutable hi = lineStarts.Count - 1

            while lo < hi do
                let mid = (lo + hi + 1) / 2

                if lineStarts.[mid] <= offset then
                    lo <- mid
                else
                    hi <- mid - 1

            lo + 1, offset - lineStarts.[lo] + 1

        let stack = Stack<Frame>()
        let mutable root: Node option = None

        let add (node: Node) =
            if stack.Count = 0 then
                root <- Some node
            else
                let frame = stack.Peek()

                if frame.IsMap then
                    match frame.Key with
                    | Some(key, keyNode) ->
                        if frame.Pairs.Exists(fun (k, _, _) -> k = key) then
                            fail keyNode $"the key \"{key}\" is there twice"

                        frame.Pairs.Add(key, keyNode, node)
                        frame.Key <- None
                    | None -> ()
                else
                    frame.Items.Add node

        let options =
            JsonReaderOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

        let mutable reader = Utf8JsonReader(ReadOnlySpan<byte>(bytes), options)

        try
            while reader.Read() do
                let line, column = position reader.TokenStartIndex

                let scalar (value: SuiteValue) (raw: string) (plain: bool) = {
                    Kind = Scalar(value, raw, plain)
                    Line = line
                    Column = column
                }

                let push (isMap: bool) =
                    stack.Push {
                        IsMap = isMap
                        Line = line
                        Column = column
                        Items = ResizeArray()
                        Pairs = ResizeArray()
                        Key = None
                    }

                let pop () =
                    let frame = stack.Pop()

                    add {
                        Kind =
                            (if frame.IsMap then
                                 Mapping(List.ofSeq frame.Pairs)
                             else
                                 Sequence(List.ofSeq frame.Items))
                        Line = frame.Line
                        Column = frame.Column
                    }

                match reader.TokenType with
                | JsonTokenType.StartObject -> push true
                | JsonTokenType.StartArray -> push false
                | JsonTokenType.EndObject
                | JsonTokenType.EndArray -> pop ()
                | JsonTokenType.PropertyName ->
                    let key = reader.GetString()
                    stack.Peek().Key <- Some(key, scalar (SuiteValue.Text key) key false)
                | JsonTokenType.String ->
                    let s = reader.GetString()
                    add (scalar (SuiteValue.Text s) s false)
                | JsonTokenType.Number ->
                    let raw = Encoding.UTF8.GetString(reader.ValueSpan.ToArray())
                    add (scalar (SuiteValue.ofPlain raw) raw true)
                | JsonTokenType.True -> add (scalar (SuiteValue.Bool true) "true" true)
                | JsonTokenType.False -> add (scalar (SuiteValue.Bool false) "false" true)
                | JsonTokenType.Null -> add (scalar SuiteValue.Null "null" true)
                | _ -> ()
        with :? JsonException as e ->
            let line =
                if e.LineNumber.HasValue then
                    int e.LineNumber.Value + 1
                else
                    1

            let column =
                if e.BytePositionInLine.HasValue then
                    int e.BytePositionInLine.Value + 1
                else
                    1

            // "... LineNumber: 3 | BytePositionInLine: 7." repeats the position.
            let message =
                Regex.Replace(e.Message, @"\s*LineNumber: \d+ \| BytePositionInLine: \d+\.?$", "")

            raise (Problem(line, column, $"not valid JSON: {message}"))

        match root with
        | Some node -> node
        | None -> raise (Problem(1, 1, "the file is empty"))

    // ---- Reading the tree -------------------------------------------------------------------------

    let private describe (n: Node) =
        match n.Kind with
        | Scalar(SuiteValue.Null, _, true) -> "nothing"
        | Scalar _ -> "a single value"
        | Sequence _ -> "a list"
        | Mapping _ -> "a mapping"

    let private asMap (what: string) (n: Node) =
        match n.Kind with
        | Mapping pairs -> pairs
        | _ -> fail n $"{what} must be a mapping (key: value), not {describe n}"

    let private asList (what: string) (n: Node) =
        match n.Kind with
        | Sequence items -> items
        | _ -> fail n $"{what} must be a list, not {describe n}"

    /// A scalar as the text it was written as. Nothing (or `null`) is not text.
    let private asText (what: string) (n: Node) =
        match n.Kind with
        | Scalar(SuiteValue.Null, _, true) -> fail n $"{what} is empty"
        | Scalar(_, raw, _) -> raw
        | _ -> fail n $"{what} must be text, not {describe n}"

    let private asValue (what: string) (n: Node) =
        match n.Kind with
        | Scalar(value, _, _) -> value
        | _ -> fail n $"{what} must be a single value, not {describe n}"

    let private asBool (what: string) (n: Node) =
        match n.Kind with
        | Scalar(SuiteValue.Bool b, _, _) -> b
        | _ -> fail n $"{what} must be true or false"

    let private asNumber (what: string) (n: Node) =
        match n.Kind with
        | Scalar(SuiteValue.Int i, _, _) -> double i
        | Scalar(SuiteValue.Float f, _, _) when Double.IsFinite f -> f
        | _ -> fail n $"{what} must be a number"

    let private asCount (what: string) (n: Node) =
        match n.Kind with
        | Scalar(SuiteValue.Int i, _, _) when i >= 0L && i <= int64 Int32.MaxValue -> int i
        | _ -> fail n $"{what} must be a whole number, 0 or more"

    let private tryKey (key: string) (pairs: (string * Node * Node) list) =
        pairs |> List.tryPick (fun (k, _, v) -> if k = key then Some v else None)

    /// How many single-character edits (insert, delete, replace, swap of neighbours) turn `a` into `b`.
    let private editDistance (a: string) (b: string) : int =
        let d = Array2D.zeroCreate<int> (a.Length + 1) (b.Length + 1)

        for i in 0 .. a.Length do
            d.[i, 0] <- i

        for j in 0 .. b.Length do
            d.[0, j] <- j

        for i in 1 .. a.Length do
            for j in 1 .. b.Length do
                let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
                d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)

                if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1] then
                    d.[i, j] <- min d.[i, j] (d.[i - 2, j - 2] + 1)

        d.[a.Length, b.Length]

    let private checkKeys (what: string) (known: string list) (pairs: (string * Node * Node) list) =
        let plain (s: string) =
            s.Replace("_", "").Replace("-", "").ToLowerInvariant()

        for (key, keyNode, _) in pairs do
            if not (List.contains key known) then
                // A misspelt key (`expcet`, `queryFile`) is the likeliest mistake: name the one meant.
                let close =
                    known
                    |> List.map (fun k -> k, editDistance (plain k) (plain key))
                    |> List.filter (fun (k, distance) -> distance <= (if k.Length <= 4 then 1 else 2))
                    |> List.sortBy snd
                    |> List.tryHead

                let hint =
                    match close with
                    | Some(k, _) -> $" (did you mean '{k}'?)"
                    | None -> ""

                let list = String.concat ", " known
                fail keyNode $"unknown key '{key}' in {what}{hint}. Known keys: {list}"

    // ---- Matrix expansion -------------------------------------------------------------------------

    /// Replace `{name}` placeholders in every value under a node. A value that is exactly one
    /// placeholder becomes the variable's value, type included; inside a longer text the
    /// placeholder becomes the value's text.
    let rec private substitute (vars: (string * Node) list) (n: Node) : Node =
        match n.Kind with
        | Scalar(_, raw, plain) ->
            match vars |> List.tryFind (fun (name, _) -> raw = "{" + name + "}") with
            | Some(_, v) -> {
                v with
                    Line = n.Line
                    Column = n.Column
              }
            | None ->
                let replaced =
                    vars
                    |> List.fold
                        (fun (s: string) (name, v) ->
                            match v.Kind with
                            | Scalar(value, _, _) -> s.Replace("{" + name + "}", SuiteValue.text value)
                            | _ -> s)
                        raw

                if replaced = raw then
                    n
                else
                    let value =
                        if plain then
                            SuiteValue.ofPlain replaced
                        else
                            SuiteValue.Text replaced

                    {
                        n with
                            Kind = Scalar(value, replaced, plain)
                    }
        | Sequence items -> {
            n with
                Kind = Sequence(items |> List.map (substitute vars))
          }
        | Mapping pairs -> {
            n with
                Kind = Mapping(pairs |> List.map (fun (k, kn, v) -> k, kn, substitute vars v))
          }

    let private matrixName =
        Regex(@"^[A-Za-z_][A-Za-z0-9_]*$", RegexOptions.CultureInvariant)

    /// The combinations a `matrix:` stands for: a mapping of name to values is their cross product
    /// (the last name varying fastest); a list of mappings is those combinations.
    let private combinations (n: Node) : (string * Node) list list =
        let variable (key: string) (keyNode: Node) (value: Node) =
            if not (matrixName.IsMatch key) then
                fail keyNode $"a matrix name must be letters, digits and _ ('{key}' is not)"

            match value.Kind with
            | Scalar _ -> key, value
            | _ -> fail value $"a matrix value must be a single value, not {describe value}"

        match n.Kind with
        | Mapping pairs ->
            if pairs.IsEmpty then
                fail n "matrix is empty"

            let axes =
                pairs
                |> List.map (fun (key, keyNode, values) ->
                    match values.Kind with
                    | Sequence [] -> fail values $"matrix '{key}' has no values"
                    | Sequence items -> items |> List.map (variable key keyNode)
                    | Scalar _ -> [ variable key keyNode values ]
                    | Mapping _ -> fail values $"matrix '{key}' must be a list of values")

            List.foldBack
                (fun (axis: (string * Node) list) (rest: (string * Node) list list) -> [
                    for v in axis do
                        for r in rest -> v :: r
                ])
                axes
                [ [] ]
        | Sequence items ->
            if items.IsEmpty then
                fail n "matrix is empty"

            items
            |> List.map (fun item ->
                match asMap "a matrix entry" item with
                | [] -> fail item "a matrix entry needs at least one name"
                | pairs -> pairs |> List.map (fun (key, keyNode, value) -> variable key keyNode value))
        | _ -> fail n "matrix must be a mapping of name to values, or a list of mappings"

    // ---- Tests ------------------------------------------------------------------------------------

    type private Defaults = {
        Tolerance: SuiteTolerance
        Tags: string list
        Filters: TestFilter list
        Tables: InlineTable list
        Directory: string
    }

    let private noBounds = {
        Gt = None
        Gte = None
        Lt = None
        Lte = None
    }

    let private boundKeys = [ "gt"; "gte"; "lt"; "lte" ]

    let private bounds (what: string) (pairs: (string * Node * Node) list) : Bounds =
        let get key =
            tryKey key pairs |> Option.map (asNumber $"{what}.{key}")

        {
            Gt = get "gt"
            Gte = get "gte"
            Lt = get "lt"
            Lte = get "lte"
        }

    let private tolerance (inherited: SuiteTolerance) (pairs: (string * Node * Node) list) : SuiteTolerance =
        let get key =
            tryKey key pairs
            |> Option.map (fun n ->
                let v = asNumber key n

                if v < 0.0 then
                    fail n $"{key} must be 0 or more"

                v)

        match get "tolerance", get "tolerance_abs" with
        | None, None -> inherited
        // A test that sets either replaces both: the other one is not inherited.
        | relative, absolute -> {
            Relative = relative
            Absolute = absolute
          }

    let private tags (n: Node) : string list =
        match n.Kind with
        | Sequence items -> items |> List.map (asText "a tag")
        | Scalar _ -> [ asText "tags" n ]
        | Mapping _ -> fail n "tags must be a list of names"

    let private filterValue (n: Node) : FilterValue =
        match n.Kind with
        | Scalar(value, _, _) -> FilterValue.Literal value
        | Mapping([ ("dax", _, dax) ] as pairs) ->
            checkKeys "a filter value" [ "dax" ] pairs
            let text = asText "dax" dax

            match DaxParser.parseExpression text with
            | Ok _ -> FilterValue.Dax text
            | Error(e :: _) -> fail dax $"not a DAX expression: {e.Message}"
            | Error [] -> fail dax "not a DAX expression"
        | _ -> fail n "a filter value must be a single value, or { dax: \"<expression>\" }"

    let private filters (n: Node) : TestFilter list =
        asMap "filters" n
        |> List.map (fun (key, keyNode, values) ->
            match DaxParser.parseExpression key with
            | Ok { Kind = DaxAst.ExprKind.Qualified _ } -> ()
            | _ ->
                fail
                    keyNode
                    $"a filter is on a column, written Table[Column] or 'My Table'[Column] ('{key}' is not one)"

            let values =
                match values.Kind with
                | Sequence [] -> fail values $"the filter on {key} has no values"
                | Sequence items -> items |> List.map filterValue
                | _ -> [ filterValue values ]

            { Column = key.Trim(); Values = values })

    let private inlineTables (n: Node) : InlineTable list =
        asMap "tables" n
        |> List.map (fun (table, keyNode, rows) ->
            let rows =
                asList $"the rows of table '{table}'" rows
                |> List.map (fun row ->
                    let fields =
                        asMap "a row" row
                        |> List.map (fun (column, _, value) ->
                            JsonSerializer.Serialize column
                            + ": "
                            + SuiteValue.json (asValue $"the value of '{column}'" value))

                    "{ " + String.concat ", " fields + " }")

            {
                Table = table
                Json = "[\n" + String.concat ",\n" rows + "\n]\n"
                Line = keyNode.Line
            })

    /// Later tables win over earlier ones of the same name (ignoring case).
    let private mergeTables (suite: InlineTable list) (test: InlineTable list) =
        let same (a: InlineTable) (b: InlineTable) =
            String.Equals(a.Table, b.Table, StringComparison.OrdinalIgnoreCase)

        (suite |> List.filter (fun s -> not (test |> List.exists (same s)))) @ test

    let private mergeFilters (defaults: TestFilter list) (test: TestFilter list) =
        let same (a: TestFilter) (b: TestFilter) =
            String.Equals(a.Column, b.Column, StringComparison.OrdinalIgnoreCase)

        (defaults |> List.filter (fun d -> not (test |> List.exists (same d)))) @ test

    let private valueKeys = [ "value"; "tolerance"; "tolerance_abs" ] @ boundKeys

    /// One column's expectation: a value, or a mapping of value / bounds / tolerance.
    let private valueExpect (what: string) (inherited: SuiteTolerance) (n: Node) : ValueExpect =
        match n.Kind with
        | Scalar(value, raw, plain) ->
            if plain && raw = "" then
                fail n $"{what} has no expected value (write null to expect BLANK)"

            {
                Value = Some value
                Bounds = noBounds
                Tolerance = inherited
                Line = n.Line
            }
        | Mapping pairs ->
            checkKeys what valueKeys pairs

            let expected = {
                Value = tryKey "value" pairs |> Option.map (asValue $"{what}.value")
                Bounds = bounds what pairs
                Tolerance = tolerance inherited pairs
                Line = n.Line
            }

            if expected.Value.IsNone && expected.Bounds.IsEmpty then
                fail n $"{what} needs a value, or one of gt, gte, lt, lte"

            expected
        | Sequence _ -> fail n $"{what} must be a value or a mapping (value, gt, gte, lt, lte, tolerance), not a list"

    let private errorExpect (n: Node) : ErrorExpect option =
        match n.Kind with
        | Scalar(SuiteValue.Bool true, _, _) -> Some { Code = None; Message = None }
        | Scalar(SuiteValue.Bool false, _, _) -> None
        | Scalar(SuiteValue.Text code, _, _) when code.Trim() <> "" ->
            Some {
                Code = Some(code.Trim())
                Message = None
            }
        | Mapping pairs ->
            checkKeys "error" [ "code"; "message" ] pairs

            Some {
                Code = tryKey "code" pairs |> Option.map (asText "error.code")
                Message = tryKey "message" pairs |> Option.map (asText "error.message")
            }
        | _ -> fail n "error must be true, an error code, or a mapping (code, message)"

    let private noExpect = {
        Values = []
        Rows = None
        Empty = None
        Columns = None
        Error = None
        Table = None
        Ordered = None
        Snapshot = false
    }

    let private tableRows (n: Node) : TableExpect =
        let rows = asList "table" n |> List.map (asMap "a row of the expected table")

        let columns =
            rows
            |> List.collect (fun row -> row |> List.map (fun (k, _, _) -> k))
            |> List.distinct

        TableExpect.Rows(
            columns,
            rows
            |> List.map (fun row ->
                columns
                |> List.map (fun c ->
                    match tryKey c row with
                    | Some v -> asValue $"the value of '{c}'" v
                    | None -> SuiteValue.Null))
        )

    let private tableFile (directory: string) (n: Node) : TableExpect =
        let path = Path.GetFullPath(Path.Combine(directory, asText "file" n))
        let extension = Path.GetExtension(path).ToLowerInvariant()

        if extension <> ".csv" && extension <> ".json" then
            fail n "the expected table must be a .csv or .json file"
        elif not (File.Exists path) then
            fail n $"the file {path} does not exist"
        else
            TableExpect.File path

    let private queryExpectKeys = [ "rows"; "empty"; "columns"; "error"; "table"; "file"; "ordered"; "snapshot" ]

    /// `expect` of a query test: the reserved keys, and every other key a column of the result.
    let private queryExpect (d: Defaults) (inherited: SuiteTolerance) (n: Node) : Expect =
        let pairs = asMap "expect" n
        let reserved key = tryKey key pairs

        let rows =
            reserved "rows"
            |> Option.map (fun r ->
                match r.Kind with
                | Mapping b ->
                    checkKeys "rows" boundKeys b
                    let limits = bounds "rows" b

                    if limits.IsEmpty then
                        fail r "rows needs one of gt, gte, lt, lte"

                    RowsExpect.Within limits
                | _ -> RowsExpect.Exactly(asCount "rows" r))

        let expect = {
            Values =
                pairs
                |> List.filter (fun (k, _, _) -> not (List.contains k queryExpectKeys))
                |> List.map (fun (k, _, v) -> k, valueExpect $"the expectation for column '{k}'" inherited v)
            Rows = rows
            Empty = reserved "empty" |> Option.map (asBool "empty")
            Columns =
                reserved "columns"
                |> Option.map (fun c -> asList "columns" c |> List.map (asText "a column name"))
            Error = reserved "error" |> Option.bind errorExpect
            Table =
                match reserved "table", reserved "file" with
                | Some _, Some f -> fail f "give the expected table as `table` or as `file`, not both"
                | Some t, None -> Some(tableRows t)
                | None, Some f -> Some(tableFile d.Directory f)
                | None, None -> None
            Ordered = reserved "ordered" |> Option.map (asBool "ordered")
            Snapshot =
                reserved "snapshot"
                |> Option.map (asBool "snapshot")
                |> Option.defaultValue false
        }

        let others =
            not expect.Values.IsEmpty
            || expect.Rows.IsSome
            || expect.Empty.IsSome
            || expect.Columns.IsSome
            || expect.Table.IsSome
            || expect.Snapshot

        if expect.Error.IsSome && others then
            fail n "a test that expects an error can't expect anything else"
        elif expect.Table.IsSome && expect.Snapshot then
            fail n "give the expected table (`table` or `file`) or `snapshot: true`, not both"
        elif expect.Error.IsNone && not others then
            fail n "expect has no assertion"
        else
            expect

    let private scalarExpectKeys = valueKeys @ [ "error"; "snapshot" ]

    /// `expect` of a `measure` / `expression` test: the value itself, or a mapping.
    let private scalarExpect (inherited: SuiteTolerance) (n: Node) : Expect =
        match n.Kind with
        | Mapping pairs ->
            checkKeys "expect" scalarExpectKeys pairs
            let error = tryKey "error" pairs |> Option.bind errorExpect

            let snapshot =
                tryKey "snapshot" pairs
                |> Option.map (asBool "snapshot")
                |> Option.defaultValue false

            let hasValue =
                pairs |> List.exists (fun (k, _, _) -> k = "value" || List.contains k boundKeys)

            if error.IsSome && (hasValue || snapshot) then
                fail n "a test that expects an error can't expect anything else"
            elif snapshot && hasValue then
                fail n "give the expected value or `snapshot: true`, not both"
            elif error.IsSome then
                { noExpect with Error = error }
            elif snapshot then
                { noExpect with Snapshot = true }
            else
                // `snapshot: false` and `error: false` say nothing: what is left is the value.
                let value = {
                    n with
                        Kind = Mapping(pairs |> List.filter (fun (k, _, _) -> k <> "error" && k <> "snapshot"))
                }

                {
                    noExpect with
                        Values = [ "Value", valueExpect "expect" inherited value ]
                }
        | _ -> {
            noExpect with
                Values = [ "Value", valueExpect "expect" inherited n ]
          }

    let private testKeys = [
        "name"
        "description"
        "tags"
        "data"
        "tables"
        "filters"
        "matrix"
        "query"
        "query_file"
        "measure"
        "expression"
        "assert"
        "expect"
        "tolerance"
        "tolerance_abs"
        "pending"
        "skip"
        "compare"
    ]

    /// A measure's name as a reference to it: `Total` is `[Total]`, `Margin [%]` is `[Margin [%]]]`.
    let private measureReference (name: string) = "[" + name.Replace("]", "]]") + "]"

    let private parseTest (d: Defaults) (matrix: (string * SuiteValue) list) (n: Node) : TestCase =
        let pairs = asMap "a test" n
        checkKeys "a test" testKeys pairs
        let get key = tryKey key pairs

        let name =
            match get "name" with
            | Some v -> asText "name" v
            | None -> fail n "a test needs a name"

        let body =
            let given =
                [ "query"; "query_file"; "measure"; "expression"; "assert" ]
                |> List.choose (fun k -> get k |> Option.map (fun v -> k, v))
                |> List.sortBy (fun (_, v: Node) -> v.Line, v.Column)

            match given with
            | [] -> fail n $"test '{name}' needs one of query, query_file, measure, expression, assert"
            | [ "query", v ] -> TestBody.Query(asText "query" v)
            | [ "query_file", v ] ->
                let path = Path.GetFullPath(Path.Combine(d.Directory, asText "query_file" v))

                try
                    // Matrix placeholders work in a query file as in an inline query.
                    let text = File.ReadAllText path

                    let text =
                        matrix
                        |> List.fold
                            (fun (s: string) (k, value) -> s.Replace("{" + k + "}", SuiteValue.text value))
                            text

                    TestBody.Query text
                with
                | :? FileNotFoundException
                | :? DirectoryNotFoundException -> fail v $"the file {path} does not exist"
                | :? IOException
                | :? UnauthorizedAccessException as e -> fail v $"can't read {path}: {e.Message}"
            | [ "measure", v ] -> TestBody.Scalar(measureReference (asText "measure" v))
            | [ "expression", v ] -> TestBody.Scalar(asText "expression" v)
            | [ "assert", v ] -> TestBody.Assert(asText "assert" v)
            | (_, _) :: (second, v) :: _ ->
                fail v $"test '{name}' has more than one of query, query_file, measure, expression, assert ('{second}')"
            | _ -> fail n "a test needs one of query, query_file, measure, expression, assert"

        let skip =
            match get "skip", get "pending" with
            | Some s, _ ->
                match s.Kind with
                | Scalar(SuiteValue.Bool true, _, _) -> Some "skipped"
                | Scalar(SuiteValue.Bool false, _, _) -> None
                | _ -> Some(asText "skip" s)
            | None, Some p -> if asBool "pending" p then Some "pending" else None
            | None, None -> None

        let testTolerance = tolerance d.Tolerance pairs

        let expect =
            match body, get "expect" with
            | TestBody.Assert _, Some e -> fail e "an `assert` test has no `expect`: the expression must be TRUE"
            | TestBody.Assert _, None -> noExpect
            // A test still to be written needs no expectation yet.
            | _, None when skip.IsSome -> noExpect
            | _, None -> fail n $"test '{name}' has no `expect` (mark it `pending: true` if it is still to be written)"
            | TestBody.Query _, Some e -> queryExpect d testTolerance e
            | TestBody.Scalar _, Some e -> scalarExpect testTolerance e

        {
            Name = name
            Description = get "description" |> Option.map (asText "description")
            Tags =
                d.Tags @ (get "tags" |> Option.map tags |> Option.defaultValue [])
                |> List.distinct
            Line = n.Line
            Data =
                get "data"
                |> Option.map (fun v -> Path.GetFullPath(Path.Combine(d.Directory, asText "data" v)))
            Tables = mergeTables d.Tables (get "tables" |> Option.map inlineTables |> Option.defaultValue [])
            Filters = mergeFilters d.Filters (get "filters" |> Option.map filters |> Option.defaultValue [])
            Body = body
            Expect = expect
            Tolerance = testTolerance
            Skip = skip
            Matrix = matrix
        }

    /// A test node as the tests it stands for: itself, or one per matrix combination.
    let private expandTest (d: Defaults) (n: Node) : TestCase list =
        let pairs = asMap "a test" n

        match tryKey "matrix" pairs with
        | None -> [ parseTest d [] n ]
        | Some matrix ->
            let withoutMatrix = {
                n with
                    Kind = Mapping(pairs |> List.filter (fun (k, _, _) -> k <> "matrix"))
            }

            combinations matrix
            |> List.map (fun vars ->
                let values =
                    vars
                    |> List.map (fun (name, node) ->
                        name,
                        match node.Kind with
                        | Scalar(value, _, _) -> value
                        | _ -> SuiteValue.Null)

                let test = parseTest d values (substitute vars withoutMatrix)

                let named =
                    match tryKey "name" pairs with
                    | Some { Kind = Scalar(_, raw, _) } ->
                        vars |> List.exists (fun (name, _) -> raw.Contains("{" + name + "}"))
                    | _ -> false

                if named then
                    test
                else
                    let suffix =
                        values
                        |> List.map (fun (name, value) -> $"{name}={SuiteValue.text value}")
                        |> String.concat ", "

                    {
                        test with
                            Name = $"{test.Name} [{suffix}]"
                    })

    let private suiteKeys = [ "suite"; "description"; "connection"; "data"; "tables"; "defaults"; "tests" ]

    let private defaultsKeys = [ "tolerance"; "tolerance_abs"; "tags"; "filters" ]

    /// Read a suite from its text. `file` says where relative paths start and whether the text is
    /// JSON (`.json`) or YAML. Every problem found is returned, each with its line.
    let parse (file: string) (text: string) : Result<TestSuiteFile, SuiteIssue list> =
        let file = Path.GetFullPath file
        let directory = Path.GetDirectoryName file
        let issues = ResizeArray<SuiteIssue>()
        let notes = ResizeArray<SuiteIssue>()

        let issue line column message = {
            File = file
            Line = line
            Column = column
            Message = message
        }

        let attempt (f: unit -> 'T) : 'T option =
            try
                Some(f ())
            with Problem(line, column, message) ->
                issues.Add(issue line column message)
                None

        let root =
            attempt (fun () ->
                let node =
                    if file.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
                        readJson text
                    else
                        readYaml text

                let pairs = asMap "a suite" node
                checkKeys "a suite" suiteKeys pairs
                node, pairs)

        match root with
        | None -> Error(List.ofSeq issues)
        | Some(node, pairs) ->
            let get key = tryKey key pairs

            let field key (read: Node -> 'T) : 'T option =
                get key |> Option.bind (fun v -> attempt (fun () -> read v))

            match
                pairs
                |> List.tryPick (fun (k, keyNode, _) -> if k = "connection" then Some keyNode else None)
            with
            | Some c ->
                notes.Add(
                    issue c.Line c.Column "`connection` is not used: the model is the one given on the command line"
                )
            | None -> ()

            let defaults =
                field "defaults" (fun v ->
                    let d = asMap "defaults" v
                    checkKeys "defaults" defaultsKeys d
                    d)
                |> Option.defaultValue []

            let d = {
                Tolerance =
                    attempt (fun () -> tolerance { Relative = None; Absolute = None } defaults)
                    |> Option.defaultValue { Relative = None; Absolute = None }
                Tags =
                    tryKey "tags" defaults
                    |> Option.bind (fun v -> attempt (fun () -> tags v))
                    |> Option.defaultValue []
                Filters =
                    tryKey "filters" defaults
                    |> Option.bind (fun v -> attempt (fun () -> filters v))
                    |> Option.defaultValue []
                Tables = field "tables" inlineTables |> Option.defaultValue []
                Directory = directory
            }

            let tests =
                match get "tests" with
                | None ->
                    issues.Add(issue node.Line node.Column "a suite needs `tests`")
                    []
                | Some t ->
                    match attempt (fun () -> asList "tests" t) with
                    | None -> []
                    | Some [] ->
                        issues.Add(issue t.Line t.Column "the suite has no tests")
                        []
                    | Some items ->
                        // One problem per test, so a suite's mistakes show together.
                        items
                        |> List.collect (fun item -> attempt (fun () -> expandTest d item) |> Option.defaultValue [])

            let seen = HashSet<string>(StringComparer.OrdinalIgnoreCase)

            for t in tests do
                if not (seen.Add t.Name) then
                    issues.Add(issue t.Line 1 $"there is more than one test named '{t.Name}'")

            let suite = {
                File = file
                Name =
                    field "suite" (asText "suite")
                    |> Option.defaultValue (Path.GetFileName(stem file))
                Description = field "description" (asText "description")
                Data = field "data" (fun v -> Path.GetFullPath(Path.Combine(directory, asText "data" v)))
                Tests = tests
                Notes = List.ofSeq notes
            }

            if issues.Count > 0 then
                Error(List.ofSeq issues)
            else
                Ok suite

    /// Read a suite file.
    let load (file: string) : Result<TestSuiteFile, SuiteIssue list> =
        let full = Path.GetFullPath file

        let problem (message: string) =
            Error [
                {
                    File = full
                    Line = 0
                    Column = 0
                    Message = message
                }
            ]

        try
            parse full (File.ReadAllText full)
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException -> problem "the file does not exist"
        | :? IOException
        | :? UnauthorizedAccessException as e -> problem $"can't read the file: {e.Message}"
