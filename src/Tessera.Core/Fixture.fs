namespace Tessera

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Microsoft.AnalysisServices.Tabular

// Fixture data for the local DAX engine: a directory with one `<Table>.csv` or `<Table>.json` per
// model table, read into a `DataStore` and checked against the model. Nothing here evaluates DAX,
// and nothing here leaves the machine: files are read from the directory and that is all.

/// The file name of a table: the table name, with the characters a file name can't hold on
/// Windows, macOS or Linux written as `%XX` (UTF-8 bytes in hex).
///
/// Escaped: `%` itself, `< > : " / \ | ? *`, control characters, a trailing dot or space, and the
/// first character of a Windows device name (`CON`, `PRN`, `AUX`, `NUL`, `COM1`.., `LPT1`..), so
/// table `CON` is file `%43ON.csv`. Everything else is kept, in Unicode NFC. Decoding accepts any
/// `%XX`, and files match tables ignoring case and Unicode normalization (macOS may hand back NFD).
module FixtureName =

    let private reserved =
        HashSet<string>(
            [ "CON"; "PRN"; "AUX"; "NUL" ]
            @ [ for d in "0123456789\u00B9\u00B2\u00B3" -> $"COM{d}" ]
            @ [ for d in "0123456789\u00B9\u00B2\u00B3" -> $"LPT{d}" ],
            StringComparer.OrdinalIgnoreCase
        )

    let private illegal = "%<>:\"/\\|?*"

    let private escape (sb: StringBuilder) (text: string) =
        for b in Encoding.UTF8.GetBytes text do
            sb.Append('%').Append(b.ToString("X2", CultureInfo.InvariantCulture)) |> ignore

    /// The file name (without extension) for a table name.
    let encode (tableName: string) : string =
        let name = tableName.Normalize NormalizationForm.FormC
        let sb = StringBuilder()

        // Windows treats "CON", "con.txt" and "CON .x" as the console device.
        let device =
            let stem = (name.Split('.').[0]).TrimEnd(' ')
            reserved.Contains stem

        let mutable i = 0

        while i < name.Length do
            let c = name.[i]
            let last = (i = name.Length - 1)

            if
                Char.IsHighSurrogate c
                && i + 1 < name.Length
                && Char.IsLowSurrogate name.[i + 1]
            then
                sb.Append(c).Append(name.[i + 1]) |> ignore
                i <- i + 1
            elif
                c < ' '
                || c = '\u007F'
                || illegal.Contains c
                || Char.IsSurrogate c
                || (last && (c = '.' || c = ' '))
                || (i = 0 && device)
            then
                escape sb (string c)
            else
                sb.Append c |> ignore

            i <- i + 1

        sb.ToString()

    /// The table name a file name (without extension) stands for; None if it has a `%` that isn't
    /// followed by two hex digits, or the escapes aren't UTF-8.
    let decode (stem: string) : string option =
        let bytes = ResizeArray<byte>()
        let sb = StringBuilder()
        let strict = UTF8Encoding(false, true)
        let mutable ok = true

        let flush () =
            if bytes.Count > 0 then
                try
                    sb.Append(strict.GetString(bytes.ToArray())) |> ignore
                with :? DecoderFallbackException ->
                    ok <- false

                bytes.Clear()

        let mutable i = 0

        while ok && i < stem.Length do
            if stem.[i] = '%' then
                if
                    i + 2 < stem.Length
                    && Uri.IsHexDigit stem.[i + 1]
                    && Uri.IsHexDigit stem.[i + 2]
                then
                    bytes.Add(Convert.ToByte(stem.Substring(i + 1, 2), 16))
                    i <- i + 3
                else
                    ok <- false
            else
                flush ()
                sb.Append stem.[i] |> ignore
                i <- i + 1

        flush ()

        if ok then
            Some(sb.ToString().Normalize NormalizationForm.FormC)
        else
            None

    /// Whether a file name (without extension) is the file of this table.
    let matches (tableName: string) (stem: string) =
        match decode stem with
        | Some name ->
            String.Equals(name, tableName.Normalize NormalizationForm.FormC, StringComparison.OrdinalIgnoreCase)
        | None -> false

/// A streaming RFC 4180 reader: comma-separated, `"` quotes, `""` for a quote inside quotes.
///
/// - Records end at LF, CRLF or a lone CR; a line break inside quotes is part of the field, kept as
///   written. The last record needs no line break.
/// - A leading byte-order mark is skipped.
/// - Nothing is trimmed. A quote that doesn't open a field is an ordinary character.
/// - An empty line is a record of one empty field (the caller decides what that means).
/// - Whether a field was quoted is kept, so `""` (empty text) can differ from nothing (BLANK).
[<Sealed>]
type CsvReader(reader: TextReader) =
    let block = Array.zeroCreate<char> 65536
    let mutable blockLength = 0
    let mutable blockPosition = 0
    let mutable atEnd = false
    let mutable first = true
    let mutable line = 1
    let mutable recordLine = 1
    let mutable record = Array.zeroCreate<char> 1024
    let mutable recordLength = 0
    let starts = ResizeArray<int>()
    let lengths = ResizeArray<int>()
    let quoted = ResizeArray<bool>()

    /// The next character without consuming it, or -1 at the end.
    let peek () : int =
        if blockPosition < blockLength then
            int block.[blockPosition]
        elif atEnd then
            -1
        else
            blockLength <- reader.Read(block, 0, block.Length)
            blockPosition <- 0

            if blockLength <= 0 then
                blockLength <- 0
                atEnd <- true
                -1
            else
                int block.[0]

    let advance () = blockPosition <- blockPosition + 1

    let append (c: char) =
        if recordLength = record.Length then
            Array.Resize(&record, record.Length * 2)

        record.[recordLength] <- c
        recordLength <- recordLength + 1

    /// The 1-based line the last record starts on.
    member _.RecordLine = recordLine

    member _.FieldCount = starts.Count

    /// A field of the last record. Valid until the next `Read`.
    member _.Field(i: int) : ReadOnlySpan<char> =
        ReadOnlySpan<char>(record, starts.[i], lengths.[i])

    member _.FieldString(i: int) : string = String(record, starts.[i], lengths.[i])

    /// Whether a field of the last record was written in quotes.
    member _.IsQuoted(i: int) : bool = quoted.[i]

    /// Whether the last record is an empty line.
    member _.IsBlankLine = starts.Count = 1 && lengths.[0] = 0 && not quoted.[0]

    /// Read the next record: Ok true, Ok false at the end of the input, or the line and reason the
    /// input stops being CSV (after which nothing more can be read).
    member _.Read() : Result<bool, int * string> =
        starts.Clear()
        lengths.Clear()
        quoted.Clear()
        recordLength <- 0

        if first then
            first <- false

            if peek () = 0xFEFF then
                advance ()

        if peek () = -1 then
            Ok false
        else
            recordLine <- line
            let mutable finished = false
            let mutable error = None

            while not finished && error.IsNone do
                let start = recordLength
                let isQuoted = (peek () = int '"')

                if isQuoted then
                    advance ()
                    let fieldLine = line
                    let mutable closed = false

                    while not closed && error.IsNone do
                        match peek () with
                        | -1 -> error <- Some(fieldLine, "a quoted field is never closed")
                        | 34 ->
                            advance ()

                            if peek () = 34 then
                                append '"'
                                advance ()
                            else
                                closed <- true
                        | 13 ->
                            append '\r'
                            advance ()

                            if peek () = 10 then
                                append '\n'
                                advance ()

                            line <- line + 1
                        | c ->
                            if c = 10 then
                                line <- line + 1

                            append (char c)
                            advance ()

                    if error.IsNone then
                        match peek () with
                        | -1
                        | 44
                        | 13
                        | 10 -> ()
                        | _ ->
                            error <-
                                Some(line, "text after a closing quote (write a quote inside a quoted field as \"\")")
                else
                    let mutable more = true

                    while more do
                        match peek () with
                        | -1
                        | 44
                        | 13
                        | 10 -> more <- false
                        | c ->
                            append (char c)
                            advance ()

                if error.IsNone then
                    starts.Add start
                    lengths.Add(recordLength - start)
                    quoted.Add isQuoted

                    match peek () with
                    | 44 -> advance ()
                    | 13 ->
                        advance ()

                        if peek () = 10 then
                            advance ()

                        line <- line + 1
                        finished <- true
                    | 10 ->
                        advance ()
                        line <- line + 1
                        finished <- true
                    | _ -> finished <- true

            match error with
            | Some e -> Error e
            | None -> Ok true

/// A problem with fixture data, or something worth knowing about it.
type FixtureIssue = {
    /// Stable kebab-case rule id; see `Fixture.rules`.
    Rule: string
    Severity: Validate.Severity
    /// The model object it is about: a table, a column, a relationship, or the model.
    Ref: ObjectRef
    Message: string
    /// The fixture file, where the issue is in one.
    File: string option
    /// The 1-based line in that file (the line a CSV record or a JSON row starts on).
    Line: int option
    /// The column of the file (a header name), for an issue with one value or one header.
    Field: string option
}

module Fixture =

    type private Sev = Validate.Severity

    /// Every rule id, with a one-line description.
    let rules = [
        "unknown-table", "a .csv or .json file whose name is no table of the model"
        "duplicate-file", "more than one file for a table (Sales.csv and Sales.json, or sales.csv and Sales.csv)"
        "calculated-table", "a file for a calculated table or calculation group, which the engine computes (ignored)"
        "missing-table", "tables with no file: they are empty"
        "malformed-file", "a file that can't be read as CSV or JSON, or has no header"
        "malformed-row", "a row with more or fewer fields than the header, or a JSON row that isn't an object"
        "unknown-column", "a column in a file that the table doesn't have"
        "duplicate-column", "a column that appears twice in a header or a JSON row"
        "ignored-column", "a calculated column (or one of a type that holds no data) in a file: the engine computes it"
        "missing-column", "a data column of the table that the file doesn't have: it is BLANK in every row"
        "invalid-value", "a value that isn't of its column's data type"
        "duplicate-key", "a value that repeats in the column on the one side of a relationship"
        "blank-key", "a BLANK in the column on the one side of a relationship, or in a key column"
        "unmatched-key",
        "values on the many side of a relationship with no match on the one side (DAX puts them on the blank row)"
        "not-unique", "a value that repeats in a column marked as key or unique"
        "unchecked-relationship", "a relationship that can't be checked: an end is a calculated table or column"
    ]

    // ---- What the model wants ---------------------------------------------------------------------

    /// A table the engine computes rather than loads: a calculated table or a calculation group.
    let isComputed (t: Table) =
        not (isNull t.CalculationGroup)
        || t.Partitions
           |> Seq.exists (fun p ->
               p.SourceType = PartitionSourceType.Calculated
               || p.SourceType = PartitionSourceType.CalculationGroup)

    /// The columns of a table that a fixture file supplies: its data columns of a type that holds
    /// values, in model order.
    let dataColumns (t: Table) : (Column * DaxType) list = [
        for c in t.Columns do
            match c with
            | :? DataColumn ->
                match DaxType.ofTom c.DataType with
                | Some dt when dt <> DaxType.Variant -> c, dt
                | _ -> ()
            | _ -> ()
    ]

    /// The tables a fixture supplies, in model order.
    let dataTables (db: Database) : Table list =
        if isNull db.Model then
            []
        else
            [
                for t in db.Model.Tables do
                    if not (isComputed t) then
                        t
            ]

    // ---- Files ------------------------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type FileFormat =
        | Csv
        | Json

    let private formatOf (fileName: string) =
        match Path.GetExtension(fileName).ToLowerInvariant() with
        | ".csv" -> Some FileFormat.Csv
        | ".json" -> Some FileFormat.Json
        | _ -> None

    /// Which files are which tables' (a pure function of the names, so it behaves the same on a
    /// case-sensitive and a case-insensitive file system).
    type Assignment = {
        /// Each table with the data files named for it: normally none or one.
        Tables: (string * string list) list
        /// Data files that are no table's.
        Unknown: string list
    }

    /// Assign file names (not paths) to table names. Files that aren't `.csv` or `.json` are left out.
    let assign (tableNames: string list) (fileNames: string list) : Assignment =
        let files =
            fileNames
            |> List.filter (fun f -> (formatOf f).IsSome)
            |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))

        let stem (f: string) = Path.GetFileNameWithoutExtension f

        let tables =
            tableNames
            |> List.map (fun t -> t, files |> List.filter (fun f -> FixtureName.matches t (stem f)))

        let claimed = tables |> List.collect snd |> HashSet

        {
            Tables = tables
            Unknown = files |> List.filter (fun f -> not (claimed.Contains f))
        }

    // ---- Values as text ---------------------------------------------------------------------------

    let private inv = CultureInfo.InvariantCulture

    /// A value as fixture files write it: invariant culture, ISO 8601 dates, `true`/`false`; BLANK
    /// is "". Reading the text back into a column of the value's type gives the value.
    let formatValue (v: DaxValue) : string =
        match v with
        | DaxValue.Blank -> ""
        | DaxValue.Integer i -> i.ToString inv
        | DaxValue.Decimal d -> d.ToString("R", inv)
        | DaxValue.Currency c -> (DaxValue.currencyValue c).ToString("0.####", inv)
        | DaxValue.String s -> s
        | DaxValue.Boolean b -> if b then "true" else "false"
        | DaxValue.DateTime serial ->
            match DaxValue.dateTimeOfSerial serial with
            | Some dt when dt.TimeOfDay = TimeSpan.Zero -> dt.ToString("yyyy-MM-dd", inv)
            | Some dt -> dt.ToString("yyyy-MM-dd'T'HH:mm:ss.FFFFFFF", inv)
            | None -> serial.ToString("R", inv)

    let private typeWords =
        function
        | DaxType.Integer -> "a whole number"
        | DaxType.Decimal -> "a number"
        | DaxType.Currency -> "a fixed decimal number (at most 4 decimals, within +-922,337,203,685,477.5807)"
        | DaxType.Boolean -> "true or false"
        | DaxType.DateTime -> "an ISO 8601 date or date and time (2024-01-31, 2024-01-31T13:05:00)"
        | DaxType.String -> "text"
        | DaxType.Variant -> "a value"

    let private isWhite (s: ReadOnlySpan<char>) = s.IsWhiteSpace()

    let private numberStyle =
        NumberStyles.AllowLeadingSign
        ||| NumberStyles.AllowLeadingWhite
        ||| NumberStyles.AllowTrailingWhite

    /// A Currency's scaled integer from a decimal number with at most four decimals.
    let private scaledOfDecimal (d: decimal) : int64 voption =
        if d > 922337203685477.5807m || d < -922337203685477.5808m then
            ValueNone
        else
            let scaled = d * 10000m

            if Decimal.Truncate scaled <> scaled then
                ValueNone
            else
                ValueSome(Decimal.ToInt64 scaled)

    /// Add a field's text to a column as a value of the column's type; false if it isn't one.
    /// An empty (or, outside text columns, all-space) field is BLANK; a quoted empty field in a text
    /// column is the empty string.
    let private addText (b: ColumnBuilder) (text: ReadOnlySpan<char>) (quoted: bool) : bool =
        match b.DataType with
        | DaxType.String ->
            if text.Length = 0 && not quoted then
                b.AddBlank()
            else
                b.AddString(text.ToString())

            true
        | _ when isWhite text ->
            b.AddBlank()
            true
        | DaxType.Integer ->
            let mutable i = 0L

            if Int64.TryParse(text, numberStyle, inv, &i) then
                b.AddInteger i
                true
            else
                false
        | DaxType.Decimal ->
            let mutable d = 0.0

            // NumberStyles.Float still reads "NaN" and "Infinity", and overflows to infinity.
            if
                Double.TryParse(
                    text,
                    numberStyle ||| NumberStyles.AllowDecimalPoint ||| NumberStyles.AllowExponent,
                    inv,
                    &d
                )
                && Double.IsFinite d
            then
                b.AddDecimal d
                true
            else
                false
        | DaxType.Currency ->
            let mutable d = 0m

            if Decimal.TryParse(text, numberStyle ||| NumberStyles.AllowDecimalPoint, inv, &d) then
                match scaledOfDecimal d with
                | ValueSome scaled ->
                    b.AddCurrency scaled
                    true
                | ValueNone -> false
            else
                false
        | DaxType.Boolean ->
            let t = text.Trim()

            if
                t.Equals("true".AsSpan(), StringComparison.OrdinalIgnoreCase)
                || t.Equals("1".AsSpan(), StringComparison.Ordinal)
            then
                b.AddBoolean true
                true
            elif
                t.Equals("false".AsSpan(), StringComparison.OrdinalIgnoreCase)
                || t.Equals("0".AsSpan(), StringComparison.Ordinal)
            then
                b.AddBoolean false
                true
            else
                false
        | DaxType.DateTime ->
            match DaxValue.parseIsoSerial text with
            | ValueSome serial ->
                b.AddDateTime serial
                true
            | ValueNone -> false
        | DaxType.Variant -> false

    // ---- Loading ----------------------------------------------------------------------------------

    type LoadOptions = {
        /// How many `invalid-value` (and `malformed-row`) issues to report one by one per column
        /// (per file); the rest are counted in one more issue.
        MaxIssuesPerColumn: int
    }

    module LoadOptions =
        let defaults = { MaxIssuesPerColumn = 10 }

    /// One table of the model, as loaded.
    type LoadedTable = {
        Table: string
        /// The file it was read from; None for a table with no file (or more than one).
        File: string option
        Rows: int
    }

    type Loaded = {
        Directory: string
        Store: DataStore
        /// Problems found while reading, most severe kinds included: a store is built regardless,
        /// with BLANK where a value was invalid. Whoever evaluates against it should refuse when
        /// there are errors (`hasErrors`).
        Issues: FixtureIssue list
        /// Every table a fixture supplies, in model order.
        Tables: LoadedTable list
        /// Calculated tables and calculation groups: not in the store.
        Computed: string list
    }

    let hasErrors (issues: FixtureIssue list) =
        issues |> List.exists (fun i -> i.Severity = Sev.Error)

    let private shown (text: string) =
        let oneLine = text.Replace("\r", "\\r").Replace("\n", "\\n")

        if oneLine.Length > 40 then
            oneLine.Substring(0, 40) + "..."
        else
            oneLine

    /// Collects the columns of one table while its file is read.
    type private Sink(table: Table, file: string, collation: Collation, opts: LoadOptions) =
        let tableRef = ObjectRef.table table.Name
        let columns = dataColumns table

        let builders =
            columns
            |> List.map (fun (c, dt) -> ColumnBuilder(c.Name, dt, collation))
            |> Array.ofList

        let invalid = Array.zeroCreate<int> builders.Length
        let issues = ResizeArray<FixtureIssue>()
        let mutable malformedRows = 0

        let byName =
            let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
            builders |> Array.iteri (fun i b -> d.[b.Name] <- i)
            d

        member _.Table = table
        member _.Builders = builders
        member _.Issues = issues

        member _.Report(rule, severity, r: ObjectRef, message, line: int option, field: string option) =
            issues.Add {
                Rule = rule
                Severity = severity
                Ref = r
                Message = message
                File = Some file
                Line = line
                Field = field
            }

        member this.Malformed(line: int option, message: string) =
            this.Report("malformed-file", Sev.Error, tableRef, message, line, None)

        member this.MalformedRow(line: int, message: string) =
            malformedRows <- malformedRows + 1

            if malformedRows <= opts.MaxIssuesPerColumn then
                this.Report("malformed-row", Sev.Error, tableRef, message, Some line, None)

        /// What a header name (or JSON property) is: the index of a data column, or -1 after
        /// reporting why it is none (once per name: `seen` remembers).
        member this.Resolve(name: string, line: int, seen: HashSet<string>) : int =
            match byName.TryGetValue name with
            | true, i -> i
            | _ ->
                if seen.Add name then
                    let computed =
                        table.Columns
                        |> Seq.tryFind (fun c -> String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))

                    match computed with
                    | Some c ->
                        let why =
                            match c with
                            | :? CalculatedColumn -> "it is a calculated column, which the engine computes"
                            | _ -> $"its data type ({c.DataType}) holds no fixture data"

                        this.Report(
                            "ignored-column",
                            Sev.Warning,
                            ObjectRef.inTable ObjectKind.Column table.Name c.Name,
                            $"column '{name}' is ignored: {why}",
                            Some line,
                            Some name
                        )
                    | None ->
                        let known = builders |> Array.map (fun b -> b.Name) |> String.concat ", "

                        this.Report(
                            "unknown-column",
                            Sev.Error,
                            tableRef,
                            $"table '{table.Name}' has no column '{name}' (its data columns: {known})",
                            Some line,
                            Some name
                        )

                -1

        member this.DuplicateColumn(name: string, line: int) =
            this.Report(
                "duplicate-column",
                Sev.Error,
                ObjectRef.inTable ObjectKind.Column table.Name name,
                $"column '{name}' appears more than once (the first is used)",
                Some line,
                Some name
            )

        /// A value that isn't of the column's type: BLANK is stored in its place.
        member this.Invalid(column: int, line: int, text: string, what: string) =
            let b = builders.[column]
            b.AddBlank()
            invalid.[column] <- invalid.[column] + 1

            if invalid.[column] <= opts.MaxIssuesPerColumn then
                this.Report(
                    "invalid-value",
                    Sev.Error,
                    ObjectRef.inTable ObjectKind.Column table.Name b.Name,
                    $"{what} is not {typeWords b.DataType}, which column '{b.Name}' ({DaxType.name b.DataType}) needs",
                    Some line,
                    Some b.Name
                )

            ignore text

        /// The table's columns, padded with BLANK where the file had none, plus the issues that
        /// are counts.
        member this.Finish(present: bool array, rows: int) : StoreColumn array =
            if malformedRows > opts.MaxIssuesPerColumn then
                this.Report(
                    "malformed-row",
                    Sev.Error,
                    tableRef,
                    $"{malformedRows - opts.MaxIssuesPerColumn} more malformed rows",
                    None,
                    None
                )

            builders
            |> Array.mapi (fun i b ->
                if invalid.[i] > opts.MaxIssuesPerColumn then
                    this.Report(
                        "invalid-value",
                        Sev.Error,
                        ObjectRef.inTable ObjectKind.Column table.Name b.Name,
                        $"{invalid.[i] - opts.MaxIssuesPerColumn} more invalid values in column '{b.Name}' ({invalid.[i]} in all)",
                        None,
                        Some b.Name
                    )

                if present.[i] then
                    b.Build()
                else
                    if rows > 0 then
                        this.Report(
                            "missing-column",
                            Sev.Warning,
                            ObjectRef.inTable ObjectKind.Column table.Name b.Name,
                            $"the file has no column '{b.Name}': it is BLANK in every row",
                            None,
                            Some b.Name
                        )

                    StoreColumn.blank collation b.Name b.DataType rows)

    let private readCsv (sink: Sink) (reader: TextReader) : StoreColumn array =
        let csv = CsvReader reader
        let builders = sink.Builders
        let present = Array.zeroCreate<bool> builders.Length
        let mutable rows = 0

        match csv.Read() with
        | Error(line, reason) -> sink.Malformed(Some line, reason)
        | Ok false -> sink.Malformed(None, "the file is empty: the first line must name the columns")
        | Ok true ->
            let seen = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            // For each field of a record: the column it fills, or -1.
            let target = Array.create csv.FieldCount -1
            let width = csv.FieldCount

            if width = 1 && csv.FieldString(0).Contains ';' then
                sink.Malformed(Some 1, "the header has one field with ';' in it: fields are separated by commas")

            for i in 0 .. width - 1 do
                let name = csv.FieldString i
                let column = sink.Resolve(name, csv.RecordLine, seen)

                if column >= 0 then
                    if present.[column] then
                        sink.DuplicateColumn(name, csv.RecordLine)
                    else
                        present.[column] <- true
                        target.[i] <- column

            let mutable reading = true

            while reading do
                match csv.Read() with
                | Error(line, reason) ->
                    sink.Malformed(Some line, reason)
                    reading <- false
                | Ok false -> reading <- false
                | Ok true ->
                    // An empty line is a row only where a row is one field.
                    if csv.IsBlankLine && width > 1 then
                        ()
                    elif csv.FieldCount <> width then
                        sink.MalformedRow(
                            csv.RecordLine,
                            $"the row has {csv.FieldCount} fields, the header has {width}"
                        )
                    else
                        rows <- rows + 1

                        for i in 0 .. width - 1 do
                            let column = target.[i]

                            if column >= 0 && not (addText builders.[column] (csv.Field i) (csv.IsQuoted i)) then
                                let text = csv.FieldString i
                                sink.Invalid(column, csv.RecordLine, text, $"'{shown text}'")

        sink.Finish(present, rows)

    let private readJson (sink: Sink) (bytes: byte array) : StoreColumn array =
        let builders = sink.Builders
        let present = Array.zeroCreate<bool> builders.Length
        let filled = Array.zeroCreate<int> builders.Length
        let seen = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let mutable rows = 0
        // Line numbers: count the line feeds up to an offset, moving forward only.
        let mutable countedTo = 0
        let mutable line = 1

        let lineAt (offset: int) =
            for i in countedTo .. offset - 1 do
                if bytes.[i] = 10uy then
                    line <- line + 1

            countedTo <- max countedTo offset
            line

        let start =
            if
                bytes.Length >= 3
                && bytes.[0] = 0xEFuy
                && bytes.[1] = 0xBBuy
                && bytes.[2] = 0xBFuy
            then
                3
            else
                0

        let options =
            JsonReaderOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

        try
            let mutable reader =
                Utf8JsonReader(ReadOnlySpan<byte>(bytes, start, bytes.Length - start), options)

            if not (reader.Read()) || reader.TokenType <> JsonTokenType.StartArray then
                sink.Malformed(
                    Some 1,
                    "the file must be a JSON array of row objects: [ { \"Column\": value, ... }, ... ]"
                )
            else
                let mutable inArray = true

                while inArray && reader.Read() do
                    match reader.TokenType with
                    | JsonTokenType.EndArray -> inArray <- false
                    | JsonTokenType.StartObject ->
                        let rowLine = lineAt (start + int reader.TokenStartIndex)
                        rows <- rows + 1
                        let mutable inObject = true

                        while inObject && reader.Read() do
                            match reader.TokenType with
                            | JsonTokenType.EndObject -> inObject <- false
                            | JsonTokenType.PropertyName ->
                                let name = reader.GetString()
                                let column = sink.Resolve(name, rowLine, seen)
                                reader.Read() |> ignore

                                if column < 0 then
                                    reader.Skip()
                                elif filled.[column] = rows then
                                    sink.DuplicateColumn(name, rowLine)
                                    reader.Skip()
                                else
                                    filled.[column] <- rows
                                    present.[column] <- true
                                    let b = builders.[column]
                                    // Rows that left this column out are BLANK there.
                                    while b.RowCount < rows - 1 do
                                        b.AddBlank()

                                    let dt = b.DataType

                                    let accepted =
                                        match reader.TokenType with
                                        | JsonTokenType.Null ->
                                            b.AddBlank()
                                            true
                                        | JsonTokenType.String ->
                                            // Text reads as the same text in a quoted CSV field.
                                            let s = reader.GetString()
                                            addText b (s.AsSpan()) true
                                        | JsonTokenType.True
                                        | JsonTokenType.False when dt = DaxType.Boolean ->
                                            b.AddBoolean(reader.TokenType = JsonTokenType.True)
                                            true
                                        | JsonTokenType.Number when dt = DaxType.Integer ->
                                            let mutable i = 0L

                                            if reader.TryGetInt64(&i) then
                                                b.AddInteger i
                                                true
                                            else
                                                false
                                        | JsonTokenType.Number when dt = DaxType.Decimal ->
                                            let mutable d = 0.0

                                            if reader.TryGetDouble(&d) && Double.IsFinite d then
                                                b.AddDecimal d
                                                true
                                            else
                                                false
                                        | JsonTokenType.Number when dt = DaxType.Currency ->
                                            let mutable d = 0m

                                            if reader.TryGetDecimal(&d) then
                                                match scaledOfDecimal d with
                                                | ValueSome scaled ->
                                                    b.AddCurrency scaled
                                                    true
                                                | ValueNone -> false
                                            else
                                                false
                                        | _ -> false

                                    if not accepted then
                                        let what =
                                            match reader.TokenType with
                                            | JsonTokenType.String -> $"'{shown (reader.GetString())}'"
                                            | JsonTokenType.Number ->
                                                "the number " + shown (Encoding.UTF8.GetString(reader.ValueSpan))
                                            | JsonTokenType.True -> "true"
                                            | JsonTokenType.False -> "false"
                                            | JsonTokenType.StartObject -> "an object"
                                            | JsonTokenType.StartArray -> "an array"
                                            | t -> string t

                                        reader.Skip()
                                        sink.Invalid(column, rowLine, "", what)
                            | _ -> ()
                    | _ ->
                        sink.MalformedRow(
                            lineAt (start + int reader.TokenStartIndex),
                            "a row must be an object: { \"Column\": value, ... }"
                        )

                        reader.Skip()

                if inArray then
                    sink.Malformed(None, "the array of rows is never closed")
                elif reader.Read() then
                    sink.Malformed(Some(lineAt (start + int reader.TokenStartIndex)), "text after the array of rows")
        with :? JsonException as e ->
            let at =
                if e.LineNumber.HasValue then
                    Some(int e.LineNumber.Value + 1)
                else
                    None

            // System.Text.Json appends its own position to the message.
            let reason = e.Message.Split(" LineNumber:").[0].Trim()
            sink.Malformed(at, $"not valid JSON: {reason}")

        for b in builders do
            while b.RowCount < rows do
                b.AddBlank()

        sink.Finish(present, rows)

    let private emptyColumns (collation: Collation) (table: Table) =
        dataColumns table
        |> List.map (fun (c, dt) -> StoreColumn.blank collation c.Name dt 0)
        |> Array.ofList

    let private strictUtf8 = UTF8Encoding(false, true)

    /// Read one table's file. Never throws for what is in the file: every problem is an issue.
    let private loadFile (collation: Collation) (opts: LoadOptions) (table: Table) (path: string) =
        let sink = Sink(table, path, collation, opts)

        let columns =
            try
                match formatOf path with
                | Some FileFormat.Json -> readJson sink (File.ReadAllBytes path)
                | _ ->
                    use reader = new StreamReader(path, strictUtf8, true, 1 <<< 16)
                    readCsv sink reader
            with
            | :? DecoderFallbackException ->
                sink.Malformed(None, "the file is not UTF-8")
                emptyColumns collation table
            | :? IOException
            | :? UnauthorizedAccessException as e ->
                sink.Malformed(None, $"can't read the file: {e.Message}")
                emptyColumns collation table

        columns, List.ofSeq sink.Issues

    /// Read a table from CSV text (the same reader `load` uses, for tests and tools). `file` only
    /// labels the issues.
    let readCsvTable
        (collation: Collation)
        (opts: LoadOptions)
        (table: Table)
        (file: string)
        (reader: TextReader)
        : StoreColumn array * FixtureIssue list =
        let sink = Sink(table, file, collation, opts)
        let columns = readCsv sink reader
        columns, List.ofSeq sink.Issues

    /// Read a table from JSON (UTF-8 bytes); see `readCsvTable`.
    let readJsonTable
        (collation: Collation)
        (opts: LoadOptions)
        (table: Table)
        (file: string)
        (json: byte array)
        : StoreColumn array * FixtureIssue list =
        let sink = Sink(table, file, collation, opts)
        let columns = readJson sink json
        columns, List.ofSeq sink.Issues

    let private modelIssue rule severity message (file: string option) = {
        Rule = rule
        Severity = severity
        Ref = ObjectRef.model
        Message = message
        File = file
        Line = None
        Field = None
    }

    /// Load a fixture directory against a model: every table that isn't computed gets its file's
    /// rows (or none), every value converted to its column's type.
    ///
    /// Fails only if the directory isn't one. Everything wrong inside it is in `Issues`, and the
    /// store is built anyway (invalid values as BLANK) so a checker can keep going; refuse to
    /// evaluate when `hasErrors`. The checks that need all the data are in `check`.
    let loadWith (opts: LoadOptions) (db: Database) (directory: string) : Result<Loaded, TessError> =
        let full = Path.GetFullPath directory

        if File.Exists full then
            Error(InvalidFixture(full, "it is a file; a fixture is a directory of <Table>.csv or <Table>.json files"))
        elif not (Directory.Exists full) then
            Error(PathNotFound full)
        else
            let collation = DataStore.collationOf db
            let tables = dataTables db

            let computed =
                if isNull db.Model then
                    []
                else
                    [
                        for t in db.Model.Tables do
                            if isComputed t then
                                t.Name
                    ]

            let files = Directory.EnumerateFiles full |> Seq.map Path.GetFileName |> List.ofSeq

            let assigned = assign (tables |> List.map (fun t -> t.Name)) files
            let issues = ResizeArray<FixtureIssue>()

            for f in assigned.Unknown do
                let stem = Path.GetFileNameWithoutExtension f
                let path = Path.Combine(full, f)

                match computed |> List.tryFind (fun t -> FixtureName.matches t stem) with
                | Some t ->
                    issues.Add {
                        modelIssue
                            "calculated-table"
                            Sev.Warning
                            $"'{t}' is a calculated table or calculation group, which the engine computes: {f} is ignored"
                            (Some path) with
                            Ref = ObjectRef.table t
                    }
                | None ->
                    let name = FixtureName.decode stem |> Option.defaultValue stem

                    issues.Add(
                        modelIssue
                            "unknown-table"
                            Sev.Error
                            $"the model has no table '{name}' ({f}); files are named <Table>.csv or <Table>.json"
                            (Some path)
                    )

            let loaded =
                List.zip tables assigned.Tables
                |> List.map (fun (table, (_, tableFiles)) ->
                    match tableFiles with
                    | [ f ] ->
                        let path = Path.Combine(full, f)
                        let columns, fileIssues = loadFile collation opts table path
                        issues.AddRange fileIssues
                        let rows = if columns.Length = 0 then 0 else columns.[0].RowCount
                        columns, Some path, rows
                    | [] -> emptyColumns collation table, None, 0
                    | several ->
                        let names = String.concat ", " several

                        issues.Add {
                            modelIssue
                                "duplicate-file"
                                Sev.Error
                                $"table '{table.Name}' has more than one file ({names}): none is loaded"
                                (Some(Path.Combine(full, several.Head))) with
                                Ref = ObjectRef.table table.Name
                        }

                        emptyColumns collation table, None, 0)

            let missing =
                List.zip tables assigned.Tables
                |> List.filter (fun (t, (_, f)) -> f.IsEmpty && not (dataColumns t).IsEmpty)
                |> List.map (fun (t, _) -> t.Name)

            if not missing.IsEmpty then
                let n = missing.Length
                let noun = if n = 1 then "1 table has" else $"{n} tables have"
                let names = String.concat ", " missing
                issues.Add(modelIssue "missing-table" Sev.Warning $"{noun} no file and no rows: {names}" None)

            let storeTables =
                List.zip tables loaded
                |> List.map (fun (table, (columns, _, rows)) ->
                    match StoreTable.Create(table.Name, columns, rows) with
                    | Ok t -> t
                    // Unreachable for a model TOM accepts: its column names are unique.
                    | Error e -> failwith e)

            match DataStore.Create(collation, storeTables, DataStore.relationshipsOf db) with
            | Error e -> Error(InvalidFixture(full, e))
            | Ok store ->
                Ok {
                    Directory = full
                    Store = store
                    Issues = List.ofSeq issues
                    Tables =
                        List.zip tables loaded
                        |> List.map (fun (t, (_, file, rows)) -> {
                            Table = t.Name
                            File = file
                            Rows = rows
                        })
                    Computed = computed
                }

    /// `loadWith` the default options.
    let load (db: Database) (directory: string) : Result<Loaded, TessError> =
        loadWith LoadOptions.defaults db directory

    // ---- Checks that need all the data ------------------------------------------------------------

    let private examples (column: StoreColumn) (ids: int seq) =
        let some =
            ids
            |> Seq.truncate 3
            |> Seq.map (fun id -> $"'{shown (formatValue column.Values.[id])}'")
            |> List.ofSeq

        String.concat ", " some

    let private plural (n: int) (one: string) (many: string) =
        if n = 1 then $"1 {one}" else $"{n} {many}"

    /// The checks of the loaded data against the model's relationships and keys:
    /// `duplicate-key`, `blank-key`, `unmatched-key`, `not-unique`, `unchecked-relationship`.
    let check (db: Database) (loaded: Loaded) : FixtureIssue list =
        let store = loaded.Store
        let issues = ResizeArray<FixtureIssue>()

        let fileOf (table: string) =
            loaded.Tables
            |> List.tryFind (fun t -> String.Equals(t.Table, table, StringComparison.OrdinalIgnoreCase))
            |> Option.bind (fun t -> t.File)

        let report rule severity (name: ColumnName) message =
            issues.Add {
                Rule = rule
                Severity = severity
                Ref = ObjectRef.inTable ObjectKind.Column name.Table name.Column
                Message = message
                File = fileOf name.Table
                Line = None
                Field = Some name.Column
            }

        let duplicated (c: StoreColumn) =
            seq { 0 .. c.DistinctCount - 1 }
            |> Seq.filter (fun id -> id <> c.BlankId && c.CountOfId id > 1)

        // Columns already reported as a relationship's one side, so a key isn't reported twice.
        let reported = HashSet<string * string>()

        let oneSide (r: StoreRelationship) (name: ColumnName) (column: StoreColumn) =
            if reported.Add((name.Table.ToUpperInvariant(), name.Column.ToUpperInvariant())) then
                let where =
                    $"'{name.Table}'[{name.Column}], the one side of relationship '{r.Name}'"

                let repeats = duplicated column |> List.ofSeq

                if not repeats.IsEmpty then
                    report
                        "duplicate-key"
                        Sev.Error
                        name
                        $"""{plural repeats.Length "value repeats" "values repeat"} in {where}: {examples column repeats}"""

                if column.BlankId >= 0 then
                    report
                        "blank-key"
                        Sev.Error
                        name
                        $"""{plural (column.CountOfId column.BlankId) "row is" "rows are"} BLANK in {where}"""

        let unmatched
            (r: StoreRelationship)
            (name: ColumnName)
            (column: StoreColumn)
            (other: ColumnName)
            (map: int array)
            (rows: int array)
            =
            if rows.Length > 0 then
                let ids =
                    seq { 0 .. map.Length - 1 } |> Seq.filter (fun id -> map.[id] < 0) |> List.ofSeq

                report
                    "unmatched-key"
                    Sev.Warning
                    name
                    ($"""{plural rows.Length "row" "rows"} of '{name.Table}' ({plural ids.Length "value" "values"} of [{name.Column}]) """
                     + $"have no match in '{other.Table}'[{other.Column}] (relationship '{r.Name}'): {examples column ids}; "
                     + $"DAX puts them on the blank row of '{other.Table}'")

        for r in store.Relationships do
            match store.Match r with
            | None ->
                let missingEnd =
                    [ r.From; r.To ]
                    |> List.filter (fun e -> (store.TryColumn e).IsNone)
                    |> List.map (fun e -> $"'{e.Table}'[{e.Column}]")
                    |> String.concat " and "

                issues.Add {
                    Rule = "unchecked-relationship"
                    Severity = Sev.Info
                    Ref = ObjectRef.modelLevel ObjectKind.Relationship r.Name
                    Message =
                        $"'{r.From.Table}'[{r.From.Column}] -> '{r.To.Table}'[{r.To.Column}] is not checked: "
                        + $"{missingEnd} is computed by the engine, not loaded from the fixture"
                    File = None
                    Line = None
                    Field = None
                }
            | Some m ->
                let resolved = m.Resolved

                if r.ToCardinality = Cardinality.One then
                    oneSide r r.To resolved.ToColumn
                    unmatched r r.From resolved.FromColumn r.To m.ToIdOfFromId m.OrphanFromRows

                if r.FromCardinality = Cardinality.One then
                    oneSide r r.From resolved.FromColumn
                    unmatched r r.To resolved.ToColumn r.From m.FromIdOfToId m.OrphanToRows

        for table in dataTables db do
            for column, _ in dataColumns table do
                if column.IsKey || column.IsUnique then
                    let name = {
                        Table = table.Name
                        Column = column.Name
                    }

                    match store.TryColumn name with
                    | Some c when reported.Add((name.Table.ToUpperInvariant(), name.Column.ToUpperInvariant())) ->
                        let marked = if column.IsKey then "the table's key" else "marked unique"
                        let repeats = duplicated c |> List.ofSeq

                        if not repeats.IsEmpty then
                            report
                                "not-unique"
                                Sev.Error
                                name
                                $"""{plural repeats.Length "value repeats" "values repeat"} in '{name.Table}'[{name.Column}], which is {marked}: {examples c repeats}"""

                        if column.IsKey && c.BlankId >= 0 then
                            report
                                "blank-key"
                                Sev.Error
                                name
                                $"""{plural (c.CountOfId c.BlankId) "row is" "rows are"} BLANK in '{name.Table}'[{name.Column}], which is {marked}"""
                    | _ -> ()

        List.ofSeq issues

    /// Load a fixture and run every check: the loader's issues, then `check`'s.
    let loadAndCheck (db: Database) (directory: string) : Result<Loaded * FixtureIssue list, TessError> =
        load db directory
        |> Result.map (fun loaded -> loaded, loaded.Issues @ check db loaded)

    // ---- Writing ----------------------------------------------------------------------------------

    /// A CSV field: quoted when it holds a comma, a quote, a line break or outer whitespace, and
    /// when it is the empty string (so it doesn't read back as BLANK).
    let csvField (v: DaxValue) : string =
        match v with
        | DaxValue.String "" -> "\"\""
        | DaxValue.String s ->
            let needsQuotes =
                s.IndexOfAny([| ','; '"'; '\r'; '\n' |]) >= 0
                || Char.IsWhiteSpace s.[0]
                || Char.IsWhiteSpace s.[s.Length - 1]
                || s.[0] = '\uFEFF'

            if needsQuotes then
                "\"" + s.Replace("\"", "\"\"") + "\""
            else
                s
        | v -> formatValue v

    /// Write a table as a fixture CSV: a header of column names, then one line per row, each ending
    /// in a line feed, in the invariant culture (see `formatValue`).
    let writeCsv (writer: TextWriter) (table: StoreTable) : unit =
        let columns = table.Columns
        // Each distinct value is formatted once.
        let texts = columns |> Array.map (fun c -> c.Values |> Array.map csvField)

        let line (fields: string seq) =
            writer.Write(String.concat "," fields)
            writer.Write '\n'

        line (columns |> Array.map (fun c -> csvField (DaxValue.String c.Name)))

        for row in 0 .. table.RowCount - 1 do
            for i in 0 .. columns.Length - 1 do
                if i > 0 then
                    writer.Write ','

                writer.Write texts.[i].[columns.[i].ValueIds.[row]]

            writer.Write '\n'
