/// A copy of a model that carries its fixture data inside it: every loaded table becomes a
/// calculated table whose expression is a DATATABLE of the fixture's rows. Such a model needs no
/// data source, so it can be deployed to any workspace and queried there as it is queried locally.
///
/// DATATABLE is used because its documentation says exactly what it accepts and does: numbers (with
/// a minus sign), strings, TRUE, FALSE, BLANK () and `dt"..."` literals, each converted to the
/// declared type of its column (https://learn.microsoft.com/en-us/dax/datatable-function).
module Tessera.FixtureEmbed

open System
open System.Globalization
open System.Text
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular

type EmbeddedTable = {
    Table: string
    Rows: int
    Columns: int
    /// The table's new expression.
    Expression: string
}

type Embedded = {
    /// The new model. The one given is not changed.
    Database: Database
    /// The tables that were turned into calculated tables, in model order.
    Tables: EmbeddedTable list
    /// Where the copy can't be exactly the fixture, or what was dropped from the model.
    Warnings: string list
}

let private inv = CultureInfo.InvariantCulture

/// A DAX string literal.
let stringLiteral (s: string) = "\"" + s.Replace("\"", "\"\"") + "\""

/// A finite double in plain decimal digits that read back as the same double: no exponent, since
/// DATATABLE only takes constants and a plain number is the one spelling nobody disputes.
let doubleLiteral (d: float) : string =
    if d = 0.0 then
        "0"
    else
        let r = d.ToString("R", inv)
        let e = r.IndexOfAny [| 'E'; 'e' |]

        if e < 0 then
            r
        else
            let mantissa = r.Substring(0, e)
            let exponent = Int32.Parse(r.Substring(e + 1), NumberStyles.AllowLeadingSign, inv)
            let negative = mantissa.StartsWith '-'
            let mantissa = mantissa.TrimStart '-'
            let point = mantissa.IndexOf '.'
            let digits = mantissa.Replace(".", "")
            // The decimal point sits after this many digits.
            let position = (if point < 0 then mantissa.Length else point) + exponent

            let text =
                if position <= 0 then
                    "0." + String('0', -position) + digits
                elif position >= digits.Length then
                    digits + String('0', position - digits.Length)
                else
                    digits.Substring(0, position) + "." + digits.Substring position

            let text =
                if text.Contains '.' then
                    text.TrimEnd('0').TrimEnd('.')
                else
                    text

            (if negative then "-" else "") + text

/// A value as a DATATABLE constant, and a warning if the constant can't be exactly the value.
let private literal (table: string) (column: string) (v: DaxValue) : string * string option =
    match v with
    | DaxValue.Blank -> "BLANK ()", None
    | DaxValue.Integer i -> i.ToString inv, None
    | DaxValue.Boolean true -> "TRUE", None
    | DaxValue.Boolean false -> "FALSE", None
    | DaxValue.String s -> stringLiteral s, None
    | DaxValue.Decimal d when Double.IsFinite d -> doubleLiteral d, None
    | DaxValue.Decimal d ->
        "BLANK ()", Some $"'{table}'[{column}] holds {QueryValue.doubleText d}, which has no constant: written as BLANK"
    | DaxValue.Currency scaled ->
        let text = (DaxValue.currencyValue scaled).ToString("0.####", inv)
        let significant = text.Replace("-", "").Replace(".", "").TrimStart('0').Length

        text,
        (if significant > 15 then
             Some
                 $"'{table}'[{column}] holds {text}, which has more than 15 significant digits: a server reads the constant as a floating-point number first and may round it"
         else
             None)
    | DaxValue.DateTime serial ->
        match DaxValue.dateTimeOfSerial serial with
        | None ->
            "BLANK ()",
            Some $"'{table}'[{column}] holds a date outside the calendar (serial {serial}): written as BLANK"
        | Some dt ->
            let whole = DateTime(dt.Ticks - dt.Ticks % TimeSpan.TicksPerSecond)

            let text =
                if whole.TimeOfDay = TimeSpan.Zero then
                    whole.ToString("yyyy-MM-dd", inv)
                else
                    whole.ToString("yyyy-MM-dd'T'HH:mm:ss", inv)

            $"dt\"{text}\"",
            (if whole <> dt then
                 Some
                     $"'{table}'[{column}] holds {QueryValue.dateTimeText dt}: a date literal has whole seconds, so the fraction is dropped"
             else
                 None)

/// The name of the placeholder column of a table that has no data columns (a calculated table
/// must return at least one).
[<Literal>]
let PlaceholderColumn = "_"

/// Write DATATABLE ( declarations, { rows } ), or for no rows the same with one BLANK row filtered
/// to nothing. `cell row column` is the constant to write.
let private writeExpression (columns: (string * DaxType) list) (rowCount: int) (cell: int -> int -> string) : string =
    let sb = StringBuilder()

    let declared =
        if columns.IsEmpty then
            [ PlaceholderColumn, DaxType.Integer ]
        else
            columns

    let empty = rowCount = 0 || columns.IsEmpty

    if empty then
        sb.Append("FILTER (\n") |> ignore

    sb.Append("DATATABLE (\n") |> ignore

    for name, dataType in declared do
        sb.Append("    ").Append(stringLiteral name).Append(", ").Append(DaxType.name dataType).Append(",\n")
        |> ignore

    sb.Append("    {\n") |> ignore

    if empty then
        let blanks = declared |> List.map (fun _ -> "BLANK ()") |> String.concat ", "
        sb.Append("        { ").Append(blanks).Append(" }\n") |> ignore
    else
        for row in 0 .. rowCount - 1 do
            sb.Append("        { ") |> ignore

            for i in 0 .. declared.Length - 1 do
                if i > 0 then
                    sb.Append(", ") |> ignore

                sb.Append(cell row i) |> ignore

            sb.Append(if row < rowCount - 1 then " },\n" else " }\n") |> ignore

    sb.Append("    }\n)") |> ignore

    if empty then
        sb.Append(",\n    FALSE ()\n)") |> ignore

    sb.ToString()

/// The DATATABLE expression holding a table's rows, with the warnings for values it can't hold
/// exactly. `columns` are the columns to write, in order, with their types; a column the store
/// table doesn't have is BLANK in every row.
///
/// A table with no rows is a one-row DATATABLE filtered to nothing, which keeps the columns and
/// their types. A table with no columns gets one, `PlaceholderColumn`, and no rows.
let tableExpression
    (table: string)
    (rows: StoreTable option)
    (columns: (string * DaxType) list)
    : string * string list =
    let warnings = ResizeArray<string>()
    let rowCount = rows |> Option.map _.RowCount |> Option.defaultValue 0

    let data =
        columns
        |> List.map (fun (name, _) -> rows |> Option.bind (fun t -> t.TryColumn name))
        |> Array.ofList

    // One text per distinct value, as the store holds one value per id.
    let texts =
        data
        |> Array.mapi (fun i column ->
            match column with
            | None -> [||]
            | Some c ->
                c.Values
                |> Array.map (fun v ->
                    let text, warning = literal table (fst columns.[i]) v
                    warning |> Option.iter warnings.Add
                    text))

    let cell (row: int) (i: int) =
        match data.[i] with
        | Some c -> texts.[i].[c.ValueIds.[row]]
        | None -> "BLANK ()"

    writeExpression columns rowCount cell, List.ofSeq warnings

/// `tableExpression` for rows given as values, each row in the order of `columns`. The values are
/// written as they are: text keeps its casing and its trailing spaces, which a `DataStore` would
/// have folded and dropped. For checking what a server does with data a store has never touched.
let rowsExpression
    (table: string)
    (columns: (string * DaxType) list)
    (rows: DaxValue array list)
    : string * string list =
    let warnings = ResizeArray<string>()
    let rows = Array.ofList rows

    let cell (row: int) (i: int) =
        let text, warning = literal table (fst columns.[i]) rows.[row].[i]
        warning |> Option.iter warnings.Add
        text

    writeExpression columns rows.Length cell, List.ofSeq warnings |> List.distinct

let private isKind (kind: Text.Json.JsonValueKind) (node: JsonNode) =
    not (isNull node) && node.GetValueKind() = kind

let private text (node: JsonNode) =
    if isKind Text.Json.JsonValueKind.String node then
        node.GetValue<string>()
    else
        ""

/// The model with every loaded table replaced by a calculated table holding the rows `expressionOf` gives: for a
/// table and its data columns, the expression, its warnings and the number of rows.
///
/// - Tables: those a fixture supplies (`Fixture.dataTables`); calculated tables and calculation
///   groups are already computed and stay as they are.
/// - Columns keep their names, types, and every other property (format strings, sort-by, keys,
///   hidden), so relationships, hierarchies, measures and calculated columns still resolve. A data
///   column of a type that holds no values here (binary) is dropped, with a warning.
/// - What only a refresh needs is removed: partitions, refresh policies, data sources and shared
///   (M) expressions.
let embedWith
    (db: Database)
    (expressionOf: string -> (string * DaxType) list -> string * string list * int)
    : Result<Embedded, TessError> =
    if isNull db.Model then
        Error(LoadFailed(db.Name, "the model is empty"))
    else
        let warnings = ResizeArray<string>()
        let tables = ResizeArray<EmbeddedTable>()

        let root =
            JsonNode.Parse(ModelIO.toBimText ModelIO.SaveOptions.defaults db).AsObject()

        let model = root.["model"].AsObject()

        let fixtureTables =
            Fixture.dataTables db
            |> List.map (fun t -> t.Name, Fixture.dataColumns t |> List.map (fun (c, dt) -> c.Name, dt))
            |> dict

        let same (a: string) (b: string) =
            String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

        match model.["tables"] with
        | :? JsonArray as tableNodes ->
            for tableNode in tableNodes do
                let table = tableNode.AsObject()
                let name = text table.["name"]

                match fixtureTables.TryGetValue name with
                | false, _ -> ()
                | true, columns ->
                    let expression, tableWarnings, rowCount = expressionOf name columns
                    warnings.AddRange tableWarnings

                    let columnNodes =
                        match table.["columns"] with
                        | :? JsonArray as a -> a
                        | _ ->
                            let a = JsonArray()
                            table.["columns"] <- a
                            a

                    for columnNode in List.ofSeq columnNodes do
                        let column = columnNode.AsObject()
                        let columnName = text column.["name"]
                        let columnType = text column.["type"]

                        // A data column has no "type" (or "data"); calculated columns keep theirs.
                        if columnType = "" || same columnType "data" then
                            if columns |> List.exists (fun (n, _) -> same n columnName) then
                                column.["type"] <- JsonValue.Create "calculatedTableColumn"
                                column.["sourceColumn"] <- JsonValue.Create("[" + columnName + "]")
                                // Declared, not inferred: TMDL writes a column's type only when it is declared,
                                // and relationships need the type before the table is ever calculated.
                                column.["isNameInferred"] <- JsonValue.Create false
                                column.["isDataTypeInferred"] <- JsonValue.Create false
                                column.Remove "sourceProviderType" |> ignore

                                if columnName.Contains ']' then
                                    warnings.Add
                                        $"'{name}'[{columnName}] has a ] in its name: the column may not bind to the table's expression on a server"
                            else
                                columnNodes.Remove columnNode |> ignore

                                let dataType = text column.["dataType"]

                                warnings.Add
                                    $"'{name}'[{columnName}] is of a type that holds no fixture data ({dataType}): the column is dropped"

                    if columns.IsEmpty then
                        let placeholder = JsonObject()
                        placeholder.["type"] <- JsonValue.Create "calculatedTableColumn"
                        placeholder.["name"] <- JsonValue.Create PlaceholderColumn
                        placeholder.["dataType"] <- JsonValue.Create "int64"
                        placeholder.["isHidden"] <- JsonValue.Create true
                        placeholder.["sourceColumn"] <- JsonValue.Create("[" + PlaceholderColumn + "]")
                        placeholder.["isNameInferred"] <- JsonValue.Create false
                        placeholder.["isDataTypeInferred"] <- JsonValue.Create false
                        columnNodes.Add placeholder

                        warnings.Add
                            $"'{name}' has no data columns: it gets a hidden column '{PlaceholderColumn}', since a calculated table must return one"

                    let source = JsonObject()
                    source.["type"] <- JsonValue.Create "calculated"
                    source.["expression"] <- JsonValue.Create expression
                    let partition = JsonObject()
                    partition.["name"] <- JsonValue.Create name
                    partition.["mode"] <- JsonValue.Create "import"
                    partition.["source"] <- source
                    table.["partitions"] <- JsonArray(partition)
                    table.Remove "refreshPolicy" |> ignore

                    tables.Add {
                        Table = name
                        Rows = rowCount
                        Columns = columns.Length
                        Expression = expression
                    }
        | _ -> ()

        let dropped (key: string) (what: string) =
            match model.[key] with
            | :? JsonArray as a when a.Count > 0 ->
                let n = a.Count
                model.Remove key |> ignore

                warnings.Add(
                    if n = 1 then
                        $"1 {what} removed: nothing loads data any more"
                    else
                        $"{n} {what}s removed: nothing loads data any more"
                )
            | _ -> model.Remove key |> ignore

        dropped "dataSources" "data source"
        dropped "expressions" "shared expression"
        // Every table is import now, whatever the model's default was.
        model.Remove "defaultMode" |> ignore

        try
            let embedded = ModelIO.parseBim (root.ToJsonString())
            embedded.Name <- db.Name
            embedded.ID <- db.ID

            Ok {
                Database = embedded
                Tables = List.ofSeq tables
                Warnings = List.ofSeq warnings |> List.distinct
            }
        with e ->
            Error(LoadFailed(db.Name, $"the model with its data embedded doesn't load: {e.Message}"))

/// The model with every loaded table replaced by a calculated table holding the store's rows.
let embed (db: Database) (store: DataStore) : Result<Embedded, TessError> =
    embedWith db (fun name columns ->
        let rows = store.TryTable name
        let expression, warnings = tableExpression name rows columns
        expression, warnings, rows |> Option.map _.RowCount |> Option.defaultValue 0)

/// `embed` from rows that never went through a `DataStore`: `rowsOf table` gives the table's rows,
/// each in the order of its data columns (`Fixture.dataColumns`). See `rowsExpression`.
let embedRows (db: Database) (rowsOf: string -> DaxValue array list) : Result<Embedded, TessError> =
    embedWith db (fun name columns ->
        let rows = rowsOf name
        let expression, warnings = rowsExpression name columns rows
        expression, warnings, rows.Length)
