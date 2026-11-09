/// Translations as a flat file for translators: one row per object, culture and translatable
/// property (caption, description, display folder), with the untranslated text alongside. Export
/// writes every row a translator could fill in; import applies a filled-in file as one edit.
module Tessera.Translations

open System
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

/// One translatable property of one object in one culture.
type Row = {
    Object: ObjectRef
    Culture: string
    Property: TranslatedProperty
    /// The untranslated text: the object's name (caption), description or display folder.
    Source: string
    /// "" when there is none.
    Translation: string
}

[<RequireQualifiedAccess>]
type Format =
    | Csv
    | Json

module Format =
    let token =
        function
        | Format.Csv -> "csv"
        | Format.Json -> "json"

/// "Caption", "Description", "DisplayFolder": the property column of a file.
let propertyName =
    function
    | TranslatedProperty.Caption -> "Caption"
    | TranslatedProperty.Description -> "Description"
    | TranslatedProperty.DisplayFolder -> "DisplayFolder"
    | p -> string p

let private properties = [
    TranslatedProperty.Caption
    TranslatedProperty.Description
    TranslatedProperty.DisplayFolder
]

let tryParseProperty (s: string) =
    properties
    |> List.tryFind (fun p -> String.Equals(propertyName p, s.Trim(), StringComparison.OrdinalIgnoreCase))

let private orEmpty (s: string) = if isNull s then "" else s

/// The untranslated value of a property: what the translation replaces.
let sourceValue (o: MetadataObject) (p: TranslatedProperty) =
    match p with
    | TranslatedProperty.Caption ->
        match o with
        | :? NamedMetadataObject as n -> orEmpty n.Name
        | _ -> ""
    | TranslatedProperty.Description ->
        match o with
        | :? Model as x -> orEmpty x.Description
        | :? Table as x -> orEmpty x.Description
        | :? Column as x -> orEmpty x.Description
        | :? Measure as x -> orEmpty x.Description
        | :? Hierarchy as x -> orEmpty x.Description
        | :? Level as x -> orEmpty x.Description
        | :? Perspective as x -> orEmpty x.Description
        | :? ModelRole as x -> orEmpty x.Description
        | :? NamedExpression as x -> orEmpty x.Description
        | :? Function as x -> orEmpty x.Description
        | :? Calendar as x -> orEmpty x.Description
        | _ -> ""
    | TranslatedProperty.DisplayFolder ->
        match o with
        | :? Column as x -> orEmpty x.DisplayFolder
        | :? Measure as x -> orEmpty x.DisplayFolder
        | :? Hierarchy as x -> orEmpty x.DisplayFolder
        | _ -> ""
    | _ -> ""

let private translation (c: Culture) (o: MetadataObject) (p: TranslatedProperty) =
    match c.ObjectTranslations.[o, p] with
    | null -> ""
    | t -> orEmpty t.Value

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// The model's cultures, or the named ones (case-insensitively, in the order given).
let cultures (db: Database) (names: string list) : Result<Culture list, TessError> =
    let all = List.ofSeq db.Model.Cultures

    match names with
    | [] -> Ok all
    | names ->
        names
        |> List.distinctBy (fun n -> n.ToLowerInvariant())
        |> List.fold
            (fun acc name ->
                acc
                |> Result.bind (fun xs ->
                    match all |> List.tryFind (fun c -> eqCi c.Name name) with
                    | Some c -> Ok(xs @ [ c ])
                    | None -> Error(ObjectNotFound $"culture:{name}")))
            (Ok [])

type ExportOptions = {
    /// Empty: every culture in the model.
    Cultures: string list
    /// Leave out rows without a translation.
    OnlyTranslated: bool
}

/// The rows to export, culture by culture, objects in model order. Captions are always listed;
/// descriptions and display folders only where the object has one or a translation exists, since
/// there is nothing to translate otherwise.
let export (db: Database) (opts: ExportOptions) : Result<Row list, TessError> =
    cultures db opts.Cultures
    |> Result.map (fun cultures ->
        let entries = objects db

        [
            for c in cultures do
                for e in entries do
                    for p in Serialize.translatableProperties e.Object do
                        let source = sourceValue e.Object p
                        let value = translation c e.Object p

                        let listed =
                            if opts.OnlyTranslated then
                                value <> ""
                            else
                                p = TranslatedProperty.Caption || source <> "" || value <> ""

                        if listed then
                            {
                                Object = e.Ref
                                Culture = c.Name
                                Property = p
                                Source = source
                                Translation = value
                            }
        ])

/// RFC 4180 CSV: fields with commas, quotes or line breaks are quoted, quotes doubled.
module Csv =

    let header = [ "object"; "culture"; "property"; "source"; "translation" ]

    let field (s: string) =
        let needsQuotes =
            s.IndexOfAny([| ','; '"'; '\r'; '\n' |]) >= 0
            || (s <> "" && (Char.IsWhiteSpace s.[0] || Char.IsWhiteSpace s.[s.Length - 1]))

        if needsQuotes then
            "\"" + s.Replace("\"", "\"\"") + "\""
        else
            s

    let record (fields: string list) =
        fields |> List.map field |> String.concat ","

    /// The file: a header row, then one record per row, each ending in a line feed.
    let write (rows: Row list) =
        let sb = StringBuilder()
        sb.Append(record header).Append('\n') |> ignore

        for r in rows do
            sb
                .Append(
                    record [
                        ObjectRef.format r.Object
                        r.Culture
                        propertyName r.Property
                        r.Source
                        r.Translation
                    ]
                )
                .Append('\n')
            |> ignore

        sb.ToString()

    /// Records with the (1-based) line each starts on. Accepts LF, CRLF and CR line breaks and a
    /// byte-order mark; skips blank lines. Line breaks inside quoted fields are kept as they are.
    let parse (text: string) : Result<(int * string list) list, int * string> =
        let text =
            // An ordinal check: a culture-sensitive StartsWith ignores the mark, so matches anything.
            if text.Length > 0 && text.[0] = '\uFEFF' then
                text.Substring 1
            else
                text

        let records = ResizeArray<int * string list>()
        let fields = ResizeArray<string>()
        let current = StringBuilder()
        let mutable line = 1
        let mutable recordLine = 1
        let mutable error = None
        let mutable i = 0

        let endField () =
            fields.Add(current.ToString())
            current.Clear() |> ignore

        let endRecord () =
            endField ()

            // A blank line is one empty field: not a record.
            if not (fields.Count = 1 && fields.[0] = "") then
                records.Add(recordLine, List.ofSeq fields)

            fields.Clear()

        /// The line break at i (CRLF counts as one), if there is one: its length.
        let lineBreak () =
            match text.[i] with
            | '\r' when i + 1 < text.Length && text.[i + 1] = '\n' -> 2
            | '\r'
            | '\n' -> 1
            | _ -> 0

        /// End the record at the line break at i.
        let endLine (n: int) =
            endRecord ()
            i <- i + n
            line <- line + 1
            recordLine <- line

        while error.IsNone && i < text.Length do
            if current.Length = 0 && text.[i] = '"' then
                // A quoted field, up to the closing quote; "" is a quote.
                let start = line
                i <- i + 1
                let mutable closed = false

                while error.IsNone && not closed do
                    if i >= text.Length then
                        error <- Some(start, "a quoted field is never closed (a '\"' is missing)")
                    elif text.[i] = '"' && i + 1 < text.Length && text.[i + 1] = '"' then
                        current.Append('"') |> ignore
                        i <- i + 2
                    elif text.[i] = '"' then
                        i <- i + 1
                        closed <- true
                    else
                        match lineBreak () with
                        | 0 ->
                            current.Append(text.[i]) |> ignore
                            i <- i + 1
                        | n ->
                            current.Append(text, i, n) |> ignore
                            i <- i + n
                            line <- line + 1

                if error.IsNone then
                    if i >= text.Length then
                        endRecord ()
                    else
                        match text.[i], lineBreak () with
                        | ',', _ ->
                            endField ()
                            i <- i + 1
                        | _, n when n > 0 -> endLine n
                        | c, _ ->
                            error <-
                                Some(
                                    line,
                                    $"unexpected '{c}' after a closing quote (a quote inside a quoted field is written \"\")"
                                )
            else
                match text.[i], lineBreak () with
                | ',', _ ->
                    endField ()
                    i <- i + 1
                | _, n when n > 0 -> endLine n
                | c, _ ->
                    current.Append(c) |> ignore
                    i <- i + 1

        match error with
        | Some e -> Error e
        | None ->
            // The last record, unless the text ended with a line break (or the quoted field
            // that ended it was already recorded).
            if current.Length > 0 || fields.Count > 0 then
                endRecord ()

            Ok(List.ofSeq records)

/// The JSON file: an array of {object, culture, property, source, translation} objects.
module Json =

    let write (rows: Row list) =
        let a = JsonArray()

        for r in rows do
            let o = JsonObject()
            o.["object"] <- JsonValue.Create(ObjectRef.format r.Object)
            o.["culture"] <- JsonValue.Create r.Culture
            o.["property"] <- JsonValue.Create(propertyName r.Property)
            o.["source"] <- JsonValue.Create r.Source
            o.["translation"] <- JsonValue.Create r.Translation
            a.Add o

        a.ToJsonString(
            JsonSerializerOptions(
                WriteIndented = true,
                Encoder = Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
            )
        )
        + "\n"

/// A row as read from a file, before it is checked against the model.
type private RawRow = {
    /// Where it is, for messages: the line (CSV) or the item number (JSON).
    Line: int option
    Item: int option
    Object: string
    Culture: string
    Property: string
    Source: string option
    Translation: string
}

let private where (r: RawRow) =
    match r.Item with
    | Some n -> $"item {n}: "
    | None -> ""

let private fail (path: string) (r: RawRow) reason =
    Error(InvalidTranslationFile(path, r.Line, where r + reason))

let private readCsv (path: string) (text: string) : Result<RawRow list, TessError> =
    match Csv.parse text with
    | Error(line, reason) -> Error(InvalidTranslationFile(path, Some line, reason))
    | Ok [] ->
        Error(
            InvalidTranslationFile(
                path,
                None,
                "the file is empty (expected a header row: object,culture,property,source,translation)"
            )
        )
    | Ok((headerLine, header) :: records) ->
        let names = header |> List.map (fun h -> h.Trim().ToLowerInvariant())

        let column name =
            names |> List.tryFindIndex (fun n -> n = name)

        let missing =
            [ "object"; "culture"; "property"; "translation" ]
            |> List.filter (fun n -> (column n).IsNone)

        let duplicate =
            names
            |> List.filter (fun n -> n <> "")
            |> List.countBy id
            |> List.tryFind (fun (_, n) -> n > 1)

        match missing, duplicate with
        | _ :: _, _ ->
            let list = missing |> List.map (fun n -> $"'{n}'") |> String.concat ", "

            Error(
                InvalidTranslationFile(
                    path,
                    Some headerLine,
                    $"the header has no {list} column (expected object,culture,property,source,translation)"
                )
            )
        | [], Some(name, _) ->
            Error(InvalidTranslationFile(path, Some headerLine, $"the header has two '{name}' columns"))
        | [], None ->
            let at name = (column name).Value

            records
            |> List.fold
                (fun acc (line, fields) ->
                    acc
                    |> Result.bind (fun rows ->
                        if fields.Length <> header.Length then
                            Error(
                                InvalidTranslationFile(
                                    path,
                                    Some line,
                                    $"{fields.Length} fields, but the header has {header.Length} (quote values that contain commas)"
                                )
                            )
                        else
                            let get name = fields.[at name]

                            Ok(
                                {
                                    Line = Some line
                                    Item = None
                                    Object = get "object"
                                    Culture = get "culture"
                                    Property = get "property"
                                    Source = column "source" |> Option.map (fun i -> fields.[i])
                                    Translation = get "translation"
                                }
                                :: rows
                            )))
                (Ok [])
            |> Result.map List.rev

let private readJson (path: string) (text: string) : Result<RawRow list, TessError> =
    let bad reason =
        Error(InvalidTranslationFile(path, None, reason))

    let parsed =
        try
            Ok(JsonNode.Parse(text, documentOptions = JsonDocumentOptions(AllowTrailingCommas = true)))
        with :? JsonException as e ->
            Error e.Message

    match parsed with
    | Error message -> bad $"not valid JSON: {message}"
    | Ok(:? JsonArray as items) ->
        items
        |> Seq.mapi (fun i node -> i + 1, node)
        |> Seq.fold
            (fun acc (n, node) ->
                acc
                |> Result.bind (fun rows ->
                    match node with
                    | :? JsonObject as o ->
                        let field (name: string) =
                            o
                            |> Seq.tryFind (fun (KeyValue(k, _)) -> eqCi k name)
                            |> Option.map (fun (KeyValue(_, v)) -> v)

                        let text name (required: bool) =
                            match field name with
                            | None when required -> Error $"item {n}: no '{name}'"
                            | None -> Ok None
                            | Some null -> Ok(Some "")
                            | Some(:? JsonValue as v) when v.GetValueKind() = JsonValueKind.String ->
                                Ok(Some(v.GetValue<string>()))
                            | Some _ -> Error $"item {n}: '{name}' must be a string"

                        match
                            text "object" true,
                            text "culture" true,
                            text "property" true,
                            text "source" false,
                            text "translation" true
                        with
                        | Ok(Some obj), Ok(Some culture), Ok(Some property), Ok source, Ok(Some translation) ->
                            Ok(
                                {
                                    Line = None
                                    Item = Some n
                                    Object = obj
                                    Culture = culture
                                    Property = property
                                    Source = source
                                    Translation = translation
                                }
                                :: rows
                            )
                        | Error e, _, _, _, _
                        | _, Error e, _, _, _
                        | _, _, Error e, _, _
                        | _, _, _, Error e, _
                        | _, _, _, _, Error e -> bad e
                        | _ -> bad $"item {n}: incomplete"
                    | _ -> bad $"item {n}: expected an object {{object, culture, property, translation}}"))
            (Ok [])
        |> Result.map List.rev
    | Ok _ -> bad "expected a JSON array of {object, culture, property, source, translation} objects"

/// The format of a file: by its extension, else by its first character ('[' or '{' is JSON).
let detectFormat (path: string) (text: string) =
    let ext = IO.Path.GetExtension(path).ToLowerInvariant()

    match ext with
    | ".json" -> Format.Json
    | ".csv" -> Format.Csv
    | _ ->
        let t = text.TrimStart('﻿', ' ', '\t', '\r', '\n')

        if t.StartsWith('[') || t.StartsWith('{') then
            Format.Json
        else
            Format.Csv

type ImportSpec = {
    /// The file's name, for messages ("-" for stdin).
    Path: string
    Text: string
    /// None: detect it (see detectFormat).
    Format: Format option
    /// Only import rows of these cultures (empty: all).
    Cultures: string list
    /// Add cultures the model doesn't have, instead of refusing.
    CreateCultures: bool
}

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private lineList (lines: int list) =
    let shown = lines |> List.truncate 10 |> List.map string |> String.concat ", "
    if lines.Length > 10 then shown + ", ..." else shown

let private lineWord (format: Format) (n: int) =
    match format, n with
    | Format.Csv, 1 -> "line"
    | Format.Csv, _ -> "lines"
    | Format.Json, 1 -> "item"
    | Format.Json, _ -> "items"

/// Apply a translation file as one edit: set each row's translation, or remove it where the
/// translation is empty. Rows the file doesn't mention are left as they are.
let import (db: Database) (spec: ImportSpec) : Result<Mutation.Effect, TessError> =
    let format =
        spec.Format |> Option.defaultWith (fun () -> detectFormat spec.Path spec.Text)

    let read =
        match format with
        | Format.Csv -> readCsv spec.Path spec.Text
        | Format.Json -> readJson spec.Path spec.Text

    let fail r reason = fail spec.Path r reason

    read
    |> Result.bind (fun raw ->
        let wanted (r: RawRow) =
            spec.Cultures.IsEmpty || spec.Cultures |> List.exists (eqCi r.Culture)

        let rows, skipped = raw |> List.partition wanted
        let model = db.Model
        let entries = objects db

        // Cultures, in order of first appearance; created ones are added only once all rows check out.
        let culturesResult =
            rows
            |> List.fold
                (fun acc r ->
                    acc
                    |> Result.bind (fun (known: (string * Culture) list, created: Culture list) ->
                        let name = r.Culture.Trim()

                        if known |> List.exists (fun (n, _) -> eqCi n name) then
                            Ok(known, created)
                        else
                            match model.Cultures |> Seq.tryFind (fun c -> eqCi c.Name name) with
                            | Some c -> Ok(known @ [ name, c ], created)
                            | None when name = "" -> fail r "no culture"
                            | None when not spec.CreateCultures ->
                                fail r $"the model has no culture '{name}' (pass --create-cultures to add it)"
                            | None ->
                                match Scaffold.tryCulture name with
                                | None -> fail r $"'{name}' is not a specific culture name, such as da-DK"
                                | Some canonical ->
                                    match created |> List.tryFind (fun c -> c.Name = canonical) with
                                    | Some c -> Ok(known @ [ name, c ], created)
                                    | None ->
                                        let c = new Culture(Name = canonical)
                                        Ok(known @ [ name, c ], created @ [ c ])))
                (Ok([], []))

        culturesResult
        |> Result.bind (fun (known, created) ->
            let cultureOf (r: RawRow) =
                known |> List.find (fun (n, _) -> eqCi n (r.Culture.Trim())) |> snd

            // Exported files name objects by their exact reference: look those up directly, and
            // anything else (another case, DAX-style names) the way `get` does.
            let resolved =
                Collections.Generic.Dictionary<string, Result<ObjectEntry, TessError>>()

            for e in entries do
                resolved.[ObjectRef.format e.Ref] <- Ok e

            let resolve (s: string) =
                match resolved.TryGetValue s with
                | true, r -> r
                | _ ->
                    let r = Selector.resolve entries s
                    resolved.[s] <- r
                    r

            // Check every row, and pair it with what it sets.
            let checkedRows: Result<(RawRow * ObjectEntry * Culture * TranslatedProperty) list, TessError> =
                rows
                |> List.fold
                    (fun acc r ->
                        acc
                        |> Result.bind (fun xs ->
                            match resolve r.Object with
                            | Error e -> fail r (TessError.message e)
                            | Ok e ->
                                match tryParseProperty r.Property with
                                | None ->
                                    fail
                                        r
                                        $"unknown property '{r.Property}' (expected Caption, Description or DisplayFolder)"
                                | Some p ->
                                    match Serialize.translatableProperties e.Object with
                                    | [] -> fail r $"{ObjectRef.format e.Ref} can't be translated"
                                    | props when not (List.contains p props) ->
                                        let names = props |> List.map propertyName |> String.concat ", "

                                        fail
                                            r
                                            $"{ObjectRef.format e.Ref} has no translatable {propertyName p} (only {names})"
                                    | _ -> Ok((r, e, cultureOf r, p) :: xs)))
                    (Ok [])
                |> Result.map List.rev

            checkedRows
            |> Result.bind (fun checkedRows ->
                // The same property twice must agree.
                let conflict =
                    checkedRows
                    |> List.groupBy (fun (_, e, c, p) -> e.Ref, c.Name, p)
                    |> List.tryPick (fun (_, group) ->
                        match group |> List.distinctBy (fun (r, _, _, _) -> r.Translation) with
                        | (first, _, _, _) :: (second, _, _, _) :: _ -> Some(first, second)
                        | _ -> None)

                match conflict with
                | Some(first, second) ->
                    let other =
                        match first.Line, first.Item with
                        | Some l, _ -> $"line {l}"
                        | _, Some n -> $"item {n}"
                        | _ -> "an earlier row"

                    fail second $"a different translation of the same property as {other}"
                | None ->
                    for c in created do
                        model.Cultures.Add c

                    let mutable set = 0
                    let mutable removed = 0
                    let mutable unchanged = 0
                    let stale = ResizeArray<int>()

                    for r, e, c, p in checkedRows |> List.distinctBy (fun (_, e, c, p) -> e.Ref, c.Name, p) do
                        let current = translation c e.Object p

                        match r.Source with
                        // A blank source (a cleared or hand-written column) isn't checked.
                        | Some s when s <> "" && s <> sourceValue e.Object p ->
                            stale.Add(defaultArg r.Line (defaultArg r.Item 0))
                        | _ -> ()

                        if r.Translation = current then
                            unchanged <- unchanged + 1
                        elif r.Translation = "" then
                            c.ObjectTranslations.SetTranslation(e.Object, p, "")
                            removed <- removed + 1
                        else
                            c.ObjectTranslations.SetTranslation(e.Object, p, r.Translation)
                            set <- set + 1

                    let names =
                        checkedRows
                        |> List.map (fun (_, _, c, _) -> c.Name)
                        |> List.distinct
                        |> String.concat ", "

                    let where = lineWord format stale.Count

                    Ok {
                        Mutation.Effect.none with
                            Notes = [
                                for c in created do
                                    $"Added {ObjectRef.format (ObjectRef.modelLevel ObjectKind.Culture c.Name)}"
                                if checkedRows.IsEmpty then
                                    "No rows to import"
                                else
                                    $"""Translations ({names}): {set} set, {removed} removed, {unchanged} unchanged"""
                                if not skipped.IsEmpty then
                                    $"""Skipped {plural skipped.Length "row"} of other cultures"""
                            ]
                            Warnings = [
                                if stale.Count > 0 then
                                    $"""the source text of {plural stale.Count "row"} no longer matches the model ({where} {lineList (List.ofSeq stale)}): it changed after the export, so check those translations"""
                                for name in spec.Cultures do
                                    if not (raw |> List.exists (fun r -> eqCi r.Culture name)) then
                                        $"the file has no rows for culture '{name}'"
                            ]
                    })))

/// How much of each culture is translated: (culture, property, translated, total) over the rows
/// `export` lists.
let coverage (db: Database) : (string * TranslatedProperty * int * int) list =
    match
        export db {
            Cultures = []
            OnlyTranslated = false
        }
    with
    | Error _ -> []
    | Ok rows -> [
        for c in db.Model.Cultures do
            for p in properties do
                let these = rows |> List.filter (fun r -> r.Culture = c.Name && r.Property = p)
                let translated = these |> List.filter (fun r -> r.Translation <> "") |> List.length
                c.Name, p, translated, these.Length
      ]
