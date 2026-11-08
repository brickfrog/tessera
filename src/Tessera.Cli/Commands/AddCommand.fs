module Tessera.Cli.AddCommand

open System
open Argu
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.AddObject
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "add a measure, column, table, hierarchy, relationship, role... (dry run unless --save)."

type Args =
    | [<MainCommand; ExactlyOnce>] Add of model: string * object: string
    | [<AltCommandLine("-e")>] Expression of dax: string
    | [<AltCommandLine("-m")>] M_Expression of m: string
    | Format_String of format: string
    | Display_Folder of folder: string
    | Description of text: string
    | Hidden
    | Data_Type of ``type``: string
    | Source_Column of name: string
    | Mode of mode: string
    | Calculation_Group
    | Precedence of precedence: int
    | Column of name: string
    | Levels of columns: string
    | Ordinal of ordinal: int
    | Format_String_Expression of dax: string
    | From of column: string
    | To of column: string
    | Inactive
    | Cross_Filter of direction: string
    | Cardinality of cardinality: string
    | Permission of permission: string
    | Filter of table_filter: string
    | Include of object: string
    | If_Not_Exists
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ ->
                "the model, and the reference of the new object, e.g. measure:Sales/Margin (`relationship` alone: a GUID name)."
            | Expression _ ->
                "DAX: a measure, calculated column, calculated table, calculation item or function (`-` reads stdin)."
            | M_Expression _ -> "M (Power Query): an import table, a partition or a named expression (`-` reads stdin)."
            | Format_String _ -> "format string (measure, column)."
            | Display_Folder _ -> "display folder (measure, column, hierarchy)."
            | Description _ -> "description."
            | Hidden -> "hide the object (table, column, measure, hierarchy, function)."
            | Data_Type _ ->
                "a data column's type: string, int64, double, decimal, datetime, boolean, binary (required without -e)."
            | Source_Column _ -> "a data column's source column (default: the column's name)."
            | Mode _ -> "storage mode of an M table or partition: import (default), directquery, dual."
            | Calculation_Group -> "table: create a calculation group."
            | Precedence _ -> "calculation group precedence (default: one more than the highest in the model)."
            | Column _ ->
                "level: its column (default: the level's name); calculation group: its column's name (default Name)."
            | Levels _ -> "hierarchy: its levels' columns, top first (comma-separated or repeated)."
            | Ordinal _ -> "level or calculation item: its position, from 0 (default: last)."
            | Format_String_Expression _ -> "calculation item: a dynamic format string (DAX)."
            | From _ -> "relationship: the many-side column, e.g. 'Sales'[DateKey]."
            | To _ -> "relationship: the one-side column, e.g. 'Date'[DateKey]."
            | Inactive -> "relationship: create it inactive."
            | Cross_Filter _ -> "relationship: single or both (default single; both for one-to-one)."
            | Cardinality _ -> "relationship: many-to-one (default), one-to-one or many-to-many."
            | Permission _ -> "role: read (default), readrefresh, refresh, administrator or none."
            | Filter _ -> "role: a row filter, 'Table'=<DAX> (repeatable)."
            | Include _ -> "perspective: a table, column, measure or hierarchy to include (repeatable)."
            | If_Not_Exists -> "if the object already exists, do nothing (exit 0) instead of failing."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

/// The options that only some kinds take, with the kinds that take them.
let private kindOptions: (string * (ParseResults<Args> -> bool) * ObjectKind list) list = [
    "--expression",
    (fun a -> a.Contains Args.Expression),
    [
        ObjectKind.Measure
        ObjectKind.Column
        ObjectKind.Table
        ObjectKind.CalculationItem
        ObjectKind.Function
    ]
    "--m-expression",
    (fun a -> a.Contains Args.M_Expression),
    [ ObjectKind.Table; ObjectKind.Expression; ObjectKind.Partition ]
    "--format-string",
    (fun a -> a.Contains Args.Format_String),
    addableKinds |> List.filter (fun k -> supportsMetadata k "formatString")
    "--display-folder",
    (fun a -> a.Contains Args.Display_Folder),
    addableKinds |> List.filter (fun k -> supportsMetadata k "displayFolder")
    "--description",
    (fun a -> a.Contains Args.Description),
    addableKinds |> List.filter (fun k -> supportsMetadata k "description")
    "--hidden", (fun a -> a.Contains Args.Hidden), addableKinds |> List.filter (fun k -> supportsMetadata k "hidden")
    "--data-type", (fun a -> a.Contains Args.Data_Type), [ ObjectKind.Column ]
    "--source-column", (fun a -> a.Contains Args.Source_Column), [ ObjectKind.Column ]
    "--mode", (fun a -> a.Contains Args.Mode), [ ObjectKind.Table; ObjectKind.Partition ]
    "--calculation-group", (fun a -> a.Contains Args.Calculation_Group), [ ObjectKind.Table ]
    "--precedence", (fun a -> a.Contains Args.Precedence), [ ObjectKind.Table ]
    "--column", (fun a -> a.Contains Args.Column), [ ObjectKind.Level; ObjectKind.Table ]
    "--levels", (fun a -> a.Contains Args.Levels), [ ObjectKind.Hierarchy ]
    "--ordinal", (fun a -> a.Contains Args.Ordinal), [ ObjectKind.Level; ObjectKind.CalculationItem ]
    "--format-string-expression", (fun a -> a.Contains Args.Format_String_Expression), [ ObjectKind.CalculationItem ]
    "--from", (fun a -> a.Contains Args.From), [ ObjectKind.Relationship ]
    "--to", (fun a -> a.Contains Args.To), [ ObjectKind.Relationship ]
    "--inactive", (fun a -> a.Contains Args.Inactive), [ ObjectKind.Relationship ]
    "--cross-filter", (fun a -> a.Contains Args.Cross_Filter), [ ObjectKind.Relationship ]
    "--cardinality", (fun a -> a.Contains Args.Cardinality), [ ObjectKind.Relationship ]
    "--permission", (fun a -> a.Contains Args.Permission), [ ObjectKind.Role ]
    "--filter", (fun a -> a.Contains Args.Filter), [ ObjectKind.Role ]
    "--include", (fun a -> a.Contains Args.Include), [ ObjectKind.Perspective ]
]

/// The new object's reference. `relationship` alone (no name) gets a GUID name.
let private parseRef (text: string) =
    match ObjectKind.tryParse text with
    | Some ObjectKind.Relationship ->
        Ok {
            Kind = ObjectKind.Relationship
            Path = []
        }
    | Some k when k <> ObjectKind.Model ->
        let example =
            match ObjectKind.arity k with
            | 1 -> $"{ObjectKind.token k}:<name>"
            | 2 -> $"{ObjectKind.token k}:<table>/<name>"
            | _ -> $"{ObjectKind.token k}:<table>/<hierarchy>/<name>"

        Error $"'{text}' needs a name: {example}"
    | _ ->
        match ObjectRef.parse text with
        | Ok r -> Ok r
        | Error e -> Error(TessError.message e)

let private token (what: string) (choices: (string list * 'T) list) (value: string) =
    let v = value.Trim().ToLowerInvariant()

    match choices |> List.tryFind (fun (names, _) -> List.contains v names) with
    | Some(_, x) -> Ok x
    | None ->
        let known = choices |> List.map (fst >> List.head) |> String.concat ", "
        Error $"unknown {what} '{value}' (expected {known})"

let private dataType =
    token "data type" [
        [ "string"; "text" ], DataType.String
        [ "int64"; "int"; "integer"; "whole" ], DataType.Int64
        [ "double" ], DataType.Double
        [ "decimal"; "currency"; "fixed" ], DataType.Decimal
        [ "datetime"; "date" ], DataType.DateTime
        [ "boolean"; "bool" ], DataType.Boolean
        [ "binary" ], DataType.Binary
    ]

let private mode =
    token "mode" [
        [ "import" ], ModeType.Import
        [ "directquery"; "direct-query"; "dq" ], ModeType.DirectQuery
        [ "dual" ], ModeType.Dual
    ]

let private permission =
    token "permission" [
        [ "read" ], ModelPermission.Read
        [ "readrefresh"; "read-refresh" ], ModelPermission.ReadRefresh
        [ "refresh" ], ModelPermission.Refresh
        [ "administrator"; "admin" ], ModelPermission.Administrator
        [ "none" ], ModelPermission.None
    ]

let private crossFilter =
    token "cross-filter direction" [ [ "single"; "one" ], CrossFilter.Single; [ "both" ], CrossFilter.Both ]

let private cardinality =
    token "cardinality" [
        [ "many-to-one"; "manytoone"; "m:1"; "*:1" ], Cardinality.ManyToOne
        [ "one-to-one"; "onetoone"; "1:1" ], Cardinality.OneToOne
        [ "many-to-many"; "manytomany"; "m:m"; "*:*" ], Cardinality.ManyToMany
    ]

/// 'Table'=<DAX> or Table=<DAX>; in a quoted name, '' is a quote.
let private roleFilter (text: string) =
    let s = text.TrimStart()
    let bad = Error $"--filter '{text}': expected 'Table'=<DAX>"

    let split (table: string) (rest: string) =
        let rest = rest.TrimStart()

        if table.Trim() = "" || not (rest.StartsWith "=") then
            bad
        else
            Ok(table.Trim(), rest.Substring(1).Trim())

    if s.StartsWith "'" then
        let sb = Text.StringBuilder()
        let mutable i = 1
        let mutable closed = -1

        while closed < 0 && i < s.Length do
            if s.[i] = '\'' then
                if i + 1 < s.Length && s.[i + 1] = '\'' then
                    sb.Append('\'') |> ignore
                    i <- i + 2
                else
                    closed <- i
            else
                sb.Append(s.[i]) |> ignore
                i <- i + 1

        if closed < 0 then
            bad
        else
            split (sb.ToString()) (s.Substring(closed + 1))
    else
        match s.IndexOf '=' with
        | -1 -> bad
        | i -> split (s.Substring(0, i)) (s.Substring i)

let private sequence (xs: Result<'a, string> list) =
    List.foldBack (fun x acc -> Result.bind (fun v -> Result.map (fun vs -> v :: vs) acc) x) xs (Ok [])

/// `-` reads the value from stdin (once); a trailing line break is dropped.
let private reader () =
    let mutable used = false

    fun (flag: string) (value: string) ->
        if value <> "-" then
            Ok value
        elif used then
            Error $"{flag} -: only one option can read stdin"
        else
            used <- true
            Ok(Console.In.ReadToEnd().TrimEnd('\r', '\n'))

let private details (args: ParseResults<Args>) (kind: ObjectKind) : Result<Details, string> =
    let read = reader ()

    let get flag (a: Quotations.Expr<string -> Args>) =
        args.TryGetResult a |> Option.map (read flag)

    let flip =
        function
        | Some(Ok v) -> Ok(Some v)
        | Some(Error e) -> Error e
        | None -> Ok None

    let dax = get "--expression" <@ Args.Expression @> |> flip
    let m = get "--m-expression" <@ Args.M_Expression @> |> flip

    let need (flag: string) (value: Result<string option, string>) =
        value
        |> Result.bind (function
            | Some v -> Ok v
            | None -> Error $"adding a {ObjectKind.token kind} needs {flag}")

    let opt (f: string -> Result<'a, string>) (v: string option) =
        match v with
        | Some v -> f v |> Result.map Some
        | None -> Ok None

    match kind with
    | ObjectKind.Measure -> need "-e/--expression <DAX>" dax |> Result.map Details.Measure
    | ObjectKind.Column ->
        dax
        |> Result.bind (function
            | Some e ->
                if args.Contains Args.Data_Type || args.Contains Args.Source_Column then
                    Error
                        "a calculated column (-e) gets its type from its expression; drop --data-type and --source-column"
                else
                    Ok(Details.Column(ColumnSource.Calculated e))
            | None ->
                match args.TryGetResult Args.Data_Type with
                | None ->
                    Error "adding a column needs -e/--expression <DAX> (calculated) or --data-type <type> (data column)"
                | Some t ->
                    dataType t
                    |> Result.map (fun t -> Details.Column(ColumnSource.Data(t, args.TryGetResult Args.Source_Column))))
    | ObjectKind.Table ->
        Result.bind
            (fun dax ->
                Result.bind
                    (fun m ->
                        let group = args.Contains Args.Calculation_Group

                        match dax, m, group with
                        | Some e, None, false when not (args.Contains Args.Mode) ->
                            Ok(Details.Table(TableSource.Calculated e))
                        | None, Some m, false ->
                            opt mode (args.TryGetResult Args.Mode)
                            |> Result.map (fun mode ->
                                Details.Table(TableSource.M(m, defaultArg mode ModeType.Import)))
                        | None, None, true when not (args.Contains Args.Mode) ->
                            Ok(
                                Details.Table(
                                    TableSource.CalculationGroup(
                                        args.TryGetResult Args.Precedence,
                                        args.TryGetResult Args.Column
                                    )
                                )
                            )
                        | Some _, None, false -> Error "--mode applies to M tables (-m); a calculated table is import"
                        | None, None, true -> Error "--mode doesn't apply to a calculation group"
                        | None, None, false ->
                            Error
                                "adding a table needs one of -e <DAX> (calculated), -m <M> (import) or --calculation-group"
                        | _ -> Error "use only one of -e, -m and --calculation-group")
                    m)
            dax
        |> Result.bind (fun d ->
            match d with
            | Details.Table(TableSource.CalculationGroup _) -> Ok d
            | _ when args.Contains Args.Precedence || args.Contains Args.Column ->
                Error "--precedence and --column apply to calculation groups (--calculation-group)"
            | _ -> Ok d)
    | ObjectKind.Hierarchy ->
        match parseList (args.GetResults Args.Levels) with
        | [] -> Error "adding a hierarchy needs --levels <column,column,...>"
        | levels -> Ok(Details.Hierarchy levels)
    | ObjectKind.Level -> Ok(Details.Level(args.TryGetResult Args.Column, args.TryGetResult Args.Ordinal))
    | ObjectKind.CalculationItem ->
        need "-e/--expression <DAX>" dax
        |> Result.bind (fun e ->
            get "--format-string-expression" <@ Args.Format_String_Expression @>
            |> flip
            |> Result.map (fun f -> Details.CalculationItem(e, f, args.TryGetResult Args.Ordinal)))
    | ObjectKind.Relationship ->
        match args.TryGetResult Args.From, args.TryGetResult Args.To with
        | Some from, Some ``to`` ->
            Result.bind
                (fun c ->
                    opt crossFilter (args.TryGetResult Args.Cross_Filter)
                    |> Result.map (fun f ->
                        Details.Relationship {
                            From = from
                            To = ``to``
                            Inactive = args.Contains Args.Inactive
                            Cardinality = defaultArg c Cardinality.ManyToOne
                            CrossFilter = f
                        }))
                (opt cardinality (args.TryGetResult Args.Cardinality))
        | _ -> Error "adding a relationship needs --from <column> and --to <column>, e.g. --from \"'Sales'[DateKey]\""
    | ObjectKind.Role ->
        Result.bind
            (fun p ->
                args.GetResults Args.Filter
                |> List.map roleFilter
                |> sequence
                |> Result.map (fun filters -> Details.Role(defaultArg p ModelPermission.Read, filters)))
            (opt permission (args.TryGetResult Args.Permission))
    | ObjectKind.Perspective -> Ok(Details.Perspective(parseList (args.GetResults Args.Include)))
    | ObjectKind.Culture -> Ok Details.Culture
    | ObjectKind.Expression -> need "-m/--m-expression <M>" m |> Result.map Details.Expression
    | ObjectKind.Function -> need "-e/--expression <DAX>" dax |> Result.map Details.Function
    | ObjectKind.Partition ->
        Result.bind
            (fun m ->
                opt mode (args.TryGetResult Args.Mode)
                |> Result.map (fun mode -> Details.Partition(m, mode)))
            (need "-m/--m-expression <M>" m)
    | k -> Error $"tess can't add a {ObjectKind.token k}"

let private spec (args: ParseResults<Args>) (objectText: string) =
    parseRef objectText
    |> Result.bind (fun r ->
        let wrong =
            kindOptions
            |> List.filter (fun (_, given, kinds) -> given args && not (List.contains r.Kind kinds))
            |> List.map (fun (flag, _, _) -> flag)

        match wrong with
        | [] when List.contains r.Kind addableKinds ->
            details args r.Kind
            |> Result.map (fun d -> {
                Ref = r
                Details = d
                Metadata = {
                    Description = args.TryGetResult Args.Description
                    DisplayFolder = args.TryGetResult Args.Display_Folder
                    FormatString = args.TryGetResult Args.Format_String
                    Hidden = args.Contains Args.Hidden
                }
                IfNotExists = args.Contains Args.If_Not_Exists
            })
        | [] -> Error $"tess can't add a {ObjectKind.token r.Kind}"
        | flags ->
            let list = String.concat ", " flags
            Error $"{list} doesn't apply to a {ObjectKind.token r.Kind}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let model, objectText = args.GetResult Args.Add

    let opts =
        Mutation.options
            (args.Contains Args.Save)
            (args.TryGetResult Args.Save_To)
            (args.Contains Args.Force)
            (args.Contains Args.Stat)
            (args.Contains Args.Name_Only)

    match
        opts
        |> Result.bind (fun o -> spec args objectText |> Result.map (fun s -> o, s))
    with
    | Error message -> UsageError message
    | Ok(opts, spec) -> Mutation.run ctx "add" opts model (fun db -> AddObject.add db spec)
