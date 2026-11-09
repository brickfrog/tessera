module Tessera.Cli.TranslationsCommand

open System
open System.IO
open System.Text
open Argu
open Tessera
open Tessera.Cli.Common
open Tessera.Cli.Output

/// One-line description, shown in `tess --help`.
let summary =
    "export translations to a CSV or JSON file for translators, import them back, or list cultures."

type FileFormat =
    | Csv
    | Json

let private formatOf =
    function
    | Csv -> Translations.Format.Csv
    | Json -> Translations.Format.Json

type ExportArgs =
    | [<MainCommand; ExactlyOnce>] Model of model: string
    | Culture of culture: string
    | Format of format: FileFormat
    | [<AltCommandLine("-o")>] Output of path: string
    | Only_Translated
    | Bom
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "the model."
            | Culture _ -> "only these cultures (repeatable or comma-separated; default: all)."
            | Format _ -> "csv (default) or json; inferred from --output's extension, and json with --json."
            | Output _ -> "write the file here instead of stdout."
            | Only_Translated -> "leave out rows that have no translation yet."
            | Bom -> "start the file with a UTF-8 byte-order mark (helps Excel detect UTF-8)."
            | Force -> "overwrite an existing --output file."

type ImportArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * file: string
    | Culture of culture: string
    | Format of format: FileFormat
    | Create_Cultures
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the file to import (`-` reads stdin)."
            | Culture _ -> "only import rows of these cultures (repeatable or comma-separated)."
            | Format _ -> "csv or json (default: from the file's extension, else its content)."
            | Create_Cultures -> "add cultures the model doesn't have yet, instead of refusing."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

type CulturesArgs =
    | [<MainCommand; ExactlyOnce>] Model of model: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "the model."

type Args =
    | [<CliPrefix(CliPrefix.None)>] Export of ParseResults<ExportArgs>
    | [<CliPrefix(CliPrefix.None)>] Import of ParseResults<ImportArgs>
    | [<CliPrefix(CliPrefix.None)>] Cultures of ParseResults<CulturesArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Export _ ->
                "write one row per object, culture and translatable property (caption, description, display "
                + "folder), with the untranslated text and the translation, if any."
            | Import _ -> "set (or, where empty, remove) the translations in a file (dry run unless --save)."
            | Cultures _ -> "list the model's cultures and how much of each is translated."

let private export (ctx: Ctx) (args: ParseResults<ExportArgs>) =
    let output = args.TryGetResult ExportArgs.Output

    let format =
        match args.TryGetResult ExportArgs.Format, output with
        | Some f, _ -> formatOf f
        | None, Some path when Path.GetExtension(path).Equals(".json", StringComparison.OrdinalIgnoreCase) ->
            Translations.Format.Json
        | None, None when ctx.Json -> Translations.Format.Json
        | None, _ -> Translations.Format.Csv

    match format, output with
    | Translations.Format.Csv, None when ctx.Json ->
        UsageError "--json writes the rows as JSON; to write CSV with a JSON summary, pass --output <file>"
    | format, output ->
        match ModelIO.loadPath (args.GetResult ExportArgs.Model) with
        | Error e -> Failed e
        | Ok(_, db) ->
            let opts: Translations.ExportOptions = {
                Cultures = parseList (args.GetResults ExportArgs.Culture)
                OnlyTranslated = args.Contains ExportArgs.Only_Translated
            }

            match Translations.export db opts with
            | Error e -> Failed e
            | Ok rows ->
                let text =
                    match format with
                    | Translations.Format.Csv -> Translations.Csv.write rows
                    | Translations.Format.Json -> Translations.Json.write rows

                let cultures = rows |> List.map (fun r -> r.Culture) |> List.distinct

                if db.Model.Cultures.Count = 0 then
                    ctx.Err.WriteLine(
                        "tess: warning: the model has no cultures; add one with `tess add <model> culture:<name>` "
                        + "(e.g. culture:da-DK), or import with --create-cultures"
                    )

                match output with
                | None ->
                    if args.Contains ExportArgs.Bom then
                        ctx.Out.Write("﻿")

                    ctx.Out.Write(text)
                    Done
                | Some path ->
                    let full = Path.GetFullPath path

                    if File.Exists full && not (args.Contains ExportArgs.Force) then
                        Failed(DestinationNotEmpty full)
                    else
                        let written =
                            try
                                File.WriteAllText(full, text, UTF8Encoding(args.Contains ExportArgs.Bom))
                                Ok()
                            with e ->
                                Error(SaveFailed(full, e.Message))

                        match written with
                        | Error e -> Failed e
                        | Ok() ->
                            if ctx.Json then
                                writeJson
                                    ctx.Out
                                    (obj [
                                        "command", str "translations export"
                                        "format", str (Translations.Format.token format)
                                        "path", str full
                                        "rows", num rows.Length
                                        "translated",
                                        num (rows |> List.filter (fun r -> r.Translation <> "") |> List.length)
                                        "cultures", arr (cultures |> List.map str)
                                    ])
                            else
                                let n = if rows.Length = 1 then "1 row" else $"{rows.Length} rows"
                                let names = String.concat ", " cultures
                                ctx.Err.WriteLine($"Wrote {n} ({names})  {full}")

                            Done

let private import (ctx: Ctx) (args: ParseResults<ImportArgs>) =
    let model, file = args.GetResult ImportArgs.Target

    let opts =
        Mutation.options
            (args.Contains ImportArgs.Save)
            (args.TryGetResult ImportArgs.Save_To)
            (args.Contains ImportArgs.Force)
            (args.Contains ImportArgs.Stat)
            (args.Contains ImportArgs.Name_Only)

    let text =
        if file = "-" then
            Ok(Console.In.ReadToEnd())
        elif File.Exists file then
            try
                Ok(File.ReadAllText file)
            with e ->
                Error(LoadFailed(file, e.Message))
        else
            Error(PathNotFound file)

    match opts, text with
    | Error message, _ -> UsageError message
    | _, Error e -> Failed e
    | Ok opts, Ok text ->
        let spec: Translations.ImportSpec = {
            Path = file
            Text = text
            Format = args.TryGetResult ImportArgs.Format |> Option.map formatOf
            Cultures = parseList (args.GetResults ImportArgs.Culture)
            CreateCultures = args.Contains ImportArgs.Create_Cultures
        }

        Mutation.run ctx "translations import" opts model (fun db -> Translations.import db spec)

let private cultures (ctx: Ctx) (args: ParseResults<CulturesArgs>) =
    match ModelIO.loadPath (args.GetResult CulturesArgs.Model) with
    | Error e -> Failed e
    | Ok(_, db) ->
        let coverage = Translations.coverage db

        let byCulture =
            [ for c in db.Model.Cultures -> c.Name ]
            |> List.map (fun name -> name, coverage |> List.filter (fun (c, _, _, _) -> c = name))

        if ctx.Json then
            writeJson
                ctx.Out
                (arr [
                    for name, props in byCulture ->
                        obj [
                            "ref", str (ObjectRef.format (ObjectRef.modelLevel ObjectKind.Culture name))
                            "name", str name
                            "translated", num (props |> List.sumBy (fun (_, _, n, _) -> n))
                            "total", num (props |> List.sumBy (fun (_, _, _, n) -> n))
                            "properties",
                            obj [
                                for _, p, translated, total in props ->
                                    Serialize.translatedPropertyToken p,
                                    obj [ "translated", num translated; "total", num total ]
                            ]
                        ]
                ])
        else
            for name, props in byCulture do
                let parts =
                    props
                    |> List.map (fun (_, p, translated, total) ->
                        let label =
                            match p with
                            | Microsoft.AnalysisServices.Tabular.TranslatedProperty.Caption -> "captions"
                            | Microsoft.AnalysisServices.Tabular.TranslatedProperty.Description -> "descriptions"
                            | _ -> "display folders"

                        $"{translated}/{total} {label}")

                ctx.Out.WriteLine(
                    $"""{ObjectRef.format (ObjectRef.modelLevel ObjectKind.Culture name)}  {String.concat ", " parts}"""
                )

        Done

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetSubCommand() with
    | Some(Export a) -> export ctx a
    | Some(Import a) -> import ctx a
    | Some(Cultures a) -> cultures ctx a
    | None -> UsageError(args.Parser.PrintUsage())
