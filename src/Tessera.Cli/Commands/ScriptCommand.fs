/// `tess script export|apply`: DAX scripts, Tabular Editor 3's text format for many objects' DAX and
/// properties in one file.
module Tessera.Cli.ScriptCommand

open System
open System.IO
open System.Text
open Argu
open Tessera
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "write DAX objects to a DAX script (export), or create and update them from one (apply)."

type ExportArgs =
    | [<MainCommand; ExactlyOnce>] Targets of model_and_objects: string list
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | [<AltCommandLine("-o")>] Output of path: string
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Targets _ ->
                "the model, then the objects to export (any form `get` accepts; a table stands for its "
                + "measures, calculated columns and its own DAX). Default: every DAX object."
            | Kind _ -> "only these kinds (repeatable or comma-separated): measure, column, table, calcitem, function."
            | Table _ -> "only objects in this table (repeatable)."
            | Output _ -> "write the script to this file (default: stdout)."
            | Force -> "overwrite an existing --output file."

type ApplyArgs =
    | [<MainCommand; ExactlyOnce>] Files of model: string * script: string
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Files _ -> "the model, and the DAX script (`-` reads it from stdin)."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

type Args =
    | [<CliPrefix(CliPrefix.None)>] Export of ParseResults<ExportArgs>
    | [<CliPrefix(CliPrefix.None)>] Apply of ParseResults<ApplyArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Export _ ->
                "write measures, calculated columns and tables, calculation groups and functions as a DAX script."
            | Apply _ ->
                "create or update the objects a DAX script defines; others are left alone (dry run unless --save)."

let private utf8 = UTF8Encoding(false)

let private export (ctx: Ctx) (args: ParseResults<ExportArgs>) =
    match args.GetResult ExportArgs.Targets, parseKinds (args.GetResults ExportArgs.Kind) with
    | [], _ -> UsageError "expected the model"
    | _, Error message -> UsageError message
    | _, Ok kinds when
        kinds
        |> List.exists (fun k -> not (List.contains k ExportScript.scriptableKinds))
        ->
        let allowed =
            ExportScript.scriptableKinds |> List.map ObjectKind.token |> String.concat ", "

        UsageError $"a DAX script holds only these kinds: {allowed}"
    | model :: selectors, Ok kinds ->
        match Source.load ctx model with
        | Error e -> Failed e
        | Ok(_, db) ->
            let selection: ExportScript.Selection = {
                Selectors = selectors
                Kinds = kinds
                Tables = args.GetResults ExportArgs.Table
            }

            match ExportScript.export db selection with
            | Error e -> Failed e
            | Ok exported ->
                let output = args.TryGetResult ExportArgs.Output

                let written =
                    match output with
                    | None -> Ok()
                    | Some path when File.Exists path && not (args.Contains ExportArgs.Force) ->
                        Error(DestinationNotEmpty(Path.GetFullPath path))
                    | Some path ->
                        try
                            File.WriteAllText(path, exported.Text, utf8) |> Ok
                        with e ->
                            Error(SaveFailed(path, e.Message))

                match written with
                | Error e -> Failed e
                | Ok() ->
                    let refs = exported.Statements |> List.map (ExportScript.refOf >> ObjectRef.format)

                    if ctx.Json then
                        writeJson
                            ctx.Out
                            (obj [
                                "command", str "script export"
                                "objects", arr (refs |> List.map str)
                                "warnings", arr (exported.Warnings |> List.map str)
                                "output", optStr output
                                "script", str exported.Text
                            ])
                    else
                        if output.IsNone then
                            ctx.Out.Write(exported.Text)

                        for w in exported.Warnings do
                            ctx.Err.WriteLine($"tess: warning: {w}")

                        match output, refs with
                        | _, [] -> ctx.Err.WriteLine("Nothing to script: no DAX objects matched.")
                        | Some path, _ ->
                            let n =
                                if refs.Length = 1 then
                                    "1 object"
                                else
                                    $"{refs.Length} objects"

                            ctx.Err.WriteLine($"Wrote {n} to {path}")
                        | None, _ -> ()

                    Done

let private apply (ctx: Ctx) (args: ParseResults<ApplyArgs>) =
    let model, script = args.GetResult ApplyArgs.Files

    let opts =
        Mutation.options
            (args.Contains ApplyArgs.Save)
            (args.TryGetResult ApplyArgs.Save_To)
            (args.Contains ApplyArgs.Force)
            (args.Contains ApplyArgs.Stat)
            (args.Contains ApplyArgs.Name_Only)

    let text =
        if script = "-" then
            Ok("<stdin>", Console.In.ReadToEnd())
        elif File.Exists script then
            try
                Ok(script, File.ReadAllText script)
            with e ->
                Error(LoadFailed(script, e.Message))
        else
            Error(PathNotFound script)

    match opts, text with
    | Error message, _ -> UsageError message
    | _, Error e -> Failed e
    | Ok opts, Ok(source, text) ->
        // A script that doesn't parse is reported before the model is even loaded.
        match DaxScript.parse text with
        | Error e -> Failed(InvalidScript(source, e.Position.Line, e.Position.Column, e.Message))
        | Ok statements -> Mutation.run ctx "script apply" opts model (fun db -> ApplyScript.apply source db statements)

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetSubCommand() with
    | Some(Export a) -> export ctx a
    | Some(Apply a) -> apply ctx a
    | _ -> UsageError(args.Parser.PrintUsage())
