module Tessera.Cli.RemoveCommand

open Argu
open Tessera
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "remove objects, refusing while other DAX refers to them (dry run unless --save)."

type Args =
    | [<MainCommand; ExactlyOnce>] Remove of model_and_objects: string list
    | Cascade
    | If_Exists
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Remove _ -> "the model, then one or more objects to remove (any form `get` accepts)."
            | Cascade ->
                "also remove what refers to them: measures, calculated columns and tables, calculation items and "
                + "functions, transitively."
            | If_Exists -> "skip objects that don't exist (with a note) instead of failing."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force ->
                "remove even what other DAX refers to, leaving those references broken; save even if the change "
                + "introduces new errors; with --save-to, overwrite a non-empty destination."
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let opts =
        Mutation.options
            (args.Contains Args.Save)
            (args.TryGetResult Args.Save_To)
            (args.Contains Args.Force)
            (args.Contains Args.Stat)
            (args.Contains Args.Name_Only)

    match args.GetResult Args.Remove, opts with
    | [], _
    | [ _ ], _ -> UsageError "expected the model and at least one object to remove"
    | _, Error message -> UsageError message
    | model :: objects, Ok opts ->
        let spec: RemoveObject.RemoveSpec = {
            Selectors = objects
            Cascade = args.Contains Args.Cascade
            IfExists = args.Contains Args.If_Exists
            Force = opts.Force
        }

        Mutation.run ctx "remove" opts model (fun db -> RemoveObject.remove db spec)
