module Tessera.Cli.MoveCommand

open Argu
open Tessera
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "move a measure to another table, rewriting references that name its old table (dry run unless --save)."

type Args =
    | [<MainCommand; ExactlyOnce>] Move of model: string * measure: string * table: string
    | Display_Folder of folder: string
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Move _ -> "the model, the measure to move (any form `get` accepts), and the table to move it to."
            | Display_Folder _ -> "also set the measure's display folder."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let model, measure, table = args.GetResult Args.Move

    let opts =
        Mutation.options
            (args.Contains Args.Save)
            (args.TryGetResult Args.Save_To)
            (args.Contains Args.Force)
            (args.Contains Args.Stat)
            (args.Contains Args.Name_Only)

    match opts with
    | Error message -> UsageError message
    | Ok opts ->
        let spec: MoveObject.MoveSpec = {
            Measure = measure
            Table = table
            DisplayFolder = args.TryGetResult Args.Display_Folder
        }

        Mutation.run ctx "move" opts model (fun db -> MoveObject.move db spec)
