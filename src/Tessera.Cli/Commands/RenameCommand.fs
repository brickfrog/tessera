module Tessera.Cli.RenameCommand

open Argu
open Tessera
open Tessera.Inventory
open Tessera.Rename
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "rename an object and rewrite every DAX reference to it (dry run unless --save)."

type Args =
    | [<MainCommand; ExactlyOnce>] Rename of model: string * object: string * new_name: string
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Rename _ -> "the model, the object to rename (any form `get` accepts), and its new name."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let private counts (p: RenamePlan) =
    let expressions = RenamePlan.expressionEdits p
    let objects = expressions |> List.map (fun e -> e.Owner.Ref) |> List.distinct
    let sourceColumns = (RenamePlan.sourceColumnEdits p).Length
    expressions.Length, objects.Length, sourceColumns, p.Edits.Length - expressions.Length - sourceColumns

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private updatedText (p: RenamePlan) =
    let expressions, objects, sourceColumns, annotations = counts p

    let extra =
        [
            if sourceColumns > 0 then
                plural sourceColumns "calculated-table sourceColumn"
            if annotations > 0 then
                plural annotations "annotation"
        ]
        |> List.map (fun s -> " and " + s)
        |> String.concat ""

    $"""{plural expressions "expression"} in {plural objects "object"}{extra}"""

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let model, selector, newName = args.GetResult Args.Rename

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
        Mutation.runWith
            ctx
            "rename"
            opts
            model
            (Mutation.followReports (fun db ->
                Selector.resolve (objects db) selector
                |> Result.bind (fun target -> Rename.plan db target newName)
                |> Result.map (fun p ->
                    if RenamePlan.isNoOp p then
                        {
                            Tessera.Mutation.Effect.none with
                                Notes = [ $"{ObjectRef.format p.Target.Ref} is already named '{p.NewName}'." ]
                        }
                    else
                        Rename.apply p

                        {
                            Warnings = p.Warnings
                            Renamed = [ p.Target.Ref, p.NewRef ]
                            Notes = [
                                $"Renamed {ObjectRef.format p.Target.Ref} -> {ObjectRef.format p.NewRef}; updated {updatedText p}"
                            ]
                        })))
