module Tessera.Cli.SetCommand

open System
open Argu
open Tessera
open Tessera.Inventory
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "set properties on an object (dry run unless --save)."

type Args =
    | [<MainCommand; ExactlyOnce>] Target of model: string * object: string
    | [<AltCommandLine("-p")>] Property of assignment: string
    | Unset of path: string
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the object (any form `get` accepts)."
            | Property _ ->
                "<path>=<value> (repeatable), with the paths `get -p` prints, e.g. formatString=0.00, "
                + "isHidden=true, kpi.targetExpression=..., annotations.X=...; `null` clears, `-` reads stdin."
            | Unset _ -> "clear or reset a property (repeatable); the same as -p <path>=null."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

/// The assignments, in command-line order. At most one value can come from stdin.
let private assignments (args: ParseResults<Args>) =
    let stdinUses =
        args.GetResults Args.Property
        |> List.filter (fun p -> p.EndsWith "=-")
        |> List.length

    if stdinUses > 1 then
        Error "only one -p value can be read from stdin (-)"
    else
        let stdin () = Console.In.ReadToEnd()

        args.GetAllResults()
        |> List.choose (function
            | Property text -> Some(SetProperty.Assignment.parse stdin text)
            | Unset path -> Some(Ok(SetProperty.Assignment.unset path))
            | _ -> None)
        |> List.fold
            (fun acc a ->
                match acc, a with
                | Ok xs, Ok x -> Ok(x :: xs)
                | Error e, _
                | _, Error e -> Error e)
            (Ok [])
        |> Result.bind (function
            | [] -> Error "nothing to set: pass -p <path>=<value> or --unset <path>"
            | xs -> Ok(List.rev xs))

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let model, selector = args.GetResult Args.Target

    let opts =
        Mutation.options
            (args.Contains Args.Save)
            (args.TryGetResult Args.Save_To)
            (args.Contains Args.Force)
            (args.Contains Args.Stat)
            (args.Contains Args.Name_Only)

    match opts, assignments args with
    | Error message, _
    | _, Error message -> UsageError message
    | Ok opts, Ok assignments ->
        Mutation.run ctx "set" opts model (fun db ->
            Selector.resolve (objects db) selector
            |> Result.bind (fun target -> SetProperty.set db target assignments))
