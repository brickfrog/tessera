module Tessera.Cli.PerspectiveCommand

open Argu
open Tessera
open Tessera.Cli.Common
open Tessera.Cli.Output

/// One-line description, shown in `tess --help`.
let summary =
    "add objects to a perspective, remove them from it, or list what it shows (dry run unless --save)."

type EditArgs =
    | [<MainCommand; ExactlyOnce>] Target of model_perspective_objects: string list
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model, the perspective, then one or more tables, columns, measures or hierarchies "
                + "(any form `get` accepts)."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

type ListArgs =
    | [<MainCommand; ExactlyOnce>] Target of model_and_perspective: string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and optionally a perspective (default: list the perspectives)."

type Args =
    | [<CliPrefix(CliPrefix.None)>] Add of ParseResults<EditArgs>
    | [<CliPrefix(CliPrefix.None)>] Remove of ParseResults<EditArgs>
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ ->
                "add objects to a perspective: a table with all its members, or single columns, measures and "
                + "hierarchies (with their table)."
            | Remove _ ->
                "remove objects from a perspective: a table with all its members, or single members (a table "
                + "whose last member goes is removed too)."
            | List _ -> "list the perspectives, or the members of one."

let private edit (ctx: Ctx) (change: Perspectives.Change) (args: ParseResults<EditArgs>) =
    let opts =
        Mutation.options
            (args.Contains EditArgs.Save)
            (args.TryGetResult EditArgs.Save_To)
            (args.Contains EditArgs.Force)
            (args.Contains EditArgs.Stat)
            (args.Contains EditArgs.Name_Only)

    let command =
        match change with
        | Perspectives.Change.Add -> "perspective add"
        | Perspectives.Change.Remove -> "perspective remove"

    match args.GetResult EditArgs.Target, opts with
    | model :: perspective :: (_ :: _ as objects), Ok opts ->
        Mutation.run ctx command opts model (fun db -> Perspectives.edit db change perspective objects)
    | _, Error message -> UsageError message
    | _ -> UsageError "expected the model, the perspective and at least one object"

let private memberJson (r: ObjectRef) =
    obj [ "ref", str (ObjectRef.format r); "kind", str (ObjectKind.token r.Kind) ]

let private list (ctx: Ctx) (args: ParseResults<ListArgs>) =
    match args.GetResult ListArgs.Target with
    | [ model ] ->
        match ModelIO.loadPath model with
        | Error e -> Failed e
        | Ok(_, db) ->
            let perspectives = [
                for p in db.Model.Perspectives ->
                    let members = Perspectives.members p

                    let count k =
                        members |> List.filter (fun r -> r.Kind = k) |> List.length

                    p, count
            ]

            let kinds = [
                ObjectKind.Table, "table", "tables"
                ObjectKind.Column, "column", "columns"
                ObjectKind.Measure, "measure", "measures"
                ObjectKind.Hierarchy, "hierarchy", "hierarchies"
            ]

            if ctx.Json then
                writeJson
                    ctx.Out
                    (arr [
                        for p, count in perspectives ->
                            obj [
                                yield "ref", str (ObjectRef.format (ObjectRef.modelLevel ObjectKind.Perspective p.Name))
                                yield "name", str p.Name
                                for k, _, plural in kinds do
                                    yield plural, num (count k)
                            ]
                    ])
            else
                for p, count in perspectives do
                    let counts =
                        kinds
                        |> List.map (fun (k, one, many) ->
                            match count k with
                            | 1 -> $"1 {one}"
                            | n -> $"{n} {many}")
                        |> String.concat ", "

                    ctx.Out.WriteLine(
                        $"{ObjectRef.format (ObjectRef.modelLevel ObjectKind.Perspective p.Name)}  {counts}"
                    )

            Done
    | [ model; perspective ] ->
        match ModelIO.loadPath model with
        | Error e -> Failed e
        | Ok(_, db) ->
            match Perspectives.find db perspective with
            | Error e -> Failed e
            | Ok p ->
                let members = Perspectives.members p

                if ctx.Json then
                    writeJson
                        ctx.Out
                        (obj [
                            "perspective", str (ObjectRef.format (ObjectRef.modelLevel ObjectKind.Perspective p.Name))
                            "members", arr (members |> List.map memberJson)
                        ])
                else
                    for r in members do
                        ctx.Out.WriteLine(ObjectRef.format r)

                Done
    | _ -> UsageError "expected the model, and optionally a perspective"

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetSubCommand() with
    | Some(Add a) -> edit ctx Perspectives.Change.Add a
    | Some(Remove a) -> edit ctx Perspectives.Change.Remove a
    | Some(List a) -> list ctx a
    | None -> UsageError(args.Parser.PrintUsage())
