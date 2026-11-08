module Tessera.Cli.GetCommand

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Inventory
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "show one object (TMDL, or its JSON with --json)."

type Args =
    | [<MainCommand; ExactlyOnce>] Target of model: string * object: string
    | [<AltCommandLine("-p")>] Property of path: string
    | Children

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "model path, and the object: a ref (measure:Sales/Total), DAX style ('Sales'[Total], [Total]) "
                + "or a bare name."
            | Property _ ->
                "print just this property (repeatable), e.g. expression, source.expression, annotations.PBI_FormatHint."
            | Children -> "include child objects (a table's columns, measures, ...)."

let getJson (e: ObjectEntry) (objectJson: string) =
    obj (refFields e @ [ "object", JsonNode.Parse objectJson ])

let propertiesJson (e: ObjectEntry) (leaves: (string * string) list) =
    obj (refFields e @ [ "properties", obj [ for p, v in leaves -> p, str v ] ])

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath, selector = args.GetResult Args.Target
    let children = args.Contains Args.Children
    let wanted = args.GetResults Args.Property

    let result =
        ModelIO.loadPath modelPath
        |> Result.bind (fun (_, db) ->
            Inventory.Selector.resolve (Inventory.objects db) selector
            |> Result.map (fun e -> db, e))

    match result with
    | Error e -> Failed e
    | Ok(db, e) ->
        let json = Serialize.objectJson db children e.Object

        match wanted with
        | [] ->
            if ctx.Json then
                writeJson ctx.Out (getJson e json)
            else
                ctx.Out.Write(Serialize.objectTmdl children e.Object)

            Done
        | paths ->
            let props =
                let own = Serialize.flatten json

                if e.Ref.Kind = ObjectKind.Model then
                    ("compatibilityLevel", string db.CompatibilityLevel) :: own
                else
                    own

            let selected = paths |> List.map (fun p -> p, Serialize.select p props)

            match selected |> List.tryFind (snd >> List.isEmpty) with
            | Some(missing, _) -> Failed(PropertyNotFound(ObjectRef.format e.Ref, missing))
            | None ->
                let leaves = selected |> List.collect snd

                if ctx.Json then
                    writeJson ctx.Out (propertiesJson e leaves)
                else
                    match paths, leaves with
                    // One exact property: print the bare value, for scripting.
                    | [ p ], [ (leaf, value) ] when leaf = p -> ctx.Out.WriteLine(value)
                    | _ ->
                        for leaf, value in leaves do
                            ctx.Out.WriteLine($"{leaf}: {value}")

                Done
