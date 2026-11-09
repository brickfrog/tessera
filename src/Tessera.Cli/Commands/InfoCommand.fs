module Tessera.Cli.InfoCommand

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
let summary = "summarise a model."

type Args =
    | [<MainCommand; ExactlyOnce>] Model of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."

let infoJson (i: ModelInfo) =
    obj [
        "name", str i.Name
        "compatibilityLevel", num i.CompatibilityLevel
        "compatibilityMode", str i.CompatibilityMode
        "culture", optStr i.Culture
        "source", sourceJson i.Source
        "calculationGroups", num i.CalculationGroups
        "counts", obj [ for k, n in i.Counts -> ObjectKind.token k, num n ]
    ]

let private noun (kind: ObjectKind) n =
    let singular, plural =
        match kind with
        | ObjectKind.Hierarchy -> "hierarchy", "hierarchies"
        | ObjectKind.CalculationItem -> "calculation item", "calculation items"
        | ObjectKind.DataSource -> "data source", "data sources"
        | k -> ObjectKind.token k, ObjectKind.token k + "s"

    if n = 1 then singular else plural

let infoText (out: TextWriter) (i: ModelInfo) =
    let row (label: string) (value: string) = out.WriteLine($"{label, -15} {value}")

    let where =
        match i.Source.SemanticModelFolder with
        | Some sm -> $"{ModelFormat.token i.Source.Format} in PBIP semantic model {sm}"
        | None -> ModelFormat.token i.Source.Format

    row "Name" (if i.Name = "" then "(unnamed)" else i.Name)
    row "Format" where
    row "Path" i.Source.Path

    row
        "Compatibility"
        (if i.CompatibilityMode = "Unknown" then
             string i.CompatibilityLevel
         else
             $"{i.CompatibilityLevel} ({i.CompatibilityMode})")

    i.Culture |> Option.iter (row "Culture")
    out.WriteLine()

    for kind, n in i.Counts do
        if n > 0 then
            let extra =
                if kind = ObjectKind.Table && i.CalculationGroups > 0 then
                    let groups =
                        if i.CalculationGroups = 1 then
                            "calculation group"
                        else
                            "calculation groups"

                    $"  (incl. {i.CalculationGroups} {groups})"
                else
                    ""

            out.WriteLine($"{n, 6}  {noun kind n}{extra}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match Source.load ctx (args.GetResult Args.Model) with
    | Error e -> Failed e
    | Ok(source, db) ->
        let info = Inventory.info source db

        if ctx.Json then
            writeJson ctx.Out (infoJson info)
        else
            infoText ctx.Out info

        Done
