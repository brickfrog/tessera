module Tessera.Cli.RenameCommand

open System.IO
open System.Text.Json
open Argu
open Tessera
open Tessera.Inventory
open Tessera.Rename
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "rename an object and rewrite every DAX reference to it."

type Args =
    | [<MainCommand; ExactlyOnce>] Rename of model: string * object: string * new_name: string
    | Dry_Run
    | [<AltCommandLine("-o")>] Output of path: string
    | Force

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Rename _ -> "the model, the object to rename (any form `get` accepts), and its new name."
            | Dry_Run -> "show what would change, and write nothing."
            | Output _ ->
                "write the renamed model here instead of saving in place (format inferred like `convert`: "
                + "*.bim, *.SemanticModel, otherwise a tmdl folder)."
            | Force -> "with --output: overwrite a non-empty destination."

/// Where an in-place save goes: back to the model's own format and location.
let private inPlace (src: ModelSource) =
    match src.Format, src.SemanticModelFolder with
    | Bim, _ -> ModelIO.BimFile, src.Path
    | Tmdl, Some folder -> ModelIO.PbipFolder, folder
    | Tmdl, None -> ModelIO.TmdlFolder, src.Path

let private counts (p: RenamePlan) =
    let expressions = RenamePlan.expressionEdits p
    let objects = expressions |> List.map (fun e -> e.Owner.Ref) |> List.distinct
    expressions.Length, objects.Length, p.Edits.Length - expressions.Length

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private updatedText (p: RenamePlan) =
    let expressions, objects, others = counts p

    let annotations =
        if others > 0 then
            " and " + plural others "annotation"
        else
            ""

    $"""{plural expressions "expression"} in {plural objects "object"}{annotations}"""

let renameJson (dryRun: bool) (p: RenamePlan) (saved: ModelIO.SavedModel option) =
    let expressions, objects, _ = counts p

    obj [
        "dryRun", bool dryRun
        "rename",
        obj [
            "kind", str (ObjectKind.token p.Target.Ref.Kind)
            "before", str (ObjectRef.format p.Target.Ref)
            "after", str (ObjectRef.format p.NewRef)
            "oldName", str p.OldName
            "newName", str p.NewName
        ]
        "edits",
        arr [
            for e in p.Edits ->
                obj [
                    "ref", str (ObjectRef.format e.Owner.Ref)
                    "property", str e.Property
                    "references", num e.References
                    "before", str e.Before
                    "after", str e.After
                ]
        ]
        "summary", obj [ "expressions", num expressions; "objects", num objects ]
        "warnings", arr [ for w in p.Warnings -> str w ]
        "output",
        match saved with
        | None -> null
        | Some s ->
            obj [
                "format", str (ModelIO.OutputFormat.token s.Format)
                "path", str s.Path
                "eol", str (ModelIO.Eol.token s.Eol)
            ]
    ]

let private quote (s: string) =
    JsonSerializer.Serialize(s, JsonSerializerOptions(jsonOptions, WriteIndented = false))

/// The edits, grouped by object, as `tess diff` shows property changes.
let planText (out: TextWriter) (p: RenamePlan) =
    for owner, edits in p.Edits |> List.groupBy (fun e -> e.Owner.Ref) do
        out.WriteLine($"~ {ObjectRef.format owner}")

        for e in edits do
            if e.Before.Contains '\n' || e.After.Contains '\n' then
                out.WriteLine($"    {e.Property}:")
                DiffCommand.lineDiffText out e.Before e.After
            else
                out.WriteLine($"    {e.Property}: {quote e.Before} -> {quote e.After}")

let private warningsText (err: TextWriter) (p: RenamePlan) =
    for w in p.Warnings do
        err.WriteLine($"tess: warning: {w}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let model, selector, newName = args.GetResult Args.Rename
    let dryRun = args.Contains Args.Dry_Run
    let output = args.TryGetResult Args.Output

    let planned =
        ModelIO.loadPath model
        |> Result.bind (fun (src, db) ->
            Selector.resolve (objects db) selector
            |> Result.bind (fun target -> Rename.plan db target newName)
            |> Result.map (fun p -> src, db, p))

    match planned with
    | Error e -> Failed e
    | Ok(src, db, p) ->
        let label = $"{ObjectRef.format p.Target.Ref} -> {ObjectRef.format p.NewRef}"

        if dryRun then
            if ctx.Json then
                writeJson ctx.Out (renameJson true p None)
            elif RenamePlan.isNoOp p then
                ctx.Out.WriteLine($"{ObjectRef.format p.Target.Ref} is already named '{p.NewName}'; nothing to do.")
            else
                ctx.Out.WriteLine($"Would rename {label}")
                planText ctx.Out p
                ctx.Out.WriteLine($"Would update {updatedText p}. Dry run: nothing written.")
                warningsText ctx.Err p

            Done
        elif RenamePlan.isNoOp p && output.IsNone then
            if ctx.Json then
                writeJson ctx.Out (renameJson false p None)
            else
                ctx.Out.WriteLine($"{ObjectRef.format p.Target.Ref} is already named '{p.NewName}'; nothing to do.")

            Done
        else
            Rename.apply p

            // In place: the model's own format and location, matching its line endings and keeping
            // everything in it (restricted information included). --output works like `convert`.
            let format, destination, force, includeSensitive =
                match output with
                | None ->
                    let format, path = inPlace src
                    format, path, true, true
                | Some path -> ModelIO.OutputFormat.infer path, path, args.Contains Args.Force, false

            let opts: ModelIO.SaveOptions = {
                ModelIO.SaveOptions.defaults with
                    Eol = ModelIO.Eol.resolveAuto src.Path destination
                    IncludeSensitive = includeSensitive
            }

            match ModelIO.save format opts force db destination with
            | Error e -> Failed e
            | Ok saved ->
                if ctx.Json then
                    writeJson ctx.Out (renameJson false p (Some saved))
                else
                    ctx.Out.WriteLine($"Renamed {label}; updated {updatedText p}")

                    ctx.Out.WriteLine(
                        $"Wrote {ModelIO.OutputFormat.token saved.Format}  {saved.Path}  ({ModelIO.Eol.token saved.Eol})"
                    )

                    warningsText ctx.Err p

                Done
