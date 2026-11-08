/// The shared front end of every editing command (rename, set, add, remove, move), following the
/// Tabular Editor CLI's conventions:
///   - a dry run by default: show what would change and write nothing;
///   - --save writes back in place, --save-to <path> writes elsewhere (like `convert`);
///   - output: the changes as a line diff (default), --stat, or --name-only; --json gives changes[];
///   - a save that would introduce new errors is refused unless --force.
/// Each command declares the same five switches in its Argu Args (see `usage`) and builds `Options`.
module Tessera.Cli.Mutation

open System.IO
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Cli.Output
open Tessera.Cli.Common

type Target =
    | DryRun
    | InPlace
    | SaveTo of path: string

type Show =
    | ShowDiff
    | ShowStat
    | ShowNames

type Options = {
    Target: Target
    Force: bool
    Show: Show
}

/// Usage text for the shared switches, so every editing command describes them the same way.
module Usage =
    let save =
        "write the change back to the model, in place (default: dry run, nothing written)."

    let saveTo =
        "write the changed model here instead (format inferred like `convert`: *.bim, *.SemanticModel, "
        + "otherwise a tmdl folder)."

    let force =
        "save even if the change introduces new errors; with --save-to, overwrite a non-empty destination."

    let stat = "show one line per changed object instead of a full diff."
    let nameOnly = "show only the references of the changed objects."

/// Build options from the parsed switches.
let options (save: bool) (saveTo: string option) (force: bool) (stat: bool) (nameOnly: bool) =
    match save, saveTo, stat, nameOnly with
    | true, Some _, _, _ -> Error "--save and --save-to can't be combined"
    | _, _, true, true -> Error "--stat and --name-only can't be combined"
    | _ ->
        Ok {
            Target =
                match save, saveTo with
                | _, Some path -> SaveTo path
                | true, None -> InPlace
                | false, None -> DryRun
            Force = force
            Show =
                if stat then ShowStat
                elif nameOnly then ShowNames
                else ShowDiff
        }

let private findingOf (i: Validate.Issue) =
    let f = Findings.about i.Ref Findings.Severity.Error "validate" i.Rule i.Message

    {
        f with
            Position =
                match i.Property, i.Line with
                | Some p, Some l ->
                    Some {
                        Property = p
                        Line = l
                        Column = i.Column
                    }
                | _ -> None
    }

let private resultJson
    (command: string)
    (opts: Options)
    (applied: Mutation.Applied)
    (saved: ModelIO.SavedModel option)
    : JsonNode =
    let o = Changes.changesJson command applied.Changes :?> JsonObject
    o.["dryRun"] <- bool (opts.Target = DryRun)
    o.["introducedErrors"] <- arr (applied.IntroducedErrors |> List.map (findingOf >> Findings.findingJson))
    o.["warnings"] <- arr (applied.Effect.Warnings |> List.map str)
    o.["notes"] <- arr (applied.Effect.Notes |> List.map str)

    o.["saved"] <-
        match saved with
        | Some s ->
            obj [
                "format", str (ModelIO.OutputFormat.token s.Format)
                "path", str s.Path
                "eol", str (ModelIO.Eol.token s.Eol)
            ]
        | None -> null

    o

/// Load the model, apply `edit`, show the changes, and save if asked (and safe).
let run
    (ctx: Ctx)
    (command: string)
    (opts: Options)
    (modelPath: string)
    (edit: Database -> Result<Mutation.Effect, TessError>)
    : Outcome =
    match ModelIO.loadPath modelPath with
    | Error e -> Failed e
    | Ok(source, db) ->
        match Mutation.apply db edit with
        | Error e -> Failed e
        | Ok applied ->
            let blocked =
                opts.Target <> DryRun && not applied.IntroducedErrors.IsEmpty && not opts.Force

            let saved =
                if applied.Changes.IsEmpty || blocked then
                    Ok None
                else
                    match opts.Target with
                    | DryRun -> Ok None
                    | InPlace -> ModelIO.saveBack source db |> Result.map Some
                    | SaveTo path -> ModelIO.saveTo source opts.Force db path |> Result.map Some

            match saved with
            | Error e -> Failed e
            | Ok saved ->
                let blockedError = ChangeIntroducesErrors applied.IntroducedErrors.Length

                if ctx.Json then
                    let json = resultJson command opts applied saved :?> JsonObject

                    // One JSON document: a refused save is reported inside the result.
                    if blocked then
                        json.["error"] <-
                            obj [
                                "code", str (TessError.code blockedError)
                                "message", str (TessError.message blockedError)
                            ]

                    writeJson ctx.Out json
                else
                    if applied.Changes.IsEmpty then
                        ctx.Out.WriteLine("No changes.")
                    else
                        match opts.Show with
                        | ShowDiff -> Changes.diffText ctx.Out applied.Diff
                        | ShowStat -> Changes.statText ctx.Out applied.Changes
                        | ShowNames -> Changes.namesText ctx.Out applied.Changes

                    // Notices go to stderr, so stdout stays the change itself.
                    for note in applied.Effect.Notes do
                        ctx.Err.WriteLine(note)

                    for w in applied.Effect.Warnings do
                        ctx.Err.WriteLine($"tess: warning: {w}")

                    if not applied.IntroducedErrors.IsEmpty then
                        let errors =
                            match applied.IntroducedErrors.Length with
                            | 1 -> "1 error"
                            | n -> $"{n} errors"

                        ctx.Err.WriteLine($"tess: this change introduces {errors}:")

                        for i in applied.IntroducedErrors do
                            ctx.Err.WriteLine($"  {ObjectRef.format i.Ref}  {i.Message}  [{i.Rule}]")

                    if not applied.Changes.IsEmpty then
                        match opts.Target, saved with
                        | DryRun, _ -> ctx.Err.WriteLine("Dry run - nothing saved. Add --save to persist.")
                        | _, Some s ->
                            let format = ModelIO.OutputFormat.token s.Format
                            ctx.Err.WriteLine($"Saved {format}  {s.Path}  ({ModelIO.Eol.token s.Eol})")
                        | _, None -> ()

                match blocked, ctx.Json with
                | true, true -> DoneWith(ExitCode.ofError blockedError)
                | true, false -> Failed blockedError
                | false, _ -> Done
