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

/// Report file edits: the changed lines, and whether they were written.
let private filesJson (files: Report.FileEdit list) (written: bool) : JsonNode =
    arr [
        for f in files ->
            let before = f.Before.Split '\n'
            let after = f.After.Split '\n'

            obj [
                "path", str f.Path
                "display", str f.Display
                "references", num f.References
                "written", bool written
                "lines",
                arr [
                    for i in 0 .. (min before.Length after.Length) - 1 do
                        if before.[i] <> after.[i] then
                            yield
                                obj [
                                    "line", num (i + 1)
                                    "before", str (before.[i].TrimEnd '\r')
                                    "after", str (after.[i].TrimEnd '\r')
                                ]
                ]
            ]
    ]

let private referencesText n =
    if n = 1 then "1 reference" else $"{n} references"

/// Load the model, apply `edit`, show the changes, and save if asked (and safe).
let rec run
    (ctx: Ctx)
    (command: string)
    (opts: Options)
    (modelPath: string)
    (edit: Database -> Result<Mutation.Effect, TessError>)
    : Outcome =
    runWith ctx command opts modelPath (fun _ db -> edit db |> Result.map (fun e -> e, []))

/// `run`, for edits that also change files next to the model (report files that name its fields).
/// `edit` gets the model's location. The files are written only when the model is saved in place,
/// after it; a dry run shows them, and `--save-to` leaves them alone (they point at this model).
and runWith
    (ctx: Ctx)
    (command: string)
    (opts: Options)
    (modelPath: string)
    (edit: ModelSource -> Database -> Result<Mutation.Effect * Report.FileEdit list, TessError>)
    : Outcome =
    let loaded =
        if XmlaAddress.isRemote modelPath && opts.Target = InPlace then
            // Refused before anything is contacted.
            Error(
                RemoteSourceUnsupported(
                    modelPath,
                    "--save can't write to a server. Use --save-to <path> to save the changed model as files, "
                    + "then `tess deploy` them"
                )
            )
        else
            Source.load ctx modelPath

    match loaded with
    | Error e -> Failed e
    | Ok(source, db) ->
        let files = ref []

        let applied =
            Mutation.apply db (fun db ->
                edit source db
                |> Result.map (fun (effect, fs) ->
                    files.Value <- fs
                    effect))

        match applied with
        | Error e -> Failed e
        | Ok applied ->
            let files = files.Value

            let blocked =
                opts.Target <> DryRun && not applied.IntroducedErrors.IsEmpty && not opts.Force

            let saved =
                if applied.Changes.IsEmpty || blocked then
                    Ok None
                else
                    match opts.Target with
                    | DryRun -> Ok None
                    | InPlace ->
                        ModelIO.saveBack source db
                        |> Result.bind (fun s -> Report.write files |> Result.map (fun () -> Some s))
                    | SaveTo path -> ModelIO.saveTo source opts.Force db path |> Result.map Some

            let filesWritten =
                opts.Target = InPlace && not blocked && not applied.Changes.IsEmpty

            let applied =
                match opts.Target, files with
                | SaveTo _, _ :: _ ->
                    let w =
                        $"{List.length files} report files are not updated: --save-to writes the model elsewhere, "
                        + "and the reports still point at this one"

                    {
                        applied with
                            Effect.Warnings = applied.Effect.Warnings @ [ w ]
                    }
                | _ -> applied

            match saved with
            | Error e -> Failed e
            | Ok saved ->
                let blockedError = ChangeIntroducesErrors applied.IntroducedErrors.Length

                if ctx.Json then
                    let json = resultJson command opts applied saved :?> JsonObject
                    json.["files"] <- filesJson files filesWritten

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

                        if not files.IsEmpty then
                            match opts.Show with
                            | ShowDiff ->
                                for f in files do
                                    ctx.Out.WriteLine()
                                    ctx.Out.WriteLine($"~ report {f.Display}  ({referencesText f.References})")
                                    Changes.lineDiffText ctx.Out f.Before f.After
                            | ShowStat ->
                                let width = files |> List.map (fun f -> f.Display.Length) |> List.fold max 0

                                for f in files do
                                    ctx.Out.WriteLine(
                                        $"~ report {f.Display.PadRight width}  {referencesText f.References}"
                                    )
                            | ShowNames ->
                                for f in files do
                                    ctx.Out.WriteLine(f.Display)

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

                            if filesWritten && not files.IsEmpty then
                                let n = files |> List.sumBy (fun f -> f.References)

                                let fileCount =
                                    if files.Length = 1 then
                                        "1 file"
                                    else
                                        $"{files.Length} files"

                                ctx.Err.WriteLine($"Updated {referencesText n} in {fileCount} of the report")
                        | _, None -> ()

                match blocked, ctx.Json with
                | true, true -> DoneWith(ExitCode.ofError blockedError)
                | true, false -> Failed blockedError
                | false, _ -> Done

/// Make a model edit also update the report files of the model's PBIP project: for each object it
/// renames or moves (Effect.Renamed), the PBIR field references in report folders whose
/// definition.pbir points at this semantic model folder.
let followReports
    (edit: Database -> Result<Mutation.Effect, TessError>)
    : ModelSource -> Database -> Result<Mutation.Effect * Report.FileEdit list, TessError> =
    fun source db ->
        edit db
        |> Result.map (fun effect ->
            let reports =
                source.SemanticModelFolder
                |> Option.map Report.reportsOf
                |> Option.defaultValue []

            let changes = effect.Renamed |> List.choose (fun (o, n) -> Report.Change.ofMove o n)

            match reports, changes with
            | [], _
            | _, [] -> effect, []
            | _, [ change ] ->
                let files, warnings = Report.plan reports change

                {
                    effect with
                        Warnings = effect.Warnings @ warnings
                },
                files
            | _ ->
                {
                    effect with
                        Warnings =
                            effect.Warnings
                            @ [ "report files are not updated for edits that rename several objects" ]
                },
                [])
