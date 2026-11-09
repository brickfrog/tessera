/// Editing a model safely: snapshot it, apply an edit, and report exactly what changed and which
/// errors the edit introduced. Commands decide whether to save (dry run by default).
module Tessera.Mutation

open Microsoft.AnalysisServices.Tabular

/// What an edit reports besides the model changes themselves.
type Effect = {
    /// Things the edit didn't (or couldn't) do, for the user to check.
    Warnings: string list
    /// A one-line description of what was done, e.g. "Renamed ... ; updated 3 expressions".
    Notes: string list
    /// Objects the edit renamed (old ref, new ref), so they're reported as moves, not delete + create.
    Renamed: (ObjectRef * ObjectRef) list
}

module Effect =
    let none = {
        Warnings = []
        Notes = []
        Renamed = []
    }

type Applied = {
    /// Changes in diff form (for line-diff rendering) ...
    Diff: Diff.Change list
    /// ... and in the shared change-set shape.
    Changes: ChangeSet.ObjectChange list
    /// Errors present after the edit that weren't before it.
    IntroducedErrors: Validate.Issue list
    Effect: Effect
}

/// Validate rules an edit can break. Others (relationship data types, sort-by, empty expressions)
/// describe the model as the user left it, not something an edit typically causes.
let private gatedRules =
    set [
        "unresolved-reference"
        "circular-reference"
        "duplicate-name"
        "invalid-token"
        "tom-validation"
    ]

let private errors (db: Database) =
    Validate.validate db
    |> List.filter (fun i -> i.Severity = Validate.Severity.Error && gatedRules.Contains i.Rule)

/// Apply `edit` to `db` (mutating it). Before-and-after comparison runs on a snapshot, so the
/// change set is exact. Issues of renamed objects are matched to their old names, so they don't
/// count as new.
let apply (db: Database) (edit: Database -> Result<Effect, TessError>) : Result<Applied, TessError> =
    let before = ModelIO.clone db
    let beforeErrors = errors before

    edit db
    |> Result.map (fun effect ->
        let diff =
            Diff.compare
                {
                    Diff.Options.none with
                        KnownRenames = effect.Renamed
                }
                before
                db

        let changes = ChangeSet.ofDiff diff

        let oldRefOf =
            changes
            |> List.choose (fun c -> c.MovedFrom |> Option.map (fun old -> c.Ref, old))
            |> Map.ofList

        let key (i: Validate.Issue) =
            i.Rule, Map.tryFind i.Ref oldRefOf |> Option.defaultValue i.Ref, i.Message

        let known = beforeErrors |> List.map key |> Set.ofList

        {
            Diff = diff
            Changes = changes
            IntroducedErrors = errors db |> List.filter (fun i -> not (known.Contains(key i)))
            Effect = effect
        })
