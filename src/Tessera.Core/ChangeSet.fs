/// Model changes in one shape, shared by `diff` and every editing command (the Tabular Editor CLI's
/// changes[] shape): what was created, deleted, modified or moved (renamed), with property changes.
module Tessera.ChangeSet

[<RequireQualifiedAccess>]
type ChangeKind =
    | Created
    | Deleted
    | Modified
    /// Renamed (or moved): matched through its lineageTag, or reported by the command that renamed it.
    | Moved

module ChangeKind =
    let token =
        function
        | ChangeKind.Created -> "created"
        | ChangeKind.Deleted -> "deleted"
        | ChangeKind.Modified -> "modified"
        | ChangeKind.Moved -> "moved"

type ObjectChange = {
    /// The object as it is now (before, for deletions).
    Ref: ObjectRef
    ChangeKind: ChangeKind
    /// For moves: where it was.
    MovedFrom: ObjectRef option
    /// Human-readable identity (a relationship's endpoints, "old -> new" for moves).
    Label: string
    Properties: Diff.PropertyChange list
}

let ofDiff (changes: Diff.Change list) : ObjectChange list = [
    for c in changes do
        match c.Kind, c.Before, c.After with
        | Diff.Added, _, Some after -> {
            Ref = after
            ChangeKind = ChangeKind.Created
            MovedFrom = None
            Label = c.Label
            Properties = c.Properties
          }
        | Diff.Removed, Some before, _ -> {
            Ref = before
            ChangeKind = ChangeKind.Deleted
            MovedFrom = None
            Label = c.Label
            Properties = c.Properties
          }
        | Diff.Renamed, Some before, Some after -> {
            Ref = after
            ChangeKind = ChangeKind.Moved
            MovedFrom = Some before
            Label = c.Label
            Properties = c.Properties
          }
        | Diff.Modified, _, Some after -> {
            Ref = after
            ChangeKind = ChangeKind.Modified
            MovedFrom = None
            Label = c.Label
            Properties = c.Properties
          }
        | _ -> ()
]

type Summary = {
    Created: int
    Deleted: int
    Modified: int
    Moved: int
    Total: int
}

let summarize (changes: ObjectChange list) =
    let count k =
        changes |> List.filter (fun c -> c.ChangeKind = k) |> List.length

    {
        Created = count ChangeKind.Created
        Deleted = count ChangeKind.Deleted
        Modified = count ChangeKind.Modified
        Moved = count ChangeKind.Moved
        Total = changes.Length
    }
