namespace Tessera

/// Every failure tessera can report. Core code returns these instead of throwing or printing;
/// the CLI decides how to render them and which exit code to use.
type TessError =
    | PathNotFound of path: string
    | UnrecognizedModel of path: string * reason: string
    | AmbiguousModel of path: string * candidates: string list
    | InvalidPbip of path: string * reason: string
    | RemoteModelReference of reportPath: string
    | LoadFailed of path: string * message: string
    | DestinationNotEmpty of path: string
    | SaveFailed of path: string * message: string
    | InvalidObjectRef of input: string * reason: string
    | ObjectNotFound of input: string
    | AmbiguousObject of input: string * candidates: string list
    | PropertyNotFound of objectRef: string * path: string
    | InvalidPattern of pattern: string * reason: string
    | InvalidName of name: string * reason: string
    | NameCollision of name: string * existing: string
    | UnsupportedRename of objectRef: string * reason: string
    | RenameConflict of objectRef: string * newName: string * details: string list
    /// A Best Practice Analyzer rule file that can't be read as rules.
    | InvalidRules of path: string * reason: string
    /// An edit would leave the model with errors it didn't have (save refused without --force).
    | ChangeIntroducesErrors of count: int
    /// `set`: a property path the object doesn't have (with close matches).
    | UnknownProperty of objectRef: string * path: string * suggestions: string list
    /// `set`: a property that can't be written (read-only, `name`, child collections, ...).
    | ReadOnlyProperty of objectRef: string * path: string * reason: string
    /// `set`: a value the property can't take.
    | InvalidPropertyValue of objectRef: string * path: string * value: string * reason: string
    /// `add` can't create this object here (e.g. a column of a calculated table).
    | CannotAdd of objectRef: string * reason: string
    /// No object matches, but some are named similarly.
    | ObjectNotFoundSuggest of input: string * candidates: string list

module TessError =

    /// Stable, machine-readable identifier (used in JSON output).
    let code =
        function
        | PathNotFound _ -> "path-not-found"
        | UnrecognizedModel _ -> "unrecognized-model"
        | AmbiguousModel _ -> "ambiguous-model"
        | InvalidPbip _ -> "invalid-pbip"
        | RemoteModelReference _ -> "remote-model-reference"
        | LoadFailed _ -> "load-failed"
        | DestinationNotEmpty _ -> "destination-not-empty"
        | SaveFailed _ -> "save-failed"
        | InvalidObjectRef _ -> "invalid-object-ref"
        | ObjectNotFound _ -> "object-not-found"
        | AmbiguousObject _ -> "ambiguous-object"
        | PropertyNotFound _ -> "property-not-found"
        | InvalidPattern _ -> "invalid-pattern"
        | InvalidName _ -> "invalid-name"
        | NameCollision _ -> "name-collision"
        | UnsupportedRename _ -> "unsupported-rename"
        | RenameConflict _ -> "rename-conflict"
        | InvalidRules _ -> "invalid-rules"
        | ChangeIntroducesErrors _ -> "change-introduces-errors"
        | UnknownProperty _ -> "unknown-property"
        | ReadOnlyProperty _ -> "read-only-property"
        | InvalidPropertyValue _ -> "invalid-property-value"
        | CannotAdd _ -> "cannot-add"
        | ObjectNotFoundSuggest _ -> "object-not-found"

    let message =
        function
        | PathNotFound path -> $"Path not found: {path}"
        | UnrecognizedModel(path, reason) -> $"Not a recognised semantic model: {path} ({reason})"
        | AmbiguousModel(path, candidates) ->
            let list = candidates |> List.map (fun c -> $"  {c}") |> String.concat "\n"
            $"More than one semantic model found under {path}; pass one of these explicitly:\n{list}"
        | InvalidPbip(path, reason) -> $"Invalid Power BI project file {path}: {reason}"
        | RemoteModelReference reportPath ->
            $"Report {reportPath} references a published semantic model (byConnection), not a local one"
        | LoadFailed(path, msg) -> $"Failed to load {path}: {msg}"
        | DestinationNotEmpty path -> $"Destination exists and is not empty: {path} (use --force to overwrite)"
        | SaveFailed(path, msg) -> $"Failed to write {path}: {msg}"
        | InvalidObjectRef(input, reason) -> $"Invalid object reference '{input}': {reason}"
        | ObjectNotFound input -> $"No object matches '{input}'"
        | AmbiguousObject(input, candidates) ->
            let list = candidates |> List.map (fun c -> $"  {c}") |> String.concat "\n"
            $"'{input}' matches more than one object; use one of these:\n{list}"
        | PropertyNotFound(objectRef, path) -> $"{objectRef} has no property '{path}'"
        | InvalidPattern(pattern, reason) -> $"Invalid pattern '{pattern}': {reason}"
        | InvalidName(name, reason) -> $"Invalid name '{name}': {reason}"
        | NameCollision(name, existing) -> $"'{name}' is already taken by {existing} (names are case-insensitive)"
        | UnsupportedRename(objectRef, reason) -> $"Can't rename {objectRef}: {reason}"
        | RenameConflict(objectRef, newName, details) ->
            let list = details |> List.map (fun d -> $"  {d}") |> String.concat "\n"
            $"Renaming {objectRef} to '{newName}' would change what other references mean:\n{list}"
        | InvalidRules(path, reason) -> $"Invalid BPA rules in {path}: {reason}"
        | ChangeIntroducesErrors n ->
            let errors = if n = 1 then "1 error" else $"{n} errors"
            $"Not saved: the change introduces {errors} (see above; use --force to save anyway)"
        | UnknownProperty(objectRef, path, suggestions) ->
            let hint =
                match suggestions with
                | [] -> ""
                | s ->
                    " - did you mean "
                    + (s |> List.map (fun p -> $"'{p}'") |> String.concat " or ")
                    + "?"

            $"{objectRef} has no property '{path}'{hint} (`tess get <model> <object> --properties` lists them)"
        | ReadOnlyProperty(objectRef, path, reason) -> $"Can't set {path} on {objectRef}: {reason}"
        | InvalidPropertyValue(objectRef, path, value, reason) ->
            let first = value.Split('\n').[0]

            let shown =
                if first.Length > 40 then first.Substring(0, 40) + "..."
                elif first.Length < value.Length then first + "..."
                else first

            $"Can't set {path} on {objectRef} to '{shown}': {reason}"
        | CannotAdd(objectRef, reason) -> $"Can't add {objectRef}: {reason}"
        | ObjectNotFoundSuggest(input, candidates) ->
            let list = candidates |> String.concat ", "
            $"No object matches '{input}'; did you mean {list}?"
