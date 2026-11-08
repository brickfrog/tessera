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
