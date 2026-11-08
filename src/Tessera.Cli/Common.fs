/// What every command shares: exit codes, outcomes, the output context, argument helpers.
module Tessera.Cli.Common

open System
open System.IO
open System.Reflection
open Tessera

/// Process exit codes. Stable: scripts and CI may depend on them.
module ExitCode =
    let ok = 0
    /// find: no matches; diff: differences; bpa/validate: issues. See README.
    let findings = 1
    let usage = 2
    let input = 3
    let output = 4
    let internalError = 70

    let ofError =
        function
        | PathNotFound _
        | UnrecognizedModel _
        | AmbiguousModel _
        | InvalidPbip _
        | RemoteModelReference _
        | LoadFailed _
        | InvalidObjectRef _
        | ObjectNotFound _
        | AmbiguousObject _
        | PropertyNotFound _ -> input
        | DestinationNotEmpty _
        | SaveFailed _ -> output
        | InvalidPattern _ -> usage

let version () =
    let asm = Assembly.GetExecutingAssembly()

    match asm.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | null -> string (asm.GetName().Version)
    | a -> a.InformationalVersion.Split('+').[0]

type Outcome =
    | Done
    /// Completed, but with the "negative" result its Unix counterpart signals with exit 1
    /// (find: nothing matched; diff: models differ).
    | DoneWith of exitCode: int
    | Failed of TessError
    | UsageError of string

/// Where a command writes, and whether it should write JSON.
type Ctx = {
    Out: TextWriter
    Err: TextWriter
    Json: bool
}

let parseList (values: string list) =
    values
    |> List.collect (fun v ->
        v.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> List.ofArray)

/// Kind tokens (repeatable and/or comma-separated) to ObjectKinds.
let parseKinds (tokens: string list) =
    let kinds = parseList tokens |> List.map (fun t -> t, ObjectKind.tryParse t)

    match kinds |> List.tryFind (fun (_, k) -> k.IsNone) with
    | Some(bad, _) ->
        let known = ObjectKind.all |> List.map ObjectKind.token |> String.concat ", "
        Error $"unknown kind '{bad}' (known: {known})"
    | None -> Ok(kinds |> List.choose snd)
