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
    /// The server couldn't be reached, refused the credentials, or reported an error.
    let remote = 5
    let internalError = 70

    let rec ofError =
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
        | PropertyNotFound _
        | NameCollision _
        | UnsupportedRename _
        | RenameConflict _ -> input
        | RemoveBlocked _
        | UnsupportedRemove _
        | UnsupportedMove _
        | MoveConflict _ -> input
        | InvalidRules _ -> input
        | ChangeIntroducesErrors _ -> findings
        | DestinationNotEmpty _
        | SaveFailed _ -> output
        | InvalidPattern _
        | InvalidName _ -> usage
        | UnknownProperty _
        | ReadOnlyProperty _
        | InvalidPropertyValue _ -> usage
        | CannotAdd _
        | ObjectNotFoundSuggest _ -> input
        | InvalidTranslationFile _
        | UnsupportedPerspectiveChange _ -> input
        | InvalidScript _
        | CannotScript _ -> input
        | ScriptObjectFailed(_, _, _, inner) -> ofError inner
        | RuleNotFound _
        | RuleExists _
        | CannotIgnore _ -> input
        | InvalidServerAddress _
        | SecretOnCommandLine _ -> usage
        | RemoteSourceUnsupported _
        | InvalidCredentials _ -> input
        | AuthenticationRequired _
        | AuthenticationFailed _
        | ConnectionFailed _
        | UnsupportedConnection _
        | DatabaseNotFound _
        | DatabaseRequired _
        | QueryFailed _
        | DeployFailed _ -> remote
        | DatabaseExists _ -> output
        | DeployIntroducesErrors _ -> findings
        | InvalidFixture _ -> input
        | CannotRefresh _ -> input
        | RefreshFailed _ -> remote
        | DaxSyntax _
        | DaxNotSupported _
        | DaxInvalid _
        | DaxEvaluation _
        | FixtureHasErrors _ -> input

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
    /// Environment variables. Commands read them only through this, so tests can supply their own.
    GetEnv: string -> string option
    /// How connections to servers are made. Only commands given an `xmla:` model (or a server
    /// argument) use it; tests supply their own.
    Connector: Tessera.Xmla.IXmlaConnector
}

/// The process's environment variables (an empty value counts as unset).
let processEnv (name: string) =
    match Environment.GetEnvironmentVariable name with
    | null
    | "" -> None
    | v -> Some v

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
