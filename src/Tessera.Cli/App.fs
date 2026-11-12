/// The CLI entry point, kept separate from Program.main so tests can drive it with captured writers.
module Tessera.Cli.App

open System.IO
open Argu
open Tessera
open Tessera.Cli.Args
open Tessera.Cli.Common
open Tessera.Cli.Output

let private parser = lazy (ArgumentParser.Create<CliArgs>(programName = "tess"))

/// For Argu's "unrecognized argument: 'x'": the subcommands (or, for `-x`, the options) of the
/// command it was given to that are close to x. The command is found by following the
/// subcommands named before x on the command line.
let private argumentSuggestions (argv: string array) (message: string) =
    let m =
        System.Text.RegularExpressions.Regex.Match(message, "unrecognized argument: '(.*?)'")

    if not m.Success then
        []
    else
        let bad = m.Groups.[1].Value

        let subcommands (p: ArgumentParser) = [
            for sub in p.GetSubCommandParsers() do
                match sub.ParentInfo with
                | Some info -> info.CommandLineNames.Value, sub
                | None -> ()
        ]

        let rec walk (p: ArgumentParser) (tokens: string list) =
            match tokens with
            | [] -> p
            | t :: _ when t = bad -> p
            | t :: rest ->
                match subcommands p |> List.tryFind (fun (names, _) -> List.contains t names) with
                | Some(_, sub) -> walk sub rest
                | None -> walk p rest

        let p = walk parser.Value (List.ofArray argv)

        let candidates =
            if bad.StartsWith "-" then
                [
                    for c in p.GetArgumentCases() do
                        if not c.IsMainCommand && not c.IsHidden.Value then
                            yield! c.CommandLineNames.Value |> List.filter (fun n -> n.StartsWith "-")
                    "--help"
                    "--json"
                ]
            else
                subcommands p |> List.collect fst

        // Options are compared without their dashes: "-x" is one letter, not two.
        let bare (s: string) = s.TrimStart '-'

        Suggest.closest (candidates |> List.map bare) (bare bad)
        |> List.map (fun b -> candidates |> List.find (fun c -> bare c = b))

/// Argu's message with the hint added to its first line ("ERROR: unrecognized argument: 'x'.").
let private withHint (message: string) (suggestions: string list) =
    match suggestions with
    | [] -> message
    | _ ->
        // Argu ends lines with Environment.NewLine: "\r\n" on Windows.
        let newline =
            match message.IndexOf '\n' with
            | n when n > 0 && message.[n - 1] = '\r' -> n - 1
            | n -> n

        let first, rest =
            if newline < 0 then
                message, ""
            else
                message.Substring(0, newline), message.Substring newline

        first.TrimEnd('.') + Suggest.hint suggestions + "." + rest

/// `tess test run ... --watch`.
let private (|WatchedTestRun|_|) (args: ParseResults<TestCommand.Args>) =
    match args.TryGetSubCommand() with
    | Some(TestCommand.Run run) when run.Contains TestCommand.RunArgs.Watch -> Some run
    | _ -> None

/// Run the CLI with these environment variables, making any server connections through
/// `connector`. Returns the process exit code.
let runWithConnector
    (connector: Tessera.Xmla.IXmlaConnector)
    (getEnv: string -> string option)
    (out: TextWriter)
    (err: TextWriter)
    (argv: string array)
    : int =
    // `tess __complete <words...>` reaches Argu as one argument, so no word is read as an option.
    let argv = CompletionCommand.packWords argv
    // Decide JSON mode before parsing so usage errors can be reported as JSON too.
    let jsonRequested = argv |> Array.exists (fun a -> a = "--json" || a = "-j")

    let usageErrorWith (message: string) (suggestions: string list) =
        if jsonRequested then
            writeJson out (errorJsonWith "usage" message suggestions)
        else
            err.WriteLine(message)

        ExitCode.usage

    let usageError (message: string) = usageErrorWith message []

    try
        let results = parser.Value.ParseCommandLine(argv, raiseOnUsage = true)

        let ctx = {
            Out = out
            Err = err
            Json = results.Contains Json
            GetEnv = getEnv
            Connector = connector
        }

        if results.Contains Version then
            if ctx.Json then
                writeJson out (versionJson (version ()))
            else
                out.WriteLine($"tess {version ()}")

            ExitCode.ok
        else
            let outcome =
                match results.TryGetSubCommand() with
                | Some(Convert args) -> ConvertCommand.run ctx args
                | Some(Info args) -> InfoCommand.run ctx args
                | Some(List args) -> ListCommand.run ctx args
                | Some(Get args) -> GetCommand.run ctx args
                | Some(Find args) -> FindCommand.run ctx args
                | Some(Diff args) -> DiffCommand.run ctx args
                | Some(Deps args) -> DepsCommand.run ctx args
                | Some(Init args) -> InitCommand.run ctx args
                | Some(Validate args) when args.Contains ValidateCommand.Args.Watch -> WatchCommands.validate ctx args
                | Some(Validate args) -> ValidateCommand.run ctx args
                | Some(Rename args) -> RenameCommand.run ctx args
                | Some(Remove args) -> RemoveCommand.run ctx args
                | Some(Move args) -> MoveCommand.run ctx args
                | Some(Bpa args) when args.Contains BpaCommand.Args.Watch -> WatchCommands.bpa ctx args
                | Some(Bpa args) -> BpaCommand.run ctx args
                | Some(Set args) -> SetCommand.run ctx args
                | Some(Add args) -> AddCommand.run ctx args
                | Some(Translations args) -> TranslationsCommand.run ctx args
                | Some(Perspective args) -> PerspectiveCommand.run ctx args
                | Some(Script args) -> ScriptCommand.run ctx args
                | Some(Lint args) when args.Contains LintCommand.Args.Watch -> WatchCommands.lint ctx args
                | Some(Lint args) -> LintCommand.run ctx args
                | Some(Format args) -> FormatCommand.run ctx args
                | Some(Connect args) -> ConnectCommand.run ctx args
                | Some(Query args) -> QueryCommand.run ctx args
                | Some(Deploy args) -> DeployCommand.run ctx args
                | Some(Data args) -> DataCommand.run ctx args
                | Some(Refresh args) -> RefreshCommand.run ctx args
                | Some(Test(WatchedTestRun run as args)) -> WatchCommands.testRun ctx args run
                | Some(Test args) -> TestCommand.run ctx args
                | Some(Completion args) -> CompletionCommand.run parser.Value ctx args
                | Some(Complete args) -> CompletionCommand.complete parser.Value ctx args
                | _ -> UsageError(parser.Value.PrintUsage())

            match outcome with
            | Done -> ExitCode.ok
            | DoneWith code -> code
            | UsageError message -> usageError message
            | UsageErrorSuggest(message, suggestions) -> usageErrorWith message suggestions
            | Failed e ->
                if ctx.Json then
                    writeJson out (tessErrorJson e)
                else
                    err.WriteLine($"tess: {TessError.message e}")

                ExitCode.ofError e
    with
    | :? ArguParseException as e when e.ErrorCode = ErrorCode.HelpText ->
        out.WriteLine(e.Message)
        ExitCode.ok
    | :? ArguParseException as e ->
        let suggestions = argumentSuggestions argv e.Message
        usageErrorWith (withHint e.Message suggestions) suggestions
    | e ->
        if jsonRequested then
            writeJson out (errorJson "internal" e.Message)
        else
            err.WriteLine($"tess: internal error: {e}")

        ExitCode.internalError

/// Run the CLI with these environment variables. Returns the process exit code.
let runWith (getEnv: string -> string option) (out: TextWriter) (err: TextWriter) (argv: string array) : int =
    runWithConnector (Tessera.Xmla.LiveConnector()) getEnv out err argv

/// Run the CLI. Returns the process exit code.
let run (out: TextWriter) (err: TextWriter) (argv: string array) : int = runWith processEnv out err argv
