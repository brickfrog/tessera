/// The CLI entry point, kept separate from Program.main so tests can drive it with captured writers.
module Tessera.Cli.App

open System.IO
open Argu
open Tessera
open Tessera.Cli.Args
open Tessera.Cli.Common
open Tessera.Cli.Output

let private parser = lazy (ArgumentParser.Create<CliArgs>(programName = "tess"))

/// Run the CLI. Returns the process exit code.
let run (out: TextWriter) (err: TextWriter) (argv: string array) : int =
    // Decide JSON mode before parsing so usage errors can be reported as JSON too.
    let jsonRequested = argv |> Array.exists (fun a -> a = "--json" || a = "-j")

    let usageError (message: string) =
        if jsonRequested then
            writeJson out (errorJson "usage" message)
        else
            err.WriteLine(message)

        ExitCode.usage

    try
        let results = parser.Value.ParseCommandLine(argv, raiseOnUsage = true)

        let ctx = {
            Out = out
            Err = err
            Json = results.Contains Json
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
                | Some(Validate args) -> ValidateCommand.run ctx args
                | Some(Rename args) -> RenameCommand.run ctx args
                | Some(Remove args) -> RemoveCommand.run ctx args
                | Some(Move args) -> MoveCommand.run ctx args
                | Some(Bpa args) -> BpaCommand.run ctx args
                | Some(Set args) -> SetCommand.run ctx args
                | Some(Add args) -> AddCommand.run ctx args
                | Some(Translations args) -> TranslationsCommand.run ctx args
                | Some(Perspective args) -> PerspectiveCommand.run ctx args
                | Some(Script args) -> ScriptCommand.run ctx args
                | _ -> UsageError(parser.Value.PrintUsage())

            match outcome with
            | Done -> ExitCode.ok
            | DoneWith code -> code
            | UsageError message -> usageError message
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
    | :? ArguParseException as e -> usageError e.Message
    | e ->
        if jsonRequested then
            writeJson out (errorJson "internal" e.Message)
        else
            err.WriteLine($"tess: internal error: {e}")

        ExitCode.internalError
