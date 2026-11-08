/// The top-level command line. Each command's own arguments live in Commands/<Name>Command.fs.
module Tessera.Cli.Args

open Argu

[<HelpFlags("--help", "-h")>]
type CliArgs =
    | [<Inherit; AltCommandLine("-j")>] Json
    | Version
    | [<CliPrefix(CliPrefix.None)>] Convert of ParseResults<ConvertCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Info of ParseResults<InfoCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Get of ParseResults<GetCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Find of ParseResults<FindCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Diff of ParseResults<DiffCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Deps of ParseResults<DepsCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<InitCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Validate of ParseResults<ValidateCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Rename of ParseResults<RenameCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Bpa of ParseResults<BpaCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Set of ParseResults<SetCommand.Args>
    | [<CliPrefix(CliPrefix.None)>] Add of ParseResults<AddCommand.Args>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Json -> "write machine-readable JSON to stdout."
            | Version -> "print the version."
            | Convert _ -> ConvertCommand.summary
            | Info _ -> InfoCommand.summary
            | List _ -> ListCommand.summary
            | Get _ -> GetCommand.summary
            | Find _ -> FindCommand.summary
            | Diff _ -> DiffCommand.summary
            | Deps _ -> DepsCommand.summary
            | Init _ -> InitCommand.summary
            | Validate _ -> ValidateCommand.summary
            | Rename _ -> RenameCommand.summary
            | Bpa _ -> BpaCommand.summary
            | Set _ -> SetCommand.summary
            | Add _ -> AddCommand.summary
