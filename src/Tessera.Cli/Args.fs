module Tessera.Cli.Args

open Argu

type ConvertArgs =
    | [<MainCommand; ExactlyOnce>] Paths of source: string * destination: string
    | [<AltCommandLine("-f")>] Format of format: string
    | Force
    | Eol of eol: string
    | Indent_Spaces of spaces: int
    | Include_Sensitive

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Paths _ -> "model to read, and where to write it."
            | Format _ ->
                "output format: bim | tmdl | pbip. Default: inferred from the destination "
                + "(*.bim -> bim, *.SemanticModel -> pbip, otherwise a tmdl folder)."
            | Force -> "overwrite a non-empty destination."
            | Eol _ ->
                "line endings: auto (default: match files already at the destination, else the source, else lf) "
                + "| lf | crlf."
            | Indent_Spaces _ -> "indent TMDL with this many spaces instead of tabs."
            | Include_Sensitive -> "keep restricted information such as data source passwords."

type InfoArgs =
    | [<MainCommand; ExactlyOnce>] Model of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."

type ListArgs =
    | [<MainCommand; ExactlyOnce>] Model of path: string
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | [<AltCommandLine("-n")>] Name of pattern: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "model path (.bim, TMDL folder, .SemanticModel folder, .pbip, ...)."
            | Kind _ -> "only these kinds (repeatable or comma-separated), e.g. measure,column."
            | Table _ -> "only objects in this table (repeatable)."
            | Name _ -> "only objects whose name matches this wildcard (* and ?), case-insensitive."

type GetArgs =
    | [<MainCommand; ExactlyOnce>] Target of model: string * object: string
    | [<AltCommandLine("-p")>] Property of path: string
    | Children

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "model path, and the object: a ref (measure:Sales/Total), DAX style ('Sales'[Total], [Total]) "
                + "or a bare name."
            | Property _ ->
                "print just this property (repeatable), e.g. expression, source.expression, annotations.PBI_FormatHint."
            | Children -> "include child objects (a table's columns, measures, ...)."

[<HelpFlags("--help", "-h")>]
type CliArgs =
    | [<Inherit; AltCommandLine("-j")>] Json
    | Version
    | [<CliPrefix(CliPrefix.None)>] Convert of ParseResults<ConvertArgs>
    | [<CliPrefix(CliPrefix.None)>] Info of ParseResults<InfoArgs>
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
    | [<CliPrefix(CliPrefix.None)>] Get of ParseResults<GetArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Json -> "write machine-readable JSON to stdout."
            | Version -> "print the version."
            | Convert _ -> "convert a model between .bim, TMDL and PBIP."
            | Info _ -> "summarise a model."
            | List _ -> "list the objects in a model."
            | Get _ -> "show one object (TMDL, or its JSON with --json)."
