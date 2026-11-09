/// `tess query`: run a DAX query against a model on a server and print the result.
module Tessera.Cli.QueryCommand

open System
open System.Diagnostics
open System.IO
open Argu
open Tessera
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "run a DAX query on a server model (xmla:<server>;<database>): a table, --csv or --json."

/// Rows shown per result unless --max-rows says otherwise.
let defaultMaxRows = 1000

type Args =
    | [<MainCommand>] Target of string list
    | [<AltCommandLine("-f")>] File of path: string
    | Csv
    | Max_Rows of rows: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model, xmla:<server>[;<database>] (quote it: `;` ends a shell command), followed by the DAX "
                + "query, or `-` to read it from standard input."
            | File _ -> "read the DAX query from this file."
            | Csv -> "write CSV (several results are separated by an empty line)."
            | Max_Rows _ ->
                $"rows to return per result (default {defaultMaxRows}; 0 for all). A result cut short is marked truncated."

let private queryJson (source: string) (results: QueryResult list) (elapsed: TimeSpan) =
    obj [
        "command", str "query"
        "source", str source
        "results", QueryResult.listToJson results
        "durationMs", num (int elapsed.TotalMilliseconds)
    ]

let private readDax (args: ParseResults<Args>) (rest: string list) : Result<string, Outcome> =
    match args.TryGetResult Args.File, rest with
    | Some _, _ :: _ -> Error(UsageError "give the query as an argument or with --file, not both")
    | Some path, [] ->
        try
            Ok(File.ReadAllText path)
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException -> Error(Failed(PathNotFound(Path.GetFullPath path)))
        | e -> Error(Failed(LoadFailed(path, e.Message)))
    | None, [ "-" ] -> Ok(Console.In.ReadToEnd())
    | None, [ dax ] -> Ok dax
    | None, [] -> Error(UsageError "expected a DAX query: an argument, `-` (standard input) or --file <path>")
    | None, _ -> Error(UsageError "expected one DAX query (quote it)")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let maxRows = args.TryGetResult Args.Max_Rows |> Option.defaultValue defaultMaxRows

    match args.TryGetResult Args.Target |> Option.defaultValue [] with
    | [] -> UsageError "expected a model (xmla:<server>;<database>) and a DAX query"
    | _ when maxRows < 0 -> UsageError "--max-rows must be 0 (all rows) or more"
    | _ when ctx.Json && args.Contains Args.Csv -> UsageError "--json and --csv can't be combined"
    | source :: _ when not (XmlaAddress.isRemote source) ->
        UsageError
            $"'{source}' is not a server model: tess runs queries on a server only (xmla:<server>;<database>), not yet on model files"
    | source :: rest ->
        match readDax args rest with
        | Error outcome -> outcome
        | Ok dax when String.IsNullOrWhiteSpace dax -> UsageError "the DAX query is empty"
        | Ok dax ->
            let clock = Stopwatch.StartNew()
            let limit = if maxRows = 0 then None else Some maxRows

            match
                XmlaAddress.parse source
                |> Result.bind (fun address -> Remote.query (Source.access ctx) address dax limit)
            with
            | Error e -> Failed e
            | Ok(address, results) ->
                if ctx.Json then
                    writeJson ctx.Out (queryJson (XmlaAddress.display address) results clock.Elapsed)
                elif args.Contains Args.Csv then
                    results
                    |> List.iteri (fun i r ->
                        if i > 0 then
                            ctx.Out.WriteLine()

                        ctx.Out.Write(QueryResult.toCsv ctx.Out.NewLine r))
                else
                    results
                    |> List.iteri (fun i r ->
                        if i > 0 then
                            ctx.Out.WriteLine()

                        for line in QueryResult.toTable r do
                            ctx.Out.WriteLine line)

                if not ctx.Json && results |> List.exists _.Truncated then
                    ctx.Err.WriteLine($"Showing the first {maxRows} rows of each result (--max-rows 0 for all).")

                Done
