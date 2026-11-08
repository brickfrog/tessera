/// Text search across every object's properties.
module Tessera.Find

open System
open System.Text.RegularExpressions
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory

type Options = {
    Pattern: string
    /// Treat Pattern as a .NET regular expression instead of literal text.
    Regex: bool
    CaseSensitive: bool
    /// Which objects to search (kind/table/name filters, as for `list`).
    Objects: Filter
    /// Property-path globs to search; empty = all properties except IDs (see excluded).
    Properties: string list
}

module Options =
    let literal pattern = {
        Pattern = pattern
        Regex = false
        CaseSensitive = false
        Objects = Filter.none
        Properties = []
    }

type Hit = {
    Ref: ObjectRef
    /// Property path, e.g. "expression" or "source.expression".
    Path: string
    /// 1-based line within the property value.
    Line: int
    /// The matching line, trimmed (and shortened if very long).
    Text: string
}

/// Never what anyone is searching for; searchable only when asked for by --in.
let excluded = [ "lineageTag"; "sourceLineageTag" ]

let private maxText = 200

let private matcher (opts: Options) : Result<string -> bool, TessError> =
    let options =
        RegexOptions.CultureInvariant
        ||| (if opts.CaseSensitive then
                 RegexOptions.None
             else
                 RegexOptions.IgnoreCase)

    let source =
        if opts.Regex then
            opts.Pattern
        else
            Regex.Escape opts.Pattern

    if opts.Pattern = "" then
        Error(InvalidPattern(opts.Pattern, "empty pattern"))
    else
        try
            let rx = Regex(source, options, TimeSpan.FromSeconds 2.0)
            Ok(fun (s: string) -> rx.IsMatch s)
        with :? ArgumentException as e ->
            Error(InvalidPattern(opts.Pattern, e.Message))

let search (db: Database) (opts: Options) : Result<Hit list, TessError> =
    matcher opts
    |> Result.map (fun isMatch ->
        let wanted =
            match opts.Properties |> List.map Serialize.pathGlob with
            | [] ->
                let skip = excluded |> List.map Serialize.pathGlob
                fun path -> not (skip |> List.exists (fun g -> g path))
            | globs -> fun path -> globs |> List.exists (fun g -> g path)

        [
            for e in objects db |> Filter.apply opts.Objects do
                for path, value in Serialize.properties db e do
                    if wanted path then
                        for i, line in value.Split('\n') |> Array.indexed do
                            if isMatch line then
                                let text = line.Trim()

                                {
                                    Ref = e.Ref
                                    Path = path
                                    Line = i + 1
                                    Text =
                                        if text.Length > maxText then
                                            text.Substring(0, maxText) + "…"
                                        else
                                            text
                                }
        ])
