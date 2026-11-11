/// "Did you mean": the known names closest to one that wasn't found.
module Tessera.Suggest

open System

/// Edit distance between two strings, ignoring case: insertions, deletions, substitutions and
/// swaps of two adjacent characters each count one (Damerau-Levenshtein, optimal string alignment).
let distance (a: string) (b: string) =
    let a = a.ToLowerInvariant()
    let b = b.ToLowerInvariant()
    let d = Array2D.zeroCreate (a.Length + 1) (b.Length + 1)

    for i in 0 .. a.Length do
        d.[i, 0] <- i

    for j in 0 .. b.Length do
        d.[0, j] <- j

    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)

            if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1] then
                d.[i, j] <- min d.[i, j] (d.[i - 2, j - 2] + 1)

    d.[a.Length, b.Length]

/// How many edits still count as a typo of a name this long: about a third of it, from 1 to 3.
let threshold (length: int) = max 1 (min 3 (length / 3))

/// The candidates within `threshold` edits of `input` (case-insensitive), closest first (ties in
/// the candidates' order), at most three. A candidate equal to the input is not a suggestion.
let closest (candidates: string seq) (input: string) : string list =
    let limit = threshold input.Length

    candidates
    |> Seq.distinct
    |> Seq.filter (fun c -> not (String.Equals(c, input, StringComparison.Ordinal)))
    |> Seq.mapi (fun i c -> c, distance c input, i)
    |> Seq.filter (fun (_, d, _) -> d <= limit)
    |> Seq.sortBy (fun (_, d, i) -> d, i)
    |> Seq.truncate 3
    |> Seq.map (fun (c, _, _) -> c)
    |> List.ofSeq

/// " (did you mean 'a' or 'b'?)", or "" with no suggestions.
let hint (suggestions: string list) =
    match suggestions with
    | [] -> ""
    | s ->
        let quoted = s |> List.map (fun x -> $"'{x}'") |> String.concat " or "
        $" (did you mean {quoted}?)"

/// "unknown <what> '<value>' (did you mean '...'?) (known: a, b, c)", and the suggestions.
let unknown (what: string) (value: string) (known: string list) : string * string list =
    let suggestions = closest known value
    let list = String.concat ", " known
    $"unknown {what} '{value}'{hint suggestions} (known: {list})", suggestions
