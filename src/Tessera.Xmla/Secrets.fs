/// Keeping credentials out of everything tess prints. Every message that comes back from a client
/// library or a server passes through `scrub` before it becomes an error.
module Tessera.Xmla.Secrets

open System
open System.Text.RegularExpressions

let masked = "***"

let private options = RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant

/// password=...; in a connection string (quoted or not).
let private passwordPair =
    Regex("""\b(password|pwd)\s*=\s*("(?:[^"]|"")*"|'(?:[^']|'')*'|[^;\s]*)""", options)

/// A JSON Web Token: three base64url parts, the first starting with a JSON object's `{"`.
let private jwt =
    Regex("""\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]*""", RegexOptions.CultureInvariant)

let private bearer = Regex("""\b(Bearer)\s+[A-Za-z0-9._~+/=-]+""", options)

/// Remove the given secrets, and anything that looks like one (a password in a connection string, a
/// bearer token, a JWT), from a text.
let scrub (secrets: string list) (text: string) : string =
    if String.IsNullOrEmpty text then
        ""
    else
        let known =
            secrets
            |> List.filter (fun s -> not (String.IsNullOrEmpty s))
            // Longest first, so a secret that contains another is removed whole.
            |> List.sortByDescending _.Length
            |> List.fold (fun (t: string) s -> t.Replace(s, masked)) text

        let t = passwordPair.Replace(known, fun m -> m.Groups.[1].Value + "=" + masked)
        let t = bearer.Replace(t, fun m -> m.Groups.[1].Value + " " + masked)
        jwt.Replace(t, masked)
