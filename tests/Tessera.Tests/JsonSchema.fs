/// A small JSON Schema (draft-04) checker: enough for the SARIF 2.1.0 schema, and strict about it.
/// A keyword it doesn't implement is an error (so a schema it can't fully check never passes
/// silently); only annotations (description, default, title, id, $schema) are ignored.
module Tessera.Tests.JsonSchema

open System
open System.Text.Json
open System.Text.RegularExpressions

let private annotations =
    set [ "description"; "default"; "title"; "id"; "$schema"; "definitions" ]

let private implemented =
    set [
        "$ref"
        "type"
        "properties"
        "additionalProperties"
        "required"
        "items"
        "minItems"
        "uniqueItems"
        "minimum"
        "maximum"
        "enum"
        "pattern"
        "format"
        "anyOf"
        "oneOf"
        "allOf"
    ]

let private kindOk (t: string) (v: JsonElement) =
    match t, v.ValueKind with
    | "object", JsonValueKind.Object
    | "array", JsonValueKind.Array
    | "string", JsonValueKind.String
    | "boolean", (JsonValueKind.True | JsonValueKind.False)
    | "null", JsonValueKind.Null
    | "number", JsonValueKind.Number -> true
    | "integer", JsonValueKind.Number ->
        let raw = v.GetRawText()
        not (raw.Contains '.' || raw.Contains 'e' || raw.Contains 'E')
    | _ -> false

let private formatOk (format: string) (s: string) =
    match format with
    | "uri" -> Uri.IsWellFormedUriString(s, UriKind.Absolute)
    | "uri-reference" -> Uri.IsWellFormedUriString(s, UriKind.RelativeOrAbsolute)
    | "date-time" ->
        DateTimeOffset.TryParse(s, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None)
        |> fst
    | other -> failwithf "JsonSchema: format '%s' isn't implemented" other

/// Every violation of `schema` (a whole schema document, for its #/definitions) by `instance`,
/// as "path: problem". Empty when it conforms.
let validate (root: JsonElement) (instance: JsonElement) : string list =
    let errors = ResizeArray<string>()

    let resolve (reference: string) =
        if not (reference.StartsWith "#/") then
            failwithf "JsonSchema: only local $ref is implemented, got '%s'" reference

        reference.Substring(2).Split('/')
        |> Array.fold
            (fun (e: JsonElement) (part: string) -> e.GetProperty(part.Replace("~1", "/").Replace("~0", "~")))
            root

    let rec check (schema: JsonElement) (v: JsonElement) (path: string) : string list =
        let local = ResizeArray<string>()
        let fail (message: string) = local.Add $"{path}: {message}"

        for p in schema.EnumerateObject() do
            if not (annotations.Contains p.Name || implemented.Contains p.Name) then
                failwithf "JsonSchema: keyword '%s' isn't implemented (at %s)" p.Name path

        let has (name: string) =
            match schema.TryGetProperty name with
            | true, x -> Some x
            | _ -> None

        match has "$ref" with
        | Some r -> local.AddRange(check (resolve (r.GetString())) v path)
        | None -> ()

        match has "type" with
        | Some t ->
            let types =
                if t.ValueKind = JsonValueKind.Array then
                    [ for x in t.EnumerateArray() -> x.GetString() ]
                else
                    [ t.GetString() ]

            if not (types |> List.exists (fun t -> kindOk t v)) then
                fail $"expected {String.Join(' ', types)}, got {v.ValueKind}"
        | None -> ()

        match has "enum" with
        | Some e ->
            if not (e.EnumerateArray() |> Seq.exists (fun x -> x.GetRawText() = v.GetRawText())) then
                fail $"{v.GetRawText()} is not one of {e.GetRawText()}"
        | None -> ()

        if v.ValueKind = JsonValueKind.Object then
            let props = has "properties"

            match has "required" with
            | Some r ->
                for name in r.EnumerateArray() do
                    match v.TryGetProperty(name.GetString()) with
                    | true, _ -> ()
                    | _ -> fail $"missing required property '{name.GetString()}'"
            | None -> ()

            for p in v.EnumerateObject() do
                let declared =
                    props
                    |> Option.bind (fun ps ->
                        match ps.TryGetProperty p.Name with
                        | true, s -> Some s
                        | _ -> None)

                match declared with
                | Some s -> local.AddRange(check s p.Value $"{path}/{p.Name}")
                | None ->
                    match has "additionalProperties" with
                    | Some a when a.ValueKind = JsonValueKind.False -> fail $"property '{p.Name}' is not allowed"
                    | Some a when a.ValueKind = JsonValueKind.Object ->
                        local.AddRange(check a p.Value $"{path}/{p.Name}")
                    | _ -> ()

        if v.ValueKind = JsonValueKind.Array then
            let items = [ for x in v.EnumerateArray() -> x ]

            match has "minItems" with
            | Some n when items.Length < n.GetInt32() -> fail $"needs at least {n.GetInt32()} items"
            | _ -> ()

            match has "uniqueItems" with
            | Some u when u.ValueKind = JsonValueKind.True ->
                let raw = items |> List.map (fun x -> x.GetRawText())

                if (List.distinct raw).Length <> raw.Length then
                    fail "items are not unique"
            | _ -> ()

            match has "items" with
            | Some s -> items |> List.iteri (fun i x -> local.AddRange(check s x $"{path}/{i}"))
            | None -> ()

        if v.ValueKind = JsonValueKind.Number then
            match has "minimum" with
            | Some m when v.GetDouble() < m.GetDouble() -> fail $"{v.GetRawText()} is below {m.GetRawText()}"
            | _ -> ()

            match has "maximum" with
            | Some m when v.GetDouble() > m.GetDouble() -> fail $"{v.GetRawText()} is above {m.GetRawText()}"
            | _ -> ()

        if v.ValueKind = JsonValueKind.String then
            let s = v.GetString()

            match has "pattern" with
            | Some p when not (Regex.IsMatch(s, p.GetString())) -> fail $"'{s}' doesn't match {p.GetString()}"
            | _ -> ()

            match has "format" with
            | Some f when not (formatOk (f.GetString()) s) -> fail $"'{s}' is not a valid {f.GetString()}"
            | _ -> ()

        let branches name =
            has name
            |> Option.map (fun xs -> [ for x in xs.EnumerateArray() -> check x v path ])

        match branches "allOf" with
        | Some results -> results |> List.iter local.AddRange
        | None -> ()

        match branches "anyOf" with
        | Some results when not (results |> List.exists List.isEmpty) -> fail "matches none of anyOf"
        | _ -> ()

        match branches "oneOf" with
        | Some results when (results |> List.filter List.isEmpty).Length <> 1 ->
            fail "doesn't match exactly one of oneOf"
        | _ -> ()

        List.ofSeq local

    errors.AddRange(check root instance "")
    List.ofSeq errors
