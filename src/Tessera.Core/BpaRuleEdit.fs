/// Editing Best Practice Analyzer rule sets and ignore lists, in Tabular Editor's formats: the rules
/// embedded in a model, rule files, and objects' BestPracticeAnalyzer_IgnoreRules annotations.
/// Rule sets are edited as JSON, so fields tessera doesn't model survive. Model edits are
/// Mutation edits.
module Tessera.BpaRuleEdit

open System
open System.IO
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera.BpaRules

let private jsonOptions (indented: bool) (eol: string) =
    JsonSerializerOptions(
        WriteIndented = indented,
        NewLine = eol,
        // Keep expressions readable: no > for > or & for &.
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

let private documentOptions comments =
    JsonDocumentOptions(
        AllowTrailingCommas = comments,
        CommentHandling =
            (if comments then
                 JsonCommentHandling.Skip
             else
                 JsonCommentHandling.Disallow)
    )

/// A rule set as JSON: the array TE writes.
type RuleSet = {
    Rules: JsonArray
    /// The line ending to write it back with.
    Eol: string
    /// The text had comments or trailing commas, which are not kept.
    HadComments: bool
}

let private eolOf (text: string) =
    if text.Contains "\r\n" then "\r\n" else "\n"

/// Parse a rule set for editing. Each element must be a valid rule.
let parseSet (text: string) : Result<RuleSet, string> =
    let strict =
        try
            JsonNode.Parse(text, documentOptions = documentOptions false) |> ignore
            true
        with :? JsonException ->
            false

    try
        match JsonNode.Parse(text, documentOptions = documentOptions true) with
        | :? JsonArray as a ->
            // Validate the rules the way the analyzer reads them.
            match parse BuiltIn text with
            | Error e -> Error e
            | Ok _ ->
                Ok {
                    Rules = a
                    Eol = eolOf text
                    HadComments = not strict
                }
        | _ -> Error "expected a JSON array of rules"
    with :? JsonException as e ->
        Error e.Message

let emptySet = {
    Rules = JsonArray()
    Eol = "\n"
    HadComments = false
}

/// The set as TE writes it: an indented array.
let formatSet (set: RuleSet) =
    set.Rules.ToJsonString(jsonOptions true set.Eol)

let private idOf (n: JsonNode) =
    match n with
    | :? JsonObject as o ->
        o
        |> Seq.tryFind (fun kv -> String.Equals(kv.Key, "ID", StringComparison.OrdinalIgnoreCase))
        |> Option.bind (fun kv ->
            match kv.Value with
            | :? JsonValue as v ->
                match v.TryGetValue<string>() with
                | true, s -> Some s
                | _ -> None
            | _ -> None)
    | _ -> None

let private sameId (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// The IDs in a set, in order.
let ids (set: RuleSet) =
    set.Rules |> Seq.choose idOf |> List.ofSeq

let private indexOf (set: RuleSet) (id: string) =
    set.Rules |> Seq.tryFindIndex (fun n -> idOf n |> Option.exists (sameId id))

/// Rule objects from JSON text (an array of rules, or one rule), each checked as a rule. Returns
/// the objects as written, with the parsed rules.
let ruleObjects (text: string) : Result<(JsonObject * Rule) list, string> =
    try
        let objects =
            match JsonNode.Parse(text, documentOptions = documentOptions true) with
            | :? JsonArray as a -> Ok(List.ofSeq a)
            | :? JsonObject as o -> Ok [ o :> JsonNode ]
            | _ -> Error "expected a rule (a JSON object) or an array of rules"

        objects
        |> Result.bind (fun nodes ->
            let array = JsonArray(nodes |> List.map (fun n -> n.DeepClone()) |> Array.ofList)

            parse BuiltIn (array.ToJsonString())
            |> Result.map (fun rules -> List.zip (nodes |> List.map (fun n -> n.DeepClone() :?> JsonObject)) rules))
    with
    | :? JsonException as e -> Error e.Message
    | :? InvalidCastException -> Error "expected a rule (a JSON object) or an array of rules"

/// What adding rules to a set did.
type Added = {
    Set: RuleSet
    Added: string list
    /// Rules that replaced one with the same ID (with replace).
    Replaced: string list
}

/// Add rules at the end of a set. A rule whose ID is already there replaces it in place with
/// `replace`; without it, the ID is returned as the error.
let addRules (replace: bool) (rules: JsonObject list) (set: RuleSet) : Result<Added, string> =
    let copy = {
        set with
            Rules = set.Rules.DeepClone() :?> JsonArray
    }

    rules
    |> List.fold
        (fun acc (o: JsonObject) ->
            acc
            |> Result.bind (fun (added, replaced) ->
                let id = idOf o |> Option.defaultValue ""

                match indexOf copy id with
                | Some _ when not replace -> Error id
                | Some i ->
                    copy.Rules.[i] <- o.DeepClone()
                    Ok(added, replaced @ [ id ])
                | None ->
                    copy.Rules.Add(o.DeepClone())
                    Ok(added @ [ id ], replaced)))
        (Ok([], []))
    |> Result.map (fun (added, replaced) -> {
        Set = copy
        Added = added
        Replaced = replaced
    })

/// Remove rules from a set by ID (case-insensitive). A missing ID is returned as the error.
let removeRules (ruleIds: string list) (set: RuleSet) : Result<RuleSet, string> =
    let copy = {
        set with
            Rules = set.Rules.DeepClone() :?> JsonArray
    }

    ruleIds
    |> List.fold
        (fun acc id ->
            acc
            |> Result.bind (fun () ->
                match indexOf copy id with
                | None -> Error id
                | Some i ->
                    copy.Rules.RemoveAt i
                    Ok()))
        (Ok())
    |> Result.map (fun () -> copy)

// ---- rule files ----------------------------------------------------------------------------

/// Read a rule file for editing; a file that doesn't exist yet is an empty set.
let readFile (path: string) : Result<RuleSet, TessError> =
    if not (File.Exists path) then
        Ok emptySet
    else
        try
            parseSet (File.ReadAllText path)
            |> Result.mapError (fun e -> InvalidRules(path, e))
        with e ->
            Error(InvalidRules(path, e.Message))

/// Write a rule set to a file (creating its folder), UTF-8 without BOM, with a final newline.
let writeFile (path: string) (set: RuleSet) : Result<unit, TessError> =
    try
        match Path.GetDirectoryName(Path.GetFullPath path) with
        | null -> ()
        | dir -> Directory.CreateDirectory dir |> ignore

        File.WriteAllText(path, formatSet set + set.Eol, Text.UTF8Encoding(false))
        Ok()
    with e ->
        Error(SaveFailed(path, e.Message))

// ---- rules embedded in the model ------------------------------------------------------------

let private modelAnnotation = List.head modelRulesAnnotations

let private modelWhere = "the model's rules"

/// The model's embedded rule set (empty without the annotation).
let modelSet (m: Model) : Result<RuleSet, TessError> =
    match
        modelRulesAnnotations
        |> List.tryPick (fun n -> Option.ofObj (m.Annotations.Find n))
    with
    | None -> Ok emptySet
    | Some a when String.IsNullOrWhiteSpace a.Value -> Ok emptySet
    | Some a ->
        parseSet a.Value
        |> Result.map (fun s -> { s with Eol = "\n" })
        |> Result.mapError (fun e -> InvalidRules($"the model's {a.Name} annotation", e))

/// Store a rule set in the model's annotation, as TE does: removed when empty. TE's misspelled
/// annotation is replaced by the correct one.
let private storeModelSet (m: Model) (set: RuleSet) =
    for n in List.tail modelRulesAnnotations do
        if not (isNull (m.Annotations.Find n)) then
            m.Annotations.Remove n |> ignore

    match m.Annotations.Find modelAnnotation with
    | null when set.Rules.Count = 0 -> ()
    | null -> m.Annotations.Add(Annotation(Name = modelAnnotation, Value = formatSet set))
    | a when set.Rules.Count = 0 -> m.Annotations.Remove a |> ignore
    | a -> a.Value <- formatSet set

/// Add rules to the model's embedded rules (a Mutation edit).
let addToModel (replace: bool) (rules: JsonObject list) (db: Database) : Result<Mutation.Effect, TessError> =
    modelSet db.Model
    |> Result.bind (fun set ->
        addRules replace rules set
        |> Result.mapError (fun id -> RuleExists(id, "The model"))
        |> Result.map (fun added ->
            storeModelSet db.Model added.Set

            {
                Mutation.Effect.none with
                    Notes = [
                        for id in added.Added -> $"Added rule {id} to {modelWhere}"
                        for id in added.Replaced -> $"Replaced rule {id} in {modelWhere}"
                    ]
            }))

/// Remove rules from the model's embedded rules (a Mutation edit).
let removeFromModel (ruleIds: string list) (db: Database) : Result<Mutation.Effect, TessError> =
    modelSet db.Model
    |> Result.bind (fun set ->
        removeRules ruleIds set
        |> Result.mapError (fun id -> RuleNotFound(id, modelWhere, None))
        |> Result.map (fun set ->
            storeModelSet db.Model set

            {
                Mutation.Effect.none with
                    Notes = [ for id in ruleIds -> $"Removed rule {id} from {modelWhere}" ]
            }))

// ---- ignore annotations ---------------------------------------------------------------------

let private ignoreAnnotation = List.head ignoreAnnotations

/// An object's annotation collection. TOM has one collection type per object type and no common
/// interface, so this goes through reflection.
type Annotations = {
    Find: string -> Annotation option
    Add: Annotation -> unit
    Remove: string -> unit
}

/// An object's annotations, if TOM gives it any (calculation items, for one, have none).
let annotationsOf (o: MetadataObject) : Annotations option =
    match o.GetType().GetProperty("Annotations") with
    | null -> None
    | p ->
        match p.GetValue o with
        | :? Collections.IEnumerable as coll ->
            let t = coll.GetType()
            let add = t.GetMethod("Add", [| typeof<Annotation> |])
            let remove = t.GetMethod("Remove", [| typeof<string> |])

            if isNull add || isNull remove then
                None
            else
                Some {
                    Find =
                        fun name ->
                            coll
                            |> Seq.cast<Annotation>
                            |> Seq.tryFind (fun a -> String.Equals(a.Name, name, StringComparison.Ordinal))
                    Add = fun a -> add.Invoke(coll, [| box a |]) |> ignore
                    Remove = fun name -> remove.Invoke(coll, [| box name |]) |> ignore
                }
        | _ -> None

/// The rule IDs an object's ignore annotation lists (TE's misspelled annotation is read too).
let ignoredOn (annotations: Annotations) : string list =
    ignoreAnnotations
    |> List.tryPick (fun n ->
        match annotations.Find n with
        | Some a when not (String.IsNullOrEmpty a.Value) -> Some a.Value
        | _ -> None)
    |> Option.map parseIgnoreList
    |> Option.defaultValue []

/// The annotation value TE writes: {"RuleIDs":["A","B"]}.
let formatIgnoreList (ruleIds: string list) =
    let o = JsonObject()
    o.["RuleIDs"] <- JsonArray(ruleIds |> List.map (fun id -> JsonValue.Create id :> JsonNode) |> Array.ofList)
    o.ToJsonString(jsonOptions false "\n")

/// Write an ignore list as TE does (replacing the misspelled annotation); an empty list removes
/// the annotation, where TE would write an empty list.
let private storeIgnoreList (annotations: Annotations) (ruleIds: string list) =
    for n in List.tail ignoreAnnotations do
        if (annotations.Find n).IsSome then
            annotations.Remove n

    match annotations.Find ignoreAnnotation, ruleIds with
    | None, [] -> ()
    | None, ids -> annotations.Add(Annotation(Name = ignoreAnnotation, Value = formatIgnoreList ids))
    | Some _, [] -> annotations.Remove ignoreAnnotation
    | Some a, ids -> a.Value <- formatIgnoreList ids

/// Add rule IDs to (ignore = true) or remove them from the ignore annotations of objects (a Mutation
/// edit). Ignoring on the model turns the rules off for the whole model, as in TE.
let setIgnored (ignore: bool) (ruleIds: string list) (objects: string list) (db: Database) =
    let entries = Inventory.objects db

    let objects = if objects.IsEmpty then [ "model" ] else objects

    objects
    |> List.fold
        (fun acc input ->
            acc
            |> Result.bind (fun notes ->
                Inventory.Selector.resolve entries input
                |> Result.bind (fun entry ->
                    let name = ObjectRef.format entry.Ref

                    match annotationsOf entry.Object with
                    | None ->
                        Error(
                            CannotIgnore(
                                name,
                                "TOM gives it no annotations; ignore the rule on the model instead, "
                                + "which turns it off for the whole model"
                            )
                        )
                    | Some annotations ->
                        let current = ignoredOn annotations
                        let has id = current |> List.exists (sameId id)

                        let updated =
                            if ignore then
                                current
                                @ (ruleIds |> List.filter (has >> not) |> List.distinctBy _.ToUpperInvariant())
                            else
                                current |> List.filter (fun c -> not (ruleIds |> List.exists (sameId c)))

                        storeIgnoreList annotations updated

                        let verb = if ignore then "Ignored" else "Stopped ignoring"

                        let changed, unchanged =
                            ruleIds |> List.partition (fun id -> if ignore then not (has id) else has id)

                        let already = if ignore then "already ignored" else "not ignored"

                        Ok(
                            notes
                            @ [
                                for id in changed -> $"{verb} rule {id} on {name}"
                                for id in unchanged -> $"Rule {id} is {already} on {name}"
                            ]
                        ))))
        (Ok [])
    |> Result.map (fun notes -> {
        Mutation.Effect.none with
            Notes = notes
    })
