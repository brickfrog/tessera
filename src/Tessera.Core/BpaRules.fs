/// Best Practice Analyzer rules in Tabular Editor's JSON format: the rule model, scopes, and loading
/// rules from files, from the model's annotations, and from the built-in set.
module Tessera.BpaRules

open System
open System.IO
open System.Text.Json

/// The kinds of object a rule applies to. Names and bit values follow TE2's RuleScope.
[<RequireQualifiedAccess>]
type Scope =
    | Model
    | Table
    | Measure
    | Hierarchy
    | Level
    | Relationship
    | Perspective
    | Culture
    | Partition
    | ProviderDataSource
    | DataColumn
    | CalculatedColumn
    | CalculatedTable
    | CalculatedTableColumn
    | KPI
    | StructuredDataSource
    | Variation
    | NamedExpression
    | ModelRole
    | TablePermission
    | CalculationGroup
    | CalculationItem
    | ModelRoleMember
    | UserDefinedFunction
    | Calendar

module Scope =

    /// Every scope with its TE2 RuleScope flag value, in flag order.
    let all = [
        Scope.Model, 0x0001
        Scope.Table, 0x0002
        Scope.Measure, 0x0004
        Scope.Hierarchy, 0x0008
        Scope.Level, 0x0010
        Scope.Relationship, 0x0020
        Scope.Perspective, 0x0040
        Scope.Culture, 0x0080
        Scope.Partition, 0x0100
        Scope.ProviderDataSource, 0x0200
        Scope.DataColumn, 0x0400
        Scope.CalculatedColumn, 0x0800
        Scope.CalculatedTable, 0x1000
        Scope.CalculatedTableColumn, 0x2000
        Scope.KPI, 0x4000
        Scope.StructuredDataSource, 0x8000
        Scope.Variation, 0x10000
        Scope.NamedExpression, 0x20000
        Scope.ModelRole, 0x40000
        Scope.TablePermission, 0x80000
        Scope.CalculationGroup, 0x100000
        Scope.CalculationItem, 0x200000
        Scope.ModelRoleMember, 0x400000
        Scope.UserDefinedFunction, 0x800000
        Scope.Calendar, 0x1000000
    ]

    /// The name TE writes in rule files, e.g. "CalculatedColumn".
    let name (s: Scope) =
        match s with
        | Scope.KPI -> "KPI"
        | s -> sprintf "%A" s

    let private byName =
        all
        |> List.map (fun (s, _) -> (name s).ToLowerInvariant(), [ s ])
        |> List.append [
            // Older TE versions had a single Column scope and a single DataSource scope.
            "column", [ Scope.DataColumn; Scope.CalculatedColumn; Scope.CalculatedTableColumn ]
            "datasource", [ Scope.ProviderDataSource ]
            "function", [ Scope.UserDefinedFunction ]
            "hierarchies", [ Scope.Hierarchy ]
        ]
        |> dict

    /// One scope name as TE reads it: spaces are ignored, a trailing "s" is optional
    /// ("Calculated Columns"), "Hierarchies" is Hierarchy. Case-insensitive.
    let tryParse (text: string) : Scope list option =
        let key = text.Replace(" ", "").Trim().ToLowerInvariant()

        let lookup (k: string) =
            match byName.TryGetValue k with
            | true, scopes -> Some scopes
            | _ -> None

        match lookup key with
        | Some s -> Some s
        | None when key.EndsWith "s" -> lookup (key.Substring(0, key.Length - 1))
        | None -> None

    /// Scopes from a TE flags value.
    let ofFlags (flags: int) =
        all |> List.filter (fun (_, bit) -> flags &&& bit <> 0) |> List.map fst

/// Where a rule came from. Later sources override earlier ones with the same ID.
type RuleSource =
    | BuiltIn
    | RuleFile of path: string
    /// A file the model lists in its BestPracticeAnalyzer_ExternalRuleFiles annotation.
    | ExternalRuleFile of path: string
    /// The model's BestPracticeAnalyzer annotation.
    | ModelAnnotation

module RuleSource =
    let describe =
        function
        | BuiltIn -> "built-in"
        | RuleFile path -> path
        | ExternalRuleFile path -> path
        | ModelAnnotation -> "model annotation"

type Rule = {
    Id: string
    Name: string
    Category: string
    Description: string option
    /// TE uses 1 (information), 2 (warning) and 3 (error); any integer is accepted.
    Severity: int
    Scopes: Scope list
    /// Scope names this version doesn't know; a rule with any is reported as unsupported.
    UnknownScopes: string list
    Expression: string
    FixExpression: string option
    /// The rule is skipped on models below this compatibility level.
    CompatibilityLevel: int
    LearnMoreUrl: string option
    Source: RuleSource
}

/// The model annotation holding model-embedded rules (and an older misspelling TE also reads).
let modelRulesAnnotations = [ "BestPracticeAnalyzer"; "BestPractizeAnalyzer" ]

/// The annotation listing extra rule files, relative to the model's folder.
let externalRulesAnnotation = "BestPracticeAnalyzer_ExternalRuleFiles"

/// The per-object annotation listing rule IDs to ignore (and the older misspelling).
let ignoreAnnotations = [ "BestPracticeAnalyzer_IgnoreRules"; "BestPractizeAnalyzer_IgnoreRules" ]

type private ResultBuilder() =
    member _.Bind(r: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) = Result.bind f r
    member _.Return(x: 'a) : Result<'a, 'e> = Ok x
    member _.ReturnFrom(r: Result<'a, 'e>) = r

let private result = ResultBuilder()

let private documentOptions =
    JsonDocumentOptions(AllowTrailingCommas = true, CommentHandling = JsonCommentHandling.Skip)

/// A property by name, case-insensitively (TE reads rule files with Newtonsoft, which is).
let private prop (o: JsonElement) (name: string) =
    o.EnumerateObject()
    |> Seq.tryFind (fun p -> String.Equals(p.Name, name, StringComparison.OrdinalIgnoreCase))
    |> Option.map (fun p -> p.Value)
    |> Option.filter (fun v -> v.ValueKind <> JsonValueKind.Null)

let private parseRule (source: RuleSource) (index: int) (o: JsonElement) : Result<Rule, string> =
    let at = $"rule #{index + 1}"

    let str name =
        match prop o name with
        | None -> Ok None
        | Some v when v.ValueKind = JsonValueKind.String -> Ok(Some(v.GetString()))
        | Some _ -> Error $"{at}: \"{name}\" must be a string"

    let int name fallback =
        match prop o name with
        | None -> Ok fallback
        | Some v when v.ValueKind = JsonValueKind.Number && fst (v.TryGetInt32()) -> Ok(v.GetInt32())
        | Some v when v.ValueKind = JsonValueKind.String && fst (Int32.TryParse(v.GetString())) ->
            Ok(Int32.Parse(v.GetString()))
        | Some _ -> Error $"{at}: \"{name}\" must be an integer"

    let required name =
        match str name with
        | Ok(Some s) when not (String.IsNullOrWhiteSpace s) -> Ok s
        | Ok _ -> Error $"{at}: missing \"{name}\""
        | Error e -> Error e

    let scopes () =
        match prop o "Scope" with
        | None -> Error $"{at}: missing \"Scope\""
        | Some v when v.ValueKind = JsonValueKind.Number && fst (v.TryGetInt32()) ->
            Ok(Scope.ofFlags (v.GetInt32()), [])
        | Some v when v.ValueKind = JsonValueKind.String ->
            let parsed =
                v.GetString().Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                |> List.ofArray
                |> List.map (fun n -> n, Scope.tryParse n)

            let known = parsed |> List.collect (snd >> Option.defaultValue []) |> List.distinct
            let unknown = parsed |> List.filter (snd >> Option.isNone) |> List.map fst
            Ok(known, unknown)
        | Some _ -> Error $"{at}: \"Scope\" must be a string such as \"Measure, CalculatedColumn\""

    let nonBlank = Option.filter (String.IsNullOrWhiteSpace >> not)

    result {
        let! id = required "ID"
        let! name = str "Name"
        let! category = str "Category"
        let! description = str "Description"
        let! severity = int "Severity" 1
        let! known, unknown = scopes ()
        let! expression = required "Expression"
        let! fix = str "FixExpression"
        let! compatibilityLevel = int "CompatibilityLevel" 0
        let! url = str "LearnMoreUrl"

        if known.IsEmpty && unknown.IsEmpty then
            return! Error $"{at} ({id}): \"Scope\" is empty"
        else
            return {
                Id = id
                Name = name |> nonBlank |> Option.defaultValue id
                Category = category |> Option.defaultValue ""
                Description = nonBlank description
                Severity = severity
                Scopes = known
                UnknownScopes = unknown
                Expression = expression
                FixExpression = nonBlank fix
                CompatibilityLevel = compatibilityLevel
                LearnMoreUrl = nonBlank url
                Source = source
            }
    }

/// Parse rule JSON: an array of rule objects, as TE writes it.
let parse (source: RuleSource) (json: string) : Result<Rule list, string> =
    try
        use doc = JsonDocument.Parse(json, documentOptions)

        if doc.RootElement.ValueKind <> JsonValueKind.Array then
            Error "expected a JSON array of rules"
        else
            doc.RootElement.EnumerateArray()
            |> Seq.mapi (fun i el ->
                if el.ValueKind = JsonValueKind.Object then
                    parseRule source i el
                else
                    Error $"rule #{i + 1}: expected an object")
            |> Seq.fold
                (fun acc r ->
                    match acc, r with
                    | Error e, _ -> Error e
                    | Ok xs, Ok x -> Ok(x :: xs)
                    | Ok _, Error e -> Error e)
                (Ok [])
            |> Result.map List.rev
    with :? JsonException as e ->
        Error e.Message

/// Load a rule file.
let loadFile (path: string) : Result<Rule list, TessError> =
    if not (File.Exists path) then
        Error(PathNotFound path)
    else
        let text =
            try
                Ok(File.ReadAllText path)
            with e ->
                Error(InvalidRules(path, e.Message))

        text
        |> Result.bind (fun json -> parse (RuleFile path) json |> Result.mapError (fun e -> InvalidRules(path, e)))

/// Rule IDs listed in an ignore annotation: {"RuleIDs": ["ID", ...]} (a bare array is accepted too).
let parseIgnoreList (json: string) : string list =
    try
        use doc = JsonDocument.Parse(json, documentOptions)

        let ids (el: JsonElement) =
            if el.ValueKind = JsonValueKind.Array then
                [
                    for x in el.EnumerateArray() do
                        if x.ValueKind = JsonValueKind.String then
                            x.GetString()
                ]
            else
                []

        match doc.RootElement.ValueKind with
        | JsonValueKind.Array -> ids doc.RootElement
        | JsonValueKind.Object -> prop doc.RootElement "RuleIDs" |> Option.map ids |> Option.defaultValue []
        | _ -> []
    with :? JsonException -> []

/// Paths in an external-rule-files annotation: a JSON array of strings.
let parseExternalRuleFiles (json: string) : Result<string list, string> =
    try
        use doc = JsonDocument.Parse(json, documentOptions)

        if doc.RootElement.ValueKind <> JsonValueKind.Array then
            Error "expected a JSON array of paths"
        else
            Ok [
                for x in doc.RootElement.EnumerateArray() do
                    if x.ValueKind = JsonValueKind.String then
                        x.GetString()
            ]
    with :? JsonException as e ->
        Error e.Message

/// Combine rule sets: a later rule replaces an earlier one with the same ID (case-insensitive),
/// keeping the earlier rule's position.
let merge (ruleSets: Rule list list) : Rule list =
    let order = Collections.Generic.List<string>()

    let byId =
        Collections.Generic.Dictionary<string, Rule>(StringComparer.OrdinalIgnoreCase)

    for rules in ruleSets do
        for r in rules do
            if not (byId.ContainsKey r.Id) then
                order.Add r.Id

            byId.[r.Id] <- r

    [ for id in order -> byId.[id] ]

/// The built-in rules, embedded in this assembly.
let builtIn: Lazy<Rule list> =
    lazy
        (let asm = Reflection.Assembly.GetExecutingAssembly()
         use stream = asm.GetManifestResourceStream "Tessera.BpaRules.json"
         use reader = new StreamReader(stream)

         match parse BuiltIn (reader.ReadToEnd()) with
         | Ok rules -> rules
         | Error e -> failwithf "built-in BPA rules are invalid: %s" e)
