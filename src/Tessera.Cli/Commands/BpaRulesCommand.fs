/// `tess bpa rules ...`: list and show the effective BPA rules with the layer each comes from, add
/// and remove rules in the model or a rule file, and ignore rules on objects. Also the rule-source
/// switches `bpa` shares. Compiled before BpaCommand, whose Args hold this subcommand.
module Tessera.Cli.BpaRulesCommand

open System
open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.BpaRules
open Tessera.Cli.Output
open Tessera.Cli.Common

/// Usage text for the rule-source switches, shared with `bpa`.
module SourceUsage =
    let rules =
        "a Tabular Editor BPA rule file (JSON); highest precedence (repeatable, later wins)."

    let noBuiltin = "don't use tessera's built-in rules."

    let builtinOnly =
        "use only the built-in rules: no --rules, TESS_BPA_RULES, config files or model rules."

    let noModelRules =
        "ignore the rules the model carries (its BestPracticeAnalyzer annotations)."

    let noConfig =
        "ignore TESS_BPA_RULES and the .tessera/bpa.json and user config files."

    let allowExternal =
        "fetch https:// rule files the model lists in BestPracticeAnalyzer_ExternalRuleFiles (off by default: "
        + "tess makes no network requests without this)."

/// The rule-source switches, as parsed.
type Sources = {
    Rules: string list
    NoBuiltin: bool
    BuiltinOnly: bool
    NoModelRules: bool
    NoConfig: bool
    AllowExternal: bool
}

/// The environment `bpa` reads its config from.
let environment (ctx: Ctx) : Bpa.RuleEnvironment = {
    GetEnv = ctx.GetEnv
    IsWindows = OperatingSystem.IsWindows()
}

/// Rule options from the switches; contradictory ones are a usage error.
let ruleOptions (ctx: Ctx) (s: Sources) : Result<Bpa.RuleOptions, string> =
    if s.BuiltinOnly && not s.Rules.IsEmpty then
        Error "--builtin-only can't be combined with --rules"
    elif s.BuiltinOnly && s.NoBuiltin then
        Error "--builtin-only can't be combined with --no-builtin"
    elif s.BuiltinOnly then
        Ok Bpa.RuleOptions.builtInOnly
    else
        Ok {
            Bpa.RuleOptions.defaults with
                BuiltIn = not s.NoBuiltin
                Files = s.Rules
                ModelRules = not s.NoModelRules
                Config = not s.NoConfig
                AllowExternal = s.AllowExternal
                Fetch = BpaFetch.fetch
                Environment = environment ctx
        }

/// What happened while collecting rules, on stderr: requested URLs (always, even with --json, so a
/// network request never goes unmentioned), then notes and warnings (text mode; JSON has them).
let reportCollected (ctx: Ctx) (command: string) (c: Bpa.CollectedRules) =
    for url in c.Fetched do
        ctx.Err.WriteLine $"tess: {command}: requested external rules from {url}"

    if not ctx.Json then
        for n in c.Notes do
            ctx.Err.WriteLine $"tess: {command}: note: {n}"

        for w in c.Warnings do
            ctx.Err.WriteLine $"tess: {command}: warning: {w}"

/// The JSON keys describing a collection: warnings, notes, fetched.
let collectedFields (c: Bpa.CollectedRules) = [
    "warnings", arr (c.Warnings |> List.map str)
    "notes", arr (c.Notes |> List.map str)
    "fetched", arr (c.Fetched |> List.map str)
]

let private loadRules (ctx: Ctx) (modelPath: string) (sources: Sources) =
    match ruleOptions ctx sources with
    | Error message -> Error(Choice1Of2 message)
    | Ok opts ->
        ModelIO.loadPath modelPath
        |> Result.bind (fun (source, db) -> Bpa.collectRules source db opts |> Result.map (fun c -> db, c))
        |> Result.mapError Choice2Of2

let private ofLoadError =
    function
    | Choice1Of2 message -> UsageError message
    | Choice2Of2 e -> Failed e

let private scopesText (r: Rule) =
    (r.Scopes |> List.map Scope.name) @ r.UnknownScopes |> String.concat ", "

let private severityName (s: int) =
    if s >= 3 then "error"
    elif s = 2 then "warning"
    else "info"

/// Rule IDs the model's own ignore annotation turns off.
let private disabledIds (db: Microsoft.AnalysisServices.Tabular.Database) =
    BpaRuleEdit.annotationsOf db.Model
    |> Option.map BpaRuleEdit.ignoredOn
    |> Option.defaultValue []
    |> List.map (fun s -> s.ToUpperInvariant())
    |> Set.ofList

let private sourceJson (s: RuleSource) =
    obj [
        "layer", str (RuleLayer.token (RuleSource.layer s))
        "source", str (RuleSource.describe s)
    ]

let private ruleJson (c: Bpa.CollectedRules) (disabled: Set<string>) (r: Rule) =
    let support = Bpa.compile r

    obj [
        "id", str r.Id
        "name", str r.Name
        "category", str r.Category
        "severity", num r.Severity
        "scope", str (scopesText r)
        "expression", str r.Expression
        "fixExpression", optStr r.FixExpression
        "description", optStr r.Description
        "compatibilityLevel", num r.CompatibilityLevel
        "learnMoreUrl", optStr r.LearnMoreUrl
        "layer", str (RuleLayer.token (RuleSource.layer r.Source))
        "source", str (RuleSource.describe r.Source)
        "overrides", arr (Bpa.CollectedRules.overridden c r |> List.map (fun o -> sourceJson o.Source))
        "disabled", bool (disabled.Contains(r.Id.ToUpperInvariant()))
        "supported", bool (Result.isOk support)
        "error",
        (match support with
         | Error e -> str e
         | Ok() -> null)
    ]

// ---- list / show ----------------------------------------------------------------------------

type ListArgs =
    | [<MainCommand; ExactlyOnce>] Model of path: string
    | Layer of layer: string
    | [<AltCommandLine("-r")>] Rules of file: string
    | [<CustomCommandLine("--no-builtin"); AltCommandLine("--no-default-rules")>] No_Builtin
    | Builtin_Only
    | No_Model_Rules
    | No_Config
    | Allow_External_Rules

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "the model."
            | Layer _ ->
                "only rules from these layers: builtin, model, config, env, cli (repeatable or comma-separated)."
            | Rules _ -> SourceUsage.rules
            | No_Builtin -> SourceUsage.noBuiltin
            | Builtin_Only -> SourceUsage.builtinOnly
            | No_Model_Rules -> SourceUsage.noModelRules
            | No_Config -> SourceUsage.noConfig
            | Allow_External_Rules -> SourceUsage.allowExternal

type ShowArgs =
    | [<MainCommand; ExactlyOnce>] Target of model_and_rule: string * string
    | [<AltCommandLine("-r")>] Rules of file: string
    | [<CustomCommandLine("--no-builtin"); AltCommandLine("--no-default-rules")>] No_Builtin
    | Builtin_Only
    | No_Model_Rules
    | No_Config
    | Allow_External_Rules

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, and the rule ID."
            | Rules _ -> SourceUsage.rules
            | No_Builtin -> SourceUsage.noBuiltin
            | Builtin_Only -> SourceUsage.builtinOnly
            | No_Model_Rules -> SourceUsage.noModelRules
            | No_Config -> SourceUsage.noConfig
            | Allow_External_Rules -> SourceUsage.allowExternal

let private layerNames = [
    RuleLayer.BuiltIn
    RuleLayer.Model
    RuleLayer.Config
    RuleLayer.Env
    RuleLayer.CommandLine
]

let private list (ctx: Ctx) (args: ParseResults<ListArgs>) =
    let sources = {
        Rules = args.GetResults ListArgs.Rules
        NoBuiltin = args.Contains ListArgs.No_Builtin
        BuiltinOnly = args.Contains ListArgs.Builtin_Only
        NoModelRules = args.Contains ListArgs.No_Model_Rules
        NoConfig = args.Contains ListArgs.No_Config
        AllowExternal = args.Contains ListArgs.Allow_External_Rules
    }

    let layers =
        parseList (args.GetResults ListArgs.Layer)
        |> List.map (fun t ->
            match layerNames |> List.tryFind (fun l -> RuleLayer.token l = t.ToLowerInvariant()) with
            | Some l -> Ok l
            | None -> Error $"--layer: unknown layer '{t}' (known: builtin, model, config, env, cli)")

    match layers |> List.tryPick (fun r -> if Result.isError r then Some r else None) with
    | Some(Error message) -> UsageError message
    | _ ->
        let layers = layers |> List.choose Result.toOption |> Set.ofList

        match loadRules ctx (args.GetResult ListArgs.Model) sources with
        | Error e -> ofLoadError e
        | Ok(db, c) ->
            reportCollected ctx "bpa" c
            let disabled = disabledIds db

            let rules =
                c.Effective
                |> List.filter (fun r -> layers.IsEmpty || layers.Contains(RuleSource.layer r.Source))

            if ctx.Json then
                writeJson
                    ctx.Out
                    (obj [
                        "rules", arr (rules |> List.map (ruleJson c disabled))
                        yield! collectedFields c
                    ])
            else
                let width = rules |> List.map (fun r -> r.Id.Length) |> List.fold max 0

                let layerWidth =
                    rules
                    |> List.map (fun r -> (RuleLayer.token (RuleSource.layer r.Source)).Length)
                    |> List.fold max 0

                for r in rules do
                    let layer = RuleLayer.token (RuleSource.layer r.Source)

                    let source =
                        match r.Source with
                        | BuiltIn -> ""
                        | s -> $"  ({RuleSource.describe s})"

                    let overrides =
                        match Bpa.CollectedRules.overridden c r with
                        | [] -> ""
                        | os ->
                            let names =
                                os
                                |> List.map (fun o -> RuleLayer.token (RuleSource.layer o.Source))
                                |> List.distinct

                            let joined = String.concat ", " names
                            $"  [overrides {joined}]"

                    let flags = [
                        if disabled.Contains(r.Id.ToUpperInvariant()) then
                            "  [ignored on the model]"
                        match Bpa.compile r with
                        | Error e -> $"  [unsupported: {e}]"
                        | Ok() -> ()
                    ]

                    ctx.Out.WriteLine(
                        $"{r.Id.PadRight width}  {r.Severity}  {layer.PadRight layerWidth}  {r.Name}{source}{overrides}"
                        + String.concat "" flags
                    )

            Done

let private findRule (rules: Rule list) (id: string) =
    rules
    |> List.tryFind (fun r -> String.Equals(r.Id, id, StringComparison.OrdinalIgnoreCase))

let private show (ctx: Ctx) (args: ParseResults<ShowArgs>) =
    let modelPath, ruleId = args.GetResult ShowArgs.Target

    let sources = {
        Rules = args.GetResults ShowArgs.Rules
        NoBuiltin = args.Contains ShowArgs.No_Builtin
        BuiltinOnly = args.Contains ShowArgs.Builtin_Only
        NoModelRules = args.Contains ShowArgs.No_Model_Rules
        NoConfig = args.Contains ShowArgs.No_Config
        AllowExternal = args.Contains ShowArgs.Allow_External_Rules
    }

    match loadRules ctx modelPath sources with
    | Error e -> ofLoadError e
    | Ok(db, c) ->
        reportCollected ctx "bpa" c

        match findRule c.Effective ruleId with
        | None -> Failed(RuleNotFound(ruleId, "the effective rules", None))
        | Some r ->
            let disabled = disabledIds db

            if ctx.Json then
                let o = ruleJson c disabled r :?> JsonObject

                for k, v in collectedFields c do
                    o.[k] <- v

                writeJson ctx.Out o
            else
                let line (label: string) (value: string) =
                    ctx.Out.WriteLine $"""{(label + ":").PadRight 15}{value}"""

                line "ID" r.Id
                line "Name" r.Name

                if r.Category <> "" then
                    line "Category" r.Category

                line "Severity" $"{r.Severity} ({severityName r.Severity})"
                line "Scope" (scopesText r)

                if r.CompatibilityLevel > 0 then
                    line "Compat level" (string r.CompatibilityLevel)

                line "Layer" (RuleLayer.token (RuleSource.layer r.Source))
                line "Source" (RuleSource.describe r.Source)

                for o in Bpa.CollectedRules.overridden c r do
                    line "Overrides" $"{RuleLayer.token (RuleSource.layer o.Source)} ({RuleSource.describe o.Source})"

                if disabled.Contains(r.Id.ToUpperInvariant()) then
                    line "Status" "ignored on the model (BestPracticeAnalyzer_IgnoreRules)"

                match Bpa.compile r with
                | Error e -> line "Supported" $"no: {e}"
                | Ok() -> ()

                r.LearnMoreUrl |> Option.iter (line "Learn more")
                r.Description |> Option.iter (line "Description")
                line "Expression" ""

                for l in r.Expression.Replace("\r\n", "\n").Split('\n') do
                    ctx.Out.WriteLine $"  {l}"

                match r.FixExpression with
                | Some fix ->
                    line "Fix" ""

                    for l in fix.Replace("\r\n", "\n").Split('\n') do
                        ctx.Out.WriteLine $"  {l}"
                | None -> ()

            Done

// ---- add / remove ---------------------------------------------------------------------------

type AddArgs =
    | [<MainCommand; ExactlyOnce>] Model of path: string
    | To of target: string
    | Id of id: string
    | Name of name: string
    | Category of category: string
    | Description of text: string
    | Severity of level: int
    | Scope of scope: string
    | [<AltCommandLine("-e")>] Expression of expression: string
    | Fix_Expression of expression: string
    | Compatibility_Level of level: int
    | Learn_More_Url of url: string
    | From of file: string
    | Replace
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Model _ -> "the model."
            | To _ -> "where to add it: `model` (the model's own rules; default) or a rule file (created if missing)."
            | Id _ -> "the rule's ID."
            | Name _ -> "its name (default: the ID)."
            | Category _ -> "its category."
            | Description _ -> "its description."
            | Severity _ -> "1 info (default), 2 warning, 3 error."
            | Scope _ -> "the kinds of object it checks, e.g. \"Measure, CalculatedColumn\"."
            | Expression _ -> "the Dynamic LINQ predicate an object violates the rule by matching."
            | Fix_Expression _ -> "TE's fix expression (stored; tess doesn't apply it)."
            | Compatibility_Level _ -> "the lowest compatibility level it runs on (default 1200)."
            | Learn_More_Url _ -> "a link to documentation."
            | From _ -> "copy the rules in this TE rule file instead (all of them, as written)."
            | Replace -> "replace a rule with the same ID instead of refusing."
            | Save -> "write the change (the model in place, or the rule file); default: dry run."
            | Save_To _ -> Mutation.Usage.saveTo + " Only with --to model."
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

type RemoveArgs =
    | [<MainCommand; ExactlyOnce>] Target of model_and_rules: string list
    | [<AltCommandLine("--from")>] To of target: string
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "the model, then the IDs of the rules to remove."
            | To _ -> "where to remove them from: `model` (the model's own rules; default) or a rule file."
            | Save -> "write the change (the model in place, or the rule file); default: dry run."
            | Save_To _ -> Mutation.Usage.saveTo + " Only with --to model."
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

/// The rule object built from add's switches, in the property order TE writes.
let private ruleFromSwitches (args: ParseResults<AddArgs>) : Result<JsonObject, string> =
    let need (name: string) (v: string option) =
        match v with
        | Some s when not (String.IsNullOrWhiteSpace s) -> Ok s
        | _ -> Error $"--{name} is required (or use --from <file>)"

    need "id" (args.TryGetResult AddArgs.Id)
    |> Result.bind (fun id ->
        need "scope" (args.TryGetResult AddArgs.Scope)
        |> Result.bind (fun scope ->
            need "expression" (args.TryGetResult AddArgs.Expression)
            |> Result.map (fun expression ->
                let o = JsonObject()
                let text (v: string) : JsonNode = JsonValue.Create v
                o.["ID"] <- text id
                o.["Name"] <- text (args.TryGetResult AddArgs.Name |> Option.defaultValue id)

                args.TryGetResult AddArgs.Category
                |> Option.filter ((<>) "")
                |> Option.iter (fun c -> o.["Category"] <- text c)

                args.TryGetResult AddArgs.Description
                |> Option.filter ((<>) "")
                |> Option.iter (fun d -> o.["Description"] <- text d)

                o.["Severity"] <- JsonValue.Create(args.TryGetResult AddArgs.Severity |> Option.defaultValue 1)
                o.["Scope"] <- text scope
                o.["Expression"] <- text expression

                args.TryGetResult AddArgs.Fix_Expression
                |> Option.filter (String.IsNullOrWhiteSpace >> not)
                |> Option.iter (fun f -> o.["FixExpression"] <- text f)

                o.["CompatibilityLevel"] <-
                    JsonValue.Create(args.TryGetResult AddArgs.Compatibility_Level |> Option.defaultValue 1200)

                args.TryGetResult AddArgs.Learn_More_Url
                |> Option.filter ((<>) "")
                |> Option.iter (fun u -> o.["LearnMoreUrl"] <- text u)

                o)))

/// Where `add`/`remove` write.
type private Destination =
    | ToModel
    | ToFile of path: string

let private destination (value: string option) =
    match value with
    | None -> ToModel
    | Some v when v.Equals("model", StringComparison.OrdinalIgnoreCase) -> ToModel
    | Some path -> ToFile(Path.GetFullPath path)

/// Show and (with --save) write a change to a rule file. Rule files are not models, so this is the
/// file analogue of the editing pipeline: dry run by default, the change on stdout, notes on stderr.
let private runFileEdit
    (ctx: Ctx)
    (command: string)
    (save: bool)
    (path: string)
    (warnings: string list)
    (edit: BpaRuleEdit.RuleSet -> Result<BpaRuleEdit.RuleSet * string list * string list, TessError>)
    =
    match BpaRuleEdit.readFile path |> Result.bind edit with
    | Error e -> Failed e
    | Ok(set, added, removed) ->
        let notes = [
            if set.HadComments then
                yield $"comments and trailing commas in {path} are not kept"
            for id in removed do
                yield $"Removed rule {id} from {path}"
            for id in added do
                yield $"Added rule {id} to {path}"
        ]

        let ruleText (id: string) =
            set.Rules
            |> Seq.tryPick (fun n ->
                match n with
                | :? JsonObject as o when
                    o
                    |> Seq.exists (fun kv ->
                        String.Equals(kv.Key, "ID", StringComparison.OrdinalIgnoreCase)
                        && kv.Value.ToString().Equals(id, StringComparison.OrdinalIgnoreCase))
                    ->
                    Some(
                        BpaRuleEdit.formatSet {
                            set with
                                Rules = JsonArray(o.DeepClone())
                        }
                    )
                | _ -> None)

        let written =
            if save then
                BpaRuleEdit.writeFile path set |> Result.map (fun () -> true)
            else
                Ok false

        match written with
        | Error e -> Failed e
        | Ok saved ->
            if ctx.Json then
                writeJson
                    ctx.Out
                    (obj [
                        "command", str command
                        "file", str path
                        "dryRun", bool (not save)
                        "added", arr (added |> List.map str)
                        "removed", arr (removed |> List.map str)
                        "warnings", arr (warnings |> List.map str)
                        "notes", arr (notes |> List.map str)
                        "saved", (if saved then str path else null)
                    ])
            else
                for id in removed do
                    ctx.Out.WriteLine $"- rule {id}"

                for id in added do
                    ctx.Out.WriteLine $"+ rule {id}"

                    ruleText id
                    |> Option.iter (fun t ->
                        // The rule as written, without the enclosing array's brackets.
                        let lines = t.Replace("\r\n", "\n").Split('\n')

                        for l in lines.[1 .. lines.Length - 2] do
                            ctx.Out.WriteLine $"+ {l.Substring(min 2 l.Length)}")

                for n in notes do
                    ctx.Err.WriteLine n

                for w in warnings do
                    ctx.Err.WriteLine $"tess: warning: {w}"

                if saved then
                    ctx.Err.WriteLine $"Saved {path}"
                else
                    ctx.Err.WriteLine "Dry run - nothing saved. Add --save to persist."

            Done

/// Warnings for rules that won't run as written.
let private compileWarnings (rules: Rule list) = [
    for r in rules do
        match Bpa.compile r with
        | Error e -> $"rule {r.Id} won't run: {e}"
        | Ok() -> ()
]

let private withWarnings (warnings: string list) (edit: _ -> Result<Mutation.Effect, TessError>) db =
    edit db
    |> Result.map (fun (e: Mutation.Effect) -> {
        e with
            Warnings = e.Warnings @ warnings
    })

let private editOptions save saveTo force stat nameOnly (dest: Destination) =
    match dest, saveTo, force, stat, nameOnly with
    | ToFile _, Some _, _, _, _ -> Error "--save-to only works with --to model (a rule file is written in place)"
    | ToFile _, _, true, _, _
    | ToFile _, _, _, true, _
    | ToFile _, _, _, _, true -> Error "--force, --stat and --name-only only work with --to model"
    | _ -> Mutation.options save saveTo force stat nameOnly

let private add (ctx: Ctx) (args: ParseResults<AddArgs>) =
    let dest = destination (args.TryGetResult AddArgs.To)
    let replace = args.Contains AddArgs.Replace

    let hasSwitches =
        args.Contains AddArgs.Id
        || args.Contains AddArgs.Name
        || args.Contains AddArgs.Category
        || args.Contains AddArgs.Description
        || args.Contains AddArgs.Severity
        || args.Contains AddArgs.Scope
        || args.Contains AddArgs.Expression
        || args.Contains AddArgs.Fix_Expression
        || args.Contains AddArgs.Compatibility_Level
        || args.Contains AddArgs.Learn_More_Url

    let rules =
        match args.TryGetResult AddArgs.From with
        | Some _ when hasSwitches -> Error(Choice1Of2 "--from can't be combined with --id, --scope, --expression, ...")
        | Some file ->
            if not (File.Exists file) then
                Error(Choice2Of2(PathNotFound file))
            else
                BpaRuleEdit.ruleObjects (File.ReadAllText file)
                |> Result.mapError (fun e -> Choice2Of2(InvalidRules(file, e)))
        | None ->
            ruleFromSwitches args
            |> Result.mapError Choice1Of2
            |> Result.bind (fun o ->
                BpaRuleEdit.ruleObjects (o.ToJsonString())
                |> Result.mapError (fun e -> Choice1Of2 $"invalid rule: {e}"))

    let opts =
        editOptions
            (args.Contains AddArgs.Save)
            (args.TryGetResult AddArgs.Save_To)
            (args.Contains AddArgs.Force)
            (args.Contains AddArgs.Stat)
            (args.Contains AddArgs.Name_Only)
            dest

    match rules, opts with
    | Error(Choice1Of2 message), _
    | _, Error message -> UsageError message
    | Error(Choice2Of2 e), _ -> Failed e
    | Ok rules, Ok opts ->
        let objects = rules |> List.map fst
        let warnings = compileWarnings (rules |> List.map snd)
        let modelPath = args.GetResult AddArgs.Model

        match dest with
        | ToModel ->
            Mutation.run
                ctx
                "bpa rules add"
                opts
                modelPath
                (withWarnings warnings (BpaRuleEdit.addToModel replace objects))
        | ToFile path ->
            runFileEdit ctx "bpa rules add" (args.Contains AddArgs.Save) path warnings (fun set ->
                BpaRuleEdit.addRules replace objects set
                |> Result.mapError (fun id -> RuleExists(id, path))
                |> Result.map (fun a -> a.Set, a.Added @ a.Replaced, a.Replaced))

/// Where else a rule is defined, for a "not found" message.
let private definedElsewhere (ctx: Ctx) (source: ModelSource) db (id: string) =
    let opts = {
        Bpa.RuleOptions.defaults with
            Environment = environment ctx
    }

    match Bpa.collectRules source db opts with
    | Ok c ->
        findRule c.Effective id
        |> Option.map (fun r ->
            match r.Source with
            | BuiltIn -> $"it is a built-in rule; `tess bpa rules ignore <model> {r.Id}` turns it off for the model"
            | s -> $"it comes from {RuleSource.describe s}")
    | Error _ -> None

let private remove (ctx: Ctx) (args: ParseResults<RemoveArgs>) =
    let dest = destination (args.TryGetResult RemoveArgs.To)

    let opts =
        editOptions
            (args.Contains RemoveArgs.Save)
            (args.TryGetResult RemoveArgs.Save_To)
            (args.Contains RemoveArgs.Force)
            (args.Contains RemoveArgs.Stat)
            (args.Contains RemoveArgs.Name_Only)
            dest

    match args.GetResult RemoveArgs.Target, opts with
    | _, Error message -> UsageError message
    | [ _ ], _
    | [], _ -> UsageError "give the model and at least one rule ID"
    | modelPath :: ids, Ok opts ->
        let ids = parseList ids

        match dest with
        | ToModel ->
            match ModelSource.resolve modelPath with
            | Error e -> Failed e
            | Ok source ->
                Mutation.run ctx "bpa rules remove" opts modelPath (fun db ->
                    BpaRuleEdit.removeFromModel ids db
                    |> Result.mapError (fun e ->
                        match e with
                        | RuleNotFound(id, where, None) -> RuleNotFound(id, where, definedElsewhere ctx source db id)
                        | e -> e))
        | ToFile path ->
            if not (File.Exists path) then
                Failed(PathNotFound path)
            else
                runFileEdit ctx "bpa rules remove" (args.Contains RemoveArgs.Save) path [] (fun set ->
                    BpaRuleEdit.removeRules ids set
                    |> Result.mapError (fun id -> RuleNotFound(id, path, None))
                    |> Result.map (fun s -> s, [], ids))

// ---- ignore / unignore ----------------------------------------------------------------------

type IgnoreArgs =
    | [<MainCommand; ExactlyOnce>] Target of model_rule_objects: string list
    | Save
    | Save_To of path: string
    | Force
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "the model, the rule ID (or comma-separated IDs), then the objects (any form `get` accepts). "
                + "Without objects: the model, which turns the rule off for the whole model."
            | Save -> Mutation.Usage.save
            | Save_To _ -> Mutation.Usage.saveTo
            | Force -> Mutation.Usage.force
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let private setIgnored (ignore: bool) (ctx: Ctx) (args: ParseResults<IgnoreArgs>) =
    let command = if ignore then "bpa rules ignore" else "bpa rules unignore"

    let opts =
        Mutation.options
            (args.Contains IgnoreArgs.Save)
            (args.TryGetResult IgnoreArgs.Save_To)
            (args.Contains IgnoreArgs.Force)
            (args.Contains IgnoreArgs.Stat)
            (args.Contains IgnoreArgs.Name_Only)

    match args.GetResult IgnoreArgs.Target, opts with
    | _, Error message -> UsageError message
    | [ _ ], _
    | [], _ -> UsageError "give the model and a rule ID"
    | modelPath :: ruleIds :: objects, Ok opts ->
        let ids = parseList [ ruleIds ]

        match ModelSource.resolve modelPath with
        | Error e -> Failed e
        | Ok source ->
            Mutation.run ctx command opts modelPath (fun db ->
                // TE ignores any ID; an unknown one is most likely a typo, so say so.
                let unknown =
                    match
                        Bpa.collectRules source db {
                            Bpa.RuleOptions.defaults with
                                Environment = environment ctx
                        }
                    with
                    | Ok c -> ids |> List.filter (fun id -> (findRule c.Effective id).IsNone)
                    | Error _ -> []

                let warnings = [
                    for id in unknown -> $"no rule with ID {id} is in effect (built-in, config or model rules)"
                ]

                withWarnings warnings (BpaRuleEdit.setIgnored ignore ids objects) db)

// ---- the subcommand -------------------------------------------------------------------------

type Args =
    | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>
    | [<CliPrefix(CliPrefix.None)>] Show of ParseResults<ShowArgs>
    | [<CliPrefix(CliPrefix.None)>] Add of ParseResults<AddArgs>
    | [<CliPrefix(CliPrefix.None)>] Remove of ParseResults<RemoveArgs>
    | [<CliPrefix(CliPrefix.None)>] Ignore of ParseResults<IgnoreArgs>
    | [<CliPrefix(CliPrefix.None)>] Unignore of ParseResults<IgnoreArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | List _ -> "list the rules in effect, with the layer each comes from and what it overrides."
            | Show _ -> "show one rule in full."
            | Add _ -> "add a rule to the model's own rules or to a rule file (dry run unless --save)."
            | Remove _ -> "remove rules from the model's own rules or from a rule file (dry run unless --save)."
            | Ignore _ ->
                "ignore a rule on objects (BestPracticeAnalyzer_IgnoreRules), or on the model to turn it off "
                + "(dry run unless --save)."
            | Unignore _ -> "stop ignoring a rule on objects or the model (dry run unless --save)."

let run (ctx: Ctx) (args: ParseResults<Args>) =
    match args.TryGetSubCommand() with
    | Some(List a) -> list ctx a
    | Some(Show a) -> show ctx a
    | Some(Add a) -> add ctx a
    | Some(Remove a) -> remove ctx a
    | Some(Ignore a) -> setIgnored true ctx a
    | Some(Unignore a) -> setIgnored false ctx a
    | None -> UsageError(args.Parser.PrintUsage())
