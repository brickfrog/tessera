/// The Best Practice Analyzer: runs Tabular Editor-style rules (Dynamic LINQ predicates) against a
/// model's objects, honouring TE's ignore annotations.
module Tessera.Bpa

open System
open System.Collections.Generic
open System.IO
open System.Linq.Dynamic.Core
open System.Linq.Expressions
open System.Text
open Microsoft.AnalysisServices.Tabular
open Tessera.BpaRules
open Tessera.BpaObjects

type private TomDatabase = Microsoft.AnalysisServices.Tabular.Database

/// Rewrites TE2's expression dialect (an older Dynamic LINQ) into System.Linq.Dynamic.Core's.
module Dialect =

    [<RequireQualifiedAccess>]
    type private Tok =
        | Ident of string
        | Str of string
        | Chr of string
        | Space of string
        | Other of string

    /// Split an expression into identifiers, literals and everything else. TE strings use "" to
    /// escape a quote and treat backslashes literally; '...' is a char when one character long.
    let private lex (s: string) =
        let out = List<Tok>()
        let mutable i = 0

        let quoted (q: char) =
            let sb = StringBuilder()
            i <- i + 1
            let mutable closed = false

            while not closed && i < s.Length do
                if s.[i] = q && i + 1 < s.Length && s.[i + 1] = q then
                    sb.Append q |> ignore
                    i <- i + 2
                elif s.[i] = q then
                    closed <- true
                    i <- i + 1
                else
                    sb.Append s.[i] |> ignore
                    i <- i + 1

            sb.ToString()

        while i < s.Length do
            let c = s.[i]

            if Char.IsWhiteSpace c then
                let start = i

                while i < s.Length && Char.IsWhiteSpace s.[i] do
                    i <- i + 1

                out.Add(Tok.Space(s.Substring(start, i - start)))
            elif c = '"' then
                out.Add(Tok.Str(quoted '"'))
            elif c = '\'' then
                out.Add(Tok.Chr(quoted '\''))
            elif Char.IsLetter c || c = '_' || c = '@' then
                let start = i
                i <- i + 1

                while i < s.Length && (Char.IsLetterOrDigit s.[i] || s.[i] = '_') do
                    i <- i + 1

                out.Add(Tok.Ident(s.Substring(start, i - start)))
            elif Char.IsDigit c then
                let start = i

                while i < s.Length && (Char.IsLetterOrDigit s.[i] || s.[i] = '.') do
                    i <- i + 1

                out.Add(Tok.Other(s.Substring(start, i - start)))
            else
                out.Add(Tok.Other(string c))
                i <- i + 1

        List.ofSeq out

    let private escape (s: string) =
        s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t")

    /// Type names TE resolves case-insensitively (e.g. RegEx); Dynamic.Core needs their exact case.
    let private typeNames (types: Type seq) =
        let d = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for t in types do
            d.TryAdd(t.Name, t.Name) |> ignore

        d

    let private csharpAliases =
        set [
            "string"
            "char"
            "int"
            "long"
            "double"
            "decimal"
            "bool"
            "object"
            "byte"
            "float"
            "short"
        ]

    /// DAX token names that are also operators or literals in the rule language.
    let private notTokenNames =
        set [
            "AND"
            "OR"
            "NOT"
            "TRUE"
            "FALSE"
            "NULL"
            "IT"
            "NEW"
            "IIF"
            "TEXT"
            "IN"
        ]

    /// Translate one TE rule expression.
    let translate (knownTypes: Type seq) (expression: string) =
        let types = typeNames knownTypes
        let tokens = lex expression |> Array.ofList

        let usesTokenize =
            expression.IndexOf("Tokenize", StringComparison.OrdinalIgnoreCase) >= 0

        let significant (step: int) (from: int) =
            let mutable j = from + step

            while j >= 0
                  && j < tokens.Length
                  && (match tokens.[j] with
                      | Tok.Space _ -> true
                      | _ -> false) do
                j <- j + step

            if j >= 0 && j < tokens.Length then
                Some tokens.[j]
            else
                None

        let sb = StringBuilder()

        tokens
        |> Array.iteri (fun i t ->
            match t with
            | Tok.Space s
            | Tok.Other s -> sb.Append s |> ignore
            | Tok.Str s -> sb.Append('"').Append(escape s).Append('"') |> ignore
            | Tok.Chr s when s.Length = 1 ->
                sb.Append('\'').Append(escape(s).Replace("'", "\\'")).Append('\'') |> ignore
            | Tok.Chr s -> sb.Append('"').Append(escape s).Append('"') |> ignore
            | Tok.Ident id ->
                let afterDot = significant -1 i = Some(Tok.Other ".")
                let beforeCall = significant 1 i = Some(Tok.Other "(")

                let rewritten =
                    if afterDot then
                        id
                    elif id.Equals("outerIt", StringComparison.OrdinalIgnoreCase) then
                        "parent"
                    elif id.Equals("current", StringComparison.OrdinalIgnoreCase) then
                        "root"
                    elif usesTokenize && not beforeCall && not (notTokenNames.Contains id) then
                        match BpaDaxToken.tryCode id with
                        | Some code -> string code
                        | None -> id
                    else
                        match types.TryGetValue id with
                        | true, canonical when canonical <> id && not (csharpAliases.Contains id) -> canonical
                        | _ -> id

                sb.Append rewritten |> ignore)

        sb.ToString()

/// Makes member access null-safe, as TE does: a member of null is the member type's default,
/// and a null string behaves as "".
type private NullSafe() =
    inherit ExpressionVisitor()

    static member Guard(instance: Expression, access: Expression -> Expression) : Expression =
        if instance.Type.IsValueType then
            access instance
        elif instance.Type = typeof<string> then
            access (Expression.Coalesce(instance, Expression.Constant("")))
        else
            let v = Expression.Variable(instance.Type)
            let body = access v

            Expression.Block(
                body.Type,
                [ v ],
                [
                    Expression.Assign(v, instance) :> Expression
                    Expression.Condition(
                        Expression.Equal(v, Expression.Constant(null, instance.Type)),
                        Expression.Default(body.Type),
                        body
                    )
                ]
            )

    override this.VisitMember(node: MemberExpression) =
        match node.Expression with
        | null -> node :> Expression
        | instance ->
            let visited = this.Visit instance

            NullSafe.Guard(visited, fun i -> Expression.MakeMemberAccess(i, node.Member))

    override this.VisitMethodCall(node: MethodCallExpression) =
        let args = node.Arguments |> Seq.map this.Visit |> Array.ofSeq

        match node.Object with
        | null -> Expression.Call(node.Method, args) :> Expression
        | instance -> NullSafe.Guard(this.Visit instance, fun i -> Expression.Call(i, node.Method, args))

/// The wrapper type each scope's expression is compiled against, and the objects it runs over.
let private scopeType (s: Scope) =
    match s with
    | Scope.Model -> typeof<BpaObjects.Model>
    | Scope.Table -> typeof<BpaObjects.Table>
    | Scope.Measure -> typeof<BpaObjects.Measure>
    | Scope.Hierarchy -> typeof<BpaObjects.Hierarchy>
    | Scope.Level -> typeof<BpaObjects.Level>
    | Scope.Relationship -> typeof<BpaObjects.SingleColumnRelationship>
    | Scope.Perspective -> typeof<BpaObjects.Perspective>
    | Scope.Culture -> typeof<BpaObjects.Culture>
    | Scope.Partition -> typeof<BpaObjects.Partition>
    | Scope.ProviderDataSource -> typeof<BpaObjects.ProviderDataSource>
    | Scope.DataColumn -> typeof<BpaObjects.DataColumn>
    | Scope.CalculatedColumn -> typeof<BpaObjects.CalculatedColumn>
    | Scope.CalculatedTable -> typeof<BpaObjects.CalculatedTable>
    | Scope.CalculatedTableColumn -> typeof<BpaObjects.CalculatedTableColumn>
    | Scope.KPI -> typeof<BpaObjects.KPI>
    | Scope.StructuredDataSource -> typeof<BpaObjects.StructuredDataSource>
    | Scope.Variation -> typeof<BpaObjects.Variation>
    | Scope.NamedExpression -> typeof<BpaObjects.NamedExpression>
    | Scope.ModelRole -> typeof<BpaObjects.ModelRole>
    | Scope.TablePermission -> typeof<BpaObjects.TablePermission>
    | Scope.CalculationGroup -> typeof<BpaObjects.CalculationGroupTable>
    | Scope.CalculationItem -> typeof<BpaObjects.CalculationItem>
    | Scope.ModelRoleMember -> typeof<BpaObjects.ModelRoleMember>
    | Scope.UserDefinedFunction -> typeof<BpaObjects.Function>
    | Scope.Calendar -> typeof<BpaObjects.Calendar>

let private objectsIn (m: BpaObjects.Model) (s: Scope) : TabularObject list =
    let up (xs: #TabularObject seq) =
        xs |> Seq.map (fun x -> x :> TabularObject) |> List.ofSeq

    match s with
    | Scope.Model -> [ m ]
    | Scope.Table -> m.Tables |> Seq.filter (fun t -> t.GetType() = typeof<BpaObjects.Table>) |> up
    | Scope.CalculatedTable -> up (ofType<CalculatedTable> m.Tables)
    | Scope.CalculationGroup -> up m.CalculationGroups
    | Scope.Measure -> up m.AllMeasures
    | Scope.Hierarchy -> up m.AllHierarchies
    | Scope.Level -> up m.AllLevels
    | Scope.Partition -> up m.AllPartitions
    | Scope.DataColumn -> up (ofType<DataColumn> m.AllColumns)
    | Scope.CalculatedColumn -> up (ofType<CalculatedColumn> m.AllColumns)
    | Scope.CalculatedTableColumn -> up (ofType<CalculatedTableColumn> m.AllColumns)
    | Scope.KPI -> m.AllMeasures |> Seq.choose (fun x -> Option.ofObj x.KPI) |> up
    | Scope.Relationship -> up m.Relationships
    | Scope.Perspective -> up m.Perspectives
    | Scope.Culture -> up m.Cultures
    | Scope.ProviderDataSource -> up (ofType<ProviderDataSource> m.DataSources)
    | Scope.StructuredDataSource -> up (ofType<StructuredDataSource> m.DataSources)
    | Scope.NamedExpression -> up m.Expressions
    | Scope.ModelRole -> up m.Roles
    | Scope.TablePermission -> m.Roles |> Seq.collect (fun r -> r.TablePermissions) |> up
    | Scope.ModelRoleMember -> m.Roles |> Seq.collect (fun r -> r.Members) |> up
    | Scope.CalculationItem -> up m.AllCalculationItems
    | Scope.UserDefinedFunction -> up m.Functions
    | Scope.Calendar -> up m.AllCalendars
    | Scope.Variation -> m.AllColumns |> Seq.collect (fun c -> c.Variations) |> up

/// Every type a rule expression may name or call methods on: the wrappers, the enums their
/// properties use, and a few framework types TE rules use.
let private knownTypes =
    lazy
        (let wrappers =
            typeof<TabularObject>.DeclaringType.GetNestedTypes()
            |> Array.filter (fun t -> t.IsPublic || t.IsNestedPublic)
            |> Array.filter (fun t -> not t.IsGenericTypeDefinition)
            |> List.ofArray

         let enums =
             wrappers
             |> List.collect (fun t -> t.GetProperties() |> Array.map (fun p -> p.PropertyType) |> List.ofArray)
             |> List.filter (fun t -> t.IsEnum)

         let framework = [
             typeof<Text.RegularExpressions.Regex>
             typeof<Text.RegularExpressions.RegexOptions>
             typeof<StringComparison>
             typeof<BpaDaxToken.DaxToken>
         ]

         wrappers @ enums @ framework |> List.distinct)

type private TypeProvider(config: ParsingConfig) =
    inherit
        CustomTypeProviders.DefaultDynamicLinqCustomTypeProvider(
            config,
            List<Type>(knownTypes.Value) :> IList<Type>,
            true
        )

let private parsingConfig =
    lazy
        (let config =
            ParsingConfig(IsCaseSensitive = false, AllowEqualsAndToStringMethodsOnObject = true)

         config.CustomTypeProvider <- TypeProvider config
         config)

/// A compiled rule predicate for one scope.
type private Predicate = Func<TabularObject, bool>

/// Compile a TE rule expression against a wrapper type.
let private compileFor (itType: Type) (expression: string) : Result<Predicate, string> =
    try
        let translated = Dialect.translate knownTypes.Value expression

        let lambda =
            DynamicExpressionParser.ParseLambda(parsingConfig.Value, itType, typeof<bool>, translated)

        let safe = NullSafe().Visit(lambda) :?> LambdaExpression
        let p = Expression.Parameter(typeof<TabularObject>, "o")

        let wrapped =
            Expression.Lambda<Predicate>(Expression.Invoke(safe, Expression.Convert(p, itType)), p)

        Ok(wrapped.Compile())
    with e ->
        Error e.Message

/// Check that an expression compiles for every scope of a rule (no model needed).
let compile (rule: Rule) : Result<unit, string> =
    match rule.UnknownScopes with
    | s :: _ -> Error $"unknown scope '{s}'"
    | [] ->
        rule.Scopes
        |> List.tryPick (fun s ->
            match compileFor (scopeType s) rule.Expression with
            | Error e -> Some(Error $"{Scope.name s}: {e}")
            | Ok _ -> None)
        |> Option.defaultValue (Ok())

/// An object that violates a rule.
type Finding = {
    Rule: Rule
    /// The object, or the nearest addressable one (a KPI's measure, a table permission's role).
    Ref: ObjectRef
    /// TE's name for the object, e.g. 'Sales'[Amount], or 'Sales'[Total].KPI.
    ObjectName: string
    /// TE's type name, e.g. "Column", "Calculated Table", "Table (Import)".
    ObjectType: string
}

type RuleStatus =
    /// The rule ran on every object in its scopes.
    | Evaluated
    /// The expression (or a scope) isn't supported; the rule did not run.
    | Unsupported of reason: string
    /// The expression compiled but failed while running.
    | Failed of reason: string
    /// The model's compatibility level is below the rule's.
    | Skipped of requiredLevel: int
    /// The model's ignore annotation turns the rule off.
    | Disabled

type RuleResult = {
    Rule: Rule
    Status: RuleStatus
    Findings: Finding list
    /// Findings suppressed by the object's ignore annotation.
    Ignored: Finding list
}

let private ignoredIds (o: TabularObject) =
    ignoreAnnotations
    |> List.tryPick (fun a -> Option.ofObj (o.GetAnnotation a))
    |> Option.map parseIgnoreList
    |> Option.defaultValue []
    |> Set.ofList
    |> Set.map (fun s -> s.ToUpperInvariant())

let private describe (ctx: Context) (o: TabularObject) : ObjectRef * string =
    let refOf (tom: MetadataObject) =
        ctx.Entry tom
        |> Option.map (fun e -> e.Ref)
        |> Option.defaultValue ObjectRef.model

    match o with
    | :? KPI as k -> refOf k.Measure.Tom, k.Measure.DaxObjectFullName + ".KPI"
    | :? TablePermission as tp -> refOf tp.Role.Tom, $"{tp.Role.Name}[{tp.Table.Name}]"
    | :? ModelRoleMember as m -> refOf m.Role.Tom, $"{m.Role.Name}/{m.Name}"
    | :? Variation as v -> refOf v.Column.Tom, $"{v.Column.DaxObjectFullName}/{v.Name}"
    | :? BpaObjects.Table as t -> refOf o.Tom, t.DaxObjectFullName
    | :? BpaObjects.Column as c -> refOf o.Tom, c.DaxObjectFullName
    | :? BpaObjects.Measure as m -> refOf o.Tom, m.DaxObjectFullName
    | :? BpaObjects.Hierarchy as h -> refOf o.Tom, h.DaxObjectFullName
    | _ -> refOf o.Tom, o.Name

/// Run rules against a model.
let analyze (db: TomDatabase) (rules: Rule list) : RuleResult list =
    let ctx = Context db
    let model = ctx.Model
    let modelIgnores = ignoredIds model

    let result rule status = {
        Rule = rule
        Status = status
        Findings = []
        Ignored = []
    }

    [
        for rule in rules do
            if modelIgnores.Contains(rule.Id.ToUpperInvariant()) then
                result rule Disabled
            elif rule.CompatibilityLevel > db.CompatibilityLevel then
                result rule (Skipped rule.CompatibilityLevel)
            else
                match rule.UnknownScopes with
                | s :: _ -> result rule (Unsupported $"unknown scope '{s}'")
                | [] ->
                    let compiled =
                        rule.Scopes |> List.map (fun s -> s, compileFor (scopeType s) rule.Expression)

                    match compiled |> List.tryFind (snd >> Result.isError) with
                    | Some(s, Error e) -> result rule (Unsupported $"{Scope.name s}: {e}")
                    | _ ->
                        let mutable failure = None

                        let hits =
                            compiled
                            |> List.collect (fun (s, p) ->
                                match failure, p with
                                | None, Ok predicate ->
                                    try
                                        objectsIn model s |> List.filter predicate.Invoke
                                    with e ->
                                        let inner =
                                            match e with
                                            | :? Reflection.TargetInvocationException as t when
                                                not (isNull t.InnerException)
                                                ->
                                                t.InnerException
                                            | e -> e

                                        failure <- Some $"{Scope.name s}: {inner.Message}"
                                        []
                                | _ -> [])
                            |> distinctByReference

                        match failure with
                        | Some reason -> result rule (Failed reason)
                        | None ->
                            let id = rule.Id.ToUpperInvariant()

                            let findings =
                                hits
                                |> List.map (fun o ->
                                    let r, name = describe ctx o

                                    (ignoredIds o).Contains id,
                                    {
                                        Rule = rule
                                        Ref = r
                                        ObjectName = name
                                        ObjectType = o.ObjectTypeName
                                    })

                            {
                                Rule = rule
                                Status = Evaluated
                                Findings = findings |> List.filter (fst >> not) |> List.map snd
                                Ignored = findings |> List.filter fst |> List.map snd
                            }
    ]

/// Fetches an external rule file's text. Injected, so nothing touches the network unless the
/// caller passes a real fetcher (and the user passed --allow-external-rules).
type Fetcher = Uri -> Result<string, string>

/// Where configuration comes from: environment variables (TESS_BPA_RULES, XDG_CONFIG_HOME, HOME,
/// APPDATA) are read only through this, so tests can run without the real environment.
type RuleEnvironment = {
    GetEnv: string -> string option
    IsWindows: bool
}

module RuleEnvironment =
    /// No environment variables at all: no TESS_BPA_RULES, no user config file.
    let empty = {
        GetEnv = fun _ -> None
        IsWindows = OperatingSystem.IsWindows()
    }

    let ofProcess = {
        GetEnv =
            fun name ->
                match Environment.GetEnvironmentVariable name with
                | null
                | "" -> None
                | v -> Some v
        IsWindows = OperatingSystem.IsWindows()
    }

/// Which rules to run.
type RuleOptions = {
    /// Include tessera's built-in rules.
    BuiltIn: bool
    /// Rule files given by the user (--rules), in order (later files override earlier ones by ID).
    Files: string list
    /// Include the rules the model carries: its BestPracticeAnalyzer annotation and the files its
    /// BestPracticeAnalyzer_ExternalRuleFiles annotation lists.
    ModelRules: bool
    /// Include TESS_BPA_RULES and the config files.
    Config: bool
    /// Fetch https:// URLs in BestPracticeAnalyzer_ExternalRuleFiles (with `Fetch`).
    AllowExternal: bool
    Fetch: Fetcher
    Environment: RuleEnvironment
}

module RuleOptions =
    /// Built-in rules, the model's rules and config; nothing fetched; no environment.
    let defaults = {
        BuiltIn = true
        Files = []
        ModelRules = true
        Config = true
        AllowExternal = false
        Fetch = fun _ -> Error "fetching is disabled"
        Environment = RuleEnvironment.empty
    }

    /// Only the built-in rules.
    let builtInOnly = {
        defaults with
            ModelRules = false
            Config = false
    }

/// The rules found, and what happened while finding them.
type CollectedRules = {
    /// Every rule definition from every layer, lowest precedence first (before merging by ID).
    All: Rule list
    /// The rules that run: `All` merged by ID, the highest-precedence definition winning.
    Effective: Rule list
    /// Rules the model refers to that couldn't be read.
    Warnings: string list
    /// Things deliberately not done, e.g. URLs not fetched.
    Notes: string list
    /// URLs fetched (only with AllowExternal).
    Fetched: string list
}

module CollectedRules =
    /// Lower-layer definitions the effective rule with this ID replaced, lowest first.
    let overridden (c: CollectedRules) (rule: Rule) =
        let defs =
            c.All
            |> List.filter (fun r -> String.Equals(r.Id, rule.Id, StringComparison.OrdinalIgnoreCase))

        defs |> List.take (max 0 (defs.Length - 1))

/// Folders a model's relative external rule file paths are resolved against.
let private baseFolders (source: ModelSource) = [
    match source.Format with
    | Bim -> Path.GetDirectoryName(Path.GetFullPath source.Path)
    | Tmdl -> Path.GetFullPath source.Path
    | DatabaseJson -> Path.GetFullPath source.Path
    | Xmla -> ()
    yield! Option.toList source.SemanticModelFolder
]

let private annotation (m: Microsoft.AnalysisServices.Tabular.Model) (names: string list) =
    names
    |> List.tryPick (fun n ->
        match m.Annotations.Find n with
        | null -> None
        | a -> Some a.Value)

/// Config files and the TESS_BPA_RULES variable.
module RuleConfig =
    /// The environment variable listing rule files (separated like PATH).
    let envVar = "TESS_BPA_RULES"

    /// A project's config file, relative to a folder: .tessera/bpa.json.
    let projectFile = Path.Combine(".tessera", "bpa.json")

    /// The user's config file: %APPDATA%\tessera\bpa.json on Windows; elsewhere
    /// $XDG_CONFIG_HOME/tessera/bpa.json, or ~/.config/tessera/bpa.json without it.
    let userFile (env: RuleEnvironment) : string option =
        let rooted = Option.filter (fun (d: string) -> Path.IsPathRooted d)

        if env.IsWindows then
            env.GetEnv "APPDATA"
            |> rooted
            |> Option.map (fun d -> Path.Combine(d, "tessera", "bpa.json"))
        else
            match env.GetEnv "XDG_CONFIG_HOME" |> rooted with
            | Some d -> Some(Path.Combine(d, "tessera", "bpa.json"))
            | None ->
                env.GetEnv "HOME"
                |> rooted
                |> Option.map (fun h -> Path.Combine(h, ".config", "tessera", "bpa.json"))

    /// The model's own folders: the .bim file's folder or the TMDL folder; in a PBIP project also
    /// the X.SemanticModel folder and the project folder holding it.
    let private modelFolders (source: ModelSource) = [
        yield! baseFolders source

        match source.SemanticModelFolder with
        | Some f ->
            match Path.GetDirectoryName(Path.GetFullPath f) with
            | null -> ()
            | parent -> parent
        | None -> ()
    ]

    let private isRepoRoot (dir: string) =
        let git = Path.Combine(dir, ".git")
        Directory.Exists git || File.Exists git

    /// Folders searched for .tessera/bpa.json, nearest first: from the model's folder up to the
    /// root of the git repository it is in (the first folder holding .git); outside a repository,
    /// only the model's own folders.
    let searchFolders (source: ModelSource) : string list =
        let rec up (dir: string) acc =
            let acc = dir :: acc

            if isRepoRoot dir then
                Some(List.rev acc)
            else
                match Path.GetDirectoryName dir with
                | null -> None
                | parent when parent = dir -> None
                | parent -> up parent acc

        match baseFolders source with
        // A remote model has no folder, so no project config.
        | [] -> []
        | start :: _ ->
            match up start [] with
            | Some dirs -> dirs
            | None -> modelFolders source |> List.distinct

    /// The nearest project config file, if any.
    let projectFileFor (source: ModelSource) : string option =
        searchFolders source
        |> List.map (fun d -> Path.Combine(d, projectFile))
        |> List.tryFind File.Exists

    /// Paths in TESS_BPA_RULES, relative ones against the current directory.
    let envFiles (env: RuleEnvironment) : string list =
        match env.GetEnv envVar with
        | None -> []
        | Some value ->
            value.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> List.ofArray
            |> List.map Path.GetFullPath

/// An external rule file entry that is a URL (http or https), rather than a path.
let private asUrl (entry: string) =
    match Uri.TryCreate(entry, UriKind.Absolute) with
    | true, u when u.Scheme = Uri.UriSchemeHttp || u.Scheme = Uri.UriSchemeHttps -> Some u
    | _ -> None

/// Why a URL won't be fetched even with --allow-external-rules, if it won't.
let refuseUrl (u: Uri) : string option =
    if u.Scheme <> Uri.UriSchemeHttps then
        Some "only https:// URLs are fetched"
    elif not (String.IsNullOrEmpty u.UserInfo) then
        Some "URLs with credentials are not fetched"
    else
        None

/// The rules to run, by layer, lowest precedence first (a rule replaces a lower layer's rule
/// with the same ID):
///   1. built-in rules
///   2. the model's rules: the files (and, with AllowExternal, URLs) its
///      BestPracticeAnalyzer_ExternalRuleFiles annotation lists - the first listed wins, as in TE -
///      then the rules embedded in its BestPracticeAnalyzer annotation
///   3. config files: the user's, then the project's .tessera/bpa.json
///   4. TESS_BPA_RULES files, in order
///   5. --rules files, in order
/// Files the user names (layers 3-5) must be valid; problems with the model's own references are
/// warnings, so a model can always be analyzed.
let collectRules (source: ModelSource) (db: TomDatabase) (opts: RuleOptions) : Result<CollectedRules, TessError> =
    let warnings = List<string>()
    let notes = List<string>()
    let fetched = List<string>()

    let loadAll (as': string -> RuleSource) (paths: string list) =
        paths
        |> List.fold
            (fun acc path ->
                match acc, loadFileAs as' path with
                | Error e, _ -> Error e
                | Ok sets, Ok rules -> Ok(sets @ rules)
                | Ok _, Error e -> Error e)
            (Ok [])

    let builtIn = if opts.BuiltIn then BpaRules.builtIn.Value else []

    let external () =
        match annotation db.Model [ externalRulesAnnotation ] with
        | None -> []
        | Some json ->
            match parseExternalRuleFiles json with
            | Error e ->
                warnings.Add $"{externalRulesAnnotation} annotation: {e}"
                []
            | Ok entries ->
                let one (entry: string) : Rule list =
                    match asUrl entry with
                    | Some _ when not opts.AllowExternal ->
                        notes.Add $"external rule file {entry} not fetched (pass --allow-external-rules to fetch it)"
                        []
                    | Some url ->
                        match refuseUrl url with
                        | Some reason ->
                            warnings.Add $"external rule file {entry} not fetched: {reason}"
                            []
                        | None ->
                            let text = opts.Fetch url
                            fetched.Add entry

                            match text |> Result.bind (parse (ExternalRuleUrl entry)) with
                            | Ok rules -> rules
                            | Error e ->
                                warnings.Add $"external rule file {entry}: {e}"
                                []
                    | None ->
                        let candidates =
                            if Path.IsPathRooted entry then
                                [ entry ]
                            else
                                baseFolders source |> List.map (fun b -> Path.Combine(b, entry))

                        match candidates |> List.tryFind File.Exists with
                        | None ->
                            warnings.Add $"external rule file {entry} not found"
                            []
                        | Some file ->
                            match loadFileAs ExternalRuleFile file with
                            | Ok rules -> rules
                            | Error e ->
                                warnings.Add(TessError.message e)
                                []

                // Load in the listed order (so fetches happen in that order), but give the first
                // listed the highest precedence, as TE does.
                entries |> List.map one |> List.rev |> List.concat

    let embedded () =
        match annotation db.Model modelRulesAnnotations with
        | None -> []
        | Some json ->
            match parse ModelAnnotation json with
            | Ok rules -> rules
            | Error e ->
                warnings.Add $"rules in the model's BestPracticeAnalyzer annotation: {e}"
                []

    let modelRules = if opts.ModelRules then external () @ embedded () else []

    let configPaths =
        if opts.Config then
            [
                yield!
                    RuleConfig.userFile opts.Environment
                    |> Option.filter File.Exists
                    |> Option.toList
                yield! RuleConfig.projectFileFor source |> Option.toList
            ]
            |> List.distinct
        else
            []

    let envPaths =
        if opts.Config then
            RuleConfig.envFiles opts.Environment
        else
            []

    loadAll ConfigFile configPaths
    |> Result.bind (fun config ->
        loadAll EnvRuleFile envPaths
        |> Result.bind (fun env ->
            loadAll RuleFile opts.Files
            |> Result.map (fun cli ->
                let all = builtIn @ modelRules @ config @ env @ cli

                {
                    All = all
                    Effective = merge [ all ]
                    Warnings = List.ofSeq warnings
                    Notes = List.ofSeq notes
                    Fetched = List.ofSeq fetched
                })))
