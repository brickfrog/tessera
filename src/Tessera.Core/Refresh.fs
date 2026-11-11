/// Planning a refresh: the TMSL `refresh` command for a model, some tables or some partitions, and
/// what it covers. Pure: nothing here talks to a server. The caller reads the model through its
/// connection, plans, shows the plan, and only then, when asked, runs the script.
///
/// The command's shape is Microsoft's (the TMSL reference, "Refresh command" and "Sequence
/// command"), and matches what the client library's own JsonScripter.ScriptRefresh writes for the
/// same request (the tests compare them).
module Tessera.Refresh

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular

/// What the server does to the objects. Every type TMSL documents is here; anything else is
/// refused when parsing.
[<RequireQualifiedAccess>]
type Kind =
    /// Reload data and recalculate dependents.
    | Full
    /// Reload and recalculate only what isn't already in a ready state.
    | Automatic
    /// Reload data without recalculating (dependents are left cleared).
    | DataOnly
    /// Recalculate what needs it; loads no data.
    | Calculate
    /// Drop the data.
    | ClearValues
    /// Clean up column dictionaries. The model or tables only.
    | Defragment
    /// Append the result of a partition's query to what it holds. Partitions only.
    | Add

module Kind =
    let all = [
        Kind.Full
        Kind.Automatic
        Kind.DataOnly
        Kind.Calculate
        Kind.ClearValues
        Kind.Defragment
        Kind.Add
    ]

    /// The spelling TMSL uses.
    let token =
        function
        | Kind.Full -> "full"
        | Kind.Automatic -> "automatic"
        | Kind.DataOnly -> "dataOnly"
        | Kind.Calculate -> "calculate"
        | Kind.ClearValues -> "clearValues"
        | Kind.Defragment -> "defragment"
        | Kind.Add -> "add"

    /// Any casing of a TMSL type name ("dataOnly", "dataonly", "DataOnly").
    let tryParse (s: string) =
        let s = if isNull s then "" else s.Trim()

        all
        |> List.tryFind (fun k -> String.Equals(token k, s, StringComparison.OrdinalIgnoreCase))

    /// Whether an incremental refresh policy changes what this type does (per the TMSL reference).
    let usesRefreshPolicy =
        function
        | Kind.Full
        | Kind.Automatic
        | Kind.DataOnly -> true
        | Kind.Calculate
        | Kind.ClearValues
        | Kind.Defragment
        | Kind.Add -> false

/// One thing to refresh, by its exact names in the model.
type Target =
    | WholeModel
    | Table of table: string
    | Partition of table: string * partition: string

module Target =
    let ref =
        function
        | WholeModel -> ObjectRef.model
        | Table t -> ObjectRef.table t
        | Partition(t, p) -> ObjectRef.inTable ObjectKind.Partition t p

/// A refresh command, fully decided.
type Request = {
    /// The database's name on the server.
    Database: string
    Kind: Kind
    /// In the order they go in the script. Empty is not allowed (use WholeModel).
    Targets: Target list
    /// Whether tables with an incremental refresh policy have it applied. None leaves it out of
    /// the script: the server then applies the policy (its default). Power BI only.
    ApplyRefreshPolicy: bool option
    /// The date an applied refresh policy counts as "today". Implies applying the policy.
    EffectiveDate: DateOnly option
    /// Wrap the refresh in a `sequence` that limits how many objects are processed at once.
    MaxParallelism: int option
}

/// Selectors that can't mean anything `refresh` takes.
let private unsupported (target: string) (reason: string) = Error(CannotRefresh(target, reason))

let private describe (t: Target) = ObjectRef.format (Target.ref t)

/// Check a request against TMSL's rules, which the server would otherwise report (or, for `add`
/// on a table, might not).
let check (r: Request) : Result<unit, TessError> =
    let policyAsked = r.ApplyRefreshPolicy.IsSome || r.EffectiveDate.IsSome

    let partitions =
        r.Targets
        |> List.filter (function
            | Partition _ -> true
            | _ -> false)

    if r.Targets.IsEmpty then
        unsupported "nothing" "name the model, tables or partitions"
    elif r.MaxParallelism |> Option.exists (fun n -> n < 1) then
        unsupported "with this parallelism" "the maximum parallelism must be 1 or more"
    elif r.EffectiveDate.IsSome && r.ApplyRefreshPolicy = Some false then
        unsupported
            "with an effective date"
            "an effective date only matters when the refresh policy is applied, and this refresh doesn't apply it"
    else
        match r.Kind, partitions with
        | Kind.Defragment, p :: _ ->
            unsupported (describe p) "'defragment' works on the model or whole tables, not on a partition"
        | Kind.Add, _ when partitions.Length <> r.Targets.Length ->
            let other = r.Targets |> List.find (fun t -> not (List.contains t partitions))
            unsupported (describe other) "'add' appends to partitions: name the partitions to append to"
        | _, p :: _ when policyAsked ->
            // The client library refuses to script this too: a refresh policy belongs to a table.
            unsupported
                (describe p)
                "a refresh policy is applied to tables, so the refresh-policy options can't be used when refreshing a partition"
        | _ -> Ok()

let private writeOptions = JsonSerializerOptions(WriteIndented = true)

/// Month/day/year, as in the TMSL reference's example ("12/31/2013"). The client library's
/// JsonScripter writes the current culture's short date here (this, under the invariant culture),
/// so what else a server accepts isn't known; this form never depends on the machine.
let private effectiveDateText (d: DateOnly) =
    d.ToString("MM/dd/yyyy", CultureInfo.InvariantCulture)

/// The command as JSON, without checking it.
let private command (r: Request) : JsonObject =
    let refresh = JsonObject()
    refresh.["type"] <- JsonValue.Create(Kind.token r.Kind)

    match r.ApplyRefreshPolicy, r.EffectiveDate with
    | Some apply, None -> refresh.["applyRefreshPolicy"] <- JsonValue.Create apply
    | _, Some date ->
        refresh.["applyRefreshPolicy"] <- JsonValue.Create true
        refresh.["effectiveDate"] <- JsonValue.Create(effectiveDateText date)
    | None, None -> ()

    let objects = JsonArray()

    for target in r.Targets do
        let o = JsonObject()
        o.["database"] <- JsonValue.Create r.Database

        match target with
        | WholeModel -> ()
        | Table t -> o.["table"] <- JsonValue.Create t
        | Partition(t, p) ->
            o.["table"] <- JsonValue.Create t
            o.["partition"] <- JsonValue.Create p

        objects.Add o

    refresh.["objects"] <- objects
    let root = JsonObject()
    root.["refresh"] <- refresh

    match r.MaxParallelism with
    | None -> root
    | Some n ->
        let sequence = JsonObject()
        sequence.["maxParallelism"] <- JsonValue.Create n
        sequence.["operations"] <- JsonArray(root)
        let outer = JsonObject()
        outer.["sequence"] <- sequence
        outer

/// The TMSL script for a request: a `refresh` command, inside a `sequence` when a maximum
/// parallelism is given. Fails if the request breaks TMSL's rules (see `check`).
let tmsl (r: Request) : Result<string, TessError> =
    check r |> Result.map (fun () -> (command r).ToJsonString writeOptions)

/// What the user asked for, before it is matched against a model.
type Options = {
    Kind: Kind
    ApplyRefreshPolicy: bool option
    EffectiveDate: DateOnly option
    MaxParallelism: int option
}

module Options =
    /// What Tabular Editor's `te refresh` and the server's own tools default to.
    let defaults = {
        Kind = Kind.Automatic
        ApplyRefreshPolicy = None
        EffectiveDate = None
        MaxParallelism = None
    }

/// A table the refresh touches, as the model has it now.
type CoveredTable = {
    Table: string
    /// The partitions named by the request (all of the table's, for a whole table or model).
    Partitions: string list
    /// False when only some of the table's partitions are refreshed.
    WholeTable: bool
    /// The table has an incremental refresh policy.
    HasRefreshPolicy: bool
}

type Plan = {
    Request: Request
    /// The tables and partitions the script names, in model order. The server also recalculates
    /// whatever depends on them (for the types that recalculate).
    Covers: CoveredTable list
    /// Things worth knowing before running it.
    Warnings: string list
    Tmsl: string
}

let private eq (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// Prefer an exact-case match, then any case (as selectors do everywhere else).
let private byName (name: 'T -> string) (wanted: string) (items: 'T list) =
    match items |> List.filter (fun i -> name i = wanted) with
    | [] -> items |> List.filter (fun i -> eq (name i) wanted)
    | exact -> exact

/// One selector to a target. `model`, a table (`Sales`, `'Sales'`, `table:Sales`) or a partition
/// (`partition:Sales/2024`, `Sales[2024]`, or a partition name no table and no other partition has).
let private resolve (db: Database) (input: string) : Result<Target, TessError> =
    let tables = db.Model.Tables |> List.ofSeq

    let partitions =
        tables
        |> List.collect (fun t -> t.Partitions |> Seq.map (fun p -> t, p) |> List.ofSeq)

    let partitionRef (t: Table, p: Microsoft.AnalysisServices.Tabular.Partition) =
        ObjectRef.format (ObjectRef.inTable ObjectKind.Partition t.Name p.Name)

    let table (name: string) =
        match byName (fun (t: Table) -> t.Name) name tables with
        | [ t ] -> Some t
        | _ -> None

    let partitionOf (tableName: string) (name: string) =
        match table tableName with
        | None -> Error(ObjectNotFound input)
        | Some t ->
            match
                byName (fun (p: Microsoft.AnalysisServices.Tabular.Partition) -> p.Name) name (List.ofSeq t.Partitions)
            with
            | [ p ] -> Ok(Partition(t.Name, p.Name))
            | _ -> Error(ObjectNotFound input)

    /// What else the selector would mean, to explain why it can't be refreshed.
    let otherwise () =
        match Inventory.Selector.resolve (Inventory.objects db) input with
        | Ok entry ->
            let kind = ObjectKind.token entry.Ref.Kind

            unsupported
                (ObjectRef.format entry.Ref)
                $"a {kind} isn't refreshed by itself: name the model, tables or partitions"
        | Error(AmbiguousObject _)
        | Error(ObjectNotFound _) -> Error(ObjectNotFound input)
        | Error(ObjectNotFoundSuggest _ as e) -> Error e
        | Error e -> Error e

    Inventory.Selector.parse input
    |> Result.bind (fun selector ->
        match selector with
        | Inventory.Exact r ->
            match r.Kind, r.Path with
            | ObjectKind.Model, _ -> Ok WholeModel
            | ObjectKind.Table, [ name ] ->
                match table name with
                | Some t -> Ok(Table t.Name)
                | None -> Error(ObjectNotFound input)
            | ObjectKind.Partition, [ tableName; name ] -> partitionOf tableName name
            | _ -> otherwise ()
        | Inventory.Qualified(tableName, name) ->
            match partitionOf tableName name with
            | Ok p -> Ok p
            | Error _ -> otherwise ()
        | Inventory.Member _ -> otherwise ()
        | Inventory.Bare name ->
            match table name with
            | Some t -> Ok(Table t.Name)
            | None ->
                match byName (fun (_, p: Microsoft.AnalysisServices.Tabular.Partition) -> p.Name) name partitions with
                | [ (t, p) ] -> Ok(Partition(t.Name, p.Name))
                | [] -> otherwise ()
                | many -> Error(AmbiguousObject(input, many |> List.map partitionRef)))

/// Drop repeats and anything a wider target already includes, keeping the order given.
let private normalize (targets: Target list) : Target list * string list =
    let targets = List.distinct targets

    if List.contains WholeModel targets then
        let dropped = targets |> List.filter (fun t -> t <> WholeModel)

        [ WholeModel ],
        (if dropped.IsEmpty then
             []
         else
             [
                 "the whole model is refreshed, which includes "
                 + (dropped |> List.map describe |> String.concat ", ")
             ])
    else
        let whole =
            targets
            |> List.choose (function
                | Table t -> Some t
                | _ -> None)

        let included =
            targets
            |> List.filter (function
                | Partition(t, _) -> List.contains t whole
                | _ -> false)

        targets |> List.filter (fun t -> not (List.contains t included)),
        included
        |> List.map (fun p ->
            match p with
            | Partition(t, _) -> $"{describe p} is part of {describe (Table t)}, which is refreshed whole"
            | _ -> "")

let private covers (db: Database) (targets: Target list) : CoveredTable list = [
    for t in db.Model.Tables do
        let all = t.Partitions |> Seq.map _.Name |> List.ofSeq

        let whole =
            targets |> List.exists (fun x -> x = WholeModel || x = Target.Table t.Name)

        let named =
            all |> List.filter (fun p -> targets |> List.contains (Partition(t.Name, p)))

        if whole || not named.IsEmpty then
            {
                Table = t.Name
                Partitions = if whole then all else named
                WholeTable = whole || named.Length = all.Length
                HasRefreshPolicy = not (isNull t.RefreshPolicy)
            }
]

/// Plan a refresh of `database` (its name on the server), whose model is `db`. No selectors means
/// the whole model.
let plan (options: Options) (database: string) (db: Database) (selectors: string list) : Result<Plan, TessError> =
    let resolved =
        (Ok [], selectors)
        ||> List.fold (fun acc selector ->
            acc
            |> Result.bind (fun done' -> resolve db selector |> Result.map (fun t -> t :: done')))
        |> Result.map List.rev

    resolved
    |> Result.bind (fun targets ->
        let targets, subsumed =
            normalize (if targets.IsEmpty then [ WholeModel ] else targets)

        let request = {
            Database = database
            Kind = options.Kind
            Targets = targets
            ApplyRefreshPolicy = options.ApplyRefreshPolicy
            EffectiveDate = options.EffectiveDate
            MaxParallelism = options.MaxParallelism
        }

        tmsl request
        |> Result.map (fun script ->
            let covered = covers db targets
            let policyAsked = options.ApplyRefreshPolicy.IsSome || options.EffectiveDate.IsSome
            let applies = options.ApplyRefreshPolicy <> Some false
            // A policy is applied when its table (or the model) is refreshed, not one partition.
            let withPolicy =
                covered
                |> List.filter (fun c ->
                    c.HasRefreshPolicy
                    && targets |> List.exists (fun t -> t = WholeModel || t = Table c.Table))
                |> List.map (fun c -> $"'{c.Table}'")

            let kind = Kind.token options.Kind

            let warnings = [
                yield! subsumed

                if policyAsked && not (Kind.usesRefreshPolicy options.Kind) then
                    $"a refresh policy makes no difference to a '{kind}' refresh: the refresh-policy options have no effect"
                elif policyAsked && withPolicy.IsEmpty then
                    "no table in this refresh has a refresh policy: the refresh-policy options have no effect"
                elif Kind.usesRefreshPolicy options.Kind && applies && not withPolicy.IsEmpty then
                    let tables = String.concat ", " withPolicy

                    $"the incremental refresh policy of {tables} is applied: the server may add, merge and remove "
                    + "partitions, and refreshes only the incremental range"

                if options.Kind = Kind.Add then
                    "'add' appends what each partition's query returns to the rows already loaded: unless the "
                    + "source returns only new rows, rows are duplicated"

                if options.Kind = Kind.ClearValues then
                    "'clearValues' removes the data: the objects are empty until they are refreshed again"
            ]

            {
                Request = request
                Covers = covered
                Warnings = warnings
                Tmsl = script
            }))
