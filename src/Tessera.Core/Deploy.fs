/// Planning a deployment: what a server would hold after deploying a local model over what it has
/// now, and the TMSL script that gets it there. Pure: nothing here talks to a server. The caller
/// reads the deployed database (if any) through its connection, plans, shows the plan, and only
/// then, when asked, runs the script.
///
/// The merge rules are Tabular Editor 2's (TabularDeployer.cs): a deployment replaces the model's
/// metadata but, unless asked, keeps what belongs to the server's side of the fence: partitions
/// (so refresh state and incremental-refresh partitions survive), data sources, shared
/// expressions, roles and role members.
module Tessera.Deploy

open System
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular

type Options = {
    /// Replace the server's data sources with the model's.
    Connections: bool
    /// Replace the server's partitions (and refresh policies) with the model's.
    Partitions: bool
    /// With Partitions: still keep the partitions of server tables that have a refresh policy.
    SkipRefreshPolicy: bool
    /// Replace the server's shared (M) expressions with the model's.
    SharedExpressions: bool
    /// Replace the server's roles with the model's.
    Roles: bool
    /// With Roles: also replace role members (otherwise each role keeps the members it has on the
    /// server, and a role new to the server gets none).
    RoleMembers: bool
}

module Options =
    /// Metadata only: partitions, data sources, shared expressions, roles and members stay as
    /// they are on the server.
    let defaults = {
        Connections = false
        Partitions = false
        SkipRefreshPolicy = false
        SharedExpressions = false
        Roles = false
        RoleMembers = false
    }

    /// Everything from the model.
    let full = {
        Connections = true
        Partitions = true
        SkipRefreshPolicy = false
        SharedExpressions = true
        Roles = true
        RoleMembers = true
    }

/// What is known about where the model is going.
type Target = {
    /// The database name to deploy as.
    Database: string
    /// The database as it is on the server now; None if it doesn't exist.
    Deployed: Database option
    /// The server's compatibility mode, if known.
    CompatibilityMode: Microsoft.AnalysisServices.CompatibilityMode option
    /// The compatibility levels the server accepts; empty if unknown.
    SupportedCompatibilityLevels: int list
}

type Plan = {
    Database: string
    /// True when the database doesn't exist yet.
    Creates: bool
    /// The script to run (a TMSL createOrReplace).
    Tmsl: string
    /// The model the server would hold afterwards.
    Merged: Database
    /// From what is deployed now (nothing, when creating) to Merged.
    Changes: Diff.Change list
    /// Errors Merged has that the deployed model doesn't.
    IntroducedErrors: Validate.Issue list
    Warnings: string list
    /// What was kept from the server rather than taken from the model, in words.
    Kept: string list
}

let private eq (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let private nameOf (node: JsonNode) =
    match node with
    | :? JsonObject as o ->
        match o.["name"] with
        | null -> ""
        | n -> n.GetValue<string>()
    | _ -> ""

let private array (parent: JsonObject) (key: string) : JsonArray option =
    match parent.[key] with
    | :? JsonArray as a -> Some a
    | _ -> None

let private serialize (o: MetadataObject) : JsonNode =
    JsonNode.Parse(JsonSerializer.SerializeObject o)

let private sourceType (t: Table) =
    t.Partitions
    |> Seq.tryHead
    |> Option.map _.SourceType
    |> Option.defaultValue PartitionSourceType.None

/// A table whose data comes from a source query (TE2's IsQueryTable): only those keep the
/// server's partitions. Calculated tables and calculation groups always take the model's.
let private isQueryTable (t: Table) =
    match sourceType t with
    | PartitionSourceType.M
    | PartitionSourceType.Query
    | PartitionSourceType.PolicyRange -> true
    | PartitionSourceType.None -> not (isNull t.RefreshPolicy)
    | _ -> false

let private hasPolicySource (t: Table) =
    match t.RefreshPolicy with
    | :? BasicRefreshPolicy as p -> not (String.IsNullOrEmpty p.SourceExpression)
    | _ -> false

let private plural (n: int) (one: string) (many: string) =
    if n = 1 then $"1 {one}" else $"{n} {many}"

/// Roles: all from the server, or the model's with the server's members.
let private mergeRoles (opts: Options) (model: JsonObject) (deployed: Model) (kept: ResizeArray<string>) =
    if not opts.Roles then
        model.["roles"] <- JsonArray(deployed.Roles |> Seq.map serialize |> Array.ofSeq)
        kept.Add $"""roles: the server's ({plural deployed.Roles.Count "role" "roles"})"""
    elif not opts.RoleMembers then
        match array model "roles" with
        | Some roles ->
            for role in roles do
                let members =
                    match deployed.Roles |> Seq.tryFind (fun r -> eq r.Name (nameOf role)) with
                    | Some r -> r.Members |> Seq.map serialize |> Array.ofSeq
                    | None -> [||]

                (role :?> JsonObject).["members"] <- JsonArray members

            if roles.Count > 0 then
                kept.Add "role members: the server's (a role new to the server gets none)"
        | None -> ()

/// Data sources: the server's replace the model's of the same name, and are added otherwise.
let private mergeDataSources (model: JsonObject) (deployed: Model) (kept: ResizeArray<string>) =
    if deployed.DataSources.Count > 0 then
        // (TE2 does this only when the model has data sources of its own; without them it would
        // drop the server's, leaving the partitions kept from the server pointing at nothing.)
        let sources =
            match array model "dataSources" with
            | Some sources -> sources
            | None ->
                let sources = JsonArray()
                model.["dataSources"] <- sources
                sources

        for d in deployed.DataSources do
            sources
            |> Seq.tryFind (fun s -> eq (nameOf s) d.Name)
            |> Option.iter (fun s -> sources.Remove s |> ignore)

            sources.Add(serialize d)

        kept.Add $"""data sources: the server's ({plural deployed.DataSources.Count "data source" "data sources"})"""

let private mergePartitions
    (opts: Options)
    (model: JsonObject)
    (local: Model)
    (deployed: Model)
    (kept: ResizeArray<string>)
    =
    let mutable n = 0

    match array model "tables" with
    | Some tables ->
        for table in tables do
            let name = nameOf table

            match
                local.Tables |> Seq.tryFind (fun t -> eq t.Name name),
                deployed.Tables |> Seq.tryFind (fun t -> eq t.Name name)
            with
            | Some mine, Some theirs when isQueryTable mine && isQueryTable theirs ->
                if not opts.Partitions || (opts.SkipRefreshPolicy && hasPolicySource theirs) then
                    let table = table :?> JsonObject
                    table.["partitions"] <- JsonArray(theirs.Partitions |> Seq.map serialize |> Array.ofSeq)

                    if not (isNull theirs.RefreshPolicy) then
                        table.["refreshPolicy"] <- serialize theirs.RefreshPolicy

                    n <- n + 1
            | _ -> ()
    | None -> ()

    if n > 0 then
        kept.Add $"""partitions and refresh policies: the server's, on {plural n "table" "tables"} it already has"""

/// A table with a refresh policy but no partitions can't be created: give it one from the
/// policy's source expression (the server replaces it when the policy is applied).
let private addPolicyPartitions (model: JsonObject) =
    match array model "tables" with
    | Some tables ->
        for table in tables do
            let table = table :?> JsonObject

            let empty =
                match array table "partitions" with
                | Some p -> p.Count = 0
                | None -> true

            match table.["refreshPolicy"] with
            | :? JsonObject as policy when empty ->
                let expression =
                    match policy.["sourceExpression"] with
                    | :? JsonArray as lines -> Some(String.Join("\n", lines |> Seq.map (fun l -> l.GetValue<string>())))
                    | :? JsonValue as v -> Some(v.GetValue<string>())
                    | _ -> None

                match expression with
                | Some e ->
                    let source = JsonObject()
                    source.["type"] <- JsonValue.Create "m"
                    source.["expression"] <- JsonValue.Create e
                    let partition = JsonObject()
                    partition.["name"] <- JsonValue.Create(nameOf table)
                    partition.["mode"] <- JsonValue.Create "import"
                    partition.["source"] <- source
                    table.["partitions"] <- JsonArray(partition)
                | None -> ()
            | _ -> ()
    | None -> ()

/// From compatibility level 1470 the server wants a calculation group's Name column first and its
/// Ordinal column second.
let private fixCalculationGroupColumns (level: int) (model: JsonObject) =
    if level >= 1470 then
        match array model "tables" with
        | Some tables ->
            for table in tables do
                let table = table :?> JsonObject

                match table.["calculationGroup"], array table "columns" with
                | cg, Some columns when not (isNull cg) && columns.Count >= 2 ->
                    let moveFirst (sourceColumn: string) =
                        columns
                        |> Seq.tryFind (fun c ->
                            match c with
                            | :? JsonObject as o ->
                                match o.["sourceColumn"] with
                                | :? JsonValue as v ->
                                    let ok, s = v.TryGetValue<string>()
                                    ok && eq s sourceColumn
                                | _ -> false
                            | _ -> false)
                        |> Option.iter (fun c ->
                            columns.Remove c |> ignore
                            columns.Insert(0, c))

                    moveFirst "ordinal"
                    moveFirst "name"
                | _ -> ()
        | None -> ()

let private issueKey (i: Validate.Issue) =
    ObjectRef.format i.Ref, i.Rule, i.Message

let private errorsOf (db: Database) =
    Validate.validate db
    |> List.filter (fun i -> i.Severity = Validate.Severity.Error)

let private emptyLike (db: Database) =
    let empty = new Database()
    empty.CompatibilityLevel <- db.CompatibilityLevel
    empty.Model <- new Model()
    empty

let private writeOptions = JsonSerializerOptions(WriteIndented = true)

/// Changes between calculated and loaded that a single createOrReplace can't make on a server
/// holding data (TE2 deploys those in two steps; tess doesn't yet).
let private kindChanges (local: Model) (deployed: Model) = [
    let calculated (t: Table) =
        sourceType t = PartitionSourceType.Calculated

    for mine in local.Tables do
        match deployed.Tables |> Seq.tryFind (fun t -> eq t.Name mine.Name) with
        | Some theirs ->
            if calculated mine <> calculated theirs then
                $"table '{mine.Name}' changes between calculated and loaded"
            else
                for c in mine.Columns do
                    match theirs.Columns |> Seq.tryFind (fun o -> eq o.Name c.Name) with
                    | Some o when
                        o.Type <> c.Type
                        && c.Type <> ColumnType.RowNumber
                        && c.Type <> ColumnType.CalculatedTableColumn
                        ->
                        $"column '{mine.Name}'[{c.Name}] changes between calculated and data"
                    | _ -> ()
        | None -> ()
]

/// Plan deploying `local` to `target`. `diff` says which properties the reported changes leave out.
/// Fails when the model can't go to this server at all.
let plan (opts: Options) (diff: Diff.Options) (target: Target) (local: Database) : Result<Plan, TessError> =
    let fail (reason: string) =
        Error(DeployFailed("the server", target.Database, [ reason ]))

    let level = local.CompatibilityLevel
    let supported = target.SupportedCompatibilityLevels

    if String.IsNullOrWhiteSpace target.Database then
        fail "no database name"
    elif isNull local.Model then
        fail "the local model is empty"
    elif not supported.IsEmpty && not (List.contains level supported) then
        let levels = supported |> List.map string |> String.concat ", "
        fail $"the server doesn't support compatibility level {level} (it supports {levels})"
    else
        let source = ModelIO.clone local
        source.Name <- target.Database
        source.ID <- target.Database

        let root = JsonNode.Parse(JsonScripter.ScriptCreateOrReplace(source, false))
        let command = root.["createOrReplace"] :?> JsonObject
        let database = command.["database"] :?> JsonObject
        let model = database.["model"] :?> JsonObject
        let kept = ResizeArray<string>()
        let warnings = ResizeArray<string>()

        (command.["object"] :?> JsonObject).["database"] <- JsonValue.Create target.Database
        database.["name"] <- JsonValue.Create target.Database

        match target.CompatibilityMode with
        | Some mode when not (isNull database.["compatibilityMode"]) ->
            let text = string mode

            database.["compatibilityMode"] <-
                JsonValue.Create(text.Substring(0, 1).ToLowerInvariant() + text.Substring 1)
        | _ -> ()

        match target.Deployed with
        | Some deployed when not (isNull deployed.Model) ->
            // An existing database is addressed by name and keeps its ID.
            if String.IsNullOrEmpty deployed.ID then
                database.Remove "id" |> ignore
            else
                database.["id"] <- JsonValue.Create deployed.ID

            mergeRoles opts model deployed.Model kept

            if not opts.Connections then
                mergeDataSources model deployed.Model kept

            if not opts.Partitions || opts.SkipRefreshPolicy then
                mergePartitions opts model local.Model deployed.Model kept

            if opts.Partitions then
                addPolicyPartitions model

            if level >= 1400 && not opts.SharedExpressions then
                model.["expressions"] <- JsonArray(deployed.Model.Expressions |> Seq.map serialize |> Array.ofSeq)

                if deployed.Model.Expressions.Count > 0 || local.Model.Expressions.Count > 0 then
                    kept.Add
                        $"""shared expressions: the server's ({plural deployed.Model.Expressions.Count "expression" "expressions"})"""

            if level < deployed.CompatibilityLevel then
                warnings.Add
                    $"the model's compatibility level ({level}) is lower than the deployed one ({deployed.CompatibilityLevel}); servers refuse to lower it"

            for change in kindChanges local.Model deployed.Model do
                warnings.Add $"{change}: the server may refuse this in one step (remove it there first)"
        | _ ->
            // A new database: there is nothing to keep, so everything comes from the model, except
            // role members, which are only ever deployed when asked for.
            database.Remove "id" |> ignore

            if not opts.RoleMembers then
                match array model "roles" with
                | Some roles ->
                    let mutable dropped = false

                    for role in roles do
                        match array (role :?> JsonObject) "members" with
                        | Some m when m.Count > 0 ->
                            dropped <- true
                            (role :?> JsonObject).["members"] <- JsonArray()
                        | _ -> ()

                    if dropped then
                        warnings.Add "role members are not deployed (use --deploy-role-members)"
                | None -> ()

            addPolicyPartitions model

        fixCalculationGroupColumns level model

        let merged =
            try
                Ok(ModelIO.parseBim (database.ToJsonString()))
            with e ->
                Error e.Message

        match merged with
        | Error reason ->
            fail
                $"the model doesn't fit what the server keeps ({reason}); deploy the missing parts too (--deploy-connections, --deploy-partitions, --deploy-shared-expressions, --deploy-roles)"
        | Ok merged ->
            let before =
                match target.Deployed with
                | Some d when not (isNull d.Model) -> d
                | _ -> emptyLike merged

            let known = errorsOf before |> List.map issueKey |> set

            Ok {
                Database = target.Database
                Creates = target.Deployed.IsNone
                Tmsl = root.ToJsonString writeOptions
                Merged = merged
                Changes = Diff.compare diff before merged
                IntroducedErrors = errorsOf merged |> List.filter (fun i -> not (known.Contains(issueKey i)))
                Warnings = List.ofSeq warnings
                Kept = List.ofSeq kept
            }
