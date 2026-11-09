/// `tess deploy`: put a model on a server. A dry run by default: it connects, reads what is deployed,
/// and shows what would change and the script it would run. Nothing is written without --execute.
module Tessera.Cli.DeployCommand

open System.IO
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "deploy a model to a server: shows the changes and the TMSL script; --execute to run it."

type Args =
    | [<MainCommand>] Target of string list
    | Execute
    | Overwrite
    | Force
    | Deploy_Connections
    | Deploy_Partitions
    | Skip_Refresh_Policy
    | Deploy_Shared_Expressions
    | Deploy_Roles
    | Deploy_Role_Members
    | Deploy_Full
    | Tmsl
    | Ignore of properties: string
    | Stat
    | Name_Only

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "<model> <server> [<database>]: the model to deploy (any path tess accepts, or another xmla: model), "
                + "the server (as for `tess connect`), and the database name there (or put it in the server: "
                + "\"<server>;<database>\")."
            | Execute -> "deploy for real (default: dry run, nothing on the server changes)."
            | Overwrite -> "with --execute: replace the model of a database that already exists."
            | Force -> "with --execute: deploy even if the model would have errors the deployed one doesn't."
            | Deploy_Connections -> "replace the server's data sources with the model's (default: keep the server's)."
            | Deploy_Partitions ->
                "replace the server's partitions and refresh policies with the model's (default: keep the server's, "
                + "so refresh state and incremental-refresh partitions survive)."
            | Skip_Refresh_Policy ->
                "with --deploy-partitions: still keep the partitions of tables that have a refresh policy on the server."
            | Deploy_Shared_Expressions ->
                "replace the server's shared (M) expressions with the model's (default: keep the server's)."
            | Deploy_Roles -> "replace the server's roles with the model's (default: keep the server's)."
            | Deploy_Role_Members ->
                "with --deploy-roles: also replace role members (default: each role keeps the members it has on the server)."
            | Deploy_Full -> "all of the --deploy-... options."
            | Tmsl -> "print only the TMSL script."
            | Ignore _ -> "leave these properties out of the changes shown (as in `tess diff`)."
            | Stat -> Mutation.Usage.stat
            | Name_Only -> Mutation.Usage.nameOnly

let private findingOf (i: Validate.Issue) =
    Findings.about i.Ref Findings.Severity.Error "validate" i.Rule i.Message

type private Outcome' = {
    Address: XmlaAddress
    Plan: Deploy.Plan
    /// Why --execute didn't deploy, if it didn't.
    Refused: TessError option
    Deployed: bool
    ServerMessages: string list
}

let private resultJson (dryRun: bool) (r: Outcome') : JsonNode =
    let o =
        Changes.changesJson "deploy" (ChangeSet.ofDiff r.Plan.Changes) :?> JsonObject

    o.["server"] <- str r.Address.DataSource
    o.["database"] <- str r.Plan.Database
    o.["dryRun"] <- bool dryRun
    o.["creates"] <- bool r.Plan.Creates
    o.["deployed"] <- bool r.Deployed
    o.["kept"] <- arr (r.Plan.Kept |> List.map str)
    o.["warnings"] <- arr (r.Plan.Warnings |> List.map str)
    o.["introducedErrors"] <- arr (r.Plan.IntroducedErrors |> List.map (findingOf >> Findings.findingJson))
    o.["serverMessages"] <- arr (r.ServerMessages |> List.map str)
    o.["tmsl"] <- str r.Plan.Tmsl

    match r.Refused with
    | Some e -> o.["error"] <- obj [ "code", str (TessError.code e); "message", str (TessError.message e) ]
    | None -> ()

    o

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let full = args.Contains Deploy_Full

    let opts: Deploy.Options = {
        Connections = full || args.Contains Deploy_Connections
        Partitions = full || args.Contains Deploy_Partitions
        SkipRefreshPolicy = args.Contains Skip_Refresh_Policy
        SharedExpressions = full || args.Contains Deploy_Shared_Expressions
        Roles = full || args.Contains Deploy_Roles || args.Contains Deploy_Role_Members
        RoleMembers = full || args.Contains Deploy_Role_Members
    }

    let diffOptions: Diff.Options = {
        Diff.Options.none with
            Ignore = parseList (args.GetResults Args.Ignore)
    }

    let execute = args.Contains Execute

    let target =
        match args.TryGetResult Args.Target |> Option.defaultValue [] with
        | [ model; server ] -> Ok(model, server, None)
        | [ model; server; database ] -> Ok(model, server, Some database)
        | _ -> Error "expected <model> <server> [<database>]"

    match target with
    | Error message -> UsageError message
    | Ok _ when args.Contains Stat && args.Contains Name_Only -> UsageError "--stat and --name-only can't be combined"
    | Ok _ when args.Contains Skip_Refresh_Policy && not opts.Partitions ->
        UsageError "--skip-refresh-policy only matters with --deploy-partitions"
    | Ok(model, server, database) ->
        let address =
            XmlaAddress.parseServer server
            |> Result.bind (fun a ->
                match a.Database, database with
                | Some inAddress, Some named when
                    not (System.String.Equals(inAddress, named, System.StringComparison.OrdinalIgnoreCase))
                    ->
                    Error(
                        InvalidServerAddress(server, $"it names database '{inAddress}', but '{named}' was given too")
                    )
                | _, Some named -> Ok { a with Database = Some named }
                | Some _, None -> Ok a
                | None, None -> Error(InvalidServerAddress(server, "no database name: add it as a third argument")))

        let sameAsSource (a: XmlaAddress) =
            XmlaAddress.isRemote model
            && (match XmlaAddress.parse model with
                | Ok s ->
                    XmlaAddress.sameServer s a
                    && s.Database
                       |> Option.exists (fun d ->
                           System.String.Equals(d, a.Database.Value, System.StringComparison.OrdinalIgnoreCase))
                | Error _ -> false)

        let result =
            address
            |> Result.bind (fun address ->
                if sameAsSource address then
                    Error(RemoteSourceUnsupported(model, "it is the database being deployed to"))
                else
                    Source.load ctx model
                    |> Result.bind (fun (_, local) ->
                        Remote.withConnection (Source.access ctx) address (fun c ->
                            let name = address.Database.Value

                            Remote.serverProperties c
                            |> Result.bind (fun server ->
                                Remote.findDatabase c name
                                |> Result.bind (fun existing ->
                                    let deployed =
                                        match existing with
                                        | Some d -> Remote.readDatabase c d.Name |> Result.map Some
                                        | None -> Ok None

                                    deployed
                                    |> Result.bind (fun deployed ->
                                        let target: Deploy.Target = {
                                            // An existing database keeps its exact name.
                                            Database = existing |> Option.map _.Name |> Option.defaultValue name
                                            Deployed = deployed
                                            CompatibilityMode = server.CompatibilityMode
                                            SupportedCompatibilityLevels = server.SupportedCompatibilityLevels
                                        }

                                        Deploy.plan opts diffOptions target local
                                        |> Result.mapError (function
                                            | DeployFailed(_, database, messages) ->
                                                DeployFailed(address.DataSource, database, messages)
                                            | e -> e)
                                        |> Result.bind (fun plan ->
                                            let refused =
                                                if not execute then
                                                    None
                                                elif not plan.Creates && not (args.Contains Overwrite) then
                                                    Some(DatabaseExists(address.DataSource, plan.Database))
                                                elif
                                                    not plan.IntroducedErrors.IsEmpty
                                                    && not (args.Contains Force)
                                                then
                                                    Some(DeployIntroducesErrors plan.IntroducedErrors.Length)
                                                else
                                                    None

                                            let outcome = {
                                                Address = address
                                                Plan = plan
                                                Refused = refused
                                                Deployed = false
                                                ServerMessages = []
                                            }

                                            if execute && refused.IsNone then
                                                Remote.execute c plan.Database plan.Tmsl
                                                |> Result.map (fun messages -> {
                                                    outcome with
                                                        Deployed = true
                                                        ServerMessages = messages
                                                })
                                            else
                                                Ok outcome)))))))

        match result with
        | Error e -> Failed e
        | Ok r ->
            let plan = r.Plan

            if ctx.Json then
                writeJson ctx.Out (resultJson (not execute) r)
            elif args.Contains Tmsl then
                ctx.Out.WriteLine plan.Tmsl
            else
                let out: TextWriter = ctx.Out
                let action = if plan.Creates then "create" else "replace the model of"
                out.WriteLine($"Deploy {model} -> {action} '{plan.Database}' on {r.Address.DataSource}")

                for k in plan.Kept do
                    out.WriteLine($"  kept: {k}")

                out.WriteLine()

                if plan.Changes.IsEmpty then
                    out.WriteLine("No changes.")
                elif args.Contains Stat then
                    Changes.statText out (ChangeSet.ofDiff plan.Changes)
                elif args.Contains Name_Only then
                    Changes.namesText out (ChangeSet.ofDiff plan.Changes)
                else
                    Changes.diffText out plan.Changes

                if not execute then
                    out.WriteLine()
                    out.WriteLine("TMSL:")
                    out.WriteLine plan.Tmsl

            // Notices go to stderr, so stdout stays the plan itself.
            if not ctx.Json then
                for w in plan.Warnings do
                    ctx.Err.WriteLine($"warning: {w}")

                for i in plan.IntroducedErrors do
                    ctx.Err.WriteLine($"error: {ObjectRef.format i.Ref}: {i.Message}")

                for m in r.ServerMessages do
                    ctx.Err.WriteLine($"server: {m}")

            match r.Refused with
            | Some e when ctx.Json -> DoneWith(ExitCode.ofError e)
            | Some e -> Failed e
            | None ->
                if not ctx.Json then
                    if r.Deployed then
                        ctx.Err.WriteLine(
                            $"Deployed '{plan.Database}' to {r.Address.DataSource}. New or changed tables may need a refresh."
                        )
                    else
                        let gates = [
                            if not plan.Creates && not (args.Contains Overwrite) then
                                "--overwrite (the database exists)"
                            if not plan.IntroducedErrors.IsEmpty && not (args.Contains Force) then
                                "--force (the model would have new errors)"
                        ]

                        let also =
                            match gates with
                            | [] -> ""
                            | g -> " It also needs " + String.concat " and " g + "."

                        ctx.Err.WriteLine($"Dry run: nothing deployed. Add --execute to deploy.{also}")

                Done
