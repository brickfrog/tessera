module Tessera.Tests.DeployTests

open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private target (name: string) (deployed: Database option) : Deploy.Target = {
    Database = name
    Deployed = deployed
    CompatibilityMode = None
    SupportedCompatibilityLevels = []
}

let private plan opts name deployed local =
    Deploy.plan opts Diff.Options.none (target name deployed) local

/// The sample model as it might be on a server: its own partitions, a data source, a role with
/// members, a shared expression with another value.
let private serverDatabase () =
    let db = sampleDatabase ()
    db.Name <- "Prod"
    db.ID <- "prod-id-123"
    let sales = db.Model.Tables.["Sales"]
    sales.Partitions.Clear()

    for year in [ "2023"; "2024" ] do
        sales.Partitions.Add(
            new Partition(Name = $"Sales {year}", Source = new MPartitionSource(Expression = $"let y = {year} in y"))
        )

    db.Model.DataSources.Add(
        new StructuredDataSource(
            Name = "Warehouse",
            ConnectionDetails =
                new ConnectionDetails("""{"protocol":"tds","address":{"server":"prod-sql","database":"dw"}}""")
        )
    )

    db.Model.Roles.["Readers"].Members.Add(
        new ExternalModelRoleMember(MemberName = "ann@example.com", IdentityProvider = "AzureAD")
    )

    db.Model.Roles.Add(new ModelRole(Name = "Server Only", ModelPermission = ModelPermission.Read))

    db.Model.Expressions.["ServerName"].Expression <-
        "\"prod-sql\" meta [IsParameterQuery=true, Type=\"Text\", IsParameterQueryRequired=true]"

    db

/// The local model: a new measure, a changed partition, a role member the server doesn't have.
let private localDatabase () =
    let db = sampleDatabase ()
    db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Average", Expression = "AVERAGE(Sales[Amount])"))
    db.Model.Tables.["Sales"].Partitions.["Sales"].Source <- new MPartitionSource(Expression = "let dev = 1 in dev")

    db.Model.Roles.["Readers"].Members.Add(
        new ExternalModelRoleMember(MemberName = "dev@example.com", IdentityProvider = "AzureAD")
    )

    db

let private partitionNames (db: Database) (table: string) =
    db.Model.Tables.[table].Partitions |> Seq.map _.Name |> List.ofSeq

let private members (db: Database) (role: string) =
    db.Model.Roles.[role].Members |> Seq.map _.MemberName |> List.ofSeq

let private changed (p: Deploy.Plan) =
    p.Changes
    |> ChangeSet.ofDiff
    |> List.map (fun c -> ObjectRef.format c.Ref)
    |> List.sort

[<Tests>]
let tests =
    testList "Deploy.plan" [
        testCase "by default only metadata changes: the server keeps partitions, data sources, expressions, roles"
        <| fun () ->
            let p =
                plan Deploy.Options.defaults "prod" (Some(serverDatabase ())) (localDatabase ())
                |> ok

            Expect.isFalse p.Creates ""
            Expect.equal p.Database "prod" ""
            Expect.equal (partitionNames p.Merged "Sales") [ "Sales 2023"; "Sales 2024" ] "the server's partitions"
            Expect.equal (partitionNames p.Merged "Date") [ "Date" ] ""
            Expect.equal [ for d in p.Merged.Model.DataSources -> d.Name ] [ "Warehouse" ] "the server's data source"

            Expect.stringContains
                p.Merged.Model.Expressions.["ServerName"].Expression
                "prod-sql"
                "the server's expression"

            Expect.equal [ for r in p.Merged.Model.Roles -> r.Name ] [ "Readers"; "Server Only" ] "the server's roles"
            Expect.equal (members p.Merged "Readers") [ "ann@example.com" ] "the server's members"
            Expect.isNotNull (p.Merged.Model.Tables.["Sales"].Measures.Find "Average") "the model's new measure"
            // The only change the server would see is the new measure.
            Expect.equal (changed p) [ "measure:Sales/Average" ] ""
            Expect.isEmpty p.IntroducedErrors ""
            Expect.equal p.Kept.Length 4 (String.concat "; " p.Kept)

        testCase "the script is a createOrReplace of the merged model, addressed by name and keeping the ID"
        <| fun () ->
            let p =
                plan Deploy.Options.defaults "PROD" (Some(serverDatabase ())) (localDatabase ())
                |> ok

            use doc = JsonDocument.Parse p.Tmsl
            let command = doc.RootElement.GetProperty "createOrReplace"
            Expect.equal (command.GetProperty("object").GetProperty("database").GetString()) "PROD" ""
            let database = command.GetProperty "database"
            Expect.equal (database.GetProperty("name").GetString()) "PROD" ""
            Expect.equal (database.GetProperty("id").GetString()) "prod-id-123" ""
            // What the script holds is what the plan says the server will hold.
            let fromScript = ModelIO.parseBim (database.GetRawText())
            Expect.isEmpty (Diff.compare Diff.Options.none p.Merged fromScript) ""
            Expect.isFalse (p.Tmsl.Contains "dev@example.com") "role members weren't asked for"

        testCase "--deploy-partitions, --deploy-connections, --deploy-shared-expressions take the model's"
        <| fun () ->
            let opts = {
                Deploy.Options.defaults with
                    Partitions = true
                    Connections = true
                    SharedExpressions = true
            }

            let p = plan opts "Prod" (Some(serverDatabase ())) (localDatabase ()) |> ok
            Expect.equal (partitionNames p.Merged "Sales") [ "Sales" ] ""
            Expect.equal p.Merged.Model.DataSources.Count 0 "the model has no data sources"
            Expect.stringContains p.Merged.Model.Expressions.["ServerName"].Expression "localhost" ""
            Expect.contains (changed p) "partition:Sales/Sales" ""
            Expect.contains (changed p) "datasource:Warehouse" ""

        testCase "--deploy-roles without members keeps each role's server members; with members replaces them"
        <| fun () ->
            let local = localDatabase ()
            local.Model.Roles.Add(new ModelRole(Name = "New Role", ModelPermission = ModelPermission.Read))

            local.Model.Roles.["New Role"].Members.Add(
                new ExternalModelRoleMember(MemberName = "new@example.com", IdentityProvider = "AzureAD")
            )

            let roles = {
                Deploy.Options.defaults with
                    Roles = true
            }

            let p = plan roles "Prod" (Some(serverDatabase ())) local |> ok
            Expect.equal [ for r in p.Merged.Model.Roles -> r.Name ] [ "Readers"; "New Role" ] "the model's roles"
            Expect.equal (members p.Merged "Readers") [ "ann@example.com" ] ""
            Expect.equal (members p.Merged "New Role") [] "a role new to the server gets no members"

            let both = { roles with RoleMembers = true }
            let p = plan both "Prod" (Some(serverDatabase ())) local |> ok
            Expect.equal (members p.Merged "Readers") [ "dev@example.com" ] ""
            Expect.equal (members p.Merged "New Role") [ "new@example.com" ] ""

        testCase "--skip-refresh-policy keeps the partitions of tables with a refresh policy on the server"
        <| fun () ->
            let server = serverDatabase ()

            server.Model.Tables.["Sales"].RefreshPolicy <-
                new BasicRefreshPolicy(
                    SourceExpression = "let p = 1 in p",
                    RollingWindowGranularity = RefreshGranularityType.Year,
                    RollingWindowPeriods = 3,
                    IncrementalGranularity = RefreshGranularityType.Month,
                    IncrementalPeriods = 1
                )

            let opts = {
                Deploy.Options.defaults with
                    Partitions = true
                    SkipRefreshPolicy = true
            }

            let local = localDatabase ()

            local.Model.Tables.["Date"].Partitions.["Date"].Source <-
                new MPartitionSource(Expression = "let d = 2 in d")

            let p = plan opts "Prod" (Some server) local |> ok
            Expect.equal (partitionNames p.Merged "Sales") [ "Sales 2023"; "Sales 2024" ] "kept: it has a policy"
            Expect.isNotNull p.Merged.Model.Tables.["Sales"].RefreshPolicy "and the policy with it"

            Expect.stringContains
                (p.Merged.Model.Tables.["Date"].Partitions.["Date"].Source :?> MPartitionSource).Expression
                "d = 2"
                "no policy: the model's partition"

        testCase "a calculation group never takes the server's partitions"
        <| fun () ->
            let p =
                plan Deploy.Options.defaults "Prod" (Some(serverDatabase ())) (localDatabase ())
                |> ok

            let partition =
                p.Merged.Model.Tables.["Time Intelligence"].Partitions |> Seq.exactlyOne

            Expect.equal partition.SourceType PartitionSourceType.CalculationGroup ""

        testCase "a new database takes everything from the model except role members"
        <| fun () ->
            let p = plan Deploy.Options.defaults "Fresh" None (localDatabase ()) |> ok
            Expect.isTrue p.Creates ""
            Expect.equal p.Merged.Name "Fresh" ""
            Expect.equal (partitionNames p.Merged "Sales") [ "Sales" ] ""

            Expect.equal
                [ for r in p.Merged.Model.Roles -> r.Name ]
                [ "Readers" ]
                "roles are deployed: there is nothing to keep"

            Expect.equal (members p.Merged "Readers") [] ""
            Expect.isNonEmpty p.Warnings "says that members were left out"
            use doc = JsonDocument.Parse p.Tmsl
            let database = doc.RootElement.GetProperty("createOrReplace").GetProperty "database"
            let has, _ = database.TryGetProperty "id"
            Expect.isFalse has "the server assigns the ID"
            Expect.contains (changed p) "table:Sales" "everything is new"

            let withMembers = {
                Deploy.Options.defaults with
                    Roles = true
                    RoleMembers = true
            }

            let p = plan withMembers "Fresh" None (localDatabase ()) |> ok
            Expect.equal (members p.Merged "Readers") [ "dev@example.com" ] ""

        testCase "an unnamed local model (as Power BI Desktop writes TMDL) deploys under the target name"
        <| fun () ->
            use t = new TempDir()
            let folder = System.IO.Path.Combine(t.Path, "tmdl")
            let db = sampleDatabase ()

            ModelIO.save ModelIO.TmdlFolder ModelIO.SaveOptions.defaults false db folder
            |> ok
            |> ignore

            System.IO.File.Delete(System.IO.Path.Combine(folder, "database.tmdl"))
            let _, unnamed = ModelIO.loadPath folder |> ok
            let p = plan Deploy.Options.defaults "Named" None unnamed |> ok
            Expect.equal p.Merged.Name "Named" ""

        testCase "errors the deployed model already has aren't held against the deployment; new ones are"
        <| fun () ->
            let server = serverDatabase ()
            server.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Old Broken", Expression = "'Gone'[X]"))
            let local = localDatabase ()
            local.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Old Broken", Expression = "'Gone'[X]"))
            let p = plan Deploy.Options.defaults "Prod" (Some server) local |> ok
            Expect.isEmpty p.IntroducedErrors "it was broken before"

            local.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "New Broken", Expression = "'Nope'[X]"))
            let p = plan Deploy.Options.defaults "Prod" (Some server) local |> ok
            Expect.equal [ for i in p.IntroducedErrors -> ObjectRef.format i.Ref ] [ "measure:Sales/New Broken" ] ""

        testCase "what the server keeps can break the model: a new table needing an expression the server lacks"
        <| fun () ->
            let local = localDatabase ()

            local.Model.Expressions.Add(
                new NamedExpression(Name = "NewParam", Kind = ExpressionKind.M, Expression = "1")
            )
            // Kept expressions: the server's set, so NewParam is absent and reported as a removal.
            let p = plan Deploy.Options.defaults "Prod" (Some(serverDatabase ())) local |> ok
            Expect.isNull (p.Merged.Model.Expressions.Find "NewParam") ""
            Expect.exists p.Kept (fun k -> k.Contains "shared expressions") ""

        testCase "compatibility level: unsupported is refused, lowering is warned about"
        <| fun () ->
            let unsupported = {
                target "Prod" None with
                    SupportedCompatibilityLevels = [ 1200; 1400; 1500 ]
            }

            match Deploy.plan Deploy.Options.defaults Diff.Options.none unsupported (localDatabase ()) with
            | Error(DeployFailed(_, "Prod", [ reason ])) -> Expect.stringContains reason "1702" ""
            | other -> failtestf "%A" other

            let server = serverDatabase ()
            server.CompatibilityLevel <- 1703
            let p = plan Deploy.Options.defaults "Prod" (Some server) (localDatabase ()) |> ok
            Expect.exists p.Warnings (fun w -> w.Contains "1702" && w.Contains "1703") ""

        testCase "a table changing between calculated and loaded is warned about"
        <| fun () ->
            let local = localDatabase ()
            let date = local.Model.Tables.["Date"]
            date.Partitions.Clear()

            date.Partitions.Add(
                new Partition(Name = "Date", Source = new CalculatedPartitionSource(Expression = "CALENDARAUTO()"))
            )

            let p = plan Deploy.Options.defaults "Prod" (Some(serverDatabase ())) local |> ok
            Expect.exists p.Warnings (fun w -> w.Contains "'Date'" && w.Contains "calculated") ""

        testCase "the compatibility mode in the script follows the server"
        <| fun () ->
            let t = {
                target "Prod" None with
                    CompatibilityMode = Some Microsoft.AnalysisServices.CompatibilityMode.AnalysisServices
            }

            let p =
                Deploy.plan Deploy.Options.defaults Diff.Options.none t (localDatabase ()) |> ok

            use doc = JsonDocument.Parse p.Tmsl
            let database = doc.RootElement.GetProperty("createOrReplace").GetProperty "database"
            let has, mode = database.TryGetProperty "compatibilityMode"

            if has then
                Expect.equal (mode.GetString()) "analysisServices" ""

        testCase "planning leaves the local and deployed models untouched"
        <| fun () ->
            let local = localDatabase ()
            let server = serverDatabase ()
            let localBefore = ModelIO.clone local
            let serverBefore = ModelIO.clone server
            plan Deploy.Options.full "Other Name" (Some server) local |> ok |> ignore
            Expect.isEmpty (Diff.compare Diff.Options.none localBefore local) ""
            Expect.isEmpty (Diff.compare Diff.Options.none serverBefore server) ""
            Expect.equal local.Name "Sample" ""
    ]
