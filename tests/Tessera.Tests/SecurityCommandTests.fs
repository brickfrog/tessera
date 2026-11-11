/// `--role` on `tess query` (model files and a server), `roles:` and `identity:` in `tess test`,
/// and roles in `--coverage`.
module Tessera.Tests.SecurityCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.XmlaFakes
open Tessera.Tests.EngineHelpers
open Tessera.Cli.Common

/// The star model with roles: UK (Bob: sales 2 and 5, Amount 100 + 40 = 140), Me (the customer
/// whose name is CUSTOMDATA), Bulk (sales of 3 or more: 3, 5 and 6, 60 + 40 + 100 = 200), Hidden
/// (object-level security on Product). Sale 5 has a product that doesn't exist (Product's blank
/// row): a role that hides it is refused, so these roles keep it.
let private withRoles (db: Database) =
    let add (name: string) (table: string) (dax: string) =
        let r = new ModelRole(Name = name, ModelPermission = ModelPermission.Read)
        r.TablePermissions.Add(new TablePermission(Table = db.Model.Tables.[table], FilterExpression = dax))
        db.Model.Roles.Add r

    add "UK" "Customer" "Customer[Country] = \"UK\""
    add "Me" "Customer" "Customer[Name] = CUSTOMDATA ()"
    add "Bulk" "Sales" "Sales[Qty] >= 3"
    let hidden = new ModelRole(Name = "Hidden", ModelPermission = ModelPermission.Read)

    hidden.TablePermissions.Add(
        new TablePermission(Table = db.Model.Tables.["Product"], MetadataPermission = MetadataPermission.None)
    )

    db.Model.Roles.Add hidden

/// model.bim with the roles, and its data in `data`.
let private setup () =
    let t = new TempDir()
    let db, store = build star
    withRoles db
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    let dir = t.Combine "data"
    Directory.CreateDirectory dir |> ignore

    for table in store.Tables do
        use writer =
            new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

        Fixture.writeCsv writer table

    t, path, dir

let private lines (s: string) =
    s.Split('\n')
    |> Array.map (fun l -> l.TrimEnd '\r')
    |> Array.filter ((<>) "")
    |> List.ofArray

let private errorCode (r: Run) =
    use doc = json r
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

let private remote = $"xmla:{workspace};Test"

/// A server holding the model, answering every query with one row.
let private server () =
    let db, _ = build star
    let s = FakeServer()
    s.Add db

    s.OnQuery <-
        fun _ -> [
            {
                Columns = [
                    {
                        Name = "[Value]"
                        Type = QueryType.Int64
                    }
                ]
                Rows = [ [| QueryValue.Int64 1L |] ]
                Truncated = false
            }
        ]

    s

let private properties (s: FakeServer) =
    s.Requests |> List.map (fun r -> r.Address.Properties)

[<Tests>]
let tests =
    testList "CLI roles" [
        testCase "query --role over model files" (fun () ->
            let t, model, dir = setup ()
            use _ = t

            let all = tess [ "query"; model; "--data"; dir; "EVALUATE { [Amount] }"; "--csv" ]
            Expect.equal all.Code ExitCode.ok all.Err
            Expect.equal (lines all.Out |> List.tail) [ "625" ] "everything"

            let uk =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "--role"
                    "UK"
                    "EVALUATE { [Amount] }"
                    "--csv"
                ]

            Expect.equal uk.Code ExitCode.ok uk.Err
            Expect.equal (lines uk.Out |> List.tail) [ "140" ] "the UK role"

            let me =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "--role"
                    "Me"
                    "--custom-data"
                    "Bob"
                    "EVALUATE { [Orders] }"
                    "--csv"
                ]

            Expect.equal me.Code ExitCode.ok me.Err
            Expect.equal (lines me.Out |> List.tail) [ "2" ] "CUSTOMDATA in the role's filter: Bob's two sales"

            // Cy's sales don't include sale 5: the role would take Product's blank row away.
            let refused =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "--role"
                    "Me"
                    "--custom-data"
                    "Cy"
                    "EVALUATE { 1 }"
                    "--json"
                ]

            Expect.equal (errorCode refused) "not-supported" refused.Out
            Expect.stringContains refused.Out "blank row" "why")

        testCase "query --role: an unknown role, object-level security" (fun () ->
            let t, model, dir = setup ()
            use _ = t

            let r =
                tess [ "query"; model; "--data"; dir; "--role"; "Nope"; "EVALUATE { 1 }"; "--json" ]

            Expect.equal r.Code ExitCode.input r.Out
            Expect.equal (errorCode r) "role-not-found" "code"

            let r =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "--role"
                    "Hidden"
                    "EVALUATE { 1 }"
                    "--json"
                ]

            Expect.equal (errorCode r) "not-supported" "code"
            Expect.stringContains r.Out "object-level security" "why")

        testCase "coverage lists roles; --role doesn't go with it" (fun () ->
            let t, model, _ = setup ()
            use _ = t
            let r = tess [ "query"; model; "--coverage"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let roles = doc.RootElement.GetProperty("summary").GetProperty("roles")
            Expect.equal (roles.GetProperty("total").GetInt32()) 4 "four roles"
            Expect.equal (roles.GetProperty("supported").GetInt32()) 3 "Hidden is not"

            let usage = tess [ "query"; model; "--coverage"; "--role"; "UK" ]
            Expect.equal usage.Code ExitCode.usage usage.Out)

        testCase "query --role and the identity on a server become connection properties" (fun () ->
            let s = server ()

            let r =
                tessOn s tokenEnv [
                    "query"
                    remote
                    "--role"
                    "UK"
                    "--role"
                    "Me"
                    "--custom-data"
                    "Cy"
                    "--user-principal-name"
                    "cy@contoso.com"
                    "EVALUATE { 1 }"
                ]

            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (properties s)
                [
                    [ "Roles", "UK,Me"; "EffectiveUserName", "cy@contoso.com"; "CustomData", "Cy" ]
                ]
                "the connection's properties")

        testCase "on a server: what has no connection property is refused before connecting" (fun () ->
            let s = server ()

            for args in
                [
                    [ "--user-culture"; "en-US" ]
                    [ "--user-object-id"; "x" ]
                    [ "--user-name"; "a"; "--user-principal-name"; "b" ]
                    [ "--role"; "A,B" ]
                ] do
                let r = tessOn s tokenEnv ([ "query"; remote ] @ args @ [ "EVALUATE { 1 }" ])
                Expect.equal r.Code ExitCode.usage $"{args}: {r.Out}{r.Err}"

            let r =
                tessOn s tokenEnv [
                    "query"
                    $"xmla:Data Source={workspace};Initial Catalog=Test;Roles=X"
                    "--role"
                    "UK"
                    "EVALUATE { 1 }"
                ]

            Expect.equal r.Code ExitCode.usage r.Out
            Expect.isEmpty s.Requests "no connection")

        testCase "test: roles and identity in a suite, a test's replacing the suite's" (fun () ->
            let t, model, _ = setup ()
            use _ = t

            write
                (t.Combine(".tess-tests", "s.test.yaml"))
                """suite: S
data: ../data
roles: UK
tests:
  - name: the suite's role
    measure: Amount
    expect: 140
  - name: a test's own roles
    roles: [ Bulk ]
    measure: Amount
    expect: 200
  - name: and identity
    roles: Me
    identity: { custom_data: Bob }
    measure: Orders
    expect: 2
  - name: no role
    roles: []
    measure: Amount
    expect: 625
"""

            let r = tess [ "test"; "run"; model; "--json" ]
            use doc = json r

            let outcomes = [
                for s in doc.RootElement.GetProperty("suites").EnumerateArray() do
                    for x in s.GetProperty("tests").EnumerateArray() ->
                        x.GetProperty("name").GetString(), x.GetProperty("outcome").GetString()
            ]

            Expect.equal (outcomes |> List.map snd) [ "passed"; "passed"; "passed"; "passed" ] (r.Out + r.Err)

            // A role the model doesn't have stops the run, like fixture data that can't be loaded.
            write
                (t.Combine(".tess-tests", "s.test.yaml"))
                "suite: S\ndata: ../data\ntests:\n  - name: x\n    roles: Nope\n    measure: Amount\n    expect: 1\n"

            let r = tess [ "test"; "run"; model; "--json" ]
            Expect.equal r.Code ExitCode.input r.Out
            Expect.equal (errorCode r) "role-not-found" r.Out)

        testCase "test: on a server, a test with roles has its own connection" (fun () ->
            let t, _, _ = setup ()
            use _ = t
            let s = server ()

            write
                (t.Combine(".tess-tests", "s.test.yaml"))
                """suite: S
tests:
  - name: plain
    expression: 1
    expect: 1
  - name: as UK
    roles: [ UK, Me ]
    identity: { user_name: "contoso\\cy", custom_data: Cy }
    expression: 1
    expect: 1
  - name: culture
    identity: { user_culture: fr-FR }
    expression: 1
    expect: 1
"""

            let r =
                tessOn s tokenEnv [ "test"; "run"; remote; t.Combine ".tess-tests"; "--json" ]

            Expect.equal r.Code ExitCode.ok (r.Out + r.Err)

            Expect.equal
                (properties s)
                [
                    []
                    [ "Roles", "UK,Me"; "EffectiveUserName", "contoso\\cy"; "CustomData", "Cy" ]
                ]
                "a second connection, with the test's roles"

            Expect.stringContains r.Out "no connection property" "user_culture is skipped on a server")
    ]
