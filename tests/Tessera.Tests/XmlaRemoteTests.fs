module Tessera.Tests.XmlaRemoteTests

open Expecto
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private address (text: string) = XmlaAddress.parseServer text |> ok

let private access (server: IXmlaConnector) (vars: (string * string) list) (windows: bool) : Remote.Access =
    let vars = Map.ofList vars

    {
        Connector = server
        Environment = {
            GetEnv = fun name -> Map.tryFind name vars
            ReadSecret = fun path -> Error $"no file {path}"
        }
        Notify = ignore
        IsWindows = windows
    }

let private serverWith (names: string list) =
    let server = FakeServer()

    for name in names do
        let db = sampleDatabase ()
        db.Name <- name
        db.ID <- name + "-id"
        server.Add db

    server

[<Tests>]
let tests =
    testList "Xmla remote" [
        testCase "loads the named database (any case) and reports its exact name; the session is closed"
        <| fun () ->
            let server = serverWith [ "Sales"; "Finance" ]

            let source, db =
                Remote.loadModel (access server tokenEnv false) (address $"{workspace};sales")
                |> ok

            Expect.equal source.Format Xmla ""
            Expect.equal source.Path $"xmla:{workspace};Sales" ""
            Expect.equal source.SemanticModelFolder None ""
            Expect.equal db.Name "Sales" ""
            Expect.isNull db.Server "detached"
            Expect.equal server.Closed 1 ""
            let request = server.Requests |> List.exactlyOne
            Expect.equal request.Credential (XmlaCredential.AccessToken fakeToken) ""
            Expect.equal request.Address.DataSource workspace ""

        testCase "a wrong database name lists the ones that exist"
        <| fun () ->
            let server = serverWith [ "Sales"; "finance"; "Budget" ]

            match Remote.loadModel (access server tokenEnv false) (address $"{workspace};Sale") with
            | Error(DatabaseNotFound(s, "Sale", existing) as e) ->
                Expect.equal s workspace ""
                Expect.equal existing [ "Budget"; "finance"; "Sales" ] "sorted without regard to case"
                Expect.stringContains (TessError.message e) "  finance" ""
            | other -> failtestf "%A" other

            Expect.equal server.Closed 1 "closed on failure too"

        testCase "no database named: fine with exactly one, otherwise the list"
        <| fun () ->
            let _, db =
                Remote.loadModel (access (serverWith [ "Only" ]) tokenEnv false) (address workspace)
                |> ok

            Expect.equal db.Name "Only" ""

            match Remote.loadModel (access (serverWith [ "A"; "B" ]) tokenEnv false) (address workspace) with
            | Error(DatabaseRequired(_, [ "A"; "B" ])) -> ()
            | other -> failtestf "%A" other

            match Remote.loadModel (access (serverWith []) tokenEnv false) (address workspace) with
            | Error(DatabaseRequired(_, []) as e) -> Expect.stringContains (TessError.message e) "no databases" ""
            | other -> failtestf "%A" other

        testCase "without credentials nothing is opened"
        <| fun () ->
            let server = serverWith [ "Sales" ]

            match Remote.loadModel (access server [] false) (address $"{workspace};Sales") with
            | Error(AuthenticationRequired _) -> ()
            | other -> failtestf "%A" other

            Expect.isEmpty server.Requests ""

        testCase "SSAS over TCP: refused before connecting off Windows, attempted on Windows"
        <| fun () ->
            let server = serverWith [ "Db" ]

            match Remote.loadModel (access server [] false) (address "localhost:51234") with
            | Error(UnsupportedConnection("localhost:51234", reason)) -> Expect.stringContains reason "Windows" ""
            | other -> failtestf "%A" other

            Expect.isEmpty server.Requests ""

            let _, db =
                Remote.loadModel (access server [] true) (address "localhost:51234") |> ok

            Expect.equal db.Name "Db" ""
            Expect.equal (server.Requests |> List.exactlyOne).Credential XmlaCredential.Integrated ""

        testCase "a failing connection is classified, and scrubbed of the secret in use"
        <| fun () ->
            let server = serverWith []
            let secret = "v3ry-s3cr3t"
            server.OpenError <- Some(exn $"AADSTS7000215: Invalid client secret provided: {secret}")

            let vars = [
                Credentials.Var.clientId, "app"
                Credentials.Var.tenantId, "tenant"
                Credentials.Var.clientSecret, secret
            ]

            match Remote.loadModel (access server vars false) (address workspace) with
            | Error(AuthenticationFailed _ as e) ->
                Expect.isFalse ((TessError.message e).Contains secret) (TessError.message e)
                Expect.stringContains (TessError.message e) "***" ""
            | other -> failtestf "%A" other

        testCase "query: results, the row limit, the resolved database, and query errors apart from connection errors"
        <| fun () ->
            let server = serverWith [ "Sales" ]

            let result: QueryResult = {
                Columns = [ { Name = "[N]"; Type = QueryType.Int64 } ]
                Rows = [ for i in 1L .. 5L -> [| QueryValue.Int64 i |] ]
                Truncated = false
            }

            server.OnQuery <- fun _ -> [ result; { result with Rows = [] } ]
            let a = address $"{workspace};SALES"

            let resolved, results =
                Remote.query (access server tokenEnv false) a "EVALUATE x" (Some 2) |> ok

            Expect.equal resolved.Database (Some "Sales") ""
            Expect.equal [ for r in results -> r.Rows.Length, r.Truncated ] [ 2, true; 0, false ] ""
            Expect.equal server.Queries [ "Sales", "EVALUATE x", Some 2 ] ""

            server.OnQuery <- fun _ -> raise (XmlaQueryError $"Syntax error near '{fakeToken}'")

            match Remote.query (access server tokenEnv false) a "EVALUATE" None with
            | Error(QueryFailed reason) -> Expect.equal reason "Syntax error near '***'" ""
            | other -> failtestf "%A" other

            server.OnQuery <- fun _ -> failwith "The connection either timed out or was lost."

            match Remote.query (access server tokenEnv false) a "EVALUATE x" None with
            | Error(ConnectionFailed _) -> ()
            | other -> failtestf "%A" other

        testCase "execute: warnings come back, an error fails the deployment"
        <| fun () ->
            let server = serverWith [ "Sales" ]
            let a = address $"{workspace};Sales"

            let run () =
                Remote.withConnection (access server tokenEnv false) a (fun c -> Remote.execute c "Sales" "{}")

            server.ExecuteMessages <- [ { IsError = false; Text = "a warning" } ]
            Expect.equal (run ()) (Ok [ "a warning" ]) ""

            server.ExecuteMessages <- [
                { IsError = false; Text = "a warning" }
                {
                    IsError = true
                    Text = "column X not found"
                }
            ]

            match run () with
            | Error(DeployFailed(s, "Sales", [ "column X not found" ])) -> Expect.equal s workspace ""
            | other -> failtestf "%A" other

            server.ExecuteMessages <- []
            server.ExecuteError <- Some(exn "The remote server returned an error: (403) Forbidden.")

            match run () with
            | Error(DeployFailed(_, "Sales", reason :: _) as e) ->
                Expect.stringContains reason "403" ""
                Expect.stringContains (TessError.message e) "Deploying 'Sales'" "not \"can't connect\""
            | other -> failtestf "%A" other
    ]
