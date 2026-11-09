/// Stand-ins for a server. No test opens a real connection: they go through these.
module Tessera.Tests.XmlaFakes

open System
open System.Text.Json.Nodes
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla

/// A connector for commands that must stay offline: it counts attempts (the harness fails the
/// test on any) and refuses.
type NoConnections() =
    let mutable attempts = 0
    member _.Attempts = attempts

    interface IXmlaConnector with
        member _.Open(_: ConnectionRequest) =
            attempts <- attempts + 1
            failwith "this test must not open a server connection"

/// An in-memory server: databases that can be read and replaced by a createOrReplace script, and a
/// scripted answer for queries. It records everything asked of it.
type FakeServer() =
    let databases = ResizeArray<Database>()
    let requests = ResizeArray<ConnectionRequest>()
    let executed = ResizeArray<string>()
    let queries = ResizeArray<string * string * int option>()
    let mutable closed = 0

    /// Thrown by Open instead of connecting, when set.
    member val OpenError: exn option = None with get, set
    /// Thrown by Execute, when set.
    member val ExecuteError: exn option = None with get, set
    /// Messages Execute returns. With an error among them, the script is not applied.
    member val ExecuteMessages: XmlaMessage list = [] with get, set
    /// The answer to a query (before any row limit).
    member val OnQuery: string -> QueryResult list = (fun _ -> []) with get, set

    member val Properties: ServerProperties =
        {
            Version = "16.0.1.2"
            ProductName = "Fake Analysis Services"
            CompatibilityMode = Some Microsoft.AnalysisServices.CompatibilityMode.PowerBI
            SupportedCompatibilityLevels = []
        } with get, set

    member _.Databases = databases
    member _.Requests = List.ofSeq requests
    member _.Executed = List.ofSeq executed
    member _.Queries = List.ofSeq queries
    /// Sessions closed so far.
    member _.Closed = closed

    member _.Add(db: Database) = databases.Add db

    member _.Find(name: string) =
        databases
        |> Seq.tryFind (fun d -> String.Equals(d.Name, name, StringComparison.OrdinalIgnoreCase))

    member private this.Apply(script: string) =
        match JsonNode.Parse(script).["createOrReplace"] with
        | null -> ()
        | command ->
            let db = ModelIO.parseBim (command.["database"].ToJsonString())
            let name = command.["object"].["database"].GetValue<string>()
            this.Find name |> Option.iter (fun old -> databases.Remove old |> ignore)
            databases.Add db

    interface IXmlaConnector with
        member this.Open(request: ConnectionRequest) =
            requests.Add request

            match this.OpenError with
            | Some e -> raise e
            | None -> ()

            {
                new IXmlaSession with
                    member _.Server = this.Properties

                    member _.Databases() = [
                        for d in databases ->
                            {
                                Name = d.Name
                                Id = d.ID
                                CompatibilityLevel = d.CompatibilityLevel
                            }
                    ]

                    member _.ReadDatabase(name: string) =
                        match this.Find name with
                        | Some d -> ResultReader.detach d
                        | None -> failwith $"No database '{name}' on the server."

                    member _.Execute(script: string) =
                        executed.Add script
                        this.ExecuteError |> Option.iter raise

                        if not (this.ExecuteMessages |> List.exists _.IsError) then
                            this.Apply script

                        this.ExecuteMessages

                    member _.Query(database: string, dax: string, maxRows: int option) =
                        queries.Add(database, dax, maxRows)

                        this.OnQuery dax
                        |> List.map (fun r ->
                            match maxRows with
                            | Some limit when r.Rows.Length > limit -> {
                                r with
                                    Rows = List.truncate limit r.Rows
                                    Truncated = true
                              }
                            | _ -> r)
                interface IDisposable with
                    member _.Dispose() = closed <- closed + 1
            }

/// A workspace address the fakes answer for (never contacted).
let workspace = "powerbi://api.powerbi.com/v1.0/myorg/Test Workspace"

/// A made-up access token: shaped like a JWT, so scrubbing by shape can be tested too.
let fakeToken =
    "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJhdWQiOiJ0ZXN0IiwiZXhwIjo0MTAyNDQ0ODAwfQ.c2lnbmF0dXJl"

/// Environment variables that sign in with the made-up token.
let tokenEnv = [ Credentials.Var.token, fakeToken ]
