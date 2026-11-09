/// What tessera does with a server, in terms of the seam (Session.fs): pure logic over
/// IXmlaConnector, so all of it is testable without a server. Failures come back as TessErrors
/// with credentials scrubbed.
module Tessera.Xmla.Remote

open System
open Microsoft.AnalysisServices.Tabular
open Tessera

/// What a command needs to reach servers.
type Access = {
    Connector: IXmlaConnector
    Environment: Credentials.Environment
    /// Shows the user a message they must act on (device-code instructions): stderr in the CLI.
    Notify: string -> unit
    IsWindows: bool
}

/// An open session and what is needed to report its failures safely.
type Connection = {
    Session: IXmlaSession
    Address: XmlaAddress
    Secrets: string list
}

let private eq (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// Run something on a session, turning what it throws into an error.
let private guard (c: Connection) (f: unit -> 'T) : Result<'T, TessError> =
    try
        Ok(f ())
    with e ->
        Error(Failures.classify c.Address c.Secrets e)

/// Connect to an address and use the session, which is closed afterwards.
let withConnection
    (access: Access)
    (address: XmlaAddress)
    (f: Connection -> Result<'T, TessError>)
    : Result<'T, TessError> =
    if address.Endpoint = XmlaEndpoint.Tcp && not access.IsWindows then
        Error(Failures.unsupportedTcp (XmlaAddress.server address))
    else
        match Credentials.resolve access.Environment address with
        | Error e -> Error e
        | Ok credential ->
            let secrets = XmlaCredential.secrets credential

            let opened =
                try
                    Ok(
                        access.Connector.Open {
                            Address = address
                            Credential = credential
                            Notify = access.Notify
                        }
                    )
                with e ->
                    Error(Failures.classify address secrets e)

            match opened with
            | Error e -> Error e
            | Ok session ->
                use session = session

                f {
                    Session = session
                    Address = address
                    Secrets = secrets
                }

let serverProperties (c: Connection) = guard c (fun () -> c.Session.Server)

let databases (c: Connection) : Result<DatabaseSummary list, TessError> =
    guard c (fun () -> c.Session.Databases() |> List.sortBy (fun d -> d.Name.ToUpperInvariant()))

/// The database an address means: the one it names (matched without regard to case), or the only
/// one the server has. Otherwise an error listing the databases that exist.
let resolveDatabase (c: Connection) : Result<DatabaseSummary, TessError> =
    let server = XmlaAddress.server c.Address

    databases c
    |> Result.bind (fun all ->
        let names = all |> List.map _.Name

        match c.Address.Database with
        | Some wanted ->
            match all |> List.tryFind (fun d -> eq d.Name wanted) with
            | Some d -> Ok d
            | None -> Error(DatabaseNotFound(server, wanted, names))
        | None ->
            match all with
            | [ one ] -> Ok one
            | _ -> Error(DatabaseRequired(server, names)))

/// The database an address names, if it exists (None: it doesn't). The address must name one.
let findDatabase (c: Connection) (name: string) : Result<DatabaseSummary option, TessError> =
    databases c |> Result.map (List.tryFind (fun d -> eq d.Name name))

let readDatabase (c: Connection) (name: string) : Result<Database, TessError> =
    guard c (fun () -> c.Session.ReadDatabase name)

/// Load a remote model: connect, find the database, read it. The source's path is the address
/// with the database's exact name.
let loadModel (access: Access) (address: XmlaAddress) : Result<ModelSource * Database, TessError> =
    withConnection access address (fun c ->
        resolveDatabase c
        |> Result.bind (fun d ->
            readDatabase c d.Name
            |> Result.map (fun db -> ModelSource.remote (XmlaAddress.withDatabase d.Name address), db)))

/// Run a DAX query against the database an address means.
let query
    (access: Access)
    (address: XmlaAddress)
    (dax: string)
    (maxRows: int option)
    : Result<XmlaAddress * QueryResult list, TessError> =
    withConnection access address (fun c ->
        resolveDatabase c
        |> Result.bind (fun d ->
            try
                Ok(XmlaAddress.withDatabase d.Name address, c.Session.Query(d.Name, dax, maxRows))
            with
            | XmlaQueryError message -> Error(QueryFailed(Secrets.scrub c.Secrets message))
            | e -> Error(Failures.classify c.Address c.Secrets e)))

/// Run a deployment script. Fails if the server reports any error; otherwise returns its warnings.
let execute (c: Connection) (database: string) (tmsl: string) : Result<string list, TessError> =
    let ran =
        try
            Ok(c.Session.Execute tmsl)
        with e ->
            // The connection was fine a moment ago: this is the script being refused.
            match Failures.classify c.Address c.Secrets e with
            | ConnectionFailed(server, reason, hint) ->
                Error(DeployFailed(server, database, reason :: Option.toList hint))
            | other -> Error other

    ran
    |> Result.bind (fun messages ->
        let text (m: XmlaMessage) = Secrets.scrub c.Secrets m.Text

        match messages |> List.filter _.IsError with
        | [] -> Ok(messages |> List.map text)
        | errors -> Error(DeployFailed(XmlaAddress.server c.Address, database, errors |> List.map text)))
