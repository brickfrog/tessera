namespace Tessera.Xmla

open System
open System.Threading.Tasks
open Microsoft.AnalysisServices.AdomdClient
open Microsoft.Identity.Client
open Tessera

// The real thing: connections through Microsoft's client libraries (AMO/TOM for metadata and
// scripts, ADOMD.NET for queries, MSAL for device-code sign-in). This is the only file in tessera
// that opens a connection to a server, and it does so only when a command calls
// IXmlaConnector.Open with an address the user gave.
//
// What gets contacted, all by the client libraries: the server named in the address; for
// powerbi:// and asazure:// also Microsoft's sign-in service (login.microsoftonline.com or the
// national cloud's) unless an access token is supplied, and the service's own name resolution
// (the Power BI workspace lookup on the address's host; for asazure://, the region's host and
// global.asazure.windows.net). Nothing else.

type private AmoToken = Microsoft.AnalysisServices.AccessToken
type private AmoServer = Microsoft.AnalysisServices.Tabular.Server

module private Live =

    /// The application AMO/ADOMD themselves sign in through (their DefaultApplicationId).
    let analysisServicesClientId = "cf710c6e-dfcc-4fa8-a093-d47294e44c66"

    let staticToken (token: string) =
        let expiry =
            Credentials.tokenExpiry token
            |> Option.defaultWith (fun () -> DateTimeOffset.UtcNow.AddHours 1.0)

        AmoToken(token, expiry)

    /// Sign in with a device code. Returns a function giving a current token (silently renewed).
    let deviceCode (request: ConnectionRequest) (opts: DeviceCodeOptions) : unit -> AmoToken =
        // Ask the client library which authority and resource this server uses (it knows the
        // national clouds); this contacts the service's name resolution, not the sign-in service.
        let properties =
            use probe =
                new AdomdConnection(ConnectionString.build request.Address XmlaCredential.Integrated None)

            probe.GetCloudConnectionAuthenticationProperties()

        let authority =
            match opts.TenantId with
            | Some tenant ->
                let uri = Uri properties.Authority
                $"{uri.Scheme}://{uri.Authority}/{tenant}"
            | None -> properties.Authority

        let scopes = [| properties.ResourceId.TrimEnd('/') + "/.default" |]

        let app =
            PublicClientApplicationBuilder
                .Create(opts.ClientId |> Option.defaultValue analysisServicesClientId)
                .WithAuthority(authority)
                .Build()

        match opts.CachePath with
        | Some path ->
            app.UserTokenCache.SetBeforeAccess(fun args ->
                match TokenCache.read path with
                | Some data -> args.TokenCache.DeserializeMsalV3(data, true)
                | None -> ())

            app.UserTokenCache.SetAfterAccess(fun args ->
                if args.HasStateChanged then
                    TokenCache.write path (args.TokenCache.SerializeMsalV3()))
        | None -> ()

        let wait (task: Task<'T>) = task.GetAwaiter().GetResult()

        let acquire () =
            let account = app.GetAccountsAsync() |> wait |> Seq.tryHead

            let silent =
                match account with
                | Some a ->
                    try
                        Some(app.AcquireTokenSilent(scopes, a).ExecuteAsync() |> wait)
                    with :? MsalUiRequiredException ->
                        None
                | None -> None

            let result =
                match silent with
                | Some r -> r
                | None ->
                    app
                        .AcquireTokenWithDeviceCode(
                            scopes,
                            fun (code: DeviceCodeResult) ->
                                request.Notify code.Message
                                Task.CompletedTask
                        )
                        .ExecuteAsync()
                    |> wait

            AmoToken(result.AccessToken, result.ExpiresOn)

        acquire

    let compatibilityLevels (text: string) =
        if String.IsNullOrWhiteSpace text then
            []
        else
            text.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.choose (fun s ->
                match Int32.TryParse s with
                | true, n -> Some n
                | _ -> None)
            |> List.ofArray

type private LiveSession(request: ConnectionRequest) =
    // None for credentials that live in the connection string (or nowhere).
    let token: (unit -> AmoToken) option =
        match request.Credential with
        | XmlaCredential.AccessToken t ->
            let fixedToken = Live.staticToken t
            Some(fun () -> fixedToken)
        | XmlaCredential.DeviceCode opts -> Some(Live.deviceCode request opts)
        | XmlaCredential.ServicePrincipal _
        | XmlaCredential.Basic _
        | XmlaCredential.Integrated -> None

    let renews =
        match request.Credential with
        | XmlaCredential.DeviceCode _ -> true
        | _ -> false

    let server =
        let s = new AmoServer()

        try
            match token with
            | Some get ->
                s.AccessToken <- get ()

                if renews then
                    s.OnAccessTokenExpired <- Func<AmoToken, AmoToken>(fun _ -> get ())
            | None -> ()

            // No Initial Catalog: the whole server (workspace) is visible, so a wrong database
            // name can be answered with the ones that exist.
            s.Connect(ConnectionString.build { request.Address with Database = None } request.Credential None)
            s
        with _ ->
            s.Dispose()
            reraise ()

    interface IXmlaSession with
        member _.Server = {
            Version = server.Version
            ProductName = server.ProductName
            CompatibilityMode = Some server.CompatibilityMode
            SupportedCompatibilityLevels = Live.compatibilityLevels server.SupportedCompatibilityLevels
        }

        member _.Databases() = [
            for db in server.Databases ->
                {
                    Name = db.Name
                    Id = db.ID
                    CompatibilityLevel = db.CompatibilityLevel
                }
        ]

        member _.ReadDatabase(name: string) =
            match server.Databases.FindByName name with
            | null -> failwith $"No database '{name}' on the server."
            | db ->
                if isNull db.Model then
                    failwith
                        $"'{name}' is not a tabular model tess can read (compatibility level {db.CompatibilityLevel}; 1200 or higher is needed)."

                ResultReader.detach db

        member _.Execute(script: string) = [
            for result in server.Execute script |> Seq.cast<Microsoft.AnalysisServices.XmlaResult> do
                for message in result.Messages |> Seq.cast<Microsoft.AnalysisServices.XmlaMessage> ->
                    {
                        IsError = (message :? Microsoft.AnalysisServices.XmlaError)
                        Text = message.Description
                    }
        ]

        member _.Query(database: string, dax: string, maxRows: int option) =
            use connection =
                new AdomdConnection(ConnectionString.build request.Address request.Credential (Some database))

            match token with
            | Some get ->
                connection.AccessToken <- get ()

                if renews then
                    connection.OnAccessTokenExpired <- Func<AmoToken, AmoToken>(fun _ -> get ())
            | None -> ()

            connection.Open()

            try
                use command = connection.CreateCommand()
                command.CommandText <- dax
                use reader = command.ExecuteReader()
                ResultReader.readAll reader maxRows
            with :? AdomdErrorResponseException as e ->
                raise (XmlaQueryError e.Message)

    interface IDisposable with
        member _.Dispose() =
            try
                if server.Connected then
                    server.Disconnect()
            finally
                server.Dispose()

/// Connects for real. The only IXmlaConnector that touches the network.
type LiveConnector() =
    interface IXmlaConnector with
        member _.Open(request: ConnectionRequest) =
            new LiveSession(request) :> IXmlaSession
