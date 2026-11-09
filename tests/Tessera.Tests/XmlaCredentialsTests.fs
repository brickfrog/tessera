module Tessera.Tests.XmlaCredentialsTests

open System
open System.IO
open Expecto
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures
open Tessera.Tests.XmlaFakes

module Var = Credentials.Var

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private address (text: string) = XmlaAddress.parseServer text |> ok
let private cloud = address workspace
let private ssas = address "ssas01;Db"

/// An environment of just these variables; secret files are an in-memory map.
let private env (vars: (string * string) list) (files: (string * string) list) : Credentials.Environment =
    let vars = Map.ofList vars
    let files = Map.ofList files

    {
        GetEnv = fun name -> Map.tryFind name vars
        ReadSecret =
            fun path ->
                match Map.tryFind path files with
                | Some text -> Ok text
                | None -> Error $"can't read {path}: no such file"
    }

let private resolve vars files a = Credentials.resolve (env vars files) a

let private code (r: Result<'a, TessError>) =
    match r with
    | Ok _ -> "ok"
    | Error e -> TessError.code e

let private spn = [
    Var.clientId, "app-1"
    Var.tenantId, "tenant-1"
    Var.clientSecret, "s3cr3t-value"
]

[<Tests>]
let credentialTests =
    testList "Xmla credentials" [
        testCase "auto, cloud: a token wins, then a service principal, then the AZURE_ names"
        <| fun () ->
            Expect.equal (resolve ((Var.token, " tok ") :: spn) [] cloud) (Ok(XmlaCredential.AccessToken "tok")) ""

            Expect.equal
                (resolve spn [] cloud)
                (Ok(XmlaCredential.ServicePrincipal("app-1", "tenant-1", "s3cr3t-value")))
                ""

            let azure = [
                Var.azureClientId, "az-app"
                Var.azureTenantId, "az-tenant"
                Var.azureClientSecret, "az-secret"
            ]

            Expect.equal
                (resolve azure [] cloud)
                (Ok(XmlaCredential.ServicePrincipal("az-app", "az-tenant", "az-secret")))
                ""

            Expect.equal
                (resolve (spn @ azure) [] cloud)
                (Ok(XmlaCredential.ServicePrincipal("app-1", "tenant-1", "s3cr3t-value")))
                "tess's own names come first"

        testCase "auto, cloud, nothing configured: an error that says how, and nothing is tried"
        <| fun () ->
            match resolve [] [] cloud with
            | Error(AuthenticationRequired(server, hint)) ->
                Expect.equal server workspace ""
                Expect.stringContains hint Var.token ""
                Expect.stringContains hint Var.clientSecret ""
                Expect.stringContains hint "device-code" ""
            | other -> failtestf "%A" other

            // A client and tenant ID without a secret aren't a service principal.
            Expect.equal (code (resolve [ Var.clientId, "a"; Var.tenantId, "t" ] [] cloud)) "authentication-required" ""

        testCase "secrets can come from files, but not from both a variable and a file"
        <| fun () ->
            Expect.equal
                (resolve [ Var.tokenFile, "/run/secrets/token" ] [ "/run/secrets/token", "file-token" ] cloud)
                (Ok(XmlaCredential.AccessToken "file-token"))
                ""

            Expect.equal
                (resolve
                    [ Var.clientId, "a"; Var.tenantId, "t"; Var.clientSecretFile, "sec" ]
                    [ "sec", "from-file" ]
                    cloud)
                (Ok(XmlaCredential.ServicePrincipal("a", "t", "from-file")))
                ""

            Expect.equal
                (resolve [ Var.tokenFile, "-" ] [ "-", "stdin-token" ] cloud)
                (Ok(XmlaCredential.AccessToken "stdin-token"))
                ""

            Expect.equal
                (code (resolve [ Var.token, "a"; Var.tokenFile, "f" ] [ "f", "b" ] cloud))
                "invalid-credentials"
                ""

            match resolve [ Var.tokenFile, "/missing" ] [] cloud with
            | Error(InvalidCredentials reason) -> Expect.stringContains reason "/missing" ""
            | other -> failtestf "%A" other

        testCase "explicit modes say exactly what is missing"
        <| fun () ->
            let mode m = [ Var.auth, m ]
            Expect.equal (code (resolve (mode "token") [] cloud)) "invalid-credentials" ""
            Expect.equal (code (resolve (mode "spn") [] cloud)) "invalid-credentials" ""

            Expect.equal
                (code (resolve (mode "spn" @ [ Var.clientId, "a"; Var.tenantId, "t" ]) [] cloud))
                "invalid-credentials"
                ""

            Expect.equal (code (resolve (mode "basic") [] ssas)) "invalid-credentials" ""
            Expect.equal (code (resolve (mode "kerberos") [] ssas)) "invalid-credentials" ""
            Expect.equal (resolve (mode "SPN" @ spn) [] cloud |> code) "ok" "the mode is case-insensitive"

            Expect.equal
                (resolve (mode "device-code" @ [ Var.tenantId, "contoso.com"; Var.tokenCache, "/c/cache" ]) [] cloud)
                (Ok(
                    XmlaCredential.DeviceCode {
                        TenantId = Some "contoso.com"
                        ClientId = None
                        CachePath = Some "/c/cache"
                    }
                ))
                ""

            Expect.equal
                (resolve (mode "device-code") [] cloud)
                (Ok(
                    XmlaCredential.DeviceCode {
                        TenantId = None
                        ClientId = None
                        CachePath = None
                    }
                ))
                "no cache file unless asked for"

        testCase "SSAS: integrated unless a user and password are given; cloud sign-ins are refused"
        <| fun () ->
            Expect.equal (resolve [] [] ssas) (Ok XmlaCredential.Integrated) ""

            Expect.equal
                (resolve [ Var.user, "DOMAIN\\svc"; Var.password, "pw" ] [] ssas)
                (Ok(XmlaCredential.Basic("DOMAIN\\svc", "pw")))
                ""
            // A token in the environment is not sent to a server it wasn't issued for.
            Expect.equal (resolve [ Var.token, "tok" ] [] ssas) (Ok XmlaCredential.Integrated) ""

            for m in [ "token"; "spn"; "device-code" ] do
                Expect.equal (code (resolve ((Var.auth, m) :: (Var.token, "t") :: spn) [] ssas)) "invalid-credentials" m

            Expect.equal
                (code (resolve [ Var.auth, "integrated" ] [] cloud))
                "invalid-credentials"
                "no interactive fallback"

        testCase "describe never shows a secret; secrets lists them"
        <| fun () ->
            let all = [
                XmlaCredential.AccessToken "tok-secret"
                XmlaCredential.ServicePrincipal("app-1", "tenant-1", "spn-secret")
                XmlaCredential.Basic("user", "pw-secret")
                XmlaCredential.DeviceCode {
                    TenantId = None
                    ClientId = None
                    CachePath = None
                }
                XmlaCredential.Integrated
            ]

            for c in all do
                Expect.isFalse ((XmlaCredential.describe c).Contains "secret") ""

            Expect.equal (all |> List.collect XmlaCredential.secrets) [ "tok-secret"; "spn-secret"; "pw-secret" ] ""
            Expect.stringContains (XmlaCredential.describe all.[1]) "app-1" ""

        testCase "readSecret trims the line break, reads stdin for -, and reports unreadable or empty files"
        <| fun () ->
            use t = new TempDir()
            let file = Path.Combine(t.Path, "secret")
            File.WriteAllText(file, "value with spaces \r\n")
            Expect.equal (Credentials.readSecret TextReader.Null file) (Ok "value with spaces ") ""
            Expect.equal (Credentials.readSecret (new StringReader "piped\n") "-") (Ok "piped") ""
            Expect.isError (Credentials.readSecret TextReader.Null "-") "empty stdin"
            Expect.isError (Credentials.readSecret TextReader.Null (Path.Combine(t.Path, "nope"))) ""
            File.WriteAllText(file, "\n")
            Expect.isError (Credentials.readSecret TextReader.Null file) "empty file"

        testCase "a token's expiry is read from its exp claim"
        <| fun () ->
            Expect.equal
                (Credentials.tokenExpiry fakeToken)
                (Some(DateTimeOffset(2100, 1, 1, 0, 0, 0, TimeSpan.Zero)))
                ""

            Expect.equal (Credentials.tokenExpiry "not-a-jwt") None ""
            Expect.equal (Credentials.tokenExpiry "a.b.c") None ""
            Expect.equal (Credentials.tokenExpiry "") None ""
    ]

[<Tests>]
let secretTests =
    testList "Xmla secrets" [
        testCase "known secrets are removed wherever they appear, longest first"
        <| fun () ->
            Expect.equal
                (Secrets.scrub [ "abc"; "abc-123-long" ] "failed with abc-123-long and abc.")
                "failed with *** and ***."
                ""

            Expect.equal (Secrets.scrub [ "" ] "unchanged") "unchanged" ""
            Expect.equal (Secrets.scrub [] null) "" ""

        testCase "anything shaped like a secret goes too, even if it isn't one we hold"
        <| fun () ->
            let cases = [
                "Data Source=x;Password=hunter2;Timeout=5", "Data Source=x;Password=***;Timeout=5"
                "data source=x;PWD = \"semi;colon\";a=b", "data source=x;PWD=***;a=b"
                "User ID=app:a@b;password='q''uoted'", "User ID=app:a@b;password=***"
                "Authorization: Bearer abc.def-123", "Authorization: Bearer ***"
                $"token {fakeToken} rejected", "token *** rejected"
            ]

            for input, expected in cases do
                Expect.equal (Secrets.scrub [] input) expected input

        testCase "the connection string holds the secret properly quoted, and nothing else does"
        <| fun () ->
            let a =
                address "Data Source=https://host/olap/msmdpump.dll;Initial Catalog=Db;Timeout=30"

            let secret = "p;w=\"x\" "
            let cs = ConnectionString.build a (XmlaCredential.Basic("u", secret)) None

            Expect.equal
                cs
                "Data Source=https://host/olap/msmdpump.dll;Initial Catalog=Db;Timeout=30;User ID=u;Password=\"p;w=\"\"x\"\" \""
                ""

            // What the builder writes, the address parser's splitter reads back: the quoting is sound.
            Expect.isFalse ((Secrets.scrub [ secret ] cs).Contains "p;w") ""
            Expect.isFalse ((Secrets.scrub [] cs).Contains "p;w") "by shape alone, too"

            let spn =
                ConnectionString.build
                    cloud
                    (XmlaCredential.ServicePrincipal("app-1", "tenant-1", "shh"))
                    (Some "My Db")

            Expect.equal spn $"Data Source={workspace};Initial Catalog=My Db;User ID=app:app-1@tenant-1;Password=shh" ""

            for c in
                [
                    XmlaCredential.AccessToken fakeToken
                    XmlaCredential.Integrated
                    XmlaCredential.DeviceCode {
                        TenantId = None
                        ClientId = None
                        CachePath = None
                    }
                ] do
                Expect.equal
                    (ConnectionString.build cloud c None)
                    $"Data Source={workspace}"
                    "tokens never go in the string"

        testCase "the token cache file is written whole, and owner-only on Unix"
        <| fun () ->
            use t = new TempDir()
            let path = Path.Combine(t.Path, "nested", "cache.bin")
            Expect.equal (TokenCache.read path) None ""
            TokenCache.write path [| 1uy; 2uy; 3uy |]
            TokenCache.write path [| 9uy |]
            Expect.equal (TokenCache.read path) (Some [| 9uy |]) ""
            Expect.isFalse (File.Exists(path + ".tmp")) ""

            if not (OperatingSystem.IsWindows()) then
                Expect.equal (File.GetUnixFileMode path) (UnixFileMode.UserRead ||| UnixFileMode.UserWrite) "0600"
    ]

/// Messages as the client libraries produce them (the first two copied from real responses to
/// made-up credentials; the rest are the shapes documented by Microsoft).
[<Tests>]
let failureTests =
    let classify a (e: exn) =
        Failures.classify a [ "s3cr3t-value" ] e

    testList "Xmla failures" [
        testCase "wrong tenant: authentication failed, with the tenant hint and no trace IDs"
        <| fun () ->
            let inner =
                exn
                    "AADSTS90002: Tenant '00000000-0000-0000-0000-000000000002' not found. Check to make sure you have the correct tenant ID and are signing into the correct cloud. Trace ID: adf4be81 Correlation ID: 76c495ad Timestamp: 2026-11-09 22:16:43Z"

            match
                classify cloud (exn ("Unable to obtain authentication token using the credentials provided.", inner))
            with
            | AuthenticationFailed(server, reason, Some hint) ->
                Expect.equal server workspace ""
                Expect.stringContains reason "AADSTS90002" ""
                Expect.isFalse (reason.Contains "Trace ID") ""
                Expect.stringContains hint Var.tenantId ""
            | other -> failtestf "%A" other

        testCase "bad client secret: the secret itself never comes back out"
        <| fun () ->
            let e =
                exn "AADSTS7000215: Invalid client secret provided. Password=s3cr3t-value was sent for app 'app-1'."

            match classify cloud e with
            | AuthenticationFailed(_, reason, Some hint) as error ->
                Expect.isFalse (reason.Contains "s3cr3t") reason
                Expect.isFalse ((TessError.message error).Contains "s3cr3t") ""
                Expect.stringContains hint Var.clientSecret ""
            | other -> failtestf "%A" other

        testCase "workspace refused (403) and not found (404)"
        <| fun () ->
            let pbi (status: string) =
                exn (
                    "PowerBI Request Failed.\n\nAction: Resolving the PBI workspace\n\nDescription: \n\nTechnical Details:\nRootActivityId: ea3139c2-68e1-4e56-bb1b-a5d1b46614bc",
                    exn $"The remote server returned an error: {status}."
                )

            match classify cloud (pbi "(403) Forbidden") with
            | ConnectionFailed(_, reason, Some hint) ->
                Expect.stringContains hint "Service principals" ""
                Expect.isFalse (reason.Contains "RootActivityId") reason
                Expect.isFalse (reason.Contains "\n") "one line"
            | other -> failtestf "%A" other

            match classify cloud (pbi "(404) Not Found") with
            | ConnectionFailed(_, _, Some hint) -> Expect.stringContains hint "workspace" ""
            | other -> failtestf "%A" other

        testCase "XMLA endpoint read-only, and workspace not on a capacity"
        <| fun () ->
            match
                classify cloud (exn "The operation is only supported when the XMLA endpoint is read/write enabled.")
            with
            | ConnectionFailed(_, _, Some hint) -> Expect.stringContains hint "Read Write" ""
            | other -> failtestf "%A" other

            match classify cloud (exn "The workspace is not hosted on a Premium capacity.") with
            | ConnectionFailed(_, _, Some hint) -> Expect.stringContains hint "Premium" ""
            | other -> failtestf "%A" other

        testCase "expired or wrong-audience token (401)"
        <| fun () ->
            match
                classify
                    cloud
                    (exn ("The connection failed.", exn "The remote server returned an error: (401) Unauthorized."))
            with
            | AuthenticationFailed(_, _, Some hint) -> Expect.stringContains hint "analysis.windows.net" ""
            | other -> failtestf "%A" other

        testCase "TCP outside Windows, and HTTP without a password outside Windows"
        <| fun () ->
            let windowsOnly =
                NotSupportedException "This feature is supported for a .NET Core client only on Windows systems."

            match classify ssas windowsOnly with
            | UnsupportedConnection(server, reason) ->
                Expect.equal server "ssas01" ""
                Expect.stringContains reason "only works on Windows" ""
            | other -> failtestf "%A" other

            match classify (address "https://host/olap/msmdpump.dll") windowsOnly with
            | UnsupportedConnection(_, reason) -> Expect.stringContains reason Var.password ""
            | other -> failtestf "%A" other

        testCase "Azure AS server not found; unreachable host; anything else is still reported"
        <| fun () ->
            let aas = address "asazure://westeurope.asazure.windows.net/nosuch"

            match
                classify aas (exn "Server 'nosuch' is not found.\n\nTechnical Details:\nRootActivityId: 594ee75e")
            with
            | ConnectionFailed(_, reason, Some hint) ->
                Expect.equal reason "Server 'nosuch' is not found." ""
                Expect.stringContains hint "region" ""
            | other -> failtestf "%A" other

            match
                classify
                    cloud
                    (exn ("A connection cannot be made. Ensure that the server is running.", exn "Connection refused"))
            with
            | ConnectionFailed(_, _, Some _) -> ()
            | other -> failtestf "%A" other

            match classify cloud (AggregateException(exn "something new", exn "something new")) with
            | ConnectionFailed(_, "something new", None) -> ()
            | other -> failtestf "%A" other

            match classify cloud (exn "") with
            | ConnectionFailed(_, "Exception", None) -> ()
            | other -> failtestf "%A" other
    ]
