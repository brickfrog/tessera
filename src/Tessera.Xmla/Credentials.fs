namespace Tessera.Xmla

open System
open System.IO
open System.Text
open System.Text.Json
open Tessera

/// Device-code sign-in settings.
type DeviceCodeOptions = {
    /// The Microsoft Entra tenant (ID or domain); None lets the address or the user's home tenant decide.
    TenantId: string option
    /// The public client application to sign in through; None uses the Analysis Services client
    /// libraries' own application.
    ClientId: string option
    /// Where to keep the sign-in between runs; None keeps it in memory only.
    CachePath: string option
}

/// How to authenticate. Values of this type hold secrets: never print one (use `describe`).
[<RequireQualifiedAccess>]
type XmlaCredential =
    /// A Microsoft Entra access token obtained elsewhere (e.g. `az account get-access-token`).
    | AccessToken of token: string
    | ServicePrincipal of clientId: string * tenantId: string * secret: string
    /// A user name and password (SSAS over HTTP, or a Windows account for SSAS).
    | Basic of user: string * password: string
    /// Sign in as a user by entering a code in a browser on any device.
    | DeviceCode of DeviceCodeOptions
    /// No credentials from tess: the current Windows identity (or anonymous HTTP).
    | Integrated

module XmlaCredential =
    /// What may be shown about a credential.
    let describe =
        function
        | XmlaCredential.AccessToken _ -> "access token"
        | XmlaCredential.ServicePrincipal(clientId, tenantId, _) -> $"service principal {clientId} in tenant {tenantId}"
        | XmlaCredential.Basic(user, _) -> $"user {user}"
        | XmlaCredential.DeviceCode _ -> "device code sign-in"
        | XmlaCredential.Integrated -> "integrated (no credentials from tess)"

    /// The secret parts, for scrubbing messages.
    let secrets =
        function
        | XmlaCredential.AccessToken token -> [ token ]
        | XmlaCredential.ServicePrincipal(_, _, secret) -> [ secret ]
        | XmlaCredential.Basic(_, password) -> [ password ]
        | XmlaCredential.DeviceCode _
        | XmlaCredential.Integrated -> []

/// Where credentials come from: environment variables, and files or standard input they point at.
/// Never the command line (it shows in `ps` and in shell history).
module Credentials =

    module Var =
        /// auto (default), token, spn, device-code, basic, integrated.
        let auth = "TESS_XMLA_AUTH"
        let token = "TESS_XMLA_TOKEN"
        /// A file holding the token, or `-` for standard input.
        let tokenFile = "TESS_XMLA_TOKEN_FILE"
        let clientId = "TESS_XMLA_CLIENT_ID"
        let tenantId = "TESS_XMLA_TENANT_ID"
        let clientSecret = "TESS_XMLA_CLIENT_SECRET"
        let clientSecretFile = "TESS_XMLA_CLIENT_SECRET_FILE"
        let user = "TESS_XMLA_USER"
        let password = "TESS_XMLA_PASSWORD"
        let passwordFile = "TESS_XMLA_PASSWORD_FILE"
        /// Opt-in: a file to keep a device-code sign-in in between runs.
        let tokenCache = "TESS_XMLA_TOKEN_CACHE"
        // The Azure SDK's conventional names, honoured for a service principal when the
        // TESS_XMLA_ ones aren't set (CI systems often provide them).
        let azureClientId = "AZURE_CLIENT_ID"
        let azureTenantId = "AZURE_TENANT_ID"
        let azureClientSecret = "AZURE_CLIENT_SECRET"

        /// Every variable tess reads for credentials (tests blank them all).
        let all = [
            auth
            token
            tokenFile
            clientId
            tenantId
            clientSecret
            clientSecretFile
            user
            password
            passwordFile
            tokenCache
            azureClientId
            azureTenantId
            azureClientSecret
        ]

    /// The outside world, as credential resolution sees it.
    type Environment = {
        GetEnv: string -> string option
        /// Read a secret from a file path, or from standard input for `-`.
        ReadSecret: string -> Result<string, string>
    }

    /// Read a secret file (or standard input for `-`): its text without the trailing line break.
    let readSecret (stdin: TextReader) (path: string) : Result<string, string> =
        try
            let text =
                if path = "-" then
                    stdin.ReadToEnd()
                else
                    File.ReadAllText path

            let text = text.TrimEnd('\r', '\n')

            if text = "" then
                Error(
                    if path = "-" then
                        "standard input is empty"
                    else
                        $"{path} is empty"
                )
            else
                Ok text
        with e ->
            Error $"can't read {path}: {e.Message}"

    let processEnvironment (getEnv: string -> string option) = {
        GetEnv = getEnv
        ReadSecret = readSecret Console.In
    }

    /// A value from a variable, or from the file another variable names. Error when the file can't
    /// be read or both are set.
    let private secret (env: Environment) (direct: string) (file: string) : Result<string option, TessError> =
        match env.GetEnv direct, env.GetEnv file with
        | Some _, Some _ -> Error(InvalidCredentials $"set {direct} or {file}, not both")
        | Some v, None -> Ok(Some v)
        | None, Some path ->
            match env.ReadSecret path with
            | Ok v -> Ok(Some v)
            | Error reason -> Error(InvalidCredentials $"{file}: {reason}")
        | None, None -> Ok None

    let private hint =
        "set TESS_XMLA_TOKEN (or TESS_XMLA_TOKEN_FILE) to an access token; or TESS_XMLA_CLIENT_ID, "
        + "TESS_XMLA_TENANT_ID and TESS_XMLA_CLIENT_SECRET (or TESS_XMLA_CLIENT_SECRET_FILE) for a service "
        + "principal; or TESS_XMLA_AUTH=device-code to sign in as a user"

    let private servicePrincipal (env: Environment) (required: bool) : Result<XmlaCredential option, TessError> =
        let own =
            match env.GetEnv Var.clientId, env.GetEnv Var.tenantId with
            | Some c, Some t -> Some(c, t, secret env Var.clientSecret Var.clientSecretFile, Var.clientSecret)
            | _ -> None

        let azure () =
            match env.GetEnv Var.azureClientId, env.GetEnv Var.azureTenantId, env.GetEnv Var.azureClientSecret with
            | Some c, Some t, Some s -> Some(c, t, Ok(Some s), Var.azureClientSecret)
            | _ -> None

        match own |> Option.orElseWith azure with
        | Some(clientId, tenantId, secret, name) ->
            match secret with
            | Error e -> Error e
            | Ok(Some s) -> Ok(Some(XmlaCredential.ServicePrincipal(clientId, tenantId, s)))
            | Ok None when required -> Error(InvalidCredentials $"a service principal needs {name} (or {name}_FILE)")
            // A client and tenant ID alone also configure device-code sign-in; not an error in auto.
            | Ok None -> Ok None
        | None when required ->
            Error(
                InvalidCredentials
                    $"a service principal needs {Var.clientId}, {Var.tenantId} and {Var.clientSecret} (or {Var.clientSecretFile})"
            )
        | None -> Ok None

    let private token (env: Environment) (required: bool) : Result<XmlaCredential option, TessError> =
        match secret env Var.token Var.tokenFile with
        | Error e -> Error e
        | Ok(Some t) -> Ok(Some(XmlaCredential.AccessToken(t.Trim())))
        | Ok None when required -> Error(InvalidCredentials $"an access token needs {Var.token} (or {Var.tokenFile})")
        | Ok None -> Ok None

    let private basic (env: Environment) (required: bool) : Result<XmlaCredential option, TessError> =
        match env.GetEnv Var.user, secret env Var.password Var.passwordFile with
        | _, Error e -> Error e
        | Some u, Ok(Some p) -> Ok(Some(XmlaCredential.Basic(u, p)))
        | _ when required ->
            Error(
                InvalidCredentials
                    $"a user name and password need {Var.user} and {Var.password} (or {Var.passwordFile})"
            )
        | _ -> Ok None

    let private deviceCode (env: Environment) =
        XmlaCredential.DeviceCode {
            TenantId = env.GetEnv Var.tenantId
            ClientId = env.GetEnv Var.clientId
            CachePath = env.GetEnv Var.tokenCache
        }

    /// Work out the credential for an address from the environment. Nothing is contacted.
    let resolve (env: Environment) (address: XmlaAddress) : Result<XmlaCredential, TessError> =
        let cloud = XmlaAddress.isCloud address
        let server = XmlaAddress.server address

        let cloudOnly (mode: string) (next: unit -> Result<XmlaCredential, TessError>) =
            if cloud then
                next ()
            else
                Error(
                    InvalidCredentials
                        $"{Var.auth}={mode} only works with powerbi:// and asazure:// servers, not {server}"
                )

        let require (found: Result<XmlaCredential option, TessError>) = found |> Result.map Option.get

        match (env.GetEnv Var.auth |> Option.defaultValue "auto").Trim().ToLowerInvariant() with
        | "token" -> cloudOnly "token" (fun () -> require (token env true))
        | "spn" -> cloudOnly "spn" (fun () -> require (servicePrincipal env true))
        | "device-code" -> cloudOnly "device-code" (fun () -> Ok(deviceCode env))
        | "basic" -> require (basic env true)
        | "integrated" ->
            if cloud then
                // The client library would fall back to its own interactive sign-in, which tess
                // doesn't ship the parts for.
                Error(
                    InvalidCredentials
                        $"{Var.auth}=integrated only works with SSAS servers; for {server} use a token, a service principal or device-code"
                )
            else
                Ok XmlaCredential.Integrated
        | "auto" ->
            if cloud then
                let firstOf (attempts: (unit -> Result<XmlaCredential option, TessError>) list) =
                    let rec go attempts =
                        match attempts with
                        | [] -> Error(AuthenticationRequired(server, hint))
                        | attempt :: rest ->
                            match attempt () with
                            | Error e -> Error e
                            | Ok(Some c) -> Ok c
                            | Ok None -> go rest

                    go attempts

                firstOf [ (fun () -> token env false); (fun () -> servicePrincipal env false) ]
            else
                basic env false |> Result.map (Option.defaultValue XmlaCredential.Integrated)
        | other ->
            Error(InvalidCredentials $"{Var.auth}={other}: expected auto, token, spn, device-code, basic or integrated")

    /// When a JWT access token expires, read from its `exp` claim; None if it isn't a readable JWT.
    /// (The token is only decoded to find this out: tess never validates or stores it.)
    let tokenExpiry (token: string) : DateTimeOffset option =
        try
            match token.Split '.' with
            | [| _; payload; _ |] ->
                let padded =
                    payload.Replace('-', '+').Replace('_', '/')
                    + String('=', (4 - payload.Length % 4) % 4)

                use doc =
                    JsonDocument.Parse(Encoding.UTF8.GetString(Convert.FromBase64String padded))

                let ok, exp = doc.RootElement.TryGetProperty "exp"

                if ok && exp.ValueKind = JsonValueKind.Number then
                    Some(DateTimeOffset.FromUnixTimeSeconds(exp.GetInt64()))
                else
                    None
            | _ -> None
        with _ ->
            None
