/// Building the connection string the client libraries take. Pure text. The result can hold a
/// secret (a service principal's, or a password): it is handed to the client library and nowhere
/// else: never logged, printed or put in an error.
module Tessera.Xmla.ConnectionString

open Tessera

/// A value, quoted when it has to be (`;`, quotes, or surrounding spaces).
let quote (value: string) =
    if
        value.Contains ';'
        || value.Contains '"'
        || value.Contains '''
        || value.Contains '='
        || value <> value.Trim()
        || value = ""
    then
        "\"" + value.Replace("\"", "\"\"") + "\""
    else
        value

/// The connection string for an address and credential; `catalog` overrides the address's database.
/// A token or device-code credential adds nothing here: the token goes in separately.
let build (address: XmlaAddress) (credential: XmlaCredential) (catalog: string option) : string =
    let pairs = [
        "Data Source", address.DataSource

        match catalog |> Option.orElse address.Database with
        | Some c -> "Initial Catalog", c
        | None -> ()

        yield! address.Properties

        match credential with
        | XmlaCredential.ServicePrincipal(clientId, tenantId, secret) ->
            "User ID", $"app:{clientId}@{tenantId}"
            "Password", secret
        | XmlaCredential.Basic(user, password) ->
            "User ID", user
            "Password", password
        | XmlaCredential.AccessToken _
        | XmlaCredential.DeviceCode _
        | XmlaCredential.Integrated -> ()
    ]

    pairs |> List.map (fun (k, v) -> $"{k}={quote v}") |> String.concat ";"
