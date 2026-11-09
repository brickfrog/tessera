/// Turning what a client library throws into errors a person can act on. The texts matched here
/// are Microsoft's and may change; anything unrecognised is still reported, just without a hint.
module Tessera.Xmla.Failures

open System
open System.Text.RegularExpressions
open Tessera

/// The messages of an exception and its causes, outermost first: one line each, without repeats,
/// and without the "Technical Details" (activity IDs, timestamps) the service appends.
let messages (e: exn) : string list =
    let rec chain (e: exn) =
        if isNull e then
            []
        else
            match e with
            | :? AggregateException as a when a.InnerExceptions.Count > 0 ->
                a.InnerExceptions |> Seq.collect chain |> List.ofSeq
            | _ -> e.Message :: chain e.InnerException

    let tidy (m: string) =
        let m = if isNull m then "" else m

        let cut =
            match m.IndexOf("Technical Details", StringComparison.OrdinalIgnoreCase) with
            | -1 -> m
            | i -> m.Substring(0, i)

        // AADSTS messages carry trace and correlation IDs and a timestamp after the sentence.
        let cut =
            match cut.IndexOf(" Trace ID:", StringComparison.OrdinalIgnoreCase) with
            | -1 -> cut
            | i -> cut.Substring(0, i)

        cut.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        // The service's "Description:" line is often empty.
        |> Array.filter (fun l -> l <> "" && l <> "Description:")
        |> String.concat " "

    chain e |> List.map tidy |> List.filter (fun m -> m <> "") |> List.distinct

let private has (text: string) (part: string) =
    text.Contains(part, StringComparison.OrdinalIgnoreCase)

let private aadsts = Regex(@"AADSTS(\d+)", RegexOptions.CultureInvariant)

let private aadHint (code: string) =
    match code with
    | "90002" -> Some "The tenant wasn't found: check TESS_XMLA_TENANT_ID (a tenant ID or domain)."
    | "700016" -> Some "The application isn't in this tenant: check TESS_XMLA_CLIENT_ID and TESS_XMLA_TENANT_ID."
    | "7000215" -> Some "The client secret is wrong: use the secret's value (not its ID) in TESS_XMLA_CLIENT_SECRET."
    | "7000222" -> Some "The client secret has expired: create a new one for the application."
    | "50034" -> Some "The user doesn't exist in this tenant."
    | "50076"
    | "50079"
    | "50158" ->
        Some
            "Multi-factor authentication is required: sign in with TESS_XMLA_AUTH=device-code, or use a service principal."
    | "65001" -> Some "The application hasn't been granted consent in this tenant."
    | "500011" -> Some "The token's resource isn't known in this tenant: is this the right cloud?"
    | "70016"
    | "70020" -> Some "The device code wasn't entered in time: run the command again."
    | _ -> None

let private windowsOnly =
    "SSAS over TCP (host, host:port, host\\instance, and Power BI Desktop's localhost:port) only works on "
    + "Windows: Microsoft's client library has no TCP transport on other systems. Use an HTTP endpoint "
    + "(https://host/olap/msmdpump.dll), powerbi:// or asazure://, or run tess on Windows."

/// Why SSAS over TCP can't be used here.
let unsupportedTcp (server: string) =
    UnsupportedConnection(server, windowsOnly)

/// A failure to connect or authenticate, as a TessError. `secrets` are removed from every text.
let classify (address: XmlaAddress) (secrets: string list) (e: exn) : TessError =
    let server = XmlaAddress.server address
    let lines = messages e |> List.map (Secrets.scrub secrets)
    // "Outer message: inner message." rather than "Outer message.: inner message."
    let text =
        match List.rev lines with
        | [] -> ""
        | last :: outer ->
            (outer |> List.rev |> List.map (fun l -> l.TrimEnd('.', ':', ' '))) @ [ last ]
            |> String.concat ": "

    let reason = if text = "" then e.GetType().Name else text
    let has = has text

    let rec notSupported (e: exn) =
        not (isNull e)
        && ((e :? NotSupportedException) || notSupported e.InnerException)

    let status (code: string) (name: string) = has $"({code})" || has name

    if notSupported e && has "only on Windows" then
        if address.Endpoint = XmlaEndpoint.Tcp then
            unsupportedTcp server
        else
            UnsupportedConnection(
                server,
                reason
                + " (on this system an HTTP server needs a user name and password: TESS_XMLA_USER and TESS_XMLA_PASSWORD)"
            )
    elif aadsts.IsMatch text then
        AuthenticationFailed(server, reason, aadHint (aadsts.Match text).Groups.[1].Value)
    elif has "Unable to obtain authentication token" || has "authentication failed" then
        AuthenticationFailed(server, reason, None)
    elif has "access-token is required" || has "access token is required" then
        AuthenticationRequired(server, "the client library needs a token or a service principal for this server")
    elif has "Resolving the PBI workspace" && status "403" "Forbidden" then
        ConnectionFailed(
            server,
            reason,
            Some(
                "Power BI refused access to the workspace. Check the workspace name; that the identity is a "
                + "Contributor, Member or Admin of it; and, for a service principal, that the tenant setting "
                + "\"Service principals can use Fabric APIs\" allows it."
            )
        )
    elif has "Resolving the PBI workspace" && status "404" "Not Found" then
        ConnectionFailed(server, reason, Some "No such workspace: check its name (and the tenant in the URL).")
    elif
        has "XMLA"
        && (has "read/write"
            || has "readwrite"
            || has "read-only"
            || has "readonly"
            || has "read only")
    then
        ConnectionFailed(
            server,
            reason,
            Some
                "The capacity's XMLA endpoint is read-only or off: an admin sets \"XMLA Endpoint\" to \"Read Write\" in the capacity settings (Power BI workloads)."
        )
    elif
        (has "Premium" || has "dedicated capacity" || has "capacity")
        && (has "workspace" || has "XMLA")
    then
        ConnectionFailed(
            server,
            reason,
            Some
                "XMLA endpoints need the workspace to be on a Premium, Premium Per User, Embedded or Fabric capacity, with the XMLA endpoint enabled."
        )
    elif status "401" "Unauthorized" then
        AuthenticationFailed(
            server,
            reason,
            Some
                "The server refused the credentials. For a token: is it still valid, and issued for this server's resource (Power BI: https://analysis.windows.net/powerbi/api)?"
        )
    elif status "403" "Forbidden" then
        ConnectionFailed(server, reason, Some "The identity is known but not allowed to do this on the server.")
    elif address.Endpoint = XmlaEndpoint.AzureAS && has "is not found" then
        ConnectionFailed(server, reason, Some "No such Azure Analysis Services server: check the region and name.")
    elif
        has "No such host"
        || has "Name or service not known"
        || has "nodename nor servname"
        || has "Connection refused"
        || has "actively refused"
        || has "timed out"
        || has "A connection cannot be made"
    then
        ConnectionFailed(server, reason, Some "Check the server address and that it can be reached from here.")
    else
        ConnectionFailed(server, reason, None)
