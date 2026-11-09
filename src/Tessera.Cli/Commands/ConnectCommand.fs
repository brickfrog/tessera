/// `tess connect`: check that a server can be reached with the configured credentials, and list
/// its databases. It keeps nothing: every command that takes an `xmla:` model connects by itself.
module Tessera.Cli.ConnectCommand

open System.IO
open Argu
open Tessera
open Tessera.Xmla
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "check a connection to a server (powerbi://, asazure://, SSAS) and list its databases."

type Args =
    | [<MainCommand; ExactlyOnce>] Server of server: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Server _ ->
                "the server: powerbi://api.powerbi.com/v1.0/myorg/<workspace>, asazure://<region>.asazure.windows.net/<server>, "
                + "https://<host>/olap/msmdpump.dll, or (Windows only) an SSAS host, host:port or host\\instance; "
                + "optionally followed by ;<database>. The `xmla:` prefix is optional here. Credentials come from the "
                + "environment, never the command line: TESS_XMLA_TOKEN (or TESS_XMLA_TOKEN_FILE) for an access token; "
                + "TESS_XMLA_CLIENT_ID, TESS_XMLA_TENANT_ID and TESS_XMLA_CLIENT_SECRET (or ..._SECRET_FILE) for a "
                + "service principal; TESS_XMLA_AUTH=device-code to sign in as a user; TESS_XMLA_USER and "
                + "TESS_XMLA_PASSWORD (or ..._PASSWORD_FILE) for a user name and password."

type private Report = {
    Address: XmlaAddress
    Credential: string
    Server: ServerProperties
    Databases: DatabaseSummary list
}

let private reportJson (r: Report) =
    obj [
        "server", str r.Address.DataSource
        "database", optStr r.Address.Database
        "authentication", str r.Credential
        "product", str r.Server.ProductName
        "version", str r.Server.Version
        "compatibilityMode",
        (match r.Server.CompatibilityMode with
         | Some m -> str (string m)
         | None -> null)
        "databases",
        arr [
            for d in r.Databases ->
                obj [
                    "name", str d.Name
                    "id", str d.Id
                    "compatibilityLevel", num d.CompatibilityLevel
                    "source", str (XmlaAddress.display (XmlaAddress.withDatabase d.Name r.Address))
                ]
        ]
    ]

let private reportText (out: TextWriter) (r: Report) =
    let row (label: string) (value: string) = out.WriteLine($"{label, -15} {value}")
    row "Server" r.Address.DataSource
    row "Signed in with" r.Credential

    row
        "Product"
        (match r.Server.ProductName, r.Server.Version with
         | "", v -> v
         | p, v -> $"{p} {v}")

    r.Server.CompatibilityMode |> Option.iter (fun m -> row "Mode" (string m))
    out.WriteLine()

    match r.Databases with
    | [] -> out.WriteLine("No databases (that these credentials can see).")
    | databases ->
        let width = databases |> List.map (fun d -> d.Name.Length) |> List.fold max 0

        for d in databases do
            out.WriteLine($"{d.Name.PadRight width}  {d.CompatibilityLevel}")

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let access = Source.access ctx

    let result =
        XmlaAddress.parseServer (args.GetResult Args.Server)
        |> Result.bind (fun address ->
            Remote.withConnection access address (fun c ->
                Remote.serverProperties c
                |> Result.bind (fun server ->
                    Remote.databases c
                    |> Result.bind (fun databases ->
                        // A database in the address must exist.
                        let known =
                            match address.Database with
                            | Some _ -> Remote.resolveDatabase c |> Result.map ignore
                            | None -> Ok()

                        known
                        |> Result.bind (fun () ->
                            Credentials.resolve access.Environment address
                            |> Result.map (fun credential -> {
                                Address = address
                                Credential = XmlaCredential.describe credential
                                Server = server
                                Databases = databases
                            }))))))

    match result with
    | Error e -> Failed e
    | Ok report ->
        if ctx.Json then
            writeJson ctx.Out (reportJson report)
        else
            reportText ctx.Out report

        Done
