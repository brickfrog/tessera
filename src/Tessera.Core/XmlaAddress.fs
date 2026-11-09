namespace Tessera

open System

/// The kind of endpoint a server address names. It decides the transport and which ways of
/// authenticating make sense.
[<RequireQualifiedAccess>]
type XmlaEndpoint =
    /// powerbi://api.powerbi.com/v1.0/myorg/Workspace (Power BI Premium / Fabric XMLA endpoint).
    | PowerBI
    /// asazure://region.asazure.windows.net/server (Azure Analysis Services).
    | AzureAS
    /// http(s)://host/olap/msmdpump.dll (SSAS behind IIS).
    | Http
    /// host, host:port, host\instance: SSAS over TCP, including Power BI Desktop's localhost:port.
    | Tcp

/// A server (and optionally a database on it) to talk to over XMLA. Parsing is pure text work:
/// nothing here opens a connection. An address never holds credentials.
type XmlaAddress = {
    /// The data source, as the client library takes it.
    DataSource: string
    Endpoint: XmlaEndpoint
    Database: string option
    /// Other connection-string properties the user gave, in order (never credentials).
    Properties: (string * string) list
}

module XmlaAddress =

    /// A model argument starting with this names a remote model: `xmla:<server>[;<database>]`.
    /// An explicit prefix, so that a remote model can never be mistaken for a path (a plain SSAS
    /// address like `localhost:1234` or `host\instance` looks just like one), and no command
    /// opens a connection unless its argument says so.
    let prefix = "xmla:"

    let isRemote (input: string) =
        not (isNull input)
        && input.TrimStart().StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    let private eq (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let private normalizeKey (key: string) =
        key.Replace(" ", "").Replace("_", "").ToLowerInvariant()

    let private dataSourceKeys = [ "datasource"; "dsn" ]
    let private catalogKeys = [ "initialcatalog"; "catalog"; "database" ]

    /// Connection-string keys that carry (or select) credentials. They never come from the
    /// command line: see Tessera.Xmla.Credentials for where credentials do come from.
    let private isCredentialKey (key: string) =
        let k = normalizeKey key

        [ "password"; "pwd"; "token"; "secret"; "userid"; "uid" ]
        |> List.exists (fun bad -> k = bad || ((bad.Length > 3) && k.Contains bad))

    /// Split a connection string into key/value pairs. Values may be quoted with " or ' (doubled
    /// to escape), as in OLE DB connection strings.
    let private splitConnectionString (text: string) : Result<(string * string) list, string> =
        let pairs = ResizeArray<string * string>()
        let mutable i = 0
        let mutable error = None
        let n = text.Length

        while error.IsNone && i < n do
            // key
            let keyStart = i

            while i < n && text.[i] <> '=' && text.[i] <> ';' do
                i <- i + 1

            let key = text.Substring(keyStart, i - keyStart).Trim()

            if i >= n || text.[i] = ';' then
                if key <> "" then
                    error <- Some $"'{key}' has no value (expected key=value)"

                i <- i + 1
            else
                i <- i + 1 // '='

                while i < n && text.[i] = ' ' do
                    i <- i + 1

                let value = Text.StringBuilder()

                if i < n && (text.[i] = '"' || text.[i] = ''') then
                    let quote = text.[i]
                    i <- i + 1
                    let mutable closed = false

                    while not closed && i < n do
                        if text.[i] = quote then
                            if i + 1 < n && text.[i + 1] = quote then
                                value.Append quote |> ignore
                                i <- i + 2
                            else
                                closed <- true
                                i <- i + 1
                        else
                            value.Append text.[i] |> ignore
                            i <- i + 1

                    if not closed then
                        error <- Some $"the value of '{key}' has an unclosed quote"

                    while i < n && text.[i] <> ';' do
                        i <- i + 1
                else
                    while i < n && text.[i] <> ';' do
                        value.Append text.[i] |> ignore
                        i <- i + 1

                i <- i + 1 // ';'

                if key = "" then
                    error <- Some "a value has no key (expected key=value)"
                else
                    pairs.Add(key, value.ToString().Trim())

        match error with
        | Some e -> Error e
        | None -> Ok(List.ofSeq pairs)

    let private classify (dataSource: string) : Result<XmlaEndpoint, string> =
        let starts (p: string) =
            dataSource.StartsWith(p, StringComparison.OrdinalIgnoreCase)

        if String.IsNullOrWhiteSpace dataSource then
            Error "the server is empty"
        elif starts "powerbi://" then
            Ok XmlaEndpoint.PowerBI
        elif starts "asazure://" then
            Ok XmlaEndpoint.AzureAS
        elif starts "https://" || starts "http://" then
            Ok XmlaEndpoint.Http
        elif dataSource.Contains "://" then
            Error "unknown scheme (expected powerbi://, asazure://, http(s)://, or a host name)"
        elif dataSource.Contains '/' || dataSource.Contains '=' then
            Error "not a server name (expected host, host:port or host\\instance)"
        else
            Ok XmlaEndpoint.Tcp

    let private looksLikeConnectionString (text: string) =
        match text.IndexOf '=' with
        | -1 -> false
        | i ->
            let key = normalizeKey (text.Substring(0, i))
            key = "provider" || List.contains key dataSourceKeys

    let private ofConnectionString (input: string) (text: string) : Result<XmlaAddress, TessError> =
        match splitConnectionString text with
        | Error reason -> Error(InvalidServerAddress(input, reason))
        | Ok pairs ->
            match pairs |> List.tryFind (fun (k, _) -> isCredentialKey k) with
            | Some(key, _) -> Error(SecretOnCommandLine key)
            | None ->
                let pick keys =
                    pairs
                    |> List.filter (fun (k, _) -> List.contains (normalizeKey k) keys)
                    |> List.map snd

                match pick dataSourceKeys, pick catalogKeys with
                | [], _ -> Error(InvalidServerAddress(input, "the connection string has no Data Source"))
                | _ :: _ :: _, _ -> Error(InvalidServerAddress(input, "more than one Data Source"))
                | _, _ :: _ :: _ -> Error(InvalidServerAddress(input, "more than one Initial Catalog"))
                | [ dataSource ], catalog ->
                    match classify dataSource with
                    | Error reason -> Error(InvalidServerAddress(input, reason))
                    | Ok endpoint ->
                        let rest =
                            pairs
                            |> List.filter (fun (k, _) ->
                                let k = normalizeKey k

                                not (List.contains k dataSourceKeys || List.contains k catalogKeys || k = "provider"))

                        Ok {
                            DataSource = dataSource
                            Endpoint = endpoint
                            Database = catalog |> List.tryHead |> Option.filter (fun c -> c <> "")
                            Properties = rest
                        }

    /// Parse a server address, with or without the `xmla:` prefix: `<server>[;<database>]`, or a
    /// connection string (`Data Source=...;Initial Catalog=...`, without credentials).
    let parseServer (input: string) : Result<XmlaAddress, TessError> =
        let text =
            let t = (if isNull input then "" else input).Trim()

            if t.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                t.Substring(prefix.Length).Trim()
            else
                t

        if looksLikeConnectionString text then
            ofConnectionString input text
        else
            let server, database =
                match text.LastIndexOf ';' with
                | -1 -> text, None
                | i -> text.Substring(0, i).Trim(), Some(text.Substring(i + 1).Trim())

            if server.Contains ';' then
                Error(InvalidServerAddress(input, "expected <server>[;<database>], or a connection string"))
            else
                match classify server with
                | Error reason -> Error(InvalidServerAddress(input, reason))
                | Ok endpoint ->
                    Ok {
                        DataSource = server
                        Endpoint = endpoint
                        Database = database |> Option.filter (fun d -> d <> "")
                        Properties = []
                    }

    /// Parse a remote model argument. It must carry the `xmla:` prefix.
    let parse (input: string) : Result<XmlaAddress, TessError> =
        if isRemote input then
            parseServer input
        else
            Error(InvalidServerAddress(input, $"a remote model starts with '{prefix}'"))

    /// The server alone, for messages: the data source, never the other properties.
    let server (a: XmlaAddress) = a.DataSource

    /// The address in its `xmla:<server>[;<database>]` form (extra properties left out).
    let display (a: XmlaAddress) =
        match a.Database with
        | Some d -> $"{prefix}{a.DataSource};{d}"
        | None -> $"{prefix}{a.DataSource}"

    let withDatabase (database: string) (a: XmlaAddress) = { a with Database = Some database }

    /// Is this one of Microsoft's cloud endpoints (authenticated with Microsoft Entra ID)?
    let isCloud (a: XmlaAddress) =
        a.Endpoint = XmlaEndpoint.PowerBI || a.Endpoint = XmlaEndpoint.AzureAS

    let sameServer (a: XmlaAddress) (b: XmlaAddress) =
        eq (a.DataSource.TrimEnd '/') (b.DataSource.TrimEnd '/')
