namespace Tessera.Xmla

open System
open Microsoft.AnalysisServices.Tabular
open Tessera

// The seam between tessera and a server. Everything tessera does over a connection goes through
// these two interfaces; LiveConnector (Live.fs) is the one implementation that opens sockets, and
// tests substitute their own.

/// A message a server returned for a script.
type XmlaMessage = { IsError: bool; Text: string }

type ServerProperties = {
    Version: string
    ProductName: string
    CompatibilityMode: Microsoft.AnalysisServices.CompatibilityMode option
    /// Empty when the server doesn't say.
    SupportedCompatibilityLevels: int list
}

type DatabaseSummary = {
    Name: string
    Id: string
    CompatibilityLevel: int
}

/// What to connect to, and as whom.
type ConnectionRequest = {
    Address: XmlaAddress
    Credential: XmlaCredential
    /// Shows the user a message they must act on (the device-code sign-in instructions).
    Notify: string -> unit
}

/// The server reported an error in a query (as opposed to the connection failing).
exception XmlaQueryError of message: string

/// An open connection to one server. Members throw on failure; Remote turns what they throw into
/// TessErrors (with credentials scrubbed).
type IXmlaSession =
    inherit IDisposable
    abstract Server: ServerProperties
    /// The databases these credentials can see.
    abstract Databases: unit -> DatabaseSummary list
    /// A database's model, detached from the connection (as if loaded from a .bim file).
    abstract ReadDatabase: name: string -> Database
    /// Run a TMSL or XMLA script.
    abstract Execute: script: string -> XmlaMessage list
    /// Run a DAX query: one result per EVALUATE. With maxRows, each result holds at most that many
    /// rows and says whether there were more. Raises XmlaQueryError for an error in the query.
    abstract Query: database: string * dax: string * maxRows: int option -> QueryResult list

type IXmlaConnector =
    /// Open a connection. Throws when the server can't be reached or refuses the credentials.
    abstract Open: ConnectionRequest -> IXmlaSession
