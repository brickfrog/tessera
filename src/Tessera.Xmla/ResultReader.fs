/// Reading query results and databases off a connection into tessera's own shapes. No connection
/// types here: this works on any System.Data reader and any TOM database, so it is tested without
/// a server.
module Tessera.Xmla.ResultReader

open System
open System.Data
open Microsoft.AnalysisServices.Tabular
open Tessera

/// The result type for a column the reader declares as this .NET type. Anything it doesn't
/// declare precisely (ADOMD says Object for a column of mixed or unknown type) is a variant.
let queryType (t: Type) =
    if t = typeof<string> then
        QueryType.String
    elif t = typeof<int64> || t = typeof<int32> || t = typeof<int16> || t = typeof<byte> then
        QueryType.Int64
    elif t = typeof<double> || t = typeof<single> then
        QueryType.Double
    elif t = typeof<decimal> then
        QueryType.Decimal
    elif t = typeof<DateTime> then
        QueryType.DateTime
    elif t = typeof<bool> then
        QueryType.Boolean
    elif t = typeof<byte array> then
        QueryType.Binary
    else
        QueryType.Variant

/// A value as the reader returns it. null and DBNull are BLANK.
let queryValue (v: obj) : QueryValue =
    match v with
    | null -> QueryValue.Blank
    | :? DBNull -> QueryValue.Blank
    | :? string as s -> QueryValue.String s
    | :? int64 as i -> QueryValue.Int64 i
    | :? int32 as i -> QueryValue.Int64(int64 i)
    | :? int16 as i -> QueryValue.Int64(int64 i)
    | :? byte as i -> QueryValue.Int64(int64 i)
    | :? uint32 as i -> QueryValue.Int64(int64 i)
    | :? double as d -> QueryValue.Double d
    | :? single as d -> QueryValue.Double(double d)
    | :? decimal as d -> QueryValue.Decimal d
    | :? DateTime as d -> QueryValue.DateTime d
    | :? bool as b -> QueryValue.Boolean b
    | :? (byte array) as b -> QueryValue.Binary b
    | other -> QueryValue.String(Convert.ToString(other, Globalization.CultureInfo.InvariantCulture))

/// The reader's current result set. With maxRows, at most that many rows are kept, and Truncated
/// says whether there was at least one more.
let readResult (reader: IDataReader) (maxRows: int option) : QueryResult =
    let columns = [
        for i in 0 .. reader.FieldCount - 1 ->
            {
                Name = reader.GetName i
                Type = queryType (reader.GetFieldType i)
            }
    ]

    let rows = ResizeArray<QueryValue array>()
    let mutable truncated = false
    let mutable more = true

    while more && reader.Read() do
        match maxRows with
        | Some limit when rows.Count >= limit ->
            truncated <- true
            more <- false
        | _ -> rows.Add(Array.init reader.FieldCount (fun i -> queryValue (reader.GetValue i)))

    {
        Columns = columns
        Rows = List.ofSeq rows
        Truncated = truncated
    }

/// Every result set of a reader, in order: one per EVALUATE.
let readAll (reader: IDataReader) (maxRows: int option) : QueryResult list =
    let results = ResizeArray<QueryResult>()
    results.Add(readResult reader maxRows)

    while reader.NextResult() do
        results.Add(readResult reader maxRows)

    List.ofSeq results

/// A copy of a database that has no tie to where it came from, and looks like a model loaded from
/// a .bim file: no server-inferred objects, no timestamps, the same name and ID. (A connected
/// TOM database talks to its server when touched; nothing downstream should.)
let detach (db: Database) : Database =
    let options = {
        ModelIO.SaveOptions.defaults with
            IncludeSensitive = true
    }

    ModelIO.parseBim (ModelIO.toBimText options db)
