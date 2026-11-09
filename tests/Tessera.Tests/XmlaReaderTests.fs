/// The code the live connector runs on what a server returns, driven by plain System.Data readers
/// and TOM databases (the connection types themselves can't be constructed without a server).
module Tessera.Tests.XmlaReaderTests

open System
open System.Data
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Tests.Fixtures

let private table (columns: (string * Type) list) (rows: obj list list) =
    let t = new DataTable()

    for name, columnType in columns do
        t.Columns.Add(name, columnType) |> ignore

    for row in rows do
        t.Rows.Add(Array.ofList row) |> ignore

    t

let private reader (tables: DataTable list) : IDataReader =
    let set = new DataSet()

    for t in tables do
        set.Tables.Add t

    set.CreateDataReader()

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

[<Tests>]
let tests =
    testList "Xmla reader" [
        testCase "declared column types, and each value by its own type"
        <| fun () ->
            let t =
                table [
                    "Sales[Product]", typeof<string>
                    "[Qty]", typeof<int64>
                    "[Small]", typeof<int32>
                    "[Ratio]", typeof<double>
                    "[Amount]", typeof<decimal>
                    "[When]", typeof<DateTime>
                    "[Flag]", typeof<bool>
                    "[Bytes]", typeof<byte array>
                    "[Any]", typeof<obj>
                ] [
                    [
                        box "Bike"
                        box 3L
                        box 7
                        box 0.5
                        box 12.34M
                        box (DateTime(2024, 1, 31))
                        box true
                        box [| 1uy; 2uy |]
                        box 9L
                    ]
                    [
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box DBNull.Value
                        box "text"
                    ]
                ]

            use r = reader [ t ]
            let result = ResultReader.readResult r None

            Expect.equal
                (result.Columns |> List.map (fun c -> c.Name, c.Type))
                [
                    "Sales[Product]", QueryType.String
                    "[Qty]", QueryType.Int64
                    "[Small]", QueryType.Int64
                    "[Ratio]", QueryType.Double
                    "[Amount]", QueryType.Decimal
                    "[When]", QueryType.DateTime
                    "[Flag]", QueryType.Boolean
                    "[Bytes]", QueryType.Binary
                    "[Any]", QueryType.Variant
                ]
                ""

            Expect.equal
                result.Rows.[0]
                [|
                    QueryValue.String "Bike"
                    QueryValue.Int64 3L
                    QueryValue.Int64 7L
                    QueryValue.Double 0.5
                    QueryValue.Decimal 12.34M
                    QueryValue.DateTime(DateTime(2024, 1, 31))
                    QueryValue.Boolean true
                    QueryValue.Binary [| 1uy; 2uy |]
                    QueryValue.Int64 9L
                |]
                "a variant cell keeps its own type"

            Expect.equal (result.Rows.[1] |> Array.take 8) (Array.create 8 QueryValue.Blank) "DBNull is BLANK"
            Expect.equal result.Rows.[1].[8] (QueryValue.String "text") ""
            Expect.isFalse result.Truncated ""

        testCase "an empty string is not BLANK"
        <| fun () ->
            use r =
                reader [ table [ "[s]", typeof<string> ] [ [ box "" ]; [ box DBNull.Value ] ] ]

            Expect.equal (ResultReader.readResult r None).Rows [ [| QueryValue.String "" |]; [| QueryValue.Blank |] ] ""

        testCase "the row limit: truncated only when there really is another row"
        <| fun () ->
            let three () =
                reader [ table [ "[n]", typeof<int64> ] [ for i in 1L .. 3L -> [ box i ] ] ]

            let read (limit: int option) =
                use r = three ()
                let result = ResultReader.readResult r limit
                result.Rows.Length, result.Truncated

            Expect.equal (read None) (3, false) ""
            Expect.equal (read (Some 5)) (3, false) ""
            Expect.equal (read (Some 3)) (3, false) "exactly the limit is not truncated"
            Expect.equal (read (Some 2)) (2, true) ""
            Expect.equal (read (Some 1)) (1, true) ""

        testCase "every result set, in order, each with its own columns and limit"
        <| fun () ->
            use r =
                reader [
                    table [ "[a]", typeof<int64> ] [ for i in 1L .. 4L -> [ box i ] ]
                    table [ "[b]", typeof<string>; "[c]", typeof<double> ] [ [ box "x"; box 1.5 ] ]
                    table [ "[empty]", typeof<string> ] []
                ]

            let results = ResultReader.readAll r (Some 2)

            Expect.equal
                [
                    for x in results -> [ for c in x.Columns -> c.Name ], x.Rows.Length, x.Truncated
                ]
                [ [ "[a]" ], 2, true; [ "[b]"; "[c]" ], 1, false; [ "[empty]" ], 0, false ]
                ""

        testCase "an unexpected value type is kept as invariant text rather than lost"
        <| fun () ->
            Expect.equal (ResultReader.queryValue (box 1.5f)) (QueryValue.Double 1.5) ""

            Expect.equal
                (ResultReader.queryValue (box (Guid.Empty)))
                (QueryValue.String "00000000-0000-0000-0000-000000000000")
                ""

            Expect.equal (ResultReader.queryValue null) QueryValue.Blank ""
            Expect.equal (ResultReader.queryType typeof<Guid>) QueryType.Variant ""

        testCase "detach: a copy equal to the original, with its name and ID, tied to nothing"
        <| fun () ->
            let db = sampleDatabase ()
            db.Name <- "Deployed Name"
            db.ID <- "3f2c1a7e-0000-4000-8000-123456789abc"
            let copy = ResultReader.detach db
            Expect.isFalse (obj.ReferenceEquals(copy, db)) ""
            Expect.equal copy.Name "Deployed Name" ""
            Expect.equal copy.ID "3f2c1a7e-0000-4000-8000-123456789abc" "deploy keeps the ID from here"
            Expect.equal copy.CompatibilityLevel db.CompatibilityLevel ""
            Expect.isNull copy.Server ""
            Expect.isEmpty (Diff.compare Diff.Options.none db copy) ""
            // Changing the copy leaves the original alone.
            copy.Model.Tables.["Sales"].Measures.["Total"].Expression <- "0"
            Expect.equal db.Model.Tables.["Sales"].Measures.["Total"].Expression "SUM(Sales[Amount])" ""

        testCase "detach keeps a real model whole (TESSERA_SAMPLE_MODEL)"
        <| fun () ->
            match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
            | null
            | "" -> skiptest "TESSERA_SAMPLE_MODEL is not set"
            | path ->
                let _, db = ModelIO.loadPath path |> ok
                let copy = ResultReader.detach db
                Expect.isEmpty (Diff.compare Diff.Options.none db copy) ""

                // And planning a deployment of a model onto itself changes nothing.
                let target: Deploy.Target = {
                    Database = "Target"
                    Deployed = Some copy
                    CompatibilityMode = None
                    SupportedCompatibilityLevels = []
                }

                let plan = Deploy.plan Deploy.Options.defaults Diff.Options.none target db |> ok

                Expect.isEmpty
                    (plan.Changes
                     |> ChangeSet.ofDiff
                     |> List.filter (fun c -> c.Ref.Kind <> ObjectKind.Model))
                    "only the database's own name may differ"

                Expect.isEmpty plan.IntroducedErrors ""
    ]
