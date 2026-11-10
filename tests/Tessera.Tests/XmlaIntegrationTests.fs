/// Checks against a real server. They run only when TESSERA_XMLA_SERVER and TESSERA_XMLA_DATABASE
/// are set (with credentials in the TESS_XMLA_* variables, as for the tool itself); otherwise they
/// are skipped. They only read: nothing is deployed. See docs/xmla-manual-test.md.
/// The one exception needs a second opt-in, TESSERA_XMLA_ALLOW_REFRESH=1: it runs a `calculate`
/// refresh of the whole model (which loads no data, but does write to the server).
module Tessera.Tests.XmlaIntegrationTests

open System
open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Cli
open Tessera.Tests.Fixtures

let private variable (name: string) =
    match Environment.GetEnvironmentVariable name with
    | null
    | "" -> None
    | v -> Some v

let private target =
    match variable "TESSERA_XMLA_SERVER", variable "TESSERA_XMLA_DATABASE" with
    | Some server, Some database -> Some(server, database)
    | _ -> None

/// Run the real CLI, with the real connector and the real environment.
let private tess (args: string list) =
    use out = new StringWriter()
    use err = new StringWriter()
    let code = App.run out err (Array.ofList args)
    code, out.ToString(), err.ToString()

[<Tests>]
let tests =
    testSequenced
    <| testList "XMLA integration (TESSERA_XMLA_SERVER, TESSERA_XMLA_DATABASE)" [
        match target with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_XMLA_SERVER and TESSERA_XMLA_DATABASE are not set"
        | Some(server, database) ->
            let source = $"xmla:{server};{database}"

            testCase "connect lists the database"
            <| fun () ->
                let code, out, err = tess [ "connect"; server; "--json" ]
                Expect.equal code 0 err
                use doc = JsonDocument.Parse out

                let names = [
                    for d in doc.RootElement.GetProperty("databases").EnumerateArray() ->
                        d.GetProperty("name").GetString()
                ]

                Expect.exists names (fun n -> String.Equals(n, database, StringComparison.OrdinalIgnoreCase)) ""

            testCase "a wrong database name lists the ones that exist"
            <| fun () ->
                let code, out, _ =
                    tess [ "info"; $"xmla:{server};tessera-no-such-database"; "--json" ]

                Expect.equal code Common.ExitCode.remote out
                use doc = JsonDocument.Parse out

                Expect.equal
                    (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                    "database-not-found"
                    ""

            testCase "the remote model converts to files that load back identical"
            <| fun () ->
                use t = new TempDir()
                let folder = Path.Combine(t.Path, "model")
                let code, _, err = tess [ "convert"; source; folder ]
                Expect.equal code 0 err
                let code, out, err = tess [ "diff"; source; folder ]
                Expect.equal code 0 (out + err)

            testCase "a query returns typed values in the documented shape"
            <| fun () ->
                let dax =
                    "EVALUATE ROW(\"i\", 42, \"d\", 1.5, \"c\", CURRENCY(12.34), \"s\", \"x\", \"t\", DATE(2024,1,31), "
                    + "\"b\", TRUE(), \"n\", BLANK()) EVALUATE { 1, 2, 3 }"

                let code, out, err = tess [ "query"; source; dax; "--json"; "--max-rows"; "2" ]
                Expect.equal code 0 err
                use doc = JsonDocument.Parse out
                let results = doc.RootElement.GetProperty "results"
                Expect.equal (results.GetArrayLength()) 2 "one result per EVALUATE"

                Expect.equal
                    (results.[0]
                        .GetProperty("rows")
                        .[0].GetRawText()
                        .Replace(" ", "")
                        .Replace("\n", "")
                        .Replace("\r", ""))
                    """[42,1.5,12.34,"x","2024-01-31T00:00:00",true,null]"""
                    ""

                // The seventh column is all BLANK: its declared type is whatever the server says.
                Expect.equal
                    ([
                        for c in results.[0].GetProperty("columns").EnumerateArray() ->
                            c.GetProperty("type").GetString()
                     ]
                     |> List.truncate 6)
                    [ "int64"; "double"; "decimal"; "string"; "dateTime"; "boolean" ]
                    ""

                Expect.isTrue (results.[1].GetProperty("truncated").GetBoolean()) "3 rows, limit 2"
                Expect.equal (results.[1].GetProperty("rowCount").GetInt32()) 2 ""

            testCase "a query error is reported as one"
            <| fun () ->
                let code, out, _ =
                    tess [ "query"; source; "EVALUATE 'tessera no such table'"; "--json" ]

                Expect.equal code Common.ExitCode.remote out
                use doc = JsonDocument.Parse out
                Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "query-failed" ""

            testCase "deploying the model onto a copy of itself is a dry run with no changes"
            <| fun () ->
                use t = new TempDir()
                let folder = Path.Combine(t.Path, "model")
                let code, _, err = tess [ "convert"; source; folder; "--include-sensitive" ]
                Expect.equal code 0 err
                let code, out, err = tess [ "deploy"; folder; server; database; "--json" ]
                Expect.equal code 0 err
                use doc = JsonDocument.Parse out
                Expect.isTrue (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
                Expect.isFalse (doc.RootElement.GetProperty("deployed").GetBoolean()) ""
                Expect.equal (doc.RootElement.GetProperty("changes").GetArrayLength()) 0 out

            testCase "refresh: a dry run plans the whole model and runs nothing"
            <| fun () ->
                let code, out, err = tess [ "refresh"; source; "--type"; "full"; "--json" ]
                Expect.equal code 0 err
                use doc = JsonDocument.Parse out
                let root = doc.RootElement
                Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
                Expect.isFalse (root.GetProperty("refreshed").GetBoolean()) ""
                Expect.equal (root.GetProperty("objects").[0].GetProperty("ref").GetString()) "model" ""
                Expect.isGreaterThan (root.GetProperty("covers").GetArrayLength()) 0 "the model's tables"
                use script = JsonDocument.Parse(root.GetProperty("tmsl").GetString())

                Expect.equal
                    (script.RootElement
                        .GetProperty("refresh")
                        .GetProperty("objects")
                        .[0].GetProperty("database")
                        .GetString())
                    (root.GetProperty("database").GetString())
                    "the server's name for the database"

            testCase "refresh: --execute recalculates the model and reports progress (TESSERA_XMLA_ALLOW_REFRESH=1)"
            <| fun () ->
                if variable "TESSERA_XMLA_ALLOW_REFRESH" <> Some "1" then
                    skiptest "TESSERA_XMLA_ALLOW_REFRESH is not 1: this test writes to the server"

                let code, out, err =
                    tess [
                        "refresh"
                        source
                        "--type"
                        "calculate"
                        "--execute"
                        "--progress"
                        "all"
                        "--json"
                    ]

                Expect.equal code 0 (out + err)
                use doc = JsonDocument.Parse out
                let root = doc.RootElement
                Expect.isTrue (root.GetProperty("refreshed").GetBoolean()) out
                // If the trace didn't work this says why; the refresh itself must still succeed.
                Expect.equal (root.GetProperty("traceError").ValueKind) JsonValueKind.Null out

            testCase "refresh: what can't be refreshed is an error before anything runs"
            <| fun () ->
                let code, out, _ =
                    tess [ "refresh"; source; "tessera no such table"; "--execute"; "--json" ]

                Expect.equal code Common.ExitCode.input out
                use doc = JsonDocument.Parse out

                Expect.equal
                    (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                    "object-not-found"
                    ""
    ]
