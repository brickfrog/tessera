/// tess's JSON leaves an absent value out rather than writing `null` (the Tabular Editor CLI's
/// convention). `null` appears only where it is a value: a property that holds none (`get
/// --properties`, a change's before/after) and a BLANK cell in query results.
module Tessera.Tests.JsonNullTests

open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.XmlaFakes
open Tessera.Cli.Common

/// Paths ("/a/0/b") of every null in a document.
let nullPaths (doc: JsonElement) =
    let rec walk (path: string) (e: JsonElement) = [
        match e.ValueKind with
        | JsonValueKind.Null -> yield path
        | JsonValueKind.Object ->
            for p in e.EnumerateObject() do
                yield! walk $"{path}/{p.Name}" p.Value
        | JsonValueKind.Array ->
            let mutable i = 0

            for x in e.EnumerateArray() do
                yield! walk $"{path}/{i}" x
                i <- i + 1
        | _ -> ()
    ]

    walk "" doc

/// Where a null is a value, by command.
let private allowed =
    [
        // get --properties: a property with no value.
        "get --properties", @"^/\d+/value$"
        // A change's before/after: the property had (or has) no value.
        "changes", @"/changes/\d+/properties/\d+/(before|after)$"
        // query: BLANK cells, and cells with no format string.
        "query", @"^/results/\d+/(rows|formatted)/\d+/\d+$"
    ]
    |> List.map (fun (_, rx) -> Regex rx)

let private unexpectedNulls (r: Run) =
    use doc =
        try
            JsonDocument.Parse r.Out
        with e ->
            failtestf "not JSON (exit %d): %s\n%s\n%s" r.Code e.Message r.Out r.Err

    nullPaths doc.RootElement
    |> List.filter (fun p -> not (allowed |> List.exists (fun rx -> rx.IsMatch p)))

[<Tests>]
let tests =
    testList "JSON nulls" [
        testCase "no command writes null for an absent value"
        <| fun () ->
            use t = new TempDir()
            let model = t.Combine "model.bim"
            let db = sampleDatabase ()
            File.WriteAllText(model, JsonSerializer.SerializeDatabase db)
            let other = t.Combine "other.bim"
            let changed = sampleDatabase ()
            changed.Model.Tables.["Sales"].Measures.["Total"].Expression <- "SUM(Sales[Amount]) + 1"
            changed.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "New", Expression = "1"))
            File.WriteAllText(other, JsonSerializer.SerializeDatabase changed)
            let pbip = t.Combine "pbip"
            let semanticModel = writePbipProject pbip "Sales"
            let data = t.Combine "data"
            let script = t.Combine "script.dax"
            File.WriteAllText(script, "MEASURE Sales[Scripted] = 1\n")

            write
                (t.Combine(".tess-tests", "s.test.yaml"))
                (String.concat "\n" [
                    "suite: S"
                    "data: ../data"
                    "tests:"
                    "  - name: blank"
                    "    expression: BLANK()"
                    "    expect: null"
                    "  - name: wrong"
                    "    expression: BLANK()"
                    "    expect: 12345"
                    "  - name: later"
                    "    expression: 1"
                    "    expect: 1"
                    "    skip: not yet"
                    ""
                ])

            let local = [
                [ "info"; model ]
                [ "info"; semanticModel ]
                [ "list"; model ]
                [ "get"; model; "measure:Sales/Total" ]
                [ "get"; model; "measure:Sales/Total"; "--properties" ]
                [ "find"; model; "Total" ]
                [ "diff"; model; other ]
                [ "deps"; model; "measure:Sales/Total" ]
                [ "deps"; model; "--unused" ]
                [ "validate"; model ]
                [ "bpa"; model ]
                [ "bpa"; model; "--list-rules" ]
                [ "bpa"; "rules"; "list"; model ]
                [ "lint"; model ]
                [ "format"; model ]
                [ "format"; "--expr"; "SUM(x" ]
                [ "set"; model; "measure:Sales/Total"; "-p"; "description=x" ]
                [ "add"; model; "measure:Sales/Added"; "--expression"; "1" ]
                [ "remove"; model; "measure:Sales/Total (hidden)" ]
                [ "rename"; model; "measure:Sales/Total"; "Grand" ]
                [ "move"; model; "measure:Sales/Total"; "Date" ]
                [ "translations"; "export"; model ]
                [ "script"; "export"; model ]
                [ "data"; "init"; model; data; "--rows"; "3" ]
                [ "data"; "check"; model; data ]
                [ "data"; "info"; model; data ]
                [ "query"; model; "--data"; data; "EVALUATE ROW(\"x\", BLANK())" ]
                [
                    "query"
                    model
                    "--data"
                    data
                    "--formatted"
                    "EVALUATE ROW(\"x\", BLANK())"
                ]
                [ "test"; "run"; model ]
                [ "test"; "snapshot"; model ]
                [ "perspective"; "list"; model; "Sales View" ]
                [ "translations"; "list"; model ]
                [ "bpa"; "rules"; "show"; model; "TESS_DIVIDE" ]
                [ "script"; "apply"; model; script ]
                [ "get"; model; "column:Sales/Amount"; "-p"; "sortByColumn" ]
                [ "get"; model; "column:Sales/Amount"; "-p"; "description" ]
            ]

            let failures = [
                for args in local do
                    let r = tess (args @ [ "--json" ])

                    match unexpectedNulls r with
                    | [] -> ()
                    | paths -> $"""tess {String.concat " " args}: {String.concat ", " paths}"""
            ]

            let server = FakeServer()
            let sales = sampleDatabase ()
            sales.Name <- "Sales"
            server.Add sales

            let remote = [
                [ "connect"; workspace ]
                [ "refresh"; $"xmla:{workspace};Sales" ]
                [ "info"; $"xmla:{workspace};Sales" ]
                [ "deploy"; model; workspace; "Sales" ]
                [ "deploy"; model; workspace; "New" ]
            ]

            let remoteFailures = [
                for args in remote do
                    let r = tessOn server tokenEnv (args @ [ "--json" ])

                    match unexpectedNulls r with
                    | [] -> ()
                    | paths -> $"""tess {String.concat " " args}: {String.concat ", " paths}"""
            ]

            match failures @ remoteFailures with
            | [] -> ()
            | xs -> failtestf "nulls for absent values:\n%s" (String.concat "\n" xs)

        testCase "nulls that are values stay: a property with no value, a BLANK cell"
        <| fun () ->
            use t = new TempDir()
            let model = t.Combine "model.bim"
            File.WriteAllText(model, JsonSerializer.SerializeDatabase(sampleDatabase ()))
            let props = tess [ "get"; model; "column:Sales/Amount"; "--properties"; "--json" ]
            use doc = JsonDocument.Parse props.Out

            let sortBy =
                doc.RootElement.EnumerateArray()
                |> Seq.find (fun p -> p.GetProperty("property").GetString() = "sortByColumn")

            Expect.equal (sortBy.GetProperty("value").ValueKind) JsonValueKind.Null "unset, so null"
            tess [ "data"; "init"; model; t.Combine "data" ] |> ignore

            let q =
                tess [
                    "query"
                    model
                    "--data"
                    t.Combine "data"
                    "EVALUATE ROW(\"x\", BLANK())"
                    "--json"
                ]

            Expect.contains (nullPaths (JsonDocument.Parse q.Out).RootElement) "/results/0/rows/0/0" "BLANK is null"
    ]
