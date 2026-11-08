/// End-to-end: drive the real CLI entry point with captured stdout/stderr.
module Tessera.Tests.CliTests

open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Cli
open Tessera.Tests.Fixtures

type private Run = { Code: int; Out: string; Err: string }

let private tess (args: string list) =
    use out = new StringWriter()
    use err = new StringWriter()
    let code = App.run out err (Array.ofList args)

    {
        Code = code
        Out = out.ToString()
        Err = err.ToString()
    }

let private json (r: Run) =
    try
        JsonDocument.Parse(r.Out)
    with e ->
        failtestf "stdout is not JSON (%s):\n%s" e.Message r.Out

let private project () =
    let t = new TempDir()
    writePbipProject t.Path "Sales" |> ignore
    t

[<Tests>]
let tests =
    testList "CLI" [
        testCase "--version"
        <| fun () ->
            let r = tess [ "--version" ]
            Expect.equal r.Code App.ExitCode.ok ""
            Expect.stringStarts r.Out "tess " ""

        testCase "--help exits 0 and lists subcommands"
        <| fun () ->
            let r = tess [ "--help" ]
            Expect.equal r.Code App.ExitCode.ok ""

            for sub in [ "convert"; "info"; "list" ] do
                Expect.stringContains r.Out sub ""

        testCase "no subcommand is a usage error"
        <| fun () -> Expect.equal (tess []).Code App.ExitCode.usage ""

        testCase "unknown option is a usage error, reported as JSON with --json"
        <| fun () ->
            let r = tess [ "list"; "x"; "--bogus"; "--json" ]
            Expect.equal r.Code App.ExitCode.usage ""
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "usage" ""

        testCase "info --json"
        <| fun () ->
            use t = project ()
            let r = tess [ "info"; t.Path; "--json" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("compatibilityLevel").GetInt32()) 1702 ""
            Expect.equal (root.GetProperty("source").GetProperty("format").GetString()) "tmdl" ""
            Expect.equal (root.GetProperty("counts").GetProperty("measure").GetInt32()) 2 ""
            Expect.equal (root.GetProperty("calculationGroups").GetInt32()) 1 ""

        testCase "--json may come before the subcommand"
        <| fun () ->
            use t = project ()
            let r = tess [ "--json"; "info"; t.Path ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            use _ = json r
            ()

        testCase "info text"
        <| fun () ->
            use t = project ()
            let r = tess [ "info"; t.Path ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            Expect.stringContains r.Out "1702 (PowerBI)" ""
            Expect.stringContains r.Out "1 calculation group" ""

        testCase "list text is one ref per line"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "-k"; "measure"; "-t"; "Sales" ]
            Expect.equal r.Code App.ExitCode.ok r.Err

            Expect.equal
                (r.Out.Trim().Split('\n') |> Array.map (fun s -> s.TrimEnd()) |> List.ofArray)
                [ "measure:Sales/Total"; "measure:Sales/Total (hidden)  (hidden)" ]
                ""

        testCase "list --json"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "--kind"; "relationship,role"; "--json" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            use doc = json r
            let items = doc.RootElement.EnumerateArray() |> List.ofSeq
            Expect.equal items.Length 2 ""
            Expect.equal (items.[0].GetProperty("detail").GetString()) "Sales[DateKey] -> Date[DateKey]" ""
            Expect.equal (items.[1].GetProperty("ref").GetString()) "role:Readers" ""

        testCase "list with an unknown kind is a usage error"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "-k"; "nope" ]
            Expect.equal r.Code App.ExitCode.usage ""
            Expect.stringContains r.Err "unknown kind" ""

        testCase "missing model is an input error"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "info"; t.Combine "missing" ]
            Expect.equal r.Code App.ExitCode.input ""
            Expect.stringContains r.Err "Path not found" ""

        testCase "convert pbip -> bim -> pbip"
        <| fun () ->
            use t = project ()
            let bim = t.Combine "out.bim"
            let r1 = tess [ "convert"; t.Path; bim ]
            Expect.equal r1.Code App.ExitCode.ok r1.Err
            let r2 = tess [ "convert"; bim; t.Combine "Copy.SemanticModel"; "--json" ]
            Expect.equal r2.Code App.ExitCode.ok r2.Err
            use doc = json r2
            Expect.equal (doc.RootElement.GetProperty("output").GetProperty("format").GetString()) "pbip" ""
            Expect.isTrue (File.Exists(t.Combine("Copy.SemanticModel", ".platform"))) ""

        testCase "convert onto a non-empty destination is an output error unless --force"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "out"
            Expect.equal (tess [ "convert"; t.Path; dest ]).Code App.ExitCode.ok ""
            let again = tess [ "convert"; t.Path; dest; "--json" ]
            Expect.equal again.Code App.ExitCode.output ""
            use doc = json again

            Expect.equal
                (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                "destination-not-empty"
                ""

            Expect.equal (tess [ "convert"; t.Path; dest; "--force" ]).Code App.ExitCode.ok ""

        testCase "convert validates its options"
        <| fun () ->
            use t = project ()

            for bad in [ [ "--format"; "xml" ]; [ "--eol"; "cr" ]; [ "--indent-spaces"; "0" ] ] do
                let r = tess ([ "convert"; t.Path; t.Combine "o" ] @ bad)
                Expect.equal r.Code App.ExitCode.usage (String.concat " " bad)

        testCase "convert --eol crlf --indent-spaces 2"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "o"
            let r = tess [ "convert"; t.Path; dest; "--eol"; "crlf"; "--indent-spaces"; "2" ]
            Expect.equal r.Code App.ExitCode.ok r.Err

            Expect.stringContains
                (File.ReadAllText(Path.Combine(dest, "tables", "Sales.tmdl")))
                "\r\n  measure Total"
                ""

        testCase "convert keeps a CRLF destination CRLF without being told"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "o"
            Expect.equal (tess [ "convert"; t.Path; dest; "--eol"; "crlf" ]).Code App.ExitCode.ok ""
            let r = tess [ "convert"; t.Path; dest; "--force"; "--json" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("output").GetProperty("eol").GetString()) "crlf" ""
            Expect.stringContains (File.ReadAllText(Path.Combine(dest, "model.tmdl"))) "\r\n" ""

        testCase "get prints TMDL by default"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "[Total]" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            Expect.stringStarts r.Out "measure Total = SUM(Sales[Amount])" ""

        testCase "get -p prints a bare value"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "'Sales'[Total]"; "-p"; "expression" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            Expect.equal r.Out "SUM(Sales[Amount])\n" ""

        testCase "get -p with several paths prints path: value lines"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "[Total]"; "-p"; "expression"; "-p"; "formatString" ]
            Expect.equal r.Out "expression: SUM(Sales[Amount])\nformatString: #,0\n" ""

        testCase "get --json"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "Sales"; "--json" ]
            Expect.equal r.Code App.ExitCode.ok r.Err
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("ref").GetString()) "table:Sales" ""
            Expect.equal (doc.RootElement.GetProperty("object").GetProperty("name").GetString()) "Sales" ""

            let mutable columns = Unchecked.defaultof<JsonElement>
            Expect.isFalse (doc.RootElement.GetProperty("object").TryGetProperty("columns", &columns)) "no children"

        testCase "get --children includes child objects"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "Sales"; "--children" ]
            Expect.stringContains r.Out "measure Total" ""

        testCase "get errors: unknown object, ambiguous object, unknown property"
        <| fun () ->
            use t = project ()

            for args, code in
                [
                    [ "[Nope]" ], "object-not-found"
                    [ "[DateKey]" ], "ambiguous-object"
                    [ "[Total]"; "-p"; "nope" ], "property-not-found"
                ] do
                let r = tess ([ "get"; t.Path ] @ args @ [ "--json" ])
                Expect.equal r.Code App.ExitCode.input (String.concat " " args)
                use doc = json r
                Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) code ""
    ]
