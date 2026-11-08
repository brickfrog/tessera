/// End-to-end: drive the real CLI entry point with captured stdout/stderr.
module Tessera.Tests.CliTests

open System.IO
open System.Text.Json
open Expecto
open Tessera
open Tessera.Cli
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

[<Tests>]
let tests =
    testList "CLI" [
        testCase "--version"
        <| fun () ->
            let r = tess [ "--version" ]
            Expect.equal r.Code ExitCode.ok ""
            Expect.stringStarts r.Out "tess " ""

        testCase "--help exits 0 and lists subcommands"
        <| fun () ->
            let r = tess [ "--help" ]
            Expect.equal r.Code ExitCode.ok ""

            for sub in [ "convert"; "info"; "list" ] do
                Expect.stringContains r.Out sub ""

        testCase "no subcommand is a usage error"
        <| fun () -> Expect.equal (tess []).Code ExitCode.usage ""

        testCase "unknown option is a usage error, reported as JSON with --json"
        <| fun () ->
            let r = tess [ "list"; "x"; "--bogus"; "--json" ]
            Expect.equal r.Code ExitCode.usage ""
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "usage" ""

        testCase "info --json"
        <| fun () ->
            use t = project ()
            let r = tess [ "info"; t.Path; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
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
            Expect.equal r.Code ExitCode.ok r.Err
            use _ = json r
            ()

        testCase "info text"
        <| fun () ->
            use t = project ()
            let r = tess [ "info"; t.Path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "1702 (PowerBI)" ""
            Expect.stringContains r.Out "1 calculation group" ""

        testCase "list text is one ref per line"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "-k"; "measure"; "-t"; "Sales" ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (r.Out.Trim().Split('\n') |> Array.map (fun s -> s.TrimEnd()) |> List.ofArray)
                [ "measure:Sales/Total"; "measure:Sales/Total (hidden)  (hidden)" ]
                ""

        testCase "list --json"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "--kind"; "relationship,role"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let items = doc.RootElement.EnumerateArray() |> List.ofSeq
            Expect.equal items.Length 2 ""
            Expect.equal (items.[0].GetProperty("detail").GetString()) "Sales[DateKey] -> Date[DateKey]" ""
            Expect.equal (items.[1].GetProperty("ref").GetString()) "role:Readers" ""

        testCase "list with an unknown kind is a usage error"
        <| fun () ->
            use t = project ()
            let r = tess [ "list"; t.Path; "-k"; "nope" ]
            Expect.equal r.Code ExitCode.usage ""
            Expect.stringContains r.Err "unknown kind" ""

        testCase "missing model is an input error"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "info"; t.Combine "missing" ]
            Expect.equal r.Code ExitCode.input ""
            Expect.stringContains r.Err "Path not found" ""

        testCase "convert pbip -> bim -> pbip"
        <| fun () ->
            use t = project ()
            let bim = t.Combine "out.bim"
            let r1 = tess [ "convert"; t.Path; bim ]
            Expect.equal r1.Code ExitCode.ok r1.Err
            let r2 = tess [ "convert"; bim; t.Combine "Copy.SemanticModel"; "--json" ]
            Expect.equal r2.Code ExitCode.ok r2.Err
            use doc = json r2
            Expect.equal (doc.RootElement.GetProperty("output").GetProperty("format").GetString()) "pbip" ""
            Expect.isTrue (File.Exists(t.Combine("Copy.SemanticModel", ".platform"))) ""

        testCase "convert onto a non-empty destination is an output error unless --force"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "out"
            Expect.equal (tess [ "convert"; t.Path; dest ]).Code ExitCode.ok ""
            let again = tess [ "convert"; t.Path; dest; "--json" ]
            Expect.equal again.Code ExitCode.output ""
            use doc = json again

            Expect.equal
                (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                "destination-not-empty"
                ""

            Expect.equal (tess [ "convert"; t.Path; dest; "--force" ]).Code ExitCode.ok ""

        testCase "convert validates its options"
        <| fun () ->
            use t = project ()

            for bad in [ [ "--format"; "xml" ]; [ "--eol"; "cr" ]; [ "--indent-spaces"; "0" ] ] do
                let r = tess ([ "convert"; t.Path; t.Combine "o" ] @ bad)
                Expect.equal r.Code ExitCode.usage (String.concat " " bad)

        testCase "convert --eol crlf --indent-spaces 2"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "o"
            let r = tess [ "convert"; t.Path; dest; "--eol"; "crlf"; "--indent-spaces"; "2" ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.stringContains
                (File.ReadAllText(Path.Combine(dest, "tables", "Sales.tmdl")))
                "\r\n  measure Total"
                ""

        testCase "convert keeps a CRLF destination CRLF without being told"
        <| fun () ->
            use t = project ()
            let dest = t.Combine "o"
            Expect.equal (tess [ "convert"; t.Path; dest; "--eol"; "crlf" ]).Code ExitCode.ok ""
            let r = tess [ "convert"; t.Path; dest; "--force"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            Expect.equal (doc.RootElement.GetProperty("output").GetProperty("eol").GetString()) "crlf" ""
            Expect.stringContains (File.ReadAllText(Path.Combine(dest, "model.tmdl"))) "\r\n" ""

        testCase "get prints TMDL by default"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "[Total]" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringStarts r.Out "measure Total = SUM(Sales[Amount])" ""

        testCase "get -p prints a bare value"
        <| fun () ->
            use t = project ()
            let r = tess [ "get"; t.Path; "'Sales'[Total]"; "-p"; "expression" ]
            Expect.equal r.Code ExitCode.ok r.Err
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
            Expect.equal r.Code ExitCode.ok r.Err
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
                Expect.equal r.Code ExitCode.input (String.concat " " args)
                use doc = json r
                Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) code ""

        testCase "find: matches exit 0, nothing found exits 1 (like grep)"
        <| fun () ->
            use t = project ()
            let r = tess [ "find"; t.Path; "SUM(" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "measure:Sales/Total  expression  SUM(Sales[Amount])" ""
            Expect.equal (tess [ "find"; t.Path; "no-such-text" ]).Code 1 ""

        testCase "find --json, -k, --in, -E"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "find"
                    t.Path
                    "^sum"
                    "-E"
                    "-k"
                    "measure"
                    "--in"
                    "expression"
                    "--json"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let hits = doc.RootElement.EnumerateArray() |> List.ofSeq
            Expect.hasLength hits 1 ""
            Expect.equal (hits.[0].GetProperty("ref").GetString()) "measure:Sales/Total" ""
            Expect.equal (hits.[0].GetProperty("line").GetInt32()) 1 ""

        testCase "find with a bad regex is a usage error"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "find"; t.Path; "("; "-E" ]).Code ExitCode.usage ""

        testCase "diff: identical exits 0, different exits 1"
        <| fun () ->
            use t = project ()
            let bim = t.Combine "m.bim"
            Expect.equal (tess [ "convert"; t.Path; bim ]).Code ExitCode.ok ""
            let sm = t.Combine "Sales.SemanticModel"
            let same = tess [ "diff"; sm; bim ]
            Expect.equal same.Code ExitCode.ok same.Err
            Expect.stringContains same.Out "No differences." ""

            let edited = t.Combine "edited"
            Expect.equal (tess [ "convert"; sm; edited ]).Code ExitCode.ok ""
            let salesFile = Path.Combine(edited, "tables", "Sales.tmdl")

            File.WriteAllText(
                salesFile,
                File.ReadAllText(salesFile).Replace("SUM(Sales[Amount])", "SUMX(Sales, Sales[Amount])")
            )

            let diff = tess [ "diff"; sm; edited ]
            Expect.equal diff.Code ExitCode.findings diff.Err
            Expect.stringContains diff.Out "~ measure:Sales/Total" ""
            Expect.stringContains diff.Out "expression: \"SUM(Sales[Amount])\" -> \"SUMX(Sales, Sales[Amount])\"" ""
            Expect.stringContains diff.Out "1 modified" ""

            let ignored = tess [ "diff"; sm; edited; "--ignore"; "expression"; "--json" ]
            Expect.equal ignored.Code ExitCode.ok ignored.Err
            use doc = json ignored
            Expect.equal (doc.RootElement.GetProperty("summary").GetProperty("modified").GetInt32()) 0 ""

        testCase "diff shows multi-line changes as a line diff"
        <| fun () ->
            use t = project ()
            let edited = t.Combine "edited"
            Expect.equal (tess [ "convert"; t.Path; edited ]).Code ExitCode.ok ""
            let salesFile = Path.Combine(edited, "tables", "Sales.tmdl")

            File.WriteAllText(
                salesFile,
                File
                    .ReadAllText(salesFile)
                    .Replace("= SUM(Sales[Amount])", "=\n\t\t\tSUMX(\n\t\t\t\tSales,\n\t\t\t\tSales[Amount])")
            )

            let diff = tess [ "diff"; t.Path; edited ]
            Expect.stringContains diff.Out "    expression:\n      - SUM(Sales[Amount])\n      + SUMX(" diff.Out

        testCase "deps: both directions by default"
        <| fun () ->
            use t = project ()
            let r = tess [ "deps"; t.Path; "Sales[DateKey]" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "depends on\n    (nothing)" ""
            Expect.stringContains r.Out "relationship:Sales-Date  fromColumn" ""

        testCase "deps --json --depends-on"
        <| fun () ->
            use t = project ()

            let r =
                tess [ "deps"; t.Path; "calcitem:Time Intelligence/YTD"; "--depends-on"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let mutable referencedBy = Unchecked.defaultof<JsonElement>
            Expect.isFalse (doc.RootElement.TryGetProperty("referencedBy", &referencedBy)) ""

            let refs =
                doc.RootElement.GetProperty("dependsOn").EnumerateArray()
                |> Seq.map (fun n -> n.GetProperty("ref").GetString())
                |> List.ofSeq

            Expect.equal refs [ "column:Date/DateKey" ] ""

        testCase "deps rejects a negative depth"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "deps"; t.Path; "[Total]"; "-d"; "-1" ]).Code ExitCode.usage ""
    ]
