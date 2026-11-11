/// `tess convert` to and from Tabular Editor's database.json folders, and other commands on them.
module Tessera.Tests.FolderFormatCommandTests

open System.IO
open Expecto
open Tessera.Cli
open Tessera.Cli.Common
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness

[<Tests>]
let tests =
    testList "CLI database.json" [
        testCase "convert --serialization database.json, and back"
        <| fun () ->
            use t = project ()
            let folder = t.Combine "folder"

            let r =
                tess [ "convert"; t.Path; folder; "--serialization"; "database.json"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let output = doc.RootElement.GetProperty "output"
            Expect.equal (output.GetProperty("format").GetString()) "database.json" ""
            Expect.isTrue (File.Exists(Path.Combine(folder, "database.json"))) ""
            Expect.isTrue (File.Exists(Path.Combine(folder, "tables", "Sales", "measures", "Total.json"))) ""

            let back =
                tess [
                    "convert"
                    Path.Combine(folder, "database.json")
                    t.Combine "back.bim"
                    "--json"
                ]

            Expect.equal back.Code ExitCode.ok back.Err
            use backDoc = json back

            Expect.equal
                (backDoc.RootElement.GetProperty("source").GetProperty("format").GetString())
                "database.json"
                ""

        testCase "-f database.json and a destination ending in database.json both mean the folder"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "convert"; t.Path; t.Combine "a"; "-f"; "database.json" ]).Code ExitCode.ok ""
            let r = tess [ "convert"; t.Path; t.Combine("b", "database.json") ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.isTrue (File.Exists(t.Combine("b", "database.json"))) ""
            Expect.isFalse (Directory.Exists(t.Combine("b", "database.json"))) ""

        testCase "an unknown format lists the formats"
        <| fun () ->
            use t = project ()
            let r = tess [ "convert"; t.Path; t.Combine "x"; "--serialization"; "folder" ]
            Expect.equal r.Code ExitCode.usage ""
            Expect.stringContains r.Err "bim, tmdl, pbip, database.json" ""

        testCase "info and set --save work on a folder, and the save keeps its format"
        <| fun () ->
            use t = project ()
            let folder = t.Combine "folder"
            Expect.equal (tess [ "convert"; t.Path; folder; "-f"; "database.json" ]).Code ExitCode.ok ""
            let info = tess [ "info"; folder ]
            Expect.equal info.Code ExitCode.ok info.Err
            Expect.stringContains info.Out "database.json" ""

            let set = tess [ "set"; folder; "[Total]"; "-p"; "formatString=0.0"; "--save" ]

            Expect.equal set.Code ExitCode.ok set.Err

            Expect.stringContains
                (File.ReadAllText(Path.Combine(folder, "tables", "Sales", "measures", "Total.json")))
                "\"formatString\": \"0.0\""
                ""

            Expect.isFalse (File.Exists(Path.Combine(folder, "model.tmdl"))) "no TMDL written into the folder"

        testCase "converting into an existing folder needs --force, and keeps unrelated files"
        <| fun () ->
            use t = project ()
            let folder = t.Combine "folder"
            Expect.equal (tess [ "convert"; t.Path; folder; "-f"; "database.json" ]).Code ExitCode.ok ""
            write (Path.Combine(folder, "README.md")) "notes"
            Expect.equal (tess [ "convert"; t.Path; folder ]).Code ExitCode.output "refused without --force"

            Expect.equal
                (tess [ "convert"; t.Path; folder; "--force" ]).Code
                ExitCode.ok
                "the existing format is inferred"

            Expect.isTrue (File.Exists(Path.Combine(folder, "README.md"))) ""
            Expect.isTrue (File.Exists(Path.Combine(folder, "database.json"))) ""
    ]
