module Tessera.Tests.InitTests

open System.IO
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.ModelIO
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "unexpected error: %s" (TessError.message e)

let private opts name = {
    Scaffold.defaults name with
        Name = name
}

let private reload path = loadPath path |> ok |> snd

[<Tests>]
let tests =
    testList "Scaffold" [
        testList "nameFor" [
            for path, expected in
                [
                    "Sales.SemanticModel", "Sales"
                    "Sales.semanticmodel/", "Sales"
                    "models/Finance.bim", "Finance"
                    "tmdl/Finance", "Finance"
                    "My Model.SemanticModel", "My Model"
                    ".SemanticModel", Scaffold.fallbackName
                ] do
                testCase path <| fun () -> Expect.equal (Scaffold.nameFor path) expected ""
        ]

        testCase "tryCulture canonicalises specific cultures and rejects the rest"
        <| fun () ->
            Expect.equal (Scaffold.tryCulture "en-us") (Some "en-US") ""
            Expect.equal (Scaffold.tryCulture "da-DK") (Some "da-DK") ""

            for bad in [ "en"; ""; "xx-Nowhere" ] do
                Expect.isNone (Scaffold.tryCulture bad) bad

        testCase "a new model has Power BI Desktop's model properties"
        <| fun () ->
            let db = Scaffold.create (opts "Sales") |> Result.defaultWith failtest
            Expect.equal db.CompatibilityLevel Scaffold.defaultCompatibilityLevel ""
            Expect.isTrue (System.String.IsNullOrEmpty db.Name) "database stays unnamed, as Desktop leaves it"
            let m = db.Model
            Expect.equal m.Name "Sales" ""
            Expect.equal m.Culture "en-US" ""
            Expect.equal m.SourceQueryCulture "en-US" ""
            Expect.equal m.DefaultPowerBIDataSourceVersion PowerBIDataSourceVersion.PowerBI_V3 ""
            Expect.isTrue m.DiscourageImplicitMeasures ""
            Expect.isTrue m.DataAccessOptions.LegacyRedirects ""
            Expect.isTrue m.DataAccessOptions.ReturnErrorValuesAsNull ""
            Expect.equal m.Tables.Count 0 ""

        testCase "model.tmdl matches what Power BI Desktop writes"
        <| fun () ->
            use t = new TempDir()
            Scaffold.init TmdlFolder (opts "Sales") false (t.Combine "m") |> ok |> ignore

            Expect.equal
                (File.ReadAllText(t.Combine("m", "model.tmdl")).TrimEnd())
                ("model Sales\n"
                 + "\tculture: en-US\n"
                 + "\tdefaultPowerBIDataSourceVersion: powerBI_V3\n"
                 + "\tdiscourageImplicitMeasures\n"
                 + "\tsourceQueryCulture: en-US\n"
                 + "\tdataAccessOptions\n"
                 + "\t\tlegacyRedirects\n"
                 + "\t\treturnErrorValuesAsNull")
                ""

            Expect.equal
                (File.ReadAllText(t.Combine("m", "database.tmdl")).TrimEnd())
                "database\n\tcompatibilityLevel: 1702"
                ""

        testCase "every format reloads with the same model"
        <| fun () ->
            use t = new TempDir()

            let canonical path =
                toBimText SaveOptions.defaults (reload path)

            let paths = [
                TmdlFolder, t.Combine "tmdl"
                BimFile, t.Combine "Sales.bim"
                PbipFolder, t.Combine "Sales.SemanticModel"
            ]

            for format, path in paths do
                let saved = Scaffold.init format (opts "Sales") false path |> ok
                Expect.equal saved.Format format ""
                let src, db = loadPath path |> ok
                let info = Inventory.info src db
                Expect.equal info.Name "Sales" path
                Expect.equal info.CompatibilityLevel 1702 path
                Expect.equal info.CompatibilityMode "PowerBI" path
                Expect.equal info.Culture (Some "en-US") path

            let texts = paths |> List.map (snd >> canonical) |> List.distinct
            Expect.hasLength texts 1 "tmdl, bim and pbip hold the same model"

        testCase "pbip gets its support files"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "Sales.SemanticModel"
            Scaffold.init PbipFolder (opts "Sales") false dir |> ok |> ignore

            for f in
                [
                    "definition.pbism"
                    ".platform"
                    "definition/database.tmdl"
                    "definition/model.tmdl"
                ] do
                Expect.isTrue (File.Exists(Path.Combine(dir, f))) f

            Expect.stringContains (File.ReadAllText(Path.Combine(dir, ".platform"))) "\"displayName\": \"Sales\"" ""

        testCase "custom level and culture"
        <| fun () ->
            use t = new TempDir()

            let o = {
                opts "Sales" with
                    CompatibilityLevel = 1600
                    Culture = "da-DK"
            }

            Scaffold.init BimFile o false (t.Combine "m.bim") |> ok |> ignore
            let db = reload (t.Combine "m.bim")
            Expect.equal db.CompatibilityLevel 1600 ""
            Expect.equal db.Model.Culture "da-DK" ""
            Expect.equal db.Model.SourceQueryCulture "da-DK" ""
            Expect.equal (effectiveCompatibilityMode db) Microsoft.AnalysisServices.CompatibilityMode.PowerBI ""

        testCase "a level TOM rejects is an error, not an exception"
        <| fun () ->
            match
                Scaffold.create {
                    opts "Sales" with
                        CompatibilityLevel = 1100
                }
            with
            | Ok _ -> failtest "expected an error"
            | Error reason -> Expect.isNotEmpty reason ""

        testCase "refuses a non-empty destination unless forced"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "m"
            write (Path.Combine(dir, "notes.txt")) "keep me"

            match Scaffold.init TmdlFolder (opts "Sales") false dir with
            | Error(DestinationNotEmpty _) -> ()
            | other -> failtestf "expected DestinationNotEmpty, got %A" other

            Scaffold.init TmdlFolder (opts "Sales") true dir |> ok |> ignore
            Expect.isTrue (File.Exists(Path.Combine(dir, "model.tmdl"))) ""
            Expect.equal (File.ReadAllText(Path.Combine(dir, "notes.txt"))) "keep me" "unrelated files are left alone"
    ]
