module Tessera.Tests.ModelSourceTests

open System.IO
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private resolveOk path =
    match ModelSource.resolve path with
    | Ok s -> s
    | Error e -> failtestf "expected %s to resolve: %s" path (TessError.message e)

let private resolveErr path =
    match ModelSource.resolve path with
    | Ok s -> failtestf "expected %s to fail, got %A" path s
    | Error e -> e

let private writeBim (path: string) =
    write path (JsonSerializer.SerializeDatabase(sampleDatabase ()))

[<Tests>]
let tests =
    testList "ModelSource.resolve" [
        testCase "an empty path is an input error, not a crash"
        <| fun () ->
            for input in [ ""; "   " ] do
                match ModelSource.resolve input with
                | Error(PathNotFound _) -> ()
                | other -> failtestf "%A" other

        testCase "a .bim file"
        <| fun () ->
            use t = new TempDir()
            let bim = t.Combine "model.bim"
            writeBim bim
            let s = resolveOk bim
            Expect.equal s.Format Bim ""
            Expect.equal s.Path bim ""
            Expect.isNone s.SemanticModelFolder ""

        testCase "a TMDL root folder"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "tmdl"
            TmdlSerializer.SerializeDatabaseToFolder(sampleDatabase (), dir)
            let s = resolveOk dir
            Expect.equal s.Format Tmdl ""
            Expect.equal s.Path dir ""

        testCase "a .tmdl file deep inside a TMDL folder walks up to the root"
        <| fun () ->
            use t = new TempDir()
            let dir = t.Combine "tmdl"
            TmdlSerializer.SerializeDatabaseToFolder(sampleDatabase (), dir)
            let s = resolveOk (Path.Combine(dir, "tables", "Sales.tmdl"))
            Expect.equal s.Path dir ""

        testCase "a .SemanticModel folder, its definition.pbism, its definition/ folder"
        <| fun () ->
            use t = new TempDir()
            let sm = writePbipProject t.Path "Sales"
            let definition = Path.Combine(sm, "definition")

            for input in [ sm; Path.Combine(sm, "definition.pbism"); definition ] do
                let s = resolveOk input
                Expect.equal s.Format Tmdl input
                Expect.equal s.Path definition input
                Expect.equal s.SemanticModelFolder (Some sm) input

        testCase "a .pbip file follows report -> definition.pbir -> byPath"
        <| fun () ->
            use t = new TempDir()
            let sm = writePbipProject t.Path "Sales"
            let s = resolveOk (t.Combine "Sales.pbip")
            Expect.equal s.SemanticModelFolder (Some sm) ""

        testCase "a .Report folder and its definition.pbir"
        <| fun () ->
            use t = new TempDir()
            let sm = writePbipProject t.Path "Sales"
            Expect.equal (resolveOk (t.Combine "Sales.Report")).SemanticModelFolder (Some sm) ""
            Expect.equal (resolveOk (t.Combine("Sales.Report", "definition.pbir"))).SemanticModelFolder (Some sm) ""

        testCase "pbir paths with Windows separators"
        <| fun () ->
            use t = new TempDir()
            let sm = writePbipProject t.Path "Sales"
            write (t.Combine("Sales.Report", "definition.pbir")) (pbirByPath @"..\\Sales.SemanticModel")
            Expect.equal (resolveOk (t.Combine "Sales.pbip")).SemanticModelFolder (Some sm) ""

        testCase "a project root with a single semantic model"
        <| fun () ->
            use t = new TempDir()
            let sm = writePbipProject t.Path "Sales"
            Expect.equal (resolveOk t.Path).SemanticModelFolder (Some sm) ""

        testCase "a PBIP semantic model stored as model.bim"
        <| fun () ->
            use t = new TempDir()
            let sm = t.Combine "Legacy.SemanticModel"
            write (Path.Combine(sm, "definition.pbism")) (pbism "1.0")
            writeBim (Path.Combine(sm, "model.bim"))
            let s = resolveOk sm
            Expect.equal s.Format Bim ""
            Expect.equal s.SemanticModelFolder (Some sm) ""

        testCase "a model.bim beside definition.pbism reports its folder"
        <| fun () ->
            use t = new TempDir()
            let sm = t.Combine "Legacy.SemanticModel"
            write (Path.Combine(sm, "definition.pbism")) (pbism "1.0")
            writeBim (Path.Combine(sm, "model.bim"))
            Expect.equal (resolveOk (Path.Combine(sm, "model.bim"))).SemanticModelFolder (Some sm) ""

        testList "errors" [
            testCase "missing path"
            <| fun () ->
                use t = new TempDir()

                match resolveErr (t.Combine "nope") with
                | PathNotFound _ -> ()
                | e -> failtestf "%A" e

            testCase "empty folder"
            <| fun () ->
                use t = new TempDir()

                match resolveErr t.Path with
                | UnrecognizedModel _ -> ()
                | e -> failtestf "%A" e

            testCase "unsupported file"
            <| fun () ->
                use t = new TempDir()
                write (t.Combine "notes.txt") "hi"

                match resolveErr (t.Combine "notes.txt") with
                | UnrecognizedModel _ -> ()
                | e -> failtestf "%A" e

            testCase "two semantic models in one project root"
            <| fun () ->
                use t = new TempDir()
                writePbipProject t.Path "A" |> ignore
                writePbipProject t.Path "B" |> ignore

                match resolveErr t.Path with
                | AmbiguousModel(_, candidates) -> Expect.equal candidates.Length 2 ""
                | e -> failtestf "%A" e

            testCase "semantic model folder with both definition/ and model.bim"
            <| fun () ->
                use t = new TempDir()
                let sm = writePbipProject t.Path "Sales"
                writeBim (Path.Combine(sm, "model.bim"))

                match resolveErr sm with
                | AmbiguousModel _ -> ()
                | e -> failtestf "%A" e

            testCase "semantic model folder with neither"
            <| fun () ->
                use t = new TempDir()
                let sm = t.Combine "Empty.SemanticModel"
                write (Path.Combine(sm, "definition.pbism")) (pbism "4.2")

                match resolveErr sm with
                | InvalidPbip _ -> ()
                | e -> failtestf "%A" e

            testCase "report bound to a published model"
            <| fun () ->
                use t = new TempDir()
                write (t.Combine("Live.Report", "definition.pbir")) pbirByConnection
                write (t.Combine "Live.pbip") (pbipJson "Live.Report")

                match resolveErr (t.Combine "Live.pbip") with
                | RemoteModelReference _ -> ()
                | e -> failtestf "%A" e

            testCase "malformed .pbip"
            <| fun () ->
                use t = new TempDir()
                write (t.Combine "Bad.pbip") "{ not json"

                match resolveErr (t.Combine "Bad.pbip") with
                | InvalidPbip _ -> ()
                | e -> failtestf "%A" e
        ]
    ]
