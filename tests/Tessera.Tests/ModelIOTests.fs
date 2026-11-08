module Tessera.Tests.ModelIOTests

open System
open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.ModelIO
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "unexpected error: %s" (TessError.message e)

let private canonical (db: Database) = toBimText SaveOptions.defaults db

let private reload path = loadPath path |> ok |> snd

let private saveTo format path db =
    save format SaveOptions.defaults false db path |> ok

[<Tests>]
let tests =
    testList "ModelIO" [
        testList "round trips" [
            testCase "bim -> tmdl -> bim is lossless"
            <| fun () ->
                use t = new TempDir()
                let original = sampleDatabase ()
                saveTo BimFile (t.Combine "a.bim") original |> ignore
                saveTo TmdlFolder (t.Combine "tmdl") (reload (t.Combine "a.bim")) |> ignore
                saveTo BimFile (t.Combine "b.bim") (reload (t.Combine "tmdl")) |> ignore
                Expect.equal (File.ReadAllText(t.Combine "b.bim")) (File.ReadAllText(t.Combine "a.bim")) ""
                Expect.equal (canonical (reload (t.Combine "b.bim"))) (canonical original) ""

            testCase "pbip -> bim -> pbip is lossless"
            <| fun () ->
                use t = new TempDir()
                let sm = saveTo PbipFolder (t.Combine "X.SemanticModel") (sampleDatabase ())
                saveTo BimFile (t.Combine "x.bim") (reload sm.Path) |> ignore

                let sm2 =
                    saveTo PbipFolder (t.Combine "Y.SemanticModel") (reload (t.Combine "x.bim"))

                Expect.equal (canonical (reload sm2.Path)) (canonical (reload sm.Path)) ""

            testCase "multi-line descriptions with leading whitespace survive (cf. TE2 #1321)"
            <| fun () ->
                use t = new TempDir()
                let expected = (sampleDatabase ()).Model.Tables.["Sales"].Description

                for format, path in
                    [
                        TmdlFolder, t.Combine "tmdl"
                        BimFile, t.Combine "m.bim"
                        PbipFolder, t.Combine "P.SemanticModel"
                    ] do
                    saveTo format path (sampleDatabase ()) |> ignore
                    Expect.equal (reload path).Model.Tables.["Sales"].Description expected (OutputFormat.token format)

            testCase "unnamed databases (Power BI Desktop TMDL) still produce a loadable .bim"
            <| fun () ->
                use t = new TempDir()
                let tmdl = t.Combine "tmdl"
                saveTo TmdlFolder tmdl (sampleDatabase ()) |> ignore
                // Power BI Desktop writes a bare `database` header, with no name.
                let header = Path.Combine(tmdl, "database.tmdl")
                let lines = File.ReadAllLines header

                File.WriteAllLines(
                    header,
                    lines |> Array.map (fun l -> if l.StartsWith "database" then "database" else l)
                )

                let db = reload tmdl
                Expect.isTrue (String.IsNullOrEmpty db.Name) "precondition: unnamed"
                saveTo BimFile (t.Combine "m.bim") db |> ignore
                Expect.equal (reload (t.Combine "m.bim")).Name "Model" "falls back to the model name"
                Expect.isTrue (String.IsNullOrEmpty db.Name) "the loaded database is not mutated"

            testCase "Power BI-only properties load from .bim (compatibility mode detection)"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase ()
                db.Model.DefaultPowerBIDataSourceVersion <- PowerBIDataSourceVersion.PowerBI_V3
                saveTo BimFile (t.Combine "m.bim") db |> ignore
                let back = reload (t.Combine "m.bim")
                Expect.equal back.Model.DefaultPowerBIDataSourceVersion PowerBIDataSourceVersion.PowerBI_V3 ""
                Expect.equal (effectiveCompatibilityMode back) Microsoft.AnalysisServices.CompatibilityMode.PowerBI ""

            testCase "effective compatibility mode is not written into TMDL"
            <| fun () ->
                use t = new TempDir()
                saveTo TmdlFolder (t.Combine "a") (sampleDatabase ()) |> ignore
                let db = reload (t.Combine "a")
                effectiveCompatibilityMode db |> ignore
                saveTo TmdlFolder (t.Combine "b") db |> ignore
                Expect.isFalse (File.ReadAllText(t.Combine("b", "database.tmdl")).Contains "compatibilityMode") ""
        ]

        testList "line endings" [
            testCase "Eol.ofText"
            <| fun () ->
                Expect.equal (Eol.ofText "a\nb\nc") (Some Lf) ""
                Expect.equal (Eol.ofText "a\r\nb\r\n") (Some CrLf) ""
                Expect.equal (Eol.ofText "a\r\nb\nc\r\n") (Some CrLf) "majority wins"
                Expect.equal (Eol.ofText "no breaks") None ""
                Expect.equal (Eol.ofText "") None ""

            testCase "auto: the destination's existing files win"
            <| fun () ->
                use t = new TempDir()
                saveTo TmdlFolder (t.Combine "lf-src") (sampleDatabase ()) |> ignore

                save
                    TmdlFolder
                    { SaveOptions.defaults with Eol = CrLf }
                    false
                    (sampleDatabase ())
                    (t.Combine "crlf-dest")
                |> ok
                |> ignore

                Expect.equal (Eol.resolveAuto (t.Combine "lf-src") (t.Combine "crlf-dest")) CrLf ""

            testCase "auto: falls back to the source, then LF"
            <| fun () ->
                use t = new TempDir()

                save BimFile { SaveOptions.defaults with Eol = CrLf } false (sampleDatabase ()) (t.Combine "m.bim")
                |> ok
                |> ignore

                Expect.equal (Eol.resolveAuto (t.Combine "m.bim") (t.Combine "new")) CrLf "source"
                Expect.equal (Eol.resolveAuto (t.Combine "missing.bim") (t.Combine "new")) Lf "default"

            testCase "auto: a PBIP destination is sampled under definition/"
            <| fun () ->
                use t = new TempDir()
                let sm = t.Combine "S.SemanticModel"

                save PbipFolder { SaveOptions.defaults with Eol = CrLf } false (sampleDatabase ()) sm
                |> ok
                |> ignore

                Expect.equal (Eol.detectAt sm) (Some CrLf) ""
        ]

        testList "options" [
            testCase "lf by default, crlf on request"
            <| fun () ->
                use t = new TempDir()
                saveTo TmdlFolder (t.Combine "lf") (sampleDatabase ()) |> ignore

                save TmdlFolder { SaveOptions.defaults with Eol = CrLf } false (sampleDatabase ()) (t.Combine "crlf")
                |> ok
                |> ignore

                let lf = File.ReadAllText(t.Combine("lf", "tables", "Sales.tmdl"))
                let crlf = File.ReadAllText(t.Combine("crlf", "tables", "Sales.tmdl"))
                Expect.isFalse (lf.Contains "\r") "lf output has no CR"
                Expect.isTrue (crlf.Contains "\r\n") "crlf output uses CRLF"
                Expect.equal (crlf.Replace("\r\n", "\n")) lf "same content otherwise"

            testCase "tabs by default, spaces on request"
            <| fun () ->
                use t = new TempDir()
                saveTo TmdlFolder (t.Combine "tabs") (sampleDatabase ()) |> ignore

                save
                    TmdlFolder
                    {
                        SaveOptions.defaults with
                            IndentSpaces = Some 4
                    }
                    false
                    (sampleDatabase ())
                    (t.Combine "spaces")
                |> ok
                |> ignore

                let tabs = File.ReadAllText(t.Combine("tabs", "tables", "Sales.tmdl"))
                let spaces = File.ReadAllText(t.Combine("spaces", "tables", "Sales.tmdl"))
                Expect.stringContains tabs "\n\tmeasure Total" ""
                Expect.stringContains spaces "\n    measure Total" ""

            testCase "no BOM"
            <| fun () ->
                use t = new TempDir()
                saveTo BimFile (t.Combine "m.bim") (sampleDatabase ()) |> ignore
                saveTo TmdlFolder (t.Combine "tmdl") (sampleDatabase ()) |> ignore

                for f in [ t.Combine "m.bim"; t.Combine("tmdl", "model.tmdl") ] do
                    let bytes = File.ReadAllBytes f
                    Expect.isFalse (bytes.Length >= 3 && bytes.[0] = 0xEFuy && bytes.[1] = 0xBBuy) f
        ]

        testList "destinations" [
            testCase "refuses a non-empty destination without force"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "out"
                write (Path.Combine(dir, "keep.txt")) "x"

                match save TmdlFolder SaveOptions.defaults false (sampleDatabase ()) dir with
                | Error(DestinationNotEmpty _) -> ()
                | other -> failtestf "%A" other

                Expect.isTrue (File.Exists(Path.Combine(dir, "keep.txt"))) "untouched"

            testCase "force overwrites, removes stale tables, keeps unrelated files"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "out"
                saveTo TmdlFolder dir (sampleDatabase ()) |> ignore
                write (Path.Combine(dir, "README.md")) "notes"
                let smaller = sampleDatabase ()
                smaller.Model.Relationships.Clear()
                smaller.Model.Tables.Remove(smaller.Model.Tables.["Date"]) |> ignore

                smaller.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["YTD"].Expression <-
                    "SELECTEDMEASURE()"

                save TmdlFolder SaveOptions.defaults true smaller dir |> ok |> ignore
                Expect.isFalse (File.Exists(Path.Combine(dir, "tables", "Date.tmdl"))) "stale table removed"
                Expect.isTrue (File.Exists(Path.Combine(dir, "README.md"))) "unrelated file kept"
                Expect.isFalse ((reload dir).Model.Tables.Contains "Date") ""

            testCase "a folder format onto an existing file is an error"
            <| fun () ->
                use t = new TempDir()
                write (t.Combine "file") "x"

                match save TmdlFolder SaveOptions.defaults true (sampleDatabase ()) (t.Combine "file") with
                | Error(SaveFailed _) -> ()
                | other -> failtestf "%A" other

            testCase "infers the format from the destination"
            <| fun () ->
                Expect.equal (OutputFormat.infer "x/model.bim") BimFile ""
                Expect.equal (OutputFormat.infer "x/Sales.SemanticModel/") PbipFolder ""
                Expect.equal (OutputFormat.infer "x/Sales.semanticmodel") PbipFolder "case-insensitive"
                Expect.equal (OutputFormat.infer "x/out") TmdlFolder ""

            testCase "re-serializing in place (source = destination, --force)"
            <| fun () ->
                use t = new TempDir()
                let expected = canonical (sampleDatabase ())

                for format, path in
                    [
                        TmdlFolder, t.Combine "tmdl"
                        PbipFolder, t.Combine "S.SemanticModel"
                        BimFile, t.Combine "m.bim"
                    ] do
                    saveTo format path (sampleDatabase ()) |> ignore
                    let platform = Path.Combine(path, ".platform")

                    let idBefore =
                        if File.Exists platform then
                            File.ReadAllText platform
                        else
                            ""

                    save format { SaveOptions.defaults with Eol = CrLf } true (reload path) path
                    |> ok
                    |> ignore

                    Expect.equal (canonical (reload path)) expected (OutputFormat.token format)

                    if File.Exists platform then
                        Expect.equal (File.ReadAllText platform) idBefore "logicalId kept"
        ]

        testList "PBIP support files" [
            testCase "new folders get definition.pbism and .platform"
            <| fun () ->
                use t = new TempDir()
                let sm = t.Combine "Sales.SemanticModel"
                saveTo PbipFolder sm (sampleDatabase ()) |> ignore

                use pbism =
                    JsonDocument.Parse(File.ReadAllText(Path.Combine(sm, "definition.pbism")))

                Expect.equal (pbism.RootElement.GetProperty("version").GetString()) Pbip.pbismVersion ""
                use platform = JsonDocument.Parse(File.ReadAllText(Path.Combine(sm, ".platform")))
                let meta = platform.RootElement.GetProperty "metadata"
                Expect.equal (meta.GetProperty("type").GetString()) "SemanticModel" ""
                Expect.equal (meta.GetProperty("displayName").GetString()) "Sales" ""

                let id =
                    platform.RootElement.GetProperty("config").GetProperty("logicalId").GetString()

                Expect.isTrue (fst (Guid.TryParse id)) "logicalId is a GUID"
                // and the result resolves as a PBIP model
                Expect.equal (ModelSource.resolve sm |> ok).SemanticModelFolder (Some sm) ""

            testCase "re-saving keeps the .platform logicalId and pbism settings"
            <| fun () ->
                use t = new TempDir()
                let sm = t.Combine "Sales.SemanticModel"
                saveTo PbipFolder sm (sampleDatabase ()) |> ignore
                write (Path.Combine(sm, "definition.pbism")) (pbism "4.2")
                let platformBefore = File.ReadAllText(Path.Combine(sm, ".platform"))
                save PbipFolder SaveOptions.defaults true (sampleDatabase ()) sm |> ok |> ignore
                Expect.equal (File.ReadAllText(Path.Combine(sm, ".platform"))) platformBefore ""
                Expect.stringContains (File.ReadAllText(Path.Combine(sm, "definition.pbism"))) "qnaEnabled" ""

            testCase "converting a model.bim PBIP to TMDL bumps an old pbism and removes model.bim"
            <| fun () ->
                use t = new TempDir()
                let sm = t.Combine "Legacy.SemanticModel"
                write (Path.Combine(sm, "definition.pbism")) (pbism "1.0")
                saveTo BimFile (Path.Combine(sm, "model.bim")) (sampleDatabase ()) |> ignore
                let db = reload sm
                save PbipFolder SaveOptions.defaults true db sm |> ok |> ignore
                Expect.isFalse (File.Exists(Path.Combine(sm, "model.bim"))) "model.bim removed"
                let text = File.ReadAllText(Path.Combine(sm, "definition.pbism"))
                use doc = JsonDocument.Parse text
                Expect.equal (doc.RootElement.GetProperty("version").GetString()) Pbip.pbismVersion "bumped"
                Expect.stringContains text "qnaEnabled" "other settings kept"
                Expect.equal (ModelSource.resolve sm |> ok).Format Tmdl ""
        ]
    ]
