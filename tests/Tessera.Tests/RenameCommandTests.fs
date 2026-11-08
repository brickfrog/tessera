module Tessera.Tests.RenameCommandTests

open System.IO
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// Every file under a folder, with its contents.
let private snapshot (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.sort
    |> Array.map (fun f -> f, File.ReadAllText f)

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

let private semanticModel (t: TempDir) = t.Combine "Sales.SemanticModel"

[<Tests>]
let tests =
    testList "CLI rename" [
        testCase "--dry-run shows the plan and writes nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--dry-run" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "Would rename measure:Sales/Total -> measure:Sales/Total Sales" ""
            Expect.stringContains r.Out "~ measure:Sales/Total (hidden)" ""
            Expect.stringContains r.Out "expression: \"[Total]\" -> \"[Total Sales]\"" ""
            Expect.stringContains r.Out "Would update 1 expression in 1 object" ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "saves a PBIP folder in place, and it reloads"
        <| fun () ->
            use t = project ()
            let pbism = File.ReadAllText(t.Combine("Sales.SemanticModel", "definition.pbism"))
            let r = tess [ "rename"; t.Path; "'Sales'[Total]"; "Total Sales" ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.stringContains
                r.Out
                "Renamed measure:Sales/Total -> measure:Sales/Total Sales; updated 1 expression in 1 object"
                ""

            let db = load t.Path
            let sales = db.Model.Tables.["Sales"]
            Expect.isTrue (sales.Measures.Contains "Total Sales") ""
            Expect.isFalse (sales.Measures.Contains "Total") ""
            Expect.equal sales.Measures.["Total (hidden)"].Expression "[Total Sales]" ""

            Expect.equal
                (File.ReadAllText(t.Combine("Sales.SemanticModel", "definition.pbism")))
                pbism
                "keeps the PBIP support files"

            Expect.isFalse (File.Exists(t.Combine("Sales.SemanticModel", "model.bim"))) "stays TMDL"

        testCase "a table rename renames its file; structural links follow"
        <| fun () ->
            use t = project ()
            let r = tess [ "rename"; t.Path; "Date"; "Calendar Date" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let tables = t.Combine("Sales.SemanticModel", "definition", "tables")
            Expect.isTrue (File.Exists(Path.Combine(tables, "Calendar Date.tmdl"))) ""
            Expect.isFalse (File.Exists(Path.Combine(tables, "Date.tmdl"))) "the stale file is removed"
            let db = load t.Path
            let rel = db.Model.Relationships.["Sales-Date"] :?> SingleColumnRelationship
            Expect.equal rel.ToTable.Name "Calendar Date" ""

            Expect.equal
                db.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["YTD"].Expression
                "TOTALYTD(SELECTEDMEASURE(), 'Calendar Date'[DateKey])"
                ""

        testCase ".bim and TMDL folders are saved in place in their own format and line endings"
        <| fun () ->
            use t = new TempDir()
            let bim = t.Combine "model.bim"

            ModelIO.save
                ModelIO.BimFile
                {
                    ModelIO.SaveOptions.defaults with
                        Eol = ModelIO.CrLf
                }
                false
                (sampleDatabase ())
                bim
            |> ignore

            let r = tess [ "rename"; bim; "[Total]"; "T" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let text = File.ReadAllText bim
            Expect.stringContains text "\r\n" "keeps CRLF"
            Expect.isFalse (text.Replace("\r\n", "").Contains "\n") "only CRLF"
            Expect.isTrue ((load bim).Model.Tables.["Sales"].Measures.Contains "T") ""

            let tmdl = t.Combine "tmdl"

            ModelIO.save ModelIO.TmdlFolder ModelIO.SaveOptions.defaults false (sampleDatabase ()) tmdl
            |> ignore

            Expect.equal (tess [ "rename"; tmdl; "Sales[Amount]"; "Net" ]).Code ExitCode.ok ""
            Expect.isFalse (File.Exists(Path.Combine(tmdl, "definition.pbism"))) "not turned into PBIP"
            Expect.equal (load tmdl).Model.Tables.["Sales"].Measures.["Total"].Expression "SUM(Sales[Net])" ""

        testCase "--output writes elsewhere and leaves the model alone"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            use o = new TempDir()
            let out = o.Combine "renamed.bim"
            let r = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--output"; out ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (snapshot t.Path) before "source untouched"
            Expect.isTrue ((load out).Model.Tables.["Sales"].Measures.Contains "Total Sales") ""

            let again = tess [ "rename"; t.Path; "[Total]"; "X"; "--output"; out ]
            Expect.equal again.Code ExitCode.output "won't overwrite without --force"

            Expect.equal (tess [ "rename"; t.Path; "[Total]"; "X"; "--output"; out; "--force" ]).Code ExitCode.ok ""

        testCase "the same name does nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "rename"; t.Path; "[Total]"; "Total" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "nothing to do" ""
            Expect.equal (snapshot t.Path) before ""

        testCase "named expressions warn that M isn't updated"
        <| fun () ->
            use t = project ()
            let r = tess [ "rename"; t.Path; "expression:ServerName"; "Server" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "warning: M references to 'ServerName' are not updated" ""

        testCase "exit codes"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let code args =
                (tess ([ "rename"; t.Path ] @ args)).Code

            Expect.equal (code [ "[Total]"; "amount" ]) ExitCode.input "collision"
            Expect.equal (code [ "[Nope]"; "X" ]) ExitCode.input "no such object"
            Expect.equal (code [ "model"; "X" ]) ExitCode.input "the model"
            Expect.equal (code [ "[Total]"; " " ]) ExitCode.usage "invalid name"
            Expect.equal (code [ "function:Double"; "a b" ]) ExitCode.usage "invalid function name"
            Expect.equal (code [ "[Total]" ]) ExitCode.usage "missing new name"
            Expect.equal (snapshot t.Path) before "failures write nothing"

        testCase "--json"
        <| fun () ->
            use t = project ()
            let dry = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--dry-run"; "--json" ]
            Expect.equal dry.Code ExitCode.ok dry.Err
            use doc = json dry
            let root = doc.RootElement
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            let rename = root.GetProperty "rename"
            Expect.equal (rename.GetProperty("kind").GetString()) "measure" ""
            Expect.equal (rename.GetProperty("before").GetString()) "measure:Sales/Total" ""
            Expect.equal (rename.GetProperty("after").GetString()) "measure:Sales/Total Sales" ""
            Expect.equal (rename.GetProperty("newName").GetString()) "Total Sales" ""
            let edit = root.GetProperty("edits").[0]
            Expect.equal (edit.GetProperty("ref").GetString()) "measure:Sales/Total (hidden)" ""
            Expect.equal (edit.GetProperty("property").GetString()) "expression" ""
            Expect.equal (edit.GetProperty("before").GetString()) "[Total]" ""
            Expect.equal (edit.GetProperty("after").GetString()) "[Total Sales]" ""
            Expect.equal (edit.GetProperty("references").GetInt32()) 1 ""
            Expect.equal (root.GetProperty("summary").GetProperty("expressions").GetInt32()) 1 ""
            Expect.equal (root.GetProperty("warnings").GetArrayLength()) 0 ""
            Expect.equal (root.GetProperty("output").ValueKind) System.Text.Json.JsonValueKind.Null ""

            let real = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--json" ]
            Expect.equal real.Code ExitCode.ok real.Err
            use doc = json real
            let output = doc.RootElement.GetProperty "output"
            Expect.equal (output.GetProperty("format").GetString()) "pbip" ""
            Expect.equal (output.GetProperty("path").GetString()) (Path.GetFullPath(semanticModel t)) ""

            let failed = tess [ "rename"; t.Path; "[Total (hidden)]"; "total sales"; "--json" ]
            Expect.equal failed.Code ExitCode.input ""
            use doc = json failed
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "name-collision" ""
    ]
