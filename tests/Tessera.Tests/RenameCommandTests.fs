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
        testCase "a dry run by default: shows the change and writes nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "rename"; t.Path; "[Total]"; "Total Sales" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "> measure:Sales/Total -> measure:Sales/Total Sales" ""
            Expect.stringContains r.Out "~ measure:Sales/Total (hidden)" ""
            Expect.stringContains r.Out "expression: \"[Total]\" -> \"[Total Sales]\"" ""
            Expect.stringContains r.Err "updated 1 expression in 1 object" ""
            Expect.stringContains r.Err "Dry run - nothing saved. Add --save to persist." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "--save writes a PBIP folder back in place, and it reloads"
        <| fun () ->
            use t = project ()
            let pbism = File.ReadAllText(t.Combine("Sales.SemanticModel", "definition.pbism"))
            let r = tess [ "rename"; t.Path; "'Sales'[Total]"; "Total Sales"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "Saved pbip" ""

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
            let r = tess [ "rename"; t.Path; "Date"; "Calendar Date"; "--save" ]
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

            let r = tess [ "rename"; bim; "[Total]"; "T"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let text = File.ReadAllText bim
            Expect.stringContains text "\r\n" "keeps CRLF"
            Expect.isFalse (text.Replace("\r\n", "").Contains "\n") "only CRLF"
            Expect.isTrue ((load bim).Model.Tables.["Sales"].Measures.Contains "T") ""

            let tmdl = t.Combine "tmdl"

            ModelIO.save ModelIO.TmdlFolder ModelIO.SaveOptions.defaults false (sampleDatabase ()) tmdl
            |> ignore

            Expect.equal (tess [ "rename"; tmdl; "Sales[Amount]"; "Net"; "--save" ]).Code ExitCode.ok ""
            Expect.isFalse (File.Exists(Path.Combine(tmdl, "definition.pbism"))) "not turned into PBIP"
            Expect.equal (load tmdl).Model.Tables.["Sales"].Measures.["Total"].Expression "SUM(Sales[Net])" ""

        testCase "--save-to writes elsewhere and leaves the model alone"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            use o = new TempDir()
            let out = o.Combine "renamed.bim"
            let r = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--save-to"; out ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (snapshot t.Path) before "source untouched"
            Expect.isTrue ((load out).Model.Tables.["Sales"].Measures.Contains "Total Sales") ""

            let again = tess [ "rename"; t.Path; "[Total]"; "X"; "--save-to"; out ]
            Expect.equal again.Code ExitCode.output "won't overwrite without --force"

            Expect.equal (tess [ "rename"; t.Path; "[Total]"; "X"; "--save-to"; out; "--force" ]).Code ExitCode.ok ""

        testCase "--save and --save-to, --stat and --name-only don't combine"
        <| fun () ->
            use t = project ()

            Expect.equal
                (tess [ "rename"; t.Path; "[Total]"; "X"; "--save"; "--save-to"; t.Combine "o" ]).Code
                ExitCode.usage
                ""

            Expect.equal (tess [ "rename"; t.Path; "[Total]"; "X"; "--stat"; "--name-only" ]).Code ExitCode.usage ""

        testCase "--stat and --name-only"
        <| fun () ->
            use t = project ()
            let stat = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--stat" ]
            Expect.stringContains stat.Out "~ measure:Sales/Total (hidden)" ""
            Expect.stringContains stat.Out "1 property" ""
            Expect.stringContains stat.Out "1 renamed, 1 modified" ""
            let names = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--name-only" ]

            Expect.equal
                (names.Out.Trim().Split('\n') |> List.ofArray)
                [ "measure:Sales/Total Sales"; "measure:Sales/Total (hidden)" ]
                ""

        testCase "the same name does nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "rename"; t.Path; "[Total]"; "Total"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "No changes." ""
            Expect.stringContains r.Err "already named 'Total'" ""
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
                (tess ([ "rename"; t.Path ] @ args @ [ "--save" ])).Code

            Expect.equal (code [ "[Total]"; "amount" ]) ExitCode.input "collision"
            Expect.equal (code [ "[Nope]"; "X" ]) ExitCode.input "no such object"
            Expect.equal (code [ "model"; "X" ]) ExitCode.input "the model"
            Expect.equal (code [ "[Total]"; " " ]) ExitCode.usage "invalid name"
            Expect.equal (code [ "function:Double"; "a b" ]) ExitCode.usage "invalid function name"
            Expect.equal (code [ "[Total]" ]) ExitCode.usage "missing new name"
            Expect.equal (snapshot t.Path) before "failures write nothing"

        testCase "--json: the shared change-set shape"
        <| fun () ->
            use t = project ()
            let dry = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--json" ]
            Expect.equal dry.Code ExitCode.ok dry.Err
            use doc = json dry
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "rename" ""
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            let changes = root.GetProperty("changes").EnumerateArray() |> List.ofSeq

            let moved =
                changes
                |> List.find (fun c -> c.GetProperty("changeKind").GetString() = "moved")

            Expect.equal (moved.GetProperty("objectPath").GetString()) "measure:Sales/Total Sales" ""
            Expect.equal (moved.GetProperty("movedFromObjectPath").GetString()) "measure:Sales/Total" ""
            Expect.equal (moved.GetProperty("objectType").GetString()) "Measure" ""

            let modified =
                changes
                |> List.find (fun c -> c.GetProperty("changeKind").GetString() = "modified")

            Expect.equal (modified.GetProperty("objectPath").GetString()) "measure:Sales/Total (hidden)" ""
            let p = modified.GetProperty("properties").[0]
            Expect.equal (p.GetProperty("property").GetString()) "expression" ""
            Expect.equal (p.GetProperty("before").GetString()) "[Total]" ""
            Expect.equal (p.GetProperty("after").GetString()) "[Total Sales]" ""
            let summary = root.GetProperty "summary"
            Expect.equal (summary.GetProperty("moved").GetInt32()) 1 ""
            Expect.equal (summary.GetProperty("modified").GetInt32()) 1 ""
            Expect.equal (root.GetProperty("introducedErrors").GetArrayLength()) 0 ""
            Expect.equal (root.GetProperty("saved").ValueKind) System.Text.Json.JsonValueKind.Null ""

            let real = tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--json"; "--save" ]
            Expect.equal real.Code ExitCode.ok real.Err
            use doc = json real
            let saved = doc.RootElement.GetProperty "saved"
            Expect.equal (saved.GetProperty("format").GetString()) "pbip" ""
            Expect.equal (saved.GetProperty("path").GetString()) (Path.GetFullPath(semanticModel t)) ""

            let failed = tess [ "rename"; t.Path; "[Total (hidden)]"; "total sales"; "--json" ]
            Expect.equal failed.Code ExitCode.input ""
            use doc = json failed
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "name-collision" ""
    ]
