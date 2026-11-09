/// Case-only renames in TMDL/PBIP folders (Sales -> SALES): the file is renamed and the object is never
/// lost, also on case-insensitive file systems (Windows, macOS), where writing SALES.tmdl lands on
/// Sales.tmdl and a case-blind stale-file cleanup would then delete it.
module Tessera.Tests.CaseRenameTests

open System.IO
open Expecto
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

/// File names in a folder exactly as the file system stores them.
let private namesIn (dir: string) =
    Directory.GetFiles dir
    |> Array.map Path.GetFileName
    |> Array.sort
    |> List.ofArray

[<Tests>]
let tests =
    testList "case-only rename" [
        testCase "stale TMDL files are found by exact-case path"
        <| fun () ->
            let stale =
                ModelIO.staleTmdlFiles [
                    "database.tmdl"
                    "tables/Sales.tmdl"
                    "tables/Date.tmdl"
                    "notes.txt"
                    "extra/x.tmdl"
                ] [ "database.tmdl"; "tables/SALES.tmdl"; "tables/Date.tmdl" ]

            Expect.equal stale [ "extra/x.tmdl"; "tables/Sales.tmdl" ] "only .tmdl files, compared by exact case"

        testCase "a table in a PBIP folder: the file is renamed and the table reloads"
        <| fun () ->
            use t = project ()
            let r = tess [ "rename"; t.Path; "Sales"; "SALES"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err

            let tables = t.Combine("Sales.SemanticModel", "definition", "tables")
            let names = namesIn tables
            Expect.contains names "SALES.tmdl" "the file has the new casing"
            Expect.isFalse (List.contains "Sales.tmdl" names) "the old casing is gone"

            let db = load t.Path
            let sales = db.Model.Tables.Find "SALES"
            Expect.isNotNull sales "the table survives"
            Expect.equal sales.Name "SALES" "with its new name"
            Expect.isTrue (sales.Measures.Contains "Total") "and its measures"
            Expect.equal sales.Measures.["Total"].Expression "SUM(SALES[Amount])" "references follow"

        testCase "a table in a plain TMDL folder"
        <| fun () ->
            use t = new TempDir()
            let tmdl = t.Combine "tmdl"

            ModelIO.save ModelIO.TmdlFolder ModelIO.SaveOptions.defaults false (sampleDatabase ()) tmdl
            |> ignore

            let r = tess [ "rename"; tmdl; "Date"; "DATE"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let names = namesIn (Path.Combine(tmdl, "tables"))
            Expect.contains names "DATE.tmdl" ""
            Expect.isFalse (List.contains "Date.tmdl" names) ""
            let db = load tmdl
            Expect.equal (db.Model.Tables.Find "DATE").Name "DATE" ""
            Expect.equal (db.Model.Tables.["DATE"].Hierarchies.["Calendar"].Levels.Count) 2 "hierarchy intact"

        testCase "a role: its own file is renamed"
        <| fun () ->
            use t = project ()
            let db = load t.Path
            let roles = db.Model.Roles |> Seq.map (fun r -> r.Name) |> List.ofSeq
            let role = List.head roles
            let upper = role.ToUpperInvariant()
            Expect.notEqual upper role "the sample role isn't already upper case"
            let r = tess [ "rename"; t.Path; $"role:{role}"; upper; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let names = namesIn (t.Combine("Sales.SemanticModel", "definition", "roles"))
            Expect.contains names $"{upper}.tmdl" ""
            Expect.isFalse (List.contains $"{role}.tmdl" names) ""
            Expect.isNotNull ((load t.Path).Model.Roles.Find upper) ""

        testCase "measures and columns (inside their table's file)"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "rename"; t.Path; "[Total]"; "TOTAL"; "--save" ]).Code ExitCode.ok ""
            Expect.equal (tess [ "rename"; t.Path; "Sales[Amount]"; "AMOUNT"; "--save" ]).Code ExitCode.ok ""
            let names = namesIn (t.Combine("Sales.SemanticModel", "definition", "tables"))
            Expect.contains names "Sales.tmdl" "the table file stays"
            let sales = (load t.Path).Model.Tables.["Sales"]
            Expect.equal sales.Measures.["TOTAL"].Name "TOTAL" ""
            Expect.equal sales.Measures.["TOTAL"].Expression "SUM(Sales[AMOUNT])" ""
            Expect.equal sales.Measures.["Total (hidden)"].Expression "[TOTAL]" ""
            Expect.equal sales.Columns.["AMOUNT"].Name "AMOUNT" ""

        testCase "files other than .tmdl are left alone"
        <| fun () ->
            use t = project ()
            let definition = t.Combine("Sales.SemanticModel", "definition")
            write (Path.Combine(definition, "tables", "notes.txt")) "keep"
            Expect.equal (tess [ "rename"; t.Path; "Sales"; "SALES"; "--save" ]).Code ExitCode.ok ""
            Expect.isTrue (File.Exists(Path.Combine(definition, "tables", "notes.txt"))) ""
            Expect.isTrue (File.Exists(Path.Combine(definition, "tables", "SALES.tmdl"))) ""

        testCase "saving over a TMDL folder removes stray .tmdl files and the folders they leave empty"
        <| fun () ->
            use t = new TempDir()
            let tmdl = t.Combine "tmdl"

            let save () =
                ModelIO.save ModelIO.TmdlFolder ModelIO.SaveOptions.defaults true (sampleDatabase ()) tmdl

            Expect.isOk (save ()) ""
            write (Path.Combine(tmdl, "extra", "deep", "stray.tmdl")) "x"
            write (Path.Combine(tmdl, "tables", "Old.tmdl")) "x"
            write (Path.Combine(tmdl, "keep", "readme.md")) "x"
            Expect.isOk (save ()) ""
            Expect.isFalse (Directory.Exists(Path.Combine(tmdl, "extra"))) "emptied folders go"
            Expect.isFalse (File.Exists(Path.Combine(tmdl, "tables", "Old.tmdl"))) ""
            Expect.isTrue (File.Exists(Path.Combine(tmdl, "keep", "readme.md"))) "other files stay"
            Expect.isTrue (File.Exists(Path.Combine(tmdl, "tables", "Sales.tmdl"))) ""

        testCase "an unchanged file is not rewritten"
        <| fun () ->
            use t = project ()
            let date = t.Combine("Sales.SemanticModel", "definition", "tables", "Date.tmdl")
            let stamp = System.DateTime(2001, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
            File.SetLastWriteTimeUtc(date, stamp)
            Expect.equal (tess [ "rename"; t.Path; "[Total]"; "Total Sales"; "--save" ]).Code ExitCode.ok ""
            Expect.equal (File.GetLastWriteTimeUtc date) stamp "Date.tmdl has the same content: not touched"
    ]
