module Tessera.Tests.AddCommandTests

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

let private changes (r: Run) =
    let doc = json r

    [
        for c in doc.RootElement.GetProperty("changes").EnumerateArray() ->
            c.GetProperty("objectPath").GetString(), c.GetProperty("changeKind").GetString()
    ]

[<Tests>]
let tests =
    testList "CLI add" [
        testCase "a dry run by default: shows the new object and writes nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            let r = tess [ "add"; t.Path; "measure:Sales/Margin"; "-e"; "[Total] * 0.1" ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "+ measure:Sales/Margin" ""
            Expect.stringContains r.Err "Added measure:Sales/Margin" ""
            Expect.stringContains r.Err "Dry run - nothing saved. Add --save to persist." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "--save writes the PBIP back, and the object reloads"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "add"
                    t.Path
                    "measure:Sales/Margin"
                    "-e"
                    "[Total] * 0.1"
                    "--format-string"
                    "0.0%"
                    "--display-folder"
                    "KPIs"
                    "--description"
                    "Ten percent."
                    "--hidden"
                    "--save"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "Saved pbip" ""
            let m = (load t.Path).Model.Tables.["Sales"].Measures.["Margin"]
            Expect.equal m.Expression "[Total] * 0.1" ""
            Expect.equal m.FormatString "0.0%" ""
            Expect.equal m.DisplayFolder "KPIs" ""
            Expect.equal m.Description "Ten percent." ""
            Expect.isTrue m.IsHidden ""
            Expect.isFalse (System.String.IsNullOrEmpty m.LineageTag) "a lineage tag"

        testCase "--json: the change set shows the new objects as created"
        <| fun () ->
            use t = project ()

            let r =
                tess [ "add"; t.Path; "hierarchy:Date/By Month"; "--levels"; "Month,Year"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Out

            Expect.equal
                (changes r)
                [
                    "hierarchy:Date/By Month", "created"
                    "level:Date/By Month/Month", "created"
                    "level:Date/By Month/Year", "created"
                ]
                ""

            let doc = json r
            Expect.equal (doc.RootElement.GetProperty("command").GetString()) "add" ""
            Expect.isTrue (doc.RootElement.GetProperty("dryRun").GetBoolean()) ""
            Expect.equal (doc.RootElement.GetProperty("summary").GetProperty("created").GetInt32()) 3 ""

        testCase "a change that introduces errors is shown, but --save refuses it unless --force"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let r =
                tess [ "add"; t.Path; "measure:Sales/Broken"; "-e"; "SUM('Gone'[X])"; "--save" ]

            Expect.equal r.Code ExitCode.findings r.Err
            Expect.stringContains r.Out "+ measure:Sales/Broken" ""
            Expect.stringContains r.Err "this change introduces 1 error" ""
            Expect.stringContains r.Err "Not saved" ""
            Expect.equal (snapshot t.Path) before "nothing written"

            let r =
                tess [
                    "add"
                    t.Path
                    "measure:Sales/Broken"
                    "-e"
                    "SUM('Gone'[X])"
                    "--save"
                    "--json"
                ]

            Expect.equal r.Code ExitCode.findings r.Out
            let doc = json r

            Expect.equal
                (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                "change-introduces-errors"
                ""

            Expect.equal (doc.RootElement.GetProperty("introducedErrors").GetArrayLength()) 1 ""

            let r =
                tess [
                    "add"
                    t.Path
                    "measure:Sales/Broken"
                    "-e"
                    "SUM('Gone'[X])"
                    "--save"
                    "--force"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.isTrue ((load t.Path).Model.Tables.["Sales"].Measures.Contains "Broken") "forced"

        testCase "exit codes: usage errors 2, model refusals 3"
        <| fun () ->
            use t = project ()
            let code args = (tess ([ "add"; t.Path ] @ args)).Code
            Expect.equal (code [ "measure:Sales/X" ]) ExitCode.usage "no -e"
            Expect.equal (code [ "measure:Sales/X"; "-e"; "1"; "--levels"; "A" ]) ExitCode.usage "--levels on a measure"
            Expect.equal (code [ "measure"; "-e"; "1" ]) ExitCode.usage "no name"
            Expect.equal (code [ "column:Sales/X" ]) ExitCode.usage "neither -e nor --data-type"
            Expect.equal (code [ "column:Sales/X"; "--data-type"; "money" ]) ExitCode.usage "unknown data type"
            Expect.equal (code [ "table:T"; "-e"; "{1}"; "-m"; "x" ]) ExitCode.usage "two sources"
            Expect.equal (code [ "measure:Sales/X"; "-e"; "1"; "--save"; "--save-to"; "x.bim" ]) ExitCode.usage ""
            Expect.equal (code [ "measure:Sales/Total"; "-e"; "1" ]) ExitCode.input "collision"
            Expect.equal (code [ "measure:Sles/X"; "-e"; "1" ]) ExitCode.input "no such table"
            Expect.equal (code [ "column:Time Intelligence/X"; "-e"; "1" ]) ExitCode.input "calculation group column"
            Expect.equal (code [ "measure:Sales/ X"; "-e"; "1" ]) ExitCode.usage "invalid name"

        testCase "a missing table suggests a close one"
        <| fun () ->
            use t = project ()
            let r = tess [ "add"; t.Path; "measure:Sles/X"; "-e"; "1" ]
            Expect.stringContains r.Err "did you mean table:Sales?" ""
            let r = tess [ "add"; t.Path; "measure:Sles/X"; "-e"; "1"; "--json" ]

            Expect.equal
                ((json r).RootElement.GetProperty("error").GetProperty("code").GetString())
                "object-not-found"
                ""

        testCase "--if-not-exists: exit 0, a note, no changes"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let r =
                tess [ "add"; t.Path; "measure:Sales/Total"; "-e"; "1"; "--if-not-exists"; "--save" ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "No changes." ""
            Expect.stringContains r.Err "measure:Sales/Total already exists; nothing to do." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "relationship shorthand: no name, so a GUID, and the defaults"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "add"
                    t.Path
                    "relationship"
                    "--from"
                    "'Sales'[Amount]"
                    "--to"
                    "'Date'[Year]"
                    "--inactive"
                    "--save"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "+ relationship:Sales[Amount] -> Date[Year]" ""
            Expect.stringContains r.Err "different data types" ""

            let rel =
                (load t.Path).Model.Relationships |> Seq.find (fun r -> r.Name <> "Sales-Date")
                :?> SingleColumnRelationship

            Expect.isTrue (fst (System.Guid.TryParse rel.Name)) rel.Name
            Expect.isFalse rel.IsActive ""
            Expect.equal rel.ToCardinality RelationshipEndCardinality.One ""
            Expect.equal rel.CrossFilteringBehavior CrossFilteringBehavior.OneDirection ""

        testCase "every kind's options parse"
        <| fun () ->
            use t = project ()

            let add args =
                let r = tess ([ "add"; t.Path ] @ args @ [ "--save" ])
                Expect.equal r.Code ExitCode.ok $"{args}: {r.Err}"

            add [ "column:Sales/Qty"; "--data-type"; "int64"; "--source-column"; "qty" ]
            add [ "column:Sales/Double"; "-e"; "Sales[Amount] * 2"; "--format-string"; "#,0" ]
            add [ "table:Numbers"; "-e"; "GENERATESERIES(1, 3)" ]
            add [ "table:Budget"; "-m"; "let x = 1 in x"; "--mode"; "directquery" ]

            add [
                "table:Units"
                "--calculation-group"
                "--precedence"
                "3"
                "--column"
                "Unit"
            ]

            add [
                "calcitem:Units/Raw"
                "-e"
                "SELECTEDMEASURE()"
                "--format-string-expression"
                "\"0\""
                "--ordinal"
                "0"
            ]

            add [ "level:Date/Calendar/Key"; "--column"; "DateKey"; "--ordinal"; "0" ]

            add [
                "role:Danes"
                "--permission"
                "readrefresh"
                "--filter"
                "'Sales'=Sales[Amount] > 0"
                "--filter"
                "Date=TRUE()"
            ]

            add [ "perspective:Finance"; "--include"; "Sales"; "--include"; "'Date'[Year]" ]
            add [ "culture:fr-FR" ]
            add [ "expression:Database"; "-m"; "\"DW\""; "--description"; "the database" ]
            add [ "function:My.Triple"; "-e"; "(x: INT64) => x * 3" ]
            add [ "partition:Sales/Sales 2024"; "-m"; "let y = 2 in y" ]

            let db = load t.Path
            let m = db.Model
            Expect.equal (m.Tables.["Sales"].Columns.["Qty"] :?> DataColumn).SourceColumn "qty" ""
            Expect.equal m.Tables.["Budget"].Partitions.[0].Mode ModeType.DirectQuery ""
            Expect.equal m.Tables.["Units"].CalculationGroup.Precedence 3 ""
            Expect.isTrue (m.Tables.["Units"].Columns.Contains "Unit") ""
            Expect.equal m.Roles.["Danes"].ModelPermission ModelPermission.ReadRefresh ""
            Expect.equal m.Roles.["Danes"].TablePermissions.["Sales"].FilterExpression "Sales[Amount] > 0" ""
            Expect.equal m.Roles.["Danes"].TablePermissions.["Date"].FilterExpression "TRUE()" ""
            Expect.equal m.Perspectives.["Finance"].PerspectiveTables.Count 2 ""
            Expect.isTrue (m.Cultures.Contains "fr-FR") ""
            Expect.isTrue (m.Functions.Contains "My.Triple") ""
            Expect.equal m.Tables.["Sales"].Partitions.Count 2 ""

            let v = tess [ "validate"; t.Path ]
            Expect.equal v.Code ExitCode.ok $"{v.Out}{v.Err}"

        testCase "--save-to writes elsewhere"
        <| fun () ->
            use t = project ()
            let out = t.Combine "out.bim"
            let r = tess [ "add"; t.Path; "measure:Sales/Margin"; "-e"; "1"; "--save-to"; out ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.isTrue ((load out).Model.Tables.["Sales"].Measures.Contains "Margin") ""

            Expect.isFalse
                ((load (t.Combine "Sales.SemanticModel")).Model.Tables.["Sales"].Measures.Contains "Margin")
                "the source is untouched"

        testSequenced
        <| testList "stdin" [
            testCase "-e - reads the expression from stdin"
            <| fun () ->
                use t = project ()

                let r =
                    tessWithStdin "VAR x = [Total]\nRETURN x * 2\n" [
                        "add"
                        t.Path
                        "measure:Sales/Twice"
                        "-e"
                        "-"
                        "--save"
                    ]

                Expect.equal r.Code ExitCode.ok r.Err

                Expect.equal
                    (load t.Path).Model.Tables.["Sales"].Measures.["Twice"].Expression
                    "VAR x = [Total]\nRETURN x * 2"
                    "the trailing line break is dropped"

            testCase "-m - too; only one option reads stdin"
            <| fun () ->
                use t = project ()

                let r =
                    tessWithStdin "let x = 1 in x" [ "add"; t.Path; "table:Budget"; "-m"; "-"; "--save" ]

                Expect.equal r.Code ExitCode.ok r.Err
                let p = (load t.Path).Model.Tables.["Budget"].Partitions.[0]
                Expect.equal (p.Source :?> MPartitionSource).Expression "let x = 1 in x" ""

                let r =
                    tessWithStdin "x" [
                        "add"
                        t.Path
                        "calcitem:Time Intelligence/X"
                        "-e"
                        "-"
                        "--format-string-expression"
                        "-"
                    ]

                Expect.equal r.Code ExitCode.usage r.Err
        ]
    ]
