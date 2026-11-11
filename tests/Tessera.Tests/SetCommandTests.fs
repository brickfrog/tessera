module Tessera.Tests.SetCommandTests

open System
open System.IO
open System.Text.Json
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

let private errorCode (r: Run) =
    use doc = json r
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

[<Tests>]
let tests =
    testList "CLI set" [
        testCase "a dry run by default: shows the change and writes nothing"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let r =
                tess [ "set"; t.Path; "[Total]"; "-p"; "formatString=0.00"; "-p"; "isHidden=true" ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Out "~ measure:Sales/Total" ""
            Expect.stringContains r.Out "formatString: \"#,0\" -> \"0.00\"" ""
            Expect.stringContains r.Out "isHidden: (none) -> \"true\"" ""
            Expect.stringContains r.Err "Set formatString, isHidden on measure:Sales/Total" ""
            Expect.stringContains r.Err "Dry run - nothing saved. Add --save to persist." ""
            Expect.equal (snapshot t.Path) before "nothing written"

        testCase "--save writes back in place, and it reloads"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "set"
                    t.Path
                    "Sales[Amount]"
                    "-p"
                    "isHidden=true"
                    "-p"
                    "summarizeBy=none"
                    "-p"
                    "annotations.Owner=finance"
                    "--save"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "Saved pbip" ""
            let amount = (load t.Path).Model.Tables.["Sales"].Columns.["Amount"]
            Expect.isTrue amount.IsHidden ""
            Expect.equal amount.SummarizeBy AggregateFunction.None ""
            Expect.equal amount.Annotations.["Owner"].Value "finance" ""

            let r =
                tess [ "set"; t.Path; "[Total]"; "-p"; "kpi.targetExpression=100"; "--save" ]

            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (load t.Path).Model.Tables.["Sales"].Measures.["Total"].KPI.TargetExpression
                "100"
                "sub-objects too"

            let again =
                tess [ "set"; t.Path; "[Total]"; "-p"; "kpi.targetExpression=100"; "--save" ]

            Expect.stringContains again.Out "No changes." "the same value again"

        testCase "--unset and -p x=null clear"
        <| fun () ->
            use t = project ()

            let r =
                tess [
                    "set"
                    t.Path
                    "[Total]"
                    "--unset"
                    "formatString"
                    "-p"
                    "description=null"
                    "--save"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringContains r.Err "Cleared formatString, description" ""
            Expect.isTrue (String.IsNullOrEmpty (load t.Path).Model.Tables.["Sales"].Measures.["Total"].FormatString) ""

        testCase "--save-to writes elsewhere"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path
            use o = new TempDir()
            let out = o.Combine "set.bim"

            let r =
                tess [ "set"; t.Path; "[Total]"; "-p"; "displayFolder=KPIs"; "--save-to"; out ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (snapshot t.Path) before "source untouched"
            Expect.equal (load out).Model.Tables.["Sales"].Measures.["Total"].DisplayFolder "KPIs" ""

        testCase "--stat and --name-only"
        <| fun () ->
            use t = project ()

            let stat =
                tess [
                    "set"
                    t.Path
                    "[Total]"
                    "-p"
                    "formatString=0"
                    "-p"
                    "isHidden=true"
                    "--stat"
                ]

            Expect.stringContains stat.Out "~ measure:Sales/Total" ""
            Expect.stringContains stat.Out "2 properties" ""
            Expect.stringContains stat.Out "1 modified" ""
            let names = tess [ "set"; t.Path; "[Total]"; "-p"; "formatString=0"; "--name-only" ]
            Expect.equal (names.Out.Trim()) "measure:Sales/Total" ""

        testCase "--json: the shared change-set shape"
        <| fun () ->
            use t = project ()

            let r =
                tess [ "set"; t.Path; "Date[Month]"; "-p"; "sortByColumn=DateKey"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "set" ""
            Expect.isTrue (root.GetProperty("dryRun").GetBoolean()) ""
            let change = root.GetProperty("changes").[0]
            Expect.equal (change.GetProperty("objectPath").GetString()) "column:Date/Month" ""
            Expect.equal (change.GetProperty("changeKind").GetString()) "modified" ""
            let prop = change.GetProperty("properties").[0]
            Expect.equal (prop.GetProperty("property").GetString()) "sortByColumn" ""
            Expect.equal (prop.GetProperty("after").GetString()) "DateKey" ""
            Expect.equal (root.GetProperty("notes").[0].GetString()) "Set sortByColumn on column:Date/Month" ""
            Expect.isFalse (root.TryGetProperty "saved" |> fst) ""

        testCase "the error gate: broken DAX is refused without --force, saved with it"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let args = [
                "set"
                t.Path
                "[Total]"
                "-p"
                "expression=SUM('Missing'[Amount])"
                "--save"
            ]

            let refused = tess args
            Expect.equal refused.Code ExitCode.findings refused.Err
            Expect.stringContains refused.Err "this change introduces 1 error" ""
            Expect.stringContains refused.Err "unresolved-reference" ""
            Expect.stringContains refused.Err "Not saved" ""
            Expect.equal (snapshot t.Path) before "nothing written"

            let json = tess (args @ [ "--json" ])
            Expect.equal json.Code ExitCode.findings ""
            Expect.equal (errorCode json) "change-introduces-errors" ""

            let forced = tess (args @ [ "--force" ])
            Expect.equal forced.Code ExitCode.ok forced.Err

            Expect.equal (load t.Path).Model.Tables.["Sales"].Measures.["Total"].Expression "SUM('Missing'[Amount])" ""

        testCase "exit codes"
        <| fun () ->
            use t = project ()
            let before = snapshot t.Path

            let code args =
                (tess ([ "set"; t.Path ] @ args @ [ "--save" ])).Code

            Expect.equal (code [ "[Total]"; "-p"; "formatStrng=0" ]) ExitCode.usage "unknown property"
            Expect.equal (code [ "[Total]"; "-p"; "name=X" ]) ExitCode.usage "name"
            Expect.equal (code [ "Sales"; "-p"; "columns=X" ]) ExitCode.usage "children"
            Expect.equal (code [ "Sales[Amount]"; "-p"; "type=calculated" ]) ExitCode.usage "read-only"
            Expect.equal (code [ "[Total]"; "-p"; "isHidden=maybe" ]) ExitCode.usage "bad value"
            Expect.equal (code [ "[Total]"; "-p"; "isHidden" ]) ExitCode.usage "no ="
            Expect.equal (code [ "[Total]" ]) ExitCode.usage "nothing to set"
            Expect.equal (code [ "[Total]"; "-p"; "a=-"; "-p"; "b=-" ]) ExitCode.usage "stdin twice"
            Expect.equal (code [ "[Nope]"; "-p"; "isHidden=true" ]) ExitCode.input "no such object"
            Expect.equal (code [ "Date[Month]"; "-p"; "sortByColumn=Nope" ]) ExitCode.input "no such reference"
            Expect.equal (snapshot t.Path) before "failures write nothing"

        testCase "errors in JSON, with their codes"
        <| fun () ->
            use t = project ()

            let run args =
                tess ([ "set"; t.Path ] @ args @ [ "--json" ])

            let unknown = run [ "[Total]"; "-p"; "formatStrng=0" ]
            Expect.equal (errorCode unknown) "unknown-property" ""
            Expect.stringContains unknown.Out "did you mean 'formatString'?" ""
            Expect.equal (errorCode (run [ "[Total]"; "-p"; "name=X" ])) "read-only-property" ""
            Expect.equal (errorCode (run [ "[Total]"; "-p"; "isHidden=2" ])) "invalid-property-value" ""

        // Console.In is process-wide: keep this one off the parallel runner.
        testSequenced (
            testCase "-p <path>=- reads the value from stdin" (fun () ->
                use t = project ()
                let previous = Console.In

                try
                    Console.SetIn(new StringReader("SUMX(Sales, Sales[Amount])\n"))
                    let r = tess [ "set"; t.Path; "[Total]"; "-p"; "expression=-"; "--save" ]
                    Expect.equal r.Code ExitCode.ok r.Err
                finally
                    Console.SetIn previous

                Expect.equal
                    (load t.Path).Model.Tables.["Sales"].Measures.["Total"].Expression
                    "SUMX(Sales, Sales[Amount])"
                    "")
        )

        testList "get --properties" [
            testCase "an aligned table of writable properties"
            <| fun () ->
                use t = project ()
                let r = tess [ "get"; t.Path; "[Total]"; "--properties" ]
                Expect.equal r.Code ExitCode.ok r.Err
                let lines = r.Out.TrimEnd().Split('\n') |> List.ofArray

                let line (path: string) =
                    lines |> List.find (fun l -> l.StartsWith(path + " "))

                Expect.stringContains (line "formatString") "string" ""
                Expect.stringContains (line "formatString") "#,0" ""
                Expect.stringContains (line "kpi.targetExpression") "expression" ""
                Expect.isFalse (lines |> List.exists (fun l -> l.StartsWith "name ")) "name is read-only"
                // Columns line up: every type starts at the same offset.
                let offsets = lines |> List.map (fun l -> l.IndexOf("  ")) |> List.distinct
                Expect.isTrue (List.forall (fun o -> o > 0) offsets) ""

                let enum = tess [ "get"; t.Path; "Sales[Amount]"; "--properties" ]
                Expect.stringContains enum.Out "default|none|sum" "enum values"
                Expect.stringContains enum.Out "-> column" "what a reference points at"

            testCase "--all adds read-only properties, with why"
            <| fun () ->
                use t = project ()
                let r = tess [ "get"; t.Path; "[Total]"; "--properties"; "--all" ]
                Expect.equal r.Code ExitCode.ok r.Err
                let name = r.Out.Split('\n') |> Array.find (fun l -> l.StartsWith "name ")
                Expect.stringContains name "read-only" ""
                Expect.stringContains name "tess rename" ""

            testCase "--json: an array of {property, type, writable, value, allowed?}"
            <| fun () ->
                use t = project ()
                let r = tess [ "get"; t.Path; "Sales[Amount]"; "--properties"; "--json" ]
                Expect.equal r.Code ExitCode.ok r.Err
                use doc = json r
                Expect.equal doc.RootElement.ValueKind JsonValueKind.Array ""
                let items = doc.RootElement.EnumerateArray() |> List.ofSeq

                let find (path: string) =
                    items |> List.find (fun i -> i.GetProperty("property").GetString() = path)

                let keys (e: JsonElement) =
                    e.EnumerateObject() |> Seq.map (fun p -> p.Name) |> List.ofSeq

                let summarize = find "summarizeBy"
                Expect.equal (keys summarize) [ "property"; "type"; "writable"; "value"; "allowed" ] ""
                Expect.equal (summarize.GetProperty("type").GetString()) "enum" ""
                Expect.isTrue (summarize.GetProperty("writable").GetBoolean()) ""
                Expect.equal (summarize.GetProperty("value").GetString()) "default" ""

                Expect.contains
                    [ for a in summarize.GetProperty("allowed").EnumerateArray() -> a.GetString() ]
                    "none"
                    ""

                let hidden = find "isHidden"
                Expect.equal (keys hidden) [ "property"; "type"; "writable"; "value" ] ""
                Expect.equal (hidden.GetProperty("value").ValueKind) JsonValueKind.False "a JSON boolean"
                Expect.equal ((find "displayOrdinal").GetProperty("value").ValueKind) JsonValueKind.Number ""
                Expect.equal ((find "description").GetProperty("value").ValueKind) JsonValueKind.Null ""
                let sortBy = find "sortByColumn"
                Expect.equal (sortBy.GetProperty("type").GetString()) "reference" ""
                Expect.equal (sortBy.GetProperty("refersTo").GetString()) "column" ""
                Expect.isFalse (items |> List.exists (fun i -> i.GetProperty("property").GetString() = "name")) ""

                let all = tess [ "get"; t.Path; "Sales[Amount]"; "--properties"; "--all"; "--json" ]
                use doc = json all

                let name =
                    doc.RootElement.EnumerateArray()
                    |> Seq.find (fun i -> i.GetProperty("property").GetString() = "name")

                Expect.isFalse (name.GetProperty("writable").GetBoolean()) ""
                Expect.stringContains (name.GetProperty("reason").GetString()) "rename" ""

            testCase "the catalog and set agree: every writable path is accepted"
            <| fun () ->
                use t = project ()
                let r = tess [ "get"; t.Path; "[Total]"; "--properties"; "--json" ]
                use doc = json r

                for item in doc.RootElement.EnumerateArray() do
                    let path = item.GetProperty("property").GetString().Replace("<name>", "X")
                    let s = tess [ "set"; t.Path; "[Total]"; "--unset"; path ]
                    Expect.equal s.Code ExitCode.ok $"{path}: {s.Err}"

            testCase "usage errors"
            <| fun () ->
                use t = project ()
                Expect.equal (tess [ "get"; t.Path; "[Total]"; "--all" ]).Code ExitCode.usage ""

                Expect.equal
                    (tess [ "get"; t.Path; "[Total]"; "--properties"; "-p"; "expression" ]).Code
                    ExitCode.usage
                    ""

                Expect.equal (tess [ "get"; t.Path; "[Nope]"; "--properties" ]).Code ExitCode.input ""
        ]
    ]
