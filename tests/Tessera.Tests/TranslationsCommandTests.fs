module Tessera.Tests.TranslationsCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

let private load (path: string) =
    match ModelIO.loadPath path with
    | Ok(_, db) -> db
    | Error e -> failtestf "%s" (TessError.message e)

let private caption (db: Database) (culture: string) (o: MetadataObject) =
    match db.Model.Cultures.[culture].ObjectTranslations.[o, TranslatedProperty.Caption] with
    | null -> None
    | t -> Some t.Value

let private errorCode (doc: JsonDocument) =
    doc.RootElement.GetProperty("error").GetProperty("code").GetString()

let private header = "object,culture,property,source,translation"

/// Write a translation file into the project folder.
let private file (t: Fixtures.TempDir) (name: string) (lines: string list) =
    let path = t.Combine name
    File.WriteAllText(path, String.concat "\n" lines + "\n")
    path

[<Tests>]
let tests =
    testList "CLI translations" [
        testCase "export: CSV on stdout, one row per object, culture and property, translations empty"
        <| fun () ->
            use t = project ()
            let r = tess [ "translations"; "export"; t.Path ]
            Expect.equal r.Code ExitCode.ok r.Err
            let lines = r.Out.Split('\n')
            Expect.equal lines.[0] header ""
            Expect.contains lines "measure:Sales/Total,da-DK,Caption,Total," ""
            Expect.contains lines "column:Sales/Path\\/With\\\\Slashes,da-DK,Caption,Path/With\\Slashes," "refs escaped"

            Expect.stringContains
                r.Out
                "table:Sales,da-DK,Description,\"  Fact table.\n\n   Indented second paragraph.  \","
                ""

        testCase "export --json: the rows as a JSON array"
        <| fun () ->
            use t = project ()
            let r = tess [ "translations"; "export"; t.Path; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let rows = (json r).RootElement
            let first = rows.[0]
            Expect.equal (first.GetProperty("object").GetString()) "model" ""
            Expect.equal (first.GetProperty("culture").GetString()) "da-DK" ""
            Expect.equal (first.GetProperty("property").GetString()) "Caption" ""
            Expect.equal (first.GetProperty("source").GetString()) "Model" ""
            Expect.equal (first.GetProperty("translation").GetString()) "" ""

        testCase "export -o: writes the file, reports on stderr (JSON summary with --json), won't overwrite"
        <| fun () ->
            use t = project ()
            let out = t.Combine "da.csv"

            let r =
                tess [ "translations"; "export"; t.Path; "-o"; out; "--culture"; "da-DK"; "--bom" ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Out "" "nothing on stdout"
            Expect.stringContains r.Err "Wrote " ""
            let bytes = File.ReadAllBytes out
            Expect.equal bytes.[0..2] [| 0xEFuy; 0xBBuy; 0xBFuy |] "--bom"

            let again = tess [ "translations"; "export"; t.Path; "-o"; out ]
            Expect.equal again.Code ExitCode.output "refuses to overwrite"
            Expect.stringContains again.Err "--force" ""

            let j =
                tess [ "translations"; "export"; t.Path; "-o"; t.Combine "da.json"; "--json" ]

            Expect.equal j.Code ExitCode.ok j.Err
            let doc = json j
            Expect.equal (doc.RootElement.GetProperty("format").GetString()) "json" "from the extension"
            Expect.equal (doc.RootElement.GetProperty("cultures").[0].GetString()) "da-DK" ""

            Expect.equal
                (JsonDocument.Parse(File.ReadAllText(t.Combine "da.json")).RootElement.ValueKind)
                JsonValueKind.Array
                ""

        testCase "export --json --format csv without -o is a usage error"
        <| fun () ->
            use t = project ()
            let r = tess [ "translations"; "export"; t.Path; "--json"; "--format"; "csv" ]
            Expect.equal r.Code ExitCode.usage r.Out
            Expect.equal (errorCode (json r)) "usage" ""

        testCase "export --culture with an unknown culture: exit 3"
        <| fun () ->
            use t = project ()
            let r = tess [ "translations"; "export"; t.Path; "--culture"; "fr-FR" ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err "culture:fr-FR" ""

        testCase "import: a dry run by default; --save writes it; export -> import is then no change"
        <| fun () ->
            use t = project ()

            let f =
                file t "da.csv" [
                    header
                    "measure:Sales/Total,da-DK,Caption,Total,I alt"
                    "'Sales'[Amount],da-DK,Caption,Amount,\"Beløb, \"\"netto\"\"\""
                ]

            let dry = tess [ "translations"; "import"; t.Path; f ]
            Expect.equal dry.Code ExitCode.ok dry.Err

            Expect.stringContains
                dry.Out
                "~ measure:Sales/Total\n    translations.da-DK.caption: (none) -> \"I alt\""
                ""

            Expect.stringContains dry.Err "Dry run - nothing saved." ""
            Expect.isNone (caption (load t.Path) "da-DK" (load t.Path).Model.Tables.["Sales"]) "nothing written"

            let saved = tess [ "translations"; "import"; t.Path; f; "--save" ]
            Expect.equal saved.Code ExitCode.ok saved.Err
            let db = load t.Path
            Expect.equal (caption db "da-DK" db.Model.Tables.["Sales"].Measures.["Total"]) (Some "I alt") ""
            Expect.equal (caption db "da-DK" db.Model.Tables.["Sales"].Columns.["Amount"]) (Some "Beløb, \"netto\"") ""

            let out = t.Combine "all.csv"
            Expect.equal (tess [ "translations"; "export"; t.Path; "-o"; out ]).Code ExitCode.ok ""
            let round = tess [ "translations"; "import"; t.Path; out ]
            Expect.equal round.Code ExitCode.ok round.Err
            Expect.equal round.Out "No changes.\n" ""

        testCase "import --json: the change set shows translations on the objects they translate"
        <| fun () ->
            use t = project ()
            let f = file t "da.csv" [ header; "measure:Sales/Total,da-DK,Caption,Total,I alt" ]
            let r = tess [ "translations"; "import"; t.Path; f; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out
            let doc = (json r).RootElement
            Expect.equal (doc.GetProperty("command").GetString()) "translations import" ""
            let change = doc.GetProperty("changes").[0]
            Expect.equal (change.GetProperty("objectPath").GetString()) "measure:Sales/Total" ""
            Expect.equal (change.GetProperty("changeKind").GetString()) "modified" ""
            let p = change.GetProperty("properties").[0]
            Expect.equal (p.GetProperty("property").GetString()) "translations.da-DK.caption" ""
            Expect.equal (p.GetProperty("before").ValueKind) JsonValueKind.Null ""
            Expect.equal (p.GetProperty("after").GetString()) "I alt" ""
            Expect.isTrue (doc.GetProperty("dryRun").GetBoolean()) ""

        testCase "import: an unknown object is exit 3, with the file and line"
        <| fun () ->
            use t = project ()

            let f =
                file t "bad.csv" [
                    header
                    "table:Sales,da-DK,Description,,\"two"
                    "lines\""
                    "[Nope],da-DK,Caption,,x"
                ]

            let r = tess [ "translations"; "import"; t.Path; f ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err "bad.csv line 4: No object matches '[Nope]'" ""

            let j = tess [ "translations"; "import"; t.Path; f; "--json" ]
            Expect.equal (errorCode (json j)) "invalid-translation-file" ""

        testCase "import: an unknown culture is exit 3 unless --create-cultures; then it's created"
        <| fun () ->
            use t = project ()
            let f = file t "fr.csv" [ header; "table:Sales,fr-FR,Caption,Sales,Ventes" ]
            let r = tess [ "translations"; "import"; t.Path; f ]
            Expect.equal r.Code ExitCode.input r.Err
            Expect.stringContains r.Err "--create-cultures" ""

            let c = tess [ "translations"; "import"; t.Path; f; "--create-cultures"; "--save" ]
            Expect.equal c.Code ExitCode.ok c.Err
            Expect.stringContains c.Out "+ culture:fr-FR" ""
            Expect.stringContains c.Err "Added culture:fr-FR" ""
            let db = load t.Path
            Expect.equal (caption db "fr-FR" db.Model.Tables.["Sales"]) (Some "Ventes") ""

        testCase "import: a missing file is exit 3; a malformed one too"
        <| fun () ->
            use t = project ()
            Expect.equal (tess [ "translations"; "import"; t.Path; t.Combine "nope.csv" ]).Code ExitCode.input ""
            let f = file t "x.csv" [ "a,b"; "1,2" ]
            let r = tess [ "translations"; "import"; t.Path; f ]
            Expect.equal r.Code ExitCode.input ""

            Expect.stringContains
                r.Err
                "line 1: the header has no 'object', 'culture', 'property', 'translation' column"
                ""

        testCase "rename a translated measure: the translation follows, and no culture shows as changed"
        <| fun () ->
            use t = project ()
            let f = file t "da.csv" [ header; "measure:Sales/Total,da-DK,Caption,Total,I alt" ]
            Expect.equal (tess [ "translations"; "import"; t.Path; f; "--save" ]).Code ExitCode.ok ""
            let r = tess [ "rename"; t.Path; "[Total]"; "Sum"; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Out

            let paths = [
                for c in (json r).RootElement.GetProperty("changes").EnumerateArray() ->
                    c.GetProperty("objectPath").GetString()
            ]

            Expect.isFalse (paths |> List.contains "culture:da-DK") $"{paths}"

        testCase "get -p and find see translations on the object"
        <| fun () ->
            use t = project ()
            let f = file t "da.csv" [ header; "measure:Sales/Total,da-DK,Caption,Total,I alt" ]
            tess [ "translations"; "import"; t.Path; f; "--save" ] |> ignore
            let g = tess [ "get"; t.Path; "[Total]"; "-p"; "translations.da-DK.caption" ]
            Expect.equal g.Out "I alt\n" ""
            let found = tess [ "find"; t.Path; "I alt" ]
            Expect.stringContains found.Out "measure:Sales/Total  translations.da-DK.caption  I alt" ""

        testCase "set refuses translations, pointing at `translations import`"
        <| fun () ->
            use t = project ()
            let r = tess [ "set"; t.Path; "[Total]"; "-p"; "translations.da-DK.caption=x" ]
            Expect.equal r.Code ExitCode.usage r.Err
            Expect.stringContains r.Err "tess translations import" ""

        testCase "cultures: how much of each culture is translated"
        <| fun () ->
            use t = project ()
            let f = file t "da.csv" [ header; "measure:Sales/Total,da-DK,Caption,Total,I alt" ]
            tess [ "translations"; "import"; t.Path; f; "--save" ] |> ignore
            let r = tess [ "translations"; "cultures"; t.Path ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.stringStarts r.Out "culture:da-DK  1/19 captions, 0/1 descriptions, 0/0 display folders" ""

            let j =
                (json (tess [ "translations"; "cultures"; t.Path; "--json" ])).RootElement.[0]

            Expect.equal (j.GetProperty("translated").GetInt32()) 1 ""
            Expect.equal (j.GetProperty("properties").GetProperty("caption").GetProperty("total").GetInt32()) 19 ""
    ]

/// stdin is process-wide: keep these apart from other tests.
[<Tests>]
let stdinTests =
    testSequenced
    <| testList "CLI translations (stdin)" [
        testCase "import - reads the file from stdin, detecting JSON"
        <| fun () ->
            use t = project ()

            let text =
                """[ { "object": "table:Date", "culture": "da-DK", "property": "Caption", "translation": "Dato" } ]"""

            let r = tessWithStdin text [ "translations"; "import"; t.Path; "-"; "--save" ]
            Expect.equal r.Code ExitCode.ok r.Err
            let db = load t.Path
            Expect.equal (caption db "da-DK" db.Model.Tables.["Date"]) (Some "Dato") ""
    ]
