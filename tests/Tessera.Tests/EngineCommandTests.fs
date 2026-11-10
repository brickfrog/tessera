/// `tess query <model files> --data <fixture>` and `--coverage`, through the real CLI entry point.
/// (The harness fails a test if the command so much as asks for a server connection.)
module Tessera.Tests.EngineCommandTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Tests.EngineHelpers
open Tessera.Cli.Common

/// A test model as a .bim file with its data as a fixture directory next to it.
let private setup (m: TestModel) =
    let t = new TempDir()
    let db, store = build m
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    let dir = t.Combine "data"
    Directory.CreateDirectory dir |> ignore

    for table in store.Tables do
        use writer =
            new StreamWriter(Path.Combine(dir, FixtureName.encode table.Name + ".csv"))

        Fixture.writeCsv writer table

    t, path, dir

let private withFormatted = {
    star with
        Measures =
            star.Measures
            @ [
                "Sales", "Formatted", "FORMAT ( [Amount], \"0\" )"
                "Sales", "Uses Formatted", "[Formatted] & \"!\""
            ]
}

/// An element's JSON without the whitespace the output is indented with.
let private raw (e: JsonElement) =
    e.GetRawText().Replace("\n", "").Replace("\r", "").Replace(" ", "")

let private lines (s: string) =
    s.Split('\n') |> Array.filter ((<>) "") |> List.ofArray

[<Tests>]
let tests =
    testList "CLI query on model files" [
        testCase "a table by default" (fun () ->
            let t, model, dir = setup star
            use _ = t

            let r =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "EVALUATE SUMMARIZECOLUMNS ( Customer[Country], \"Amount\", [Amount] ) ORDER BY [Amount] DESC"
                ]

            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (lines r.Out)
                [
                    "Customer[Country]  [Amount]"
                    "-----------------  --------"
                    "US                      485"
                    "UK                      140"
                ]
                "table")

        testCase "--json has the shape of a server query's result" (fun () ->
            let t, model, dir = setup star
            use _ = t

            let r =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "EVALUATE ROW ( \"n\", COUNTROWS ( Sales ), \"avg\", DIVIDE ( 1, 4 ), \"none\", BLANK () ) EVALUATE { \"a\" }"
                    "--json"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "query" "command"
            Expect.equal (root.GetProperty("source").GetString()) (Path.GetFullPath model) "source"
            Expect.isTrue (root.TryGetProperty("durationMs") |> fst) "durationMs"
            let results = root.GetProperty "results"
            Expect.equal (results.GetArrayLength()) 2 "one result per EVALUATE"
            let first = results.[0]

            Expect.equal
                (raw (first.GetProperty "columns"))
                """[{"name":"[n]","type":"int64"},{"name":"[avg]","type":"double"},{"name":"[none]","type":"variant"}]"""
                "columns"

            Expect.equal (raw (first.GetProperty "rows")) "[[7,0.25,null]]" "rows"
            Expect.equal (first.GetProperty("rowCount").GetInt32()) 1 "rowCount"
            Expect.isFalse (first.GetProperty("truncated").GetBoolean()) "truncated")

        testCase "--csv, --max-rows and --file" (fun () ->
            let t, model, dir = setup star
            use _ = t
            let file = t.Combine "q.dax"
            File.WriteAllText(file, "EVALUATE VALUES ( Customer[Name] ) ORDER BY Customer[Name]")

            let r =
                tess [ "query"; model; "--data"; dir; "-f"; file; "--csv"; "--max-rows"; "2" ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (lines r.Out) [ "Customer[Name]"; "Ann"; "Bob" ] "csv"
            Expect.stringContains r.Err "first 2 rows" "says it was cut"

            let all =
                tess [ "query"; model; "--data"; dir; "-f"; file; "--json"; "--max-rows"; "2" ]

            use doc = json all

            Expect.isTrue
                (doc.RootElement.GetProperty("results").[0].GetProperty("truncated").GetBoolean())
                "truncated")

        testCase "model files need --data, and a server model can't take it" (fun () ->
            let t, model, dir = setup star
            use _ = t
            let r = tess [ "query"; model; "EVALUATE Sales" ]
            Expect.equal r.Code ExitCode.usage r.Out
            Expect.stringContains r.Err "--data" "hint"
            let remote = tess [ "query"; "xmla:localhost;db"; "--data"; dir; "EVALUATE Sales" ]
            Expect.equal remote.Code ExitCode.usage remote.Out)

        testCase "USERNAME and the others are refused unless an identity option gives them" (fun () ->
            let t, model, dir = setup star
            use _ = t
            let q = "EVALUATE { ( USERNAME (), USERCULTURE (), ISBLANK ( CUSTOMDATA () ) ) }"
            let refused = tess [ "query"; model; "--data"; dir; q; "--json" ]
            Expect.notEqual refused.Code ExitCode.ok refused.Out
            use doc = json refused

            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "not-supported" "code"

            let r =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    q
                    "--csv"
                    "--user-name"
                    "ann"
                    "--user-culture"
                    "en-US"
                ]

            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal (lines r.Out |> List.tail) [ "ann,en-US,TRUE" ] "csv"

            let remote =
                tess [ "query"; "xmla:localhost;db"; "--user-name"; "ann"; "EVALUATE { 1 }" ]

            Expect.equal remote.Code ExitCode.usage remote.Out)

        testCase "a fixture with errors is refused" (fun () ->
            let t, model, dir = setup star
            use _ = t
            File.WriteAllText(Path.Combine(dir, "Customer.csv"), "CustomerKey,Name,Country\nx,Ann,US\n")
            let r = tess [ "query"; model; "--data"; dir; "EVALUATE Sales"; "--json" ]
            Expect.equal r.Code ExitCode.input r.Out
            use doc = json r

            Expect.equal
                (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                "fixture-has-errors"
                "code")

        testCase "a missing fixture directory or model" (fun () ->
            let t, model, _ = setup star
            use _ = t

            Expect.equal
                (tess [ "query"; model; "--data"; t.Combine "nope"; "EVALUATE Sales" ]).Code
                ExitCode.input
                "directory"

            Expect.equal
                (tess [ "query"; t.Combine "nope.bim"; "--data"; t.Path; "EVALUATE Sales" ]).Code
                ExitCode.input
                "model")

        testCase "what the engine doesn't support is an error that says what and where" (fun () ->
            let t, model, dir = setup withFormatted
            use _ = t

            let r =
                tess [ "query"; model; "--data"; dir; "EVALUATE ROW ( \"v\", [Uses Formatted] )" ]

            Expect.equal r.Code ExitCode.input r.Out
            Expect.equal r.Out "" "no partial result"

            Expect.stringContains
                r.Err
                "Not supported by the local DAX engine: the function FORMAT (measure 'Sales'[Formatted] 1:1)"
                "message"

            let j =
                tess [
                    "query"
                    model
                    "--data"
                    dir
                    "EVALUATE ROW ( \"v\", [Uses Formatted] )"
                    "--json"
                ]

            use doc = json j
            Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "not-supported" "code")

        testCase "a syntax error and a DAX error" (fun () ->
            let t, model, dir = setup star
            use _ = t
            let syntax = tess [ "query"; model; "--data"; dir; "EVALUATE FILTER ( Sales" ]
            Expect.equal syntax.Code ExitCode.input syntax.Out
            Expect.stringContains syntax.Err "query 1:" "position"

            let error =
                tess [ "query"; model; "--data"; dir; "EVALUATE ROW ( \"v\", ERROR ( \"stop\" ) )" ]

            Expect.equal error.Code ExitCode.input error.Out
            Expect.stringContains error.Err "stop" "the text")

        testCase "--coverage lists what can't be evaluated, and why" (fun () ->
            let t, model, _ = setup withFormatted
            use _ = t
            let r = tess [ "query"; model; "--coverage" ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (lines r.Out)
                [
                    "measure:Sales/Formatted  FORMAT"
                    "measure:Sales/Uses Formatted  FORMAT"
                    "3 of 5 measures can be evaluated by the local engine"
                    "Not supported, by how many objects use it:"
                    "     2  FORMAT"
                ]
                "text"

            let j = tess [ "query"; model; "--coverage"; "--json" ]
            use doc = json j
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "query coverage" "command"
            let measures = root.GetProperty("summary").GetProperty("measures")

            Expect.equal
                (measures.GetProperty("total").GetInt32(), measures.GetProperty("supported").GetInt32())
                (5, 3)
                "summary"

            let formatted =
                root.GetProperty("objects").EnumerateArray()
                |> Seq.find (fun o -> o.GetProperty("ref").GetString() = "measure:Sales/Formatted")

            Expect.isFalse (formatted.GetProperty("supported").GetBoolean()) "supported"
            Expect.equal (raw (formatted.GetProperty "unsupported")) """["FORMAT"]""" "unsupported"
            Expect.equal (raw (root.GetProperty "unsupported")) """[{"reason":"FORMAT","objects":2}]""" "ranking"

            Expect.equal
                (tess [ "query"; model; "--coverage"; "EVALUATE Sales" ]).Code
                ExitCode.usage
                "no query with --coverage")

        testSequenced (
            testCase "the query from standard input" (fun () ->
                let t, model, dir = setup star
                use _ = t

                let r =
                    tessWithStdin "EVALUATE ROW ( \"v\", [Units] )" [ "query"; model; "--data"; dir; "-"; "--csv" ]

                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal (lines r.Out) [ "[v]"; "17" ] "csv")
        )
    ]
