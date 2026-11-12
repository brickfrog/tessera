module Tessera.Tests.DataCommandTests

open System
open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// FixtureTests' model as a .bim file in a temp folder, with a path for the fixture directory.
let private setup () =
    let t = new TempDir()
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase(FixtureTests.model ()))
    t, path, t.Combine "data"

let private lines (s: string) =
    s.Split('\n') |> Array.map (fun l -> l.TrimEnd '\r') |> Array.filter ((<>) "")

let private names (e: JsonElement) (key: string) =
    e.EnumerateArray()
    |> Seq.map (fun x -> x.GetProperty(key).GetString())
    |> List.ofSeq

[<Tests>]
let tests =
    testList "CLI data" [
        testCase "init writes a header-only CSV per table, and says so"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            let r = tess [ "data"; "init"; model; dir ]
            Expect.equal r.Code ExitCode.ok r.Err

            Expect.equal
                (lines r.Out)
                [|
                    $"Wrote 3 files to {Path.GetFullPath dir} (headers only)"
                    "  Product.csv   0 rows, 6 columns"
                    "  Customer.csv  0 rows, 2 columns"
                    "  Sales.csv     0 rows, 5 columns"
                    "No file for tables the engine computes: Calendar, Time"
                |]
                ""

            Expect.equal (File.ReadAllText(Path.Combine(dir, "Customer.csv"))) "Code,Name\n" ""
            Expect.equal (Directory.GetFiles dir |> Array.length) 3 ""

        testCase "init refuses to overwrite without --force"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Expect.equal (tess [ "data"; "init"; model; dir ]).Code ExitCode.ok ""
            File.WriteAllText(Path.Combine(dir, "Customer.csv"), "Code,Name\nmine,mine\n")
            let again = tess [ "data"; "init"; model; dir; "--rows"; "5" ]
            Expect.equal again.Code ExitCode.output again.Out
            Expect.stringContains again.Err "Destination exists" ""
            Expect.stringContains again.Err "--force" ""
            Expect.equal (File.ReadAllText(Path.Combine(dir, "Customer.csv"))) "Code,Name\nmine,mine\n" "untouched"

            let json = tess [ "data"; "init"; model; dir; "--json" ]
            Expect.equal json.Code ExitCode.output ""
            use doc = CliHarness.json json

            Expect.equal
                (doc.RootElement.GetProperty("error").GetProperty("code").GetString())
                "destination-not-empty"
                ""

            let forced = tess [ "data"; "init"; model; dir; "--rows"; "5"; "--force" ]
            Expect.equal forced.Code ExitCode.ok forced.Err
            Expect.equal (File.ReadAllLines(Path.Combine(dir, "Customer.csv")).Length) 6 ""

        testCase "init --rows --seed: a fixture that passes check, the same every time; --json lists the files"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t

            let r =
                tess [ "data"; "init"; model; dir; "--rows"; "50"; "--seed"; "42"; "--json" ]

            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "data init" ""
            Expect.equal (root.GetProperty("directory").GetString()) (Path.GetFullPath dir) ""
            Expect.equal (root.GetProperty("rows").GetInt32()) 50 ""
            Expect.equal (root.GetProperty("seed").GetInt64()) 42L ""
            Expect.equal (names (root.GetProperty "files") "table") [ "Product"; "Customer"; "Sales" ] ""
            let first = root.GetProperty("files").[0]
            Expect.equal (first.GetProperty("path").GetString()) (Path.Combine(Path.GetFullPath dir, "Product.csv")) ""
            Expect.equal (first.GetProperty("rows").GetInt32()) 50 ""
            Expect.equal (first.GetProperty("columns").GetInt32()) 6 ""

            Expect.equal
                (root.GetProperty("computed").EnumerateArray()
                 |> Seq.map (fun e -> e.GetString())
                 |> List.ofSeq)
                [ "Calendar"; "Time" ]
                ""

            let check = tess [ "data"; "check"; model; dir; "--fail-on"; "warning" ]
            Expect.equal check.Code ExitCode.ok check.Out

            let other = t.Combine "again"
            Expect.equal (tess [ "data"; "init"; model; other; "--rows"; "50"; "--seed"; "42" ]).Code ExitCode.ok ""

            for f in [ "Product.csv"; "Customer.csv"; "Sales.csv" ] do
                Expect.equal (File.ReadAllBytes(Path.Combine(other, f))) (File.ReadAllBytes(Path.Combine(dir, f))) f

            let seeded = t.Combine "seeded"
            Expect.equal (tess [ "data"; "init"; model; seeded; "--rows"; "50"; "--seed"; "43" ]).Code ExitCode.ok ""

            Expect.notEqual
                (File.ReadAllText(Path.Combine(seeded, "Sales.csv")))
                (File.ReadAllText(Path.Combine(dir, "Sales.csv")))
                "another seed, other rows"

        testCase "init: usage and input errors"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            let negative = tess [ "data"; "init"; model; dir; "--rows"; "-1" ]
            Expect.equal negative.Code ExitCode.usage ""
            Expect.stringContains negative.Err "--rows must not be negative" ""
            Expect.equal (tess [ "data"; "init"; model ]).Code ExitCode.usage "the directory is required"
            Expect.equal (tess [ "data"; "init"; t.Combine "missing.bim"; dir ]).Code ExitCode.input ""
            Expect.equal (tess [ "data"; "init"; model; model ]).Code ExitCode.input "a file is not a directory"
            Expect.equal (tess [ "data" ]).Code ExitCode.usage ""
            Expect.isFalse (Directory.Exists dir) "nothing was created"

        testCase "check: a clean fixture exits 0"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            tess [ "data"; "init"; model; dir; "--rows"; "10" ] |> ignore
            let r = tess [ "data"; "check"; model; dir ]
            Expect.equal r.Code ExitCode.ok r.Out

            Expect.equal
                (lines r.Out)
                [|
                    "info  relationship:Sales-Calendar  'Sales'[When] -> 'Calendar'[Date] is not checked: 'Calendar'[Date] is computed by the engine, not loaded from the fixture  [unchecked-relationship]"
                    "1 info"
                |]
                ""

            let quiet = tess [ "data"; "check"; model; dir; "--min-severity"; "warning" ]
            Expect.equal (lines quiet.Out) [| "No issues." |] ""

        testCase "check: errors exit 1, with file:line on each issue"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Product.csv"), "ProductKey,Name\n1,Bike\n1,Lock\nx,Bell\n")

            File.WriteAllText(Path.Combine(dir, "Customer.csv"), "Code,Name\nC1,Ann\n")

            File.WriteAllText(
                Path.Combine(dir, "Sales.csv"),
                "OrderId,ProductKey,CustomerCode,Amount,When\n1,1,C1,,\n2,7,C1,,\n"
            )

            let r =
                tess [
                    "data"
                    "check"
                    model
                    dir
                    "--min-severity"
                    "warning"
                    "--rule"
                    "invalid-value,duplicate-key,unmatched-key"
                ]

            Expect.equal r.Code ExitCode.findings r.Err

            Expect.equal
                (lines r.Out)
                [|
                    "error    column:Product/ProductKey  Product.csv:4  'x' is not a whole number, which column 'ProductKey' (INTEGER) needs  [invalid-value]"
                    "error    column:Product/ProductKey  Product.csv    1 value repeats in 'Product'[ProductKey], the one side of relationship 'Sales-Product': '1'  [duplicate-key]"
                    "warning  column:Sales/ProductKey    Sales.csv      1 row of 'Sales' (1 value of [ProductKey]) have no match in 'Product'[ProductKey] (relationship 'Sales-Product'): '7'; DAX puts them on the blank row of 'Product'  [unmatched-key]"
                    "2 errors, 1 warning"
                |]
                ""

        testCase "check --json: the findings envelope, with file, line and columnName"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Directory.CreateDirectory dir |> ignore
            let product = Path.Combine(Path.GetFullPath dir, "Product.csv")
            File.WriteAllText(product, "ProductKey,Name\n1,Bike\nx,Bell\n")
            let r = tess [ "data"; "check"; model; dir; "--json" ]
            Expect.equal r.Code ExitCode.findings r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "data check" ""
            Expect.isTrue (root.GetProperty("durationMs").GetInt32() >= 0) ""
            Expect.isFalse (root.GetProperty("valid").GetBoolean()) ""
            Expect.equal (root.GetProperty("directory").GetString()) (Path.GetFullPath dir) ""
            Expect.equal (root.GetProperty("summary").GetProperty("errors").GetInt32()) 1 ""
            let tables = root.GetProperty "tables"
            Expect.equal (names tables "table") [ "Product"; "Customer"; "Sales" ] ""
            Expect.equal (tables.[0].GetProperty("file").GetString()) product ""
            Expect.equal (tables.[0].GetProperty("rows").GetInt32()) 2 ""
            Expect.isFalse (tables.[1].TryGetProperty "file" |> fst) "no file"

            let finding =
                root.GetProperty("findings").EnumerateArray()
                |> Seq.find (fun f -> f.GetProperty("code").GetString() = "invalid-value")

            Expect.equal (finding.GetProperty("severity").GetString()) "error" ""
            Expect.equal (finding.GetProperty("source").GetString()) "data" ""
            Expect.equal (finding.GetProperty("objectPath").GetString()) "column:Product/ProductKey" ""
            Expect.equal (finding.GetProperty("objectType").GetString()) "Column" ""
            Expect.equal (finding.GetProperty("file").GetString()) product ""
            Expect.equal (finding.GetProperty("line").GetInt32()) 3 ""
            Expect.equal (finding.GetProperty("columnName").GetString()) "ProductKey" ""

            Expect.stringStarts
                (finding.GetProperty("message").GetString())
                "Product.csv:3: 'x' is not a whole number"
                ""

            let missing =
                root.GetProperty("findings").EnumerateArray()
                |> Seq.find (fun f -> f.GetProperty("code").GetString() = "missing-table")

            let mutable none = Unchecked.defaultof<JsonElement>
            Expect.isFalse (missing.TryGetProperty("file", &none)) "optional keys are left out"
            Expect.equal (missing.GetProperty("objectPath").GetString()) "model" ""

        testCase "check: --fail-on, --min-severity, --rule and --ci, as validate"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Product.csv"), "ProductKey\n1\n")
            // Warnings only: two tables have no file.
            Expect.equal (tess [ "data"; "check"; model; dir ]).Code ExitCode.ok ""
            Expect.equal (tess [ "data"; "check"; model; dir; "--fail-on"; "warning" ]).Code ExitCode.findings ""

            let hidden =
                tess [
                    "data"
                    "check"
                    model
                    dir
                    "--fail-on"
                    "warning"
                    "--min-severity"
                    "error"
                ]

            Expect.equal (lines hidden.Out) [| "No issues." |] ""
            Expect.equal hidden.Code ExitCode.findings "hidden issues still count"

            let one =
                tess [ "data"; "check"; model; dir; "--rule"; "missing-table"; "--ci"; "github" ]

            Expect.equal (Array.last (lines one.Out)) "1 warning" ""

            Expect.stringContains
                one.Err
                "::warning title=missing-table::model: 2 tables have no file and no rows: Customer, Sales"
                ""

            let unknown = tess [ "data"; "check"; model; dir; "--rule"; "nope" ]
            Expect.equal unknown.Code ExitCode.usage ""
            Expect.stringContains unknown.Err "unknown rule 'nope'" ""
            Expect.stringContains unknown.Err "unmatched-key" "lists the known rules"
            Expect.equal (tess [ "data"; "check"; model; dir; "--fail-on"; "fatal" ]).Code ExitCode.usage ""
            Expect.equal (tess [ "data"; "check"; model; dir; "--ci"; "jenkins" ]).Code ExitCode.usage ""

        testCase "check and info: a missing directory or model is an input error"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t

            for command in [ "check"; "info" ] do
                let r = tess [ "data"; command; model; dir ]
                Expect.equal r.Code ExitCode.input command
                Expect.stringContains r.Err "Path not found" ""
                Expect.equal (tess [ "data"; command; t.Combine "missing.bim"; dir ]).Code ExitCode.input ""
                let file = tess [ "data"; command; model; model; "--json" ]
                Expect.equal file.Code ExitCode.input ""
                use doc = json file
                Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "invalid-fixture" ""

        testCase "info: tables with rows, and distinct and BLANK counts per column"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Customer.csv"), "Code,Name\nC1,Ann\nC2,ann\nC3,\nC4,\n")
            File.WriteAllText(Path.Combine(dir, "product.json"), """[ { "ProductKey": 1, "Price": 2.5 } ]""")
            let r = tess [ "data"; "info"; model; dir ]
            Expect.equal r.Code ExitCode.ok r.Err
            Expect.equal r.Err "" ""

            Expect.equal
                (lines r.Out)
                [|
                    "Product  product.json  1 row"
                    "  ProductKey  integer   1 distinct"
                    "  Name        string    1 distinct, 1 blank"
                    "  Price       currency  1 distinct"
                    "  Weight      decimal   1 distinct, 1 blank"
                    "  Active      boolean   1 distinct, 1 blank"
                    "  Launched    dateTime  1 distinct, 1 blank"
                    "Customer  Customer.csv  4 rows"
                    "  Code  string  4 distinct"
                    "  Name  string  2 distinct, 2 blank"
                    "Sales  (no file)  0 rows"
                    "  OrderId       integer   0 distinct"
                    "  ProductKey    integer   0 distinct"
                    "  CustomerCode  string    0 distinct"
                    "  Amount        currency  0 distinct"
                    "  When          dateTime  0 distinct"
                    "Computed by the engine, not loaded: Calendar, Time"
                |]
                "BLANK counts as a distinct value; 'Ann' and 'ann' are one"

        testCase "info --json; errors in the fixture are counted and noted on stderr"
        <| fun () ->
            let t, model, dir = setup ()
            use _ = t
            Directory.CreateDirectory dir |> ignore
            File.WriteAllText(Path.Combine(dir, "Customer.csv"), "Code,Name\nC1,Ann\nC2,\n")
            File.WriteAllText(Path.Combine(dir, "Sales.csv"), "OrderId\nx\n")
            let r = tess [ "data"; "info"; model; dir; "--json" ]
            Expect.equal r.Code ExitCode.ok r.Err
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("command").GetString()) "data info" ""
            Expect.equal (root.GetProperty("directory").GetString()) (Path.GetFullPath dir) ""
            let tables = root.GetProperty "tables"
            Expect.equal (names tables "table") [ "Product"; "Customer"; "Sales" ] ""
            let customer = tables.[1]
            Expect.equal (customer.GetProperty("ref").GetString()) "table:Customer" ""
            Expect.equal (customer.GetProperty("rows").GetInt32()) 2 ""
            Expect.equal (Path.GetFileName(customer.GetProperty("file").GetString())) "Customer.csv" ""
            let name = customer.GetProperty("columns").[1]
            Expect.equal (name.GetProperty("name").GetString()) "Name" ""
            Expect.equal (name.GetProperty("ref").GetString()) "column:Customer/Name" ""
            Expect.equal (name.GetProperty("dataType").GetString()) "string" ""
            Expect.equal (name.GetProperty("distinct").GetInt32()) 2 ""
            Expect.equal (name.GetProperty("blanks").GetInt32()) 1 ""
            Expect.equal (root.GetProperty("issues").GetProperty("errors").GetInt32()) 1 ""
            Expect.isTrue (root.GetProperty("issues").GetProperty("warnings").GetInt32() >= 1) ""

            let text = tess [ "data"; "info"; model; dir ]
            Expect.equal text.Code ExitCode.ok ""
            Expect.stringContains text.Err "1 error in the fixture" ""
            Expect.stringContains text.Err "tess data check" ""

        testCase "--help lists the subcommands"
        <| fun () ->
            let r = tess [ "data"; "--help" ]
            Expect.equal r.Code ExitCode.ok ""

            for word in [ "init"; "check"; "info" ] do
                Expect.stringContains r.Out word ""

            Expect.stringContains (tess [ "--help" ]).Out "fixture data" ""
    ]

// ---- The sample model (optional) ---------------------------------------------------------------

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

[<Tests>]
let sampleTests =
    testList "Sample model data (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            testCase "init --rows always passes check, for several sizes and seeds"
            <| fun () ->
                use t = new TempDir()

                for rows, seed in [ 0, 1; 1, 1; 2, 5; 37, 2; 500, 3; 500, 4 ] do
                    let dir = t.Combine $"fx-{rows}-{seed}"

                    let init =
                        tess [ "data"; "init"; path; dir; "--rows"; string rows; "--seed"; string seed ]

                    Expect.equal init.Code ExitCode.ok init.Err
                    let check = tess [ "data"; "check"; path; dir; "--fail-on"; "warning"; "--json" ]
                    Expect.equal check.Code ExitCode.ok check.Out
                    use doc = json check
                    Expect.isTrue (doc.RootElement.GetProperty("valid").GetBoolean()) ""
                    Expect.equal (doc.RootElement.GetProperty("summary").GetProperty("warnings").GetInt32()) 0 ""

                    for table in doc.RootElement.GetProperty("tables").EnumerateArray() do
                        Expect.isTrue (table.GetProperty("rows").GetInt32() <= rows) ""

            testCase "info lists every loaded table"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "fx"
                Expect.equal (tess [ "data"; "init"; path; dir; "--rows"; "20" ]).Code ExitCode.ok ""
                let info = tess [ "data"; "info"; path; dir; "--json" ]
                Expect.equal info.Code ExitCode.ok info.Err
                use doc = json info
                let tables = doc.RootElement.GetProperty "tables"
                Expect.isTrue (tables.GetArrayLength() > 0) ""
                Expect.equal (doc.RootElement.GetProperty("issues").GetProperty("errors").GetInt32()) 0 ""
                Expect.equal (Directory.GetFiles dir |> Array.length) (tables.GetArrayLength()) "one file per table"
    ]
