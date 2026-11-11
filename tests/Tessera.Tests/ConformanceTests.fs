/// The conformance corpus as tests: every case file is sound, the harness judges engines the way
/// it says, and (once there is one) the engine agrees with the corpus.
module Tessera.Tests.ConformanceTests

open System
open System.IO
open Expecto
open Tessera
open Tessera.Tests.Fixtures
open Tessera.Tests.Conformance

// ---- The engine under test ------------------------------------------------------------------------

/// The local engine, as members of the case's roles when it says (`"as"`).
let private engine: EvaluateAs option =
    Some(fun runAs db store dax ->
        Engine.prepare db store
        |> Result.bind (fun e ->
            match runAs with
            | None -> Ok e
            | Some a ->
                let e =
                    match a.CustomData with
                    | Some text ->
                        Engine.withIdentity
                            {
                                DaxEngine.Identity.none with
                                    CustomData = Some text
                            }
                            e
                    | None -> e

                Engine.asRoles a.Roles e)
        |> Result.bind (fun e -> Engine.evaluateQuery e dax))

/// Tags the engine isn't expected to handle yet. A case with one of them is reported as skipped
/// unless it passes; remove a tag here when its area is implemented.
let private expectedUnsupported: Set<string> =
    set laterAreas
    - set [
        "date-functions"
        "text-functions"
        "format"
        "time-intelligence"
        "math-functions"
        "allselected"
        "calculation-groups"
        "window-functions"
        "security"
        "format-strings"
    ]

/// Which of the engine's errors mean "not implemented" (see `RunConfig.defaults`).
let private engineConfig = RunConfig.defaults

/// Cases the engine is known not to pass, one `<case id><tab><kind>` per line, where kind is
/// `not-supported` (an honest refusal), `error` or `fail` (the engine disagrees with the corpus:
/// one of the two is wrong, and that needs settling). A case whose outcome differs from its line
/// fails, and so does a line whose case now passes, so the file only ever shrinks.
let private baselineFile = "engine-baseline.tsv"

let private baseline () : Map<string, string> =
    match corpusDirectory with
    | None -> Map.empty
    | Some dir ->
        let path = Path.Combine(dir, baselineFile)

        if not (File.Exists path) then
            Map.empty
        else
            File.ReadAllLines path
            |> Array.filter (fun l -> l.Trim() <> "" && not (l.StartsWith "#"))
            |> Array.map (fun l ->
                match l.Split('\t') with
                | [| id; kind |] -> id.Trim(), kind.Trim()
                | _ -> failwithf "%s: expected '<case id><tab><kind>': %s" baselineFile l)
            |> Map.ofArray

// ---- The corpus -----------------------------------------------------------------------------------

/// The items marked ASSUMPTION in src/Tessera.Core/DaxValue.fs, in file order: the tag (after
/// `assumption:`) of the cases designed to settle each, and what it is about.
let assumptions = [
    "collation-accents", "text comparison: accents (kana, width) count; only case doesn't"
    "currency-rounding", "a fifth decimal is rounded half away from zero when a number becomes a currency"
    "date-to-text", "a date becomes text in the en-US general date form"
    "text-to-date", "text is read as a date in ISO 8601 and the culture's formats"
    "exact-number-comparison",
    "an integer above 2^53 against a decimal number: exact where rounding agrees, else refused"
    "blank-sorts-first", "ORDER BY puts BLANK before negative numbers"
    "integer-overflow", "integer arithmetic that overflows wraps around (refused at the two reserved values)"
    "date-multiplication", "a date multiplied by a number is a decimal number"
    "integer-divided-by-currency", "an integer divided by a currency is a decimal number"
    "power-result-type", "the result of ^ is always a decimal number"
    "logical-blank", "&& and || give TRUE or FALSE; BLANK with BLANK, and NOT of BLANK, are refused"
    "convert-integer-rounding", "converting to an integer rounds half away from zero"
    "text-to-number-separators",
    "text with spaces, thousands separators or a currency symbol read as a number is refused"
]

/// The assumptions that are no longer made: `DaxValue.fs` now follows a source that settles
/// them, or refuses (`Unsettled`) where none does. Their cases stay, with their tags.
let resolvedAssumptions =
    set [
        "currency-rounding" // half away from zero (CURRENCY page, SQLBI's 0.6173); near-ties refused
        "exact-number-comparison" // the documentation's comparison table: as floats
        "integer-overflow" // wraps around (SQLBI, "Rounding errors with different data types in DAX")
        "date-multiplication" // the documentation's multiplication table; the rest refused
        "integer-divided-by-currency" // a decimal number (SQLBI's table and example, Microsoft's sentence)
        "power-result-type" // POWER returns "A decimal number", and ^ is POWER
        "convert-integer-rounding" // away from zero (dax.guide/convert)
        "text-to-number-separators" // refused
    ]

let private corpus =
    lazy
        (match corpusDirectory with
         | Some directory -> Some(load directory)
         | None -> None)

let private withCorpus (f: Corpus -> unit) () =
    match corpus.Value with
    | Some c -> f c
    | None -> skiptest "tests/conformance was not found (set TESSERA_CONFORMANCE_DIR)"

let private cases () =
    match corpus.Value with
    | Some c -> c.Cases
    | None -> []

[<Tests>]
let corpusTests =
    testList "conformance (corpus)" [
        testCase "every file reads as a model or a case"
        <| withCorpus (fun c ->
            let text =
                c.Problems
                |> List.map (fun (file, why) -> $"{file}: {why}")
                |> String.concat "\n"

            Expect.isEmpty c.Problems text)

        testCase "there is a corpus"
        <| withCorpus (fun c ->
            Expect.isGreaterThanOrEqual c.Cases.Length 300 "cases"
            Expect.isNonEmpty c.Models "models")

        testCase "every shared model is used by a case"
        <| withCorpus (fun c ->
            let used = c.Cases |> List.map (fun x -> x.ModelName.Split('-').[0]) |> set

            let unused =
                c.Models
                |> Map.keys
                |> Seq.filter (fun m ->
                    not (
                        c.Cases
                        |> List.exists (fun x -> x.ModelName = m || x.ModelName.StartsWith(m + "-"))
                    ))
                |> List.ofSeq

            Expect.isEmpty unused $"unused models (used: {used})")

        testCase "every ASSUMPTION in DaxValue.fs has a case designed to settle it"
        <| withCorpus (fun c ->
            match repositoryRoot with
            | None -> ()
            | Some root ->
                let source =
                    File.ReadAllText(Path.Combine(root, "src", "Tessera.Core", "DaxValue.fs"))

                let marked = source.Split("ASSUMPTION:").Length - 1

                Expect.equal
                    marked
                    (assumptions.Length - resolvedAssumptions.Count)
                    "DaxValue.fs has a different number of ASSUMPTION markers than the list in ConformanceTests.fs: update the list, and give a new one a case"

            let tags = c.Cases |> List.collect _.Tags |> set

            let missing =
                assumptions
                |> List.map fst
                |> List.filter (fun id -> not (tags.Contains("assumption:" + id)))

            Expect.isEmpty missing "assumptions with no case"

            let unknown =
                tags
                |> Seq.filter (fun t -> t.StartsWith "assumption:")
                |> Seq.filter (fun t -> not (assumptions |> List.exists (fun (id, _) -> "assumption:" + id = t)))
                |> List.ofSeq

            Expect.isEmpty unknown "assumption tags that name no assumption")

        testCase "case ids are unique whatever the file system's case rules"
        <| withCorpus (fun c ->
            let clashes =
                c.Cases
                |> List.countBy (fun x -> x.Id.ToUpperInvariant())
                |> List.filter (fun (_, n) -> n > 1)

            Expect.isEmpty clashes "")

        // What the server runner deploys: the model with its rows inside it, exactly as written
        // (not as the store normalises them).
        testList "each model embeds its rows exactly" [
            for c in cases () |> List.distinctBy _.ModelKey ->
                testCase c.ModelName
                <| fun () ->
                    match prepared c with
                    | Error e -> failtest e
                    | Ok p ->
                        match embedForServer p with
                        | Error e -> failtest (TessError.message e)
                        | Ok embedded ->
                            Expect.isEmpty embedded.Warnings "a corpus model must embed without loss"

                            for table in Fixture.dataTables p.Database do
                                let source =
                                    embedded.Database.Model.Tables.[table.Name].Partitions.[0].Source
                                    :?> Microsoft.AnalysisServices.Tabular.CalculatedPartitionSource

                                let columns, rows = EmbedTests.readTable source.Expression

                                Expect.equal
                                    columns
                                    (Fixture.dataColumns table |> List.map (fun (column, t) -> column.Name, t))
                                    $"{table.Name}: columns"

                                Expect.equal rows (rawRows p table.Name) $"{table.Name}: rows, value for value"

                            let errors (db: Microsoft.AnalysisServices.Tabular.Database) =
                                Validate.validate db
                                |> List.filter (fun i -> i.Severity = Validate.Severity.Error)
                                |> List.map (fun i -> i.Rule, i.Message)

                            Expect.equal (errors embedded.Database) (errors p.Database) "no new validation errors"
        ]

        testList "each case is sound" [
            for c in cases () ->
                testCase c.Id
                <| fun () ->
                    let problems = validate c
                    Expect.isEmpty problems (String.concat "\n" problems)
        ]
    ]

// ---- The engine against the corpus ----------------------------------------------------------------

/// TESSERA_CONFORMANCE_REPORT=<file>: every case's outcome is appended to it as
/// `<case id><tab><kind><tab><why>`, to see what the baseline's refusals are about.
let private reportFile =
    match Environment.GetEnvironmentVariable "TESSERA_CONFORMANCE_REPORT" with
    | null
    | "" -> None
    | path -> Some path

let private reportLock = obj ()

let private record (c: Case) (kind: string) (lines: string list) =
    match reportFile with
    | None -> ()
    | Some path ->
        let why =
            (String.concat " | " lines).Replace('\t', ' ').Replace('\n', ' ').Replace('\r', ' ')

        lock reportLock (fun () -> File.AppendAllText(path, $"{c.Id}\t{kind}\t{why}\n"))

let private report (c: Case) (lines: string list) =
    let uncertain =
        match c.Alternative with
        | Some a when c.Uncertain -> [ $"(uncertain; the other reading: {a})" ]
        | _ -> []

    String.concat "\n" ([ c.Title; c.Query ] @ lines @ uncertain)

[<Tests>]
let engineTests =
    testList "conformance (engine)" [
        match engine with
        | None ->
            testCase "no engine yet"
            <| fun () -> skiptest "point `engine` in ConformanceTests.fs at the local DAX engine"
        | Some evaluate ->
            let known = baseline ()

            for c in cases () do
                testCase c.Id
                <| fun () ->
                    let later = c.Tags |> List.exists expectedUnsupported.Contains
                    let outcome = runAsWith engineConfig evaluate c

                    let kind, lines =
                        match outcome with
                        | Outcome.Pass -> "pass", []
                        | Outcome.NotSupported reason -> "not-supported", [ $"not supported: {reason}" ]
                        | Outcome.Error message -> "error", [ $"error: {message}" ]
                        | Outcome.Fail mismatch -> "fail", Mismatch.describe mismatch

                    record c kind lines

                    match kind, Map.tryFind c.Id known with
                    | "pass", None -> ()
                    | "pass", Some was ->
                        failtest $"{c.Id} now passes: remove its line ({was}) from tests/conformance/{baselineFile}"
                    | _, Some was when was = kind -> ()
                    | _, Some was -> failtest (report c ($"the baseline says {was}, now {kind}" :: lines))
                    | _ when later -> skiptest $"not expected to work yet ({kind})"
                    // An expectation no source settles can't fail the engine: a server decides it first.
                    | "fail", None when c.Uncertain -> skiptest (report c lines)
                    | _ -> failtest (report c lines)
    ]

// ---- The harness itself ---------------------------------------------------------------------------

let private model =
    """{ "tables": [ { "name": "T", "columns": [ { "name": "k", "type": "int64", "key": true }, { "name": "s", "type": "string" } ],
                       "measures": [ { "name": "N", "expression": "COUNTROWS ( T )" } ],
                       "rows": [ [1, "a"], [2, null], [3, ""] ] },
                     { "name": "F", "columns": [ { "name": "k", "type": "int64" }, { "name": "amount", "type": "decimal" },
                                                 { "name": "at", "type": "dateTime" }, { "name": "x", "type": "double" }, { "name": "b", "type": "boolean" } ],
                       "rows": [ [1, 10.5, "2024-01-31T13:05:00", 0.1, true], [9, 922337203685477.5807, null, null, null] ] } ],
         "relationships": [ { "from": "F[k]", "to": "T[k]" } ] }"""

let private caseText (extra: string) =
    $$"""{ "title": "a case", "model": "m", "query": "EVALUATE ROW ( \"r\", 1 )",
           "expected": [ { "columns": [ { "name": "[r]", "type": "int64" } ], "rows": [ [1] ] } ],
           "source": { "url": "https://example.org/dax", "supports": "says so" },
           "confidence": "published", "tags": [ "row" ] {{extra}} }"""

/// A corpus in a temp folder: models/m.json and the given (relative path, text) case files.
let private corpusOf (files: (string * string) list) =
    let t = new TempDir()
    write (t.Combine("models", "m.json")) model

    for path, text in files do
        write (t.Combine(path.Split '/')) text

    t, load t.Path

let private single (text: string) =
    let t, c = corpusOf [ "area/one.json", text ]
    (t :> IDisposable).Dispose()

    match c.Cases, c.Problems with
    | [ one ], [] -> one
    | _, problems -> failtestf "the case didn't load: %A" problems

let private int64Result (name: string) (value: int64) : QueryResult = {
    Columns = [ { Name = name; Type = QueryType.Int64 } ]
    Rows = [ [| QueryValue.Int64 value |] ]
    Truncated = false
}

let private answering (results: QueryResult list) : Evaluate = fun _ _ _ -> Ok results
let private failing (e: TessError) : Evaluate = fun _ _ _ -> Error e

[<Tests>]
let harnessTests =
    testList "conformance (harness)" [
        testCase "a corpus loads: models, cases by id, problems apart"
        <| fun () ->
            let t, c =
                corpusOf [
                    "area/one.json", caseText ""
                    "area/sub/two.json", caseText ""
                    "other/broken.json", "{ not json"
                    "other/unknown-key.json", caseText """, "sources": [] """
                    "other/no-model.json", (caseText "").Replace("\"m\"", "\"nope\"")
                ]

            use _ = t
            Expect.equal (c.Cases |> List.map _.Id) [ "area/one"; "area/sub/two" ] ""
            Expect.equal (c.Cases |> List.map _.Area) [ "area"; "area" ] ""

            Expect.equal
                (c.Problems |> List.map fst)
                [ "other/broken.json"; "other/no-model.json"; "other/unknown-key.json" ]
                ""

            let why (file: string) =
                c.Problems |> List.find (fun (f, _) -> f = file) |> snd

            Expect.stringContains (why "other/unknown-key.json") "sources" ""
            Expect.stringContains (why "other/no-model.json") "models/nope.json" ""
            Expect.isTrue (c.Models.ContainsKey "m") ""

        testCase "a sound case has nothing wrong with it"
        <| fun () -> Expect.isEmpty (validate (single (caseText ""))) ""

        testCase "the model is built in TOM and its rows loaded through Fixture"
        <| fun () ->
            match prepared (single (caseText "")) with
            | Error e -> failtest e
            | Ok p ->
                Expect.equal p.Database.Model.Tables.Count 2 ""
                Expect.equal p.Database.Model.Relationships.Count 1 ""
                let t = p.Store.TryTable("T").Value
                Expect.equal t.RowCount 3 ""
                Expect.equal (t.TryColumn("s").Value.[1]) DaxValue.Blank "null is BLANK"
                Expect.equal (t.TryColumn("s").Value.[2]) (DaxValue.String "") "the empty string stays one"
                let f = p.Store.TryTable("F").Value
                Expect.equal (f.TryColumn("amount").Value.[1]) (DaxValue.Currency Int64.MaxValue) "a decimal is exact"

                Expect.equal (f.TryColumn("at").Value.[0]) (DaxValue.ofDateTime (DateTime(2024, 1, 31, 13, 5, 0))) ""

                Expect.equal (f.TryColumn("b").Value.[0]) (DaxValue.Boolean true) ""
                // F[k] = 9 has no match in T: a warning, which is how a case asks for the blank row.
                Expect.exists p.Issues (fun i -> i.Rule = "unmatched-key") ""
                Expect.isFalse (Fixture.hasErrors p.Issues) ""

        testCase "validate names what is wrong with a case"
        <| fun () ->
            let problems (text: string) = validate (single text)

            let expectProblem (text: string) (part: string) =
                let found = problems text
                Expect.exists found (fun p -> p.Contains part) $"{part} in {found}"

            expectProblem ((caseText "").Replace("EVALUATE ROW", "EVALUATE ROW (")) "query:"
            expectProblem (caseText """, "ordered": true""") "ORDER BY"
            expectProblem ((caseText "").Replace("\"published\"", "\"derived\"")) "derivation"
            expectProblem (caseText """, "uncertain": true""") "alternative"
            expectProblem (caseText """, "uncertain": true, "alternative": "x" """) "published and uncertain"
            expectProblem (caseText """, "alternative": "x" """) "only for uncertain"
            expectProblem ((caseText "").Replace("https://", "http://")) "https"
            expectProblem ((caseText "").Replace("[ \"row\" ]", "[ ]")) "\"tags\" is empty"
            expectProblem ((caseText "").Replace("[ \"row\" ]", "[ \"Row\" ]")) "lower case"
            expectProblem ((caseText "").Replace("ROW ( \\\"r\\\", 1 )", "{ 1 } EVALUATE { 2 }")) "2 EVALUATE"

            let later, c = corpusOf [ "format/one.json", caseText "" ]
            use _ = later
            Expect.exists (validate c.Cases.Head) (fun p -> p.Contains "must carry the tag 'format'") ""

        testCase "validate reports a model that is wrong: a duplicate key, a broken measure"
        <| fun () ->
            let withModel (overlay: string) =
                validate (single ((caseText "").Replace("\"m\"", $$"""{ "base": "m", {{overlay}} }""")))

            Expect.exists
                (withModel """ "rows": { "T": [ [1, "a"], [1, "b"] ] } """)
                (fun p -> p.Contains "duplicate-key")
                ""

            Expect.exists
                (withModel """ "measures": [ { "table": "T", "name": "Bad", "expression": "SUM ( Nope[x] )" } ] """)
                (fun p -> p.StartsWith "model:")
                ""

            Expect.isEmpty
                (withModel """ "measures": [ { "table": "T", "name": "Good", "expression": "SUM ( F[amount] )" } ] """)
                ""

        testCase "validate catches an ordered expectation whose ORDER BY leaves rows tied"
        <| fun () ->
            let ordered (rows: string) =
                (caseText """, "ordered": true""")
                    .Replace("ROW ( \\\"r\\\", 1 )", "T ORDER BY T[s]")
                    .Replace(
                        """[ { "columns": [ { "name": "[r]", "type": "int64" } ], "rows": [ [1] ] } ]""",
                        $$"""[ { "columns": [ { "name": "T[k]", "type": "int64" }, { "name": "T[s]", "type": "string" } ], "rows": {{rows}} } ]"""
                    )

            let tied = validate (single (ordered """[ [1, "a"], [2, "a"], [3, "b"] ]"""))
            Expect.exists tied (fun p -> p.Contains "tie on the ORDER BY keys") $"%A{tied}"
            Expect.isEmpty (validate (single (ordered """[ [1, "a"], [2, "b"], [3, "c"] ]"""))) "no ties"
            Expect.isEmpty (validate (single (ordered """[ [1, "a"], [1, "a"], [3, "c"] ]"""))) "identical rows may tie"

        testCase "validate catches a sort-by column with two sort values for one value"
        <| fun () ->
            let withTable (rows: string) =
                validate (
                    single (
                        (caseText "")
                            .Replace(
                                "\"m\"",
                                $$"""{ "base": "m", "tables": [ { "name": "D", "columns": [ { "name": "Month", "type": "string", "sortBy": "No" }, { "name": "No", "type": "int64" } ], "rows": {{rows}} } ] }"""
                            )
                    )
                )

            Expect.isEmpty (withTable """[ ["Jan", 1], ["Feb", 2], ["Jan", 1] ]""") ""

            Expect.exists
                (withTable """[ ["Jan", 1], ["Feb", 2], ["Jan", 13] ]""")
                (fun p -> p.Contains "sorted by No")
                ""

        testCase "for a server the rows are embedded as written; the store's are folded"
        <| fun () ->
            let c =
                single (
                    (caseText "")
                        .Replace("\"m\"", """{ "base": "m", "rows": { "T": [ [1, "a"], [2, "A"], [3, "b  "] ] } }""")
                )

            match prepared c with
            | Error e -> failtest e
            | Ok p ->
                let stored = p.Store.TryTable("T").Value.TryColumn("s").Value

                Expect.equal
                    [ stored.[0]; stored.[1]; stored.[2] ]
                    [ DaxValue.String "a"; DaxValue.String "a"; DaxValue.String "b" ]
                    "the store folds case and drops trailing spaces"

                Expect.equal
                    (rawRows p "t" |> List.map (fun row -> row.[1]))
                    [ DaxValue.String "a"; DaxValue.String "A"; DaxValue.String "b  " ]
                    ""

                match embedForServer p with
                | Error e -> failtest (TessError.message e)
                | Ok embedded ->
                    let source =
                        embedded.Database.Model.Tables.["T"].Partitions.[0].Source
                        :?> Microsoft.AnalysisServices.Tabular.CalculatedPartitionSource

                    Expect.stringContains source.Expression "{ 2, \"A\" }" "the server gets the casing as written"
                    Expect.stringContains source.Expression "{ 3, \"b  \" }" "and the trailing spaces"
                    Expect.equal (snd (EmbedTests.readTable source.Expression)) (rawRows p "T") ""

        testCase "a case's changes to a shared model make a model of its own"
        <| fun () ->
            let plain = single (caseText "")

            let changed =
                single (
                    (caseText "")
                        .Replace(
                            "\"m\"",
                            """{ "base": "m",
                                 "measures": [ { "table": "T", "name": "Extra", "expression": "1" } ],
                                 "calculatedColumns": [ { "table": "T", "name": "k2", "type": "int64", "expression": "T[k] * 2" } ],
                                 "calculatedTables": [ { "name": "C", "expression": "VALUES ( T[s] )" } ],
                                 "tables": [ { "name": "U", "columns": [ { "name": "k", "type": "int64" } ], "rows": [ [1] ] } ],
                                 "addRelationships": [ { "from": "U[k]", "to": "T[k]", "crossFilter": "both" } ],
                                 "functions": [ { "name": "Twice", "expression": "( x : NUMERIC ) => x * 2" } ],
                                 "rows": { "F": [ ] } }"""
                        )
                )

            Expect.notEqual plain.ModelKey changed.ModelKey ""
            Expect.isTrue (changed.ModelName.StartsWith "m-") changed.ModelName
            Expect.equal (databaseName plain) "tessera-conformance-m" ""
            Expect.isEmpty (validate changed) ""

            match prepared changed, prepared plain with
            | Ok p, Ok original ->
                let m = p.Database.Model
                Expect.equal m.Tables.Count 4 "T, F, U, C"
                Expect.isTrue (m.Tables.["T"].Measures.ContainsName "Extra") ""
                Expect.isTrue (m.Tables.["T"].Columns.ContainsName "k2") ""
                Expect.equal m.Relationships.Count 2 ""
                Expect.equal m.Functions.Count 1 ""
                Expect.equal (p.Store.TryTable("F").Value.RowCount) 0 "rows replaced"

                Expect.isFalse
                    (original.Database.Model.Tables.["T"].Measures.ContainsName "Extra")
                    "the shared model is untouched"
            | a, b -> failtestf "%A %A" a b

        testCase "an engine that answers as expected passes"
        <| fun () -> Expect.equal (run (answering [ int64Result "[r]" 1L ]) (single (caseText ""))) Outcome.Pass ""

        testCase "an engine is given the model, the store and the query"
        <| fun () ->
            let c = single (caseText "")
            let mutable seen = None

            let evaluate: Evaluate =
                fun db store dax ->
                    seen <- Some(db.Model.Tables.Count, store.Tables.Length, dax)
                    Ok [ int64Result "[r]" 1L ]

            run evaluate c |> ignore
            Expect.equal seen (Some(2, 2, c.Query)) ""

        testCase "a wrong value fails with the value-level difference"
        <| fun () ->
            match run (answering [ int64Result "[r]" 2L ]) (single (caseText "")) with
            | Outcome.Fail(Mismatch.Differences [ 0, comparison ] as mismatch) ->
                Expect.equal
                    comparison.Differences
                    [
                        ResultDifference.Value(0, 0, "[r]", QueryValue.Int64 1L, QueryValue.Int64 2L)
                    ]
                    ""

                Expect.isFalse (Mismatch.isTypeOnly mismatch) ""
                Expect.equal (Mismatch.describe mismatch) [ "row 0, [r]: 2 (int64), expected 1 (int64)" ] ""
            | other -> failtestf "%A" other

        testCase "the right value in the wrong type fails, and is told apart"
        <| fun () ->
            let actual: QueryResult = {
                Columns = [
                    {
                        Name = "[r]"
                        Type = QueryType.Double
                    }
                ]
                Rows = [ [| QueryValue.Double 1.0 |] ]
                Truncated = false
            }

            match run (answering [ actual ]) (single (caseText "")) with
            | Outcome.Fail mismatch -> Expect.isTrue (Mismatch.isTypeOnly mismatch) ""
            | other -> failtestf "%A" other

        testCase "a column with no type asserts none"
        <| fun () ->
            let c =
                single ((caseText "").Replace("""{ "name": "[r]", "type": "int64" }""", """{ "name": "[r]" }"""))

            Expect.equal c.UntypedColumns (set [ "[r]" ]) ""

            let actual (v: QueryValue) : QueryResult = {
                Columns = [
                    {
                        Name = "[r]"
                        Type = QueryType.Decimal
                    }
                ]
                Rows = [ [| v |] ]
                Truncated = false
            }

            Expect.equal (run (answering [ actual (QueryValue.Decimal 1M) ]) c) Outcome.Pass ""
            Expect.notEqual (run (answering [ actual (QueryValue.Decimal 2M) ]) c) Outcome.Pass ""

        testCase "the wrong number of results fails"
        <| fun () ->
            Expect.equal (run (answering []) (single (caseText ""))) (Outcome.Fail(Mismatch.ResultCount(1, 0))) ""

        testCase "ordered cases compare rows in order; others as a multiset"
        <| fun () ->
            let text (ordered: bool) =
                (caseText (if ordered then """, "ordered": true""" else ""))
                    .Replace("ROW ( \\\"r\\\", 1 )", "{ 1, 2 } ORDER BY [Value]")
                    .Replace("\"[r]\"", "\"[Value]\"")
                    .Replace("[ [1] ]", "[ [1], [2] ]")

            let reversed: QueryResult = {
                Columns = [
                    {
                        Name = "[Value]"
                        Type = QueryType.Int64
                    }
                ]
                Rows = [ [| QueryValue.Int64 2L |]; [| QueryValue.Int64 1L |] ]
                Truncated = false
            }

            Expect.equal (run (answering [ reversed ]) (single (text false))) Outcome.Pass ""
            Expect.notEqual (run (answering [ reversed ]) (single (text true))) Outcome.Pass ""

        testCase "an engine error is an error; one that says not supported is not-supported"
        <| fun () ->
            let c = single (caseText "")

            match run (failing (QueryFailed "boom")) c with
            | Outcome.Error message -> Expect.stringContains message "boom" ""
            | other -> failtestf "%A" other

            let config = {
                RunConfig.defaults with
                    IsNotSupported =
                        function
                        | QueryFailed reason -> reason.StartsWith "not supported"
                        | _ -> false
            }

            match runWith config (failing (QueryFailed "not supported: RANKX")) c with
            | Outcome.NotSupported reason -> Expect.stringContains reason "RANKX" ""
            | other -> failtestf "%A" other

            match runWith config (failing (QueryFailed "boom")) c with
            | Outcome.Error _ -> ()
            | other -> failtestf "%A" other

        testCase "an engine that throws is an error, not a crash"
        <| fun () ->
            match run (fun _ _ _ -> failwith "kaboom") (single (caseText "")) with
            | Outcome.Error message -> Expect.stringContains message "kaboom" ""
            | other -> failtestf "%A" other

        testCase "a case that expects an error passes on any error, and fails on a result"
        <| fun () ->
            let c =
                single (
                    (caseText "")
                        .Replace(
                            """[ { "columns": [ { "name": "[r]", "type": "int64" } ], "rows": [ [1] ] } ]""",
                            """{ "error": "text can't be compared with a number" }"""
                        )
                )

            Expect.equal c.Expected (Expected.Error "text can't be compared with a number") ""
            Expect.isEmpty (validate c) ""
            Expect.equal (run (failing (QueryFailed "whatever the engine says")) c) Outcome.Pass ""

            match run (answering [ int64Result "[r]" 1L ]) c with
            | Outcome.Fail(Mismatch.ExpectedError _ as mismatch) ->
                Expect.stringContains (Mismatch.describe mismatch).Head "expected an error" ""
            | other -> failtestf "%A" other

        testCase "a case whose model doesn't build is an error for every engine"
        <| fun () ->
            let c =
                single (
                    (caseText "")
                        .Replace(
                            "\"m\"",
                            """{ "base": "m", "addRelationships": [ { "from": "F[k]", "to": "Nope[k]" } ] }"""
                        )
                )

            match run (answering [ int64Result "[r]" 1L ]) c with
            | Outcome.Error message -> Expect.stringContains message "Nope" ""
            | other -> failtestf "%A" other

            Expect.exists (validate c) (fun p -> p.StartsWith "model:") ""

        testCase "a case can run as members of the model's roles, and only an engine that takes them runs it"
        <| fun () ->
            let roles =
                """{ "base": "m", "roles": [ { "name": "One", "filters": { "T": "T[k] = 1" } },
                                             { "name": "Admins", "permission": "administrator" } ],
                     "relationships": [ { "from": "F[k]", "to": "T[k]", "securityFilter": "both", "crossFilter": "both" } ] }"""

            let c =
                single (
                    (caseText ", \"as\": { \"roles\": [ \"One\" ], \"customData\": \"x\" }").Replace("\"m\"", roles)
                )

            Expect.equal
                c.As
                (Some {
                    Roles = [ "One" ]
                    CustomData = Some "x"
                })
                "as"

            Expect.isEmpty (validate c) ""

            match prepared c with
            | Ok p ->
                let r = p.Database.Model.Roles.["One"]
                Expect.equal r.ModelPermission Microsoft.AnalysisServices.Tabular.ModelPermission.Read "read by default"
                Expect.equal r.TablePermissions.["T"].FilterExpression "T[k] = 1" "the filter"

                Expect.equal
                    p.Database.Model.Roles.["Admins"].ModelPermission
                    Microsoft.AnalysisServices.Tabular.ModelPermission.Administrator
                    "administrator"

                let rel =
                    p.Database.Model.Relationships.[0] :?> Microsoft.AnalysisServices.Tabular.SingleColumnRelationship

                Expect.equal
                    rel.SecurityFilteringBehavior
                    Microsoft.AnalysisServices.Tabular.SecurityFilteringBehavior.BothDirections
                    "securityFilter"
            | Error e -> failtest e

            match run (answering [ int64Result "[r]" 1L ]) c with
            | Outcome.Error message -> Expect.stringContains message "runAsWith" ""
            | other -> failtestf "%A" other

            let mutable seen = None

            let evaluate: EvaluateAs =
                fun runAs _ _ _ ->
                    seen <- runAs
                    Ok [ int64Result "[r]" 1L ]

            Expect.equal (runAsWith RunConfig.defaults evaluate c) Outcome.Pass ""
            Expect.equal seen c.As "the engine is told the roles"

        testCase "a role's permission and filters are checked"
        <| fun () ->
            let t, corpus =
                corpusOf [
                    "area/bad.json",
                    (caseText "")
                        .Replace("\"m\"", """{ "base": "m", "roles": [ { "name": "R", "permission": "owner" } ] }""")
                ]

            use _ = t

            let c = corpus.Cases.Head

            match c.Model.Value with
            | Error message -> Expect.stringContains message "owner" ""
            | Ok _ -> failtest "an unknown permission"
    ]
