/// Tabular Editor's database.json folder format (FolderFormat), portable file names, and the PBIP
/// "merge, never clobber" rules.
module Tessera.Tests.FolderFormatTests

open System
open System.IO
open System.Text.Json.Nodes
open Expecto
open FsCheck
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.FolderFormat
open Tessera.ModelIO
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "unexpected error: %s" (TessError.message e)

let private okText =
    function
    | Ok v -> v
    | Error(e: string) -> failtestf "unexpected error: %s" e

let private reload path = loadPath path |> ok |> snd

let private bimText (db: Database) = toBimText SaveOptions.defaults db

let private setOptions (json: string) (db: Database) =
    match db.Model.Annotations.Find FolderOptions.annotationName with
    | null -> db.Model.Annotations.Add(Annotation(Name = FolderOptions.annotationName, Value = json))
    | a -> a.Value <- json

    db

let private withOptions (o: FolderOptions) (db: Database) = setOptions (FolderOptions.toJson o) db

/// The collections the folder format splits into files: without PrefixFilenames, their order is
/// not stored, so a round trip reads them back in file-name order.
let private splitCollections =
    set [
        "tables"
        "columns"
        "measures"
        "hierarchies"
        "partitions"
        "calculationItems"
        "dataSources"
        "expressions"
        "functions"
        "relationships"
        "roles"
        "perspectives"
        "cultures"
        "annotations"
    ]

/// A .bim text with split collections sorted by name, and (optionally) without the options
/// annotation, for comparing round trips that don't keep order.
let private canonical (dropOptions: bool) (bim: string) =
    let root = JsonText.parse bim

    let rec go (key: string) (node: JsonNode) =
        match node with
        | :? JsonObject as o ->
            for KeyValue(k, v) in List.ofSeq o do
                go k v
        | :? JsonArray as a ->
            for v in a do
                go "" v

            let named =
                a
                |> Seq.forall (fun n ->
                    match n with
                    | :? JsonObject as o -> o.ContainsKey "name"
                    | _ -> false)

            if splitCollections.Contains key && named && a.Count > 1 then
                let items = [ for n in a -> n ]
                a.Clear()

                for n in items |> List.sortBy (fun n -> n.["name"].ToJsonString()) do
                    a.Add n
        | _ -> ()

    go "" root

    if dropOptions then
        match root.["model"] with
        | :? JsonObject as m ->
            match m.["annotations"] with
            | :? JsonArray as anns ->
                for a in List.ofSeq anns do
                    if a.["name"].GetValue<string>() = FolderOptions.annotationName then
                        anns.Remove a |> ignore

                if anns.Count = 0 then
                    m.Remove "annotations" |> ignore
            | _ -> ()
        | _ -> ()

    JsonText.write "\n" root

let private sampleFolder (t: TempDir) (db: Database) =
    let dir = t.Combine "folder"
    save DatabaseJsonFolder SaveOptions.defaults false db dir |> ok |> ignore
    dir

let private relativeFiles (dir: string) =
    Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
    |> Array.map (fun f -> Path.GetRelativePath(dir, f).Replace('\\', '/'))
    |> Array.sortWith (fun a b -> String.CompareOrdinal(a, b))
    |> List.ofArray

/// Golden files live in tests/Tessera.Tests/golden; TESSERA_UPDATE_GOLDEN=1 rewrites them.
let private golden (name: string) (actual: string) =
    match Conformance.repositoryRoot with
    | None -> skiptest "no repository checkout next to the test binaries"
    | Some root ->
        let path = Path.Combine(root, "tests", "Tessera.Tests", "golden", name)

        if Environment.GetEnvironmentVariable "TESSERA_UPDATE_GOLDEN" = "1" then
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, actual)
        else
            Expect.isTrue (File.Exists path) $"golden file {name} is missing (set TESSERA_UPDATE_GOLDEN=1)"
            // Byte for byte: .gitattributes keeps these LF on every OS.
            Expect.equal (File.ReadAllText path) actual name

/// Every file of a set as one text: "=== path" headers, then the content as written.
let private manifest (files: (string * string) list) =
    let sb = Text.StringBuilder()

    for path, text in files do
        sb.Append("=== ").Append(path).Append('\n').Append(text).Append('\n') |> ignore

    sb.ToString()

/// A model whose object names stress file naming: device names, Windows-invalid characters, names
/// that escape to the same text, case-only differences, trailing dots and spaces, non-ASCII.
let tricky () =
    let db = sampleDatabase ()
    let m = db.Model

    let table (name: string) =
        let t = new Table(Name = name)
        t.Columns.Add(new DataColumn(Name = "c", DataType = DataType.String, SourceColumn = "c"))
        t.Partitions.Add(new Partition(Name = name + " p", Source = new MPartitionSource(Expression = "1")))
        m.Tables.Add t
        t

    let con = table "CON"
    table "_CON" |> ignore
    table "nul.txt" |> ignore
    table "Trailing." |> ignore
    table "Trailing " |> ignore
    // TE2's escaping gives both a%3ab.json.
    table "a:b" |> ignore
    table "a%3ab" |> ignore
    table "Café" |> ignore
    con.Measures.Add(new Measure(Name = "Growth %20", Expression = "1"))
    con.Measures.Add(new Measure(Name = "x/y", Expression = "2"))
    con.Measures.Add(new Measure(Name = "AUX", Expression = "3"))
    db

[<Tests>]
let tests =
    testList "FolderFormat" [
        testList "safe names" [
            testCase "TE2's escaping, with a fixed character set"
            <| fun () ->
                Expect.equal (SafeName.escape false "a:b") "a%3ab" "invalid characters, lowercase hex"
                Expect.equal (SafeName.escape false "a/b\\c") "a%2fb%5cc" ""
                Expect.equal (SafeName.escape false "x%20y") "x%2520y" "a literal %20 keeps its meaning"
                Expect.equal (SafeName.escape false "x%5fy") "x%255fy" ""
                Expect.equal (SafeName.escape false "Growth %") "Growth %" "other % signs stay"
                Expect.equal (SafeName.escape false "CON") "_CON" "device name"
                Expect.equal (SafeName.escape false "_CON") "%5FCON" "a name that looks escaped"
                Expect.equal (SafeName.escape false "nul.txt") "_nul.txt" "device name with an extension"
                Expect.equal (SafeName.escape false "COM0") "_COM0" ""
                Expect.equal (SafeName.escape false "Console") "Console" ""
                Expect.equal (SafeName.escape true "Sales ") "Sales%20" "a folder can't end in a space"
                Expect.equal (SafeName.escape true "Sales.") "Sales%2e" "nor a dot"
                Expect.equal (SafeName.escape true "..") ".%2e" ""
                Expect.equal (SafeName.escape false "Café ünïcode") "Café ünïcode" "non-ASCII stays"

            testCase "colliding names get ~n, the first keeps its name"
            <| fun () ->
                let names = [ "Sales"; "sales"; "SALES"; "Other"; "sales~2" ]

                Expect.equal
                    (SafeName.uniqueIn ".json" names)
                    [ "Sales"; "sales~2"; "SALES~3"; "Other"; "sales~2~2" ]
                    ""

                // macOS treats composed and decomposed forms as one name.
                let composed = "Caf\u00E9"
                let decomposed = "Cafe\u0301"
                Expect.equal (SafeName.uniqueIn "" [ composed; decomposed ]) [ composed; decomposed + "~2" ] ""

            testProperty "escaped names are portable"
            <| fun (NonNull(name: string)) ->
                SafeName.isPortable (SafeName.escape true name)
                && SafeName.isPortable (SafeName.escape false name + ".json")

            testProperty "unique names never collide, and keep first occurrences"
            <| fun (names: NonNull<string> list) ->
                let names = names |> List.map (fun (NonNull n) -> SafeName.escape false n)
                let unique = SafeName.uniqueIn ".json" names
                let keys = unique |> List.map (fun n -> SafeName.collisionKey (n + ".json"))

                List.length (List.distinct keys) = List.length keys
                && List.length unique = List.length names
                && (List.isEmpty names || List.head unique = List.head names)
        ]

        testList "JSON text" [
            testCase "Json.NET's escaping"
            <| fun () ->
                Expect.equal (JsonText.quote "a\"b\\c") "\"a\\\"b\\\\c\"" ""
                Expect.equal (JsonText.quote "line\nnext\r\ttab") "\"line\\nnext\\r\\ttab\"" ""
                Expect.equal (JsonText.quote "\u0001") "\"\\u0001\"" ""
                Expect.equal (JsonText.quote "'Sales'[Amount] <> 0 && é ✓") "\"'Sales'[Amount] <> 0 && é ✓\"" ""

            testCase "re-writing TOM's JSON gives TOM's text back"
            <| fun () ->
                let db = sampleDatabase ()

                db.Model.Tables.["Sales"].Measures.["Total"].Expression <-
                    "IF([x] <> 0 && 'Sales'[y] > 1, \"é\", \"✓ 😀\")"

                let bim = bimText db
                Expect.equal (JsonText.write "\n" (JsonText.parse bim)) bim ""
        ]

        testList "serialize options annotation" [
            testCase "TE2's annotation text parses; the old calculation items level is normalized"
            <| fun () ->
                // From TE2's TOMWrapperTest/RegressionTests/v2_28/SerializeOptionsCompatibilityTests.cs.
                let o =
                    FolderOptions.parse
                        "{\"IgnoreInferredObjects\":true,\"IgnoreInferredProperties\":true,\"IgnoreTimestamps\":true,\"SplitMultilineStrings\":true,\"PrefixFilenames\":false,\"LocalTranslations\":false,\"LocalPerspectives\":false,\"LocalRelationships\":false,\"Levels\":[\"Data Sources\",\"Perspectives\",\"Relationships\",\"Roles\",\"Tables\",\"Tables/CalculationItems\",\"Translations\"]}"
                    |> okText

                Expect.contains o.Levels "Tables/Calculation Items" ""
                Expect.isFalse (List.contains "Tables/CalculationItems" o.Levels) ""
                Expect.isFalse o.LocalTranslations ""

            testCase "missing keys take TE's defaults; keys match case-insensitively"
            <| fun () ->
                let o =
                    FolderOptions.parse "{\"levels\":[\"Tables\"],\"prefixfilenames\":true}"
                    |> okText

                Expect.equal o.Levels [ "Tables" ] ""
                Expect.isTrue o.PrefixFilenames ""
                Expect.isTrue o.IgnoreInferredObjects "default"

            testCase "TE's default folder options, in TE's key order"
            <| fun () ->
                Expect.equal
                    (FolderOptions.toJson FolderOptions.defaultFolder)
                    "{\"IgnoreInferredObjects\":true,\"IgnoreInferredProperties\":true,\"IgnoreTimestamps\":true,\"SplitMultilineStrings\":true,\"PrefixFilenames\":false,\"LocalTranslations\":false,\"LocalPerspectives\":false,\"SortArrays\":false,\"LocalRelationships\":false,\"Levels\":[\"Data Sources\",\"Functions\",\"Shared Expressions\",\"Perspectives\",\"Relationships\",\"Roles\",\"Tables\",\"Tables/Columns\",\"Tables/Hierarchies\",\"Tables/Measures\",\"Tables/Partitions\",\"Tables/Calculation Items\",\"Translations\"]}"
                    ""

                Expect.equal
                    (FolderOptions.parse (FolderOptions.toJson FolderOptions.defaultFolder))
                    (Ok FolderOptions.defaultFolder)
                    ""

            testCase "bad annotations are errors"
            <| fun () ->
                Expect.isError (FolderOptions.parse "{not json") ""
                Expect.isError (FolderOptions.parse "{\"PrefixFilenames\":\"yes\"}") ""
                Expect.isError (FolderOptions.parse "{\"Levels\":\"Tables\"}") ""
        ]

        testList "round trips" [
            testCase "bim -> database.json -> bim: the same model, plus the options annotation"
            <| fun () ->
                use t = new TempDir()
                let original = sampleDatabase ()
                let dir = sampleFolder t original
                let back = reload dir
                Expect.equal (canonical true (bimText back)) (canonical true (bimText original)) ""

                Expect.equal
                    (back.Model.Annotations.Find(FolderOptions.annotationName).Value)
                    (FolderOptions.toJson FolderOptions.defaultFolder)
                    "a new folder records TE's default levels"

                Expect.isNull (original.Model.Annotations.Find FolderOptions.annotationName) "the source is not changed"

            testCase "with PrefixFilenames, the order survives too: the same .bim text"
            <| fun () ->
                use t = new TempDir()

                let original =
                    sampleDatabase ()
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            PrefixFilenames = true
                            Levels = FolderOptions.knownLevels
                    }

                let back = reload (sampleFolder t original)
                Expect.equal (bimText back) (bimText original) ""

                Expect.isTrue
                    (File.Exists(t.Combine("folder", "tables", "000 Sales", "measures", "001 Total (hidden).json")))
                    ""

            testCase "with no levels, database.json is the .bim text"
            <| fun () ->
                use t = new TempDir()
                let original = sampleDatabase () |> withOptions FolderOptions.empty
                let dir = sampleFolder t original
                Expect.equal (relativeFiles dir) [ "database.json" ] ""

                Expect.equal
                    (File.ReadAllText(Path.Combine(dir, "database.json")) + "\n")
                    (File.ReadAllText((save BimFile SaveOptions.defaults false original (t.Combine "m.bim") |> ok).Path))
                    ""

            testCase "with PrefixFilenames, more than 1000 objects keep their order"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase ()
                let sales = db.Model.Tables.["Sales"]

                for i in 1..1005 do
                    sales.Measures.Add(new Measure(Name = $"m{1005 - i}", Expression = "1"))

                let original =
                    db
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            PrefixFilenames = true
                    }

                let back = reload (sampleFolder t original)
                Expect.equal (bimText back) (bimText original) ""
                Expect.isTrue (File.Exists(t.Combine("folder", "tables", "000 Sales", "measures", "1006 m0.json"))) ""

            testCase "file order: number prefixes by number, then the rest ordinally"
            <| fun () ->
                let names = [ "b"; "101 x"; "1000 y"; "a"; "002 z"; "9x"; "B" ]

                Expect.equal (names |> List.sortWith fileOrder) [ "002 z"; "101 x"; "1000 y"; "9x"; "B"; "a"; "b" ] ""

            testCase "every level, and back"
            <| fun () ->
                use t = new TempDir()

                let original =
                    sampleDatabase ()
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            Levels = FolderOptions.knownLevels
                    }

                let dir = sampleFolder t original
                Expect.equal (canonical false (bimText (reload dir))) (canonical false (bimText original)) ""

            testCase "local translations, perspectives and relationships, and back"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase ()
                let m = db.Model
                let sales = m.Tables.["Sales"]
                let da = m.Cultures.["da-DK"]
                da.ObjectTranslations.SetTranslation(sales, TranslatedProperty.Caption, "Salg")
                da.ObjectTranslations.SetTranslation(sales.Measures.["Total"], TranslatedProperty.Caption, "I alt")

                da.ObjectTranslations.SetTranslation(
                    sales.Measures.["Total"],
                    TranslatedProperty.DisplayFolder,
                    "Mappe"
                )

                da.ObjectTranslations.SetTranslation(m, TranslatedProperty.Description, "Modellen")
                let view = m.Perspectives.["Sales View"]
                let pt = PerspectiveTable(Table = sales)
                view.PerspectiveTables.Add pt
                pt.PerspectiveMeasures.Add(PerspectiveMeasure(Measure = sales.Measures.["Total"]))
                pt.PerspectiveColumns.Add(PerspectiveColumn(Column = sales.Columns.["Amount"]))

                let original =
                    db
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            LocalTranslations = true
                            LocalPerspectives = true
                            LocalRelationships = true
                    }

                let dir = sampleFolder t original
                let files = relativeFiles dir

                Expect.isFalse
                    (files |> List.exists (fun f -> f.StartsWith "relationships/"))
                    "relationships are on the tables"

                Expect.isFalse (files |> List.exists (fun f -> f.StartsWith "perspectives/")) ""
                Expect.isFalse (files |> List.exists (fun f -> f.StartsWith "cultures/")) ""
                let salesJson = File.ReadAllText(Path.Combine(dir, "tables", "Sales", "Sales.json"))
                Expect.stringContains salesJson "TabularEditor_Relationships" ""
                Expect.stringContains salesJson "Sales-Date" "the relationship's own name is kept"
                Expect.stringContains salesJson "{\\\"da-DK\\\":\\\"Salg\\\"}" "TE's translation annotation"
                let back = reload dir
                Expect.equal (canonical false (bimText back)) (canonical false (bimText original)) ""

            testCase "local translations keep a culture's linguistic metadata (TE2 drops it)"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase ()

                db.Model.Cultures.["da-DK"].LinguisticMetadata <-
                    LinguisticMetadata(
                        ContentType = ContentType.Json,
                        Content = "{\"Version\":\"1.0.0\",\"Language\":\"da-DK\"}"
                    )

                db.Model.Cultures.["da-DK"].ObjectTranslations.SetTranslation(
                    db.Model.Tables.["Sales"],
                    TranslatedProperty.Caption,
                    "Salg"
                )

                let original =
                    db
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            LocalTranslations = true
                    }

                let dir = sampleFolder t original
                Expect.isTrue (File.Exists(Path.Combine(dir, "cultures", "da-DK.json"))) "the culture keeps a file"

                Expect.isFalse
                    (File.ReadAllText(Path.Combine(dir, "cultures", "da-DK.json")).Contains "translations")
                    ""

                let back = reload dir
                Expect.isNotNull back.Model.Cultures.["da-DK"].LinguisticMetadata ""
                Expect.equal (canonical false (bimText back)) (canonical false (bimText original)) ""

            testCase "a tricky model's names survive on disk"
            <| fun () ->
                use t = new TempDir()
                let original = tricky ()
                let dir = sampleFolder t original
                Expect.equal (canonical true (bimText (reload dir))) (canonical true (bimText original)) ""

                for f in relativeFiles dir do
                    for segment in f.Split '/' do
                        Expect.isTrue (SafeName.isPortable segment) f

            testCase "spaceparts (TESSERA_SAMPLE_MODEL): its own TE layout, and back"
            <| fun () ->
                match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
                | null
                | "" -> skiptest "TESSERA_SAMPLE_MODEL is not set"
                | path ->
                    use t = new TempDir()
                    let original = reload path
                    let dir = sampleFolder t original
                    Expect.equal (canonical false (bimText (reload dir))) (canonical false (bimText original)) ""

                    let prefixed =
                        save
                            DatabaseJsonFolder
                            SaveOptions.defaults
                            false
                            (original
                             |> withOptions {
                                 FolderOptions.defaultFolder with
                                     PrefixFilenames = true
                             })
                            (t.Combine "prefixed")
                        |> ok

                    Expect.equal (bimText (reload prefixed.Path)) (bimText original) "exact, with prefixes"
        ]

        testList "TE2 compatibility" [
            // TE2's TOMWrapperTest/RegressionTests/v2_8/SerializationAnnotationsTests.cs: a database.json
            // TE2 wrote with LocalTranslations, LocalPerspectives and LocalRelationships. TE2's test
            // loads it and checks a save reproduces it exactly; so does this one.
            // Copyright (c) 2017 Daniel Otykier. MIT License.
            testCase "a TE2-written folder with local annotations loads, and saves back byte for byte"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "te2"
                write (Path.Combine(dir, "database.json")) Te2Fixture.databaseJson
                let source, db = loadPath dir |> ok
                Expect.equal source.Format DatabaseJson ""
                let t1 = db.Model.Tables.["Table 1"]

                Expect.equal
                    (db.Model.Cultures.["da-DK"].ObjectTranslations
                        .[t1.Columns.["Column 1"], TranslatedProperty.Caption].Value)
                    "Test 1"
                    ""

                Expect.equal db.Model.Relationships.Count 1 ""
                let p2 = db.Model.Perspectives.["Perspective 2"]
                Expect.isNotNull (p2.PerspectiveTables.Find "Table 2") ""
                Expect.equal p2.PerspectiveTables.["Table 2"].PerspectiveColumns.Count 2 ""

                Expect.isNull
                    (t1.Annotations.Find "TabularEditor_InPerspective")
                    "TE's annotations are resolved, then gone"

                let again =
                    save DatabaseJsonFolder SaveOptions.defaults false db (t.Combine "again") |> ok

                Expect.equal (File.ReadAllText(Path.Combine(again.Path, "database.json"))) Te2Fixture.databaseJson ""

            testCase "a split TE2 folder: tables/<name>/<name>.json and per-object files"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "split"

                write
                    (Path.Combine(dir, "database.json"))
                    "{\n  \"name\": \"Db\",\n  \"compatibilityLevel\": 1702,\n  \"model\": {\n    \"culture\": \"en-US\"\n  }\n}"

                write
                    (Path.Combine(dir, "tables", "Sales", "Sales.json"))
                    "{\n  \"name\": \"Sales\",\n  \"partitions\": [ { \"name\": \"p\", \"source\": { \"type\": \"m\", \"expression\": \"1\" } } ]\n}"

                write
                    (Path.Combine(dir, "tables", "Sales", "columns", "Amount.json"))
                    "{ \"name\": \"Amount\", \"dataType\": \"decimal\", \"sourceColumn\": \"Amount\" }"

                write
                    (Path.Combine(dir, "tables", "Sales", "measures", "Total.json"))
                    "{ \"name\": \"Total\", \"expression\": \"SUM(Sales[Amount])\" }"

                write (Path.Combine(dir, "tables", "Sales", "measures", "notes.txt")) "not a model file"

                write
                    (Path.Combine(dir, "functions", "Double.json"))
                    "{ \"name\": \"Double\", \"expression\": \"(x) => x * 2\" }"

                let db = reload (Path.Combine(dir, "database.json"))
                Expect.equal db.Model.Tables.["Sales"].Measures.["Total"].Expression "SUM(Sales[Amount])" ""
                Expect.equal db.Model.Functions.Count 1 ""

            testCase "a table folder with two .json files is refused, as TE2 does"
            <| fun () ->
                use t = new TempDir()
                let dir = t.Combine "bad"

                write
                    (Path.Combine(dir, "database.json"))
                    "{ \"name\": \"Db\", \"compatibilityLevel\": 1500, \"model\": {} }"

                write (Path.Combine(dir, "tables", "Sales", "Sales.json")) "{ \"name\": \"Sales\" }"
                write (Path.Combine(dir, "tables", "Sales", "Other.json")) "{ \"name\": \"Other\" }"

                match loadPath dir with
                | Error(LoadFailed(_, reason)) -> Expect.stringContains reason "exactly one .json file" ""
                | other -> failtestf "expected a load failure, got %A" (Result.map fst other)
        ]

        testList "layout" [
            testCase "the default layout of the sample model (golden)"
            <| fun () ->
                let files = databaseJsonFiles SaveOptions.defaults (sampleDatabase ()) |> okText
                golden "database-json-sample.txt" (manifest [ for f in files -> f.Path, f.Text ])

            testCase "a tricky model's file names (golden)"
            <| fun () ->
                let files = databaseJsonFiles SaveOptions.defaults (tricky ()) |> okText
                golden "database-json-tricky-names.txt" (String.Join("\n", files |> List.map (fun f -> f.Path)) + "\n")

            testCase "CRLF on request, and nothing else changes"
            <| fun () ->
                let lf = databaseJsonFiles SaveOptions.defaults (sampleDatabase ()) |> okText

                let crlf =
                    databaseJsonFiles { SaveOptions.defaults with Eol = CrLf } (sampleDatabase ())
                    |> okText

                for a, b in List.zip lf crlf do
                    Expect.equal b.Path a.Path ""
                    Expect.equal (b.Text.Replace("\r\n", "\n")) a.Text a.Path
                    Expect.isFalse (b.Text.Replace("\r\n", "").Contains "\n") "only CRLF"

            testCase "functions (TE3 3.27) and calculation items (TE3 3.25.5) get files of their own"
            <| fun () ->
                let files =
                    databaseJsonFiles SaveOptions.defaults (sampleDatabase ())
                    |> okText
                    |> List.map (fun f -> f.Path)

                Expect.contains files "functions/Double.json" ""
                Expect.contains files "tables/Time Intelligence/calculationItems/YTD.json" ""

            testCase "an existing annotation decides the layout"
            <| fun () ->
                let files =
                    sampleDatabase ()
                    |> setOptions "{\"Levels\":[\"Tables\",\"Tables/Measures\"]}"
                    |> databaseJsonFiles SaveOptions.defaults
                    |> okText
                    |> List.map (fun f -> f.Path)

                Expect.contains files "tables/Sales/measures/Total.json" ""
                Expect.isFalse (files |> List.exists (fun f -> f.Contains "/columns/")) "columns stay in the table file"
                Expect.isFalse (files |> List.exists (fun f -> f.StartsWith "functions/")) "functions stay inline"

            testCase "SortArrays sorts roles"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.Roles.Add(new ModelRole(Name = "Admins", ModelPermission = ModelPermission.Administrator))

                let file =
                    db
                    |> setOptions "{\"SortArrays\":true,\"Levels\":[]}"
                    |> databaseJsonFiles SaveOptions.defaults
                    |> okText
                    |> List.exactlyOne

                Expect.isLessThan (file.Text.IndexOf "\"Admins\"") (file.Text.IndexOf "\"Readers\"") ""

            testCase "IgnorePrivacySettings drops credentials' privacy settings"
            <| fun () ->
                let bim =
                    "{\n  \"name\": \"Db\",\n  \"model\": {\n    \"dataSources\": [\n      {\n        \"type\": \"structured\",\n        \"name\": \"Sql\",\n        \"credential\": {\n          \"AuthenticationKind\": \"ServiceAccount\",\n          \"PrivacySetting\": \"Organizational\"\n        }\n      }\n    ]\n  }\n}"

                let o = {
                    FolderOptions.empty with
                        IgnorePrivacySettings = true
                }

                let file = split o "\n" bim |> okText |> List.exactlyOne
                Expect.isFalse (file.Text.Contains "PrivacySetting") ""
                Expect.stringContains file.Text "AuthenticationKind" ""
                let kept = split FolderOptions.empty "\n" bim |> okText |> List.exactlyOne
                Expect.equal kept.Text bim "without the option, the text is as it was"

            testCase "IgnoreLineageTags drops lineage tags"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.Tables.["Sales"].LineageTag <- "abc"

                let text =
                    db
                    |> setOptions "{\"IgnoreLineageTags\":true,\"Levels\":[]}"
                    |> databaseJsonFiles SaveOptions.defaults
                    |> okText
                    |> List.exactlyOne

                Expect.isFalse (text.Text.Contains "lineageTag") ""

            testCase "refused: IgnoreIncrementalRefreshPartitions with incremental refresh partitions"
            <| fun () ->
                let db = sampleDatabase ()
                let sales = db.Model.Tables.["Sales"]

                sales.Partitions.Add(
                    new Partition(
                        Name = "2024",
                        Mode = ModeType.Import,
                        Source =
                            PolicyRangePartitionSource(
                                Start = DateTime(2024, 1, 1),
                                End = DateTime(2025, 1, 1),
                                Granularity = RefreshGranularityType.Year
                            )
                    )
                )

                db
                |> setOptions "{\"IgnoreIncrementalRefreshPartitions\":true,\"Levels\":[]}"
                |> databaseJsonFiles SaveOptions.defaults
                |> Expect.isError
                <| ""

            testCase "refused: LocalPerspectives with annotations TE's format can't hold"
            <| fun () ->
                let db = sampleDatabase ()
                let pt = PerspectiveTable(Table = db.Model.Tables.["Sales"])
                db.Model.Perspectives.["Sales View"].PerspectiveTables.Add pt
                pt.Annotations.Add(Annotation(Name = "x", Value = "y"))

                match
                    db
                    |> withOptions {
                        FolderOptions.defaultFolder with
                            LocalPerspectives = true
                    }
                    |> databaseJsonFiles SaveOptions.defaults
                with
                | Error reason -> Expect.stringContains reason "LocalPerspectives" ""
                | Ok _ -> failtest "expected a refusal"

            testCase "refused: an unreadable options annotation"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase () |> setOptions "{oops"

                match save DatabaseJsonFolder SaveOptions.defaults false db (t.Combine "f") with
                | Error(SaveFailed(_, reason)) -> Expect.stringContains reason FolderOptions.annotationName ""
                | other -> failtestf "expected a save failure, got %A" (Result.map (fun s -> s.Path) other)
        ]

        testList "saving into a folder" [
            testCase "stale object files go, other files stay, unchanged files aren't rewritten"
            <| fun () ->
                use t = new TempDir()
                let dir = sampleFolder t (sampleDatabase ())
                write (Path.Combine(dir, "README.md")) "notes"
                write (Path.Combine(dir, "extra", "keep.json")) "{}"
                let untouched = Path.Combine(dir, "tables", "Date", "Date.json")
                let stamp = DateTime(2001, 1, 1, 0, 0, 0, DateTimeKind.Utc)
                File.SetLastWriteTimeUtc(untouched, stamp)
                let db = reload dir
                db.Model.Tables.["Sales"].Measures.["Total"].Name <- "Grand Total"
                save DatabaseJsonFolder SaveOptions.defaults true db dir |> ok |> ignore
                let files = relativeFiles dir
                Expect.contains files "tables/Sales/measures/Grand Total.json" ""
                Expect.isFalse (List.contains "tables/Sales/measures/Total.json" files) "the old file is gone"
                Expect.contains files "README.md" ""
                Expect.contains files "extra/keep.json" ""
                Expect.equal (File.GetLastWriteTimeUtc untouched) stamp "an unchanged file isn't rewritten"

            testCase "a case-only rename keeps the object"
            <| fun () ->
                use t = new TempDir()
                let dir = sampleFolder t (sampleDatabase ())
                let db = reload dir
                db.Model.Tables.["Sales"].Measures.["Total"].Name <- "TOTAL"
                save DatabaseJsonFolder SaveOptions.defaults true db dir |> ok |> ignore
                Expect.isNotNull ((reload dir).Model.Tables.["Sales"].Measures.Find "TOTAL") ""

            testCase "saving back (--save) keeps the folder format"
            <| fun () ->
                use t = new TempDir()
                let dir = sampleFolder t (sampleDatabase ())
                let source, db = loadPath dir |> ok
                Expect.equal (inPlace source) (DatabaseJsonFolder, dir) ""
                db.Model.Tables.["Sales"].Measures.["Total"].Expression <- "42"
                saveBack source db |> ok |> ignore

                Expect.stringContains
                    (File.ReadAllText(Path.Combine(dir, "tables", "Sales", "measures", "Total.json")))
                    "\"42\""
                    ""

            testCase "model paths: the folder, or its database.json"
            <| fun () ->
                use t = new TempDir()
                let dir = sampleFolder t (sampleDatabase ())

                for p in [ dir; Path.Combine(dir, "database.json") ] do
                    let s = ModelSource.resolve p |> ok
                    Expect.equal s.Format DatabaseJson p
                    Expect.equal s.Path dir p

            testCase "format inference"
            <| fun () ->
                use t = new TempDir()
                let dir = sampleFolder t (sampleDatabase ())
                Expect.equal (OutputFormat.infer dir) DatabaseJsonFolder "an existing folder"

                Expect.equal
                    (OutputFormat.infer (t.Combine("new", "database.json")))
                    DatabaseJsonFolder
                    "a database.json path"

                Expect.equal (OutputFormat.tryParse "database.json") (Some DatabaseJsonFolder) ""
                Expect.equal OutputFormat.tokens [ "bim"; "tmdl"; "pbip"; "database.json" ] ""
        ]

        testList "TMDL file names" [
            testCase "the same file names as TOM's folder serializer for ordinary names"
            <| fun () ->
                use t = new TempDir()
                let db = sampleDatabase ()
                let mine = tmdlFiles SaveOptions.defaults db
                let tomDir = t.Combine "tom"

                // Options matching SaveOptions.defaults, as ModelIO builds them.
                save TmdlFolder SaveOptions.defaults false db (t.Combine "tess") |> ok |> ignore
                let written = relativeFiles (t.Combine "tess")
                Expect.equal (mine |> List.map fst |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))) written ""

                TmdlSerializer.SerializeDatabaseToFolder(db, tomDir)

                Expect.equal (relativeFiles tomDir) written "TOM's own names"

            testCase "the same bytes as TOM's folder serializer, for every TMDL option"
            <| fun () ->
                for opts in
                    [
                        SaveOptions.defaults
                        { SaveOptions.defaults with Eol = CrLf }
                        {
                            SaveOptions.defaults with
                                IndentSpaces = Some 4
                        }
                        {
                            SaveOptions.defaults with
                                IncludeSensitive = true
                        }
                    ] do
                    use t = new TempDir()
                    let db = sampleDatabase ()
                    TmdlSerializer.SerializeDatabaseToFolder(db, t.Path, tmdlOptions opts)

                    let tom =
                        relativeFiles t.Path |> List.map (fun f -> f, File.ReadAllBytes(t.Combine f))

                    let mine =
                        tmdlFiles opts db
                        |> List.sortWith (fun (a, _) (b, _) -> String.CompareOrdinal(a, b))

                    Expect.equal (List.map fst mine) (List.map fst tom) (sprintf "%A" opts)

                    for (f, a), (_, b) in List.zip mine tom do
                        Expect.isTrue ((a: byte array) = b) f

            testCase "device names and name clashes get portable names; everything loads back"
            <| fun () ->
                use t = new TempDir()
                let db = tricky ()
                let names = tmdlFiles SaveOptions.defaults db |> List.map fst
                Expect.contains names "tables/_CON.tmdl" ""
                Expect.contains names "tables/_nul.txt.tmdl" ""
                Expect.contains names "tables/_CON~2.tmdl" "the table really named _CON"
                // TOM itself puts a:b and a%3ab (one name on Windows and macOS) in one document.
                Expect.contains names "tables/a%3Ab.tmdl" ""

                for n in names do
                    for segment in n.Split '/' do
                        Expect.isTrue (SafeName.isPortable segment) n

                golden "tmdl-tricky-names.txt" (String.Join("\n", names) + "\n")
                save TmdlFolder SaveOptions.defaults false db (t.Combine "m") |> ok |> ignore
                let back = reload (t.Combine "m")

                Expect.equal
                    (back.Model.Tables |> Seq.map (fun x -> x.Name) |> List.ofSeq)
                    (db.Model.Tables |> Seq.map (fun x -> x.Name) |> List.ofSeq)
                    "every table, in model order"

            testCase "the sample model's TMDL (golden)"
            <| fun () ->
                let files = tmdlFiles SaveOptions.defaults (sampleDatabase ())
                golden "tmdl-sample.txt" (manifest [ for p, bytes in files -> p, Text.Encoding.UTF8.GetString bytes ])

            testCase "the sample model's .bim (golden)"
            <| fun () -> golden "bim-sample.txt" (bimText (sampleDatabase ()) + "\n")
        ]

        testList "PBIP" [
            testCase "new support files (golden)"
            <| fun () ->
                let id = Guid "00000000-0000-0000-0000-000000000001"

                let platform =
                    Pbip.platformText
                        {
                            DisplayName = "Ventes été"
                            Description = Some "Desktop's \"model\""
                        }
                        id
                        "\n"

                golden
                    "pbip-support-files.txt"
                    (manifest [ ".platform", platform; "definition.pbism", Pbip.pbismText "\n" ])

            testCase "a project folder gets a semantic model folder named after the item"
            <| fun () ->
                use t = new TempDir()
                write (t.Combine("proj", "Sample.pbip")) (pbipJson "Sample.Report")

                let saved =
                    save PbipFolder SaveOptions.defaults false (sampleDatabase ()) (t.Combine "proj")
                    |> ok

                Expect.equal saved.Path (t.Combine("proj", "Sample.SemanticModel")) ""

                Expect.stringContains
                    (File.ReadAllText(Path.Combine(saved.Path, ".platform")))
                    "\"displayName\": \"Sample\""
                    ""

            testCase "a destination that isn't a project folder is written as it is"
            <| fun () ->
                use t = new TempDir()

                let saved =
                    save PbipFolder SaveOptions.defaults false (sampleDatabase ()) (t.Combine "out")
                    |> ok

                Expect.equal saved.Path (t.Combine "out") ""
                Expect.stringContains (File.ReadAllText(t.Combine("out", ".platform"))) "\"displayName\": \"out\"" ""

            testCase "converting a PBIP carries its description to a new one"
            <| fun () ->
                use t = new TempDir()
                let sm = writePbipProject (t.Combine "src") "Desk"

                write
                    (Path.Combine(sm, ".platform"))
                    "{\n  \"metadata\": {\n    \"type\": \"SemanticModel\",\n    \"displayName\": \"Ventes été\",\n    \"description\": \"From Desktop\"\n  },\n  \"config\": {\n    \"version\": \"2.0\",\n    \"logicalId\": \"11111111-2222-3333-4444-555555555555\"\n  }\n}\n"

                let source, db = loadPath sm |> ok
                let saved = saveTo source false db (t.Combine "out.SemanticModel") |> ok
                let platform = File.ReadAllText(Path.Combine(saved.Path, ".platform"))
                Expect.stringContains platform "\"displayName\": \"out\"" "a folder named by the user names the item"
                Expect.stringContains platform "\"description\": \"From Desktop\"" ""
                Expect.isFalse (platform.Contains "11111111") "a new item gets a new logicalId"

                write (t.Combine("proj", "Other.pbip")) (pbipJson "Other.Report")

                let intoProject =
                    saveItem (sourceItem source) PbipFolder SaveOptions.defaults false db (t.Combine "proj")
                    |> ok

                Expect.equal intoProject.Path (t.Combine("proj", "Ventes été.SemanticModel")) ""

                Expect.stringContains
                    (File.ReadAllText(Path.Combine(intoProject.Path, ".platform")))
                    "\"displayName\": \"Ventes été\""
                    "non-ASCII written as is"

            testCase "saving back into a Desktop-authored PBIP changes nothing outside the model"
            <| fun () ->
                use t = new TempDir()
                let sm = writePbipProject (t.Path) "Desk"

                let platform =
                    "{\r\n  \"$schema\": \"x\",\r\n  \"metadata\": {\r\n    \"type\": \"SemanticModel\",\r\n    \"displayName\": \"Renamed In Desktop\",\r\n    \"description\": \"Keep me\"\r\n  },\r\n  \"config\": {\r\n    \"version\": \"2.0\",\r\n    \"logicalId\": \"ed766c11-9c05-4d47-aac7-2700a4fa5d29\"\r\n  }\r\n}"

                write (Path.Combine(sm, ".platform")) platform
                write (Path.Combine(sm, "diagramLayout.json")) "{}"
                write (Path.Combine(sm, ".pbi", "editorSettings.json")) "{ \"x\": 1 }"

                let outside () =
                    relativeFiles t.Path
                    |> List.filter (fun f -> not (f.StartsWith "Desk.SemanticModel/definition/"))
                    |> List.map (fun f -> f, File.ReadAllBytes(Path.Combine(t.Path, f)))

                let before = outside ()
                let source, db = loadPath sm |> ok
                db.Model.Tables.["Sales"].Measures.["Total"].Expression <- "42"
                saveBack source db |> ok |> ignore
                Expect.equal (outside ()) before "byte for byte"

            testCase "spaceparts (TESSERA_SAMPLE_MODEL): a save back leaves every file outside the model as it was"
            <| fun () ->
                match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
                | null
                | "" -> skiptest "TESSERA_SAMPLE_MODEL is not set"
                | path ->
                    use t = new TempDir()

                    let rec copy (from: string) (into: string) =
                        Directory.CreateDirectory into |> ignore

                        for f in Directory.GetFiles from do
                            File.Copy(f, Path.Combine(into, Path.GetFileName f))

                        for d in Directory.GetDirectories from do
                            copy d (Path.Combine(into, Path.GetFileName d))

                    copy path t.Path

                    let all () =
                        relativeFiles t.Path
                        |> List.map (fun f -> f, File.ReadAllBytes(Path.Combine(t.Path, f)))

                    let outside (files: (string * byte array) list) =
                        files |> List.filter (fun (f, _) -> not (f.Contains "/definition/"))

                    let before = all ()
                    let source, db = loadPath t.Path |> ok
                    saveBack source db |> ok |> ignore
                    let first = all ()
                    Expect.equal (first |> List.map fst) (before |> List.map fst) "the same files"
                    Expect.isTrue (outside first = outside before) "byte for byte outside definition/"
                    let source, db = loadPath t.Path |> ok
                    saveBack source db |> ok |> ignore
                    Expect.isTrue (all () = first) "a second save changes nothing at all"
        ]
    ]
