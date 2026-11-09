module Tessera.Tests.TranslationsTests

open Expecto
open FsCheck
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Translations
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private failsWith (expected: string) =
    function
    | Ok _ -> failtestf "expected an error containing %s" expected
    | Error e -> Expect.stringContains (TessError.message e) expected ""

/// The sample model with some translations, a display folder and a description.
let private model () =
    let db = sampleDatabase ()
    let m = db.Model
    let sales = m.Tables.["Sales"]
    sales.Measures.["Total"].DisplayFolder <- "KPIs"
    sales.Measures.["Total"].Description <- "All sales"
    let da = m.Cultures.["da-DK"]
    da.ObjectTranslations.SetTranslation(sales.Measures.["Total"], TranslatedProperty.Caption, "I alt")
    da.ObjectTranslations.SetTranslation(sales, TranslatedProperty.Caption, "Salg")
    db

let private exportAll db =
    export db {
        Cultures = []
        OnlyTranslated = false
    }
    |> ok

let private row (rows: Row list) (r: string) (p: TranslatedProperty) =
    rows |> List.tryFind (fun x -> ObjectRef.format x.Object = r && x.Property = p)

let private spec (text: string) = {
    Path = "t.csv"
    Text = text
    Format = None
    Cultures = []
    CreateCultures = false
}

/// Read from stdin: the format is detected from the content.
let private stdin (text: string) = { spec text with Path = "-" }

let private applied db (s: ImportSpec) =
    Mutation.apply db (fun db -> import db s) |> ok

let private csv (lines: string list) = String.concat "\n" lines + "\n"

let private header = "object,culture,property,source,translation"

let private caption (db: Database) (culture: string) (o: MetadataObject) =
    match db.Model.Cultures.[culture].ObjectTranslations.[o, TranslatedProperty.Caption] with
    | null -> None
    | t -> Some t.Value

[<Tests>]
let csvTests =
    testList "Translations.Csv" [
        testCase "quotes fields with commas, quotes, line breaks and edge spaces only"
        <| fun () ->
            Expect.equal
                (Csv.record [ "a"; "b,c"; "say \"hi\""; "x\ny"; " pad"; "" ])
                "a,\"b,c\",\"say \"\"hi\"\"\",\"x\ny\",\" pad\","
                ""

        testCase "parses quoted fields, keeping line breaks inside them, and numbers records by their first line"
        <| fun () ->
            let text = "h1,h2\r\n\"multi\r\nline\",\"a \"\"q\"\"\"\n\nplain,\"\"\nlast,x"

            Expect.equal
                (Csv.parse text)
                (Ok [
                    1, [ "h1"; "h2" ]
                    2, [ "multi\r\nline"; "a \"q\"" ]
                    5, [ "plain"; "" ]
                    6, [ "last"; "x" ]
                ])
                "blank line skipped, CRLF inside a field kept"

        testCase "a byte-order mark is skipped"
        <| fun () -> Expect.equal (Csv.parse "﻿object,x\n") (Ok [ 1, [ "object"; "x" ] ]) ""

        testCase "an unterminated quote is reported at the line it opens on"
        <| fun () ->
            Expect.equal
                (Csv.parse "a,b\nc,\"open\nstill open\n")
                (Error(2, "a quoted field is never closed (a '\"' is missing)"))
                ""

        testCase "text after a closing quote is an error"
        <| fun () ->
            match Csv.parse "a,\"b\"c\n" with
            | Error(1, message) -> Expect.stringContains message "unexpected 'c' after a closing quote" ""
            | other -> failtestf "%A" other

        testProperty "a record written and parsed back is the same"
        <| fun (fields: NonNull<string> list) ->
            let fields = fields |> List.map (fun f -> f.Get)
            // One empty field is a blank line, which the reader skips.
            let fields = "first" :: fields
            Csv.parse (Csv.record fields + "\n") = Ok [ 1, fields ]
    ]

[<Tests>]
let exportTests =
    testList "Translations.export" [
        testCase "one row per object and culture: captions always, descriptions and folders where there is one"
        <| fun () ->
            let rows = exportAll (model ())
            Expect.isTrue (rows |> List.forall (fun r -> r.Culture = "da-DK")) ""

            let total = row rows "measure:Sales/Total" TranslatedProperty.Caption
            Expect.equal (total |> Option.map (fun r -> r.Source, r.Translation)) (Some("Total", "I alt")) ""

            Expect.equal
                (row rows "measure:Sales/Total" TranslatedProperty.Description
                 |> Option.map (fun r -> r.Source, r.Translation))
                (Some("All sales", ""))
                "untranslated rows are listed for the translator"

            Expect.equal
                (row rows "measure:Sales/Total" TranslatedProperty.DisplayFolder
                 |> Option.map (fun r -> r.Source))
                (Some "KPIs")
                ""

            Expect.isNone (row rows "measure:Sales/Total (hidden)" TranslatedProperty.Description) "no description"
            Expect.isSome (row rows "model" TranslatedProperty.Caption) ""
            Expect.isSome (row rows "level:Date/Calendar/Year" TranslatedProperty.Caption) ""
            Expect.isSome (row rows "perspective:Sales View" TranslatedProperty.Caption) ""

        testCase "objects TOM can't translate are left out: calculation items, partitions, relationships"
        <| fun () ->
            let kinds = exportAll (model ()) |> List.map (fun r -> r.Object.Kind) |> set

            for k in
                [
                    ObjectKind.CalculationItem
                    ObjectKind.Partition
                    ObjectKind.Relationship
                    ObjectKind.Culture
                ] do
                Expect.isFalse (kinds.Contains k) (ObjectKind.token k)

        testCase "a role's description is its only translatable property"
        <| fun () ->
            let db = model ()
            db.Model.Roles.["Readers"].Description <- "Can read"
            let rows = exportAll db |> List.filter (fun r -> r.Object.Kind = ObjectKind.Role)
            Expect.equal (rows |> List.map (fun r -> r.Property)) [ TranslatedProperty.Description ] ""

        testCase "--only-translated keeps rows with a translation; --culture picks cultures, case-insensitively"
        <| fun () ->
            let db = model ()
            db.Model.Cultures.Add(new Culture(Name = "fr-FR"))

            let only = export db { Cultures = []; OnlyTranslated = true } |> ok

            Expect.equal
                (only |> List.map (fun r -> ObjectRef.format r.Object))
                [ "table:Sales"; "measure:Sales/Total" ]
                "in model order"

            let fr =
                export db {
                    Cultures = [ "FR-fr" ]
                    OnlyTranslated = false
                }
                |> ok

            Expect.isNonEmpty fr ""
            Expect.isTrue (fr |> List.forall (fun r -> r.Culture = "fr-FR")) ""

        testCase "rows are grouped by culture, in the model's culture order"
        <| fun () ->
            let db = model ()
            db.Model.Cultures.Add(new Culture(Name = "fr-FR"))

            Expect.equal (exportAll db |> List.map (fun r -> r.Culture) |> List.distinct) [ "da-DK"; "fr-FR" ] ""

        testCase "an unknown --culture is an error"
        <| fun () ->
            export (model ()) {
                Cultures = [ "de-DE" ]
                OnlyTranslated = false
            }
            |> failsWith "culture:de-DE"
    ]

[<Tests>]
let importTests =
    testList "Translations.import" [
        testCase "sets, updates and removes translations; rows it doesn't mention stay"
        <| fun () ->
            let db = model ()

            let a =
                applied
                    db
                    (spec (
                        csv [
                            header
                            "measure:Sales/Total,da-DK,Caption,Total,Samlet"
                            "table:Sales,da-DK,Caption,Sales,"
                            "column:Sales/Amount,da-DK,Caption,Amount,Beløb"
                            "column:Date/Year,da-DK,Caption,Year,"
                        ]
                    ))

            let m = db.Model
            Expect.equal (caption db "da-DK" m.Tables.["Sales"].Measures.["Total"]) (Some "Samlet") "updated"
            Expect.equal (caption db "da-DK" m.Tables.["Sales"]) None "removed"
            Expect.equal (caption db "da-DK" m.Tables.["Sales"].Columns.["Amount"]) (Some "Beløb") "added"
            Expect.contains a.Effect.Notes "Translations (da-DK): 2 set, 1 removed, 1 unchanged" ""

            Expect.equal
                (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref, c.ChangeKind))
                [
                    "table:Sales", ChangeSet.ChangeKind.Modified
                    "column:Sales/Amount", ChangeSet.ChangeKind.Modified
                    "measure:Sales/Total", ChangeSet.ChangeKind.Modified
                ]
                "a translation change shows on the object it translates, not the culture"

            let total =
                a.Changes |> List.find (fun c -> ObjectRef.format c.Ref = "measure:Sales/Total")

            Expect.equal
                total.Properties
                [
                    {
                        Path = "translations.da-DK.caption"
                        Before = Some "I alt"
                        After = Some "Samlet"
                    }
                ]
                ""

        testCase "export, then import the file unchanged: no changes (CSV and JSON)"
        <| fun () ->
            let db = model ()
            let m = db.Model
            let da = m.Cultures.["da-DK"]
            let sales = m.Tables.["Sales"]

            da.ObjectTranslations.SetTranslation(
                sales.Columns.["Amount"],
                TranslatedProperty.Caption,
                "Beløb, \"netto\""
            )

            da.ObjectTranslations.SetTranslation(
                sales,
                TranslatedProperty.Description,
                "  Faktatabel.\r\n\nAnden linje  "
            )

            da.ObjectTranslations.SetTranslation(
                sales.Columns.["Path/With\\Slashes"],
                TranslatedProperty.Caption,
                "Sti/Med\\"
            )

            da.ObjectTranslations.SetTranslation(sales.Measures.["Total"], TranslatedProperty.DisplayFolder, "Nøgletal")
            let rows = exportAll db

            for format, text in [ Format.Csv, Csv.write rows; Format.Json, Json.write rows ] do
                let a = applied db { spec text with Format = Some format }

                Expect.isEmpty a.Changes $"{format}: {a.Changes}"
                Expect.isEmpty a.Effect.Warnings $"{format}"

        testCase "an unknown object is an error naming the line it's on, counting multi-line fields"
        <| fun () ->
            let text =
                csv [
                    header
                    "table:Sales,da-DK,Description,,\"two"
                    "lines\""
                    "measure:Sales/Nope,da-DK,Caption,Nope,x"
                ]

            match import (model ()) (spec text) with
            | Error(InvalidTranslationFile("t.csv", Some 4, reason)) ->
                Expect.stringContains reason "No object matches 'measure:Sales/Nope'" ""
            | other -> failtestf "%A" other

        testCase "objects can be named in any form `get` accepts"
        <| fun () ->
            let db = model ()

            applied db (spec (csv [ header; "'Sales'[Amount],da-DK,Caption,,Beløb"; "SALES,DA-dk,caption,,Salg2" ]))
            |> ignore

            Expect.equal (caption db "da-DK" db.Model.Tables.["Sales"].Columns.["Amount"]) (Some "Beløb") ""
            Expect.equal (caption db "da-DK" db.Model.Tables.["Sales"]) (Some "Salg2") ""

        testCase "an unknown culture is refused, unless --create-cultures adds it"
        <| fun () ->
            let text = csv [ header; "table:Sales,fr-fr,Caption,Sales,Ventes" ]

            match import (model ()) (spec text) with
            | Error(InvalidTranslationFile(_, Some 2, reason)) ->
                Expect.stringContains reason "the model has no culture 'fr-fr' (pass --create-cultures" ""
            | other -> failtestf "%A" other

            let db = model ()

            let a = applied db { spec text with CreateCultures = true }

            Expect.isTrue (db.Model.Cultures.Contains "fr-FR") "canonical case"
            Expect.contains a.Effect.Notes "Added culture:fr-FR" ""
            Expect.equal (caption db "fr-FR" db.Model.Tables.["Sales"]) (Some "Ventes") ""

            Expect.isTrue
                (a.Changes
                 |> List.exists (fun c ->
                     ObjectRef.format c.Ref = "culture:fr-FR"
                     && c.ChangeKind = ChangeSet.ChangeKind.Created))
                ""

            import (model ()) {
                spec (csv [ header; "table:Sales,xx-NOPE,Caption,,x" ]) with
                    CreateCultures = true
            }
            |> failsWith "'xx-NOPE' is not a specific culture name"

        testCase "a property the object can't have translated is refused (TOM would drop it on save)"
        <| fun () ->
            import (model ()) (spec (csv [ header; "table:Sales,da-DK,DisplayFolder,,x" ]))
            |> failsWith "line 2: table:Sales has no translatable DisplayFolder (only Caption, Description)"

            import (model ()) (spec (csv [ header; "partition:Sales/Sales,da-DK,Caption,,x" ]))
            |> failsWith "partition:Sales/Sales can't be translated"

            import (model ()) (spec (csv [ header; "table:Sales,da-DK,Title,,x" ]))
            |> failsWith "unknown property 'Title'"

        testCase "two rows for the same property must agree"
        <| fun () ->
            let rows = [
                header
                "table:Sales,da-DK,Caption,,A"
                "table:Sales,da-DK,Caption,,A"
                "Sales,da-dk,caption,,B"
            ]

            import (model ()) (spec (csv rows))
            |> failsWith "line 4: a different translation of the same property as line 2"

            applied (model ()) (spec (csv (List.take 3 rows))) |> ignore

        testCase "a source text that changed since the export is a warning"
        <| fun () ->
            let a =
                applied (model ()) (spec (csv [ header; "measure:Sales/Total,da-DK,Description,Old text,Alt salg" ]))

            Expect.equal
                a.Effect.Warnings
                [
                    "the source text of 1 row no longer matches the model (line 2): it changed after the export, so check those translations"
                ]
                ""

        testCase "a blank source isn't checked"
        <| fun () ->
            let a =
                applied (model ()) (spec (csv [ header; "measure:Sales/Total,da-DK,Description,,Alt salg" ]))

            Expect.isEmpty a.Effect.Warnings ""

        testCase "the source column is optional"
        <| fun () ->
            let db = model ()

            applied db (spec "object,culture,property,translation\ntable:Date,da-DK,Caption,Dato\n")
            |> ignore

            Expect.equal (caption db "da-DK" db.Model.Tables.["Date"]) (Some "Dato") ""

        testCase "--culture imports only those cultures' rows"
        <| fun () ->
            let db = model ()

            let a =
                applied db {
                    spec (
                        csv [
                            header
                            "table:Date,da-DK,Caption,,Dato"
                            "table:Date,fr-FR,Caption,,Date (fr)"
                        ]
                    ) with
                        Cultures = [ "da-dk" ]
                }

            Expect.equal (caption db "da-DK" db.Model.Tables.["Date"]) (Some "Dato") ""
            Expect.contains a.Effect.Notes "Skipped 1 row of other cultures" "fr-FR isn't needed"

        testCase "malformed files: a missing column, a short row"
        <| fun () ->
            import (model ()) (spec "object,culture,translation\n")
            |> failsWith "line 1: the header has no 'property' column"

            import (model ()) (spec (csv [ header; "table:Sales,da-DK,Caption,x,y,z" ]))
            |> failsWith "line 2: 6 fields, but the header has 5"

            import (model ()) (spec "") |> failsWith "the file is empty"

        testCase "JSON: rows as objects, keys in any case, null for no translation; errors give the item"
        <| fun () ->
            let db = model ()

            let text =
                """[ { "Object": "table:Sales", "culture": "da-DK", "property": "Caption", "translation": null },
                     { "object": "table:Date", "culture": "da-DK", "property": "Caption", "translation": "Dato" } ]"""

            applied db (stdin text) |> ignore
            Expect.equal (caption db "da-DK" db.Model.Tables.["Sales"]) None "null removes"
            Expect.equal (caption db "da-DK" db.Model.Tables.["Date"]) (Some "Dato") "detected as JSON"

            import
                (model ())
                (stdin
                    """[ { "object": "table:Nope", "culture": "da-DK", "property": "Caption", "translation": "x" } ]""")
            |> failsWith "item 1: No object matches 'table:Nope'"

            import (model ()) (stdin """{ "object": "x" }""")
            |> failsWith "expected a JSON array"

            import (model ()) (stdin """[ { "object": "table:Sales" } ]""")
            |> failsWith "item 1: no 'culture'"
    ]

[<Tests>]
let diffTests =
    testList "Translations and perspectives in diffs" [
        testCase "a culture's entry no longer carries the translations; the translated objects do"
        <| fun () ->
            let db = model ()
            let entries = Inventory.objects db

            let props (r: string) =
                entries
                |> List.find (fun e -> ObjectRef.format e.Ref = r)
                |> Serialize.properties db

            Expect.equal (props "culture:da-DK") [ "name", "da-DK" ] ""
            Expect.contains (props "measure:Sales/Total") ("translations.da-DK.caption", "I alt") ""
            Expect.contains (props "table:Sales") ("translations.da-DK.caption", "Salg") ""

        testCase "a perspective's members are one multi-line property, in model order"
        <| fun () ->
            let db = model ()
            let m = db.Model
            let p = m.Perspectives.["Sales View"]
            let pt = new PerspectiveTable(Table = m.Tables.["Date"])
            pt.PerspectiveColumns.Add(new PerspectiveColumn(Column = m.Tables.["Date"].Columns.["Year"]))
            p.PerspectiveTables.Add pt
            let ps = new PerspectiveTable(Table = m.Tables.["Sales"])
            ps.PerspectiveMeasures.Add(new PerspectiveMeasure(Measure = m.Tables.["Sales"].Measures.["Total"]))
            p.PerspectiveTables.Add ps

            let e =
                Inventory.objects db |> List.find (fun e -> e.Ref.Kind = ObjectKind.Perspective)

            Expect.contains
                (Serialize.properties db e)
                ("members", "table:Sales\nmeasure:Sales/Total\ntable:Date\ncolumn:Date/Year")
                ""
    ]
