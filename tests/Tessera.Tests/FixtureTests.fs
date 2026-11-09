module Tessera.Tests.FixtureTests

open System
open System.IO
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

type private Severity = Validate.Severity

let private ok (r: Result<'a, TessError>) =
    match r with
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

/// A model with every data type, a calculated column, a calculated table, a calculation group, and
/// relationships to a loaded table, a keyed table and the calculated table.
let model () =
    let db = new Database(Name = "Shop", ID = "Shop", CompatibilityLevel = 1702)
    let m = new Model(Name = "Model", Culture = "en-US")
    db.Model <- m

    let add (t: Table) (name: string) (dataType: DataType) =
        let c = new DataColumn(Name = name, DataType = dataType, SourceColumn = name)
        t.Columns.Add c
        c

    let partition (t: Table) =
        t.Partitions.Add(new Partition(Name = t.Name, Source = new MPartitionSource(Expression = "let x = 1 in x")))

    let product = new Table(Name = "Product")
    let productKey = add product "ProductKey" DataType.Int64
    add product "Name" DataType.String |> ignore
    add product "Price" DataType.Decimal |> ignore
    add product "Weight" DataType.Double |> ignore
    add product "Active" DataType.Boolean |> ignore
    add product "Launched" DataType.DateTime |> ignore

    product.Columns.Add(
        new CalculatedColumn(Name = "Margin", DataType = DataType.Double, Expression = "Product[Price] * 0.2")
    )

    partition product
    m.Tables.Add product

    let customer = new Table(Name = "Customer")
    let code = add customer "Code" DataType.String
    code.IsKey <- true
    add customer "Name" DataType.String |> ignore
    partition customer
    m.Tables.Add customer

    let sales = new Table(Name = "Sales")
    let orderId = add sales "OrderId" DataType.Int64
    orderId.IsUnique <- true
    let salesProduct = add sales "ProductKey" DataType.Int64
    let salesCustomer = add sales "CustomerCode" DataType.String
    add sales "Amount" DataType.Decimal |> ignore
    let salesWhen = add sales "When" DataType.DateTime
    partition sales
    m.Tables.Add sales

    let calendar = new Table(Name = "Calendar")

    let calendarDate =
        new CalculatedTableColumn(Name = "Date", DataType = DataType.DateTime, SourceColumn = "[Date]")

    calendar.Columns.Add calendarDate

    calendar.Partitions.Add(
        new Partition(
            Name = "Calendar",
            Source = new CalculatedPartitionSource(Expression = "CALENDAR(DATE(2020,1,1), DATE(2020,12,31))")
        )
    )

    m.Tables.Add calendar

    let group = new Table(Name = "Time", CalculationGroup = new CalculationGroup())
    group.Columns.Add(new DataColumn(Name = "Period", DataType = DataType.String, SourceColumn = "Name"))
    group.CalculationGroup.CalculationItems.Add(new CalculationItem(Name = "Current", Expression = "SELECTEDMEASURE()"))
    group.Partitions.Add(new Partition(Name = "Time", Source = new CalculationGroupSource()))
    m.Tables.Add group

    m.Relationships.Add(
        new SingleColumnRelationship(Name = "Sales-Product", FromColumn = salesProduct, ToColumn = productKey)
    )

    m.Relationships.Add(
        new SingleColumnRelationship(Name = "Sales-Customer", FromColumn = salesCustomer, ToColumn = code)
    )

    m.Relationships.Add(
        new SingleColumnRelationship(Name = "Sales-Calendar", FromColumn = salesWhen, ToColumn = calendarDate)
    )

    db

let private productCsv =
    "ProductKey,Name,Price,Weight,Active,Launched\n"
    + "1,Bike,499.95,12.5,true,2020-03-01\n"
    + "2,Helmet,49,0.35,false,2021-11-15T08:30:00\n"
    + "3,\"Lock, heavy\",19.9999,1e0,1,\n"

let private customerCsv = "Code,Name\nC1,Ann\nC2,Bob\n"

let private salesCsv =
    "OrderId,ProductKey,CustomerCode,Amount,When\n"
    + "100,1,C1,499.95,2020-05-01\n"
    + "101,2,c2,49,2020-05-02\n"
    + "102,1,C1,10,2020-05-02\n"

/// A fixture directory with these files (name, text).
let private fixture (files: (string * string) list) =
    let t = new TempDir()

    for name, text in files do
        File.WriteAllText(t.Combine name, text, UTF8Encoding false)

    t

let private good () =
    fixture [
        "Product.csv", productCsv
        "Customer.csv", customerCsv
        "Sales.csv", salesCsv
    ]

let private issuesOf (db: Database) (dir: string) =
    Fixture.loadAndCheck db dir |> ok |> snd

let private rulesOf (issues: FixtureIssue list) = issues |> List.map (fun i -> i.Rule)

let private column (loaded: Fixture.Loaded) (table: string) (name: string) =
    loaded.Store.TryColumn { Table = table; Column = name } |> Option.get

let private values (c: StoreColumn) = [ for row in 0 .. c.RowCount - 1 -> c.[row] ]

let private cur (d: decimal) =
    match DaxValue.currency d with
    | Ok v -> v
    | Error _ -> failtest "currency"

let private date (y: int) (m: int) (d: int) = DaxValue.ofDateTime (DateTime(y, m, d))

/// Read a Product table from CSV text.
let private readProduct (text: string) =
    let db = model ()
    use reader = new StringReader(text)

    Fixture.readCsvTable
        Collation.ordinalIgnoreCase
        Fixture.LoadOptions.defaults
        db.Model.Tables.["Product"]
        "Product.csv"
        reader

let private readProductJson (text: string) =
    let db = model ()

    Fixture.readJsonTable
        Collation.ordinalIgnoreCase
        Fixture.LoadOptions.defaults
        db.Model.Tables.["Product"]
        "Product.json"
        (Encoding.UTF8.GetBytes text)

let private col (columns: StoreColumn array) (name: string) =
    columns |> Array.find (fun c -> c.Name = name)

/// Every record of some CSV text, as (line, fields with their quoted flags).
let private records (text: string) =
    let csv = CsvReader(new StringReader(text))

    let rec loop acc =
        match csv.Read() with
        | Ok true ->
            let fields = [ for i in 0 .. csv.FieldCount - 1 -> csv.FieldString i, csv.IsQuoted i ]
            loop ((csv.RecordLine, fields) :: acc)
        | Ok false -> Ok(List.rev acc)
        | Error e -> Error e

    loop []

let private plain (r: Result<(int * (string * bool) list) list, int * string>) =
    match r with
    | Ok rows -> rows |> List.map (fun (_, fields) -> fields |> List.map fst)
    | Error(line, reason) -> failtestf "line %d: %s" line reason

// ---- Random models, for the generator's property ----------------------------------------------

type RandomModel = {
    Describe: string
    Build: unit -> Database
}

let private genModel: Gen<RandomModel> =
    let types = [
        DataType.Int64
        DataType.String
        DataType.Decimal
        DataType.Double
        DataType.DateTime
        DataType.Boolean
    ]

    gen {
        let! tableCount = Gen.choose (1, 4)

        let! tables =
            Gen.listOfLength
                tableCount
                (gen {
                    let! columnCount = Gen.choose (1, 4)

                    return!
                        Gen.listOfLength
                            columnCount
                            (gen {
                                let! t = Gen.elements types
                                let! flag = Gen.frequency [ 6, Gen.constant 0; 1, Gen.constant 1; 1, Gen.constant 2 ]
                                return t, flag
                            })
                })

        let! relationshipCount = Gen.choose (0, 5)

        let! relationships =
            Gen.listOfLength
                relationshipCount
                (gen {
                    let! ft = Gen.choose (0, tableCount - 1)
                    let! tt = Gen.choose (0, tableCount - 1)
                    let! fc = Gen.choose (0, 3)
                    let! tc = Gen.choose (0, 3)

                    let! kind =
                        Gen.frequency [ 6, Gen.constant 0; 1, Gen.constant 1; 1, Gen.constant 2; 1, Gen.constant 3 ]

                    return ft, fc, tt, tc, kind
                })

        let build () =
            let db = new Database(Name = "R", ID = "R", CompatibilityLevel = 1702)
            db.Model <- new Model(Name = "Model")

            let built =
                tables
                |> List.mapi (fun ti columns ->
                    let t = new Table(Name = $"T{ti}")

                    let cs =
                        columns
                        |> List.mapi (fun ci (dt, flag) ->
                            let c = new DataColumn(Name = $"C{ci}", DataType = dt, SourceColumn = $"C{ci}")
                            // At most one key per table.
                            if flag = 1 && ci = 0 then
                                c.IsKey <- true

                            if flag = 2 then
                                c.IsUnique <- true

                            t.Columns.Add c
                            c)

                    db.Model.Tables.Add t
                    cs)

            relationships
            |> List.iteri (fun i (ft, fc, tt, tc, kind) ->
                if ft <> tt then
                    let from = built.[ft].[fc % built.[ft].Length]
                    let target = built.[tt].[tc % built.[tt].Length]

                    let r =
                        new SingleColumnRelationship(Name = $"R{i}", FromColumn = from, ToColumn = target)

                    match kind with
                    | 1 ->
                        r.FromCardinality <- RelationshipEndCardinality.One
                        r.ToCardinality <- RelationshipEndCardinality.One
                    | 2 ->
                        r.FromCardinality <- RelationshipEndCardinality.Many
                        r.ToCardinality <- RelationshipEndCardinality.Many
                    | 3 ->
                        r.FromCardinality <- RelationshipEndCardinality.One
                        r.ToCardinality <- RelationshipEndCardinality.Many
                    | _ -> ()

                    db.Model.Relationships.Add r)

            db

        return {
            Describe = sprintf "%A / %A" tables relationships
            Build = build
        }
    }

type Generators =
    static member RandomModel() = Arb.fromGen genModel

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators> ]
        maxTest = 300
}

/// Generate, write, load and check: the issues of a freshly generated fixture.
let private generatedIssues (db: Database) (rows: int) (seed: int64) =
    use t = new TempDir()
    let dir = t.Combine "fx"
    FixtureInit.write db dir { Rows = rows; Seed = seed } false |> ok |> ignore
    issuesOf db dir

let private nameGen =
    Gen.frequency [
        3,
        Gen.elements [
            "Sales"
            "CON"
            "con"
            "COM1"
            "LPT9.old"
            "nul.txt"
            "a/b"
            "a\\b"
            "50% off"
            "What?"
            "x:y"
            "trailing."
            "trailing "
            " leading"
            "."
            ".."
            "<tag>"
            "pipe|star*"
            "\"quoted\""
            "tab\there"
            "Ünïcödé"
            "日本語"
            "é"
            "😀 table"
            "%41"
            "100%"
        ]
        2,
        ArbMap.defaults
        |> ArbMap.generate<NonEmptyString>
        |> Gen.map (fun s -> s.Get)
        // Lone surrogates aren't text; a model can't be saved with one in a name.
        |> Gen.filter (fun s ->
            try
                UTF8Encoding(false, true).GetBytes s |> ignore
                true
            with _ ->
                false)
    ]

[<Tests>]
let tests =
    testList "Fixture" [
        testList "file names" [
            testCase "ordinary names are themselves; what a file name can't hold is %XX"
            <| fun () ->
                Expect.equal (FixtureName.encode "Sales") "Sales" ""
                Expect.equal (FixtureName.encode "Invoiced Sales") "Invoiced Sales" ""
                Expect.equal (FixtureName.encode "a/b") "a%2Fb" ""
                Expect.equal (FixtureName.encode "a\\b:c") "a%5Cb%3Ac" ""
                Expect.equal (FixtureName.encode "50% off?") "50%25 off%3F" ""
                Expect.equal (FixtureName.encode "trailing.") "trailing%2E" ""
                Expect.equal (FixtureName.encode "trailing ") "trailing%20" ""
                Expect.equal (FixtureName.encode " leading.dot") " leading.dot" ""
                Expect.equal (FixtureName.encode "..") ".%2E" ""
                Expect.equal (FixtureName.encode "tab\t") "tab%09" ""
                Expect.equal (FixtureName.encode "Ünïcödé 日本語 😀") "Ünïcödé 日本語 😀" "Unicode is kept"

            testCase "Windows device names lose their first letter to an escape"
            <| fun () ->
                Expect.equal (FixtureName.encode "CON") "%43ON" ""
                Expect.equal (FixtureName.encode "nul") "%6Eul" ""
                Expect.equal (FixtureName.encode "COM1") "%43OM1" ""
                Expect.equal (FixtureName.encode "lpt9.old") "%6Cpt9.old" "a device name before a dot"
                Expect.equal (FixtureName.encode "CONSOLE") "CONSOLE" ""
                Expect.equal (FixtureName.encode "COM10") "COM10" ""

            testCase "decoding"
            <| fun () ->
                Expect.equal (FixtureName.decode "a%2Fb") (Some "a/b") ""
                Expect.equal (FixtureName.decode "a%2fb") (Some "a/b") "hex in either case"
                Expect.equal (FixtureName.decode "%E6%97%A5") (Some "日") "UTF-8 bytes"
                Expect.equal (FixtureName.decode "Sales") (Some "Sales") ""
                Expect.equal (FixtureName.decode "100%") None "% must be followed by two hex digits"
                Expect.equal (FixtureName.decode "%4") None ""
                Expect.equal (FixtureName.decode "%ZZ") None ""
                Expect.equal (FixtureName.decode "%FF") None "not UTF-8"
                Expect.equal (FixtureName.decode "%2") None ""

            testCase "files match tables ignoring case and Unicode normalization"
            <| fun () ->
                Expect.isTrue (FixtureName.matches "Sales" "sales") ""
                Expect.isTrue (FixtureName.matches "Sales" "SALES") ""
                Expect.isTrue (FixtureName.matches "CON" "%43on") ""
                Expect.isTrue (FixtureName.matches "Café" "Café") "NFD, as macOS may return it"
                Expect.isTrue (FixtureName.matches "Café" "Café") ""
                Expect.isFalse (FixtureName.matches "Sales" "Sale") ""
                Expect.isFalse (FixtureName.matches "Sales" "Sales%") ""

            testPropertyWithConfig
                { config with maxTest = 1000 }
                "decode (encode name) = name, and the encoding is a legal file name everywhere"
            <| fun () ->
                Prop.forAll (Arb.fromGen nameGen) (fun name ->
                    let encoded = FixtureName.encode name
                    let device = FixtureName.encode (encoded.Split('.').[0]) <> encoded.Split('.').[0]

                    FixtureName.decode encoded = Some(name.Normalize NormalizationForm.FormC)
                    && FixtureName.matches name encoded
                    && encoded.IndexOfAny([| '<'; '>'; ':'; '"'; '/'; '\\'; '|'; '?'; '*' |]) < 0
                    && encoded |> Seq.forall (fun c -> c >= ' ' && c <> '\u007F')
                    && not (encoded.EndsWith '.')
                    && not (encoded.EndsWith ' ')
                    && encoded <> ""
                    // Encoding again changes only the % signs: no device name is left to escape.
                    && not (device && not (encoded.Contains '%')))

            testCase "assigning files to tables"
            <| fun () ->
                let a =
                    Fixture.assign [ "Sales"; "Date"; "a/b"; "Empty" ] [
                        "sales.csv"
                        "Sales.CSV"
                        "Date.json"
                        "a%2Fb.csv"
                        "Nope.csv"
                        "README.md"
                        "bad%.csv"
                        "Sales.csv.bak"
                    ]

                Expect.equal
                    a.Tables
                    [
                        "Sales", [ "Sales.CSV"; "sales.csv" ]
                        "Date", [ "Date.json" ]
                        "a/b", [ "a%2Fb.csv" ]
                        "Empty", []
                    ]
                    "both spellings of Sales, as on a case-sensitive file system"

                Expect.equal a.Unknown [ "Nope.csv"; "bad%.csv" ] "other extensions are not data files"
        ]

        testList "CSV reader" [
            testCase "fields, quotes, doubled quotes, commas and line breaks inside quotes"
            <| fun () ->
                let text =
                    "a,b,c\n1,\"x, y\",\"say \"\"hi\"\"\"\n\"two\nlines\",,\"\"\nlast,row,here"

                Expect.equal
                    (records text)
                    (Ok [
                        1, [ "a", false; "b", false; "c", false ]
                        2, [ "1", false; "x, y", true; "say \"hi\"", true ]
                        3, [ "two\nlines", true; "", false; "", true ]
                        5, [ "last", false; "row", false; "here", false ]
                    ])
                    "a record's line is where it starts; the quoted flag tells \"\" from nothing"

            testCase "LF, CRLF and lone CR end records; the last needs no line break"
            <| fun () ->
                let expected = [ [ "a"; "b" ]; [ "1"; "2" ]; [ "3"; "4" ] ]
                Expect.equal (plain (records "a,b\n1,2\n3,4\n")) expected "LF"
                Expect.equal (plain (records "a,b\r\n1,2\r\n3,4\r\n")) expected "CRLF"
                Expect.equal (plain (records "a,b\r1,2\r3,4")) expected "CR, no final break"
                Expect.equal (plain (records "a,b\r\n1,2\n3,4\r")) expected "mixed"

            testCase "line numbers count CRLF once, inside quotes too"
            <| fun () ->
                let lines = records "h\r\n\"a\r\nb\rc\nd\"\r\nz\r\n" |> Result.map (List.map fst)
                Expect.equal lines (Ok [ 1; 2; 6 ]) ""

                Expect.equal
                    (records "h\r\n\"a\r\nb\"" |> plain)
                    [ [ "h" ]; [ "a\r\nb" ] ]
                    "the break is kept as written"

            testCase "a byte-order mark is skipped; nothing is trimmed"
            <| fun () -> Expect.equal (plain (records "﻿a,b\n 1 , 2\t\n")) [ [ "a"; "b" ]; [ " 1 "; " 2\t" ] ] ""

            testCase "empty input, empty lines, trailing commas"
            <| fun () ->
                Expect.equal (records "") (Ok []) ""
                Expect.equal (records "﻿") (Ok []) ""

                Expect.equal
                    (plain (records "a\n\nb\n"))
                    [ [ "a" ]; [ "" ]; [ "b" ] ]
                    "an empty line is one empty field"

                Expect.equal (plain (records "a,\n,")) [ [ "a"; "" ]; [ ""; "" ] ] ""
                Expect.equal (plain (records "\n")) [ [ "" ] ] ""

            testCase "a quote inside an unquoted field is an ordinary character"
            <| fun () -> Expect.equal (plain (records "5\" nail,x")) [ [ "5\" nail"; "x" ] ] ""

            testCase "an unclosed quote and text after a closing quote stop the file, with the line"
            <| fun () ->
                match records "a,b\n1,\"open\n2,x\n" with
                | Error(line, reason) ->
                    Expect.equal line 2 "where the field starts"
                    Expect.stringContains reason "never closed" ""
                | Ok _ -> failtest "expected an error"

                match records "a\n\"x\"y\n" with
                | Error(line, reason) ->
                    Expect.equal line 2 ""
                    Expect.stringContains reason "after a closing quote" ""
                | Ok _ -> failtest "expected an error"

            testCase "records longer than the buffers"
            <| fun () ->
                let big = String('x', 200000)
                let text = $"a,b\n{big},\"{big}\"\n1,2\n"
                let rows = plain (records text)
                Expect.equal rows.Length 3 ""
                Expect.equal rows.[1].[0].Length 200000 ""
                Expect.equal rows.[1].[1] big ""
                Expect.equal rows.[2] [ "1"; "2" ] ""

            testProperty "what is written is what is read"
            <| fun (rows: (string * string) list) ->
                let clean (s: string) =
                    if isNull s then "" else s.Replace("﻿", "")

                let rows = rows |> List.map (fun (a, b) -> clean a, clean b)

                let text =
                    rows
                    |> List.map (fun (a, b) ->
                        Fixture.csvField (DaxValue.String a)
                        + ","
                        + Fixture.csvField (DaxValue.String b)
                        + "\n")
                    |> String.concat ""

                plain (records text) = (rows |> List.map (fun (a, b) -> [ a; b ]))
        ]

        testList "CSV tables" [
            testCase "values become the column's type"
            <| fun () ->
                let columns, issues = readProduct productCsv
                Expect.isEmpty issues ""

                Expect.equal
                    (columns |> Array.map (fun c -> c.Name))
                    [| "ProductKey"; "Name"; "Price"; "Weight"; "Active"; "Launched" |]
                    "data columns, in model order"

                Expect.equal
                    (values (col columns "ProductKey"))
                    [ DaxValue.Integer 1L; DaxValue.Integer 2L; DaxValue.Integer 3L ]
                    ""

                Expect.equal
                    (values (col columns "Name"))
                    [
                        DaxValue.String "Bike"
                        DaxValue.String "Helmet"
                        DaxValue.String "Lock, heavy"
                    ]
                    ""

                Expect.equal
                    (values (col columns "Price"))
                    [ cur 499.95m; cur 49m; cur 19.9999m ]
                    "TOM's Decimal is Currency"

                Expect.equal
                    (values (col columns "Weight"))
                    [ DaxValue.Decimal 12.5; DaxValue.Decimal 0.35; DaxValue.Decimal 1.0 ]
                    ""

                Expect.equal
                    (values (col columns "Active"))
                    [ DaxValue.Boolean true; DaxValue.Boolean false; DaxValue.Boolean true ]
                    ""

                Expect.equal
                    (values (col columns "Launched"))
                    [
                        date 2020 3 1
                        DaxValue.ofDateTime (DateTime(2021, 11, 15, 8, 30, 0))
                        DaxValue.Blank
                    ]
                    "an empty field is BLANK"

                Expect.equal (col columns "Price").DataType DaxType.Currency ""

            testCase "empty is BLANK; \"\" is the empty string in a text column and BLANK elsewhere"
            <| fun () ->
                let columns, issues =
                    readProduct "ProductKey,Name,Price\n,,\n\"\",\"\",\"\"\n  ,  , \n"

                Expect.equal (rulesOf issues |> List.distinct) [ "missing-column" ] ""
                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Blank; DaxValue.Blank; DaxValue.Blank ] ""
                Expect.equal (values (col columns "Price")) [ DaxValue.Blank; DaxValue.Blank; DaxValue.Blank ] ""

                Expect.equal
                    (values (col columns "Name"))
                    [ DaxValue.Blank; DaxValue.String ""; DaxValue.String "" ]
                    "spaces alone are text, stored without the trailing spaces"

            testCase "headers match column names ignoring case, in any order"
            <| fun () ->
                let columns, issues = readProduct "NAME,productkey\nBike,1\n"
                Expect.equal (issues |> List.filter (fun i -> i.Severity = Severity.Error)) [] ""
                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Integer 1L ] ""
                Expect.equal (values (col columns "Name")) [ DaxValue.String "Bike" ] ""

            testCase "a missing column is BLANK in every row, with a warning"
            <| fun () ->
                let columns, issues = readProduct "ProductKey\n1\n2\n"
                Expect.equal (values (col columns "Weight")) [ DaxValue.Blank; DaxValue.Blank ] ""
                let missing = issues |> List.filter (fun i -> i.Rule = "missing-column")
                Expect.equal missing.Length 5 "one per column"
                Expect.isTrue (missing |> List.forall (fun i -> i.Severity = Severity.Warning)) ""
                Expect.equal missing.Head.Ref (ObjectRef.inTable ObjectKind.Column "Product" "Name") ""
                Expect.equal missing.Head.Field (Some "Name") ""

            testCase "wrong types: an error with the file, line and column, and BLANK in the store"
            <| fun () ->
                let text =
                    "ProductKey,Name,Price,Weight,Active,Launched\n"
                    + "x1,ok,1.23456,NaN,yes,01/02/2020\n"
                    + "1.0,\"multi\nline\",1e3,Infinity,TRUE,2020-13-01\n"
                    + "9223372036854775808,ok,922337203685477.5808,1e999,2,2020-01-01T10:00:00+01:00\n"

                let columns, issues = readProduct text
                Expect.equal issues.Length 14 "every value but the names and TRUE"

                Expect.isTrue
                    (issues
                     |> List.forall (fun i -> i.Rule = "invalid-value" && i.Severity = Severity.Error))
                    ""

                Expect.isTrue (issues |> List.forall (fun i -> i.File = Some "Product.csv")) ""

                let first = issues.Head
                Expect.equal first.Line (Some 2) ""
                Expect.equal first.Field (Some "ProductKey") ""
                Expect.equal first.Ref (ObjectRef.inTable ObjectKind.Column "Product" "ProductKey") ""
                Expect.stringContains first.Message "'x1' is not a whole number" ""
                Expect.stringContains first.Message "INTEGER" ""

                let lineOf field n =
                    (issues |> List.filter (fun i -> i.Field = Some field) |> List.item n).Line

                Expect.equal
                    (lineOf "ProductKey" 2)
                    (Some 5)
                    "the third record starts on line 5, after the two-line field"

                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Blank; DaxValue.Blank; DaxValue.Blank ] ""

                Expect.equal
                    (values (col columns "Active"))
                    [ DaxValue.Blank; DaxValue.Boolean true; DaxValue.Blank ]
                    ""

                Expect.equal (col columns "Name").RowCount 3 ""

                let message field =
                    (issues |> List.find (fun i -> i.Field = Some field)).Message

                Expect.stringContains (message "Price") "at most 4 decimals" ""
                Expect.stringContains (message "Launched") "ISO 8601" ""
                Expect.stringContains (message "Weight") "'NaN' is not a number" ""

            testCase "invalid values are capped per column, with a count of the rest"
            <| fun () ->
                let rows = String.replicate 25 "x,y\n"
                let _, issues = readProduct ("ProductKey,Weight\n" + rows)
                let invalid = issues |> List.filter (fun i -> i.Rule = "invalid-value")
                Expect.equal invalid.Length 22 "10 + 1 per column"
                let rest = invalid |> List.filter (fun i -> i.Line.IsNone)
                Expect.equal rest.Length 2 ""
                Expect.stringContains rest.Head.Message "15 more invalid values in column 'ProductKey' (25 in all)" ""

            testCase "unknown, duplicate and calculated columns in the header"
            <| fun () ->
                let columns, issues =
                    readProduct "ProductKey,Colour,productkey,Margin,Name\n1,red,2,0.5,Bike\n"

                Expect.equal
                    (issues |> List.map (fun i -> i.Rule, i.Severity, i.Field, i.Line))
                    [
                        "unknown-column", Severity.Error, Some "Colour", Some 1
                        "duplicate-column", Severity.Error, Some "productkey", Some 1
                        "ignored-column", Severity.Warning, Some "Margin", Some 1
                        "missing-column", Severity.Warning, Some "Price", None
                        "missing-column", Severity.Warning, Some "Weight", None
                        "missing-column", Severity.Warning, Some "Active", None
                        "missing-column", Severity.Warning, Some "Launched", None
                    ]
                    ""

                Expect.stringContains issues.Head.Message "no column 'Colour'" ""
                Expect.stringContains issues.Head.Message "ProductKey, Name, Price" "lists what it has"
                Expect.stringContains issues.[2].Message "calculated column" ""

                Expect.equal
                    (values (col columns "ProductKey"))
                    [ DaxValue.Integer 1L ]
                    "the first of a repeated column"

                Expect.equal (values (col columns "Name")) [ DaxValue.String "Bike" ] ""
                Expect.equal columns.Length 6 "calculated columns are not in the store"

            testCase "rows with the wrong number of fields are errors and are left out"
            <| fun () ->
                let columns, issues =
                    readProduct "ProductKey,Name\n1,Bike\n2\n3,Lock,extra\n\n4,Bell\n"

                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Integer 1L; DaxValue.Integer 4L ] ""
                let bad = issues |> List.filter (fun i -> i.Rule = "malformed-row")
                Expect.equal (bad |> List.map (fun i -> i.Line)) [ Some 3; Some 4 ] "the empty line is skipped"
                Expect.stringContains bad.Head.Message "the row has 1 fields, the header has 2" ""

            testCase "in a one-column file an empty line is a BLANK row"
            <| fun () ->
                let columns, _ = readProduct "Name\nBike\n\nLock\n"

                Expect.equal
                    (values (col columns "Name"))
                    [ DaxValue.String "Bike"; DaxValue.Blank; DaxValue.String "Lock" ]
                    ""

            testCase "an empty file, a header only, and a semicolon-separated file"
            <| fun () ->
                let _, empty = readProduct ""
                Expect.equal (rulesOf empty) [ "malformed-file" ] ""
                Expect.stringContains empty.Head.Message "first line must name the columns" ""

                let columns, header = readProduct "ProductKey,Name,Price,Weight,Active,Launched\n"
                Expect.isEmpty header "no rows, so nothing is missing"
                Expect.equal columns.[0].RowCount 0 ""

                let _, semicolons = readProduct "ProductKey;Name\n1;Bike\n"
                Expect.contains (rulesOf semicolons) "malformed-file" ""
                Expect.stringContains semicolons.Head.Message "separated by commas" ""

            testCase "a syntax error keeps the rows before it"
            <| fun () ->
                let columns, issues = readProduct "ProductKey,Name\n1,Bike\n2,\"open\n"
                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Integer 1L ] ""
                let e = issues |> List.find (fun i -> i.Rule = "malformed-file")
                Expect.equal (e.Line, e.Severity) (Some 3, Severity.Error) ""

            testCase "text keeps its case and inner spaces; trailing spaces go; values group case-insensitively"
            <| fun () ->
                let columns, _ = readProduct "Name\nBike \n\" bike\"\nBIKE\nbike\n"
                let name = col columns "Name"

                Expect.equal
                    (values name)
                    [
                        DaxValue.String "Bike"
                        DaxValue.String " bike"
                        DaxValue.String "Bike"
                        DaxValue.String "Bike"
                    ]
                    ""

                Expect.equal name.DistinctCount 2 ""

            testCase "numbers and dates: the forms accepted"
            <| fun () ->
                let columns, issues =
                    readProduct (
                        "ProductKey,Price,Weight,Active,Launched\n"
                        + "+7,-0.5,-1.5E-3,False,2020-01-02 03:04:05\n"
                        + " 8 ,1000000,.5,0,2020-01-02T03:04:05.250Z\n"
                        + "-9,0.0001,5.,true,06:00\n"
                    )

                Expect.equal (issues |> List.filter (fun i -> i.Rule = "invalid-value")) [] ""

                Expect.equal
                    (values (col columns "ProductKey"))
                    [ DaxValue.Integer 7L; DaxValue.Integer 8L; DaxValue.Integer -9L ]
                    ""

                Expect.equal (values (col columns "Price")) [ cur -0.5m; cur 1000000m; DaxValue.Currency 1L ] ""

                Expect.equal
                    (values (col columns "Weight"))
                    [ DaxValue.Decimal -0.0015; DaxValue.Decimal 0.5; DaxValue.Decimal 5.0 ]
                    ""

                Expect.equal
                    (values (col columns "Launched"))
                    [
                        DaxValue.ofDateTime (DateTime(2020, 1, 2, 3, 4, 5))
                        DaxValue.ofDateTime (DateTime(2020, 1, 2, 3, 4, 5, 250))
                        DaxValue.DateTime 0.25
                    ]
                    ""

            testCase "invariant culture whatever the thread's"
            <| fun () ->
                let before = Globalization.CultureInfo.CurrentCulture

                try
                    Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo.GetCultureInfo "da-DK"
                    let columns, issues = readProduct "Weight,Price,Launched\n1.5,2.25,2020-03-01\n"
                    Expect.equal (issues |> List.filter (fun i -> i.Severity = Severity.Error)) [] ""
                    Expect.equal (values (col columns "Weight")) [ DaxValue.Decimal 1.5 ] ""
                    Expect.equal (values (col columns "Price")) [ cur 2.25m ] ""
                    let _, comma = readProduct "Weight\n\"1,5\"\n"
                    Expect.contains (rulesOf comma) "invalid-value" "a decimal comma is not a number"
                    Expect.equal (Fixture.formatValue (DaxValue.Decimal 1.5)) "1.5" ""
                    Expect.equal (Fixture.formatValue (cur 2.25m)) "2.25" ""
                finally
                    Globalization.CultureInfo.CurrentCulture <- before
        ]

        testList "JSON tables" [
            testCase "typed values, null and missing properties"
            <| fun () ->
                let json =
                    """[
  { "ProductKey": 1, "Name": "Bike", "Price": 499.95, "Weight": 12.5, "Active": true, "Launched": "2020-03-01" },
  { "ProductKey": 2, "Name": null, "Price": "49", "Weight": 1e-1, "Active": false },
  { "productkey": "3", "Name": "", "Launched": "2021-11-15T08:30:00" }
]"""

                let columns, issues = readProductJson json
                Expect.isEmpty issues ""

                Expect.equal
                    (values (col columns "ProductKey"))
                    [ DaxValue.Integer 1L; DaxValue.Integer 2L; DaxValue.Integer 3L ]
                    ""

                Expect.equal
                    (values (col columns "Name"))
                    [ DaxValue.String "Bike"; DaxValue.Blank; DaxValue.String "" ]
                    "null is BLANK, \"\" is empty text"

                Expect.equal
                    (values (col columns "Price"))
                    [ cur 499.95m; cur 49m; DaxValue.Blank ]
                    "a left-out property is BLANK"

                Expect.equal
                    (values (col columns "Weight"))
                    [ DaxValue.Decimal 12.5; DaxValue.Decimal 0.1; DaxValue.Blank ]
                    ""

                Expect.equal
                    (values (col columns "Active"))
                    [ DaxValue.Boolean true; DaxValue.Boolean false; DaxValue.Blank ]
                    ""

                Expect.equal
                    (values (col columns "Launched"))
                    [
                        date 2020 3 1
                        DaxValue.Blank
                        DaxValue.ofDateTime (DateTime(2021, 11, 15, 8, 30, 0))
                    ]
                    ""

            testCase "JSON types must be the column's: errors with the row's line"
            <| fun () ->
                let json =
                    "[\n"
                    + "  { \"ProductKey\": 1.5, \"Name\": 12, \"Price\": 1.23456, \"Active\": 1 },\n"
                    + "  { \"ProductKey\": true, \"Name\": \"ok\",\n    \"Launched\": 43831, \"Weight\": \"heavy\" },\n"
                    + "  { \"ProductKey\": [1], \"Name\": {\"a\": 1}, \"Weight\": 1e999 }\n"
                    + "]"

                let columns, issues = readProductJson json
                let invalid = issues |> List.filter (fun i -> i.Rule = "invalid-value")

                Expect.equal
                    (invalid |> List.map (fun i -> i.Line, i.Field))
                    [
                        Some 2, Some "ProductKey"
                        Some 2, Some "Name"
                        Some 2, Some "Price"
                        Some 2, Some "Active"
                        Some 3, Some "ProductKey"
                        Some 3, Some "Launched"
                        Some 3, Some "Weight"
                        Some 5, Some "ProductKey"
                        Some 5, Some "Name"
                        Some 5, Some "Weight"
                    ]
                    "a row's line is where its object starts"

                Expect.stringContains invalid.[0].Message "the number 1.5 is not a whole number" ""
                Expect.stringContains invalid.[1].Message "the number 12 is not text" "numbers are not text"
                Expect.stringContains invalid.[7].Message "an array is not" ""
                Expect.equal (values (col columns "Name")) [ DaxValue.Blank; DaxValue.String "ok"; DaxValue.Blank ] ""
                Expect.equal (col columns "ProductKey").RowCount 3 ""

            testCase "unknown and repeated properties; rows that aren't objects"
            <| fun () ->
                let json =
                    """[ { "Colour": "red", "ProductKey": 1, "productKey": 2 }, 5, { "Colour": 1, "ProductKey": 3 } ]"""

                let columns, issues = readProductJson json
                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Integer 1L; DaxValue.Integer 3L ] ""

                Expect.equal
                    (rulesOf issues |> List.filter (fun r -> r <> "missing-column"))
                    [ "unknown-column"; "duplicate-column"; "malformed-row" ]
                    "an unknown column is reported once"

            testCase "an empty array, a byte-order mark, comments and trailing commas"
            <| fun () ->
                let columns, issues = readProductJson "[]"
                Expect.isEmpty issues ""
                Expect.equal columns.[0].RowCount 0 ""

                let withBom =
                    Array.append
                        [| 0xEFuy; 0xBBuy; 0xBFuy |]
                        (Encoding.UTF8.GetBytes "[ // rows\n { \"ProductKey\": 1, }, ]")

                let db = model ()

                let columns, issues =
                    Fixture.readJsonTable
                        Collation.ordinalIgnoreCase
                        Fixture.LoadOptions.defaults
                        db.Model.Tables.["Product"]
                        "p.json"
                        withBom

                Expect.equal (issues |> List.filter (fun i -> i.Severity = Severity.Error)) [] ""
                Expect.equal (values (col columns "ProductKey")) [ DaxValue.Integer 1L ] ""

            testCase "not an array, and invalid JSON, with the line"
            <| fun () ->
                let _, notArray = readProductJson """{ "ProductKey": 1 }"""
                Expect.equal (rulesOf notArray) [ "malformed-file" ] ""
                Expect.stringContains notArray.Head.Message "must be a JSON array of row objects" ""

                let columns, broken =
                    readProductJson "[\n  { \"ProductKey\": 1 },\n  { \"ProductKey\": }\n]"

                let e = broken |> List.find (fun i -> i.Rule = "malformed-file")
                Expect.equal e.Line (Some 3) ""
                Expect.stringContains e.Message "not valid JSON" ""
                Expect.equal (col columns "ProductKey").RowCount (col columns "Name").RowCount "columns stay one length"

                let _, trailing = readProductJson "[] []"
                Expect.equal (rulesOf trailing) [ "malformed-file" ] ""
        ]

        testList "load" [
            testCase "a good fixture: every table, typed, no issues beyond the unchecked calculated table"
            <| fun () ->
                use t = good ()
                let db = model ()
                let loaded, issues = Fixture.loadAndCheck db t.Path |> ok
                Expect.equal (rulesOf issues) [ "unchecked-relationship" ] ""
                Expect.equal issues.Head.Severity Severity.Info ""
                Expect.stringContains issues.Head.Message "'Calendar'[Date] is computed by the engine" ""

                Expect.equal
                    (loaded.Tables
                     |> List.map (fun x -> x.Table, x.File |> Option.map Path.GetFileName, x.Rows))
                    [
                        "Product", Some "Product.csv", 3
                        "Customer", Some "Customer.csv", 2
                        "Sales", Some "Sales.csv", 3
                    ]
                    "model order"

                Expect.equal loaded.Computed [ "Calendar"; "Time" ] ""
                Expect.isNone (loaded.Store.TryTable "Calendar") "computed tables are not in the store"

                Expect.equal
                    (loaded.Store.Relationships |> List.map (fun r -> r.Name))
                    [ "Sales-Product"; "Sales-Customer"; "Sales-Calendar" ]
                    ""

                Expect.equal (column loaded "Sales" "Amount").DataType DaxType.Currency ""
                let m = loaded.Store.Match loaded.Store.Relationships.[1] |> Option.get
                Expect.equal (List.ofArray m.ToRowOfFromRow) [ 0; 1; 0 ] "'c2' matches 'C2'"

            testCase "tables with no file are empty, with one warning listing them"
            <| fun () ->
                use t = fixture [ "Product.csv", productCsv ]
                let db = model ()
                let loaded, issues = Fixture.loadAndCheck db t.Path |> ok
                let missing = issues |> List.find (fun i -> i.Rule = "missing-table")
                Expect.equal missing.Severity Severity.Warning ""
                Expect.equal missing.Ref ObjectRef.model ""
                Expect.stringContains missing.Message "2 tables have no file and no rows: Customer, Sales" ""
                let sales = loaded.Store.TryTable "Sales" |> Option.get
                Expect.equal (sales.RowCount, sales.Columns.Length) (0, 5) "empty, with its columns"
                Expect.isFalse (Fixture.hasErrors issues) ""

            testCase "an unknown table file is an error; other files are ignored"
            <| fun () ->
                use t =
                    fixture [
                        "Product.csv", productCsv
                        "Prodcut.csv", productCsv
                        "notes.txt", "hello"
                        "a%2Fb.json", "[]"
                        "bad%.csv", ""
                    ]

                Directory.CreateDirectory(t.Combine "Sales.csv.d") |> ignore
                let issues = issuesOf (model ()) t.Path
                let unknown = issues |> List.filter (fun i -> i.Rule = "unknown-table")

                Expect.equal
                    (unknown |> List.map (fun i -> i.File |> Option.map Path.GetFileName))
                    [ Some "Prodcut.csv"; Some "a%2Fb.json"; Some "bad%.csv" ]
                    ""

                Expect.stringContains unknown.[1].Message "no table 'a/b'" "the decoded name"
                Expect.isTrue (unknown |> List.forall (fun i -> i.Severity = Severity.Error)) ""

            testCase "file names match tables ignoring case, and escapes are decoded"
            <| fun () ->
                let db = model ()
                db.Model.Tables.["Customer"].RequestRename "Key/Account: CON?"

                use t =
                    fixture [
                        "PRODUCT.CSV", productCsv
                        "key%2Faccount%3A con%3f.Csv", customerCsv
                        "sales.csv", salesCsv
                    ]

                let loaded, issues = Fixture.loadAndCheck db t.Path |> ok
                Expect.isFalse (Fixture.hasErrors issues) (sprintf "%A" issues)
                Expect.equal (loaded.Tables |> List.map (fun x -> x.Rows)) [ 3; 2; 3 ] ""

            testCase "two files for one table: an error, and neither is loaded"
            <| fun () ->
                use t = fixture [ "Product.csv", productCsv; "Product.json", "[]" ]
                let loaded, issues = Fixture.loadAndCheck (model ()) t.Path |> ok
                let dup = issues |> List.find (fun i -> i.Rule = "duplicate-file")
                Expect.equal (dup.Severity, dup.Ref) (Severity.Error, ObjectRef.table "Product") ""
                Expect.stringContains dup.Message "Product.csv, Product.json" ""
                Expect.equal (loaded.Store.TryTable "Product" |> Option.get).RowCount 0 ""

                // Two spellings can only coexist on a case-sensitive file system.
                use t2 = fixture [ "Product.csv", productCsv ]
                File.WriteAllText(t2.Combine "product.csv", "ProductKey\n9\n")

                if Directory.GetFiles(t2.Path).Length = 2 then
                    Expect.contains (rulesOf (issuesOf (model ()) t2.Path)) "duplicate-file" ""

            testCase "a file for a calculated table or calculation group is ignored, with a warning"
            <| fun () ->
                use t = fixture [ "Calendar.csv", "Date\n2020-01-01\n"; "time.json", "[]" ]
                let issues = issuesOf (model ()) t.Path
                let computed = issues |> List.filter (fun i -> i.Rule = "calculated-table")

                Expect.equal
                    (computed |> List.map (fun i -> i.Ref, i.Severity))
                    [
                        ObjectRef.table "Calendar", Severity.Warning
                        ObjectRef.table "Time", Severity.Warning
                    ]
                    ""

                Expect.isFalse (Fixture.hasErrors issues) ""

            testCase "a UTF-8 byte-order mark, CRLF and non-ASCII text"
            <| fun () ->
                use t = new TempDir()
                let text = "Code,Name\r\nC1,\"Zoë\r\nÅkesson\"\r\nC2,日本\r\n"
                File.WriteAllText(t.Combine "Customer.csv", text, UTF8Encoding true)
                let loaded, issues = Fixture.loadAndCheck (model ()) t.Path |> ok
                Expect.isFalse (Fixture.hasErrors issues) (sprintf "%A" issues)

                Expect.equal
                    (values (column loaded "Customer" "Code"))
                    [ DaxValue.String "C1"; DaxValue.String "C2" ]
                    "the mark is not part of the header"

                Expect.equal
                    (values (column loaded "Customer" "Name"))
                    [ DaxValue.String "Zoë\r\nÅkesson"; DaxValue.String "日本" ]
                    ""

            testCase "a file that isn't UTF-8 is an error, not garbage"
            <| fun () ->
                use t = new TempDir()
                File.WriteAllBytes(t.Combine "Customer.csv", Encoding.Latin1.GetBytes "Code,Name\nC1,Zoë\n")
                let loaded, issues = Fixture.loadAndCheck (model ()) t.Path |> ok
                let e = issues |> List.find (fun i -> i.Rule = "malformed-file")
                Expect.stringContains e.Message "not UTF-8" ""
                Expect.equal (loaded.Store.TryTable "Customer" |> Option.get).RowCount 0 ""

            testCase "issues carry the file's full path"
            <| fun () ->
                use t = fixture [ "Product.csv", "ProductKey\nx\n" ]
                let e = issuesOf (model ()) t.Path |> List.find (fun i -> i.Rule = "invalid-value")
                Expect.equal e.File (Some(Path.GetFullPath(t.Combine "Product.csv"))) ""
                Expect.equal e.Line (Some 2) ""

            testCase "not a directory"
            <| fun () ->
                use t = new TempDir()
                File.WriteAllText(t.Combine "file.csv", "")
                let db = model ()

                match Fixture.load db (t.Combine "missing") with
                | Error(PathNotFound p) -> Expect.equal p (Path.GetFullPath(t.Combine "missing")) ""
                | other -> failtestf "expected PathNotFound, got %A" (other |> Result.map ignore)

                match Fixture.load db (t.Combine "file.csv") with
                | Error(InvalidFixture(_, reason) as e) ->
                    Expect.stringContains reason "a fixture is a directory" ""
                    Expect.equal (TessError.code e) "invalid-fixture" ""
                | other -> failtestf "expected InvalidFixture, got %A" (other |> Result.map ignore)

            testCase "an empty directory: every table empty"
            <| fun () ->
                use t = new TempDir()
                let loaded, issues = Fixture.loadAndCheck (Fixtures.sampleDatabase ()) t.Path |> ok
                Expect.equal (loaded.Tables |> List.map (fun x -> x.Table, x.Rows)) [ "Sales", 0; "Date", 0 ] ""
                Expect.equal (rulesOf issues) [ "missing-table" ] ""
                Expect.equal loaded.Computed [ "Time Intelligence" ] ""
        ]

        testList "checks" [
            testCase "duplicate and BLANK keys on the one side of a relationship are errors"
            <| fun () ->
                use t =
                    fixture [
                        "Product.csv", "ProductKey,Name\n1,a\n2,b\n1,c\n,d\n2,e\n3,f\n"
                        "Sales.csv", "OrderId,ProductKey\n1,1\n2,3\n"
                    ]

                let issues = issuesOf (model ()) t.Path
                let dup = issues |> List.find (fun i -> i.Rule = "duplicate-key")

                Expect.equal
                    (dup.Severity, dup.Ref)
                    (Severity.Error, ObjectRef.inTable ObjectKind.Column "Product" "ProductKey")
                    ""

                Expect.stringContains
                    dup.Message
                    "2 values repeat in 'Product'[ProductKey], the one side of relationship 'Sales-Product': '1', '2'"
                    ""

                Expect.equal (dup.File |> Option.map Path.GetFileName) (Some "Product.csv") ""
                let blank = issues |> List.find (fun i -> i.Rule = "blank-key")
                Expect.equal blank.Severity Severity.Error ""
                Expect.stringContains blank.Message "1 row is BLANK in 'Product'[ProductKey]" ""

            testCase "many-side keys with no match: a warning, and the orphan rows are in the store"
            <| fun () ->
                use t =
                    fixture [
                        "Product.csv", "ProductKey\n1\n2\n"
                        "Customer.csv", customerCsv
                        "Sales.csv", "OrderId,ProductKey,CustomerCode\n1,1,C1\n2,7,C1\n3,,C9\n4,7,c2\n5,8,C1\n"
                    ]

                let db = model ()
                let loaded, issues = Fixture.loadAndCheck db t.Path |> ok
                let unmatched = issues |> List.filter (fun i -> i.Rule = "unmatched-key")
                Expect.equal unmatched.Length 2 ""
                Expect.isTrue (unmatched |> List.forall (fun i -> i.Severity = Severity.Warning)) ""
                Expect.equal unmatched.[0].Ref (ObjectRef.inTable ObjectKind.Column "Sales" "ProductKey") ""

                Expect.stringContains
                    unmatched.[0].Message
                    "4 rows of 'Sales' (3 values of [ProductKey]) have no match in 'Product'[ProductKey] (relationship 'Sales-Product'): '7', '', '8'"
                    "a BLANK key is an orphan too"

                Expect.stringContains unmatched.[0].Message "blank row of 'Product'" ""
                Expect.stringContains unmatched.[1].Message "1 row of 'Sales' (1 value of [CustomerCode])" ""
                Expect.isFalse (Fixture.hasErrors issues) "DAX handles it; so must the evaluator"

                let orphans (name: string) =
                    let r = loaded.Store.Relationships |> List.find (fun r -> r.Name = name)
                    (loaded.Store.Match r |> Option.get).OrphanFromRows |> List.ofArray

                Expect.equal (orphans "Sales-Product") [ 1; 2; 3; 4 ] ""
                Expect.equal (orphans "Sales-Customer") [ 2 ] ""

            testCase "key and unique columns: repeats and BLANK keys"
            <| fun () ->
                use t =
                    fixture [
                        "Customer.csv", "Code,Name\nC1,a\nc1 ,b\n,c\n"
                        "Sales.csv", "OrderId,ProductKey\n1,\n1,\n,\n,\n"
                    ]

                let issues = issuesOf (model ()) t.Path
                // Customer[Code] is a key and the one side of a relationship: reported once, as the latter.
                let code = issues |> List.filter (fun i -> i.Field = Some "Code")
                Expect.equal (rulesOf code) [ "duplicate-key"; "blank-key" ] ""
                Expect.stringContains code.Head.Message "'C1'" "case and trailing spaces don't make a different key"
                let order = issues |> List.filter (fun i -> i.Field = Some "OrderId")
                Expect.equal (rulesOf order) [ "not-unique" ] "a unique column may be BLANK, even twice"

                Expect.stringContains
                    order.Head.Message
                    "1 value repeats in 'Sales'[OrderId], which is marked unique: '1'"
                    ""

            testCase "one-to-one: both sides must be unique, and are matched both ways"
            <| fun () ->
                let db = model ()
                let r = db.Model.Relationships.["Sales-Product"] :?> SingleColumnRelationship
                r.FromCardinality <- RelationshipEndCardinality.One
                r.ToCardinality <- RelationshipEndCardinality.One

                use t =
                    fixture [
                        "Product.csv", "ProductKey\n1\n2\n3\n"
                        "Sales.csv", "OrderId,ProductKey\n1,1\n2,1\n3,9\n"
                    ]

                let issues =
                    issuesOf db t.Path |> List.filter (fun i -> i.Message.Contains "Sales-Product")

                Expect.equal
                    (issues |> List.map (fun i -> i.Rule, ObjectRef.format i.Ref))
                    [
                        "unmatched-key", "column:Sales/ProductKey"
                        "duplicate-key", "column:Sales/ProductKey"
                        "unmatched-key", "column:Product/ProductKey"
                    ]
                    ""

            testCase "many-to-many: nothing to be unique, nothing unmatched"
            <| fun () ->
                let db = model ()
                let r = db.Model.Relationships.["Sales-Product"] :?> SingleColumnRelationship
                r.ToCardinality <- RelationshipEndCardinality.Many

                use t =
                    fixture [
                        "Product.csv", "ProductKey\n1\n1\n"
                        "Sales.csv", "OrderId,ProductKey\n1,5\n"
                    ]

                Expect.isEmpty (issuesOf db t.Path |> List.filter (fun i -> i.Message.Contains "Sales-Product")) ""

            testCase "every rule that can be reported is documented"
            <| fun () ->
                let known = Fixture.rules |> List.map fst
                Expect.equal (List.distinct known) known ""

                use t =
                    fixture [
                        "Product.csv", "ProductKey,Nope,Margin,ProductKey\n1,a,b,c\n1\n,,,\nx,,,\n"
                        "Customer.csv", "Code\nC1\nC1\n"
                        "Sales.csv", "OrderId,ProductKey\n1,5\n1,5\n"
                        "Sales.json", "[]"
                        "Other.csv", ""
                        "Calendar.csv", ""
                    ]

                let reported = issuesOf (model ()) t.Path |> rulesOf |> List.distinct

                for r in reported do
                    Expect.contains known r ""
        ]

        testList "writing" [
            testCase "values as fixture text"
            <| fun () ->
                Expect.equal (Fixture.formatValue DaxValue.Blank) "" ""
                Expect.equal (Fixture.formatValue (DaxValue.Integer -12L)) "-12" ""
                Expect.equal (Fixture.formatValue (DaxValue.Decimal 0.1)) "0.1" ""
                Expect.equal (Fixture.formatValue (DaxValue.Decimal 1e21)) "1E+21" ""
                Expect.equal (Fixture.formatValue (cur 1234.5m)) "1234.5" ""
                Expect.equal (Fixture.formatValue (DaxValue.Boolean true)) "true" ""
                Expect.equal (Fixture.formatValue (date 2024 1 31)) "2024-01-31" ""

                Expect.equal
                    (Fixture.formatValue (DaxValue.ofDateTime (DateTime(2024, 1, 31, 13, 5, 0))))
                    "2024-01-31T13:05:00"
                    ""

                Expect.equal
                    (Fixture.formatValue (DaxValue.ofDateTime (DateTime(2024, 1, 31, 13, 5, 0, 250))))
                    "2024-01-31T13:05:00.25"
                    ""

                Expect.equal (Fixture.csvField (DaxValue.String "")) "\"\"" "so it doesn't read back as BLANK"
                Expect.equal (Fixture.csvField (DaxValue.String "a,b")) "\"a,b\"" ""
                Expect.equal (Fixture.csvField (DaxValue.String " a")) "\" a\"" ""
                Expect.equal (Fixture.csvField (DaxValue.String "say \"hi\"")) "\"say \"\"hi\"\"\"" ""

            testPropertyWithConfig { config with maxTest = 200 } "a table written as CSV reads back the same"
            <| fun (rows: (int64 * NormalFloat * int64 * string * bool * int * bool) list) ->
                let db = model ()
                let table = db.Model.Tables.["Product"]
                let ci = Collation.ordinalIgnoreCase

                let source =
                    rows
                    |> List.map (fun (i, NormalFloat w, price, name, active, day, blank) ->
                        if blank then
                            List.replicate 6 DaxValue.Blank
                        else
                            [
                                DaxValue.Integer i
                                DaxValue.String(if isNull name then "" else name.Replace("﻿", ""))
                                DaxValue.Currency price
                                DaxValue.Decimal w
                                DaxValue.Boolean active
                                DaxValue.ofDateTime (
                                    DateTime(2000, 1, 1)
                                        .AddDays(float (day % 20000))
                                        .AddMilliseconds(float (abs day % 86400000))
                                )
                            ])

                let columns =
                    Fixture.dataColumns table
                    |> List.mapi (fun i (c, dt) ->
                        match StoreColumn.ofValues ci c.Name dt (source |> List.map (fun r -> r.[i])) with
                        | Ok c -> c
                        | Error e -> failtestf "%s" (DaxError.message e))

                let original =
                    match StoreTable.Create("Product", columns) with
                    | Ok t -> t
                    | Error e -> failtest e

                use writer = new StringWriter()
                Fixture.writeCsv writer original
                use reader = new StringReader(writer.ToString())

                let back, issues =
                    Fixture.readCsvTable ci Fixture.LoadOptions.defaults table "Product.csv" reader

                List.isEmpty issues
                && Array.forall2
                    (fun (a: StoreColumn) (b: StoreColumn) -> values a = values b && a.Values = b.Values)
                    original.Columns
                    back
        ]

        testList "init" [
            testCase "headers only by default: one CSV per table with data columns, model order, LF, no BOM"
            <| fun () ->
                use t = new TempDir()
                let db = model ()

                let written =
                    FixtureInit.write db (t.Combine "fx") FixtureInit.Options.defaults false |> ok

                Expect.equal
                    (written |> List.map (fun w -> Path.GetFileName w.Path, w.Rows, w.Columns))
                    [ "Product.csv", 0, 6; "Customer.csv", 0, 2; "Sales.csv", 0, 5 ]
                    ""

                Expect.equal
                    (File.ReadAllBytes(t.Combine("fx", "Product.csv")))
                    (Encoding.UTF8.GetBytes "ProductKey,Name,Price,Weight,Active,Launched\n")
                    ""

                let _, issues = Fixture.loadAndCheck db (t.Combine "fx") |> ok
                Expect.equal (rulesOf issues) [ "unchecked-relationship" ] "an empty fixture is a valid one"

            testCase "synthetic rows: deterministic for a seed, different for another"
            <| fun () ->
                let db = model ()

                let text (seed: int64) =
                    FixtureInit.generate db { Rows = 25; Seed = seed }
                    |> List.map (fun table ->
                        use w = new StringWriter()
                        Fixture.writeCsv w table
                        w.ToString())

                Expect.equal (text 1L) (text 1L) ""
                Expect.notEqual (text 1L) (text 2L) ""

                // Pinned: the same bytes on every machine and OS, and from one release to the next.
                let sales = (text 1L).[2].Split('\n')
                Expect.equal sales.[0] "OrderId,ProductKey,CustomerCode,Amount,When" ""
                Expect.equal sales.Length 27 "header, 25 rows, and the end of the last line"
                Expect.isTrue (sales.[1].StartsWith "1,") "OrderId is unique: 1, 2, 3..."
                Expect.equal (text 1L).[1].[0..37] "Code,Name\nCode 1,Name 7\nCode 2,Name 8\n" "pinned output"

            testCase "keys are unique, foreign keys match, nothing is BLANK"
            <| fun () ->
                let db = model ()
                let tables = FixtureInit.generate db { Rows = 200; Seed = 7L }
                let product = tables |> List.find (fun t -> t.Name = "Product")
                let sales = tables |> List.find (fun t -> t.Name = "Sales")
                let key = product.TryColumn "ProductKey" |> Option.get
                Expect.isTrue key.IsUnique ""
                Expect.isTrue (sales.TryColumn "OrderId" |> Option.get).IsUnique ""
                let fk = sales.TryColumn "ProductKey" |> Option.get
                Expect.isTrue (fk.Values |> Array.forall (fun v -> key.TryFindId v >= 0)) ""
                Expect.isTrue (fk.DistinctCount > 20) "spread over the products"

                for t in tables do
                    for c in t.Columns do
                        Expect.equal c.BlankId -1 $"{t.Name}[{c.Name}]"
                        Expect.equal c.RowCount 200 ""

            testCase "refuses to overwrite a table's file, of either format and any casing, without force"
            <| fun () ->
                use t = new TempDir()
                let db = model ()
                let dir = t.Combine "fx"
                Directory.CreateDirectory dir |> ignore
                File.WriteAllText(Path.Combine(dir, "SALES.json"), "[]")
                File.WriteAllText(Path.Combine(dir, "notes.txt"), "keep me")

                match FixtureInit.write db dir FixtureInit.Options.defaults false with
                | Error(DestinationNotEmpty p) -> Expect.equal (Path.GetFileName p) "SALES.json" ""
                | other -> failtestf "expected DestinationNotEmpty, got %A" other

                Expect.equal (Directory.GetFiles dir |> Array.length) 2 "nothing was written"
                let written = FixtureInit.write db dir { Rows = 3; Seed = 1L } true |> ok
                Expect.equal written.Length 3 ""

                Expect.isFalse
                    (File.Exists(Path.Combine(dir, "SALES.json"))
                     && File.Exists(Path.Combine(dir, "Sales.csv"))
                     && Directory.GetFiles(dir).Length > 4)
                    "the old file is replaced"

                Expect.equal (File.ReadAllText(Path.Combine(dir, "notes.txt"))) "keep me" "other files are left alone"
                Expect.isFalse (Fixture.hasErrors (issuesOf db dir)) "no duplicate-file left behind"

                match FixtureInit.write db (Path.Combine(dir, "notes.txt")) FixtureInit.Options.defaults true with
                | Error(InvalidFixture _) -> ()
                | other -> failtestf "expected InvalidFixture, got %A" other

            testCase "table names that aren't file names are escaped, and load back"
            <| fun () ->
                use t = new TempDir()
                let db = model ()
                db.Model.Tables.["Customer"].RequestRename "CON"
                db.Model.Tables.["Product"].RequestRename "Parts/Products: 50%?"
                let written = FixtureInit.write db t.Path { Rows = 5; Seed = 1L } false |> ok

                Expect.equal
                    (written |> List.map (fun w -> Path.GetFileName w.Path))
                    [ "Parts%2FProducts%3A 50%25%3F.csv"; "%43ON.csv"; "Sales.csv" ]
                    ""

                let loaded, issues = Fixture.loadAndCheck db t.Path |> ok
                Expect.equal (rulesOf issues) [ "unchecked-relationship" ] ""
                Expect.equal (loaded.Tables |> List.map (fun x -> x.Rows)) [ 5; 5; 5 ] ""

            testCase "a unique True/False column allows two rows; a one-to-one partner gets as many"
            <| fun () ->
                let db = model ()
                db.Model.Tables.["Product"].Columns.["Active"].IsUnique <- true
                let r = db.Model.Relationships.["Sales-Product"] :?> SingleColumnRelationship
                r.FromCardinality <- RelationshipEndCardinality.One
                r.ToCardinality <- RelationshipEndCardinality.One
                let tables = FixtureInit.generate db { Rows = 10; Seed = 1L }

                Expect.equal
                    (tables |> List.map (fun t -> t.Name, t.RowCount))
                    [ "Product", 2; "Customer", 10; "Sales", 2 ]
                    ""

                Expect.equal (generatedIssues db 10 1L |> List.filter (fun i -> i.Severity <> Severity.Info)) [] ""

            testPropertyWithConfig config "a generated fixture has no errors and no warnings: the fixture models"
            <| fun (NonNegativeInt rows) (seed: int64) ->
                [ model (); Fixtures.sampleDatabase () ]
                |> List.forall (fun db ->
                    generatedIssues db (rows % 60) seed
                    |> List.forall (fun i -> i.Severity = Severity.Info))

            testPropertyWithConfig config "a generated fixture has no errors: random models"
            <| fun (m: RandomModel) (NonNegativeInt rows) (seed: int64) ->
                let issues = generatedIssues (m.Build()) (rows % 40) seed
                let errors = issues |> List.filter (fun i -> i.Severity = Severity.Error)

                if errors.IsEmpty then
                    true
                else
                    failtestf "%s\n%A" m.Describe errors

            testPropertyWithConfig config "random many-to-one models: no unmatched keys either"
            <| fun (m: RandomModel) (NonNegativeInt rows) (seed: int64) ->
                let db = m.Build()

                // The guarantee holds where each column is the many side of at most one relationship
                // and the two ends have one type.
                let simple =
                    db.Model.Relationships
                    |> Seq.cast<SingleColumnRelationship>
                    |> Seq.forall (fun r ->
                        r.FromCardinality = RelationshipEndCardinality.Many
                        && r.ToCardinality = RelationshipEndCardinality.One
                        && r.FromColumn.DataType = r.ToColumn.DataType)
                    && (db.Model.Relationships
                        |> Seq.cast<SingleColumnRelationship>
                        |> Seq.countBy (fun r -> r.FromColumn.Table.Name, r.FromColumn.Name)
                        |> Seq.forall (fun (_, n) -> n = 1))

                not simple
                || generatedIssues db (rows % 40) seed
                   |> List.forall (fun i -> i.Severity = Severity.Info || i.Rule = "missing-table")
        ]
    ]
