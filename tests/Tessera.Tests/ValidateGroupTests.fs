module Tessera.Tests.ValidateGroupTests

open System.IO
open System.Text.Json
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Validate
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness
open Tessera.Cli.Common

/// The sample model with measures whose DAX names missing objects.
let private modelWith (measures: (string * string * string) list) =
    let db = sampleDatabase ()

    for table, name, expression in measures do
        db.Model.Tables.[table].Measures.Add(new Measure(Name = name, Expression = expression))

    db

/// A table, Old, referenced three ways from three measures; a missing column twice; [Ghost] twice.
let private broken () =
    modelWith [
        "Sales", "A", "SUM('Old'[X])"
        "Sales", "B", "MAX(Old[Y]) + COUNTROWS('Old')"
        "Sales", "C", "Sales[Gone] + [Ghost]"
        "Date", "D", "SUM(sales[GONE]) + [Ghost]"
        "Date", "E", "'Nope'[Z]"
    ]

let private grouped (db: Database) =
    let g = Deps.build db
    group g (validateGraph g db)

let private describe (grouped: (Issue * Grouping) list) =
    grouped
    |> List.map (fun (i, g) ->
        let r = ObjectRef.format i.Ref

        match g with
        | Alone -> r, "alone", []
        | Root(cause, dependents) -> r, $"root {cause}", dependents |> List.map (fun d -> ObjectRef.format d.Ref)
        | Dependent(cause, root) -> r, $"dependent {cause}", [ ObjectRef.format root.Ref ])

let private writeBim (t: TempDir) (db: Database) =
    let path = t.Combine "model.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase db)
    path

let private lines (s: string) =
    s.Split('\n') |> Array.filter ((<>) "") |> List.ofArray

[<Tests>]
let tests =
    testList "Validate grouping" [
        testCase "issues naming one missing name group under the first; order is kept"
        <| fun () ->
            Expect.equal
                (describe (grouped (broken ())))
                [
                    "measure:Sales/A", "root 'Old'", [ "measure:Sales/B"; "measure:Sales/B" ]
                    "measure:Sales/B", "dependent 'Old'", [ "measure:Sales/A" ]
                    "measure:Sales/B", "dependent 'Old'", [ "measure:Sales/A" ]
                    "measure:Sales/C", "root 'Sales'[Gone]", [ "measure:Date/D" ]
                    "measure:Sales/C", "root [Ghost]", [ "measure:Date/D" ]
                    "measure:Date/D", "dependent 'Sales'[Gone]", [ "measure:Sales/C" ]
                    "measure:Date/D", "dependent [Ghost]", [ "measure:Sales/C" ]
                    "measure:Date/E", "alone", []
                ]
                "a missing table's members are one cause; names match ignoring case"

        testCase "other rules, and distinct missing names, stay alone"
        <| fun () ->
            let db =
                modelWith [
                    "Sales", "L1", "[L2]"
                    "Sales", "L2", "[L1]"
                    "Sales", "Bad", "\"abc"
                    "Sales", "X", "'One'[A]"
                    "Sales", "Y", "'Two'[A]"
                ]

            for _, g in grouped db do
                Expect.equal g Alone ""

        testCase "a table that exists groups by member, not by table"
        <| fun () ->
            let db = modelWith [ "Sales", "X", "Sales[Gone1]"; "Sales", "Y", "Sales[Gone2]" ]

            for _, g in grouped db do
                Expect.equal g Alone ""

        testCase "no issues, no groups"
        <| fun () -> Expect.isEmpty (grouped (sampleDatabase ())) ""

        testCase "CLI text: one line per root with a count; the summary still counts every issue"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; writeBim t (broken ()) ]
            Expect.equal r.Code ExitCode.findings r.Err
            let out = lines r.Out
            Expect.equal out.Length 5 r.Out
            Expect.stringContains out.[0] "measure:Sales/A" ""
            Expect.isTrue (out.[0].EndsWith "[unresolved-reference]  +2 more for 'Old'") out.[0]
            Expect.isTrue (out.[1].EndsWith "+1 more for 'Sales'[Gone]") out.[1]
            Expect.isTrue (out.[2].EndsWith "+1 more for [Ghost]") out.[2]
            Expect.isTrue (out.[3].EndsWith "[unresolved-reference]") "a lone issue has no count"

            Expect.equal out.[4] "6 errors, 2 warnings (4 grouped under their root cause; --no-group lists them)" ""

        testCase "CLI --no-group: the flat list"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; writeBim t (broken ()); "--no-group" ]
            Expect.equal r.Code ExitCode.findings r.Err
            let out = lines r.Out
            Expect.equal out.Length 9 r.Out
            Expect.equal out.[8] "6 errors, 2 warnings" ""
            Expect.isFalse (r.Out.Contains "more for") ""

        testCase "CLI --json: every finding stays; roots list dependents, dependents name their root"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; writeBim t (broken ()); "--json" ]
            Expect.equal r.Code ExitCode.findings r.Out
            use doc = json r
            let root = doc.RootElement
            Expect.equal (root.GetProperty("summary").GetProperty("total").GetInt32()) 8 ""

            let findings = [
                for f in root.GetProperty("findings").EnumerateArray() ->
                    let opt (key: string) =
                        match f.TryGetProperty key with
                        | true, v -> Some v
                        | _ -> None

                    f.GetProperty("objectPath").GetString(),
                    opt "dependents"
                    |> Option.map (fun d -> [ for x in d.EnumerateArray() -> x.GetString() ]),
                    opt "rootCause" |> Option.map (fun x -> x.GetString())
            ]

            Expect.equal
                findings
                [
                    "measure:Sales/A", Some [ "measure:Sales/B" ], None
                    "measure:Sales/B", None, Some "measure:Sales/A"
                    "measure:Sales/B", None, Some "measure:Sales/A"
                    "measure:Sales/C", Some [ "measure:Date/D" ], None
                    "measure:Sales/C", Some [ "measure:Date/D" ], None
                    "measure:Date/D", None, Some "measure:Sales/C"
                    "measure:Date/D", None, Some "measure:Sales/C"
                    "measure:Date/E", None, None
                ]
                "dependents are distinct object paths; keys are left out when there is no group"

        testCase "CLI --json --no-group: no grouping keys"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; writeBim t (broken ()); "--json"; "--no-group" ]
            Expect.equal r.Code ExitCode.findings r.Out
            Expect.isFalse (r.Out.Contains "\"dependents\"") ""
            Expect.isFalse (r.Out.Contains "\"rootCause\"") ""

        testCase "CLI --ci github: one annotation per root, with the count"
        <| fun () ->
            use t = new TempDir()
            let path = writeBim t (broken ())
            let grouped = tess [ "validate"; path; "--ci"; "github" ]
            Expect.equal (lines grouped.Err).Length 4 grouped.Err
            Expect.stringContains grouped.Err "(+2 more for 'Old')" ""
            let flat = tess [ "validate"; path; "--ci"; "github"; "--no-group" ]
            Expect.equal (lines flat.Err).Length 8 flat.Err

        testCase "CLI: grouping follows --min-severity and --rule"
        <| fun () ->
            use t = new TempDir()
            let r = tess [ "validate"; writeBim t (broken ()); "--min-severity"; "error" ]
            Expect.equal r.Code ExitCode.findings r.Err
            let out = lines r.Out
            Expect.equal out.Length 4 r.Out
            Expect.isFalse (r.Out.Contains "[Ghost]") ""
            Expect.equal out.[3] "6 errors (3 grouped under their root cause; --no-group lists them)" ""
    ]
