module Tessera.Tests.DiffTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Diff
open Tessera.Tests.Fixtures

let private changes (edit: Database -> unit) =
    let after = sampleDatabase ()
    edit after
    compare Options.none (sampleDatabase ()) after

let private brief (cs: Change list) =
    cs |> List.map (fun c -> c.Kind, c.Label)

[<Tests>]
let tests =
    testList "Diff" [
        testCase "identical models have no changes"
        <| fun () -> Expect.isEmpty (changes ignore) ""

        testCase "added and removed objects"
        <| fun () ->
            let cs =
                changes (fun db ->
                    let sales = db.Model.Tables.["Sales"]
                    sales.Measures.Add(new Measure(Name = "New", Expression = "1"))
                    db.Model.Roles.Remove(db.Model.Roles.["Readers"]) |> ignore)

            Expect.equal (brief cs) [ Added, "measure:Sales/New"; Removed, "role:Readers" ] ""

        testCase "modified properties, before and after"
        <| fun () ->
            let cs =
                changes (fun db ->
                    let m = db.Model.Tables.["Sales"].Measures.["Total"]
                    m.Expression <- "SUMX(Sales, Sales[Amount])"
                    m.Description <- "Total sales")

            match cs with
            | [ c ] ->
                Expect.equal c.Kind Modified ""
                Expect.equal c.Label "measure:Sales/Total" ""

                Expect.equal
                    (c.Properties |> List.map (fun p -> p.Path, p.Before, p.After))
                    [
                        "expression", Some "SUM(Sales[Amount])", Some "SUMX(Sales, Sales[Amount])"
                        "description", None, Some "Total sales"
                    ]
                    ""
            | other -> failtestf "%A" (brief other)

        testCase "a rename is detected through lineageTag"
        <| fun () ->
            let tag (db: Database) =
                db.Model.Tables.["Sales"].Measures.["Total"].LineageTag <- "tag-total"

            let before = sampleDatabase ()
            tag before
            let after = sampleDatabase ()
            tag after
            let m = after.Model.Tables.["Sales"].Measures.["Total"]
            m.Name <- "Total Sales"
            m.FormatString <- "0.00"

            match compare Options.none before after with
            | [ c ] ->
                Expect.equal c.Kind Renamed ""
                Expect.equal c.Label "measure:Sales/Total -> measure:Sales/Total Sales" ""
                Expect.equal (c.Properties |> List.map (fun p -> p.Path)) [ "formatString" ] "other changes ride along"
            | other -> failtestf "%A" (brief other)

        testCase "without a lineageTag a rename is a remove and an add"
        <| fun () ->
            let cs =
                changes (fun db -> db.Model.Tables.["Sales"].Measures.["Total"].Name <- "Total Sales")

            Expect.equal (cs |> List.map (fun c -> c.Kind) |> List.sort) [ Added; Removed ] ""

        testCase "duplicate lineageTags disable rename matching for that kind"
        <| fun () ->
            // TOM forbids duplicate tags within one collection, but not across tables.
            let tagBoth (db: Database) =
                db.Model.Tables.["Sales"].Measures.["Total"].LineageTag <- "same"

                db.Model.Tables.["Date"].Measures.Add(
                    new Measure(Name = "Other", Expression = "1", LineageTag = "same")
                )

            let before = sampleDatabase ()
            tagBoth before
            let after = sampleDatabase ()
            tagBoth after
            after.Model.Tables.["Sales"].Measures.["Total"].Name <- "Renamed"
            let kinds = compare Options.none before after |> List.map (fun c -> c.Kind)
            Expect.isFalse (List.contains Renamed kinds) ""

        testCase "relationships match by endpoints, not their (GUID) names"
        <| fun () ->
            let cs =
                changes (fun db -> db.Model.Relationships.["Sales-Date"].Name <- "3f2a-guid")

            Expect.isEmpty cs ""

        testCase "relationship changes are labelled by endpoints"
        <| fun () ->
            let cs = changes (fun db -> db.Model.Relationships.["Sales-Date"].IsActive <- false)

            match cs with
            | [ c ] ->
                Expect.equal c.Label "relationship:Sales[DateKey] -> Date[DateKey]" ""
                Expect.equal (c.Properties |> List.map (fun p -> p.Path)) [ "isActive" ] ""
            | other -> failtestf "%A" (brief other)

        testCase "annotation changes are keyed by annotation name"
        <| fun () ->
            let cs =
                changes (fun db ->
                    db.Model.Tables.["Sales"].Annotations.Add(new Annotation(Name = "Owner", Value = "BI")))

            Expect.equal
                (cs |> List.collect (fun c -> c.Properties |> List.map (fun p -> p.Path)))
                [ "annotations.Owner" ]
                ""

        testCase "compatibility level changes show on the model"
        <| fun () ->
            let cs = changes (fun db -> db.CompatibilityLevel <- 1703)

            match cs with
            | [ c ] ->
                Expect.equal c.Label "model" ""

                Expect.equal
                    (c.Properties |> List.map (fun p -> p.Path, p.After))
                    [ "compatibilityLevel", Some "1703" ]
                    ""
            | other -> failtestf "%A" (brief other)

        testCase "ignore globs"
        <| fun () ->
            let after = sampleDatabase ()
            let m = after.Model.Tables.["Sales"].Measures.["Total"]
            m.FormatString <- "0.00"
            m.Annotations.Add(new Annotation(Name = "PBI_FormatHint", Value = "{}"))

            let opts = {
                Ignore = [ "formatString"; "annotations.PBI_*" ]
            }

            Expect.isEmpty (compare opts (sampleDatabase ()) after) ""

        testCase "a table with its children removed"
        <| fun () ->
            let cs =
                changes (fun db ->
                    db.Model.Relationships.Clear()
                    db.Model.Tables.Remove(db.Model.Tables.["Date"]) |> ignore)

            let removed =
                cs |> List.filter (fun c -> c.Kind = Removed) |> List.map (fun c -> c.Label)

            Expect.contains removed "table:Date" ""
            Expect.contains removed "level:Date/Calendar/Year" ""
            Expect.contains removed "relationship:Sales[DateKey] -> Date[DateKey]" ""

        testList "LineDiff" [
            testCase "keeps common lines, marks the rest"
            <| fun () ->
                Expect.equal
                    (LineDiff.diff [| "a"; "b"; "c" |] [| "a"; "x"; "c"; "d" |])
                    [
                        LineDiff.Same "a"
                        LineDiff.Removed "b"
                        LineDiff.Added "x"
                        LineDiff.Same "c"
                        LineDiff.Added "d"
                    ]
                    ""

            testCase "empty sides"
            <| fun () ->
                Expect.equal (LineDiff.diff [||] [| "a" |]) [ LineDiff.Added "a" ] ""
                Expect.equal (LineDiff.diff [| "a" |] [||]) [ LineDiff.Removed "a" ] ""
        ]
    ]
