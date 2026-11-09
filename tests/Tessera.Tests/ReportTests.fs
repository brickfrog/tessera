module Tessera.Tests.ReportTests

open System.Text
open System.Text.Json
open Expecto
open Tessera
open Tessera.Report

/// A visual as Power BI writes it (shapes from the spaceparts-analytics report): a measure and a
/// column projected, a hierarchy level, a filter that names its fields through a From alias, a
/// report-level (extension) measure, and literal values and free text that look like names.
let visual =
    """{
  "$schema": "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/visualContainer/2.7.0/schema.json",
  "name": "v1",
  "visual": {
    "visualType": "tableEx",
    "query": {
      "queryState": {
        "Values": {
          "projections": [
            {
              "field": { "Measure": { "Expression": { "SourceRef": { "Entity": "Sales" } }, "Property": "Total" } },
              "queryRef": "Sales.Total",
              "nativeQueryRef": "Total"
            },
            {
              "field": { "Column": { "Expression": { "SourceRef": { "Entity": "Sales" } }, "Property": "Amount" } },
              "queryRef": "Sales.Amount",
              "nativeQueryRef": "Amount"
            },
            {
              "field": { "Column": { "Expression": { "SourceRef": { "Entity": "Date" } }, "Property": "Amount" } },
              "queryRef": "Date.Amount"
            },
            {
              "field": {
                "HierarchyLevel": {
                  "Expression": { "Hierarchy": { "Expression": { "SourceRef": { "Entity": "Date" } }, "Hierarchy": "Calendar" } },
                  "Level": "Year"
                }
              },
              "queryRef": "Date.Calendar.Year"
            },
            {
              "field": { "Measure": { "Expression": { "SourceRef": { "Schema": "extension", "Entity": "Sales" } }, "Property": "Report Total" } },
              "queryRef": "Sales.Report Total"
            }
          ]
        }
      }
    },
    "objects": {
      "title": [ { "properties": { "text": { "expr": { "Literal": { "Value": "'Total'" } } } } } ],
      "columnWidth": [ { "selector": { "metadata": "Sales.Total" } } ]
    }
  },
  "filterConfig": {
    "filters": [
      {
        "name": "f1",
        "field": { "Measure": { "Expression": { "SourceRef": { "Entity": "Sales" } }, "Property": "Total" } },
        "filter": {
          "Version": 2,
          "From": [ { "Name": "s", "Entity": "Sales", "Type": 0 } ],
          "Where": [
            {
              "Condition": {
                "Comparison": {
                  "ComparisonKind": 1,
                  "Left": { "Measure": { "Expression": { "SourceRef": { "Source": "s" } }, "Property": "Total" } },
                  "Right": { "Literal": { "Value": "0D" } }
                }
              }
            },
            {
              "Condition": {
                "In": {
                  "Expressions": [ { "Column": { "Expression": { "SourceRef": { "Source": "s" } }, "Property": "Amount" } } ],
                  "Values": [ [ { "Literal": { "Value": "'Amount'" } } ] ]
                }
              }
            }
          ]
        }
      }
    ]
  }
}"""

let private rw change json =
    let after, n, skipped = rewrite change json
    after, n, skipped

/// The lines that differ, as (before, after) with surrounding whitespace trimmed.
let private changedLines (before: string) (after: string) =
    let a = before.Split '\n'
    let b = after.Split '\n'
    Expect.equal b.Length a.Length "same number of lines"

    Array.zip a b
    |> Array.filter (fun (x, y) -> x <> y)
    |> Array.map (fun (x, y) -> x.Trim(), y.Trim())
    |> List.ofArray

/// Every string of a JSON document with its path, for comparing what changed structurally.
let private strings (json: string) =
    let rec go (path: string) (e: JsonElement) =
        seq {
            match e.ValueKind with
            | JsonValueKind.Object ->
                for p in e.EnumerateObject() do
                    yield! go $"{path}.{p.Name}" p.Value
            | JsonValueKind.Array ->
                let mutable i = 0

                for x in e.EnumerateArray() do
                    yield! go $"{path}[{i}]" x
                    i <- i + 1
            | JsonValueKind.String -> yield path, e.GetString()
            | _ -> ()
        }

    use d = JsonDocument.Parse json
    go "$" d.RootElement |> List.ofSeq

let private changedStrings before after =
    List.zip (strings before) (strings after)
    |> List.filter (fun ((_, x), (_, y)) -> x <> y)
    |> List.map (fun ((p, x), (_, y)) -> p, x, y)

[<Tests>]
let tests =
    testList "report references" [
        testCase "a measure: its Property wherever it names Sales' measure, nothing else"
        <| fun () ->
            let after, n, skipped = rw (RenameMeasure("Sales", "Total", "Total Sales")) visual
            Expect.equal (n, skipped) (3, 0) "projection, filter field and the aliased filter condition"

            Expect.equal
                (changedStrings visual after |> List.map (fun (_, x, y) -> x, y))
                (List.replicate 3 ("Total", "Total Sales"))
                "only those three strings"

            Expect.stringContains after "\"queryRef\": \"Sales.Total\"" "queryRef is a visual-local id: kept"
            Expect.stringContains after "\"nativeQueryRef\": \"Total\"" "so is nativeQueryRef"
            Expect.stringContains after "\"metadata\": \"Sales.Total\"" "and selectors"
            Expect.stringContains after "\"Value\": \"'Total'\"" "literal values are data"

        testCase "only the changed tokens differ: every other byte is kept"
        <| fun () ->
            let crlf = visual.Replace("\n", "\r\n")
            let after, _, _ = rw (RenameMeasure("Sales", "Total", "Total Sales")) crlf
            Expect.equal (after.Replace("\"Total Sales\"", "\"Total\"")) crlf "a pure token substitution"

        testCase "a column: in its own table only, also through a From alias"
        <| fun () ->
            let after, n, _ = rw (RenameColumn("Sales", "Amount", "Net")) visual
            Expect.equal n 2 ""

            Expect.equal
                (changedStrings visual after |> List.map (fun (p, _, _) -> p))
                [
                    "$.visual.query.queryState.Values.projections[1].field.Column.Property"
                    "$.filterConfig.filters[0].filter.Where[1].Condition.In.Expressions[0].Column.Property"
                ]
                "not Date[Amount], not the literal"

        testCase "names match case-insensitively, as in the model"
        <| fun () ->
            let _, n, _ = rw (RenameColumn("SALES", "amount", "Net")) visual
            Expect.equal n 2 ""

        testCase "a table: SourceRef and From entities, not report-level (extension) entities"
        <| fun () ->
            let after, n, _ = rw (RenameTable("Sales", "Fact Sales")) visual
            Expect.equal n 4 "two projections, the filter field, the From source"

            let extension =
                strings after
                |> List.find (fun (p, _) -> p.EndsWith "projections[4].field.Measure.Expression.SourceRef.Entity")

            Expect.equal (snd extension) "Sales" "the extension entity is the report's own"

        testCase "a case-only table rename"
        <| fun () ->
            let after, n, _ = rw (RenameTable("Sales", "SALES")) visual
            Expect.equal n 4 ""
            Expect.stringContains after "\"Entity\": \"SALES\"" ""

        testCase "hierarchies and levels"
        <| fun () ->
            let after, n, _ = rw (RenameHierarchy("Date", "Calendar", "Cal")) visual
            Expect.equal n 1 ""
            Expect.stringContains after "\"Hierarchy\": \"Cal\"" ""

            let after, n, _ =
                rw (RenameLevel("Date", "Calendar", "Year", "Calendar Year")) visual

            Expect.equal n 1 ""
            Expect.stringContains after "\"Level\": \"Calendar Year\"" ""
            let _, n, _ = rw (RenameLevel("Date", "Other", "Year", "Y")) visual
            Expect.equal n 0 "a level of another hierarchy"
            let _, n, _ = rw (RenameHierarchy("Sales", "Calendar", "Cal")) visual
            Expect.equal n 0 "a hierarchy of another table"

        testCase "a moved measure: its own SourceRef entity; one named through a shared alias is reported"
        <| fun () ->
            let after, n, skipped = rw (MoveMeasure("Total", "Sales", "Measures")) visual
            Expect.equal (n, skipped) (2, 1) ""

            Expect.equal
                (changedStrings visual after |> List.map (fun (p, _, _) -> p))
                [
                    "$.visual.query.queryState.Values.projections[0].field.Measure.Expression.SourceRef.Entity"
                    "$.filterConfig.filters[0].field.Measure.Expression.SourceRef.Entity"
                ]
                "the From alias also serves the Amount column: left alone"

        testCase "date variations name their column"
        <| fun () ->
            let json =
                """{ "field": { "HierarchyLevel": { "Expression": { "Hierarchy": { "Expression": { "PropertyVariationSource": {
                     "Expression": { "SourceRef": { "Entity": "Sales" } }, "Name": "Variation", "Property": "Order Date" } },
                     "Hierarchy": "Date Hierarchy" } }, "Level": "Year" } } }"""

            let after, n, _ = rw (RenameColumn("Sales", "Order Date", "Ordered")) json
            Expect.equal n 1 ""
            Expect.stringContains after "\"Property\": \"Ordered\"" ""

        testCase "new names are JSON-escaped; escaped old names are found"
        <| fun () ->
            let json =
                "{ \"Measure\": { \"Expression\": { \"SourceRef\": { \"Entity\": \"S\\u00e6lg\" } }, "
                + "\"Property\": \"A \\\"q\\\"\" } }"

            let after, n, _ = rw (RenameMeasure("Sælg", "A \"q\"", "B \\ \"r\" æ")) json
            Expect.equal n 1 ""
            Expect.stringContains after "\"Property\": \"B \\\\ \\\"r\\\" æ\"" ""
            Expect.stringContains after "S\\u00e6lg" "untouched strings keep their escapes"

        testCase "a byte order mark is kept"
        <| fun () ->
            let bytes =
                Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (Encoding.UTF8.GetBytes visual)

            let before, after, n, _ = rewriteBytes (RenameMeasure("Sales", "Total", "T")) bytes
            Expect.equal n 3 ""
            Expect.equal before visual "the text, without the mark"
            Expect.isFalse (after.Length > 0 && after.[0] = char 0xFEFF) ""

        testCase "invalid JSON is an error"
        <| fun () -> Expect.throws (fun () -> rewrite (RenameTable("Sales", "X")) "{ \"a\": " |> ignore) ""

        testCase "what renames and moves become"
        <| fun () ->
            let m t n =
                ObjectRef.inTable ObjectKind.Measure t n

            Expect.equal (Change.ofMove (m "Sales" "A") (m "Sales" "B")) (Some(RenameMeasure("Sales", "A", "B"))) ""
            Expect.equal (Change.ofMove (m "Sales" "A") (m "Other" "A")) (Some(MoveMeasure("A", "Sales", "Other"))) ""

            Expect.equal
                (Change.ofMove
                    {
                        Kind = ObjectKind.Level
                        Path = [ "Date"; "Cal"; "Y" ]
                    }
                    {
                        Kind = ObjectKind.Level
                        Path = [ "Date"; "Cal"; "Year" ]
                    })
                (Some(RenameLevel("Date", "Cal", "Y", "Year")))
                ""

            Expect.equal
                (Change.ofMove
                    {
                        Kind = ObjectKind.Role
                        Path = [ "A" ]
                    }
                    {
                        Kind = ObjectKind.Role
                        Path = [ "B" ]
                    })
                None
                "roles aren't in reports"
    ]
