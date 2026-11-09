/// Report rewrites against a real PBIP project (TESSERA_SAMPLE_MODEL, e.g. spaceparts-analytics, whose
/// report is PBIR); skipped otherwise.
module Tessera.Tests.SampleReportTests

open System
open System.Text.Json
open Expecto
open Tessera

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

/// (key, value) of every string in a document, in order.
let private strings (json: string) =
    let rec go (key: string) (e: JsonElement) =
        seq {
            match e.ValueKind with
            | JsonValueKind.Object ->
                for p in e.EnumerateObject() do
                    yield! go p.Name p.Value
            | JsonValueKind.Array ->
                for x in e.EnumerateArray() do
                    yield! go key x
            | JsonValueKind.String -> yield key, e.GetString()
            | _ -> ()
        }

    use d = JsonDocument.Parse json
    go "" d.RootElement |> List.ofSeq

[<Tests>]
let tests =
    testList "Sample report (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            testCase "renaming each table and measure changes only Entity and Property names"
            <| fun () ->
                match ModelIO.loadPath path with
                | Error e -> failtestf "%s" (TessError.message e)
                | Ok(source, db) ->
                    let reports =
                        source.SemanticModelFolder
                        |> Option.map Report.reportsOf
                        |> Option.defaultValue []

                    if reports.IsEmpty then
                        skiptest "the sample has no PBIR report next to its model"

                    let changes = [
                        for t in db.Model.Tables do
                            Report.RenameTable(t.Name, t.Name + " X"), "Entity"

                            for m in t.Measures do
                                Report.RenameMeasure(t.Name, m.Name, m.Name + " X"), "Property"
                    ]

                    let mutable total = 0

                    for change, key in changes do
                        let edits, _ = Report.plan reports change

                        for e in edits do
                            let a = strings e.Before
                            let b = strings e.After
                            Expect.equal b.Length a.Length e.Display

                            let changed = List.zip a b |> List.filter (fun (x, y) -> x <> y)
                            Expect.equal changed.Length e.References e.Display

                            for (k, v), (k2, v2) in changed do
                                Expect.equal (k, k2) (key, key) $"{e.Display}: only {key} names change"
                                Expect.equal v2 (v + " X") e.Display

                            total <- total + e.References

                    Expect.isGreaterThan total 0 "the report names some of the model's fields"
    ]
