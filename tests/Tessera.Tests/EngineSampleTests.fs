/// Optional check of the local DAX engine against a real model. Set TESSERA_SAMPLE_MODEL to any
/// path tessera accepts (e.g. a clone of TabularEditor/spaceparts-analytics); skipped otherwise.
module Tessera.Tests.EngineSampleTests

open System
open Expecto
open Tessera
open Tessera.Tests.Fixtures

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

[<Tests>]
let tests =
    testList "Engine on a sample model (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            // Coverage and the evaluator share their checks: a measure coverage clears must not be
            // refused when evaluated over synthetic data, and one that is refused must be listed.
            testCase "every measure that coverage clears evaluates"
            <| fun () ->
                use t = new TempDir()
                let _, db = ModelIO.loadPath path |> ok
                let fixture = t.Combine "fixture"
                FixtureInit.write db fixture { Rows = 200; Seed = 1L } false |> ok |> ignore
                let loaded, issues = Fixture.loadAndCheck db fixture |> ok
                Expect.isFalse (Fixture.hasErrors issues) "the synthetic fixture is valid"
                let prepared = Engine.prepare db loaded.Store |> ok
                let coverage = Engine.coverage db
                Expect.isNonEmpty coverage "the model has expressions"

                let failures = [
                    for (ref, unsupported) in coverage do
                        if ref.Kind = ObjectKind.Measure && unsupported.IsEmpty then
                            let name = List.last ref.Path

                            match Engine.evaluateScalar prepared ("[" + name.Replace("]", "]]") + "]") with
                            | Error(DaxNotSupported(what, at)) ->
                                // What depends on the data can't be seen without it: the blank row, rows that
                                // repeat, the sign of a number FORMAT is given, the order of NETWORKDAYS' dates.
                                let dataDependent = [
                                    "blank row"
                                    "repeat"
                                    "a negative number with a format"
                                    "start date after the end date"
                                ]

                                if not (dataDependent |> List.exists what.Contains) then
                                    $"{ObjectRef.format ref}: coverage cleared it, the engine refused: {what} ({at})"
                            // The model is valid in Power BI, so the engine calling its DAX invalid (a
                            // circular dependency, say) is a wrong verdict; an error of evaluation can
                            // come from the synthetic data.
                            | Error(DaxInvalid _ as e) -> $"{ObjectRef.format ref}: {TessError.message e}"
                            | _ -> ()
                ]

                if not failures.IsEmpty then
                    failtestf "%d measures:\n%s" failures.Length (String.concat "\n" failures)

            // The field parameters: their display column has Group By Columns (its Fields column),
            // and Power BI groups by both ("Power BI takes care of adding the columns to the DAX
            // query": SQLBI, "Understanding Group By Columns in Power BI").
            testCase "field parameters group by their display column with its Fields column"
            <| fun () ->
                use t = new TempDir()
                let _, db = ModelIO.loadPath path |> ok
                let fixture = t.Combine "fixture"
                FixtureInit.write db fixture { Rows = 20; Seed = 1L } false |> ok |> ignore
                let loaded, _ = Fixture.loadAndCheck db fixture |> ok
                let prepared = Engine.prepare db loaded.Store |> ok

                let parameters = [
                    for table in db.Model.Tables do
                        for c in table.Columns do
                            if
                                not (isNull c.RelatedColumnDetails)
                                && c.RelatedColumnDetails.GroupByColumns.Count > 0
                            then
                                table.Name,
                                c.Name,
                                [
                                    for g in c.RelatedColumnDetails.GroupByColumns do
                                        if not (isNull g.GroupingColumn) then
                                            g.GroupingColumn.Name
                                ]
                ]

                // spaceparts has three; any other model is checked for what it has.
                if path.Contains("spaceparts", StringComparison.OrdinalIgnoreCase) then
                    Expect.equal parameters.Length 3 "spaceparts has three field parameters"

                for (table, display, fields) in parameters do
                    let quote (c: string) = $"'{table}'[{c}]"
                    let grouped = String.concat ", " (quote display :: (fields |> List.map quote))

                    let n =
                        match Engine.evaluateScalar prepared $"COUNTROWS ( '{table}' )" |> ok with
                        | QueryValue.Int64 n -> n
                        | other -> failtestf "%A" other

                    match Engine.evaluateQuery prepared $"EVALUATE SUMMARIZECOLUMNS ( {grouped} )" with
                    | Ok [ r ] -> Expect.equal (int64 r.Rows.Length) n $"{table}: one row per parameter"
                    | other -> failtestf "%s: %A" table other

                    match Engine.evaluateQuery prepared $"EVALUATE SUMMARIZECOLUMNS ( {quote display} )" with
                    | Error(DaxNotSupported(what, _)) -> Expect.stringContains what "Group By Columns" table
                    | other -> failtestf "%s: %A" table other
    ]
