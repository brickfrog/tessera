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

                for (ref, unsupported) in coverage do
                    if ref.Kind = ObjectKind.Measure && unsupported.IsEmpty then
                        let name = List.last ref.Path

                        match Engine.evaluateScalar prepared ("[" + name.Replace("]", "]]") + "]") with
                        | Error(DaxNotSupported(what, at)) ->
                            // What depends on the data can't be seen without it.
                            if not (what.Contains "blank row" || what.Contains "repeat") then
                                failtestf
                                    "%s: coverage cleared it, the engine refused: %s (%s)"
                                    (ObjectRef.format ref)
                                    what
                                    at
                        | _ -> ()
    ]
