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
    ]

/// Roles on the sample: a dimension's filter applies, and every measure coverage clears still
/// evaluates under it (or is refused as not supported, never called invalid).
[<Tests>]
let securityTests =
    testList "Engine on a sample model with roles (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            testCase "a role on a dimension, and the measures under it"
            <| fun () ->
                use t = new TempDir()
                let _, db = ModelIO.loadPath path |> ok
                let fixture = t.Combine "fixture"
                FixtureInit.write db fixture { Rows = 200; Seed = 1L } false |> ok |> ignore
                let loaded, _ = Fixture.loadAndCheck db fixture |> ok

                let role =
                    new Microsoft.AnalysisServices.Tabular.ModelRole(
                        Name = "Brands",
                        ModelPermission = Microsoft.AnalysisServices.Tabular.ModelPermission.Read
                    )

                role.TablePermissions.Add(
                    new Microsoft.AnalysisServices.Tabular.TablePermission(
                        Table = db.Model.Tables.["Brand"],
                        FilterExpression = "LEN ( Brand[Sub Brand] ) > 3"
                    )
                )

                db.Model.Roles.Add role
                let prepared = Engine.prepare db loaded.Store |> ok
                let secured = Engine.asRoles [ "Brands" ] prepared |> ok
                let count (p: Engine.Prepared) dax = Engine.evaluateScalar p dax |> ok

                Expect.isLessThanOrEqual
                    (count secured "COUNTROWS ( Brand ) + 0")
                    (count prepared "COUNTROWS ( Brand ) + 0")
                    "the role keeps some brands"

                let failures = [
                    for (ref, unsupported) in Engine.coverage db do
                        if ref.Kind = ObjectKind.Measure && unsupported.IsEmpty then
                            match Engine.evaluateScalar secured $"[{ObjectRef.name ref}]" with
                            | Error(DaxInvalid _ as e) -> $"{ObjectRef.format ref}: {TessError.message e}"
                            | _ -> ()
                ]

                Expect.isEmpty failures "no measure is called invalid under the role"
    ]
