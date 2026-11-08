module Tessera.Tests.MutationTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

[<Tests>]
let tests =
    testList "Mutation.apply" [
        testCase "reports exactly what changed, in the change-set shape"
        <| fun () ->
            let db = sampleDatabase ()

            let applied =
                Mutation.apply db (fun db ->
                    db.Model.Tables.["Sales"].Measures.["Total"].FormatString <- "0.00"
                    Ok Mutation.Effect.none)
                |> ok

            Expect.equal
                (applied.Changes |> List.map (fun c -> ObjectRef.format c.Ref, c.ChangeKind))
                [ "measure:Sales/Total", ChangeSet.ChangeKind.Modified ]
                ""

            Expect.isEmpty applied.IntroducedErrors ""
            Expect.equal db.Model.Tables.["Sales"].Measures.["Total"].FormatString "0.00" "the edit is applied"

        testCase "no edit, no changes"
        <| fun () ->
            Expect.isEmpty (Mutation.apply (sampleDatabase ()) (fun _ -> Ok Mutation.Effect.none) |> ok).Changes ""

        testCase "errors the edit introduces are reported; existing ones aren't"
        <| fun () ->
            let db = sampleDatabase ()
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Old Broken", Expression = "[Nope]+'Gone'[X]"))

            let applied =
                Mutation.apply db (fun db ->
                    db.Model.Tables.["Sales"].Measures.["Total"].Expression <- "SUM('Missing'[Amount])"
                    Ok Mutation.Effect.none)
                |> ok

            Expect.equal
                (applied.IntroducedErrors |> List.map (fun i -> ObjectRef.format i.Ref, i.Rule))
                [ "measure:Sales/Total", "unresolved-reference" ]
                "only the new one"

        testCase "a renamed object's existing errors don't count as new"
        <| fun () ->
            let db = sampleDatabase ()

            let m =
                new Measure(Name = "Broken", Expression = "'Gone'[X]", LineageTag = "tag-broken")

            db.Model.Tables.["Sales"].Measures.Add m

            let applied =
                Mutation.apply db (fun db ->
                    db.Model.Tables.["Sales"].Measures.["Broken"].Name <- "Still Broken"
                    Ok Mutation.Effect.none)
                |> ok

            Expect.equal (applied.Changes |> List.map (fun c -> c.ChangeKind)) [ ChangeSet.ChangeKind.Moved ] ""
            Expect.isEmpty applied.IntroducedErrors ""

        testCase "an edit that fails changes nothing that's reported"
        <| fun () ->
            match Mutation.apply (sampleDatabase ()) (fun _ -> Error(ObjectNotFound "x")) with
            | Error(ObjectNotFound _) -> ()
            | other -> failtestf "%A" other
    ]
