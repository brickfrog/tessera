module Tessera.Tests.DepsKeywordTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

[<Tests>]
let tests =
    testList "Deps keywords" [
        testCase "RETURN [X], NOT [X] and IN [X]: the keyword is not a table"
        <| fun () ->
            let db = sampleDatabase ()
            let sales = db.Model.Tables.["Sales"]
            sales.Measures.Add(new Measure(Name = "R", Expression = "VAR x = 1\nRETURN\n    [Total] + x"))
            sales.Measures.Add(new Measure(Name = "N", Expression = "IF(NOT [Total], 1)"))
            sales.Measures.Add(new Measure(Name = "I", Expression = "1 IN [Total]"))

            let g = Deps.build db

            for name in [ "R"; "N"; "I" ] do
                let refs = Deps.dependsOn g (ObjectRef.inTable ObjectKind.Measure "Sales" name)

                Expect.equal
                    (refs |> List.map (fun r -> r.Target, r.Text))
                    [
                        Deps.Resolved(ObjectRef.inTable ObjectKind.Measure "Sales" "Total"), "[Total]"
                    ]
                    name

            Expect.isEmpty
                (Validate.validate db |> List.filter (fun i -> i.Rule = "unresolved-reference"))
                "no table named RETURN, NOT or IN is reported missing"
    ]
