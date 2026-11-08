module Tessera.Tests.SerializeTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private sales () =
    (sampleDatabase ()).Model.Tables.["Sales"]

[<Tests>]
let tests =
    testList "Serialize" [
        testCase "every kind of object serializes to JSON and TMDL, with and without children"
        <| fun () ->
            let db = sampleDatabase ()
            // Power BI-only property on a column: serializing in AS mode would throw.
            db.Model.DefaultPowerBIDataSourceVersion <- PowerBIDataSourceVersion.PowerBI_V3

            for e in Inventory.objects db do
                for children in [ false; true ] do
                    let name = $"{ObjectRef.format e.Ref} children={children}"
                    Expect.isNotEmpty (Serialize.objectJson db children e.Object) name
                    Expect.isNotEmpty (Serialize.objectTmdl children e.Object) name
                    Serialize.flatten (Serialize.objectJson db children e.Object) |> ignore

        testCase "without children, a table's JSON has no columns but keeps annotations"
        <| fun () ->
            let db = sampleDatabase ()
            let t = db.Model.Tables.["Sales"]
            t.Annotations.Add(new Annotation(Name = "Owner", Value = "finance"))
            let props = Serialize.flatten (Serialize.objectJson db false t)
            Expect.isFalse (props |> List.exists (fun (p, _) -> p.StartsWith "columns")) ""
            Expect.contains props ("annotations.Owner", "finance") ""

            let withChildren = Serialize.flatten (Serialize.objectJson db true t)
            Expect.isTrue (withChildren |> List.exists (fun (p, _) -> p.StartsWith "columns.Amount")) ""

        testCase "without children, an object's own sub-objects and extended properties are kept"
        <| fun () ->
            let db = sampleDatabase ()
            let m = db.Model.Tables.["Sales"].Measures.["Total"]
            m.KPI <- new KPI(TargetExpression = "2")
            m.FormatStringDefinition <- new FormatStringDefinition(Expression = "\"0\"")
            m.DetailRowsDefinition <- new DetailRowsDefinition(Expression = "Sales")
            m.ExtendedProperties.Add(new JsonExtendedProperty(Name = "J", Value = """{"a":1}"""))
            m.ExtendedProperties.Add(new StringExtendedProperty(Name = "S", Value = "v"))
            let props = Serialize.flatten (Serialize.objectJson db false m)
            Expect.contains props ("kpi.targetExpression", "2") ""
            Expect.contains props ("formatStringDefinition.expression", "\"0\"") ""
            Expect.contains props ("detailRowsDefinition.expression", "Sales") ""
            Expect.contains props ("extendedProperties.J.value.a", "1") ""
            Expect.contains props ("extendedProperties.S", "v") ""

            let ti = db.Model.Tables.["Time Intelligence"]
            ti.CalculationGroup.Precedence <- 3

            ti.CalculationGroup.MultipleOrEmptySelectionExpression <-
                new CalculationGroupExpression(Expression = "BLANK()")

            let paths = Serialize.flatten (Serialize.objectJson db false ti) |> List.map fst
            Expect.contains paths "calculationGroup.precedence" ""
            Expect.contains paths "calculationGroup.multipleOrEmptySelectionExpression.expression" ""

            for child in [ "columns"; "measures"; "partitions"; "calculationGroup.calculationItems" ] do
                Expect.isFalse (paths |> List.exists (fun p -> p.StartsWith child)) child

            let role = db.Model.Roles.["Readers"]
            role.Members.Add(new ExternalModelRoleMember(MemberName = "a@b.c", IdentityProvider = "AzureAD"))

            let rolePaths =
                Serialize.flatten (Serialize.objectJson db false role) |> List.map fst

            Expect.isFalse (rolePaths |> List.exists (fun p -> p.StartsWith "members")) "role members stay out"

        testCase "flatten: nested objects, named arrays, plain arrays, scalars"
        <| fun () ->
            let json =
                """{ "name": "x", "n": 3, "b": true, "z": null,
                     "source": { "type": "m", "expression": "let\na" },
                     "annotations": [ { "name": "A", "value": "1" }, { "name": "B", "value": "2" } ],
                     "extendedProperties": [ { "name": "E", "type": "json", "value": { "k": 1 } } ],
                     "list": [ "p", "q" ] }"""

            Expect.equal
                (Serialize.flatten json)
                [
                    "name", "x"
                    "n", "3"
                    "b", "true"
                    "z", "null"
                    "source.type", "m"
                    "source.expression", "let\na"
                    "annotations.A", "1"
                    "annotations.B", "2"
                    "extendedProperties.E.type", "json"
                    "extendedProperties.E.value.k", "1"
                    "list[0]", "p"
                    "list[1]", "q"
                ]
                ""

        testCase "multi-line expressions are single values, not split arrays"
        <| fun () ->
            let db = sampleDatabase ()
            let m = db.Model.Tables.["Sales"].Measures.["Total"]
            m.Expression <- "SUMX (\n    Sales,\n    Sales[Amount]\n)"
            let props = Serialize.flatten (Serialize.objectJson db false m)
            Expect.contains props ("expression", m.Expression) ""

        testCase "select: exact leaf, or everything below a prefix"
        <| fun () ->
            let props = [ "source.type", "m"; "source.expression", "x"; "sourceColumn", "y" ]
            Expect.equal (Serialize.select "source.type" props) [ "source.type", "m" ] ""
            Expect.equal (Serialize.select "source" props) [ "source.type", "m"; "source.expression", "x" ] ""
            Expect.equal (Serialize.select "nope" props) [] ""

        testCase "the model entry carries compatibilityLevel"
        <| fun () ->
            let db = sampleDatabase ()
            let model = Inventory.objects db |> List.head
            Expect.contains (Serialize.properties db model) ("compatibilityLevel", "1702") ""

        testCase "TMDL snippet is unqualified and LF"
        <| fun () ->
            let tmdl = Serialize.objectTmdl false ((sales ()).Measures.["Total"])
            Expect.stringStarts tmdl "measure Total = SUM(Sales[Amount])" ""
            Expect.isFalse (tmdl.Contains "\r") ""
            Expect.isFalse (tmdl.Contains "ref table") ""
    ]
