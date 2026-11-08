module Tessera.Tests.AddTests

open System
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.AddObject
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private error =
    function
    | Ok _ -> failtest "expected an error"
    | Error e -> e

let private ref (s: string) =
    match ObjectRef.parse s with
    | Ok r -> r
    | Error e -> failtestf "%s" (TessError.message e)

let private spec (r: string) (details: Details) = AddSpec.create (ref r) details

/// Add through the editing pipeline, as the CLI does.
let private apply (db: Database) (s: AddSpec) =
    Mutation.apply db (fun db -> AddObject.add db s) |> ok

let private created (a: Mutation.Applied) =
    a.Changes
    |> List.filter (fun c -> c.ChangeKind = ChangeSet.ChangeKind.Created)
    |> List.map (fun c -> ObjectRef.format c.Ref)

let private isGuid (s: string) =
    not (isNull s) && fst (Guid.TryParse s) && s = s.ToLowerInvariant()

let private addOk db s = AddObject.add db s |> ok
let private addError db s = AddObject.add db s |> error

let private expectCode (e: TessError) (code: string) =
    Expect.equal (TessError.code e) code (TessError.message e)

let private sales (db: Database) = db.Model.Tables.["Sales"]

/// A second dimension with no relationship to Sales yet: Product[ProductKey], and Sales[ProductKey].
let private withProduct () =
    let db = sampleDatabase ()
    let product = new Table(Name = "Product")

    product.Columns.Add(new DataColumn(Name = "ProductKey", DataType = DataType.Int64, SourceColumn = "ProductKey"))
    product.Columns.Add(new DataColumn(Name = "Name", DataType = DataType.String, SourceColumn = "Name"))
    db.Model.Tables.Add product

    (sales db).Columns.Add(new DataColumn(Name = "ProductKey", DataType = DataType.Int64, SourceColumn = "ProductKey"))

    db

let private rel =
    RelationshipSpec.between "'Sales'[ProductKey]" "'Product'[ProductKey]"

[<Tests>]
let tests =
    testList "AddObject.add" [
        testList "measure" [
            testCase "is created with a fresh lineage tag and its metadata"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db {
                        spec "measure:Sales/Margin" (Details.Measure "[Total] * 0.1") with
                            Metadata = {
                                Description = Some "Ten percent."
                                DisplayFolder = Some "KPIs"
                                FormatString = Some "0.0%"
                                Hidden = true
                            }
                    }

                Expect.equal (created a) [ "measure:Sales/Margin" ] ""
                Expect.isEmpty a.IntroducedErrors ""
                Expect.equal a.Effect.Notes [ "Added measure:Sales/Margin" ] ""
                let m = (sales db).Measures.["Margin"]
                Expect.equal m.Expression "[Total] * 0.1" ""
                Expect.isTrue (isGuid m.LineageTag) m.LineageTag
                Expect.equal m.Description "Ten percent." ""
                Expect.equal m.DisplayFolder "KPIs" ""
                Expect.equal m.FormatString "0.0%" ""
                Expect.isTrue m.IsHidden ""

            testCase "lineage tags are unique"
            <| fun () ->
                let db = sampleDatabase ()
                addOk db (spec "measure:Sales/A" (Details.Measure "1")) |> ignore
                addOk db (spec "measure:Sales/B" (Details.Measure "2")) |> ignore
                Expect.notEqual (sales db).Measures.["A"].LineageTag (sales db).Measures.["B"].LineageTag ""

            testCase "the table matches case-insensitively, and the ref uses its real name"
            <| fun () ->
                let a =
                    apply (sampleDatabase ()) (spec "measure:sales/Margin" (Details.Measure "1"))

                Expect.equal (created a) [ "measure:Sales/Margin" ] ""

            testCase "a missing table is reported, with a suggestion"
            <| fun () ->
                let e = addError (sampleDatabase ()) (spec "measure:Sles/X" (Details.Measure "1"))
                expectCode e "object-not-found"
                Expect.stringContains (TessError.message e) "did you mean table:Sales?" ""

                let e =
                    addError (sampleDatabase ()) (spec "measure:Nothing Like It/X" (Details.Measure "1"))

                Expect.equal e (ObjectNotFound "table:Nothing Like It") "no suggestion when nothing is close"

            testCase "measure names are unique model-wide, and shared with the table's columns"
            <| fun () ->
                let db = sampleDatabase ()

                Expect.equal
                    (addError db (spec "measure:Date/total" (Details.Measure "1")))
                    (NameCollision("total", "measure:Sales/Total"))
                    "another table's measure"

                Expect.equal
                    (addError db (spec "measure:Sales/AMOUNT" (Details.Measure "1")))
                    (NameCollision("AMOUNT", "column:Sales/Amount"))
                    "a column of the same table"

                Expect.equal
                    (addError db (spec "measure:Date/Calendar" (Details.Measure "1")))
                    (NameCollision("Calendar", "hierarchy:Date/Calendar"))
                    "a hierarchy of the same table"

            testCase "an empty expression is refused"
            <| fun () ->
                expectCode (addError (sampleDatabase ()) (spec "measure:Sales/X" (Details.Measure "  "))) "cannot-add"

            testCase "broken DAX is added, but reported as an introduced error"
            <| fun () ->
                let a =
                    apply (sampleDatabase ()) (spec "measure:Sales/Broken" (Details.Measure "SUM('Gone'[X])"))

                Expect.equal (created a) [ "measure:Sales/Broken" ] ""

                Expect.equal
                    (a.IntroducedErrors |> List.map (fun i -> ObjectRef.format i.Ref, i.Rule))
                    [ "measure:Sales/Broken", "unresolved-reference" ]
                    ""
        ]

        testList "names" [
            testCase "invalid names are refused, as by rename"
            <| fun () ->
                for name in [ " Lead"; "Trail "; "Line\nBreak" ] do
                    let r = ObjectRef.inTable ObjectKind.Measure "Sales" name
                    expectCode (addError (sampleDatabase ()) (AddSpec.create r (Details.Measure "1"))) "invalid-name"

            testCase "function names must be identifiers"
            <| fun () ->
                let e =
                    addError (sampleDatabase ()) (spec "function:My Fn" (Details.Function "() => 1"))

                expectCode e "invalid-name"

            testCase "the details must match the reference's kind"
            <| fun () ->
                expectCode (addError (sampleDatabase ()) (spec "column:Sales/X" (Details.Measure "1"))) "cannot-add"

            testCase "data sources and calendars aren't addable"
            <| fun () -> expectCode (addError (sampleDatabase ()) (spec "datasource:SQL" Details.Culture)) "cannot-add"

            testCase "metadata a kind doesn't have is refused"
            <| fun () ->
                let e =
                    addError (sampleDatabase ()) {
                        spec "culture:fr-FR" Details.Culture with
                            Metadata = {
                                Metadata.none with
                                    Description = Some "x"
                            }
                    }

                expectCode e "cannot-add"

                let e =
                    addError (sampleDatabase ()) {
                        spec "role:R" (Details.Role(ModelPermission.Read, [])) with
                            Metadata = { Metadata.none with Hidden = true }
                    }

                expectCode e "cannot-add"
        ]

        testList "--if-not-exists" [
            testCase "an existing object (any case) is a no-op with a note"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db {
                        spec "measure:sales/TOTAL" (Details.Measure "42") with
                            IfNotExists = true
                    }

                Expect.isEmpty a.Changes ""
                Expect.equal a.Effect.Notes [ "measure:Sales/Total already exists; nothing to do." ] ""
                Expect.equal (sales db).Measures.["Total"].Expression "SUM(Sales[Amount])" "untouched"

            testCase "without it, an existing object is a collision"
            <| fun () ->
                Expect.equal
                    (addError (sampleDatabase ()) (spec "measure:Sales/Total" (Details.Measure "1")))
                    (NameCollision("Total", "measure:Sales/Total"))
                    ""

            testCase "a different kind holding the name is still a collision"
            <| fun () ->
                let e =
                    addError (sampleDatabase ()) {
                        spec "measure:Sales/Amount" (Details.Measure "1") with
                            IfNotExists = true
                    }

                Expect.equal e (NameCollision("Amount", "column:Sales/Amount")) ""

            testCase "a relationship over the same columns counts as existing"
            <| fun () ->
                let a =
                    apply (sampleDatabase ()) {
                        spec
                            "relationship:Other"
                            (Details.Relationship(RelationshipSpec.between "Sales[DateKey]" "Date[DateKey]")) with
                            IfNotExists = true
                    }

                Expect.isEmpty a.Changes ""
                Expect.stringContains (List.head a.Effect.Notes) "relationship:Sales-Date already joins" ""
        ]

        testList "column" [
            testCase "a calculated column"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db (spec "column:Sales/Double" (Details.Column(ColumnSource.Calculated "Sales[Amount] * 2")))

                Expect.equal (created a) [ "column:Sales/Double" ] ""
                Expect.isEmpty a.IntroducedErrors ""
                let c = (sales db).Columns.["Double"] :?> CalculatedColumn
                Expect.equal c.Expression "Sales[Amount] * 2" ""
                Expect.isTrue (isGuid c.LineageTag) ""

            testCase "a data column: Desktop's summarization, source column defaulting to its name"
            <| fun () ->
                let db = sampleDatabase ()

                addOk db (spec "column:Sales/Qty" (Details.Column(ColumnSource.Data(DataType.Int64, None))))
                |> ignore

                addOk
                    db
                    (spec "column:Sales/Region" (Details.Column(ColumnSource.Data(DataType.String, Some "region_code"))))
                |> ignore

                let qty = (sales db).Columns.["Qty"] :?> DataColumn
                Expect.equal qty.SourceColumn "Qty" ""
                Expect.equal qty.DataType DataType.Int64 ""
                Expect.equal qty.SummarizeBy AggregateFunction.Sum "numeric: sum"
                Expect.equal qty.Annotations.["SummarizationSetBy"].Value "Automatic" ""
                Expect.isTrue (isGuid qty.LineageTag) ""
                let region = (sales db).Columns.["Region"] :?> DataColumn
                Expect.equal region.SourceColumn "region_code" ""
                Expect.equal region.SummarizeBy AggregateFunction.None "text: none"

            testCase "column names are unique within the table, against measures and hierarchies too"
            <| fun () ->
                let db = sampleDatabase ()
                let calc = Details.Column(ColumnSource.Calculated "1")

                Expect.equal
                    (addError db (spec "column:Sales/amount" calc))
                    (NameCollision("amount", "column:Sales/Amount"))
                    ""

                Expect.equal
                    (addError db (spec "column:Sales/Total" calc))
                    (NameCollision("Total", "measure:Sales/Total"))
                    ""

                Expect.equal
                    (addError db (spec "column:Date/Calendar" calc))
                    (NameCollision("Calendar", "hierarchy:Date/Calendar"))
                    ""

                addOk db (spec "column:Date/Amount" calc) |> ignore

            testCase "calculated tables' and calculation groups' columns aren't addable"
            <| fun () ->
                let db = sampleDatabase ()

                addOk db (spec "table:Calc" (Details.Table(TableSource.Calculated "{1}")))
                |> ignore

                let e =
                    addError db (spec "column:Calc/X" (Details.Column(ColumnSource.Calculated "1")))

                expectCode e "cannot-add"
                Expect.stringContains (TessError.message e) "inferred" ""

                let e =
                    addError
                        db
                        (spec "column:Time Intelligence/X" (Details.Column(ColumnSource.Data(DataType.String, None))))

                expectCode e "cannot-add"
        ]

        testList "table" [
            testCase "a calculated table: one calculated import partition"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db (spec "table:Numbers" (Details.Table(TableSource.Calculated "GENERATESERIES(1, 10)")))

                Expect.equal (created a) [ "table:Numbers"; "partition:Numbers/Numbers" ] ""
                Expect.isEmpty a.IntroducedErrors ""
                let t = db.Model.Tables.["Numbers"]
                Expect.isTrue (isGuid t.LineageTag) ""
                let p = t.Partitions.["Numbers"]
                Expect.equal p.Mode ModeType.Import ""
                Expect.equal (p.Source :?> CalculatedPartitionSource).Expression "GENERATESERIES(1, 10)" ""

            testCase "an M table, and Power BI's query order follows"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.Annotations.Add(new Annotation(Name = "PBI_QueryOrder", Value = "[\"Sales\",\"Date\"]"))

                let a =
                    apply
                        db
                        (spec "table:Budget" (Details.Table(TableSource.M("let x = 1 in x", ModeType.DirectQuery))))

                Expect.equal (created a) [ "table:Budget"; "partition:Budget/Budget" ] ""
                let p = db.Model.Tables.["Budget"].Partitions.["Budget"]
                Expect.equal p.Mode ModeType.DirectQuery ""
                Expect.equal (p.Source :?> MPartitionSource).Expression "let x = 1 in x" ""
                Expect.equal db.Model.Annotations.["PBI_QueryOrder"].Value "[\"Sales\",\"Date\",\"Budget\"]" ""

            testCase "a calculation group, as Power BI creates one"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.DiscourageImplicitMeasures <- false

                let a =
                    apply db (spec "table:Units" (Details.Table(TableSource.CalculationGroup(None, None))))

                Expect.equal
                    (created a)
                    [
                        "table:Units"
                        "column:Units/Name"
                        "column:Units/Ordinal"
                        "partition:Units/Units"
                    ]
                    ""

                Expect.isEmpty a.IntroducedErrors ""
                let t = db.Model.Tables.["Units"]
                Expect.equal t.CalculationGroup.Precedence 1 "one more than Time Intelligence's"
                let name = t.Columns.["Name"] :?> DataColumn
                let ordinal = t.Columns.["Ordinal"] :?> DataColumn
                Expect.equal name.SourceColumn "Name" ""
                Expect.equal name.DataType DataType.String ""
                Expect.isTrue (obj.ReferenceEquals(name.SortByColumn, ordinal)) "sorted by Ordinal"
                Expect.isTrue ordinal.IsHidden ""
                Expect.equal ordinal.DataType DataType.Int64 ""
                Expect.isTrue (t.Partitions.[0].Source :? CalculationGroupSource) ""
                Expect.isTrue db.Model.DiscourageImplicitMeasures "calculation groups require it"
                Expect.contains (a.Changes |> List.map (fun c -> ObjectRef.format c.Ref)) "model" ""

            testCase "a calculation group with its own column name and precedence"
            <| fun () ->
                let db = sampleDatabase ()

                addOk db (spec "table:Units" (Details.Table(TableSource.CalculationGroup(Some 5, Some "Unit"))))
                |> ignore

                let t = db.Model.Tables.["Units"]
                Expect.equal t.CalculationGroup.Precedence 5 ""
                Expect.isTrue (t.Columns.Contains "Unit") ""

            testCase "calculation groups need compatibility level 1470"
            <| fun () ->
                let db = new Database(CompatibilityLevel = 1400, Model = new Model())

                expectCode
                    (addError db (spec "table:Units" (Details.Table(TableSource.CalculationGroup(None, None)))))
                    "cannot-add"

            testCase "table names are unique, and shared with named expressions"
            <| fun () ->
                let db = sampleDatabase ()
                let m = Details.Table(TableSource.M("let x = 1 in x", ModeType.Import))
                Expect.equal (addError db (spec "table:SALES" m)) (NameCollision("SALES", "table:Sales")) ""
                expectCode (addError db (spec "table:ServerName" m)) "name-collision"
                expectCode (addError db (spec "expression:Sales" (Details.Expression "1"))) "name-collision"
        ]

        testList "hierarchy and level" [
            testCase "a hierarchy gets ordered levels named after their columns"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db (spec "hierarchy:Date/By Month" (Details.Hierarchy [ "month"; "Year" ]))

                Expect.equal
                    (created a)
                    [
                        "hierarchy:Date/By Month"
                        "level:Date/By Month/Month"
                        "level:Date/By Month/Year"
                    ]
                    ""

                let h = db.Model.Tables.["Date"].Hierarchies.["By Month"]
                Expect.isTrue (isGuid h.LineageTag) ""

                Expect.equal
                    [ for l in h.Levels -> l.Name, l.Ordinal, l.Column.Name, isGuid l.LineageTag ]
                    [ "Month", 0, "Month", true; "Year", 1, "Year", true ]
                    ""

            testCase "a hierarchy's columns must exist, once each"
            <| fun () ->
                let db = sampleDatabase ()
                let e = addError db (spec "hierarchy:Date/H" (Details.Hierarchy [ "Yeer" ]))
                Expect.stringContains (TessError.message e) "did you mean column:Date/Year?" ""
                expectCode (addError db (spec "hierarchy:Date/H" (Details.Hierarchy [ "Year"; "year" ]))) "cannot-add"
                expectCode (addError db (spec "hierarchy:Date/H" (Details.Hierarchy []))) "cannot-add"

            testCase "a level is appended; its column defaults to its name"
            <| fun () ->
                let db = sampleDatabase ()
                let a = apply db (spec "level:Date/Calendar/DateKey" (Details.Level(None, None)))
                Expect.equal (created a) [ "level:Date/Calendar/DateKey" ] ""
                let l = db.Model.Tables.["Date"].Hierarchies.["Calendar"].Levels.["DateKey"]
                Expect.equal l.Ordinal 2 ""
                Expect.equal l.Column.Name "DateKey" ""
                Expect.isTrue (isGuid l.LineageTag) ""

            testCase "a level at an ordinal moves the others down"
            <| fun () ->
                let db = sampleDatabase ()

                addOk db (spec "level:Date/Calendar/Key" (Details.Level(Some "DateKey", Some 0)))
                |> ignore

                let h = db.Model.Tables.["Date"].Hierarchies.["Calendar"]

                Expect.equal
                    [ for l in h.Levels |> Seq.sortBy (fun l -> l.Ordinal) -> l.Name, l.Ordinal ]
                    [ "Key", 0; "Year", 1; "Month", 2 ]
                    ""

            testCase "a level's column can't already be in the hierarchy; ordinals must be in range"
            <| fun () ->
                let db = sampleDatabase ()
                expectCode (addError db (spec "level:Date/Calendar/Y2" (Details.Level(Some "Year", None)))) "cannot-add"

                expectCode
                    (addError db (spec "level:Date/Calendar/K" (Details.Level(Some "DateKey", Some 3))))
                    "cannot-add"

                expectCode
                    (addError db (spec "level:Date/Calendar/year" (Details.Level(Some "DateKey", None))))
                    "name-collision"

                expectCode
                    (addError db (spec "level:Date/Calendr/K" (Details.Level(Some "DateKey", None))))
                    "object-not-found"
        ]

        testList "calcitem" [
            testCase "is appended after the last ordinal, with a format string expression"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply
                        db
                        (spec
                            "calcitem:Time Intelligence/PY"
                            (Details.CalculationItem(
                                "CALCULATE(SELECTEDMEASURE(), SAMEPERIODLASTYEAR('Date'[DateKey]))",
                                Some "\"0.0\"",
                                None
                            )))

                Expect.equal (created a) [ "calcitem:Time Intelligence/PY" ] ""

                let item =
                    db.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["PY"]

                Expect.equal item.Ordinal 2 ""
                Expect.equal item.FormatStringDefinition.Expression "\"0.0\"" ""

            testCase "at an ordinal, the others move down"
            <| fun () ->
                let db = sampleDatabase ()

                addOk
                    db
                    (spec
                        "calcitem:Time Intelligence/First"
                        (Details.CalculationItem("SELECTEDMEASURE()", None, Some 1)))
                |> ignore

                Expect.equal
                    [
                        for i in
                            db.Model.Tables.["Time Intelligence"].CalculationGroup.CalculationItems
                            |> Seq.sortBy (fun i -> i.Ordinal) -> i.Name, i.Ordinal
                    ]
                    [ "Current", 0; "First", 1; "YTD", 2 ]
                    ""

            testCase "only in calculation groups"
            <| fun () ->
                let e =
                    addError (sampleDatabase ()) (spec "calcitem:Sales/X" (Details.CalculationItem("1", None, None)))

                expectCode e "cannot-add"
                Expect.stringContains (TessError.message e) "not a calculation group" ""
        ]

        testList "relationship" [
            testCase "defaults: a GUID name, many-to-one, single direction, active"
            <| fun () ->
                let db = withProduct ()

                let a =
                    apply
                        db
                        (AddSpec.create
                            {
                                Kind = ObjectKind.Relationship
                                Path = []
                            }
                            (Details.Relationship rel))

                Expect.isEmpty a.IntroducedErrors ""
                Expect.isEmpty a.Effect.Warnings ""

                let r =
                    db.Model.Relationships |> Seq.find (fun r -> r.Name <> "Sales-Date") :?> SingleColumnRelationship

                Expect.isTrue (isGuid r.Name) r.Name
                Expect.equal (created a) [ "relationship:" + r.Name ] ""
                Expect.equal r.FromColumn.Name "ProductKey" ""
                Expect.equal r.FromTable.Name "Sales" ""
                Expect.equal r.ToTable.Name "Product" ""
                Expect.equal r.FromCardinality RelationshipEndCardinality.Many ""
                Expect.equal r.ToCardinality RelationshipEndCardinality.One ""
                Expect.equal r.CrossFilteringBehavior CrossFilteringBehavior.OneDirection ""
                Expect.isTrue r.IsActive ""

                Expect.equal
                    a.Effect.Notes
                    [ $"Added relationship:{r.Name} (Sales[ProductKey] -> Product[ProductKey])" ]
                    ""

            testCase "a name, cardinality, direction and inactivity can be given"
            <| fun () ->
                let db = withProduct ()

                addOk
                    db
                    (spec
                        "relationship:Sales-Product"
                        (Details.Relationship {
                            rel with
                                Cardinality = Cardinality.ManyToMany
                                CrossFilter = Some CrossFilter.Both
                                Inactive = true
                        }))
                |> ignore

                let r = db.Model.Relationships.["Sales-Product"] :?> SingleColumnRelationship
                Expect.equal r.FromCardinality RelationshipEndCardinality.Many ""
                Expect.equal r.ToCardinality RelationshipEndCardinality.Many ""
                Expect.equal r.CrossFilteringBehavior CrossFilteringBehavior.BothDirections ""
                Expect.isFalse r.IsActive ""

            testCase "one-to-one filters both ways by default, as in Desktop"
            <| fun () ->
                let db = withProduct ()

                addOk
                    db
                    (spec
                        "relationship:R"
                        (Details.Relationship {
                            rel with
                                Cardinality = Cardinality.OneToOne
                        }))
                |> ignore

                let r = db.Model.Relationships.["R"]
                Expect.equal r.CrossFilteringBehavior CrossFilteringBehavior.BothDirections ""

                Expect.equal (r :?> SingleColumnRelationship).FromCardinality RelationshipEndCardinality.One ""

            testCase "a second active path between two tables is made inactive, with a warning"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply
                        db
                        (spec
                            "relationship:ByYear"
                            (Details.Relationship(RelationshipSpec.between "Sales[Amount]" "Date[Year]")))

                Expect.isFalse db.Model.Relationships.["ByYear"].IsActive ""
                Expect.equal a.Effect.Warnings.Length 2 "inactive, and data types differ"
                Expect.stringContains a.Effect.Warnings.[0] "relationship:Sales-Date already joins" ""
                Expect.stringContains a.Effect.Warnings.[1] "different data types" ""

            testCase "refused: a self-join, the same columns again, a non-column, a missing column"
            <| fun () ->
                let db = withProduct ()
                let r = Details.Relationship

                expectCode
                    (addError db (spec "relationship:X" (r (RelationshipSpec.between "Sales[Amount]" "Sales[DateKey]"))))
                    "cannot-add"

                expectCode
                    (addError db (spec "relationship:X" (r (RelationshipSpec.between "Sales[DateKey]" "Date[DateKey]"))))
                    "cannot-add"

                expectCode
                    (addError db (spec "relationship:X" (r (RelationshipSpec.between "Date[DateKey]" "Sales[DateKey]"))))
                    "cannot-add"

                expectCode
                    (addError db (spec "relationship:X" (r (RelationshipSpec.between "Sales[Total]" "Date[DateKey]"))))
                    "cannot-add"

                expectCode
                    (addError db (spec "relationship:X" (r (RelationshipSpec.between "Sales[Nope]" "Date[DateKey]"))))
                    "object-not-found"

                expectCode (addError db (spec "relationship:sales-date" (r rel))) "name-collision"
        ]

        testList "role, perspective, culture" [
            testCase "a role with a permission and row filters"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply
                        db
                        (spec
                            "role:Danes"
                            (Details.Role(ModelPermission.Read, [ "sales", "Sales[Amount] > 0"; "Date", "TRUE()" ])))

                Expect.equal (created a) [ "role:Danes" ] ""
                Expect.isEmpty a.IntroducedErrors ""
                let r = db.Model.Roles.["Danes"]
                Expect.equal r.ModelPermission ModelPermission.Read ""

                Expect.equal
                    [ for tp in r.TablePermissions -> tp.Table.Name, tp.FilterExpression ]
                    [ "Sales", "Sales[Amount] > 0"; "Date", "TRUE()" ]
                    ""

            testCase "a role filter's table must exist, once"
            <| fun () ->
                let db = sampleDatabase ()

                expectCode
                    (addError db (spec "role:R" (Details.Role(ModelPermission.Read, [ "Gone", "TRUE()" ]))))
                    "object-not-found"

                expectCode
                    (addError
                        db
                        (spec "role:R" (Details.Role(ModelPermission.Read, [ "Sales", "TRUE()"; "SALES", "FALSE()" ]))))
                    "cannot-add"

                expectCode (addError db (spec "role:readers" (Details.Role(ModelPermission.Read, [])))) "name-collision"

            testCase "a perspective: tables bring all their members"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply
                        db
                        (spec
                            "perspective:Finance"
                            (Details.Perspective [ "Date"; "[Total]"; "Sales[Amount]"; "measure:Sales/Total" ]))

                Expect.equal (created a) [ "perspective:Finance" ] ""
                let p = db.Model.Perspectives.["Finance"]
                let date = p.PerspectiveTables.["Date"]
                Expect.equal date.PerspectiveColumns.Count 3 ""
                Expect.equal date.PerspectiveHierarchies.Count 1 ""
                let s = p.PerspectiveTables.["Sales"]
                Expect.equal [ for c in s.PerspectiveColumns -> c.Name ] [ "Amount" ] ""
                Expect.equal [ for m in s.PerspectiveMeasures -> m.Name ] [ "Total" ] "listed twice, included once"

            testCase "a perspective holds only tables, columns, measures and hierarchies"
            <| fun () ->
                expectCode
                    (addError (sampleDatabase ()) (spec "perspective:P" (Details.Perspective [ "role:Readers" ])))
                    "cannot-add"

            testCase "a culture, with its canonical name"
            <| fun () ->
                let db = sampleDatabase ()
                let a = apply db (spec "culture:fr-fr" Details.Culture)
                Expect.equal (created a) [ "culture:fr-FR" ] ""
                expectCode (addError db (spec "culture:da-dk" Details.Culture)) "name-collision"
                expectCode (addError db (spec "culture:xx-nope" Details.Culture)) "invalid-name"
        ]

        testList "expression, function, partition" [
            testCase "a named M expression, with a lineage tag, in Power BI's query order"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.Annotations.Add(new Annotation(Name = "PBI_QueryOrder", Value = "[\"ServerName\"]"))
                let a = apply db (spec "expression:Database" (Details.Expression "\"DW\""))
                Expect.equal (created a) [ "expression:Database" ] ""
                let x = db.Model.Expressions.["Database"]
                Expect.equal x.Kind ExpressionKind.M ""
                Expect.equal x.Expression "\"DW\"" ""
                Expect.isTrue (isGuid x.LineageTag) ""
                Expect.equal db.Model.Annotations.["PBI_QueryOrder"].Value "[\"ServerName\",\"Database\"]" ""

            testCase "a function, from compatibility level 1702"
            <| fun () ->
                let db = sampleDatabase ()

                let a =
                    apply db (spec "function:My.Triple" (Details.Function "(x: INT64) => x * 3"))

                Expect.equal (created a) [ "function:My.Triple" ] ""
                Expect.isEmpty a.IntroducedErrors ""
                Expect.isTrue (isGuid db.Model.Functions.["My.Triple"].LineageTag) ""
                expectCode (addError db (spec "function:double" (Details.Function "() => 1"))) "name-collision"
                db.Model.Functions.Clear()
                db.CompatibilityLevel <- 1601
                expectCode (addError db (spec "function:F" (Details.Function "() => 1"))) "cannot-add"

            testCase "an M partition takes its table's mode"
            <| fun () ->
                let db = sampleDatabase ()
                (sales db).Partitions.["Sales"].Mode <- ModeType.DirectQuery

                let a =
                    apply db (spec "partition:Sales/Sales 2024" (Details.Partition("let y = 2 in y", None)))

                Expect.equal (created a) [ "partition:Sales/Sales 2024" ] ""
                let p = (sales db).Partitions.["Sales 2024"]
                Expect.equal p.Mode ModeType.DirectQuery ""
                Expect.equal (p.Source :?> MPartitionSource).Expression "let y = 2 in y" ""
                expectCode (addError db (spec "partition:Sales/sales" (Details.Partition("x", None)))) "name-collision"

            testCase "no partitions for calculated tables and calculation groups"
            <| fun () ->
                let db = sampleDatabase ()

                addOk db (spec "table:Calc" (Details.Table(TableSource.Calculated "{1}")))
                |> ignore

                expectCode (addError db (spec "partition:Calc/P2" (Details.Partition("x", None)))) "cannot-add"

                expectCode
                    (addError db (spec "partition:Time Intelligence/P2" (Details.Partition("x", None))))
                    "cannot-add"
        ]

        testCase "every kind, added together, leaves a valid model that reloads"
        <| fun () ->
            let db = withProduct ()

            let specs = [
                spec "measure:Sales/Margin" (Details.Measure "[Total] * 0.1")
                spec "column:Sales/Double" (Details.Column(ColumnSource.Calculated "Sales[Amount] * 2"))
                spec "column:Sales/Qty" (Details.Column(ColumnSource.Data(DataType.Int64, None)))
                spec "table:Numbers" (Details.Table(TableSource.Calculated "GENERATESERIES(1, 3)"))
                spec "table:Budget" (Details.Table(TableSource.M("let x = 1 in x", ModeType.Import)))
                spec "table:Units" (Details.Table(TableSource.CalculationGroup(None, None)))
                spec "calcitem:Units/Raw" (Details.CalculationItem("SELECTEDMEASURE()", None, None))
                spec "hierarchy:Product/Products" (Details.Hierarchy [ "Name" ])
                spec "level:Product/Products/Key" (Details.Level(Some "ProductKey", None))
                spec "relationship:Sales-Product" (Details.Relationship rel)
                spec "role:Danes" (Details.Role(ModelPermission.Read, [ "Sales", "Sales[Amount] > 0" ]))
                spec "perspective:Finance" (Details.Perspective [ "Sales" ])
                spec "culture:fr-FR" Details.Culture
                spec "expression:Database" (Details.Expression "\"DW\"")
                spec "function:My.Triple" (Details.Function "(x: INT64) => x * 3")
                spec "partition:Sales/Sales 2024" (Details.Partition("let y = 2 in y", None))
            ]

            for s in specs do
                let a = apply db s
                Expect.isEmpty a.IntroducedErrors (ObjectRef.format s.Ref)
                Expect.isNonEmpty (created a) (ObjectRef.format s.Ref)

            Expect.isEmpty
                (Validate.validate db
                 |> List.filter (fun i -> i.Severity = Validate.Severity.Error))
                "no errors"

            use t = new TempDir()
            let dir = t.Combine "tmdl"
            TmdlSerializer.SerializeDatabaseToFolder(db, dir)
            let back = TmdlSerializer.DeserializeDatabaseFromFolder dir
            Expect.isTrue (back.Model.Tables.["Units"].CalculationGroup.CalculationItems.Contains "Raw") ""
            Expect.isTrue (back.Model.Relationships.Contains "Sales-Product") ""
    ]
