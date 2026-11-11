/// Row-level security in the local engine: roles' row filters, how they reach other tables, how
/// roles combine, and what is refused.
module Tessera.Tests.EngineSecurityTests

open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// Region (2): 1 North | 2 South
/// Customer (4): 1 Ann North ann@x | 2 Bob North bob@x | 3 Cy South cy@x | 4 Di South di@x
/// Product (3): 1 Bike | 2 Helmet | 3 Lock
/// Sales (6): Id Customer Product Amount
///   1 1 1 100 | 2 2 2 20 | 3 3 1 100 | 4 4 3 15 | 5 1 3 15 | 6 3 2 30      total 280
///   North (customers 1, 2): sales 1, 2, 5 = 135. South: 3, 4, 6 = 145. Bikes: 1, 3 = 200.
/// Users (not related): ann@x -> 1, cy@x -> 2.
/// Customer[AllSales] = SUM ( Sales[Amount] ), a calculated column: 280 in every row.
/// Totals = ROW ( "Total", SUM ( Sales[Amount] ) ), a calculated table: 280.
let private shop: TestModel = {
    Tables = [
        table "Region" [ "RegionKey", DataType.Int64; "Name", DataType.String ] [ [ 1; "North" ]; [ 2; "South" ] ]
        table "Customer" [
            "CustomerKey", DataType.Int64
            "Name", DataType.String
            "RegionKey", DataType.Int64
            "Email", DataType.String
        ] [
            [ 1; "Ann"; 1; "ann@x" ]
            [ 2; "Bob"; 1; "bob@x" ]
            [ 3; "Cy"; 2; "cy@x" ]
            [ 4; "Di"; 2; "di@x" ]
        ]
        |> withCalculated [ "AllSales", DataType.Int64, "SUM ( Sales[Amount] )" ]
        table "Product" [ "ProductKey", DataType.Int64; "Name", DataType.String ] [
            [ 1; "Bike" ]
            [ 2; "Helmet" ]
            [ 3; "Lock" ]
        ]
        table "Sales" [
            "Id", DataType.Int64
            "CustomerKey", DataType.Int64
            "ProductKey", DataType.Int64
            "Amount", DataType.Int64
        ] [
            [ 1; 1; 1; 100 ]
            [ 2; 2; 2; 20 ]
            [ 3; 3; 1; 100 ]
            [ 4; 4; 3; 15 ]
            [ 5; 1; 3; 15 ]
            [ 6; 3; 2; 30 ]
        ]
        table "Users" [ "Email", DataType.String; "RegionKey", DataType.Int64 ] [ [ "ann@x"; 1 ]; [ "cy@x"; 2 ] ]
        calculatedTable "Totals" [ "Total", DataType.Int64 ] "ROW ( \"Total\", SUM ( Sales[Amount] ) )"
    ]
    Rels = [
        rel ("Customer", "RegionKey") ("Region", "RegionKey")
        rel ("Sales", "CustomerKey") ("Customer", "CustomerKey")
        rel ("Sales", "ProductKey") ("Product", "ProductKey")
    ]
    Measures = [
        "Sales", "Amount", "SUM ( Sales[Amount] )"
        "Customer", "Customers", "COUNTROWS ( Customer )"
    ]
    Functions = [ "Twice", "( x : NUMERIC ) => x * 2" ]
}

let private role (db: Database) (name: string) (permission: ModelPermission) (filters: (string * string) list) =
    let r = new ModelRole(Name = name, ModelPermission = permission)

    for (table, dax) in filters do
        r.TablePermissions.Add(new TablePermission(Table = db.Model.Tables.[table], FilterExpression = dax))

    db.Model.Roles.Add r
    r

/// The standard roles of `shop`.
let private withRoles (db: Database) =
    role db "North" ModelPermission.Read [ "Region", "Region[Name] = \"North\"" ]
    |> ignore

    role db "South" ModelPermission.Read [ "Region", "Region[Name] = \"South\"" ]
    |> ignore

    role db "Bikes" ModelPermission.Read [ "Product", "Product[Name] = \"Bike\"" ]
    |> ignore

    role db "BigSales" ModelPermission.Read [ "Sales", "Sales[Amount] > 50" ]
    |> ignore

    role db "Me" ModelPermission.Read [ "Customer", "Customer[Email] = USERPRINCIPALNAME ()" ]
    |> ignore

    role db "Lookup" ModelPermission.ReadRefresh [
        "Region", "Region[RegionKey] = LOOKUPVALUE ( Users[RegionKey], Users[Email], CUSTOMDATA () )"
    ]
    |> ignore

    role db "Everyone" ModelPermission.Read [] |> ignore
    role db "Admin" ModelPermission.Administrator [ "Region", "FALSE ()" ] |> ignore
    role db "Nobody" ModelPermission.None [] |> ignore
    role db "Processor" ModelPermission.Refresh [] |> ignore
    role db "Closed" ModelPermission.Read [ "Region", "FALSE ()" ] |> ignore
    role db "Text" ModelPermission.Read [ "Region", "Region[Name]" ] |> ignore

    role db "OwnTable" ModelPermission.Read [ "Customer", "Customer[CustomerKey] = MAX ( Customer[CustomerKey] )" ]
    |> ignore

    role db "ReadsSales" ModelPermission.Read [
        "Customer", "CONTAINS ( Sales, Sales[CustomerKey], Customer[CustomerKey] )"
    ]
    |> ignore

    role db "Measure" ModelPermission.Read [ "Customer", "[Amount] > 50" ] |> ignore

    role db "Udf" ModelPermission.Read [ "Customer", "Twice ( Customer[CustomerKey] ) = 2" ]
    |> ignore

    role db "WithVar" ModelPermission.Read [ "Customer", "VAR k = Customer[CustomerKey] RETURN k IN { 1, 3 }" ]
    |> ignore

    let ols = role db "Hidden" ModelPermission.Read []

    ols.TablePermissions.Add(
        new TablePermission(Table = db.Model.Tables.["Product"], MetadataPermission = MetadataPermission.None)
    )

    let cols = role db "HiddenColumn" ModelPermission.Read []
    let tp = new TablePermission(Table = db.Model.Tables.["Customer"])

    tp.ColumnPermissions.Add(
        new ColumnPermission(
            Column = db.Model.Tables.["Customer"].Columns.["Email"],
            MetadataPermission = MetadataPermission.None
        )
    )

    cols.TablePermissions.Add tp

let private prepareWith (m: TestModel) (change: Database -> unit) : Engine.Prepared =
    let db, store = build m
    change db

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private shopPrepared = lazy (prepareWith shop withRoles)

let private asRoles (roles: string list) (p: Engine.Prepared) : Engine.Prepared =
    match Engine.asRoles roles p with
    | Ok s -> s
    | Error e -> failtestf "asRoles %A failed: %s" roles (TessError.message e)

let private asRolesError (roles: string list) (p: Engine.Prepared) : TessError =
    match Engine.asRoles roles p with
    | Ok _ -> failtestf "asRoles %A: expected an error" roles
    | Error e -> e

let private refusedMentioning (mentions: string) (e: TessError) =
    match e with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions "the refusal says why"
    | other -> failtestf "expected not-supported, got %A" other

let private upn (name: string) : DaxEngine.Identity = {
    DaxEngine.Identity.none with
        UserPrincipalName = Some name
}

[<Tests>]
let securityTests =
    testList "row-level security" [
        testCase "without roles every row is visible" (fun () ->
            let p = shopPrepared.Value
            is p "SUM ( Sales[Amount] )" 280
            Expect.equal (Engine.roles p) [] "no roles")

        testCase "a role's filter hides rows of its table and of the tables on the many side" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value
            is p "COUNTROWS ( Region )" 1
            is p "COUNTROWS ( Customer )" 2
            is p "SUM ( Sales[Amount] )" 135
            returns p "EVALUATE VALUES ( Customer[Name] )" [ [ "Ann" ]; [ "Bob" ] ]
            Expect.equal (Engine.roles p) [ "North" ] "the roles applied")

        testCase "the one side of a fact table a role filters is not filtered (one direction)" (fun () ->
            let p = asRoles [ "BigSales" ] shopPrepared.Value
            is p "SUM ( Sales[Amount] )" 200
            is p "COUNTROWS ( Customer )" 4
            is p "COUNTROWS ( Product )" 3)

        testCase "ALL, REMOVEFILTERS and ALLSELECTED can't bring hidden rows back" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value
            is p "CALCULATE ( SUM ( Sales[Amount] ), ALL ( Customer ) )" 135
            is p "CALCULATE ( SUM ( Sales[Amount] ), REMOVEFILTERS () )" 135
            is p "CALCULATE ( SUM ( Sales[Amount] ), ALL ( Region ) )" 135
            is p "COUNTROWS ( ALL ( Customer ) )" 2
            is p "CALCULATE ( [Amount], Region[Name] = \"South\" )" null

            returns
                p
                "EVALUATE SUMMARIZECOLUMNS ( Customer[Name], \"a\", [Amount], \"all\", CALCULATE ( [Amount], ALLSELECTED () ) )"
                [ [ "Ann"; 115; 135 ]; [ "Bob"; 20; 135 ] ])

        testCase "a role's filter is not a filter a query sees" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value
            is p "ISFILTERED ( Region[Name] )" false
            is p "ISCROSSFILTERED ( Customer )" false)

        testCase "calculated columns and tables keep the values computed without the roles" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value

            returns p "EVALUATE SELECTCOLUMNS ( Customer, \"n\", Customer[Name], \"all\", Customer[AllSales] )" [
                [ "Ann"; 280 ]
                [ "Bob"; 280 ]
            ]

            is p "MAX ( Totals[Total] )" 280)

        testCase "several roles: what any of them lets through" (fun () ->
            let p = asRoles [ "North"; "South" ] shopPrepared.Value
            is p "SUM ( Sales[Amount] )" 280
            let same = asRoles [ "North"; "Closed" ] shopPrepared.Value
            is same "SUM ( Sales[Amount] )" 135)

        testCase "several roles that filter different tables are refused where the readings differ" (fun () ->
            asRolesError [ "North"; "Bikes" ] shopPrepared.Value
            |> refusedMentioning "together")

        testCase "a role without filters, and an administrator, see everything" (fun () ->
            is (asRoles [ "Everyone" ] shopPrepared.Value) "SUM ( Sales[Amount] )" 280
            is (asRoles [ "Admin" ] shopPrepared.Value) "COUNTROWS ( Region )" 2
            is (asRoles [ "Admin"; "North" ] shopPrepared.Value) "SUM ( Sales[Amount] )" 280
            is (asRoles [ "Everyone"; "North" ] shopPrepared.Value) "SUM ( Sales[Amount] )" 280)

        testCase "FALSE () hides every row" (fun () ->
            let p = asRoles [ "Closed" ] shopPrepared.Value
            // COUNTROWS of an empty table is BLANK.
            is p "COUNTROWS ( Region )" null
            is p "COUNTROWS ( Customer )" null
            is p "SUM ( Sales[Amount] )" null
            is p "COUNTROWS ( Product )" 3)

        testCase "role names are matched without regard to case, and repeats are one role" (fun () ->
            let p = asRoles [ "north"; "NORTH" ] shopPrepared.Value
            is p "SUM ( Sales[Amount] )" 135
            Expect.equal (Engine.roles p) [ "North" ] "the model's name, once")

        testCase "a role the model doesn't have" (fun () ->
            match asRolesError [ "Nope" ] shopPrepared.Value with
            | RoleNotFound("Nope", existing) -> Expect.contains existing "North" "lists the roles"
            | other -> failtestf "%A" other)

        testCase "roles that can't read data" (fun () ->
            match asRolesError [ "Nobody"; "Processor" ] shopPrepared.Value with
            | RoleCannotRead roles -> Expect.equal roles [ "Nobody", "None"; "Processor", "Refresh" ] "roles"
            | other -> failtestf "%A" other

            // "the permissions defined for each role are cumulative" (Learn): Read wins.
            is (asRoles [ "Nobody"; "North" ] shopPrepared.Value) "SUM ( Sales[Amount] )" 135)

        testCase "dynamic security: USERPRINCIPALNAME from the identity" (fun () ->
            let p = Engine.withIdentity (upn "ann@x") shopPrepared.Value |> asRoles [ "Me" ]
            is p "COUNTROWS ( Customer )" 1
            is p "SUM ( Sales[Amount] )" 115

            // Without an identity the filter can't be evaluated.
            asRolesError [ "Me" ] shopPrepared.Value
            |> refusedMentioning "USERPRINCIPALNAME")

        testCase "dynamic security: LOOKUPVALUE in another table, with CUSTOMDATA" (fun () ->
            let id = {
                DaxEngine.Identity.none with
                    CustomData = Some "cy@x"
            }

            let p = Engine.withIdentity id shopPrepared.Value |> asRoles [ "Lookup" ]
            is p "SUM ( Sales[Amount] )" 145)

        testCase "a variable in a filter is the row's value" (fun () ->
            let p = asRoles [ "WithVar" ] shopPrepared.Value
            is p "SUM ( Sales[Amount] )" 245)

        testCase "the identity can't change after the roles are applied" (fun () ->
            let p =
                Engine.withIdentity (upn "ann@x") shopPrepared.Value
                |> asRoles [ "Me" ]
                |> Engine.withIdentity (upn "cy@x")

            match scalarError p "1" with
            | DaxInvalid(message, _) -> Expect.stringContains message "identity" "why"
            | other -> failtestf "%A" other)

        testCase "a filter must be TRUE or FALSE" (fun () ->
            asRolesError [ "Text" ] shopPrepared.Value
            |> refusedMentioning "not TRUE or FALSE")

        testCase "object-level security is refused" (fun () ->
            asRolesError [ "Hidden" ] shopPrepared.Value
            |> refusedMentioning "object-level security"

            asRolesError [ "HiddenColumn" ] shopPrepared.Value |> refusedMentioning "Email")

        testCase "a filter that reads its own table, a secured table, a measure or a function is refused" (fun () ->
            asRolesError [ "OwnTable" ] shopPrepared.Value
            |> refusedMentioning "other than as the row"

            asRolesError [ "ReadsSales" ] shopPrepared.Value |> refusedMentioning "'Sales'"
            asRolesError [ "Measure" ] shopPrepared.Value |> refusedMentioning "measure"
            asRolesError [ "Udf" ] shopPrepared.Value |> refusedMentioning "Twice")

        testCase "USERELATIONSHIP and CROSSFILTER are refused under a role, in the query or a measure" (fun () ->
            let m = {
                shop with
                    Measures =
                        shop.Measures
                        @ [
                            "Sales",
                            "Crossed",
                            "CALCULATE ( [Amount], CROSSFILTER ( Sales[CustomerKey], Customer[CustomerKey], BOTH ) )"
                        ]
            }

            let p = prepareWith m withRoles
            let north = asRoles [ "North" ] p
            notSupported north "EVALUATE { [Crossed] }" "CROSSFILTER"

            notSupported
                north
                "EVALUATE { CALCULATE ( [Amount], USERELATIONSHIP ( Sales[CustomerKey], Customer[CustomerKey] ) ) }"
                "USERELATIONSHIP"

            // A role that filters nothing doesn't refuse it.
            is (asRoles [ "Everyone" ] p) "[Crossed]" 280)

        testCase "DEFINE COLUMN and DEFINE TABLE are refused under a role" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value
            notSupported p "DEFINE COLUMN Sales[x] = 1 EVALUATE { COUNTROWS ( Sales ) }" "DEFINE COLUMN"
            notSupported p "DEFINE TABLE T = { 1 } EVALUATE T" "DEFINE TABLE"
            // The query's measures are fine.
            returns p "DEFINE MEASURE Sales[m] = [Amount] * 2 EVALUATE { [m] }" [ [ 270 ] ])

        testCase "roles are applied once" (fun () ->
            let p = asRoles [ "North" ] shopPrepared.Value

            match Engine.asRoles [ "South" ] p with
            | Error(DaxInvalid _) -> ()
            | other -> failtestf "%A" other)

        testCase "security filtering in both directions reaches the one side" (fun () ->
            let p =
                prepareWith shop (fun db ->
                    withRoles db

                    for r in db.Model.Relationships do
                        match r with
                        | :? SingleColumnRelationship as s when
                            s.FromTable.Name = "Sales" && s.ToTable.Name = "Customer"
                            ->
                            s.SecurityFilteringBehavior <- SecurityFilteringBehavior.BothDirections
                        | _ -> ())

            let big = asRoles [ "BigSales" ] p
            // Sales 1 (Ann) and 3 (Cy) are over 50.
            returns big "EVALUATE VALUES ( Customer[Name] )" [ [ "Ann" ]; [ "Cy" ] ]
            // Region stays: Customer -> Region is one direction.
            is big "COUNTROWS ( Region )" 2
            // And back down: Sales of Ann and Cy, but only the big ones, as the role says.
            is big "SUM ( Sales[Amount] )" 200)

        testCase "rows of the many side with no match on a secured one side are refused" (fun () ->
            let m = {
                shop with
                    Tables =
                        shop.Tables
                        |> List.map (fun t ->
                            if t.Name = "Sales" then
                                {
                                    t with
                                        Rows = t.Rows @ [ [ box 7; box 9; box 1; box 5 ] ]
                                }
                            else
                                t)
            }

            let p = prepareWith m withRoles
            asRolesError [ "WithVar" ] p |> refusedMentioning "no match")

        testCase "a role that would take a table's blank row away is refused" (fun () ->
            // Sale 7 has product 9, which doesn't exist: Product has a blank row. North hides it.
            let m = {
                shop with
                    Tables =
                        shop.Tables
                        |> List.map (fun t ->
                            if t.Name = "Sales" then
                                {
                                    t with
                                        Rows = t.Rows @ [ [ box 7; box 3; box 9; box 5 ] ]
                                }
                            else
                                t)
            }

            let p = prepareWith m withRoles
            is p "COUNTROWS ( VALUES ( Product[Name] ) )" 4
            asRolesError [ "North" ] p |> refusedMentioning "blank row"
            // South keeps the sale: the blank row stays.
            is (asRoles [ "South" ] p) "COUNTROWS ( VALUES ( Product[Name] ) )" 4)

        testCase "an inactive relationship to a secured table whose visible rows point at hidden ones" (fun () ->
            // Sales[Referrer] -> Customer, inactive: sale 2 (Bob, North) was referred by Cy (South).
            let m = {
                shop with
                    Tables =
                        shop.Tables
                        |> List.map (fun t ->
                            if t.Name = "Sales" then
                                {
                                    t with
                                        Columns = t.Columns @ [ "Referrer", DataType.Int64 ]
                                        Rows =
                                            t.Rows |> List.map (fun r -> r @ [ box (if r.[0] = box 2 then 3 else 1) ])
                                }
                            else
                                t)
                    Rels = shop.Rels @ [ inactive (rel ("Sales", "Referrer") ("Customer", "CustomerKey")) ]
            }

            let p = prepareWith m withRoles
            asRolesError [ "North" ] p |> refusedMentioning "blank row"
            // South: Cy's sales were referred by Ann (North), hidden.
            asRolesError [ "South" ] p |> refusedMentioning "blank row")

        testCase "a filter reaching a many-to-many relationship is refused" (fun () ->
            let m = {
                shop with
                    Tables =
                        shop.Tables
                        @ [
                            table "Budget" [ "RegionKey", DataType.Int64; "Amount", DataType.Int64 ] [ [ 1; 5 ] ]
                        ]
                    Rels =
                        shop.Rels
                        @ [
                            {
                                (rel ("Budget", "RegionKey") ("Region", "RegionKey")) with
                                    ToCardinality = RelationshipEndCardinality.Many
                            }
                        ]
            }

            let p = prepareWith m withRoles
            asRolesError [ "North" ] p |> refusedMentioning "many-to-many"
            // A role that doesn't reach it is fine.
            is (asRoles [ "Bikes" ] p) "SUM ( Sales[Amount] )" 200)

        testCase "coverage reports each role with filters, and what refuses it" (fun () ->
            let db, _ = build shop
            withRoles db
            let coverage = Engine.coverage db |> Map.ofList

            let of' (name: string) =
                coverage |> Map.find (ObjectRef.modelLevel ObjectKind.Role name)

            Expect.isEmpty (of' "North") "North"
            Expect.isEmpty (of' "Lookup") "Lookup"
            Expect.isNonEmpty (of' "OwnTable") "OwnTable"
            Expect.isNonEmpty (of' "Hidden" @ of' "HiddenColumn") "OLS"

            Expect.isFalse
                (coverage.ContainsKey(ObjectRef.modelLevel ObjectKind.Role "Everyone"))
                "no filters, no entry")

        testCase "the secured model answers as the same model with the hidden rows deleted" (fun () ->
            // No calculated objects, no unmatched keys: then nothing but the rows differs.
            let plain = {
                shop with
                    Tables =
                        shop.Tables
                        |> List.filter (fun t -> t.Expression.IsNone)
                        |> List.map (fun t -> { t with Calculated = [] })
                    Functions = []
            }

            let keep (table: string) (keepRow: obj list -> bool) (m: TestModel) = {
                m with
                    Tables =
                        m.Tables
                        |> List.map (fun t ->
                            if t.Name = table then
                                {
                                    t with
                                        Rows = t.Rows |> List.filter keepRow
                                }
                            else
                                t)
            }

            let secured =
                prepareWith plain (fun db ->
                    role db "North" ModelPermission.Read [ "Region", "Region[Name] = \"North\"" ]
                    |> ignore)
                |> asRoles [ "North" ]

            let deleted =
                plain
                |> keep "Region" (fun r -> r.[1] = box "North")
                |> keep "Customer" (fun r -> r.[2] = box 1)
                |> keep "Sales" (fun r -> List.contains r.[1] [ box 1; box 2 ])
                |> prepare

            for q in
                [
                    "EVALUATE SUMMARIZECOLUMNS ( Customer[Name], Product[Name], \"a\", [Amount], \"n\", [Customers] )"
                    "EVALUATE ADDCOLUMNS ( VALUES ( Product[Name] ), \"a\", [Amount], \"all\", CALCULATE ( [Amount], ALL ( Sales ) ) )"
                    "EVALUATE ROW ( \"c\", COUNTROWS ( Customer ), \"r\", COUNTROWS ( ALL ( Region ) ), \"d\", DISTINCTCOUNT ( Sales[CustomerKey] ) )"
                    "EVALUATE FILTER ( Customer, CALCULATE ( [Amount] ) > 50 )"
                    "EVALUATE TOPN ( 2, Sales, Sales[Amount], DESC, Sales[Id], ASC )"
                ] do
                Expect.equal (sortedRows secured q) (sortedRows deleted q) q)

        testCase "a filter that doesn't parse, and one that is a DAX error" (fun () ->
            let p =
                prepareWith shop (fun db ->
                    role db "Broken" ModelPermission.Read [ "Region", "Region[Name] = = 1" ]
                    |> ignore

                    role db "Fails" ModelPermission.Read [ "Region", "VALUE ( Region[Name] ) = 1" ]
                    |> ignore)

            match asRolesError [ "Broken" ] p with
            | DaxSyntax(source, _, _, _) -> Expect.stringContains source "Broken" "where"
            | other -> failtestf "%A" other

            match asRolesError [ "Fails" ] p with
            | DaxEvaluation _ -> ()
            | other -> failtestf "%A" other)

        testCase "a filter on a calculated table; a filter on a calculation group is refused" (fun () ->
            let p =
                prepareWith shop (fun db ->
                    role db "NoTotals" ModelPermission.Read [ "Totals", "Totals[Total] > 1000" ]
                    |> ignore

                    let group =
                        new Table(Name = "Calc", CalculationGroup = new CalculationGroup(Precedence = 0))

                    group.Columns.Add(
                        new DataColumn(Name = "Item", DataType = DataType.String, SourceColumn = "Name")
                    )

                    group.CalculationGroup.CalculationItems.Add(
                        new CalculationItem(Name = "Same", Expression = "SELECTEDMEASURE ()")
                    )

                    group.Partitions.Add(new Partition(Name = "Calc", Source = new CalculationGroupSource()))
                    db.Model.Tables.Add group

                    role db "Items" ModelPermission.Read [ "Calc", "Calc[Item] = \"Same\"" ]
                    |> ignore)

            let noTotals = asRoles [ "NoTotals" ] p
            is noTotals "COUNTROWS ( Totals )" null
            is noTotals "SUM ( Sales[Amount] )" 280
            asRolesError [ "Items" ] p |> refusedMentioning "calculation group")

        testCase "a filter reaching a one-to-one relationship, or two paths, is refused" (fun () ->
            let oneToOne = {
                shop with
                    Tables =
                        shop.Tables
                        @ [
                            table "Info" [ "CustomerKey", DataType.Int64; "Vip", DataType.Boolean ] [
                                [ 1; true ]
                                [ 2; false ]
                                [ 3; false ]
                                [ 4; true ]
                            ]
                        ]
                    Rels =
                        shop.Rels
                        @ [
                            {
                                (rel ("Info", "CustomerKey") ("Customer", "CustomerKey")) with
                                    FromCardinality = RelationshipEndCardinality.One
                                    Both = true
                            }
                        ]
            }

            asRolesError [ "North" ] (prepareWith oneToOne withRoles)
            |> refusedMentioning "one-to-one"

            // Region filters Sales through Customer and directly.
            let diamond = {
                shop with
                    Tables =
                        shop.Tables
                        |> List.map (fun t ->
                            if t.Name = "Sales" then
                                {
                                    t with
                                        Columns = t.Columns @ [ "RegionKey", DataType.Int64 ]
                                        Rows = t.Rows |> List.map (fun r -> r @ [ box 1 ])
                                }
                            else
                                t)
                    Rels = shop.Rels @ [ rel ("Sales", "RegionKey") ("Region", "RegionKey") ]
            }

            match asRolesError [ "North" ] (prepareWith diamond withRoles) with
            | DaxNotSupported _ -> ()
            | other -> failtestf "%A" other)

        testCase "a calculation item with USERELATIONSHIP refuses only queries that refer to a measure" (fun () ->
            let p =
                prepareWith shop (fun db ->
                    withRoles db

                    let group =
                        new Table(Name = "Calc", CalculationGroup = new CalculationGroup(Precedence = 0))

                    group.Columns.Add(
                        new DataColumn(Name = "Item", DataType = DataType.String, SourceColumn = "Name")
                    )

                    group.CalculationGroup.CalculationItems.Add(
                        new CalculationItem(
                            Name = "Crossed",
                            Expression =
                                "CALCULATE ( SELECTEDMEASURE (), CROSSFILTER ( Sales[CustomerKey], Customer[CustomerKey], BOTH ) )"
                        )
                    )

                    group.Partitions.Add(new Partition(Name = "Calc", Source = new CalculationGroupSource()))
                    db.Model.Tables.Add group)

            let north = asRoles [ "North" ] p
            is north "SUM ( Sales[Amount] )" 135

            match scalarError north "[Amount]" with
            | DaxNotSupported(what, _) -> Expect.stringContains what "CROSSFILTER" "why"
            | other -> failtestf "%A" other)
    ]
