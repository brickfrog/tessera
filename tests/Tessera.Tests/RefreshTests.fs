/// The TMSL refresh builder (Tessera.Refresh): the script against the client library's own
/// JsonScripter, TMSL's rules, and matching selectors to a model.
module Tessera.Tests.RefreshTests

open System
open System.Text.Json.Nodes
open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private request (kind: Refresh.Kind) (targets: Refresh.Target list) : Refresh.Request = {
    Database = "Sample"
    Kind = kind
    Targets = targets
    ApplyRefreshPolicy = None
    EffectiveDate = None
    MaxParallelism = None
}

let private sameJson (actual: string) (expected: string) (message: string) =
    if not (JsonNode.DeepEquals(JsonNode.Parse actual, JsonNode.Parse expected)) then
        failtestf "%s\nexpected:\n%s\nactual:\n%s" message expected actual

let private tomType =
    function
    | Refresh.Kind.Full -> RefreshType.Full
    | Refresh.Kind.Automatic -> RefreshType.Automatic
    | Refresh.Kind.DataOnly -> RefreshType.DataOnly
    | Refresh.Kind.Calculate -> RefreshType.Calculate
    | Refresh.Kind.ClearValues -> RefreshType.ClearValues
    | Refresh.Kind.Defragment -> RefreshType.Defragment
    | Refresh.Kind.Add -> RefreshType.Add

/// The model's objects for some targets, for JsonScripter.
let private tomObjects (db: Database) (targets: Refresh.Target list) =
    ResizeArray<NamedMetadataObject>(
        targets
        |> List.map (fun t ->
            match t with
            | Refresh.WholeModel -> failtest "the scripter takes the database itself"
            | Refresh.Table name -> db.Model.Tables.[name] :> NamedMetadataObject
            | Refresh.Partition(table, name) -> db.Model.Tables.[table].Partitions.[name] :> NamedMetadataObject)
    )

let private cannot (r: Result<'T, TessError>) (part: string) =
    match r with
    | Error(CannotRefresh _ as e) -> Expect.stringContains (TessError.message e) part ""
    | Error e -> failtestf "expected cannot-refresh, got %s" (TessError.message e)
    | Ok _ -> failtest "expected cannot-refresh"

let private options = Refresh.Options.defaults

/// The sample model with a second Sales partition, a refresh-policy table and a partition whose
/// name is shared by two tables.
let private model () =
    let db = sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]
    sales.Partitions.Add(new Partition(Name = "Sales 2024", Source = new MPartitionSource(Expression = "x")))
    sales.Partitions.Add(new Partition(Name = "Archive", Source = new MPartitionSource(Expression = "x")))

    db.Model.Tables.["Date"].Partitions.Add(
        new Partition(Name = "Archive", Source = new MPartitionSource(Expression = "x"))
    )

    let orders = new Table(Name = "Orders")
    orders.Columns.Add(new DataColumn(Name = "Id", DataType = DataType.Int64, SourceColumn = "Id"))
    orders.Partitions.Add(new Partition(Name = "Orders-2023", Source = new MPartitionSource(Expression = "x")))

    orders.RefreshPolicy <-
        new BasicRefreshPolicy(
            RollingWindowGranularity = RefreshGranularityType.Year,
            RollingWindowPeriods = 3,
            IncrementalGranularity = RefreshGranularityType.Day,
            IncrementalPeriods = 7,
            SourceExpression = "x"
        )

    db.Model.Tables.Add orders
    db

let private targets (db: Database) (selectors: string list) =
    (Refresh.plan options "Sample" db selectors |> ok).Request.Targets

[<Tests>]
let tests =
    testList "Refresh" [
        testList "tmsl" [
            testCase "every type, for the model, a table and a partition, is what JsonScripter writes"
            <| fun () ->
                let db = sampleDatabase ()

                for kind in Refresh.Kind.all do
                    let scripted (targets: Refresh.Target list) =
                        match Refresh.tmsl (request kind targets) with
                        | Ok script -> Some script
                        | Error(CannotRefresh _) -> None
                        | Error e -> failtestf "%s" (TessError.message e)

                    let name = Refresh.Kind.token kind

                    scripted [ Refresh.WholeModel ]
                    |> Option.iter (fun s -> sameJson s (JsonScripter.ScriptRefresh(db, tomType kind)) name)

                    for targets in
                        [
                            [ Refresh.Table "Sales" ]
                            [ Refresh.Partition("Date", "Date") ]
                            [ Refresh.Table "Sales"; Refresh.Table "Date" ]
                            [ Refresh.Partition("Sales", "Sales"); Refresh.Partition("Date", "Date") ]
                        ] do
                        scripted targets
                        |> Option.iter (fun s ->
                            sameJson s (JsonScripter.ScriptRefresh(tomObjects db targets, tomType kind)) name)

            testCase "the type's spelling is TMSL's"
            <| fun () ->
                Expect.equal
                    (Refresh.Kind.all |> List.map Refresh.Kind.token)
                    [
                        "full"
                        "automatic"
                        "dataOnly"
                        "calculate"
                        "clearValues"
                        "defragment"
                        "add"
                    ]
                    ""

                Expect.equal (Refresh.Kind.tryParse "DATAONLY") (Some Refresh.Kind.DataOnly) ""
                Expect.equal (Refresh.Kind.tryParse " clearvalues ") (Some Refresh.Kind.ClearValues) ""
                Expect.equal (Refresh.Kind.tryParse "indexes") None "not a documented TMSL type"
                Expect.equal (Refresh.Kind.tryParse "") None ""
                Expect.equal (Refresh.Kind.tryParse null) None ""

            testCase "the refresh-policy options are written as JsonScripter writes them"
            <| fun () ->
                let db = sampleDatabase ()
                let table = [ Refresh.Table "Sales" ]
                let basic = request Refresh.Kind.Full table

                sameJson
                    (Refresh.tmsl {
                        basic with
                            ApplyRefreshPolicy = Some false
                     }
                     |> ok)
                    (JsonScripter.ScriptRefresh(tomObjects db table, RefreshType.Full, RefreshPolicyBehavior.Ignore))
                    "ignore"

                sameJson
                    (Refresh.tmsl {
                        basic with
                            ApplyRefreshPolicy = Some true
                     }
                     |> ok)
                    (JsonScripter.ScriptRefresh(tomObjects db table, RefreshType.Full, RefreshPolicyBehavior.Default))
                    "apply"

                // The scripter writes the date as the current culture's short date, so the two only
                // have to agree under the invariant culture. (Its overload for several objects
                // leaves the date out altogether, so one object at a time.)
                let before = Globalization.CultureInfo.CurrentCulture

                try
                    Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

                    for apply in [ None; Some true ] do
                        sameJson
                            (Refresh.tmsl {
                                basic with
                                    ApplyRefreshPolicy = apply
                                    EffectiveDate = Some(DateOnly(2024, 2, 3))
                             }
                             |> ok)
                            (JsonScripter.ScriptRefresh(
                                db.Model.Tables.["Sales"],
                                RefreshType.Full,
                                DateTime(2024, 2, 3)
                            ))
                            "effective date"

                    sameJson
                        (Refresh.tmsl {
                            request Refresh.Kind.DataOnly [ Refresh.WholeModel ] with
                                EffectiveDate = Some(DateOnly(2013, 12, 31))
                         }
                         |> ok)
                        (JsonScripter.ScriptRefresh(db, RefreshType.DataOnly, DateTime(2013, 12, 31)))
                        "the whole model"
                finally
                    Globalization.CultureInfo.CurrentCulture <- before

            testCase "the effective date is month/day/year whatever the culture"
            <| fun () ->
                let before = Globalization.CultureInfo.CurrentCulture

                try
                    Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo "da-DK"

                    let script =
                        Refresh.tmsl {
                            request Refresh.Kind.Full [ Refresh.WholeModel ] with
                                EffectiveDate = Some(DateOnly(2024, 2, 3))
                        }
                        |> ok

                    Expect.equal
                        (JsonNode.Parse(script).["refresh"].["effectiveDate"].GetValue<string>())
                        "02/03/2024"
                        ""
                finally
                    Globalization.CultureInfo.CurrentCulture <- before

            testCase "no refresh-policy option: neither property is in the script"
            <| fun () ->
                let refresh =
                    JsonNode.Parse(Refresh.tmsl (request Refresh.Kind.Full [ Refresh.WholeModel ]) |> ok).["refresh"]

                Expect.isNull refresh.["applyRefreshPolicy"] ""
                Expect.isNull refresh.["effectiveDate"] ""

            testCase "a maximum parallelism wraps the refresh in a sequence"
            <| fun () ->
                let plain = request Refresh.Kind.Full [ Refresh.Table "Sales" ]

                let script =
                    Refresh.tmsl { plain with MaxParallelism = Some 2 } |> ok |> JsonNode.Parse

                let sequence = script.["sequence"]
                Expect.equal (script.AsObject().Count) 1 "only the sequence"
                Expect.equal (sequence.["maxParallelism"].GetValue<int>()) 2 ""
                let operations = sequence.["operations"].AsArray()
                Expect.equal operations.Count 1 ""
                sameJson (operations.[0].ToJsonString()) (Refresh.tmsl plain |> ok) "the same refresh inside"

            testCase "what TMSL doesn't allow is refused"
            <| fun () ->
                let partition = Refresh.Partition("Sales", "Sales")
                cannot (Refresh.tmsl (request Refresh.Kind.Full [])) "name the model"

                cannot
                    (Refresh.tmsl (request Refresh.Kind.Defragment [ Refresh.Table "Date"; partition ]))
                    "partition:Sales/Sales"

                cannot (Refresh.tmsl (request Refresh.Kind.Add [ partition; Refresh.Table "Date" ])) "table:Date"
                cannot (Refresh.tmsl (request Refresh.Kind.Add [ Refresh.WholeModel ])) "append"
                Expect.isOk (Refresh.tmsl (request Refresh.Kind.Add [ partition ])) "add to a partition"
                Expect.isOk (Refresh.tmsl (request Refresh.Kind.Defragment [ Refresh.WholeModel ])) ""

                for r in
                    [
                        {
                            request Refresh.Kind.Full [ partition ] with
                                ApplyRefreshPolicy = Some false
                        }
                        {
                            request Refresh.Kind.Full [ Refresh.Table "Date"; partition ] with
                                ApplyRefreshPolicy = Some true
                        }
                        {
                            request Refresh.Kind.Full [ partition ] with
                                EffectiveDate = Some(DateOnly(2024, 1, 1))
                        }
                    ] do
                    cannot (Refresh.tmsl r) "refresh policy"

                cannot
                    (Refresh.tmsl {
                        request Refresh.Kind.Full [ Refresh.WholeModel ] with
                            ApplyRefreshPolicy = Some false
                            EffectiveDate = Some(DateOnly(2024, 1, 1))
                    })
                    "effective date"

                for n in [ 0; -1 ] do
                    cannot
                        (Refresh.tmsl {
                            request Refresh.Kind.Full [ Refresh.WholeModel ] with
                                MaxParallelism = Some n
                        })
                        "1 or more"

            testProperty "any names come back out of the script unchanged"
            <| fun (NonNull(database: string)) (NonNull(table: string)) (NonNull(partition: string)) ->
                let script =
                    Refresh.tmsl {
                        request Refresh.Kind.Full [ Refresh.Table table; Refresh.Partition(table, partition) ] with
                            Database = database
                    }
                    |> ok
                    |> JsonNode.Parse

                let objects = script.["refresh"].["objects"].AsArray()

                objects.Count = 2
                && objects.[0].["database"].GetValue<string>() = database
                && objects.[0].["table"].GetValue<string>() = table
                && isNull objects.[0].["partition"]
                && objects.[1].["partition"].GetValue<string>() = partition
        ]

        testList "plan" [
            testCase "no selectors, or `model`, is the whole model"
            <| fun () ->
                let db = model ()
                Expect.equal (targets db []) [ Refresh.WholeModel ] ""
                Expect.equal (targets db [ "model" ]) [ Refresh.WholeModel ] ""
                let plan = Refresh.plan options "Deployed Name" db [] |> ok
                Expect.equal plan.Request.Database "Deployed Name" "the server's name, not the model's"
                Expect.equal plan.Request.Kind Refresh.Kind.Automatic "the default type"

                Expect.equal
                    (plan.Covers |> List.map _.Table)
                    [ "Sales"; "Date"; "Time Intelligence"; "Orders" ]
                    "every table"

                Expect.all plan.Covers _.WholeTable ""

                sameJson
                    plan.Tmsl
                    """{ "refresh": { "type": "automatic", "objects": [ { "database": "Deployed Name" } ] } }"""
                    ""

            testCase "tables and partitions, in every spelling, with the model's exact names"
            <| fun () ->
                let db = model ()

                for selector, expected in
                    [
                        "Sales", Refresh.Table "Sales"
                        "sales", Refresh.Table "Sales"
                        "'Time Intelligence'", Refresh.Table "Time Intelligence"
                        "table:DATE", Refresh.Table "Date"
                        // A table and its partition often share a name: the bare name is the table.
                        "Date", Refresh.Table "Date"
                        "partition:Date/Date", Refresh.Partition("Date", "Date")
                        "partition:sales/SALES 2024", Refresh.Partition("Sales", "Sales 2024")
                        "Sales[Sales 2024]", Refresh.Partition("Sales", "Sales 2024")
                        "'Sales'[archive]", Refresh.Partition("Sales", "Archive")
                        "Sales 2024", Refresh.Partition("Sales", "Sales 2024")
                        "orders-2023", Refresh.Partition("Orders", "Orders-2023")
                    ] do
                    Expect.equal (targets db [ selector ]) [ expected ] selector

            testCase "what can't be refreshed, doesn't exist or is ambiguous"
            <| fun () ->
                let db = model ()

                let plan selector =
                    Refresh.plan options "Sample" db [ selector ]

                cannot (plan "[Total]") "measure:Sales/Total"
                cannot (plan "measure:Sales/Total") "a measure isn't refreshed by itself"
                cannot (plan "Sales[Amount]") "column:Sales/Amount"
                cannot (plan "Readers") "role:Readers"

                for missing in
                    [
                        "Nope"
                        "table:Nope"
                        "partition:Sales/Nope"
                        "partition:Nope/Sales"
                        "Nope[Sales]"
                    ] do
                    match plan missing with
                    | Error(ObjectNotFound input) -> Expect.equal input missing ""
                    | other -> failtestf "%s: %A" missing other

                match plan "Archive" with
                | Error(AmbiguousObject("Archive", candidates)) ->
                    Expect.equal candidates [ "partition:Sales/Archive"; "partition:Date/Archive" ] ""
                | other -> failtestf "%A" other

                match plan "'Sales" with
                | Error(InvalidObjectRef _) -> ()
                | other -> failtestf "%A" other

            testCase "repeats, and what a wider target includes, are dropped and said"
            <| fun () ->
                let db = model ()

                let plan =
                    Refresh.plan options "Sample" db [ "partition:Sales/Archive"; "Date"; "Sales"; "date" ]
                    |> ok

                Expect.equal plan.Request.Targets [ Refresh.Table "Date"; Refresh.Table "Sales" ] ""
                Expect.equal plan.Warnings.Length 1 ""
                Expect.stringContains plan.Warnings.[0] "partition:Sales/Archive is part of table:Sales" ""

                let whole = Refresh.plan options "Sample" db [ "Sales"; "model" ] |> ok
                Expect.equal whole.Request.Targets [ Refresh.WholeModel ] ""
                Expect.stringContains whole.Warnings.[0] "table:Sales" ""

            testCase "covers: the partitions named, per table, in model order"
            <| fun () ->
                let db = model ()

                let plan =
                    Refresh.plan options "Sample" db [ "Orders"; "partition:Sales/Archive"; "partition:Sales/Sales" ]
                    |> ok

                Expect.equal
                    plan.Covers
                    [
                        {
                            Table = "Sales"
                            Partitions = [ "Sales"; "Archive" ]
                            WholeTable = false
                            HasRefreshPolicy = false
                        }
                        {
                            Table = "Orders"
                            Partitions = [ "Orders-2023" ]
                            WholeTable = true
                            HasRefreshPolicy = true
                        }
                    ]
                    ""

            testCase "refresh policies: what is said, and when the options mean nothing"
            <| fun () ->
                let db = model ()

                let warnings (o: Refresh.Options) selectors =
                    (Refresh.plan o "Sample" db selectors |> ok).Warnings

                let one (w: string list) (part: string) =
                    Expect.equal w.Length 1 (String.concat "\n" w)
                    Expect.stringContains w.[0] part ""

                one (warnings options [ "Orders" ]) "refresh policy of 'Orders' is applied"
                one (warnings options []) "refresh policy of 'Orders' is applied"
                Expect.isEmpty (warnings options [ "Sales" ]) "no policy in scope"

                Expect.isEmpty
                    (warnings options [ "partition:Orders/Orders-2023" ])
                    "one partition: the policy isn't applied"

                let ignoring = {
                    options with
                        ApplyRefreshPolicy = Some false
                }

                Expect.isEmpty (warnings ignoring [ "Orders" ]) "ignored on purpose"
                one (warnings ignoring [ "Sales" ]) "no table in this refresh has a refresh policy"

                one
                    (warnings
                        {
                            options with
                                Kind = Refresh.Kind.Calculate
                                EffectiveDate = Some(DateOnly(2024, 1, 1))
                        }
                        [ "Orders" ])
                    "makes no difference to a 'calculate' refresh"

                Expect.isEmpty
                    (warnings
                        {
                            options with
                                Kind = Refresh.Kind.Calculate
                        }
                        [ "Orders" ])
                    "calculate applies no policy"

                cannot
                    (Refresh.plan
                        {
                            options with
                                ApplyRefreshPolicy = Some true
                        }
                        "Sample"
                        db
                        [ "partition:Orders/Orders-2023" ])
                    "refresh policy"

            testCase "add and clearValues say what they do to the data"
            <| fun () ->
                let db = model ()

                let warnings kind selectors =
                    (Refresh.plan { options with Kind = kind } "Sample" db selectors |> ok).Warnings

                Expect.stringContains (warnings Refresh.Kind.Add [ "partition:Sales/Archive" ]).[0] "duplicated" ""
                Expect.stringContains (warnings Refresh.Kind.ClearValues [ "Sales" ]).[0] "removes the data" ""
                cannot (Refresh.plan { options with Kind = Refresh.Kind.Add } "Sample" db [ "Sales" ]) "table:Sales"
                cannot (Refresh.plan { options with Kind = Refresh.Kind.Add } "Sample" db []) "model"

                cannot
                    (Refresh.plan
                        {
                            options with
                                Kind = Refresh.Kind.Defragment
                        }
                        "Sample"
                        db
                        [ "partition:Sales/Archive" ])
                    "defragment"

            testCase "the plan's script is the request's"
            <| fun () ->
                let db = model ()

                let plan =
                    Refresh.plan
                        {
                            Kind = Refresh.Kind.Full
                            ApplyRefreshPolicy = None
                            EffectiveDate = Some(DateOnly(2024, 6, 30))
                            MaxParallelism = Some 4
                        }
                        "Sample"
                        db
                        [ "Orders"; "Sales" ]
                    |> ok

                Expect.equal plan.Tmsl (Refresh.tmsl plan.Request |> ok) ""

                sameJson
                    plan.Tmsl
                    """{ "sequence": { "maxParallelism": 4, "operations": [ { "refresh": {
                         "type": "full", "applyRefreshPolicy": true, "effectiveDate": "06/30/2024",
                         "objects": [ { "database": "Sample", "table": "Orders" },
                                      { "database": "Sample", "table": "Sales" } ] } } ] } }"""
                    ""
        ]
    ]
