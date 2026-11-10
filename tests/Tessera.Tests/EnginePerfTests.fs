module Tessera.Tests.EnginePerfTests

open System
open System.Diagnostics
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// 100,000 sales rows over 1,000 products (20 colours), 2,000 customers and 730 dates.
let private big =
    lazy
        (let products = 1000
         let customers = 2000
         let days = 730
         let first = DateTime(2023, 1, 1)

         prepare {
             Tables = [
                 table "Product" [ "ProductKey", DataType.Int64; "Color", DataType.String ] [
                     for i in 1..products -> [ box i; box $"Colour {i % 20}" ]
                 ]
                 table "Customer" [ "CustomerKey", DataType.Int64; "Country", DataType.String ] [
                     for i in 1..customers -> [ box i; box $"Country {i % 30}" ]
                 ]
                 table "Date" [ "Date", DataType.DateTime; "Year", DataType.Int64 ] [
                     for i in 0 .. days - 1 -> [ box (first.AddDays(float i)); box (first.AddDays(float i).Year) ]
                 ]
                 table "Sales" [
                     "ProductKey", DataType.Int64
                     "CustomerKey", DataType.Int64
                     "OrderDate", DataType.DateTime
                     "Qty", DataType.Int64
                     "Price", DataType.Double
                 ] [
                     for i in 0..99999 ->
                         [
                             box (1 + (i * 7919) % products)
                             box (1 + (i * 104729) % customers)
                             box (first.AddDays(float ((i * 31) % days)))
                             box (1 + i % 5)
                             box (float (1 + i % 97) + 0.5)
                         ]
                 ]
             ]
             Rels = [
                 rel ("Sales", "ProductKey") ("Product", "ProductKey")
                 rel ("Sales", "CustomerKey") ("Customer", "CustomerKey")
                 rel ("Sales", "OrderDate") ("Date", "Date")
             ]
             Measures = [ "Sales", "Amount", "SUMX ( Sales, Sales[Qty] * Sales[Price] )" ]
             Functions = []
         })

let private timed (name: string) (limitMs: int64) (work: unit -> unit) =
    testCase name (fun () ->
        big.Value |> ignore
        work () // once to warm up (JIT, indexes)
        let clock = Stopwatch.StartNew()
        work ()
        clock.Stop()
        printfn "engine perf: %s: %d ms" name clock.ElapsedMilliseconds
        Expect.isLessThan clock.ElapsedMilliseconds limitMs name)

[<Tests>]
let tests =
    testSequenced (
        testList "Engine performance" [
            timed "SUMX over 100k rows" 1000L (fun () -> scalar big.Value "[Amount]" |> ignore)
            timed "CALCULATE with two filters" 1000L (fun () ->
                scalar big.Value "CALCULATE ( [Amount], Product[Color] = \"Colour 5\", 'Date'[Year] = 2024 )"
                |> ignore)
            timed "SUMMARIZECOLUMNS by a dimension column (20 groups)" 1000L (fun () ->
                rows big.Value "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"Amount\", [Amount] )"
                |> ignore)
            timed "SUMMARIZECOLUMNS by a key (2000 groups)" 2000L (fun () ->
                rows big.Value "EVALUATE SUMMARIZECOLUMNS ( Customer[CustomerKey], \"Amount\", [Amount] )"
                |> ignore)
            timed "SUMMARIZECOLUMNS by two dimensions, with a filter" 2000L (fun () ->
                rows
                    big.Value
                    "EVALUATE SUMMARIZECOLUMNS ( Product[Color], Customer[Country], TREATAS ( { 2024 }, 'Date'[Year] ), \"Amount\", [Amount] )"
                |> ignore)
            timed "SUMMARIZE with a measure per group (2000 groups)" 3000L (fun () ->
                rows big.Value "EVALUATE SUMMARIZE ( Sales, Customer[CustomerKey], \"Amount\", [Amount] )"
                |> ignore)
            timed "ADDCOLUMNS over SUMMARIZE (2000 groups)" 2000L (fun () ->
                rows
                    big.Value
                    "EVALUATE ADDCOLUMNS ( SUMMARIZE ( Sales, Customer[CustomerKey] ), \"Amount\", [Amount] )"
                |> ignore)
            timed "a measure per row of the fact table (100k context transitions)" 5000L (fun () ->
                scalar big.Value "SUMX ( Sales, [Amount] )" |> ignore)
            timed "context transition per row of a dimension" 2000L (fun () ->
                scalar big.Value "SUMX ( Product, [Amount] )" |> ignore)
        ]
    )
