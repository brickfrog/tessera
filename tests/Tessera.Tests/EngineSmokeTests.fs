module Tessera.Tests.EngineSmokeTests

open Expecto
open Tessera
open Tessera.Tests.EngineHelpers

[<Tests>]
let tests =
    testList "Engine smoke" [
        testCase "scalars" (fun () ->
            let p = starPrepared.Value
            is p "1 + 2" 3
            is p "[Amount]" 625
            is p "[Units]" 17
            is p "CALCULATE ( [Amount], Product[Color] = \"Red\" )" 570
            is p "COUNTROWS ( VALUES ( Product[Color] ) )" 4
            is p "COUNTROWS ( DISTINCT ( Product[Color] ) )" 3
            is p "COUNTROWS ( Product )" 4
            is p "COUNTROWS ( VALUES ( Product ) )" 5)
        testCase "tables" (fun () ->
            let p = starPrepared.Value

            returns p "EVALUATE SUMMARIZECOLUMNS ( Product[Color], \"Amount\", [Amount] )" [
                [ "Red"; 570 ]
                [ "Black"; 15 ]
                [ null; 40 ]
            ]

            returns p "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Country] ), \"A\", [Amount] )" [
                [ "US"; 485 ]
                [ "UK"; 140 ]
            ])
    ]
