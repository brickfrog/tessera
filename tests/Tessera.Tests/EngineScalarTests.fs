/// Scalar semantics of the local DAX engine: BLANK, operators, logical functions, errors,
/// aggregations. Each expected value is worked out by hand from the `star` model in EngineHelpers
/// (its doc comment lists every row), with the source of the rule.
module Tessera.Tests.EngineScalarTests

open System
open Expecto
open Tessera
open Tessera.Tests.EngineHelpers

let private p () = starPrepared.Value

let private isError (code: string) (dax: string) =
    Expect.equal (TessError.code (scalarError (p ()) dax)) code dax

[<Tests>]
let tests =
    testList "Engine scalars" [
        testList "BLANK" [
            // Microsoft Learn "DAX operators" / SQLBI "Handling BLANK in DAX": BLANK + 5 = 5,
            // BLANK * 5 = BLANK, 5 / BLANK = Infinity, BLANK / 5 = BLANK.
            testCase "in arithmetic" (fun () ->
                is (p ()) "BLANK () + 5" 5
                is (p ()) "5 - BLANK ()" 5
                is (p ()) "BLANK () + BLANK ()" null
                is (p ()) "BLANK () * 5" null
                is (p ()) "BLANK () / 5" null
                is (p ()) "5 / BLANK ()" Double.PositiveInfinity
                is (p ()) "5 / 0" Double.PositiveInfinity
                is (p ()) "-5 / 0" Double.NegativeInfinity)
            // "BLANK = 0" is TRUE, "BLANK == 0" is FALSE (Microsoft Learn, strict equality).
            testCase "in comparisons" (fun () ->
                is (p ()) "BLANK () = 0" true
                is (p ()) "BLANK () = \"\"" true
                is (p ()) "BLANK () = FALSE" true
                is (p ()) "BLANK () == 0" false
                is (p ()) "BLANK () == BLANK ()" true
                is (p ()) "BLANK () < 1" true
                is (p ()) "BLANK () < -1" false)
            // "ISBLANK ( <expression> ) -- same result as <expression> == BLANK()" (dax.guide/isblank).
            testCase "ISBLANK tells BLANK from zero and the empty text" (fun () ->
                is (p ()) "ISBLANK ( BLANK () )" true
                is (p ()) "ISBLANK ( 0 )" false
                is (p ()) "ISBLANK ( \"\" )" false)
            testCase "as text and as a condition" (fun () ->
                is (p ()) "BLANK () & \"x\"" "x"
                // "With two arguments it works as the CONCATENATE function", for which "If both
                // arguments are BLANK, the result is BLANK" (dax.guide, & and CONCATENATE).
                is (p ()) "BLANK () & BLANK ()" null
                is (p ()) "ISBLANK ( BLANK () & \"\" )" false
                is (p ()) "IF ( BLANK (), 1, 2 )" 2
                is (p ()) "NOT BLANK ()" true)
        ]

        testList "operators" [
            testCase "precedence and types" (fun () ->
                is (p ()) "1 + 2 * 3" 7
                is (p ()) "( 1 + 2 ) * 3" 9
                is (p ()) "-2 ^ 2" -4.0
                is (p ()) "7 / 2" 3.5
                is (p ()) "6 / 3" 2.0
                is (p ()) "1 + 2.5" 3.5
                is (p ()) "2 & 3" "23"
                is (p ()) "\"a\" & \"b\" & 1" "ab1")
            // Text compares without case, by the collation.
            testCase "text comparison ignores case" (fun () ->
                is (p ()) "\"red\" = \"RED\"" true
                is (p ()) "\"a\" < \"B\"" true)
            // "DAX comparison operations do not support comparing values of type Text with values
            // of type Integer" (the engine's own message).
            testCase "comparing text with a number is an error" (fun () -> isError "dax-error" "\"1\" = 1")
            // "Unlike the = operator, the IN operator and the CONTAINSROW function perform strict
            // comparison ... The comparison of text is not case-sensitive" (dax.guide/containsrow).
            testCase "IN is strict" (fun () ->
                is (p ()) "2 IN { 1, 2 }" true
                is (p ()) "3 IN { 1, 2 }" false
                is (p ()) "\"red\" IN { \"RED\" }" true
                is (p ()) "BLANK () IN { 0 }" false
                is (p ()) "BLANK () IN { BLANK () }" true
                is (p ()) "( 1, \"x\" ) IN { ( 1, \"x\" ), ( 2, \"y\" ) }" true
                is (p ()) "( 1, \"y\" ) IN { ( 1, \"x\" ), ( 2, \"y\" ) }" false
                is (p ()) "NOT 3 IN { 1, 2 }" true
                is (p ()) "CONTAINSROW ( { 1, 2 }, 2 )" true
                is (p ()) "CONTAINSROW ( { ( 1, \"x\" ) }, 1, \"y\" )" false)
            // 499 operators in one chain: evaluated in a loop, not by recursion.
            testCase "a long chain of operators" (fun () ->
                let chain = String.Join(" + ", Array.create 499 "1")
                is (p ()) chain 499)
        ]

        testList "conversions" [
            // "You can use a string in a numeric expression and the string is automatically
            // converted into a corresponding number" (SQLBI, numeric data type conversions).
            testCase "text that is a number is a number in arithmetic" (fun () ->
                is (p ()) "\"22\" + 1" 23
                is (p ()) "\"1.5\" * 2" 3.0)
            testCase "a date plus a number is a date" (fun () ->
                is (p ()) "dt\"2024-01-01\" + 1" (DateTime(2024, 1, 2))
                is (p ()) "dt\"2024-01-03\" - dt\"2024-01-01\"" (DateTime(1900, 1, 1)))
            // What depends on the model's culture, or what the sources disagree on, is refused.
            testCase "culture-dependent conversions are refused" (fun () ->
                isError "not-supported" "dt\"2024-01-31\" & \"\""
                isError "not-supported" "\"1/31/2024\" + 1"
                isError "not-supported" "TRUE & \"\""
                isError "not-supported" "dt\"2024-01-31\" * 2"
                isError "not-supported" "CONCATENATEX ( 'Date', 'Date'[Date], \",\", 'Date'[Date] )")
        ]

        testList "logical" [
            // "The value that is returned if the logical test is FALSE. If omitted, a BLANK is
            // returned" (dax.guide/if).
            testCase "IF" (fun () ->
                is (p ()) "IF ( 1 < 2, \"yes\", \"no\" )" "yes"
                is (p ()) "IF ( 1 > 2, \"yes\" )" null)
            // "the function attempts to return a single data type if both ... are of numeric data
            // types ... will implicitly convert data types to accommodate both values"
            // (dax.guide/if.eager; the same remark on Microsoft Learn's IF page).
            testCase "IF with a whole and a decimal branch is a decimal" (fun () ->
                is (p ()) "IF ( TRUE, 1, 2.5 )" 1.0
                is (p ()) "IF ( FALSE, 1, 2.5 )" 2.5)
            // Only the branch taken is evaluated.
            testCase "IF evaluates one branch" (fun () ->
                is (p ()) "IF ( FALSE, ERROR ( \"no\" ), 1 )" 1
                is (p ()) "IF ( TRUE, 1, ERROR ( \"no\" ) )" 1)
            // "SWITCH compares Expression with each Value like the == operator ... BLANK does not
            // match 0 or an empty string" (dax.guide/switch).
            testCase "SWITCH" (fun () ->
                is (p ()) "SWITCH ( 2, 1, \"one\", 2, \"two\", \"other\" )" "two"
                is (p ()) "SWITCH ( 3, 1, \"one\", 2, \"two\", \"other\" )" "other"
                is (p ()) "SWITCH ( 3, 1, \"one\", 2, \"two\" )" null
                is (p ()) "SWITCH ( BLANK (), 0, \"zero\", \"other\" )" "other"
                is (p ()) "SWITCH ( TRUE (), 1 > 2, \"a\", 2 > 1, \"b\", \"c\" )" "b"
                is (p ()) "SWITCH ( 1, 1, \"hit\", ERROR ( \"not evaluated\" ) )" "hit")
            testCase "AND, OR, && and || stop at the side that decides" (fun () ->
                is (p ()) "FALSE && ERROR ( \"no\" )" false
                is (p ()) "TRUE || ERROR ( \"no\" )" true
                is (p ()) "AND ( FALSE, ERROR ( \"no\" ) )" false
                is (p ()) "OR ( TRUE, ERROR ( \"no\" ) )" true
                is (p ()) "TRUE && 1" true
                is (p ()) "BLANK () || BLANK ()" false)
            // "COALESCE returns the first argument that is not BLANK ... An empty string and 0 are
            // not BLANK" (dax.guide/coalesce).
            testCase "COALESCE" (fun () ->
                is (p ()) "COALESCE ( BLANK (), 0 )" 0
                is (p ()) "COALESCE ( BLANK (), \"\", \"x\" )" ""
                is (p ()) "COALESCE ( BLANK (), BLANK () )" null
                is (p ()) "COALESCE ( 1, ERROR ( \"not evaluated\" ) )" 1
                // The result takes the common numeric type: a whole number next to a decimal.
                is (p ()) "COALESCE ( BLANK (), 1, 2.5 )" 1.0)
            // "Safe Divide ... AlternateResult in case there is a division by zero"; "A single
            // decimal value" (dax.guide/divide).
            testCase "DIVIDE" (fun () ->
                is (p ()) "DIVIDE ( 6, 4 )" 1.5
                is (p ()) "DIVIDE ( 6, 3 )" 2.0
                is (p ()) "DIVIDE ( 1, 0 )" null
                is (p ()) "DIVIDE ( 1, BLANK () )" null
                is (p ()) "DIVIDE ( 1, 0, 9 )" 9.0
                is (p ()) "DIVIDE ( BLANK (), 5 )" null)
        ]

        testList "errors" [
            // "IFERROR ( A, B ) corresponds to IF ( ISERROR ( A ), B, A )" (dax.guide/iferror).
            testCase "ERROR, IFERROR and ISERROR" (fun () ->
                is (p ()) "IFERROR ( ERROR ( \"x\" ), 7 )" 7
                is (p ()) "IFERROR ( 5, 7 )" 5
                is (p ()) "ISERROR ( ERROR ( \"x\" ) )" true
                is (p ()) "ISERROR ( 1 )" false
                is (p ()) "IFERROR ( 1 + \"a\", -1 )" -1)
            testCase "an ERROR is reported with its text and position" (fun () ->
                match scalarError (p ()) "1 + ERROR ( \"boom\" )" with
                | DaxEvaluation(text, at) ->
                    Expect.equal text "boom" "the text"
                    Expect.equal at "expression 1:5" "where"
                | other -> failtestf "%A" other)
            testCase "text that isn't a number in arithmetic" (fun () -> isError "dax-error" "1 + \"a\"")
            // Something the engine doesn't implement is never swallowed by IFERROR.
            testCase "IFERROR does not hide a not-supported" (fun () ->
                isError "not-supported" "IFERROR ( FORMAT ( 1, \"0\" ), 2 )")
            testCase "a table of several rows where a value is needed" (fun () ->
                isError "dax-error" "VALUES ( Customer[Name] ) & \"\"")
            testCase "a column outside a row context" (fun () -> isError "dax-invalid" "Sales[Qty] + 1")
            testCase "an unknown name" (fun () ->
                isError "dax-invalid" "[No Such Measure]"
                isError "dax-invalid" "SUM ( Nope[x] )")
            testCase "a syntax error has its line and column" (fun () ->
                match scalarError (p ()) "1 +\n  * 2" with
                | DaxSyntax("expression", 2, 3, _) -> ()
                | other -> failtestf "%A" other)
        ]

        testList "math" [
            // dax.guide/round: decimal places, the nearest integer, left of the decimal point.
            testCase "ROUND" (fun () ->
                is (p ()) "ROUND ( 2.675, 2 )" 2.68
                // 1.005 * 100 as a float is 100.49999999999999: the number as written rounds to
                // 1.01, the scaled float to 1.00, and no page says which is rounded. Refused.
                isError "not-supported" "ROUND ( 1.005, 2 )"
                is (p ()) "ROUND ( 2.625, 2 )" 2.63
                is (p ()) "ROUND ( 2.5, 0 )" 3.0
                // "rounds 2.15 up, to one decimal place. The expected result is 2.2" (Microsoft Learn).
                is (p ()) "ROUND ( 2.15, 1 )" 2.2
                is (p ()) "ROUND ( -2.4, 0 )" -2.0
                is (p ()) "ROUND ( 21.5, -1 )" 20.0
                // "Ties are broken by rounding half away from zero": ROUND(-1.15, 1) is -1.2.
                is (p ()) "ROUND ( -2.5, 0 )" -3.0
                is (p ()) "ROUND ( -1.15, 1 )" -1.2
                is (p ()) "ROUND ( 1234.5, -2 )" 1200.0)
            // "INT rounds <Number> towards −∞" (dax.guide/int).
            testCase "INT" (fun () ->
                is (p ()) "INT ( 1.9 )" 1
                is (p ()) "INT ( -1.5 )" -2
                is (p ()) "INT ( 7 )" 7)
            testCase "MOD, ABS, CONCATENATE" (fun () ->
                is (p ()) "MOD ( 7, 3 )" 1
                // The sign of the divisor: -7 - 3 * INT ( -7 / 3 ) = -7 - 3 * -3 = 2.
                is (p ()) "MOD ( -7, 3 )" 2
                is (p ()) "ABS ( -3 )" 3
                is (p ()) "ABS ( -2.5 )" 2.5
                is (p ()) "CONCATENATE ( \"a\", 1 )" "a1"
                is (p ()) "CONCATENATE ( BLANK (), BLANK () )" null)
        ]

        testList "aggregations" [
            // Qty: 2 1 3 1 4 5 1 = 17. Price: 100 100 20 15 10 20 110.
            testCase "over a column" (fun () ->
                is (p ()) "SUM ( Sales[Qty] )" 17
                is (p ()) "AVERAGE ( Sales[Qty] )" (17.0 / 7.0)
                is (p ()) "MIN ( Sales[Price] )" 10
                is (p ()) "MAX ( Sales[Price] )" 110
                is (p ()) "COUNT ( Sales[Id] )" 7
                is (p ()) "COUNTA ( Customer[Name] )" 3
                is (p ()) "COUNTROWS ( Sales )" 7
                // Product keys 1, 2, 3 and 9.
                is (p ()) "DISTINCTCOUNT ( Sales[ProductKey] )" 4
                is (p ()) "DISTINCTCOUNTNOBLANK ( Sales[ProductKey] )" 4
                is (p ()) "MIN ( Customer[Name] )" "Ann"
                is (p ()) "MAX ( Customer[Name] )" "Cy"
                is (p ()) "MAX ( 'Date'[Date] )" (DateTime(2024, 2, 1)))
            // "If the table has no rows, COUNTROWS returns BLANK, not 0" (dax.guide/countrows);
            // "The result is blank in case there are no rows" (dax.guide/sum); "The function never
            // returns 0" (dax.guide/countblank).
            testCase "nothing to aggregate is BLANK, not zero" (fun () ->
                is (p ()) "CALCULATE ( SUM ( Sales[Qty] ), Sales[Qty] > 100 )" null
                is (p ()) "CALCULATE ( COUNTROWS ( Sales ), Sales[Qty] > 100 )" null
                is (p ()) "CALCULATE ( COUNT ( Sales[Id] ), Sales[Qty] > 100 )" null
                is (p ()) "CALCULATE ( DISTINCTCOUNT ( Sales[Id] ), Sales[Qty] > 100 )" null
                is (p ()) "CALCULATE ( MAX ( Sales[Qty] ), Sales[Qty] > 100 )" null
                is (p ()) "CALCULATE ( AVERAGE ( Sales[Qty] ), Sales[Qty] > 100 )" null
                is (p ()) "COUNTBLANK ( Sales[Qty] )" null)
            // "MIN ( 2, BLANK() )" returns BLANK and "MINX ( { 2, BLANK() }, [Value] )" returns 2
            // (dax.guide/min).
            testCase "MIN and MAX of two values count BLANK, MINX skips it" (fun () ->
                is (p ()) "MIN ( 2, BLANK () )" null
                is (p ()) "MAX ( 2, BLANK () )" 2
                is (p ()) "MIN ( 2, 3 )" 2
                is (p ()) "MAX ( -2, BLANK () )" null
                is (p ()) "MINX ( { 2, BLANK () }, [Value] )" 2)
            // Amounts per row: 200 100 60 15 40 100 110.
            testCase "iterators" (fun () ->
                is (p ()) "SUMX ( Sales, Sales[Qty] * Sales[Price] )" 625
                is (p ()) "MAXX ( Sales, Sales[Qty] * Sales[Price] )" 200
                is (p ()) "MINX ( Sales, Sales[Qty] * Sales[Price] )" 15
                is (p ()) "AVERAGEX ( Sales, Sales[Qty] * Sales[Price] )" (625.0 / 7.0)
                // Qty above 2: 3, 4 and 5.
                is (p ()) "COUNTX ( Sales, IF ( Sales[Qty] > 2, 1 ) )" 3
                is (p ()) "COUNTAX ( Sales, IF ( Sales[Qty] > 2, TRUE ) )" 3
                is (p ()) "PRODUCTX ( { 2, 3, 4 }, [Value] )" 24.0
                is (p ()) "SUMX ( FILTER ( Sales, FALSE ), Sales[Qty] )" null)
            // "COUNTX ( Customer, \"\" ) returns the number of rows in Customer, and COUNTX (
            // Customer, BLANK () ) returns BLANK" (dax.guide/countx).
            testCase "COUNTX counts empty text, not BLANK" (fun () ->
                is (p ()) "COUNTX ( Customer, \"\" )" 3
                is (p ()) "COUNTX ( Customer, BLANK () )" null)
            // "A BLANK value of Expression becomes an empty string, but CONCATENATEX still adds
            // the delimiter"; "The result is BLANK when Table has no rows" (dax.guide/concatenatex).
            testCase "CONCATENATEX" (fun () ->
                is
                    (p ())
                    "CONCATENATEX ( VALUES ( Customer[Name] ), Customer[Name], \", \", Customer[Name], ASC )"
                    "Ann, Bob, Cy"

                is
                    (p ())
                    "CONCATENATEX ( VALUES ( Customer[Name] ), Customer[Name], \"|\", Customer[Name], DESC )"
                    "Cy|Bob|Ann"

                is (p ()) "CONCATENATEX ( FILTER ( Customer, FALSE ), Customer[Name], \",\", Customer[Name] )" null
                is (p ()) "CONCATENATEX ( { 1, 2 }, IF ( [Value] = 1, \"a\" ), \"-\", [Value] )" "a-")
            testCase "CONCATENATEX without an order is refused: the engine doesn't define one" (fun () ->
                isError "not-supported" "CONCATENATEX ( VALUES ( Customer[Name] ), Customer[Name] )")
            // Amount per product name: Bike 410, Helmet 160, Lock 15, Pump BLANK, and the blank row
            // 40. "If expression or value evaluates to BLANK it is treated as a 0" (Microsoft Learn,
            // RANKX); order DESC and ties SKIP by default.
            testCase "RANKX" (fun () ->
                let rank (name: string) (rest: string) =
                    $"CALCULATE ( RANKX ( ALL ( Product[Name] ), [Amount]{rest} ), Product[Name] = \"{name}\" )"

                is (p ()) (rank "Bike" "") 1
                is (p ()) (rank "Helmet" "") 2
                // Above Lock's 15: 410, 160 and the blank row's 40.
                is (p ()) (rank "Lock" "") 4
                // Pump has no sales: 0. Above it: 410, 160, 40, 15.
                is (p ()) (rank "Pump" "") 5
                // Ascending: below Lock's 15 is only Pump's 0.
                is (p ()) (rank "Lock" ", , ASC") 2
                // A value given explicitly: 100 would be third, after 410 and 160.
                is (p ()) "RANKX ( ALL ( Product[Name] ), [Amount], 100 )" 3
                // Ties: { 10, 10, 5 }: 5 is third with SKIP, second with DENSE.
                is (p ()) "RANKX ( { 10, 10, 5 }, [Value], 5 )" 3
                is (p ()) "RANKX ( { 10, 10, 5 }, [Value], 5, , DENSE )" 2)
        ]

        testList "decimal sums" [
            // "SUM ( table[column] ) corresponds to SUMX ( table, table[column] )" (dax.guide/sum):
            // also in the last digit, where a floating-point sum depends on its order. Ten times
            // 0.1 added one by one is 0.9999999999999999, not 1.
            testCase "SUM adds in row order, as SUMX does" (fun () ->
                let m =
                    prepare {
                        Tables = [
                            table "T" [ "V", Microsoft.AnalysisServices.Tabular.DataType.Double ] [
                                for i in 1..12 -> [ box (if i > 10 then 0.7 else 0.1) ]
                            ]
                        ]
                        Rels = []
                        Measures = []
                        Functions = []
                    }

                let expected = List.replicate 10 0.1 @ [ 0.7; 0.7 ] |> List.fold (+) 0.0
                is m "SUM ( T[V] )" expected
                is m "SUMX ( T, T[V] )" expected
                is m "SUM ( T[V] ) = SUMX ( T, T[V] )" true
                is m "CALCULATE ( SUM ( T[V] ), T[V] < 0.5 )" 0.9999999999999999
                is m "AVERAGE ( T[V] ) = AVERAGEX ( T, T[V] )" true)
        ]

        testList "evaluateScalar" [
            testCase "returns typed values" (fun () ->
                Expect.equal (scalar (p ()) "1") (QueryValue.Int64 1L) "integer"
                Expect.equal (scalar (p ()) "1.5") (QueryValue.Double 1.5) "decimal"
                Expect.equal (scalar (p ()) "\"a\"") (QueryValue.String "a") "text"
                Expect.equal (scalar (p ()) "TRUE") (QueryValue.Boolean true) "boolean"
                Expect.equal (scalar (p ()) "dt\"2024-01-31\"") (QueryValue.DateTime(DateTime(2024, 1, 31))) "date"
                Expect.equal (scalar (p ()) "BLANK ()") QueryValue.Blank "blank")
        ]
    ]
