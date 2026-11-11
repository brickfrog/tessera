/// The conversion, math, statistical and information functions of the local DAX engine. Each
/// expected value is one a source publishes (quoted) or follows from a rule a source states.
module Tessera.Tests.EngineMathTests

open System
open System.Globalization
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// N: the sample dax.guide uses for its statistical examples, { 2, 4, 4, 4, 5, 5, 7, 9 }, as whole
/// numbers (v), decimal numbers (d) and fixed decimal numbers (c), with a flag (b: TRUE for the
/// first three rows) and a text (s). B: the same with a BLANK row first. Empty: no rows.
let private model = {
    Tables = [
        table "N" [
            "v", DataType.Int64
            "d", DataType.Double
            "c", DataType.Decimal
            "b", DataType.Boolean
            "s", DataType.String
        ] [
            for i, x in List.indexed [ 2; 4; 4; 4; 5; 5; 7; 9 ] ->
                [ box x; box (float x); box (decimal x); box (i < 3); box (string x) ]
        ]
        table "B" [ "v", DataType.Int64; "d", DataType.Double ] [
            yield [ null; null ]
            for x in [ 2; 4; 4; 4; 5; 5; 7; 9 ] -> [ box x; box (float x) ]
        ]
        table "Empty" [ "v", DataType.Int64; "d", DataType.Double ] []
    ]
    Rels = []
    Measures = []
    Functions = []
}

let private prepared = lazy (prepare model)
let private p () = prepared.Value

let private number (v: QueryValue) : float =
    match v with
    | QueryValue.Double d -> d
    | QueryValue.Int64 i -> float i
    | QueryValue.Decimal d -> float d
    | other -> failtestf "not a number: %A" other

let private code (dax: string) =
    match Engine.evaluateScalar (p ()) dax with
    | Ok v -> failtestf "expected an error, got %A\n%s" v dax
    | Error e -> TessError.code e

let private refused (dax: string) =
    Expect.equal (code dax) "not-supported" dax

/// A DAX error: the kind IFERROR catches.
let private daxError (dax: string) =
    Expect.notEqual (code dax) "not-supported" dax
    Expect.equal (number (scalar (p ()) $"IFERROR ( {dax}, -1 )")) -1.0 dax

let private close (dax: string) (expected: float) =
    match scalar (p ()) dax with
    | QueryValue.Double d -> Expect.floatClose Accuracy.high d expected dax
    | other -> failtestf "expected a decimal number, got %A\n%s" other dax

let private inv = CultureInfo.InvariantCulture

/// A decimal literal: always with a decimal point.
let private lit (x: float) = x.ToString("0.0###############", inv)

let private tryScalar (dax: string) : QueryValue option =
    match Engine.evaluateScalar (p ()) dax with
    | Ok v -> Some v
    | Error e when TessError.code e = "not-supported" -> None
    | Error e -> failtestf "%s\n%s" (TessError.message e) dax

let private config = {
    FsCheckConfig.defaultConfig with
        maxTest = 200
}

[<Tests>]
let tests =
    testList "Engine math" [
        testList "CONVERT, CURRENCY, VALUE" [
            // dax.guide/convert: "A value that ends in .5 is rounded away from zero. For example,
            // CONVERT ( 2.5, INTEGER ) returns 3 and CONVERT ( -2.5, INTEGER ) returns -3 ...
            // CONVERT ( 3.7, INTEGER ) returns 4".
            testCase "to INTEGER" (fun () ->
                is (p ()) "CONVERT ( 2.5, INTEGER )" 3
                is (p ()) "CONVERT ( -2.5, INTEGER )" -3
                is (p ()) "CONVERT ( 3.7, INTEGER )" 4
                is (p ()) "CONVERT ( \"12\", INT64 )" 12
                is (p ()) "CONVERT ( TRUE, INTEGER )" 1
                // Microsoft Learn, CONVERT: "CONVERT ( DATE ( 1900, 1, 1 ), INTEGER )" is 2.
                is (p ()) "CONVERT ( dt\"1900-01-01\", INTEGER )" 2
                // "CONVERT ( "abc", INTEGER ) returns an error. CONVERT ( 1E20, INTEGER ) also
                // returns an error".
                daxError "CONVERT ( \"abc\", INTEGER )"
                daxError "CONVERT ( 1E20, INTEGER )")
            // A float within a rounding error of a half: the binary value and the number as
            // written round differently, and no page says which is rounded.
            testCase "a number a rounding error away from a half is refused" (fun () ->
                refused "CONVERT ( 0.1 + 0.2 + 0.2, INTEGER ) + CONVERT ( 2.4999999999999996, INTEGER )"
                // 2.00005 as a float is just below the tie; times 10000 it is 20000.5 exactly.
                refused "CURRENCY ( 2.00005 )"
                refused "CONVERT ( 0.00015, CURRENCY )"
                is (p ()) "CURRENCY ( 1.00015 )" 1.0002m)
            // "When Expression is BLANK, CONVERT returns BLANK. The only exception is BOOLEAN".
            testCase "BLANK" (fun () ->
                is (p ()) "CONVERT ( BLANK (), INTEGER )" null
                is (p ()) "CONVERT ( BLANK (), STRING )" null
                is (p ()) "CONVERT ( BLANK (), BOOLEAN )" false
                is (p ()) "CURRENCY ( BLANK () )" null)
            // dax.guide/convert: "CONVERT ( 1.123456, CURRENCY ) returns 1.1235"; Microsoft
            // Learn, CURRENCY: 3.6666666666666 gives 3.6667 and 3.0123456789 gives 3.0123, TRUE
            // gives 1; dax.guide/currency: CURRENCY ( "1950.00" ) and CURRENCY ( 190.29876 ).
            testCase "to CURRENCY" (fun () ->
                is (p ()) "CONVERT ( 1.123456, CURRENCY )" 1.1235m
                is (p ()) "CURRENCY ( 3.6666666666666 )" 3.6667m
                is (p ()) "CURRENCY ( 3.0123456789 )" 3.0123m
                is (p ()) "CURRENCY ( 0.03125 )" 0.0313m
                is (p ()) "CURRENCY ( -0.03125 )" -0.0313m
                is (p ()) "CURRENCY ( TRUE )" 1m
                is (p ()) "CURRENCY ( \"1950.00\" )" 1950m
                is (p ()) "CURRENCY ( 190.29876 )" 190.2988m
                is (p ()) "CONVERT ( 10, DECIMAL )" 10m
                daxError "CURRENCY ( \"abc\" )"
                daxError "CURRENCY ( 1E20 )")
            // dax.guide/convert: CONVERT ( "12/25/1966", DATETIME ) is 1966-12-25, CONVERT ( 12345,
            // DATETIME ) is 1933-10-18, CONVERT ( FALSE, DATETIME ) is 1899-12-30.
            testCase "to DATETIME" (fun () ->
                is (p ()) "CONVERT ( \"12/25/1966\", DATETIME )" (DateTime(1966, 12, 25))
                is (p ()) "CONVERT ( \"25/12/1966\", DATETIME )" (DateTime(1966, 12, 25))
                is (p ()) "CONVERT ( \"1966-12-25\", DATETIME )" (DateTime(1966, 12, 25))
                is (p ()) "CONVERT ( 12345, DATETIME )" (DateTime(1933, 10, 18))
                is (p ()) "CONVERT ( FALSE, DATETIME )" (DateTime(1899, 12, 30))
                // Text is read as the date functions read it, by the model's culture (en-US here):
                // month first, day first only when month first is no date (Microsoft Learn, DATEVALUE).
                is (p ()) "CONVERT ( \"1/2/2024\", DATETIME )" (DateTime(2024, 1, 2))
                is (p ()) "CONVERT ( \"23/10/2020\", DATETIME )" (DateTime(2020, 10, 23))
                is (p ()) "CONVERT ( \"Jan 2 2024\", DATETIME )" (DateTime(2024, 1, 2))
                refused "CONVERT ( \"2.1.2024\", DATETIME )"
                refused "CONVERT ( -1, DATETIME )"
                daxError "CONVERT ( \"abc\", DATETIME )")
            testCase "to STRING, DOUBLE and BOOLEAN" (fun () ->
                is (p ()) "CONVERT ( 12, STRING )" "12"
                is (p ()) "CONVERT ( 1.5, TEXT )" "1.5"
                is (p ()) "CONVERT ( 3, DOUBLE )" 3.0
                is (p ()) "CONVERT ( \"1e3\", DOUBLE )" 1000.0
                is (p ()) "CONVERT ( 2, BOOLEAN )" true
                is (p ()) "CONVERT ( 0, LOGICAL )" false
                // A date's text depends on the culture; the text of TRUE is not settled.
                refused "CONVERT ( dt\"2024-01-31\", STRING )"
                refused "CONVERT ( TRUE, STRING )"
                refused "CONVERT ( \"true\", BOOLEAN )"
                Expect.equal (code "CONVERT ( 1, NOTATYPE )") "dax-invalid" "")
            // dax.guide/value: VALUE of "123", "123.45678", "123e3", "123e-3", "2020-10-23",
            // "10/23/2020", "23/10/2020" (all 44127), "10:23:45" (0.4331597222222222), "18:05:00",
            // "6:05:00 pm" and "6:05pm" (0.7534722222222222). An empty string is an error (SQLBI).
            testCase "VALUE" (fun () ->
                is (p ()) "VALUE ( \"123\" )" 123.0
                is (p ()) "VALUE ( \"123.45678\" )" 123.45678
                is (p ()) "VALUE ( \"123e3\" )" 123000.0
                is (p ()) "VALUE ( \"123e-3\" )" 0.123
                is (p ()) "VALUE ( \"2020-10-23\" )" 44127.0
                is (p ()) "VALUE ( \"10/23/2020\" )" 44127.0
                is (p ()) "VALUE ( \"23/10/2020\" )" 44127.0
                close "VALUE ( \"10:23:45\" )" 0.4331597222222222
                close "VALUE ( \"18:05:00\" )" 0.7534722222222222
                close "VALUE ( \"6:05:00 pm\" )" 0.7534722222222222
                close "VALUE ( \"6:05pm\" )" 0.7534722222222222
                is (p ()) "VALUE ( 3 )" 3.0
                daxError "VALUE ( \"\" )"
                daxError "VALUE ( \"abc\" )"
                refused "VALUE ( \"1,234\" )"
                refused "VALUE ( \"$5\" )"
                refused "VALUE ( BLANK () )")
            // SQLBI: "The division (/) operator displays the same behavior as the DIVIDE function
            // ... unless a currency is divided by an integer or by a decimal; in this case, the
            // result is currency".
            testCase "DIVIDE has the result types of /" (fun () ->
                is (p ()) "DIVIDE ( CURRENCY ( 10 ), 4 )" 2.5m
                is (p ()) "DIVIDE ( CURRENCY ( 10 ), CURRENCY ( 4 ) )" 2.5
                is (p ()) "DIVIDE ( 10, CURRENCY ( 4 ) )" 2.5
                is (p ()) "DIVIDE ( CURRENCY ( 10 ), CURRENCY ( 0 ) )" null
                is (p ()) "DIVIDE ( 10, 4 )" 2.5)
        ]

        testList "values" [
            // Microsoft Learn's comparison table: a currency or an integer with a decimal number
            // compares as decimal numbers.
            testCase "numbers of different types compare as the table says" (fun () ->
                is (p ()) "CURRENCY ( 0.1 ) = 0.1" true
                is (p ()) "9007199254740993 = 9007199254740992.0" true
                is (p ()) "9007199254740993 = 9007199254740992" false
                is (p ()) "COUNTROWS ( FILTER ( N, N[c] = 4.0 ) )" 3
                is (p ()) "CALCULATE ( COUNTROWS ( N ), N[c] = 4.0 )" 3
                is (p ()) "CALCULATE ( COUNTROWS ( N ), N[v] = 4.0 )" 3)
            // SQLBI, "Rounding errors with different data types in DAX".
            testCase "integer overflow wraps around in an operator, and is refused in an aggregation" (fun () ->
                is (p ()) "CONVERT ( 5E18, INTEGER ) + CONVERT ( 5E18, INTEGER )" -8446744073709551616L
                is (p ()) "3037000500 * 3037000500" -9223372036709301616L
                refused "SUMX ( N, 5000000000000000000 )"
                // Not an error IFERROR may turn into a value.
                refused "IFERROR ( SUMX ( N, 5000000000000000000 ), 0 )")
            testCase "what POWER leaves open is refused" (fun () ->
                is (p ()) "POWER ( 5, 2 )" 25.0
                is (p ()) "POWER ( 4, -1 )" 0.25
                is (p ()) "POWER ( 16, 0.5 )" 4.0
                is (p ()) "POWER ( \"5\", \"4\" )" 625.0
                is (p ()) "POWER ( BLANK (), 2 )" null
                is (p ()) "POWER ( 1, BLANK () )" 1.0
                refused "POWER ( -8, 0.5 )"
                refused "0 ^ 0"
                refused "POWER ( 0, -1 )")
        ]

        testList "rounding" [
            // Microsoft Learn, ROUND / ROUNDUP / ROUNDDOWN / TRUNC and their examples.
            testCase "ROUND, ROUNDUP, ROUNDDOWN, TRUNC" (fun () ->
                is (p ()) "ROUND ( 1.15, 1 )" 1.2
                is (p ()) "ROUND ( -1.15, 1 )" -1.2
                is (p ()) "ROUND ( 21.5, -1 )" 20.0
                is (p ()) "ROUND ( 7, 2 )" 7.0
                is (p ()) "ROUND ( CURRENCY ( 2.345 ), 2 )" 2.35
                is (p ()) "ROUND ( \"2.5\", 0 )" 3.0
                close "ROUNDUP ( PI (), 4 )" 3.1416
                is (p ()) "ROUNDUP ( 13.25, -1 )" 20.0
                is (p ()) "ROUNDUP ( 2.45, -1 )" 10.0
                is (p ()) "ROUNDUP ( 345.01, -1 )" 350.0
                is (p ()) "ROUNDUP ( -1.1, 0 )" -2.0
                is (p ()) "ROUNDUP ( 0.1, 1 )" 0.1
                is (p ()) "ROUNDDOWN ( 3.14159, 3 )" 3.141
                is (p ()) "ROUNDDOWN ( 31415.92654, -2 )" 31400.0
                is (p ()) "ROUNDDOWN ( -1.9, 0 )" -1.0
                is (p ()) "TRUNC ( PI () )" 3.0
                is (p ()) "TRUNC ( -8.9 )" -8.0
                is (p ()) "TRUNC ( -4.3 )" -4.0
                is (p ()) "TRUNC ( 1.239, 2 )" 1.23)
            testCase "what the pages don't settle is refused" (fun () ->
                // 0.1 + 0.2 is 0.30000000000000004: up to 0.4 as binary, 0.3 as written.
                refused "ROUNDUP ( 0.1 + 0.2, 1 )"
                refused "ROUND ( BLANK (), 1 )"
                refused "ROUND ( 1.5, 0.5 )"
                refused "ROUND ( TRUE, 0 )"
                refused "ROUND ( dt\"2024-01-31\", 0 )"
                refused "RAND ()"
                refused "RANDBETWEEN ( 1, 6 )")
            // dax.guide/int and SQLBI ("INT ( BLANK () )" is a "Blank value").
            testCase "INT" (fun () ->
                is (p ()) "INT ( -2.1 )" -3
                is (p ()) "INT ( -0.1 )" -1
                is (p ()) "INT ( 2.9 )" 2
                is (p ()) "INT ( TRUE )" 1
                is (p ()) "INT ( \"1950.00\" )" 1950
                is (p ()) "INT ( \"1E6\" )" 1000000
                is (p ()) "INT ( CURRENCY ( 2.9 ) )" 2m
                is (p ()) "INT ( CURRENCY ( -2.1 ) )" -3m
                is (p ()) "INT ( BLANK () )" null)
            // Microsoft Learn, MROUND; dax.guide/mround ("MROUND ( 6.05, 0.1 ) returns 6.0 while
            // MROUND ( 7.05, 0.1 ) returns 7.1": the direction of such a midpoint is undefined).
            testCase "MROUND" (fun () ->
                is (p ()) "MROUND ( -10, -3 )" -9.0
                is (p ()) "MROUND ( 7, 3 )" 6.0
                is (p ()) "MROUND ( 8, 3 )" 9.0
                is (p ()) "MROUND ( 5, 2 )" 6.0
                close "MROUND ( 1.25, 0.2 )" 1.2
                daxError "MROUND ( 5, -2 )"
                refused "MROUND ( 6.05, 0.1 )"
                refused "MROUND ( 1.3, 0.2 )"
                refused "MROUND ( 5, 0 )")
            // dax.guide/ceiling, with the result types it shows: "CEILING ( 10.2, 1 ) -- Returns
            // 11 (Integer)", "CEILING ( 10.2, 0.5 ) -- Returns 10.5 (Decimal)", "CEILING ( 10.2,
            // CURRENCY ( 0.5 ) ) -- Returns 10.5 (Currency)", "CEILING ( -10.7, -1 ) -- Returns
            // -11 (Integer)", "CEILING ( -10.7, 0.5 ) -- Returns -10.5 (Decimal)".
            testCase "CEILING" (fun () ->
                is (p ()) "CEILING ( 10.2, 1 )" 11
                is (p ()) "CEILING ( 10.7, 1 )" 11
                is (p ()) "CEILING ( 10.2, 0.5 )" 10.5
                is (p ()) "CEILING ( 10.7, 0.5 )" 11.0
                is (p ()) "CEILING ( 10.2, CURRENCY ( 0.5 ) )" 10.5m
                is (p ()) "CEILING ( -10.2, 1 )" -10
                is (p ()) "CEILING ( -10.2, -1 )" -11
                is (p ()) "CEILING ( -10.7, -1 )" -11
                is (p ()) "CEILING ( -10.2, 0.5 )" -10.0
                is (p ()) "CEILING ( -10.7, 0.5 )" -10.5
                is (p ()) "CEILING ( -10.7, CURRENCY ( 0.5 ) )" -10.5m
                close "CEILING ( 4.42, 0.05 )" 4.45
                is (p ()) "CEILING ( 11, 3 )" 12
                // "If Significance is 0 or BLANK, CEILING returns 0. If Number is BLANK, CEILING
                // returns BLANK. If Number is positive and Significance is negative, CEILING
                // returns an error".
                is (p ()) "CEILING ( 10.2, 0 )" 0
                is (p ()) "CEILING ( BLANK (), 1 )" null
                daxError "CEILING ( 10.2, -1 )")
            // dax.guide/iso-ceiling: "ISO.CEILING ( -10.2, -1 ) -- Returns -10 (Integer)",
            // "ISO.CEILING ( 10.2, 0.5 ) -- Returns 10.5 (Decimal)".
            testCase "ISO.CEILING" (fun () ->
                is (p ()) "ISO.CEILING ( 10.2, 0.5 )" 10.5
                is (p ()) "ISO.CEILING ( -10.7, 0.5 )" -10.5
                is (p ()) "ISO.CEILING ( -11, -3 )" -9
                is (p ()) "ISO.CEILING ( 10.2 )" 11
                is (p ()) "ISO.CEILING ( -10.2 )" -10
                is (p ()) "ISO.CEILING ( 10.2, CURRENCY ( 0.5 ) )" 10.5m
                is (p ()) "ISO.CEILING ( -10.2, -1 )" -10
                is (p ()) "ISO.CEILING ( -10.2, 1 )" -10
                refused "ISO.CEILING ( 10.2, 0 )")
            // Microsoft Learn, FLOOR: 10.8423, 8.0373 and 2.9733 to .1 are 10.8, 8 and 2.9;
            // dax.guide/ceiling's table: FLOOR ( 5, 3 ) is 3, FLOOR ( 11, 3 ) is 9.
            testCase "FLOOR" (fun () ->
                close "FLOOR ( 10.8423, 0.1 )" 10.8
                close "FLOOR ( 8.0373, 0.1 )" 8.0
                close "FLOOR ( 2.9733, 0.1 )" 2.9
                is (p ()) "FLOOR ( 5, 3 )" 3.0
                is (p ()) "FLOOR ( 11, 3 )" 9.0
                is (p ()) "FLOOR ( -2.5, -2 )" -2.0
                daxError "FLOOR ( 2.5, -2 )"
                refused "FLOOR ( 2.5, 0 )")
        ]

        testList "whole-number functions" [
            // Microsoft Learn, MOD: "MOD(n, d) = n - d*INT(n/d)"; MOD(3,2) is 1, MOD(-3,-2) is -1.
            testCase "MOD and QUOTIENT" (fun () ->
                is (p ()) "MOD ( 3, 2 )" 1
                is (p ()) "MOD ( -3, -2 )" -1
                is (p ()) "MOD ( -3, 2 )" 1
                is (p ()) "MOD ( 3, -2 )" -1
                is (p ()) "MOD ( 8, 3 )" 2
                is (p ()) "MOD ( 5.5, 2 )" 1.5
                is (p ()) "MOD ( -5.5, 2 )" 0.5
                daxError "MOD ( 5, 0 )"
                refused "MOD ( CURRENCY ( 5 ), 2 )"
                // dax.guide/quotient: QUOTIENT ( 1, 2 ) is 0, ( 7, 3 ) is 2; Microsoft Learn:
                // QUOTIENT(10/2, 2) is 2.
                is (p ()) "QUOTIENT ( 1, 2 )" 0
                is (p ()) "QUOTIENT ( 7, 3 )" 2
                is (p ()) "QUOTIENT ( 10 / 2, 2 )" 2
                is (p ()) "QUOTIENT ( -5, 2 )" -2
                is (p ()) "QUOTIENT ( CURRENCY ( 7.5 ), 2 )" 3
                daxError "QUOTIENT ( 5, 0 )")
            // Microsoft Learn, GCD / LCM / FACT / EVEN / ODD / SIGN and their examples.
            testCase "GCD, LCM, FACT, EVEN, ODD, SIGN, ABS" (fun () ->
                is (p ()) "GCD ( 5, 2 )" 1
                is (p ()) "GCD ( 24, 36 )" 12
                is (p ()) "GCD ( 24.9, 36.9 )" 12
                is (p ()) "GCD ( GCD ( 12, 18 ), 8 )" 2
                is (p ()) "LCM ( 5, 2 )" 10
                is (p ()) "LCM ( 24, 36 )" 72
                is (p ()) "LCM ( LCM ( 4, 6 ), 10 )" 60
                daxError "GCD ( -1, 2 )"
                daxError "LCM ( 9007199254740991, 9007199254740990 )"
                is (p ()) "FACT ( 0 )" 1.0
                is (p ()) "FACT ( 5 )" 120.0
                close "FACT ( 170 ) / 1E306" 7.257415615308
                daxError "FACT ( 171 )"
                refused "FACT ( 2.5 )"
                is (p ()) "EVEN ( 1.5 )" 2
                is (p ()) "EVEN ( 3 )" 4
                is (p ()) "EVEN ( 2 )" 2
                is (p ()) "EVEN ( -1 )" -2
                is (p ()) "ODD ( 1.5 )" 3
                is (p ()) "ODD ( 3 )" 3
                is (p ()) "ODD ( 2 )" 3
                is (p ()) "ODD ( -1 )" -1
                is (p ()) "ODD ( -2 )" -3
                refused "ODD ( 0 )"
                is (p ()) "SIGN ( -2.5 )" -1
                is (p ()) "SIGN ( 0 )" 0
                is (p ()) "SIGN ( CURRENCY ( 3 ) )" 1
                is (p ()) "ABS ( CURRENCY ( -3 ) )" 3m
                is (p ()) "ABS ( \"-2\" )" 2)
            // Microsoft Learn: SQRT of a negative number is an error; LOG(100,10), LOG(100) and
            // LOG10(100) are 2; SQRTPI(1) is 1.772454; dax.guide/ln: not positive is an error.
            testCase "roots and logarithms" (fun () ->
                is (p ()) "SQRT ( 25 )" 5.0
                daxError "SQRT ( -1 )"
                Expect.floatClose Accuracy.medium (number (scalar (p ()) "SQRTPI ( 1 )")) 1.772454 ""
                Expect.floatClose Accuracy.medium (number (scalar (p ()) "SQRTPI ( 2 )")) 2.506628 ""
                is (p ()) "LOG ( 100, 10 )" 2.0
                is (p ()) "LOG ( 100 )" 2.0
                is (p ()) "LOG10 ( 100 )" 2.0
                close "LOG ( 25, 5 )" 2.0
                close "EXP ( 1 )" Math.E
                is (p ()) "LN ( 1 )" 0.0
                daxError "LN ( 0 )"
                refused "LOG ( 0 )"
                refused "EXP ( 1000 )"
                refused "SQRT ( BLANK () )")
        ]

        testList "statistical" [
            // dax.guide/median and /percentile-inc, on { 2, 4, 4, 4, 5, 5, 7, 9 }: MEDIAN 4.5;
            // PERCENTILE.EXC 0.25 / 0.50 / 0.75 are 4, 4.5, 6.5; PERCENTILE.INC 4, 4.5, 5.5.
            testCase "MEDIAN and percentiles" (fun () ->
                is (p ()) "MEDIAN ( N[v] )" 4.5
                is (p ()) "MEDIAN ( N[d] )" 4.5
                is (p ()) "MEDIANX ( N, N[v] )" 4.5
                is (p ()) "PERCENTILE.EXC ( N[d], 0.25 )" 4.0
                is (p ()) "PERCENTILE.EXC ( N[d], 0.50 )" 4.5
                is (p ()) "PERCENTILE.EXC ( N[d], 0.75 )" 6.5
                is (p ()) "PERCENTILE.INC ( N[d], 0.25 )" 4.0
                is (p ()) "PERCENTILE.INC ( N[d], 0.50 )" 4.5
                is (p ()) "PERCENTILE.INC ( N[d], 0.75 )" 5.5
                is (p ()) "PERCENTILE.INC ( N[v], 0.75 )" 5.5
                is (p ()) "PERCENTILEX.INC ( N, N[d] * 2, 0.75 )" 11.0
                is (p ()) "PERCENTILE.INC ( N[d], 1 )" 9.0
                is (p ()) "PERCENTILE.INC ( N[d], 0 )" 2.0
                is (p ()) "MEDIAN ( Empty[v] )" null
                is (p ()) "PERCENTILE.INC ( Empty[d], 0.5 )" null
                daxError "PERCENTILE.INC ( N[d], 1.5 )"
                // "Desired percentile value in the interval [1/(n+1),1-1/(n+1)]".
                daxError "PERCENTILE.EXC ( N[d], 0.05 )"
                daxError "PERCENTILE.EXC ( N[d], 1 )")
            // dax.guide/medianx: "(Integer for non-ties, Decimal for ties)"; MEDIAN ignores a
            // BLANK and MEDIANX doesn't: over { BLANK(), 2, 4, 4, 4, 5, 5, 7, 9 } they are 4.5
            // and 4.
            testCase "the median's type, and BLANKs" (fun () ->
                is (p ()) "MEDIAN ( B[v] )" 4.5
                is (p ()) "MEDIANX ( B, B[v] )" 4
                is (p ()) "MEDIANX ( B, B[d] )" 4.0
                is (p ()) "MEDIANX ( FILTER ( N, N[v] > 4 ), N[v] )" 6.0
                is (p ()) "MEDIANX ( FILTER ( N, N[v] > 5 ), N[v] )" 8.0
                is (p ()) "MEDIANX ( FILTER ( N, N[v] >= 5 ), N[v] )" 6.0
                is (p ()) "MEDIANX ( FILTER ( N, N[v] < 5 ), N[v] )" 4.0
                is (p ()) "MEDIANX ( FILTER ( N, N[v] <= 4 && N[v] > 2 ), N[v] )" 4
                // Only the median's type is documented.
                refused "PERCENTILE.INC ( N[v], 1 )"
                is (p ()) "MEDIANX ( B, B[v] - 5 )" -1
                refused "MEDIANX ( B, IF ( B[v] > 2, B[v] - 5 ) )"
                refused "MEDIANX ( FILTER ( B, B[v] < 3 ), B[v] )"
                refused "MEDIAN ( N[c] )"
                refused "MEDIAN ( N[s] )"
                refused "MEDIAN ( N[b] )")
            // dax.guide/stdev-s on the same sample: STDEV.P 2.0000000000, STDEV.S 2.1380899353;
            // dax.guide/geomean: 4.60.
            testCase "STDEV, VAR, GEOMEAN" (fun () ->
                close "STDEV.P ( N[v] )" 2.0
                Expect.floatClose Accuracy.medium (number (scalar (p ()) "STDEV.S ( N[v] )")) 2.1380899353 ""
                close "STDEVX.P ( N, N[d] )" 2.0
                Expect.floatClose Accuracy.medium (number (scalar (p ()) "STDEVX.S ( N, N[d] )")) 2.1380899353 ""
                close "VAR.P ( N[v] )" 4.0
                close "VAR.S ( N[v] )" (32.0 / 7.0)
                close "VARX.P ( N, N[v] * 2 )" 16.0
                close "STDEV.P ( B[v] )" 2.0
                Expect.equal (Math.Round(number (scalar (p ()) "GEOMEAN ( N[v] )"), 2)) 4.6 ""
                close "GEOMEANX ( N, N[d] )" (Math.Pow(2.0 * 4.0 * 4.0 * 4.0 * 5.0 * 5.0 * 7.0 * 9.0, 1.0 / 8.0))
                daxError "CALCULATE ( STDEV.S ( N[v] ), N[v] = 9 )"
                refused "CALCULATE ( STDEV.P ( N[v] ), N[v] = 9 )"
                refused "STDEV.P ( Empty[v] )"
                refused "GEOMEANX ( N, N[v] - 2 )"
                refused "STDEV.S ( N[c] )")
            // dax.guide/averagea ("FALSE is 0 and TRUE is 1"; "always considers a string as 0"),
            // dax.guide/mina, and dax.guide/rank-eq: RANK.EQ ( 70, { 10..100 } ) is 4, ASC 7.
            testCase "AVERAGEA, MINA, MAXA, RANK.EQ" (fun () ->
                is (p ()) "AVERAGEA ( N[v] )" 5.0
                is (p ()) "AVERAGEA ( N[b] )" 0.375
                is (p ()) "AVERAGEA ( N[s] )" 0.0
                is (p ()) "AVERAGEA ( Empty[v] )" null
                is (p ()) "MINA ( N[v] )" 2
                is (p ()) "MAXA ( N[d] )" 9.0
                is (p ()) "MINA ( N[b] )" 0
                is (p ()) "MAXA ( N[b] )" 1
                is (p ()) "CALCULATE ( MINA ( N[b] ), N[b] = TRUE )" 1
                refused "MINA ( N[s] )"
                refused "MINA ( Empty[v] )"
                is (p ()) "RANK.EQ ( 7, N[v] )" 2
                is (p ()) "RANK.EQ ( 7, N[v], ASC )" 7
                is (p ()) "RANK.EQ ( 4, N[v] )" 5
                is (p ()) "RANK.EQ ( 4, N[v], 1 )" 2
                is (p ()) "RANK.EQ ( 4.0, N[c], DESC )" 5
                // "If value is not in columnName or value is a blank, then RANK.EQ returns a
                // blank value" (Microsoft Learn).
                is (p ()) "RANK.EQ ( 6, N[v] )" null
                is (p ()) "RANK.EQ ( BLANK (), N[v] )" null
                refused "RANK.EQ ( 4, B[v] )")
        ]

        testList "information" [
            // dax.guide/isnumber ("any numeric data type (Currency, DateTime, Decimal, Integer)";
            // "does not test whether a string can be converted to a number"); Microsoft Learn,
            // ISTEXT and ISNONTEXT (an empty string is text; BLANK is not).
            testCase "type tests" (fun () ->
                is (p ()) "ISNUMBER ( 1 )" true
                is (p ()) "ISNUMBER ( 1.5 )" true
                is (p ()) "ISNUMBER ( CURRENCY ( 1 ) )" true
                is (p ()) "ISNUMBER ( dt\"2024-01-31\" )" true
                is (p ()) "ISNUMBER ( \"1\" )" false
                is (p ()) "ISNUMBER ( TRUE )" false
                is (p ()) "ISTEXT ( \"text\" )" true
                is (p ()) "ISTEXT ( \"\" )" true
                is (p ()) "ISTEXT ( 42 )" false
                is (p ()) "ISTEXT ( BLANK () )" false
                is (p ()) "ISNONTEXT ( 1 )" true
                is (p ()) "ISNONTEXT ( BLANK () )" true
                is (p ()) "ISNONTEXT ( \"\" )" false
                is (p ()) "ISLOGICAL ( TRUE )" true
                is (p ()) "ISLOGICAL ( 42 )" false
                // A BLANK from a column has the column's type.
                is (p ()) "COUNTROWS ( FILTER ( B, ISNUMBER ( B[v] ) ) )" 9
                refused "ISNUMBER ( BLANK () )"
                refused "ISLOGICAL ( BLANK () )")
            testCase "ISEVEN, ISODD, ISEMPTY, CONTAINS" (fun () ->
                is (p ()) "ISEVEN ( 4 )" true
                is (p ()) "ISEVEN ( -3 )" false
                is (p ()) "ISODD ( 7.0 )" true
                is (p ()) "ISODD ( 0 )" false
                refused "ISEVEN ( 2.5 )"
                daxError "ISEVEN ( \"abc\" )"
                is (p ()) "ISEMPTY ( Empty )" true
                is (p ()) "ISEMPTY ( N )" false
                is (p ()) "ISEMPTY ( FILTER ( N, N[v] > 100 ) )" true
                is (p ()) "CONTAINS ( N, N[v], 7 )" true
                is (p ()) "CONTAINS ( N, N[v], 6 )" false
                is (p ()) "CONTAINS ( N, N[v], 4, N[b], TRUE )" true
                is (p ()) "CONTAINS ( N, N[v], 9, N[b], TRUE )" false
                is (p ()) "CONTAINS ( N, N[s], \"7\" )" true
                // Strict: "BLANK does not match 0 or an empty string".
                is (p ()) "CONTAINS ( B, B[v], BLANK () )" true
                is (p ()) "CONTAINS ( B, B[v], 0 )" false
                is (p ()) "CONTAINS ( VALUES ( N[v] ), N[v], 5 )" true)
            // The local engine has no connection: these are refused until an identity is given.
            testCase "USERNAME and the others answer only from a given identity" (fun () ->
                let fresh = prepare model

                for f in [ "USERNAME"; "USERPRINCIPALNAME"; "USEROBJECTID"; "USERCULTURE"; "CUSTOMDATA" ] do
                    match Engine.evaluateScalar fresh $"{f} ()" with
                    | Error e -> Expect.equal (TessError.code e) "not-supported" f
                    | Ok v -> failtestf "%s answered %A without an identity" f v

                let known =
                    fresh
                    |> Engine.withIdentity {
                        Tessera.DaxEngine.Identity.none with
                            UserName = Some "CONTOSO\\ann"
                            UserPrincipalName = Some "ann@contoso.com"
                            UserCulture = Some "en-US"
                    }

                is known "USERNAME ()" "CONTOSO\\ann"
                is known "USERPRINCIPALNAME ()" "ann@contoso.com"
                is known "USERCULTURE ()" "en-US"
                // "Blank, if CustomData property was not defined at connection time".
                is known "CUSTOMDATA ()" null

                match Engine.evaluateScalar known "USEROBJECTID ()" with
                | Error e -> Expect.equal (TessError.code e) "not-supported" "not given"
                | Ok v -> failtestf "USEROBJECTID answered %A" v

                // The original is unchanged: a Prepared stays shareable.
                match Engine.evaluateScalar fresh "USERNAME ()" with
                | Error e -> Expect.equal (TessError.code e) "not-supported" "the original"
                | Ok v -> failtestf "the original answered %A" v

                // A measure evaluated for the query sees the identity too.
                match Engine.evaluateQuery known "DEFINE MEASURE N[who] = USERNAME () EVALUATE { [who] }" with
                | Ok [ r ] -> Expect.equal r.Rows [ [| QueryValue.String "CONTOSO\\ann" |] ] "measure"
                | other -> failtestf "measure: %A" other)
        ]

        testList "properties" [
            testPropertyWithConfig config "ROUND is idempotent"
            <| fun (n: int) (d: uint8) ->
                let x = float (n % 10000000) / 1000.0
                let digits = int d % 4
                let once = $"ROUND ( {lit x}, {digits} )"

                match tryScalar once with
                | None -> true
                | Some r ->
                    match tryScalar $"ROUND ( {once}, {digits} )" with
                    | None -> true
                    | Some again -> again = r

            testPropertyWithConfig config "ROUNDDOWN <= x <= ROUNDUP in size, and ROUND is one of them"
            <| fun (n: int) (d: uint8) ->
                let x = float (n % 10000000) / 1000.0
                let digits = int d % 4 - 1

                match
                    tryScalar $"ROUNDDOWN ( {lit x}, {digits} )",
                    tryScalar $"ROUNDUP ( {lit x}, {digits} )",
                    tryScalar $"ROUND ( {lit x}, {digits} )"
                with
                | Some down, Some up, Some nearest ->
                    abs (number down) <= abs x
                    && abs x <= abs (number up)
                    && (nearest = down || nearest = up)
                | _ -> true

            testPropertyWithConfig config "FLOOR <= x <= CEILING, both multiples, less than one multiple apart"
            <| fun (n: uint16) (m: uint8) ->
                let x = float (int n + 1) / 8.0
                let s = float (int m % 40 + 1) / 4.0

                match tryScalar $"FLOOR ( {lit x}, {lit s} )", tryScalar $"CEILING ( {lit x}, {lit s} )" with
                | Some f, Some c ->
                    let f, c = number f, number c

                    f <= x
                    && x <= c
                    && c - f < s + 1e-9
                    && abs (f / s - Math.Round(f / s)) < 1e-9
                    && abs (c / s - Math.Round(c / s)) < 1e-9
                | _ -> true

            // "MOD(n, d) = n - d*INT(n/d)"; "the sign is always the same as the sign of the divisor".
            testPropertyWithConfig config "MOD is the remainder of INT ( n / d ), with the divisor's sign"
            <| fun (n: int) (d: int) ->
                d = 0
                || (let m = number (scalar (p ()) $"MOD ( {n}, {d} )")
                    let q = number (scalar (p ()) $"INT ( {n} / {d} )")

                    float n = float d * q + m
                    && abs m < abs (float d)
                    && (m = 0.0 || (m < 0.0) = (d < 0)))

            testPropertyWithConfig config "QUOTIENT is the division truncated toward zero"
            <| fun (n: int) (d: int) ->
                d = 0
                || (let q = number (scalar (p ()) $"QUOTIENT ( {n}, {d} )")
                    let rest = float n - q * float d

                    abs rest < abs (float d) && (rest = 0.0 || (rest < 0.0) = (n < 0)))

            // "MEDIAN corresponds to PERCENTILE.INC with k=0.50" (dax.guide/percentile-inc).
            testPropertyWithConfig config "MEDIANX is PERCENTILEX.INC at 0.5"
            <| fun (xs: int list) ->
                let values = xs |> List.truncate 12 |> List.map (fun n -> float (n % 1000) / 4.0)

                values.IsEmpty
                || (let t = values |> List.map lit |> String.concat ", "

                    scalar (p ()) $"MEDIANX ( {{ {t} }}, [Value] )" =
                        scalar (p ()) $"PERCENTILEX.INC ( {{ {t} }}, [Value], 0.5 )")

            // The formulas of Microsoft Learn's VAR.P and VAR.S pages.
            testPropertyWithConfig config "variance identities"
            <| fun (xs: int list) ->
                let values = xs |> List.truncate 12 |> List.map (fun n -> float (n % 1000) / 4.0)
                let n = float values.Length

                values.Length < 2
                || (let t = "{ " + (values |> List.map lit |> String.concat ", ") + " }"

                    let get f =
                        number (scalar (p ()) $"{f} ( {t}, [Value] )")

                    let varP, varS = get "VARX.P", get "VARX.S"

                    let near (a: float) (b: float) =
                        abs (a - b) <= 1e-9 * (1.0 + abs a + abs b)

                    varP >= 0.0
                    && near (varS * (n - 1.0)) (varP * n)
                    && near (get "STDEVX.P" * get "STDEVX.P") varP
                    && near (get "STDEVX.S" * get "STDEVX.S") varS)
        ]
        testList "trigonometric" [
            test "the published values" {
                // Microsoft Learn's examples, each function's page.
                close "SIN ( PI () / 2 )" 1.0
                close "SIN ( RADIANS ( 30 ) )" 0.5
                close "COS ( RADIANS ( 60 ) )" 0.5
                close "TAN ( RADIANS ( 45 ) )" 1.0
                close "COT ( 30 )" -0.156119952161659
                close "ASIN ( -0.5 )" (-Math.PI / 6.0)
                close "DEGREES ( ASIN ( -0.5 ) )" -30.0
                close "ACOS ( -0.5 )" (2.0 * Math.PI / 3.0)
                close "ATAN ( 1 )" (Math.PI / 4.0)
                close "COSH ( 4 )" 27.308232836016487
                close "TANH ( 0.5 )" 0.46211715726000974
                close "COTH ( 2 )" 1.03731472072755
                close "ASINH ( 10 )" 2.99822295029797
                close "ACOSH ( 1 )" 0.0
                close "ATANH ( -0.1 )" -0.100335347731076
                close "DEGREES ( PI () )" 180.0
                close "RADIANS ( 270 )" (3.0 * Math.PI / 2.0)
                // dax.guide/acot's table: the arccotangent of -1 is 2.36, of a large negative 1.57.
                close "ACOT ( -1 )" (3.0 * Math.PI / 4.0)
                close "ACOT ( 1 )" (Math.PI / 4.0)
                close "ACOT ( 0 )" (Math.PI / 2.0)
                close "ACOTH ( 2 )" (0.5 * log 3.0)
            }

            test "the result is a decimal number, also of a whole number" {
                Expect.equal (scalar (p ()) "SIN ( 0 )") (QueryValue.Double 0.0) "SIN ( 0 )"
                Expect.equal (scalar (p ()) "DEGREES ( 0 )") (QueryValue.Double 0.0) "DEGREES ( 0 )"
            }

            test "the documented errors" {
                // COT: 0, or 2^27 and more; BLANK is 0 (dax.guide/cot). COTH: 0 (dax.guide/coth).
                daxError "COT ( 0 )"
                daxError "COT ( 134217728 )"
                daxError "COT ( -134217728 )"
                daxError "COT ( BLANK () )"
                daxError "COTH ( 0 )"
                daxError "COTH ( BLANK () )"
                // ATANH: -1, 1 or outside (dax.guide/atanh).
                daxError "ATANH ( 1 )"
                daxError "ATANH ( -1 )"
                daxError "ATANH ( 2 )"
                close "COT ( 134217727 )" (1.0 / tan 134217727.0)
            }

            test "what the pages leave open is refused" {
                for f in
                    [
                        "SIN"
                        "COS"
                        "TAN"
                        "ASIN"
                        "ACOS"
                        "ATAN"
                        "ACOT"
                        "SINH"
                        "COSH"
                        "TANH"
                        "ASINH"
                        "ACOSH"
                        "ATANH"
                        "ACOTH"
                        "DEGREES"
                        "RADIANS"
                    ] do
                    refused $"{f} ( BLANK () )"

                refused "ASIN ( 1.5 )"
                refused "ACOS ( -2 )"
                refused "ACOSH ( 0.5 )"
                refused "ACOTH ( 1 )"
                refused "ACOTH ( 0.5 )"
                refused "SINH ( 1000 )"
                refused "SIN ( TRUE () )"
                refused "SIN ( DATE ( 2024, 1, 1 ) )"
            }

            test "text that is a number is read as one" { close "SIN ( \"0\" )" 0.0 }

            testPropertyWithConfig config "identities, within a rounding error"
            <| fun (n: int) ->
                let x = float (n % 100000) / 1000.0
                let get (dax: string) = number (scalar (p ()) dax)

                let near (a: float) (b: float) =
                    abs (a - b) <= 1e-9 * (1.0 + abs a + abs b)

                let l = lit x
                let hx = x / 100.0
                let h = lit hx
                let s, c = get $"SIN ( {l} )", get $"COS ( {l} )"

                near (s * s + c * c) 1.0
                && (abs c < 1e-6 || near (get $"TAN ( {l} )") (s / c))
                && (abs c < 1e-6
                    || (let a = get $"ATAN ( TAN ( {l} ) )"
                        let k = Math.Round((x - a) / Math.PI)
                        abs a <= Math.PI / 2.0 && abs (x - a - k * Math.PI) < 1e-6))
                && near (get $"RADIANS ( DEGREES ( {l} ) )") x
                && near (get $"ASINH ( SINH ( {h} ) )") hx
                && near (get $"COSH ( {h} ) ^ 2 - SINH ( {h} ) ^ 2") 1.0
                && near (get $"ACOT ( {l} ) + ATAN ( {l} )") (Math.PI / 2.0)
                && (hx = 0.0 || near (get $"COTH ( {h} ) * TANH ( {h} )") 1.0)
                && (abs (s) < 1e-9 || near (get $"COT ( {l} ) * TAN ( {l} )") 1.0)
        ]
    ]
