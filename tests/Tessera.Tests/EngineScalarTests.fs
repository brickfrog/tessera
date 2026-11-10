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

/// The expression is refused, with a reason that mentions this.
let private refused (dax: string) (mentions: string) =
    match scalarError (p ()) dax with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported, got %A\n%s" other dax

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
                is (p ()) "IF ( BLANK (), 1, 2 )" 2)
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
            // "It uses an eager execution plan which always executes the branch expressions
            // regardless of the condition expression" (Microsoft Learn, IF.EAGER).
            testCase "IF.EAGER evaluates both branches" (fun () ->
                is (p ()) "IF.EAGER ( TRUE, 1, 2 )" 1
                is (p ()) "IF.EAGER ( FALSE, 1, 2.5 )" 2.5
                is (p ()) "IF.EAGER ( FALSE, 1 )" null
                isError "dax-error" "IF.EAGER ( TRUE, 1, ERROR ( \"no\" ) )"
                isError "dax-error" "IF.EAGER ( FALSE, ERROR ( \"no\" ), 1 )")
            // SQLBI, "BLANK handling in DAX": the table gives FALSE, TRUE, TRUE, FALSE for these
            // four, and "When <expression> is of Boolean data type, IF is of Boolean data type and
            // an implicit cast is applied to BLANK which converts to FALSE"; "a BLANK value is
            // never returned by IF in case the result should be a Boolean data type".
            testCase "IF with a BLANK branch and a True/False branch is never BLANK" (fun () ->
                is (p ()) "IF ( 1 = 1, BLANK (), TRUE )" false
                is (p ()) "IF ( 1 <> 1, BLANK (), TRUE )" true
                is (p ()) "IF ( 1 = 1, TRUE, BLANK () )" true
                is (p ()) "IF ( 1 <> 1, TRUE, BLANK () )" false
                // A branch left out is a BLANK ("If omitted, a BLANK is returned").
                is (p ()) "IF ( 1 <> 1, TRUE )" false
                is (p ()) "IF.EAGER ( 1 = 1, BLANK (), TRUE )" false
                // "SWITCH can be rewritten as nested IF".
                is (p ()) "SWITCH ( 3, 1, TRUE, 2, FALSE )" false
                is (p ()) "SWITCH ( 3, 1, TRUE, BLANK () )" false
                // Any other type keeps the BLANK.
                is (p ()) "IF ( 1 = 1, BLANK (), 1 )" null
                is (p ()) "IF ( 1 = 1, BLANK (), \"x\" )" null
                is (p ()) "SWITCH ( 3, 1, 10, 2, 20 )" null)
            // "there is no cast, hence BLANK is preserved" (the same note): a True/False
            // expression that is BLANK stays BLANK. MAXX of no rows is one.
            testCase "IF keeps the BLANK of a True/False expression" (fun () ->
                is (p ()) "ISBLANK ( IF ( TRUE, IF ( FALSE, 1 ) = 1 && FALSE ) )" false
                is (p ()) "VAR b = BLANK () RETURN IF ( TRUE, b, TRUE )" false)
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
                is (p ()) "BLANK () && TRUE" false
                is (p ()) "BLANK () || TRUE" true
                is (p ()) "FALSE || BLANK ()" false)
            // Microsoft's table of BLANK in operations gives BLANK for BLANK AND BLANK and BLANK OR
            // BLANK; SQLBI ("BLANK handling in DAX") says a logical expression is never BLANK.
            testCase "BLANK on both sides of && or ||, and NOT of BLANK, are refused" (fun () ->
                refused "BLANK () || BLANK ()" "BLANK on both sides"
                refused "BLANK () && BLANK ()" "BLANK on both sides"
                refused "AND ( BLANK (), BLANK () )" "BLANK on both sides"
                refused "OR ( BLANK (), BLANK () )" "BLANK on both sides"
                refused "BLANK () && ERROR ( \"no\" )" "whether the right side is evaluated"
                refused "NOT BLANK ()" "NOT of BLANK"
                // IFERROR doesn't turn a refusal into a value.
                refused "IFERROR ( NOT BLANK (), 1 )" "NOT of BLANK")
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
            // "The following example returns 9999 if the expression 25/0 evaluates to an error"
            // (Microsoft Learn, IFERROR); "ISERROR also returns TRUE when the value is positive
            // infinity, negative infinity or NaN ... IFERROR has the same behavior"; the page's
            // example gives TRUE, TRUE, TRUE, FALSE, FALSE (dax.guide/iserror).
            testCase "IFERROR and ISERROR count the infinities and NaN as errors" (fun () ->
                is (p ()) "IFERROR ( 25 / 0, 9999 )" 9999.0
                is (p ()) "ISERROR ( 1 / 0 )" true
                is (p ()) "ISERROR ( -1 / 0 )" true
                is (p ()) "ISERROR ( 0 / 0 )" true
                is (p ()) "ISERROR ( DIVIDE ( 1, 0 ) )" false
                is (p ()) "ISERROR ( DIVIDE ( 0, 0 ) )" false
                is (p ()) "IFERROR ( 1 / 4, 9999 )" 0.25)
            // "Names of existing tables aren't allowed" (Microsoft Learn, VAR).
            testCase "a variable can't have the name of a table" (fun () ->
                isError "dax-invalid" "VAR Sales = 1 RETURN Sales + 1"
                is (p ()) "VAR SalesX = 1 RETURN SalesX + 1" 2)
            // "a variable that is referenced in the code is always evaluated, unless the DAX
            // engine removes the variable references through a static analysis of the code"; "If
            // the variable is defined but not referenced, then it is not evaluated" (SQLBI, "When
            // are variables evaluated in DAX?"). An error in a referenced variable the result
            // didn't need is raised or not depending on that analysis: refused.
            testCase "a variable that is referenced and not needed" (fun () ->
                is (p ()) "VAR e = ERROR ( \"x\" ) RETURN 1" 1
                is (p ()) "VAR a = 1 VAR b = 2 RETURN IF ( TRUE, a, b )" 1
                refused "VAR e = ERROR ( \"x\" ) RETURN IF ( FALSE, e, 1 )" "the variable e"
                refused "VAR e = ERROR ( \"x\" ) VAR f = e + 1 RETURN IF ( FALSE, f, 1 )" "the variable"
                // Not referenced by anything that is used: never evaluated.
                is (p ()) "VAR e = ERROR ( \"x\" ) VAR f = e + 1 RETURN 1" 1
                // An inner variable of the same name hides it: the outer one isn't referenced.
                is (p ()) "VAR e = ERROR ( \"x\" ) RETURN VAR e = 2 RETURN e" 2
                // Needed: the error is the result.
                isError "dax-error" "VAR e = ERROR ( \"x\" ) RETURN IF ( TRUE, e, 1 )"
                refused "IFERROR ( VAR e = ERROR ( \"x\" ) RETURN IF ( FALSE, e, 1 ), 2 )" "the variable e")
            testCase "an ERROR is reported with its text and position" (fun () ->
                match scalarError (p ()) "1 + ERROR ( \"boom\" )" with
                | DaxEvaluation(text, at) ->
                    Expect.equal text "boom" "the text"
                    Expect.equal at "expression 1:5" "where"
                | other -> failtestf "%A" other)
            testCase "text that isn't a number in arithmetic" (fun () -> isError "dax-error" "1 + \"a\"")
            // Something the engine doesn't implement is never swallowed by IFERROR.
            testCase "IFERROR does not hide a not-supported" (fun () ->
                isError "not-supported" "IFERROR ( PATHITEM ( 1, 1 ), 2 )")
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

        testList "what the sources leave open" [
            // "there is an overflow condition, but there is no overflow error and the result is the
            // least significant representable digits" (SQLBI, "Rounding errors with different data
            // types in DAX"): 3037000500^2 = 9223372037000250000, minus 2^64.
            testCase "whole numbers wrap around" (fun () ->
                is (p ()) "3037000500 * 3037000500" -9223372036709301616L
                is (p ()) "5000000000000000000 + 5000000000000000000" -8446744073709551616L
                // The documentation's range ends at 2^63 - 2.
                refused "9223372036854775807 + 1" "outside the range of whole numbers"
                refused "9223372036854775806 + 1" "outside the range of whole numbers")
            // The article is about the operators; nothing says an aggregation wraps.
            testCase "an aggregation that overflows is refused" (fun () ->
                refused "SUMX ( { 5000000000000000000, 5000000000000000000 }, [Value] )" "out of the range"
                is (p ()) "SUMX ( { 5000000000000000000, -5000000000000000000, 1 }, [Value] )" 1)
            // The comparison table of the data types page makes both sides REAL; 2^53 + 1 has no
            // REAL of its own, and whether the engine compares the exact values is not said.
            testCase "a whole number above 2^53 against a decimal number" (fun () ->
                refused "9007199254740993 = 9007199254740992.0" "too large for a decimal number"
                refused "9007199254740993 > 9007199254740992.0" "too large for a decimal number"
                is (p ()) "9007199254740993 = 9007199254740992" false
                is (p ()) "9007199254740993 > 1.5" true)
            // The multiplication table of the data types page has a Date/time column and no
            // Date/time row: REAL by a date is REAL. 2020-10-23 is day 44127.
            testCase "a decimal number times a date" (fun () ->
                is (p ()) "2.0 * dt\"2020-10-23\"" 88254.0
                is (p ()) "2.0 * dt\"2020-10-23T12:00:00\"" 88255.0
                refused "2 * dt\"2020-10-23\"" "multiplying a date"
                refused "dt\"2020-10-23\" * 2.0" "multiplying a date")
            testCase "text with spaces, separators or a currency symbol as a number" (fun () ->
                is (p ()) "\"22\" + 22" 44
                refused "\" 12 \" + 1" "spaces around a number"
                refused "\"$5\" + 1" "separators and symbols"
                refused "\"1,234\" + 1" ""
                isError "dax-error" "\"abc\" + 1")
            // "If the expression has variant or mixed value types such as text and number, then by
            // default MAXX considers only numbers. If <variant> = TRUE, the maximum value is
            // returned" (Microsoft Learn, MAXX and MINX). Qty is 2, 1, 3, 1, 4, 5, 1.
            testCase "MAXX and MINX over text mixed with numbers" (fun () ->
                is (p ()) "MAXX ( Sales, IF ( Sales[Qty] = 5, \"x\", Sales[Qty] ) )" 4
                is (p ()) "MINX ( Sales, IF ( Sales[Qty] = 1, \"x\", Sales[Qty] ) )" 2
                is (p ()) "MAXX ( Sales, Sales[Qty], TRUE )" 5
                is (p ()) "MAXX ( Sales, Sales[Qty], FALSE )" 5
                refused "MAXX ( Sales, IF ( Sales[Qty] = 5, \"x\", Sales[Qty] ), TRUE )" "third argument TRUE")
        ]

        testList "math" [
            // dax.guide/round: decimal places, the nearest integer, left of the decimal point.
            testCase "ROUND" (fun () ->
                is (p ()) "ROUND ( 2.675, 2 )" 2.68
                is (p ()) "ROUND ( 2.5, 0 )" 3.0
                // "rounds 2.15 up, to one decimal place. The expected result is 2.2" (Microsoft Learn).
                is (p ()) "ROUND ( 2.15, 1 )" 2.2
                is (p ()) "ROUND ( -2.4, 0 )" -2.0
                is (p ()) "ROUND ( 21.5, -1 )" 20.0
                // The pages don't say which way a negative number halfway goes.
                isError "not-supported" "ROUND ( -2.5, 0 )"
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
                is (p ()) "COUNTAX ( Sales, IF ( Sales[Qty] > 2, \"big\" ) )" 3
                // A True/False IF is never BLANK (see "IF with a BLANK branch"): all 7 rows count.
                is (p ()) "COUNTAX ( Sales, IF ( Sales[Qty] > 2, TRUE ) )" 7
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
            // Without an order-by the engine doesn't define the order, so the result is given only
            // when no order could change it: "The result is BLANK when Table has no rows"
            // (dax.guide/concatenatex), or every text is the same.
            testCase "CONCATENATEX without an order is refused unless the order can't matter" (fun () ->
                isError "not-supported" "CONCATENATEX ( VALUES ( Customer[Name] ), Customer[Name] )"
                is (p ()) "CONCATENATEX ( FILTER ( Customer, FALSE ), Customer[Name], \",\" )" null
                // Two US customers: "US" twice.
                is
                    (p ())
                    "CONCATENATEX ( FILTER ( Customer, Customer[Country] = \"US\" ), Customer[Country], \"/\" )"
                    "US/US"

                is (p ()) "CONCATENATEX ( { 7 }, [Value] )" "7")
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
