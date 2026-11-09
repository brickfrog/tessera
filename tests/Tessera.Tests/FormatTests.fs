module Tessera.Tests.FormatTests

open System
open System.IO
open System.Text.RegularExpressions
open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Dax
open Tessera.Format
open Tessera.Tests.Fixtures

let private defaults = FormatOptions.defaults

let private fmtWith (opts: FormatOptions) (dax: string) =
    match format opts dax with
    | Ok s -> s
    | Error e -> failtestf "format failed: %s\ninput:\n%s" (FormatError.message e) dax

let private fmt = fmtWith defaults

/// Lines joined with LF, so expected outputs read like the DAX they are.
let private lines (xs: string list) = String.concat "\n" xs

let private golden (opts: FormatOptions) (input: string) (expected: string list) =
    let actual = fmtWith opts input
    Expect.equal actual (lines expected) $"formatting:\n{input}"
    Expect.equal (fmtWith opts actual) actual "formatting the result again changes nothing"

let private syntaxErrors (opts: FormatOptions) (dax: string) =
    match format opts dax with
    | Error(Syntax problems) -> problems
    | Error(Unsafe reason) -> failtestf "expected a syntax error, got a refusal: %s" reason
    | Ok s -> failtestf "expected a syntax error, got:\n%s" s

// ---- An independent check of what the formatter promises ---------------------------------------

/// The significant tokens as raw source text, and the comments, of some DAX.
let private shape (dax: string) =
    let toks = tokenize dax

    let code =
        toks
        |> List.filter (Token.isTrivia >> not)
        |> List.map (fun t -> t.Kind, dax.Substring(t.Start, t.Length))

    let comments =
        toks
        |> List.filter (fun t -> t.Kind = TokenKind.Comment)
        |> List.map (fun t -> t.Text.TrimEnd())

    code, comments

/// `after` has `before`'s tokens (identifiers may differ in case only) and comments, in order.
let private sameMeaning (before: string) (after: string) =
    let codeA, commentsA = shape before
    let codeB, commentsB = shape after

    codeA.Length = codeB.Length
    && List.forall2
        (fun (ka, ta: string) (kb, tb: string) ->
            ka = kb
            && (ta = tb
                || (ka = TokenKind.Identifier
                    && String.Equals(ta, tb, StringComparison.OrdinalIgnoreCase))))
        codeA
        codeB
    && commentsA = commentsB

/// Formatting is safe and settles: the result means the same, and formatting it changes nothing.
let private settles (opts: FormatOptions) (dax: string) =
    match format opts dax with
    | Ok once ->
        sameMeaning dax once
        && (match format opts once with
            | Ok twice -> twice = once
            | Error _ -> false)
    | Error(Syntax _) -> true
    | Error(Unsafe _) -> false

// ---- Golden tests: SQLBI's rules -----------------------------------------------------------------

/// The example from https://www.sqlbi.com/articles/rules-for-dax-code-formatting/, unformatted.
let private sqlbiExample =
    "calculate(sumx(Orders,Orders[Amount]),filter(all(Customers),calculate(countrows(Sales),all(Calendar[Date]))"
    + ">42+8-25*(3-1)+2-1+2-1+calculate(2+2-2+2-2)-calculate(4)))"

[<Tests>]
let goldenTests =
    testList "Format: SQLBI rules" [
        testCase "the article's example, --long: arguments of multi-argument calls on their own lines"
        <| fun () ->
            golden { defaults with Long = true } sqlbiExample [
                "CALCULATE ("
                "    SUMX ("
                "        Orders,"
                "        Orders[Amount]"
                "    ),"
                "    FILTER ("
                "        ALL ( Customers ),"
                "        CALCULATE ("
                "            COUNTROWS ( Sales ),"
                "            ALL ( Calendar[Date] )"
                "        ) > 42 + 8 - 25 * ( 3 - 1 ) + 2 - 1 + 2 - 1"
                "            + CALCULATE ( 2 + 2 - 2 + 2 - 2 )"
                "            - CALCULATE ( 4 )"
                "    )"
                ")"
            ]

        testCase "the article's example, default: calls that fit stay on one line"
        <| fun () ->
            golden defaults sqlbiExample [
                "CALCULATE ("
                "    SUMX ( Orders, Orders[Amount] ),"
                "    FILTER ("
                "        ALL ( Customers ),"
                "        CALCULATE ( COUNTROWS ( Sales ), ALL ( Calendar[Date] ) )"
                "            > 42 + 8 - 25 * ( 3 - 1 ) + 2 - 1 + 2 - 1"
                "                + CALCULATE ( 2 + 2 - 2 + 2 - 2 )"
                "                - CALCULATE ( 4 )"
                "    )"
                ")"
            ]

        testCase "function names upper case, a space before '(' and inside the parentheses"
        <| fun () ->
            golden defaults "sumx(Sales,Sales[Quantity]*Sales[Net Price])" [
                "SUMX ( Sales, Sales[Quantity] * Sales[Net Price] )"
            ]

            golden defaults "blank()" [ "BLANK ()" ]

        testCase "--no-space-after-function"
        <| fun () ->
            golden
                {
                    defaults with
                        SpaceAfterFunction = false
                }
                "divide(sum(Sales[Amount]), countrows(Sales))"
                [ "DIVIDE( SUM( Sales[Amount] ), COUNTROWS( Sales ) )" ]

            golden
                {
                    defaults with
                        SpaceAfterFunction = false
                }
                "blank()"
                [ "BLANK()" ]

        testCase "a call that doesn't fit: arguments indented 4, commas at line end, ')' under the function"
        <| fun () ->
            golden
                { defaults with Width = 60 }
                "if(isblank([Sales Amount]),blank(),divide([Sales Amount]-[Sales Amount PY],[Sales Amount PY]))"
                [
                    "IF ("
                    "    ISBLANK ( [Sales Amount] ),"
                    "    BLANK (),"
                    "    DIVIDE ("
                    "        [Sales Amount] - [Sales Amount PY],"
                    "        [Sales Amount PY]"
                    "    )"
                    ")"
                ]

        testCase "--long splits every call with two or more arguments, or a call as its only argument"
        <| fun () ->
            golden { defaults with Long = true } "divide(sum(Sales[Amount]), countrows(Sales))" [
                "DIVIDE ("
                "    SUM ( Sales[Amount] ),"
                "    COUNTROWS ( Sales )"
                ")"
            ]

            golden { defaults with Long = true } "countrows(values(Sales[Key]))" [
                "COUNTROWS ("
                "    VALUES ( Sales[Key] )"
                ")"
            ]

            golden { defaults with Long = true } "all(Customers) + calculate(4)" [
                "ALL ( Customers ) + CALCULATE ( 4 )"
            ]

        testCase "operators are spaced; a split chain starts each line with its operator"
        <| fun () ->
            golden defaults "1+2*3-4/5^6&\"x\"" [ "1 + 2 * 3 - 4 / 5 ^ 6 & \"x\"" ]

            golden
                defaults
                "'T'[a] && 'T'[b] && 'T'[ccccccccccccccccccccccccc] = 1 && 'T'[dddddddddddddddddddddddd] = 2 && 'T'[eeeeeeeeeeeeeeeeeee] = 3"
                [
                    "'T'[a]"
                    "    && 'T'[b]"
                    "    && 'T'[ccccccccccccccccccccccccc] = 1"
                    "    && 'T'[dddddddddddddddddddddddd] = 2"
                    "    && 'T'[eeeeeeeeeeeeeeeeeee] = 3"
                ]

        testCase "arithmetic packs operands while they fit"
        <| fun () ->
            golden
                defaults
                "[Sales Amount]+[Sales Amount PY]+[Sales Amount PY]+[Sales Amount PY]+[Sales Amount PY]+[Sales Amount PY]+[Sales Amount PY]"
                [
                    "[Sales Amount] + [Sales Amount PY] + [Sales Amount PY] + [Sales Amount PY] + [Sales Amount PY]"
                    "    + [Sales Amount PY] + [Sales Amount PY]"
                ]

        testCase "after a multi-line operand, the operator continues its closing line"
        <| fun () ->
            golden
                { defaults with Width = 60 }
                "sumx('Budget Rate', (calculate(sum('Invoiced Sales'[Value])) - calculate(sum('Invoiced Sales'[COGS]))) / 'Budget Rate'[Rate])"
                [
                    "SUMX ("
                    "    'Budget Rate',"
                    "    ("
                    "        CALCULATE ( SUM ( 'Invoiced Sales'[Value] ) )"
                    "            - CALCULATE ( SUM ( 'Invoiced Sales'[COGS] ) )"
                    "    ) / 'Budget Rate'[Rate]"
                    ")"
                ]

        testCase "keywords upper case; VAR values that fit stay on the VAR's line; RETURN on its own line"
        <| fun () ->
            golden
                defaults
                "var x = sum(Sales[Amount]) var y = calculate(sum(Sales[Amount]), all('Date')) return if(not isblank(y) && true, divide(x, y), false)"
                [
                    "VAR x = SUM ( Sales[Amount] )"
                    "VAR y = CALCULATE ( SUM ( Sales[Amount] ), ALL ( 'Date' ) )"
                    "RETURN"
                    "    IF ( NOT ISBLANK ( y ) && TRUE, DIVIDE ( x, y ), FALSE )"
                ]

        testCase "a VAR value that doesn't fit goes on the next line, indented"
        <| fun () ->
            golden
                { defaults with Width = 40 }
                "VAR Result = CALCULATE([Sales Amount], 'Date'[Year] = 2024) RETURN Result"
                [
                    "VAR Result ="
                    "    CALCULATE ("
                    "        [Sales Amount],"
                    "        'Date'[Year] = 2024"
                    "    )"
                    "RETURN"
                    "    Result"
                ]

        testCase "SWITCH keeps value/result pairs together"
        <| fun () ->
            golden
                defaults
                "switch(true(), [m] > 100, \"High\", [m] > 50, \"Medium\", [m] > 25, \"Low-ish but still something\", \"Low\")"
                [
                    "SWITCH ("
                    "    TRUE (),"
                    "    [m] > 100, \"High\","
                    "    [m] > 50, \"Medium\","
                    "    [m] > 25, \"Low-ish but still something\","
                    "    \"Low\""
                    ")"
                ]

        testCase "names, quoting, strings and quoted identifiers are kept exactly"
        <| fun () ->
            golden defaults "'Sales Table'[Total  Amount] + Sales[x] + \"a  b, (c)\" & 'O''Brien'[It]]s]" [
                "'Sales Table'[Total  Amount] + Sales[x] + \"a  b, (c)\" & 'O''Brien'[It]]s]"
            ]

        testCase "a table name and its column are never separated"
        <| fun () -> golden defaults "SUM ( 'Sales' [Amount] ) + Sales  [x]" [ "SUM ( 'Sales'[Amount] ) + Sales[x]" ]

        testCase "a DEFINE'd measure is on the line before its expression"
        <| fun () ->
            golden
                defaults
                "define measure Sales[Total] = sum(Sales[Amount]) var q = 2 evaluate summarizecolumns('Date'[Year], \"Total\", [Total]) order by 'Date'[Year] desc start at 2020"
                [
                    "DEFINE"
                    "    MEASURE Sales[Total] ="
                    "        SUM ( Sales[Amount] )"
                    "    VAR q = 2"
                    "EVALUATE"
                    "SUMMARIZECOLUMNS ( 'Date'[Year], \"Total\", [Total] )"
                    "ORDER BY 'Date'[Year] DESC"
                    "START AT 2020"
                ]

        testCase "--width changes where lines break"
        <| fun () ->
            let dax = "CALCULATE([Sales Amount], 'Date'[Year] = 2024)"
            golden defaults dax [ "CALCULATE ( [Sales Amount], 'Date'[Year] = 2024 )" ]

            golden { defaults with Width = 30 } dax [
                "CALCULATE ("
                "    [Sales Amount],"
                "    'Date'[Year] = 2024"
                ")"
            ]

        testCase "CRLF input gives CRLF output"
        <| fun () ->
            let out = fmt "IF(a,\r\n  b, c)"
            Expect.equal out "IF ( a, b, c )" ""
            let out = fmt "IF(a, // why\r\n  b, c)"
            Expect.equal out "IF (\r\n    a, // why\r\n    b,\r\n    c\r\n)" ""
    ]

// ---- Comments -----------------------------------------------------------------------------------

[<Tests>]
let commentTests =
    testList "Format: comments stay in place" [
        testCase "a comment after a token stays on its line"
        <| fun () -> golden defaults "IF(a, // why\nb, c)" [ "IF ("; "    a, // why"; "    b,"; "    c"; ")" ]

        testCase "a comment after an argument moves past its comma"
        <| fun () -> golden defaults "F(a // after a\n, b)" [ "F ("; "    a, // after a"; "    b"; ")" ]

        testCase "comments on lines of their own stay on lines of their own, blank lines before them too"
        <| fun () ->
            golden defaults "// header\n\n// why\nDIVIDE([Gross Profit], [Revenue])" [
                "// header"
                ""
                "// why"
                "DIVIDE ( [Gross Profit], [Revenue] )"
            ]

        testCase "comments on both sides of a comma keep their order"
        <| fun () ->
            golden defaults "{ a\n/* own */\n, -- dash\n b }" [
                "{"
                "    a,"
                "    /* own */"
                "    -- dash"
                "    b"
                "}"
            ]

        testCase "an inline block comment stays inline"
        <| fun () -> golden defaults "SUM(x) /* inline */ + 1" [ "SUM ( x ) /* inline */ + 1" ]

        testCase "a comment before ')' stays inside the parentheses"
        <| fun () -> golden defaults "F(\n  a\n  // the end\n)" [ "F ("; "    a"; "    // the end"; ")" ]

        testCase "a comment at the end, and comment-only input"
        <| fun () ->
            golden defaults "x // trailing" [ "x // trailing" ]
            golden defaults "1\n-- done" [ "1"; "-- done" ]
            golden defaults "// nothing here" [ "// nothing here" ]

        testCase "blank lines between VARs are kept (one at most)"
        <| fun () ->
            golden defaults "VAR a = 1\n\n\nVAR b = 2\n\nRETURN a + b" [
                "VAR a = 1"
                ""
                "VAR b = 2"
                ""
                "RETURN"
                "    a + b"
            ]

        testCase "a multi-line block comment and a multi-line string are kept verbatim"
        <| fun () ->
            golden defaults "IF(a, /* one\n   two */ b, \"x\n y\")" [
                "IF ("
                "    a, /* one"
                "   two */"
                "    b,"
                "    \"x"
                " y\""
                ")"
            ]
    ]

// ---- Regressions: known formatter bugs elsewhere -----------------------------------------------

[<Tests>]
let regressionTests =
    testList "Format: regression corpus" [
        testCase "UDF parameters: '=' defaults stay '=', ':' types stay ':'"
        <| fun () ->
            golden defaults "(p = [m], x: int64, t : table expr) => x * 2" [
                "( p = [m], x: INT64, t: TABLE EXPR ) => x * 2"
            ]

        testCase "DEFINE MEASURE keeps its table qualifier"
        <| fun () ->
            golden defaults "DEFINE MEASURE 'Nope'[M] = 1 EVALUATE {[M]}" [
                "DEFINE"
                "    MEASURE 'Nope'[M] ="
                "        1"
                "EVALUATE"
                "{ [M] }"
            ]

        testCase "multi-word keywords stay together: ASC BLANKS FIRST, NOT IN"
        <| fun () ->
            golden defaults "ORDERBY([x] asc blanks first)" [ "ORDERBY ( [x] ASC BLANKS FIRST )" ]
            golden defaults "'T'[c] not in {1,2}" [ "'T'[c] NOT IN { 1, 2 }" ]

        testCase "ROWS and COLUMNS"
        <| fun () ->
            golden defaults "runningsum([Sales], rows columns) + collapse([Sales], rows)" [
                "RUNNINGSUM ( [Sales], ROWS COLUMNS ) + COLLAPSE ( [Sales], ROWS )"
            ]

        testCase "--semicolons: ';' separators and ',' decimals, written back the same way"
        <| fun () ->
            let semi = { defaults with Semicolons = true }
            golden semi "IF([a] > 1,5;2,25;{1,5; 3})" [ "IF ( [a] > 1,5; 2,25; { 1,5; 3 } )" ]

            golden { semi with Width = 20 } "DIVIDE([Sales Amount];[Quantity] * 0,5)" [
                "DIVIDE ("
                "    [Sales Amount];"
                "    [Quantity] * 0,5"
                ")"
            ]

        testCase "a ';' without --semicolons, or a stray ',' with it, is an error (never reinterpreted)"
        <| fun () ->
            let ps = syntaxErrors defaults "IF([a] > 1,5; 2,25; 3)"
            Expect.equal (ps |> List.map (fun p -> p.Column)) [ 13; 19 ] ""
            Expect.stringContains ps.Head.Message "--semicolons" ""
            let ps = syntaxErrors { defaults with Semicolons = true } "IF([a], 1)"
            Expect.equal (ps.Head.Line, ps.Head.Column) (1, 7) ""

        testCase "lexemes the lexer splits stay whole: ==, - - (not a comment)"
        <| fun () ->
            golden defaults "a==b" [ "a == b" ]
            golden defaults "1 - -2 * - -[x]" [ "1 - -2 * - -[x]" ]

        testCase "built-in calls are upper-cased; UDF calls and names keep their case"
        <| fun () ->
            golden
                defaults
                "DaxLib.Format.Percent(sum(Sales[x])) + addTax(1) + info.tables() + COUNTROWS(Table) + Date[Date]"
                [
                    // Operands with calls start lines of their own once the chain is split.
                    "DaxLib.Format.Percent ( SUM ( Sales[x] ) )"
                    "    + addTax ( 1 )"
                    "    + INFO.TABLES ()"
                    "    + COUNTROWS ( Table ) + Date[Date]"
                ]

        testCase "contextual keywords are upper-cased only where they're keywords"
        <| fun () ->
            golden defaults "dateadd('Date'[Date], -1, year) + calculate([m], Year = 2020)" [
                "DATEADD ( 'Date'[Date], -1, YEAR ) + CALCULATE ( [m], Year = 2020 )"
            ]

            golden defaults "VAR Year = 1 RETURN Year + rankx(all(T), [m], , desc, dense)" [
                "VAR Year = 1"
                "RETURN"
                "    Year + RANKX ( ALL ( T ), [m], , DESC, DENSE )"
            ]

        testCase "empty arguments and empty constructors"
        <| fun () ->
            golden defaults "F(a,,b)" [ "F ( a, , b )" ]
            golden defaults "{}" [ "{}" ]
            golden defaults "" [ "" ]

        testCase "DEFINE FUNCTION"
        <| fun () ->
            golden defaults "DEFINE FUNCTION AddTax = (amount : NUMERIC) => amount * 1.1 EVALUATE {AddTax(10)}" [
                "DEFINE"
                "    FUNCTION AddTax ="
                "        ( amount: NUMERIC ) => amount * 1.1"
                "EVALUATE"
                "{ AddTax ( 10 ) }"
            ]
    ]

// ---- Errors -------------------------------------------------------------------------------------

[<Tests>]
let errorTests =
    testList "Format: malformed input" [
        testCase "errors say where (1-based line and column)"
        <| fun () ->
            let at dax =
                syntaxErrors defaults dax |> List.map (fun p -> p.Line, p.Column, p.Message)

            Expect.equal (at "SUM(x") [ 1, 4, "'(' is never closed" ] ""
            Expect.equal (at "1 +\n  )") [ 2, 3, "')' without a matching '('" ] ""
            Expect.equal (at "\"abc") [ 1, 1, "unterminated string" ] ""
            Expect.equal (at "1 +") [ 1, 3, "expected an expression after '+'" ] ""
            Expect.equal (at "VAR x = 1") [ 1, 1, "VAR without RETURN" ] ""
            Expect.equal (at "RETURN 1") [ 1, 1, "unexpected 'RETURN'" ] ""
            Expect.equal (at "/* open") [ 1, 1, "unterminated /* comment" ] ""
            Expect.equal (at "[a b") [ 1, 1, "unterminated [name]" ] ""
            Expect.equal (at "(1, 2}") [ 1, 6, "'}' doesn't match the '(' at line 1, column 1" ] ""
            Expect.equal (at "a​b") [ 1, 2, "unexpected character '​' (U+200B)" ] ""
    ]

// ---- Properties ---------------------------------------------------------------------------------

/// DAX built from a small grammar, with odd whitespace, comments and case mixed in.
type FormatGen =
    static member Dax() =
        let ws =
            Gen.elements [
                " "
                " "
                " "
                "\n"
                "\n    "
                "\t"
                " // c\n"
                " /* c */ "
                "\n\n"
                "\n// own line\n"
            ]

        let ws0 = Gen.oneof [ Gen.constant ""; ws ]

        let atom =
            Gen.elements [
                "[Amount]"
                "[Total Sales]"
                "'Sales'[Total]"
                "Sales[Amount]"
                "'Date'[Month]"
                "Sales"
                "'Sales Table'"
                "1"
                "0.5"
                "1e3"
                "\"x\""
                "\"a, b\""
                "\"\""
                "TRUE"
                "false"
                "BLANK()"
                "a"
                "_x"
                "DT\"2024-01-01\""
            ]

        let name = Gen.elements [ "a"; "b"; "_x"; "Result" ]

        let fn =
            Gen.elements [
                "SUM"
                "calculate"
                "If"
                "SUMX"
                "COUNTROWS"
                "DIVIDE"
                "SWITCH"
                "FILTER"
                "DaxLib.Format"
                "MyFunc"
            ]

        let rec expr (size: int) : Gen<string> =
            if size <= 0 then
                atom
            else
                let sub = expr (size / 2)

                Gen.oneof [
                    atom
                    gen {
                        let! l = sub
                        let! a = ws

                        let! op =
                            Gen.elements [ "+"; "*"; "/"; "-"; "="; "||"; "&&"; "&"; "<>"; ">="; "^"; "IN"; "not in" ]

                        let! b = ws
                        let! r = sub
                        return l + a + op + b + r
                    }
                    gen {
                        let! f = fn
                        let! n = Gen.choose (0, 4)
                        let! args = Gen.listOfLength n sub
                        let! a = ws0
                        let! b = ws0
                        return f + "(" + a + String.concat ("," + b) args + ")"
                    }
                    gen {
                        let! vars = Gen.listOfLength 2 (Gen.zip name sub)
                        let! seps = Gen.listOfLength 3 ws
                        let! body = sub
                        let! var = Gen.elements [ "VAR"; "var" ]

                        let defs =
                            List.zip vars (List.take 2 seps)
                            |> List.map (fun ((n, e), s) -> $"{var} {n} = {e}{s}")
                            |> String.concat ""

                        return defs + "RETURN" + seps.[2] + body
                    }
                    gen {
                        let! e = sub
                        let! s = ws0
                        return "(" + s + e + ")"
                    }
                    gen {
                        let! xs = Gen.listOfLength 3 sub
                        return "{" + String.concat ", " xs + "}"
                    }
                    gen {
                        let! sign = Gen.elements [ "-"; "NOT "; "+" ]
                        let! e = atom
                        return sign + e
                    }
                    gen {
                        let! e = sub
                        let! w = Gen.elements [ " ASC"; " DESC"; " asc blanks last" ]
                        return "ORDERBY(" + e + w + ")"
                    }
                ]

        let query =
            gen {
                let! m = expr 4
                let! e = expr 6
                let! o = expr 2
                return $"DEFINE MEASURE 'T'[M] = {m} EVALUATE {{ {e} }} ORDER BY {o} DESC"
            }

        Gen.sized (fun n -> Gen.frequency [ 9, expr (min n 14); 1, query ])
        |> Arb.fromGen

/// Random option sets: width, --long, --no-space-after-function.
type FormatOptionsGen =
    static member Options() =
        gen {
            let! width = Gen.choose (10, 140)
            let! long = Gen.elements [ true; false ]
            let! space = Gen.elements [ true; false ]

            return {
                defaults with
                    Width = width
                    Long = long
                    SpaceAfterFunction = space
            }
        }
        |> Arb.fromGen

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<FormatGen>; typeof<FormatOptionsGen> ]
        maxTest = 500
}

/// Token soup: DAX tokens in arbitrary order, so malformed input of every shape.
let private soup =
    Gen.elements [
        "("
        ")"
        "{"
        "}"
        ","
        "VAR"
        "RETURN"
        "x"
        "="
        "+"
        "-"
        "*"
        "IN"
        "NOT"
        "SUM"
        "[a]"
        "'T'"
        "1"
        "\"s\""
        "// c\n"
        "/* c */"
        "\n"
        "DEFINE"
        "EVALUATE"
        "MEASURE"
        "ORDER"
        "BY"
        "=>"
        ":"
        "ASC"
    ]
    |> Gen.listOf
    |> Gen.map (String.concat " ")
    |> Arb.fromGen

[<Tests>]
let propertyTests =
    testList "Format properties" [
        testPropertyWithConfig config "formatting keeps the tokens and comments, and formatting again changes nothing"
        <| fun (dax: string) (opts: FormatOptions) ->
            match format opts dax with
            | Ok once -> sameMeaning dax once && format opts once = Ok once
            | Error e -> failtestf "%s\n%s" (FormatError.message e) dax

        testPropertyWithConfig config "comments and line breaks between any two tokens stay, and settle"
        <| fun (dax: string) (seed: int) (opts: FormatOptions) ->
            let rnd = Random(seed)

            let gaps = [| " /* c */ "; "\n// line\n"; "\n\n"; " -- dash\n"; "\n/* own */\n"; " " |]

            let sprinkled =
                tokenize dax
                |> List.map (fun t ->
                    let text = dax.Substring(t.Start, t.Length)

                    if Token.isTrivia t then text
                    elif rnd.Next 3 = 0 then text + gaps.[rnd.Next gaps.Length]
                    else text)
                |> String.concat ""

            match format opts sprinkled with
            | Ok once -> sameMeaning sprinkled once && format opts once = Ok once
            | Error e -> failtestf "%s\n%s" (FormatError.message e) sprinkled

        testPropertyWithConfig config "line endings: CRLF input formats like LF input"
        <| fun (dax: string) ->
            let lf = fmt dax
            let crlf = fmtWith defaults (dax.Replace("\n", "\r\n"))
            crlf = (if dax.Contains "\n" then lf.Replace("\n", "\r\n") else lf)

        testProperty "any text: never throws, and what it formats is safe and settles"
        <| fun (s: string) -> settles defaults (if isNull s then "" else s)

        testProperty "token soup: never throws, and what it formats is safe and settles"
        <| Prop.forAll soup (fun s ->
            settles defaults s
            && settles
                {
                    defaults with
                        Long = true
                        Width = 20
                }
                s)
    ]

// ---- Every expression in the test fixtures ------------------------------------------------------

/// The DAX in this project's test sources: every `Expression = "..."` literal.
let private fixtureExpressions =
    lazy
        (let unescape (s: string) =
            Regex.Replace(
                s,
                @"\\(u[0-9A-Fa-f]{4}|.)",
                fun m ->
                    let e = m.Groups.[1].Value

                    match e.[0] with
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | 'u' -> string (char (Convert.ToInt32(e.Substring 1, 16)))
                    | c -> string c
            )

         Directory.GetFiles(__SOURCE_DIRECTORY__, "*.fs")
         |> Array.collect (fun f ->
             Regex.Matches(File.ReadAllText f, "Expression = \"((?:[^\"\\\\]|\\\\.)*)\"")
             |> Seq.map (fun m -> unescape m.Groups.[1].Value)
             |> Array.ofSeq)
         |> Array.distinct
         |> List.ofArray)

let private optionSets = [
    defaults
    { defaults with Long = true }
    {
        defaults with
            SpaceAfterFunction = false
    }
    { defaults with Width = 30 }
]

[<Tests>]
let corpusTests =
    testList "Format: fixture corpus" [
        testCase "every DAX expression in the sample model formats safely, and settles"
        <| fun () ->
            let sources = Deps.daxSources (Inventory.objects (sampleDatabase ()))
            Expect.isNonEmpty sources ""

            for src in sources do
                for opts in optionSets do
                    Expect.isTrue (settles opts src.Dax) src.Dax
                    Expect.isOk (format opts src.Dax) src.Dax

        testCase "every Expression = \"...\" in the test sources formats safely, and settles"
        <| fun () ->
            let corpus = fixtureExpressions.Force()
            Expect.isGreaterThan corpus.Length 50 "the corpus was found"

            for dax in corpus do
                for opts in optionSets do
                    Expect.isTrue (settles opts dax) dax

            // Some fixtures are malformed on purpose; nearly all are DAX.
            let formatted = corpus |> List.filter (fun d -> Result.isOk (format defaults d))
            Expect.isGreaterThan (float formatted.Length) (0.9 * float corpus.Length) "most fixtures format"
    ]

// ---- Models -------------------------------------------------------------------------------------

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private modelWith (measures: (string * string) list) =
    let db = sampleDatabase ()

    for name, expr in measures do
        db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = name, Expression = expr))

    db

let private changedRefs (result: ModelFormat) =
    result.Outcomes
    |> List.filter Outcome.changed
    |> List.map (fun o -> ObjectRef.format o.Source.Owner.Ref, o.Source.Property)

[<Tests>]
let modelTests =
    testList "Format: models" [
        testCase "formats the model's DAX, leaves M alone, and a second run changes nothing"
        <| fun () ->
            let db = modelWith [ "Ugly", "sum(Sales[Amount])+1" ]
            let result = formatModel defaults db Selection.all |> ok

            Expect.containsAll
                (changedRefs result)
                [
                    "measure:Sales/Total", "expression"
                    "measure:Sales/Ugly", "expression"
                    "calcitem:Time Intelligence/YTD", "expression"
                    "function:Double", "expression"
                ]
                ""

            Expect.isTrue (result.Outcomes |> List.forall (fun o -> Result.isOk o.Result)) "no failures"
            let before = Deps.build db
            apply result.Outcomes
            Expect.equal db.Model.Tables.["Sales"].Measures.["Ugly"].Expression "SUM ( Sales[Amount] ) + 1" ""
            Expect.equal db.Model.Functions.["Double"].Expression "( x: INT64 ) => x * 2" ""

            let mPartition =
                db.Model.Tables.["Sales"].Partitions.[0].Source :?> MPartitionSource

            Expect.equal mPartition.Expression "let x = 1 in x" "M untouched"

            let after = Deps.build db

            let targets (g: Deps.Graph) =
                g.References |> List.map (fun r -> r.From, r.Target, r.Via) |> List.sort

            Expect.equal (targets after) (targets before) "every reference resolves as before"
            Expect.isEmpty (changedRefs (formatModel defaults db Selection.all |> ok)) "settled"

        testCase "selectors, kinds and tables narrow what's formatted"
        <| fun () ->
            let db = modelWith [ "Ugly", "sum(Sales[Amount])+1" ]

            let refsOf sel =
                formatModel defaults db sel
                |> ok
                |> fun r -> r.Outcomes |> List.map (fun o -> ObjectRef.format o.Source.Owner.Ref)

            Expect.equal
                (refsOf {
                    Selection.all with
                        Selectors = [ "[Ugly]" ]
                })
                [ "measure:Sales/Ugly" ]
                ""

            Expect.equal
                (refsOf {
                    Selection.all with
                        Kinds = [ ObjectKind.Function ]
                })
                [ "function:Double" ]
                ""

            Expect.equal
                (refsOf {
                    Selection.all with
                        Tables = [ "time intelligence" ]
                })
                [ "calcitem:Time Intelligence/Current"; "calcitem:Time Intelligence/YTD" ]
                ""

            Expect.equal
                (refsOf {
                    Selection.all with
                        Selectors = [ "Sales" ]
                })
                [ "measure:Sales/Total"; "measure:Sales/Total (hidden)"; "measure:Sales/Ugly" ]
                "a table stands for the expressions in it"

        testCase "a selector without DAX is reported; one that doesn't resolve is an error"
        <| fun () ->
            let db = sampleDatabase ()

            let result =
                formatModel defaults db {
                    Selection.all with
                        Selectors = [ "Sales[Amount]" ]
                }
                |> ok

            Expect.isEmpty result.Outcomes ""
            Expect.equal result.WithoutDax [ "Sales[Amount]" ] ""

            Expect.isError
                (formatModel defaults db {
                    Selection.all with
                        Selectors = [ "[Nope]" ]
                })
                ""

        testCase "an expression that can't be parsed is left alone, with its error"
        <| fun () ->
            let db = modelWith [ "Broken", "SUM(Sales[Amount]" ]

            let result =
                formatModel defaults db {
                    Selection.all with
                        Selectors = [ "[Broken]" ]
                }
                |> ok

            match result.Outcomes with
            | [ { Result = Error(Syntax [ p ]) } ] -> Expect.equal (p.Line, p.Column) (1, 4) ""
            | other -> failtestf "%A" other

            apply result.Outcomes
            Expect.equal db.Model.Tables.["Sales"].Measures.["Broken"].Expression "SUM(Sales[Amount]" "unchanged"

        testCase "names that look like keywords keep their case: model tables and UDFs"
        <| fun () ->
            let db = sampleDatabase ()
            db.Model.Tables.Add(new Table(Name = "Year"))

            db.Model.Tables.["Year"].Columns.Add(
                new DataColumn(Name = "Y", DataType = DataType.Int64, SourceColumn = "Y")
            )

            db.Model.Functions.Add(new Function(Name = "Sum2", Expression = "(x: NUMERIC) => x"))
            db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "K", Expression = "COUNTROWS(Year) + Sum2(1)"))

            let result =
                formatModel defaults db {
                    Selection.all with
                        Selectors = [ "[K]" ]
                }
                |> ok

            Expect.equal result.Outcomes.Head.Result (Ok "COUNTROWS ( Year ) + Sum2 ( 1 )") ""

        testCase "a calculated table with two partitions is formatted once"
        <| fun () ->
            let db = sampleDatabase ()
            let t = new Table(Name = "Calc")

            for i in 1..2 do
                t.Partitions.Add(
                    new Partition(Name = $"P{i}", Source = new CalculatedPartitionSource(Expression = "{1,2}"))
                )

            db.Model.Tables.Add t

            let result =
                formatModel defaults db {
                    Selection.all with
                        Selectors = [ "Calc" ]
                }
                |> ok

            Expect.equal result.Outcomes.Length 1 ""
            apply result.Outcomes
            let first = t.Partitions.[0].Source :?> CalculatedPartitionSource
            Expect.equal first.Expression "{ 1, 2 }" ""
    ]

// ---- A real model (TESSERA_SAMPLE_MODEL) --------------------------------------------------------

[<Tests>]
let sampleTests =
    testList "Format: sample model (TESSERA_SAMPLE_MODEL)" [
        match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
        | null
        | "" ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | path ->
            testCase "every expression formats safely and settles; references and validation are unchanged"
            <| fun () ->
                let _, db = ModelIO.loadPath path |> ok
                let sources = Deps.daxSources (Inventory.objects db)

                for src in sources do
                    for opts in optionSets do
                        Expect.isTrue (settles opts src.Dax) $"{ObjectRef.format src.Owner.Ref} {src.Property}"

                let unresolved = Deps.unresolved (Deps.build db) |> List.length
                let errors = Validate.validate db |> List.length
                let result = formatModel defaults db Selection.all |> ok

                let failures =
                    result.Outcomes
                    |> List.filter (fun o -> Result.isError o.Result)
                    |> List.map (fun o -> ObjectRef.format o.Source.Owner.Ref)

                Expect.isEmpty failures "every expression formats"
                apply result.Outcomes
                Expect.equal (Deps.unresolved (Deps.build db) |> List.length) unresolved "unresolved references"
                Expect.equal (Validate.validate db |> List.length) errors "validation issues"

                Expect.isEmpty
                    (changedRefs (formatModel defaults db Selection.all |> ok))
                    "a second run changes nothing"
    ]
