module Tessera.Tests.DaxParserTests

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading
open Expecto
open FsCheck
open FsCheck.FSharp
open Tessera
open Tessera.Dax
open Tessera.DaxAst
open Tessera.DaxParser
open Tessera.DaxPrinter

// ---- Helpers ------------------------------------------------------------------------------------

let private messages (errors: ParseError list) =
    errors |> List.map ParseError.format |> String.concat "; "

let private parsed (dax: string) : Expr =
    match parseExpression dax with
    | Ok e -> e
    | Error errors -> failtestf "%s\n  does not parse: %s" dax (messages errors)

/// The tree, without positions.
let private shape (dax: string) : Expr = stripSpans (parsed dax)

let private errorsOf (dax: string) : string list =
    match parse dax with
    | Ok _ -> failtestf "%s\n  parses, but an error was expected" dax
    | Error errors -> errors |> List.map ParseError.format

let private k (kind: ExprKind) : Expr = node kind
let private int' (v: int) = k (IntegerLiteral(int64 v, string v))
let private str (s: string) = k (StringLiteral s)
let private name (s: string) = k (Identifier s)
let private br (s: string) = k (Bracketed s)

let private ident (s: string) : Ident = { Name = s; Span = Span.none }

let private tbl (s: string) (quoted: bool) : TableName = {
    Name = s
    Quoted = quoted
    Span = Span.none
}

let private col (t: string) (c: string) = k (Qualified(tbl t false, c))
let private call (f: string) (args: Expr list) = k (Call(ident f, args))
let private bin (op: BinaryOp) (l: Expr) (r: Expr) = k (Binary(op, l, r))
let private un (op: UnaryOp) (x: Expr) = k (Unary(op, x))

let private var (n: string) (v: Expr) : VarDecl = {
    Name = ident n
    Value = v
    Span = Span.none
}

/// Every operator's grouping spelled out, and the source's own parentheses dropped:
/// `1 + 2 * 3` is `(1 + (2 * 3))`.
let rec private grouped (e: Expr) : string =
    match e.Kind with
    | Paren x -> grouped x
    | Binary(op, l, r) -> $"({grouped l} {binaryText op} {grouped r})"
    | Unary(UnaryOp.Not, x) -> $"(NOT {grouped x})"
    | Unary(UnaryOp.Minus, x) -> $"(-{grouped x})"
    | Unary(UnaryOp.Plus, x) -> $"(+{grouped x})"
    | _ -> printExpression e

/// Run on a thread with a small stack and a time limit: a parser that recursed without bound, or
/// looped, fails here instead of taking the test run down.
let private boundedOn (stackKb: int) (what: string) (f: unit -> 'a) : 'a =
    let mutable result = Unchecked.defaultof<'a>
    let mutable error: exn = null

    let t =
        Thread(
            (fun () ->
                try
                    result <- f ()
                with e ->
                    error <- e),
            stackKb * 1024
        )

    t.IsBackground <- true
    t.Start()

    if not (t.Join(TimeSpan.FromSeconds 60.0)) then
        failtestf "%s: did not finish in 60 seconds" what

    if not (isNull error) then
        failtestf "%s: threw %s" what (error.ToString())

    result

/// A stack much smaller than any thread's default: parsing alone must fit.
let private bounded (what: string) (f: unit -> 'a) : 'a = boundedOn 256 what f

/// The smallest default stack of the platforms tessera runs on (1 MB, Windows): parsing and then
/// recursing over the tree must fit.
let private boundedDefault (what: string) (f: unit -> 'a) : 'a = boundedOn 1024 what f

/// What every parse must satisfy, whatever the text: it returns, and its errors are usable.
let private wellBehaved (text: string) =
    let length = if isNull text then 0 else text.Length

    let check (what: string) (result: Result<'a, ParseError list>) =
        match result with
        | Ok _ -> ()
        | Error errors ->
            if errors.IsEmpty then
                failtestf "%s: failed without an error for %A" what text

            for e in errors do
                if
                    e.Offset < 0
                    || e.Offset > length
                    || e.Length < 0
                    || e.Offset + e.Length > length
                    || e.Line < 1
                    || e.Column < 1
                    || String.IsNullOrWhiteSpace e.Message
                then
                    failtestf "%s: bad error %A for %A" what e text

            if errors <> (errors |> List.sortBy (fun e -> e.Offset)) then
                failtestf "%s: errors out of order for %A" what text

    for options in [ ParseOptions.defaults; { Semicolons = true } ] do
        check "parseExpression" (parseExpressionWith options text)
        check "parseFunction" (parseFunctionWith options text)
        check "parseQuery" (parseQueryWith options text)
        check "parse" (parseWith options text)

/// Significant tokens, with words in upper case: what parse -> print must keep.
let private tokensOf (dax: string) =
    tokenize dax
    |> significant
    |> List.map (fun t ->
        t.Kind,
        (if t.Kind = TokenKind.Identifier then
             t.Text.ToUpperInvariant()
         else
             t.Text))

/// parse -> print -> parse gives the same tree, the printed text has the source's tokens, and
/// printing is stable.
let private roundTrips (dax: string) =
    match parse dax with
    | Error errors -> failtestf "%s\n  does not parse: %s" dax (messages errors)
    | Ok first ->
        let text = print first

        match parse text with
        | Error errors -> failtestf "%s\n  printed as\n%s\n  which does not parse: %s" dax text (messages errors)
        | Ok second ->
            let strip (d: Document) =
                match d with
                | Document.Expression e -> Document.Expression(stripSpans e)
                | Document.Function f -> Document.Function(FunctionDef.stripSpans f)
                | Document.Query q -> Document.Query(Query.stripSpans q)

            Expect.equal (strip second) (strip first) $"the same tree after printing: {dax}"
            Expect.equal (tokensOf text) (tokensOf dax) $"the same tokens after printing: {dax}"
            Expect.equal (print second) text $"printing is stable: {dax}"

// ---- The tree -----------------------------------------------------------------------------------

[<Tests>]
let treeTests =
    testList "DAX parser: the tree" [
        testCase "literals"
        <| fun () ->
            Expect.equal (shape "42") (int' 42) "integer"
            Expect.equal (shape "007") (k (IntegerLiteral(7L, "007"))) "the text is kept"
            Expect.equal (shape "1.5") (k (NumberLiteral(1.5, "1.5"))) "decimal"
            Expect.equal (shape ".5") (k (NumberLiteral(0.5, ".5"))) "no leading digit"
            Expect.equal (shape "1.") (k (NumberLiteral(1.0, "1."))) "no trailing digit"
            Expect.equal (shape "1e3") (k (NumberLiteral(1000.0, "1e3"))) "exponent"
            Expect.equal (shape "2.5E-3") (k (NumberLiteral(0.0025, "2.5E-3"))) "signed exponent"

            Expect.equal
                (shape "99999999999999999999")
                (k (NumberLiteral(1e20, "99999999999999999999")))
                "too big for Int64: a float, with its digits kept"

            Expect.equal (shape "9223372036854775807") (k (IntegerLiteral(Int64.MaxValue, "9223372036854775807"))) ""
            Expect.equal (shape "\"a \"\"b\"\"\"") (str "a \"b\"") "string, unescaped"
            Expect.equal (shape "\"two\nlines\"") (str "two\nlines") "a string may span lines"
            Expect.equal (shape "TRUE") (k (BooleanLiteral true)) ""
            Expect.equal (shape "false") (k (BooleanLiteral false)) "any case"
            Expect.equal (shape "TRUE()") (call "TRUE" []) "TRUE() is a call"
            Expect.equal (shape "dt\"2024-01-31\"") (k (DateLiteral "2024-01-31")) ""
            Expect.equal (shape "DT\"2024-01-31T13:05:00\"") (k (DateLiteral "2024-01-31T13:05:00")) ""

        testCase "a negative number is a sign and a literal"
        <| fun () -> Expect.equal (shape "-1") (un UnaryOp.Minus (int' 1)) ""

        testCase "date literals read as dates"
        <| fun () ->
            Expect.equal (tryDate "2024-01-31") (Some(DateTime(2024, 1, 31))) ""
            Expect.equal (tryDate "2024-1-5") (Some(DateTime(2024, 1, 5))) ""
            Expect.equal (tryDate "2024-01-31T13:05:07") (Some(DateTime(2024, 1, 31, 13, 5, 7))) ""
            Expect.equal (tryDate "2024-01-31 13:05") (Some(DateTime(2024, 1, 31, 13, 5, 0))) ""
            Expect.equal (tryDate "31/01/2024") None ""
            Expect.equal (tryDate "") None ""
            Expect.equal (tryDate null) None ""

        testCase "names stay unresolved"
        <| fun () ->
            Expect.equal (shape "Sales") (name "Sales") "a bare word: table, variable or parameter"
            Expect.equal (shape "'Sales Table'") (k (QuotedTable "Sales Table")) ""
            Expect.equal (shape "'It''s'") (k (QuotedTable "It's")) "'' unescaped"
            Expect.equal (shape "Sales[Amount]") (col "Sales" "Amount") ""
            Expect.equal (shape "Sales [Amount]") (col "Sales" "Amount") "space before the bracket"

            Expect.equal
                (shape "'Sales Table'[Net ]] Amount]")
                (k (Qualified(tbl "Sales Table" true, "Net ] Amount")))
                "quoted table, ]] unescaped"

            Expect.equal (shape "[Total Sales]") (br "Total Sales") ""
            Expect.equal (shape "@Region") (k (QueryParameter "Region")) "query parameter"
            Expect.equal (shape "DaxLib.Fmt(1)") (call "DaxLib.Fmt" [ int' 1 ]) "dotted function name"

        testCase "calls, with omitted arguments"
        <| fun () ->
            Expect.equal (shape "BLANK()") (call "BLANK" []) "no arguments"
            Expect.equal (shape "sum ( Sales[Amount] )") (call "sum" [ col "Sales" "Amount" ]) "the name as written"

            Expect.equal
                (shape "RANKX(T, [m], , DESC)")
                (call "RANKX" [ name "T"; br "m"; k Omitted; name "DESC" ])
                "an argument left out in the middle"

            Expect.equal (shape "F(a,)") (call "F" [ name "a"; k Omitted ]) "left out at the end"
            Expect.equal (shape "F(,a)") (call "F" [ k Omitted; name "a" ]) "left out at the start"
            Expect.equal (shape "F(,)") (call "F" [ k Omitted; k Omitted ]) "both left out"

        testCase "an omitted argument has an empty span, at the separator or parenthesis that ends it"
        <| fun () ->
            match (parsed "RANKX(T, [m], , DESC)").Kind with
            | Call(_, [ _; _; omitted; _ ]) ->
                Expect.equal omitted.Kind Omitted ""
                Expect.equal omitted.Span { Start = 14; Length = 0 } ""
            | other -> failtestf "%A" other

        testCase "keyword arguments are bare words; several side by side are Words"
        <| fun () ->
            Expect.equal
                (shape "DATEADD('Date'[Date], -1, YEAR)")
                (call "DATEADD" [
                    k (Qualified(tbl "Date" true, "Date"))
                    un UnaryOp.Minus (int' 1)
                    name "YEAR"
                ])
                ""

            Expect.equal
                (shape "ORDERBY([x], asc blanks last)")
                (call "ORDERBY" [ br "x"; k (Words [ "asc"; "blanks"; "last" ]) ])
                "as written"

            Expect.equal
                (shape "RUNNINGSUM([x], ROWS COLUMNS)")
                (call "RUNNINGSUM" [ br "x"; k (Words [ "ROWS"; "COLUMNS" ]) ])
                ""

            let keyword (dax: string) =
                match (parsed dax).Kind with
                | Call(_, args) ->
                    args
                    |> List.choose (function
                        | Keyword w -> Some w
                        | _ -> None)
                | _ -> []

            Expect.equal
                (keyword "RANKX(T, [m], , desc, Dense)")
                [ "T"; "DESC"; "DENSE" ]
                "upper case; any bare word matches"

            Expect.equal (keyword "ORDERBY([x], asc  blanks\nlast)") [ "ASC BLANKS LAST" ] "single spaces"
            Expect.equal (keyword "CONVERT([x], INTEGER)") [ "INTEGER" ] ""
            Expect.isTrue (isKeywordWord "oneway_leftfiltersright") ""
            Expect.isFalse (isKeywordWord "Sales") ""

        testCase "VAR blocks nest anywhere"
        <| fun () ->
            Expect.equal
                (shape "VAR x = 1 VAR y = x + 1 RETURN x * y")
                (k (
                    VarBlock(
                        [ var "x" (int' 1); var "y" (bin BinaryOp.Add (name "x") (int' 1)) ],
                        bin BinaryOp.Multiply (name "x") (name "y")
                    )
                ))
                ""

            Expect.equal
                (shape "IF(a, VAR x = 1 RETURN x, 2) + 1")
                (bin
                    BinaryOp.Add
                    (call "IF" [ name "a"; k (VarBlock([ var "x" (int' 1) ], name "x")); int' 2 ])
                    (int' 1))
                "as an argument"

            Expect.equal
                (shape "var x = VAR y = 1 RETURN y return x")
                (k (VarBlock([ var "x" (k (VarBlock([ var "y" (int' 1) ], name "y"))) ], name "x")))
                "as a variable's value, in any case"

            Expect.equal
                (shape "1 + VAR x = 2 RETURN x + 3")
                (bin BinaryOp.Add (int' 1) (k (VarBlock([ var "x" (int' 2) ], bin BinaryOp.Add (name "x") (int' 3)))))
                "as an operand: RETURN takes everything after it"

        testCase "table constructors, tuples and parentheses"
        <| fun () ->
            Expect.equal (shape "{1, 2}") (k (TableConstructor [ int' 1; int' 2 ])) ""

            Expect.equal
                (shape "{(1, \"a\"), (2, \"b\")}")
                (k (TableConstructor [ k (Tuple [ int' 1; str "a" ]); k (Tuple [ int' 2; str "b" ]) ]))
                ""

            Expect.equal
                (shape "(a, b) IN {(1, 2)}")
                (bin
                    BinaryOp.In
                    (k (Tuple [ name "a"; name "b" ]))
                    (k (TableConstructor [ k (Tuple [ int' 1; int' 2 ]) ])))
                ""

            Expect.equal (shape "(a)") (k (Paren(name "a"))) "one value in parentheses is not a tuple"
            Expect.equal (shape "((a))") (k (Paren(k (Paren(name "a"))))) "every pair is kept"
            Expect.equal (shape "{(1)}") (k (TableConstructor [ k (Paren(int' 1)) ])) ""

            Expect.equal
                (shape "DATATABLE(\"a\", INTEGER, {{1}, {2}})")
                (call "DATATABLE" [
                    str "a"
                    name "INTEGER"
                    k (TableConstructor [ k (TableConstructor [ int' 1 ]); k (TableConstructor [ int' 2 ]) ])
                ])
                "DATATABLE's rows in braces"

        testCase "every operator"
        <| fun () ->
            for text, op in
                [
                    "+", BinaryOp.Add
                    "-", BinaryOp.Subtract
                    "*", BinaryOp.Multiply
                    "/", BinaryOp.Divide
                    "^", BinaryOp.Power
                    "&", BinaryOp.Concat
                    "=", BinaryOp.Equal
                    "==", BinaryOp.StrictEqual
                    "<>", BinaryOp.NotEqual
                    "<", BinaryOp.Less
                    "<=", BinaryOp.LessOrEqual
                    ">", BinaryOp.Greater
                    ">=", BinaryOp.GreaterOrEqual
                    "&&", BinaryOp.And
                    "||", BinaryOp.Or
                    "IN", BinaryOp.In
                    "in", BinaryOp.In
                ] do
                Expect.equal (shape $"a {text} b") (bin op (name "a") (name "b")) text

                if text <> "in" then
                    Expect.equal (binaryText op) text text

            Expect.equal (shape "a==b") (bin BinaryOp.StrictEqual (name "a") (name "b")) "no spaces"
            Expect.equal (shape "+a") (un UnaryOp.Plus (name "a")) ""
            Expect.equal (shape "-a") (un UnaryOp.Minus (name "a")) ""
            Expect.equal (shape "NOT a") (un UnaryOp.Not (name "a")) ""
            Expect.equal (shape "not a") (un UnaryOp.Not (name "a")) ""

        testCase "a leading = is allowed and is not part of the tree"
        <| fun () ->
            let dax = " = SUM ( Sales[Amount] ) = 1"
            let e = parsed dax
            Expect.equal (stripSpans e) (shape "SUM(Sales[Amount]) = 1") ""

            Expect.equal
                (dax.Substring(e.Span.Start, e.Span.Length))
                "SUM ( Sales[Amount] ) = 1"
                "the span starts after it"

            Expect.equal (errorsOf "= = 1") [ "1:3: expected an expression, found '='" ] "only one"
            Expect.equal (errorsOf "=") [ "1:2: expected an expression, found the end of the text" ] ""

        testCase "== is one token for the parser, and still two = for BPA rules, as in Tabular Editor 2"
        <| fun () ->
            Expect.equal
                (tokenize "a == b"
                 |> significant
                 |> List.map (fun t -> t.Kind, t.Text, t.Start, t.Length))
                [
                    TokenKind.Identifier, "a", 0, 1
                    TokenKind.Operator, "==", 2, 2
                    TokenKind.Identifier, "b", 5, 1
                ]
                ""

            let assignment = (BpaDaxToken.tokenize false "x = 1").[1].Type

            Expect.equal
                (BpaDaxToken.tokenize false "a == b"
                 |> Seq.map (fun t -> t.Text, t.Type, t.StartIndex, t.StopIndex, t.TokenIndex)
                 |> List.ofSeq
                 |> List.tail
                 |> List.take 2)
                [ "=", assignment, 2, 2, 1; "=", assignment, 3, 3, 2 ]
                ""

        testCase "comments and layout are not part of the tree"
        <| fun () ->
            Expect.equal
                (shape "/* total */ SUM ( -- the amount\n    Sales[Amount] // here\n)\n")
                (shape "SUM(Sales[Amount])")
                ""

        testCase "spans are the source text of each node"
        <| fun () ->
            let dax =
                "VAR total =\n    CALCULATE ( SUM ( 'Sales T'[Amount] ), Sales[Year] = 2024 )\nRETURN\n    IF ( total > 0, -total ^ 2, { ( 1, \"a\" ) } )"

            let e = parsed dax
            let text (s: Span) = dax.Substring(s.Start, s.Length)
            let all = descendants e
            let texts = all |> List.map (fun x -> text x.Span)

            Expect.equal (text e.Span) dax "the root is the whole text"

            for expected in
                [
                    "CALCULATE ( SUM ( 'Sales T'[Amount] ), Sales[Year] = 2024 )"
                    "SUM ( 'Sales T'[Amount] )"
                    "'Sales T'[Amount]"
                    "Sales[Year] = 2024"
                    "Sales[Year]"
                    "2024"
                    "IF ( total > 0, -total ^ 2, { ( 1, \"a\" ) } )"
                    "total > 0"
                    "-total ^ 2"
                    "total ^ 2"
                    "{ ( 1, \"a\" ) }"
                    "( 1, \"a\" )"
                    "\"a\""
                ] do
                Expect.contains texts expected expected

            match e.Kind with
            | VarBlock([ v ], _) ->
                Expect.equal (text v.Name.Span) "total" "the variable's name"

                Expect.equal
                    (text v.Span)
                    "VAR total =\n    CALCULATE ( SUM ( 'Sales T'[Amount] ), Sales[Year] = 2024 )"
                    "the declaration"
            | other -> failtestf "%A" other

            let calls =
                all
                |> List.choose (fun x ->
                    match x.Kind with
                    | Call(f, _) -> Some(text f.Span)
                    | Qualified(t, _) -> Some(text t.Span)
                    | _ -> None)

            Expect.equal calls [ "CALCULATE"; "SUM"; "'Sales T'"; "Sales"; "IF" ] "function and table names"

        testCase "walking: children, descendants, depth, stripParens"
        <| fun () ->
            let e = shape "IF((a + 1) > 2, {b})"
            Expect.equal (children e |> List.length) 2 ""
            Expect.equal (depth e) 5 "IF > '>' > Paren > '+' > a"
            Expect.equal (depth (int' 1)) 1 ""

            Expect.equal
                (descendants e |> List.map printExpression)
                [
                    "IF((a + 1) > 2, {b})"
                    "(a + 1) > 2"
                    "(a + 1)"
                    "a + 1"
                    "a"
                    "1"
                    "2"
                    "{b}"
                    "b"
                ]
                "parents first, in source order"

            Expect.equal (stripParens e) (shape "IF(a + 1 > 2, {b})") ""

            let first, rest = leftSpine (shape "a + b * 2 - c & (d & e)")

            Expect.equal
                (printExpression first, rest |> List.map (fun (op, r) -> binaryText op, printExpression r))
                ("a", [ "+", "b * 2"; "-", "c"; "&", "(d & e)" ])
                "a chain, left to right"

            Expect.equal (leftSpine (int' 1)) (int' 1, []) "not a chain"
            Expect.equal (stripParens (shape "((a, b))")) (shape "(a, b)") "a tuple stays"
    ]

// ---- Precedence ---------------------------------------------------------------------------------

/// Source of truth for each row:
///   [MS]  Microsoft, "DAX operators", section "Operator precedence"
///         https://learn.microsoft.com/en-us/dax/dax-operator-reference
///         ^ ; sign ; * / ; + - ; & ; = == < > <= >= <> IN ; NOT. "If the operators have equal
///         precedence value, they are ordered from left to right." "=-2^2 ... is -4."
///   [DG]  SQLBI, DAX Guide, "DAX Operators": https://dax.guide/operators/
///         ... comparison (7) ; NOT (8) ; && (9) ; || (10)
///   [NOT] SQLBI, DAX Guide, "NOT": https://dax.guide/op/not/
///         "NOT is an operator and not a function": NOT ( FALSE ) * FALSE is TRUE.
let private precedence = [
    // [MS] one row above the next
    "a * b ^ c", "(a * (b ^ c))"
    "a ^ b * c", "((a ^ b) * c)"
    "a + b * c", "(a + (b * c))"
    "a * b + c", "((a * b) + c)"
    "a - b / c", "(a - (b / c))"
    "a & b + c", "(a & (b + c))"
    "a + b & c", "((a + b) & c)"
    "a = b & c", "(a = (b & c))"
    "a & b = c", "((a & b) = c)"
    "a < b + c", "(a < (b + c))"
    "a + b >= c * d", "((a + b) >= (c * d))"
    // [MS] the sign is below ^ and above * /
    "-a ^ b", "(-(a ^ b))"
    "-2 ^ 2", "(-(2 ^ 2))"
    "-a * b", "((-a) * b)"
    "-a + b", "((-a) + b)"
    "+a - b", "((+a) - b)"
    "-a = b", "((-a) = b)"
    // A sign may start any operand, also to the right of a tighter operator.
    "a ^ -b", "(a ^ (-b))"
    "a * -b", "(a * (-b))"
    "a - -b", "(a - (-b))"
    "a ^ -b * c", "((a ^ (-b)) * c)"
    "a ^ -b ^ c", "(a ^ (-(b ^ c)))"
    "- - a", "(-(-a))"
    // [MS] NOT is below the comparisons
    "NOT a = b", "(NOT (a = b))"
    "NOT a <> b", "(NOT (a <> b))"
    "NOT a IN t", "(NOT (a IN t))"
    "NOT a + b", "(NOT (a + b))"
    "NOT NOT a", "(NOT (NOT a))"
    "NOT -a", "(NOT (-a))"
    // [NOT] NOT ( x ) is the operator applied to what follows, not a call
    "NOT (a) = b", "(NOT (a = b))"
    "NOT (FALSE) * FALSE", "(NOT (FALSE * FALSE))"
    "NOT (a) IN t", "(NOT (a IN t))"
    // [DG] && is below NOT, || below &&
    "NOT a && b", "((NOT a) && b)"
    "NOT a || b", "((NOT a) || b)"
    "a && NOT b", "(a && (NOT b))"
    "a || b && c", "(a || (b && c))"
    "a && b || c", "((a && b) || c)"
    "a = b && c = d", "((a = b) && (c = d))"
    "a = b || c = d && e = f", "((a = b) || ((c = d) && (e = f)))"
    "a IN t && b", "((a IN t) && b)"
    "a || b = c", "(a || (b = c))"
    // NOT may start any operand too, and takes everything down to the comparisons.
    "1 + NOT a = b", "(1 + (NOT (a = b)))"
    "1 + NOT a = b && c", "((1 + (NOT (a = b))) && c)"
    // [MS] equal precedence: left to right
    "a - b - c", "((a - b) - c)"
    "a - b + c", "((a - b) + c)"
    "a / b / c", "((a / b) / c)"
    "a / b * c", "((a / b) * c)"
    "a ^ b ^ c", "((a ^ b) ^ c)"
    "a & b & c", "((a & b) & c)"
    "a = b = c", "((a = b) = c)"
    "a < b <= c", "((a < b) <= c)"
    "a == b <> c", "((a == b) <> c)"
    "a IN t = b", "((a IN t) = b)"
    "a && b && c", "((a && b) && c)"
    "a || b || c", "((a || b) || c)"
    // Parentheses win.
    "(a + b) * c", "((a + b) * c)"
    "a ^ (b * c)", "(a ^ (b * c))"
    "(NOT a) = b", "((NOT a) = b)"
    "(-a) ^ b", "((-a) ^ b)"
]

[<Tests>]
let precedenceTests =
    testList "DAX parser: precedence" [
        for dax, expected in precedence do
            testCase dax <| fun () -> Expect.equal (grouped (parsed dax)) expected dax

        // Microsoft lists IN with the comparison operators; the DAX Guide lists it at the top,
        // above ^. The parser follows Microsoft. If the engine turns out to side with the DAX
        // Guide, these become (a + (b IN t)) and (a & (b IN t)): change `inBindingPower`.
        testCase "IN binds like a comparison (Microsoft's table, not the DAX Guide's)"
        <| fun () ->
            Expect.equal (grouped (parsed "a + b IN t")) "((a + b) IN t)" ""
            Expect.equal (grouped (parsed "a & b IN t")) "((a & b) IN t)" ""
            Expect.equal (grouped (parsed "a IN t & b")) "(a IN (t & b))" ""
            Expect.equal (bindingPower BinaryOp.In) (bindingPower BinaryOp.Equal) ""

        testCase "every binary operator has a binding power, in the documented order"
        <| fun () ->
            let levels = [
                [ BinaryOp.Or ]
                [ BinaryOp.And ]
                [
                    BinaryOp.Equal
                    BinaryOp.StrictEqual
                    BinaryOp.NotEqual
                    BinaryOp.Less
                    BinaryOp.LessOrEqual
                    BinaryOp.Greater
                    BinaryOp.GreaterOrEqual
                    BinaryOp.In
                ]
                [ BinaryOp.Concat ]
                [ BinaryOp.Add; BinaryOp.Subtract ]
                [ BinaryOp.Multiply; BinaryOp.Divide ]
                [ BinaryOp.Power ]
            ]

            for level in levels do
                Expect.equal (level |> List.map bindingPower |> List.distinct |> List.length) 1 $"%A{level}"

            let powers = levels |> List.map (List.head >> bindingPower)
            Expect.equal powers (List.sort powers |> List.distinct) "strictly rising"

            let notPower = prefixBindingPower UnaryOp.Not
            Expect.isGreaterThan notPower (bindingPower BinaryOp.And) "NOT above &&"
            Expect.isLessThan notPower (bindingPower BinaryOp.Equal) "NOT below the comparisons"

            let sign = prefixBindingPower UnaryOp.Minus
            Expect.equal (prefixBindingPower UnaryOp.Plus) sign ""
            Expect.isGreaterThan sign (bindingPower BinaryOp.Multiply) "sign above * /"
            Expect.isLessThan sign (bindingPower BinaryOp.Power) "sign below ^"
    ]

// ---- Functions and queries ----------------------------------------------------------------------

let private func (dax: string) : FunctionDef =
    match parseFunction dax with
    | Ok f -> FunctionDef.stripSpans f
    | Error errors -> failtestf "%s\n  does not parse: %s" dax (messages errors)

let private query (dax: string) : Query =
    match parseQuery dax with
    | Ok q -> Query.stripSpans q
    | Error errors -> failtestf "%s\n  does not parse: %s" dax (messages errors)

let private param (n: string) ty subtype mode fallback : Parameter = {
    Name = ident n
    Type = ty
    Subtype = subtype
    Mode = mode
    Default = fallback
    Span = Span.none
}

[<Tests>]
let functionTests =
    testList "DAX parser: function definitions" [
        testCase "parameters with type, subtype and mode in every combination"
        <| fun () ->
            let f =
                func
                    "(a: INT64, b: SCALAR VAL, c: ANYREF EXPR, d, e: expr, f: Scalar Numeric Val, g: TABLE, h: columnref) => a + b"

            Expect.equal
                f.Parameters
                [
                    param "a" None (Some ScalarType.Int64) None None
                    param "b" (Some ParameterType.Scalar) None (Some ParameterMode.Val) None
                    param "c" (Some ParameterType.AnyRef) None (Some ParameterMode.Expr) None
                    param "d" None None None None
                    param "e" None None (Some ParameterMode.Expr) None
                    param "f" (Some ParameterType.Scalar) (Some ScalarType.Numeric) (Some ParameterMode.Val) None
                    param "g" (Some ParameterType.Table) None None None
                    param "h" (Some ParameterType.ColumnRef) None None None
                ]
                ""

            Expect.equal f.Body (bin BinaryOp.Add (name "a") (name "b")) ""

        testCase "every type keyword"
        <| fun () ->
            for word, expected in
                [
                    "ANYVAL", ParameterType.AnyVal
                    "SCALAR", ParameterType.Scalar
                    "TABLE", ParameterType.Table
                    "ANYREF", ParameterType.AnyRef
                    "CALENDARREF", ParameterType.CalendarRef
                    "COLUMNREF", ParameterType.ColumnRef
                    "MEASUREREF", ParameterType.MeasureRef
                    "TABLEREF", ParameterType.TableRef
                ] do
                Expect.equal (func $"(x: {word}) => x").Parameters.[0].Type (Some expected) word
                Expect.equal (parameterTypeText expected) word word

            for word, expected in
                [
                    "VARIANT", ScalarType.Variant
                    "INT64", ScalarType.Int64
                    "DECIMAL", ScalarType.Decimal
                    "DOUBLE", ScalarType.Double
                    "STRING", ScalarType.String
                    "DATETIME", ScalarType.DateTime
                    "BOOLEAN", ScalarType.Boolean
                    "NUMERIC", ScalarType.Numeric
                ] do
                Expect.equal (func $"(x: {word}) => x").Parameters.[0].Subtype (Some expected) word
                Expect.equal (scalarTypeText expected) word word

        testCase "no parameters, default values, comments between parameters"
        <| fun () ->
            Expect.equal (func "() => 1").Parameters [] ""

            Expect.equal
                (func "(a: INT64 = 1 + 2, b = \"x\") => a").Parameters
                [
                    param "a" None (Some ScalarType.Int64) None (Some(bin BinaryOp.Add (int' 1) (int' 2)))
                    param "b" None None None (Some(str "x"))
                ]
                "= default"

            Expect.equal
                (func "(\n    // the amount\n    amount : NUMERIC, /* rate */ rate: DOUBLE\n)\n=>\n    amount * rate")
                (func "(amount: NUMERIC, rate: DOUBLE) => amount * rate")
                ""

        testCase "DAX's defaults for a parameter's type and mode"
        <| fun () ->
            let p (text: string) = (func $"({text}) => 1").Parameters.[0]

            let both text =
                Parameter.effectiveType (p text), Parameter.effectiveMode (p text)

            Expect.equal (both "x") (ParameterType.AnyVal, ParameterMode.Val) "nothing: AnyVal val"
            Expect.equal (both "x: INT64") (ParameterType.Scalar, ParameterMode.Val) "a subtype means scalar"
            Expect.equal (both "x: EXPR") (ParameterType.AnyRef, ParameterMode.Expr) ""
            Expect.equal (both "x: ANYREF") (ParameterType.AnyRef, ParameterMode.Expr) "references pass by expression"
            Expect.equal (both "x: TABLE EXPR") (ParameterType.Table, ParameterMode.Expr) ""
            Expect.equal (both "x: SCALAR STRING") (ParameterType.Scalar, ParameterMode.Val) ""

        testCase "a function's spans"
        <| fun () ->
            let dax = "( amount : NUMERIC, rate = 0.1 ) => amount * rate"

            match parseFunction dax with
            | Ok f ->
                let text (s: Span) = dax.Substring(s.Start, s.Length)
                Expect.equal (text f.Span) dax ""
                Expect.equal (f.Parameters |> List.map (fun p -> text p.Span)) [ "amount : NUMERIC"; "rate = 0.1" ] ""
                Expect.equal (f.Parameters |> List.map (fun p -> text p.Name.Span)) [ "amount"; "rate" ] ""
                Expect.equal (text f.Body.Span) "amount * rate" ""
            | Error errors -> failtestf "%s" (messages errors)
    ]

[<Tests>]
let queryTests =
    testList "DAX parser: queries" [
        testCase "EVALUATE alone"
        <| fun () ->
            Expect.equal
                (query "EVALUATE Sales")
                {
                    Definitions = []
                    Evaluates = [
                        {
                            Table = name "Sales"
                            OrderBy = []
                            StartAt = []
                            Span = Span.none
                        }
                    ]
                    Span = Span.none
                }
                ""

        testCase "every kind of definition"
        <| fun () ->
            let q =
                query
                    """
DEFINE
    MEASURE Sales[Big] = SUM ( Sales[Amount] ) * 2
    MEASURE 'Sales T'[Net ]] x] = 1
    COLUMN Sales[Margin] = Sales[Amount] - Sales[Cost]
    TABLE Top = TOPN ( 3, Sales )
    TABLE 'Top Two' = { 1, 2 }
    VAR limit = 10
    VAR nested = VAR a = 1 RETURN a
    /// Adds tax.
    FUNCTION AddTax = ( amount : NUMERIC ) => amount * 1.1
    FUNCTION DaxLib.Zero = () => 0
    MPARAMETER Region = "West"
EVALUATE Top
"""

            let kinds = q.Definitions |> List.map (fun d -> d.Kind)

            Expect.equal
                kinds
                [
                    DefinitionKind.Measure(
                        tbl "Sales" false,
                        ident "Big",
                        bin BinaryOp.Multiply (call "SUM" [ col "Sales" "Amount" ]) (int' 2)
                    )
                    DefinitionKind.Measure(tbl "Sales T" true, ident "Net ] x", int' 1)
                    DefinitionKind.Column(
                        tbl "Sales" false,
                        ident "Margin",
                        bin BinaryOp.Subtract (col "Sales" "Amount") (col "Sales" "Cost")
                    )
                    DefinitionKind.Table(tbl "Top" false, call "TOPN" [ int' 3; name "Sales" ])
                    DefinitionKind.Table(tbl "Top Two" true, k (TableConstructor [ int' 1; int' 2 ]))
                    DefinitionKind.Var(ident "limit", int' 10)
                    DefinitionKind.Var(ident "nested", k (VarBlock([ var "a" (int' 1) ], name "a")))
                    DefinitionKind.Function(
                        ident "AddTax",
                        {
                            Parameters = [ param "amount" None (Some ScalarType.Numeric) None None ]
                            Body = bin BinaryOp.Multiply (name "amount") (k (NumberLiteral(1.1, "1.1")))
                            Span = Span.none
                        }
                    )
                    DefinitionKind.Function(
                        ident "DaxLib.Zero",
                        {
                            Parameters = []
                            Body = int' 0
                            Span = Span.none
                        }
                    )
                    DefinitionKind.MParameter(ident "Region", str "West")
                ]
                ""

        testCase "ORDER BY and START AT, several EVALUATEs"
        <| fun () ->
            let q =
                query
                    "EVALUATE Sales ORDER BY Sales[Year] DESC, [Total] asc, Sales[Id] START AT 2024, @Start\nevaluate { 1 }\nEVALUATE T order by [x]"

            Expect.equal
                (q.Evaluates
                 |> List.map (fun e -> e.OrderBy |> List.map (fun o -> o.Expr, o.Direction)))
                [
                    [
                        col "Sales" "Year", Some SortDirection.Desc
                        br "Total", Some SortDirection.Asc
                        col "Sales" "Id", None
                    ]
                    []
                    [ br "x", None ]
                ]
                ""

            Expect.equal
                (q.Evaluates |> List.map (fun e -> e.StartAt))
                [ [ int' 2024; k (QueryParameter "Start") ]; []; [] ]
                ""

            Expect.equal
                (q.Evaluates |> List.map (fun e -> e.Table))
                [ name "Sales"; k (TableConstructor [ int' 1 ]); name "T" ]
                ""

        testCase "a table called Order or Start is still a table"
        <| fun () ->
            Expect.equal (query "EVALUATE Order ORDER BY Order[Id]").Evaluates.[0].Table (name "Order") ""
            Expect.equal (query "EVALUATE Start").Evaluates.[0].Table (name "Start") ""

            Expect.equal
                (shape "COUNTROWS(Measure) + COUNTROWS(Table)")
                (shape "COUNTROWS ( Measure ) + COUNTROWS ( Table )")
                ""

        testCase "a query's spans"
        <| fun () ->
            let dax =
                "DEFINE\n  MEASURE Sales[Big] = 1 -- one\n  VAR x = 2\nEVALUATE Sales ORDER BY [a] DESC, [b] START AT 1 // end\n"

            match parseQuery dax with
            | Ok q ->
                let text (s: Span) = dax.Substring(s.Start, s.Length)

                Expect.equal
                    (q.Definitions |> List.map (fun d -> text d.Span))
                    [ "MEASURE Sales[Big] = 1"; "VAR x = 2" ]
                    ""

                Expect.equal
                    (q.Evaluates |> List.map (fun e -> text e.Span))
                    [ "EVALUATE Sales ORDER BY [a] DESC, [b] START AT 1" ]
                    ""

                Expect.equal (q.Evaluates.[0].OrderBy |> List.map (fun o -> text o.Span)) [ "[a] DESC"; "[b]" ] ""
                Expect.equal (text q.Span) (dax.Substring(0, dax.IndexOf(" // end"))) ""

                match q.Definitions.[0].Kind with
                | DefinitionKind.Measure(t, n, _) -> Expect.equal (text t.Span, text n.Span) ("Sales", "[Big]") ""
                | other -> failtestf "%A" other
            | Error errors -> failtestf "%s" (messages errors)

        testCase "parse tells queries, functions and expressions apart"
        <| fun () ->
            let kind (dax: string) =
                match parse dax with
                | Ok(Document.Query _) -> "query"
                | Ok(Document.Function _) -> "function"
                | Ok(Document.Expression _) -> "expression"
                | Error errors -> failtestf "%s: %s" dax (messages errors)

            Expect.equal (kind "EVALUATE Sales") "query" ""
            Expect.equal (kind " -- c\n define measure T[m] = 1 evaluate T") "query" ""
            Expect.equal (kind "(a, b) => a + b") "function" ""
            Expect.equal (kind "() => 1") "function" ""
            Expect.equal (kind "(a, b) IN T") "expression" ""
            Expect.equal (kind "(a) + 1") "expression" ""
            Expect.equal (kind "SUM(T[x])") "expression" ""
    ]

// ---- The semicolon dialect ----------------------------------------------------------------------

let private semi = { ParseOptions.Semicolons = true }

[<Tests>]
let dialectTests =
    testList "DAX parser: the semicolon dialect" [
        testCase "; separates, and , is the decimal mark"
        <| fun () ->
            let p (dax: string) =
                match parseExpressionWith semi dax with
                | Ok e -> stripSpans e
                | Error errors -> failtestf "%s: %s" dax (messages errors)

            Expect.equal
                (p "IF(a; 1,5; 2)")
                (call "IF" [ name "a"; k (NumberLiteral(1.5, "1.5")); int' 2 ])
                "the text has '.'"

            Expect.equal (p "{1,5; 2}") (k (TableConstructor [ k (NumberLiteral(1.5, "1.5")); int' 2 ])) ""
            Expect.equal (p "{(1; 2); (3; 4,25)}") (shape "{(1, 2), (3, 4.25)}") ""
            Expect.equal (p ",5 + 1,5e3") (shape ".5 + 1.5e3") "no leading digit; exponent"
            Expect.equal (p "RANKX(T; [m]; ; DESC)") (shape "RANKX(T, [m], , DESC)") "omitted"
            Expect.equal (p "1.5") (shape "1.5") "a point is still read"

            match parseFunctionWith semi "(a: INT64; b = 0,5) => a * b" with
            | Ok f -> Expect.equal (FunctionDef.stripSpans f) (func "(a: INT64, b = 0.5) => a * b") ""
            | Error errors -> failtestf "%s" (messages errors)

            match parseQueryWith semi "EVALUATE T ORDER BY [a] DESC; [b] START AT 1,5; 2" with
            | Ok q -> Expect.equal (Query.stripSpans q) (query "EVALUATE T ORDER BY [a] DESC, [b] START AT 1.5, 2") ""
            | Error errors -> failtestf "%s" (messages errors)

        testCase "a decimal comma's span covers the whole number"
        <| fun () ->
            let dax = "ROUND( 12,75 ; 1 )"

            match parseExpressionWith semi dax with
            | Ok { Kind = Call(_, [ n; _ ]) } -> Expect.equal (dax.Substring(n.Span.Start, n.Span.Length)) "12,75" ""
            | other -> failtestf "%A" other

        testCase "printing in the dialect, and back"
        <| fun () ->
            let e = parsed "IF(a, 1.5, ROUND(.25 + 2., 1), {(1, 2.5)})"
            let text = printExpressionWith { Semicolons = true } e
            Expect.equal text "IF(a; 1,5; ROUND(,25 + 2.; 1); {(1; 2,5)})" ""
            Expect.equal (parseExpressionWith semi text |> Result.map stripSpans) (Ok(stripSpans e)) ""

        testCase "the wrong separator says which dialect"
        <| fun () ->
            Expect.equal
                (errorsOf "IF(a; 1; 2)")
                [
                    "1:5: ';' is not a separator here: separate arguments with ',' (text that uses ';' and decimal commas is the semicolon dialect)"
                    "1:8: ';' is not a separator here: separate arguments with ',' (text that uses ';' and decimal commas is the semicolon dialect)"
                ]
                ""

            match parseExpressionWith semi "IF(a, 1, 2)" with
            | Error(first :: _) ->
                Expect.equal
                    (ParseError.format first)
                    "1:5: ',' is not a separator in the ';' dialect: separate arguments with ';' (',' is the decimal mark)"
                    ""
            | other -> failtestf "%A" other
    ]

// ---- Errors -------------------------------------------------------------------------------------

/// The typos people make, each with the one message it must give.
let private typos = [
    "a missing )", "SUM(Sales[Amount]", "1:18: expected ')' to close the '(' at 1:4, found the end of the text"
    "a missing ) before the next argument list ends",
    "IF(SUM(Sales[Amount], 1, 2)",
    "1:28: expected ')' to close the '(' at 1:3, found the end of the text"
    "a ) too many", "SUM(Sales[Amount]))", "1:19: unexpected ')': there is no '(' for it to close"
    "a missing comma",
    "IF([a] > 1 \"x\", \"y\")",
    "1:12: expected ',' or ')' after the argument of the '(' at 1:3, found '\"x\"'"
    "a missing }", "{1, 2", "1:6: expected '}' to close the '{' at 1:1, found the end of the text"
    "the wrong closer", "SUM(Sales[Amount]}", "1:18: expected ')' to close the '(' at 1:4, found '}'"
    "an operator with nothing after it", "[a] +", "1:6: expected an expression after '+', found the end of the text"
    "an operator twice", "[a] * / 2", "1:7: expected an expression after '*', found '/'"
    "an empty argument list's operator", "IF([a] >, 1)", "1:9: expected an expression after '>', found ','"
    "an unterminated string", "IF(a, \"yes, 1)", "1:7: unterminated string: add the closing \""
    "an unterminated [name]", "SUM(Sales[Amount)", "1:10: unterminated [name]: add the closing ]"
    "an unterminated table name", "COUNTROWS('Sales)", "1:11: unterminated table name: add the closing '"
    "an unterminated comment", "1 /* note", "1:3: unterminated /* comment: add the closing */"
    "VAR without RETURN", "VAR x = 1\nx + 1", "2:1: expected RETURN to finish the VAR at 1:1, found 'x'"
    "VAR without RETURN at the end",
    "VAR x = 1",
    "1:10: expected RETURN to finish the VAR at 1:1, found the end of the text"
    "RETURN without VAR", "RETURN 1", "1:1: RETURN without a VAR before it"
    "VAR without =", "VAR x 1 RETURN x", "1:7: expected '=' after VAR x, found '1'"
    "VAR without a name", "VAR = 1 RETURN 2", "1:5: expected a variable name after VAR, found '='"
    "!= for <>", "IF(a != b, 1)", "1:6: DAX has no '!=': write '<>' for \"not equal\""
    "| for ||", "a | b", "1:3: DAX has no '|': write '||' for \"or\""
    // SQLBI, "The IN operator in DAX" (https://www.sqlbi.com/articles/the-in-operator-in-dax/): "You need
    // to write NOT before the first argument of IN, and not just before the IN operator as you would
    // do in SQL."
    "NOT IN", "a NOT IN {1}", "1:3: DAX has no NOT IN: write NOT (value IN table)"
    // Microsoft, ORDERBY: the order ("ASC BLANKS LAST") is an argument of its own.
    "an order without its comma",
    "ORDERBY([x] ASC BLANKS LAST)",
    "1:13: expected ',' or ')' after the argument of the '(' at 1:8, found 'ASC'"
    "a table name with a space, unquoted",
    "SUM(Sales Amount[x])",
    "1:11: expected ',' or ')' after the argument of the '(' at 1:4, found 'Amount' (a name with a space in it needs quotes: 'Sales Amount')"
    "the same at the top level",
    "Sales Amount[x] + 1",
    "1:7: unexpected 'Amount' after 'Sales': expected an operator or the end of the expression (a name with a space in it needs quotes: 'Sales Amount')"
    "two values side by side", "1 2", "1:3: unexpected '2' after '1': expected an operator or the end of the expression"
    "a comma outside parentheses",
    "SUM(a), 2",
    "1:7: unexpected ',': it separates arguments, and this one is outside any parentheses"
    "nothing at all", "", "1:1: expected an expression, found the end of the text"
    "only a comment", "-- nothing\n", "2:1: expected an expression, found the end of the text"
    "empty parentheses", "1 + ()", "1:6: empty parentheses: expected an expression between '(' and ')'"
    "an empty table constructor", "{}", "1:2: empty table constructor: expected at least one value between '{' and '}'"
    "a tuple with a hole", "(1, , 2)", "1:5: expected an expression, found ','"
    "curly quotes",
    "IF(a, \u201Cx\u201D)",
    "1:7: unexpected character '\u201C' (U+201C): DAX strings need straight double quotes (\")"
    ":= for =", "Total := 1", "1:7: unexpected ':' (it only separates a function parameter from its type)"
    "a function body where an expression goes",
    "a => a + 1",
    "1:3: '=>' belongs to a function definition, (parameters) => body: this text was read as an expression"
    "a definition outside DEFINE",
    "MEASURE Sales[m] = 1",
    "1:9: unexpected 'Sales' after 'MEASURE': MEASURE defines something only under a query's DEFINE"
    "an unknown parameter type",
    "(a: FOO) => 1",
    "1:5: unknown parameter type 'FOO': expected ANYVAL, SCALAR, TABLE, ANYREF, CALENDARREF, COLUMNREF, MEASUREREF or TABLEREF; a scalar type (VARIANT, INT64, DECIMAL, DOUBLE, STRING, DATETIME, BOOLEAN, NUMERIC); VAL or EXPR"
    "a parameter type in the wrong order",
    "(a: VAL SCALAR) => 1",
    "1:9: unexpected 'SCALAR': a parameter's type is written [type] [subtype] [VAL or EXPR], each at most once"
    "a subtype on a table",
    "(a: TABLE INT64) => 1",
    "1:11: INT64 is a scalar type: it can only follow SCALAR, or stand alone"
    "a parameter without its type",
    "(a: ) => 1",
    "1:5: expected a type after 'a:' (for example INT64, SCALAR VAL, TABLE or ANYREF EXPR), found ')'"
    "two parameters without a comma", "(a b) => 1", "1:4: expected ',' or ')' after the parameter, found 'b'"
    "a function without =>",
    "(a, b) a + b",
    "1:8: unexpected 'a' after ')': expected an operator or the end of the expression"
    "EVALUATE with nothing", "EVALUATE", "1:9: expected an expression after EVALUATE, found the end of the text"
    "DEFINE with nothing",
    "DEFINE EVALUATE x",
    "1:8: expected MEASURE, VAR, TABLE, COLUMN, FUNCTION or MPARAMETER after DEFINE, found 'EVALUATE'"
    "DEFINE without EVALUATE", "DEFINE MEASURE T[m] = 1", "1:24: a query needs an EVALUATE"
    "ORDER without BY", "EVALUATE T ORDER [x]", "1:18: expected BY after ORDER, found '[x]'"
    "a measure defined without its table",
    "DEFINE MEASURE [m] = 1 EVALUATE T",
    "1:16: expected a table name after MEASURE, found '[m]'"
    "a measure defined without brackets",
    "DEFINE MEASURE T.m = 1 EVALUATE T",
    "1:20: expected [Name] after MEASURE T.m (as in MEASURE Sales[Name] = ...), found '='"
    "a definition without =", "DEFINE VAR x 1 EVALUATE T", "1:14: expected '=' after VAR x, found '1'"
    "START AT without ORDER BY", "EVALUATE T START AT 1", "1:12: START AT needs an ORDER BY before it"
    "a second DEFINE",
    "DEFINE VAR x = 1 DEFINE VAR y = 2 EVALUATE T",
    "1:18: a query has one DEFINE: put every definition under the first"
    "DEFINE after EVALUATE", "EVALUATE T DEFINE VAR y = 2", "1:12: DEFINE must come before the first EVALUATE"
]

[<Tests>]
let errorTests =
    testList "DAX parser: errors" [
        for what, dax, expected in typos do
            testCase $"one typo, one message: {what}"
            <| fun () -> Expect.equal (errorsOf dax) [ expected ] dax

        testCase "an error has its offset, length, line and column"
        <| fun () ->
            match parseExpression "IF (\n    [a] >> 1,\n    2\n)" with
            | Error [ e ] ->
                Expect.equal (e.Offset, e.Length, e.Line, e.Column) (14, 1, 2, 10) ""
                Expect.equal e.Message "expected an expression after '>', found '>'" ""
            | other -> failtestf "%A" other

            match parseExpression "SUM(\r\n  Sales[Amount]\r\n" with
            | Error [ e ] ->
                Expect.equal (e.Offset, e.Length, e.Line, e.Column) (23, 0, 3, 1) "at the end, after CRLF lines"
                Expect.equal e.Message "expected ')' to close the '(' at 1:4, found the end of the text" ""
            | other -> failtestf "%A" other

        testCase "independent mistakes are each reported, in order"
        <| fun () ->
            Expect.equal
                (errorsOf "IF(a b, c d, SUM(x y) + 1)")
                [
                    "1:6: expected ',' or ')' after the argument of the '(' at 1:3, found 'b' (a name with a space in it needs quotes: 'a b')"
                    "1:11: expected ',' or ')' after the argument of the '(' at 1:3, found 'd' (a name with a space in it needs quotes: 'c d')"
                    "1:20: expected ',' or ')' after the argument of the '(' at 1:17, found 'y' (a name with a space in it needs quotes: 'x y')"
                ]
                ""

            Expect.equal
                (errorsOf "DEFINE\n  MEASURE T[a] = 1 +\n  MEASURE T[b] = (2\nEVALUATE T ORDER BY")
                [
                    "3:3: expected an expression after '+', found 'MEASURE'"
                    "4:1: expected ')' to close the '(' at 3:18, found 'EVALUATE'"
                    "4:20: expected an expression after ORDER BY, found the end of the text"
                ]
                "a query recovers at each definition and statement"

        testCase "a lexical problem and a syntax problem are both reported"
        <| fun () ->
            Expect.equal
                (errorsOf "IF(a # b, 1 +)")
                [
                    "1:6: unexpected character '#' (U+0023)"
                    "1:14: expected an expression after '+', found ')'"
                ]
                ""

        testCase "no more than 25 errors, however bad the text"
        <| fun () ->
            match parseExpression (String.replicate 200 "F(a b), ") with
            | Error errors -> Expect.isLessThanOrEqual errors.Length 25 ""
            | Ok _ -> failtest "parsed"

            match parseExpression (String('#', 500)) with
            | Error errors -> Expect.equal errors.Length 25 ""
            | Ok _ -> failtest "parsed"

        testCase "null is an empty text"
        <| fun () ->
            Expect.equal
                (parseExpression null |> Result.mapError (List.map ParseError.format))
                (parseExpression "" |> Result.mapError (List.map ParseError.format))
                ""

            wellBehaved null
    ]

// ---- Printing and round trips -------------------------------------------------------------------

/// DAX written the way people write it: every construct, with layout and comments.
let private samples = [
    "1"
    "SUM ( Sales[Amount] )"
    "CALCULATE ( [Total], 'Date'[Year] = 2024, ALL ( Sales ) )"
    "VAR total = [Total]\nVAR prior = CALCULATE ( [Total], DATEADD ( 'Date'[Date], -1, YEAR ) )\nRETURN\n    DIVIDE ( total - prior, prior )"
    "IF ( NOT ISBLANK ( [x] ) && [x] > 0 || [y] = \"a \"\"b\"\"\", 1, -1 )"
    "RANKX ( ALL ( Product ), [Sales], , DESC, Dense )"
    "TOPN ( 10, Sales, [Amount], DESC, Sales[Id], ASC )"
    "SWITCH ( TRUE (), [x] < 0, \"neg\", [x] = 0, \"zero\", \"pos\" )"
    "Sales[Color] IN { \"Red\", \"Blue\" }"
    "( Sales[Color], Sales[Size] ) IN { ( \"Red\", 1 ), ( \"Blue\", 2 ) }"
    "NOT Sales[Color] IN { \"Red\" }"
    "- ( 1 + 2 ) * - 3 ^ - 2"
    "1 - - 1 + + 1"
    "a == b && a <> c && a <= d && a >= e && a < f && a > g"
    "\"a\" & \"b\" & 1 + 2"
    "DATATABLE ( \"Name\", STRING, \"Value\", INTEGER, { { \"a\", 1 }, { \"b\", 2 } } )"
    "CONVERT ( [x], CURRENCY ) + DATEDIFF ( dt\"2024-01-01\", TODAY (), DAY )"
    "INDEX ( 1, ALLSELECTED ( 'Date'[Year] ), ORDERBY ( 'Date'[Year], ASC BLANKS LAST ), DEFAULT, PARTITIONBY ( Sales[Region] ) )"
    "CROSSFILTER ( Sales[DateKey], 'Date'[DateKey], BOTH )"
    "SELECTCOLUMNS ( ADDCOLUMNS ( Sales, \"Net\", [Amount] - [Tax] ), \"Net\", [Net], \"Table\", NAMEOF ( Sales[Amount] ) )"
    "''[Value] + ( ( 1 ) )"
    "IF ( a, , 2 ) + F ( , ) + G ( b, )"
    "VAR x = VAR y = 1 RETURN y RETURN IF ( x, VAR z = { 1 } RETURN COUNTROWS ( z ), 0 )"
    "1.5 + .5 + 2. + 1e3 + 1.5E-3 + 007 + 99999999999999999999"
    "DaxLib.Format.Number ( SELECTEDMEASURE (), \"#,0\" ) & IF.EAGER ( TRUE, \"a\", \"b\" )"
    "true && FALSE || not True"
    "/* lead */ SUM ( -- inner\n Sales[Amount] ) // trail"
    "@p + [x]"
    "(a: INT64, b: SCALAR VAL, c: ANYREF EXPR, d, e: expr = 1 + 2, f: scalar numeric val) => a + b"
    "() => VAR x = 1 RETURN x"
    "EVALUATE Sales"
    "DEFINE\n  MEASURE Sales[Big] = SUM ( Sales[Amount] ) * 2\n  COLUMN 'Sales T'[m] = 1\n  TABLE Top = TOPN ( 3, Sales )\n  VAR lim = VAR a = 1 RETURN a\n  FUNCTION AddTax = ( amount : NUMERIC ) => amount * 1.1\n  MPARAMETER Region = \"West\"\nEVALUATE SUMMARIZECOLUMNS ( 'Date'[Year], \"Big\", [Big] )\nORDER BY 'Date'[Year] DESC, [Big] asc, [x]\nSTART AT 2024, @p\nEVALUATE { AddTax ( lim ) }"
]

[<Tests>]
let printTests =
    testList "DAX printer" [
        testCase "minimal canonical text"
        <| fun () ->
            let printed (dax: string) =
                match parse dax with
                | Ok d -> print d
                | Error errors -> failtestf "%s: %s" dax (messages errors)

            for dax, expected in
                [
                    "sum ( Sales[Amount] )", "sum(Sales[Amount])"
                    "var x=1 return x+1", "VAR x = 1 RETURN x + 1"
                    "not a in {1,2}", "NOT a IN {1, 2}"
                    "- - a - - b", "- -a - -b"
                    "+ + a", "+ +a"
                    "- + a", "- +a"
                    "RANKX(T,[m],,desc)", "RANKX(T, [m], , desc)"
                    "F(,)", "F(, )"
                    "ORDERBY([x], asc blanks last)", "ORDERBY([x], asc blanks last)"
                    "'Sales T'[a]]b]&\"q\"\"\"", "'Sales T'[a]]b] & \"q\"\"\""
                    "true&&False", "TRUE && FALSE"
                    "DT\"2024-01-31\"", "dt\"2024-01-31\""
                    "( ( a ) ,b ) in t", "((a), b) IN t"
                    "(a :int64,b:Scalar  Val=1)=>a", "(a: INT64, b: SCALAR VAL = 1) => a"
                    "define measure T[m]=1 var q=2 evaluate T order by [a] desc,[b] start at 1",
                    "DEFINE\nMEASURE T[m] = 1\nVAR q = 2\nEVALUATE T ORDER BY [a] DESC, [b] START AT 1"
                ] do
                Expect.equal (printed dax) expected dax

        testCase "a tree built by hand gets the parentheses its shape needs"
        <| fun () ->
            let a, b, c = name "a", name "b", name "c"

            for tree, expected in
                [
                    bin BinaryOp.Multiply (bin BinaryOp.Add a b) c, "(a + b) * c"
                    bin BinaryOp.Multiply a (bin BinaryOp.Add b c), "a * (b + c)"
                    bin BinaryOp.Add (bin BinaryOp.Multiply a b) c, "a * b + c"
                    bin BinaryOp.Subtract a (bin BinaryOp.Subtract b c), "a - (b - c)"
                    bin BinaryOp.Subtract (bin BinaryOp.Subtract a b) c, "a - b - c"
                    bin BinaryOp.Power a (bin BinaryOp.Power b c), "a ^ (b ^ c)"
                    bin BinaryOp.Power (un UnaryOp.Minus a) b, "(-a) ^ b"
                    un UnaryOp.Minus (bin BinaryOp.Power a b), "-a ^ b"
                    un UnaryOp.Minus (bin BinaryOp.Multiply a b), "-(a * b)"
                    bin BinaryOp.Multiply (un UnaryOp.Minus a) b, "-a * b"
                    bin BinaryOp.Power (bin BinaryOp.Power a (un UnaryOp.Minus b)) c, "(a ^ -b) ^ c"
                    bin BinaryOp.Multiply (bin BinaryOp.Power a (un UnaryOp.Minus b)) c, "a ^ -b * c"
                    bin BinaryOp.Equal (un UnaryOp.Not a) b, "(NOT a) = b"
                    un UnaryOp.Not (bin BinaryOp.Equal a b), "NOT a = b"
                    un UnaryOp.Not (bin BinaryOp.And a b), "NOT (a && b)"
                    bin BinaryOp.And (un UnaryOp.Not a) b, "NOT a && b"
                    bin BinaryOp.Equal (bin BinaryOp.Add a (un UnaryOp.Not b)) c, "(a + NOT b) = c"
                    bin BinaryOp.Add a (un UnaryOp.Not b), "a + NOT b"
                    bin BinaryOp.Add (k (VarBlock([ var "x" a ], b))) c, "(VAR x = a RETURN b) + c"
                    bin BinaryOp.Add a (k (VarBlock([ var "x" b ], c))), "a + VAR x = b RETURN c"
                    bin BinaryOp.Multiply (bin BinaryOp.Add a (k (VarBlock([ var "x" b ], c)))) c,
                    "(a + VAR x = b RETURN c) * c"
                    un UnaryOp.Minus (un UnaryOp.Minus a), "- -a"
                    call "F" [ k Omitted ], "F()"
                    k (Qualified(tbl "Sales T" false, "x")), "'Sales T'[x]"
                    k (Qualified(tbl "Return" false, "x")), "'Return'[x]"
                ] do
                Expect.equal (printExpression tree) expected expected

                // The text means the tree: parsing it gives the tree, plus the parentheses.
                // (Not for the three a parser cannot give back: F() has no argument, and a table
                // that needed quotes comes back quoted.)
                if not (List.contains expected [ "F()"; "'Sales T'[x]"; "'Return'[x]" ]) then
                    Expect.equal (stripParens (shape expected)) (stripParens tree) expected

        for dax in samples do
            testCase $"round trip: {dax.Split('\n').[0]}" <| fun () -> roundTrips dax

        testCase "printing a parsed tree adds no parentheses"
        <| fun () ->
            for dax, _ in precedence do
                let e = parsed dax

                let count (s: string) =
                    s |> Seq.filter (fun c -> c = '(') |> Seq.length

                Expect.equal (count (printExpression e)) (count dax) dax
                roundTrips dax
    ]

// ---- Random trees -------------------------------------------------------------------------------

module private Gen' =
    let names = [ "a"; "b"; "Sales"; "_x"; "Total.Net"; "Year"; "DESC"; "x1" ]

    let word: Gen<string> = Gen.elements names

    let text: Gen<string> =
        Gen.elements [ ""; "a"; "a b"; "it's"; "say \"hi\""; "x]y"; "two\nlines"; "1,5"; "é ✓" ]

    let tableName: Gen<TableName> =
        Gen.oneof [
            word |> Gen.map (fun n -> tbl n false)
            Gen.elements [ "Sales T"; "it's"; "Date"; ""; "a]b"; "VAR" ]
            |> Gen.map (fun n -> tbl n true)
        ]

    let bracketName: Gen<string> =
        Gen.elements [ "Amount"; "Total Sales"; "a]b"; "x"; "%"; "1" ]

    let leaf: Gen<Expr> =
        Gen.oneof [
            Gen.choose (0, 5000) |> Gen.map int'
            Gen.elements [ "007", 7L; "0", 0L; "9223372036854775807", Int64.MaxValue ]
            |> Gen.map (fun (t, v) -> k (IntegerLiteral(v, t)))
            Gen.elements [
                "1.5", 1.5
                ".5", 0.5
                "2.", 2.0
                "1e3", 1000.0
                "2.5E-3", 0.0025
                "10.25", 10.25
            ]
            |> Gen.map (fun (t, v) -> k (NumberLiteral(v, t)))
            text |> Gen.map str
            Gen.elements [ true; false ] |> Gen.map (BooleanLiteral >> k)
            Gen.elements [ "2024-01-31"; "2024-01-31T13:00:00" ]
            |> Gen.map (DateLiteral >> k)
            word |> Gen.map name
            Gen.elements [ "Sales T"; "it's"; ""; "a b c" ] |> Gen.map (QuotedTable >> k)
            Gen.zip tableName bracketName |> Gen.map (Qualified >> k)
            bracketName |> Gen.map br
            Gen.elements [ "p"; "Start" ] |> Gen.map (QueryParameter >> k)
        ]

    let binaryOp: Gen<BinaryOp> =
        Gen.elements [
            BinaryOp.Add
            BinaryOp.Subtract
            BinaryOp.Multiply
            BinaryOp.Divide
            BinaryOp.Power
            BinaryOp.Concat
            BinaryOp.Equal
            BinaryOp.StrictEqual
            BinaryOp.NotEqual
            BinaryOp.Less
            BinaryOp.LessOrEqual
            BinaryOp.Greater
            BinaryOp.GreaterOrEqual
            BinaryOp.And
            BinaryOp.Or
            BinaryOp.In
        ]

    let unaryOp: Gen<UnaryOp> =
        Gen.elements [ UnaryOp.Plus; UnaryOp.Minus; UnaryOp.Not ]

    let words: Gen<Expr> =
        Gen.elements [
            [ "ASC"; "BLANKS"; "LAST" ]
            [ "ROWS"; "COLUMNS" ]
            [ "DESC"; "BLANKS"; "FIRST" ]
        ]
        |> Gen.map (Words >> k)

    /// Any tree the AST allows that DAX can write: no parentheses unless it chooses to add them.
    let rec expr (size: int) : Gen<Expr> =
        if size <= 0 then
            leaf
        else
            let sub = expr (size / 2)
            let small = expr (size / 3)

            let args =
                gen {
                    let! n = Gen.choose (0, 4)

                    let! items =
                        Gen.listOfLength n (Gen.frequency [ 8, small; 1, Gen.constant (k Omitted); 1, words ])

                    // A lone omitted argument cannot be written: F() has none.
                    return
                        match items with
                        | [ { Kind = Omitted } ] -> []
                        | items -> items
                }

            Gen.frequency [
                2, leaf
                6, Gen.map3 (fun op l r -> bin op l r) binaryOp sub sub
                3, Gen.map2 un unaryOp sub
                3, Gen.map2 call (Gen.elements [ "SUM"; "IF"; "DaxLib.Fmt"; "rankx"; "TRUE" ]) args
                1, sub |> Gen.map (Paren >> k)
                1,
                gen {
                    let! n = Gen.choose (2, 3)
                    let! items = Gen.listOfLength n small
                    return k (Tuple items)
                }
                1,
                gen {
                    let! n = Gen.choose (1, 3)
                    let! rows = Gen.listOfLength n small
                    return k (TableConstructor rows)
                }
                2,
                gen {
                    let! n = Gen.choose (1, 3)
                    let! vars = Gen.listOfLength n (Gen.map2 var word small)
                    let! body = sub
                    return k (VarBlock(vars, body))
                }
            ]

    let parameter: Gen<Parameter> =
        gen {
            let! n = word

            let! ty =
                Gen.elements [
                    None
                    Some ParameterType.AnyVal
                    Some ParameterType.Scalar
                    Some ParameterType.Table
                    Some ParameterType.AnyRef
                    Some ParameterType.CalendarRef
                    Some ParameterType.ColumnRef
                    Some ParameterType.MeasureRef
                    Some ParameterType.TableRef
                ]

            let! subtype =
                if ty.IsNone || ty = Some ParameterType.Scalar then
                    Gen.elements [
                        None
                        Some ScalarType.Variant
                        Some ScalarType.Int64
                        Some ScalarType.Decimal
                        Some ScalarType.Double
                        Some ScalarType.String
                        Some ScalarType.DateTime
                        Some ScalarType.Boolean
                        Some ScalarType.Numeric
                    ]
                else
                    Gen.constant None

            let! mode = Gen.elements [ None; Some ParameterMode.Val; Some ParameterMode.Expr ]
            let! fallback = Gen.frequency [ 3, Gen.constant None; 1, expr 3 |> Gen.map Some ]
            return param n ty subtype mode fallback
        }

    let functionDef (size: int) : Gen<FunctionDef> =
        gen {
            let! n = Gen.choose (0, 3)
            let! parameters = Gen.listOfLength n parameter
            let! body = expr size

            return {
                Parameters = parameters
                Body = body
                Span = Span.none
            }
        }

    let query (size: int) : Gen<Query> =
        let e = expr (size / 2)

        let definition =
            Gen.oneof [
                Gen.map3 (fun t n v -> DefinitionKind.Measure(t, ident n, v)) tableName bracketName e
                Gen.map3 (fun t n v -> DefinitionKind.Column(t, ident n, v)) tableName bracketName e
                Gen.map2 (fun t v -> DefinitionKind.Table(t, v)) tableName e
                Gen.map2 (fun n v -> DefinitionKind.Var(ident n, v)) word e
                Gen.map2 (fun n f -> DefinitionKind.Function(ident n, f)) word (functionDef (size / 2))
                Gen.map2 (fun n v -> DefinitionKind.MParameter(ident n, v)) word e
            ]
            |> Gen.map (fun kind -> { Kind = kind; Span = Span.none })

        let evaluate =
            gen {
                let! table = e
                let! keys = Gen.choose (0, 2)

                let! orderBy =
                    Gen.listOfLength
                        keys
                        (Gen.map2
                            (fun x d -> {
                                Expr = x
                                Direction = d
                                Span = Span.none
                            })
                            e
                            (Gen.elements [ None; Some SortDirection.Asc; Some SortDirection.Desc ]))

                let! starts = if keys = 0 then Gen.constant 0 else Gen.choose (0, 2)
                let! startAt = Gen.listOfLength starts e

                return {
                    Table = table
                    OrderBy = orderBy
                    StartAt = startAt
                    Span = Span.none
                }
            }

        gen {
            let! d = Gen.choose (0, 3)
            let! definitions = Gen.listOfLength d definition
            let! n = Gen.choose (1, 2)
            let! evaluates = Gen.listOfLength n evaluate

            return {
                Definitions = definitions
                Evaluates = evaluates
                Span = Span.none
            }
        }

/// A query with no positions and no `Paren` nodes.
let private withoutParens (q: Query) : Query =
    let x (e: Expr) = stripParens (stripSpans e)

    let fn (f: FunctionDef) =
        let f = FunctionDef.stripSpans f

        {
            f with
                Body = x f.Body
                Parameters =
                    f.Parameters
                    |> List.map (fun p -> {
                        p with
                            Default = p.Default |> Option.map x
                    })
        }

    let q = Query.stripSpans q

    {
        q with
            Definitions =
                q.Definitions
                |> List.map (fun d -> {
                    d with
                        Kind =
                            match d.Kind with
                            | DefinitionKind.Measure(t, n, v) -> DefinitionKind.Measure(t, n, x v)
                            | DefinitionKind.Column(t, n, v) -> DefinitionKind.Column(t, n, x v)
                            | DefinitionKind.Table(t, v) -> DefinitionKind.Table(t, x v)
                            | DefinitionKind.Var(n, v) -> DefinitionKind.Var(n, x v)
                            | DefinitionKind.Function(n, f) -> DefinitionKind.Function(n, fn f)
                            | DefinitionKind.MParameter(n, v) -> DefinitionKind.MParameter(n, x v)
                })
            Evaluates =
                q.Evaluates
                |> List.map (fun e -> {
                    e with
                        Table = x e.Table
                        OrderBy = e.OrderBy |> List.map (fun o -> { o with Expr = x o.Expr })
                        StartAt = e.StartAt |> List.map x
                })
    }

type private Trees =
    static member Expr() =
        Gen.sized (fun n -> Gen'.expr (min n 40)) |> Arb.fromGen

    static member FunctionDef() =
        Gen.sized (fun n -> Gen'.functionDef (min n 30)) |> Arb.fromGen

    static member Query() =
        Gen.sized (fun n -> Gen'.query (min n 30)) |> Arb.fromGen

let private treeConfig = {
    FsCheckConfig.defaultConfig with
        maxTest = 1500
        endSize = 60
        arbitrary = [ typeof<Trees> ]
}

/// A quoted table whose name could have been bare prints with its quotes; a bare one whose name
/// needs quotes gets them. The generator only makes bare names that can stay bare.
let private sameTree (dialect: bool) (tree: Expr) =
    let printOptions = { PrintOptions.Semicolons = dialect }
    let parseOptions = { ParseOptions.Semicolons = dialect }
    let text = printExpressionWith printOptions tree

    match parseExpressionWith parseOptions text with
    | Error errors -> failtestf "%s\n  from %A\n  does not parse: %s" text tree (messages errors)
    | Ok first ->
        // The text means the tree: only parentheses were added.
        Expect.equal (stripParens (stripSpans first)) (stripParens tree) text
        // And a parsed tree round-trips exactly: same text, same tree, same positions.
        Expect.equal (printExpressionWith printOptions first) text "printing a parsed tree adds nothing"
        Expect.equal (parseExpressionWith parseOptions text) (Ok first) "parsing is deterministic"

        if not dialect then
            Expect.equal (tokensOf (printExpression first)) (tokensOf text) ""

[<Tests>]
let propertyTests =
    testList "DAX parser: random trees" [
        testPropertyWithConfig treeConfig "print then parse gives the tree back, parentheses aside"
        <| fun (tree: Expr) -> sameTree false tree

        testPropertyWithConfig treeConfig "the same in the semicolon dialect"
        <| fun (tree: Expr) -> sameTree true tree

        testPropertyWithConfig treeConfig "function definitions round-trip"
        <| fun (f: FunctionDef) ->
            let text = printFunction f

            match parseFunction text with
            | Error errors -> failtestf "%s\n  does not parse: %s" text (messages errors)
            | Ok first ->
                let strip (x: FunctionDef) =
                    let wrapped = {
                        Definitions = [
                            {
                                Kind = DefinitionKind.Function(ident "f", x)
                                Span = Span.none
                            }
                        ]
                        Evaluates = []
                        Span = Span.none
                    }

                    withoutParens wrapped

                Expect.equal (strip first) (strip f) text
                Expect.equal (printFunction first) text "stable"

                match parse text with
                | Ok(Document.Function again) -> Expect.equal again first "parse sees a function"
                | other -> failtestf "%s\n  read as %A" text other

        testPropertyWithConfig treeConfig "queries round-trip"
        <| fun (q: Query) ->
            let text = printQuery q

            match parseQuery text with
            | Error errors -> failtestf "%s\n  does not parse: %s" text (messages errors)
            | Ok first ->
                Expect.equal (printQuery first) text "stable"

                Expect.equal
                    (parseQuery (printQuery first) |> Result.map Query.stripSpans)
                    (Ok(Query.stripSpans first))
                    text

                Expect.equal (withoutParens first) (withoutParens q) text

        testPropertyWithConfig treeConfig "every node's span lies inside its parent's, in order"
        <| fun (tree: Expr) ->
            let text = printExpression tree

            match parseExpression text with
            | Error errors -> failtestf "%s: %s" text (messages errors)
            | Ok e ->
                Expect.equal e.Span { Start = 0; Length = text.Length } text

                for parent in descendants e do
                    let mutable at = parent.Span.Start

                    for child in children parent do
                        if child.Span.Start < at || child.Span.End > parent.Span.End then
                            failtestf "%s: %A is outside or out of order in %A" text child.Span parent.Span

                        at <- child.Span.End
    ]

// ---- Fuzz ---------------------------------------------------------------------------------------

let private soupTokens = [
    "("
    ")"
    "{"
    "}"
    ","
    ";"
    "+"
    "-"
    "*"
    "/"
    "^"
    "&"
    "="
    "=="
    "<>"
    "<"
    "<="
    ">"
    ">="
    "&&"
    "||"
    "=>"
    ":"
    "IN"
    "NOT"
    "VAR"
    "RETURN"
    "DEFINE"
    "EVALUATE"
    "MEASURE"
    "COLUMN"
    "TABLE"
    "FUNCTION"
    "MPARAMETER"
    "ORDER"
    "BY"
    "START"
    "AT"
    "ASC"
    "DESC"
    "BLANKS"
    "TRUE"
    "INT64"
    "EXPR"
    "x"
    "Sales"
    "SUM"
    "[m]"
    "'T t'"
    "Sales[a]"
    "1"
    "1.5"
    ".5"
    "\"s\""
    "dt\"2024-01-01\""
    "@p"
    "--c\n"
    "/*c*/"
    "\n"
    // Broken things.
    "\""
    "'"
    "["
    "/*"
    "!"
    "!="
    "|"
    "#"
    "\u201C"
    "dt\""
    "1e"
    "..."
    "\000"
    "\uD83D"
]

let private soup: Gen<string> =
    gen {
        let! n = Gen.choose (0, 60)
        let! parts = Gen.listOfLength n (Gen.elements soupTokens)
        let! gaps = Gen.listOfLength n (Gen.elements [ " "; ""; " "; "\n"; "\t" ])
        return List.zip parts gaps |> List.map (fun (p, g) -> p + g) |> String.concat ""
    }

type private Soup =
    static member String() = Arb.fromGen soup

let private soupConfig = {
    FsCheckConfig.defaultConfig with
        maxTest = 3000
        arbitrary = [ typeof<Soup> ]
}

let private anyStringConfig = {
    FsCheckConfig.defaultConfig with
        maxTest = 2000
}

/// Every prefix of a text, and the text with each single token removed.
let private mutilations (dax: string) : string seq =
    seq {
        let step = max 1 (dax.Length / 400)

        for i in 0..step .. dax.Length do
            yield dax.Substring(0, i)

        let tokens = tokenize dax |> significant |> Array.ofList
        let tokenStep = max 1 (tokens.Length / 200)

        for i in 0..tokenStep .. tokens.Length - 1 do
            yield dax.Remove(tokens.[i].Start, tokens.[i].Length)
    }

[<Tests>]
let fuzzTests =
    testList "DAX parser: fuzz" [
        testPropertyWithConfig soupConfig "token soup never throws, and its errors are usable"
        <| fun (text: string) -> wellBehaved text

        testPropertyWithConfig anyStringConfig "any string never throws"
        <| fun (text: string) -> wellBehaved text

        testPropertyWithConfig treeConfig "a mangled tree never throws"
        <| fun (tree: Expr) (cut: int) ->
            let text = printExpression tree
            let at = if text.Length = 0 then 0 else abs cut % (text.Length + 1)
            wellBehaved (text.Substring(0, at))
            wellBehaved (text.Substring at)
            wellBehaved (text.Substring(at) + text.Substring(0, at))

        testCase "truncated and token-dropped samples never throw"
        <| fun () ->
            for dax in samples do
                for broken in mutilations dax do
                    wellBehaved broken

        testCase "deep nesting is an error, not a stack overflow"
        <| fun () ->
            let deep = 10000

            let cases = [
                "(", String.replicate deep "(" + "1" + String.replicate deep ")"
                "( unclosed", String.replicate deep "("
                "{", String.replicate deep "{" + "1" + String.replicate deep "}"
                "{ unclosed", String.replicate deep "{"
                "F(", String.replicate deep "F(" + "1" + String.replicate deep ")"
                "F(a, unclosed", String.replicate deep "F(a, "
                "-", String.replicate deep "- " + "1"
                "NOT", String.replicate deep "NOT " + "1"
                "1 +", String.replicate deep "1 + (" + "1" + String.replicate deep ")"
                "VAR", String.replicate deep "VAR x = " + "1" + String.replicate deep " RETURN x"
                "VAR body", String.replicate deep "VAR x = 1 RETURN " + "1"
                "rising operators", String.replicate deep "a || b && c = d & e + f * g ^ ("
                "=> ", "(" + String.replicate deep "a = (" + "1"
            ]

            for what, text in cases do
                bounded what (fun () ->
                    match parseExpression text with
                    | Ok _ -> failtestf "%s: parsed" what
                    | Error errors ->
                        Expect.isNonEmpty errors what

                        Expect.isTrue
                            (errors |> List.exists (fun e -> e.Message.Contains "nested too deeply"))
                            $"{what}: {messages errors}"

                    wellBehaved text)

        testCase "nesting up to the limit parses"
        <| fun () ->
            for opener, closer in [ "(", ")"; "{", "}"; "F(", ")"; "- ", ""; "NOT ", ""; "IF(a, 1 + ", ")" ] do
                let levels = if opener = "IF(a, 1 + " then 120 else 250
                let text = String.replicate levels opener + "1" + String.replicate levels closer

                boundedOn 4096 opener (fun () ->
                    match parseExpression text with
                    | Ok e ->
                        Expect.isGreaterThanOrEqual (depth e) levels opener

                        Expect.equal
                            (parseExpression (printExpression e) |> Result.map stripSpans)
                            (Ok(stripSpans e))
                            opener
                    | Error errors -> failtestf "%s: %s" opener (messages errors))

        testCase "on a stack too small for the nesting, an error rather than a crash"
        <| fun () ->
            for opener in [ "("; "{"; "F("; "- "; "IF(a, 1 + "; "VAR x = " ] do
                boundedOn 160 opener (fun () ->
                    match parseExpression (String.replicate 250 opener + "1") with
                    // How much stack a level costs, and the smallest stack a thread can have, differ by
                    // platform: where 160 KB turns out to be enough (seen on macOS arm64), parsing is fine.
                    | Ok _ when
                        not (
                            System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform
                                System.Runtime.InteropServices.OSPlatform.Linux
                        )
                        ->
                        ()
                    | Ok _ -> failtestf "%s parsed" opener
                    | Error errors -> Expect.isNonEmpty errors opener)

        testCase "walking a hand-built tree deeper than the stack raises, it does not crash"
        <| fun () ->
            let mutable tree = int' 1

            for _ in 1..200000 do
                tree <- un UnaryOp.Minus tree

            Expect.equal (depth tree) 200001 "depth does not recurse"

            for what, walk in
                [
                    "print", (fun (e: Expr) -> printExpression e |> ignore)
                    "stripSpans", (fun e -> stripSpans e |> ignore)
                    "stripParens", (fun e -> stripParens e |> ignore)
                    "descendants", (fun e -> descendants e |> ignore)
                    "references", (fun e -> references e |> ignore)
                ] do
                boundedDefault what (fun () ->
                    Expect.throwsT<InsufficientExecutionStackException> (fun () -> walk tree) what)

        testCase "a long chain of operators is fine up to maxDepth levels, and an error beyond"
        <| fun () ->
            let chain (n: int) (op: string) =
                String.Join(op, Array.create (n + 1) "a")

            boundedDefault "chains" (fun () ->
                for op in [ " + "; " && "; " & "; " ^ "; " = " ] do
                    match parseExpression (chain (maxDepth - 1) op) with
                    | Ok e ->
                        Expect.equal (depth e) maxDepth op
                        Expect.equal (printExpression e) (chain (maxDepth - 1) op) op
                    | Error errors -> failtestf "%s: %s" op (messages errors)

                    for n in [ maxDepth; 100000 ] do
                        match parseExpression (chain n op) with
                        | Ok _ -> failtestf "%d of %s parsed" n op
                        | Error [ e ] ->
                            Expect.stringContains e.Message "too deep" op
                            Expect.equal (e.Line, e.Column) (1, 1) op
                        | Error errors -> failtestf "%s: %s" op (messages errors))

        testCase "long flat inputs take linear time"
        <| fun () ->
            let args = String.Join(", ", Array.create 50000 "Sales[Amount] * 2")

            bounded "many arguments" (fun () ->
                match parseExpression ("SWITCH(" + args + ")") with
                | Ok { Kind = Call(_, items) } -> Expect.equal items.Length 50000 ""
                | other -> failtestf "%A" (other |> Result.map (fun _ -> ())))

            bounded "many errors" (fun () -> wellBehaved (String.replicate 50000 "F(a b), "))
            bounded "many unclosed" (fun () -> wellBehaved (String.replicate 50000 "F(a, "))

            bounded "many statements" (fun () ->
                wellBehaved (String.replicate 20000 "EVALUATE T ORDER BY [x] MEASURE "))

            bounded "many definitions" (fun () -> wellBehaved ("DEFINE " + String.replicate 20000 "VAR x = 1 + "))
    ]

// ---- Names, and the cross-check with Deps -------------------------------------------------------

/// Why an AST name and Deps may disagree. Everything else must agree exactly.
[<RequireQualifiedAccess>]
type private Difference =
    /// A call to something that is not a user-defined function of the model: Deps only reports UDFs.
    | NotAModelFunction
    /// A bare word that is no table or calendar of the model (a keyword argument, an unknown
    /// name): Deps only reports bare words that are tables.
    | NotAModelTable
    /// `[X]` where X is also a string in the expression (a column ADDCOLUMNS, SELECTCOLUMNS or
    /// SUMMARIZE names) or a column the calculated table itself produces: Deps takes it as local.
    | LocalColumn
    /// `x[Col]` where x is a VAR or parameter, or `''[Col]`: no model table is named.
    | LocalTable
    /// Deps knows VAR names for the whole expression, the AST only where they are in scope: a
    /// table name used outside the VAR that shadows it is a table to the AST, a variable to Deps.
    | DepsScopeIsFlat
    /// Deps reported a bare word the AST knows is a parameter (Deps only sees typed parameters) or
    /// a word in the parameter list.
    | InParameterList

/// Compare the names the AST finds in one expression with what Deps extracts from it. Returns the
/// number of references both agree on and every difference with its reason; fails on any
/// difference that has no reason.
let private crossCheck
    (db: Microsoft.AnalysisServices.Tabular.Database)
    (resolve: Deps.DaxSource -> Deps.Reference list)
    (src: Deps.DaxSource)
    =
    let isFunction = src.Owner.Ref.Kind = ObjectKind.Function
    let model = db.Model

    let eq (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let astNames, bodyStart, parameters =
        if isFunction then
            match parseFunction src.Dax with
            | Ok f -> functionReferences f, f.Body.Span.Start, f.Parameters |> List.map (fun p -> p.Name.Name)
            | Error errors -> failtestf "%s: %s" src.Dax (messages errors)
        else
            match parseExpression src.Dax with
            | Ok e -> references e, 0, []
            | Error errors -> failtestf "%s: %s" src.Dax (messages errors)

    let deps =
        resolve src
        |> List.choose (fun r ->
            match r.Start, r.Length with
            | Some s, Some l -> Some((s, l), r)
            | _ -> None)

    let tokens = tokenize src.Dax |> significant

    let strings =
        tokens
        |> List.filter (fun t -> t.Kind = TokenKind.String)
        |> List.map (fun t -> t.Text)

    let varNames =
        tokens
        |> List.pairwise
        |> List.choose (fun (a, b) ->
            if
                a.Kind = TokenKind.Identifier
                && eq a.Text "VAR"
                && b.Kind = TokenKind.Identifier
            then
                Some b.Text
            else
                None)

    let isTable (n: string) =
        Inventory.objects db
        |> List.exists (fun e ->
            (e.Ref.Kind = ObjectKind.Table || e.Ref.Kind = ObjectKind.Calendar)
            && eq (ObjectRef.name e.Ref) n)

    let isUdf (n: string) =
        model.Functions |> Seq.exists (fun f -> eq f.Name n)

    let bySpan = deps |> List.map fst |> Set.ofList

    let astBySpan =
        astNames |> List.map (fun r -> (r.Span.Start, r.Span.Length), r) |> Map.ofList

    let differences = ResizeArray<Difference * string>()
    let mutable agreed = 0

    // Everything Deps found is a name the AST found, of the same kind.
    for (span, r) in deps do
        match Map.tryFind span astBySpan with
        | Some a ->
            let expected =
                if r.Text.EndsWith "]" && not (r.Text.StartsWith "[") then
                    NameKind.Qualified
                elif r.Text.StartsWith "[" then
                    NameKind.Bracketed
                elif r.Text.StartsWith "'" then
                    NameKind.QuotedTable
                else
                    a.Kind

            Expect.equal a.Kind expected $"{r.Text} in {src.Dax}"

            match a.Kind, r.Target with
            | NameKind.Function, Deps.Resolved t -> Expect.equal t.Kind ObjectKind.Function r.Text
            | NameKind.Bare, Deps.Resolved t ->
                Expect.isTrue (t.Kind = ObjectKind.Table || t.Kind = ObjectKind.Calendar) r.Text
            | (NameKind.Function | NameKind.Bare), Deps.Unresolved _ ->
                failtestf "Deps reports unresolved %s, which it never should for a bare word" r.Text
            | _ -> ()

            agreed <- agreed + 1
        | None ->
            let bare = not (r.Text.Contains "[") && not (r.Text.StartsWith "'")

            if
                isFunction
                && bare
                && (fst span < bodyStart || parameters |> List.exists (eq r.Text))
            then
                differences.Add(Difference.InParameterList, r.Text)
            else
                failtestf "Deps found %s at %d in\n%s\n  but the AST has no name there" r.Text (fst span) src.Dax

    // Every name the AST found that Deps did not report has a reason.
    for a in astNames do
        if not (bySpan.Contains((a.Span.Start, a.Span.Length))) then
            let text = src.Dax.Substring(a.Span.Start, a.Span.Length)

            let reason =
                match a.Kind with
                | NameKind.Function when not (isUdf a.Name) -> Some Difference.NotAModelFunction
                | NameKind.Bare when not (isTable a.Name) -> Some Difference.NotAModelTable
                | NameKind.Bare when varNames |> List.exists (eq a.Name) -> Some Difference.DepsScopeIsFlat
                | NameKind.Bracketed when
                    strings |> List.exists (eq a.Name)
                    || src.ProducedColumns |> List.exists (eq a.Name)
                    ->
                    Some Difference.LocalColumn
                | NameKind.Qualified when a.LocalTable || a.Table = Some "" -> Some Difference.LocalTable
                | NameKind.Qualified when
                    not (text.StartsWith "'")
                    && (varNames @ parameters) |> List.exists (eq (defaultArg a.Table ""))
                    ->
                    Some Difference.DepsScopeIsFlat
                | _ -> None

            match reason with
            | Some d -> differences.Add(d, text)
            | None ->
                failtestf
                    "the AST found %s (%A) at %d in\n%s\n  but Deps did not, for no known reason"
                    text
                    a.Kind
                    a.Span.Start
                    src.Dax

    agreed, List.ofSeq differences

[<Tests>]
let nameTests =
    testList "DAX parser: names" [
        testCase "references: every name that points outside the expression, VARs in scope left out"
        <| fun () ->
            let dax =
                "VAR t = FILTER ( Sales, Sales[Amount] > [Limit] )\nVAR n = COUNTROWS ( t )\nRETURN IF ( n > 0, SUMX ( t, t[Amount] ) / n, 'Other T'[x] + Year + MyLib.Calc ( DESC ) )"

            let refs = references (parsed dax)

            Expect.equal
                (refs
                 |> List.map (fun r ->
                     r.Kind, r.Table, r.Name, dax.Substring(r.Span.Start, r.Span.Length), r.LocalTable))
                [
                    NameKind.Function, None, "FILTER", "FILTER", false
                    NameKind.Bare, None, "Sales", "Sales", false
                    NameKind.Qualified, Some "Sales", "Amount", "Sales[Amount]", false
                    NameKind.Bracketed, None, "Limit", "[Limit]", false
                    NameKind.Function, None, "COUNTROWS", "COUNTROWS", false
                    NameKind.Function, None, "IF", "IF", false
                    NameKind.Function, None, "SUMX", "SUMX", false
                    NameKind.Qualified, Some "t", "Amount", "t[Amount]", true
                    NameKind.Qualified, Some "Other T", "x", "'Other T'[x]", false
                    NameKind.Bare, None, "Year", "Year", false
                    NameKind.Function, None, "MyLib.Calc", "MyLib.Calc", false
                    NameKind.Bare, None, "DESC", "DESC", false
                ]
                ""

        testCase "a VAR is in scope after its declaration, not in its own value or outside its block"
        <| fun () ->
            let bare (dax: string) =
                references (parsed dax)
                |> List.filter (fun r -> r.Kind = NameKind.Bare)
                |> List.map (fun r -> r.Name)

            Expect.equal (bare "VAR Sales = Sales RETURN Sales") [ "Sales" ] "its own value sees the table"
            Expect.equal (bare "VAR a = 1 VAR b = a RETURN a + b + c") [ "c" ] ""

            Expect.equal
                (bare "IF ( x, VAR a = 1 RETURN a, a )")
                [ "x"; "a" ]
                "outside the block, a is not the variable"

            Expect.equal (bare "VAR A = 1 RETURN a") [] "names are case-insensitive"

            Expect.equal
                (referencesWith [ "q" ] (parsed "q + r") |> List.map (fun r -> r.Name))
                [ "r" ]
                "declared outside"

        testCase "a function's parameters are in scope in its body and defaults"
        <| fun () ->
            match parseFunction "(a: INT64, b = a + c) => a * b + d + F ( a )" with
            | Ok f -> Expect.equal (functionReferences f |> List.map (fun r -> r.Name)) [ "c"; "d"; "F" ] ""
            | Error errors -> failtestf "%s" (messages errors)

        testCase "reserved words"
        <| fun () ->
            for w in [ "VAR"; "return"; "In"; "NOT"; "DEFINE"; "evaluate" ] do
                Expect.isTrue (isReserved w) w

            for w in [ "TRUE"; "ORDER"; "MEASURE"; "Sales"; "TABLE" ] do
                Expect.isFalse (isReserved w) w
    ]

/// The sample model plus expressions that exercise every way the AST and Deps may differ.
let private crossCheckDatabase () =
    let db = Fixtures.sampleDatabase ()
    let sales = db.Model.Tables.["Sales"]

    let add (n: string) (dax: string) =
        sales.Measures.Add(new Microsoft.AnalysisServices.Tabular.Measure(Name = n, Expression = dax))

    add "Plain" "CALCULATE ( [Total], 'Date'[Year] = 2024, ALL ( Sales ), Sales[Amount] > 0 )"

    add
        "Vars"
        "VAR Date = 1 VAR t = FILTER ( Sales, [Amount] > Date ) RETURN SUMX ( t, t[Amount] ) + COUNTROWS ( 'Date' )"

    add "Scoped" "IF ( [Total] > 0, VAR Date = 1 RETURN Date, COUNTROWS ( Date ) )"
    add "Local columns" "SUMX ( ADDCOLUMNS ( Sales, \"Net\", [Amount] * 2 ), [Net] ) + SUMX ( { 1 }, ''[Value] )"
    add "Udf" "Double ( [Total] ) + DOUBLE ( 2 ) + Unknown.Fn ( 1 )"
    add "Keywords" "RANKX ( ALL ( Sales ), [Total], , DESC, Dense ) + DATEDIFF ( dt\"2024-01-01\", TODAY (), YEAR )"
    add "Unresolved" "'No Such'[x] + [Nope] + COUNTROWS ( 'Missing' ) + NoTable[y]"
    add "Nameof" "SELECTCOLUMNS ( Sales, \"Total\", NAMEOF ( [Total] ) )"
    add "Not" "IF ( NOT [Total] IN { 1 }, 1 )"

    db.Model.Functions.Add(
        new Microsoft.AnalysisServices.Tabular.Function(
            Name = "Scale",
            Expression =
                "( Sales, factor: DOUBLE = 2, t: TABLE ) => SUMX ( t, [Amount] ) * factor + COUNTROWS ( Sales ) + Double ( 1 )"
        )
    )

    db

/// Every `Expression = "..."` literal in the test sources: the fixtures' DAX.
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
         |> Array.filter (fun f -> Path.GetFileName f <> "DaxParserTests.fs")
         |> Array.collect (fun f ->
             Regex.Matches(File.ReadAllText f, "Expression = \"((?:[^\"\\\\]|\\\\.)*)\"")
             |> Seq.map (fun m -> unescape m.Groups.[1].Value)
             |> Array.ofSeq)
         |> Array.distinct
         |> List.ofArray)

/// Parse a model's every DAX expression (functions as function definitions), and check each one
/// round-trips and agrees with Deps. Returns the differences found, by reason.
let private checkModel (db: Microsoft.AnalysisServices.Tabular.Database) =
    let entries = Inventory.objects db
    let sources = Deps.daxSources entries
    let resolve = Deps.resolver entries
    let mutable agreed = 0
    let differences = ResizeArray<Difference * string>()

    for src in sources do
        let result =
            if src.Owner.Ref.Kind = ObjectKind.Function then
                parseFunction src.Dax |> Result.map ignore
            else
                parseExpression src.Dax |> Result.map ignore

        match result with
        | Ok() -> ()
        | Error errors ->
            failtestf
                "%s %s does not parse: %s\n%s"
                (ObjectRef.format src.Owner.Ref)
                src.Property
                (messages errors)
                src.Dax

        roundTrips src.Dax
        let a, d = crossCheck db resolve src
        agreed <- agreed + a
        differences.AddRange d

    sources, agreed, List.ofSeq differences

[<Tests>]
let corpusTests =
    testList "DAX parser: corpus" [
        testCase "every DAX expression in the fixture model parses, round-trips and agrees with Deps"
        <| fun () ->
            let sources, agreed, differences = checkModel (crossCheckDatabase ())
            Expect.isGreaterThan sources.Length 12 "measures, calculation items, a function"
            Expect.isGreaterThan agreed 20 "references both found"

            let reasons = differences |> List.map fst |> List.distinct |> Set.ofList

            Expect.equal
                reasons
                (Set.ofList [
                    Difference.NotAModelFunction
                    Difference.NotAModelTable
                    Difference.LocalColumn
                    Difference.LocalTable
                    Difference.DepsScopeIsFlat
                    Difference.InParameterList
                ])
                "the fixture shows every reason the two may differ"

            let of' (d: Difference) =
                differences
                |> List.filter (fun (x, _) -> x = d)
                |> List.map snd
                |> List.distinct

            Expect.containsAll (of' Difference.NotAModelFunction) [ "CALCULATE"; "Unknown.Fn"; "NAMEOF" ] ""
            Expect.containsAll (of' Difference.NotAModelTable) [ "DESC"; "Dense"; "YEAR" ] ""

            Expect.equal
                (of' Difference.LocalColumn)
                [ "[Net]" ]
                "NAMEOF([Total]) is not local, though \"Total\" is a string"

            Expect.containsAll (of' Difference.LocalTable) [ "t[Amount]"; "''[Value]" ] ""
            Expect.equal (of' Difference.DepsScopeIsFlat) [ "Date" ] "COUNTROWS ( Date ) outside the VAR Date block"
            Expect.equal (of' Difference.InParameterList) [ "Sales" ] "an untyped parameter named like a table"

        testCase "row-level security filters, detail rows, format strings, KPIs and calculated tables parse"
        <| fun () ->
            let db = Fixtures.sampleDatabase ()
            let m = db.Model
            let sales = m.Tables.["Sales"]
            let total = sales.Measures.["Total"]

            total.FormatStringDefinition <-
                new Microsoft.AnalysisServices.Tabular.FormatStringDefinition(
                    Expression = "IF ( SELECTEDMEASURE () > 1e6, \"#,0,,.0M\", \"#,0\" )"
                )

            total.DetailRowsDefinition <-
                new Microsoft.AnalysisServices.Tabular.DetailRowsDefinition(
                    Expression = "SELECTCOLUMNS ( Sales, \"Amount\", Sales[Amount], \"Key\", Sales[DateKey] )"
                )

            total.KPI <-
                new Microsoft.AnalysisServices.Tabular.KPI(
                    StatusExpression = "VAR x = [Total] RETURN IF ( x > 0, 1, -1 )",
                    TargetExpression = "[Total] * 1.1",
                    TrendExpression = "IF ( [Total] >= [Total (hidden)], 1, 0 )"
                )

            sales.DefaultDetailRowsDefinition <-
                new Microsoft.AnalysisServices.Tabular.DetailRowsDefinition(
                    Expression = "TOPN ( 100, Sales, Sales[Amount], DESC )"
                )

            sales.Columns.Add(
                new Microsoft.AnalysisServices.Tabular.CalculatedColumn(
                    Name = "Double Amount",
                    Expression = "Sales[Amount] * 2 + RELATED ( 'Date'[Year] )"
                )
            )

            let calc = new Microsoft.AnalysisServices.Tabular.Table(Name = "Years")

            calc.Partitions.Add(
                new Microsoft.AnalysisServices.Tabular.Partition(
                    Name = "Years",
                    Source =
                        new Microsoft.AnalysisServices.Tabular.CalculatedPartitionSource(
                            Expression = "ADDCOLUMNS ( DISTINCT ( 'Date'[Year] ), \"Next\", [Year] + 1 )"
                        )
                )
            )

            m.Tables.Add calc

            let role = m.Roles.["Readers"]

            role.TablePermissions.Add(
                new Microsoft.AnalysisServices.Tabular.TablePermission(
                    Table = sales,
                    FilterExpression = "Sales[DateKey] >= 20240101 && [Amount] > 0 || USERPRINCIPALNAME () = \"a@b.c\""
                )
            )

            m.Tables.["Time Intelligence"].CalculationGroup.CalculationItems.["YTD"].FormatStringDefinition <-
                new Microsoft.AnalysisServices.Tabular.FormatStringDefinition(Expression = "\"0.0%\"")

            let sources, agreed, _ = checkModel db
            let properties = sources |> List.map (fun s -> s.Property) |> List.distinct

            for expected in
                [
                    "expression"
                    "formatStringDefinition.expression"
                    "detailRowsDefinition.expression"
                    "kpi.statusExpression"
                    "kpi.targetExpression"
                    "kpi.trendExpression"
                    "defaultDetailRowsDefinition.expression"
                    "tablePermissions.Sales.filterExpression"
                ] do
                Expect.contains properties expected expected

            Expect.isGreaterThan agreed 15 ""

        testCase "every Expression = \"...\" in the test sources: whatever Format accepts, the parser accepts"
        <| fun () ->
            let corpus = fixtureExpressions.Force()
            Expect.isGreaterThan corpus.Length 50 "the corpus was found"
            let mutable parsedCount = 0

            // What Format lets through that is not DAX. Format only needs balanced, tokenisable
            // text; these are the fixtures that rely on that.
            let notDax (dax: string) (errors: ParseError list) =
                errors
                |> List.forall (fun e ->
                    // An empty expression: nothing to format, nothing to parse.
                    String.IsNullOrWhiteSpace dax
                    // Power Query (M) in the fixtures: `let x = 1 in x`, `"..." meta [...]`.
                    || dax.StartsWith "let "
                    || dax.Contains " meta ["
                    // `a NOT IN b`, which Format keeps as written but DAX does not have.
                    || e.Message.StartsWith "DAX has no NOT IN")

            for dax in corpus do
                wellBehaved dax

                for broken in mutilations dax |> Seq.truncate 60 do
                    wellBehaved broken

                match parse dax with
                | Ok _ ->
                    parsedCount <- parsedCount + 1
                    roundTrips dax
                | Error errors ->
                    match Format.format Format.FormatOptions.defaults dax with
                    | Ok _ when not (notDax dax errors) ->
                        failtestf "Format accepts this, the parser does not: %s\n%s" (messages errors) dax
                    | _ -> ()

            Expect.isGreaterThan (float parsedCount) (0.85 * float corpus.Length) "nearly all fixtures are DAX"
    ]

// ---- A real model (optional) --------------------------------------------------------------------

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

[<Tests>]
let sampleTests =
    testList "DAX parser: sample model (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            let load () =
                match ModelIO.loadPath path with
                | Ok(_, db) -> db
                | Error e -> failtestf "%s" (TessError.message e)

            testCase "every DAX expression parses with zero errors, round-trips, and agrees with Deps"
            <| fun () ->
                let sources, agreed, differences = checkModel (load ())
                Expect.isNonEmpty sources ""
                Expect.isGreaterThan agreed 0 "model references found by both"

                // Whatever the model, nothing the AST and Deps disagree on is unexplained
                // (checkModel fails on those); say what the explained ones were.
                let summary =
                    differences
                    |> List.countBy fst
                    |> List.map (fun (d, n) -> $"%A{d}: {n}")
                    |> String.concat ", "

                printfn
                    "DAX parser, %s: %d expressions, %d references agreed with Deps; differences: %s"
                    path
                    sources.Length
                    agreed
                    summary

            testCase "truncated and token-dropped expressions never throw"
            <| fun () ->
                for src in Deps.daxSources (Inventory.objects (load ())) do
                    for broken in mutilations src.Dax do
                        wellBehaved broken

            testCase "whatever Format accepts, the parser accepts"
            <| fun () ->
                for src in Deps.daxSources (Inventory.objects (load ())) do
                    match Format.format Format.FormatOptions.defaults src.Dax with
                    | Ok formatted ->
                        match parse src.Dax, parse formatted with
                        | Ok a, Ok b ->
                            let strip (d: Document) =
                                match d with
                                | Document.Expression e -> Document.Expression(stripSpans e)
                                | Document.Function f -> Document.Function(FunctionDef.stripSpans f)
                                | Document.Query q -> Document.Query(Query.stripSpans q)

                            Expect.equal
                                (strip b)
                                (strip a)
                                $"formatting keeps the tree: {ObjectRef.format src.Owner.Ref}"
                        | Error errors, _
                        | _, Error errors -> failtestf "%s: %s" (ObjectRef.format src.Owner.Ref) (messages errors)
                    | Error _ -> ()
    ]
