module Tessera.Tests.DaxTests

open Expecto
open FsCheck.FSharp
open Tessera
open Tessera.Dax

let private kinds (dax: string) =
    tokenize dax |> significant |> List.map (fun t -> t.Kind, t.Text)

[<Tests>]
let tests =
    testList "Dax.tokenize" [
        testCase "names, brackets, quoted tables, functions"
        <| fun () ->
            Expect.equal
                (kinds "CALCULATE([Total], 'Sales Table'[Amount] > 0, Sales[Qty])")
                [
                    TokenKind.Identifier, "CALCULATE"
                    TokenKind.Punctuation, "("
                    TokenKind.Bracketed, "Total"
                    TokenKind.Punctuation, ","
                    TokenKind.QuotedTable, "Sales Table"
                    TokenKind.Bracketed, "Amount"
                    TokenKind.Operator, ">"
                    TokenKind.Number, "0"
                    TokenKind.Punctuation, ","
                    TokenKind.Identifier, "Sales"
                    TokenKind.Bracketed, "Qty"
                    TokenKind.Punctuation, ")"
                ]
                ""

        testCase "escapes inside quotes, brackets and strings"
        <| fun () ->
            Expect.equal
                (kinds "'O''Brien'[A]]B] & \"say \"\"hi\"\"\"")
                [
                    TokenKind.QuotedTable, "O'Brien"
                    TokenKind.Bracketed, "A]B"
                    TokenKind.Operator, "&"
                    TokenKind.String, "say \"hi\""
                ]
                ""

        testCase "comments: //, --, /* */"
        <| fun () ->
            let dax = "1 // a [X]\n-- b 'Y'\n/* c\n[Z] */ 2"
            Expect.equal (kinds dax) [ TokenKind.Number, "1"; TokenKind.Number, "2" ] ""

            let comments =
                tokenize dax |> List.filter (fun t -> t.Kind = TokenKind.Comment) |> List.length

            Expect.equal comments 3 ""

        testCase "operators, longest first"
        <| fun () ->
            Expect.equal
                (kinds "a <= b && c <> d || e >= f => g")
                [
                    TokenKind.Identifier, "a"
                    TokenKind.Operator, "<="
                    TokenKind.Identifier, "b"
                    TokenKind.Operator, "&&"
                    TokenKind.Identifier, "c"
                    TokenKind.Operator, "<>"
                    TokenKind.Identifier, "d"
                    TokenKind.Operator, "||"
                    TokenKind.Identifier, "e"
                    TokenKind.Operator, ">="
                    TokenKind.Identifier, "f"
                    TokenKind.Operator, "=>"
                    TokenKind.Identifier, "g"
                ]
                ""

        testCase "numbers and dotted function names"
        <| fun () ->
            Expect.equal
                (kinds "BETA.DIST(1.5, .25, 3e2) + DaxLib.Format.X()")
                [
                    TokenKind.Identifier, "BETA.DIST"
                    TokenKind.Punctuation, "("
                    TokenKind.Number, "1.5"
                    TokenKind.Punctuation, ","
                    TokenKind.Number, ".25"
                    TokenKind.Punctuation, ","
                    TokenKind.Number, "3e2"
                    TokenKind.Punctuation, ")"
                    TokenKind.Operator, "+"
                    TokenKind.Identifier, "DaxLib.Format.X"
                    TokenKind.Punctuation, "("
                    TokenKind.Punctuation, ")"
                ]
                ""

        testCase "date literals"
        <| fun () ->
            Expect.equal
                (kinds "DT\"2024-01-31\" > x")
                [
                    TokenKind.DateLiteral, "2024-01-31"
                    TokenKind.Operator, ">"
                    TokenKind.Identifier, "x"
                ]
                ""

        testCase "unterminated tokens are Invalid and lexing goes on"
        <| fun () ->
            Expect.equal (kinds "'Sales\n[X]") [ TokenKind.Invalid, "'Sales"; TokenKind.Bracketed, "X" ] ""
            Expect.equal (kinds "[Open") [ TokenKind.Invalid, "[Open" ] ""
            Expect.equal (kinds "\"never closed") [ TokenKind.Invalid, "\"never closed" ] ""

        testCase "positions point at the source span"
        <| fun () ->
            let dax = "  'A B'[C]"
            let toks = tokenize dax |> significant
            Expect.equal (toks |> List.map (fun t -> dax.Substring(t.Start, t.Length))) [ "'A B'"; "[C]" ] ""

        testCase "lineOf"
        <| fun () ->
            Expect.equal (lineOf "a\nb\nc" 0) 1 ""
            Expect.equal (lineOf "a\nb\nc" 2) 2 ""
            Expect.equal (lineOf "a\nb\nc" 4) 3 ""

        testCase "columnOf"
        <| fun () ->
            Expect.equal (columnOf "abc" 0) 1 ""
            Expect.equal (columnOf "abc" 2) 3 ""
            Expect.equal (columnOf "ab\ncd" 3) 1 "first character after a line break"
            Expect.equal (columnOf "ab\ncd" 4) 2 ""
            Expect.equal (columnOf "ab\n\ncd" 4) 1 ""

        testCase "quoteTable / bracket escape"
        <| fun () ->
            Expect.equal (quoteTable "O'Brien") "'O''Brien'" ""
            Expect.equal (bracket "A]B") "[A]]B]" ""

        testProperty "tokens cover the input exactly, in order (lossless)"
        <| fun (s: string) ->
            let s = if isNull s then "" else s
            let toks = tokenize s

            let rebuilt =
                toks |> List.map (fun t -> s.Substring(t.Start, t.Length)) |> String.concat ""

            let contiguous =
                toks
                |> List.pairwise
                |> List.forall (fun (a, b) -> a.Start + a.Length = b.Start)

            rebuilt = s && contiguous && toks |> List.forall (fun t -> t.Length > 0)
    ]
