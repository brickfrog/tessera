/// A small, total DAX lexer: it never throws, and every character of the input belongs to exactly
/// one token, so tokens can be used to rewrite expressions byte-for-byte.
module Tessera.Dax

open System
open System.Text

[<RequireQualifiedAccess>]
type TokenKind =
    /// Unquoted name: a table, variable, function or keyword (Sales, CALCULATE, VAR, DaxLib.Format).
    | Identifier
    /// 'Sales Table'. Text is the name, with '' unescaped.
    | QuotedTable
    /// [Total Sales]. Text is the name, with ]] unescaped.
    | Bracketed
    /// "text". Text is the value, with "" unescaped.
    | String
    /// DT"2024-01-01". Text is the value.
    | DateLiteral
    | Number
    /// + - * / ^ & = < > <= >= <> && || => :
    | Operator
    /// ( ) { } , ;
    | Punctuation
    | Comment
    | Whitespace
    /// An unterminated string, quoted name or bracket, or a character DAX doesn't use.
    | Invalid

type Token = {
    Kind: TokenKind
    /// The decoded text (see TokenKind); for other kinds, the raw source text.
    Text: string
    /// Offset of the token's first character in the source.
    Start: int
    /// Length of the token in the source.
    Length: int
}

module Token =
    /// Whitespace and comments: no meaning to the expression.
    let isTrivia (t: Token) =
        t.Kind = TokenKind.Whitespace || t.Kind = TokenKind.Comment

    let isName (t: Token) =
        t.Kind = TokenKind.Identifier || t.Kind = TokenKind.QuotedTable

let private isIdentStart (c: char) = Char.IsLetter c || c = '_'

let private isIdentPart (c: char) =
    Char.IsLetterOrDigit c || c = '_' || c = '.'

let private twoCharOperators = [ "&&"; "||"; "<>"; "<="; ">="; "=>" ]

let private oneCharOperators = "+-*/^&=<>:"
let private punctuation = "(){},;"

let tokenize (dax: string) : Token list =
    let s = if isNull dax then "" else dax
    let tokens = ResizeArray<Token>()
    let mutable i = 0

    let add kind text start =
        tokens.Add {
            Kind = kind
            Text = text
            Start = start
            Length = i - start
        }

    let at k =
        if i + k < s.Length then s.[i + k] else '\000'

    /// Read a delimited token whose closer is escaped by doubling. Names may not span lines.
    let delimited (opener: int) (close: char) (kind: TokenKind) (allowNewlines: bool) =
        let sb = StringBuilder()
        i <- opener + 1
        let mutable closed = false
        let mutable broken = false

        while not closed && not broken && i < s.Length do
            let c = s.[i]

            if c = close && at 1 = close then
                sb.Append(close) |> ignore
                i <- i + 2
            elif c = close then
                closed <- true
                i <- i + 1
            elif not allowNewlines && (c = '\n' || c = '\r') then
                broken <- true
            else
                sb.Append(c) |> ignore
                i <- i + 1

        if closed then
            add kind (sb.ToString()) opener
        else
            add TokenKind.Invalid (s.Substring(opener, i - opener)) opener

    while i < s.Length do
        let start = i
        let c = s.[i]

        if Char.IsWhiteSpace c then
            while i < s.Length && Char.IsWhiteSpace s.[i] do
                i <- i + 1

            add TokenKind.Whitespace (s.Substring(start, i - start)) start
        elif (c = '/' && at 1 = '/') || (c = '-' && at 1 = '-') then
            while i < s.Length && s.[i] <> '\n' && s.[i] <> '\r' do
                i <- i + 1

            add TokenKind.Comment (s.Substring(start, i - start)) start
        elif c = '/' && at 1 = '*' then
            match s.IndexOf("*/", i + 2, StringComparison.Ordinal) with
            | -1 -> i <- s.Length
            | close -> i <- close + 2

            add TokenKind.Comment (s.Substring(start, i - start)) start
        elif c = '"' then
            delimited start '"' TokenKind.String true
        elif c = '\'' then
            delimited start '\'' TokenKind.QuotedTable false
        elif c = '[' then
            delimited start ']' TokenKind.Bracketed false
        elif Char.IsDigit c || (c = '.' && Char.IsDigit(at 1)) then
            while i < s.Length && Char.IsDigit s.[i] do
                i <- i + 1

            if i < s.Length && s.[i] = '.' then
                i <- i + 1

                while i < s.Length && Char.IsDigit s.[i] do
                    i <- i + 1

            if i < s.Length && (s.[i] = 'e' || s.[i] = 'E') then
                let save = i
                i <- i + 1

                if i < s.Length && (s.[i] = '+' || s.[i] = '-') then
                    i <- i + 1

                if i < s.Length && Char.IsDigit s.[i] then
                    while i < s.Length && Char.IsDigit s.[i] do
                        i <- i + 1
                else
                    i <- save

            add TokenKind.Number (s.Substring(start, i - start)) start
        elif isIdentStart c then
            while i < s.Length && isIdentPart s.[i] do
                i <- i + 1

            let text = s.Substring(start, i - start)

            if (text = "DT" || text = "dt") && i < s.Length && s.[i] = '"' then
                // A date literal: re-read from the quote as a string, then widen the span to DT.
                delimited i '"' TokenKind.DateLiteral true
                let last = tokens.[tokens.Count - 1]

                tokens.[tokens.Count - 1] <- {
                    last with
                        Start = start
                        Length = i - start
                        Text =
                            if last.Kind = TokenKind.Invalid then
                                s.Substring(start, i - start)
                            else
                                last.Text
                }
            else
                add TokenKind.Identifier text start
        elif i + 1 < s.Length && List.contains (s.Substring(i, 2)) twoCharOperators then
            i <- i + 2
            add TokenKind.Operator (s.Substring(start, 2)) start
        elif oneCharOperators.IndexOf c >= 0 then
            i <- i + 1
            add TokenKind.Operator (string c) start
        elif punctuation.IndexOf c >= 0 then
            i <- i + 1
            add TokenKind.Punctuation (string c) start
        else
            i <- i + 1
            add TokenKind.Invalid (string c) start

    List.ofSeq tokens

/// Tokens with whitespace and comments removed.
let significant (tokens: Token list) =
    tokens |> List.filter (Token.isTrivia >> not)

/// 1-based line of an offset in a text.
let lineOf (text: string) (offset: int) =
    let mutable line = 1

    for k in 0 .. (min offset text.Length) - 1 do
        if text.[k] = '\n' then
            line <- line + 1

    line

/// 1-based column of an offset in a text (characters since the last line break).
let columnOf (text: string) (offset: int) =
    let offset = min offset text.Length

    match text.LastIndexOf('\n', max 0 (offset - 1)) with
    | _ when offset = 0 -> 1
    | -1 -> offset + 1
    | nl -> offset - nl

/// Quote a table name for DAX: 'Sales Table' (always quoted, '' escaped).
let quoteTable (name: string) = "'" + name.Replace("'", "''") + "'"

/// Bracket a column or measure name for DAX: [Total ]]x] (]] escaped).
let bracket (name: string) = "[" + name.Replace("]", "]]") + "]"
