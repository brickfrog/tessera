/// A DAX parser: text to `DaxAst`. It never throws and always ends, whatever the input: problems
/// come back as a list of errors, each with a position and a message that says what to fix.
///
/// Operator precedence, loosest first (operators on one line group left to right):
///
///     ||
///     &&
///     NOT                      (prefix)
///     =  ==  <>  <  <=  >  >=  IN
///     &
///     +  -
///     *  /
///     +  -                     (prefix: the sign)
///     ^
///
/// Sources: Microsoft's "DAX operators" reference, section "Operator precedence"
/// (https://learn.microsoft.com/en-us/dax/dax-operator-reference), which orders ^, sign, * /, + -,
/// &, comparisons with IN, NOT, says equal precedence groups left to right, and gives -2^2 = -4;
/// and SQLBI's DAX Guide (https://dax.guide/operators/), which adds && below NOT and || below &&,
/// and (https://dax.guide/op/not/) that NOT is an operator and never a function, so
/// `NOT (FALSE) * FALSE` is `NOT ((FALSE) * FALSE)`.
///
/// The two disagree about IN: Microsoft lists it with the comparison operators, the DAX Guide at
/// the top, above ^. This parser follows Microsoft. The readings differ only when IN's left side is
/// an unparenthesised operator expression (`a + b IN t`), which is rare; `inBindingPower` is the
/// one place to change.
///
/// A sign or NOT may start any operand, and then takes as much as its own precedence allows:
/// `2 ^ -1` is `2 ^ (-1)`, `1 + NOT a = b` is `1 + (NOT (a = b))`.
module Tessera.DaxParser

open System
open System.Globalization
open Tessera.Dax
open Tessera.DaxAst

type ParseOptions = {
    /// The text separates arguments with `;` and writes decimals with `,` (`IF(a; 1,5; 2)`), as DAX
    /// does under locales with a decimal comma. Otherwise `,` separates and `.` is the decimal mark.
    Semicolons: bool
}

module ParseOptions =
    let defaults = { Semicolons = false }

type ParseError = {
    /// Offset of the problem in the text (the text's length for a problem at its end).
    Offset: int
    /// Length of the offending text (0 at the end of the text).
    Length: int
    /// 1-based line.
    Line: int
    /// 1-based column: characters since the line break.
    Column: int
    Message: string
}

module ParseError =
    /// "3:14: expected ')' to close the '(' at 3:2"
    let format (e: ParseError) = $"{e.Line}:{e.Column}: {e.Message}"

/// The deepest tree the parser returns, in levels. Deeper input is an error rather than a tree,
/// so code that recurses over an AST cannot overflow the stack. A chain of n operators of one
/// precedence (`a + b + c ...`) is n levels deep, so this also caps such chains.
let maxDepth = 500

/// How deeply expressions may nest inside one another (parentheses, calls, braces, VAR blocks,
/// prefix operators and operators of rising precedence each count). Hand-written DAX stays far
/// below this; the cap is what keeps the parser's own recursion within the stack.
let private maxNesting = 256

/// No more errors than this are reported for one text.
let private maxErrors = 25

[<RequireQualifiedAccess>]
type private K =
    | Ident
    | Quoted
    | Bracket
    | Str
    | Date
    | Num
    | Op
    | Punct
    /// @name
    | Param
    /// Something unterminated, already reported: stands in for an operand.
    | Bad

type private T = {
    K: K
    /// Decoded text (the name, the string's value); for a number, the literal with `.` as the mark.
    Text: string
    Start: int
    Length: int
}

let private binaryOps = [
    "||", BinaryOp.Or
    "&&", BinaryOp.And
    "=", BinaryOp.Equal
    "==", BinaryOp.StrictEqual
    "<>", BinaryOp.NotEqual
    "<", BinaryOp.Less
    "<=", BinaryOp.LessOrEqual
    ">", BinaryOp.Greater
    ">=", BinaryOp.GreaterOrEqual
    "&", BinaryOp.Concat
    "+", BinaryOp.Add
    "-", BinaryOp.Subtract
    "*", BinaryOp.Multiply
    "/", BinaryOp.Divide
    "^", BinaryOp.Power
]

let private binaryByText = dict binaryOps

/// IN's precedence: with the comparison operators (Microsoft), not above ^ (DAX Guide).
let private inBindingPower = 4

/// How tightly a binary operator binds; higher binds tighter. All group left to right.
let bindingPower (op: BinaryOp) : int =
    match op with
    | BinaryOp.Or -> 1
    | BinaryOp.And -> 2
    | BinaryOp.In -> inBindingPower
    | BinaryOp.Equal
    | BinaryOp.StrictEqual
    | BinaryOp.NotEqual
    | BinaryOp.Less
    | BinaryOp.LessOrEqual
    | BinaryOp.Greater
    | BinaryOp.GreaterOrEqual -> 4
    | BinaryOp.Concat -> 5
    | BinaryOp.Add
    | BinaryOp.Subtract -> 6
    | BinaryOp.Multiply
    | BinaryOp.Divide -> 7
    | BinaryOp.Power -> 9

/// How much a prefix operator takes as its operand: operators binding at least this tightly.
let prefixBindingPower (op: UnaryOp) : int =
    match op with
    | UnaryOp.Not -> 3
    | UnaryOp.Plus
    | UnaryOp.Minus -> 8

/// The operator as DAX writes it.
let binaryText (op: BinaryOp) : string =
    match op with
    | BinaryOp.In -> "IN"
    | _ -> binaryOps |> List.pick (fun (text, o) -> if o = op then Some text else None)

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let private parameterTypes = [
    "ANYVAL", ParameterType.AnyVal
    "SCALAR", ParameterType.Scalar
    "TABLE", ParameterType.Table
    "ANYREF", ParameterType.AnyRef
    "CALENDARREF", ParameterType.CalendarRef
    "COLUMNREF", ParameterType.ColumnRef
    "MEASUREREF", ParameterType.MeasureRef
    "TABLEREF", ParameterType.TableRef
]

let private scalarTypes = [
    "VARIANT", ScalarType.Variant
    "INT64", ScalarType.Int64
    "DECIMAL", ScalarType.Decimal
    "DOUBLE", ScalarType.Double
    "STRING", ScalarType.String
    "DATETIME", ScalarType.DateTime
    "BOOLEAN", ScalarType.Boolean
    "NUMERIC", ScalarType.Numeric
]

let private parameterModes = [ "VAL", ParameterMode.Val; "EXPR", ParameterMode.Expr ]

let private lookup (table: (string * 'a) list) (word: string) =
    table |> List.tryPick (fun (w, v) -> if eqCi w word then Some v else None)

/// The keyword for a parameter type, subtype or mode, as DAX writes it.
let parameterTypeText (t: ParameterType) =
    parameterTypes |> List.pick (fun (w, v) -> if v = t then Some w else None)

let scalarTypeText (t: ScalarType) =
    scalarTypes |> List.pick (fun (w, v) -> if v = t then Some w else None)

let parameterModeText (m: ParameterMode) =
    parameterModes |> List.pick (fun (w, v) -> if v = m then Some w else None)

let private definitionKeywords = [ "MEASURE"; "COLUMN"; "TABLE"; "FUNCTION"; "VAR"; "MPARAMETER" ]

type private Parser(source: string, options: ParseOptions) =
    let src = if isNull source then "" else source
    let sep = if options.Semicolons then ";" else ","
    let otherSep = if options.Semicolons then "," else ";"
    let errors = ResizeArray<ParseError>()

    /// Token index at the last reported error: no second error until a token has been consumed.
    let mutable lastErrorAt = -1
    let mutable fatal = false
    /// The text ends inside something unterminated: what follows that error would only be noise.
    let mutable quiet = false
    /// Inside a query, a definition or clause keyword ends an expression instead of being a name.
    let mutable inQuery = false
    let mutable curlyReported = false
    /// Token indexes that follow a character that was reported and dropped.
    let afterDropped = System.Collections.Generic.HashSet<int>()

    let lineStarts =
        lazy
            ([|
                yield 0

                for i in 0 .. src.Length - 1 do
                    if src.[i] = '\n' then
                        yield i + 1
            |])

    /// 1-based line and column of an offset.
    let position (offset: int) =
        let starts = lineStarts.Value
        let found = Array.BinarySearch(starts, offset)
        let line = if found >= 0 then found else (~~~found) - 1
        line + 1, offset - starts.[line] + 1

    let mutable pos = 0
    let mutable nesting = 0
    /// The end offset of the last consumed token.
    let mutable lastEnd = 0

    let addError (offset: int) (length: int) (message: string) =
        if errors.Count < maxErrors then
            let offset = max 0 (min offset src.Length)
            let line, column = position offset

            errors.Add {
                Offset = offset
                Length = max 0 (min length (src.Length - offset))
                Line = line
                Column = column
                Message = message
            }

    let where (offset: int) =
        let line, column = position offset
        $"{line}:{column}"

    // ---- Tokens: trivia dropped, lexical problems reported, the dialect's numbers joined ----

    let toks: T array =
        let out = ResizeArray<T>()
        let raw = Array.ofList (tokenize src)

        let push k text start length =
            out.Add {
                K = k
                Text = text
                Start = start
                Length = length
            }

        let adjacentToLast (t: Token) =
            out.Count > 0
            && out.[out.Count - 1].Start + out.[out.Count - 1].Length = t.Start

        let mutable i = 0

        while i < raw.Length do
            let t = raw.[i]

            let next =
                if i + 1 < raw.Length && raw.[i + 1].Start = t.Start + t.Length then
                    Some raw.[i + 1]
                else
                    None

            match t.Kind with
            | TokenKind.Whitespace -> ()
            | TokenKind.Comment ->
                let text = src.Substring(t.Start, t.Length)

                if text.StartsWith "/*" && not (text.Length >= 4 && text.EndsWith "*/") then
                    addError t.Start 2 "unterminated /* comment: add the closing */"
            | TokenKind.Identifier -> push K.Ident t.Text t.Start t.Length
            | TokenKind.QuotedTable -> push K.Quoted t.Text t.Start t.Length
            | TokenKind.Bracketed -> push K.Bracket t.Text t.Start t.Length
            | TokenKind.String -> push K.Str t.Text t.Start t.Length
            | TokenKind.DateLiteral -> push K.Date t.Text t.Start t.Length
            | TokenKind.Operator -> push K.Op t.Text t.Start t.Length
            | TokenKind.Number ->
                let isDigits = t.Text.Length > 0 && Char.IsDigit t.Text.[0]

                // 1,5 with semicolons: a decimal comma joins the digits on both sides.
                let comma =
                    options.Semicolons
                    && isDigits
                    && out.Count > 0
                    && out.[out.Count - 1].K = K.Punct
                    && out.[out.Count - 1].Text = ","
                    && adjacentToLast t

                if comma then
                    let c = out.[out.Count - 1]
                    out.RemoveAt(out.Count - 1)

                    let whole =
                        if out.Count > 0 then
                            let n = out.[out.Count - 1]

                            if n.K = K.Num && n.Start + n.Length = c.Start && n.Text |> Seq.forall Char.IsDigit then
                                Some n
                            else
                                None
                        else
                            None

                    match whole with
                    | Some n ->
                        out.RemoveAt(out.Count - 1)
                        push K.Num (n.Text + "." + t.Text) n.Start (t.Start + t.Length - n.Start)
                    | None -> push K.Num ("." + t.Text) c.Start (t.Start + t.Length - c.Start)
                else
                    push K.Num t.Text t.Start t.Length
            | TokenKind.Punctuation -> push K.Punct t.Text t.Start t.Length
            | TokenKind.Invalid ->
                let unterminated (what: string) (closer: string) =
                    addError t.Start t.Length $"unterminated {what}: add the closing {closer}"
                    push K.Bad t.Text t.Start t.Length

                match t.Text.[0] with
                | '"' -> unterminated "string" "\""
                | '\'' -> unterminated "table name" "'"
                | '[' -> unterminated "[name]" "]"
                | _ when t.Text.Length > 1 -> unterminated "date literal" "\""
                | '@' when next |> Option.exists (fun n -> n.Kind = TokenKind.Identifier) ->
                    let n = next.Value
                    push K.Param n.Text t.Start (t.Length + n.Length)
                    i <- i + 1
                | '!' when next |> Option.exists (fun n -> n.Kind = TokenKind.Operator && n.Text = "=") ->
                    addError t.Start 2 "DAX has no '!=': write '<>' for \"not equal\""
                    push K.Op "<>" t.Start 2
                    i <- i + 1
                | '!' ->
                    addError t.Start 1 "DAX has no '!': write NOT"
                    afterDropped.Add out.Count |> ignore
                | '|' ->
                    addError t.Start 1 "DAX has no '|': write '||' for \"or\""
                    push K.Op "||" t.Start 1
                | '\u201C'
                | '\u201D'
                | '\u2018'
                | '\u2019' ->
                    // One message for the pair (or the many) of them.
                    if not curlyReported then
                        curlyReported <- true

                        let straight =
                            if t.Text.[0] = '\u201C' || t.Text.[0] = '\u201D' then
                                "DAX strings need straight double quotes (\")"
                            else
                                "DAX table names need straight single quotes (')"

                        addError t.Start 1 $"unexpected character '{t.Text}' (U+{int t.Text.[0]:X4}): {straight}"

                    afterDropped.Add out.Count |> ignore
                | c ->
                    addError t.Start 1 $"unexpected character '{c}' (U+{int c:X4})"
                    afterDropped.Add out.Count |> ignore

            i <- i + 1

        out.ToArray()

    let n = toks.Length

    // ---- Reading tokens ----

    let peekAt (k: int) : T voption =
        if pos + k < n then ValueSome toks.[pos + k] else ValueNone

    let isOp (o: string) (t: T) = t.K = K.Op && t.Text = o
    let isPunct (p: string) (t: T) = t.K = K.Punct && t.Text = p
    let isWord (w: string) (t: T) = t.K = K.Ident && eqCi t.Text w

    let atK (k: int) (pred: T -> bool) =
        match peekAt k with
        | ValueSome t -> pred t
        | ValueNone -> false

    let at (pred: T -> bool) = atK 0 pred
    let atEnd () = pos >= n

    let advance () =
        let t = toks.[pos]
        pos <- pos + 1
        lastEnd <- t.Start + t.Length
        t

    /// Where the next token starts (the end of the text if there is none).
    let here () =
        if pos < n then toks.[pos].Start else src.Length

    let spanOf (t: T) = Span.make t.Start t.Length

    let spanFrom (start: int) =
        Span.make start (max 0 (lastEnd - start))

    /// The next token as a message shows it.
    let found () =
        if pos >= n then
            "the end of the text"
        else
            let t = toks.[pos]
            let raw = src.Substring(t.Start, t.Length).Split('\n').[0].TrimEnd('\r')

            let raw =
                if raw.Length > 30 then
                    raw.Substring(0, 30) + "..."
                else
                    raw

            $"'{raw}'"

    /// Report a problem at the next token, unless one was already reported here.
    let fail (message: string) =
        if not fatal && not quiet && lastErrorAt <> pos then
            lastErrorAt <- pos

            if pos < n && toks.[pos].K = K.Bad then
                // Already reported as unterminated. If the text ends in it, nothing after counts.
                if pos = n - 1 then
                    quiet <- true
            elif afterDropped.Contains pos then
                // The character dropped just before this token was reported; this is its echo.
                ()
            elif pos < n then
                addError toks.[pos].Start toks.[pos].Length message
            else
                addError src.Length 0 message

    let placeholder () : Expr = {
        Kind = Omitted
        Span = Span.make (here ()) 0
    }

    let isStatementStart (k: int) =
        atK k (fun t -> isWord "DEFINE" t || isWord "EVALUATE" t)
        || (atK k (isWord "ORDER") && atK (k + 1) (isWord "BY"))
        || (atK k (isWord "START") && atK (k + 1) (isWord "AT"))

    /// MEASURE T[m] =, TABLE T =, VAR x =, ...: a definition keyword, a name, then [name] or =.
    let isDefinitionStart () =
        at (fun t -> definitionKeywords |> List.exists (fun w -> isWord w t))
        && atK 1 (fun t -> t.K = K.Ident || t.K = K.Quoted)
        && atK 2 (fun t -> t.K = K.Bracket || isOp "=" t)

    /// Something no argument list can contain: the list was never closed.
    let isHardStop () =
        atEnd ()
        || at (fun t ->
            isPunct ")" t
            || isPunct "}" t
            || isWord "VAR" t
            || isWord "RETURN" t
            || isWord "DEFINE" t
            || isWord "EVALUATE" t)

    /// Skip to the next separator or closer of the current list (stepping over nested groups).
    let skipToListBoundary () =
        let mutable depth = 0
        let mutable go = true

        while go && pos < n do
            let t = toks.[pos]

            if t.K = K.Punct then
                match t.Text with
                | "("
                | "{" ->
                    depth <- depth + 1
                    pos <- pos + 1
                | ")"
                | "}" when depth = 0 -> go <- false
                | ")"
                | "}" ->
                    depth <- depth - 1
                    pos <- pos + 1
                | s when depth = 0 && (s = sep || s = otherSep) -> go <- false
                | _ -> pos <- pos + 1
            else
                pos <- pos + 1

        if pos > 0 && pos <= n then
            lastEnd <- toks.[pos - 1].Start + toks.[pos - 1].Length

    let wrongSeparatorMessage () =
        if options.Semicolons then
            "',' is not a separator in the ';' dialect: separate arguments with ';' (',' is the decimal mark)"
        else
            "';' is not a separator here: separate arguments with ',' (text that uses ';' and decimal commas is the semicolon dialect)"

    let number (t: T) : ExprKind =
        let text = t.Text

        let asFloat () =
            match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, v -> NumberLiteral(v, text)
            | _ -> NumberLiteral(nan, text)

        if text |> Seq.forall Char.IsDigit then
            match Int64.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, v -> IntegerLiteral(v, text)
            | _ -> asFloat ()
        else
            asFloat ()

    // ---- Expressions ----

    /// An expression made of operators binding at least as tightly as `minBp`. `after` says what
    /// came before, for the message when there is no expression here ("after '+'").
    member p.Expression(minBp: int, after: string) : Expr =
        if fatal then
            placeholder ()
        elif nesting >= maxNesting then
            if not quiet then
                addError (here ()) 0 $"this is nested too deeply to read (more than {maxNesting} levels)"

            fatal <- true
            pos <- n
            placeholder ()
        elif not (System.Runtime.CompilerServices.RuntimeHelpers.TryEnsureSufficientExecutionStack()) then
            // On an unusually small stack, stop before the stack does.
            if not quiet then
                addError (here ()) 0 "this is nested too deeply to read on this thread's stack"

            fatal <- true
            pos <- n
            placeholder ()
        else
            nesting <- nesting + 1
            let start = here ()
            let mutable left = p.Operand after
            let mutable go = true

            while go && not fatal do
                let op =
                    match peekAt 0 with
                    | ValueSome t when t.K = K.Op ->
                        match binaryByText.TryGetValue t.Text with
                        | true, op -> ValueSome(op, bindingPower op, t.Text, 1)
                        | _ -> ValueNone
                    | ValueSome t when isWord "IN" t -> ValueSome(BinaryOp.In, inBindingPower, "IN", 1)
                    | ValueSome t when isWord "NOT" t && atK 1 (isWord "IN") ->
                        fail "DAX has no NOT IN: write NOT (value IN table)"
                        ValueSome(BinaryOp.In, inBindingPower, "NOT IN", 2)
                    | _ -> ValueNone

                match op with
                | ValueSome(op, bp, text, tokens) when bp >= minBp ->
                    for _ in 1..tokens do
                        advance () |> ignore

                    let right = p.Expression(bp + 1, $"after '{text}'")

                    left <- {
                        Kind = Binary(op, left, right)
                        Span = spanFrom start
                    }
                | _ -> go <- false

            nesting <- nesting - 1
            left

    /// A prefix operator and its operand, a VAR block, or a primary.
    // The methods that recurse (Expression, Operand, Prefix, Grouped, Items, VarBlock) are kept
    // small, and everything that only builds a message or a leaf is in a method of its own:
    // what a level of nesting costs in stack is the size of these few frames.

    /// A prefix operator and its operand, a VAR block, something in brackets, or a leaf.
    member private p.Operand(after: string) : Expr =
        match peekAt 0 with
        | ValueSome t when isOp "-" t -> p.Prefix(UnaryOp.Minus, 8, "after '-'")
        | ValueSome t when isOp "+" t -> p.Prefix(UnaryOp.Plus, 8, "after '+'")
        | ValueSome t when isWord "NOT" t -> p.Prefix(UnaryOp.Not, 3, "after 'NOT'")
        | ValueSome t when isWord "VAR" t -> p.VarBlock()
        | ValueSome t when t.K = K.Punct && (t.Text = "(" || t.Text = "{") -> p.Grouped()
        | ValueSome t when
            t.K = K.Ident
            && atK 1 (isPunct "(")
            && not (isReserved t.Text)
            && not (inQuery && (isStatementStart 0 || isDefinitionStart ()))
            ->
            p.Grouped()
        | _ -> p.Leaf after

    member private p.Prefix(op: UnaryOp, power: int, after: string) : Expr =
        let start = here ()
        advance () |> ignore
        let operand = p.Expression(power, after)

        {
            Kind = Unary(op, operand)
            Span = spanFrom start
        }

    /// `NAME(args)`, `(x)`, `(a, b)` or `{rows}`.
    member private p.Grouped() : Expr =
        let first = advance ()

        if first.K = K.Ident then
            let args = p.Items(advance (), ")", true)

            {
                Kind =
                    Call(
                        {
                            Name = first.Text
                            Span = spanOf first
                        },
                        args
                    )
                Span = spanFrom first.Start
            }
        elif first.Text = "(" then
            if at (isPunct ")") then
                p.EmptyGroup first
            else
                match p.Items(first, ")", false) with
                | [ one ] -> {
                    Kind = Paren one
                    Span = spanFrom first.Start
                  }
                | items -> {
                    Kind = Tuple items
                    Span = spanFrom first.Start
                  }
        elif at (isPunct "}") then
            p.EmptyGroup first
        else
            let rows = p.Items(first, "}", false)

            {
                Kind = TableConstructor rows
                Span = spanFrom first.Start
            }

    /// `()` or `{}`: the opener is consumed, the closer is next.
    member private p.EmptyGroup(opener: T) : Expr =
        if opener.Text = "(" then
            fail "empty parentheses: expected an expression between '(' and ')'"
        else
            fail "empty table constructor: expected at least one value between '{' and '}'"

        advance () |> ignore

        {
            Kind = Omitted
            Span = spanFrom opener.Start
        }

    member private p.VarBlock() : Expr =
        let start = here ()
        let first = toks.[pos]
        let vars = ResizeArray<VarDecl>()

        while at (isWord "VAR") && not fatal do
            let declStart = here ()
            advance () |> ignore

            let name =
                match peekAt 0 with
                | ValueSome t when t.K = K.Ident && not (isReserved t.Text) ->
                    advance () |> ignore
                    { Name = t.Text; Span = spanOf t }
                | _ ->
                    fail $"expected a variable name after VAR, found {found ()}"

                    {
                        Name = ""
                        Span = Span.make (here ()) 0
                    }

            if at (isOp "=") then
                advance () |> ignore
            else
                fail $"expected '=' after VAR {name.Name}, found {found ()}"

            let value = p.Expression(0, "after '='")

            vars.Add {
                Name = name
                Value = value
                Span = spanFrom declStart
            }

        let body =
            if at (isWord "RETURN") then
                advance () |> ignore
                p.Expression(0, "after RETURN")
            else
                fail $"expected RETURN to finish the VAR at {where first.Start}, found {found ()}"
                placeholder ()

        {
            Kind = VarBlock(List.ofSeq vars, body)
            Span = spanFrom start
        }

    /// A literal or a name; or, where there is neither, the problem.
    member private p.Leaf(after: string) : Expr =
        let leaf (t: T) (kind: ExprKind) : Expr =
            advance () |> ignore
            { Kind = kind; Span = spanOf t }

        let missing () =
            let context = if after = "" then "" else " " + after
            fail $"expected an expression{context}, found {found ()}"
            placeholder ()

        let qualified (t: T) (quoted: bool) : Expr =
            advance () |> ignore
            let b = advance ()

            {
                Kind =
                    Qualified(
                        {
                            Name = t.Text
                            Quoted = quoted
                            Span = spanOf t
                        },
                        b.Text
                    )
                Span = spanFrom t.Start
            }

        match peekAt 0 with
        | ValueNone -> missing ()
        | ValueSome t ->
            match t.K with
            | K.Num -> leaf t (number t)
            | K.Str -> leaf t (StringLiteral t.Text)
            | K.Date -> leaf t (DateLiteral t.Text)
            | K.Param -> leaf t (QueryParameter t.Text)
            | K.Bracket -> leaf t (Bracketed t.Text)
            | K.Bad ->
                if pos = n - 1 then
                    quiet <- true

                leaf t Omitted
            | K.Quoted when atK 1 (fun b -> b.K = K.Bracket) -> qualified t true
            | K.Quoted -> leaf t (QuotedTable t.Text)
            | K.Ident when isWord "RETURN" t ->
                fail "RETURN without a VAR before it"
                placeholder ()
            | K.Ident when isWord "IN" t ->
                fail "IN needs a value on its left: value IN { ... }"
                placeholder ()
            | K.Ident when isWord "DEFINE" t || isWord "EVALUATE" t ->
                let context = if after = "" then "" else " " + after

                fail
                    $"expected an expression{context}, found {found ()}: {t.Text.ToUpperInvariant()} starts a DAX query"

                placeholder ()
            | K.Ident when inQuery && (isStatementStart 0 || isDefinitionStart ()) -> missing ()
            | K.Ident when atK 1 (fun b -> b.K = K.Bracket) -> qualified t false
            | K.Ident when eqCi t.Text "TRUE" -> leaf t (BooleanLiteral true)
            | K.Ident when eqCi t.Text "FALSE" -> leaf t (BooleanLiteral false)
            | K.Ident -> leaf t (Identifier t.Text)
            | K.Punct when t.Text = otherSep ->
                fail (wrongSeparatorMessage ())
                placeholder ()
            | K.Op when t.Text = "=>" ->
                fail "'=>' belongs to a function definition, (parameters) => body, which can't be part of an expression"
                placeholder ()
            | _ -> missing ()

    /// The items of a list up to and including its closer; the opener is consumed already. In a
    /// call (`allowOmitted`), an item may be left out, or be several keywords side by side.
    member private p.Items(opener: T, closer: string, allowOmitted: bool) : Expr list =
        let items = ResizeArray<Expr>()
        let mutable fin = false

        if allowOmitted && at (isPunct closer) then
            advance () |> ignore
            fin <- true

        while not fin do
            // The item.
            if at (isPunct sep) || at (isPunct closer) then
                if not allowOmitted then
                    p.MissingItem()

                items.Add(placeholder ())
            elif allowOmitted && p.WordsAhead() > 0 then
                items.Add(p.WordsItem())
            else
                items.Add(
                    p.Expression(
                        0,
                        (if items.Count = 0 then
                             p.AfterOpener opener
                         else
                             p.AfterSeparator())
                    )
                )

            // What follows it.
            if fatal then
                fin <- true
            elif at (isPunct sep) then
                advance () |> ignore
            elif at (isPunct closer) then
                advance () |> ignore
                fin <- true
            else
                fin <- p.ItemTrouble(opener, closer)

        List.ofSeq items

    member private p.AfterOpener(opener: T) : string = $"after '{opener.Text}'"
    member private p.AfterSeparator() : string = $"after '{sep}'"

    member private p.MissingItem() =
        fail $"expected an expression, found {found ()}"

    /// How many keywords stand side by side as one whole argument here (0 unless two or more).
    member private p.WordsAhead() : int =
        let mutable k = 0

        while atK k (fun t -> t.K = K.Ident && isKeywordWord t.Text) do
            k <- k + 1

        if k >= 2 && (atK k (isPunct sep) || atK k (isPunct ")")) then
            k
        else
            0

    member private p.WordsItem() : Expr =
        let start = here ()
        let texts = [ for _ in 1 .. p.WordsAhead() -> (advance ()).Text ]

        {
            Kind = Words texts
            Span = spanFrom start
        }

    /// An item is followed by neither a separator nor the closer. Returns whether the list ends here.
    member private p.ItemTrouble(opener: T, closer: string) : bool =
        if at (isPunct otherSep) then
            fail (wrongSeparatorMessage ())
            advance () |> ignore
            false
        elif isHardStop () || isStatementStart 0 then
            fail $"expected '{closer}' to close the '{opener.Text}' at {where opener.Start}, found {found ()}"
            true
        else
            let hint =
                if pos > 0 && toks.[pos - 1].K = K.Ident && at (fun t -> t.K = K.Ident) then
                    $" (a name with a space in it needs quotes: '{toks.[pos - 1].Text} {toks.[pos].Text}')"
                else
                    ""

            fail
                $"expected '{sep}' or '{closer}' after the argument of the '{opener.Text}' at {where opener.Start}, found {found ()}{hint}"

            skipToListBoundary ()
            atEnd ()

    // ---- Whole texts ----

    /// Report whatever follows a finished expression at the top level.
    member private p.Trailing() =
        match peekAt 0 with
        | ValueNone -> ()
        | ValueSome t ->
            if isPunct ")" t then
                fail "unexpected ')': there is no '(' for it to close"
            elif isPunct "}" t then
                fail "unexpected '}': there is no '{' for it to close"
            elif isPunct sep t then
                fail $"unexpected '{sep}': it separates arguments, and this one is outside any parentheses"
            elif isPunct otherSep t then
                fail (wrongSeparatorMessage ())
            elif isWord "RETURN" t then
                fail "RETURN without a VAR before it"
            elif isOp "=>" t then
                fail "'=>' belongs to a function definition, (parameters) => body: this text was read as an expression"
            elif isWord "ORDER" t && not (atK 1 (isWord "BY")) && pos > 0 then
                advance () |> ignore
                fail $"expected BY after ORDER, found {found ()}"
            elif isWord "START" t && not (atK 1 (isWord "AT")) && pos > 0 then
                advance () |> ignore
                fail $"expected AT after START, found {found ()}"
            elif
                pos = 1
                && [ "MEASURE"; "COLUMN"; "TABLE"; "FUNCTION"; "MPARAMETER" ]
                   |> List.exists (fun w -> isWord w toks.[0])
            then
                fail
                    $"unexpected {found ()} after '{toks.[0].Text}': {toks.[0].Text.ToUpperInvariant()} defines something only under a query's DEFINE"
            elif isOp ":" t then
                fail "unexpected ':' (it only separates a function parameter from its type)"
            elif pos > 0 && toks.[pos - 1].K = K.Ident && t.K = K.Ident then
                fail
                    $"unexpected {found ()} after '{toks.[pos - 1].Text}': expected an operator or the end of the expression (a name with a space in it needs quotes: '{toks.[pos - 1].Text} {t.Text}')"
            elif pos > 0 then
                let before =
                    src.Substring(toks.[pos - 1].Start, min 30 toks.[pos - 1].Length).Split('\n').[0]

                fail $"unexpected {found ()} after '{before}': expected an operator or the end of the expression"
            else
                fail $"unexpected {found ()}"

    member p.WholeExpression() : Expr =
        // "=SUM(...)": the equal sign formulas start with in Excel and in Microsoft's reference.
        if at (isOp "=") then
            advance () |> ignore

        let e = p.Expression(0, "")

        if not fatal then
            p.Trailing()

        e

    member private p.Parameter() : Parameter =
        let start = here ()

        let name =
            match peekAt 0 with
            | ValueSome t when t.K = K.Ident && not (isReserved t.Text) ->
                advance () |> ignore
                { Name = t.Text; Span = spanOf t }
            | _ ->
                fail $"expected a parameter name, found {found ()}"

                {
                    Name = ""
                    Span = Span.make (here ()) 0
                }

        let mutable ty = None
        let mutable subtype = None
        let mutable mode = None

        if at (isOp ":") then
            advance () |> ignore

            if not (at (fun t -> t.K = K.Ident)) then
                fail
                    $"expected a type after '{name.Name}:' (for example INT64, SCALAR VAL, TABLE or ANYREF EXPR), found {found ()}"

            while at (fun t -> t.K = K.Ident) do
                let word = toks.[pos].Text

                match lookup parameterTypes word, lookup scalarTypes word, lookup parameterModes word with
                | Some t, _, _ ->
                    if ty.IsSome || subtype.IsSome || mode.IsSome then
                        fail
                            $"unexpected {found ()}: a parameter's type is written [type] [subtype] [VAL or EXPR], each at most once"

                    ty <- Some t
                | _, Some s, _ ->
                    if subtype.IsSome || mode.IsSome then
                        fail
                            $"unexpected {found ()}: a parameter's type is written [type] [subtype] [VAL or EXPR], each at most once"
                    elif ty.IsSome && ty <> Some ParameterType.Scalar then
                        fail $"{word.ToUpperInvariant()} is a scalar type: it can only follow SCALAR, or stand alone"

                    subtype <- Some s
                | _, _, Some m ->
                    if mode.IsSome then
                        fail
                            $"unexpected {found ()}: a parameter's type is written [type] [subtype] [VAL or EXPR], each at most once"

                    mode <- Some m
                | _ ->
                    fail
                        $"unknown parameter type {found ()}: expected ANYVAL, SCALAR, TABLE, ANYREF, CALENDARREF, COLUMNREF, MEASUREREF or TABLEREF; a scalar type (VARIANT, INT64, DECIMAL, DOUBLE, STRING, DATETIME, BOOLEAN, NUMERIC); VAL or EXPR"

                advance () |> ignore

        let fallback =
            if at (isOp "=") then
                advance () |> ignore
                Some(p.Expression(0, "after '='"))
            else
                None

        {
            Name = name
            Type = ty
            Subtype = subtype
            Mode = mode
            Default = fallback
            Span = spanFrom start
        }

    /// `(parameters) => body`
    member p.FunctionDef() : FunctionDef =
        let start = here ()
        let parameters = ResizeArray<Parameter>()

        if not (at (isPunct "(")) then
            fail $"expected a function definition, (parameters) => body, found {found ()}"
        else
            let opener = advance ()
            let mutable fin = false

            if at (isPunct ")") then
                advance () |> ignore
                fin <- true

            while not fin && not fatal do
                parameters.Add(p.Parameter())

                if at (isPunct sep) then
                    advance () |> ignore
                elif at (isPunct ")") then
                    advance () |> ignore
                    fin <- true
                elif at (isPunct otherSep) then
                    fail (wrongSeparatorMessage ())
                    advance () |> ignore
                elif isHardStop () || at (isOp "=>") then
                    fail $"expected ')' to close the '(' at {where opener.Start}, found {found ()}"
                    fin <- true
                else
                    fail $"expected '{sep}' or ')' after the parameter, found {found ()}"
                    skipToListBoundary ()

                    if at (isPunct ")") then
                        advance () |> ignore
                        fin <- true
                    elif atEnd () then
                        fin <- true

        if at (isOp "=>") then
            advance () |> ignore
        else
            fail $"expected '=>' after the parameter list, found {found ()}"

        let body = p.Expression(0, "after '=>'")

        {
            Parameters = List.ofSeq parameters
            Body = body
            Span = spanFrom start
        }

    member p.WholeFunction() : FunctionDef =
        let f = p.FunctionDef()

        if not fatal then
            p.Trailing()

        f

    /// Skip to the next thing that starts a statement or a definition.
    member private p.SkipToStatement() =
        let mutable depth = 0

        while pos < n && not (depth = 0 && (isStatementStart 0 || isDefinitionStart ())) do
            let t = toks.[pos]

            if isPunct "(" t || isPunct "{" t then
                depth <- depth + 1
            elif (isPunct ")" t || isPunct "}" t) && depth > 0 then
                depth <- depth - 1

            pos <- pos + 1

        if pos > 0 then
            lastEnd <- toks.[pos - 1].Start + toks.[pos - 1].Length

    /// After a statement's expression: anything but the next statement is a problem.
    member private p.EndOfStatement(inDefine: bool) =
        if
            not (atEnd ())
            && not (isStatementStart 0)
            && not (inDefine && isDefinitionStart ())
        then
            p.Trailing()
            p.SkipToStatement()

    member private p.Definition() : Definition =
        let start = here ()
        let keyword = (advance ()).Text.ToUpperInvariant()

        let equals (name: string) =
            if at (isOp "=") then
                advance () |> ignore
            else
                fail $"expected '=' after {keyword} {name}, found {found ()}"

        let tableName () : TableName =
            match peekAt 0 with
            | ValueSome t when t.K = K.Ident || t.K = K.Quoted ->
                advance () |> ignore

                {
                    Name = t.Text
                    Quoted = (t.K = K.Quoted)
                    Span = spanOf t
                }
            | _ ->
                fail $"expected a table name after {keyword}, found {found ()}"

                {
                    Name = ""
                    Quoted = false
                    Span = Span.make (here ()) 0
                }

        let ident (what: string) : Ident =
            match peekAt 0 with
            | ValueSome t when t.K = K.Ident ->
                advance () |> ignore
                { Name = t.Text; Span = spanOf t }
            | _ ->
                fail $"expected {what} after {keyword}, found {found ()}"

                {
                    Name = ""
                    Span = Span.make (here ()) 0
                }

        let member' (table: TableName) : Ident =
            match peekAt 0 with
            | ValueSome t when t.K = K.Bracket ->
                advance () |> ignore
                { Name = t.Text; Span = spanOf t }
            | _ ->
                fail
                    $"expected [Name] after {keyword} {table.Name} (as in {keyword} Sales[Name] = ...), found {found ()}"

                {
                    Name = ""
                    Span = Span.make (here ()) 0
                }

        let kind =
            match keyword with
            | "MEASURE"
            | "COLUMN" ->
                let table = tableName ()
                let name = member' table
                equals (table.Name + bracket name.Name)
                let value = p.Expression(0, "after '='")

                if keyword = "MEASURE" then
                    DefinitionKind.Measure(table, name, value)
                else
                    DefinitionKind.Column(table, name, value)
            | "TABLE" ->
                let table = tableName ()
                equals table.Name
                DefinitionKind.Table(table, p.Expression(0, "after '='"))
            | "FUNCTION" ->
                let name = ident "the function's name"
                equals name.Name
                DefinitionKind.Function(name, p.FunctionDef())
            | "MPARAMETER" ->
                let name = ident "the parameter's name"
                equals name.Name
                DefinitionKind.MParameter(name, p.Expression(0, "after '='"))
            | _ ->
                let name = ident "a variable name"
                equals name.Name
                DefinitionKind.Var(name, p.Expression(0, "after '='"))

        let d = { Kind = kind; Span = spanFrom start }
        p.EndOfStatement true
        d

    member private p.Evaluate() : Evaluate =
        let start = here ()
        advance () |> ignore
        let table = p.Expression(0, "after EVALUATE")
        let keys = ResizeArray<OrderKey>()
        let startAt = ResizeArray<Expr>()

        if at (isWord "ORDER") && atK 1 (isWord "BY") then
            advance () |> ignore
            advance () |> ignore
            let mutable more = true

            while more && not fatal do
                let keyStart = here ()
                let e = p.Expression(0, "after ORDER BY")

                let direction =
                    if at (isWord "ASC") then
                        advance () |> ignore
                        Some SortDirection.Asc
                    elif at (isWord "DESC") then
                        advance () |> ignore
                        Some SortDirection.Desc
                    else
                        None

                keys.Add {
                    Expr = e
                    Direction = direction
                    Span = spanFrom keyStart
                }

                if at (isPunct sep) then
                    advance () |> ignore
                else
                    more <- false

        if at (isWord "START") && atK 1 (isWord "AT") then
            if keys.Count = 0 then
                fail "START AT needs an ORDER BY before it"

            advance () |> ignore
            advance () |> ignore
            let mutable more = true

            while more && not fatal do
                startAt.Add(p.Expression(0, "after START AT"))

                if at (isPunct sep) then
                    advance () |> ignore
                else
                    more <- false

        let e = {
            Table = table
            OrderBy = List.ofSeq keys
            StartAt = List.ofSeq startAt
            Span = spanFrom start
        }

        p.EndOfStatement false
        e

    member p.WholeQuery() : Query =
        inQuery <- true
        let start = here ()
        let definitions = ResizeArray<Definition>()
        let evaluates = ResizeArray<Evaluate>()

        while not (atEnd ()) && not fatal do
            let before = pos

            if at (isWord "DEFINE") then
                if evaluates.Count > 0 then
                    fail "DEFINE must come before the first EVALUATE"
                elif definitions.Count > 0 then
                    fail "a query has one DEFINE: put every definition under the first"

                advance () |> ignore
                let count = definitions.Count

                while at (fun t -> definitionKeywords |> List.exists (fun w -> isWord w t))
                      && not fatal do
                    definitions.Add(p.Definition())

                if definitions.Count = count then
                    fail $"expected MEASURE, VAR, TABLE, COLUMN, FUNCTION or MPARAMETER after DEFINE, found {found ()}"
            elif at (isWord "EVALUATE") then
                evaluates.Add(p.Evaluate())
            elif
                at (fun t -> definitionKeywords |> List.exists (fun w -> isWord w t))
                && evaluates.Count = 0
                && definitions.Count = 0
            then
                fail $"expected DEFINE before {found ()}"
                p.SkipToStatement()
            else
                fail $"expected EVALUATE, found {found ()}"
                p.SkipToStatement()

            // Whatever happened, move on.
            if pos = before && not (atEnd ()) then
                pos <- pos + 1

        if evaluates.Count = 0 && not fatal then
            fail "a query needs an EVALUATE"

        {
            Definitions = List.ofSeq definitions
            Evaluates = List.ofSeq evaluates
            Span = spanFrom start
        }

    /// How the text starts: a query, a function definition or an expression.
    member _.Shape() =
        if at (fun t -> isWord "DEFINE" t || isWord "EVALUATE" t) then
            2
        elif at (isPunct "(") then
            let mutable depth = 0
            let mutable k = 0
            let mutable close = -1

            while close < 0 && k < n do
                let t = toks.[k]

                if isPunct "(" t || isPunct "{" t then
                    depth <- depth + 1
                elif isPunct ")" t || isPunct "}" t then
                    depth <- depth - 1

                    if depth = 0 then
                        close <- k

                k <- k + 1

            if close >= 0 && close + 1 < n && isOp "=>" toks.[close + 1] then
                1
            else
                0
        else
            0

    member _.Fail(offset: int, message: string) = addError offset 0 message

    member _.Errors: ParseError list =
        errors |> Seq.sortBy (fun e -> e.Offset) |> List.ofSeq

let private tooDeep (p: Parser) (exprs: Expr list) =
    for e in exprs do
        if p.Errors.IsEmpty && depth e > maxDepth then
            p.Fail(
                e.Span.Start,
                $"this expression is too deep to read: more than {maxDepth} levels (a chain of that many operators counts)"
            )

let private finish (p: Parser) (value: 'a) : Result<'a, ParseError list> =
    match p.Errors with
    | [] -> Ok value
    | errors -> Error errors

let private functionExprs (f: FunctionDef) =
    (f.Parameters |> List.choose (fun p -> p.Default)) @ [ f.Body ]

let private queryExprs (q: Query) =
    (q.Definitions
     |> List.collect (fun d ->
         match d.Kind with
         | DefinitionKind.Measure(_, _, v)
         | DefinitionKind.Column(_, _, v)
         | DefinitionKind.Table(_, v)
         | DefinitionKind.Var(_, v)
         | DefinitionKind.MParameter(_, v) -> [ v ]
         | DefinitionKind.Function(_, f) -> functionExprs f))
    @ (q.Evaluates
       |> List.collect (fun e -> e.Table :: (e.OrderBy |> List.map (fun k -> k.Expr)) @ e.StartAt))

/// Parse a DAX expression: a measure, a calculated column or table, a calculation item, a format
/// string expression, a row-level security filter, detail rows. One leading `=` is allowed and
/// ignored (`=SUM(Sales[Amount])`, as formulas are written in Excel); it is not part of the tree.
let parseExpressionWith (options: ParseOptions) (dax: string) : Result<Expr, ParseError list> =
    let p = Parser(dax, options)
    let e = p.WholeExpression()
    tooDeep p [ e ]
    finish p e

/// `parseExpressionWith` for DAX that separates arguments with `,`.
let parseExpression (dax: string) : Result<Expr, ParseError list> =
    parseExpressionWith ParseOptions.defaults dax

/// Parse a user-defined function's expression: `(a: INT64, b: SCALAR VAL) => body`.
let parseFunctionWith (options: ParseOptions) (dax: string) : Result<FunctionDef, ParseError list> =
    let p = Parser(dax, options)
    let f = p.WholeFunction()
    tooDeep p (functionExprs f)
    finish p f

/// `parseFunctionWith` for DAX that separates arguments with `,`.
let parseFunction (dax: string) : Result<FunctionDef, ParseError list> =
    parseFunctionWith ParseOptions.defaults dax

/// Parse a DAX query: `[DEFINE ...] EVALUATE ... [ORDER BY ...] [START AT ...]`, one EVALUATE or more.
let parseQueryWith (options: ParseOptions) (dax: string) : Result<Query, ParseError list> =
    let p = Parser(dax, options)
    let q = p.WholeQuery()
    tooDeep p (queryExprs q)
    finish p q

/// `parseQueryWith` for DAX that separates arguments with `,`.
let parseQuery (dax: string) : Result<Query, ParseError list> =
    parseQueryWith ParseOptions.defaults dax

/// Parse a query (the text starts with DEFINE or EVALUATE), a function definition (it starts with
/// a parenthesised list followed by `=>`) or else an expression.
let parseWith (options: ParseOptions) (dax: string) : Result<Document, ParseError list> =
    match Parser(dax, options).Shape() with
    | 2 -> parseQueryWith options dax |> Result.map Document.Query
    | 1 -> parseFunctionWith options dax |> Result.map Document.Function
    | _ -> parseExpressionWith options dax |> Result.map Document.Expression

/// `parseWith` for DAX that separates arguments with `,`.
let parse (dax: string) : Result<Document, ParseError list> = parseWith ParseOptions.defaults dax
