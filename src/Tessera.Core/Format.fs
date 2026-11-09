/// A local DAX formatter, in the style of SQLBI's "Rules for DAX code formatting"
/// (https://www.sqlbi.com/articles/rules-for-dax-code-formatting/). Nothing is sent anywhere.
///
/// The formatter only changes whitespace and the case of keywords and built-in function names.
/// Every result is checked before it is returned: it must lex to the same tokens (the case of
/// keywords and functions aside) with the same comments in the same order, or it is refused.
module Tessera.Format

open System
open System.Collections.Generic
open System.Text
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

type FormatOptions = {
    /// The line width layouts aim for. A call, a parenthesis or an operator chain that fits stays on one line.
    Width: int
    /// SQLBI's strict rules: every call with two or more arguments, or whose only argument is a
    /// function call, has its arguments on separate lines, even when it would fit.
    Long: bool
    /// Write a space between a function name and its "(" (SQLBI's style): `SUM ( x )`, not `SUM( x )`.
    SpaceAfterFunction: bool
    /// The input separates arguments with ";" and writes decimals with ",". The output keeps that dialect.
    Semicolons: bool
}

module FormatOptions =
    let defaults = {
        Width = 100
        Long = false
        SpaceAfterFunction = true
        Semicolons = false
    }

/// Something in the input that stops it being formatted, with its 1-based position.
type Problem = {
    Line: int
    Column: int
    Message: string
}

type FormatError =
    /// The input isn't DAX the formatter can read.
    | Syntax of Problem list
    /// The formatted text would not mean what the input meant, so it was refused. A formatter bug.
    | Unsafe of reason: string

module FormatError =
    let message =
        function
        | Syntax problems ->
            problems
            |> List.map (fun p -> $"line {p.Line}, column {p.Column}: {p.Message}")
            |> String.concat "; "
        | Unsafe reason -> $"refused: {reason}"

// ---- Vocabulary ---------------------------------------------------------------------------------

let private ci = StringComparer.OrdinalIgnoreCase

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let private wordSet (words: string) =
    HashSet<string>(words.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries), ci)

/// DAX's built-in functions. A call to one of these is written in upper case; any other call
/// (a user-defined function) keeps its case.
let private builtInFunctions =
    wordSet
        """
ABS ACCRINT ACCRINTM ACOS ACOSH ACOT ACOTH ADDCOLUMNS ADDMISSINGITEMS ALL ALLCROSSFILTERED ALLEXCEPT
ALLNOBLANKROW ALLSELECTED AMORDEGRC AMORLINC AND APPROXIMATEDISTINCTCOUNT ASIN ASINH ATAN ATANH AVERAGE
AVERAGEA AVERAGEX BETA.DIST BETA.INV BITAND BITLSHIFT BITOR BITRSHIFT BITXOR BLANK CALCULATE
CALCULATETABLE CALENDAR CALENDARAUTO CEILING CHISQ.DIST CHISQ.DIST.RT CHISQ.INV CHISQ.INV.RT
CLOSINGBALANCEMONTH CLOSINGBALANCEQUARTER CLOSINGBALANCEWEEK CLOSINGBALANCEYEAR COALESCE COLLAPSE
COLLAPSEALL COLUMNSTATISTICS COMBIN COMBINA COMBINEVALUES CONCATENATE CONCATENATEX CONFIDENCE.NORM
CONFIDENCE.T CONTAINS CONTAINSROW CONTAINSSTRING CONTAINSSTRINGEXACT CONVERT COS COSH COT COTH COUNT COUNTA
COUNTAX COUNTBLANK COUNTROWS COUNTX COUPDAYBS COUPDAYS COUPDAYSNC COUPNCD COUPNUM COUPPCD CROSSFILTER
CROSSJOIN CUMIPMT CUMPRINC CURRENCY CURRENTGROUP CUSTOMDATA DATATABLE DATE DATEADD DATEDIFF DATESBETWEEN
DATESINPERIOD DATESMTD DATESQTD DATESWTD DATESYTD DATEVALUE DAY DB DDB DEGREES DETAILROWS DISC DISTINCT
DISTINCTCOUNT DISTINCTCOUNTNOBLANK DIVIDE DOLLARDE DOLLARFR DURATION EARLIER EARLIEST EDATE EFFECT
ENDOFMONTH ENDOFQUARTER ENDOFWEEK ENDOFYEAR EOMONTH ERROR EVALUATEANDLOG EVEN EXACT EXCEPT EXP EXPAND
EXPANDALL EXPON.DIST EXTERNALMEASURE FACT FALSE FILTER FILTERS FIND FIRST FIRSTDATE FIRSTNONBLANK
FIRSTNONBLANKVALUE FIXED FLOOR FORMAT FV GCD GENERATE GENERATEALL GENERATESERIES GEOMEAN GEOMEANX GROUPBY
HASONEFILTER HASONEVALUE HOUR IF IF.EAGER IFERROR IGNORE INDEX INT INTERSECT INTRATE IPMT ISATLEVEL
ISBLANK ISCROSSFILTERED ISEMPTY ISERROR ISEVEN ISFILTERED ISINSCOPE ISLOGICAL ISNONTEXT ISNUMBER
ISO.CEILING ISODD ISONORAFTER ISPMT ISSELECTEDMEASURE ISSUBTOTAL ISTEXT KEEPFILTERS KEYWORDMATCH
KMEANSCLUSTERING LAST LASTDATE LASTNONBLANK LASTNONBLANKVALUE LCM LEFT LEN LINEST LINESTX LN LOG LOG10
LOOKUP LOOKUPVALUE LOOKUPWITHTOTALS LOWER MATCHBY MAX MAXA MAXX MDURATION MEDIAN MEDIANX MID MIN MINA
MINUTE MINX MOD MONTH MOVINGAVERAGE MROUND NAMEOF NATURALINNERJOIN NATURALLEFTOUTERJOIN NETWORKDAYS NEXT
NEXTDAY NEXTMONTH NEXTQUARTER NEXTWEEK NEXTYEAR NOMINAL NONVISUAL NORM.DIST NORM.INV NORM.S.DIST
NORM.S.INV NOT NOW NPER ODD ODDFPRICE ODDFYIELD ODDLPRICE ODDLYIELD OFFSET OPENINGBALANCEMONTH
OPENINGBALANCEQUARTER OPENINGBALANCEWEEK OPENINGBALANCEYEAR OR ORDERBY PARALLELPERIOD PARTITIONBY PATH
PATHCONTAINS PATHITEM PATHITEMREVERSE PATHLENGTH PDURATION PERCENTILE.EXC PERCENTILE.INC PERCENTILEX.EXC
PERCENTILEX.INC PERMUT PI PMT POISSON.DIST POWER PPMT PREVIOUS PREVIOUSDAY PREVIOUSMONTH PREVIOUSQUARTER
PREVIOUSWEEK PREVIOUSYEAR PRICE PRICEDISC PRICEMAT PRODUCT PRODUCTX PV QUARTER QUOTIENT RADIANS RAND
RANDBETWEEN RANGE RANK RANK.EQ RANKX RATE RECEIVED RELATED RELATEDTABLE REMOVEFILTERS REPLACE REPT RIGHT
ROLLUP ROLLUPADDISSUBTOTAL ROLLUPGROUP ROLLUPISSUBTOTAL ROUND ROUNDDOWN ROUNDUP ROW ROWNUMBER RRI
RUNNINGSUM SAMEPERIODLASTYEAR SAMPLE SAMPLEAXISWITHLOCALMINMAX SEARCH SECOND SELECTCOLUMNS
SELECTEDMEASURE SELECTEDMEASUREFORMATSTRING SELECTEDMEASURENAME SELECTEDVALUE SIGN SIN SINH SLN SQRT
SQRTPI STARTOFMONTH STARTOFQUARTER STARTOFWEEK STARTOFYEAR STDEV.P STDEV.S STDEVX.P STDEVX.S SUBSTITUTE
SUBSTITUTEWITHINDEX SUM SUMMARIZE SUMMARIZECOLUMNS SUMX SWITCH SYD T.DIST T.DIST.2T T.DIST.RT T.INV
T.INV.2T TAN TANH TBILLEQ TBILLPRICE TBILLYIELD TIME TIMEVALUE TOCSV TODAY TOJSON TOPN TOPNPERLEVEL
TOPNSKIP TOTALMTD TOTALQTD TOTALWTD TOTALYTD TREATAS TRIM TRUE TRUNC UNICHAR UNICODE UNION UPPER
USERCULTURE USERELATIONSHIP USERNAME USEROBJECTID USERPRINCIPALNAME UTCNOW UTCTODAY VALUE VALUES VAR.P
VAR.S VARX.P VARX.S VDB WEEKDAY WEEKNUM WINDOW XIRR XNPV YEAR YEARFRAC YIELD YIELDDISC YIELDMAT
"""

let private isBuiltInFunction (name: string) =
    builtInFunctions.Contains name
    || name.StartsWith("INFO.", StringComparison.OrdinalIgnoreCase)

/// Words that are always keywords where they appear bare: written in upper case.
let private reserved =
    wordSet "VAR RETURN DEFINE EVALUATE MEASURE ORDER BY ASC DESC IN NOT TRUE FALSE START AT MPARAMETER"

/// Words that are keywords in some places (function arguments, UDF types, DEFINE) but could be
/// names elsewhere: written in upper case when bare, unless they name a table or variable.
let private contextual =
    wordSet
        """
COLUMN TABLE FUNCTION BLANKS FIRST LAST ROWS COLUMNS SKIP DENSE REL ABS KEEP DEFAULT
BOTH NONE ONEWAY ONEWAY_RIGHTFILTERSLEFT ONEWAY_LEFTFILTERSRIGHT INTEGER DOUBLE STRING BOOLEAN DATETIME
VARIANT TEXT ALPHABETICAL CURRENCY DAY WEEK MONTH QUARTER YEAR HOUR MINUTE SECOND
SCALAR VAL EXPR ANYVAL ANYREF NUMERIC INT64 DECIMAL TABLEREF COLUMNREF MEASUREREF CALENDARREF
"""

/// Identifiers whose case the formatter may change: keywords and built-in functions.
let private caseFree (name: string) =
    reserved.Contains name || contextual.Contains name || isBuiltInFunction name

// ---- Lexing in a dialect ------------------------------------------------------------------------

/// Tokens as the dialect reads them, with Text = the raw source text. The lexer splits a few
/// lexemes DAX reads as one, which a formatter must never pull apart: `==`, and with semicolons,
/// a decimal comma (`1,5`). Formatting and checking both use this, so a split one is caught.
let private lex (semicolons: bool) (text: string) : Token list =
    let raw =
        tokenize text
        |> List.map (fun t -> {
            t with
                Text = text.Substring(t.Start, t.Length)
        })

    let out = ResizeArray<Token>()

    let adjacent (a: Token) (b: Token) = a.Start + a.Length = b.Start

    let join (a: Token) (b: Token) kind = {
        Kind = kind
        Text = a.Text + b.Text
        Start = a.Start
        Length = a.Length + b.Length
    }

    let isOp o (t: Token) =
        t.Kind = TokenKind.Operator && t.Text = o

    let isComma (t: Token) =
        t.Kind = TokenKind.Punctuation && t.Text = ","

    let isDigits (t: Token) =
        t.Kind = TokenKind.Number && t.Text.Length > 0 && Char.IsDigit t.Text.[0]

    for t in raw do
        let last = if out.Count > 0 then Some out.[out.Count - 1] else None

        match last with
        | Some l when isOp "=" l && isOp "=" t && adjacent l t -> out.[out.Count - 1] <- join l t TokenKind.Operator
        | Some l when isOp ":" l && isOp "=" t && adjacent l t -> out.[out.Count - 1] <- join l t TokenKind.Operator
        // 1,5: a decimal comma, joined with the digits on both sides.
        | Some l when semicolons && isComma l && isDigits t && adjacent l t ->
            let before = if out.Count > 1 then Some out.[out.Count - 2] else None

            match before with
            | Some n when n.Kind = TokenKind.Number && adjacent n l && not (n.Text.Contains ",") ->
                out.RemoveAt(out.Count - 1)
                out.[out.Count - 1] <- join (join n l TokenKind.Number) t TokenKind.Number
            | Some n when n.Kind = TokenKind.Number && adjacent n l -> out.Add t
            | _ -> out.[out.Count - 1] <- join l t TokenKind.Number
        | _ -> out.Add t

    List.ofSeq out

let private isLineComment (text: string) =
    text.StartsWith "//" || text.StartsWith "--"

/// Problems the lexer sees, and separators that don't belong to the dialect.
let private lexProblems (semicolons: bool) (text: string) (tokens: Token list) : Problem list =
    let at (t: Token) message = {
        Line = lineOf text t.Start
        Column = columnOf text t.Start
        Message = message
    }

    let opens = Stack<Token>()

    [
        for t in tokens do
            match t.Kind with
            | TokenKind.Invalid ->
                let message =
                    match t.Text.[0] with
                    | '"' -> "unterminated string"
                    | '\'' -> "unterminated quoted name"
                    | '[' -> "unterminated [name]"
                    | _ when t.Text.StartsWith "DT" || t.Text.StartsWith "dt" -> "unterminated date literal"
                    | c -> $"unexpected character '{c}' (U+{int c:X4})"

                at t message
            | TokenKind.Comment when t.Text.StartsWith "/*" && not (t.Text.Length >= 4 && t.Text.EndsWith "*/") ->
                at t "unterminated /* comment"
            | TokenKind.Punctuation ->
                match t.Text with
                | "("
                | "{" -> opens.Push t
                | ")"
                | "}" ->
                    let expected = if t.Text = ")" then "(" else "{"

                    if opens.Count = 0 then
                        at t $"'{t.Text}' without a matching '{expected}'"
                    elif opens.Peek().Text <> expected then
                        let o = opens.Pop()

                        at
                            t
                            $"'{t.Text}' doesn't match the '{o.Text}' at line {lineOf text o.Start}, column {columnOf text o.Start}"
                    else
                        opens.Pop() |> ignore
                | ";" when not semicolons ->
                    at
                        t
                        "';' is not a separator in this dialect (for DAX that separates arguments with ';', use --semicolons)"
                | "," when semicolons ->
                    at t "',' is not a separator with --semicolons (arguments are separated by ';')"
                | _ -> ()
            | _ -> ()

        for o in opens |> Seq.rev do
            at o $"'{o.Text}' is never closed"
    ]

// ---- Tokens with their comments -----------------------------------------------------------------

type private Comment = {
    Text: string
    IsLine: bool
    /// A blank line before it in the source.
    BlankBefore: bool
}

type private Tok = {
    Kind: TokenKind
    /// What to write: the source text, upper-cased for keywords and built-in function names.
    mutable Text: string
    Start: int
    /// Comments on lines of their own before the token.
    mutable Leading: Comment list
    /// Comments after the token on its line.
    mutable Trailing: Comment list
    /// A blank line directly before the token (after its leading comments).
    BlankBefore: bool
}

/// Names whose case must not change even if they look like keywords.
type private Names = {
    /// Tables, calendars and variables: bare identifiers.
    Bare: HashSet<string>
    /// User-defined functions: calls.
    Functions: HashSet<string>
}

let private newlines (ws: string) =
    let mutable n = 0

    for i in 0 .. ws.Length - 1 do
        if ws.[i] = '\n' || (ws.[i] = '\r' && (i + 1 = ws.Length || ws.[i + 1] <> '\n')) then
            n <- n + 1

    n

/// Attach every comment to a token: comments on a token's line follow it (Trailing), comments on
/// lines of their own precede the next one (Leading). Comments after the last token are returned
/// separately. Keywords and built-in functions are upper-cased here.
let private attach (names: Names) (tokens: Token list) : Tok array * Comment list * HashSet<string> =
    let code = tokens |> List.filter (Token.isTrivia >> not) |> Array.ofList
    let declared = HashSet<string>(names.Bare, ci)

    let isId (t: Token) = t.Kind = TokenKind.Identifier
    let kw (w: string) (t: Token) = isId t && eqCi t.Text w

    // Variables, UDF parameters and DEFINE'd names keep their case wherever they're used.
    for i in 0 .. code.Length - 2 do
        let t, n = code.[i], code.[i + 1]

        if kw "VAR" t && isId n then
            declared.Add n.Text |> ignore
        elif isId t && n.Kind = TokenKind.Operator && n.Text = ":" then
            declared.Add t.Text |> ignore
        elif
            (kw "TABLE" t || kw "FUNCTION" t || kw "MPARAMETER" t)
            && isId n
            && i + 2 < code.Length
            && code.[i + 2].Kind = TokenKind.Operator
            && code.[i + 2].Text = "="
        then
            declared.Add n.Text |> ignore

    let textOf (i: int) =
        let t = code.[i]
        let next = if i + 1 < code.Length then Some code.[i + 1] else None

        if not (isId t) then
            t.Text
        else
            let upper = t.Text.ToUpperInvariant()

            match next with
            | Some n when n.Kind = TokenKind.Punctuation && n.Text = "(" ->
                if names.Functions.Contains t.Text || not (isBuiltInFunction t.Text) then
                    t.Text
                else
                    upper
            | _ when declared.Contains t.Text -> t.Text
            | _ when reserved.Contains t.Text -> upper
            // Contextual keywords are upper-cased after parsing, where they're in a keyword's place.
            | _ -> t.Text

    let toks = ResizeArray<Tok>()
    let mutable pending: Comment list = []
    let mutable lines = 0
    let mutable lineBroken = true
    let mutable k = 0

    for t in tokens do
        match t.Kind with
        | TokenKind.Whitespace ->
            let n = newlines t.Text
            lines <- lines + n

            if n > 0 then
                lineBroken <- true
        | TokenKind.Comment ->
            let line = isLineComment t.Text

            let c = {
                Text = if line then t.Text.TrimEnd() else t.Text
                IsLine = line
                BlankBefore = lines >= 2
            }

            if not lineBroken && toks.Count > 0 then
                let last = toks.[toks.Count - 1]
                last.Trailing <- last.Trailing @ [ c ]
            else
                pending <- pending @ [ c ]

            lines <- 0
        | _ ->
            toks.Add {
                Kind = t.Kind
                Text = textOf k
                Start = t.Start
                Leading = pending
                Trailing = []
                BlankBefore = lines >= 2
            }

            k <- k + 1
            pending <- []
            lines <- 0
            lineBroken <- false

    toks.ToArray(), pending, declared

// ---- Parsing ------------------------------------------------------------------------------------
// A tolerant structural parse: enough to lay the expression out. It never decides what the DAX
// means; whatever the layout, the output has the input's tokens in the input's order.

type private Node =
    /// One token, or a table and its [column].
    | Atom of Tok list
    | Call of name: Tok * Group
    /// ( ... ) or { ... }
    | Paren of Group
    | Unary of op: Tok * operand: Node
    /// Operands joined by operators of one precedence level.
    | Chain of first: Node * rest: (Tok list * Node) list
    /// Expressions side by side: [Sales] DESC, ROWS COLUMNS, ASC BLANKS FIRST.
    | Juxt of Node list
    | VarBlock of vars: Def list * ret: Tok * body: Node

and private Group = {
    Open: Tok
    Items: Item list
    Close: Tok
}

and private Item = { Arg: Node option; Sep: Tok option }

/// VAR x = ..., MEASURE 'T'[M] = ..., FUNCTION F = ...
and private Def = {
    Keyword: Tok
    Name: Tok list
    Eq: Tok
    Value: Node
}

type private Statement =
    | Define of Tok * Def list
    | Evaluate of Tok * Node
    /// ORDER BY ..., START AT ...
    | Clause of Tok list * Item list

type private Tree =
    | Expression of Node option
    | Query of Statement list

type private ParseFail(offset: int, message: string) =
    inherit Exception(message)
    /// Where (-1: the end of the input).
    member _.Offset = offset

let rec private firstTok (n: Node) =
    match n with
    | Atom ts -> List.head ts
    | Call(t, _) -> t
    | Paren g -> g.Open
    | Unary(t, _) -> t
    | Chain(f, _) -> firstTok f
    | Juxt ns -> firstTok (List.head ns)
    | VarBlock(ds, _, _) -> (List.head ds).Keyword

let rec private lastTok (n: Node) =
    match n with
    | Atom ts -> List.last ts
    | Call(_, g)
    | Paren g -> g.Close
    | Unary(_, x) -> lastTok x
    | Chain(f, []) -> lastTok f
    | Chain(_, rest) -> lastTok (snd (List.last rest))
    | Juxt ns -> lastTok (List.last ns)
    | VarBlock(_, _, body) -> lastTok body

/// Comments next to a separator are moved to where the layout keeps them readable: a comment
/// after an argument goes after its comma, a comment on its own line before a comma goes before
/// the next argument. Only comments move, never past anything but the separator.
let private tidySeparators (items: Item list) (close: Tok option) =
    let items = Array.ofList items

    for i in 0 .. items.Length - 1 do
        match items.[i].Sep with
        | Some s ->
            if not s.Leading.IsEmpty then
                let target =
                    if i + 1 < items.Length then
                        items.[i + 1].Arg |> Option.map firstTok
                    else
                        None

                // The comma's own comments go along, so the comments keep their order.
                match target |> Option.orElse close with
                | Some t ->
                    t.Leading <- s.Leading @ s.Trailing @ t.Leading
                    s.Leading <- []
                    s.Trailing <- []
                | None -> ()

            match items.[i].Arg with
            | Some a ->
                let last = lastTok a

                if not last.Trailing.IsEmpty && s.Trailing.IsEmpty && s.Leading.IsEmpty then
                    s.Trailing <- last.Trailing
                    last.Trailing <- []
            | None -> ()
        | None -> ()

let private level (op: Tok list) =
    match op with
    | [ t ] when t.Kind = TokenKind.Operator ->
        match t.Text with
        | "=>" -> 0
        | ":"
        | ":=" -> 1
        | "||" -> 2
        | "&&" -> 3
        | "&" -> 6
        | "+"
        | "-" -> 7
        | "*"
        | "/" -> 8
        | "^" -> 9
        | _ -> 5
    | _ -> 5

/// Operands and the operators between them, as chains split at the loosest operators first.
let rec private build (operands: Node list) (ops: Tok list list) : Node =
    match ops with
    | [] -> List.head operands
    | _ ->
        let os = Array.ofList operands
        let ps = Array.ofList ops
        let low = ps |> Array.map level |> Array.min
        let segments = ResizeArray<int * int>()
        let splits = ResizeArray<Tok list>()
        let mutable start = 0

        for i in 0 .. ps.Length - 1 do
            if level ps.[i] = low then
                segments.Add(start, i)
                splits.Add ps.[i]
                start <- i + 1

        segments.Add(start, os.Length - 1)

        let sub (a, b) =
            build (List.ofArray os.[a..b]) (List.ofArray ps.[a .. b - 1])

        Chain(sub segments.[0], [ for j in 1 .. segments.Count - 1 -> splits.[j - 1], sub segments.[j] ])

let private definitionKeywords = [ "VAR"; "MEASURE"; "COLUMN"; "TABLE"; "FUNCTION"; "MPARAMETER" ]

let private parse (toks: Tok array) (sep: string) : Tree =
    let mutable pos = 0

    let peekAt k =
        if pos + k < toks.Length then Some toks.[pos + k] else None

    let peek () = peekAt 0

    let next () =
        let t = toks.[pos]
        pos <- pos + 1
        t

    let fail (t: Tok option) (message: string) =
        raise (
            ParseFail(
                (match t with
                 | Some t -> t.Start
                 | None -> -1),
                message
            )
        )

    let isKw (w: string) (t: Tok) =
        t.Kind = TokenKind.Identifier && eqCi t.Text w

    let isPunct (p: string) (t: Tok) =
        t.Kind = TokenKind.Punctuation && t.Text = p

    let isOp (o: string) (t: Tok) =
        t.Kind = TokenKind.Operator && t.Text = o

    let atK k pred =
        match peekAt k with
        | Some t -> pred t
        | None -> false

    let at pred = atK 0 pred
    let atEnd () = pos >= toks.Length

    let isCloser (t: Tok) = isPunct ")" t || isPunct "}" t

    let statementAt k =
        atK k (fun t -> List.exists (fun w -> isKw w t) [ "DEFINE"; "EVALUATE" ])
        || atK k (fun t -> List.exists (fun w -> isKw w t) definitionKeywords)
        || (atK k (isKw "ORDER") && atK (k + 1) (isKw "BY"))
        || (atK k (isKw "START") && atK (k + 1) (isKw "AT"))

    /// The current expression can't continue here. `top`: at a query's top level.
    let ends (top: bool) =
        atEnd ()
        || at (isPunct sep)
        || at isCloser
        || at (isKw "VAR")
        || at (isKw "RETURN")
        || (top && statementAt 0)

    let atBinop () =
        match peek () with
        | Some t when t.Kind = TokenKind.Operator -> true
        | Some t when isKw "IN" t -> true
        | Some t when isKw "NOT" t -> atK 1 (isKw "IN")
        | _ -> false

    let describe (ts: Tok list) =
        ts |> List.map (fun t -> t.Text) |> String.concat " "

    let rec expr (top: bool) : Node option =
        match operand top with
        | None -> None
        | Some first ->
            let operands = ResizeArray [ first ]
            let ops = ResizeArray<Tok list>()

            while atBinop () do
                let op =
                    if at (isKw "NOT") then
                        [ next (); next () ]
                    else
                        [ next () ]

                match operand top with
                | Some n ->
                    ops.Add op
                    operands.Add n
                | None -> fail (Some(List.last op)) $"expected an expression after '{describe op}'"

            Some(build (List.ofSeq operands) (List.ofSeq ops))

    and operand (top: bool) : Node option =
        match peek () with
        | None -> None
        | Some t when isKw "VAR" t -> Some(varBlock top)
        | Some _ when ends top -> None
        | Some t when isOp "-" t || isOp "+" t || (isKw "NOT" t && not (atK 1 (isPunct "("))) ->
            next () |> ignore

            match operand top with
            | Some n -> Some(Unary(t, n))
            | None -> fail (Some t) $"expected an expression after '{t.Text}'"
        | Some t when t.Kind = TokenKind.Operator -> fail (Some t) $"unexpected '{t.Text}'"
        | Some _ ->
            let parts = ResizeArray [ primary () ]

            while not (ends top) && not (atBinop ()) do
                parts.Add(primary ())

            Some(
                if parts.Count = 1 then
                    parts.[0]
                else
                    Juxt(List.ofSeq parts)
            )

    and primary () : Node =
        let t = next ()

        match t.Kind with
        | TokenKind.Identifier when at (isPunct "(") -> Call(t, group (next ()) ")")
        | TokenKind.Identifier
        | TokenKind.QuotedTable when
            at (fun n -> n.Kind = TokenKind.Bracketed)
            && not (t.Kind = TokenKind.Identifier && reserved.Contains t.Text)
            ->
            Atom [ t; next () ]
        | TokenKind.Punctuation when t.Text = "(" -> Paren(group t ")")
        | TokenKind.Punctuation when t.Text = "{" -> Paren(group t "}")
        | TokenKind.Punctuation
        | TokenKind.Operator -> fail (Some t) $"unexpected '{t.Text}'"
        | _ -> Atom [ t ]

    and group (openTok: Tok) (close: string) : Group =
        let items = ResizeArray<Item>()
        let mutable closeTok = None

        if at (isPunct close) then
            closeTok <- Some(next ())

        while closeTok.IsNone do
            let arg = expr false

            if at (isPunct sep) then
                items.Add { Arg = arg; Sep = Some(next ()) }

                if at (isPunct close) then
                    items.Add { Arg = None; Sep = None }
                    closeTok <- Some(next ())
            elif at (isPunct close) then
                items.Add { Arg = arg; Sep = None }
                closeTok <- Some(next ())
            else
                match peek () with
                | None -> fail None $"expected '{close}'"
                | Some t -> fail (Some t) $"unexpected '{t.Text}'"

        let items = List.ofSeq items
        tidySeparators items closeTok

        {
            Open = openTok
            Items = items
            Close = closeTok.Value
        }

    and definition (top: bool) : Def =
        let kw = next ()
        let isVar = isKw "VAR" kw
        let name = ResizeArray<Tok>()

        let nameToken (t: Tok) =
            if isVar then
                t.Kind = TokenKind.Identifier && name.Count = 0
            else
                (t.Kind = TokenKind.Identifier
                 || t.Kind = TokenKind.QuotedTable
                 || t.Kind = TokenKind.Bracketed)
                && name.Count < 2

        while at nameToken do
            name.Add(next ())

        if name.Count = 0 then
            fail (peek ()) $"expected a name after {kw.Text}"

        if not (at (isOp "=")) then
            fail (peek ()) $"expected '=' after {describe (List.ofSeq name)}"

        let eq = next ()

        match expr top with
        | Some v -> {
            Keyword = kw
            Name = List.ofSeq name
            Eq = eq
            Value = v
          }
        | None -> fail (Some eq) "expected an expression after '='"

    and varBlock (top: bool) : Node =
        let defs = ResizeArray<Def>()

        while at (isKw "VAR") do
            defs.Add(definition top)

        match peek () with
        | Some t when isKw "RETURN" t ->
            next () |> ignore

            match expr top with
            | Some body -> VarBlock(List.ofSeq defs, t, body)
            | None -> fail (Some t) "expected an expression after RETURN"
        | _ -> fail (Some defs.[0].Keyword) "VAR without RETURN"

    let items (top: bool) =
        let out = ResizeArray<Item>()
        let mutable go = true

        while go do
            match expr top with
            | None -> fail (peek ()) "expected an expression"
            | Some e ->
                if at (isPunct sep) then
                    out.Add { Arg = Some e; Sep = Some(next ()) }
                else
                    out.Add { Arg = Some e; Sep = None }
                    go <- false

        let out = List.ofSeq out
        tidySeparators out None
        out

    let query () =
        let statements = ResizeArray<Statement>()

        while not (atEnd ()) do
            let t = peek () |> Option.get

            if isKw "DEFINE" t then
                next () |> ignore
                let defs = ResizeArray<Def>()

                while at (fun x -> List.exists (fun w -> isKw w x) definitionKeywords) do
                    defs.Add(definition true)

                if defs.Count = 0 then
                    fail (peek ()) "expected MEASURE, VAR, TABLE, COLUMN, FUNCTION or MPARAMETER after DEFINE"

                statements.Add(Define(t, List.ofSeq defs))
            elif isKw "EVALUATE" t then
                next () |> ignore

                match expr true with
                | Some e -> statements.Add(Evaluate(t, e))
                | None -> fail (peek ()) "expected an expression after EVALUATE"
            elif statementAt 0 && (isKw "ORDER" t || isKw "START" t) then
                let k1 = next ()
                let k2 = next ()
                statements.Add(Clause([ k1; k2 ], items true))
            else
                fail (Some t) $"unexpected '{t.Text}'"

        List.ofSeq statements

    if toks.Length = 0 then
        Expression None
    elif isKw "DEFINE" toks.[0] || isKw "EVALUATE" toks.[0] then
        Query(query ())
    else
        let e = expr false

        match peek () with
        | Some t -> fail (Some t) $"unexpected '{t.Text}'"
        | None -> Expression e

/// Upper-case contextual keywords where they stand in a keyword's place: a whole argument of a
/// built-in function (`DATEADD ( ..., -1, YEAR )`, `RANKX ( ..., DESC, DENSE )`), words side by
/// side (`ROWS COLUMNS`, `ASC BLANKS FIRST`), UDF parameter types (`t: TABLE EXPR`) and DEFINE's
/// keywords. Elsewhere they may be names (a table called Year), so they keep their case.
let private keywordCase (declared: HashSet<string>) (tree: Tree) =
    let definitionWords = wordSet "COLUMN TABLE FUNCTION MEASURE"

    let upper (allowDefinitionWords: bool) (n: Node) =
        match n with
        | Atom [ t ] when
            t.Kind = TokenKind.Identifier
            && contextual.Contains t.Text
            && not (declared.Contains t.Text)
            && (allowDefinitionWords || not (definitionWords.Contains t.Text))
            ->
            t.Text <- t.Text.ToUpperInvariant()
        | _ -> ()

    let rec walk (n: Node) =
        match n with
        | Atom _ -> ()
        | Call(name, g) ->
            if isBuiltInFunction name.Text then
                for it in g.Items do
                    it.Arg |> Option.iter (upper false)

            group g
        | Paren g -> group g
        | Unary(_, x) -> walk x
        | Chain(first, rest) ->
            walk first

            for op, x in rest do
                if op.Head.Kind = TokenKind.Operator && op.Head.Text = ":" then
                    match x with
                    | Juxt parts -> parts |> List.iter (upper true)
                    | _ -> upper true x

                walk x
        | Juxt parts ->
            for p in parts do
                upper false p
                walk p
        | VarBlock(defs, _, body) ->
            for d in defs do
                walk d.Value

            walk body

    and group (g: Group) =
        for it in g.Items do
            it.Arg |> Option.iter walk

    match tree with
    | Expression n -> n |> Option.iter walk
    | Query statements ->
        for s in statements do
            match s with
            | Define(kw, defs) ->
                kw.Text <- kw.Text.ToUpperInvariant()

                for d in defs do
                    d.Keyword.Text <- d.Keyword.Text.ToUpperInvariant()
                    walk d.Value
            | Evaluate(_, e) -> walk e
            | Clause(_, items) ->
                for it in items do
                    it.Arg |> Option.iter walk

// ---- Layout -------------------------------------------------------------------------------------

/// Text being written, line by line. A line comment leaves a pending line break: whatever comes
/// next starts a new line.
type private Out(nl: string) =
    let sb = StringBuilder()
    let mutable col = 0
    let mutable content = false
    let mutable pending = false
    let mutable broken = false
    let mutable lineStart = 0

    let trimSpaces () =
        while sb.Length > 0 && sb.[sb.Length - 1] = ' ' do
            sb.Length <- sb.Length - 1

    member _.Col = col
    /// The current line has something on it besides indentation.
    member _.Content = content
    member _.Pending = pending
    /// A line break was written (for measuring flat layouts).
    member _.Broken = broken

    member _.Newline(indent: int) =
        trimSpaces ()

        if content then
            sb.Append(nl) |> ignore
            broken <- true
            lineStart <- sb.Length

        sb.Append(' ', indent) |> ignore
        col <- indent
        content <- false
        pending <- false

    /// An empty line before the current one, which must be empty so far.
    member _.Blank(indent: int) =
        trimSpaces ()

        let endsWithBlank =
            sb.Length >= 2 * nl.Length
            && sb.ToString(sb.Length - 2 * nl.Length, 2 * nl.Length) = nl + nl

        if sb.Length > 0 && not endsWithBlank then
            sb.Append(nl) |> ignore
            broken <- true
            lineStart <- sb.Length

        sb.Append(' ', indent) |> ignore
        col <- indent

    member this.Write(indent: int, text: string) =
        if pending then
            this.Newline indent

        sb.Append(text) |> ignore

        match text.LastIndexOf '\n' with
        | -1 -> col <- col + text.Length
        | i ->
            broken <- true
            col <- text.Length - i - 1
            lineStart <- sb.Length - col

        content <- true

    member _.Space() =
        if content && not pending && sb.Length > 0 && sb.[sb.Length - 1] <> ' ' then
            sb.Append(' ') |> ignore
            col <- col + 1

    member _.Break() =
        pending <- true
        broken <- true

    member _.SetPending() = pending <- true

    /// The indentation of the current line.
    member this.LineIndent =
        let line: string = this.LineText
        line.Length - line.TrimStart(' ').Length

    /// The current line so far.
    member _.LineText: string =
        let start = min lineStart sb.Length
        sb.ToString(start, sb.Length - start)

    member _.Text =
        trimSpaces ()
        sb.ToString()

let private layout (opts: FormatOptions) (nl: string) (tree: Tree) (tail: Comment list) : string =
    let width = opts.Width
    let flatWidths = Dictionary<obj, int option>(HashIdentity.Reference)
    // Tokens whose leading comments are written (or, while measuring, left out): a node's own
    // leading comments go on the lines before it, so they don't stop it fitting on one line.
    let noLeading = HashSet<Tok>(HashIdentity.Reference)

    let isOpText (s: string) (t: Tok) =
        t.Kind = TokenKind.Operator && t.Text = s

    let leading (o: Out) (indent: int) (cs: Comment list) =
        for c in cs do
            if o.Content then
                o.Newline indent

            if c.BlankBefore then
                o.Blank indent

            o.Write(indent, c.Text)
            o.SetPending()

    let trailing (o: Out) (indent: int) (cs: Comment list) =
        for c in cs do
            o.Space()
            o.Write(indent, c.Text)

            if c.IsLine then o.SetPending() else o.Space()

    /// Write a token with its comments. `blank`: keep a blank line before it, as in the source.
    let tok (o: Out) (indent: int) (blank: bool) (t: Tok) =
        if not (noLeading.Contains t) then
            leading o indent t.Leading

        if blank && t.BlankBefore then
            if o.Content then
                o.Newline indent

            o.Blank indent

        o.Write(indent, t.Text)
        trailing o indent t.Trailing

    /// A table and its [column], or a definition's name: no spaces.
    let glued (o: Out) (indent: int) (ts: Tok list) =
        for t in ts do
            tok o indent false t

    let ops (o: Out) (indent: int) (op: Tok list) =
        op
        |> List.iteri (fun i t ->
            if i > 0 then
                o.Space()

            tok o indent false t)

    let gapAfterSign (op: Tok) (n: Node) =
        op.Kind = TokenKind.Identifier || (firstTok n).Kind = TokenKind.Operator

    let rec hasCall (n: Node) =
        match n with
        | Call _ -> true
        | Atom _ -> false
        | Paren g -> g.Items |> List.exists (fun it -> it.Arg |> Option.exists hasCall)
        | Unary(_, x) -> hasCall x
        | Chain(f, rest) -> hasCall f || rest |> List.exists (snd >> hasCall)
        | Juxt ns -> List.exists hasCall ns
        | VarBlock _ -> true

    let rec flat (o: Out) (indent: int) (n: Node) =
        match n with
        | Atom ts -> glued o indent ts
        | Call(name, g) ->
            tok o indent false name

            if opts.SpaceAfterFunction then
                o.Space()

            flatGroup o indent g
        | Paren g -> flatGroup o indent g
        | Unary(op, x) ->
            tok o indent false op

            if gapAfterSign op x then
                o.Space()

            flat o indent x
        | Chain(first, rest) ->
            flat o indent first

            for op, x in rest do
                if not (isOpText ":" op.Head) then
                    o.Space()

                ops o indent op
                o.Space()
                flat o indent x
        | Juxt parts ->
            parts
            |> List.iteri (fun i p ->
                if i > 0 then
                    o.Space()

                flat o indent p)
        | VarBlock _ -> o.Break()

    and flatGroup (o: Out) (indent: int) (g: Group) =
        tok o indent false g.Open

        if not g.Items.IsEmpty then
            o.Space()

            for it in g.Items do
                it.Arg |> Option.iter (flat o indent)

                match it.Sep with
                | Some s ->
                    tok o indent false s
                    o.Space()
                | None -> ()

            o.Space()

        tok o indent false g.Close

    /// The node's width on one line, or None if it can't be on one line (comments, VAR blocks,
    /// line breaks in strings or comments).
    and flatWidth (n: Node) : int option =
        match flatWidths.TryGetValue(box n) with
        | true, w -> w
        | _ ->
            let scratch = Out(nl)
            let first = firstTok n
            let skipped = noLeading.Add first
            flat scratch 0 n

            if skipped then
                noLeading.Remove first |> ignore

            let w = if scratch.Broken then None else Some scratch.Col
            flatWidths.[box n] <- w
            w

    /// Lay out a node starting at the current position. `indent`: where it starts if it has to
    /// start a new line; `extra`: what follows it on its last line (a comma).
    and render (o: Out) (indent: int) (n: Node) (extra: int) =
        let first = firstTok n

        if not first.Leading.IsEmpty && noLeading.Add first then
            leading o indent first.Leading

        let startsLine = o.Pending
        let c = if startsLine then indent else o.Col

        let fits () =
            match flatWidth n with
            | Some w -> c + w + extra <= width
            | None -> false

        match n with
        | VarBlock(defs, ret, body) -> renderVars o c defs ret body extra
        | Call(name, g) ->
            let strict =
                opts.Long
                && (g.Items.Length > 1
                    || g.Items
                       |> List.exists (fun it ->
                           match it.Arg with
                           | Some(Call _) -> true
                           | _ -> false))

            if not strict && fits () then
                flat o indent n
            else
                tok o c false name

                if opts.SpaceAfterFunction then
                    o.Space()

                let pairs = not opts.Long && eqCi name.Text "SWITCH"
                renderGroup o c g pairs
        | Paren g -> if fits () then flat o indent n else renderGroup o c g false
        | Atom _ -> flat o indent n
        | Unary(op, x) ->
            if fits () then
                flat o indent n
            else
                tok o c false op

                if gapAfterSign op x then
                    o.Space()

                render o (c + 4) x extra
        | Chain(first, rest) ->
            if fits () then
                flat o indent n
            elif rest |> List.forall (fun (op, _) -> isOpText ":" op.Head) then
                flat o (c + 4) n
            elif rest |> List.forall (fun (op, _) -> isOpText "=>" op.Head) then
                // (params) =>
                //     body
                render o c first 3

                for op, x in rest do
                    o.Space()
                    ops o (c + 4) op
                    o.Newline(c + 4)
                    render o (c + 4) x extra
            else
                // Operators start the continuation lines, indented one step from the line the chain
                // starts on, except right after a closing bracket on a line of its own
                // (`) / 'Rate'[Rate]`). Arithmetic and `&` pack operands onto a line while they
                // fit; an operand with a function call in it always starts a line.
                let cont = (if startsLine then indent else o.LineIndent) + 4
                let fill = rest |> List.forall (fun (op, _) -> level op >= 6)
                render o c first 0
                let count = List.length rest

                rest
                |> List.iteri (fun i (op, x) ->
                    let tailWidth = if i = count - 1 then extra else 0
                    let opWidth = (op |> List.sumBy (fun t -> t.Text.Length)) + op.Length - 1

                    let packs () =
                        fill
                        && not (hasCall x)
                        && op |> List.forall (fun t -> t.Leading.IsEmpty && t.Trailing.IsEmpty)
                        && (match flatWidth x with
                            | Some w -> o.Col + 1 + opWidth + 1 + w + tailWidth <= width
                            | None -> false)

                    if not o.Pending && (closesLine o || packs ()) then
                        o.Space()
                    else
                        o.Newline cont

                    ops o cont op
                    o.Space()
                    render o cont x tailWidth)
        | Juxt parts ->
            let count = List.length parts

            parts
            |> List.iteri (fun i p ->
                if i > 0 then
                    o.Space()

                render o (c + 4) p (if i = count - 1 then extra else 0))

    /// The current line holds just closing brackets (the end of a multi-line operand).
    and closesLine (o: Out) =
        let line = (o.LineText: string).Trim()
        line <> "" && line |> Seq.forall (fun ch -> ch = ')' || ch = '}' || ch = ' ')

    and renderGroup (o: Out) (c: int) (g: Group) (pairs: bool) =
        tok o c false g.Open

        if g.Items.IsEmpty then
            tok o c false g.Close
        else
            let inner = c + 4
            let items = Array.ofList g.Items
            let mutable i = 0

            let pairFits (a: Item) (b: Item) =
                match a.Arg, a.Sep, b.Arg with
                | Some x, Some s, Some y when s.Leading.IsEmpty && s.Trailing.IsEmpty ->
                    match flatWidth x, flatWidth y with
                    | Some wx, Some wy ->
                        let tailWidth = if b.Sep.IsSome then 1 else 0
                        inner + wx + 2 + wy + tailWidth <= width
                    | _ -> false
                | _ -> false

            while i < items.Length do
                o.Newline inner
                let it = items.[i]

                if pairs && i >= 1 && i + 1 < items.Length && pairFits it items.[i + 1] then
                    let b = items.[i + 1]
                    flat o inner it.Arg.Value
                    tok o inner false it.Sep.Value
                    o.Space()
                    flat o inner b.Arg.Value
                    b.Sep |> Option.iter (tok o inner false)
                    i <- i + 2
                else
                    it.Arg
                    |> Option.iter (fun a -> render o inner a (if it.Sep.IsSome then 1 else 0))

                    it.Sep |> Option.iter (tok o inner false)
                    i <- i + 1

            closer o c inner g.Close

    /// A closing bracket on a line of its own; comments before it stay inside the brackets.
    and closer (o: Out) (c: int) (inner: int) (t: Tok) =
        leading o inner t.Leading
        o.Newline c
        o.Write(c, t.Text)
        trailing o c t.Trailing

    /// A VAR's value: on the VAR's line if it fits there, else indented on the next.
    and value (o: Out) (indent: int) (d: Def) =
        let fitsInline =
            not o.Pending
            && (firstTok d.Value).Leading.IsEmpty
            && (match flatWidth d.Value with
                | Some w -> o.Col + 1 + w <= width
                | None -> false)

        if fitsInline then
            o.Space()
            flat o indent d.Value
        else
            o.Newline indent
            render o indent d.Value 0

    and defHead (o: Out) (c: int) (blank: bool) (d: Def) =
        tok o c blank d.Keyword
        o.Space()
        glued o c d.Name
        o.Space()
        tok o c false d.Eq

    and renderVars (o: Out) (c: int) (defs: Def list) (ret: Tok) (body: Node) (extra: int) =
        defs
        |> List.iteri (fun i d ->
            if i > 0 then
                o.Newline c

            defHead o c (i > 0) d
            value o (c + 4) d)

        o.Newline c
        tok o c true ret
        o.Newline(c + 4)
        render o (c + 4) body extra

    let renderItems (o: Out) (items: Item list) =
        let widths = items |> List.map (fun it -> it.Arg |> Option.bind flatWidth)

        let separatorsPlain =
            items
            |> List.forall (fun it -> it.Sep |> Option.forall (fun s -> s.Leading.IsEmpty && s.Trailing.IsEmpty))

        let total =
            if widths |> List.forall Option.isSome && separatorsPlain then
                Some((widths |> List.sumBy Option.get) + 2 * (items.Length - 1))
            else
                None

        match total with
        | Some w when o.Col + 1 + w <= width ->
            o.Space()

            for it in items do
                it.Arg |> Option.iter (flat o 4)

                it.Sep
                |> Option.iter (fun s ->
                    tok o 4 false s
                    o.Space())
        | _ ->
            for it in items do
                o.Newline 4
                it.Arg |> Option.iter (fun a -> render o 4 a (if it.Sep.IsSome then 1 else 0))
                it.Sep |> Option.iter (tok o 4 false)

    let o = Out(nl)

    match tree with
    | Expression None -> ()
    | Expression(Some n) -> render o 0 n 0
    | Query statements ->
        statements
        |> List.iteri (fun i s ->
            if i > 0 then
                o.Newline 0

            match s with
            | Define(kw, defs) ->
                tok o 0 (i > 0) kw

                for d in defs do
                    o.Newline 4
                    defHead o 4 true d

                    // SQLBI: a measure's (or column's, table's...) definition is on the line before its expression.
                    if eqCi d.Keyword.Text "VAR" then
                        value o 8 d
                    else
                        o.Newline 8
                        render o 8 d.Value 0
            | Evaluate(kw, e) ->
                tok o 0 (i > 0) kw
                o.Newline 0
                render o 0 e 0
            | Clause(kws, items) ->
                kws
                |> List.iteri (fun j k ->
                    if j > 0 then
                        o.Space()

                    tok o 0 (i > 0 && j = 0) k)

                renderItems o items)

    leading o 0 tail
    o.Text

// ---- Checking -----------------------------------------------------------------------------------

/// Why `after` doesn't mean what `before` means, if it doesn't: the same tokens in the same order
/// (keywords and built-in functions may change case), and the same comments in the same order.
let private difference (semicolons: bool) (before: string) (after: string) : string option =
    let a = lex semicolons before
    let b = lex semicolons after

    let code (ts: Token list) =
        ts |> List.filter (Token.isTrivia >> not)

    let comments (ts: Token list) =
        ts
        |> List.filter (fun t -> t.Kind = TokenKind.Comment)
        |> List.map (fun t -> if isLineComment t.Text then t.Text.TrimEnd() else t.Text)

    let same (x: Token) (y: Token) =
        x.Kind = y.Kind
        && (x.Text = y.Text
            || (x.Kind = TokenKind.Identifier && eqCi x.Text y.Text && caseFree x.Text))

    let ca, cb = code a, code b

    if ca.Length <> cb.Length then
        Some $"the result has {cb.Length} tokens where the input has {ca.Length}"
    else
        match List.zip ca cb |> List.tryFind (fun (x, y) -> not (same x y)) with
        | Some(x, y) -> Some $"'{x.Text}' at line {lineOf before x.Start} would become '{y.Text}'"
        | None when comments a <> comments b -> Some "the comments would change"
        | None -> None

// ---- Formatting ---------------------------------------------------------------------------------

let private formatWith (names: Names) (opts: FormatOptions) (input: string) : Result<string, FormatError> =
    let text = if isNull input then "" else input
    let tokens = lex opts.Semicolons text

    match lexProblems opts.Semicolons text tokens with
    | _ :: _ as problems -> Error(Syntax problems)
    | [] ->
        let toks, tail, declared = attach names tokens

        try
            let tree = parse toks (if opts.Semicolons then ";" else ",")
            keywordCase declared tree
            let nl = if text.Contains "\r\n" then "\r\n" else "\n"
            let output = layout opts nl tree tail

            match difference opts.Semicolons text output with
            | None -> Ok output
            | Some reason -> Error(Unsafe reason)
        with :? ParseFail as f ->
            let offset = if f.Offset < 0 then text.TrimEnd().Length else f.Offset

            Error(
                Syntax [
                    {
                        Line = lineOf text offset
                        Column = columnOf text offset
                        Message = f.Message
                    }
                ]
            )

let private noNames = {
    Bare = HashSet<string>(ci)
    Functions = HashSet<string>(ci)
}

/// Format a DAX expression or query.
let format (opts: FormatOptions) (dax: string) : Result<string, FormatError> = formatWith noNames opts dax

// ---- Models -------------------------------------------------------------------------------------

/// Which of a model's DAX expressions to format. Empty lists mean "no restriction".
type Selection = {
    /// Objects in any form `get` accepts. A table stands for every expression in it (its own DAX,
    /// its measures, calculated columns, calculation items, and row filters on it).
    Selectors: string list
    /// Only expressions held by these kinds of object.
    Kinds: ObjectKind list
    /// Only expressions in these tables (row filters count for the table they filter).
    Tables: string list
}

module Selection =
    let all = {
        Selectors = []
        Kinds = []
        Tables = []
    }

/// One expression and what formatting makes of it.
type Outcome = {
    Source: Deps.DaxSource
    /// The formatted DAX (the same text when it's already formatted), or why it can't be formatted.
    Result: Result<string, FormatError>
}

module Outcome =
    /// Formatting changes it.
    let changed (o: Outcome) =
        match o.Result with
        | Ok after -> after <> o.Source.Dax
        | Error _ -> false

type ModelFormat = {
    Outcomes: Outcome list
    /// Selectors that name an object without DAX.
    WithoutDax: string list
}

/// The table an expression belongs to: the owner's, or for a row filter, the filtered table's.
let private tableOf (src: Deps.DaxSource) =
    let prefix, suffix = "tablePermissions.", ".filterExpression"

    if
        src.Owner.Ref.Kind = ObjectKind.Role
        && src.Property.StartsWith(prefix, StringComparison.Ordinal)
        && src.Property.EndsWith(suffix, StringComparison.Ordinal)
    then
        Some(src.Property.Substring(prefix.Length, src.Property.Length - prefix.Length - suffix.Length))
    else
        ObjectEntry.table src.Owner

let private covers (e: ObjectEntry) (src: Deps.DaxSource) =
    e.Ref = src.Owner.Ref
    || (e.Ref.Kind = ObjectKind.Table
        && tableOf src |> Option.exists (eqCi (ObjectEntry.name e)))

/// What a reference points at, for comparing an expression before and after formatting.
let private targetKey (r: Deps.Reference) =
    match r.Target with
    | Deps.Resolved t -> "R:" + ObjectRef.format t
    | Deps.Unresolved text -> "U:" + String(text |> Seq.filter (Char.IsWhiteSpace >> not) |> Array.ofSeq)

/// Format the selected DAX expressions of a model, without changing it. Each result is checked
/// twice: by tokens (as `format` does) and by what its references resolve to.
let formatModel (opts: FormatOptions) (db: Database) (sel: Selection) : Result<ModelFormat, TessError> =
    let entries = objects db

    let chosen =
        sel.Selectors
        |> List.fold
            (fun acc s ->
                acc
                |> Result.bind (fun xs -> Selector.resolve entries s |> Result.map (fun e -> xs @ [ s, e ])))
            (Ok [])

    chosen
    |> Result.map (fun chosen ->
        let names = {
            Bare =
                HashSet<string>(
                    entries
                    |> List.filter (fun e -> e.Ref.Kind = ObjectKind.Table || e.Ref.Kind = ObjectKind.Calendar)
                    |> List.map ObjectEntry.name,
                    ci
                )
            Functions =
                HashSet<string>(
                    entries
                    |> List.filter (fun e -> e.Ref.Kind = ObjectKind.Function)
                    |> List.map ObjectEntry.name,
                    ci
                )
        }

        let resolve = Deps.resolver entries
        // TOM holds DAX in the comma dialect.
        let opts = { opts with Semicolons = false }

        let targets (src: Deps.DaxSource) =
            resolve src |> List.filter (fun r -> r.Start.IsSome) |> List.map targetKey

        // A calculated table with several partitions lists its "expression" once per partition;
        // only the first is the table's expression.
        let sources =
            Deps.daxSources entries |> List.distinctBy (fun s -> s.Owner.Ref, s.Property)

        let wanted (src: Deps.DaxSource) =
            (sel.Kinds.IsEmpty || List.contains src.Owner.Ref.Kind sel.Kinds)
            && (sel.Tables.IsEmpty
                || tableOf src |> Option.exists (fun t -> sel.Tables |> List.exists (eqCi t)))
            && (chosen.IsEmpty || chosen |> List.exists (fun (_, e) -> covers e src))

        let outcomes =
            sources
            |> List.filter wanted
            |> List.map (fun src ->
                let result =
                    formatWith names opts src.Dax
                    |> Result.bind (fun after ->
                        if after = src.Dax || targets src = targets { src with Dax = after } then
                            Ok after
                        else
                            Error(Unsafe "a reference would resolve to something else"))

                { Source = src; Result = result })

        {
            Outcomes = outcomes
            WithoutDax =
                chosen
                |> List.filter (fun (_, e) -> not (sources |> List.exists (covers e)))
                |> List.map fst
        })

/// Write the changed expressions back to the database they were computed from.
let apply (outcomes: Outcome list) =
    outcomes
    |> List.choose (fun o ->
        match o.Result with
        | Ok after when after <> o.Source.Dax ->
            Some(
                {
                    Owner = o.Source.Owner
                    Property = o.Source.Property
                    Before = o.Source.Dax
                    After = after
                    References = 0
                }
                : Rename.Edit
            )
        | _ -> None)
    |> Rename.applyEdits (fun e -> e.Object)
