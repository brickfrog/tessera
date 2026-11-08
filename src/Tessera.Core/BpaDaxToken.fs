/// DAX tokens as TE2 rules see them through Tokenize(): each token's Type is a code from TE2's
/// ANTLR DAX lexer, so rules can write e.g. `Tokenize().Any(Type = DIV and Next.Type = INTEGER_LITERAL)`.
/// Tokens come from tessera's own lexer (Dax.fs); only the type codes are TE2's.
module Tessera.BpaDaxToken

open System
open System.Collections.Generic
open Tessera.Dax

// The token type vocabulary (names and order, hence codes 1..N) is ported from Tabular Editor 2,
// TOMWrapper/Utils/DaxToken.Generated.cs and AntlrGrammars/DAXLexer.g4.
// Copyright (c) 2017 Daniel Otykier. MIT License: https://github.com/TabularEditor/TabularEditor
// NAME, or NAME:LITERAL where the keyword's DAX spelling differs from its symbolic name.
let private vocabulary =
    String.concat "" [
        "SINGLE_LINE_COMMENT DELIMITED_COMMENT WHITESPACES ABS ACOS ACOSH ACOT ACOTH ADDCOLUMNS "
        "ADDMISSINGITEMS ALL ALLCROSSFILTERED ALLEXCEPT ALLNOBLANKROW ALLSELECTED AND "
        "APPROXIMATEDISTINCTCOUNT ASIN ASINH ATAN ATANH AVERAGE AVERAGEA AVERAGEX BETADIST:BETA.DIST "
        "BETAINV:BETA.INV BLANK CALCULATE CALCULATETABLE CALENDAR CALENDARAUTO CEILING "
        "CHISQDIST:CHISQ.DIST CHISQDISTRT:CHISQ.DIST.RT CHISQINV:CHISQ.INV CHISQINVRT:CHISQ.INV.RT "
        "CLOSINGBALANCEMONTH CLOSINGBALANCEQUARTER CLOSINGBALANCEYEAR COALESCE COMBIN COMBINA "
        "COMBINEVALUES CONCATENATE CONCATENATEX CONFIDENCENORM:CONFIDENCE.NORM CONFIDENCET:CONFIDENCE.T "
        "CONTAINS CONTAINSROW CONTAINSSTRING CONTAINSSTRINGEXACT CONVERT COS COSH COT COTH COUNT COUNTA "
        "COUNTAX COUNTBLANK COUNTROWS COUNTX CROSSFILTER CROSSJOIN CURRENCY CURRENTGROUP CUSTOMDATA "
        "DATATABLE DATE DATEADD DATEDIFF DATESBETWEEN DATESINPERIOD DATESMTD DATESQTD DATESYTD DATEVALUE "
        "DAY DEGREES DETAILROWS DISTINCT DISTINCTCOUNT DISTINCTCOUNTNOBLANK DIVIDE EARLIER EARLIEST "
        "EDATE ENDOFMONTH ENDOFQUARTER ENDOFYEAR EOMONTH ERROR EVEN EXACT EXCEPT EXP "
        "EXPONDIST:EXPON.DIST FACT FALSE FILTER FILTERS FIND FIRSTDATE FIRSTNONBLANK FIRSTNONBLANKVALUE "
        "FIXED FLOOR FORMAT GCD GENERATE GENERATEALL GENERATESERIES GEOMEAN GEOMEANX GROUPBY "
        "HASONEFILTER HASONEVALUE HOUR IF IFEAGER:IF.EAGER IFERROR IGNORE INT INTERSECT ISBLANK "
        "ISCROSSFILTERED ISEMPTY ISERROR ISEVEN ISFILTERED ISINSCOPE ISLOGICAL ISNONTEXT ISNUMBER "
        "ISOCEILING:ISO.CEILING ISODD ISONORAFTER ISSELECTEDMEASURE ISSUBTOTAL ISTEXT KEEPFILTERS "
        "KEYWORDMATCH LASTDATE LASTNONBLANK LASTNONBLANKVALUE LCM LEFT LEN LN LOG LOG10 LOOKUPVALUE "
        "LOWER MAX MAXA MAXX MEDIAN MEDIANX MID MIN MINA MINUTE MINX MOD MONTH MROUND NATURALINNERJOIN "
        "NATURALLEFTOUTERJOIN NEXTDAY NEXTMONTH NEXTQUARTER NEXTYEAR NONVISUAL NORMDIST:NORM.DIST "
        "NORMINV:NORM.INV NORMSDIST:NORM.S.DIST NORMSINV:NORM.S.INV NOT NOW ODD OPENINGBALANCEMONTH "
        "OPENINGBALANCEQUARTER OPENINGBALANCEYEAR OR PARALLELPERIOD PATH PATHCONTAINS PATHITEM "
        "PATHITEMREVERSE PATHLENGTH PERCENTILEEXC:PERCENTILE.EXC PERCENTILEINC:PERCENTILE.INC "
        "PERCENTILEXEXC:PERCENTILEX.EXC PERCENTILEXINC:PERCENTILEX.INC PERMUT PI "
        "POISSONDIST:POISSON.DIST POWER PREVIOUSDAY PREVIOUSMONTH PREVIOUSQUARTER PREVIOUSYEAR PRODUCT "
        "PRODUCTX QUARTER QUOTIENT RADIANS RAND RANDBETWEEN RANKEQ:RANK.EQ RANKX RELATED RELATEDTABLE "
        "REMOVEFILTERS REPLACE REPT RIGHT ROLLUP ROLLUPADDISSUBTOTAL ROLLUPGROUP ROLLUPISSUBTOTAL ROUND "
        "ROUNDDOWN ROUNDUP ROW SAMEPERIODLASTYEAR SAMPLE SEARCH SECOND SELECTCOLUMNS SELECTEDMEASURE "
        "SELECTEDMEASUREFORMATSTRING SELECTEDMEASURENAME SELECTEDVALUE SIGN SIN SINH SQRT SQRTPI "
        "STARTOFMONTH STARTOFQUARTER STARTOFYEAR STDEVP:STDEV.P STDEVS:STDEV.S STDEVXP:STDEVX.P "
        "STDEVXS:STDEVX.S SUBSTITUTE SUBSTITUTEWITHINDEX SUM SUMMARIZE SUMMARIZECOLUMNS SUMX SWITCH "
        "TDIST:T.DIST TDIST2T:T.DIST.2T TDISTRT:T.DIST.RT TINV:T.INV TINV2T:T.INV.2T TAN TANH TIME "
        "TIMEVALUE TODAY TOPN TOPNPERLEVEL TOPNSKIP TOTALMTD TOTALQTD TOTALYTD TREATAS TRIM TRUE TRUNC "
        "UNICHAR UNICODE UNION UPPER USERELATIONSHIP USERNAME USEROBJECTID USERPRINCIPALNAME UTCNOW "
        "UTCTODAY VALUE VALUES VARP:VAR.P VARS:VAR.S VARXP:VARX.P VARXS:VARX.S WEEKDAY YEARFRAC WEEKNUM "
        "XIRR XNPV YEAR ACCRINT ACCRINTM AMORDEGRC AMORLINC COUPDAYBS COUPDAYS COUPDAYSNC COUPNCD "
        "COUPNUM COUPPCD CUMIPMT CUMPRINC DB DDB DISC DOLLARDE DOLLARFR DURATION EFFECT FV INTRATE IPMT "
        "ISPMT MDURATION NOMINAL NPER ODDFPRICE ODDFYIELD ODDLPRICE ODDLYIELD PDURATION PMT PPMT PRICE "
        "PRICEDISC PRICEMAT PV RATE RECEIVED RRI SLN SYD TBILLEQ TBILLPRICE TBILLYIELD VDB YIELD "
        "YIELDDISC YIELDMAT SAMPLEAXISWITHLOCALMINMAX EVALUATEANDLOG OFFSET INDEX WINDOW ORDERBY RANK "
        "ROWNUMBER PARTITIONBY EXTERNALMEASURE KMEANSCLUSTERING DEFINE EVALUATE ORDER BY START AT RETURN "
        "VAR IN ASC DESC SKIP_:SKIP DENSE BLANKS LAST FIRST WEEK BOTH NONE ONEWAY "
        "ONEWAYRIGHTFILTERSLEFT:ONEWAY_RIGHTFILTERSLEFT ONEWAYLEFTFILTERSRIGHT:ONEWAY_LEFTFILTERSRIGHT "
        "INTEGER DOUBLE STRING BOOLEAN DATETIME VARIANT TEXT ALPHABETICAL KEEP REL EXPR VAL ANYVAL "
        "ANYREF SCALAR INT64 DECIMAL NUMERIC DATE_LITERAL INTEGER_LITERAL REAL_LITERAL STRING_LITERAL "
        "TABLE COLUMN_OR_MEASURE TABLE_OR_VARIABLE OPEN_CURLY CLOSE_CURLY OPEN_PARENS CLOSE_PARENS COMMA "
        "PLUS MINUS STAR DIV CARET AMP ASSIGNMENT LT GT OP_AND OP_OR OP_NE OP_LE OP_GE LAMBDA COLON "
    ]

/// Symbolic name -> type code (1-based, in vocabulary order).
let private codes =
    let d = Dictionary<string, int>(StringComparer.Ordinal)

    vocabulary.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.iteri (fun i entry ->
        let name = entry.Split(':').[0]
        d.TryAdd(name, i + 1) |> ignore)

    d

/// Keyword spelling (upper case) -> type code. Only keywords: names before the first literal kind.
let private keywords =
    let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
    let firstNonKeyword = codes.["DATE_LITERAL"]

    vocabulary.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.iteri (fun i entry ->
        let code = i + 1

        if code > codes.["WHITESPACES"] && code < firstNonKeyword then
            let spelling =
                match entry.Split(':') with
                | [| _; literal |] -> literal
                | parts -> parts.[0]

            d.TryAdd(spelling, code) |> ignore)

    d

/// The type code for a symbolic token name, e.g. "DIV" -> 411.
let tryCode (name: string) =
    match codes.TryGetValue name with
    | true, c -> Some c
    | _ -> None

let private code name = codes.[name]

let private operatorCodes =
    dict [
        "+", code "PLUS"
        "-", code "MINUS"
        "*", code "STAR"
        "/", code "DIV"
        "^", code "CARET"
        "&", code "AMP"
        "=", code "ASSIGNMENT"
        "<", code "LT"
        ">", code "GT"
        "&&", code "OP_AND"
        "||", code "OP_OR"
        "<>", code "OP_NE"
        "<=", code "OP_LE"
        ">=", code "OP_GE"
        "=>", code "LAMBDA"
        ":", code "COLON"
        "{", code "OPEN_CURLY"
        "}", code "CLOSE_CURLY"
        "(", code "OPEN_PARENS"
        ")", code "CLOSE_PARENS"
        ",", code "COMMA"
    ]

let private typeOf (t: Token) =
    match t.Kind with
    | TokenKind.Whitespace -> code "WHITESPACES"
    | TokenKind.Comment when t.Text.StartsWith "/*" -> code "DELIMITED_COMMENT"
    | TokenKind.Comment -> code "SINGLE_LINE_COMMENT"
    | TokenKind.Identifier ->
        match keywords.TryGetValue t.Text with
        | true, c -> c
        | _ -> code "TABLE_OR_VARIABLE"
    | TokenKind.QuotedTable -> code "TABLE"
    | TokenKind.Bracketed -> code "COLUMN_OR_MEASURE"
    | TokenKind.String -> code "STRING_LITERAL"
    | TokenKind.DateLiteral -> code "DATE_LITERAL"
    | TokenKind.Number when t.Text.IndexOfAny [| '.'; 'e'; 'E' |] >= 0 -> code "REAL_LITERAL"
    | TokenKind.Number -> code "INTEGER_LITERAL"
    | TokenKind.Operator
    | TokenKind.Punctuation ->
        match operatorCodes.TryGetValue t.Text with
        | true, c -> c
        | _ -> 0
    | _ -> 0

/// One token of a DAX expression, with TE2's member names.
[<AllowNullLiteral>]
type DaxToken internal (token: Token, tokenType: int, index: int, all: IList<DaxToken>) =
    member _.Type = tokenType
    /// The token's text: names without their quotes or brackets, strings without their quotes.
    member _.Text = token.Text
    member _.TokenIndex = index
    member _.StartIndex = token.Start
    member _.StopIndex = token.Start + token.Length - 1
    member _.CommentOrWhitespace = Token.isTrivia token

    /// The next token that isn't a comment or whitespace (null at the end).
    member _.Next =
        let mutable i = index + 1

        while i < all.Count && all.[i].CommentOrWhitespace do
            i <- i + 1

        if i < all.Count then all.[i] else null

    /// The previous token that isn't a comment or whitespace (null at the start).
    member _.Prev =
        let mutable i = index - 1

        while i >= 0 && all.[i].CommentOrWhitespace do
            i <- i - 1

        if i >= 0 then all.[i] else null

    override _.ToString() = $"Token #{index}: {token.Text}"

/// Tokens of a DAX expression (empty for null). With includeHidden, whitespace and comments are kept.
let tokenize (includeHidden: bool) (dax: string) : IList<DaxToken> =
    let result = List<DaxToken>()

    if not (String.IsNullOrEmpty dax) then
        for t in Dax.tokenize dax do
            if includeHidden || not (Token.isTrivia t) then
                result.Add(DaxToken(t, typeOf t, result.Count, result))

    result
