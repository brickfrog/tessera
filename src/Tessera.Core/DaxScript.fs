/// DAX scripts, Tabular Editor 3's text format: many objects' DAX expressions and properties in one
/// file, to edit and apply back. This module is the syntax only: parsing (a small recursive-descent
/// parser over Dax tokens) and printing. ExportScript and ApplyScript connect it to a model.
///
///     MEASURE 'Sales'[Total] = SUM ( Sales[Amount] )
///         Description = "..."
///         FormatString = "0.00"
///
/// A DAX expression runs until one of these, whichever comes first:
///   - a `,` outside parentheses and braces (never valid there in DAX), which must start a property;
///   - a known property name followed by `=` and a value of its kind (TRUE or FALSE for Visible, a
///     string for Description, ...; any DAX for an expression property), as the first token of a
///     line, outside parentheses;
///   - an object keyword (MEASURE, COLUMN, TABLE, CALCULATIONGROUP, CALCULATIONITEM, FUNCTION, DEFINE,
///     EVALUATE) at the start of a line, or followed by an object reference outside parentheses.
/// Comments after an expression belong to it if they are on its last line, or indented deeper than
/// the line's keyword (or property); others (like the banners) don't. An expression that starts on
/// the line after `=` has its common indentation removed. Leading and trailing whitespace is not
/// part of an expression.
module Tessera.DaxScript

open System
open System.Collections.Generic
open System.Globalization
open System.Text
open Tessera.Dax

[<RequireQualifiedAccess>]
type ObjectType =
    | Measure
    | Column
    | Table
    | CalculationGroup
    | CalculationItem
    | Function

module ObjectType =
    let keyword =
        function
        | ObjectType.Measure -> "MEASURE"
        | ObjectType.Column -> "COLUMN"
        | ObjectType.Table -> "TABLE"
        | ObjectType.CalculationGroup -> "CALCULATIONGROUP"
        | ObjectType.CalculationItem -> "CALCULATIONITEM"
        | ObjectType.Function -> "FUNCTION"

    /// "a measure", for messages.
    let describe =
        function
        | ObjectType.Measure -> "a measure"
        | ObjectType.Column -> "a column"
        | ObjectType.Table -> "a table"
        | ObjectType.CalculationGroup -> "a calculation group"
        | ObjectType.CalculationItem -> "a calculation item"
        | ObjectType.Function -> "a function"

/// What a property's value is written as.
[<RequireQualifiedAccess>]
type PropertyType =
    /// "text" ("" escapes a quote).
    | String
    /// TRUE or FALSE.
    | Bool
    /// A whole number.
    | Int
    /// BOOLEAN, DOUBLE, INTEGER, DATETIME, CURRENCY or STRING.
    | DataType
    /// A DAX expression, delimited like an object's.
    | Expression
    /// A measure's format string: a lone string literal is the static format string; any other DAX is
    /// a dynamic format string expression.
    | StringOrExpression

[<RequireQualifiedAccess>]
type Value =
    | String of string
    | Bool of bool
    | Int of int
    | DataType of string
    | Expression of string

/// 1-based line and column (characters since the line break) in the script.
type Position = { Line: int; Column: int }

module Position =
    let none = { Line = 0; Column = 0 }

type Property = {
    /// The canonical name (as `properties` lists it), whatever case the script used.
    Name: string
    Value: Value
    Position: Position
}

/// One object in a script.
type Statement = {
    Type: ObjectType
    /// The table of a measure, column, calculation group or calculation item (for an item, its
    /// group's table). None for tables and functions.
    Table: string option
    /// The measure, column, table, item or function name; for a calculation group, its column.
    Name: string
    /// The DAX after `=`; None when the `=` is omitted (keep the existing expression).
    Expression: string option
    Properties: Property list
    /// Where the object's keyword is.
    Position: Position
}

type ParseError = { Position: Position; Message: string }

/// The data types a calculated column's `Datatype` takes.
let dataTypes = [ "BOOLEAN"; "DOUBLE"; "INTEGER"; "DATETIME"; "CURRENCY"; "STRING" ]

/// The properties each object type takes, in the order they are written.
let properties (t: ObjectType) : (string * PropertyType) list =
    let s, b, i, e =
        PropertyType.String, PropertyType.Bool, PropertyType.Int, PropertyType.Expression

    match t with
    | ObjectType.Measure -> [
        "Description", s
        "FormatString", PropertyType.StringOrExpression
        "FormatStringDefinition", e
        "DisplayFolder", s
        "Visible", b
        "DetailRows", e
        "KpiStatusExpression", e
        "KpiStatusDescription", s
        "KpiStatusGraphic", s
        "KpiTrendExpression", e
        "KpiTrendDescription", s
        "KpiTrendGraphic", s
        "KpiTargetExpression", e
        "KpiTargetDescription", s
        "KpiTargetFormatString", s
      ]
    | ObjectType.Column -> [
        "Description", s
        "FormatString", s
        "DisplayFolder", s
        "Visible", b
        "Datatype", PropertyType.DataType
      ]
    | ObjectType.Table -> [ "Description", s; "Visible", b; "DetailRows", e ]
    | ObjectType.CalculationGroup -> [ "Description", s; "Visible", b; "Precedence", i ]
    | ObjectType.CalculationItem -> [ "Description", s; "FormatString", e; "Ordinal", i ]
    | ObjectType.Function -> [ "Description", s; "Visible", b ]

let private allTypes = [
    ObjectType.Measure
    ObjectType.Column
    ObjectType.Table
    ObjectType.CalculationGroup
    ObjectType.CalculationItem
    ObjectType.Function
]

/// Every property name, of any object type: any of them ends an expression.
let private propertyNames =
    HashSet<string>(allTypes |> Seq.collect (properties >> Seq.map fst), StringComparer.OrdinalIgnoreCase)

/// Each property name's value types, across object types.
let private propertyTypes =
    let d = Dictionary<string, PropertyType list>(StringComparer.OrdinalIgnoreCase)

    for t in allTypes do
        for name, pt in properties t do
            match d.TryGetValue name with
            | true, types -> d.[name] <- pt :: types
            | _ -> d.[name] <- [ pt ]

    d

let private objectKeywords =
    HashSet<string>(
        [
            "MEASURE"
            "COLUMN"
            "TABLE"
            "CALCULATIONGROUP"
            "CALCULATIONITEM"
            "FUNCTION"
            "DEFINE"
            "EVALUATE"
        ]
    )

let private isBlank (c: char) = c = ' ' || c = '\t'

let private leading (line: string) =
    let mutable c = 0

    while c < line.Length && isBlank line.[c] do
        c <- c + 1

    c

/// The expression's text as written after `=`: when it starts on the next line, without the line
/// break and the lines' common indentation; trimmed.
let private layout (text: string) =
    let p = leading text

    if p < text.Length && (text.[p] = '\n' || text.[p] = '\r') then
        let skip =
            if text.[p] = '\r' && p + 1 < text.Length && text.[p + 1] = '\n' then
                p + 2
            else
                p + 1

        let lines = text.Substring(skip).Split('\n')

        let depth =
            match lines |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [||] -> 0
            | content -> content |> Array.map leading |> Array.min

        lines
        |> Array.map (fun l -> l.Substring(min depth (leading l)))
        |> String.concat "\n"
        |> fun s -> s.Trim()
    else
        text.Trim()

exception private SyntaxError of offset: int * message: string

/// Parse a DAX script, or a DAX query's DEFINE section (its EVALUATE sections are skipped).
let parse (text: string) : Result<Statement list, ParseError> =
    let src = if isNull text then "" else text
    let toks = Array.ofList (tokenize src)
    let n = toks.Length

    let lineStarts = [|
        yield 0
        for i in 0 .. src.Length - 1 do
            if src.[i] = '\n' then
                yield i + 1
    |]

    let position (offset: int) =
        let idx = Array.BinarySearch(lineStarts, offset)
        let line = if idx >= 0 then idx else (~~~idx) - 1

        {
            Line = line + 1
            Column = offset - lineStarts.[line] + 1
        }

    let fail offset message = raise (SyntaxError(offset, message))

    let offsetOf k =
        if k < n then toks.[k].Start else src.Length

    let shown k =
        if k >= n then
            "the end of the script"
        else
            let raw =
                src.Substring(toks.[k].Start, toks.[k].Length).Split('\n').[0].TrimEnd('\r')

            let raw =
                if raw.Length > 30 then
                    raw.Substring(0, 30) + "..."
                else
                    raw

            $"'{raw}'"

    let rec nextSig k =
        if k < n && Token.isTrivia toks.[k] then
            nextSig (k + 1)
        else
            k

    let isKind k kind = k < n && toks.[k].Kind = kind

    let word k =
        if isKind k TokenKind.Identifier then
            toks.[k].Text.ToUpperInvariant()
        else
            ""

    let isOp k s =
        isKind k TokenKind.Operator && toks.[k].Text = s

    let isPunct k s =
        isKind k TokenKind.Punctuation && toks.[k].Text = s

    let isName k =
        isKind k TokenKind.QuotedTable || isKind k TokenKind.Identifier

    /// Only spaces or tabs between the token and the start of its line.
    let lineStart k =
        let mutable i = toks.[k].Start - 1

        while i >= 0 && isBlank src.[i] do
            i <- i - 1

        i < 0 || src.[i] = '\n' || src.[i] = '\r'

    /// An object keyword followed by what that object starts with.
    let objectStart k =
        match word k with
        | "MEASURE"
        | "COLUMN"
        | "CALCULATIONGROUP" ->
            let t = nextSig (k + 1)
            isName t && isKind (nextSig (t + 1)) TokenKind.Bracketed
        | "TABLE"
        | "FUNCTION" -> isName (nextSig (k + 1))
        | "CALCULATIONITEM" -> isKind (nextSig (k + 1)) TokenKind.String
        | "DEFINE"
        | "EVALUATE" -> true
        | _ -> false

    /// The value after a property's `=` is empty: the next thing starts right away.
    let emptyValue v =
        v >= n
        || isPunct v ","
        || (lineStart v && objectKeywords.Contains(word v))
        || objectStart v
        || (isKind v TokenKind.Identifier
            && propertyNames.Contains toks.[v].Text
            && isOp (nextSig (v + 1)) "=")

    /// `Name = value` for a known property name, with a value of the right kind (so that a DAX line
    /// like `Visible = 1` doesn't read as the Visible property, which takes TRUE or FALSE).
    let propertyStart k =
        isKind k TokenKind.Identifier
        && propertyNames.Contains toks.[k].Text
        && (let eq = nextSig (k + 1)

            isOp eq "="
            && (let v = nextSig (eq + 1)

                propertyTypes.[toks.[k].Text]
                |> List.exists (function
                    | PropertyType.String -> isKind v TokenKind.String || emptyValue v
                    | PropertyType.Bool -> word v = "TRUE" || word v = "FALSE"
                    | PropertyType.Int ->
                        isKind v TokenKind.Number
                        || (isOp v "-" && isKind (nextSig (v + 1)) TokenKind.Number)
                    | PropertyType.DataType -> List.contains (word v) dataTypes
                    | PropertyType.Expression
                    | PropertyType.StringOrExpression -> true)))

    let objectEnd k =
        k >= n || (lineStart k && objectKeywords.Contains(word k)) || objectStart k

    /// Read the DAX expression that starts at token k, just after the `=` that ends at eqEnd.
    /// Comments after it belong to it if on its last line or deeper than column `baseline`.
    /// Returns the text and the token it stopped at.
    let readExpression (k: int) (eqEnd: int) (baseline: int) =
        let mutable depth = 0
        let mutable i = k
        let mutable stop = false
        let mutable last = -1

        while not stop && i < n do
            let t = toks.[i]

            if Token.isTrivia t then
                i <- i + 1
            else
                let atLineStart = lineStart i

                let ends =
                    (isPunct i "," && depth <= 0)
                    || (atLineStart && objectKeywords.Contains(word i))
                    || (depth <= 0 && objectStart i)
                    || (atLineStart && depth <= 0 && propertyStart i)

                if ends then
                    stop <- true
                else
                    if t.Kind = TokenKind.Punctuation then
                        match t.Text with
                        | "("
                        | "{" -> depth <- depth + 1
                        | ")"
                        | "}" -> depth <- depth - 1
                        | _ -> ()

                    last <- i
                    i <- i + 1

        let mutable endOffset =
            if last >= 0 then
                toks.[last].Start + toks.[last].Length
            else
                eqEnd

        let mutable j = if last >= 0 then last + 1 else k
        let mutable more = true

        while more && j < i do
            let t = toks.[j]

            if t.Kind = TokenKind.Comment then
                let p = position t.Start

                if (position (max 0 (endOffset - 1))).Line = p.Line || p.Column > baseline then
                    endOffset <- t.Start + t.Length
                else
                    more <- false

            j <- j + 1

        layout (src.Substring(eqEnd, endOffset - eqEnd)), i

    let parseValue (pt: PropertyType) (name: string) (k: int) (eqEnd: int) (baseline: int) =
        let s = nextSig k

        let atDelimiter j =
            isPunct j "," || objectEnd j || propertyStart j

        match pt with
        | PropertyType.String ->
            if isKind s TokenKind.String then
                Value.String toks.[s].Text, s + 1
            elif atDelimiter s then
                Value.String "", s
            else
                fail (offsetOf s) $"expected a string in double quotes for {name}, found {shown s}"
        | PropertyType.Bool ->
            match word s with
            | ("TRUE" | "FALSE") as w ->
                let after = nextSig (s + 1)
                let close = nextSig (after + 1)

                let next =
                    if isPunct after "(" && isPunct close ")" then
                        close + 1
                    else
                        s + 1

                Value.Bool(w = "TRUE"), next
            | _ -> fail (offsetOf s) $"expected TRUE or FALSE for {name}, found {shown s}"
        | PropertyType.Int ->
            let negative = isOp s "-"
            let d = if negative then nextSig (s + 1) else s

            match
                (if isKind d TokenKind.Number then
                     Int32.TryParse(toks.[d].Text, NumberStyles.None, CultureInfo.InvariantCulture)
                 else
                     (false, 0))
            with
            | true, v -> Value.Int(if negative then -v else v), d + 1
            | _ -> fail (offsetOf s) $"expected a whole number for {name}, found {shown s}"
        | PropertyType.DataType ->
            let w = word s

            if List.contains w dataTypes then
                Value.DataType w, s + 1
            else
                let allowed = String.concat ", " dataTypes
                fail (offsetOf s) $"expected one of {allowed} for {name}, found {shown s}"
        | PropertyType.Expression ->
            let text, next = readExpression k eqEnd baseline
            Value.Expression text, next
        | PropertyType.StringOrExpression ->
            let text, next = readExpression k eqEnd baseline

            match significant (List.ofArray toks.[k .. next - 1]) with
            | [ t ] when t.Kind = TokenKind.String -> Value.String t.Text, next
            | _ -> Value.Expression text, next

    let parseProperties (ot: ObjectType) (k0: int) =
        let props = ResizeArray<Property>()
        let known = properties ot
        let mutable k = nextSig k0
        let mutable go = true

        while go && k < n do
            let comma = isPunct k ","
            let nameK = if comma then nextSig (k + 1) else k

            let assignment = isKind nameK TokenKind.Identifier && isOp (nextSig (nameK + 1)) "="

            if not assignment then
                if comma then
                    fail (offsetOf nameK) $"expected a property (Name = value) after ',', found {shown nameK}"

                go <- false
            else
                let nameTok = toks.[nameK]

                match
                    known
                    |> List.tryFind (fun (p, _) -> String.Equals(p, nameTok.Text, StringComparison.OrdinalIgnoreCase))
                with
                | None ->
                    let names = known |> List.map fst |> String.concat ", "

                    fail
                        nameTok.Start
                        $"'{nameTok.Text}' is not a property of {ObjectType.describe ot} (its properties: {names})"
                | Some(name, pt) ->
                    let eq = nextSig (nameK + 1)
                    let baseline = (position toks.[k].Start).Column
                    let value, next = parseValue pt name (eq + 1) (toks.[eq].Start + 1) baseline

                    props.Add {
                        Name = name
                        Value = value
                        Position = position nameTok.Start
                    }

                    k <- nextSig next

        List.ofSeq props, k

    let statements = ResizeArray<Statement>()
    let mutable group: string option = None
    let mutable k = nextSig 0

    try
        while k < n do
            let kw = k
            let pos = position toks.[kw].Start
            let w = word kw

            let tableAt j =
                if isName j then
                    toks.[j].Text, nextSig (j + 1)
                else
                    fail (offsetOf j) $"expected a table name ('Sales' or Sales) after {w}, found {shown j}"

            let bracketAt j (after: string) =
                if isKind j TokenKind.Bracketed then
                    toks.[j].Text, nextSig (j + 1)
                else
                    fail (offsetOf j) $"expected [Name] after {after}, found {shown j}"

            /// `= expression` (optional) and properties; returns the next token.
            let body (ot: ObjectType) (table: string option) (name: string) (j: int) =
                let expression, j =
                    if isOp j "=" then
                        let text, next = readExpression (j + 1) (toks.[j].Start + 1) pos.Column
                        Some text, next
                    else
                        None, j

                let props, j = parseProperties ot j

                statements.Add {
                    Type = ot
                    Table = table
                    Name = name
                    Expression = expression
                    Properties = props
                    Position = pos
                }

                j

            match w with
            | "DEFINE" -> k <- nextSig (kw + 1)
            | "EVALUATE" ->
                let mutable j = nextSig (kw + 1)

                while j < n
                      && not (lineStart j && (word j = "DEFINE" || (word j <> "EVALUATE" && objectStart j))) do
                    j <- nextSig (j + 1)

                group <- None
                k <- j
            | "MEASURE"
            | "COLUMN" ->
                let ot =
                    if w = "MEASURE" then
                        ObjectType.Measure
                    else
                        ObjectType.Column

                let table, j = tableAt (nextSig (kw + 1))
                let name, j = bracketAt j (w + " " + quoteTable table)
                group <- None
                k <- body ot (Some table) name j
            | "TABLE" ->
                let name, j = tableAt (nextSig (kw + 1))
                group <- None
                k <- body ObjectType.Table None name j
            | "FUNCTION" ->
                let j = nextSig (kw + 1)

                if not (isName j) then
                    fail (offsetOf j) $"expected the function's name after FUNCTION, found {shown j}"

                group <- None
                k <- body ObjectType.Function None toks.[j].Text (nextSig (j + 1))
            | "CALCULATIONGROUP" ->
                let table, j = tableAt (nextSig (kw + 1))
                let column, j = bracketAt j ("CALCULATIONGROUP " + quoteTable table)

                if isOp j "=" then
                    fail toks.[j].Start "a calculation group has no expression of its own (its CALCULATIONITEMs have)"

                let props, j = parseProperties ObjectType.CalculationGroup j

                statements.Add {
                    Type = ObjectType.CalculationGroup
                    Table = Some table
                    Name = column
                    Expression = None
                    Properties = props
                    Position = pos
                }

                group <- Some table
                k <- j
            | "CALCULATIONITEM" ->
                match group with
                | None -> fail toks.[kw].Start "CALCULATIONITEM must follow its CALCULATIONGROUP"
                | Some g ->
                    let j = nextSig (kw + 1)

                    if not (isKind j TokenKind.String) then
                        fail (offsetOf j) $"expected the item's name in double quotes, found {shown j}"

                    k <- body ObjectType.CalculationItem (Some g) toks.[j].Text (nextSig (j + 1))
            | _ ->
                fail
                    toks.[kw].Start
                    $"expected MEASURE, COLUMN, TABLE, CALCULATIONGROUP, CALCULATIONITEM or FUNCTION, found {shown kw}"

        Ok(List.ofSeq statements)
    with SyntaxError(offset, message) ->
        Error {
            Position = position offset
            Message = message
        }

// --- printing -------------------------------------------------------------------------------------

/// "text", with "" escaping a quote.
let quoteString (s: string) = "\"" + s.Replace("\"", "\"\"") + "\""

let private isIdentifier (s: string) =
    s.Length > 0
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '.')

let private oneLine (s: string) = s.Replace('\r', ' ').Replace('\n', ' ')

let private reference (s: Statement) =
    let table = defaultArg s.Table ""

    match s.Type with
    | ObjectType.Measure
    | ObjectType.Column
    | ObjectType.CalculationGroup -> quoteTable table + bracket s.Name
    | ObjectType.Table -> quoteTable s.Name
    | ObjectType.CalculationItem -> quoteString s.Name
    | ObjectType.Function -> if isIdentifier s.Name then s.Name else quoteTable s.Name

/// The comment banner above an object, as Tabular Editor 3 writes it.
let private banner (s: Statement) =
    let title =
        match s.Type with
        | ObjectType.Measure -> "-- Measure: " + bracket s.Name
        | ObjectType.Column -> "-- Column: " + reference s
        | ObjectType.Table -> "-- Table: " + reference s
        | ObjectType.CalculationGroup -> "-- Calculation Group: " + quoteTable (defaultArg s.Table "")
        | ObjectType.CalculationItem -> "-- Calculation Item: " + reference s
        | ObjectType.Function -> "-- Function: " + reference s
        |> oneLine

    let rule = String('-', title.Length)
    [ rule; title; rule ]

/// " = expr" on one line; a multi-line expression starts on the next line, every line indented.
let private assignment (indent: string) (expr: string) =
    if expr = "" then
        " ="
    elif expr.Contains '\n' then
        expr.Split('\n')
        |> Array.map (fun l -> if l = "" then "" else indent + l)
        |> String.concat "\n"
        |> sprintf " =\n%s"
    else
        " = " + expr

let private valueText (indent: string) =
    function
    | Value.String s -> " = " + quoteString s
    | Value.Bool b -> if b then " = TRUE" else " = FALSE"
    | Value.Int i -> " = " + string i
    | Value.DataType d -> " = " + d
    | Value.Expression e -> assignment indent e

/// Write statements as a script: a banner above each object, its properties indented below it, and
/// calculation items indented under their group. Expressions are written as given (pass them
/// trimmed, with \n line breaks).
let print (statements: Statement list) : string =
    let sb = StringBuilder()

    let line (s: string) = sb.Append(s).Append('\n') |> ignore

    statements
    |> List.iteri (fun i s ->
        let item = s.Type = ObjectType.CalculationItem
        let indent = if item then "    " else ""

        if i > 0 then
            line ""

        if not item then
            banner s |> List.iter line

        let head = indent + ObjectType.keyword s.Type + " " + reference s

        match s.Expression with
        | Some e -> line (head + assignment (indent + "    ") e)
        | None -> line head

        let propIndent = indent + "    "

        s.Properties
        |> List.iteri (fun j p ->
            // After an empty expression, Tabular Editor needs a comma before the first property.
            let lead = if j = 0 && s.Expression = Some "" then ", " else ""
            line (propIndent + lead + p.Name + valueText (propIndent + "    ") p.Value)))

    sb.ToString()
