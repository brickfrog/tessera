/// Offline DAX linting, after Tabular Editor 3's code actions: rules over every DAX expression in a
/// model. A finding can carry a fix: token-span rewrites of the expression text, checked before they
/// are offered (the rewritten DAX must lex as cleanly, keep its VAR blocks parseable, and refer to the
/// same objects). Rules that match one of TE3's code actions use its id and name.
module Tessera.Lint

open System
open System.Collections.Generic
open System.Globalization
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

type Rule = {
    /// TE3's code action id (DI001, DR014...), or a tessera id (TS...) for rules TE3 doesn't have.
    Id: string
    Name: string
    /// TE3's category (Improvement, Readability), or Correctness for tessera's own.
    Category: string
    Severity: Validate.Severity
    Description: string
}

let private rule id name category severity description = {
    Id = id
    Name = name
    Category = category
    Severity = severity
    Description = description
}

/// Every rule, in the order findings are reported within an expression.
let rules = [
    rule
        "TS001"
        "Replace non-standard whitespace"
        "Correctness"
        Validate.Severity.Error
        "no-break, zero-width and other non-standard spaces outside strings, names and comments (TE3 3.27.1 rejects them)"
    rule "DI001" "Remove unused variable" "Improvement" Validate.Severity.Warning "a VAR that nothing refers to"
    rule
        "DI002"
        "Remove all unused variables"
        "Improvement"
        Validate.Severity.Warning
        "a VAR used only by other unused variables, never (directly or indirectly) by its RETURN"
    rule
        "DI003"
        "Remove table name from measure references"
        "Improvement"
        Validate.Severity.Warning
        "a measure written with its table, 'Sales'[Total] instead of [Total]"
    rule
        "DI004"
        "Add table name to column references"
        "Improvement"
        Validate.Severity.Warning
        "a column written without its table, [Amount] instead of 'Sales'[Amount]"
    rule
        "DI008"
        "Remove superfluous CALCULATE"
        "Improvement"
        Validate.Severity.Warning
        "a variable as the first argument of CALCULATE or CALCULATETABLE, which the filters can't change (reported, not fixed)"
    rule
        "DI014"
        "Replace IFERROR with DIVIDE"
        "Improvement"
        Validate.Severity.Warning
        "IFERROR(a / b, x), which DIVIDE(a, b, x) says directly"
    rule
        "DR014"
        "Simplify using IN"
        "Readability"
        Validate.Severity.Info
        "x = 1 || x = 2 || ..., which x IN {1, 2, ...} says more briefly"
]

let private ruleById = rules |> List.map (fun r -> r.Id, r) |> dict

/// The rule with this id, ignoring case.
let tryRule (id: string) =
    match ruleById.TryGetValue(id.Trim().ToUpperInvariant()) with
    | true, r -> Some r
    | _ -> None

/// Replace Length characters at Start with Text.
type TextEdit = {
    Start: int
    Length: int
    Text: string
}

type Finding = {
    Rule: Rule
    /// The object holding the expression (as Deps attributes it).
    Ref: ObjectRef
    /// The DAX property, as `get -p` takes it.
    Property: string
    /// Offset in the expression, and its 1-based line and column.
    Offset: int
    Line: int
    Column: int
    Message: string
    /// Edits to the expression that fix it, already checked; None when there is no safe fix.
    Fix: TextEdit list option
}

module Finding =
    let fixable (f: Finding) = f.Fix.IsSome

    /// As a validate issue, for the shared text and findings renderers.
    let toIssue (f: Finding) : Validate.Issue = {
        Rule = f.Rule.Id
        Severity = f.Rule.Severity
        Ref = f.Ref
        Property = Some f.Property
        Line = Some f.Line
        Column = Some f.Column
        Message = f.Message
    }

let private ci = StringComparison.OrdinalIgnoreCase
let private eqCi (a: string) (b: string) = String.Equals(a, b, ci)

let private isKw (kw: string) (t: Token) =
    t.Kind = TokenKind.Identifier && eqCi t.Text kw

let private isPunct (p: string) (t: Token) =
    t.Kind = TokenKind.Punctuation && t.Text = p

let private isOp (o: string) (t: Token) =
    t.Kind = TokenKind.Operator && t.Text = o

let private endOf (t: Token) = t.Start + t.Length

/// Apply non-overlapping edits.
let applyEdits (text: string) (edits: TextEdit list) =
    edits
    |> List.sortByDescending (fun e -> e.Start)
    |> List.fold (fun (s: string) e -> s.Substring(0, e.Start) + e.Text + s.Substring(e.Start + e.Length)) text

// ---- VAR blocks ---------------------------------------------------------------------------------

type private VarDef = {
    Name: string
    /// Indices into the significant tokens: VAR, its name, and its expression [ExprStart, ExprEnd).
    VarIndex: int
    NameIndex: int
    ExprStart: int
    ExprEnd: int
}

type private Block = {
    Vars: VarDef list
    ReturnIndex: int
    /// One past the last token of the RETURN expression.
    End: int
}

/// After these, an operand is expected: a VAR there starts a nested block, not the next variable.
let private expectsOperand (t: Token) =
    t.Kind = TokenKind.Operator
    || (t.Kind = TokenKind.Punctuation && t.Text <> ")" && t.Text <> "}")
    || isKw "RETURN" t
    || isKw "NOT" t
    || isKw "IN" t

/// Every VAR ... RETURN block, or None if the expression's VAR structure doesn't parse.
let private parseBlocks (code: Token array) : Block list option =
    let n = code.Length
    let blocks = ResizeArray<Block>()
    let mutable ok = true

    // An expression from i0: the index of the token that ends it (not consumed).
    let rec expr (i0: int) : int =
        let mutable i = i0
        let mutable depth = 0
        let mutable stop = false

        while ok && not stop && i < n do
            let t = code.[i]

            if isKw "VAR" t then
                if depth = 0 && i > i0 && not (expectsOperand code.[i - 1]) then
                    stop <- true
                else
                    i <- block i
            elif isKw "RETURN" t then
                if depth > 0 then
                    ok <- false

                stop <- true
            elif isPunct "(" t || isPunct "{" t then
                depth <- depth + 1
                i <- i + 1
            elif isPunct ")" t || isPunct "}" t then
                if depth = 0 then
                    stop <- true
                else
                    depth <- depth - 1
                    i <- i + 1
            elif (isPunct "," t || isPunct ";" t) && depth = 0 then
                stop <- true
            else
                i <- i + 1

        i

    // A block starting at a VAR: the index after its RETURN expression.
    and block (i0: int) : int =
        let vars = ResizeArray<VarDef>()
        let mutable i = i0

        while ok && i < n && isKw "VAR" code.[i] do
            if i + 2 < n && code.[i + 1].Kind = TokenKind.Identifier && isOp "=" code.[i + 2] then
                let e = expr (i + 3)

                if e = i + 3 then
                    ok <- false

                vars.Add {
                    Name = code.[i + 1].Text
                    VarIndex = i
                    NameIndex = i + 1
                    ExprStart = i + 3
                    ExprEnd = e
                }

                i <- e
            else
                ok <- false

        if ok && i < n && isKw "RETURN" code.[i] then
            let e = expr (i + 1)

            if e = i + 1 then
                ok <- false

            blocks.Add {
                Vars = List.ofSeq vars
                ReturnIndex = i
                End = e
            }

            e
        else
            ok <- false
            n

    let mutable i = 0

    while ok && i < n do
        let e = expr i

        if e < n && isKw "RETURN" code.[e] then
            ok <- false

        i <- if e = i then e + 1 else e

    if ok then Some(List.ofSeq blocks) else None

/// UDF parameter names: (name: TYPE, ...) before the first =>.
let private parameters (isFunction: bool) (code: Token array) =
    let names = HashSet<string>(StringComparer.OrdinalIgnoreCase)

    if isFunction then
        match code |> Array.tryFindIndex (isOp "=>") with
        | Some lambda ->
            for i in 0 .. lambda - 1 do
                if
                    code.[i].Kind = TokenKind.Identifier
                    && i + 1 < code.Length
                    && isOp ":" code.[i + 1]
                then
                    names.Add code.[i].Text |> ignore
        | None -> ()

    names

// ---- One expression -----------------------------------------------------------------------------

/// What a fix must keep: the referenced objects in order, as a set, or no new ones.
type private Keep =
    | SameReferences
    | SameObjects
    | NoNewObjects

type private Candidate = {
    RuleId: string
    Offset: int
    Message: string
    Fix: (TextEdit list * Keep) option
}

/// The model-wide facts rules need.
type Context = private {
    Resolve: Deps.DaxSource -> Deps.Reference list
    MeasureNames: HashSet<string>
}

let context (entries: ObjectEntry list) =
    let measures = HashSet<string>(StringComparer.OrdinalIgnoreCase)

    for e in entries do
        if e.Ref.Kind = ObjectKind.Measure then
            measures.Add(ObjectEntry.name e) |> ignore

    {
        Resolve = Deps.resolver entries
        MeasureNames = measures
    }

/// Problems the lexer sees: invalid tokens and unterminated /* comments.
let private lexProblems (tokens: Token list) =
    tokens
    |> List.filter (fun t ->
        t.Kind = TokenKind.Invalid
        || (t.Kind = TokenKind.Comment
            && t.Text.StartsWith "/*"
            && (t.Length < 4 || not (t.Text.EndsWith "*/"))))
    |> List.length

let private standardWhitespace (c: char) =
    c = ' ' || c = '\t' || c = '\r' || c = '\n'

/// Characters with no visible width that DAX doesn't accept as whitespace (the lexer reads them as
/// invalid characters).
let private zeroWidth (c: char) =
    c = '\u200B'
    || c = '\u200C'
    || c = '\u200D'
    || c = '\u2060'
    || c = '\uFEFF'
    || c = '\u180E'

let private charName (c: char) =
    let name =
        match c with
        | '\u00A0' -> "no-break space"
        | '\u2007' -> "figure space"
        | '\u202F' -> "narrow no-break space"
        | '\u3000' -> "ideographic space"
        | '\u0085' -> "next line"
        | '\u2028' -> "line separator"
        | '\u2029' -> "paragraph separator"
        | '\u000B' -> "vertical tab"
        | '\u000C' -> "form feed"
        | '\u200B' -> "zero-width space"
        | '\u200C' -> "zero-width non-joiner"
        | '\u200D' -> "zero-width joiner"
        | '\u2060' -> "word joiner"
        | '\uFEFF' -> "zero-width no-break space (byte order mark)"
        | '\u180E' -> "Mongolian vowel separator"
        | c when c >= '\u2000' && c <= '\u200A' -> "typographic space"
        | _ -> "space"

    $"U+{int c:X4} ({name})"

let private whitespaceRule (all: Token array) = [
    for t in all do
        let bad =
            match t.Kind with
            | TokenKind.Whitespace -> t.Text |> Seq.filter (standardWhitespace >> not) |> List.ofSeq
            | TokenKind.Invalid when t.Length = 1 && zeroWidth t.Text.[0] -> [ t.Text.[0] ]
            | _ -> []

        if not bad.IsEmpty then
            let replaced =
                String(
                    t.Text.ToCharArray()
                    |> Array.map (fun c -> if standardWhitespace c then c else ' ')
                )

            let names = bad |> List.distinct |> List.map charName |> String.concat ", "

            {
                RuleId = "TS001"
                Offset = t.Start
                Message = $"non-standard whitespace {names}; use a regular space"
                Fix =
                    Some(
                        [
                            {
                                Start = t.Start
                                Length = t.Length
                                Text = replaced
                            }
                        ],
                        SameReferences
                    )
            }
]

/// The index of the token closing the parenthesis at `openAt`, if any.
let private closing (code: Token array) (openAt: int) =
    let mutable depth = 0
    let mutable result = None
    let mutable i = openAt

    while result.IsNone && i < code.Length do
        let t = code.[i]

        if isPunct "(" t || isPunct "{" t then
            depth <- depth + 1
        elif isPunct ")" t || isPunct "}" t then
            depth <- depth - 1

            if depth = 0 then
                result <- Some i

        i <- i + 1

    result

/// Measures written with their table, and columns written without one. References inside NAMEOF
/// are left as written: NAMEOF returns the name (field parameters store it), so how it's written
/// may matter.
let private qualificationRules (ctx: Context) (dax: string) (code: Token array) (refs: Deps.Reference list) = [
    let nameofArguments = [
        for i in 0 .. code.Length - 2 do
            if isKw "NAMEOF" code.[i] && isPunct "(" code.[i + 1] then
                match closing code (i + 1) with
                | Some c -> code.[i + 1].Start, code.[c].Start
                | None -> ()
    ]

    let inNameof (start: int) =
        nameofArguments |> List.exists (fun (s, e) -> start > s && start < e)

    for r in refs do
        match r.Target, r.Start, r.Length with
        | Deps.Resolved target, Some start, Some length when not (inNameof start) ->
            let tokens = tokenize (dax.Substring(start, length)) |> significant
            let last = List.last tokens
            let bracketText = dax.Substring(start + last.Start, last.Length)
            let qualified = tokens.Length > 1

            match target.Kind with
            | ObjectKind.Measure when qualified -> {
                RuleId = "DI003"
                Offset = start
                Message = $"{r.Text} is a measure: write it without its table, as {bracketText}"
                Fix =
                    Some(
                        [
                            {
                                Start = start
                                Length = length
                                Text = bracketText
                            }
                        ],
                        SameReferences
                    )
              }
            | ObjectKind.Column when not qualified ->
                match ObjectRef.tableName target with
                | Some table ->
                    let full = quoteTable table + bracketText
                    let ambiguous = ctx.MeasureNames.Contains(ObjectRef.name target)

                    {
                        RuleId = "DI004"
                        Offset = start
                        Message =
                            if ambiguous then
                                $"{r.Text} is a column: write it with its table, as {full} (not fixed automatically: a measure has the same name)"
                            else
                                $"{r.Text} is a column: write it with its table, as {full}"
                        Fix =
                            if ambiguous then
                                None
                            else
                                Some(
                                    [
                                        {
                                            Start = start
                                            Length = length
                                            Text = full
                                        }
                                    ],
                                    SameReferences
                                )
                    }
                | None -> ()
            | _ -> ()
        | _ -> ()
]

/// Where a whole VAR block can be replaced by its RETURN expression without changing how the
/// surrounding DAX parses: at the start of an expression, an argument, or a variable's definition.
let private startsExpression (code: Token array) (i: int) =
    i = 0
    || (let p = code.[i - 1]

        (p.Kind = TokenKind.Punctuation && p.Text <> ")" && p.Text <> "}")
        || isKw "RETURN" p
        || isOp "=>" p
        || (isOp "=" p
            && i >= 3
            && isKw "VAR" code.[i - 3]
            && code.[i - 2].Kind = TokenKind.Identifier))

/// The deletion that removes a variable definition: whole lines when it has lines of its own,
/// otherwise the definition and the spaces that separate it from its neighbours.
let private varDeletion (dax: string) (all: Token array) (code: Token array) (v: VarDef) =
    let first = code.[v.VarIndex]
    let last = code.[v.ExprEnd - 1]
    let start, stop = first.Start, endOf last

    let lineStart =
        match dax.LastIndexOf('\n', max 0 (start - 1)) with
        | _ when start = 0 -> 0
        | -1 -> 0
        | nl -> nl + 1

    let startsLine =
        dax.Substring(lineStart, start - lineStart)
        |> Seq.forall (fun c -> c = ' ' || c = '\t')

    // After the definition: spaces, then optionally a comment on the same line, then a line break.
    let afterIndex =
        all
        |> Array.tryFindIndex (fun t -> t.Start >= stop)
        |> Option.defaultValue all.Length

    let lineEnd =
        let rec go k =
            if k >= all.Length then
                Some dax.Length
            else
                let t = all.[k]
                let oneLine = not (t.Text.Contains '\n' || t.Text.Contains '\r')

                match t.Kind with
                | TokenKind.Whitespace when t.Text.Contains '\n' -> Some(t.Start + t.Text.IndexOf '\n' + 1)
                | TokenKind.Whitespace
                | TokenKind.Comment when oneLine -> go (k + 1)
                | _ -> None

        go afterIndex

    let next =
        if afterIndex < all.Length && all.[afterIndex].Kind = TokenKind.Whitespace then
            Some all.[afterIndex]
        else
            None

    match startsLine, lineEnd with
    | true, Some e -> lineStart, e
    | false, Some _ ->
        // Ends its line but shares it with what comes before: take the spaces before it instead.
        let before =
            all
            |> Array.tryFindBack (fun t -> endOf t <= start)
            |> Option.filter (fun t -> t.Kind = TokenKind.Whitespace && endOf t = start)

        (before |> Option.map (fun t -> t.Start) |> Option.defaultValue start), stop
    | _ ->
        let e = next |> Option.map endOf |> Option.defaultValue stop
        start, e

/// Union overlapping or touching deletions into edits.
let private deletions (ranges: (int * int) list) =
    ranges
    |> List.sortBy fst
    |> List.fold
        (fun acc (s, e) ->
            match acc with
            | (ps, pe) :: rest when s <= pe -> (ps, max pe e) :: rest
            | _ -> (s, e) :: acc)
        []
    |> List.rev
    |> List.filter (fun (s, e) -> e > s)
    |> List.map (fun (s, e) -> { Start = s; Length = e - s; Text = "" })

/// Unused variables (DI001, DI002) and variables as CALCULATE's first argument (DI008).
let private variableRules (dax: string) (all: Token array) (code: Token array) (isFunction: bool) =
    match parseBlocks code with
    | None -> []
    | Some blocks ->
        let defined =
            blocks
            |> List.collect (fun b -> b.Vars)
            |> List.countBy (fun v -> v.Name.ToUpperInvariant())
            |> dict

        let parameters = parameters isFunction code

        /// A name that stands for exactly one variable in the expression.
        let unique (v: VarDef) =
            defined.[v.Name.ToUpperInvariant()] = 1 && not (parameters.Contains v.Name)

        let usesOf (v: VarDef) = [
            for j in 0 .. code.Length - 1 do
                let t = code.[j]

                if
                    j <> v.NameIndex
                    && t.Kind = TokenKind.Identifier
                    && eqCi t.Text v.Name
                    && not (j + 1 < code.Length && isPunct "(" code.[j + 1])
                then
                    j
        ]

        let unused = [
            for b in blocks do
                let vars = Array.ofList b.Vars

                // For each variable: which later variables of the block use it, and whether
                // anything else (the RETURN, or anything outside the block) does.
                let usedBy = Array.init vars.Length (fun _ -> ResizeArray<int>())
                let rooted = Array.create vars.Length false
                let used = Array.create vars.Length false

                for k in 0 .. vars.Length - 1 do
                    let v = vars.[k]

                    if not (unique v) then
                        rooted.[k] <- true

                    for j in usesOf v do
                        used.[k] <- true

                        let user =
                            vars
                            |> Array.tryFindIndex (fun w ->
                                w.VarIndex > v.VarIndex && j >= w.ExprStart && j < w.ExprEnd)

                        match user with
                        | Some w -> usedBy.[k].Add w
                        | None -> rooted.[k] <- true

                // Live: rooted, or used by a live variable. Users come later in the block, so one
                // pass from the end settles it.
                let live = Array.copy rooted

                for k in vars.Length - 1 .. -1 .. 0 do
                    if not live.[k] && usedBy.[k] |> Seq.exists (fun w -> live.[w]) then
                        live.[k] <- true

                let dead = [
                    for k in 0 .. vars.Length - 1 do
                        if not live.[k] then
                            k
                ]

                if not dead.IsEmpty then
                    let allDead = dead.Length = vars.Length
                    let blockStart = vars.[0].VarIndex

                    let fix =
                        if allDead && not (startsExpression code blockStart) then
                            None
                        else
                            let ranges = [
                                for k in dead do
                                    varDeletion dax all code vars.[k]

                                if allDead then
                                    let r = code.[b.ReturnIndex]

                                    let after =
                                        all
                                        |> Array.tryFind (fun t -> t.Start = endOf r && t.Kind = TokenKind.Whitespace)

                                    r.Start, (after |> Option.map endOf |> Option.defaultValue (endOf r))
                            ]

                            Some(deletions ranges, NoNewObjects)

                    for k in dead do
                        let v = vars.[k]

                        {
                            RuleId = if used.[k] then "DI002" else "DI001"
                            Offset = code.[v.NameIndex].Start
                            Message =
                                if used.[k] then
                                    $"variable {v.Name} is only used by other unused variables"
                                else
                                    $"variable {v.Name} is never used"
                            Fix = fix
                        }
        ]

        let calculate = [
            for i in 0 .. code.Length - 4 do
                let t = code.[i]

                if
                    (isKw "CALCULATE" t || isKw "CALCULATETABLE" t)
                    && isPunct "(" code.[i + 1]
                    && code.[i + 2].Kind = TokenKind.Identifier
                    && (isPunct "," code.[i + 3] || isPunct ")" code.[i + 3])
                then
                    let name = code.[i + 2].Text

                    let inScope =
                        blocks
                        |> List.exists (fun b ->
                            b.Vars |> List.exists (fun v -> eqCi v.Name name && i >= v.ExprEnd && i < b.End))

                    if inScope && not (parameters.Contains name) then
                        let effect =
                            if isPunct "," code.[i + 3] then
                                "its filter arguments don't change the result"
                            else
                                "it changes nothing"

                        {
                            RuleId = "DI008"
                            Offset = t.Start
                            Message =
                                $"{t.Text}'s first argument is the variable {name}, which was evaluated where it was defined, so {effect}"
                            Fix = None
                        }
        ]

        unused @ calculate

/// The arguments of the call whose parenthesis is at `openAt`: [start, end) index ranges, and the
/// index of the closing parenthesis.
let private arguments (code: Token array) (openAt: int) =
    closing code openAt
    |> Option.map (fun close ->
        let args = ResizeArray<int * int>()
        let mutable depth = 0
        let mutable start = openAt + 1

        for i in openAt + 1 .. close - 1 do
            let t = code.[i]

            if isPunct "(" t || isPunct "{" t then
                depth <- depth + 1
            elif isPunct ")" t || isPunct "}" t then
                depth <- depth - 1
            elif isPunct "," t && depth = 0 then
                args.Add(start, i)
                start <- i + 1

        args.Add(start, close)
        List.ofSeq args, close)

let private keywords = set [ "VAR"; "RETURN"; "IN"; "NOT"; "AND"; "OR" ]

/// IFERROR(a / b, x) -> DIVIDE(a, b, x), when a / b is the whole first argument: one division at
/// its top level, with only *, ^ and signs binding tighter around it.
let private iferrorRule (dax: string) (all: Token array) (code: Token array) = [
    for i in 0 .. code.Length - 2 do
        if isKw "IFERROR" code.[i] && isPunct "(" code.[i + 1] then
            match arguments code (i + 1) with
            | Some([ (a1, a1End); (a2, a2End) ], close) when a1End > a1 && a2End > a2 ->
                // Top-level tokens of the first argument, with whether each operator is a sign.
                let top = ResizeArray<int * bool>()
                let mutable depth = 0
                let mutable shapeOk = true

                for k in a1 .. a1End - 1 do
                    let t = code.[k]

                    if depth = 0 then
                        if t.Kind = TokenKind.Operator then
                            let sign =
                                (t.Text = "-" || t.Text = "+")
                                && (k = a1 || code.[k - 1].Kind = TokenKind.Operator)

                            top.Add(k, sign)
                        elif
                            t.Kind = TokenKind.Identifier
                            && keywords.Contains(t.Text.ToUpperInvariant())
                            && not (k + 1 < a1End && isPunct "(" code.[k + 1])
                        then
                            shapeOk <- false

                    if isPunct "(" t || isPunct "{" t then
                        depth <- depth + 1
                    elif isPunct ")" t || isPunct "}" t then
                        depth <- depth - 1

                let binary = top |> Seq.filter (snd >> not) |> Seq.map fst |> List.ofSeq
                let divisions = binary |> List.filter (fun k -> code.[k].Text = "/")

                match divisions with
                | [ d ] when shapeOk && d > a1 && d < a1End - 1 ->
                    let tighter =
                        binary
                        |> List.forall (fun k ->
                            let op = code.[k].Text
                            k = d || op = "^" || (op = "*" && k < d))

                    if tighter then
                        let allIndex (t: Token) =
                            all |> Array.findIndex (fun x -> x.Start = t.Start)

                        /// Remove a group's redundant parentheses (an argument needs none), with the
                        /// spaces just inside them.
                        let unwrap (s: int) (e: int) = [
                            if isPunct "(" code.[s] && closing code s = Some(e - 1) && e - 1 > s + 1 then
                                let o = allIndex code.[s]
                                let c = allIndex code.[e - 1]

                                let openEnd =
                                    if all.[o + 1].Kind = TokenKind.Whitespace then
                                        endOf all.[o + 1]
                                    else
                                        endOf all.[o]

                                let closeStart =
                                    if all.[c - 1].Kind = TokenKind.Whitespace then
                                        all.[c - 1].Start
                                    else
                                        all.[c].Start

                                {
                                    Start = all.[o].Start
                                    Length = openEnd - all.[o].Start
                                    Text = ""
                                }

                                {
                                    Start = closeStart
                                    Length = endOf all.[c] - closeStart
                                    Text = ""
                                }
                        ]

                        // "a / b" -> "a, b": the slash and the space before it become a comma.
                        let slash = allIndex code.[d]

                        let slashStart =
                            if all.[slash - 1].Kind = TokenKind.Whitespace then
                                all.[slash - 1].Start
                            else
                                code.[d].Start

                        // IFERROR(..., BLANK()) is DIVIDE's default: drop it, unless comments are in the way.
                        let blankDefault =
                            a2End - a2 = 3
                            && isKw "BLANK" code.[a2]
                            && isPunct "(" code.[a2 + 1]
                            && isPunct ")" code.[a2 + 2]

                        let tailStart = endOf code.[a1End - 1]
                        let tailEnd = code.[close].Start

                        let tailHasComments =
                            all
                            |> Array.exists (fun t ->
                                t.Kind = TokenKind.Comment && t.Start >= tailStart && t.Start < tailEnd)

                        let edits = [
                            {
                                Start = code.[i].Start
                                Length = code.[i].Length
                                Text = "DIVIDE"
                            }
                            yield! unwrap a1 d

                            {
                                Start = slashStart
                                Length = endOf code.[d] - slashStart
                                Text = ","
                            }
                            yield! unwrap (d + 1) a1End

                            if blankDefault && not tailHasComments then
                                {
                                    Start = tailStart
                                    Length = tailEnd - tailStart
                                    Text = ""
                                }
                        ]

                        // The call as the fix writes it, for the message.
                        let callEnd = endOf code.[close]
                        let grow = edits |> List.sumBy (fun e -> e.Text.Length - e.Length)

                        let rewritten =
                            (applyEdits dax edits).Substring(code.[i].Start, callEnd + grow - code.[i].Start)

                        {
                            RuleId = "DI014"
                            Offset = code.[i].Start
                            Message = $"IFERROR around a division: write {rewritten}"
                            Fix = Some(edits, SameReferences)
                        }
                | _ -> ()
            | _ -> ()
]

/// x = 1 || x = 2 || ... -> x IN {1, 2, ...}: the whole expression (or argument) is such a chain,
/// x is a column, measure or variable, and the values are literals that no BLANK equals (non-zero
/// numbers, or non-empty strings), so = and IN agree.
let private inRule (dax: string) (all: Token array) (code: Token array) (variables: HashSet<string>) = [
    let n = code.Length

    let starts = [
        for i in 0 .. n - 1 do
            if
                i = 0
                || (let p = code.[i - 1]

                    isPunct "(" p
                    || isPunct "," p
                    || isPunct "{" p
                    || isPunct ";" p
                    || isKw "RETURN" p
                    || isOp "=>" p
                    || (isOp "=" p && i >= 3 && isKw "VAR" code.[i - 3]))
            then
                i
    ]

    for s in starts do
        // lhs at k: its length in tokens.
        let lhs (k: int) =
            if k < n && code.[k].Kind = TokenKind.Bracketed then
                Some 1
            elif k + 1 < n && Token.isName code.[k] && code.[k + 1].Kind = TokenKind.Bracketed then
                Some 2
            elif
                k < n
                && code.[k].Kind = TokenKind.Identifier
                && variables.Contains code.[k].Text
                && not (
                    k + 1 < n
                    && (isPunct "(" code.[k + 1] || code.[k + 1].Kind = TokenKind.Bracketed)
                )
            then
                Some 1
            else
                None

        // A literal at k: (length, is a string).
        let literal (k: int) =
            let nonZero (t: Token) =
                match Double.TryParse(t.Text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, v -> v <> 0.0
                | _ -> false

            if k < n && code.[k].Kind = TokenKind.String && code.[k].Text <> "" then
                Some(1, true)
            elif k < n && code.[k].Kind = TokenKind.Number && nonZero code.[k] then
                Some(1, false)
            elif
                k + 1 < n
                && isOp "-" code.[k]
                && code.[k + 1].Kind = TokenKind.Number
                && nonZero code.[k + 1]
            then
                Some(2, false)
            else
                None

        let sameLhs (a: int) (b: int) (len: int) =
            Seq.forall
                (fun o ->
                    let x, y = code.[a + o], code.[b + o]
                    x.Kind = y.Kind && eqCi x.Text y.Text)
                [ 0 .. len - 1 ]

        // Parse comparisons joined by ||.
        let rec chain (k: int) (acc: (int * int * int * int * bool) list) =
            match lhs k with
            | Some ll when k + ll < n && isOp "=" code.[k + ll] ->
                match literal (k + ll + 1) with
                | Some(vl, isString) ->
                    let item = k, ll, k + ll + 1, vl, isString
                    let next = k + ll + 1 + vl

                    if next < n && isOp "||" code.[next] then
                        chain (next + 1) (item :: acc)
                    else
                        Some(List.rev (item :: acc), next)
                | None -> None
            | _ -> None

        match chain s [] with
        | Some(items, e) when items.Length >= 2 ->
            let ends =
                e = n
                || isPunct ")" code.[e]
                || isPunct "," code.[e]
                || isPunct "}" code.[e]
                || isPunct ";" code.[e]
                || isKw "VAR" code.[e]
                || isKw "RETURN" code.[e]

            let k0, l0, _, _, s0 = items.Head

            let uniform =
                items
                |> List.forall (fun (k, l, _, _, isString) -> l = l0 && isString = s0 && sameLhs k0 k l0)

            if ends && uniform then
                let spanStart = code.[s].Start
                let spanEnd = endOf code.[e - 1]

                let text (k: int) (len: int) =
                    dax.Substring(code.[k].Start, endOf code.[k + len - 1] - code.[k].Start)

                let values =
                    items |> List.map (fun (_, _, v, vl, _) -> text v vl) |> String.concat ", "

                let replacement = $"{text k0 l0} IN {{{values}}}"

                let hasComments =
                    all
                    |> Array.exists (fun t -> t.Kind = TokenKind.Comment && t.Start > spanStart && t.Start < spanEnd)

                {
                    RuleId = "DR014"
                    Offset = spanStart
                    Message = $"{items.Length} comparisons of {text k0 l0} joined by ||: write {replacement}"
                    Fix =
                        if hasComments then
                            None
                        else
                            Some(
                                [
                                    {
                                        Start = spanStart
                                        Length = spanEnd - spanStart
                                        Text = replacement
                                    }
                                ],
                                SameObjects
                            )
                }
        | _ -> ()
]

/// What a reference points at, for comparing an expression before and after a fix. Unresolved
/// references compare by their text without whitespace.
let private targetKey (r: Deps.Reference) =
    match r.Target with
    | Deps.Resolved t -> "R:" + ObjectRef.format t
    | Deps.Unresolved text -> "U:" + String(text |> Seq.filter (Char.IsWhiteSpace >> not) |> Array.ofSeq)

let private daxTargets (ctx: Context) (src: Deps.DaxSource) =
    ctx.Resolve src |> List.filter (fun r -> r.Start.IsSome) |> List.map targetKey

/// Whether `after` is a safe rewrite of `src`'s DAX: it lexes no worse, its VAR blocks still parse,
/// and it refers to what `keep` requires.
let private safe (ctx: Context) (src: Deps.DaxSource) (keep: Keep) (after: string) =
    let beforeTokens = tokenize src.Dax
    let afterTokens = tokenize after

    let parses (tokens: Token list) =
        (parseBlocks (significant tokens |> Array.ofList)).IsSome

    lexProblems afterTokens <= lexProblems beforeTokens
    && (parses afterTokens || not (parses beforeTokens))
    && (let a = daxTargets ctx src
        let b = daxTargets ctx { src with Dax = after }

        match keep with
        | SameReferences -> a = b
        | SameObjects -> set a = set b
        | NoNewObjects -> Set.isSubset (set b) (set a))

/// Lint one expression (its DAX may differ from the model's, e.g. while fixing).
let lintSource (ctx: Context) (src: Deps.DaxSource) : Finding list =
    let dax = src.Dax
    let tokens = tokenize dax
    let all = Array.ofList tokens
    let code = significant tokens |> Array.ofList
    let isFunction = src.Owner.Ref.Kind = ObjectKind.Function

    let variables = HashSet<string>(StringComparer.OrdinalIgnoreCase)

    for i in 0 .. code.Length - 2 do
        if isKw "VAR" code.[i] && code.[i + 1].Kind = TokenKind.Identifier then
            variables.Add code.[i + 1].Text |> ignore

    let refs = ctx.Resolve src |> List.filter (fun r -> r.Start.IsSome)

    let candidates =
        whitespaceRule all
        @ variableRules dax all code isFunction
        @ qualificationRules ctx dax code refs
        @ iferrorRule dax all code
        @ inRule dax all code variables

    // Candidates that share a fix (a block's unused variables) are checked once.
    let checkedFix = Dictionary<TextEdit list, bool>(HashIdentity.Structural)

    let verified (edits: TextEdit list, keep: Keep) =
        match checkedFix.TryGetValue edits with
        | true, ok -> ok
        | _ ->
            let ok = safe ctx src keep (applyEdits dax edits)
            checkedFix.[edits] <- ok
            ok

    candidates
    |> List.map (fun c -> {
        Rule = ruleById.[c.RuleId]
        Ref = src.Owner.Ref
        Property = src.Property
        Offset = c.Offset
        Line = lineOf dax c.Offset
        Column = columnOf dax c.Offset
        Message = c.Message
        Fix = c.Fix |> Option.filter verified |> Option.map fst
    })
    |> List.sortBy (fun f -> f.Offset, rules |> List.findIndex (fun r -> r.Id = f.Rule.Id))

/// Lint every DAX expression in the model, in model order.
let lint (db: Database) : Finding list =
    let entries = objects db
    let ctx = context entries
    Deps.daxSources entries |> List.collect (lintSource ctx)

// ---- Fixing -------------------------------------------------------------------------------------

/// How many passes a fix may take to settle (removing one unused variable can leave another).
let private maxPasses = 10

type ExpressionFix = {
    Source: Deps.DaxSource
    After: string
    /// Findings fixed, by rule id (cascades included: a variable left unused by a fix counts too).
    Fixed: (string * int) list
}

type FixResult = {
    Fixes: ExpressionFix list
    /// Selected findings still there after fixing (they have no safe fix).
    Remaining: Finding list
    /// Expressions whose fixes didn't settle within the pass limit; left unchanged.
    Unsettled: Deps.DaxSource list
}

let private overlaps (a: TextEdit) (b: TextEdit) =
    a.Start < b.Start + b.Length && b.Start < a.Start + a.Length

/// Fix one expression: apply the selected findings' fixes until none are left.
let private fixSource (ctx: Context) (select: Finding -> bool) (src: Deps.DaxSource) =
    let rec pass (n: int) (dax: string) (fixedIds: string list) =
        let current = { src with Dax = dax }
        let findings = lintSource ctx current |> List.filter select

        let groups =
            findings
            |> List.filter Finding.fixable
            |> List.groupBy (fun f -> f.Fix.Value)
            |> List.sortBy (fun (edits, _) ->
                let s = edits |> List.map (fun e -> e.Start) |> List.min
                let e = edits |> List.map (fun e -> e.Start + e.Length) |> List.max
                s, -(e - s))

        if groups.IsEmpty then
            Ok(dax, fixedIds, findings)
        elif n >= maxPasses then
            Error()
        else
            let accepted =
                groups
                |> List.fold
                    (fun (acc: (TextEdit list * Finding list) list) (edits, fs) ->
                        let taken = acc |> List.collect fst

                        if edits |> List.exists (fun e -> taken |> List.exists (overlaps e)) then
                            acc
                        else
                            acc @ [ edits, fs ])
                    []

            let all = accepted |> List.collect fst
            let combined = applyEdits dax all

            // Each fix was checked alone; check them together, else take just the first.
            let next, taken =
                if safe ctx current NoNewObjects combined then
                    combined, accepted
                else
                    let edits, fs = accepted.Head
                    applyEdits dax edits, [ edits, fs ]

            let ids = taken |> List.collect snd |> List.map (fun f -> f.Rule.Id)
            pass (n + 1) next (fixedIds @ ids)

    pass 0 src.Dax []

/// Fix the selected findings across the model (without changing it): what each expression becomes.
let fix (db: Database) (select: Finding -> bool) : FixResult =
    let entries = objects db
    let ctx = context entries
    let fixes = ResizeArray<ExpressionFix>()
    let remaining = ResizeArray<Finding>()
    let unsettled = ResizeArray<Deps.DaxSource>()

    for src in Deps.daxSources entries do
        match fixSource ctx select src with
        | Ok(after, fixedIds, left) ->
            remaining.AddRange left

            if after <> src.Dax then
                fixes.Add {
                    Source = src
                    After = after
                    Fixed =
                        fixedIds
                        |> List.countBy id
                        |> List.sortBy (fun (id, _) -> rules |> List.findIndex (fun r -> r.Id = id))
                }
        | Error() ->
            unsettled.Add src
            remaining.AddRange(lintSource ctx src |> List.filter select)

    {
        Fixes = List.ofSeq fixes
        Remaining = List.ofSeq remaining
        Unsettled = List.ofSeq unsettled
    }

/// Apply fixes to the database they were computed from.
let apply (result: FixResult) =
    result.Fixes
    |> List.map (fun f ->
        ({
            Owner = f.Source.Owner
            Property = f.Source.Property
            Before = f.Source.Dax
            After = f.After
            References = f.Fixed |> List.sumBy snd
        }
        : Rename.Edit))
    |> Rename.applyEdits (fun e -> e.Object)
