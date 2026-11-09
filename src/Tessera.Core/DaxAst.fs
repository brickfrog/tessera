/// The DAX abstract syntax tree that `DaxParser` produces: expressions, user-defined function
/// definitions and queries. Nothing here knows about a model: names are unresolved, exactly as
/// written. Resolving them (is `[X]` a measure or a column? is `Year` a table, a variable or the
/// YEAR keyword of DATEADD?) is the binder's job.
///
/// Every node carries the span of the source text it was read from. Two trees compare equal with
/// `=` only if their spans agree too; compare `stripSpans a = stripSpans b` to ignore positions.
///
/// What the parser guarantees about a tree it returns (hand-built trees need not obey these, and
/// `DaxPrinter` copes with them):
///   - `Paren` is kept wherever the source had parentheses, so the tree's shape IS the source's
///     grouping: nothing needs re-deriving from precedence.
///   - `Tuple` has two or more elements; `(a)` is a `Paren`, never a one-element tuple.
///   - `Words` has two or more words. A single bare word is always an `Identifier`.
///   - `VarBlock` has one or more declarations.
///   - `TableConstructor` has one or more rows.
///   - `Omitted` appears only as an element of `Call` arguments, and never as a call's ONLY
///     argument: `F()` has no arguments, `F(,)` has two omitted ones.
///   - The tree is at most `DaxParser.maxDepth` levels deep, so plain recursion over it fits a
///     default stack. (The functions here that recurse raise InsufficientExecutionStackException
///     on a hand-built tree that is deeper than the stack, rather than overflowing it.)
module Tessera.DaxAst

open System
open System.Collections.Generic
open System.Globalization
open System.Runtime.CompilerServices

/// Where a node is in the source: `source.Substring(Start, Length)`.
type Span = {
    Start: int
    Length: int
} with

    /// The offset just past the node.
    member s.End = s.Start + s.Length

module Span =
    /// The span of a node that didn't come from source text (and of every node after `stripSpans`).
    let none = { Start = 0; Length = 0 }

    let make (start: int) (length: int) = { Start = start; Length = length }

    /// From the start of `a` to the end of `b`.
    let through (a: Span) (b: Span) = {
        Start = a.Start
        Length = max 0 (b.End - a.Start)
    }

/// A name with its position: a function, variable or parameter name, or the `[Name]` of a definition.
type Ident = { Name: string; Span: Span }

/// A table name. `Quoted` is whether it was written `'Sales'` rather than `Sales`; a quoted name is
/// always a table, a bare one may also be a variable or parameter.
type TableName = {
    Name: string
    Quoted: bool
    Span: Span
}

[<RequireQualifiedAccess>]
type UnaryOp =
    /// `+x`
    | Plus
    /// `-x`
    | Minus
    /// `NOT x`. DAX's NOT is a prefix operator, never a function: `NOT (a) = b` is `NOT ((a) = b)`.
    | Not

[<RequireQualifiedAccess>]
type BinaryOp =
    /// `+`
    | Add
    /// `-`
    | Subtract
    /// `*`
    | Multiply
    /// `/`
    | Divide
    /// `^`
    | Power
    /// `&` (text concatenation)
    | Concat
    /// `=`
    | Equal
    /// `==` (strict: BLANK is not 0 or "")
    | StrictEqual
    /// `<>`
    | NotEqual
    /// `<`
    | Less
    /// `<=`
    | LessOrEqual
    /// `>`
    | Greater
    /// `>=`
    | GreaterOrEqual
    /// `&&`
    | And
    /// `||`
    | Or
    /// `IN`: the left side is a value or a `Tuple`, the right side a table.
    | In

type Expr = { Kind: ExprKind; Span: Span }

and ExprKind =
    /// A whole number that fits Int64: `42`. `text` is the literal as written (`007`).
    | IntegerLiteral of value: int64 * text: string
    /// A number with a decimal point or exponent, or a whole number too big for Int64: `1.5`, `.5`,
    /// `1e3`. `text` is the literal as written, always with `.` as the decimal mark (also when the
    /// source used the `;` dialect's `1,5`), so a decimal can be read from it without the rounding
    /// of `value`.
    | NumberLiteral of value: float * text: string
    /// `"text"`, with `""` unescaped.
    | StringLiteral of string
    /// A bare `TRUE` or `FALSE`. (`TRUE()` is a `Call`.)
    | BooleanLiteral of bool
    /// `dt"2024-01-31"` or `dt"2024-01-31T13:00:00"`: the text between the quotes. See `tryDate`.
    | DateLiteral of text: string
    /// A bare word, as written: a variable, a parameter, a table (`Sales`), or one of the keywords
    /// some functions take as an argument (`DESC`, `MONTH`, `BOTH`, `INTEGER`...). Only the binder
    /// can tell which; see the `Keyword` pattern.
    | Identifier of name: string
    /// Two or more bare keywords side by side as one call argument: `ASC BLANKS LAST` in ORDERBY,
    /// `ROWS COLUMNS` as a visual calculation's axis. As written.
    | Words of string list
    /// `'Sales Table'`: a table (or calendar), by its quoted name.
    | QuotedTable of name: string
    /// `Sales[Amount]` or `'Sales Table'[Amount]`: a column or a measure of a table.
    | Qualified of table: TableName * name: string
    /// `[Amount]`: a measure, or a column in the current row context or of a table expression.
    | Bracketed of name: string
    /// `@Name`: a query parameter. The name is without the `@`.
    | QueryParameter of name: string
    /// `NAME(arg, ...)`: a built-in or user-defined function call. An argument left out between or
    /// after separators is `Omitted`.
    | Call of name: Ident * args: Expr list
    /// An argument that was left out: the third one of `RANKX(T, [m], , DESC)`. Its span is empty,
    /// at the separator or parenthesis that ends it.
    | Omitted
    | Unary of UnaryOp * operand: Expr
    | Binary of BinaryOp * left: Expr * right: Expr
    /// `VAR a = ... VAR b = ... RETURN body`. Each variable is in scope in the declarations after it
    /// and in the body, not in its own value.
    | VarBlock of vars: VarDecl list * body: Expr
    /// `{1, 2}`, `{(1, "a"), (2, "b")}`: each row is a scalar expression or a `Tuple`. (DATATABLE's
    /// data argument `{{1, "a"}, {2, "b"}}` has `TableConstructor` rows.)
    | TableConstructor of rows: Expr list
    /// `(a, b)`: a row of values, in a table constructor or on the left of IN.
    | Tuple of Expr list
    /// `(x)`: parentheses the source had.
    | Paren of Expr

and VarDecl = {
    Name: Ident
    Value: Expr
    /// From `VAR` to the end of the value.
    Span: Span
}

// ---- User-defined functions ---------------------------------------------------------------------

/// What kind of argument a parameter takes. Value types are evaluated before the call; reference
/// types (the ...Ref ones) pass the argument unevaluated.
[<RequireQualifiedAccess>]
type ParameterType =
    | AnyVal
    | Scalar
    | Table
    | AnyRef
    | CalendarRef
    | ColumnRef
    | MeasureRef
    | TableRef

/// The data type of a scalar parameter.
[<RequireQualifiedAccess>]
type ScalarType =
    | Variant
    | Int64
    | Decimal
    | Double
    | String
    | DateTime
    | Boolean
    | Numeric

[<RequireQualifiedAccess>]
type ParameterMode =
    /// `VAL`: the argument is evaluated once, before the call.
    | Val
    /// `EXPR`: the argument is evaluated inside the function, in the context where it is used.
    | Expr

/// One parameter of a function: `name [: [type] [subtype] [mode]] [= default]`. `Type`, `Subtype`
/// and `Mode` are what was written, each None if left out; `Parameter.effectiveType` and
/// `Parameter.effectiveMode` apply DAX's defaults.
type Parameter = {
    Name: Ident
    Type: ParameterType option
    Subtype: ScalarType option
    Mode: ParameterMode option
    /// `= expr`: the value used when the caller leaves the argument out.
    Default: Expr option
    Span: Span
}

/// `(a: INT64, b: SCALAR VAL, c: ANYREF EXPR) => body`
type FunctionDef = {
    Parameters: Parameter list
    Body: Expr
    Span: Span
}

// ---- Queries ------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type DefinitionKind =
    /// `MEASURE Sales[Total] = expr`
    | Measure of table: TableName * name: Ident * value: Expr
    /// `COLUMN Sales[Margin] = expr`
    | Column of table: TableName * name: Ident * value: Expr
    /// `TABLE Top = expr`
    | Table of name: TableName * value: Expr
    /// `VAR x = expr`: a query variable, in scope in everything after it.
    | Var of name: Ident * value: Expr
    /// `FUNCTION AddTax = (amount: NUMERIC) => amount * 1.1`
    | Function of name: Ident * definition: FunctionDef
    /// `MPARAMETER Name = expr`: overrides a Power Query parameter for the query.
    | MParameter of name: Ident * value: Expr

type Definition = {
    Kind: DefinitionKind
    /// From the keyword to the end of the value.
    Span: Span
}

[<RequireQualifiedAccess>]
type SortDirection =
    | Asc
    | Desc

/// One `ORDER BY` key. `Direction` is None when neither ASC nor DESC was written (ascending).
type OrderKey = {
    Expr: Expr
    Direction: SortDirection option
    Span: Span
}

/// `EVALUATE table [ORDER BY key, ...] [START AT value, ...]`
type Evaluate = {
    Table: Expr
    OrderBy: OrderKey list
    StartAt: Expr list
    Span: Span
}

/// `[DEFINE definition ...] EVALUATE ... [EVALUATE ...]`
type Query = {
    Definitions: Definition list
    Evaluates: Evaluate list
    Span: Span
}

/// Anything `DaxParser.parse` reads: it tells the three apart by how the text starts.
[<RequireQualifiedAccess>]
type Document =
    | Expression of Expr
    | Function of FunctionDef
    | Query of Query

// ---- Vocabulary ---------------------------------------------------------------------------------

let private wordSet (words: string) =
    HashSet<string>(
        words.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries),
        StringComparer.OrdinalIgnoreCase
    )

/// Words that can never be a name where an expression is expected.
let private reservedWords = wordSet "VAR RETURN IN NOT DEFINE EVALUATE"

/// `VAR`, `RETURN`, `IN`, `NOT`, `DEFINE`, `EVALUATE` (in any case): never a table, variable or
/// function name. `TRUE` and `FALSE` are not in the list: bare, they are literals.
let isReserved (word: string) = reservedWords.Contains word

/// The keywords DAX functions take as arguments, and the words of multi-word ones.
let private keywordWords =
    wordSet
        """
ASC DESC BLANKS FIRST LAST ROWS COLUMNS SKIP DENSE REL ABS KEEP DEFAULT
BOTH NONE ONEWAY ONEWAY_RIGHTFILTERSLEFT ONEWAY_LEFTFILTERSRIGHT
INTEGER DOUBLE STRING BOOLEAN DATETIME VARIANT TEXT ALPHABETICAL CURRENCY DECIMAL INT64 NUMERIC
SECOND MINUTE HOUR DAY WEEK MONTH QUARTER YEAR
LOWESTPARENT HIGHESTPARENT
"""

/// Whether a bare word is one of the keywords some DAX function takes as an argument (`DESC`,
/// `MONTH`, `BOTH`, `INTEGER`, `SKIP`...), in any case. The same word may still name a table or a
/// variable (a table called Year): which it is depends on the function and the argument's position.
let isKeywordWord (word: string) = keywordWords.Contains word

/// An argument that reads as a keyword: a bare word or several (`DESC`, `ASC BLANKS LAST`), given
/// in upper case with single spaces. It matches ANY bare word, so use it only where the function
/// takes a keyword: `| Call(f, [ t; e; _; Keyword "DESC" ]) -> ...`.
let (|Keyword|_|) (e: Expr) : string option =
    match e.Kind with
    | Identifier name -> Some(name.ToUpperInvariant())
    | Words words -> Some(words |> List.map (fun w -> w.ToUpperInvariant()) |> String.concat " ")
    | _ -> None

/// The date and time a `DateLiteral`'s text means, if it is one DAX accepts: `2024-01-31`,
/// `2024-01-31T13:00:00` or `2024-01-31 13:00:00` (seconds optional).
let tryDate (text: string) : DateTime option =
    let formats = [|
        "yyyy-M-d"
        "yyyy-M-dTH:m"
        "yyyy-M-dTH:m:s"
        "yyyy-M-d H:m"
        "yyyy-M-d H:m:s"
    |]

    match
        DateTime.TryParseExact(
            (if isNull text then "" else text.Trim()),
            formats,
            CultureInfo.InvariantCulture,
            DateTimeStyles.None
        )
    with
    | true, d -> Some d
    | _ -> None

module Parameter =
    /// The parameter's type with DAX's defaults applied: a subtype alone means SCALAR; only a mode,
    /// or nothing at all, means ANYVAL for VAL and ANYREF for EXPR.
    let effectiveType (p: Parameter) : ParameterType =
        match p.Type, p.Subtype, p.Mode with
        | Some t, _, _ -> t
        | None, Some _, _ -> ParameterType.Scalar
        | None, None, Some ParameterMode.Expr -> ParameterType.AnyRef
        | None, None, _ -> ParameterType.AnyVal

    /// The parameter's passing mode with DAX's defaults applied: reference types are always EXPR,
    /// everything else is VAL unless EXPR was written.
    let effectiveMode (p: Parameter) : ParameterMode =
        match p.Mode, p.Type with
        | Some m, _ -> m
        | None,
          Some(ParameterType.AnyRef | ParameterType.CalendarRef | ParameterType.ColumnRef | ParameterType.MeasureRef | ParameterType.TableRef) ->
            ParameterMode.Expr
        | None, _ -> ParameterMode.Val

// ---- Walking a tree -----------------------------------------------------------------------------

/// A node without a position.
let node (kind: ExprKind) : Expr = { Kind = kind; Span = Span.none }

/// The expressions directly inside a node, in source order.
let children (e: Expr) : Expr list =
    match e.Kind with
    | IntegerLiteral _
    | NumberLiteral _
    | StringLiteral _
    | BooleanLiteral _
    | DateLiteral _
    | Identifier _
    | Words _
    | QuotedTable _
    | Qualified _
    | Bracketed _
    | QueryParameter _
    | Omitted -> []
    | Call(_, args) -> args
    | Unary(_, x) -> [ x ]
    | Binary(_, l, r) -> [ l; r ]
    | VarBlock(vars, body) -> (vars |> List.map (fun v -> v.Value)) @ [ body ]
    | TableConstructor rows -> rows
    | Tuple items -> items
    | Paren x -> [ x ]

/// Rebuild a node with each direct child replaced by `f child`.
let mapChildren (f: Expr -> Expr) (e: Expr) : Expr =
    let kind =
        match e.Kind with
        | Call(name, args) -> Call(name, List.map f args)
        | Unary(op, x) -> Unary(op, f x)
        | Binary(op, l, r) ->
            let l = f l
            Binary(op, l, f r)
        | VarBlock(vars, body) ->
            let vars = vars |> List.map (fun v -> { v with Value = f v.Value })
            VarBlock(vars, f body)
        | TableConstructor rows -> TableConstructor(List.map f rows)
        | Tuple items -> Tuple(List.map f items)
        | Paren x -> Paren(f x)
        | leaf -> leaf

    { e with Kind = kind }

/// Fold over a node and everything inside it, parents before children, in source order.
let rec fold (f: 'State -> Expr -> 'State) (state: 'State) (e: Expr) : 'State =
    RuntimeHelpers.EnsureSufficientExecutionStack()
    children e |> List.fold (fold f) (f state e)

/// A node and every node inside it, parents before children, in source order.
let descendants (e: Expr) : Expr list =
    fold (fun acc x -> x :: acc) [] e |> List.rev

/// How many levels deep a tree is (a leaf is 1). Does not recurse, so it is safe on any tree.
let depth (e: Expr) : int =
    let stack = Stack<struct (Expr * int)>()
    stack.Push(struct (e, 1))
    let mutable deepest = 0

    while stack.Count > 0 do
        let struct (x, d) = stack.Pop()

        if d > deepest then
            deepest <- d

        for c in children x do
            stack.Push(struct (c, d + 1))

    deepest

/// The same tree with every span set to `Span.none`: for comparing trees whatever their positions.
let rec stripSpans (e: Expr) : Expr =
    RuntimeHelpers.EnsureSufficientExecutionStack()

    let kind =
        match e.Kind with
        | Qualified(t, name) -> Qualified({ t with Span = Span.none }, name)
        | Call(name, args) -> Call({ name with Span = Span.none }, List.map stripSpans args)
        | VarBlock(vars, body) ->
            let vars =
                vars
                |> List.map (fun v -> {
                    Name = { v.Name with Span = Span.none }
                    Value = stripSpans v.Value
                    Span = Span.none
                })

            VarBlock(vars, stripSpans body)
        | _ -> (mapChildren stripSpans e).Kind

    { Kind = kind; Span = Span.none }

/// The same tree without its `Paren` nodes: what is left is the meaning, without the grouping the
/// source happened to spell out. `(a)` becomes `a`; a tuple stays a tuple.
let rec stripParens (e: Expr) : Expr =
    RuntimeHelpers.EnsureSufficientExecutionStack()

    match e.Kind with
    | Paren x -> stripParens x
    | _ -> mapChildren stripParens e

module FunctionDef =
    let stripSpans (f: FunctionDef) : FunctionDef = {
        Parameters =
            f.Parameters
            |> List.map (fun p -> {
                p with
                    Name = { p.Name with Span = Span.none }
                    Default = p.Default |> Option.map stripSpans
                    Span = Span.none
            })
        Body = stripSpans f.Body
        Span = Span.none
    }

module Query =
    let stripSpans (q: Query) : Query =
        let ident (i: Ident) = { i with Span = Span.none }
        let table (t: TableName) = { t with Span = Span.none }

        let definition (d: Definition) = {
            Span = Span.none
            Kind =
                match d.Kind with
                | DefinitionKind.Measure(t, n, v) -> DefinitionKind.Measure(table t, ident n, stripSpans v)
                | DefinitionKind.Column(t, n, v) -> DefinitionKind.Column(table t, ident n, stripSpans v)
                | DefinitionKind.Table(t, v) -> DefinitionKind.Table(table t, stripSpans v)
                | DefinitionKind.Var(n, v) -> DefinitionKind.Var(ident n, stripSpans v)
                | DefinitionKind.Function(n, f) -> DefinitionKind.Function(ident n, FunctionDef.stripSpans f)
                | DefinitionKind.MParameter(n, v) -> DefinitionKind.MParameter(ident n, stripSpans v)
        }

        let evaluate (e: Evaluate) = {
            Table = stripSpans e.Table
            OrderBy =
                e.OrderBy
                |> List.map (fun k -> {
                    Expr = stripSpans k.Expr
                    Direction = k.Direction
                    Span = Span.none
                })
            StartAt = e.StartAt |> List.map stripSpans
            Span = Span.none
        }

        {
            Definitions = q.Definitions |> List.map definition
            Evaluates = q.Evaluates |> List.map evaluate
            Span = Span.none
        }

// ---- Names an expression refers to --------------------------------------------------------------

[<RequireQualifiedAccess>]
type NameKind =
    /// `'Sales Table'`
    | QuotedTable
    /// A bare word that no VAR or parameter in scope declares: a table, a keyword argument, or a
    /// name defined outside the expression (a query variable).
    | Bare
    /// `Table[Name]`
    | Qualified
    /// `[Name]`
    | Bracketed
    /// The name of a function being called.
    | Function

/// A name in an expression that refers to something outside it.
type NameRef = {
    Kind: NameKind
    /// The table of a `Qualified` name.
    Table: string option
    Name: string
    /// The whole reference: for a function, its name only; for `T[Name]`, both parts.
    Span: Span
    /// For a `Qualified` name written with a bare table (`x[Name]`): `x` is a VAR or parameter in
    /// scope, so this is not a model table.
    LocalTable: bool
}

let private scopeOf (names: string seq) =
    HashSet<string>(names, StringComparer.OrdinalIgnoreCase)

let private referencesIn (scope: HashSet<string>) (e: Expr) : NameRef list =
    let out = ResizeArray<NameRef>()

    let add kind table name span local =
        out.Add {
            Kind = kind
            Table = table
            Name = name
            Span = span
            LocalTable = local
        }

    let rec walk (scope: HashSet<string>) (e: Expr) =
        RuntimeHelpers.EnsureSufficientExecutionStack()

        match e.Kind with
        | Identifier name ->
            if not (scope.Contains name) then
                add NameKind.Bare None name e.Span false
        | QuotedTable name -> add NameKind.QuotedTable None name e.Span false
        | Qualified(t, name) -> add NameKind.Qualified (Some t.Name) name e.Span (not t.Quoted && scope.Contains t.Name)
        | Bracketed name -> add NameKind.Bracketed None name e.Span false
        | Call(name, args) ->
            add NameKind.Function None name.Name name.Span false
            List.iter (walk scope) args
        | VarBlock(vars, body) ->
            let inner = HashSet<string>(scope, StringComparer.OrdinalIgnoreCase)

            for v in vars do
                walk inner v.Value
                inner.Add v.Name.Name |> ignore

            walk inner body
        | _ -> List.iter (walk scope) (children e)

    walk scope e
    List.ofSeq out

/// Every name in an expression that refers to something outside it, in source order: tables,
/// `Table[Name]`, `[Name]`, called functions, and bare words that no enclosing VAR declares
/// (keyword arguments like DESC among them: only a model can tell those from tables).
/// `declared` are names already in scope: a function's parameters, a query's variables.
let referencesWith (declared: string seq) (e: Expr) : NameRef list = referencesIn (scopeOf declared) e

/// `referencesWith` with nothing declared outside the expression.
let references (e: Expr) : NameRef list = referencesWith [] e

/// The names a function definition refers to: its body's and its parameter defaults', with the
/// parameters in scope.
let functionReferences (f: FunctionDef) : NameRef list =
    let scope = scopeOf (f.Parameters |> List.map (fun p -> p.Name.Name))

    (f.Parameters
     |> List.collect (fun p -> p.Default |> Option.map (referencesIn scope) |> Option.defaultValue []))
    @ referencesIn scope f.Body
