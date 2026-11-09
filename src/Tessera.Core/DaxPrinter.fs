/// Writes a `DaxAst` tree as minimal canonical DAX: one line per expression, one space around
/// binary operators and after separators, keywords in upper case. It is for round trips, messages
/// and generated DAX; `Format` is the formatter.
///
/// What is kept and what is not:
///   - Names, function names, bare words (keyword arguments like `desc` among them), numbers and
///     strings are written as the tree has them (a table keeps its quotes or their absence, a
///     number its digits).
///   - `Paren` nodes are written; no other parentheses are added to a tree the parser made, so
///     parse -> print -> parse gives the same tree, and the printed text has the source's tokens,
///     except that the words that are syntax come out in upper case (VAR, RETURN, NOT, IN, TRUE,
///     FALSE, parameter types, DEFINE and the other query keywords) and `dt` in lower case.
///   - A hand-built tree that lacks the `Paren` nodes its shape needs (a `+` as the operand of a
///     `*`) gets the parentheses that make the text mean the tree: printing never changes meaning.
///     Parsing that text gives the tree with those `Paren` nodes added.
///   - Comments and layout are not part of a tree, so they are gone.
///   - A call whose only argument is `Omitted` prints as `F()`: DAX has no way to write it.
///
/// Printing recurses, as everything that walks a tree does. A tree from the parser is never deeper
/// than `DaxParser.maxDepth`, which fits any default stack; a hand-built tree too deep for the
/// stack raises InsufficientExecutionStackException.
module Tessera.DaxPrinter

open System
open System.Text
open Tessera.Dax
open Tessera.DaxAst
open Tessera.DaxParser

type PrintOptions = {
    /// Separate arguments with `;` and write decimals with `,`.
    Semicolons: bool
}

module PrintOptions =
    let defaults = { Semicolons = false }

/// An operand is "closed" when no operator after it can be drawn into it.
let private closed = Int32.MaxValue

let private isBareName (s: string) =
    s.Length > 0
    && (Char.IsLetter s.[0] || s.[0] = '_')
    && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '.')
    && not (isReserved s)

let private quoteString (s: string) = "\"" + s.Replace("\"", "\"\"") + "\""

let private tableText (t: TableName) =
    if t.Quoted || not (isBareName t.Name) then
        quoteTable t.Name
    else
        t.Name

let private unaryText (op: UnaryOp) =
    match op with
    | UnaryOp.Plus -> "+"
    | UnaryOp.Minus -> "-"
    | UnaryOp.Not -> "NOT"

type private Printer(options: PrintOptions) =
    let sep = if options.Semicolons then "; " else ", "

    let number (text: string) =
        // "1." stays as it is: the lexer reads it in either dialect, and "1," would be a separator.
        if options.Semicolons && not (text.EndsWith ".") then
            text.Replace('.', ',')
        else
            text

    /// The text of an expression, and how loosely its right edge is open: the binding power of the
    /// loosest prefix operator (or VAR block) whose operand reaches the end of the text. An
    /// operator binding at least that tightly, written after the text, would join that operand.
    /// (The cases that recurse are methods of their own, to keep each level's stack frame small.)
    member p.Expr(e: Expr) : struct (string * int) =
        // A hand-built tree deeper than the stack: an exception, not a dead process.
        System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

        match e.Kind with
        | Unary(op, x) -> p.Unary(op, x)
        | Binary(op, l, r) -> p.Binary(op, l, r)
        | VarBlock(vars, body) -> p.VarBlock(vars, body)
        | Call(name, args) -> struct (p.Call(name, args), closed)
        | Paren x -> struct ("(" + p.Text x + ")", closed)
        | Tuple items -> struct ("(" + p.List items + ")", closed)
        | TableConstructor rows -> struct ("{" + p.List rows + "}", closed)
        | _ -> struct (p.Leaf e, closed)

    member private p.Leaf(e: Expr) : string =
        match e.Kind with
        | IntegerLiteral(_, text)
        | NumberLiteral(_, text) -> number text
        | StringLiteral s -> quoteString s
        | BooleanLiteral b -> if b then "TRUE" else "FALSE"
        | DateLiteral text -> "dt" + quoteString text
        | Identifier name -> name
        | Words words -> String.Join(" ", words)
        | QuotedTable name -> quoteTable name
        | Qualified(t, name) -> tableText t + bracket name
        | Bracketed name -> bracket name
        | QueryParameter name -> "@" + name
        | _ -> ""

    member private p.Call(name: Ident, args: Expr list) : string =
        match args with
        | [ { Kind = Omitted } ] -> name.Name + "()"
        | _ -> name.Name + "(" + p.List args + ")"

    member private p.Unary(op: UnaryOp, x: Expr) : struct (string * int) =
        let bp = prefixBindingPower op
        let struct (text, tail) = p.Expr x

        let text =
            match x.Kind with
            | Binary(o, _, _) when bindingPower o < bp -> "(" + text + ")"
            | _ -> text

        // "- -x": "--" would start a comment.
        let gap =
            if op = UnaryOp.Not || text.StartsWith "-" || text.StartsWith "+" then
                " "
            else
                ""

        struct (unaryText op + gap + text, min bp tail)

    member private p.Binary(op: BinaryOp, l: Expr, r: Expr) : struct (string * int) =
        let bp = bindingPower op
        let struct (left, leftTail) = p.Expr l

        let left =
            match l.Kind with
            | Binary(o, _, _) when bindingPower o < bp -> "(" + left + ")"
            | _ when leftTail <= bp -> "(" + left + ")"
            | _ -> left

        let struct (right, rightTail) = p.Expr r

        match r.Kind with
        | Binary(o, _, _) when bindingPower o <= bp -> struct (left + " " + binaryText op + " (" + right + ")", closed)
        | _ -> struct (left + " " + binaryText op + " " + right, rightTail)

    member private p.VarBlock(vars: VarDecl list, body: Expr) : struct (string * int) =
        let sb = StringBuilder()

        for v in vars do
            sb.Append("VAR ").Append(v.Name.Name).Append(" = ").Append(p.Text v.Value).Append(' ')
            |> ignore

        sb.Append("RETURN ").Append(p.Text body) |> ignore
        struct (sb.ToString(), 0)

    member p.Text(e: Expr) : string =
        let struct (text, _) = p.Expr e
        text

    member p.List(items: Expr list) : string =
        String.Join(sep, items |> List.map p.Text)

    member p.Parameter(x: Parameter) : string =
        let words =
            [
                x.Type |> Option.map parameterTypeText
                x.Subtype |> Option.map scalarTypeText
                x.Mode |> Option.map parameterModeText
            ]
            |> List.choose id

        x.Name.Name
        + (if words.IsEmpty then "" else ": " + String.Join(" ", words))
        + (match x.Default with
           | Some d -> " = " + p.Text d
           | None -> "")

    member p.Function(f: FunctionDef) : string =
        "("
        + String.Join(sep, f.Parameters |> List.map p.Parameter)
        + ") => "
        + p.Text f.Body

    member p.Definition(d: Definition) : string =
        match d.Kind with
        | DefinitionKind.Measure(t, n, v) -> "MEASURE " + tableText t + bracket n.Name + " = " + p.Text v
        | DefinitionKind.Column(t, n, v) -> "COLUMN " + tableText t + bracket n.Name + " = " + p.Text v
        | DefinitionKind.Table(t, v) -> "TABLE " + tableText t + " = " + p.Text v
        | DefinitionKind.Var(n, v) -> "VAR " + n.Name + " = " + p.Text v
        | DefinitionKind.Function(n, f) -> "FUNCTION " + n.Name + " = " + p.Function f
        | DefinitionKind.MParameter(n, v) -> "MPARAMETER " + n.Name + " = " + p.Text v

    member p.Evaluate(e: Evaluate) : string =
        let key (k: OrderKey) =
            p.Text k.Expr
            + (match k.Direction with
               | Some SortDirection.Asc -> " ASC"
               | Some SortDirection.Desc -> " DESC"
               | None -> "")

        "EVALUATE "
        + p.Text e.Table
        + (if e.OrderBy.IsEmpty then
               ""
           else
               " ORDER BY " + String.Join(sep, e.OrderBy |> List.map key))
        + (if e.StartAt.IsEmpty then
               ""
           else
               " START AT " + p.List e.StartAt)

    member p.Query(q: Query) : string =
        [
            if not q.Definitions.IsEmpty then
                "DEFINE"

                for d in q.Definitions do
                    p.Definition d

            for e in q.Evaluates do
                p.Evaluate e
        ]
        |> String.concat "\n"

/// An expression as DAX text.
let printExpressionWith (options: PrintOptions) (e: Expr) : string = Printer(options).Text e

/// `printExpressionWith`, separating arguments with `,`.
let printExpression (e: Expr) : string =
    printExpressionWith PrintOptions.defaults e

/// A function definition as DAX text: `(a: INT64, b) => body`.
let printFunctionWith (options: PrintOptions) (f: FunctionDef) : string = Printer(options).Function f

/// `printFunctionWith`, separating arguments with `,`.
let printFunction (f: FunctionDef) : string =
    printFunctionWith PrintOptions.defaults f

/// A query as DAX text: DEFINE, each definition and each EVALUATE on a line of its own.
let printQueryWith (options: PrintOptions) (q: Query) : string = Printer(options).Query q

/// `printQueryWith`, separating arguments with `,`.
let printQuery (q: Query) : string = printQueryWith PrintOptions.defaults q

let printWith (options: PrintOptions) (d: Document) : string =
    match d with
    | Document.Expression e -> printExpressionWith options e
    | Document.Function f -> printFunctionWith options f
    | Document.Query q -> printQueryWith options q

/// `printWith`, separating arguments with `,`.
let print (d: Document) : string = printWith PrintOptions.defaults d
