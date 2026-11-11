namespace Tessera.DaxEngine

// The evaluator core: names, literals, operators, variables, measure references (each an implicit
// CALCULATE), user-defined functions, and the dispatch to the function registry. Everything that
// is a DAX function lives in `Functions/`.

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Tessera
open Tessera.DaxAst

/// Variables: which are used, and what happens to the ones a result didn't need.
module Variables =

    /// Whether the code refers to the variable `name` (upper case), not counting a part where an
    /// inner variable of the same name hides it.
    let rec refersTo (name: string) (e: Expr) : bool =
        RuntimeHelpers.EnsureSufficientExecutionStack()

        match e.Kind with
        | Identifier n -> n.ToUpperInvariant() = name
        | VarBlock(vars, body) ->
            let rec go (vars: VarDecl list) =
                match vars with
                | [] -> refersTo name body
                | v :: rest -> refersTo name v.Value || (v.Name.Name.ToUpperInvariant() <> name && go rest)

            go vars
        | _ -> children e |> List.exists (refersTo name)

    let private usedCache = ConditionalWeakTable<Expr, bool array>()

    /// For each variable of a VAR block: whether the block's result, or a later variable that
    /// is itself used, refers to it.
    let used (block: Expr) (vars: VarDecl list) (body: Expr) : bool array =
        usedCache.GetValue(
            block,
            fun _ ->
                let vars = Array.ofList vars
                let flags = Array.zeroCreate<bool> vars.Length

                for i in vars.Length - 1 .. -1 .. 0 do
                    let name = vars.[i].Name.Name.ToUpperInvariant()
                    let mutable hidden = false
                    let mutable found = false

                    for j in i + 1 .. vars.Length - 1 do
                        if not hidden then
                            if flags.[j] && refersTo name vars.[j].Value then
                                found <- true

                            if vars.[j].Name.Name.ToUpperInvariant() = name then
                                hidden <- true

                    flags.[i] <- found || (not hidden && refersTo name body)

                flags
        )

    /// "A variable that is referenced in the code is always evaluated, unless the DAX engine
    /// removes the variable references through a static analysis of the code" (SQLBI, "When are
    /// variables evaluated in DAX?"). The value of the result is the same either way; what
    /// differs is an error in a variable the result didn't need, which the engine raises unless
    /// its analysis removed the variable. So: evaluate what is referenced and wasn't needed,
    /// and if that raises an error, refuse, since which of the two the engine does can't be told
    /// here.
    let settle (ctx: Ctx) (bound: (Ident * Lazy<Value> * bool) list) : unit =
        for (name, value, isUsed) in bound do
            if isUsed && not value.IsValueCreated then
                try
                    value.Force() |> ignore
                with DaxRuntimeException(text, _) ->
                    Fail.notSupported
                        ctx
                        name.Span
                        $"the variable {name.Name}, which raises an error ({text}) although the result doesn't need its value (the engine raises that error unless it can tell before running that the variable is not used; it can't be told here which)"

[<Sealed>]
type Evaluator(registry: IReadOnlyDictionary<string, DaxFunction>) as this =
    let ev = this :> IEval

    let upper (s: string) = s.ToUpperInvariant()

    // ---- Names ------------------------------------------------------------------------------------

    let tableNamed (ctx: Ctx) (span: Span) (name: string) : BTable =
        match ctx.Model.TableByName.TryGetValue name with
        | true, t ->
            match t.Gate with
            | Some why -> Fail.notSupported ctx span why
            | None ->
                match t.Failure with
                | Some e -> raise (EngineException e)
                | None -> t
        | _ -> Fail.invalid ctx span $"there is no table '{name}'"

    let tryMeasure (ctx: Ctx) (name: string) : BMeasure option =
        let fromQuery =
            if ctx.Scope.InQuery && not (isNull ctx.Query) then
                match ctx.Query.Measures.TryGetValue name with
                | true, m -> Some m
                | _ -> None
            else
                None

        match fromQuery with
        | Some m -> Some m
        | None ->
            match ctx.Model.Measures.TryGetValue name with
            | true, m -> Some m
            | _ -> None

    let tryFunction (ctx: Ctx) (name: string) : BFunction option =
        let fromQuery =
            if ctx.Scope.InQuery && not (isNull ctx.Query) then
                match ctx.Query.Functions.TryGetValue name with
                | true, f -> Some f
                | _ -> None
            else
                None

        match fromQuery with
        | Some f -> Some f
        | None ->
            match ctx.Model.Functions.TryGetValue name with
            | true, f -> Some f
            | _ -> None

    let isQueryMeasure (ctx: Ctx) (m: BMeasure) =
        not (isNull ctx.Query)
        && (match ctx.Query.Measures.TryGetValue m.Name with
            | true, q -> obj.ReferenceEquals(q, m)
            | _ -> false)

    let isQueryFunction (ctx: Ctx) (f: BFunction) =
        not (isNull ctx.Query)
        && (match ctx.Query.Functions.TryGetValue f.Name with
            | true, q -> obj.ReferenceEquals(q, f)
            | _ -> false)

    /// What `Table[Name]` names: a column or a measure.
    let qualified (ctx: Ctx) (span: Span) (table: TableName) (name: string) : Choice<BColumn, BMeasure> =
        let t = tableNamed ctx span table.Name

        match t.ColumnByName.TryGetValue name with
        | true, c ->
            match c.Failure with
            | Some e -> raise (EngineException e)
            | None -> Choice1Of2 c
        | _ ->
            match tryMeasure ctx name with
            | Some m -> Choice2Of2 m
            | None -> Fail.invalid ctx span $"table '{t.Name}' has no column or measure '{name}'"

    /// An extension column `[Name]` of a row context, innermost first; then a model column of one.
    let rowContextColumnAt (ctx: Ctx) (name: string) : (RowCtx * int) option =
        let find (extension: bool) =
            ctx.Rows
            |> List.tryPick (fun rc ->
                rc.Table.Cols
                |> Array.tryFindIndex (fun c ->
                    isNull c.Table = extension
                    && String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun i -> rc, i))

        find true |> Option.orElseWith (fun () -> find false)

    let rowContextColumn (ctx: Ctx) (name: string) : (DaxValue * ColInfo) option =
        rowContextColumnAt ctx name
        |> Option.map (fun (rc, i) -> rc.Table.Get(i, rc.Row), rc.Table.Cols.[i])

    // ---- Measures and functions -------------------------------------------------------------------

    let evalMeasure (ctx: Ctx) (span: Span) (m: BMeasure) : DaxValue =
        if ctx.Measures |> List.exists (fun x -> obj.ReferenceEquals(x, m)) then
            Fail.invalid ctx span $"circular dependency: measure [{m.Name}] refers to itself"

        if not (isNull ctx.Group) then
            // "these operations are not allowed within GROUPBY" (SQLBI, on CALCULATE and measures).
            Fail.invalid ctx span $"a measure ([{m.Name}]) can't be used inside GROUPBY"

        // A measure reference is CALCULATE ( <the measure's expression> ): context transition,
        // then the expression with no row context and none of the caller's variables.
        let moved = Context.transition ctx span

        match moved.Filter.Measures.TryGetValue(m :> obj) with
        | true, v -> v
        | _ ->
            let inner = {
                moved with
                    Scope = {
                        Vars = Map.empty
                        Source = m.Source
                        InQuery = isQueryMeasure ctx m
                    }
                    Measures = m :: ctx.Measures
                    Group = null
            }

            let v = ev.Scalar(inner, m.Expr)
            moved.Filter.Measures.[m :> obj] <- v
            v

    /// Convert a VAL argument to the type its parameter asks for ("Value types ... support implicit
    /// type casting": Microsoft Learn, "DAX user-defined functions").
    let castArgument (ctx: Ctx) (span: Span) (p: Parameter) (v: Value) : Value =
        let scalarOf (v: Value) =
            match v with
            | Value.Scalar s -> s
            | Value.Table t -> ev.ScalarOfTable(ctx, span, t)

        match Parameter.effectiveType p with
        | ParameterType.Table ->
            match v with
            | Value.Table _ -> v
            | Value.Scalar _ -> Fail.invalid ctx span $"parameter '{p.Name.Name}' needs a table"
        | ParameterType.Scalar ->
            let s = scalarOf v

            let target =
                match p.Subtype with
                | None
                | Some ScalarType.Variant -> None
                | Some ScalarType.Int64 -> Some DaxType.Integer
                | Some ScalarType.Decimal -> Some DaxType.Currency
                | Some ScalarType.Double -> Some DaxType.Decimal
                | Some ScalarType.String -> Some DaxType.String
                | Some ScalarType.DateTime -> Some DaxType.DateTime
                | Some ScalarType.Boolean -> Some DaxType.Boolean
                | Some ScalarType.Numeric ->
                    match DaxValue.typeOf s with
                    | None
                    | Some DaxType.Integer
                    | Some DaxType.Decimal
                    | Some DaxType.Currency -> None
                    | Some _ ->
                        Fail.notSupported
                            ctx
                            span
                            $"a value that is not a number for the NUMERIC parameter '{p.Name.Name}'"

            match target with
            | None -> Value.Scalar s
            | Some t -> Value.Scalar(Fail.dax ctx span (DaxValue.convertTo t s))
        | _ -> v

    /// The context a function's body runs in, with its parameters bound.
    /// `evaluate`: false to learn about the call without evaluating its VAL arguments.
    let bindCallWith (evaluate: bool) (ctx: Ctx) (call: Call) (f: BFunction) : Ctx * Expr =
        if ctx.Functions |> List.exists (fun x -> obj.ReferenceEquals(x, f)) then
            Fail.invalid ctx call.Span $"function {f.Name} calls itself: DAX functions can't be recursive"

        let def = f.Def
        let args = Array.ofList call.Args

        if args.Length > def.Parameters.Length then
            Fail.invalid ctx call.Span $"{f.Name} takes {def.Parameters.Length} arguments, {args.Length} were given"

        let definedIn = {
            Vars = Map.empty
            Source = f.Source
            InQuery = isQueryFunction ctx f
        }

        let mutable vars = Map.empty

        def.Parameters
        |> List.iteri (fun i p ->
            let given =
                if i < args.Length && not (Fn.isOmitted args.[i]) then
                    Some args.[i]
                else
                    None

            // An argument is the caller's code; a default is the function's own.
            let expr, scope =
                match given, p.Default with
                | Some a, _ -> a, ctx.Scope
                | None, Some d -> d, definedIn
                | None, None -> Fail.invalid ctx call.Span $"{f.Name}: no argument for parameter '{p.Name.Name}'"

            let binding =
                match Parameter.effectiveMode p with
                | ParameterMode.Expr -> ExprParam(expr, scope)
                | ParameterMode.Val ->
                    // "evaluated once before invoking the function", in the caller's context.
                    let at = { ctx with Scope = scope }

                    let isTable =
                        match Parameter.effectiveType p with
                        | ParameterType.Table -> true
                        | ParameterType.Scalar -> false
                        | _ -> ev.IsTable(at, expr)

                    let value = lazy (castArgument at expr.Span p (ev.Value(at, expr)))

                    if evaluate then
                        value.Force() |> ignore

                    let staticType =
                        lazy
                            (if not value.IsValueCreated then
                                 StaticType.Unknown
                             else
                                 match value.Value with
                                 | Value.Scalar s ->
                                     match DaxValue.typeOf s with
                                     | Some t -> StaticType.Known t
                                     | None -> StaticType.Blank
                                 | _ -> StaticType.Unknown)

                    Val(value, isTable, staticType)

            vars <- vars.Add(upper p.Name.Name, binding))

        // "An expr parameter is evaluated lazily within the function body, so it inherits only the
        // filter context" (Microsoft Learn, "DAX user-defined functions"): the body has the
        // caller's filter context and no row context.
        let body = {
            ctx with
                Rows = []
                Scope = { definedIn with Vars = vars }
                Functions = f :: ctx.Functions
                Group = null
                CallerRows = ctx.CallerRows || not ctx.Rows.IsEmpty
        }

        body, def.Body

    let bindCall (ctx: Ctx) (call: Call) (f: BFunction) : Ctx * Expr = bindCallWith true ctx call f

    let checkCall (ctx: Ctx) (call: Call) (fn: DaxFunction) =
        let n = call.Args.Length

        if n < fn.MinArgs || (fn.MaxArgs >= 0 && n > fn.MaxArgs) then
            let expected =
                if fn.MaxArgs = fn.MinArgs then $"{fn.MinArgs}"
                elif fn.MaxArgs < 0 then $"at least {fn.MinArgs}"
                else $"{fn.MinArgs} to {fn.MaxArgs}"

            Fail.invalid ctx call.Span $"{fn.Name} takes {expected} arguments, {n} were given"

        match fn.Check call.Args with
        | Some why -> Fail.notSupported ctx call.Span why
        | None -> ()

    let unknownFunction (ctx: Ctx) (call: Call) : 'a =
        Fail.notSupported ctx call.Name.Span $"the function {upper call.Name.Name}"

    let bindVarsWith (ctx: Ctx) (vars: VarDecl list) : Ctx * (Ident * Lazy<Value>) list =
        let mutable c = ctx
        let bound = ResizeArray<Ident * Lazy<Value>>()

        for v in vars do
            // "Names of existing tables aren't allowed" (Microsoft Learn, VAR); "The variable name
            // cannot be a table name" (dax.guide/st/var).
            if ctx.Model.TableByName.ContainsKey v.Name.Name then
                Fail.invalid
                    ctx
                    v.Name.Span
                    $"a variable can't be named {v.Name.Name}: that is the name of a table of the model"

            // A variable is evaluated in the context where it is defined, on first use, once.
            let definedIn = c
            let isTable = ev.IsTable(definedIn, v.Value)
            let value = lazy (ev.Value(definedIn, v.Value))

            let staticType =
                lazy
                    (if isTable then
                         StaticType.Unknown
                     else
                         ev.TypeOf(definedIn, v.Value))

            bound.Add((v.Name, value))

            c <- {
                c with
                    Scope = {
                        c.Scope with
                            Vars = c.Scope.Vars.Add(upper v.Name.Name, Val(value, isTable, staticType))
                    }
            }

        c, List.ofSeq bound

    let bindVars (ctx: Ctx) (vars: VarDecl list) : Ctx = bindVarsWith ctx vars |> fst

    /// A VAR block's result, then the variables it referred to and didn't need (see
    /// `Variables.settle`).
    let varBlock (ctx: Ctx) (block: Expr) (vars: VarDecl list) (body: Expr) (eval: Ctx -> 'a) : 'a =
        let inner, bound = bindVarsWith ctx vars
        let result = eval inner
        let used = Variables.used block vars body
        Variables.settle ctx (bound |> List.mapi (fun i (name, value) -> name, value, used.[i]))
        result

    let tableConstructor (ctx: Ctx) (span: Span) (rows: Expr list) : TableVal =
        let rows =
            rows
            |> List.map (fun r ->
                match r.Kind with
                | Tuple items -> items |> List.map (fun i -> ev.Scalar(ctx, i)) |> Array.ofList
                | TableConstructor _ ->
                    Fail.invalid ctx r.Span "a row of a table constructor is a value or (value, ...)"
                | _ -> [| ev.Scalar(ctx, r) |])
            |> Array.ofList

        let width = rows.[0].Length

        if rows |> Array.exists (fun r -> r.Length <> width) then
            Fail.invalid ctx span "every row of a table constructor must have the same number of values"

        let data = Array.init width (fun c -> rows |> Array.map (fun r -> r.[c]))

        let cols =
            data
            |> Array.mapi (fun c values ->
                // "all values are converted to a common data type" (Microsoft Learn, "Table
                // constructor"); which type a mix of text and numbers becomes isn't documented.
                match Typing.ofValues values with
                | Some DaxType.Variant ->
                    Fail.notSupported ctx span "a table constructor column with values of different data types"
                | t ->
                    let name = if width = 1 then "Value" else $"Value{c + 1}"

                    Tables.extension name (t |> Option.defaultValue DaxType.Variant),
                    (match t with
                     | Some target -> values |> Array.map (Typing.widen target)
                     | None -> values))

        Tables.ofColumns (cols |> Array.map fst) (cols |> Array.map snd) rows.Length

    /// `value IN table`, `(a, b) IN table`: strict comparison ("Unlike the = operator, the IN
    /// operator and the CONTAINSROW function perform strict comparison": Microsoft Learn, IN).
    let contains (ctx: Ctx) (span: Span) (values: DaxValue array) (table: TableVal) : bool =
        if table.Cols.Length <> values.Length then
            Fail.invalid
                ctx
                span
                $"IN compares {values.Length} value(s) with a table of {table.Cols.Length} column(s): the counts must match"

        let data = Array.init values.Length table.Column
        let mutable found = false
        let mutable row = 0

        while not found && row < table.Count do
            let mutable same = true
            let mutable c = 0

            while same && c < values.Length do
                same <- Fail.dax ctx span (DaxValue.strictEquals ctx.Model.Collation values.[c] data.[c].[row])
                c <- c + 1

            found <- same
            row <- row + 1

        found

    let binary (ctx: Ctx) (op: BinaryOp) (l: DaxValue) (right: Expr) : DaxValue =
        let span = right.Span
        let coll = ctx.Model.Collation

        match op with
        | BinaryOp.And
        | BinaryOp.Or -> Logic.connective ev ctx (op = BinaryOp.And) span l right
        | BinaryOp.In -> DaxValue.Boolean(contains ctx span [| l |] (ev.Table(ctx, right)))
        | _ ->
            let r = ev.Scalar(ctx, right)

            match op with
            | BinaryOp.Add
            | BinaryOp.Subtract
            | BinaryOp.Multiply
            | BinaryOp.Divide
            | BinaryOp.Power ->
                Convert.checkOperand ctx span l
                Convert.checkOperand ctx span r
            | _ -> ()

            match op with
            // A date times a number follows the documentation's multiplication table where it
            // settles the result; `DaxValue.multiply` refuses the rest (a date on the left, a whole
            // number times a date with a time of day).
            | BinaryOp.Add -> Fail.dax ctx span (DaxValue.add l r)
            | BinaryOp.Subtract -> Fail.dax ctx span (DaxValue.subtract l r)
            | BinaryOp.Multiply -> Fail.dax ctx span (DaxValue.multiply l r)
            | BinaryOp.Divide -> Fail.dax ctx span (DaxValue.divide l r)
            | BinaryOp.Power -> Fail.dax ctx span (DaxValue.power l r)
            | BinaryOp.Concat ->
                // "With two arguments it works as the CONCATENATE function" (dax.guide, & operator),
                // and for CONCATENATE "If both arguments are BLANK, the result is BLANK".
                if DaxValue.isBlank l && DaxValue.isBlank r then
                    DaxValue.Blank
                else
                    DaxValue.String(Convert.toText ctx span l + Convert.toText ctx span r)
            | BinaryOp.Equal -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.equals coll l r))
            | BinaryOp.StrictEqual -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.strictEquals coll l r))
            | BinaryOp.NotEqual -> DaxValue.Boolean(not (Fail.dax ctx span (DaxValue.equals coll l r)))
            | BinaryOp.Less -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.compare coll l r) < 0)
            | BinaryOp.LessOrEqual -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.compare coll l r) <= 0)
            | BinaryOp.Greater -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.compare coll l r) > 0)
            | BinaryOp.GreaterOrEqual -> DaxValue.Boolean(Fail.dax ctx span (DaxValue.compare coll l r) >= 0)
            | _ -> Fail.invalid ctx span "unexpected operator"

    member private this.ScalarOfValue(ctx: Ctx, span: Span, v: Value) : DaxValue =
        match v with
        | Value.Scalar s -> s
        | Value.Table t -> ev.ScalarOfTable(ctx, span, t)

    interface IEval with

        member _.ScalarOfTable(ctx, span, t) =
            if t.Cols.Length <> 1 then
                Fail.invalid ctx span "a table of several columns was given where a single value is needed"
            elif t.Count = 0 then
                DaxValue.Blank
            elif t.Count = 1 then
                t.Get(0, 0)
            else
                Fail.runtime ctx span "A table of multiple values was supplied where a single value was expected."

        member this.Scalar(ctx, e) =
            RuntimeHelpers.EnsureSufficientExecutionStack()

            match e.Kind with
            | IntegerLiteral(i, _) -> DaxValue.Integer i
            | NumberLiteral(d, _) -> DaxValue.Decimal d
            | StringLiteral s -> DaxValue.String s
            | BooleanLiteral b -> DaxValue.Boolean b
            | DateLiteral text ->
                match tryDate text with
                | Some d -> DaxValue.ofDateTime d
                | None -> Fail.invalid ctx e.Span $"dt\"{text}\" is not a date"
            | Paren inner -> ev.Scalar(ctx, inner)
            | Identifier name ->
                match ctx.Scope.Vars.TryFind(upper name) with
                | Some(Val(value, _, _)) -> this.ScalarOfValue(ctx, e.Span, value.Value)
                | Some(ExprParam(expr, scope)) -> ev.Scalar({ ctx with Scope = scope }, expr)
                | None ->
                    if ctx.Model.TableByName.ContainsKey name then
                        ev.ScalarOfTable(ctx, e.Span, ev.Table(ctx, e))
                    else
                        Fail.invalid ctx e.Span $"'{name}' is not a variable, a parameter or a table"
            | QuotedTable _
            | TableConstructor _ -> ev.ScalarOfTable(ctx, e.Span, ev.Table(ctx, e))
            | Qualified(table, name) ->
                match qualified ctx e.Span table name with
                | Choice1Of2 c ->
                    match Context.tryColumnValue ctx c 0 with
                    | ValueSome v -> v
                    | ValueNone ->
                        if ctx.CallerRows then
                            Fail.notSupported
                                ctx
                                e.Span
                                $"column '{c.Table.Name}'[{c.Name}] inside a function that was called from a row context (whether the function sees the caller's row context is not documented)"

                        Fail.invalid
                            ctx
                            e.Span
                            $"a single value for column '{c.Table.Name}'[{c.Name}] cannot be determined: there is no row context for it"
                | Choice2Of2 m -> evalMeasure ctx e.Span m
            | Bracketed name ->
                match rowContextColumn ctx name, tryMeasure ctx name with
                | Some _, Some _ ->
                    Fail.notSupported ctx e.Span $"[{name}], which is both a column of the row context and a measure"
                | Some(v, _), None -> v
                | None, Some m -> evalMeasure ctx e.Span m
                | None, None ->
                    if ctx.CallerRows then
                        Fail.notSupported
                            ctx
                            e.Span
                            $"[{name}] inside a function that was called from a row context (whether the function sees the caller's row context is not documented)"

                    Fail.invalid ctx e.Span $"there is no measure or column [{name}] here"
            | QueryParameter name -> Fail.notSupported ctx e.Span $"the query parameter @{name}"
            | Omitted -> Fail.invalid ctx e.Span "an argument is missing"
            | Words _ -> Fail.invalid ctx e.Span "keywords where a value is needed"
            | Tuple _ ->
                Fail.invalid ctx e.Span "a (value, ...) row is only allowed in a table constructor or before IN"
            | Unary(op, operand) ->
                let v = ev.Scalar(ctx, operand)

                match op with
                | UnaryOp.Plus -> v
                | UnaryOp.Minus -> Fail.dax ctx e.Span (DaxValue.negate v)
                | UnaryOp.Not -> Fail.dax ctx e.Span (DaxValue.logicalNot v)
            | Binary _ ->
                // A chain of operators is a loop, not a recursion: 499 of them cost no stack.
                let first, rest = leftSpine e

                let mutable acc, rest =
                    match first.Kind, rest with
                    | Tuple items, (BinaryOp.In, right) :: tail ->
                        let values = items |> List.map (fun i -> ev.Scalar(ctx, i)) |> Array.ofList
                        DaxValue.Boolean(contains ctx right.Span values (ev.Table(ctx, right))), tail
                    | _ -> ev.Scalar(ctx, first), rest

                for (op, right) in rest do
                    acc <- binary ctx op acc right

                acc
            | VarBlock(vars, body) -> varBlock ctx e vars body (fun inner -> ev.Scalar(inner, body))
            | Call(name, args) ->
                let call = {
                    Name = name
                    Args = args
                    Span = e.Span
                }

                match registry.TryGetValue(upper name.Name) with
                | true, fn ->
                    checkCall ctx call fn
                    fn.Scalar ev ctx call
                | _ ->
                    match tryFunction ctx name.Name with
                    | Some f ->
                        let body, expr = bindCall ctx call f
                        ev.Scalar(body, expr)
                    | None -> unknownFunction ctx call

        member this.Table(ctx, e) =
            RuntimeHelpers.EnsureSufficientExecutionStack()

            let modelTable (name: string) =
                let t = tableNamed ctx e.Span name
                Tables.ofBase ctx.Filter.Config t (Filters.visibleStored ctx.Filter t)

            match e.Kind with
            | Paren inner -> ev.Table(ctx, inner)
            | QuotedTable name -> modelTable name
            | Identifier name ->
                match ctx.Scope.Vars.TryFind(upper name) with
                | Some(Val(value, _, _)) ->
                    match value.Value with
                    | Value.Table t -> t
                    | Value.Scalar _ -> Fail.invalid ctx e.Span $"'{name}' is a value, where a table is needed"
                | Some(ExprParam(expr, scope)) -> ev.Table({ ctx with Scope = scope }, expr)
                | None -> modelTable name
            | TableConstructor rows -> tableConstructor ctx e.Span rows
            | VarBlock(vars, body) -> varBlock ctx e vars body (fun inner -> ev.Table(inner, body))
            | Call(name, args) ->
                let call = {
                    Name = name
                    Args = args
                    Span = e.Span
                }

                match registry.TryGetValue(upper name.Name) with
                | true, fn ->
                    checkCall ctx call fn
                    fn.Table ev ctx call
                | _ ->
                    match tryFunction ctx name.Name with
                    | Some f ->
                        let body, expr = bindCall ctx call f
                        ev.Table(body, expr)
                    | None -> unknownFunction ctx call
            | _ -> Fail.invalid ctx e.Span "a table is needed here, not a value"

        member this.IsTable(ctx, e) =
            match e.Kind with
            | Paren inner -> ev.IsTable(ctx, inner)
            | QuotedTable _
            | TableConstructor _ -> true
            | Identifier name ->
                match ctx.Scope.Vars.TryFind(upper name) with
                | Some(Val(_, isTable, _)) -> isTable
                | Some(ExprParam(expr, scope)) -> ev.IsTable({ ctx with Scope = scope }, expr)
                | None -> ctx.Model.TableByName.ContainsKey name
            | VarBlock(vars, body) -> ev.IsTable(bindVars ctx vars, body)
            | Call(name, args) ->
                match registry.TryGetValue(upper name.Name) with
                | true, fn -> fn.Returns = Returns.Table
                | _ ->
                    match tryFunction ctx name.Name with
                    | Some f ->
                        let body, expr =
                            bindCallWith
                                false
                                ctx
                                {
                                    Name = name
                                    Args = args
                                    Span = e.Span
                                }
                                f

                        ev.IsTable(body, expr)
                    | None -> false
            | _ -> false

        member this.Value(ctx, e) =
            if ev.IsTable(ctx, e) then
                Value.Table(ev.Table(ctx, e))
            else
                Value.Scalar(ev.Scalar(ctx, e))

        member this.TryModelTable(ctx, e) =
            let e = Fn.unparen e

            match e.Kind with
            | QuotedTable name -> Some(tableNamed ctx e.Span name)
            | Identifier name when not (ctx.Scope.Vars.ContainsKey(upper name)) ->
                if ctx.Model.TableByName.ContainsKey name then
                    Some(tableNamed ctx e.Span name)
                else
                    None
            | _ -> None

        member this.TryColumn(ctx, e) =
            let e = Fn.unparen e

            match e.Kind with
            | Qualified(table, name) ->
                match qualified ctx e.Span table name with
                | Choice1Of2 c -> Some c
                | Choice2Of2 _ -> None
            | Bracketed name ->
                // An unqualified column: a model column of a row context.
                match tryMeasure ctx name with
                | Some _ -> None
                | None ->
                    match rowContextColumnAt ctx name with
                    | Some(rc, i) when not (isNull rc.Table.Cols.[i].Lineage) && not (isNull rc.Table.Cols.[i].Table) ->
                        Some rc.Table.Cols.[i].Lineage
                    | _ -> None
            | Identifier name ->
                match ctx.Scope.Vars.TryFind(upper name) with
                | Some(ExprParam(expr, scope)) -> ev.TryColumn({ ctx with Scope = scope }, expr)
                | _ -> None
            | _ -> None

        member this.Column(ctx, e) =
            match ev.TryColumn(ctx, e) with
            | Some c -> c
            | None -> Fail.invalid ctx e.Span "a column reference (Table[Column]) is needed here"

        member this.TypeOf(ctx, e) =
            RuntimeHelpers.EnsureSufficientExecutionStack()
            let unknown = StaticType.Unknown

            let ofDeclared (t: DaxType) = Typing.ofColumn t

            match e.Kind with
            | IntegerLiteral _ -> StaticType.Known DaxType.Integer
            | NumberLiteral _ -> StaticType.Known DaxType.Decimal
            | StringLiteral _ -> StaticType.Known DaxType.String
            | BooleanLiteral _ -> StaticType.Known DaxType.Boolean
            | DateLiteral _ -> StaticType.Known DaxType.DateTime
            | Paren inner -> ev.TypeOf(ctx, inner)
            | Identifier name ->
                match ctx.Scope.Vars.TryFind(upper name) with
                | Some(Val(_, false, t)) -> t.Value
                | Some(ExprParam(expr, scope)) -> ev.TypeOf({ ctx with Scope = scope }, expr)
                | _ -> unknown
            | Qualified(table, name) ->
                match ctx.Model.TableByName.TryGetValue table.Name with
                | true, t ->
                    match t.ColumnByName.TryGetValue name with
                    | true, c -> ofDeclared c.DataType
                    | _ ->
                        match tryMeasure ctx name with
                        | Some m -> this.TypeOfMeasure(ctx, m)
                        | None -> unknown
                | _ -> unknown
            | Bracketed name ->
                match rowContextColumnAt ctx name, tryMeasure ctx name with
                | Some(rc, i), None -> ofDeclared rc.Table.Cols.[i].Type
                | None, Some m -> this.TypeOfMeasure(ctx, m)
                | _ -> unknown
            | Unary(UnaryOp.Not, _) -> StaticType.Known DaxType.Boolean
            | Unary(_, operand) ->
                match ev.TypeOf(ctx, operand) with
                | StaticType.Known DaxType.Boolean -> StaticType.Known DaxType.Integer
                | StaticType.Known DaxType.String -> unknown
                | t -> t
            | Binary _ ->
                let first, rest = leftSpine e

                let start =
                    match first.Kind with
                    | Tuple _ -> StaticType.Known DaxType.Boolean
                    | _ -> ev.TypeOf(ctx, first)

                rest
                |> List.fold
                    (fun acc (op, right) ->
                        match op with
                        | BinaryOp.In -> StaticType.Known DaxType.Boolean
                        | _ -> Typing.binary op acc (ev.TypeOf(ctx, right)))
                    start
            | VarBlock(vars, body) -> ev.TypeOf(bindVars ctx vars, body)
            | Call(name, args) ->
                match registry.TryGetValue(upper name.Name) with
                | true, fn when fn.Returns = Returns.Scalar -> fn.Type ev ctx args
                | _ -> unknown
            | _ -> unknown

    member private this.TypeOfMeasure(ctx: Ctx, m: BMeasure) : StaticType =
        // Worked out once per measure: a measure that refers to others several times would
        // otherwise be walked again for each reference, and again inside each of those.
        let fromQuery = isQueryMeasure ctx m

        let cached =
            if fromQuery then
                match ctx.Query.MeasureTypes.TryGetValue m with
                | true, t -> Some t
                | _ -> None
            else
                match ctx.Model.MeasureTypes.TryGetValue m with
                | true, t -> Some t
                | _ -> None

        match cached with
        | Some t -> t
        | None when ctx.Measures |> List.exists (fun x -> obj.ReferenceEquals(x, m)) ->
            // A measure that refers to itself: evaluating it is the error.
            StaticType.Unknown
        | None ->
            let inner = {
                ctx with
                    Rows = []
                    Scope = {
                        Vars = Map.empty
                        Source = m.Source
                        InQuery = fromQuery
                    }
                    Measures = m :: ctx.Measures
                    Group = null
                    CallerRows = false
            }

            let t =
                try
                    ev.TypeOf(inner, m.Expr)
                with EngineException _ ->
                    StaticType.Unknown

            if fromQuery then
                ctx.Query.MeasureTypes.[m] <- t
            else
                ctx.Model.MeasureTypes.[m] <- t

            t
