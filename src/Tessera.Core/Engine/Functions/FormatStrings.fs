namespace Tessera.DaxEngine.Functions

// Format strings: a measure's static format string, its dynamic format string (the
// `formatStringDefinition` of a measure, compatibility level 1601 and up), a calculation item's
// format string expression, SELECTEDMEASUREFORMATSTRING, and the format string a query result's cell
// has (what `tess query --formatted` shows).
//
// The rules, with where they come from:
//
// - A dynamic format string is "a DAX expression that outputs the correct format string for your
//   measure" (Learn, "Create dynamic format strings for measures"). "To obtain the correct format
//   string for every row, Tabular uses an internal measure for every measure with a dynamic format
//   string" named `_<measure> FormatString`, which a query reaches as `Table[_<measure> FormatString]`
//   ("The format string reference must include the table name": SQLBI, "Introducing dynamic format
//   strings for DAX measures"). So it is evaluated as a measure is, where the measure is, and
//   `'Sales'[_Sales Amount FormatString]` works in a query. "The internal measure exists only for
//   those with a dynamic format string."
// - In it, "You can reference the measure itself in its dynamic format string directly by using its
//   name, such as [Measure A], or indirectly by using SELECTEDMEASURE()" (Learn); "the
//   SELECTEDMEASURE function always invokes the original measure" (SQLBI). "The same DAX patterns
//   that work with calculation groups also work in dynamic format strings for measures" (Learn), so
//   SELECTEDMEASURENAME and ISSELECTEDMEASURE work there too.
// - "Query measures do not have all the TOM properties; they only define the DAX code" (SQLBI): a
//   DEFINE MEASURE has no format string.
// - A calculation item's format string expression "must return a scalar string" (Learn,
//   "Calculation groups"), and "The calculation group with the highest precedence order is applied
//   first. This is valid for both the Expression and the Format String Expression properties"
//   (SQLBI, "Controlling format strings in calculation groups"). "The highest precedence calculation
//   group dynamic format string is the only one applied. If a measure itself has a dynamic format
//   string, it's considered as a lower precedence to any calculation group in the model" (Learn). An
//   item with no format string expression leaves the measure's ("all the calculation items except
//   YOY% should use the format of the current measure in context": Learn).
// - SELECTEDMEASUREFORMATSTRING is "the format string of the measure that is currently in context
//   when the calculation item is evaluated" (Learn), and "could be the one originally defined by the
//   measure or the one already modified by another calculation item" (SQLBI).
//
// Refused, because the sources leave it open:
//   - two items with format string expressions that could both be applied to one measure: SQLBI
//     has SELECTEDMEASUREFORMATSTRING in the lower one see the higher one's ("This format string
//     may already have been defined by another calculation item with higher precedence"), while
//     Learn has the highest one "the only one applied", and nests the lower item inside it;
//   - an item with no format string expression above one that has one (where the lower item's
//     format string expression is evaluated, inside the higher item's expression, is not said);
//   - a measure's dynamic format string (other than a text constant) under an item with no format
//     string expression: it is "a lower precedence to any calculation group" (Learn), and where it
//     is evaluated inside the item's expression is not said either;
//   - a measure reference, SELECTEDMEASURE or a user-defined function in a format string evaluated
//     under a calculation item (whether the item applies to them there is not said), and the
//     internal `_<measure> FormatString` measure in a model with calculation groups (whether items
//     apply to it);
//   - a format string expression that returns BLANK, an empty text or something that is not text;
//   - SELECTEDMEASUREFORMATSTRING for a measure with no format string, and inside a measure's own
//     dynamic format string;
//   - the format string of a cell where several items selected make the measure BLANK
//     (selectionExpressionBehavior Visual).

open System
open System.Collections.Concurrent
open System.Runtime.CompilerServices
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module FormatStrings =

    /// The function a query result's cell format string is computed with (`--formatted`): its
    /// argument is the measure reference of the column. Its name has spaces, so DAX can't call it.
    let probeName = "FORMAT STRING OF"

    /// The body of the internal `_<measure> FormatString` measure: the measure's own format string
    /// where it is evaluated. Its name has spaces, so DAX can't call it.
    let ownName = "OWN FORMAT STRING OF"

    /// The family the two internal functions are registered under (`Engine.functions` leaves it out).
    let internalFamily = "Internal"

    let private parsed = ConcurrentDictionary<SourceText, Lazy<Expr>>()

    /// A format string expression, parsed once.
    let parse (source: SourceText) : Expr =
        let lazyExpr =
            parsed.GetOrAdd(
                source,
                fun s ->
                    lazy
                        (match DaxParser.parseExpression s.Text with
                         | Ok e -> e
                         | Error(e :: _) -> raise (EngineException(DaxSyntax(s.Label, e.Line, e.Column, e.Message)))
                         | Error [] -> raise (EngineException(DaxSyntax(s.Label, 1, 1, "the DAX can't be parsed"))))
            )

        lazyExpr.Value

    /// What a measure's own dynamic format string is evaluated as: an "item" of no group, so that
    /// SELECTEDMEASURE () in it is the measure (`CalculationGroups.current`).
    let private ownGroup = BCalcGroup("", null, Int32.MinValue)

    // Weak: a bound model (one per DEFINE COLUMN / TABLE query) must not live as long as the process.
    let private ownItems = ConditionalWeakTable<BMeasure, BCalcItem>()

    let private ownItem (m: BMeasure) (source: SourceText) : BCalcItem =
        ownItems.GetValue(m, fun m -> BCalcItem(ownGroup, m.Name, 0, -1, source, lazy (parse source)))

    /// Whether this is a measure's own dynamic format string being evaluated, not a calculation item.
    let isOwn (app: ItemApplication) =
        obj.ReferenceEquals(app.Item.Group, ownGroup)

    /// The measure a measure reference names, as code of the model or of the query sees it.
    let private measureNamed (ctx: Ctx) (e: Expr) : BMeasure option =
        let find (name: string) =
            let fromQuery =
                if ctx.Scope.InQuery && not (isNull ctx.Query) then
                    match ctx.Query.Measures.TryGetValue name with
                    | true, m -> Some m
                    | _ -> None
                else
                    None

            fromQuery
            |> Option.orElseWith (fun () ->
                match ctx.Model.Measures.TryGetValue name with
                | true, m -> Some m
                | _ -> None)

        match (Fn.unparen e).Kind with
        | Bracketed name -> find name
        | Qualified(table, name) ->
            match ctx.Model.TableByName.TryGetValue table.Name with
            | true, t when not (t.ColumnByName.ContainsKey name) -> find name
            | _ -> None
        | _ -> None

    /// The first thing in a format string expression that evaluates a measure (a measure reference,
    /// SELECTEDMEASURE (), a user-defined function, which may do either), if any.
    let private evaluatesMeasure (ctx: Ctx) (e: Expr) : string option =
        DaxAst.descendants e
        |> List.tryPick (fun x ->
            match x.Kind with
            | Call(name, _) when String.Equals(name.Name, "SELECTEDMEASURE", StringComparison.OrdinalIgnoreCase) ->
                Some "SELECTEDMEASURE ()"
            | Call(name, _) when ctx.Model.Functions.ContainsKey name.Name -> Some $"the function {name.Name}"
            | Bracketed name when ctx.Model.Measures.ContainsKey name -> Some $"the measure [{name}]"
            | Qualified(table, name) ->
                match ctx.Model.TableByName.TryGetValue table.Name with
                | true, t when not (t.ColumnByName.ContainsKey name) && ctx.Model.Measures.ContainsKey name ->
                    Some $"the measure [{name}]"
                | _ -> None
            | _ -> None)

    /// The text a format string expression returned, or a refusal.
    let private formatStringValue (ctx: Ctx) (span: Span) (what: string) (v: DaxValue) : string =
        match v with
        | DaxValue.String s when s <> "" -> s
        | DaxValue.String _ -> Fail.notSupported ctx span $"{what} that returns an empty text"
        | DaxValue.Blank -> Fail.notSupported ctx span $"{what} that returns BLANK"
        | _ ->
            Fail.notSupported
                ctx
                span
                $"{what} that returns a value that is not text (\"The format string expression must return a scalar string\": Learn)"

    /// The measure's own format string in `ctx` (already after the reference's context transition):
    /// its dynamic format string evaluated there, or its static one; None when it has neither.
    /// `underItem`: a calculation item is being applied to the measure.
    let own (ev: IEval) (ctx: Ctx) (span: Span) (m: BMeasure) (underItem: string option) : string option =
        match m.FormatStringSource with
        | Some source ->
            // Evaluated as the internal measure is: after a context transition.
            let ctx = Context.transition ctx span
            let item = ownItem m source

            match underItem with
            | Some itemName ->
                match evaluatesMeasure ctx item.Expr with
                | Some what ->
                    Fail.notSupported
                        ctx
                        span
                        $"the dynamic format string of [{m.Name}] under the calculation item {itemName}: it evaluates {what}, and whether the item applies there is not documented"
                | None -> ()
            | None -> ()

            let app = {
                Item = item
                Measure = m
                GroupFilters = []
                Functions = ctx.Functions
            }

            let inner = {
                ctx with
                    Rows = []
                    Scope = {
                        Vars = Map.empty
                        Source = source
                        InQuery = false
                    }
                    Group = null
                    Items = { ctx.Items with Current = Some app }
            }

            Some(formatStringValue ctx span $"the dynamic format string of [{m.Name}]" (ev.Scalar(inner, item.Expr)))
        | None ->
            if String.IsNullOrEmpty m.FormatString then
                None
            else
                Some m.FormatString

    let private itemName (item: BCalcItem) = $"'{item.Group.Name}'[{item.Name}]"

    /// The groups of lower precedence than `g` (all of them for None) with an item that has a format
    /// string expression and is the one item visible in `ctx`'s filter context: it could be applied.
    let private selectableBelow (ctx: Ctx) (g: BCalcGroup) : BCalcItem option =
        let groups = ctx.Model.CalcGroups
        let at = groups |> Array.findIndex (fun x -> obj.ReferenceEquals(x, g))

        groups
        |> Array.skip (at + 1)
        |> Array.tryPick (fun lower ->
            if (CalculationItems.groupFilters ctx.Filter lower).IsEmpty then
                None
            else
                let visible = Filters.visible ctx.Filter lower.Table

                if visible.Count = 1 then
                    let item = lower.Items.[visible.Rows.[0]]

                    if item.FormatStringExpression.IsSome then
                        Some item
                    else
                        None
                else
                    None)

    /// A lower group with format string expressions that the item's expression mentions (it may
    /// select one of its items) or a function it calls (which may).
    let private mentionsBelow (ctx: Ctx) (item: BCalcItem) : string option =
        let groups = ctx.Model.CalcGroups
        let at = groups |> Array.findIndex (fun x -> obj.ReferenceEquals(x, item.Group))

        let lower =
            groups
            |> Array.skip (at + 1)
            |> Array.filter (fun g -> g.Items |> Array.exists _.FormatStringExpression.IsSome)

        if lower.Length = 0 then
            None
        else
            let named (table: string) =
                lower
                |> Array.tryFind (fun g -> String.Equals(g.Table.Name, table, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun g -> $"the calculation group '{g.Name}'")

            DaxAst.descendants item.Expr
            |> List.tryPick (fun x ->
                match x.Kind with
                | Qualified(t, _) -> named t.Name
                | QuotedTable t
                | Identifier t -> named t
                | Call(name, _) when ctx.Model.Functions.ContainsKey name.Name -> Some $"the function {name.Name}"
                | _ -> None)

    /// The format string of a reference to `m` in `ctx`: what a query result's cell holding the
    /// measure is formatted with. None when it has none.
    let atReference (ev: IEval) (ctx: Ctx) (span: Span) (m: BMeasure) : string option =
        if ctx.Items.Current.IsSome || not ctx.Items.Applied.IsEmpty then
            Fail.notSupported ctx span $"the format string of [{m.Name}] inside a calculation item"

        let internalName = $"_{m.Name} FormatString"

        if not (isNull ctx.Query) && ctx.Query.Measures.ContainsKey internalName then
            Fail.notSupported
                ctx
                span
                $"the format string of [{m.Name}] in a query that defines the measure [{internalName}]"

        let moved = Context.transition ctx span

        if ctx.Model.CalcGroups.Length = 0 then
            own ev moved span m None
        else
            match CalculationItems.next moved span m with
            | NextItem.Measure -> own ev moved span m None
            | NextItem.Blank ->
                Fail.notSupported
                    ctx
                    span
                    $"the format string of [{m.Name}] where several calculation items are selected under selectionExpressionBehavior Visual"
            | NextItem.Item app ->
                let item = app.Item

                match selectableBelow moved item.Group with
                | Some lower ->
                    Fail.notSupported
                        ctx
                        span
                        $"the format string of [{m.Name}] with the calculation items {itemName item} and {itemName lower} (which has a format string expression) both selected: the sources disagree on which applies"
                | None -> ()

                match mentionsBelow moved item with
                | Some what ->
                    Fail.notSupported
                        ctx
                        span
                        $"the format string of [{m.Name}] under the calculation item {itemName item}, whose expression refers to {what} of lower precedence with format string expressions"
                | None -> ()

                match item.FormatStringExpression with
                | Some source ->
                    let expr = parse source

                    match evaluatesMeasure moved expr with
                    | Some what ->
                        Fail.notSupported
                            ctx
                            span
                            $"the format string expression of the calculation item {itemName item}: it evaluates {what}, and whether the item applies there is not documented"
                    | None -> ()

                    let inner = {
                        moved with
                            Scope = {
                                Vars = Map.empty
                                Source = source
                                InQuery = false
                            }
                            Group = null
                            Items = {
                                Applied = app :: moved.Items.Applied
                                Current = Some app
                            }
                    }

                    Some(
                        formatStringValue
                            ctx
                            span
                            $"the format string expression of the calculation item {itemName item}"
                            (ev.Scalar(inner, expr))
                    )
                | None ->
                    // "If a measure itself has a dynamic format string, it's considered as a lower
                    // precedence to any calculation group in the model" (Learn): where it is
                    // evaluated inside the item's expression is not said (a text constant is the
                    // same everywhere).
                    match m.FormatStringSource with
                    | Some source when
                        (match (Fn.unparen (parse source)).Kind with
                         | StringLiteral _ -> false
                         | _ -> true)
                        ->
                        Fail.notSupported
                            ctx
                            span
                            $"the dynamic format string of [{m.Name}] under the calculation item {itemName item}, which has no format string expression (where the measure's is evaluated inside the item is not documented)"
                    | _ -> ()

                    let inner = {
                        moved with
                            Items = {
                                Applied = app :: moved.Items.Applied
                                Current = None
                            }
                    }

                    own ev inner span m (Some(itemName item))

    /// The internal measure `Table[_<measure> FormatString]` of a measure with a dynamic format
    /// string, if that is what `table[name]` names.
    let private internalMeasures = ConditionalWeakTable<BMeasure, BMeasure>()

    let internalMeasure (ctx: Ctx) (table: BTable) (name: string) : BMeasure option =
        let suffix = " FormatString"

        if
            name.Length > suffix.Length + 1
            && name.StartsWith("_", StringComparison.Ordinal)
            && name.EndsWith(suffix, StringComparison.OrdinalIgnoreCase)
        then
            let measureName = name.Substring(1, name.Length - 1 - suffix.Length)

            match ctx.Model.Measures.TryGetValue measureName with
            | true, m when obj.ReferenceEquals(m.Table, table) && m.FormatStringSource.IsSome ->
                Some(
                    internalMeasures.GetValue(
                        m,
                        fun m ->
                            let source = m.FormatStringSource.Value
                            let at: Span = { Start = 0; Length = 0 }

                            // Bracketed: Table[Name] would be a column of the same name.
                            let reference: Expr = { Kind = Bracketed m.Name; Span = at }

                            let body: Expr = {
                                Kind = Call({ Name = ownName; Span = at }, [ reference ])
                                Span = at
                            }

                            BMeasure($"_{m.Name} FormatString", m.Table, source, lazy body)
                    )
                )
            | _ -> None
        else
            None

    /// A measure column of a query's result that `--formatted` gives format strings: the column's
    /// name, the hidden column its format strings are computed in, and the measure reference.
    type Probe = {
        Column: string
        Hidden: string
        Reference: Expr
        /// Which name/expression pair it is, counting every pair of the call from 0.
        Pair: int
    }

    /// The query's table expression with a hidden column per measure column (a name/expression pair
    /// whose expression is a measure reference) of a top-level SUMMARIZECOLUMNS, ADDCOLUMNS or ROW,
    /// holding the cell's format string. As Power BI asks for a dynamic format string: "\"v_Sales_Amount_FormatString\",
    /// IGNORE ( 'Sales'[_Sales Amount FormatString] )" (SQLBI), so SUMMARIZECOLUMNS keeps the same rows.
    ///
    /// Also returns the number of name/expression pairs: they are the last columns of the result,
    /// before the hidden ones.
    let withProbes (table: Expr) : Expr * Probe list * int =
        let e = Fn.unparen table

        match e.Kind with
        | Call(name, args) ->
            let fn = name.Name.ToUpperInvariant()

            let isName (a: Expr) =
                match a.Kind with
                | StringLiteral _ -> true
                | _ -> false

            let start =
                match fn with
                | "ROW" -> Some 0
                | "ADDCOLUMNS" -> Some 1
                | "SUMMARIZECOLUMNS" -> args |> List.tryFindIndex isName
                | _ -> None

            match start with
            | None -> table, [], 0
            | Some start ->
                let args = Array.ofList args

                let unignore (x: Expr) =
                    match (Fn.unparen x).Kind with
                    | Call(n, [ inner ]) when fn = "SUMMARIZECOLUMNS" && n.Name.ToUpperInvariant() = "IGNORE" -> inner
                    | _ -> x

                let mutable pairs = 0

                let probes = [
                    let mutable i = start

                    while i + 1 < args.Length && isName args.[i] do
                        let reference = Fn.unparen (unignore args.[i + 1])

                        match args.[i].Kind, reference.Kind with
                        | StringLiteral column, (Bracketed _ | Qualified _) ->
                            yield {
                                Column = column
                                Hidden = $"\u0001format string {i}"
                                Reference = reference
                                Pair = pairs
                            }
                        | _ -> ()

                        pairs <- pairs + 1
                        i <- i + 2
                ]

                // Anything after the pairs (a malformed call) and the evaluator's own error.
                if probes.IsEmpty || start + 2 * pairs <> args.Length then
                    table, [], 0
                else
                    let extra =
                        probes
                        |> List.collect (fun p ->
                            let at = p.Reference.Span

                            let probe: Expr = {
                                Kind = Call({ Name = probeName; Span = at }, [ p.Reference ])
                                Span = at
                            }

                            let value: Expr =
                                if fn = "SUMMARIZECOLUMNS" then
                                    {
                                        Kind = Call({ Name = "IGNORE"; Span = at }, [ probe ])
                                        Span = at
                                    }
                                else
                                    probe

                            [
                                ({
                                    Kind = StringLiteral p.Hidden
                                    Span = at
                                }
                                : Expr)
                                value
                            ])

                    {
                        e with
                            Kind = Call(name, List.ofArray args @ extra)
                    },
                    probes,
                    pairs
        | _ -> table, [], 0

    let private deferMark = "\u0000"

    /// An error of the probe as a value: kind, message and place.
    let private defer (kind: string) (what: string) (at: string) =
        String.Join(deferMark, [| ""; kind; what; at |])

    /// A probe's value, with an error it returned raised.
    let probeValue (v: DaxValue) : DaxValue =
        match v with
        | DaxValue.String s when s.StartsWith(deferMark, StringComparison.Ordinal) ->
            match s.Split deferMark with
            | [| _; "n"; what; at |] -> raise (EngineException(DaxNotSupported(what, at)))
            | [| _; "i"; what; at |] -> raise (EngineException(DaxInvalid(what, at)))
            | [| _; _; what; at |] -> raise (DaxRuntimeException(what, at))
            | _ -> invalidOp "a malformed deferred error"
        | v -> v

    /// A cell's value formatted with its format string, as FORMAT would (BLANK is "").
    let render (ctx: Ctx) (span: Span) (column: string) (formatString: string) (v: DaxValue) : string =
        match v with
        | DaxValue.Blank -> ""
        | _ ->
            Culture.requireModel ctx span "formatting a result"

            try
                Formatting.format (Some formatString) v
            with FormatRefused why ->
                Fail.notSupported ctx span $"formatting [{column}] with its format string \"{formatString}\": {why}"

    let functions: DaxFunction list = [
        // "A string holding the format string of the measure that is currently in context when the
        // calculation item is evaluated" (Learn).
        Fn.scalar
            "SELECTEDMEASUREFORMATSTRING"
            "Calculation groups"
            (0, 0)
            (Fn.typeIs DaxType.String)
            (fun ev ctx call ->
                let app = CalculationGroups.current ctx call
                let m = app.Measure

                if isOwn app then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING in the dynamic format string of [{m.Name}] itself"

                let earlier =
                    ctx.Items.Applied
                    |> List.exists (fun a ->
                        not (obj.ReferenceEquals(a, app))
                        && obj.ReferenceEquals(a.Measure, m)
                        && a.Item.FormatStringExpression.IsSome)

                if earlier then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}] after a calculation item with a format string expression was applied to it (SQLBI: it sees that item's; Learn: the highest one is the only one applied)"

                match selectableBelow ctx app.Item.Group with
                | Some lower ->
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}] with the calculation item {itemName lower} (which has a format string expression) selected below {itemName app.Item}: the sources disagree on which it sees"
                | None -> ()

                match own ev ctx call.Span m (Some(itemName app.Item)) with
                | Some f -> DaxValue.String f
                | None ->
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}], which has no format string")

        // The cell probe of `--formatted`: unreachable from DAX. An error is returned as a value
        // (`deferred`): it is raised only for a row the result keeps (SUMMARIZECOLUMNS computes
        // rows it then drops, START AT skips some).
        Fn.scalar probeName internalFamily (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            match measureNamed ctx call.Args.[0] with
            | Some m ->
                try
                    match atReference ev ctx call.Args.[0].Span m with
                    | Some f -> DaxValue.String f
                    | None -> DaxValue.Blank
                with
                | EngineException(DaxNotSupported(what, at)) -> DaxValue.String(defer "n" what at)
                | EngineException(DaxInvalid(what, at)) -> DaxValue.String(defer "i" what at)
                | DaxRuntimeException(what, at) -> DaxValue.String(defer "r" what at)
            | None -> DaxValue.Blank)

        // The body of `Table[_<measure> FormatString]`: unreachable from DAX.
        Fn.scalar ownName internalFamily (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            match
                measureNamed
                    {
                        ctx with
                            Scope = { ctx.Scope with InQuery = false }
                    }
                    call.Args.[0]
            with
            | Some m ->
                if ctx.Model.CalcGroups.Length > 0 then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"the measure [_{m.Name} FormatString] in a model with calculation groups (whether their items apply to it is not documented)"

                match own ev ctx call.Span m None with
                | Some f -> DaxValue.String f
                | None -> DaxValue.Blank
            | None -> Fail.invalid ctx call.Span "no measure")
    ]
