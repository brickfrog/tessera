namespace Tessera.DaxEngine.Functions

// Calculation groups: which calculation item a measure reference becomes, and the functions of a
// calculation item's expression (SELECTEDMEASURE, SELECTEDMEASURENAME, ISSELECTEDMEASURE,
// SELECTEDMEASUREFORMATSTRING).
//
// The rules, with where they come from:
//
// - "A calculation item is applied by replacing measure references invoked when there is a
//   calculation item active in the filter context"; "Without a measure reference, a calculation
//   item does not apply any modification"; "If the expression inside CALCULATE contains multiple
//   measure references, all of them are replaced" (SQLBI, "Understanding the application of
//   calculation items"). So the item is applied where a measure reference is evaluated, after
//   its context transition ("still applying an implicit context transition": SQLBI,
//   "Understanding calculation groups"), and SELECTEDMEASURE () is that measure.
// - An item is selected when its group is filtered and exactly that one item is visible:
//   "Calculation groups are only applied if a single calculation item from the calculation group
//   is in filter context" (Learn, "Calculation groups"). Not filtered, several items, or none
//   (a filter naming an item that doesn't exist, a conflicting selection): with no selection
//   expression and the default behaviour, "Calculation group is not filtered" (Learn, the table of
//   selection expressions), so the measure is evaluated unchanged; "If the user does not select
//   one value or if the user selects multiple values together, then the engine does not apply any
//   variation" (SQLBI, "Introducing calculation groups"). With selectionExpressionBehavior Visual
//   several items give BLANK (Learn). Selection expressions themselves are refused.
// - Several groups: "The calculation group with the highest value is applied first; then the
//   other calculation groups are applied according to their Precedence value in a descending
//   order" (SQLBI, "Understanding calculation group precedence"), each replacing the measure
//   references of what the one before produced: "( ( 10 ) + 2 ) * 2 = 24" (Learn). Here that is:
//   the highest group's item is applied; the others are still in the filter context where its
//   SELECTEDMEASURE () (or another measure reference in its expression) is evaluated, and are
//   applied there, in the filter context of that point (SQLBI's DATESYTD / Daily AVG example).
// - An item in force is not applied again: "The calculation item is applied only once at the
//   top-level calculation and never again" (SQLBI, "Using calculation groups to selectively
//   replace measures in DAX expressions"), also to the measures that the measure it was applied
//   to refers to, while a group selected after it is ("Only now does Multiplier kick in", 25:
//   SQLBI, "Avoiding pitfalls in calculation groups precedence"). In force means: the filters on
//   the group's table are the very ones it was applied under.
// - Sideways recursion: a CALCULATE in an item that selects another item of the same group
//   applies that one ("both expressions are evaluated separately because they are using different
//   calculate statements"; PY YTD reuses YTD: Learn). "Other types of recursion aren't
//   supported" (Learn): an item selected again while it is being applied, by a filter of its own,
//   is refused.

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// What a measure reference becomes.
[<RequireQualifiedAccess>]
type NextItem =
    /// The measure itself: no item to apply.
    | Measure
    /// This item's expression, with SELECTEDMEASURE () the measure.
    | Item of ItemApplication
    /// BLANK: several items of a group selected, under selectionExpressionBehavior Visual.
    | Blank

module CalculationItems =

    /// The filters of a filter context on a calculation group's table.
    let groupFilters (fc: FilterCtx) (g: BCalcGroup) : Filter list =
        fc.Filters
        |> List.filter (fun f ->
            match f with
            | OnColumn(c, _) -> obj.ReferenceEquals(c.Table, g.Table)
            | Tuples(cols, _) -> cols |> Array.exists (fun c -> obj.ReferenceEquals(c.Table, g.Table))
            | Rows(t, _, cfg) -> cfg.IsExpanded(t, g.Table))

    let private sameFilters (a: Filter list) (b: Filter list) =
        a.Length = b.Length
        && List.forall2 (fun (x: Filter) (y: Filter) -> obj.ReferenceEquals(x, y)) a b

    let private computing (model: Model) =
        model.Tables
        |> Array.exists (fun t -> t.Computing || t.Columns |> Array.exists _.Computing)

    /// What a reference to `m` becomes in `ctx` (already after its context transition).
    let next (ctx: Ctx) (span: Span) (m: BMeasure) : NextItem =
        let groups = ctx.Model.CalcGroups
        let mutable result = ValueNone
        let mutable i = 0

        while result.IsNone && i < groups.Length do
            let g = groups.[i]
            i <- i + 1
            let filters = groupFilters ctx.Filter g

            let applied =
                ctx.Items.Applied |> List.filter (fun a -> obj.ReferenceEquals(a.Item.Group, g))

            if filters.IsEmpty then
                match g.NoSelection with
                | Some why -> Fail.notSupported ctx span why
                | None -> ()
            else
                let visible = Filters.visible ctx.Filter g.Table

                if visible.Count = 1 then
                    let item = g.Items.[visible.Rows.[0]]

                    match applied with
                    | latest :: _ when sameFilters latest.GroupFilters filters -> () // in force: not again
                    | _ when applied |> List.exists (fun a -> obj.ReferenceEquals(a.Item, item)) ->
                        Fail.notSupported
                            ctx
                            span
                            $"the calculation item '{g.Name}'[{item.Name}] selected again by a filter while it is being applied (\"Other types of recursion aren't supported\": Learn, Calculation groups)"
                    | _ ->
                        if computing ctx.Model then
                            Fail.notSupported
                                ctx
                                span
                                $"the calculation item '{g.Name}'[{item.Name}] applied in a calculated column or table"

                        result <-
                            ValueSome(
                                NextItem.Item {
                                    Item = item
                                    Measure = m
                                    GroupFilters = filters
                                    Functions = ctx.Functions
                                }
                            )
                else
                    match g.MultipleOrEmpty with
                    | Some why -> Fail.notSupported ctx span why
                    | None ->
                        if g.MultipleIsBlank then
                            if visible.Count = 0 then
                                Fail.notSupported
                                    ctx
                                    span
                                    $"no item of calculation group '{g.Name}' selected (a filter on an item that doesn't exist, or conflicting filters) under selectionExpressionBehavior Visual"

                            result <- ValueSome NextItem.Blank

        match result with
        | ValueSome r -> r
        | ValueNone -> NextItem.Measure

module CalculationGroups =

    /// The item whose expression this is: these functions are only for one ("Can only be
    /// referenced in the expression for a calculation item": Learn, SELECTEDMEASURE).
    let private current (ctx: Ctx) (call: Call) : ItemApplication =
        let name = call.Name.Name.ToUpperInvariant()

        match ctx.Items.Current with
        | Some app when obj.ReferenceEquals(app.Functions, ctx.Functions) -> app
        | Some _ ->
            Fail.notSupported
                ctx
                call.Span
                $"{name} inside a user-defined function called from a calculation item (whether the function sees the item's measure is not documented)"
        | None ->
            Fail.notSupported
                ctx
                call.Span
                $"{name} outside the expression of a calculation item (\"Can only be referenced in the expression for a calculation item\": Learn)"

    /// The measure an argument of ISSELECTEDMEASURE names, as the code it is written in sees it.
    let private measureNamed (ctx: Ctx) (arg: Expr) : BMeasure option =
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

        match (Fn.unparen arg).Kind with
        | Bracketed name -> find name
        | Qualified(table, name) ->
            match ctx.Model.TableByName.TryGetValue table.Name with
            | true, t when not (t.ColumnByName.ContainsKey name) -> find name
            | _ -> None
        | _ -> None

    let functions: DaxFunction list = [
        // "A reference to the measure that is currently in context when the calculation item or
        // format string is evaluated" (Learn); a measure reference, so a context transition.
        Fn.scalar "SELECTEDMEASURE" "Calculation groups" (0, 0) Fn.typeUnknown (fun ev ctx call ->
            let app = current ctx call
            ev.Measure(ctx, call.Span, app.Measure))

        // "A string value holding the name of the measure that is currently in context" (Learn).
        Fn.scalar "SELECTEDMEASURENAME" "Calculation groups" (0, 0) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            DaxValue.String (current ctx call).Measure.Name)

        // "A Boolean indicating whether the measure that is currently in context is one of those
        // specified in the list of parameters" (Learn); a name that is not a measure "fails with
        // the error Invalid input arguments for ISSELECTEDMEASURE" (SQLBI, "Introducing
        // calculation groups").
        Fn.scalar "ISSELECTEDMEASURE" "Calculation groups" (1, -1) (Fn.typeIs DaxType.Boolean) (fun _ ctx call ->
            let app = current ctx call

            let named =
                call.Args
                |> List.map (fun a ->
                    match measureNamed ctx a with
                    | Some m -> m
                    | None ->
                        Fail.invalid ctx a.Span "Invalid input arguments for ISSELECTEDMEASURE: a measure is needed")

            if
                not (isNull ctx.Query)
                && (match ctx.Query.Measures.TryGetValue app.Measure.Name with
                    | true, q -> obj.ReferenceEquals(q, app.Measure)
                    | _ -> false)
                && ctx.Model.Measures.ContainsKey app.Measure.Name
            then
                Fail.notSupported
                    ctx
                    call.Span
                    $"ISSELECTEDMEASURE when the measure in context is a query measure that redefines the model's [{app.Measure.Name}]"

            DaxValue.Boolean(named |> List.exists (fun m -> obj.ReferenceEquals(m, app.Measure))))

        // "A string holding the format string of the measure that is currently in context" (Learn).
        // Which format string that is once another item with a format string expression has been
        // applied to the same measure ("This format string may already have been defined by
        // another calculation item with higher precedence": SQLBI, "Controlling format strings in
        // calculation groups"), or for a measure with a dynamic format string or none, is not
        // something the engine evaluates: refused.
        Fn.scalar
            "SELECTEDMEASUREFORMATSTRING"
            "Calculation groups"
            (0, 0)
            (Fn.typeIs DaxType.String)
            (fun _ ctx call ->
                let app = current ctx call
                let m = app.Measure

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
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}] after a calculation item with a format string expression was applied to it"

                if m.HasFormatStringExpression then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}], which has a dynamic format string"

                if String.IsNullOrEmpty m.FormatString then
                    Fail.notSupported
                        ctx
                        call.Span
                        $"SELECTEDMEASUREFORMATSTRING of [{m.Name}], which has no format string"

                DaxValue.String m.FormatString)
    ]
