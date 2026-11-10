namespace Tessera.DaxEngine.Functions

// FIRSTNONBLANK, LASTNONBLANK, FIRSTNONBLANKVALUE and LASTNONBLANKVALUE.

open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module NonBlank =

    /// The values the function goes through, sorted ascending. A column reference "is replaced by
    /// CALCULATETABLE ( DISTINCT ( <ColumnName> ) )" (dax.guide/firstnonblank): the values visible
    /// after a context transition, without the blank row. A table expression must have one
    /// column. A Boolean expression as the first argument is not supported.
    let private sortedValues (ev: IEval) (ctx: Ctx) (name: string) (arg: Expr) : TableVal * int array =
        let table =
            match (Fn.unparen arg).Kind, ev.TryColumn(ctx, arg) with
            | Qualified _, Some c ->
                let moved = Context.transition ctx arg.Span
                let ids = Filters.idsIn c (Filters.visible moved.Filter c.Table) false
                Tables.ofColumns [| Tables.modelColumn c |] [| ids |> Array.map c.ValueOfId |] ids.Length
            | _ when ev.IsTable(ctx, arg) ->
                let t = ev.Table(ctx, arg)

                if t.Cols.Length <> 1 then
                    Fail.invalid ctx arg.Span $"the first argument of {name} is a column or a table of one column"

                Tables.distinct ctx.Model (Tables.materialize t)
            | _ -> Fail.notSupported ctx arg.Span $"{name} with a Boolean expression as its first argument"

        let values = table.Column 0
        // "the first value": the order of the values. A sort-by-column property is not read.
        Sorting.check ctx arg.Span name values

        let order =
            List.init table.Count id
            |> List.sortWith (fun a b -> DaxValue.sortCompare ctx.Model.Collation values.[a] values.[b])
            |> Array.ofList

        table, order

    /// The position of the first value, in this order, for which the expression is not BLANK; the
    /// expression is evaluated in a row context on each value ("Row Context" on the Expression
    /// argument, dax.guide/firstnonblank), so a measure in it does a context transition and an
    /// aggregation doesn't.
    let private firstNonBlank (ev: IEval) (ctx: Ctx) (table: TableVal) (order: int array) (expr: Expr) : int option =
        order
        |> Array.tryFind (fun row -> not (DaxValue.isBlank (ev.Scalar(Context.pushRow ctx table row, expr))))

    let private nonBlank (last: bool) (name: string) =
        Fn.table name "Time intelligence" (2, 2) (fun ev ctx call ->
            let table, order = sortedValues ev ctx name call.Args.[0]
            let order = if last then Array.rev order else order

            match firstNonBlank ev ctx table order call.Args.[1] with
            | Some row -> Tables.select table [| row |]
            | None -> Tables.select table [||])

    /// "CALCULATE ( <Expression>, FIRSTNONBLANK ( <ColumnName>, CALCULATE ( <Expression> ) ) )"
    /// (dax.guide/firstnonblankvalue).
    let private nonBlankValue (last: bool) (name: string) =
        Fn.scalar name "Time intelligence" (2, 2) (Fn.typeOfArg 1) (fun ev ctx call ->
            let expr = call.Args.[1]
            let table, order = sortedValues ev ctx name call.Args.[0]
            let order = if last then Array.rev order else order

            let calculated: Expr = {
                Kind = Call({ Name = "CALCULATE"; Span = expr.Span }, [ expr ])
                Span = expr.Span
            }

            let found = firstNonBlank ev ctx table order calculated

            // The outer CALCULATE: the one-row table (or the empty one) as its filter.
            let filterTable =
                match found with
                | Some row -> Tables.select table [| row |]
                | None -> Tables.select table [||]

            let moved = Context.transition ctx call.Span
            let filters = Filters.ofTable ctx call.Span filterTable

            ev.Scalar(
                {
                    moved with
                        Filter = Filters.add moved.Filter filters false
                },
                expr
            ))

    let functions: DaxFunction list = [
        nonBlank false "FIRSTNONBLANK"
        nonBlank true "LASTNONBLANK"
        nonBlankValue false "FIRSTNONBLANKVALUE"
        nonBlankValue true "LASTNONBLANKVALUE"
    ]
