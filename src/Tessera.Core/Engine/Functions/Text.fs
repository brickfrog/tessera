namespace Tessera.DaxEngine.Functions

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// The one text function the engine has so far: CONCATENATE.
module Text =

    let functions: DaxFunction list = [
        // "A BLANK argument is an empty string. If both arguments are BLANK, the result is BLANK"
        // (dax.guide/concatenate). The page shows TRUE as "TRUE" where other sources give "True",
        // so Booleans are refused (see `Convert.toText`).
        Fn.scalar "CONCATENATE" "Text" (2, 2) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let a = ev.Scalar(ctx, call.Args.[0])
            let b = ev.Scalar(ctx, call.Args.[1])

            match a, b with
            | DaxValue.Blank, DaxValue.Blank -> DaxValue.Blank
            | _ -> DaxValue.String(Convert.toText ctx call.Span a + Convert.toText ctx call.Span b))
    ]
