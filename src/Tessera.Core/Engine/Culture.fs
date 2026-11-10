namespace Tessera.DaxEngine

open System
open Tessera.DaxAst

/// The culture text is written in. FORMAT, the implicit conversion of a number to text and the
/// case of letters all follow the model's culture ("Predefined format strings use the model culture
/// property when formatting the result": Microsoft Learn, FORMAT). The engine writes en-US exactly
/// and refuses every other culture: .NET's own formatting of a culture is not the engine's.
module Culture =

    let enUs = "en-US"

    let isEnUs (name: string) =
        not (isNull name)
        && String.Equals(name.Trim(), enUs, StringComparison.OrdinalIgnoreCase)

    /// Refuse `what` unless `culture` is en-US.
    let requireEnUs (ctx: Ctx) (span: Span) (culture: string) (what: string) : unit =
        if String.IsNullOrWhiteSpace culture then
            Fail.notSupported
                ctx
                span
                $"{what}: the model names no culture (a server would use its own language), and only en-US is implemented"
        elif not (isEnUs culture) then
            Fail.notSupported ctx span $"{what} in the culture {culture.Trim()} (only en-US is implemented)"

    /// Refuse `what` unless the model's culture is en-US.
    let requireModel (ctx: Ctx) (span: Span) (what: string) : unit =
        requireEnUs ctx span ctx.Model.Culture what
