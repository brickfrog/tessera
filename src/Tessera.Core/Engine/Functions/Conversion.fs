namespace Tessera.DaxEngine.Functions

// Conversion functions (CONVERT, CURRENCY, VALUE) and what the other families share to read their
// arguments: text as a number, text as a date.
//
// Sources: dax.guide/convert, dax.guide/currency, dax.guide/value and the same pages of the DAX
// reference on Microsoft Learn; SQLBI "Handling BLANK in DAX". The rules of the conversions
// themselves (rounding, ranges) are in `DaxValue.convertTo`.

open System
open System.Globalization
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// Reading arguments the way the functions need them.
module Coerce =

    let private inv = CultureInfo.InvariantCulture

    /// The serial number of text that is a date, a time, or both, as the date functions read it
    /// (`DateText.read`, with the model's culture: ISO 8601 under any culture, and under en-US the
    /// forms the documentation shows results for, such as "10/23/2020", "23/10/2020" and "6:05 PM").
    /// ValueNone for text it doesn't read.
    let tryDateText (ctx: Ctx) (_: Span) (text: string) : float voption =
        match DateText.read ctx.Model.Culture text with
        | Some r ->
            let day =
                match r.Date with
                | ValueSome d -> DaxValue.serialOfDateTime d
                | ValueNone -> 0.0

            let time =
                match r.Time with
                | ValueSome t -> float (t.Ticks / TimeSpan.TicksPerMillisecond) / 86400000.0
                | ValueNone -> 0.0

            ValueSome(day + time)
        | None -> ValueNone

    /// Text that is neither a number nor a date the engine reads: an error DAX raises ("If text
    /// is not in one of these formats, an error is returned": Microsoft Learn, VALUE) when it
    /// can't be a number or a date in any culture, and refused when it might be one in some.
    let private unreadable (ctx: Ctx) (span: Span) (text: string) (target: DaxType) : 'a =
        let t = text.Trim()
        let mutable dt = DateTime.MinValue

        if t = "" || not (t |> Seq.exists Char.IsDigit) then
            Fail.runtime ctx span (DaxError.message (CannotConvert(text, DaxType.String, target)))
        elif DaxValue.isCultureNumber t then
            Fail.notSupported
                ctx
                span
                $"the text \"{text}\" read as a number (whether thousands separators, currency symbols and other culture-dependent forms are accepted is not documented)"
        elif DateTime.TryParse(t, inv, DateTimeStyles.AllowWhiteSpaces, &dt) then
            Fail.notSupported
                ctx
                span
                $"the text \"{text}\" read as a date (how a date is written depends on the model's culture)"
        else
            Fail.runtime ctx span (DaxError.message (CannotConvert(text, DaxType.String, target)))

    /// Text as a number: an Integer or a Decimal, as it reads ("123", "123.45678", "123e3": the
    /// forms dax.guide/value shows). ValueNone when it isn't a plain number.
    let private tryNumberText (text: string) : DaxValue voption =
        match DaxValue.convertTo DaxType.Decimal (DaxValue.String text) with
        | Ok(DaxValue.Decimal d) ->
            match Int64.TryParse(text.Trim(), NumberStyles.AllowLeadingSign, inv) with
            | true, i -> ValueSome(DaxValue.Integer i)
            | _ -> ValueSome(DaxValue.Decimal d)
        | _ -> ValueNone

    /// Text as a number, where a number is needed: "the string is automatically converted into a
    /// corresponding number, as long as the string is a valid representation of a number" (SQLBI,
    /// "Understanding numeric data type conversions in DAX"). Text in a date form is refused here:
    /// only VALUE and CONVERT are documented to read one.
    let numberOfText (ctx: Ctx) (span: Span) (text: string) : DaxValue =
        match tryNumberText text with
        | ValueSome n -> n
        | ValueNone ->
            match tryDateText ctx span text with
            | ValueSome _ -> Fail.notSupported ctx span $"the text \"{text}\", a date, where a number is needed"
            | ValueNone -> unreadable ctx span text DaxType.Decimal

    /// Text as VALUE reads it: a number, or the serial number of a date or a time.
    let valueOfText (ctx: Ctx) (span: Span) (text: string) : float =
        match tryNumberText text with
        | ValueSome(DaxValue.Integer i) -> float i
        | ValueSome(DaxValue.Decimal d) -> d
        | _ ->
            match tryDateText ctx span text with
            | ValueSome serial -> serial
            | ValueNone -> unreadable ctx span text DaxType.Decimal

    /// Text as a date, for CONVERT ( text, DATETIME ).
    let dateOfText (ctx: Ctx) (span: Span) (text: string) : DaxValue =
        match tryDateText ctx span text with
        | ValueSome serial -> DaxValue.DateTime serial
        | ValueNone ->
            match tryNumberText text with
            | ValueSome _ -> Fail.notSupported ctx span $"the text \"{text}\", a number, converted to a date"
            | ValueNone -> unreadable ctx span text DaxType.DateTime

    /// A numeric argument of a function: an Integer, a Decimal or a Currency as it is, text that
    /// is a number as that number, BLANK as BLANK (each function says what a BLANK is to it).
    /// Dates and Booleans are refused: the pages of the math functions don't say what they do
    /// with them (the functions that do say, like INT and CURRENCY, read them themselves).
    let number (ctx: Ctx) (span: Span) (name: string) (v: DaxValue) : DaxValue =
        match v with
        | DaxValue.Blank
        | DaxValue.Integer _
        | DaxValue.Decimal _
        | DaxValue.Currency _ -> v
        | DaxValue.String s -> numberOfText ctx span s
        | DaxValue.Boolean _ -> Fail.notSupported ctx span $"{name} of TRUE or FALSE"
        | DaxValue.DateTime _ -> Fail.notSupported ctx span $"{name} of a date"

    /// `number`, with BLANK refused: for the arguments whose page says nothing of BLANK.
    let numberNotBlank (ctx: Ctx) (span: Span) (name: string) (v: DaxValue) : DaxValue =
        match number ctx span name v with
        | DaxValue.Blank -> Fail.notSupported ctx span $"{name} of BLANK"
        | n -> n

    /// The float of an Integer, a Decimal or a Currency.
    let toFloat (v: DaxValue) : float =
        match v with
        | DaxValue.Integer i -> float i
        | DaxValue.Decimal d -> d
        | DaxValue.Currency c -> float c / 10000.0
        | _ -> nan

module Conversion =

    /// The data types CONVERT takes: "BOOLEAN/LOGICAL, CURRENCY/DECIMAL, DATETIME, DOUBLE,
    /// INTEGER/INT64, STRING/TEXT" (dax.guide/convert).
    let private targetOf (e: Expr) : DaxType option =
        match Fn.keyword e with
        | Some("BOOLEAN" | "LOGICAL") -> Some DaxType.Boolean
        | Some("CURRENCY" | "DECIMAL") -> Some DaxType.Currency
        | Some "DATETIME" -> Some DaxType.DateTime
        | Some "DOUBLE" -> Some DaxType.Decimal
        | Some("INTEGER" | "INT64") -> Some DaxType.Integer
        | Some("STRING" | "TEXT") -> Some DaxType.String
        | _ -> None

    /// 9999-12-31, the last date; a serial number below zero is before 1899-12-30.
    let private lastSerial = 2958465.0

    /// CONVERT ( value, type ).
    let convert (ctx: Ctx) (span: Span) (target: DaxType) (v: DaxValue) : DaxValue =
        let viaValue (v: DaxValue) =
            Fail.dax ctx span (DaxValue.convertTo target v)

        match target, v with
        // "When Expression is BLANK, CONVERT returns BLANK. The only exception is BOOLEAN: CONVERT
        // ( BLANK (), BOOLEAN ) returns FALSE" (dax.guide/convert).
        | DaxType.Boolean, DaxValue.Blank -> DaxValue.Boolean false
        | _, DaxValue.Blank -> DaxValue.Blank

        // Text: "will try to convert text to a number. If conversion succeeds the number will be
        // converted to currency, otherwise an error is returned" (Microsoft Learn, CURRENCY);
        // "CONVERT ( "abc", INTEGER ) returns an error" (dax.guide/convert).
        | (DaxType.Integer | DaxType.Decimal | DaxType.Currency), DaxValue.String s ->
            Coerce.numberOfText ctx span s |> ignore
            viaValue v
        | DaxType.DateTime, DaxValue.String s -> Coerce.dateOfText ctx span s
        | DaxType.Boolean, DaxValue.String _ -> Fail.notSupported ctx span "CONVERT of text to BOOLEAN"
        | DaxType.String, DaxValue.String _ -> v

        // To text: a date's text depends on the culture and a Boolean's is not settled.
        | DaxType.String, _ -> DaxValue.String(Convert.toText ctx span v)

        // "( "Date3", CONVERT ( 12345, DATETIME ) )" is 1933-10-18 and "CONVERT ( FALSE, DATETIME )"
        // 1899-12-30 (dax.guide/convert): the number is the serial. Outside the dates there are,
        // "If the value of the expression is not a proper DateTime value an error is returned" is
        // said of the other direction only, so that is refused.
        | DaxType.DateTime, (DaxValue.Integer _ | DaxValue.Decimal _ | DaxValue.Currency _ | DaxValue.Boolean _) ->
            match viaValue v with
            | DaxValue.DateTime serial when serial >= 0.0 && serial < lastSerial + 1.0 -> DaxValue.DateTime serial
            | _ ->
                Fail.notSupported
                    ctx
                    span
                    "CONVERT to DATETIME of a number that is not a date from 1899-12-30 to 9999-12-31"
        | DaxType.Boolean, DaxValue.DateTime _ -> Fail.notSupported ctx span "CONVERT of a date to BOOLEAN"
        | _ -> viaValue v

    let private convertFunction =
        Fn.scalar
            "CONVERT"
            "Math"
            (2, 2)
            (fun _ _ args ->
                match targetOf args.[1] with
                | Some t -> StaticType.Known t
                | None -> StaticType.Unknown)
            (fun ev ctx call ->
                match targetOf call.Args.[1] with
                | Some target -> convert ctx call.Span target (ev.Scalar(ctx, call.Args.[0]))
                | None ->
                    Fail.invalid
                        ctx
                        call.Args.[1].Span
                        "the second argument of CONVERT is a data type: BOOLEAN, CURRENCY, DATETIME, DOUBLE, INTEGER or STRING")

    let functions: DaxFunction list = [
        convertFunction

        // "The CURRENCY() function has the same behavior as calling CONVERT with a CURRENCY
        // argument" (dax.guide/currency).
        Fn.scalar "CURRENCY" "Math" (1, 1) (Fn.typeIs DaxType.Currency) (fun ev ctx call ->
            convert ctx call.Span DaxType.Currency (ev.Scalar(ctx, call.Args.[0])))

        // "The converted number in decimal data type"; "The value passed as the text parameter can
        // be in any of the constant, number, date, or time formats recognized by DAX. If text is
        // not in one of these formats, an error is returned" (dax.guide/value). An empty string
        // "cannot be converted to a valid value by the VALUE function" (SQLBI, "Handling BLANK in
        // DAX"). A number becomes a decimal number ("VALUE can be used to convert all values to a
        // single numeric data type": Microsoft Learn). The pages say nothing of BLANK, a date or a
        // Boolean.
        Fn.scalar "VALUE" "Text" (1, 1) (Fn.typeIs DaxType.Decimal) (fun ev ctx call ->
            let span = call.Args.[0].Span

            match ev.Scalar(ctx, call.Args.[0]) with
            | DaxValue.String s -> DaxValue.Decimal(Coerce.valueOfText ctx span s)
            | DaxValue.Integer _
            | DaxValue.Decimal _
            | DaxValue.Currency _ as v -> DaxValue.Decimal(Coerce.toFloat v)
            | DaxValue.Blank -> Fail.notSupported ctx span "VALUE of BLANK"
            | DaxValue.Boolean _ -> Fail.notSupported ctx span "VALUE of TRUE or FALSE"
            | DaxValue.DateTime _ -> Fail.notSupported ctx span "VALUE of a date")
    ]
