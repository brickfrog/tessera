namespace Tessera.DaxEngine.Functions

open System
open System.Globalization
open System.Numerics
open System.Text
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// A value or a format string the engine doesn't format exactly: the reason, for a refusal.
exception FormatRefused of string

/// FORMAT and FIXED: numbers and dates as text, in en-US.
///
/// Sources: Microsoft Learn, FORMAT (learn.microsoft.com/en-us/dax/format-function-dax: the tables
/// of predefined and custom formats) and DAX Guide (dax.guide/format: queries with their results).
/// "The format strings supported as an argument to the DAX FORMAT function are based on the format
/// strings used by Visual Basic (OLE Automation), not on the format strings used by the .NET
/// Framework", so nothing here goes through .NET's formatting: digits are produced from a decimal
/// reading of the value and laid out by the rules the pages give.
///
/// What the pages show or state is implemented; everything else is refused (`FormatRefused`), and so
/// is everything on which the two disagree:
///   - "c", "ddddd" and "ttttt": Microsoft Learn's en-US column shows 06/25/2020 13:23:45, DAX Guide
///     1/2/2020 3:30:00 PM and 1/9/2020;
///   - "Medium Date": the long date according to Microsoft Learn, 02-Jan-21 in DAX Guide;
///   - "Percent" of a number that needs a thousand separator: 1,234,567.00 % in Microsoft Learn's
///     example, 1234567.00% in DAX Guide;
///   - every culture other than en-US, for the model and for the locale argument.
///
/// Upper-case letters in a date format. The current pages write the date and time characters in
/// lower case and show two upper-case ones: "\QQ yyyy" is Q1 2019 (dax.guide/format), and Power BI's
/// own auto date/time table has `Month = FORMAT([Date], "MMMM")` (sqlbi.com, "Continuous axis for
/// any Date table in Power BI") whose rows Microsoft Learn shows as January ("Auto date/time in Power
/// BI Desktop"). The characters are Visual Basic's (OLE Automation), and in those the case doesn't
/// matter: "Unlike in previous versions of Visual Basic, these format characters are case-sensitive"
/// (Visual Basic 2005, "User-Defined Date/Time Formats (Format Function)"), and "Format Function for
/// Visual Basic 6.0 Users" writes the VB6 specifiers "Nn", "N" and "Hh". So an upper-case d, m, y,
/// h, n, s, w or q is read as the lower-case letter. The 2010 DAX pages ("Custom Date and Time formats
/// for the FORMAT Function", for PowerPivot) describe .NET's case-sensitive characters instead; they
/// no longer describe the engine (their h is the 12-hour clock, while "h:nn:ss" is 13:23:45 today),
/// but where they give an upper-case letter another meaning it is refused: M or MM right after the
/// hour (the month there, minutes in Visual Basic), H with AM/PM (always 24-hour there), Y alone
/// (the year there, the day of the year in Visual Basic), and the formats "D", "M" and "Y" (predefined
/// there). AM/PM, am/pm, A/P, a/p and AMPM/ampm are as the pages write them; mixed case is refused.
module Formatting =

    let private refuse (what: string) : 'a = raise (FormatRefused what)

    let private inv = CultureInfo.InvariantCulture

    // ---- Decimal readings of a number -----------------------------------------------------------

    /// A non-negative decimal number: `Mant` / 10^`Scale`.
    [<Struct>]
    type Dec = { Mant: BigInteger; Scale: int }

    let private pow10 (n: int) = BigInteger.Pow(BigInteger 10, n)

    /// "123.45", "1.2345E+004", "1E-05".
    let private decOfText (s: string) : Dec =
        let mantissa, exponent =
            match s.IndexOfAny [| 'E'; 'e' |] with
            | -1 -> s, 0
            | i -> s.Substring(0, i), Int32.Parse(s.Substring(i + 1), NumberStyles.AllowLeadingSign, inv)

        let decimals =
            match mantissa.IndexOf '.' with
            | -1 -> 0
            | i -> mantissa.Length - i - 1

        {
            Mant = BigInteger.Parse(mantissa.Replace(".", ""), NumberStyles.None, inv)
            Scale = decimals - exponent
        }

    /// The exact value of a finite, non-negative float.
    let private decOfFloatExact (d: float) : Dec =
        let bits = BitConverter.DoubleToInt64Bits d
        let exponent = int ((bits >>> 52) &&& 0x7FFL)
        let fraction = bits &&& 0xFFFFFFFFFFFFFL

        let mant, e2 =
            if exponent = 0 then
                fraction, -1074
            else
                (fraction ||| (1L <<< 52)), exponent - 1075

        if e2 >= 0 then
            {
                Mant = BigInteger mant * BigInteger.Pow(BigInteger 2, e2)
                Scale = 0
            }
        else
            {
                Mant = BigInteger mant * BigInteger.Pow(BigInteger 5, -e2)
                Scale = -e2
            }

    /// The ways a number reads in decimal. An Integer and a Currency are exact. A float is read
    /// three ways: to 15 significant digits (what OLE Automation shows of one), as the shortest
    /// text that gives it back, and exactly. Which of them the engine formats is not documented, so
    /// a result is given only when all three lead to the same text (2.675 with "0.00" is refused:
    /// the float is just below 2.675).
    let private readings (v: DaxValue) : bool * Dec list =
        match v with
        | DaxValue.Integer i ->
            i < 0L,
            [
                {
                    Mant = BigInteger.Abs(BigInteger i)
                    Scale = 0
                }
            ]
        | DaxValue.Currency c ->
            c < 0L,
            [
                {
                    Mant = BigInteger.Abs(BigInteger c)
                    Scale = 4
                }
            ]
        | DaxValue.Decimal d ->
            if Double.IsNaN d || Double.IsInfinity d then
                refuse "a number that is not finite"

            let a = abs d

            d < 0.0,
            [
                decOfText (a.ToString("E14", inv))
                decOfText (a.ToString("R", inv))
                decOfFloatExact a
            ]
        | _ -> refuse "a value that is not a number"

    /// The number times 10^`decimals`, rounded to a whole number, half up; and whether it was
    /// exactly halfway.
    let private roundTo (d: Dec) (decimals: int) : BigInteger * bool =
        if d.Scale <= decimals then
            d.Mant * pow10 (decimals - d.Scale), false
        else
            let divisor = pow10 (d.Scale - decimals)
            let quotient = BigInteger.DivRem(d.Mant, divisor)
            let struct (q, r) = quotient
            let twice = r * BigInteger 2

            if twice > divisor then q + BigInteger.One, false
            elif twice = divisor then q + BigInteger.One, true
            else q, false

    let private digitsOf (n: BigInteger) = n.ToString("D", inv)

    /// Commas between groups of three digits (en-US).
    let private group (digits: string) : string =
        if digits.Length <= 3 then
            digits
        else
            let sb = StringBuilder()
            let first = digits.Length % 3

            if first > 0 then
                sb.Append(digits, 0, first) |> ignore

            let mutable i = first

            while i < digits.Length do
                if sb.Length > 0 then
                    sb.Append ',' |> ignore

                sb.Append(digits, i, 3) |> ignore
                i <- i + 3

            sb.ToString()

    // ---- Number formats -------------------------------------------------------------------------

    /// One section of a custom number format, in the one shape the engine does: literal text, digit
    /// placeholders with at most one decimal point, an exponent, literal text.
    type Section = {
        Prefix: string
        Suffix: string
        /// `0` and `#` left of the decimal point.
        IntZeros: int
        IntHashes: int
        /// A thousand separator between digit placeholders.
        Grouping: bool
        /// Thousand separators right before the decimal point: each divides by 1000.
        Scaling: int
        Point: bool
        DecZeros: int
        DecHashes: int
        Percent: bool
        /// "E" or "e", whether a plus sign is shown, the least number of digits.
        Exponent: (string * bool * int) option
    } with

        member this.HasPlaceholders =
            this.IntZeros + this.IntHashes + this.DecZeros + this.DecHashes > 0

    /// The sections of a format: split at the semicolons that are not quoted or escaped.
    let private splitSections (format: string) : string list =
        let out = ResizeArray<string>()
        let sb = StringBuilder()
        let mutable i = 0
        let mutable quoted = false

        while i < format.Length do
            let ch = format.[i]

            if quoted then
                sb.Append ch |> ignore
                quoted <- ch <> '"'
            elif ch = '"' then
                sb.Append ch |> ignore
                quoted <- true
            elif ch = '\\' && i + 1 < format.Length then
                sb.Append(ch).Append(format.[i + 1]) |> ignore
                i <- i + 1
            elif ch = ';' then
                out.Add(sb.ToString())
                sb.Clear() |> ignore
            else
                sb.Append ch |> ignore

            i <- i + 1

        out.Add(sb.ToString())
        List.ofSeq out

    /// A character shown as it is. "- + $ ( )" are literal; "To display a character other than one
    /// of those listed, precede it with a backslash (\) or enclose it in double quotation marks"
    /// (Microsoft Learn). DAX Guide's examples show more of them unquoted ("$#,0,.0#K", "0:"), so
    /// they are taken literally too, but not the date and time characters ("a, c, d, h, m, n, p, q,
    /// s, t, w, y") other than the M and T those examples show, nor the string characters
    /// "@, &, <, >, and !".
    let private checkLiteral (ch: char) : unit =
        match ch with
        | '@'
        | '&'
        | '<'
        | '>'
        | '!' -> refuse $"the character {ch} in a number format (put it in quotes to show it)"
        | 'a'
        | 'c'
        | 'd'
        | 'h'
        | 'n'
        | 'p'
        | 'q'
        | 's'
        | 'w'
        | 'y'
        | 'e'
        | 'A'
        | 'C'
        | 'D'
        | 'H'
        | 'N'
        | 'P'
        | 'Q'
        | 'S'
        | 'W'
        | 'Y'
        | 'E' ->
            refuse
                $"the letter {ch} in a number format (it is a date, time or exponent character: put it in quotes to show it)"
        | _ -> ()

    let parseSection (text: string) : Section =
        let prefix = StringBuilder()
        let suffix = StringBuilder()
        let mutable intZeros = 0
        let mutable intHashes = 0
        let mutable commas = 0 // since the last digit placeholder
        let mutable grouping = false
        let mutable point = false
        let mutable decZeros = 0
        let mutable decHashes = 0
        let mutable percent = false
        let mutable exponent = None
        // 0 prefix, 1 integer digits, 2 decimals, 3 suffix
        let mutable state = 0
        let mutable i = 0

        let literal (sb: StringBuilder) =
            match text.[i] with
            | '\\' ->
                if i + 1 >= text.Length then
                    refuse "a format that ends in a backslash"

                sb.Append text.[i + 1] |> ignore
                i <- i + 2
            | '"' ->
                let close = text.IndexOf('"', i + 1)

                if close < 0 then
                    refuse "a format with a quotation mark that is not closed"

                sb.Append(text, i + 1, close - i - 1) |> ignore
                i <- close + 1
            | '%' ->
                if percent then
                    refuse "a number format with more than one %"

                percent <- true
                sb.Append '%' |> ignore
                i <- i + 1
            | '0'
            | '#'
            | '.'
            | ',' when state = 3 -> refuse "a number format with digit placeholders in more than one place"
            | ',' -> refuse "a number format with a thousand separator before the first digit placeholder"
            | ch ->
                checkLiteral ch
                sb.Append ch |> ignore
                i <- i + 1

        /// `E+00` at `i`: consumed if it is one.
        let tryExponent () : bool =
            let ch = text.[i]

            if
                (ch = 'E' || ch = 'e')
                && i + 2 < text.Length
                && (text.[i + 1] = '+' || text.[i + 1] = '-')
                && text.[i + 2] = '0'
            then
                let mutable j = i + 2

                while j < text.Length && text.[j] = '0' do
                    j <- j + 1

                if j < text.Length && text.[j] = '#' then
                    refuse "a scientific format with # in the exponent"

                exponent <- Some(string ch, text.[i + 1] = '+', j - i - 2)
                i <- j
                true
            else
                false

        while i < text.Length do
            let ch = text.[i]

            match state with
            | 0 ->
                match ch with
                | '0'
                | '#' -> state <- 1
                | '.' -> state <- 1
                | _ -> literal prefix
            | 1 ->
                match ch with
                | '0' ->
                    if commas > 0 then
                        grouping <- true
                        commas <- 0

                    intZeros <- intZeros + 1
                    i <- i + 1
                | '#' ->
                    if intZeros > 0 then
                        refuse "a number format with # to the right of 0 before the decimal point"

                    if commas > 0 then
                        grouping <- true
                        commas <- 0

                    intHashes <- intHashes + 1
                    i <- i + 1
                | ',' ->
                    if intZeros + intHashes = 0 then
                        refuse "a number format with a thousand separator before the first digit placeholder"

                    commas <- commas + 1
                    i <- i + 1
                | '.' ->
                    point <- true
                    state <- 2
                    i <- i + 1
                | _ ->
                    if not (tryExponent ()) then
                        ()

                    state <- 3
            | 2 ->
                match ch with
                | '0' ->
                    if decHashes > 0 then
                        refuse "a number format with 0 to the right of # after the decimal point"

                    decZeros <- decZeros + 1
                    i <- i + 1
                | '#' ->
                    decHashes <- decHashes + 1
                    i <- i + 1
                | '.' -> refuse "a number format with two decimal points"
                | ',' -> refuse "a number format with a thousand separator after the decimal point"
                | _ ->
                    if not (tryExponent ()) then
                        ()

                    state <- 3
            | _ -> literal suffix

        let section = {
            Prefix = prefix.ToString()
            Suffix = suffix.ToString()
            IntZeros = intZeros
            IntHashes = intHashes
            Grouping = grouping
            // "Two adjacent thousand separators or a thousand separator immediately to the left of
            // the decimal separator (whether or not a decimal is specified) means "scale the number
            // by dividing it by 1000, rounding as needed.""
            Scaling = commas
            Point = point
            DecZeros = decZeros
            DecHashes = decHashes
            Percent = percent
            Exponent = exponent
        }

        if
            exponent.IsSome
            && (intZeros <> 1 || intHashes <> 0 || grouping || commas > 0 || percent)
        then
            refuse "a scientific format other than one 0 before the decimal point (0.00E+00)"

        section

    /// A custom number format: one to three sections. "The first section applies to positive values
    /// and zeros, the second to negative values"; with three, "the third to zeros"; "If you include
    /// semicolons with nothing between them, the missing section is defined using the format of the
    /// positive value" (Microsoft Learn).
    let parseNumberFormat (format: string) : Section option list =
        let texts = splitSections format

        if texts.Length > 3 then
            refuse "a number format with more than three sections"

        let sections =
            texts |> List.map (fun t -> if t = "" then None else Some(parseSection t))

        match sections with
        | Some first :: _ when first.HasPlaceholders -> sections
        | _ ->
            refuse
                "a format without a digit placeholder (0 or #) in its first section, for a number (whether it is read as a date format is not documented)"

    type private Rendered = {
        Text: string
        Tie: bool
        /// Every digit shown is 0.
        Zero: bool
    }

    let private render (s: Section) (value: Dec) : Rendered =
        if not s.HasPlaceholders then
            {
                Text = s.Prefix + s.Suffix
                Tie = false
                Zero = false
            }
        else
            let decimals = s.DecZeros + s.DecHashes

            // "Percentage placeholder. The expression is multiplied by 100."
            let value = {
                value with
                    Scale = value.Scale - (if s.Percent then 2 else 0) + 3 * s.Scaling
            }

            let trim (fraction: string) =
                let mutable n = fraction.Length

                while n > s.DecZeros && fraction.[n - 1] = '0' do
                    n <- n - 1

                fraction.Substring(0, n)

            let withPoint (whole: string) (fraction: string) =
                if not s.Point then
                    whole
                elif fraction = "" then
                    // "0." or "#.##" of a whole number: Visual Basic leaves the separator, .NET
                    // doesn't, and the pages don't show it.
                    refuse
                        "a number with no decimals to show for a format with a decimal point (whether the point stays is not documented)"
                else
                    whole + "." + fraction

            match s.Exponent with
            | None ->
                let q, tie = roundTo value decimals
                let digits = (digitsOf q).PadLeft(decimals + 1, '0')
                let whole = digits.Substring(0, digits.Length - decimals).TrimStart('0')
                let fraction = trim (digits.Substring(digits.Length - decimals))
                let whole = whole.PadLeft(s.IntZeros, '0')
                let whole = if s.Grouping then group whole else whole

                {
                    Text = s.Prefix + withPoint whole fraction + s.Suffix
                    Tie = tie
                    Zero = q.IsZero
                }
            | Some(letter, plus, width) ->
                // One digit, the decimals, and the power of ten that makes it so.
                let mantissa, tie, power =
                    if value.Mant.IsZero then
                        BigInteger.Zero, false, 0
                    else
                        let length = (digitsOf value.Mant).Length
                        let power = length - 1 - value.Scale

                        let q, tie =
                            roundTo
                                {
                                    Mant = value.Mant
                                    Scale = length - 1
                                }
                                decimals

                        // 9.996 rounds to 10.00: one more power of ten.
                        if (digitsOf q).Length > decimals + 1 then
                            q / BigInteger 10, tie, power + 1
                        else
                            q, tie, power

                let digits = (digitsOf mantissa).PadLeft(decimals + 1, '0')
                let fraction = trim (digits.Substring 1)

                // "Use E- or e- to place a minus sign next to negative exponents. Use E+ or e+ to
                // place a minus sign next to negative exponents and a plus sign next to positive
                // exponents."
                let sign =
                    if power < 0 then "-"
                    elif plus then "+"
                    else ""

                let exponent = (digitsOf (BigInteger(abs power))).PadLeft(width, '0')

                {
                    Text =
                        s.Prefix
                        + withPoint (digits.Substring(0, 1)) fraction
                        + letter
                        + sign
                        + exponent
                        + s.Suffix
                    Tie = tie
                    Zero = mantissa.IsZero
                }

    /// A number through a custom format.
    let private formatWith (sections: Section option list) (v: DaxValue) : string =
        let negative, all = readings v
        let isZero = all.Head.Mant.IsZero
        let first = sections.Head.Value

        let section (n: int) =
            sections |> List.tryItem n |> Option.flatten

        let chosen, sign =
            if isZero then
                (section 2 |> Option.defaultValue first), ""
            elif negative then
                match section 1 with
                | Some s -> s, ""
                | None ->
                    // FORMAT ( -1980.1, "#,0.00" ) is "-1,980.10" (dax.guide/format). With text
                    // before the digits ("$#,0.00") where the sign goes is not shown.
                    if first.Prefix <> "" then
                        refuse
                            "a negative number with a format that starts with literal text and has no section for negatives (where the minus sign goes is not documented)"

                    first, "-"
            else
                first, ""

        let results = all |> List.map (render chosen)
        let one = results.Head

        if results |> List.exists (fun r -> r.Text <> one.Text || r.Tie <> one.Tie) then
            refuse
                "a number that a float holds inexactly, with a format that shows the digits where its decimal readings differ"

        // A positive number halfway rounds up: FORMAT ( 10345, "$#,0,.0#K" ) is "$10.35K"
        // (dax.guide/format). Nothing shows a negative one.
        if one.Tie && negative then
            refuse "a negative number exactly halfway between two results"

        if one.Zero && not isZero then
            if negative then
                refuse "a negative number that rounds to zero (whether it keeps its sign is not documented)"
            elif (section 2).IsSome then
                refuse
                    "a number that rounds to zero with a format that has a section for zero (which section applies is not documented)"

        sign + one.Text

    /// A number written out in full, without an exponent.
    let private plain (d: Dec) : string =
        let digits = digitsOf d.Mant

        if d.Scale <= 0 then
            if d.Mant.IsZero then
                "0"
            else
                digits + String('0', -d.Scale)
        else
            let digits = digits.PadLeft(d.Scale + 1, '0')
            let whole = digits.Substring(0, digits.Length - d.Scale)
            let fraction = digits.Substring(digits.Length - d.Scale).TrimEnd('0')
            if fraction = "" then whole else whole + "." + fraction

    /// "General Number": "Displays number with no thousand separators"; FORMAT ( 12345.67,
    /// "General Number" ) is "12345.67".
    let private generalNumber (v: DaxValue) : string =
        let negative, all = readings v

        let text =
            match v, all with
            | DaxValue.Decimal d, [ fifteen; shortest; _ ] ->
                if plain fifteen <> plain shortest then
                    refuse
                        "General Number of a number with more than 15 significant digits (how many are shown is not documented)"

                let a = abs d

                if a <> 0.0 && (a >= 1.0e15 || a < 1.0e-4) then
                    refuse
                        "General Number of a very large or very small number (whether it is written with an exponent is not documented)"

                plain fifteen
            | _ -> plain all.Head

        if negative && text <> "0" then "-" + text else text

    let private numberNames = [
        "General Number"
        "Currency"
        "Fixed"
        "Standard"
        "Percent"
        "Scientific"
        "Yes/No"
        "True/False"
        "On/Off"
    ]

    let private dateNames = [
        "General Date"
        "Long Date"
        "Medium Date"
        "Short Date"
        "Long Time"
        "Medium Time"
        "Short Time"
    ]

    /// A name that is a predefined format but for its case. The pages write the names one way and
    /// say that other strings "are handled as part of a custom format string", so whether
    /// "currency" is the predefined format is not known.
    let private almostNamed (format: string) : string option =
        numberNames @ dateNames
        |> List.tryFind (fun n ->
            String.Equals(n, format.Trim(), StringComparison.OrdinalIgnoreCase)
            && not (String.Equals(n, format, StringComparison.Ordinal)))

    let private isNonZero (v: DaxValue) =
        match v with
        | DaxValue.Integer i -> i <> 0L
        | DaxValue.Currency c -> c <> 0L
        | DaxValue.Decimal d -> d <> 0.0
        | DaxValue.Boolean b -> b
        | _ -> false

    /// "Displays No if number is 0; otherwise, displays Yes", and so on (Microsoft Learn).
    let private yesNo (format: string) (v: DaxValue) : string option =
        match format with
        | "Yes/No" -> Some(if isNonZero v then "Yes" else "No")
        | "True/False" -> Some(if isNonZero v then "True" else "False")
        | "On/Off" -> Some(if isNonZero v then "On" else "Off")
        | _ -> None

    /// A number through a format string; None is the BLANK format.
    let formatNumber (format: string option) (v: DaxValue) : string =
        match format with
        // "If format_string is BLANK, the value is formatted with a "General Number" or "General
        // Date" format"; an empty format is the table's "None: Display the number with no
        // formatting."
        | None
        | Some ""
        | Some "General Number" -> generalNumber v
        | Some name when (yesNo name v).IsSome -> (yesNo name v).Value
        | Some "Currency" ->
            // "$12,345.67". The negative form is the system's (parentheses or a minus sign).
            if fst (readings v) then
                refuse "Currency of a negative number (the form is the system's and not documented)"

            formatWith (parseNumberFormat "$#,##0.00") v
        // "Displays at least one digit to the left and two digits to the right of the decimal
        // separator."
        | Some "Fixed" -> formatWith (parseNumberFormat "0.00") v
        | Some "Standard" -> formatWith (parseNumberFormat "#,##0.00") v
        | Some "Percent" ->
            // "Displays number multiplied by 100 with a percent sign (%) appended immediately to
            // the right; always displays two digits to the right of the decimal separator":
            // FORMAT ( 0.742, "Percent" ) is "74.20%".
            let text = formatWith (parseNumberFormat "0.00%") v

            if text.TrimStart('-').Length > "999.99%".Length then
                refuse
                    "Percent of a number that needs a thousand separator (1,234,567.00 % in Microsoft Learn's example, 1234567.00% in DAX Guide's)"

            text
        // "1.23E+04".
        | Some "Scientific" -> formatWith (parseNumberFormat "0.00E+00") v
        | Some name when List.contains name dateNames ->
            refuse $"the date format {name} for a number (the pages don't show a number as a date)"
        | Some custom ->
            match almostNamed custom with
            | Some name -> refuse $"the format \"{custom}\" (the predefined format is written \"{name}\")"
            | None -> formatWith (parseNumberFormat custom) v

    // ---- Date and time formats ------------------------------------------------------------------

    let private monthNames = [|
        "January"
        "February"
        "March"
        "April"
        "May"
        "June"
        "July"
        "August"
        "September"
        "October"
        "November"
        "December"
    |]

    let private dayNames = [|
        "Sunday"
        "Monday"
        "Tuesday"
        "Wednesday"
        "Thursday"
        "Friday"
        "Saturday"
    |]

    [<RequireQualifiedAccess>]
    type DateToken =
        | Literal of string
        /// A run of one of d, m, y, h, n, s, q, w, written in either case: the letter in lower case
        /// and how many.
        | Field of char * int
        /// An `m` or `mm` that follows the hour: minutes.
        | Minute of int
        /// The texts for before and after noon.
        | AmPm of string * string
        | DateSeparator
        | TimeSeparator

    let parseDateFormat (format: string) : DateToken list =
        let out = ResizeArray<DateToken>()
        let mutable i = 0
        // Where an m or mm written with an upper-case M is, and whether an upper-case H is used.
        let upperMonth = Collections.Generic.HashSet<int>()
        let mutable upperHour = false

        // "M", "D" and "Y" alone were predefined formats in the 2010 DAX pages ("Displays the month
        // and the day of a date. For example, March 12."; the long date; "March, 2008").
        match format with
        | "D"
        | "M"
        | "Y" ->
            refuse
                $"the format \"{format}\" (a predefined format in the 2010 DAX pages, the letter {format.ToLowerInvariant()} in Visual Basic: write it in lower case)"
        | _ -> ()

        let startsWith (s: string) =
            String.CompareOrdinal(format, i, s, 0, s.Length) = 0

        while i < format.Length do
            let ch = format.[i]

            match ch with
            | '\\' ->
                if i + 1 >= format.Length then
                    refuse "a format that ends in a backslash"

                out.Add(DateToken.Literal(string format.[i + 1]))
                i <- i + 2
            | '"' ->
                let close = format.IndexOf('"', i + 1)

                if close < 0 then
                    refuse "a format with a quotation mark that is not closed"

                out.Add(DateToken.Literal(format.Substring(i + 1, close - i - 1)))
                i <- close + 1
            | '/' ->
                out.Add DateToken.DateSeparator
                i <- i + 1
            | ':' ->
                out.Add DateToken.TimeSeparator
                i <- i + 1
            | 'a'
            | 'A' ->
                // "AM/PM", "am/pm", "A/P", "a/p", and "AMPM", which "can be either uppercase or
                // lowercase" and shows the system's texts: AM and PM in en-US.
                if startsWith "AM/PM" then
                    out.Add(DateToken.AmPm("AM", "PM"))
                    i <- i + 5
                elif startsWith "am/pm" then
                    out.Add(DateToken.AmPm("am", "pm"))
                    i <- i + 5
                elif startsWith "AMPM" || startsWith "ampm" then
                    out.Add(DateToken.AmPm("AM", "PM"))
                    i <- i + 4
                elif startsWith "A/P" then
                    out.Add(DateToken.AmPm("A", "P"))
                    i <- i + 3
                elif startsWith "a/p" then
                    out.Add(DateToken.AmPm("a", "p"))
                    i <- i + 3
                else
                    refuse
                        "the letter a in a date format, other than AM/PM, am/pm, A/P, a/p or AMPM (put it in quotes to show it)"
            | _ when Char.IsAsciiLetter ch ->
                let letter = Char.ToLowerInvariant ch
                let mutable j = i

                while j < format.Length && Char.ToLowerInvariant format.[j] = letter do
                    j <- j + 1

                let run = j - i
                let text = format.Substring(i, run)
                let hasUpper = text <> String(letter, run)

                // Upper case: see "Upper-case letters" in the module's header. The letter is read
                // as its lower case, except where the 2010 DAX pages give it another meaning.
                if hasUpper then
                    match letter with
                    | 'y' when run = 1 ->
                        refuse
                            "the letter Y alone in a date format (the day of the year in Visual Basic, the year in the 2010 DAX pages: use y for the day of the year)"
                    | 'm' when run <= 2 -> upperMonth.Add out.Count |> ignore
                    | 'h' -> upperHour <- true
                    | _ -> ()

                let ok =
                    match letter with
                    | 'd' ->
                        if run = 5 then
                            refuse
                                "the date format ddddd (06/25/2020 in Microsoft Learn's en-US examples, 1/9/2020 in DAX Guide's)"

                        run <= 4 || run = 6
                    | 'm' -> run <= 4
                    | 'y' -> run = 1 || run = 2 || run = 4
                    | 'h'
                    | 'n'
                    | 's'
                    | 'w' -> run <= 2
                    | 'q' -> run = 1
                    | 'c' ->
                        refuse
                            "the date format c (06/25/2020 13:23:45 in Microsoft Learn's en-US examples, 1/2/2020 3:30:00 PM in DAX Guide's)"
                    | 't' ->
                        refuse
                            "the time format ttttt (13:23:45 in Microsoft Learn's en-US examples, against the 12-hour Long Time)"
                    | _ ->
                        refuse
                            $"the letter {ch} in a date format (it is not one of the date and time characters: put it in quotes to show it)"

                if not ok then
                    refuse $"{run} times the letter {ch} in a date format"

                out.Add(DateToken.Field(letter, run))
                i <- j
            | '0'
            | '#' -> refuse "a number format for a date (the pages don't show a date as a number)"
            | '%'
            | '@'
            | '&'
            | '<'
            | '>'
            | '!'
            | ';' -> refuse $"the character {ch} in a date format (put it in quotes to show it)"
            | _ ->
                out.Add(DateToken.Literal(string ch))
                i <- i + 1

        // "If m immediately follows h or hh, minute rather than the month is displayed", and
        // FORMAT ( DT, "hh:mm:ss" ) is "15:30:00" (dax.guide/format): the time separator may come
        // between. Anything else between them, or an m next to seconds with no hour before it, is
        // not settled by the pages.
        let tokens = out.ToArray()

        // An upper-case H is the hour of the 24-hour clock in the 2010 DAX pages ("Displays the hour
        // ... using the 24-hour clock"), and h in Visual Basic, which AM/PM turns to the 12-hour clock.
        if
            upperHour
            && tokens
               |> Array.exists (fun t ->
                   match t with
                   | DateToken.AmPm _ -> true
                   | _ -> false)
        then
            refuse
                "an upper-case H with AM/PM in a date format (the 24-hour clock in the 2010 DAX pages, the 12-hour clock in Visual Basic: write h)"

        let isLiteral (t: DateToken) =
            match t with
            | DateToken.Literal _
            | DateToken.DateSeparator
            | DateToken.TimeSeparator -> true
            | _ -> false

        tokens
        |> Array.mapi (fun at token ->
            match token with
            | DateToken.Field('m', run) when run <= 2 ->
                let mutable near = at - 1

                while near >= 0 && tokens.[near] = DateToken.TimeSeparator do
                    near <- near - 1

                let mutable far = at - 1

                while far >= 0 && isLiteral tokens.[far] do
                    far <- far - 1

                let mutable next = at + 1

                while next < tokens.Length && tokens.[next] = DateToken.TimeSeparator do
                    next <- next + 1

                let isHour (n: int) =
                    n >= 0
                    && (match tokens.[n] with
                        | DateToken.Field('h', _) -> true
                        | _ -> false)

                if isHour near && upperMonth.Contains at then
                    // "The M character displays the month in a user-defined date format. The m
                    // character displays the minutes" (the 2010 DAX pages); minutes in Visual Basic.
                    refuse
                        "an upper-case M after h in a date format (the month in the 2010 DAX pages, minutes in Visual Basic: use n for minutes)"
                elif isHour near then
                    DateToken.Minute run
                elif isHour far then
                    refuse
                        "m after h with other text between them in a date format (minutes or the month is not documented: use n for minutes)"
                elif
                    next < tokens.Length
                    && (match tokens.[next] with
                        | DateToken.Field('s', _) -> true
                        | _ -> false)
                then
                    refuse
                        "m before s with no hour before it in a date format (minutes or the month is not documented: use n for minutes)"
                else
                    token
            | _ -> token)
        |> List.ofArray

    let private two (n: int) =
        (digitsOf (BigInteger n)).PadLeft(2, '0')

    let private num (n: int) = digitsOf (BigInteger n)

    /// "The first date officially supported by DAX is March 1, 1900. Date calculations executed on
    /// time periods prior to that date might lead to errors and should be considered as inaccurate"
    /// (dax.guide/dt/datetime).
    let private firstDate = 61.0

    let private renderDate (tokens: DateToken list) (serial: float) : string =
        let dt =
            match DaxValue.dateTimeOfSerial serial with
            | Some dt when serial >= 0.0 -> dt
            | _ -> refuse "a date before December 30, 1899 or after the year 9999"

        let isTime (t: DateToken) =
            match t with
            | DateToken.Field(('h' | 'n' | 's'), _)
            | DateToken.Minute _
            | DateToken.AmPm _ -> true
            | _ -> false

        let isDate (t: DateToken) =
            match t with
            | DateToken.Field(('d' | 'm' | 'y' | 'q' | 'w'), _) -> true
            | _ -> false

        if tokens |> List.exists isDate && serial < firstDate then
            refuse "the date of a value before March 1, 1900 (DAX is not reliable before it)"

        if tokens |> List.exists isTime && dt.Millisecond <> 0 then
            refuse "a time with a fraction of a second (whether it is rounded or cut is not documented)"

        // "Use the 12-hour clock" when the format has one of the AM/PM forms.
        let twelve =
            tokens
            |> List.exists (fun t ->
                match t with
                | DateToken.AmPm _ -> true
                | _ -> false)

        let hour =
            if not twelve then dt.Hour
            elif dt.Hour % 12 = 0 then 12
            else dt.Hour % 12

        let weekday = int dt.DayOfWeek
        let sb = StringBuilder()

        for t in tokens do
            let text =
                match t with
                | DateToken.Literal s -> s
                | DateToken.DateSeparator -> "/"
                | DateToken.TimeSeparator -> ":"
                | DateToken.AmPm(am, pm) -> if dt.Hour < 12 then am else pm
                | DateToken.Minute 1 -> num dt.Minute
                | DateToken.Minute _ -> two dt.Minute
                | DateToken.Field('d', 1) -> num dt.Day
                | DateToken.Field('d', 2) -> two dt.Day
                | DateToken.Field('d', 3) -> dayNames.[weekday].Substring(0, 3)
                | DateToken.Field('d', 4) -> dayNames.[weekday]
                // "The default long date format is dddd, mmmm d, yyyy": "Thursday, June 25, 2020".
                | DateToken.Field('d', _) ->
                    $"{dayNames.[weekday]}, {monthNames.[dt.Month - 1]} {num dt.Day}, {num dt.Year}"
                | DateToken.Field('m', 1) -> num dt.Month
                | DateToken.Field('m', 2) -> two dt.Month
                | DateToken.Field('m', 3) -> monthNames.[dt.Month - 1].Substring(0, 3)
                | DateToken.Field('m', _) -> monthNames.[dt.Month - 1]
                // "Display the day of the year as a number (1-366)."
                | DateToken.Field('y', 1) -> num dt.DayOfYear
                | DateToken.Field('y', 2) -> two (dt.Year % 100)
                | DateToken.Field('y', _) -> (num dt.Year).PadLeft(4, '0')
                | DateToken.Field('h', 1) -> num hour
                | DateToken.Field('h', _) -> two hour
                | DateToken.Field('n', 1) -> num dt.Minute
                | DateToken.Field('n', _) -> two dt.Minute
                | DateToken.Field('s', 1) -> num dt.Second
                | DateToken.Field('s', _) -> two dt.Second
                | DateToken.Field('q', _) -> num ((dt.Month - 1) / 3 + 1)
                // "Display the day of the week as a number (1 for Sunday through 7 for Saturday)."
                | DateToken.Field('w', 1) -> num (weekday + 1)
                // "Display the week of the year as a number (1-54)": the week January 1 is in is
                // the first, which is what allows 54. The one example (26 for Thursday, June 25,
                // 2020) doesn't say whether a week starts on Sunday or on Monday, so a Sunday, the
                // one day on which the two differ, is refused.
                | DateToken.Field('w', _) ->
                    if weekday = 0 then
                        refuse
                            "the week of the year (ww) of a Sunday (whether weeks start on Sunday or on Monday is not documented)"

                    let first = int (DateTime(dt.Year, 1, 1).DayOfWeek)
                    num ((dt.DayOfYear - 1 + first) / 7 + 1)
                | DateToken.Field _ -> refuse "a date format"

            sb.Append text |> ignore

        sb.ToString()

    /// A date through a format string; None is the BLANK format.
    let formatDate (format: string option) (serial: float) : string =
        let custom (f: string) = renderDate (parseDateFormat f) serial

        match format with
        | None
        | Some "General Date" ->
            // "Displays a date and/or time. For example, 3/12/2008 11:07:31 AM"; "1/2/2021
            // 3:30:00 PM". What it shows of a date with no time, or of a time alone, is said of
            // "c" only, on which the pages disagree.
            match DaxValue.dateTimeOfSerial serial with
            | Some dt when serial >= 1.0 && dt.TimeOfDay <> TimeSpan.Zero -> custom "m/d/yyyy h:nn:ss AM/PM"
            | Some _ when serial >= 0.0 ->
                refuse
                    "General Date of a date with no time, or of a time alone (the pages show it with both; whether the missing part is left out is said of \"c\" only)"
            | _ -> refuse "a date before December 30, 1899 or after the year 9999"
        // "Saturday, January 2, 2021".
        | Some "Long Date" -> custom "dddd, mmmm d, yyyy"
        | Some "Medium Date" ->
            refuse "Medium Date (the long date according to Microsoft Learn, 02-Jan-21 in DAX Guide's example)"
        // "1/2/2021".
        | Some "Short Date" -> custom "m/d/yyyy"
        // "3:30:00 PM".
        | Some "Long Time" -> custom "h:nn:ss AM/PM"
        // "Displays a time in 12 hour format": "03:30 PM" (dax.guide/format).
        | Some "Medium Time" -> custom "hh:nn AM/PM"
        | Some "Short Time" ->
            // "Displays a time in 24 hour format. For example, 11:07": "15:30". No example shows an
            // hour below 10, with or without a leading zero.
            match DaxValue.dateTimeOfSerial serial with
            | Some dt when dt.Hour < 10 ->
                refuse "Short Time of an hour before 10 (a leading zero or not is not documented)"
            | _ -> custom "hh:nn"
        | Some "" -> refuse "an empty format for a date"
        | Some name when List.contains name numberNames ->
            refuse $"the number format {name} for a date (the pages don't show a date as a number)"
        | Some f ->
            match almostNamed f with
            | Some name -> refuse $"the format \"{f}\" (the predefined format is written \"{name}\")"
            | None -> custom f

    /// FORMAT of a value, in en-US.
    let format (formatString: string option) (v: DaxValue) : string =
        match v with
        // "If value is BLANK, the function returns an empty string."
        | DaxValue.Blank -> ""
        | DaxValue.String _ -> refuse "FORMAT of a text value (the pages show numbers and dates only)"
        | DaxValue.Boolean _ ->
            // FORMAT ( TRUE, "True/False" ) is "True", FORMAT ( FALSE, "On/Off" ) is "Off"
            // (dax.guide/format).
            match formatString |> Option.bind (fun f -> yesNo f v) with
            | Some text -> text
            | None -> refuse "FORMAT of TRUE or FALSE with a format other than Yes/No, True/False or On/Off"
        | DaxValue.DateTime serial -> formatDate formatString serial
        | _ -> formatNumber formatString v

    /// What is wrong with a format string whatever the value, if anything: it is neither a number
    /// format nor a date format the engine does.
    let checkFormat (formatString: string) : string option =
        let attempt (f: unit -> unit) =
            try
                f ()
                None
            with FormatRefused why ->
                Some why

        if formatString = "Medium Date" then
            Some "Medium Date (the long date according to Microsoft Learn, 02-Jan-21 in DAX Guide's example)"
        elif List.contains formatString (numberNames @ dateNames) then
            None
        else
            match almostNamed formatString with
            | Some name -> Some $"the format \"{formatString}\" (the predefined format is written \"{name}\")"
            | None ->
                match attempt (fun () -> parseNumberFormat formatString |> ignore) with
                | None -> None
                | Some asNumber ->
                    match attempt (fun () -> parseDateFormat formatString |> ignore) with
                    | None -> None
                    // Say what is wrong with it as the kind of format it looks like.
                    | Some asDate ->
                        if formatString.Contains '0' || formatString.Contains '#' then
                            Some asNumber
                        else
                            Some asDate

    // ---- FIXED ----------------------------------------------------------------------------------

    /// FIXED: "A number represented as text"; "If the value used for the decimals parameter is
    /// negative, number is rounded to the left of the decimal point" (Microsoft Learn);
    /// FIXED ( 384518.16, -2, FALSE ) is "384,500" (dax.guide/fixed).
    let fixedText (v: DaxValue) (decimals: int) (commas: bool) : string =
        let negative, all = readings v

        if negative then
            refuse "FIXED of a negative number (the pages show none)"

        let one (d: Dec) =
            let q, tie = roundTo d decimals

            let text =
                if decimals > 0 then
                    let digits = (digitsOf q).PadLeft(decimals + 1, '0')
                    let whole = digits.Substring(0, digits.Length - decimals)
                    let whole = if commas then group whole else whole
                    whole + "." + digits.Substring(digits.Length - decimals)
                else
                    let whole =
                        if q.IsZero then
                            "0"
                        else
                            digitsOf q + String('0', -decimals)

                    if commas then group whole else whole

            text, tie

        let results = all |> List.map one

        if results |> List.exists (fun r -> r <> results.Head) then
            refuse "FIXED of a number that a float holds inexactly, at the digits where its decimal readings differ"

        // ROUND rounds a positive half up, but no page shows FIXED doing it.
        if snd results.Head then
            refuse "FIXED of a number exactly halfway between two results"

        fst results.Head

    let private optional (args: Expr list) (n: int) : Expr option =
        match List.tryItem n args with
        | Some e when not (Fn.isOmitted e) -> Some e
        | _ -> None

    let private literalText (e: Expr) : string option =
        match (Fn.unparen e).Kind with
        | StringLiteral s -> Some s
        | _ -> None

    let functions: DaxFunction list = [
        // FORMAT ( <value>, <format_string> [, <locale_name>] ): "A string containing value formatted
        // as defined by format_string"; "The result is always a string, even when the value is
        // blank" (dax.guide/format).
        Fn.scalar "FORMAT" "Text" (2, 3) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let v = ev.Scalar(ctx, call.Args.[0])

            let formatString =
                match ev.Scalar(ctx, call.Args.[1]) with
                | DaxValue.Blank -> None
                | DaxValue.String s -> Some s
                | _ -> Fail.notSupported ctx call.Args.[1].Span "FORMAT with a format that is not text"

            // "Predefined format strings use the model culture property when formatting the
            // result"; the locale argument names another.
            let culture =
                match optional call.Args 2 with
                | None -> ctx.Model.Culture
                | Some e ->
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Blank -> ctx.Model.Culture
                    | DaxValue.String "" -> Fail.notSupported ctx e.Span "FORMAT with an empty locale name"
                    | DaxValue.String s -> s
                    | _ -> Fail.notSupported ctx e.Span "FORMAT with a locale name that is not text"

            match v with
            | DaxValue.Blank -> DaxValue.String ""
            | _ ->
                Culture.requireEnUs ctx call.Span culture "FORMAT"

                try
                    DaxValue.String(format formatString v)
                with FormatRefused why ->
                    Fail.notSupported ctx call.Span $"FORMAT: {why}")
        |> Fn.withCheck (fun args ->
            let locale =
                match List.tryItem 2 args |> Option.bind literalText with
                | Some name when name <> "" && not (Culture.isEnUs name) ->
                    Some $"FORMAT in the culture {name.Trim()} (only en-US is implemented)"
                | _ -> None

            match locale with
            | Some _ -> locale
            | None ->
                match List.tryItem 1 args |> Option.bind literalText with
                | Some f -> checkFormat f |> Option.map (fun why -> $"FORMAT: {why}")
                | None -> None)

        // FIXED ( <number>, <decimals>, <no_commas> ): "decimals (optional) The number of digits to
        // the right of the decimal point; if omitted, 2"; "no_commas (optional) A logical value: if
        // 1, don't display commas in the returned text; if 0 or omitted, display commas"
        // (Microsoft Learn).
        Fn.scalar "FIXED" "Text" (1, 3) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let v =
                match ev.Scalar(ctx, call.Args.[0]) with
                | DaxValue.Integer _
                | DaxValue.Decimal _
                | DaxValue.Currency _ as v -> v
                | DaxValue.Blank -> Fail.notSupported ctx call.Args.[0].Span "FIXED of BLANK"
                | _ -> Fail.notSupported ctx call.Args.[0].Span "FIXED of a value that is not a number"

            let decimals =
                match optional call.Args 1 with
                | None -> 2
                | Some e ->
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Integer i when i >= -20L && i <= 20L -> int i
                    | _ ->
                        Fail.notSupported ctx e.Span "FIXED with a number of decimals that is not a small whole number"

            let commas =
                match optional call.Args 2 with
                | None -> true
                | Some e ->
                    match ev.Scalar(ctx, e) with
                    | DaxValue.Boolean b -> not b
                    | DaxValue.Integer 0L -> true
                    | DaxValue.Integer 1L -> false
                    | _ -> Fail.notSupported ctx e.Span "FIXED with a no_commas that is not TRUE, FALSE, 1 or 0"

            Culture.requireModel ctx call.Span "FIXED"

            try
                DaxValue.String(fixedText v decimals commas)
            with FormatRefused why ->
                Fail.notSupported ctx call.Span why)
    ]
