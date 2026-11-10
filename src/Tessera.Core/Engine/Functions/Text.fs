namespace Tessera.DaxEngine.Functions

open System
open System.Globalization
open System.Text
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

/// The text functions. Each rule is quoted from Microsoft Learn (learn.microsoft.com/en-us/dax/
/// <name>-function-dax) or DAX Guide (dax.guide/<name>); where the two disagree or neither says, the
/// function refuses rather than guess.
///
/// Common to all of them:
///   - A BLANK argument read as text is the empty text: "a blank value is always converted to 0 or
///     to an empty string, depending on the data type requested by the expression" (SQLBI,
///     "Handling BLANK in DAX"). What a function that RETURNS text gives for a BLANK text (BLANK or
///     the empty text) is not said anywhere, so those refuse a BLANK text.
///   - A number given for a text is its text (`Convert.toText`); a date or a Boolean is refused.
///   - Positions and lengths count UTF-16 code units, and "This function returns different results
///     depending on the UnicodeCharacterBehavior setting of your model" (Microsoft Learn: LEN, LEFT,
///     RIGHT, MID, REPLACE, FIND), so text with surrogate pairs is refused where a position or a
///     length matters.
module Text =

    let private hasSurrogate (s: string) =
        let mutable found = false

        for ch in s do
            if Char.IsSurrogate ch then
                found <- true

        found

    let private positional (ctx: Ctx) (span: Span) (name: string) (s: string) : unit =
        if hasSurrogate s then
            Fail.notSupported
                ctx
                span
                $"{name} of text with characters outside the Basic Multilingual Plane (how they count depends on the model's UnicodeCharacterBehavior)"

    /// An argument read as text: BLANK is the empty text.
    let private text (ev: IEval) (ctx: Ctx) (e: Expr) : string =
        Convert.toText ctx e.Span (ev.Scalar(ctx, e))

    /// The text a function returns a piece or a variant of.
    let private subject (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : string =
        match ev.Scalar(ctx, e) with
        | DaxValue.Blank ->
            Fail.notSupported
                ctx
                e.Span
                $"{name} of a BLANK text (the sources don't say whether the result is BLANK or the empty text)"
        | v -> Convert.toText ctx e.Span v

    /// A whole number of characters, an instance or a position: an Integer, or another number with
    /// no fraction. The pages don't say how a fraction is rounded (for REPT they disagree).
    let private whole (ctx: Ctx) (e: Expr) (what: string) (v: DaxValue) : int64 =
        let fromFloat (d: float) =
            if d = Math.Floor d && abs d < 9.0e15 then
                int64 d
            else
                Fail.notSupported ctx e.Span $"{what} that is not a whole number"

        match v with
        | DaxValue.Integer i -> i
        | DaxValue.Decimal d -> fromFloat d
        | DaxValue.Currency c when c % 10000L = 0L -> c / 10000L
        | DaxValue.Currency _ -> Fail.notSupported ctx e.Span $"{what} that is not a whole number"
        | DaxValue.Blank -> Fail.notSupported ctx e.Span $"{what} that is BLANK"
        | _ -> Fail.notSupported ctx e.Span $"{what} that is not a number"

    /// A count of characters: zero or more.
    let private countOf (ctx: Ctx) (name: string) (e: Expr) (v: DaxValue) : int =
        let n = whole ctx e $"{name} with a number of characters" v

        if n < 0L then
            Fail.notSupported ctx e.Span $"{name} with a negative number of characters"

        int (min n (int64 Int32.MaxValue))

    let private count (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : int = countOf ctx name e (ev.Scalar(ctx, e))

    /// A position: 1 is the first character.
    let private position (ev: IEval) (ctx: Ctx) (name: string) (e: Expr) : int =
        let n = whole ctx e $"{name} with a position" (ev.Scalar(ctx, e))

        if n < 1L then
            Fail.notSupported ctx e.Span $"{name} with a position before the first character"

        int (min n (int64 Int32.MaxValue))

    let private optional (args: Expr list) (n: int) : Expr option =
        match List.tryItem n args with
        | Some e when not (Fn.isOmitted e) -> Some e
        | _ -> None

    // ---- Case ---------------------------------------------------------------------------------------

    /// A letter that is upper, lower or title case (ß is one, though .NET maps it to itself).
    let private isCased (ch: char) =
        match Char.GetUnicodeCategory ch with
        | UnicodeCategory.UppercaseLetter
        | UnicodeCategory.LowercaseLetter
        | UnicodeCategory.TitlecaseLetter -> true
        | _ -> false

    /// Upper or lower case of the ASCII letters. A letter outside ASCII is refused: which letter
    /// the engine maps it to (ß, the Turkish i, ǅ) is the culture's business and not documented.
    let private changeCase (ctx: Ctx) (span: Span) (name: string) (upper: bool) (s: string) : string =
        Culture.requireModel ctx span $"{name} (the case of letters follows the culture)"
        let sb = StringBuilder(s.Length)

        for ch in s do
            if ch < '\u0080' then
                if upper && ch >= 'a' && ch <= 'z' then
                    sb.Append(char (int ch - 32)) |> ignore
                elif (not upper) && ch >= 'A' && ch <= 'Z' then
                    sb.Append(char (int ch + 32)) |> ignore
                else
                    sb.Append ch |> ignore
            elif
                Char.IsSurrogate ch
                || Char.ToUpperInvariant ch <> ch
                || Char.ToLowerInvariant ch <> ch
                || isCased ch
            then
                Fail.notSupported ctx span $"{name} of a letter outside ASCII (U+{int ch:X4})"
            else
                sb.Append ch |> ignore

        sb.ToString()

    // ---- Searching ----------------------------------------------------------------------------------

    /// One element of a SEARCH pattern.
    [<RequireQualifiedAccess>]
    type private Pat =
        | Char of char
        /// `?`: any one character.
        | One
        /// `*`: any sequence of characters.
        | Many

    /// "A question mark matches any single character; an asterisk matches any sequence of
    /// characters. If you want to find an actual question mark or asterisk, type a tilde (~) before
    /// the character" (Microsoft Learn, SEARCH); "In order to find ~, you must use ~~" (dax.guide).
    let private pattern (ctx: Ctx) (span: Span) (name: string) (find: string) : Pat array =
        let out = ResizeArray<Pat>()
        let mutable i = 0

        while i < find.Length do
            match find.[i] with
            | '~' ->
                if
                    i + 1 < find.Length
                    && (find.[i + 1] = '?' || find.[i + 1] = '*' || find.[i + 1] = '~')
                then
                    out.Add(Pat.Char find.[i + 1])
                    i <- i + 2
                else
                    Fail.notSupported ctx span $"{name} for a tilde that is not followed by ?, * or ~"
            | '?' ->
                out.Add Pat.One
                i <- i + 1
            | '*' ->
                out.Add Pat.Many
                i <- i + 1
            | ch ->
                out.Add(Pat.Char ch)
                i <- i + 1

        out.ToArray()

    let private isKanaOrWidth (ch: char) =
        (ch >= '぀' && ch <= 'ヿ') || (ch >= 'ㇰ' && ch <= 'ㇿ') || (ch >= '＀' && ch <= '￯')

    /// A character that a linguistic comparison may find equal to other characters: one with a
    /// compatibility form (the ligature fi, a superscript 2, the fraction one half, the trade mark
    /// sign), and the letters that stand for two (sharp s, ae, oe, thorn, eth). Whether SEARCH finds
    /// "ss" in "Straße" is not documented.
    let private isExpansion (ch: char) =
        let s = string ch

        s.Normalize(NormalizationForm.FormKD) <> s.Normalize(NormalizationForm.FormD)
        || "\u00DF\u1E9E\u00E6\u00C6\u0153\u0152\u00FE\u00DE\u00F0\u00D0\u0133\u0132\u0149\u00F8\u00D8\u0142\u0141\u0111\u0110"
            .Contains
            ch

    /// SEARCH "is case insensitive", "kanatype-insensitive, width-insensitive" and "accent
    /// sensitive" (Microsoft Learn). The engine folds the case of ASCII letters and compares every
    /// other character as it is, so it refuses the characters for which that is not the whole story:
    /// kana and half- or full-width forms, combining marks and invisible characters (a linguistic
    /// comparison weighs them differently), and letters outside ASCII that have a case.
    let private checkInsensitive (ctx: Ctx) (span: Span) (name: string) (isPattern: bool) (s: string) : unit =
        for ch in s do
            if ch >= '\u0080' then
                let refuse (what: string) =
                    Fail.notSupported ctx span $"{name} of text with {what} (U+{int ch:X4})"

                if Char.IsSurrogate ch then
                    refuse "a character outside the Basic Multilingual Plane"
                elif isKanaOrWidth ch then
                    refuse "kana or a half- or full-width character (the engine folds them, this one doesn't)"
                else
                    match Char.GetUnicodeCategory ch with
                    | UnicodeCategory.NonSpacingMark
                    | UnicodeCategory.SpacingCombiningMark
                    | UnicodeCategory.EnclosingMark
                    | UnicodeCategory.Format
                    | UnicodeCategory.Control -> refuse "a combining mark or an invisible character"
                    | _ ->
                        let upper = Char.ToUpperInvariant ch
                        let lower = Char.ToLowerInvariant ch

                        if isPattern && (upper <> ch || lower <> ch || isCased ch) then
                            refuse "a letter outside ASCII to find without regard to case"
                        elif upper < '\u0080' || lower < '\u0080' then
                            refuse "a letter outside ASCII whose other case is an ASCII letter"
                        elif isExpansion ch then
                            refuse
                                "a character a linguistic comparison may read as other characters (a ligature, a superscript, a fraction)"

    let private foldAscii (ch: char) =
        if ch >= 'a' && ch <= 'z' then char (int ch - 32) else ch

    /// The pattern as the runs of characters between its asterisks ("a*d" is "a", "d").
    let private segments (pat: Pat array) : Pat array array =
        let out = ResizeArray<Pat array>()
        let current = ResizeArray<Pat>()

        for p in pat do
            match p with
            | Pat.Many ->
                out.Add(current.ToArray())
                current.Clear()
            | _ -> current.Add p

        out.Add(current.ToArray())
        out.ToArray()

    let private segmentAt (seg: Pat array) (within: string) (at: int) : bool =
        at + seg.Length <= within.Length
        && (let mutable same = true
            let mutable i = 0

            while same && i < seg.Length do
                match seg.[i] with
                | Pat.Char ch -> same <- foldAscii within.[at + i] = foldAscii ch
                | _ -> ()

                i <- i + 1

            same)

    /// Whether the pattern matches `within` from exactly `at` (and to anywhere). After an asterisk
    /// the next run is taken where it first occurs: nothing later could match that an earlier
    /// place doesn't.
    let private matchesAt (segs: Pat array array) (within: string) (at: int) : bool =
        if not (segmentAt segs.[0] within at) then
            false
        else
            let mutable pos = at + segs.[0].Length
            let mutable ok = true
            let mutable s = 1

            while ok && s < segs.Length do
                let seg = segs.[s]
                let mutable p = pos

                while p + seg.Length <= within.Length && not (segmentAt seg within p) do
                    p <- p + 1

                if p + seg.Length <= within.Length then
                    pos <- p + seg.Length
                    s <- s + 1
                else
                    ok <- false

            ok

    /// The 0-based place of the first match at or after `from`, or -1.
    let private searchFrom (pat: Pat array) (within: string) (from: int) : int =
        let segs = segments pat
        let mutable found = -1
        let mutable at = from

        while found < 0 && at <= within.Length do
            if matchesAt segs within at then
                found <- at

            at <- at + 1

        found

    let private requireDefaultCollation (ctx: Ctx) (span: Span) (name: string) : unit =
        if not (obj.ReferenceEquals(ctx.Model.Collation.Comparer, Collation.ordinalIgnoreCase.Comparer)) then
            Fail.notSupported
                ctx
                span
                $"{name} in a model whose collation is {ctx.Model.Collation.Name} (the pages describe it for the default, case-insensitive one)"

    /// SEARCH and FIND: the start position, and what to do when nothing is found.
    let private find (name: string) (insensitive: bool) : DaxFunction =
        Fn.scalar
            name
            "Text"
            (2, 4)
            (fun ev ctx args ->
                match optional args 3 with
                | Some e -> Typing.unify (StaticType.Known DaxType.Integer) (ev.TypeOf(ctx, e))
                | None -> StaticType.Known DaxType.Integer)
            (fun ev ctx call ->
                let what = text ev ctx call.Args.[0]
                // "Nulls in within_text are interpreted as an empty string in this context"
                // (Microsoft Learn, SEARCH).
                let within = text ev ctx call.Args.[1]
                positional ctx call.Args.[0].Span name what
                positional ctx call.Args.[1].Span name within

                // "(optional) The character position in within_text at which you want to start
                // searching. If omitted, 1."
                let start =
                    match optional call.Args 2 with
                    | Some e -> position ev ctx name e
                    | None -> 1

                if start > within.Length + 1 then
                    Fail.notSupported ctx call.Span $"{name} starting after the end of the text"

                let found =
                    if insensitive then
                        requireDefaultCollation ctx call.Span name
                        checkInsensitive ctx call.Args.[0].Span name true what
                        checkInsensitive ctx call.Args.[1].Span name false within

                        if what = "" then
                            Fail.notSupported ctx call.Args.[0].Span $"{name} for an empty text"

                        searchFrom (pattern ctx call.Args.[0].Span name what) within (start - 1)
                    elif what = "" then
                        // "Use double quotes (empty text) to match the first character in
                        // within_text" (Microsoft Learn, FIND): said of the first character only.
                        if start = 1 && within <> "" then
                            0
                        else
                            Fail.notSupported
                                ctx
                                call.Args.[0].Span
                                $"{name} for an empty text, other than from the first character of a text"
                    else
                        // "FIND is case-sensitive", "FIND doesn't support wildcards".
                        within.IndexOf(what, start - 1, StringComparison.Ordinal)

                if found >= 0 then
                    DaxValue.Integer(int64 found + 1L)
                else
                    // "The value to return when the operation doesn't find a matching substring,
                    // typically 0, -1, or BLANK(). If you don't specify it, the function returns an
                    // error."
                    match optional call.Args 3 with
                    | Some e -> ev.Scalar(ctx, e)
                    | None ->
                        Fail.runtime
                            ctx
                            call.Span
                            $"The search Text provided to function '{name}' could not be found in the given text.")

    // ---- UNICHAR ------------------------------------------------------------------------------------

    /// The characters XML 1.0 allows below U+0020, and not the two noncharacters that end the BMP.
    let private isXmlChar (code: int) =
        (code >= 0x20 || code = 0x9 || code = 0xA || code = 0xD)
        && code <> 0xFFFE
        && code <> 0xFFFF

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

        // "A whole number indicating the number of characters in the text string"; "Spaces count as
        // characters" (Microsoft Learn, LEN). For a value that is not text the same page says it is
        // cast "using the current column format", which the engine doesn't have: refused.
        Fn.scalar "LEN" "Text" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            match ev.Scalar(ctx, call.Args.[0]) with
            | DaxValue.Blank -> DaxValue.Integer 0L
            | DaxValue.String s ->
                positional ctx call.Span "LEN" s
                DaxValue.Integer(int64 s.Length)
            | _ ->
                Fail.notSupported
                    ctx
                    call.Args.[0].Span
                    "LEN of a value that is not text (it is cast to text with the column's format)")

        // "(optional) The number of characters you want LEFT to extract; if omitted, 1"; "If the
        // num_chars argument is larger than the number of characters available, the function returns
        // the maximum characters available and doesn't raise an error" (Microsoft Learn, LEFT).
        Fn.scalar "LEFT" "Text" (1, 2) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "LEFT" call.Args.[0]
            positional ctx call.Args.[0].Span "LEFT" s

            let n =
                match optional call.Args 1 with
                | Some e -> count ev ctx "LEFT" e
                | None -> 1

            DaxValue.String(s.Substring(0, min n s.Length)))

        // "A text string containing the specified right-most characters"; num_chars "is omitted, 1"
        // (Microsoft Learn, RIGHT). More characters than there are: said for LEFT, and RIGHT is its
        // mirror. A BLANK count is refused ("RIGHT also returns a blank": BLANK or empty?).
        Fn.scalar "RIGHT" "Text" (1, 2) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "RIGHT" call.Args.[0]
            positional ctx call.Args.[0].Span "RIGHT" s

            let n =
                match optional call.Args 1 with
                | Some e -> count ev ctx "RIGHT" e
                | None -> 1

            DaxValue.String(s.Substring(s.Length - min n s.Length)))

        // "The position of the first character you want to extract. Positions start at 1";
        // MID("abcde",2,3) "Returns "bcd"" (Microsoft Learn, MID). A piece that would run past the
        // end is not described ("A string of text of the specified length"): refused.
        Fn.scalar "MID" "Text" (3, 3) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "MID" call.Args.[0]
            positional ctx call.Args.[0].Span "MID" s
            let start = position ev ctx "MID" call.Args.[1]
            let n = count ev ctx "MID" call.Args.[2]

            if int64 start - 1L + int64 n > int64 s.Length then
                Fail.notSupported
                    ctx
                    call.Span
                    "MID for characters past the end of the text (the pages don't say what it returns)"

            DaxValue.String(s.Substring(start - 1, n)))

        // "Same text, in uppercase"; "Non-alphabetic characters are not affected" (Microsoft Learn).
        Fn.scalar "UPPER" "Text" (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "UPPER" call.Args.[0]
            DaxValue.String(changeCase ctx call.Span "UPPER" true s))

        // "Characters that are not letters are not changed. For example, the formula
        // = LOWER("123ABC") returns 123abc" (Microsoft Learn, LOWER).
        Fn.scalar "LOWER" "Text" (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "LOWER" call.Args.[0]
            DaxValue.String(changeCase ctx call.Span "LOWER" false s))

        // "TRIM removes trailing and starting spaces from a string, including multiple spaces within
        // the string" (dax.guide/trim, whose example leaves one space between words); it trims "the
        // 7-bit ASCII space character (value 32)" and "does not remove this nonbreaking space
        // character" (Microsoft Learn, TRIM).
        Fn.scalar "TRIM" "Text" (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "TRIM" call.Args.[0]
            let sb = StringBuilder(s.Length)
            let mutable pending = false

            for ch in s do
                if ch = ' ' then
                    pending <- sb.Length > 0
                else
                    if pending then
                        sb.Append ' ' |> ignore
                        pending <- false

                    sb.Append ch |> ignore

            DaxValue.String(sb.ToString()))

        // "The result of the REPT function cannot be longer than 32,767 characters, or REPT returns
        // an error" (both pages). For zero times Microsoft Learn says "REPT returns a blank" and DAX
        // Guide "returns an empty string"; for a fraction, "it is truncated" against "it is rounded
        // to the closest integer". Both are refused.
        Fn.scalar "REPT" "Text" (2, 2) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "REPT" call.Args.[0]

            let n =
                whole ctx call.Args.[1] "REPT with a number of times" (ev.Scalar(ctx, call.Args.[1]))

            if n < 0L then
                Fail.notSupported ctx call.Args.[1].Span "REPT with a negative number of times"
            elif n = 0L then
                Fail.notSupported
                    ctx
                    call.Args.[1].Span
                    "REPT zero times (BLANK according to Microsoft Learn, an empty text according to DAX Guide)"
            elif s = "" then
                DaxValue.String ""
            elif n > 32767L || int64 s.Length * n > 32767L then
                Fail.runtime ctx call.Span "The result of REPT is longer than 32,767 characters."
            else
                DaxValue.String(String.replicate (int n) s))

        // "(optional) The occurrence of old_text you want to replace. If omitted, every instance of
        // old_text is replaced"; "The SUBSTITUTE function is case-sensitive" (Microsoft Learn).
        Fn.scalar "SUBSTITUTE" "Text" (3, 4) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "SUBSTITUTE" call.Args.[0]
            let oldText = text ev ctx call.Args.[1]
            let newText = text ev ctx call.Args.[2]

            let instance =
                match optional call.Args 3 with
                | Some e ->
                    let n = whole ctx e "SUBSTITUTE with an instance" (ev.Scalar(ctx, e))

                    if n < 1L then
                        Fail.notSupported ctx e.Span "SUBSTITUTE with an instance below 1"

                    Some n
                | None -> None

            if oldText = "" then
                Fail.notSupported ctx call.Args.[1].Span "SUBSTITUTE of an empty text"

            // Every place the old text occurs, overlaps included: where two occurrences overlap
            // ("aa" in "aaa"), which of them count is not documented.
            let places = ResizeArray<int>()
            let mutable at = s.IndexOf(oldText, StringComparison.Ordinal)

            while at >= 0 do
                if places.Count > 0 && at < places.[places.Count - 1] + oldText.Length then
                    Fail.notSupported ctx call.Span "SUBSTITUTE of a text whose occurrences overlap"

                places.Add at

                at <-
                    if at + 1 <= s.Length then
                        s.IndexOf(oldText, at + 1, StringComparison.Ordinal)
                    else
                        -1

            let sb = StringBuilder(s.Length)
            let mutable from = 0

            for i in 0 .. places.Count - 1 do
                if instance.IsNone || instance.Value = int64 i + 1L then
                    sb.Append(s, from, places.[i] - from).Append(newText) |> ignore
                    from <- places.[i] + oldText.Length

            sb.Append(s, from, s.Length - from) |> ignore
            DaxValue.String(sb.ToString()))

        // REPLACE ( old_text, start_num, num_chars, new_text ): "The StartPosition starts from 1 for
        // the first character in the string" (dax.guide/replace); "If the argument, num_chars, is a
        // blank or references a column that evaluates to a blank, the string for new_text is
        // inserted at the position, start_num, without replacing any characters" (Microsoft Learn).
        // A start or a length past the end of the text is not described: refused.
        Fn.scalar "REPLACE" "Text" (4, 4) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let s = subject ev ctx "REPLACE" call.Args.[0]
            positional ctx call.Args.[0].Span "REPLACE" s
            let start = position ev ctx "REPLACE" call.Args.[1]

            let n =
                match ev.Scalar(ctx, call.Args.[2]) with
                | DaxValue.Blank -> 0
                | v -> countOf ctx "REPLACE" call.Args.[2] v

            let newText = text ev ctx call.Args.[3]

            if int64 start - 1L + int64 n > int64 s.Length || start > s.Length then
                Fail.notSupported
                    ctx
                    call.Span
                    "REPLACE of characters past the end of the text (the pages don't say what it returns)"

            DaxValue.String(s.Substring(0, start - 1) + newText + s.Substring(start - 1 + n)))

        find "SEARCH" true
        find "FIND" false

        // "EXACT performs string comparison in a case-sensitive way"; "An empty string and BLANK are
        // considered the same value" (dax.guide/exact). Whether trailing spaces count, as they
        // don't for `=`, is not said: two texts that differ only there are refused.
        Fn.scalar "EXACT" "Text" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let operand (e: Expr) =
                match ev.Scalar(ctx, e) with
                | DaxValue.Blank -> ""
                | DaxValue.String s -> s
                | _ -> Fail.notSupported ctx e.Span "EXACT of a value that is not text"

            let a = operand call.Args.[0]
            let b = operand call.Args.[1]

            if String.Equals(a, b, StringComparison.Ordinal) then
                DaxValue.Boolean true
            elif String.Equals(Collation.trimTrailing a, Collation.trimTrailing b, StringComparison.Ordinal) then
                Fail.notSupported ctx call.Span "EXACT of texts that differ only in trailing spaces"
            else
                DaxValue.Boolean false)

        // "CONTAINSSTRING internally uses the SEARCH function"; "If WithinText is BLANK or an empty
        // string, CONTAINSSTRING always returns FALSE, even when FindText is empty or "*". If
        // WithinText is not empty and FindText is BLANK or an empty string, CONTAINSSTRING returns
        // TRUE"; "CONTAINSSTRING converts a number to text before the search. For example,
        // CONTAINSSTRING ( 12345, 34 ) returns TRUE" (dax.guide/containsstring).
        Fn.scalar "CONTAINSSTRING" "Text" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let within = text ev ctx call.Args.[0]
            let what = text ev ctx call.Args.[1]
            requireDefaultCollation ctx call.Span "CONTAINSSTRING"
            checkInsensitive ctx call.Args.[0].Span "CONTAINSSTRING" false within
            checkInsensitive ctx call.Args.[1].Span "CONTAINSSTRING" true what

            if within = "" then
                DaxValue.Boolean false
            elif what = "" then
                DaxValue.Boolean true
            else
                let pat = pattern ctx call.Args.[1].Span "CONTAINSSTRING" what
                DaxValue.Boolean(searchFrom pat within 0 >= 0))

        // "CONTAINSSTRINGEXACT is case-sensitive"; its example finds no "a*d" in "abcd": no
        // wildcards (Microsoft Learn). The pages say nothing of an empty text on either side.
        Fn.scalar "CONTAINSSTRINGEXACT" "Text" (2, 2) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let within = text ev ctx call.Args.[0]
            let what = text ev ctx call.Args.[1]

            if within = "" || what = "" then
                Fail.notSupported ctx call.Span "CONTAINSSTRINGEXACT with an empty or BLANK text"

            DaxValue.Boolean(within.Contains(what, StringComparison.Ordinal)))

        // "Delimiter must be a constant text that is not empty ... An empty string, BLANK, or a
        // value that can change by row or by filter context (for example, a column reference)
        // returns an error"; "A BLANK value becomes an empty string, so the result is never BLANK.
        // For example, COMBINEVALUES ( ",", BLANK (), BLANK () ) returns ","";
        // "COMBINEVALUES needs at least two expressions after Delimiter" (dax.guide/combinevalues).
        Fn.scalar "COMBINEVALUES" "Text" (3, -1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let delimiter =
                match (Fn.unparen call.Args.[0]).Kind with
                | StringLiteral "" ->
                    Fail.invalid ctx call.Args.[0].Span "The delimiter of COMBINEVALUES must not be empty"
                | StringLiteral s -> s
                | _ ->
                    Fail.notSupported
                        ctx
                        call.Args.[0].Span
                        "COMBINEVALUES with a delimiter that is not a text literal (it must be a constant)"

            let parts =
                call.Args
                |> List.tail
                |> List.map (fun e -> Convert.toText ctx e.Span (ev.Scalar(ctx, e)))

            DaxValue.String(String.Join(delimiter, parts)))
        |> Fn.withCheck (fun args ->
            match List.tryHead args with
            | Some e ->
                match (Fn.unparen e).Kind with
                | StringLiteral _ -> None
                | _ -> Some "COMBINEVALUES with a delimiter that is not a text literal (it must be a constant)"
            | None -> None)

        // "If number is zero (0), UNICHAR returns an error"; "If Unicode numbers are partial
        // surrogates and data types are not valid, UNICHAR returns an error"; "If numbers are numeric
        // values that fall outside the allowable range, UNICHAR returns an error"; "The Unicode
        // character returned can be a string of characters, for example in UTF-8 or UTF-16 codes"
        // (Microsoft Learn); "The Unicode encoding used by DAX is UTF-16" (dax.guide/unichar).
        // Which control characters count as invalid XML characters, and the last two code points
        // (DAX Guide ends the range at 1,114,109), are not clear enough: refused.
        Fn.scalar "UNICHAR" "Text" (1, 1) (Fn.typeIs DaxType.String) (fun ev ctx call ->
            let n =
                whole ctx call.Args.[0] "UNICHAR of a number" (ev.Scalar(ctx, call.Args.[0]))

            if n = 0L then
                Fail.runtime ctx call.Span "UNICHAR: zero is not the number of a character."
            elif n < 0L || n > 0x10FFFFL then
                Fail.runtime ctx call.Span "UNICHAR: the number is outside the range of Unicode."
            elif n >= 0xD800L && n <= 0xDFFFL then
                Fail.runtime ctx call.Span "UNICHAR: the number is half of a surrogate pair, not a character."
            elif not (isXmlChar (int n)) || n > 1114109L then
                Fail.notSupported ctx call.Span $"UNICHAR of {n} (a control character or a noncharacter)"
            else
                DaxValue.String(Char.ConvertFromUtf32(int n)))

        // "A numeric code for the first character in a text string" (Microsoft Learn, UNICODE).
        Fn.scalar "UNICODE" "Text" (1, 1) (Fn.typeIs DaxType.Integer) (fun ev ctx call ->
            match ev.Scalar(ctx, call.Args.[0]) with
            | DaxValue.String s when s <> "" ->
                if Char.IsSurrogate s.[0] then
                    positional ctx call.Span "UNICODE" s

                DaxValue.Integer(int64 s.[0])
            | DaxValue.String _
            | DaxValue.Blank -> Fail.notSupported ctx call.Span "UNICODE of an empty or BLANK text"
            | _ -> Fail.notSupported ctx call.Args.[0].Span "UNICODE of a value that is not text")

        // TOCSV and TOJSON: "The sort order of the result cannot be controlled" (dax.guide), and how
        // each data type is written is shown by example only. Refused by name, so the message says
        // why rather than "unknown function".
        Fn.scalar "TOCSV" "Text" (1, -1) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            Fail.notSupported ctx call.Span "TOCSV (the order of its rows and how it writes values are not documented)")
        |> Fn.withCheck (fun _ -> Some "TOCSV (the order of its rows and how it writes values are not documented)")

        Fn.scalar "TOJSON" "Text" (1, -1) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            Fail.notSupported
                ctx
                call.Span
                "TOJSON (the order of its rows and how it writes values are not documented)")
        |> Fn.withCheck (fun _ -> Some "TOJSON (the order of its rows and how it writes values are not documented)")
    ]
