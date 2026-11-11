namespace Tessera.DaxEngine.Functions

// Information functions: the type tests (ISNUMBER, ISTEXT, ISNONTEXT, ISLOGICAL), ISEVEN, ISODD,
// ISEMPTY, CONTAINS, and the functions that return who is asking (USERNAME, USERPRINCIPALNAME,
// USEROBJECTID, USERCULTURE, CUSTOMDATA), and NAMEOF. ISBLANK, ISERROR and CONTAINSROW are in Logical.fs.
//
// Sources: the function pages of dax.guide and of Microsoft Learn, quoted at each function.

open System
open Tessera
open Tessera.DaxAst
open Tessera.DaxEngine

module Information =

    /// The type a type test sees: the value's own, or for a BLANK the type of the column the
    /// argument names ("When applied to a column reference as expression, this functions tests
    /// the data type of the column": dax.guide/isnumber). None for a BLANK that is not a column's.
    let private testedType (ev: IEval) (ctx: Ctx) (arg: Expr) : DaxType option * bool =
        let v = ev.Scalar(ctx, arg)

        match DaxValue.typeOf v with
        | Some t -> Some t, false
        | None ->
            match (Fn.unparen arg).Kind with
            | Qualified _ ->
                match ev.TryColumn(ctx, arg) with
                | Some c when c.Data.DataType <> DaxType.Variant -> Some c.Data.DataType, true
                | _ -> None, true
            | _ -> None, true

    let private typeTest (name: string) (blank: bool option) (test: DaxType -> bool) =
        Fn.scalar name "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            match testedType ev ctx call.Args.[0], blank with
            | (Some t, _), _ -> DaxValue.Boolean(test t)
            | (None, _), Some answer -> DaxValue.Boolean answer
            | (None, _), None -> Fail.notSupported ctx call.Span $"{name} of a BLANK that is not a column's value")

    /// A whole number for ISEVEN and ISODD. A number with a fraction, and BLANK, are not
    /// documented ("If number is nonnumeric, ISEVEN returns the #VALUE! error value" is all the
    /// page says of other arguments).
    let private wholeNumber (ev: IEval) (ctx: Ctx) (call: Call) (name: string) : int64 =
        match Coerce.numberNotBlank ctx call.Args.[0].Span name (ev.Scalar(ctx, call.Args.[0])) with
        | DaxValue.Integer i -> i
        | DaxValue.Decimal d when d = Math.Truncate d && abs d < 9007199254740992.0 -> int64 d
        | DaxValue.Currency c when c % 10000L = 0L -> c / 10000L
        | _ -> Fail.notSupported ctx call.Span $"{name} of a number that is not whole"

    /// The functions that answer from the connection. The local engine has no connection: they
    /// are refused unless the caller said who is asking (`Engine.withIdentity`).
    let private identity (name: string) (pick: Identity -> string option) =
        Fn.scalar name "Information" (0, 0) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            match ctx.Identity |> Option.bind pick with
            | Some text -> DaxValue.String text
            | None ->
                Fail.notSupported
                    ctx
                    call.Span
                    $"{name}: the local engine has no connection to take it from (give the engine an identity to answer it)")

    let functions: DaxFunction list = [
        // "TRUE whether the column is of any numeric data type (Currency, DateTime, Decimal,
        // Integer) and FALSE for any other data type. IMPORTANT: This function does not test
        // whether a string can be converted to a number" (dax.guide/isnumber).
        typeTest "ISNUMBER" None DaxType.isNumeric

        // "An empty string is considered text"; ISTEXT ( BLANK () ) is "Is Non-Text" (Microsoft
        // Learn, ISTEXT).
        typeTest "ISTEXT" (Some false) (fun t -> t = DaxType.String)

        // "TRUE if the value is not text or blank; FALSE if the value is text"; ISNONTEXT ( BLANK
        // ( ) ) is "Is Non-Text" (Microsoft Learn, ISNONTEXT).
        typeTest "ISNONTEXT" (Some true) (fun t -> t <> DaxType.String)

        // "TRUE if the value is a logical value; FALSE if any value other than TRUE OR FALSE"
        // (Microsoft Learn, ISLOGICAL).
        typeTest "ISLOGICAL" None (fun t -> t = DaxType.Boolean)

        // "Returns TRUE if number is even, or FALSE if number is odd" (Microsoft Learn, ISEVEN).
        Fn.scalar "ISEVEN" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            DaxValue.Boolean(wholeNumber ev ctx call "ISEVEN" % 2L = 0L))

        Fn.scalar "ISODD" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            DaxValue.Boolean(wholeNumber ev ctx call "ISODD" % 2L <> 0L))

        // "TRUE if the table is empty (has no rows), if else, FALSE" (dax.guide/isempty).
        Fn.scalar "ISEMPTY" "Information" (1, 1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            DaxValue.Boolean(ev.Table(ctx, call.Args.[0]).Count = 0))

        // "Returns TRUE if there exists at least one row where all columns have specified
        // values"; "CONTAINS uses a strict comparison, like the IN operator and CONTAINSROW. BLANK
        // does not match 0 or an empty string. The comparison of text is not case-sensitive"
        // (dax.guide/contains). The columns are named as model columns of the table.
        Fn.scalar "CONTAINS" "Information" (3, -1) (Fn.typeIs DaxType.Boolean) (fun ev ctx call ->
            let table = ev.Table(ctx, call.Args.[0])
            let pairs = call.Args.Tail |> List.chunkBySize 2

            let tests =
                pairs
                |> List.map (fun pair ->
                    match pair with
                    | [ column; value ] ->
                        let c = ev.Column(ctx, column)

                        let index =
                            table.Cols
                            |> Array.tryFindIndex (fun info -> obj.ReferenceEquals(info.Lineage, c))

                        match index with
                        | Some i -> table.Column i, ev.Scalar(ctx, value)
                        | None ->
                            Fail.invalid
                                ctx
                                column.Span
                                $"'{c.Table.Name}'[{c.Name}] is not a column of the table CONTAINS searches"
                    | _ -> Fail.invalid ctx call.Span "CONTAINS takes a table, then pairs of a column and a value")

            let found =
                seq { 0 .. table.Count - 1 }
                |> Seq.exists (fun row ->
                    tests
                    |> List.forall (fun (data, value) ->
                        Fail.dax ctx call.Span (DaxValue.strictEquals ctx.Model.Collation value data.[row])))

            DaxValue.Boolean found)
        |> Fn.withCheck (fun args ->
            if args.Length % 2 = 0 then
                Some "CONTAINS with a column that has no value"
            else
                args.Tail
                |> List.chunkBySize 2
                |> List.tryPick (fun pair ->
                    match (Fn.unparen pair.Head).Kind with
                    | Qualified _ -> None
                    | _ -> Some "CONTAINS with a column that is not written Table[Column]"))

        // "The username from the credentials given to the system at connection time"
        // (dax.guide/username).
        identity "USERNAME" (fun i -> i.UserName)
        // "Returns the name of the user as their email address" (dax.guide/userprincipalname).
        identity "USERPRINCIPALNAME" (fun i -> i.UserPrincipalName)
        // "the security identifier (SID) in Windows, and another identifier in the Power BI or
        // Azure Analysis Services service" (dax.guide/userobjectid).
        identity "USEROBJECTID" (fun i -> i.UserObjectId)
        // "The culture code as a string, such as "en-US"" (dax.guide/userculture).
        identity "USERCULTURE" (fun i -> i.UserCulture)

        // "Returns the name of a table, column, measure, or calendar as a text string"; with only
        // the object it is "a fully qualified, escaped name": 'TableName'[ColumnName],
        // 'TableName'[MeasureName], 'TableName'; the names are the model's ("NAMEOF (
        // 'Sales'[ORDER QUANTITY] )" returns "'Sales'[Order Quantity]"). Within delimiters "A
        // literal single quote ... is escaped as two single quotes" and "A literal closing bracket
        // ... is escaped as ]]" (Microsoft Learn, NAMEOF; dax.guide/nameof). The argument is a
        // reference, never evaluated. Refused: calendars, a column given without its table, the
        // combinations the pages call errors (what kind of error isn't said), UNESCAPED and
        // MINIMALLYESCAPED for a name with a quote or a bracket (the pages' examples of those are
        // garbled), and MINIMALLYESCAPED for a name that starts with a digit or has a letter
        // outside A-Z (what "simple letters" covers isn't said).
        Fn.scalar "NAMEOF" "Information" (1, 3) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            let refuse (what: string) : 'a = Fail.notSupported ctx call.Span what

            let tableNamed (name: string) =
                match ctx.Model.TableByName.TryGetValue name with
                | true, t -> Some t
                | _ -> None

            let measureNamed (name: string) =
                let fromQuery =
                    if isNull ctx.Query || not ctx.Scope.InQuery then
                        None
                    else
                        match ctx.Query.Measures.TryGetValue name with
                        | true, m -> Some m
                        | _ -> None

                match fromQuery with
                | Some m -> Some m
                | None ->
                    match ctx.Model.Measures.TryGetValue name with
                    | true, m -> Some m
                    | _ -> None

            // (parent table, own name): a table has no parent; `child` says whether the own name
            // is a column's or a measure's.
            let parent, own, kind =
                match (Fn.unparen call.Args.[0]).Kind with
                | Identifier name
                | QuotedTable name ->
                    match tableNamed name with
                    | Some t -> None, t.Name, "table"
                    | None -> Fail.invalid ctx call.Args.[0].Span $"there is no table '{name}'"
                | Qualified(table, name) ->
                    match tableNamed table.Name with
                    | None -> Fail.invalid ctx call.Args.[0].Span $"there is no table '{table.Name}'"
                    | Some t ->
                        match t.ColumnByName.TryGetValue name with
                        | true, c -> Some t.Name, c.Name, "column"
                        | _ ->
                            match measureNamed name with
                            | Some m when obj.ReferenceEquals(m.Table, t) -> Some t.Name, m.Name, "measure"
                            | Some m ->
                                refuse
                                    $"NAMEOF of the measure [{m.Name}] given with the table '{t.Name}', which is not its own"
                            | None ->
                                Fail.invalid ctx call.Args.[0].Span $"there is no column or measure '{t.Name}'[{name}]"
                | Bracketed name ->
                    match measureNamed name with
                    | Some m -> Some m.Table.Name, m.Name, "measure"
                    | None -> refuse $"NAMEOF ( [{name}] ) of something that is not a measure"
                | _ ->
                    Fail.invalid
                        ctx
                        call.Args.[0].Span
                        "the argument of NAMEOF must be a table, column or measure reference (variables and expressions are not allowed)"

            let word (i: int) (allowed: string list) (byDefault: string) =
                if call.Args.Length <= i || Fn.isOmitted call.Args.[i] then
                    byDefault
                else
                    match Fn.keyword call.Args.[i] with
                    | Some k when allowed |> List.contains k -> k
                    | _ ->
                        let list = String.concat ", " allowed
                        Fail.invalid ctx call.Args.[i].Span $"NAMEOF's argument {i + 1} must be one of {list}"

            let part =
                word 1 [ "TABLE"; "COLUMN"; "MEASURE"; "CALENDAR"; "FULL"; "SELF"; "PARENT" ] "FULL"

            let escaping = word 2 [ "ESCAPED"; "UNESCAPED"; "MINIMALLYESCAPED" ] "ESCAPED"

            let error () : 'a =
                refuse $"NAMEOF of a {kind} with {part}, which the documentation calls an error (what kind isn't said)"

            // The parts asked for: (table name, child name), either may be missing.
            let tablePart, childPart =
                match part, parent, kind with
                | "FULL", Some p, _ -> Some p, Some own
                | "FULL", None, _ -> Some own, None
                | "SELF", Some _, _ -> None, Some own
                | "SELF", None, _ -> Some own, None
                | "TABLE", Some p, _ -> Some p, None
                | "TABLE", None, _ -> Some own, None
                | "PARENT", Some p, _ -> Some p, None
                | "COLUMN", _, "column" -> None, Some own
                | "MEASURE", _, "measure" -> None, Some own
                | _ -> error ()

            let quoted (name: string) = "'" + name.Replace("'", "''") + "'"
            let bracketed (name: string) = "[" + name.Replace("]", "]]") + "]"

            let plain (name: string) =
                if name.Contains '\'' || name.Contains ']' then
                    refuse $"NAMEOF {escaping} of a name with a quote or a closing bracket ('{name}')"

            let minimal (name: string) (escape: string -> string) =
                plain name

                if name.Length > 0 && Char.IsAsciiDigit name.[0] then
                    refuse $"NAMEOF MINIMALLYESCAPED of a name that starts with a digit ('{name}')"

                if name |> Seq.exists (fun c -> c > '\u007f') then
                    refuse $"NAMEOF MINIMALLYESCAPED of a name with a letter outside A-Z ('{name}')"

                if
                    name.Length > 0
                    && name |> Seq.forall (fun c -> Char.IsAsciiLetterOrDigit c || c = '_')
                then
                    name
                else
                    escape name

            let text =
                match escaping, tablePart, childPart with
                | "ESCAPED", Some t, Some c -> quoted t + bracketed c
                | "ESCAPED", Some t, None -> quoted t
                | "ESCAPED", None, Some c -> bracketed c
                | "UNESCAPED", Some _, Some _ ->
                    refuse "NAMEOF UNESCAPED of a fully qualified name, which the documentation calls an error"
                | "UNESCAPED", Some name, None
                | "UNESCAPED", None, Some name ->
                    plain name
                    name
                // A qualified name keeps the brackets of its child: 'Sales'[m1] is Sales[m1].
                | _, Some t, Some c ->
                    plain c
                    minimal t quoted + bracketed c
                | _, Some t, None -> minimal t quoted
                | _, None, Some c -> minimal c bracketed
                | _ -> error ()

            DaxValue.String text)
        |> Fn.withCheck (fun args ->
            match args with
            | _ :: c :: _ when Fn.keyword c = Some "CALENDAR" -> Some "NAMEOF with CALENDAR (calendars)"
            | _ -> None)

        // "The content of the CustomData property in the connection string. Blank, if CustomData
        // property was not defined at connection time" (Microsoft Learn, CUSTOMDATA): BLANK once
        // an identity without custom data is given; refused when no identity is.
        Fn.scalar "CUSTOMDATA" "Information" (0, 0) (Fn.typeIs DaxType.String) (fun _ ctx call ->
            match ctx.Identity with
            | Some i ->
                match i.CustomData with
                | Some text -> DaxValue.String text
                | None -> DaxValue.Blank
            | None ->
                Fail.notSupported
                    ctx
                    call.Span
                    "CUSTOMDATA: the local engine has no connection to take it from (give the engine an identity to answer it)")
    ]
