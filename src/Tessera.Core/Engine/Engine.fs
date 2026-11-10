/// The local DAX engine: evaluates DAX over fixture data, offline.
///
/// It is an independent reimplementation written against the public documentation (Microsoft
/// Learn, dax.guide, SQLBI), not a port of anything and not yet checked against a real engine. Its
/// one rule: never a wrong answer. A function, an argument form or a model feature it doesn't
/// implement exactly is a `not-supported` error that names the thing and where it is.
///
/// `prepare` binds a model to its data and computes the calculated columns and tables; the result
/// is immutable and can be shared between threads. `evaluateQuery` and `evaluateScalar` run DAX
/// against it. `coverage` says, from the model alone, which of its expressions use something the
/// engine doesn't support.
module Tessera.Engine

open System
open System.Collections.Generic
open System.Threading
open Microsoft.AnalysisServices.Tabular
open Tessera.DaxAst
open Tessera.DaxEngine

/// A model bound to its data, ready for queries.
type Prepared = internal {
    Model: Tessera.DaxEngine.Model
    /// Calculated columns and tables that couldn't be computed, and tables whose blank row
    /// can't be decided: queries that touch them fail with the same error.
    Failures: (ObjectRef * TessError) list
}

/// The instant the date functions see as "now". `Now` is what TODAY and NOW return (local time),
/// `UtcNow` what UTCTODAY and UTCNOW return. It is fixed when a model is prepared: the calculated
/// tables and columns and every query against the prepared model see this one instant.
type Clock = { Now: DateTime; UtcNow: DateTime }

module Clock =

    /// The machine's clock, read once: local time and UTC, to the millisecond.
    let system () : Clock =
        let utc = DateTime.UtcNow

        let utc =
            DateTime(utc.Ticks - utc.Ticks % TimeSpan.TicksPerMillisecond, DateTimeKind.Unspecified)

        let local =
            utc
            + TimeZoneInfo.Local.GetUtcOffset(DateTime.SpecifyKind(utc, DateTimeKind.Utc))

        { Now = local; UtcNow = utc }

    /// The same instant for the local and the UTC functions (an offset of zero).
    let fixedAt (now: DateTime) : Clock =
        let now = DateTime.SpecifyKind(now, DateTimeKind.Unspecified)
        { Now = now; UtcNow = now }

    let private formats = [|
        "yyyy-MM-dd"
        "yyyy-MM-dd'T'HH:mm"
        "yyyy-MM-dd'T'HH:mm:ss"
        "yyyy-MM-dd'T'HH:mm:ss.FFFFFFF"
        "yyyy-MM-dd HH:mm"
        "yyyy-MM-dd HH:mm:ss"
        "yyyy-MM-dd HH:mm:ss.FFFFFFF"
    |]

    /// An ISO 8601 date or date and time: `2024-03-05`, `2024-03-05T14:30`, `2024-03-05T14:30:00`,
    /// with a `T` or a space, and optionally a UTC offset (`Z`, `+02:00`, `-0500`). The date and
    /// time as written is the local `Now`. With an offset, `UtcNow` is that instant in UTC; without
    /// one it is the same as `Now`: the machine's own time zone is never consulted.
    let parse (text: string) : Result<Clock, string> =
        let invalid () =
            Error
                $"'{text}' is not an ISO 8601 date and time (2024-03-05, 2024-03-05T14:30:00, 2024-03-05T14:30:00+02:00)"

        let t = (if isNull text then "" else text.Trim())
        let inv = Globalization.CultureInfo.InvariantCulture

        let local (s: string) =
            match DateTime.TryParseExact(s, formats, inv, Globalization.DateTimeStyles.None) with
            | true, d -> Some(DateTime.SpecifyKind(d, DateTimeKind.Unspecified))
            | _ -> None

        // The offset, if the text ends in one: Z, or a sign and hh:mm / hhmm after a time.
        let hasTime = t.Length > 10

        let body, offset =
            if hasTime && (t.EndsWith "Z" || t.EndsWith "z") then
                t.Substring(0, t.Length - 1), Some TimeSpan.Zero
            else
                let sign = t.LastIndexOfAny [| '+'; '-' |]

                if hasTime && sign > 10 then
                    let digits = t.Substring(sign + 1).Replace(":", "")

                    if digits.Length = 4 && digits |> Seq.forall Char.IsAsciiDigit then
                        let hours = int (digits.Substring(0, 2))
                        let minutes = int (digits.Substring(2, 2))

                        if hours <= 14 && minutes <= 59 then
                            let span = TimeSpan(hours, minutes, 0)
                            t.Substring(0, sign), Some(if t.[sign] = '-' then -span else span)
                        else
                            t, None
                    else
                        t, None
                else
                    t, None

        match local body, offset with
        | Some now, Some offset ->
            if
                now.Ticks - offset.Ticks < 0L
                || now.Ticks - offset.Ticks > DateTime.MaxValue.Ticks
            then
                invalid ()
            else
                Ok { Now = now; UtcNow = now - offset }
        | Some now, None when body.Length = t.Length -> Ok { Now = now; UtcNow = now }
        | _ -> invalid ()

/// How a model is prepared. `Clock = None` reads the machine's clock when `prepareWith` is called.
type Options = { Clock: Clock option }

module Options =
    let defaults: Options = { Clock = None }

/// The functions the engine implements: name and family, by family then name.
let functions: (string * string) list =
    Registry.all
    |> List.map (fun f -> f.Name, f.Family)
    |> List.sortBy (fun (name, family) -> family, name)

let private ev = Registry.evaluator

let private syntaxError (label: string) (errors: DaxParser.ParseError list) : TessError =
    match errors with
    | e :: _ -> DaxSyntax(label, e.Line, e.Column, e.Message)
    | [] -> DaxSyntax(label, 1, 1, "the DAX can't be parsed")

let private parseExpression (source: SourceText) : Expr =
    match DaxParser.parseExpression source.Text with
    | Ok e -> e
    | Error errors -> raise (EngineException(syntaxError source.Label errors))

/// Run on a thread with a deep stack: measures that call measures nest far deeper than one tree.
let private run (work: unit -> 'a) : Result<'a, TessError> =
    let mutable result: Result<'a, TessError> =
        Error(DaxInvalid("the evaluation did not finish", ""))

    let mutable crash: exn = null

    let body () =
        try
            result <- Ok(work ())
        with
        | EngineException e -> result <- Error e
        | DaxRuntimeException(text, at) -> result <- Error(DaxEvaluation(text, at))
        | :? InsufficientExecutionStackException ->
            result <- Error(DaxInvalid("the expression is nested too deeply to evaluate", ""))
        | e -> crash <- e

    let thread = Thread(body, 128 * 1024 * 1024)
    thread.IsBackground <- true
    thread.Start()
    thread.Join()

    if not (isNull crash) then
        raise (InvalidOperationException("The DAX engine failed: " + crash.Message, crash))

    result

// ---- What the model has that the engine doesn't do ----------------------------------------------

/// Tables no query can use, with why: decided from the model alone. A relationship the engine
/// doesn't do closes every table connected to it, since filters travel.
let private staticGates (db: Database) : Dictionary<string, string> =
    let gates = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

    if not (isNull db.Model) then
        let parent = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for t in db.Model.Tables do
            parent.[t.Name] <- t.Name

        let rec find (x: string) =
            if String.Equals(parent.[x], x, StringComparison.OrdinalIgnoreCase) then
                x
            else
                let root = find parent.[x]
                parent.[x] <- root
                root

        let unsupported = ResizeArray<string * string>()

        for r in db.Model.Relationships do
            if not (isNull r.FromTable) && not (isNull r.ToTable) then
                parent.[find r.FromTable.Name] <- find r.ToTable.Name

                let between = $"between '{r.FromTable.Name}' and '{r.ToTable.Name}'"

                match r with
                | :? SingleColumnRelationship as s ->
                    match s.FromCardinality, s.ToCardinality with
                    | RelationshipEndCardinality.Many, RelationshipEndCardinality.One
                    | RelationshipEndCardinality.One, RelationshipEndCardinality.Many -> ()
                    | RelationshipEndCardinality.One, RelationshipEndCardinality.One ->
                        unsupported.Add((r.FromTable.Name, $"the one-to-one relationship {between}"))
                    | _ -> unsupported.Add((r.FromTable.Name, $"the many-to-many relationship {between}"))
                | _ ->
                    unsupported.Add((r.FromTable.Name, $"the relationship {between}, which is not on a single column"))

        for t in db.Model.Tables do
            let directQuery =
                t.Partitions
                |> Seq.exists (fun p -> p.Mode = ModeType.DirectQuery || p.Mode = ModeType.Dual)
                || (db.Model.DefaultMode = ModeType.DirectQuery
                    && t.Partitions |> Seq.exists (fun p -> p.Mode = ModeType.Default))

            if directQuery then
                unsupported.Add((t.Name, $"DirectQuery (table '{t.Name}')"))

        for (table, why) in unsupported do
            let root = find table

            for t in db.Model.Tables do
                if
                    String.Equals(find t.Name, root, StringComparison.OrdinalIgnoreCase)
                    && not (gates.ContainsKey t.Name)
                then
                    gates.[t.Name] <- why

        // A calculation group only matters to a query that names it.
        for t in db.Model.Tables do
            if not (isNull t.CalculationGroup) then
                gates.[t.Name] <- $"calculation groups (table '{t.Name}')"

    gates

let private calculatedTableExpression (t: Table) : string option =
    t.Partitions
    |> Seq.tryPick (fun p ->
        match p.Source with
        | :? CalculatedPartitionSource as c -> Some(if isNull c.Expression then "" else c.Expression)
        | _ -> None)

let private isCircular (e: TessError) =
    match e with
    | DaxInvalid(message, _) -> message.StartsWith("circular dependency", StringComparison.Ordinal)
    | _ -> false

// ---- prepare -----------------------------------------------------------------------------------

let private bind
    (clock: Clock)
    (db: Database)
    (store: DataStore)
    : Tessera.DaxEngine.Model * (ObjectRef * TessError) list =
    let model = Tessera.DaxEngine.Model(store.Collation)
    model.Now <- clock.Now
    model.UtcNow <- clock.UtcNow
    model.Culture <- (if isNull db.Model.Culture then "" else db.Model.Culture)
    let gates = staticGates db
    let tables = ResizeArray<BTable>()
    let mutable columnId = 0

    let nextColumn (table: BTable) (name: string) (t: DaxType) =
        let c = BColumn(columnId, table, name, t)
        columnId <- columnId + 1
        c

    for t in db.Model.Tables do
        let table = BTable(tables.Count, model, t.Name)
        tables.Add table
        model.TableByName.[t.Name] <- table

        match gates.TryGetValue t.Name with
        | true, why -> table.Gate <- Some why
        | _ -> ()

        let calculated = calculatedTableExpression t

        match calculated with
        | Some text ->
            table.Expression <-
                Some {
                    Label = $"table '{t.Name}'"
                    Text = text
                }
        | None -> ()

        let stored = store.TryTable t.Name

        let rows =
            match stored with
            | Some s -> s.RowCount
            | None -> 0

        let columns = ResizeArray<BColumn>()

        for c in t.Columns do
            match c with
            | :? RowNumberColumn -> ()
            | _ ->
                let declared = DaxType.ofTom c.DataType |> Option.defaultValue DaxType.Variant
                let column = nextColumn table c.Name declared

                match c with
                | :? CalculatedColumn as cc ->
                    column.Expression <-
                        Some {
                            Label = $"column '{t.Name}'[{c.Name}]"
                            Text = (if isNull cc.Expression then "" else cc.Expression)
                        }
                | :? CalculatedTableColumn as ct ->
                    column.SourceName <- (if isNull ct.SourceColumn then c.Name else ct.SourceColumn)
                | _ ->
                    if calculated.IsNone then
                        match stored |> Option.bind (fun s -> s.TryColumn c.Name) with
                        | Some data -> column.SetData data
                        | None ->
                            // No data for it: BLANK in every row (a table with no file is empty).
                            column.SetData(StoreColumn.blank store.Collation c.Name declared rows)
                    else
                        column.SourceName <- c.Name

                columns.Add column
                table.ColumnByName.[c.Name] <- column

        table.Columns <- columns.ToArray()

        if calculated.IsNone then
            table.SetRowCount rows

    model.Tables <- tables.ToArray()

    // Relationships the engine does: single column, many-to-one (either way round).
    let rels = ResizeArray<BRel>()

    for r in db.Model.Relationships do
        match r with
        | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) ->
            let column (c: Column) =
                match model.TableByName.TryGetValue c.Table.Name with
                | true, t ->
                    match t.ColumnByName.TryGetValue c.Name with
                    | true, bc -> Some bc
                    | _ -> None
                | _ -> None

            let ends =
                match s.FromCardinality, s.ToCardinality with
                | RelationshipEndCardinality.Many, RelationshipEndCardinality.One -> Some(s.FromColumn, s.ToColumn)
                | RelationshipEndCardinality.One, RelationshipEndCardinality.Many -> Some(s.ToColumn, s.FromColumn)
                | _ -> None

            match ends with
            | Some(many, one) ->
                match column many, column one with
                | Some f, Some t when f.Table.Gate.IsNone ->
                    rels.Add(
                        BRel(
                            rels.Count,
                            (if isNull s.Name then "" else s.Name),
                            f,
                            t,
                            s.CrossFilteringBehavior = CrossFilteringBehavior.BothDirections,
                            s.IsActive
                        )
                    )
                | _ -> ()
            | None -> ()
        | _ -> ()

    model.Rels <- rels.ToArray()
    model.ColumnCount <- columnId

    for t in db.Model.Tables do
        for m in t.Measures do
            let source = {
                Label = $"measure '{t.Name}'[{m.Name}]"
                Text = (if isNull m.Expression then "" else m.Expression)
            }

            model.Measures.[m.Name] <-
                BMeasure(m.Name, model.TableByName.[t.Name], source, lazy (parseExpression source))

    for f in db.Model.Functions do
        let source = {
            Label = $"function {f.Name}"
            Text = (if isNull f.Expression then "" else f.Expression)
        }

        let parsed =
            lazy
                (match DaxParser.parseFunction source.Text with
                 | Ok def -> def
                 | Error errors -> raise (EngineException(syntaxError source.Label errors)))

        model.Functions.[f.Name] <- BFunction(f.Name, source, parsed)

    // The relationships as the model has them must give one path between any two tables.
    match model.DefaultConfig.Ambiguity with
    | Some between ->
        for t in model.Tables do
            if
                t.Gate.IsNone
                && (model.Rels
                    |> Array.exists (fun r ->
                        obj.ReferenceEquals(r.From.Table, t) || obj.ReferenceEquals(r.To.Table, t)))
            then
                t.Gate <- Some $"active relationships that give more than one filter path between {between}"
    | None -> ()

    // ---- Computing what is calculated, on demand ---------------------------------------------------

    let fail (set: TessError -> unit) (work: unit -> unit) =
        try
            work ()
        with
        | EngineException e ->
            set e
            reraise ()
        | DaxRuntimeException(text, at) ->
            let e = DaxEvaluation(text, at)
            set e
            raise (EngineException e)

    model.ForceColumn <-
        fun (c: BColumn) ->
            match c.Failure, c.Expression with
            | Some e, _ -> raise (EngineException e)
            | None, _ when c.Computing ->
                raise (
                    EngineException(
                        DaxInvalid($"circular dependency: column '{c.Table.Name}'[{c.Name}] depends on itself", "")
                    )
                )
            | None, None ->
                // A column of a calculated table: computing the table fills it.
                c.Table.RowCount |> ignore

                if not c.IsReady then
                    match c.Table.Failure with
                    | Some e -> raise (EngineException e)
                    | None ->
                        raise (
                            EngineException(
                                DaxInvalid($"column '{c.Table.Name}'[{c.Name}] has no data and no expression", "")
                            )
                        )
            | None, Some source ->
                c.Computing <- true

                try
                    fail (fun e -> c.Failure <- Some e) (fun () ->
                        let t = c.Table
                        let n = t.RowCount
                        let expr = parseExpression source
                        let ctx = Context.root model null source false
                        let table = Tables.ofBase model.DefaultConfig t (Array.init n id)

                        let values =
                            Array.init n (fun row -> ev.Scalar(Context.pushRow ctx table row, expr))

                        let declared, values =
                            if c.DataType = DaxType.Variant then
                                Typing.column (ev.TypeOf(Context.pushRow ctx table 0, expr)) values
                            else
                                c.DataType, values

                        match StoreColumn.ofValues model.Collation c.Name declared values with
                        | Ok data ->
                            c.DataType <- declared
                            c.SetData data
                        | Error e -> raise (DaxRuntimeException(DaxError.message e, source.Label)))
                finally
                    c.Computing <- false

    model.ForceTable <-
        fun (t: BTable) ->
            match t.Failure, t.Expression with
            | Some e, _ -> raise (EngineException e)
            | None, _ when t.Computing ->
                raise (EngineException(DaxInvalid($"circular dependency: table '{t.Name}' depends on itself", "")))
            | None, None -> t.SetRowCount 0
            | None, Some source ->
                t.Computing <- true

                try
                    fail (fun e -> t.Failure <- Some e) (fun () ->
                        let expr = parseExpression source
                        let ctx = Context.root model null source false
                        let result = ev.Table(ctx, expr)

                        // The model lists the table's columns by the result column each holds.
                        if t.Columns |> Array.forall (fun c -> c.Expression.IsSome) then
                            let made =
                                result.Cols
                                |> Array.map (fun info ->
                                    let c = nextColumn t info.Name info.Type
                                    c.SourceName <- Tables.displayName info
                                    t.ColumnByName.[info.Name] <- c
                                    c)

                            t.Columns <- Array.append made t.Columns

                        for c in t.Columns do
                            if c.Expression.IsNone then
                                let matches (info: ColInfo) =
                                    String.Equals(
                                        Tables.displayName info,
                                        c.SourceName,
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    || String.Equals(
                                        $"[{info.Name}]",
                                        c.SourceName,
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    || String.Equals(info.Name, c.Name, StringComparison.OrdinalIgnoreCase)

                                match result.Cols |> Array.tryFindIndex matches with
                                | None ->
                                    raise (
                                        EngineException(
                                            DaxInvalid(
                                                $"the expression of calculated table '{t.Name}' returns no column for '{c.Name}' ({c.SourceName})",
                                                source.Label
                                            )
                                        )
                                    )
                                | Some i ->
                                    let declared, values =
                                        if c.DataType = DaxType.Variant then
                                            Typing.column (Typing.ofColumn result.Cols.[i].Type) (result.Column i)
                                        else
                                            c.DataType, result.Column i

                                    match StoreColumn.ofValues model.Collation c.Name declared values with
                                    | Ok data ->
                                        c.DataType <- declared
                                        c.SetData data
                                    | Error e -> raise (DaxRuntimeException(DaxError.message e, source.Label))

                        t.SetRowCount result.Count)
                finally
                    t.Computing <- false

    let deciding = HashSet<int>()

    model.ForceBlankRow <-
        fun (t: BTable) ->
            let refuse (why: string) =
                t.Gate <- Some why
                raise (EngineException(DaxNotSupported(why, "")))

            match t.Gate with
            | Some why when not t.BlankRowKnown && why.StartsWith("the blank row", StringComparison.Ordinal) ->
                raise (EngineException(DaxNotSupported(why, "")))
            | _ -> ()

            if not (deciding.Add t.Id) then
                // A loop of relationships back to this table: nothing more to learn from it.
                ()
            else
                try
                    let incoming =
                        model.Rels |> Array.filter (fun r -> obj.ReferenceEquals(r.To.Table, t))

                    if incoming |> Array.exists (fun r -> r.IsActive && r.OrphanFromRows.Length > 0) then
                        // "a special row added to a table, in case the table is on the one-side of
                        // a strong relationship and the relationship is invalid" (SQLBI, "Blank
                        // row in DAX").
                        t.SetBlankRow true
                    else
                        match
                            incoming
                            |> Array.tryFind (fun r -> not r.IsActive && r.OrphanFromRows.Length > 0)
                        with
                        | Some r ->
                            refuse
                                $"the blank row of '{t.Name}': only the inactive relationship from '{r.From.Table.Name}'[{r.From.Name}] has keys without a match, and the sources don't say whether an inactive relationship adds the blank row"
                        | None -> ()

                        match
                            incoming
                            |> Array.tryFind (fun r ->
                                not (obj.ReferenceEquals(r.From.Table, t)) && r.From.Table.HasBlankRow)
                        with
                        | Some r ->
                            refuse
                                $"the blank row of '{t.Name}': '{r.From.Table.Name}', which relates to it, has a blank row of its own, and the sources don't say whether that gives '{t.Name}' one"
                        | None -> ()

                        t.SetBlankRow false
                finally
                    deciding.Remove t.Id |> ignore

    // Compute everything now, so that nothing changes once `prepare` has returned.
    let failures = ResizeArray<ObjectRef * TessError>()

    let attempt (ref: ObjectRef) (work: unit -> unit) =
        try
            work ()
        with
        | EngineException e ->
            if not (failures.Exists(fun (r, _) -> r = ref)) then
                failures.Add((ref, e))
        | DaxRuntimeException(text, at) -> failures.Add((ref, DaxEvaluation(text, at)))

    for t in model.Tables do
        if t.Gate.IsNone then
            attempt (ObjectRef.table t.Name) (fun () -> t.RowCount |> ignore)

            for c in t.Columns do
                attempt (ObjectRef.inTable ObjectKind.Column t.Name c.Name) (fun () -> c.Data |> ignore)

    for r in model.Rels do
        attempt (ObjectRef.modelLevel ObjectKind.Relationship r.Name) (fun () -> r.ToRowOfFromRow |> ignore)

    for t in model.Tables do
        if t.Gate.IsNone then
            attempt (ObjectRef.table t.Name) (fun () -> t.HasBlankRow |> ignore)

    model, List.ofSeq failures

/// Bind a model to its data and compute its calculated columns and tables, in dependency order.
///
/// Fails for a circular dependency between calculated columns or tables. A calculated column or
/// table that can't be computed for another reason (it uses something not supported) doesn't fail
/// `prepare`: see `failures`; a query that touches it gets that error.
///
/// `options.Clock` is the instant TODAY, NOW, UTCTODAY and UTCNOW return, in calculated tables and
/// columns and in every query against the result; without one the machine's clock is read once, now.
let prepareWith (options: Options) (db: Database) (store: DataStore) : Result<Prepared, TessError> =
    if isNull db.Model then
        Error(DaxInvalid("the database has no model", ""))
    else
        let clock = options.Clock |> Option.defaultWith Clock.system

        run (fun () -> bind clock db store)
        |> Result.bind (fun (model, failures) ->
            match failures |> List.tryFind (fun (_, e) -> isCircular e) with
            | Some(_, e) -> Error e
            | None -> Ok { Model = model; Failures = failures })

/// `prepareWith` with the defaults: the date functions see the machine's clock, read once here.
let prepare (db: Database) (store: DataStore) : Result<Prepared, TessError> = prepareWith Options.defaults db store

/// The instant the prepared model's date functions see.
let clock (p: Prepared) : Clock = {
    Now = p.Model.Now
    UtcNow = p.Model.UtcNow
}

/// What `prepare` could not compute, with why.
let failures (p: Prepared) : (ObjectRef * TessError) list = p.Failures

// ---- Results -----------------------------------------------------------------------------------

let private queryType (t: DaxType) : QueryType =
    match t with
    | DaxType.Integer -> QueryType.Int64
    | DaxType.Decimal -> QueryType.Double
    | DaxType.Currency -> QueryType.Decimal
    | DaxType.String -> QueryType.String
    | DaxType.Boolean -> QueryType.Boolean
    | DaxType.DateTime -> QueryType.DateTime
    | DaxType.Variant -> QueryType.Variant

let private queryValue (v: DaxValue) : QueryValue =
    match v with
    | DaxValue.Blank -> QueryValue.Blank
    | DaxValue.Integer i -> QueryValue.Int64 i
    | DaxValue.Decimal d -> QueryValue.Double d
    | DaxValue.Currency c -> QueryValue.Decimal(DaxValue.currencyValue c)
    | DaxValue.String s -> QueryValue.String s
    | DaxValue.Boolean b -> QueryValue.Boolean b
    | DaxValue.DateTime serial ->
        match DaxValue.dateTimeOfSerial serial with
        | Some d -> QueryValue.DateTime d
        | None -> raise (DaxRuntimeException("A date is outside the range of dates (0001-01-01 to 9999-12-31).", ""))

/// A column's type and values as a result declares them: the column's declared type, which
/// `Typing.column` settled when the column was made.
let private resultColumn (info: ColInfo) (values: DaxValue array) : DaxType * DaxValue array =
    let declared = info.Type

    let values =
        if declared = DaxType.Variant then
            values
        else
            values |> Array.map (Typing.widen declared)

    // A value that isn't of the declared type makes the column a Variant, rather than a lie.
    if
        declared <> DaxType.Variant
        && values |> Array.exists (fun v -> not (DaxValue.fits declared v))
    then
        DaxType.Variant, values
    else
        declared, values

let private toResult (t: TableVal) (order: int array) : QueryResult =
    let columns =
        Array.init t.Cols.Length (fun c ->
            let declared, values = resultColumn t.Cols.[c] (t.Column c)
            Tables.displayName t.Cols.[c], declared, values)

    {
        Columns = [
            for (name, declared, _) in columns ->
                {
                    Name = name
                    Type = queryType declared
                }
        ]
        Rows = [
            for row in order -> columns |> Array.map (fun (_, _, values) -> queryValue values.[row])
        ]
        Truncated = false
    }

// ---- Queries -----------------------------------------------------------------------------------

let private evaluateOne (ctx: Ctx) (e: Evaluate) : QueryResult =
    if not e.StartAt.IsEmpty then
        Fail.notSupported ctx e.StartAt.Head.Span "START AT"

    let table = ev.Table(ctx, e.Table)

    let order =
        if e.OrderBy.IsEmpty then
            Array.init table.Count id
        else
            let keys =
                e.OrderBy
                |> List.map (fun k ->
                    // A key that names a column of the result is that column (also when a measure
                    // has the same name); anything else is an expression over the result's rows.
                    let named =
                        match (Fn.unparen k.Expr).Kind with
                        | Bracketed name ->
                            table.Cols
                            |> Array.tryFindIndex (fun c ->
                                isNull c.Table
                                && String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
                        | Qualified(t, name) ->
                            table.Cols
                            |> Array.tryFindIndex (fun c ->
                                String.Equals(c.Table, t.Name, StringComparison.OrdinalIgnoreCase)
                                && String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
                        | _ -> None

                    let values =
                        match named with
                        | Some i -> table.Column i
                        | None -> Array.init table.Count (fun row -> ev.Scalar(Context.pushRow ctx table row, k.Expr))

                    Functions.TableFunctions.checkSortable ctx k.Span "ORDER BY" values
                    values, k.Direction = Some SortDirection.Desc)

            let compare (a: int) (b: int) =
                let mutable c = 0
                let mutable rest = keys

                while c = 0 && not rest.IsEmpty do
                    let values, descending = rest.Head
                    c <- DaxValue.sortCompare ctx.Model.Collation values.[a] values.[b]

                    if descending then
                        c <- -c

                    rest <- rest.Tail

                c

            // Stable: rows that tie keep the order they were produced in.
            List.init table.Count id |> List.sortWith compare |> Array.ofList

    toResult table order

/// Run a DAX query: one result per EVALUATE.
///
/// DEFINE MEASURE, VAR and FUNCTION are supported; ORDER BY sorts by any expressions over the
/// result's columns. Without ORDER BY the rows come in the order the engine produced them, which
/// (as on a server) is not defined.
let evaluateQuery (p: Prepared) (dax: string) : Result<QueryResult list, TessError> =
    match DaxParser.parseQuery dax with
    | Error errors -> Error(syntaxError "query" errors)
    | Ok query ->
        run (fun () ->
            let source = SourceText.query dax
            let defs = QueryDefs()
            let mutable ctx = Context.root p.Model defs source true

            for d in query.Definitions do
                match d.Kind with
                | DefinitionKind.Measure(table, name, value) ->
                    match p.Model.TableByName.TryGetValue table.Name with
                    | true, t -> defs.Measures.[name.Name] <- BMeasure(name.Name, t, source, lazy value)
                    | _ -> Fail.invalid ctx table.Span $"there is no table '{table.Name}'"
                | DefinitionKind.Function(name, def) ->
                    defs.Functions.[name.Name] <- BFunction(name.Name, source, lazy def)
                | DefinitionKind.Var(name, value) ->
                    // A query variable: in scope in everything after it, evaluated once.
                    let definedIn = ctx
                    let isTable = ev.IsTable(definedIn, value)
                    let lazyValue = lazy (ev.Value(definedIn, value))

                    let staticType =
                        lazy
                            (if isTable then
                                 StaticType.Unknown
                             else
                                 ev.TypeOf(definedIn, value))

                    ctx <- {
                        ctx with
                            Scope = {
                                ctx.Scope with
                                    Vars =
                                        ctx.Scope.Vars.Add(
                                            name.Name.ToUpperInvariant(),
                                            Val(lazyValue, isTable, staticType)
                                        )
                            }
                    }
                | DefinitionKind.Table(name, _) -> Fail.notSupported ctx name.Span "DEFINE TABLE"
                | DefinitionKind.Column(_, name, _) -> Fail.notSupported ctx name.Span "DEFINE COLUMN"
                | DefinitionKind.MParameter(name, _) -> Fail.notSupported ctx name.Span "DEFINE MPARAMETER"

            query.Evaluates |> List.map (evaluateOne ctx))

/// Evaluate one scalar expression with no filter context and no row context, as `EVALUATE ROW (
/// "v", <expression> )` would.
let evaluateScalar (p: Prepared) (dax: string) : Result<QueryValue, TessError> =
    let source = { Label = "expression"; Text = dax }

    match DaxParser.parseExpression dax with
    | Error errors -> Error(syntaxError source.Label errors)
    | Ok expr ->
        run (fun () ->
            let ctx = Context.root p.Model null source true
            let _, values = Typing.column (ev.TypeOf(ctx, expr)) [| ev.Scalar(ctx, expr) |]
            queryValue values.[0])

// ---- coverage ----------------------------------------------------------------------------------

/// `coverage`, on the calling thread.
let private coverageOf (db: Database) : (ObjectRef * string list) list =
    if isNull db.Model then
        []
    else
        let gates = staticGates db
        let tables = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let measures = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let udfs = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for t in db.Model.Tables do
            tables.Add t.Name |> ignore

            for m in t.Measures do
                measures.[m.Name] <- (if isNull m.Expression then "" else m.Expression)

        for f in db.Model.Functions do
            udfs.[f.Name] <- (if isNull f.Expression then "" else f.Expression)

        let columns = HashSet<string>(StringComparer.OrdinalIgnoreCase)

        for t in db.Model.Tables do
            for c in t.Columns do
                columns.Add(t.Name + "\u0000" + c.Name) |> ignore

        let memo = Dictionary<string, string list>(StringComparer.OrdinalIgnoreCase)
        let active = HashSet<string>(StringComparer.OrdinalIgnoreCase)

        let calculatedColumns = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let calculatedTables = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for t in db.Model.Tables do
            match calculatedTableExpression t with
            | Some text -> calculatedTables.[t.Name] <- text
            | None -> ()

            for c in t.Columns do
                match c with
                | :? CalculatedColumn as cc ->
                    calculatedColumns.[t.Name + "\u0000" + c.Name] <-
                        (if isNull cc.Expression then "" else cc.Expression)
                | _ -> ()

        // Tables whose rows can't be told without a calculated table or column: the two ends of a
        // relationship on one. Filled in below, once those are known.
        let dependsOn =
            Dictionary<string, ResizeArray<string>>(StringComparer.OrdinalIgnoreCase)

        let rec ofExpr (declared: Set<string>) (e: Expr) : string list =
            let found = ResizeArray<string>()

            let rec walk (scope: Set<string>) (e: Expr) =
                Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

                let table (name: string) =
                    match gates.TryGetValue name with
                    | true, why -> found.Add why
                    | _ -> ()

                    if calculatedTables.ContainsKey name then
                        found.AddRange(ofCalculated ("t\u0000" + name) calculatedTables.[name])

                    match dependsOn.TryGetValue name with
                    | true, keys ->
                        for key in List.ofSeq keys do
                            let text =
                                if key.StartsWith("t\u0000", StringComparison.Ordinal) then
                                    calculatedTables.[key.Substring 2]
                                else
                                    calculatedColumns.[key.Substring 2]

                            found.AddRange(ofCalculated key text)
                    | _ -> ()

                match e.Kind with
                | QuotedTable name -> table name
                | Identifier name when not (scope.Contains(name.ToUpperInvariant())) -> table name
                | Qualified(t, name) ->
                    table t.Name

                    match calculatedColumns.TryGetValue(t.Name + "\u0000" + name) with
                    | true, text -> found.AddRange(ofCalculated ("c\u0000" + t.Name + "\u0000" + name) text)
                    | _ ->
                        if not (columns.Contains(t.Name + "\u0000" + name)) && measures.ContainsKey name then
                            found.AddRange(ofMeasure name)
                | Bracketed name when measures.ContainsKey name -> found.AddRange(ofMeasure name)
                | QueryParameter name -> found.Add $"the query parameter @{name}"
                | Call(name, args) ->
                    match Registry.byName.TryGetValue name.Name with
                    | true, fn ->
                        match fn.Check args with
                        | Some why -> found.Add why
                        | None -> ()
                    | _ ->
                        if udfs.ContainsKey name.Name then
                            found.AddRange(ofFunction name.Name)
                        else
                            found.Add(name.Name.ToUpperInvariant())

                    List.iter (walk scope) args
                | VarBlock(vars, body) ->
                    let mutable inner = scope

                    for v in vars do
                        walk inner v.Value
                        inner <- inner.Add(v.Name.Name.ToUpperInvariant())

                    walk inner body
                | _ -> List.iter (walk scope) (children e)

            walk declared e
            List.ofSeq found

        and cached (key: string) (compute: unit -> string list) : string list =
            match memo.TryGetValue key with
            | true, known -> known
            | _ ->
                if not (active.Add key) then
                    []
                else
                    let result = compute () |> List.distinct
                    active.Remove key |> ignore
                    memo.[key] <- result
                    result

        and ofCalculated (key: string) (text: string) : string list =
            cached key (fun () ->
                match DaxParser.parseExpression text with
                | Ok e -> ofExpr Set.empty e
                | Error _ -> [ "an expression that can't be parsed" ])

        and ofMeasure (name: string) : string list =
            cached ("m\u0000" + name) (fun () ->
                match DaxParser.parseExpression measures.[name] with
                | Ok e -> ofExpr Set.empty e
                | Error _ -> [ $"measure [{name}] can't be parsed" ])

        and ofFunction (name: string) : string list =
            cached ("f\u0000" + name) (fun () ->
                match DaxParser.parseFunction udfs.[name] with
                | Ok def ->
                    let parameters =
                        def.Parameters
                        |> List.map (fun p -> p.Name.Name.ToUpperInvariant())
                        |> Set.ofList

                    (def.Parameters
                     |> List.collect (fun p -> p.Default |> Option.map (ofExpr parameters) |> Option.defaultValue []))
                    @ ofExpr parameters def.Body
                | Error _ -> [ $"function {name} can't be parsed" ])

        // A table related (over any chain of relationships) to a calculated table, or through a
        // calculated column, needs that computed before its own rows can be filtered.
        let group = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for t in db.Model.Tables do
            group.[t.Name] <- t.Name

        let rec find (x: string) =
            if String.Equals(group.[x], x, StringComparison.OrdinalIgnoreCase) then
                x
            else
                let root = find group.[x]
                group.[x] <- root
                root

        let keysOfGroup =
            Dictionary<string, ResizeArray<string>>(StringComparer.OrdinalIgnoreCase)

        for r in db.Model.Relationships do
            match r with
            | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) ->
                group.[find s.FromColumn.Table.Name] <- find s.ToColumn.Table.Name
            | _ -> ()

        for r in db.Model.Relationships do
            match r with
            | :? SingleColumnRelationship as s when not (isNull s.FromColumn) && not (isNull s.ToColumn) ->
                let root = find s.FromColumn.Table.Name

                if not (keysOfGroup.ContainsKey root) then
                    keysOfGroup.[root] <- ResizeArray<string>()

                for c in [ s.FromColumn; s.ToColumn ] do
                    if calculatedTables.ContainsKey c.Table.Name then
                        keysOfGroup.[root].Add("t\u0000" + c.Table.Name)

                    if calculatedColumns.ContainsKey(c.Table.Name + "\u0000" + c.Name) then
                        keysOfGroup.[root].Add("c\u0000" + c.Table.Name + "\u0000" + c.Name)
            | _ -> ()

        for t in db.Model.Tables do
            match keysOfGroup.TryGetValue(find t.Name) with
            | true, keys when keys.Count > 0 -> dependsOn.[t.Name] <- ResizeArray<string>(Seq.distinct keys)
            | _ -> ()

        let ofText (home: string) (text: string) : string list =
            let own =
                match gates.TryGetValue home with
                | true, why -> [ why ]
                | _ -> []

            let used =
                match DaxParser.parseExpression (if isNull text then "" else text) with
                | Ok e -> ofExpr Set.empty e
                | Error _ -> [ "the expression can't be parsed" ]

            own @ used |> List.distinct

        [
            for t in db.Model.Tables do
                for m in t.Measures do
                    yield ObjectRef.inTable ObjectKind.Measure t.Name m.Name, ofMeasure m.Name

                for c in t.Columns do
                    match c with
                    | :? CalculatedColumn as cc ->
                        // A calculated column is evaluated in a row context on its table.
                        yield
                            ObjectRef.inTable ObjectKind.Column t.Name c.Name,
                            ofText
                                t.Name
                                ("COUNTROWS ( '" + t.Name.Replace("'", "''") + "' ) + (" + cc.Expression + "\n)")
                    | _ -> ()

                match calculatedTableExpression t with
                | Some text -> yield ObjectRef.table t.Name, ofText t.Name text
                | None -> ()
        ]


/// For every measure, calculated column and calculated table of a model: what it uses that the
/// engine doesn't support (an empty list: nothing, as far as can be told without data). Follows
/// references to measures, functions and calculated objects, and through relationships to the
/// calculated tables and columns a table's rows depend on. Each item is a function name, or a
/// sentence about an argument form or a model feature.
///
/// Static: it runs each function's check of its argument forms, the same one the evaluator runs.
/// What depends on the context or on the data (a relationship whose keys repeat, a blank row that
/// can't be decided, values of mixed types, where a function is used) only shows when evaluating.
let coverage (db: Database) : (ObjectRef * string list) list =
    match run (fun () -> coverageOf db) with
    | Ok items -> items
    | Error e -> raise (InvalidOperationException(TessError.message e))
