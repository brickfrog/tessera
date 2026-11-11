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
    /// What the model was bound from: a query that defines columns or tables binds it again,
    /// with those added, for itself.
    Database: Database
    Store: DataStore
    /// Calculated columns and tables that couldn't be computed, and tables whose blank row
    /// can't be decided: queries that touch them fail with the same error.
    Failures: (ObjectRef * TessError) list
    /// Who is asking, for queries (see `withIdentity`); calculated columns and tables never see it.
    Identity: Tessera.DaxEngine.Identity option
}

/// The same model for someone in particular: USERNAME, USERPRINCIPALNAME, USEROBJECTID, USERCULTURE
/// and CUSTOMDATA answer from `identity` (a function whose value is None there is still refused;
/// CUSTOMDATA is then BLANK). Without this, the engine refuses them all: it has no connection to
/// take them from. `p` itself is unchanged: the two share the model and its data.
let withIdentity (identity: Tessera.DaxEngine.Identity) (p: Prepared) : Prepared = { p with Identity = Some identity }

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
                    // "Inactive relationships are also expanded" (Learn, "Model relationships in
                    // Power BI Desktop"): an inactive one-to-one relationship would join the two
                    // expanded tables, which isn't implemented.
                    | RelationshipEndCardinality.One, RelationshipEndCardinality.One when not r.IsActive ->
                        unsupported.Add((r.FromTable.Name, $"the inactive one-to-one relationship {between}"))
                    | RelationshipEndCardinality.One, RelationshipEndCardinality.One
                    | RelationshipEndCardinality.Many, RelationshipEndCardinality.Many -> ()
                    | f, t ->
                        unsupported.Add((r.FromTable.Name, $"the relationship {between}, whose ends are {f} and {t}"))
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

    gates

/// What a calculated column's expression does to the filter of a context transition in it, read
/// from the expression and the measures and functions it uses: (may narrow it: ALL, ALLEXCEPT,
/// ALLNOBLANKROW, REMOVEFILTERS, ALLSELECTED, ALLCROSSFILTERED, or a CALCULATE or CALCULATETABLE
/// filter argument that is not a condition; uses USERELATIONSHIP). "restrict the list of columns
/// that the calculated column depends on, by using ALLEXCEPT or REMOVEFILTERS" (SQLBI, "Avoiding
/// circular dependency errors in DAX"); "this optimization is not activated: You use
/// USERELATIONSHIP in the code of the column" (SQLBI, "Understanding circular dependencies").
let private transitionTraits (model: Model) (defs: QueryDefs option) (expr: Expr) : bool * bool =
    let removers =
        set [
            "ALL"
            "ALLEXCEPT"
            "ALLNOBLANKROW"
            "REMOVEFILTERS"
            "ALLSELECTED"
            "ALLCROSSFILTERED"
        ]

    let mutable narrows = false
    let mutable changes = false
    let seen = HashSet<obj>(HashIdentity.Reference)

    // A filter argument that is a condition replaces the filters of its columns only.
    let rec condition (e: Expr) =
        match e.Kind with
        | Paren inner -> condition inner
        | Binary _
        | Unary(UnaryOp.Not, _) -> true
        | Call(name, [ inner ]) when String.Equals(name.Name, "KEEPFILTERS", StringComparison.OrdinalIgnoreCase) ->
            condition inner
        | Call(name, _) ->
            match name.Name.ToUpperInvariant() with
            | "USERELATIONSHIP"
            | "CROSSFILTER" -> true
            | _ -> false
        | _ -> false

    let measure (name: string) : BMeasure option =
        let fromQuery =
            match defs with
            | Some d ->
                match d.Measures.TryGetValue name with
                | true, m -> Some m
                | _ -> None
            | None -> None

        match fromQuery with
        | Some m -> Some m
        | None ->
            match model.Measures.TryGetValue name with
            | true, m -> Some m
            | _ -> None

    let rec walk (e: Expr) =
        Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

        match e.Kind with
        | Call(name, args) ->
            let upper = name.Name.ToUpperInvariant()

            if removers.Contains upper then
                narrows <- true

            if upper = "USERELATIONSHIP" then
                changes <- true

            if
                (upper = "CALCULATE" || upper = "CALCULATETABLE")
                && args.Length > 1
                && not (args.Tail |> List.forall condition)
            then
                narrows <- true

            let fromModel =
                match model.Functions.TryGetValue name.Name with
                | true, f -> Some f
                | _ -> None

            let fromQuery =
                match defs with
                | Some d ->
                    match d.Functions.TryGetValue name.Name with
                    | true, f -> Some f
                    | _ -> None
                | None -> None

            match fromQuery |> Option.orElse fromModel with
            | Some f when seen.Add f ->
                // What a function does with its arguments can't be told here.
                narrows <- true

                try
                    walk f.Def.Body
                with EngineException _ ->
                    ()
            | _ -> ()
        | Bracketed name
        | Qualified(_, name) ->
            match measure name with
            | Some m when seen.Add m ->
                try
                    walk m.Expr
                with EngineException _ ->
                    ()
            | _ -> ()
        | _ -> ()

        List.iter walk (children e)

    walk expr
    narrows, changes

/// The columns with Group By Columns (a field parameter's display column has them), with why
/// queries can't use them: "The columns specified in Group By Columns must be present in the
/// grouping conditions of any query that includes the target column ... otherwise, the query fails
/// with an error", and "SUMMARIZE, SUMMARIZECOLUMNS, DISTINCT, and VALUES are all going to fail"
/// without them (SQLBI, "Understanding Group By Columns in Power BI"). Which queries those are
/// isn't implemented, so any that reads the column is refused.
let private groupedBy (db: Database) : Dictionary<string, string> =
    let found = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

    if not (isNull db.Model) then
        for t in db.Model.Tables do
            for c in t.Columns do
                if
                    not (isNull c.RelatedColumnDetails)
                    && c.RelatedColumnDetails.GroupByColumns.Count > 0
                then
                    let by =
                        c.RelatedColumnDetails.GroupByColumns
                        |> Seq.choose (fun g ->
                            if isNull g.GroupingColumn then
                                None
                            else
                                Some $"'{g.GroupingColumn.Table.Name}'[{g.GroupingColumn.Name}]")
                        |> String.concat ", "

                    found.[t.Name + "\u0000" + c.Name] <-
                        $"'{t.Name}'[{c.Name}], which has Group By Columns ({by}): a query that groups by it without them fails; the engine reads it only where SUMMARIZECOLUMNS groups by it together with all of them"

    found

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

// ---- Calculation groups ------------------------------------------------------------------------

/// Why every measure reference in the model is refused, filtered or not: two groups with the same
/// precedence ("There cannot be two calculation groups in a model with the same Precedence": SQLBI,
/// "Understanding calculation group precedence").
let private duplicatePrecedence (db: Database) : string list = [
    let groups =
        db.Model.Tables
        |> Seq.filter (fun t -> not (isNull t.CalculationGroup))
        |> List.ofSeq

    for (precedence, same) in groups |> List.groupBy (fun t -> t.CalculationGroup.Precedence) do
        if same.Length > 1 then
            let names = same |> List.map (fun t -> $"'{t.Name}'") |> String.concat ", "
            $"calculation groups with the same precedence ({precedence}): {names}"
]

/// Why a measure reference where the group is not filtered is refused: its noSelectionExpression.
let private noSelection (t: Table) : string option =
    if isNull t.CalculationGroup.NoSelectionExpression then
        None
    else
        Some $"the noSelectionExpression of calculation group '{t.Name}'"

/// For coverage: what refuses a model's measure references whatever is filtered, or whenever a
/// group is not (which is most of the time).
let private calculationGroupProblems (db: Database) : string list =
    duplicatePrecedence db
    @ [
        for t in db.Model.Tables do
            if not (isNull t.CalculationGroup) then
                match noSelection t with
                | Some why -> why
                | None -> ()
    ]

/// Why an evaluation where the group is filtered by several items, or by none, is refused.
let private multipleOrEmpty (db: Database) (t: Table) : string option =
    let behavior =
        try
            db.Model.SelectionExpressionBehavior
        with _ ->
            SelectionExpressionBehaviorType.Automatic

    if not (isNull t.CalculationGroup.MultipleOrEmptySelectionExpression) then
        Some $"the multipleOrEmptySelectionExpression of calculation group '{t.Name}'"
    elif
        behavior = SelectionExpressionBehaviorType.Automatic
        && db.CompatibilityLevel > 1702
    then
        // "Automatic. This is the default value and is the same as nonvisual ... Models above a
        // future compatibility level set to automatic will use visual instead" (Learn).
        Some
            $"several or no items of calculation group '{t.Name}' selected, with selectionExpressionBehavior Automatic at compatibility level {db.CompatibilityLevel} (Learn: Automatic becomes Visual above a future compatibility level)"
    else
        None

/// Calculation groups: each table's rows are its items, ordered by ordinal; its column with
/// source column Name holds their names, the one with source column Ordinal their ordinals.
let private bindCalculationGroups (db: Database) (store: DataStore) (model: Tessera.DaxEngine.Model) =
    let refused = duplicatePrecedence db |> List.tryHead
    let groups = ResizeArray<BCalcGroup>()

    let visual =
        try
            db.Model.SelectionExpressionBehavior = SelectionExpressionBehaviorType.Visual
        with _ ->
            false

    for t in db.Model.Tables do
        if not (isNull t.CalculationGroup) then
            let table = model.TableByName.[t.Name]
            let g = BCalcGroup(t.Name, table, t.CalculationGroup.Precedence)

            let items =
                t.CalculationGroup.CalculationItems
                |> Seq.mapi (fun i ci -> i, ci)
                |> Seq.sortBy (fun (i, ci) -> ci.Ordinal, i)
                |> Seq.map snd
                |> Array.ofSeq

            g.Items <-
                items
                |> Array.mapi (fun row ci ->
                    let source = {
                        Label = $"calculation item '{t.Name}'[{ci.Name}]"
                        Text = (if isNull ci.Expression then "" else ci.Expression)
                    }

                    let item =
                        BCalcItem(g, ci.Name, ci.Ordinal, row, source, lazy (parseExpression source))

                    if not (isNull ci.FormatStringDefinition) then
                        item.FormatStringExpression <-
                            Some {
                                Label = $"format string of calculation item '{t.Name}'[{ci.Name}]"
                                Text =
                                    (if isNull ci.FormatStringDefinition.Expression then
                                         ""
                                     else
                                         ci.FormatStringDefinition.Expression)
                            }

                    item)

            g.MultipleOrEmpty <- multipleOrEmpty db t
            g.MultipleIsBlank <- visual

            g.NoSelection <- noSelection t
            g.Refused <- refused

            for c in table.Columns do
                let source =
                    match t.Columns.Find c.Name with
                    | :? DataColumn as d when not (isNull d.SourceColumn) -> d.SourceColumn
                    | _ -> ""

                let builder = ColumnBuilder(c.Name, c.DataType, store.Collation)

                match source.ToUpperInvariant(), c.DataType with
                | "NAME", DaxType.String ->
                    for ci in items do
                        builder.AddString ci.Name

                    c.SetData(builder.Build())
                | "ORDINAL", DaxType.Integer ->
                    for ci in items do
                        builder.AddInteger(int64 ci.Ordinal)

                    c.SetData(builder.Build())

                    // "Calculation items with the default -1 aren't included in the ordering" (Learn):
                    // what the Ordinal column holds for them is not documented.
                    if items |> Array.exists (fun ci -> ci.Ordinal < 0) && table.Gate.IsNone then
                        table.Gate <-
                            Some
                                $"the Ordinal column of calculation group '{t.Name}', which has items with no ordinal (-1)"
                | _ ->
                    if table.Gate.IsNone then
                        table.Gate <-
                            Some
                                $"column '{t.Name}'[{c.Name}] of a calculation group, which is not its text Name column or its whole-number Ordinal column"

                    if c.Parsed.IsNone && c.Expression.IsNone then
                        c.SetData(StoreColumn.blank store.Collation c.Name c.DataType items.Length)

            table.SetRowCount items.Length
            table.SetBlankRow false
            table.CalcGroup <- g
            groups.Add g

    model.CalcGroups <- groups |> Seq.sortByDescending _.Precedence |> Array.ofSeq

// ---- prepare -----------------------------------------------------------------------------------

/// What a query's DEFINE COLUMN and DEFINE TABLE add to the model, for that query alone.
type private QueryObjects = {
    Source: SourceText
    Defs: QueryDefs
    /// DEFINE COLUMN: the table, the column's name, its expression.
    Columns: (TableName * Ident * Expr) list
    /// DEFINE TABLE: the table's name, its expression.
    Tables: (TableName * Expr) list
    /// Called once the tables exist and before anything is computed: the query's measures and
    /// functions are registered here, so the query's columns and tables can use them.
    Bound: Tessera.DaxEngine.Model -> unit
    /// The model as `prepare` bound it: what it computed is taken from there.
    Prepared: Tessera.DaxEngine.Model
}

/// The model's calendars (calendar-based time intelligence: CalendarModel.fs, Calendars.fs).
let private bindCalendars (db: Database) (model: Tessera.DaxEngine.Model) =
    let set = CalendarSet()
    let sortBy = Dictionary<BColumn, BColumn>(HashIdentity.Reference)

    let categoryOf (u: TimeUnit) : Category option =
        match u with
        | TimeUnit.Year -> Some Category.Year
        | TimeUnit.Quarter -> Some Category.Quarter
        | TimeUnit.QuarterOfYear -> Some Category.QuarterOfYear
        | TimeUnit.Month -> Some Category.Month
        | TimeUnit.MonthOfYear -> Some Category.MonthOfYear
        | TimeUnit.MonthOfQuarter -> Some Category.MonthOfQuarter
        | TimeUnit.Week -> Some Category.Week
        | TimeUnit.WeekOfYear -> Some Category.WeekOfYear
        | TimeUnit.WeekOfQuarter -> Some Category.WeekOfQuarter
        | TimeUnit.WeekOfMonth -> Some Category.WeekOfMonth
        | TimeUnit.Date -> Some Category.Date
        | TimeUnit.DayOfYear -> Some Category.DayOfYear
        | TimeUnit.DayOfQuarter -> Some Category.DayOfQuarter
        | TimeUnit.DayOfMonth -> Some Category.DayOfMonth
        | TimeUnit.DayOfWeek -> Some Category.DayOfWeek
        | _ -> None

    for t in db.Model.Tables do
        match model.TableByName.TryGetValue t.Name with
        | true, bt when t.Calendars.Count > 0 ->
            let column (c: Column) : BColumn option =
                if isNull c then
                    None
                else
                    match bt.ColumnByName.TryGetValue c.Name with
                    | true, b -> Some b
                    | _ -> None

            for c in t.Columns do
                match column c, column c.SortByColumn with
                | Some a, Some b -> sortBy.[a] <- b
                | _ -> ()

            // "The columns specified in the sort-by-column property are automatically considered
            // as associated columns" (SQLBI, "Introducing calendar-based time intelligence in DAX").
            let withSortBy (cols: BColumn list) =
                cols
                @ (cols
                   |> List.choose (fun c ->
                       match sortBy.TryGetValue c with
                       | true, s -> Some s
                       | _ -> None))
                |> List.distinctBy (fun c -> c.Id)

            let calendars = ResizeArray<BCalendar>()

            for cal in t.Calendars do
                let groups = ResizeArray<CalendarGroup>()
                let related = ResizeArray<BColumn>()
                let mutable problem = None

                for g in cal.CalendarColumnGroups do
                    match g with
                    | :? TimeUnitColumnAssociation as a ->
                        match categoryOf a.TimeUnit, column a.PrimaryColumn with
                        | None, _ ->
                            problem <-
                                Some
                                    $"the calendar '{cal.Name}' uses the category {a.TimeUnit}, which no source describes"
                        | _, None -> problem <- Some $"the calendar '{cal.Name}' has a category with no primary column"
                        | Some cat, Some p ->
                            if groups.Exists(fun x -> x.Category = cat) then
                                problem <- Some $"the calendar '{cal.Name}' has the category {Category.name cat} twice"

                            let associated =
                                withSortBy (p :: ([ for c in a.AssociatedColumns -> column c ] |> List.choose id))
                                |> List.filter (fun c -> not (obj.ReferenceEquals(c, p)))

                            groups.Add {
                                Category = cat
                                Primary = p
                                Associated = associated
                            }
                    | :? TimeRelatedColumnGroup as r ->
                        for c in withSortBy ([ for c in r.Columns -> column c ] |> List.choose id) do
                            related.Add c
                    | _ -> problem <- Some $"the calendar '{cal.Name}' has a kind of column group no source describes"

                let bc =
                    BCalendar(cal.Name, bt, List.ofSeq groups, related |> Seq.distinctBy (fun c -> c.Id) |> List.ofSeq)

                bc.Problem <- problem
                calendars.Add bc
                set.ByName.[cal.Name] <- bc

            set.ByTable.[bt] <- List.ofSeq calendars

            for bc in calendars do
                match bc.Problem with
                | Some why -> set.TableProblem.[bt] <- why
                | None -> ()

                for g in bc.Groups do
                    for c in g.Primary :: g.Associated do
                        match set.CategoryOf.TryGetValue c with
                        | true, other when other <> g.Category ->
                            // "Columns must have a consistent category across calendars" (Learn).
                            set.TableProblem.[bt] <-
                                $"its calendars give the column '{bt.Name}'[{c.Name}] two categories"
                        | _ -> set.CategoryOf.[c] <- g.Category

                for c in bc.TimeRelated do
                    set.TimeRelated.Add c |> ignore

            for c in set.TimeRelated do
                if obj.ReferenceEquals(c.Table, bt) && set.CategoryOf.ContainsKey c then
                    set.TableProblem.[bt] <-
                        $"its calendars have the column '{bt.Name}'[{c.Name}] both in a category and as time-related"
        | _ -> ()

    if set.ByName.Count > 0 then
        CalendarRegistry.register model set
        Calendars.registerSortBy model sortBy
        Calendars.install ()

let private bindWith
    (clock: Clock)
    (db: Database)
    (store: DataStore)
    (query: QueryObjects option)
    : Tessera.DaxEngine.Model * (ObjectRef * TessError) list =
    let model = Tessera.DaxEngine.Model(store.Collation)
    model.Now <- clock.Now
    model.UtcNow <- clock.UtcNow
    model.Culture <- (if isNull db.Model.Culture then "" else db.Model.Culture)

    /// The table as `prepare` computed it, when binding again for a query.
    let computed (name: string) : BTable option =
        match query with
        | Some q ->
            match q.Prepared.TableByName.TryGetValue name with
            | true, t -> Some t
            | _ -> None
        | None -> None

    /// Take what `prepare` computed for a calculated column: its values, or why it has none.
    let reuse (original: BColumn) (column: BColumn) =
        column.DataType <- original.DataType
        column.SourceName <- original.SourceName

        match original.Failure with
        | Some e -> column.Failure <- Some e
        | None ->
            if original.IsReady then
                column.SetData original.Stored

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

                if c.IsKey && declared = DaxType.DateTime then
                    column.DateKey <- (if t.DataCategory = "Time" then 2 else 1)

                if c.IsKey then
                    table.MarkedKey <- Some column

                column.IsDeclaredUnique <- c.IsKey || c.IsUnique

                columns.Add column
                table.ColumnByName.[c.Name] <- column

        table.Columns <- columns.ToArray()

        if calculated.IsNone then
            table.SetRowCount rows

        // Binding again for a query: the calculated columns and tables are not computed twice.
        match computed t.Name with
        | Some original ->
            if calculated.IsSome && (original.IsReady || original.Failure.IsSome) then
                // A calculated table's columns are those its expression returned.
                let copies =
                    original.Columns
                    |> Array.map (fun o ->
                        let c = nextColumn table o.Name o.DataType
                        c.Expression <- o.Expression
                        reuse o c
                        c)

                table.ColumnByName.Clear()

                for c in copies do
                    table.ColumnByName.[c.Name] <- c

                table.Columns <- copies
                table.Failure <- original.Failure

                if original.IsReady then
                    table.SetRowCount original.RowCount
            else
                for c in table.Columns do
                    match c.Expression, original.ColumnByName.TryGetValue c.Name with
                    | Some _, (true, o) -> reuse o c
                    | _ -> ()
        | None -> ()

    // What the query defines. Whether a query column or table may take the name of one the
    // model has is not documented.
    match query with
    | Some q ->
        let refuse (span: Span) (what: string) =
            raise (EngineException(DaxNotSupported(what, SourceText.where q.Source span)))

        for (name, expr) in q.Tables do
            if model.TableByName.ContainsKey name.Name then
                refuse name.Span $"DEFINE TABLE '{name.Name}': the model already has a table of that name"

            let table = BTable(tables.Count, model, name.Name)
            table.Expression <- Some q.Source
            table.Parsed <- Some expr
            tables.Add table
            model.TableByName.[name.Name] <- table

        for (tableName, name, expr) in q.Columns do
            match model.TableByName.TryGetValue tableName.Name with
            | true, table ->
                if table.ColumnByName.ContainsKey name.Name then
                    refuse
                        name.Span
                        $"DEFINE COLUMN '{table.Name}'[{name.Name}]: the table already has a column of that name"

                if table.Parsed.IsSome then
                    refuse name.Span $"DEFINE COLUMN on the query table '{table.Name}'"

                let column = nextColumn table name.Name DaxType.Variant
                column.Expression <- Some q.Source
                column.Parsed <- Some expr
                table.Columns <- Array.append table.Columns [| column |]
                table.ColumnByName.[name.Name] <- column
            | _ ->
                raise (
                    EngineException(
                        DaxInvalid($"there is no table '{tableName.Name}'", SourceText.where q.Source tableName.Span)
                    )
                )
    | None -> ()

    model.Tables <- tables.ToArray()
    bindCalculationGroups db store model
    bindCalendars db model

    // Relationships the engine does: single column, many-to-one (either way round), one-to-one and
    // many-to-many.
    let rels = ResizeArray<BRel>()

    // A one-to-one relationship has no direction: "the cross filter direction is always from both
    // tables" (Learn, "Model relationships in Power BI Desktop"), and the two expanded tables are
    // "the same ... joining the columns of two tables into a larger table using a FULL OUTER JOIN"
    // (SQLBI, "Expanded tables in DAX"). The engine joins the rows of `From` to `To`, which needs
    // every key of `To` in `From` (`BRel.FromRowOfToRow`); so the ends are taken the way round the
    // data allows, whichever way the model declares them. Whether a column has a value the other
    // lacks: only for data columns of data tables, of the same type (a calculated key keeps the
    // declared ends, so that a query that binds the model again, with it computed, keeps them too).
    let stored (c: BColumn) =
        c.IsReady
        && c.Expression.IsNone
        && c.Parsed.IsNone
        && c.Table.Expression.IsNone
        && c.Table.Parsed.IsNone

    let lacks (a: BColumn) (b: BColumn) =
        stored a
        && stored b
        && a.Stored.DataType = b.Stored.DataType
        && a.Stored.Values
           |> Array.exists (fun v -> not (DaxValue.isBlank v) && b.Stored.TryFindId v < 0)

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

            // (from, to, limited). A many-to-many relationship keeps the model's ends: in one
            // direction its `ToColumn`'s table filters its `FromColumn`'s, as for any relationship.
            // A one-to-one relationship keeps them too, and always filters both ways.
            let ends =
                match s.FromCardinality, s.ToCardinality with
                | RelationshipEndCardinality.Many, RelationshipEndCardinality.One ->
                    Some(s.FromColumn, s.ToColumn, false)
                | RelationshipEndCardinality.One, RelationshipEndCardinality.Many ->
                    Some(s.ToColumn, s.FromColumn, false)
                | RelationshipEndCardinality.Many, RelationshipEndCardinality.Many ->
                    Some(s.FromColumn, s.ToColumn, true)
                | RelationshipEndCardinality.One, RelationshipEndCardinality.One when s.IsActive ->
                    Some(s.FromColumn, s.ToColumn, false)
                | _ -> None

            let oneToOne =
                s.FromCardinality = RelationshipEndCardinality.One
                && s.ToCardinality = RelationshipEndCardinality.One

            match ends with
            | Some(many, one, limited) ->
                let ends =
                    match column many, column one with
                    | Some f, Some t when oneToOne && lacks t f && not (lacks f t) -> Some t, Some f
                    | f, t -> f, t

                match ends with
                | Some f, Some t when f.Table.Gate.IsNone ->
                    let r =
                        BRel(
                            rels.Count,
                            (if isNull s.Name then "" else s.Name),
                            f,
                            t,
                            (oneToOne || s.CrossFilteringBehavior = CrossFilteringBehavior.BothDirections),
                            s.IsActive
                        )

                    r.Limited <- limited
                    r.OneToOne <- oneToOne
                    rels.Add r
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

            model.Measures.[m.Name].FormatString <- m.FormatString
            model.Measures.[m.Name].HasFormatStringExpression <- not (isNull m.FormatStringDefinition)

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
                model.Computing.Push(c)

                try
                    fail (fun e -> c.Failure <- Some e) (fun () ->
                        let t = c.Table
                        let n = t.RowCount

                        let narrows, changes =
                            transitionTraits
                                model
                                (query |> Option.map _.Defs)
                                (match c.Parsed with
                                 | Some parsed -> parsed
                                 | None -> parseExpression source)

                        c.MayNarrow <- narrows
                        c.ChangesRelationships <- changes

                        // A query column: "populated in an empty filter context before the query
                        // is executed" (dax.guide/st/column), and it "can use any of the defined
                        // entities" (Microsoft Learn, DEFINE).
                        let expr, ctx =
                            match c.Parsed, query with
                            | Some parsed, Some q -> parsed, Context.root model q.Defs source true
                            | _ -> parseExpression source, Context.root model null source false

                        let table = Tables.ofBase model.DefaultConfig t (Array.init n id)

                        let table = Shadows.notAnIteration "the row context of a calculated column" table

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
                    model.Computing.Pop() |> ignore

    model.ForceTable <-
        fun (t: BTable) ->
            match t.Failure, t.Expression with
            | Some e, _ -> raise (EngineException e)
            | None, _ when t.Computing ->
                raise (EngineException(DaxInvalid($"circular dependency: table '{t.Name}' depends on itself", "")))
            | None, None -> t.SetRowCount 0
            | None, Some source ->
                t.Computing <- true
                model.Computing.Push(t)

                try
                    fail (fun e -> t.Failure <- Some e) (fun () ->
                        // A query table: "populated in an empty filter context before the query is
                        // executed" (dax.guide/st/table).
                        let expr, ctx =
                            match t.Parsed, query with
                            | Some parsed, Some q -> parsed, Context.root model q.Defs source true
                            | _ -> parseExpression source, Context.root model null source false

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
                    model.Computing.Pop() |> ignore

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
                    // "The additional blank row is never added to tables involved in a limited
                    // relationship" (SQLBI, "Relationships in Power BI and Tabular models"): a
                    // limited relationship adds none.
                    let incoming =
                        model.Rels
                        |> Array.filter (fun r -> obj.ReferenceEquals(r.To.Table, t) && not r.Limited)

                    // A one-to-one relationship joins the two tables row for row (`BRel.OneToOne`):
                    // `From` would need a blank row for keys of `To` it lacks (refused when the
                    // relationship is checked), and a blank row either side has for another reason
                    // isn't implemented with it.
                    for r in model.Rels do
                        if r.OneToOne && obj.ReferenceEquals(r.From.Table, t) then
                            try
                                r.Check()
                            with EngineException(DaxNotSupported(why, _)) ->
                                refuse why

                    let oneToOne =
                        model.Rels
                        |> Array.tryFind (fun r ->
                            r.OneToOne
                            && (obj.ReferenceEquals(r.From.Table, t) || obj.ReferenceEquals(r.To.Table, t)))

                    // An inactive relationship counts as an active one does: "Inactive relationships
                    // are also expanded, even when the relationship isn't used by a calculation ...
                    // When a matching value from the "many" to the "one" side doesn't exist, a blank
                    // virtual row is added to the "one" side table" (Learn, "Model relationships in
                    // Power BI Desktop"). Which relationships filter and what RELATED reads still
                    // follow the active ones (SQLBI, "Expanded tables in DAX").
                    if incoming |> Array.exists (fun r -> r.OrphanFromRows.Length > 0) then
                        match oneToOne with
                        | Some o when
                            incoming
                            |> Array.exists (fun r -> not (obj.ReferenceEquals(r, o)) && r.OrphanFromRows.Length > 0)
                            ->
                            refuse
                                $"the blank row of '{t.Name}', which is in the one-to-one relationship {o.Describe} and has keys without a match in another relationship"
                        | _ -> ()

                        // "a special row added to a table, in case the table is on the one-side of
                        // a strong relationship and the relationship is invalid" (SQLBI, "Blank
                        // row in DAX").
                        t.SetBlankRow true
                    else
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

    match query with
    | Some q -> q.Bound model
    | None -> ()

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
        attempt (ObjectRef.modelLevel ObjectKind.Relationship r.Name) (fun () -> r.Check())

    for t in model.Tables do
        if t.Gate.IsNone then
            attempt (ObjectRef.table t.Name) (fun () -> t.HasBlankRow |> ignore)

    // Computed, but not to be read by queries (see `groupedBy`).
    let grouped = groupedBy db

    for t in model.Tables do
        for c in t.Columns do
            match grouped.TryGetValue(t.Name + "\u0000" + c.Name) with
            | true, why ->
                c.Gate <- Some why

                // Its Group By Columns, as the engine's columns; one the engine doesn't have
                // leaves the column gated everywhere.
                let source = db.Model.Tables.[t.Name].Columns.[c.Name]

                let by =
                    source.RelatedColumnDetails.GroupByColumns
                    |> Seq.map (fun g ->
                        if isNull g.GroupingColumn then
                            None
                        else
                            match model.TableByName.TryGetValue g.GroupingColumn.Table.Name with
                            | true, bt ->
                                match bt.ColumnByName.TryGetValue g.GroupingColumn.Name with
                                | true, bc -> Some bc
                                | _ -> None
                            | _ -> None)
                    |> Array.ofSeq

                if by |> Array.forall Option.isSome then
                    c.GroupBy <- by |> Array.choose id
            | _ -> ()

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

        run (fun () -> bindWith clock db store None)
        |> Result.bind (fun (model, failures) ->
            match failures |> List.tryFind (fun (_, e) -> isCircular e) with
            | Some(_, e) -> Error e
            | None ->
                Ok {
                    Model = model
                    Database = db
                    Store = store
                    Failures = failures
                    Identity = None
                })

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
    if e.StartAt.Length > e.OrderBy.Length then
        Fail.invalid ctx e.StartAt.Head.Span "START AT has more values than ORDER BY has keys"

    let table = ev.Table(ctx, e.Table)
    let table = Shadows.notAnIteration "the rows ORDER BY sorts" table

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
            let sorted = List.init table.Count id |> List.sortWith compare |> Array.ofList

            if e.StartAt.IsEmpty then
                sorted
            else
                // "defines the rows to skip before the specified values according to the sort
                // order specified in the ORDER BY section" (dax.guide/st/start-at); "The second
                // argument in START AT defines the starting value in column 2 of the ORDER BY
                // columns within the rows that meet the first value for column 1" (Microsoft
                // Learn, START AT): the rows whose first keys come before the values, in the
                // order of the keys and their directions, are skipped.
                let starts =
                    e.StartAt
                    |> List.map (fun v ->
                        let value = ev.Scalar(ctx, v)

                        if DaxValue.isBlank value then
                            Fail.notSupported ctx v.Span "a START AT value that is BLANK"

                        v.Span, value)

                let before (row: int) =
                    let mutable c = 0
                    let mutable rest = List.zip (List.take starts.Length keys) starts

                    while c = 0 && not rest.IsEmpty do
                        let (values: DaxValue array, descending), (span, start) = rest.Head

                        // Where BLANK sorts against the value is not settled by `compare`.
                        if DaxValue.isBlank values.[row] then
                            Fail.notSupported ctx span "START AT on a key that is BLANK in some row"

                        c <-
                            match DaxValue.compare ctx.Model.Collation values.[row] start with
                            | Ok c -> c
                            | Error _ ->
                                Fail.notSupported
                                    ctx
                                    span
                                    "a START AT value of a different data type than its ORDER BY key"

                        if descending then
                            c <- -c

                        rest <- rest.Tail

                    c < 0

                sorted |> Array.filter (fun row -> not (before row))

    toResult table order

/// Run a DAX query: one result per EVALUATE.
///
/// DEFINE MEASURE, VAR, FUNCTION, COLUMN and TABLE are supported; ORDER BY sorts by any expressions over the
/// result's columns. Without ORDER BY the rows come in the order the engine produced them, which
/// (as on a server) is not defined.
let evaluateQuery (p: Prepared) (dax: string) : Result<QueryResult list, TessError> =
    match DaxParser.parseQuery dax with
    | Error errors -> Error(syntaxError "query" errors)
    | Ok query ->
        run (fun () ->
            let source = SourceText.query dax
            let defs = QueryDefs()

            // The query's measures and functions: any definition can use any other, whatever
            // their order ("Definitions can reference other definitions that appear before or
            // after the current definition": Microsoft Learn, "DAX queries").
            let register (model: Tessera.DaxEngine.Model) =
                for d in query.Definitions do
                    match d.Kind with
                    | DefinitionKind.Measure(table, name, value) ->
                        match model.TableByName.TryGetValue table.Name with
                        | true, t -> defs.Measures.[name.Name] <- BMeasure(name.Name, t, source, lazy value)
                        | _ ->
                            raise (
                                EngineException(
                                    DaxInvalid($"there is no table '{table.Name}'", SourceText.where source table.Span)
                                )
                            )
                    | DefinitionKind.Function(name, def) ->
                        defs.Functions.[name.Name] <- BFunction(name.Name, source, lazy def)
                    | _ -> ()

            let columns =
                query.Definitions
                |> List.choose (fun d ->
                    match d.Kind with
                    | DefinitionKind.Column(table, name, value) -> Some(table, name, value)
                    | _ -> None)

            let tables =
                query.Definitions
                |> List.choose (fun d ->
                    match d.Kind with
                    | DefinitionKind.Table(name, value) -> Some(name, value)
                    | _ -> None)

            // DEFINE COLUMN and DEFINE TABLE: the model is bound again for this query with them
            // in it, and they are computed before anything else (`p` itself never changes).
            let model =
                if columns.IsEmpty && tables.IsEmpty then
                    register p.Model
                    p.Model
                else
                    // They are computed before the query's variables exist.
                    for d in query.Definitions do
                        match d.Kind with
                        | DefinitionKind.Var(name, _) ->
                            let upper = name.Name.ToUpperInvariant()

                            let uses =
                                (columns |> List.exists (fun (_, _, e) -> Variables.refersTo upper e))
                                || (tables |> List.exists (fun (_, e) -> Variables.refersTo upper e))

                            if uses then
                                raise (
                                    EngineException(
                                        DaxNotSupported(
                                            $"a query column or table that uses the query variable {name.Name}",
                                            SourceText.where source name.Span
                                        )
                                    )
                                )
                        | _ -> ()

                    let model, failures =
                        bindWith
                            {
                                Now = p.Model.Now
                                UtcNow = p.Model.UtcNow
                            }
                            p.Database
                            p.Store
                            (Some {
                                Source = source
                                Defs = defs
                                Columns = columns
                                Tables = tables
                                Bound = register
                                Prepared = p.Model
                            })

                    match failures |> List.tryFind (fun (_, e) -> isCircular e) with
                    | Some(_, e) -> raise (EngineException e)
                    | None -> model

            let mutable ctx = {
                Context.root model defs source true with
                    Identity = p.Identity
            }

            let queryVars = ResizeArray<Ident * Lazy<Value> * bool>()

            // Whether the rest of the query refers to a variable: a later variable (until one of
            // the same name hides it) or an EVALUATE. Measures and functions don't see variables.
            let usedLater (name: string) (after: Definition list) =
                let name = name.ToUpperInvariant()

                let rec inDefinitions (rest: Definition list) =
                    match rest with
                    | [] ->
                        query.Evaluates
                        |> List.exists (fun e ->
                            Variables.refersTo name e.Table
                            || e.OrderBy |> List.exists (fun k -> Variables.refersTo name k.Expr)
                            || e.StartAt |> List.exists (Variables.refersTo name))
                    | d :: tail ->
                        match d.Kind with
                        | DefinitionKind.Var(other, value) ->
                            Variables.refersTo name value
                            || (other.Name.ToUpperInvariant() <> name && inDefinitions tail)
                        | DefinitionKind.Table(_, value) -> Variables.refersTo name value || inDefinitions tail
                        | _ -> inDefinitions tail

                inDefinitions after

            let mutable remaining = query.Definitions

            for d in query.Definitions do
                remaining <- remaining.Tail

                match d.Kind with
                | DefinitionKind.Measure _
                | DefinitionKind.Function _
                | DefinitionKind.Table _
                | DefinitionKind.Column _ -> ()
                | DefinitionKind.Var(name, value) ->
                    // A query variable: in scope in everything after it, evaluated once.
                    if model.TableByName.ContainsKey name.Name then
                        // "Names of existing tables aren't allowed" (Microsoft Learn, VAR).
                        Fail.invalid
                            ctx
                            name.Span
                            $"a variable can't be named {name.Name}: that is the name of a table of the model"

                    let definedIn = ctx
                    let isTable = ev.IsTable(definedIn, value)
                    let lazyValue = lazy (ev.Value(definedIn, value))

                    let staticType =
                        lazy
                            (if isTable then
                                 StaticType.Unknown
                             else
                                 ev.TypeOf(definedIn, value))

                    queryVars.Add((name, lazyValue, usedLater name.Name remaining))

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
                | DefinitionKind.MParameter(name, _) -> Fail.notSupported ctx name.Span "DEFINE MPARAMETER"

            let results = query.Evaluates |> List.map (evaluateOne ctx)
            // A variable the query refers to and didn't need is still evaluated (see `settle`).
            Variables.settle ctx (List.ofSeq queryVars)
            results)

/// Evaluate one scalar expression with no filter context and no row context, as `EVALUATE ROW (
/// "v", <expression> )` would.
let evaluateScalar (p: Prepared) (dax: string) : Result<QueryValue, TessError> =
    let source = { Label = "expression"; Text = dax }

    match DaxParser.parseExpression dax with
    | Error errors -> Error(syntaxError source.Label errors)
    | Ok expr ->
        run (fun () ->
            let ctx = {
                Context.root p.Model null source true with
                    Identity = p.Identity
            }

            let _, values = Typing.column (ev.TypeOf(ctx, expr)) [| ev.Scalar(ctx, expr) |]
            queryValue values.[0])

// ---- coverage ----------------------------------------------------------------------------------

/// `coverage`, on the calling thread.
let private coverageOf (db: Database) : (ObjectRef * string list) list =
    if isNull db.Model then
        []
    else
        let gates = staticGates db
        let grouped = groupedBy db
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

                    match grouped.TryGetValue(t.Name + "\u0000" + name) with
                    | true, why -> found.Add why
                    | _ -> ()

                    match calculatedColumns.TryGetValue(t.Name + "\u0000" + name) with
                    | true, text -> found.AddRange(ofCalculated ("c\u0000" + t.Name + "\u0000" + name) text)
                    | _ ->
                        if not (columns.Contains(t.Name + "\u0000" + name)) && measures.ContainsKey name then
                            found.AddRange(ofMeasure name)
                | Bracketed name when measures.ContainsKey name -> found.AddRange(ofMeasure name)
                | QueryParameter name -> found.Add $"the query parameter @{name}"
                // NAMEOF names its argument without evaluating it: what the object uses is not used.
                | Call(name, args) when String.Equals(name.Name, "NAMEOF", StringComparison.OrdinalIgnoreCase) ->
                    match Registry.byName.TryGetValue name.Name with
                    | true, fn ->
                        match fn.Check args with
                        | Some why -> found.Add why
                        | None -> ()
                    | _ -> ()
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

        // What refuses every measure reference: it is reported for each measure and item.
        let modelWide = calculationGroupProblems db

        [
            for t in db.Model.Tables do
                for m in t.Measures do
                    yield
                        ObjectRef.inTable ObjectKind.Measure t.Name m.Name,
                        modelWide @ ofMeasure m.Name |> List.distinct

                // A calculation item is reported like a measure: what its expression uses (its format
                // string expression is never evaluated).
                if not (isNull t.CalculationGroup) then
                    for ci in t.CalculationGroup.CalculationItems do
                        let selection =
                            if isNull t.CalculationGroup.MultipleOrEmptySelectionExpression then
                                []
                            else
                                [ $"the multipleOrEmptySelectionExpression of calculation group '{t.Name}'" ]

                        yield
                            ObjectRef.inTable ObjectKind.CalculationItem t.Name ci.Name,
                            modelWide @ selection @ ofText t.Name ci.Expression |> List.distinct

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
