/// `script apply`: create or update each object of a DAX script, in order. Objects the script doesn't
/// mention are left alone, and so are properties it doesn't set. New objects are created the way
/// `add` creates them. References between objects are not checked here: run it through
/// Mutation.apply, whose validation (and the save gate) sees the whole script applied, so objects
/// may refer to ones defined later in it.
module Tessera.ApplyScript

open System
open Microsoft.AnalysisServices.Tabular
open Tessera.DaxScript

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// Exact case first, then any case (Tabular names are case-insensitive).
let private pick (items: 'T seq) (nameOf: 'T -> string) (wanted: string) =
    let items = List.ofSeq items

    items
    |> List.tryFind (fun x -> nameOf x = wanted)
    |> Option.orElse (items |> List.tryFind (fun x -> eqCi (nameOf x) wanted))

/// Expressions are the same if they differ only in line breaks (\r\n or \n) and in leading and
/// trailing whitespace, which a script doesn't keep.
let private sameExpression (a: string) (b: string) =
    ExportScript.expressionOf a = ExportScript.expressionOf b

let private sameText (a: string) (b: string) =
    ExportScript.normalize a = ExportScript.normalize b

let private nullIfEmpty (s: string) = if s = "" then null else s

/// Write a string property if it changes; "" clears it.
let private setText (current: string) (value: string) (write: string -> unit) =
    if not (sameText current value) then
        write (nullIfEmpty value)

let private setExpression (current: string) (value: string) (write: string -> unit) =
    if not (sameExpression current value) then
        write value

let private setHidden (isHidden: bool) (visible: bool) (write: bool -> unit) =
    if isHidden = visible then
        write (not visible)

/// A format string definition: "" removes it.
let private setFormatDefinition (current: FormatStringDefinition) (value: string) write =
    match current, value with
    | null, "" -> ()
    | _, "" -> write null
    | null, v -> write (new FormatStringDefinition(Expression = v))
    | d, v -> setExpression d.Expression v (fun v -> d.Expression <- v)

/// A detail rows definition: "" removes it.
let private setDetailRows (current: DetailRowsDefinition) (value: string) write =
    match current, value with
    | null, "" -> ()
    | _, "" -> write null
    | null, v -> write (new DetailRowsDefinition(Expression = v))
    | d, v -> setExpression d.Expression v (fun v -> d.Expression <- v)

/// The KPI properties: script name -> (is DAX, read, write).
let private kpiProperties: (string * (bool * (KPI -> string) * (KPI -> string -> unit))) list = [
    "KpiStatusExpression", (true, (fun k -> k.StatusExpression), (fun k v -> k.StatusExpression <- v))
    "KpiStatusDescription", (false, (fun k -> k.StatusDescription), (fun k v -> k.StatusDescription <- v))
    "KpiStatusGraphic", (false, (fun k -> k.StatusGraphic), (fun k v -> k.StatusGraphic <- v))
    "KpiTrendExpression", (true, (fun k -> k.TrendExpression), (fun k v -> k.TrendExpression <- v))
    "KpiTrendDescription", (false, (fun k -> k.TrendDescription), (fun k v -> k.TrendDescription <- v))
    "KpiTrendGraphic", (false, (fun k -> k.TrendGraphic), (fun k v -> k.TrendGraphic <- v))
    "KpiTargetExpression", (true, (fun k -> k.TargetExpression), (fun k v -> k.TargetExpression <- v))
    "KpiTargetDescription", (false, (fun k -> k.TargetDescription), (fun k v -> k.TargetDescription <- v))
    "KpiTargetFormatString", (false, (fun k -> k.TargetFormatString), (fun k v -> k.TargetFormatString <- v))
]

let private setKpi (m: Measure) (name: string) (value: string) =
    match List.tryFind (fst >> (=) name) kpiProperties with
    | None -> ()
    | Some(_, (isDax, read, write)) ->
        match m.KPI with
        | null when value = "" -> ()
        | null ->
            m.KPI <- new KPI()
            write m.KPI value
        | k ->
            let same =
                if isDax then
                    sameExpression (read k) value
                else
                    sameText (read k) value

            if not same then
                write k (nullIfEmpty value)

let private textOf (p: Property) =
    match p.Value with
    | Value.String s
    | Value.Expression s
    | Value.DataType s -> s
    | Value.Bool b -> string b
    | Value.Int i -> string i

let private measureProperties (m: Measure) (props: Property list) =
    let hasDefinition =
        props |> List.exists (fun p -> p.Name = "FormatStringDefinition")

    for p in props do
        match p.Name, p.Value with
        | "Description", Value.String v -> setText m.Description v (fun v -> m.Description <- v)
        | "DisplayFolder", Value.String v -> setText m.DisplayFolder v (fun v -> m.DisplayFolder <- v)
        | "Visible", Value.Bool v -> setHidden m.IsHidden v (fun h -> m.IsHidden <- h)
        | "FormatString", Value.String v ->
            // A static format string replaces a dynamic one, unless the script also sets that.
            setText m.FormatString v (fun v -> m.FormatString <- v)

            if not hasDefinition then
                setFormatDefinition m.FormatStringDefinition "" (fun d -> m.FormatStringDefinition <- d)
        | "FormatString", Value.Expression "" ->
            setText m.FormatString "" (fun v -> m.FormatString <- v)
            setFormatDefinition m.FormatStringDefinition "" (fun d -> m.FormatStringDefinition <- d)
        | ("FormatString" | "FormatStringDefinition"), Value.Expression v ->
            setFormatDefinition m.FormatStringDefinition v (fun d -> m.FormatStringDefinition <- d)
        | "DetailRows", Value.Expression v ->
            setDetailRows m.DetailRowsDefinition v (fun d -> m.DetailRowsDefinition <- d)
        | name, _ when name.StartsWith "Kpi" -> setKpi m name (textOf p)
        | _ -> ()

let private columnProperties (c: Column) (props: Property list) =
    for p in props do
        match p.Name, p.Value with
        | "Description", Value.String v -> setText c.Description v (fun v -> c.Description <- v)
        | "DisplayFolder", Value.String v -> setText c.DisplayFolder v (fun v -> c.DisplayFolder <- v)
        | "FormatString", Value.String v -> setText c.FormatString v (fun v -> c.FormatString <- v)
        | "Visible", Value.Bool v -> setHidden c.IsHidden v (fun h -> c.IsHidden <- h)
        | "Datatype", Value.DataType v ->
            match List.tryFind (fst >> (=) v) ExportScript.dataTypeNames with
            | Some(_, t) when c.DataType <> t -> c.DataType <- t
            | _ -> ()
        | _ -> ()

let private tableProperties (t: Table) (props: Property list) =
    for p in props do
        match p.Name, p.Value with
        | "Description", Value.String v -> setText t.Description v (fun v -> t.Description <- v)
        | "Visible", Value.Bool v -> setHidden t.IsHidden v (fun h -> t.IsHidden <- h)
        | "DetailRows", Value.Expression v ->
            setDetailRows t.DefaultDetailRowsDefinition v (fun d -> t.DefaultDetailRowsDefinition <- d)
        | "Precedence", Value.Int v when not (isNull t.CalculationGroup) ->
            if t.CalculationGroup.Precedence <> v then
                t.CalculationGroup.Precedence <- v
        | _ -> ()

let private functionProperties (f: Function) (props: Property list) =
    for p in props do
        match p.Name, p.Value with
        | "Description", Value.String v -> setText f.Description v (fun v -> f.Description <- v)
        | "Visible", Value.Bool v -> setHidden f.IsHidden v (fun h -> f.IsHidden <- h)
        | _ -> ()

/// Calculation item ordinals exist from compatibility level 1500.
let private ordinalLevel = 1500

type private State = {
    Db: Database
    Source: string
    Notes: ResizeArray<string>
    Warnings: ResizeArray<string>
    mutable Created: int
}

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private applyStatement (st: State) (s: Statement) : Result<unit, TessError> =
    let m = st.Db.Model
    let r = ExportScript.refOf s

    let at (p: Position) (e: TessError) =
        ScriptObjectFailed(st.Source, p.Line, p.Column, e)

    let refuse reason =
        Error(at s.Position (CannotScript(ObjectRef.format r, reason)))

    let needsExpression () =
        refuse $"it doesn't exist, and a new {ObjectKind.token r.Kind} needs an expression (`= ...`)"

    /// Create through `add`, then `find` what it made.
    let create (details: AddObject.Details) (find: unit -> 'T option) : Result<'T, TessError> =
        AddObject.add st.Db (AddObject.AddSpec.create r details)
        |> Result.mapError (at s.Position)
        |> Result.bind (fun effect ->
            st.Created <- st.Created + 1
            st.Warnings.AddRange effect.Warnings
            // Its "Added ..." note is in the change set already; keep the others.
            st.Notes.AddRange(effect.Notes |> List.filter (fun n -> not (n.StartsWith "Added ")))

            match find () with
            | Some x -> Ok x
            | None -> refuse "it was not created")

    let table () =
        s.Table |> Option.bind (pick m.Tables (fun t -> t.Name))

    match s.Type with
    | ObjectType.Measure ->
        let find () =
            table () |> Option.bind (fun t -> pick t.Measures (fun x -> x.Name) s.Name)

        let target =
            match find (), s.Expression with
            | Some x, e ->
                e
                |> Option.iter (fun e -> setExpression x.Expression e (fun v -> x.Expression <- v))

                Ok x
            | None, None -> needsExpression ()
            | None, Some e ->
                // `add` refuses an empty expression; a script may create a measure without one.
                create (AddObject.Details.Measure(if e = "" then "BLANK()" else e)) find
                |> Result.map (fun x ->
                    if e = "" then
                        x.Expression <- ""

                    x)

        target |> Result.map (fun x -> measureProperties x s.Properties)
    | ObjectType.Column ->
        let find () =
            table ()
            |> Option.bind (fun t ->
                pick (t.Columns |> Seq.filter (fun c -> c.Type <> ColumnType.RowNumber)) (fun x -> x.Name) s.Name)

        let target =
            match find (), s.Expression with
            | Some(:? CalculatedColumn as c), Some e ->
                setExpression c.Expression e (fun v -> c.Expression <- v)
                Ok(c :> Column)
            | Some _, Some _ -> refuse "it's a data column; only calculated columns have a DAX expression"
            | Some c, None -> Ok c
            | None, None -> needsExpression ()
            | None, Some e -> create (AddObject.Details.Column(AddObject.ColumnSource.Calculated e)) find

        target |> Result.map (fun c -> columnProperties c s.Properties)
    | ObjectType.Table ->
        let find () = pick m.Tables (fun t -> t.Name) s.Name

        let target =
            match find (), s.Expression with
            | Some t, Some e ->
                match ExportScript.calculatedSource t with
                | Some source ->
                    setExpression source.Expression e (fun v -> source.Expression <- v)
                    Ok t
                | None -> refuse "it isn't a calculated table, so it has no DAX expression"
            | Some t, None -> Ok t
            | None, None -> needsExpression ()
            | None, Some e -> create (AddObject.Details.Table(AddObject.TableSource.Calculated e)) find

        target |> Result.map (fun t -> tableProperties t s.Properties)
    | ObjectType.CalculationGroup ->
        let find () = table ()

        let target =
            match find () with
            | Some t when isNull t.CalculationGroup -> refuse "it's a table, not a calculation group"
            | Some t ->
                match ExportScript.groupColumn t with
                | Some c when not (eqCi c.Name s.Name) ->
                    refuse
                        $"its items are in column '{c.Name}', not '{s.Name}' (rename the column with `tess rename` first)"
                | _ -> Ok t
            | None ->
                let precedence =
                    s.Properties
                    |> List.tryPick (fun p ->
                        match p.Name, p.Value with
                        | "Precedence", Value.Int v -> Some v
                        | _ -> None)

                create (AddObject.Details.Table(AddObject.TableSource.CalculationGroup(precedence, Some s.Name))) find

        target |> Result.map (fun t -> tableProperties t s.Properties)
    | ObjectType.CalculationItem ->
        match table () with
        | Some t when not (isNull t.CalculationGroup) ->
            let find () =
                pick t.CalculationGroup.CalculationItems (fun i -> i.Name) s.Name

            let target =
                match find (), s.Expression with
                | Some i, e ->
                    e
                    |> Option.iter (fun e -> setExpression i.Expression e (fun v -> i.Expression <- v))

                    Ok i
                | None, None -> needsExpression ()
                | None, Some e -> create (AddObject.Details.CalculationItem(e, None, None)) find

            target
            |> Result.bind (fun i ->
                s.Properties
                |> List.fold
                    (fun acc p ->
                        acc
                        |> Result.bind (fun () ->
                            match p.Name, p.Value with
                            | "Description", Value.String v ->
                                setText i.Description v (fun v -> i.Description <- v)
                                Ok()
                            | "FormatString", Value.Expression v ->
                                setFormatDefinition i.FormatStringDefinition v (fun d ->
                                    i.FormatStringDefinition <- d)

                                Ok()
                            | "Ordinal", Value.Int _ when st.Db.CompatibilityLevel < ordinalLevel ->
                                Error(
                                    at
                                        p.Position
                                        (CannotScript(
                                            ObjectRef.format r,
                                            $"calculation item ordinals need compatibility level {ordinalLevel} or higher (the model is at {st.Db.CompatibilityLevel})"
                                        ))
                                )
                            | "Ordinal", Value.Int v ->
                                if i.Ordinal <> v then
                                    i.Ordinal <- v

                                Ok()
                            | _ -> Ok()))
                    (Ok()))
        | _ -> refuse "its calculation group doesn't exist"
    | ObjectType.Function ->
        let find () =
            pick m.Functions (fun f -> f.Name) s.Name

        let target =
            match find (), s.Expression with
            | Some f, e ->
                e
                |> Option.iter (fun e -> setExpression f.Expression e (fun v -> f.Expression <- v))

                Ok f
            | None, None -> needsExpression ()
            | None, Some e -> create (AddObject.Details.Function e) find

        target |> Result.map (fun f -> functionProperties f s.Properties)

/// Apply the statements of a script read from `source` (a path or <stdin>, for messages) to `db`.
let apply (source: string) (db: Database) (script: Statement list) : Result<Mutation.Effect, TessError> =
    let st = {
        Db = db
        Source = source
        Notes = ResizeArray()
        Warnings = ResizeArray()
        Created = 0
    }

    script
    |> List.fold (fun acc s -> acc |> Result.bind (fun () -> applyStatement st s)) (Ok())
    |> Result.map (fun () ->
        let objects = plural script.Length "object"

        if script.IsEmpty then
            st.Warnings.Add $"{source} defines no objects"

        {
            Warnings = List.ofSeq st.Warnings
            Notes = $"Applied {objects} from {source} ({st.Created} new)" :: List.ofSeq st.Notes
            Renamed = []
        })

/// Parse a script's text and apply it.
let applyText (source: string) (db: Database) (text: string) : Result<Mutation.Effect, TessError> =
    match parse text with
    | Error e -> Error(InvalidScript(source, e.Position.Line, e.Position.Column, e.Message))
    | Ok script -> apply source db script
