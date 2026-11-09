/// `script export`: the DAX objects of a model (measures, calculated columns and tables, calculation
/// groups and items, functions) as a DAX script. The script is parsed back and compared before it is
/// returned, so an object whose DAX would not read back exactly is reported, not silently changed.
module Tessera.ExportScript

open System
open System.Collections.Generic
open Microsoft.AnalysisServices.Tabular
open Tessera.Inventory
open Tessera.DaxScript

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// \r\n as \n: a script has one kind of line break.
let normalize (s: string) =
    if isNull s then "" else s.Replace("\r\n", "\n")

/// An expression as a script holds it: \n line breaks, no leading or trailing whitespace.
let expressionOf (s: string) = (normalize s).Trim()

/// The column a calculation group's items are listed in (its Name column).
let groupColumn (t: Table) : Column option =
    let columns =
        t.Columns |> Seq.filter (fun c -> c.Type <> ColumnType.RowNumber) |> List.ofSeq

    columns
    |> List.tryFind (fun c ->
        match c with
        | :? DataColumn as d -> eqCi d.SourceColumn "Name"
        | _ -> false)
    |> Option.orElse (columns |> List.tryFind (fun c -> not (eqCi c.Name "Ordinal")))

/// The DAX of a calculated table (its calculated partition).
let calculatedSource (t: Table) : CalculatedPartitionSource option =
    if isNull t.CalculationGroup then
        t.Partitions
        |> Seq.tryPick (fun p ->
            match p.Source with
            | :? CalculatedPartitionSource as s -> Some s
            | _ -> None)
    else
        None

/// TE3's data type names for a calculated column, and the TOM types they stand for.
let dataTypeNames = [
    "BOOLEAN", DataType.Boolean
    "DOUBLE", DataType.Double
    "INTEGER", DataType.Int64
    "DATETIME", DataType.DateTime
    "CURRENCY", DataType.Decimal
    "STRING", DataType.String
]

/// The reference of the object a statement is about.
let refOf (s: Statement) : ObjectRef =
    let table = defaultArg s.Table ""

    match s.Type with
    | ObjectType.Measure -> ObjectRef.inTable ObjectKind.Measure table s.Name
    | ObjectType.Column -> ObjectRef.inTable ObjectKind.Column table s.Name
    | ObjectType.Table -> ObjectRef.table s.Name
    | ObjectType.CalculationGroup -> ObjectRef.table table
    | ObjectType.CalculationItem -> ObjectRef.inTable ObjectKind.CalculationItem table s.Name
    | ObjectType.Function -> ObjectRef.modelLevel ObjectKind.Function s.Name

// --- statements from objects ----------------------------------------------------------------------

let private prop name value = {
    Name = name
    Value = value
    Position = Position.none
}

let private text name (s: string) =
    if String.IsNullOrEmpty s then
        []
    else
        [ prop name (Value.String(normalize s)) ]

let private expression name (s: string) =
    match expressionOf s with
    | "" -> []
    | e -> [ prop name (Value.Expression e) ]

let private hidden (isHidden: bool) =
    if isHidden then
        [ prop "Visible" (Value.Bool false) ]
    else
        []

let private isStringLiteral (dax: string) =
    match Dax.significant (Dax.tokenize dax) with
    | [ t ] -> t.Kind = Dax.TokenKind.String
    | _ -> false

let private statement t table name expr props = {
    Type = t
    Table = table
    Name = name
    Expression = expr
    Properties = props
    Position = Position.none
}

let private measure (m: Measure) =
    let definition =
        if isNull m.FormatStringDefinition then
            ""
        else
            expressionOf m.FormatStringDefinition.Expression

    // A dynamic format string is `FormatString = <DAX>`, unless that would read as a static one.
    let dynamic =
        if definition = "" then
            []
        elif not (String.IsNullOrEmpty m.FormatString) || isStringLiteral definition then
            [ prop "FormatStringDefinition" (Value.Expression definition) ]
        else
            [ prop "FormatString" (Value.Expression definition) ]

    let kpi =
        match m.KPI with
        | null -> []
        | k ->
            expression "KpiStatusExpression" k.StatusExpression
            @ text "KpiStatusDescription" k.StatusDescription
            @ text "KpiStatusGraphic" k.StatusGraphic
            @ expression "KpiTrendExpression" k.TrendExpression
            @ text "KpiTrendDescription" k.TrendDescription
            @ text "KpiTrendGraphic" k.TrendGraphic
            @ expression "KpiTargetExpression" k.TargetExpression
            @ text "KpiTargetDescription" k.TargetDescription
            @ text "KpiTargetFormatString" k.TargetFormatString

    statement
        ObjectType.Measure
        (Some m.Table.Name)
        m.Name
        (Some(expressionOf m.Expression))
        (text "Description" m.Description
         @ text "FormatString" m.FormatString
         @ dynamic
         @ text "DisplayFolder" m.DisplayFolder
         @ hidden m.IsHidden
         @ (match m.DetailRowsDefinition with
            | null -> []
            | d -> expression "DetailRows" d.Expression)
         @ kpi)

let private column (c: CalculatedColumn) =
    let dataType =
        dataTypeNames
        |> List.tryFind (fun (_, t) -> t = c.DataType)
        |> Option.map (fun (name, _) -> prop "Datatype" (Value.DataType name))
        |> Option.toList

    statement
        ObjectType.Column
        (Some c.Table.Name)
        c.Name
        (Some(expressionOf c.Expression))
        (text "Description" c.Description
         @ text "FormatString" c.FormatString
         @ text "DisplayFolder" c.DisplayFolder
         @ hidden c.IsHidden
         @ dataType)

let private detailRows (t: Table) =
    match t.DefaultDetailRowsDefinition with
    | null -> []
    | d -> expression "DetailRows" d.Expression

let private table (t: Table) (source: CalculatedPartitionSource) =
    statement
        ObjectType.Table
        None
        t.Name
        (Some(expressionOf source.Expression))
        (text "Description" t.Description @ hidden t.IsHidden @ detailRows t)

let private group (t: Table) =
    let column =
        groupColumn t |> Option.map (fun c -> c.Name) |> Option.defaultValue "Name"

    statement
        ObjectType.CalculationGroup
        (Some t.Name)
        column
        None
        (text "Description" t.Description
         @ hidden t.IsHidden
         @ [ prop "Precedence" (Value.Int t.CalculationGroup.Precedence) ])

/// Calculation item ordinals exist from compatibility level 1500.
let private ordinalLevel = 1500

let private item (level: int) (t: Table) (i: CalculationItem) =
    statement
        ObjectType.CalculationItem
        (Some t.Name)
        i.Name
        (Some(expressionOf i.Expression))
        (text "Description" i.Description
         @ (match i.FormatStringDefinition with
            | null -> []
            | d -> expression "FormatString" d.Expression)
         @ (if level >= ordinalLevel then
                [ prop "Ordinal" (Value.Int i.Ordinal) ]
            else
                []))

let private func (f: Function) =
    statement
        ObjectType.Function
        None
        f.Name
        (Some(expressionOf f.Expression))
        (text "Description" f.Description @ hidden f.IsHidden)

// --- selection ------------------------------------------------------------------------------------

/// What to export. Empty lists mean no restriction: everything a script can hold.
type Selection = {
    /// Objects in any form `get` accepts. A table stands for its DAX objects: itself (if calculated or
    /// a calculation group, with its items), its calculated columns and its measures.
    Selectors: string list
    /// Only these kinds: measure, column (calculated), table (calculated tables and calculation
    /// groups), calcitem, function.
    Kinds: ObjectKind list
    /// Only objects in these tables.
    Tables: string list
}

module Selection =
    let all = {
        Selectors = []
        Kinds = []
        Tables = []
    }

/// The kinds a script holds.
let scriptableKinds = [
    ObjectKind.Measure
    ObjectKind.Column
    ObjectKind.Table
    ObjectKind.CalculationItem
    ObjectKind.Function
]

let private noDax (e: ObjectEntry) reason =
    Error(CannotScript(ObjectRef.format e.Ref, reason))

/// The objects a selector stands for.
let private expand (e: ObjectEntry) : Result<MetadataObject list, TessError> =
    match e.Object with
    | :? Measure
    | :? CalculatedColumn
    | :? CalculationItem
    | :? Function -> Ok [ e.Object ]
    | :? Column -> noDax e "it's a data column; only calculated columns have a DAX expression"
    | :? Table as t ->
        let own: MetadataObject list =
            if not (isNull t.CalculationGroup) then
                (t :> MetadataObject)
                :: [ for i in t.CalculationGroup.CalculationItems -> i :> MetadataObject ]
            elif (calculatedSource t).IsSome then
                [ t ]
            else
                []

        match
            own
            @ [
                for c in t.Columns do
                    match c with
                    | :? CalculatedColumn -> c :> MetadataObject
                    | _ -> ()
            ]
            @ [ for m in t.Measures -> m :> MetadataObject ]
        with
        | [] -> noDax e "it has no DAX: no measures, no calculated columns, and it isn't a calculated table"
        | objects -> Ok objects
    | _ ->
        noDax
            e
            ($"{ObjectKind.token e.Ref.Kind} objects have no DAX expression of their own; DAX scripts hold "
             + "measures, calculated columns, calculated tables, calculation groups and items, and functions")

/// The statements for the selected objects: tables in model order (each table's own DAX, or its
/// calculation group with its items by ordinal, then its calculated columns, then its measures),
/// then functions.
let statements (db: Database) (sel: Selection) : Result<Statement list, TessError> =
    let entries = objects db

    let chosen =
        sel.Selectors
        |> List.fold
            (fun acc selector ->
                acc
                |> Result.bind (fun (set: HashSet<MetadataObject>) ->
                    Selector.resolve entries selector
                    |> Result.bind expand
                    |> Result.map (fun objects ->
                        objects |> List.iter (set.Add >> ignore)
                        set)))
            (Ok(HashSet<MetadataObject>(HashIdentity.Reference)))

    chosen
    |> Result.map (fun chosen ->
        let level = db.CompatibilityLevel

        let wanted (kind: ObjectKind) (tableName: string option) (o: MetadataObject) =
            (sel.Selectors.IsEmpty || chosen.Contains o)
            && (sel.Kinds.IsEmpty || List.contains kind sel.Kinds)
            && (sel.Tables.IsEmpty
                || tableName |> Option.exists (fun t -> sel.Tables |> List.exists (eqCi t)))

        [
            for t in db.Model.Tables do
                let name = Some t.Name

                if not (isNull t.CalculationGroup) then
                    let items =
                        t.CalculationGroup.CalculationItems
                        |> Seq.filter (wanted ObjectKind.CalculationItem name)
                        |> Seq.sortBy (fun i -> i.Ordinal, i.Name)
                        |> List.ofSeq

                    // Items need their group's header, even when only they were chosen.
                    if wanted ObjectKind.Table name t || not items.IsEmpty then
                        group t

                        for i in items do
                            item level t i
                else
                    match calculatedSource t with
                    | Some source when wanted ObjectKind.Table name t -> table t source
                    | _ -> ()

                for c in t.Columns do
                    match c with
                    | :? CalculatedColumn as c when wanted ObjectKind.Column name c -> column c
                    | _ -> ()

                for m in t.Measures do
                    if wanted ObjectKind.Measure name m then
                        measure m

            for f in db.Model.Functions do
                if wanted ObjectKind.Function None f then
                    func f
        ])

type Exported = {
    Text: string
    Statements: Statement list
    /// Objects whose DAX would not read back from the script exactly.
    Warnings: string list
}

let private shape (s: Statement) =
    s.Type, s.Table, s.Name, s.Expression, s.Properties |> List.map (fun p -> p.Name, p.Value)

/// The selected objects as a script, checked by parsing it back.
let export (db: Database) (sel: Selection) : Result<Exported, TessError> =
    statements db sel
    |> Result.map (fun stmts ->
        let text = print stmts

        let warnings =
            let problem (s: Statement) =
                $"{ObjectRef.format (refOf s)}: its DAX would not read back from the script exactly (it has "
                + "a line that reads like a script property or keyword, a comma outside parentheses, or an "
                + "open string or comment); check it before applying the script"

            let readsBack (context: Statement list) =
                match parse (print context) with
                | Ok parsed when parsed.Length = context.Length -> shape (List.last parsed) = shape (List.last context)
                | _ -> false

            let whole =
                match parse text with
                | Ok parsed -> List.map shape parsed = List.map shape stmts
                | Error _ -> false

            if whole then
                []
            else
                // Find the culprits one by one (an item with its group's header).
                let mutable header = None

                let culprits = [
                    for s in stmts do
                        let context =
                            match s.Type, header with
                            | ObjectType.CalculationItem, Some h -> [ h; s ]
                            | _ -> [ s ]

                        if s.Type = ObjectType.CalculationGroup then
                            header <- Some s

                        if not (readsBack context) then
                            problem s
                ]

                match culprits with
                | [] -> [ "the script doesn't read back as a whole; check it before applying it" ]
                | xs -> xs

        {
            Text = text
            Statements = stmts
            Warnings = warnings
        })
