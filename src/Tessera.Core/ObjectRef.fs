namespace Tessera

open System
open System.Text

/// The kinds of model object tessera can address.
/// Qualified access is required: most case names collide with TOM types (Table, Column, ...).
[<RequireQualifiedAccess>]
type ObjectKind =
    | Model
    | Table
    | Column
    | Measure
    | Hierarchy
    | Level
    | Partition
    | CalculationItem
    | Calendar
    | Relationship
    | Role
    | Perspective
    | Culture
    | Expression
    | Function
    | DataSource

module ObjectKind =

    let all = [
        ObjectKind.Model
        ObjectKind.Table
        ObjectKind.Column
        ObjectKind.Measure
        ObjectKind.Hierarchy
        ObjectKind.Level
        ObjectKind.Partition
        ObjectKind.CalculationItem
        ObjectKind.Calendar
        ObjectKind.Relationship
        ObjectKind.Role
        ObjectKind.Perspective
        ObjectKind.Culture
        ObjectKind.Expression
        ObjectKind.Function
        ObjectKind.DataSource
    ]

    /// Canonical lowercase token used in references and output.
    let token =
        function
        | ObjectKind.Model -> "model"
        | ObjectKind.Table -> "table"
        | ObjectKind.Column -> "column"
        | ObjectKind.Measure -> "measure"
        | ObjectKind.Hierarchy -> "hierarchy"
        | ObjectKind.Level -> "level"
        | ObjectKind.Partition -> "partition"
        | ObjectKind.CalculationItem -> "calcitem"
        | ObjectKind.Calendar -> "calendar"
        | ObjectKind.Relationship -> "relationship"
        | ObjectKind.Role -> "role"
        | ObjectKind.Perspective -> "perspective"
        | ObjectKind.Culture -> "culture"
        | ObjectKind.Expression -> "expression"
        | ObjectKind.Function -> "function"
        | ObjectKind.DataSource -> "datasource"

    let private aliases = [
        "calculationitem", ObjectKind.CalculationItem
        "udf", ObjectKind.Function
        "rel", ObjectKind.Relationship
        "expr", ObjectKind.Expression
    ]

    let tryParse (s: string) =
        let s = s.Trim().ToLowerInvariant()

        all
        |> List.tryFind (fun k -> token k = s)
        |> Option.orElse (aliases |> List.tryFind (fun (a, _) -> a = s) |> Option.map snd)

    /// How many name segments a reference of this kind carries.
    /// e.g. a measure is addressed by table and measure name: measure:Sales/Total
    let arity =
        function
        | ObjectKind.Model -> 0
        | ObjectKind.Table
        | ObjectKind.Relationship
        | ObjectKind.Role
        | ObjectKind.Perspective
        | ObjectKind.Culture
        | ObjectKind.Expression
        | ObjectKind.Function
        | ObjectKind.DataSource -> 1
        | ObjectKind.Column
        | ObjectKind.Measure
        | ObjectKind.Hierarchy
        | ObjectKind.Partition
        | ObjectKind.CalculationItem
        | ObjectKind.Calendar -> 2
        | ObjectKind.Level -> 3

/// A stable address for one object in a model, e.g. <c>measure:Sales/Total Sales</c>.
/// Format: <c>kind:segment/segment</c>. Inside a segment, '/' and '\' are escaped with '\'.
/// The model itself is just <c>model</c>.
type ObjectRef = { Kind: ObjectKind; Path: string list }

module ObjectRef =

    let private escape (s: string) =
        s.Replace("\\", "\\\\").Replace("/", "\\/")

    let format (r: ObjectRef) =
        match r.Kind, r.Path with
        | ObjectKind.Model, _ -> "model"
        | kind, path -> ObjectKind.token kind + ":" + (path |> List.map escape |> String.concat "/")

    /// Split on unescaped '/', unescaping as we go.
    let private splitSegments (s: string) : Result<string list, string> =
        let segments = ResizeArray<string>()
        let current = StringBuilder()
        let mutable error = None
        let mutable i = 0

        while error.IsNone && i < s.Length do
            match s.[i] with
            | '\\' when i + 1 < s.Length && (s.[i + 1] = '\\' || s.[i + 1] = '/') ->
                current.Append(s.[i + 1]) |> ignore
                i <- i + 2
            | '\\' ->
                error <- Some "'\\' must be followed by '\\' or '/'"
                i <- s.Length
            | '/' ->
                segments.Add(current.ToString())
                current.Clear() |> ignore
                i <- i + 1
            | c ->
                current.Append(c) |> ignore
                i <- i + 1

        match error with
        | Some e -> Error e
        | None ->
            segments.Add(current.ToString())
            Ok(List.ofSeq segments)

    let parse (input: string) : Result<ObjectRef, TessError> =
        let fail reason = Error(InvalidObjectRef(input, reason))

        if String.IsNullOrWhiteSpace input then
            fail "empty reference"
        elif input.Trim().ToLowerInvariant() = "model" then
            Ok { Kind = ObjectKind.Model; Path = [] }
        else
            match input.IndexOf ':' with
            | -1 -> fail "expected 'kind:name', e.g. 'measure:Sales/Total'"
            | colon ->
                let kindText = input.Substring(0, colon)

                match ObjectKind.tryParse kindText with
                | None ->
                    let known = ObjectKind.all |> List.map ObjectKind.token |> String.concat ", "
                    fail $"unknown kind '{kindText}' (known: {known})"
                | Some kind ->
                    match splitSegments (input.Substring(colon + 1)) with
                    | Error e -> fail e
                    | Ok segments when segments |> List.exists String.IsNullOrEmpty -> fail "empty name segment"
                    | Ok segments when segments.Length <> ObjectKind.arity kind ->
                        let n = ObjectKind.arity kind
                        fail $"'{ObjectKind.token kind}' takes {n} name segment(s), got {segments.Length}"
                    | Ok segments -> Ok { Kind = kind; Path = segments }

    let model = { Kind = ObjectKind.Model; Path = [] }

    let table name = {
        Kind = ObjectKind.Table
        Path = [ name ]
    }

    let inTable kind tableName name = {
        Kind = kind
        Path = [ tableName; name ]
    }

    let modelLevel kind name = { Kind = kind; Path = [ name ] }

    let level tableName hierarchyName levelName = {
        Kind = ObjectKind.Level
        Path = [ tableName; hierarchyName; levelName ]
    }

    /// The object's own name (last segment); "model" for the model.
    let name (r: ObjectRef) =
        match r.Path with
        | [] -> "model"
        | path -> List.last path

    /// The containing table's name, for table-scoped objects.
    let tableName (r: ObjectRef) =
        match r.Kind, r.Path with
        | (ObjectKind.Column | ObjectKind.Measure | ObjectKind.Hierarchy | ObjectKind.Level | ObjectKind.Partition | ObjectKind.CalculationItem | ObjectKind.Calendar),
          t :: _ -> Some t
        | ObjectKind.Table, [ t ] -> Some t
        | _ -> None
