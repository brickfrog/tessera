module Tessera.Cli.GetCommand

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Argu
open Tessera
open Tessera.Inventory
open Tessera.Cli.Output
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary = "show one object (TMDL, or its JSON with --json)."

type Args =
    | [<MainCommand; ExactlyOnce>] Target of model: string * object: string
    | [<AltCommandLine("-p")>] Property of path: string
    | Children
    | Properties
    | All

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "model path, and the object: a ref (measure:Sales/Total), DAX style ('Sales'[Total], [Total]) "
                + "or a bare name."
            | Property _ ->
                "print just this property (repeatable), e.g. expression, source.expression, annotations.PBI_FormatHint."
            | Children -> "include child objects (a table's columns, measures, ...)."
            | Properties -> "list the properties `set` can write: path, type, value, and allowed values (enums)."
            | All -> "with --properties: also list read-only properties."

let getJson (e: ObjectEntry) (objectJson: string) =
    obj (refFields e @ [ "object", JsonNode.Parse objectJson ])

let propertiesJson (e: ObjectEntry) (leaves: (string * string) list) =
    obj (refFields e @ [ "properties", obj [ for p, v in leaves -> p, str v ] ])

let private valueJson (v: SetProperty.PropertyValue) : JsonNode =
    match v with
    | SetProperty.PropertyValue.Null -> null
    | SetProperty.PropertyValue.String s -> str s
    | SetProperty.PropertyValue.Bool b -> bool b
    | SetProperty.PropertyValue.Int n -> JsonValue.Create n
    | SetProperty.PropertyValue.Number n -> JsonValue.Create n
    | SetProperty.PropertyValue.Reference r -> str (ObjectRef.format r)
    | SetProperty.PropertyValue.Present -> JsonObject()

/// The catalog as JSON: [{property, type, writable, value, allowed?, refersTo?, reason?}].
let catalogJson (descriptors: SetProperty.Descriptor list) =
    arr [
        for d in descriptors ->
            obj [
                "property", str d.Path
                "type", str (SetProperty.PropertyType.token d.Type)
                "writable", bool d.Writable
                "value", valueJson d.Value
                if not d.Allowed.IsEmpty then
                    "allowed", arr (d.Allowed |> List.map str)
                match d.RefersTo with
                | Some k -> "refersTo", str (ObjectKind.token k)
                | None -> ()
                match d.Reason with
                | Some r -> "reason", str r
                | None -> ()
            ]
    ]

let private valueText (v: SetProperty.PropertyValue) =
    let text =
        match v with
        | SetProperty.PropertyValue.Null -> ""
        | SetProperty.PropertyValue.String s -> s
        | SetProperty.PropertyValue.Bool b -> if b then "true" else "false"
        | SetProperty.PropertyValue.Int n -> string n
        | SetProperty.PropertyValue.Number n -> n.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | SetProperty.PropertyValue.Reference r -> ObjectRef.format r
        | SetProperty.PropertyValue.Present -> "{...}"

    // One line per property: the first line of a multi-line value, cut short.
    let lines =
        text.Replace("\r\n", "\n").Split('\n')
        |> Array.skipWhile String.IsNullOrWhiteSpace

    let first = if lines.Length = 0 then "" else lines.[0].Trim()

    if first.Length > 50 then first.Substring(0, 50) + "..."
    elif lines.Length > 1 then first + " ..."
    else first

/// The catalog as an aligned table: path, type, [writable,] value, then allowed values, the kind a
/// reference points at, or why it's read-only.
let catalogText (out: TextWriter) (all: bool) (descriptors: SetProperty.Descriptor list) =
    let rows = [
        for d in descriptors ->
            [
                d.Path
                SetProperty.PropertyType.token d.Type
                if all then
                    (if d.Writable then "writable" else "read-only")
                valueText d.Value
                match d.Allowed, d.RefersTo, d.Reason with
                | _, _, Some reason -> $"({reason})"
                | (_ :: _) as allowed, _, _ -> String.concat "|" allowed
                | _, Some k, _ -> $"-> {ObjectKind.token k}"
                | _ -> ""
            ]
    ]

    match rows with
    | [] -> ()
    | first :: _ ->
        let widths =
            [ 0 .. first.Length - 2 ]
            |> List.map (fun i -> rows |> List.map (fun r -> r.[i].Length) |> List.max)

        for r in rows do
            let cells =
                List.mapi (fun i (c: string) -> if i < widths.Length then c.PadRight widths.[i] else c) r

            out.WriteLine((String.concat "  " cells).TrimEnd())

let private runProperties (ctx: Ctx) (modelPath: string) (selector: string) (all: bool) =
    let result =
        Source.load ctx modelPath
        |> Result.bind (fun (_, db) ->
            Inventory.Selector.resolve (Inventory.objects db) selector
            |> Result.map (fun e -> db, e))

    match result with
    | Error e -> Failed e
    | Ok(db, e) ->
        let descriptors =
            SetProperty.catalog db e |> List.filter (fun d -> all || d.Writable)

        if ctx.Json then
            writeJson ctx.Out (catalogJson descriptors)
        else
            catalogText ctx.Out all descriptors

        Done

let private show (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath, selector = args.GetResult Args.Target
    let children = args.Contains Args.Children
    let wanted = args.GetResults Args.Property

    let result =
        Source.load ctx modelPath
        |> Result.bind (fun (_, db) ->
            Inventory.Selector.resolve (Inventory.objects db) selector
            |> Result.map (fun e -> db, e))

    match result with
    | Error e -> Failed e
    | Ok(db, e) ->
        let json = Serialize.objectJson db children e.Object

        match wanted with
        | [] ->
            if ctx.Json then
                writeJson ctx.Out (getJson e json)
            else
                ctx.Out.Write(Serialize.objectTmdl children e.Object)

            Done
        | paths ->
            let props =
                if not children then
                    // The same paths `diff` and `find` use, translations and perspective members included.
                    Serialize.properties db e
                else
                    let own = Serialize.flatten json

                    if e.Ref.Kind = ObjectKind.Model then
                        ("compatibilityLevel", string db.CompatibilityLevel) :: own
                    else
                        own

            let selected = paths |> List.map (fun p -> p, Serialize.select p props)

            match selected |> List.tryFind (snd >> List.isEmpty) with
            | Some(missing, _) -> Failed(PropertyNotFound(ObjectRef.format e.Ref, missing))
            | None ->
                let leaves = selected |> List.collect snd

                if ctx.Json then
                    writeJson ctx.Out (propertiesJson e leaves)
                else
                    match paths, leaves with
                    // One exact property: print the bare value, for scripting.
                    | [ p ], [ (leaf, value) ] when leaf = p -> ctx.Out.WriteLine(value)
                    | _ ->
                        for leaf, value in leaves do
                            ctx.Out.WriteLine($"{leaf}: {value}")

                Done

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath, selector = args.GetResult Args.Target

    if args.Contains Args.Properties then
        if args.Contains Args.Property || args.Contains Args.Children then
            UsageError "--properties can't be combined with --property or --children"
        else
            runProperties ctx modelPath selector (args.Contains Args.All)
    elif args.Contains Args.All then
        UsageError "--all only goes with --properties"
    else
        show ctx args
