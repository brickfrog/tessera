module Tessera.Cli.DepsCommand

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
let summary = "what an object depends on, and what refers to it."

type Args =
    | [<MainCommand; ExactlyOnce>] Target of model: string * object: string
    | [<AltCommandLine("-d")>] Depth of levels: int
    | Depends_On
    | Referenced_By

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ -> "model path, and the object (a ref, 'Table'[Name], [Name] or a bare name)."
            | Depth _ -> "how many levels to follow (default 1; 0 = all the way)."
            | Depends_On -> "only what the object depends on."
            | Referenced_By -> "only what refers to the object."

let private targetText =
    function
    | Deps.Resolved r -> ObjectRef.format r
    | Deps.Unresolved text -> $"? {text}"

/// expression:2,5  sortByColumn
let private viaText (via: (string * int option) list) =
    via
    |> List.groupBy fst
    |> List.map (fun (property, xs) ->
        match xs |> List.choose snd |> List.distinct with
        | [] -> property
        | lines -> property + ":" + (lines |> List.map string |> String.concat ","))
    |> String.concat "  "

let rec private nodeJson (n: Deps.Node) : JsonNode =
    obj [
        match n.Target with
        | Deps.Resolved r -> yield "ref", str (ObjectRef.format r)
        | Deps.Unresolved text -> yield "unresolved", str text

        yield
            "via",
            arr [
                for property, line in n.Via ->
                    obj [
                        "property", str property
                        "line",
                        (match line with
                         | Some l -> num l
                         | None -> null)
                    ]
            ]
        if n.Cycle then
            yield "cycle", bool true

        if not n.Children.IsEmpty then
            yield "children", arr (n.Children |> List.map nodeJson)
    ]

let private directionKey =
    function
    | Deps.DependsOn -> "dependsOn"
    | Deps.ReferencedBy -> "referencedBy"

let depsJson (root: ObjectRef) (trees: (Deps.Direction * Deps.Node list) list) =
    obj [
        yield "ref", str (ObjectRef.format root)

        for direction, nodes in trees do
            yield directionKey direction, arr (nodes |> List.map nodeJson)
    ]

let depsText (out: TextWriter) (root: ObjectRef) (trees: (Deps.Direction * Deps.Node list) list) =
    out.WriteLine(ObjectRef.format root)

    let rec write (indent: string) (nodes: Deps.Node list) =
        let width =
            nodes |> List.map (fun n -> (targetText n.Target).Length) |> List.fold max 0

        for n in nodes do
            let label = targetText n.Target
            let cycle = if n.Cycle then "  (circular)" else ""
            out.WriteLine($"{indent}{label.PadRight width}  {viaText n.Via}{cycle}")
            write (indent + "    ") n.Children

    for direction, nodes in trees do
        let heading =
            match direction with
            | Deps.DependsOn -> "depends on"
            | Deps.ReferencedBy -> "referenced by"

        out.WriteLine($"  {heading}")

        if nodes.IsEmpty then
            out.WriteLine("    (nothing)")
        else
            write "    " nodes

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let modelPath, selector = args.GetResult Args.Target
    let depth = args.GetResult(Args.Depth, defaultValue = 1)

    let directions =
        match args.Contains Args.Depends_On, args.Contains Args.Referenced_By with
        | true, false -> [ Deps.DependsOn ]
        | false, true -> [ Deps.ReferencedBy ]
        | _ -> [ Deps.DependsOn; Deps.ReferencedBy ]

    if depth < 0 then
        UsageError "--depth must be 0 (unlimited) or more"
    else
        let result =
            ModelIO.loadPath modelPath
            |> Result.bind (fun (_, db) ->
                let g = Deps.build db
                Inventory.Selector.resolve g.Entries selector |> Result.map (fun e -> g, e))

        match result with
        | Error e -> Failed e
        | Ok(g, e) ->
            let trees = directions |> List.map (fun d -> d, Deps.tree d depth g e.Ref)

            if ctx.Json then
                writeJson ctx.Out (depsJson e.Ref trees)
            else
                depsText ctx.Out e.Ref trees

            Done
