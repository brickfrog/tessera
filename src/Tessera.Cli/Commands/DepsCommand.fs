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
    | [<MainCommand; ExactlyOnce>] Target of model_and_object: string list
    | [<AltCommandLine("-d")>] Depth of levels: int
    | Depends_On
    | Referenced_By
    | Unused
    | Include_Visible
    | [<AltCommandLine("-k")>] Kind of kinds: string
    | [<AltCommandLine("-t")>] Table of table: string
    | Fail_On_Unused

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Target _ ->
                "model path, and the object (a ref, 'Table'[Name], [Name] or a bare name); just the model with --unused."
            | Depth _ -> "how many levels to follow (default 1; 0 = all the way)."
            | Depends_On -> "only what the object depends on."
            | Referenced_By -> "only what refers to the object."
            | Unused -> "list hidden tables, columns, measures, hierarchies and functions nothing in the model uses."
            | Include_Visible -> "with --unused: check visible objects too (reports may still use them)."
            | Kind _ -> "with --unused: only these kinds (repeatable or comma-separated), e.g. measure,column."
            | Table _ -> "with --unused: only objects in this table (repeatable)."
            | Fail_On_Unused -> "with --unused: exit 1 if anything is unused."

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

let private runDeps (ctx: Ctx) (args: ParseResults<Args>) (modelPath: string) (selector: string) =
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

// ---- --unused ---------------------------------------------------------------------------------

let unusedJson (unused: Unused.UnusedObject list) =
    arr [
        for u in unused ->
            let e = u.Entry

            obj [
                "ref", str (ObjectRef.format e.Ref)
                "kind", str (ObjectKind.token e.Ref.Kind)
                "name", str (ObjectEntry.name e)
                "table", optStr (ObjectEntry.table e |> Option.filter (fun _ -> e.Ref.Kind <> ObjectKind.Table))
                "hidden", bool u.Hidden
                "usedBy", arr [ for r in u.UsedBy -> str (ObjectRef.format r) ]
                "note", optStr u.Note
            ]
    ]

/// One reference per line, like `list`; then (visible), who uses it, and notes, after two spaces.
let unusedText (out: TextWriter) (unused: Unused.UnusedObject list) =
    for u in unused do
        let extras = [
            if not u.Hidden then
                "(visible)"
            if not u.UsedBy.IsEmpty then
                "used only by unused "
                + (u.UsedBy |> List.map ObjectRef.format |> String.concat ", ")
            yield! Option.toList u.Note
        ]

        match extras with
        | [] -> out.WriteLine(ObjectRef.format u.Entry.Ref)
        | xs -> out.WriteLine(ObjectRef.format u.Entry.Ref + "  " + String.concat "  " xs)

let private runUnused (ctx: Ctx) (args: ParseResults<Args>) (modelPath: string) =
    let kinds =
        parseKinds (args.GetResults Args.Kind)
        |> Result.bind (fun kinds ->
            match kinds |> List.tryFind (fun k -> not (List.contains k Unused.kinds)) with
            | Some k ->
                let known = Unused.kinds |> List.map ObjectKind.token |> String.concat ", "
                Error $"--unused reports {known}, not {ObjectKind.token k}"
            | None -> Ok kinds)

    let conflicting = [
        if args.Contains Args.Depth then
            "--depth"
        if args.Contains Args.Depends_On then
            "--depends-on"
        if args.Contains Args.Referenced_By then
            "--referenced-by"
    ]

    match kinds, conflicting with
    | Error message, _ -> UsageError message
    | _, flag :: _ -> UsageError $"{flag} can't be combined with --unused"
    | Ok kinds, [] ->
        match ModelIO.loadPath modelPath with
        | Error e -> Failed e
        | Ok(_, db) ->
            let options: Unused.Options = {
                IncludeVisible = args.Contains Args.Include_Visible
            }

            let filter: Inventory.Filter = {
                Inventory.Filter.none with
                    Kinds = kinds
                    Tables = args.GetResults Args.Table
            }

            let unused = Deps.build db |> Unused.find options

            let shown =
                Inventory.Filter.apply filter (unused |> List.map (fun u -> u.Entry))
                |> List.map (fun e -> e.Ref)
                |> Set.ofList

            let unused = unused |> List.filter (fun u -> shown.Contains u.Entry.Ref)

            if ctx.Json then
                writeJson ctx.Out (unusedJson unused)
            else
                unusedText ctx.Out unused

            if args.Contains Args.Fail_On_Unused && not unused.IsEmpty then
                DoneWith ExitCode.findings
            else
                Done

let run (ctx: Ctx) (args: ParseResults<Args>) =
    let unusedOnly = [
        if args.Contains Args.Include_Visible then
            "--include-visible"
        if args.Contains Args.Kind then
            "--kind"
        if args.Contains Args.Table then
            "--table"
        if args.Contains Args.Fail_On_Unused then
            "--fail-on-unused"
    ]

    match args.Contains Args.Unused, args.GetResult Args.Target, unusedOnly with
    | true, [ model ], _ -> runUnused ctx args model
    | true, _, _ -> UsageError "--unused takes just the model path"
    | false, _, flag :: _ -> UsageError $"{flag} needs --unused"
    | false, [ model; selector ], [] -> runDeps ctx args model selector
    | false, _, [] -> UsageError "expected the model path and an object"
