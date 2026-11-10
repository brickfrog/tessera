/// The offline guarantee, checked on the compiled assemblies: which code can reach a network type
/// at all. Tessera.Core never can (except the explicit BPA rule download); the CLI only through
/// Tessera.Xmla; and in Tessera.Xmla only the live connector.
module Tessera.Tests.OfflineTests

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Expecto
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness

/// A use of an outside type: the method doing it, and the type (and member) used.
type private Usage = { In: string; Uses: string }

/// IL opcodes by their value, for walking a method body one instruction at a time.
let private opCodes =
    let codes = Collections.Generic.Dictionary<int, Reflection.Emit.OpCode>()

    for f in
        typeof<Reflection.Emit.OpCodes>.GetFields(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static) do
        match f.GetValue null with
        | :? Reflection.Emit.OpCode as code -> codes.[int (uint16 code.Value)] <- code
        | _ -> ()

    codes

/// The metadata tokens a method body's instructions name (the operands of call, newobj, ldfld,
/// ldtoken, castclass...). None when the body can't be decoded.
let private tokensOf (il: byte array) : int list option =
    let found = Collections.Generic.List<int>()
    let mutable at = 0
    let mutable ok = true

    while ok && at < il.Length do
        let value, size =
            if il.[at] = 0xFEuy && at + 1 < il.Length then
                0xFE00 ||| int il.[at + 1], 2
            else
                int il.[at], 1

        match opCodes.TryGetValue value with
        | true, code ->
            at <- at + size

            let operand =
                match code.OperandType with
                | Reflection.Emit.OperandType.InlineNone -> 0
                | Reflection.Emit.OperandType.ShortInlineBrTarget
                | Reflection.Emit.OperandType.ShortInlineI
                | Reflection.Emit.OperandType.ShortInlineVar -> 1
                | Reflection.Emit.OperandType.InlineVar -> 2
                | Reflection.Emit.OperandType.InlineI8
                | Reflection.Emit.OperandType.InlineR -> 8
                | Reflection.Emit.OperandType.InlineSwitch ->
                    if at + 4 <= il.Length then
                        4 + 4 * BitConverter.ToInt32(il, at)
                    else
                        Int32.MaxValue
                | Reflection.Emit.OperandType.InlineField
                | Reflection.Emit.OperandType.InlineMethod
                | Reflection.Emit.OperandType.InlineSig
                | Reflection.Emit.OperandType.InlineTok
                | Reflection.Emit.OperandType.InlineType ->
                    if at + 4 <= il.Length then
                        found.Add(BitConverter.ToInt32(il, at))

                    4
                | _ -> 4

            if operand < 0 || operand > il.Length - at then
                ok <- false
            else
                at <- at + operand
        | _ -> ok <- false

    if ok then Some(List.ofSeq found) else None

/// Every method in an assembly whose IL mentions a referenced type (or a member of one) that
/// `watched` picks out by namespace and name. A method body is read instruction by instruction
/// for the tokens it names. One that can't be read that way is scanned for a token's four bytes
/// anywhere, which can over-report and never under-report.
let private usages (assembly: Reflection.Assembly) (watched: string -> string -> bool) : Usage list =
    use stream = File.OpenRead assembly.Location
    use pe = new PEReader(stream)
    let md = pe.GetMetadataReader()
    let name (h: StringHandle) = md.GetString h

    let rec typeRefName (h: TypeReferenceHandle) =
        let t = md.GetTypeReference h

        // A nested type's namespace is its outermost type's.
        if t.ResolutionScope.Kind = HandleKind.TypeReference then
            let ns, outer = typeRefName (TypeReferenceHandle.op_Explicit t.ResolutionScope)
            ns, outer + "+" + name t.Name
        else
            name t.Namespace, name t.Name

    let watchedTypes =
        md.TypeReferences
        |> Seq.choose (fun h ->
            let ns, n = typeRefName h
            if watched ns n then Some(h, ns + "." + n) else None)
        |> List.ofSeq

    let typeTokens =
        watchedTypes
        |> List.map (fun (h, n) -> MetadataTokens.GetToken(TypeReferenceHandle.op_Implicit h: EntityHandle), n)

    let memberTokens =
        md.MemberReferences
        |> Seq.choose (fun h ->
            let m = md.GetMemberReference h

            let parent =
                match m.Parent.Kind with
                | HandleKind.TypeReference -> Some(TypeReferenceHandle.op_Explicit m.Parent)
                | HandleKind.TypeSpecification -> None
                | _ -> None

            parent
            |> Option.bind (fun p -> watchedTypes |> List.tryFind (fun (t, _) -> t = p))
            |> Option.map (fun (_, typeName) ->
                MetadataTokens.GetToken(MemberReferenceHandle.op_Implicit h: EntityHandle),
                typeName + "::" + name m.Name))
        |> List.ofSeq

    let tokens =
        typeTokens @ memberTokens
        |> List.map (fun (token, what) -> BitConverter.GetBytes token, what)

    let rec typeDefName (h: TypeDefinitionHandle) =
        let t = md.GetTypeDefinition h
        let declaring = t.GetDeclaringType()

        if declaring.IsNil then
            let ns = name t.Namespace
            if ns = "" then name t.Name else ns + "." + name t.Name
        else
            typeDefName declaring + "+" + name t.Name

    [
        for typeHandle in md.TypeDefinitions do
            let t = md.GetTypeDefinition typeHandle

            for methodHandle in t.GetMethods() do
                let m = md.GetMethodDefinition methodHandle

                if m.RelativeVirtualAddress <> 0 then
                    let il = pe.GetMethodBody(m.RelativeVirtualAddress).GetILBytes()

                    let named = tokensOf il

                    for bytes, what in tokens do
                        let mentioned =
                            match named with
                            | Some list -> List.contains (BitConverter.ToInt32(bytes, 0)) list
                            | None -> il.AsSpan().IndexOf(ReadOnlySpan bytes) >= 0

                        if mentioned then
                            {
                                In = typeDefName typeHandle + "::" + name m.Name
                                Uses = what
                            }
    ]

let private referencedAssemblies (assembly: Reflection.Assembly) =
    assembly.GetReferencedAssemblies() |> Array.map _.Name |> List.ofArray

let private core = typeof<Tessera.TessError>.Assembly
let private xmla = typeof<Tessera.Xmla.LiveConnector>.Assembly
let private cli = typeof<Tessera.Cli.Common.Ctx>.Assembly

/// Types that open connections: sockets, HTTP, DNS, and the Analysis Services clients' servers
/// and connections, and the sign-in library.
let private network (ns: string) (name: string) =
    ns.StartsWith "System.Net"
    || ns.StartsWith "Microsoft.AnalysisServices.AdomdClient"
    || ns.StartsWith "Microsoft.Identity"
    || (ns.StartsWith "Microsoft.AnalysisServices"
        && (name = "Server" || name = "AccessToken"))

let private describe (found: Usage list) =
    found
    |> List.map (fun u -> $"{u.In} uses {u.Uses}")
    |> List.distinct
    |> String.concat "\n"

[<Tests>]
let tests =
    testList "Offline" [
        testCase "the scan itself finds what it should"
        <| fun () ->
            // Known uses: the live connector's server, and Core's rule download.
            Expect.exists
                (usages xmla network)
                (fun u -> u.Uses = "Microsoft.AnalysisServices.Tabular.Server::.ctor")
                "sees AMO's Server in Tessera.Xmla"

            Expect.exists
                (usages core network)
                (fun u -> u.Uses.StartsWith "System.Net.Http.HttpClient")
                "sees HttpClient in Tessera.Core"

        testCase "Tessera.Core references no connection library and no Tessera.Xmla"
        <| fun () ->
            let references = referencedAssemblies core

            for forbidden in
                [
                    "Tessera.Xmla"
                    "Microsoft.AnalysisServices.AdomdClient"
                    "Microsoft.Identity.Client"
                ] do
                Expect.isFalse (List.contains forbidden references) forbidden

        testCase "in Tessera.Core only the BPA rule download (an explicit --rules URL) touches a network type"
        <| fun () ->
            let elsewhere =
                usages core network |> List.filter (fun u -> not (u.In.Contains "BpaFetch"))

            Expect.isEmpty elsewhere (describe elsewhere)

        testCase "in Tessera.Xmla only the live connector touches a network type"
        <| fun () ->
            let elsewhere =
                usages xmla network |> List.filter (fun u -> not (u.In.Contains "Live"))

            Expect.isEmpty elsewhere (describe elsewhere)

        testCase "the CLI touches no network type itself, and creates the live connector in one place"
        <| fun () ->
            let direct = usages cli network
            Expect.isEmpty direct (describe direct)

            for forbidden in [ "Microsoft.AnalysisServices.AdomdClient"; "Microsoft.Identity.Client" ] do
                Expect.isFalse (List.contains forbidden (referencedAssemblies cli)) forbidden

            let live =
                usages cli (fun ns name -> ns = "Tessera.Xmla" && name = "LiveConnector")
                |> List.map _.In
                |> List.distinct

            // (`run` too when the compiler inlines `runWith` into it.)
            Expect.isNonEmpty live ""

            Expect.all
                live
                (fun m -> m = "Tessera.Cli.App::runWith" || m = "Tessera.Cli.App::run")
                "the entry point is the only place a real connection can come from"

        testCase "no command given model files opens a connection (the harness's connector counts attempts)"
        <| fun () ->
            use p = project ()
            let model = Path.Combine(p.Path, "Sales.SemanticModel")
            let other = Path.Combine(p.Path, "copy.bim")

            // `tess` fails the test if the command so much as asks for a connection.
            for args in
                [
                    [ "info"; model ]
                    [ "list"; model ]
                    [ "get"; model; "[Total]" ]
                    [ "find"; model; "Total" ]
                    [ "deps"; model; "[Total]" ]
                    [ "validate"; model ]
                    [ "bpa"; model ]
                    [ "lint"; model ]
                    [ "script"; "export"; model ]
                    [ "convert"; model; other ]
                    [ "diff"; model; other ]
                    [ "rename"; model; "[Total]"; "Total 2"; "--save" ]
                    [ "format"; model ]
                    [ "query"; model; "--coverage" ]
                    [ "query"; model; "--data"; p.Path; "EVALUATE { 1 }" ]
                    [ "--version" ]
                ] do
                tess args |> ignore
    ]
