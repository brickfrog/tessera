/// Optional checks against a real model on disk. Set TESSERA_SAMPLE_MODEL to any path tessera
/// accepts (e.g. a clone of TabularEditor/spaceparts-analytics); skipped otherwise.
module Tessera.Tests.SampleModelTests

open System
open System.IO
open Expecto
open Tessera
open Tessera.ModelIO
open Tessera.Tests.Fixtures

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

[<Tests>]
let tests =
    testList "Sample model (TESSERA_SAMPLE_MODEL)" [
        match samplePath with
        | None ->
            testCase "not configured"
            <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
        | Some path ->
            testCase "loads, lists, and round-trips through .bim and TMDL"
            <| fun () ->
                use t = new TempDir()
                let _, db = loadPath path |> ok
                let entries = Inventory.objects db
                Expect.isNonEmpty entries ""

                for e in entries do
                    Expect.equal (ObjectRef.parse (ObjectRef.format e.Ref)) (Ok e.Ref) (ObjectRef.format e.Ref)

                save BimFile SaveOptions.defaults false db (t.Combine "a.bim") |> ok |> ignore
                let _, fromBim = loadPath (t.Combine "a.bim") |> ok

                save TmdlFolder SaveOptions.defaults false fromBim (t.Combine "tmdl")
                |> ok
                |> ignore

                let _, fromTmdl = loadPath (t.Combine "tmdl") |> ok

                save BimFile SaveOptions.defaults false fromTmdl (t.Combine "b.bim")
                |> ok
                |> ignore

                Expect.equal (File.ReadAllText(t.Combine "b.bim")) (File.ReadAllText(t.Combine "a.bim")) ""

            testCase "renaming its most-used measure and table keeps every reference resolved"
            <| fun () ->
                use t = new TempDir()
                let _, db = loadPath path |> ok
                let g = Deps.build db
                let unresolved = Deps.unresolved g |> List.length

                let mostUsed kind =
                    g.References
                    |> List.choose (fun r ->
                        match r.Target with
                        | Deps.Resolved x when x.Kind = kind && r.Start.IsSome -> Some x
                        | _ -> None)
                    |> List.countBy id
                    |> List.sortByDescending snd
                    |> List.tryHead
                    |> Option.map fst

                for kind in [ ObjectKind.Measure; ObjectKind.Table ] do
                    match mostUsed kind with
                    | None -> ()
                    | Some r ->
                        let entries = Inventory.objects db
                        let target = entries |> List.find (fun e -> e.Ref = r)
                        let users = Deps.referencedBy (Deps.build db) r |> List.length
                        let p = Rename.plan db target (ObjectRef.name r + " Renamed") |> ok
                        Rename.apply p
                        let after = Deps.build db
                        Expect.equal (Deps.referencedBy after p.NewRef |> List.length) users (ObjectRef.format r)
                        Expect.equal (Deps.unresolved after |> List.length) unresolved (ObjectRef.format r)

                save TmdlFolder SaveOptions.defaults false db (t.Combine "tmdl") |> ok |> ignore
                let _, back = loadPath (t.Combine "tmdl") |> ok
                Expect.equal (Deps.unresolved (Deps.build back) |> List.length) unresolved "after save and reload"
    ]
