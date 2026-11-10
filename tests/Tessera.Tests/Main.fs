module Tessera.Tests.Main

open System
open Expecto

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    // Not a test: runs the conformance corpus against a real server (docs/xmla-manual-test.md, F).
    | "conformance-server" :: rest ->
        ConformanceServer.main
            (Tessera.Xmla.LiveConnector())
            Tessera.Cli.Common.processEnv
            Console.Out
            Console.Error
            rest
    | _ -> runTestsInAssemblyWithCLIArgs [] argv
