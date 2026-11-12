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
    | _ ->
        // TESSERA_CHECK_INCREMENTAL=1: every incremental filter context is recomputed in full and
        // compared (slow); the run fails if any differed.
        let check = Environment.GetEnvironmentVariable "TESSERA_CHECK_INCREMENTAL" = "1"
        let stats = Tessera.DaxEngine.IncrementalStats.Global

        if check then
            stats.Check <- true

        let code = runTestsInAssemblyWithCLIArgs [] argv

        if check then
            eprintfn
                "incremental filter contexts: %d applied, %d checked, %d mismatched, %d fell back after an error"
                stats.Applied
                stats.Checked
                stats.Mismatched
                stats.Errors

            if stats.Mismatched > 0L then 1 else code
        else
            code
