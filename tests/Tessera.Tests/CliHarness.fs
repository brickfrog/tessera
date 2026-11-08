/// Drive the real CLI entry point with captured stdout/stderr. Shared by the CLI test files.
module Tessera.Tests.CliHarness

open System.IO
open System.Text.Json
open Expecto
open Tessera.Cli
open Tessera.Tests.Fixtures

type Run = { Code: int; Out: string; Err: string }

/// Run `tess <args>` in-process.
let tess (args: string list) =
    use out = new StringWriter()
    use err = new StringWriter()
    let code = App.run out err (Array.ofList args)

    // WriteLine uses the platform newline (CRLF on Windows); tests compare against LF.
    let lf (s: string) = s.Replace("\r\n", "\n")

    {
        Code = code
        Out = lf (out.ToString())
        Err = lf (err.ToString())
    }

let private stdinLock = obj ()

/// Run `tess <args>` with `input` as stdin (Console.In, swapped for the run). Runs are serialized,
/// but Console.In is process-wide: put tests that use this in a testSequenced list.
let tessWithStdin (input: string) (args: string list) =
    lock stdinLock (fun () ->
        let original = System.Console.In
        System.Console.SetIn(new StringReader(input))

        try
            tess args
        finally
            System.Console.SetIn original)

/// Parse a run's stdout as JSON (fails the test if it isn't).
let json (r: Run) =
    try
        JsonDocument.Parse(r.Out)
    with e ->
        failtestf "stdout is not JSON (%s):\n%s" e.Message r.Out

/// A temp folder holding a PBIP project (Sales.pbip, Sales.Report, Sales.SemanticModel) for the
/// sample model. Dispose to delete.
let project () =
    let t = new TempDir()
    writePbipProject t.Path "Sales" |> ignore
    t

module ExitCode = Tessera.Cli.Common.ExitCode
