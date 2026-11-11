/// `--watch`: what counts as a change, how changes coalesce into runs, and the real thing driven
/// through a temp folder (FileSystemWatcher alone, polling alone) with a cancellation token.
module Tessera.Tests.WatchTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Argu
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Cli
open Tessera.Cli.Common
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness

/// Long enough for a slow CI machine; waits end as soon as the run happens.
let private patience = TimeSpan.FromSeconds 20.0

/// A watch running on a thread of its own, with what it wrote and the exit code of each run.
type private Watching(options: Watch.Options, scope: Watch.Scope, json: bool, command: Ctx -> Outcome) =
    let out = new StringWriter()
    let err = new StringWriter()
    let runs = new SemaphoreSlim(0)
    let codes = ConcurrentQueue<int>()
    let stop = new CancellationTokenSource()
    let changes = new Watch.Changes()

    let ctx: Ctx = {
        Out = TextWriter.Synchronized out
        Err = TextWriter.Synchronized err
        Json = json
        GetEnv = fun _ -> None
        Connector = XmlaFakes.NoConnections()
    }

    let task =
        Task.Run(fun () ->
            Watch.runWithChanges
                changes
                options
                stop.Token
                (fun code ->
                    codes.Enqueue code
                    runs.Release() |> ignore)
                ctx
                scope
                command)

    member _.Changes = changes

    /// Wait for the next run (true) or give up after `timeout` (false).
    member _.NextRun(timeout: TimeSpan) = runs.Wait timeout

    member _.Runs = codes.Count
    member _.Codes = List.ofSeq codes

    /// Cancel (Ctrl+C) and return the outcome, stdout and stderr.
    member _.Stop() =
        stop.Cancel()

        if not (task.Wait patience) then
            failtest "the watch didn't stop when cancelled"

        task.Result, out.ToString(), err.ToString()

    interface IDisposable with
        member _.Dispose() =
            stop.Cancel()
            task.Wait patience |> ignore
            (changes :> IDisposable).Dispose()

let private quiet = {
    Watch.Options.defaults with
        Debounce = TimeSpan.FromMilliseconds 100.0
        Poll = None
        UseWatcher = false
}

let private expectRun (w: Watching) (what: string) =
    Expect.isTrue (w.NextRun patience) $"expected a run: {what}"

let private expectNoRun (w: Watching) (wait: TimeSpan) (what: string) =
    Expect.isFalse (w.NextRun wait) $"expected no run: {what}"

/// A TMDL model in a temp folder; returns its root.
let private tmdlModel (t: TempDir) =
    let root = t.Combine "Model"
    TmdlSerializer.SerializeDatabaseToFolder(sampleDatabase (), root)
    root

/// `tess validate <model>` as the watch runs it.
let private validateCommand (model: string) =
    let args =
        ArgumentParser.Create<ValidateCommand.Args>().ParseCommandLine([| model |])

    fun (c: Ctx) -> ValidateCommand.run c args

/// A change a poller sees too: the file grows.
let private touch (file: string) = File.AppendAllText(file, "\n")

[<Tests>]
let tests =
    testList "watch" [
        testList "what counts as a change" [
            let folder = Path.Combine(Path.GetTempPath(), "tessera-watch-scope", "Model")

            let scope = {
                Watch.Scope.empty with
                    Folders = [ folder ]
                    Files = [ Path.Combine(Path.GetTempPath(), "tessera-watch-scope", "rules.json") ]
                    Exclude = [ Path.Combine(folder, "results.trx") ]
            }

            let at (parts: string list) =
                Path.Combine(Array.ofList (folder :: parts))

            testCase "files in the folder, at any depth"
            <| fun () ->
                Expect.isTrue (Watch.concerns scope (at [ "model.tmdl" ])) ""
                Expect.isTrue (Watch.concerns scope (at [ "tables"; "Sales.tmdl" ])) ""

            testCase "the single files, and nothing else beside them"
            <| fun () ->
                Expect.isTrue
                    (Watch.concerns scope (Path.Combine(Path.GetTempPath(), "tessera-watch-scope", "rules.json")))
                    ""

                Expect.isFalse
                    (Watch.concerns scope (Path.Combine(Path.GetTempPath(), "tessera-watch-scope", "other.json")))
                    ""

            testCase "outside the folder, including a sibling whose name starts the same"
            <| fun () ->
                Expect.isFalse (Watch.concerns scope (Path.Combine(Path.GetTempPath(), "elsewhere.tmdl"))) ""

                Expect.isFalse
                    (Watch.concerns scope (Path.Combine(Path.GetTempPath(), "tessera-watch-scope", "Model2", "x.tmdl")))
                    ""

            testCase "hidden folders: .git, .pbi"
            <| fun () ->
                Expect.isFalse (Watch.concerns scope (at [ ".git"; "index" ])) ""
                Expect.isFalse (Watch.concerns scope (at [ ".pbi"; "cache.abf" ])) ""

            testCase "editors' temporary files"
            <| fun () ->
                for name in
                    [
                        "model.tmdl~"
                        ".#model.tmdl"
                        "#model.tmdl#"
                        "~$model.tmdl"
                        "model.tmdl.swp"
                        ".model.tmdl.swx"
                        "4913"
                        "model.tmdl.tmp"
                        "model.tmdl___jb_tmp___"
                        "model.tmdl.crswap"
                    ] do
                    Expect.isFalse (Watch.concerns scope (at [ name ])) name

            testCase "the command's own outputs"
            <| fun () -> Expect.isFalse (Watch.concerns scope (at [ "results.trx" ])) ""
        ]

        testList "a model's scope" [
            testCase "a .bim file is that one file"
            <| fun () ->
                use t = new TempDir()
                let bim = t.Combine "m.bim"
                File.WriteAllText(bim, JsonSerializer.SerializeDatabase(sampleDatabase ()))

                match Watch.Scope.ofModel bim with
                | Ok scope ->
                    Expect.equal scope.Folders [] ""
                    Expect.equal scope.Files [ Path.GetFullPath bim ] ""
                | Error e -> failtestf "%A" e

            testCase "a TMDL folder is the folder"
            <| fun () ->
                use t = new TempDir()
                let root = tmdlModel t

                match Watch.Scope.ofModel root with
                | Ok scope -> Expect.equal scope.Folders [ Path.GetFullPath root ] ""
                | Error e -> failtestf "%A" e

            testCase "a .pbip project is its semantic model folder, not the report"
            <| fun () ->
                use t = new TempDir()
                let sm = writePbipProject t.Path "Sales"

                match Watch.Scope.ofModel (t.Combine "Sales.pbip") with
                | Ok scope -> Expect.equal scope.Folders [ Path.GetFullPath sm ] ""
                | Error e -> failtestf "%A" e

            testCase "a server model has no files to watch"
            <| fun () ->
                match Watch.modelScope "xmla:localhost;Sales" with
                | Error(RemoteSourceUnsupported _) -> ()
                | other -> failtestf "expected remote-source-unsupported, got %A" other
        ]

        testList "JSON Lines" [
            testCase "each document on one line"
            <| fun () ->
                let text = "{\n  \"a\": 1,\n  \"b\": [ 1, 2 ]\n}\n{ \"c\": \"x y\" }\r\n"
                Expect.equal (Watch.toJsonLines text) [ """{"a":1,"b":[1,2]}"""; """{"c":"x y"}""" ] ""

            testCase "no output, no lines"
            <| fun () -> Expect.equal (Watch.toJsonLines "  \n") [] ""

            testCase "text that isn't JSON stays one line"
            <| fun () -> Expect.equal (Watch.toJsonLines "oops\nmore\n") [ "oops more" ] ""
        ]

        testSequenced
        <| testList "runs" [
            testCase "a burst of changes is one run"
            <| fun () ->
                use w = new Watching(quiet, Watch.Scope.empty, false, (fun _ -> Done))
                expectRun w "the first run"

                for _ in 1..5 do
                    w.Changes.Notify()
                    Thread.Sleep 10

                expectRun w "after the burst"
                expectNoRun w (TimeSpan.FromMilliseconds 600.0) "the burst was one run"
                let outcome, _, _ = w.Stop()
                Expect.equal outcome Done "Ctrl+C: exit 0"
                Expect.equal w.Runs 2 ""

            testCase "changes during a run mean exactly one more run"
            <| fun () ->
                let started = new ManualResetEventSlim(false)
                let release = new ManualResetEventSlim(false)
                let mutable count = 0

                let command (_: Ctx) =
                    if Interlocked.Increment(&count) = 2 then
                        started.Set()
                        release.Wait patience |> ignore

                    Done

                use w = new Watching(quiet, Watch.Scope.empty, false, command)
                expectRun w "the first run"
                w.Changes.Notify()
                Expect.isTrue (started.Wait patience) "the second run started"

                for _ in 1..3 do
                    w.Changes.Notify()

                release.Set()
                expectRun w "the second run"
                expectRun w "one more, for the changes during it"
                expectNoRun w (TimeSpan.FromMilliseconds 600.0) "and no more"
                w.Stop() |> ignore

            testCase "a usage error on the first run ends the watch with it"
            <| fun () ->
                use w =
                    new Watching(quiet, Watch.Scope.empty, false, (fun _ -> UsageError "unknown rule 'x'"))

                let outcome, _, err = w.Stop()
                Expect.equal outcome (DoneWith ExitCode.usage) ""
                Expect.stringContains err "unknown rule" ""

            testCase "a later failure is reported, and the watch goes on (JSON: one line each)"
            <| fun () ->
                let mutable count = 0

                let command (c: Ctx) =
                    match Interlocked.Increment(&count) with
                    | 2 -> Failed(LoadFailed("m", "half-saved"))
                    | 3 -> failwith "boom"
                    | _ ->
                        Output.writeJson c.Out (Output.obj [ "ok", Output.bool true ])
                        Done

                use w = new Watching(quiet, Watch.Scope.empty, true, command)
                expectRun w "the first run"

                for _ in 1..3 do
                    w.Changes.Notify()
                    expectRun w "another run"

                let outcome, out, err = w.Stop()
                Expect.equal outcome Done ""
                Expect.equal w.Codes [ ExitCode.ok; ExitCode.input; ExitCode.internalError; ExitCode.ok ] ""
                Expect.equal err "" "JSON mode writes nothing to stderr"

                let lines = out.Replace("\r\n", "\n").TrimEnd('\n').Split('\n')
                Expect.equal lines.Length 4 out

                let codes =
                    lines
                    |> Array.map (fun l ->
                        let doc = JsonDocument.Parse l

                        match doc.RootElement.TryGetProperty "error" with
                        | true, e -> e.GetProperty("code").GetString()
                        | _ -> "ok")

                Expect.equal codes [| "ok"; "load-failed"; "internal"; "ok" |] ""

            testCase "text mode says what it watches, on stderr"
            <| fun () ->
                use t = new TempDir()
                let root = tmdlModel t

                match Watch.Scope.ofModel root with
                | Error e -> failtestf "%A" e
                | Ok scope ->
                    use w = new Watching(quiet, scope, false, validateCommand root)
                    expectRun w "the first run"
                    let _, out, err = w.Stop()
                    Expect.stringContains out "No issues." ""
                    Expect.stringContains err "watching" ""
                    Expect.stringContains err "Ctrl+C" ""
        ]

        testSequenced
        <| testList "on disk" [
            let watchModel (options: Watch.Options) (body: string -> Watching -> unit) =
                use t = new TempDir()
                let root = tmdlModel t

                match Watch.Scope.ofModel root with
                | Error e -> failtestf "%A" e
                | Ok scope ->
                    use w = new Watching(options, scope, true, validateCommand root)
                    expectRun w "the first run"
                    body root w
                    let outcome, out, _ = w.Stop()
                    Expect.equal outcome Done ""

                    for line in out.Replace("\r\n", "\n").TrimEnd('\n').Split('\n') do
                        let doc = JsonDocument.Parse line
                        Expect.equal (doc.RootElement.GetProperty("command").GetString()) "validate" line

            testCase "FileSystemWatcher: saving a file runs again"
            <| fun () ->
                watchModel { quiet with UseWatcher = true } (fun root w ->
                    touch (Path.Combine(root, "model.tmdl"))
                    expectRun w "after the save")

            testCase "polling alone: saving a file runs again"
            <| fun () ->
                watchModel
                    {
                        quiet with
                            Poll = Some(TimeSpan.FromMilliseconds 100.0)
                    }
                    (fun root w ->
                        touch (Path.Combine(root, "model.tmdl"))
                        expectRun w "after the save")

            testCase "an editor's save-rename runs once"
            <| fun () ->
                watchModel
                    {
                        quiet with
                            UseWatcher = true
                            Poll = Some(TimeSpan.FromMilliseconds 100.0)
                    }
                    (fun root w ->
                        let file = Path.Combine(root, "model.tmdl")
                        let temp = file + ".tmp"
                        File.WriteAllText(temp, File.ReadAllText file + "\n")
                        File.Move(temp, file, true)
                        expectRun w "after the save"
                        expectNoRun w (TimeSpan.FromMilliseconds 800.0) "one save, one run")

            testCase "temporary files and hidden folders don't run it; a real change still does"
            <| fun () ->
                watchModel
                    {
                        quiet with
                            UseWatcher = true
                            Poll = Some(TimeSpan.FromMilliseconds 100.0)
                    }
                    (fun root w ->
                        File.WriteAllText(Path.Combine(root, "model.tmdl.swp"), "x")
                        File.WriteAllText(Path.Combine(root, "model.tmdl~"), "x")
                        Directory.CreateDirectory(Path.Combine(root, ".pbi")) |> ignore
                        File.WriteAllText(Path.Combine(root, ".pbi", "cache.abf"), "x")
                        expectNoRun w (TimeSpan.FromMilliseconds 800.0) "nothing that counts changed"
                        touch (Path.Combine(root, "model.tmdl"))
                        expectRun w "after a real change")

            testCase "files outside the model don't run it"
            <| fun () ->
                watchModel
                    {
                        quiet with
                            UseWatcher = true
                            Poll = Some(TimeSpan.FromMilliseconds 100.0)
                    }
                    (fun root w ->
                        File.WriteAllText(Path.Combine(Path.GetDirectoryName root, "notes.txt"), "x")
                        expectNoRun w (TimeSpan.FromMilliseconds 800.0) "outside the model")
        ]

        testList "the command line" [
            testCase "a server model: refused"
            <| fun () ->
                let r = tess [ "validate"; "xmla:localhost;Sales"; "--watch" ]
                Expect.equal r.Code ExitCode.input r.Err
                Expect.stringContains r.Err "--watch" ""

            testCase "a missing model: refused"
            <| fun () ->
                use t = new TempDir()
                let r = tess [ "lint"; t.Combine "nope"; "--watch" ]
                Expect.equal r.Code ExitCode.input r.Err

            testCase "lint --fix --save: refused (it would change what it watches)"
            <| fun () ->
                use t = new TempDir()
                let root = tmdlModel t
                let r = tess [ "lint"; root; "--watch"; "--fix"; "--save" ]
                Expect.equal r.Code ExitCode.usage r.Err

            testCase "bpa rules: refused"
            <| fun () ->
                use t = new TempDir()
                let root = tmdlModel t
                let r = tess [ "bpa"; "--watch"; "rules"; "list"; root ]
                Expect.equal r.Code ExitCode.usage r.Err

            testCase "a usage error stops at once, as JSON with --json"
            <| fun () ->
                use t = new TempDir()
                let root = tmdlModel t
                let r = tess [ "validate"; root; "--watch"; "--rule"; "bogus"; "--json" ]
                Expect.equal r.Code ExitCode.usage r.Out
                let doc = json r
                Expect.equal (doc.RootElement.GetProperty("error").GetProperty("code").GetString()) "usage" ""

            testCase "test run with a server model: refused"
            <| fun () ->
                let r = tess [ "test"; "run"; "xmla:localhost;Sales"; "--watch" ]
                Expect.equal r.Code ExitCode.input r.Err

            testCase "--watch is in each command's help"
            <| fun () ->
                for command in [ [ "validate" ]; [ "lint" ]; [ "bpa" ]; [ "test"; "run" ] ] do
                    let r = tess (command @ [ "--help" ])
                    Expect.stringContains r.Out "--watch" (String.concat " " command)
        ]
    ]
