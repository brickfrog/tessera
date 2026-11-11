/// `--watch`: run a command again whenever the files it reads change.
///
/// A run happens at the start, then after every burst of changes: a change starts a quiet period
/// (Options.Debounce), each further change restarts it, and the command runs once it ends. A change
/// during a run means exactly one more run after it. Changes come from a FileSystemWatcher and, as
/// a safety net for file systems that don't report changes (network drives, some container and WSL
/// mounts) or a watcher that overflows or can't be created (inotify limits), from polling the
/// files' sizes and times. Editors' temporary files, hidden folders (.git, .pbi, .vs) and the
/// command's own outputs never trigger a run.
module Tessera.Cli.Watch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Tessera
open Tessera.Cli.Common
open Tessera.Cli.Output

/// The --watch option's help, shared by the commands that take it.
let usage =
    "run again whenever the model's files change, until Ctrl+C (exit 0); with --json, one JSON document per run."

// ---- What to watch ----------------------------------------------------------------------------

/// The files a command reads: whole folders (recursively) and single files, less its outputs.
type Scope = {
    Folders: string list
    Files: string list
    /// Files the command writes (--trx, --junit): changing them must not trigger a run.
    Exclude: string list
}

/// Paths compare case-insensitively where the file system usually does (Windows, macOS).
let private comparison =
    if OperatingSystem.IsWindows() || OperatingSystem.IsMacOS() then
        StringComparison.OrdinalIgnoreCase
    else
        StringComparison.Ordinal

let private full (path: string) =
    Path.GetFullPath(path).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

module Scope =
    let empty = {
        Folders = []
        Files = []
        Exclude = []
    }

    /// A model's files: the .bim file, or the whole TMDL / PBIP semantic model folder.
    let ofSource (source: ModelSource) =
        match source.Format, source.SemanticModelFolder with
        | Bim, _ -> {
            empty with
                Files = [ full source.Path ]
          }
        | _, Some folder -> { empty with Folders = [ full folder ] }
        | _, None -> {
            empty with
                Folders = [ full source.Path ]
          }

    /// The model's files, or why the path is not a local model.
    let ofModel (model: string) =
        ModelSource.resolve model |> Result.map ofSource

    /// Add a path: a folder is watched as a whole, anything else as a file (it may not exist yet).
    let add (path: string) (scope: Scope) =
        let p = full path

        if Directory.Exists p then
            {
                scope with
                    Folders = scope.Folders @ [ p ]
            }
        else
            {
                scope with
                    Files = scope.Files @ [ p ]
            }

    let exclude (path: string) (scope: Scope) = {
        scope with
            Exclude = scope.Exclude @ [ full path ]
    }

    /// What it watches, for the status line.
    let describe (scope: Scope) =
        scope.Folders @ scope.Files |> List.distinct |> String.concat ", "

/// Editors' and tools' temporary and backup files (vim, emacs, JetBrains, VS Code, Office, Kate).
let isTemporary (name: string) =
    let lower = name.ToLowerInvariant()

    lower.StartsWith "~"
    || lower.StartsWith ".#"
    || (lower.StartsWith "#" && lower.EndsWith "#")
    || lower.EndsWith "~"
    || lower = "4913"
    || lower.Contains "___jb_"
    || [ ".tmp"; ".temp"; ".swp"; ".swo"; ".swx"; ".crswap"; ".kate-swp"; ".part" ]
       |> List.exists lower.EndsWith

let private under (folder: string) (path: string) =
    path.StartsWith(folder + string Path.DirectorySeparatorChar, comparison)
    || (Path.AltDirectorySeparatorChar <> Path.DirectorySeparatorChar
        && path.StartsWith(folder + string Path.AltDirectorySeparatorChar, comparison))

/// Does a change to this path concern the command? It must be one of the files, or inside one of
/// the folders without passing through a hidden folder (.git, .pbi, ...), and not a temporary file
/// or one of the command's outputs.
let concerns (scope: Scope) (path: string) =
    let p = full path
    let same (a: string) = String.Equals(a, p, comparison)

    if isTemporary (Path.GetFileName p) || scope.Exclude |> List.exists same then
        false
    elif scope.Files |> List.exists same then
        true
    else
        scope.Folders
        |> List.exists (fun folder ->
            under folder p
            && p.Substring(folder.Length + 1).Split([| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |])
               |> Array.forall (fun segment -> not (segment.StartsWith ".")))

// ---- Change signals ---------------------------------------------------------------------------

/// Changes not yet acted on. Notify from any thread; the watch loop waits on it.
type Changes() =
    let signal = new SemaphoreSlim(0, 1)
    let mutable last = 0L
    let mutable pending = 0

    /// Something changed.
    member _.Notify() =
        Interlocked.Exchange(&last, Diagnostics.Stopwatch.GetTimestamp()) |> ignore

        if Interlocked.Exchange(&pending, 1) = 0 then
            try
                signal.Release() |> ignore
            with :? SemaphoreFullException ->
                ()

    /// How long since the last change.
    member _.Quiet = Diagnostics.Stopwatch.GetElapsedTime(Interlocked.Read &last)

    /// Wait until something has changed since the last Take.
    member _.Wait(token: CancellationToken) = signal.WaitAsync(token)

    /// Mark the changes so far as acted on.
    member _.Take() =
        Interlocked.Exchange(&pending, 0) |> ignore

    interface IDisposable with
        member _.Dispose() = signal.Dispose()

/// Sizes and times of the files in scope, for polling.
let snapshot (scope: Scope) : Map<string, int64 * DateTime> =
    let entry (file: string) =
        try
            let info = FileInfo file

            if info.Exists then
                Some(file, (info.Length, info.LastWriteTimeUtc))
            else
                None
        with _ ->
            None

    let inFolders =
        scope.Folders
        |> Seq.collect (fun folder ->
            try
                Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories) |> Seq.toList
            with _ -> [])

    Seq.append inFolders scope.Files
    |> Seq.map full
    |> Seq.filter (concerns scope)
    |> Seq.choose entry
    |> Map.ofSeq

/// Polls the files in scope every `interval` and notifies when they differ from the baseline.
type Poller(scope: Scope, interval: TimeSpan, changes: Changes) =
    let gate = Object()
    let mutable baseline = snapshot scope
    let stop = new CancellationTokenSource()

    let loop =
        task {
            try
                while not stop.IsCancellationRequested do
                    do! Task.Delay(interval, stop.Token)
                    let now = snapshot scope
                    let differs = lock gate (fun () -> now <> baseline)

                    if differs then
                        lock gate (fun () -> baseline <- now)
                        changes.Notify()
            with :? OperationCanceledException ->
                ()
        }

    /// Take the files as they are now as the baseline (at the start of a run, so a change already
    /// reported by the watcher isn't reported again).
    member _.Rebase() =
        let now = snapshot scope
        lock gate (fun () -> baseline <- now)

    interface IDisposable with
        member _.Dispose() =
            stop.Cancel()

            try
                loop.Wait(TimeSpan.FromSeconds 5.0) |> ignore
            with _ ->
                ()

            stop.Dispose()

/// FileSystemWatchers over the scope's folders (and the folders of its files). `onError` is told
/// when one can't be created or fails (the poller still covers the files).
let private watchers (scope: Scope) (changes: Changes) (onError: string -> unit) : IDisposable list =
    let notifyIf (path: string) =
        if not (isNull path) && concerns scope path then
            changes.Notify()

    let watch (folder: string) (recursive: bool) =
        try
            let w = new FileSystemWatcher(folder)
            w.IncludeSubdirectories <- recursive

            w.NotifyFilter <-
                NotifyFilters.FileName
                ||| NotifyFilters.DirectoryName
                ||| NotifyFilters.LastWrite
                ||| NotifyFilters.Size
                ||| NotifyFilters.CreationTime

            w.InternalBufferSize <- 64 * 1024
            w.Changed.Add(fun e -> notifyIf e.FullPath)
            w.Created.Add(fun e -> notifyIf e.FullPath)
            w.Deleted.Add(fun e -> notifyIf e.FullPath)

            w.Renamed.Add(fun e ->
                // An editor's save-rename: the old name is often a temporary file, the new one counts.
                notifyIf e.FullPath
                notifyIf e.OldFullPath)

            w.Error.Add(fun e ->
                // A buffer overflow loses events: run again to be safe, and let the poller carry on.
                onError (e.GetException().Message)
                changes.Notify())

            w.EnableRaisingEvents <- true
            Some(w :> IDisposable)
        with e ->
            onError e.Message
            None

    let fileFolders =
        scope.Files
        |> List.choose (fun f ->
            match Path.GetDirectoryName f with
            | null
            | "" -> None
            | d when Directory.Exists d -> Some d
            | _ -> None)
        |> List.distinct
        // A folder watched recursively already covers these.
        |> List.filter (fun d ->
            not (
                scope.Folders
                |> List.exists (fun f -> String.Equals(f, d, comparison) || under f d)
            ))

    [
        for folder in scope.Folders |> List.distinct do
            if Directory.Exists folder then
                yield! Option.toList (watch folder true)
        for folder in fileFolders do
            yield! Option.toList (watch folder false)
    ]

// ---- Runs -------------------------------------------------------------------------------------

type Options = {
    Debounce: TimeSpan
    /// None: no polling (the watcher alone).
    Poll: TimeSpan option
    /// Use FileSystemWatcher (off: polling alone).
    UseWatcher: bool
    /// Clear the screen before each run (text output to a terminal only).
    Clear: bool
}

module Options =
    let defaults = {
        Debounce = TimeSpan.FromMilliseconds 300.0
        Poll = Some(TimeSpan.FromSeconds 1.0)
        UseWatcher = true
        Clear = false
    }

let private compactOptions =
    JsonSerializerOptions(WriteIndented = false, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

/// Each JSON document in `text` on a line of its own (JSON Lines). Text that isn't JSON is passed
/// on with its line breaks removed, so every line stays one document or one message.
let toJsonLines (text: string) : string list =
    let bytes = Encoding.UTF8.GetBytes text

    let rec read (offset: int) acc =
        let rest = ReadOnlySpan<byte>(bytes, offset, bytes.Length - offset)
        let mutable reader = Utf8JsonReader(rest)

        // The reader refuses input that is only whitespace: that is the end.
        if rest.Trim(" \t\r\n"B).IsEmpty || not (reader.Read()) then
            List.rev acc
        else
            use doc = JsonDocument.ParseValue(&reader)
            let line = JsonSerializer.Serialize(doc.RootElement, compactOptions)
            read (offset + int reader.BytesConsumed) (line :: acc)

    try
        read 0 []
    with _ ->
        match text.Trim() with
        | "" -> []
        | t -> [ t.Replace("\r\n", " ").Replace('\n', ' ') ]

/// What App does with an outcome, for one run: the exit code, with errors written out.
let private settle (ctx: Ctx) (outcome: Outcome) =
    match outcome with
    | Done -> ExitCode.ok
    | DoneWith code -> code
    | UsageError message ->
        if ctx.Json then
            writeJson ctx.Out (errorJson "usage" message)
        else
            ctx.Err.WriteLine message

        ExitCode.usage
    | Failed e ->
        if ctx.Json then
            writeJson ctx.Out (tessErrorJson e)
        else
            ctx.Err.WriteLine($"tess: {TessError.message e}")

        ExitCode.ofError e

/// A run that threw: reported as App reports it, and the watch goes on.
let private crashed (ctx: Ctx) (e: exn) =
    if ctx.Json then
        writeJson ctx.Out (errorJson "internal" e.Message)
    else
        ctx.Err.WriteLine($"tess: internal error: {e}")

    None, ExitCode.internalError

/// One run: in JSON mode its output becomes JSON Lines; in text mode it goes straight through.
/// The outcome is None when the command threw.
let private runOnce (options: Options) (ctx: Ctx) (command: Ctx -> Outcome) =
    let attempt (runCtx: Ctx) =
        try
            let outcome = command runCtx
            Some outcome, settle runCtx outcome
        with e ->
            crashed runCtx e

    if ctx.Json then
        use buffer = new StringWriter()
        let result = attempt { ctx with Out = buffer }

        for line in toJsonLines (buffer.ToString()) do
            ctx.Out.Write(line + "\n")

        ctx.Out.Flush()
        result
    else
        if options.Clear then
            try
                Console.Clear()
            with _ ->
                ()

        let result = attempt ctx
        ctx.Out.Flush()
        result

/// Run `command`, then again after each burst of changes in `scope`, until `token` is cancelled
/// (Ctrl+C), which ends it with exit 0. A usage error on the first run ends it at once, with that
/// error; later failures (a model saved half-way, say) are reported and the watch goes on.
/// `onRun` is told each run's exit code (tests wait on it).
/// `changes` also takes notifications from elsewhere (tests drive the loop through it).
let runWithChanges
    (changes: Changes)
    (options: Options)
    (token: CancellationToken)
    (onRun: int -> unit)
    (ctx: Ctx)
    (scope: Scope)
    (command: Ctx -> Outcome)
    : Outcome =
    let reported = HashSet<string>()

    let onError (message: string) =
        if not ctx.Json && lock reported (fun () -> reported.Add message) then
            ctx.Err.WriteLine($"tess: watch: {message}; polling for changes instead")

    // Watch before the first run, so nothing saved during it is missed.
    let watching =
        if options.UseWatcher then
            watchers scope changes onError
        else
            []

    let poller =
        options.Poll
        |> Option.map (fun interval -> new Poller(scope, interval, changes))

    let status () =
        if not ctx.Json then
            let time =
                DateTime.Now.ToString("HH:mm:ss", Globalization.CultureInfo.InvariantCulture)

            ctx.Err.WriteLine($"[{time}] watching {Scope.describe scope} (Ctrl+C to stop)")

    let rebase () =
        poller |> Option.iter (fun p -> p.Rebase())

    try
        rebase ()

        match runOnce options ctx command with
        | Some(UsageError _), code -> DoneWith code
        | _, code ->
            onRun code
            status ()

            try
                while not token.IsCancellationRequested do
                    changes.Wait(token).Wait(token)

                    // Debounce: wait until nothing has changed for a quiet period.
                    let mutable quiet = changes.Quiet

                    while quiet < options.Debounce do
                        Task.Delay(options.Debounce - quiet, token).Wait(token)
                        quiet <- changes.Quiet

                    changes.Take()
                    rebase ()
                    let _, code = runOnce options ctx command
                    onRun code
                    status ()
            with
            | :? OperationCanceledException -> ()
            | :? AggregateException as e when (e.InnerException :? OperationCanceledException) -> ()

            Done
    finally
        for w in watching do
            w.Dispose()

        poller |> Option.iter (fun p -> (p :> IDisposable).Dispose())

let runWith options token onRun ctx scope command =
    use changes = new Changes()
    runWithChanges changes options token onRun ctx scope command

/// `--watch` from the command line: watch until Ctrl+C.
let run (ctx: Ctx) (scope: Result<Scope, TessError>) (command: Ctx -> Outcome) : Outcome =
    match scope with
    | Error e -> Failed e
    | Ok scope ->
        use cancel = new CancellationTokenSource()

        let handler =
            ConsoleCancelEventHandler(fun _ e ->
                e.Cancel <- true
                cancel.Cancel())

        Console.CancelKeyPress.AddHandler handler

        try
            let toTerminal =
                not ctx.Json
                && Object.ReferenceEquals(ctx.Out, Console.Out)
                && not Console.IsOutputRedirected

            runWith
                {
                    Options.defaults with
                        Clear = toTerminal
                }
                cancel.Token
                ignore
                ctx
                scope
                command
        finally
            Console.CancelKeyPress.RemoveHandler handler

/// The scope for a model argument: its files, or a usage error for a server model.
let modelScope (model: string) : Result<Scope, TessError> =
    if XmlaAddress.isRemote model then
        Error(RemoteSourceUnsupported(model, "--watch watches model files, so it needs a model on disk"))
    else
        Scope.ofModel model
