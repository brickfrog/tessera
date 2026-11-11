/// `tess completion` and `tess __complete`: the scripts are stable (golden files) and valid in each
/// shell that is installed, they complete what tess accepts, and `__complete` offers the model's
/// objects (and nothing at all when anything goes wrong).
///
/// After a change to the command line, refresh the golden files with
/// TESSERA_UPDATE_GOLDEN=1 dotnet test -c Release --filter completion
module Tessera.Tests.CompletionTests

open System
open System.Diagnostics
open System.IO
open Argu
open Expecto
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Cli
open Tessera.Cli.Common
open Tessera.Cli.CompletionCommand
open Tessera.Tests.Fixtures
open Tessera.Tests.CliHarness

let private parser = ArgumentParser.Create<Args.CliArgs>(programName = "tess")
let private root = spec parser

let private goldenDir = Path.Combine(__SOURCE_DIRECTORY__, "completion")

let private shells = [ Bash, "tess.bash"; Zsh, "tess.zsh"; Fish, "tess.fish"; Pwsh, "tess.ps1" ]

let private shellName =
    function
    | Bash -> "bash"
    | Zsh -> "zsh"
    | Fish -> "fish"
    | Pwsh -> "pwsh"

// ---- Running a shell ----------------------------------------------------------------------------

type private Ran = { Code: int; Out: string; Err: string }

/// Run a program (no shell in between), with these extra environment variables. None if it can't
/// be started or doesn't finish in time.
let private runProgram (program: string) (args: string list) (env: (string * string) list) (stdin: string option) =
    try
        let psi = ProcessStartInfo(program)

        for a in args do
            psi.ArgumentList.Add a

        for k, v in env do
            psi.Environment.[k] <- v

        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.RedirectStandardInput <- true
        psi.UseShellExecute <- false
        use p = Process.Start psi

        match stdin with
        | Some text -> p.StandardInput.Write text
        | None -> ()

        p.StandardInput.Close()
        let out = p.StandardOutput.ReadToEndAsync()
        let err = p.StandardError.ReadToEndAsync()

        if p.WaitForExit 60_000 then
            Some {
                Code = p.ExitCode
                Out = out.Result.Replace("\r\n", "\n")
                Err = err.Result
            }
        else
            p.Kill true
            None
    with _ ->
        None

/// The shells installed here that really run. On Windows only PowerShell: `bash` there may be
/// WSL's, which can't read Windows paths.
let private available =
    lazy
        (let works (program: string) (args: string list) =
            match runProgram program args [] None with
            | Some r -> r.Code = 0
            | None -> false

         [
             if not (OperatingSystem.IsWindows()) then
                 if works "bash" [ "-c"; "exit 0" ] then
                     Bash

                 if works "zsh" [ "-c"; "exit 0" ] then
                     Zsh

                 if works "fish" [ "-c"; "exit 0" ] then
                     Fish
             if works "pwsh" [ "-NoProfile"; "-NonInteractive"; "-Command"; "exit 0" ] then
                 Pwsh
         ])

let private requireShell (shell: Shell) =
    if not (List.contains shell available.Value) then
        skiptest $"{shellName shell} is not installed (or, on Windows, not used for these tests)"

/// A folder with a stand-in `tess` that prints these candidates for any `__complete`, and writes
/// the arguments it was given to `args.txt` beside it.
let private stubTess (t: TempDir) (candidates: string list) =
    let bin = t.Combine "bin"
    Directory.CreateDirectory bin |> ignore

    let lines =
        candidates
        |> List.map (fun c -> c.Replace("'", "'\\''"))
        |> List.map (sprintf "'%s'")

    let argsFile = Path.Combine(bin, "args.txt")
    let quoted = String.concat " " lines

    let script =
        "#!/bin/sh\n"
        + $"printf '%%s\\n' \"$@\" > '{argsFile}'\n"
        + $"printf '%%s\\n' {quoted}\n"

    let path = Path.Combine(bin, "tess")
    File.WriteAllText(path, script)
    File.SetUnixFileMode(path, enum<UnixFileMode> 0o755)
    bin

// ---- The fixture model --------------------------------------------------------------------------

let private objects = lazy (Inventory.objects (sampleDatabase ()))

/// Candidates, with the fixture model behind any model path.
let private candidates (words: string list) (current: string) =
    candidatesWith (fun _ -> objects.Value) root words current

let private writeBim (t: TempDir) =
    let path = t.Combine "Sample.bim"
    File.WriteAllText(path, JsonSerializer.SerializeDatabase(sampleDatabase ()))
    path

let private complete (args: string list) = tess ("__complete" :: args)

let private samplePath =
    match Environment.GetEnvironmentVariable "TESSERA_SAMPLE_MODEL" with
    | null
    | "" -> None
    | p -> Some p

[<Tests>]
let tests =
    testList "completion" [
        testList "scripts" [
            for shell, file in shells do
                testCase $"{shellName shell}: the script is the golden file"
                <| fun () ->
                    let r = tess [ "completion"; shellName shell ]
                    Expect.equal r.Code ExitCode.ok r.Err
                    let golden = Path.Combine(goldenDir, file)

                    if Environment.GetEnvironmentVariable "TESSERA_UPDATE_GOLDEN" = "1" then
                        Directory.CreateDirectory goldenDir |> ignore
                        File.WriteAllText(golden, r.Out)
                    else
                        let expected = File.ReadAllText(golden).Replace("\r\n", "\n")

                        Expect.equal
                            r.Out
                            expected
                            $"{file} is out of date: TESSERA_UPDATE_GOLDEN=1 dotnet test -c Release --filter completion"

            testCase "LF line endings on every platform, and no --json form"
            <| fun () ->
                for shell, _ in shells do
                    let r = tess [ "completion"; shellName shell; "--json" ]
                    Expect.equal r.Code ExitCode.ok r.Err
                    Expect.isFalse (r.Out.Contains "\r") (shellName shell)
                    Expect.isFalse (r.Out.TrimStart().StartsWith "{") "a script, not JSON"

            testCase "an unknown shell is a usage error"
            <| fun () ->
                let r = tess [ "completion"; "tcsh" ]
                Expect.equal r.Code ExitCode.usage r.Err

            testCase "bash: valid syntax"
            <| fun () ->
                requireShell Bash
                use t = new TempDir()
                let path = t.Combine "tess.bash"
                File.WriteAllText(path, (tess [ "completion"; "bash" ]).Out)
                let r = (runProgram "bash" [ "-n"; path ] [] None).Value
                Expect.equal r.Code 0 r.Err

            testCase "zsh: valid syntax"
            <| fun () ->
                requireShell Zsh
                use t = new TempDir()
                let path = t.Combine "_tess"
                File.WriteAllText(path, (tess [ "completion"; "zsh" ]).Out)
                let r = (runProgram "zsh" [ "-n"; path ] [] None).Value
                Expect.equal r.Code 0 r.Err

            testCase "fish: valid syntax"
            <| fun () ->
                requireShell Fish
                use t = new TempDir()
                let path = t.Combine "tess.fish"
                File.WriteAllText(path, (tess [ "completion"; "fish" ]).Out)
                let r = (runProgram "fish" [ "--no-execute"; path ] [] None).Value
                Expect.equal r.Code 0 r.Err

            testCase "pwsh: valid syntax"
            <| fun () ->
                requireShell Pwsh
                use t = new TempDir()
                let path = t.Combine "tess.ps1"
                File.WriteAllText(path, (tess [ "completion"; "pwsh" ]).Out)

                // -File, not -Command: only a script file gets its arguments in $args.
                let check = t.Combine "check.ps1"

                File.WriteAllText(
                    check,
                    "$e = $null; [System.Management.Automation.Language.Parser]::ParseFile($args[0], [ref]$null, [ref]$e) | Out-Null\n"
                    + "if ($e) { $e | ForEach-Object { $_.Message }; exit 1 }\n"
                )

                let r =
                    (runProgram "pwsh" [ "-NoProfile"; "-NonInteractive"; "-File"; check; path ] [] None).Value

                Expect.equal r.Code 0 (r.Out + r.Err)
        ]

        testList "the scripts at work" [
            // bash: what readline would hand the function, words split at ':' as bash does.
            let bashComplete (t: TempDir) (bin: string) (compWords: string list) (line: string) =
                let script = t.Combine "tess.bash"
                File.WriteAllText(script, (tess [ "completion"; "bash" ]).Out)

                let quote (w: string) = "'" + w.Replace("'", "'\\''") + "'"
                let words = compWords |> List.map quote |> String.concat " "

                let driver =
                    $"source '{script}'\n"
                    + $"COMP_WORDS=({words})\n"
                    + $"COMP_CWORD={compWords.Length - 1}\n"
                    + $"COMP_LINE={quote line}\n"
                    + "COMP_POINT=${#COMP_LINE}\n"
                    + "_tess\n"
                    + "printf '%s\\n' \"${COMPREPLY[@]}\"\n"

                let r =
                    (runProgram
                        "bash"
                        [ "--norc"; "--noprofile"; "-s" ]
                        [ "PATH", bin + ":/usr/bin:/bin" ]
                        (Some driver))
                        .Value

                Expect.equal r.Code 0 r.Err
                r.Out.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

            testCase "bash: commands, options and their values"
            <| fun () ->
                requireShell Bash
                use t = new TempDir()
                let bin = stubTess t []
                Expect.equal (bashComplete t bin [ "tess"; "va" ] "tess va") [ "validate" ] ""
                Expect.contains (bashComplete t bin [ "tess"; "validate"; "--" ] "tess validate --") "--watch" ""

                Expect.equal
                    (bashComplete t bin [ "tess"; "validate"; "m"; "--fail-on"; "" ] "tess validate m --fail-on ")
                    [ "error"; "warning"; "info" ]
                    ""

                Expect.equal (bashComplete t bin [ "tess"; "bpa"; "rules"; "l" ] "tess bpa rules l") [ "list" ] ""

                Expect.equal
                    (bashComplete
                        t
                        bin
                        [ "tess"; "bpa"; "rules"; "list"; "m"; "--layer"; "b" ]
                        "tess bpa rules list m --layer b")
                    [ "builtin" ]
                    ""

            testCase "bash: model objects, rejoined at ':' and escaped"
            <| fun () ->
                requireShell Bash
                use t = new TempDir()
                let bin = stubTess t [ "measure:Sales/Total Sales"; "measure:Sales/Tax %" ]

                // `tess get m measure:Sales/T`: bash splits the word at ':' and replaces only "Sales/T".
                let reply =
                    bashComplete t bin [ "tess"; "get"; "m"; "measure"; ":"; "Sales/T" ] "tess get m measure:Sales/T"

                Expect.equal reply [ "Sales/Total\\ Sales"; "Sales/Tax\\ %" ] ""
                let args = File.ReadAllLines(Path.Combine(bin, "args.txt")) |> List.ofArray

                Expect.equal args [ "__complete"; "--quoted"; "--current=measure:Sales/T"; "--"; "get"; "m" ] ""

            testCase "fish: commands, options, values and model objects"
            <| fun () ->
                requireShell Fish
                use t = new TempDir()
                let bin = stubTess t [ "measure:Sales/Total Sales" ]
                let script = t.Combine "tess.fish"
                File.WriteAllText(script, (tess [ "completion"; "fish" ]).Out)

                let fishComplete (line: string) =
                    let r =
                        (runProgram
                            "fish"
                            [ "--no-config"; "-c"; $"source '{script}'; complete -C '{line}'" ]
                            [ "PATH", bin + ":/usr/bin:/bin:/usr/local/bin:/opt/homebrew/bin" ]
                            None)
                            .Value

                    Expect.equal r.Code 0 r.Err

                    r.Out.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun l -> l.Split('\t').[0])
                    |> List.ofArray

                Expect.equal (fishComplete "tess va") [ "validate" ] ""
                Expect.contains (fishComplete "tess validate --w") "--watch" ""
                Expect.equal (fishComplete "tess validate m --ci a") [ "azdo" ] ""
                Expect.equal (fishComplete "tess get m measure:Sales/T") [ "measure:Sales/Total Sales" ] ""
                let args = File.ReadAllLines(Path.Combine(bin, "args.txt")) |> List.ofArray
                Expect.equal args [ "__complete"; "--current=measure:Sales/T"; "--"; "get"; "m" ] ""

            testCase "pwsh: commands, options and values"
            <| fun () ->
                requireShell Pwsh
                use t = new TempDir()
                let script = t.Combine "tess.ps1"
                File.WriteAllText(script, (tess [ "completion"; "pwsh" ]).Out)

                let driver = t.Combine "drive.ps1"

                File.WriteAllText(
                    driver,
                    $". '{script}'\n"
                    + "(TabExpansion2 -inputScript $args[0] -cursorColumn $args[0].Length).CompletionMatches | ForEach-Object { $_.CompletionText }\n"
                )

                let pwshComplete (line: string) =
                    let r =
                        (runProgram "pwsh" [ "-NoProfile"; "-NonInteractive"; "-File"; driver; line ] [] None).Value

                    Expect.equal r.Code 0 r.Err
                    r.Out.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

                Expect.equal (pwshComplete "tess va") [ "validate" ] ""
                Expect.equal (pwshComplete "tess bpa r") [ "rules" ] ""
                Expect.contains (pwshComplete "tess validate --w") "--watch" ""
                Expect.equal (pwshComplete "tess validate m --fail-on w") [ "warning" ] ""
        ]

        testList "the command line, as completion sees it" [
            testCase "every command and subcommand, without the hidden __complete"
            <| fun () ->
                let names = root.Children |> List.map (fun c -> List.last c.Path)

                for c in [ "validate"; "bpa"; "test"; "completion"; "data"; "script" ] do
                    Expect.contains names c ""

                Expect.isFalse (List.contains "__complete" names) ""

                let keys = Node.all root |> List.map Node.key
                Expect.contains keys "bpa rules list" ""
                Expect.contains keys "test run" ""

            testCase "inherited --json and the help flags, everywhere"
            <| fun () ->
                for n in Node.all root do
                    let names = n.Options |> List.map (fun o -> o.Name)

                    for flag in [ "--json"; "-j"; "--help"; "-h" ] do
                        Expect.contains names flag (Node.key n)

            testCase "every value offered for an option is in its help text"
            <| fun () ->
                for n in Node.all root do
                    for o in n.Options do
                        match o.Value with
                        // --kind lists ObjectKind's own tokens; its help gives examples.
                        | Some(Choice values) when o.Name <> "--kind" && o.Name <> "-k" ->
                            for v in values do
                                Expect.isTrue
                                    (o.Help.Contains(v, StringComparison.OrdinalIgnoreCase))
                                    $"{Node.key n} {o.Name}: '{v}' isn't in its help: {o.Help}"
                        | _ -> ()

            testCase "positional arguments come from the field names"
            <| fun () ->
                let node (key: string) =
                    Node.all root |> List.find (fun n -> Node.key n = key)

                Expect.equal (node "get").Positionals [ Path; Objects [] ] ""
                Expect.equal (node "remove").Positionals [ Path ] ""
                Expect.equal (node "remove").Rest (Some(Objects [])) ""
                Expect.equal (node "move").Positionals [ Path; Objects [ ObjectKind.Measure ]; Tables ] ""
                Expect.equal (node "rename").Positionals [ Path; Objects []; Text ] ""
                Expect.equal (node "perspective add").Positionals [ Path; Perspectives ] ""
                Expect.equal (node "data init").Positionals [ Path; Directory ] ""
                Expect.equal (node "bpa rules show").Positionals [ Path; Text ] ""
                Expect.equal (node "query").Rest (Some Text) ""

            testCase "option values"
            <| fun () ->
                let value (key: string) (option: string) =
                    let n = Node.all root |> List.find (fun n -> Node.key n = key)
                    (Node.tryOption n option).Value.Value

                Expect.equal (value "list" "--table") (Some Tables) ""
                Expect.equal (value "list" "-t") (Some Tables) "aliases too"
                Expect.equal (value "query" "--role") (Some Roles) ""
                Expect.equal (value "query" "--data") (Some Directory) ""
                Expect.equal (value "bpa" "--rules") (Some Path) ""
                Expect.equal (value "get" "--property") (Some Text) "a property path, not a file"
                Expect.equal (value "translations export" "--format") (Some(Choice [ "csv"; "json" ])) ""
                Expect.equal (value "validate" "--watch") None "a flag"
        ]

        testList "candidates" [
            testCase "commands, then subcommands"
            <| fun () ->
                Expect.equal (candidates [] "va") [ "validate" ] ""

                Expect.equal
                    (candidates [ "bpa"; "rules" ] "")
                    [ "list"; "show"; "add"; "remove"; "ignore"; "unignore" ]
                    ""

            testCase "options, after options and positionals"
            <| fun () ->
                Expect.equal (candidates [ "validate"; "m"; "--no-group" ] "--fa") [ "--fail-on" ] ""
                Expect.equal (candidates [ "--json"; "validate" ] "--w") [ "--watch" ] ""

            testCase "an option's value; the word after it is not a positional argument"
            <| fun () ->
                Expect.equal (candidates [ "validate"; "m"; "--fail-on" ] "") [ "error"; "warning"; "info" ] ""
                Expect.equal (candidates [ "list"; "m"; "--kind"; "measure"; "--table" ] "S") [ "Sales" ] ""

            testCase "objects of the model, as references"
            <| fun () ->
                Expect.equal
                    (candidates [ "get"; "m" ] "measure:")
                    [ "measure:Sales/Total"; "measure:Sales/Total (hidden)" ]
                    ""

                Expect.equal
                    (candidates [ "remove"; "m"; "table:Sales" ] "table:D")
                    [ "table:Date" ]
                    "any number of them"

                Expect.equal (candidates [ "get"; "m" ] "MEASURE:sales/t").Length 2 "case-insensitive"

            testCase "only the kinds a position takes"
            <| fun () ->
                Expect.equal
                    (candidates [ "move"; "m" ] "")
                    [ "measure:Sales/Total"; "measure:Sales/Total (hidden)" ]
                    ""

                Expect.equal
                    (candidates [ "move"; "m"; "measure:Sales/Total" ] "")
                    [ "Sales"; "Date"; "Time Intelligence" ]
                    ""

                let refresh = candidates [ "refresh"; "m" ] ""

                Expect.isTrue
                    (refresh
                     |> List.forall (fun c -> c.StartsWith "table:" || c.StartsWith "partition:"))
                    $"{refresh}"

            testCase "roles and perspectives by name"
            <| fun () ->
                Expect.equal (candidates [ "query"; "m"; "--role" ] "") [ "Readers" ] ""
                Expect.equal (candidates [ "perspective"; "add"; "m" ] "") [ "Sales View" ] ""

            testCase "nothing for a path or free text (the shell completes paths)"
            <| fun () ->
                Expect.equal (candidates [ "get" ] "x") [] ""
                Expect.equal (candidates [ "rename"; "m"; "measure:Sales/Total" ] "") [] ""

            testCase "words as bash passes them: quotes and escapes removed, ~ expanded"
            <| fun () ->
                Expect.equal (unquote None "'a b'") "a b" ""
                Expect.equal (unquote None "a\\ b") "a b" ""
                Expect.equal (unquote None "\"a \\\"b\\\" $c\"") "a \"b\" $c" ""
                Expect.equal (unquote None "'unfinished") "unfinished" ""
                Expect.equal (unquote None "it\\'s") "it's" ""
                Expect.equal (unquote (Some "/home/u") "~/m.bim") "/home/u/m.bim" ""
                Expect.equal (unquote (Some "/home/u") "'~/m.bim'") "~/m.bim" "quoted: no expansion"
        ]

        testList "__complete" [
            testCase "a model's objects"
            <| fun () ->
                use t = new TempDir()
                let bim = writeBim t
                let r = complete [ "--current=measure:"; "--"; "get"; bim ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "measure:Sales/Total\nmeasure:Sales/Total (hidden)\n" ""
                Expect.equal r.Err "" ""

            testCase "words as typed in bash (--quoted)"
            <| fun () ->
                use t = new TempDir()
                let bim = writeBim t

                let r =
                    complete [ "--quoted"; "--current='measure:Sales/Total (h"; "--"; "get"; $"'{bim}'" ]

                Expect.equal r.Out "measure:Sales/Total (hidden)\n" ""

            testCase "options and help flags among the words are words, not options of __complete"
            <| fun () ->
                let r = complete [ "--current=--w"; "--"; "--json"; "validate"; "--help" ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "--watch\n" ""

            testCase "no words: the commands"
            <| fun () ->
                let r = complete []
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.stringContains r.Out "validate\n" ""
                Expect.stringContains r.Out "completion\n" ""

            testCase "a server model: nothing, and no connection"
            <| fun () ->
                // The harness fails the test if a connection is attempted.
                let r = complete [ "--current="; "--"; "query"; "xmla:localhost;Sales"; "--role" ]
                Expect.equal r.Code ExitCode.ok r.Err
                Expect.equal r.Out "" ""

            testCase "a missing or broken model: nothing, and silent"
            <| fun () ->
                use t = new TempDir()
                let broken = t.Combine "broken.bim"
                File.WriteAllText(broken, "{ not json")

                for model in [ t.Combine "nope.bim"; broken; t.Path ] do
                    let r = complete [ "--current="; "--"; "list"; model; "--table" ]
                    Expect.equal r.Code ExitCode.ok model
                    Expect.equal r.Out "" model
                    Expect.equal r.Err "" model

            testCase "a TMDL folder and a .pbip project"
            <| fun () ->
                use p = project ()

                let r =
                    complete [ "--current="; "--"; "list"; Path.Combine(p.Path, "Sales.pbip"); "--table" ]

                Expect.equal r.Out "Sales\nDate\nTime Intelligence\n" ""
        ]

        testList "the sample model (TESSERA_SAMPLE_MODEL)" [
            match samplePath with
            | None ->
                testCase "not configured"
                <| fun () -> skiptest "TESSERA_SAMPLE_MODEL is not set"
            | Some sample ->
                testCase "tables, measures with spaces and symbols, and quoted words"
                <| fun () ->
                    let tables = complete [ "--current=B"; "--"; "list"; sample; "--table" ]
                    Expect.equal tables.Out "Brand\nBudget Rate\n" ""

                    let measures =
                        complete [
                            "--quoted"
                            "--current=measure:Invoiced\\ Sales/Gross\\ P"
                            "--"
                            "get"
                            $"'{sample}'"
                        ]

                    Expect.stringContains measures.Out "measure:Invoiced Sales/Gross Profit\n" ""
                    Expect.stringContains measures.Out "measure:Invoiced Sales/Gross Profit (PY)\n" ""

                    let hash = complete [ "--current=measure:Customer/#"; "--"; "get"; sample ]
                    Expect.stringContains hash.Out "measure:Customer/# Customers\n" ""
        ]
    ]
