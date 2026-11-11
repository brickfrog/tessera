/// `--watch` for validate, lint, bpa and `test run`: what each one watches, and when it refuses.
/// App sends a command here when its arguments include --watch; the command itself runs unchanged.
module Tessera.Cli.WatchCommands

open System.IO
open Argu
open Tessera
open Tessera.Cli.Common

/// The model's scope, plus these extra paths, less these outputs.
let private scopeFor (model: string) (extra: string list) (outputs: string list) =
    Watch.modelScope model
    |> Result.map (fun scope ->
        let scope = extra |> List.fold (fun s p -> Watch.Scope.add p s) scope
        outputs |> List.fold (fun s p -> Watch.Scope.exclude p s) scope)

let validate (ctx: Ctx) (args: ParseResults<ValidateCommand.Args>) =
    Watch.run ctx (scopeFor (args.GetResult ValidateCommand.Args.Model) [] []) (fun c -> ValidateCommand.run c args)

let lint (ctx: Ctx) (args: ParseResults<LintCommand.Args>) =
    if args.Contains LintCommand.Args.Save || args.Contains LintCommand.Args.Save_To then
        UsageError "--watch doesn't combine with --save or --save-to: each run would change the files it watches"
    else
        Watch.run ctx (scopeFor (args.GetResult LintCommand.Args.Model) [] []) (fun c -> LintCommand.run c args)

let bpa (ctx: Ctx) (args: ParseResults<BpaCommand.Args>) =
    match args.TryGetResult BpaCommand.Args.Rules_Cmd, args.GetResults BpaCommand.Args.Model with
    | Some _, _ -> UsageError "--watch is for `tess bpa <model>`, not `tess bpa rules`"
    | None, [ model ] ->
        let rules =
            parseList (args.GetResults BpaCommand.Args.Rules)
            |> List.filter (fun r -> not (r.Contains "://"))

        let outputs = Option.toList (args.TryGetResult BpaCommand.Args.Trx)
        Watch.run ctx (scopeFor model rules outputs) (fun c -> BpaCommand.run c args)
    // No model, or several: the command says what's wrong.
    | None, _ -> BpaCommand.run ctx args

/// Where `test run` looks for suites when given none (as TestCommand does): `.tess-tests` beside
/// the model (inside a folder holding a .pbip), and in the current directory.
let private defaultSuites (model: string) =
    let full =
        Path.GetFullPath(model).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

    let beside =
        if
            Directory.Exists full
            && not (Seq.isEmpty (Directory.EnumerateFiles(full, "*.pbip")))
        then
            full
        else
            Path.GetDirectoryName full

    [
        if not (System.String.IsNullOrEmpty beside) then
            Path.Combine(beside, TestCommand.suiteDirectoryName)
        Path.Combine(Directory.GetCurrentDirectory(), TestCommand.suiteDirectoryName)
    ]
    |> List.distinct
    |> List.filter Directory.Exists

let testRun (ctx: Ctx) (all: ParseResults<TestCommand.Args>) (args: ParseResults<TestCommand.RunArgs>) =
    match args.GetResults TestCommand.RunArgs.Target |> List.concat with
    | model :: paths when not (XmlaAddress.isRemote model) ->
        let suites = if paths.IsEmpty then defaultSuites model else paths
        let data = Option.toList (args.TryGetResult TestCommand.RunArgs.Data)

        let outputs =
            Option.toList (args.TryGetResult TestCommand.RunArgs.Trx)
            @ Option.toList (args.TryGetResult TestCommand.RunArgs.Junit)

        Watch.run ctx (scopeFor model (suites @ data) outputs) (fun c -> TestCommand.run c all)
    | model :: _ -> Watch.run ctx (Watch.modelScope model) (fun c -> TestCommand.run c all)
    // No model: the command says what's wrong.
    | [] -> TestCommand.run ctx all
