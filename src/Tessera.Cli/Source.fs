/// Where commands get their model from: files (Tessera.Core), or, for an `xmla:` argument, a
/// server (Tessera.Xmla). This is the one place the CLI decides to open a connection for a model
/// argument, and it does so only for an argument that says so.
module Tessera.Cli.Source

open System
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Xmla
open Tessera.Cli.Common

/// What the connection layer needs from a command's context.
let access (ctx: Ctx) : Remote.Access = {
    Connector = ctx.Connector
    Environment = Credentials.processEnvironment ctx.GetEnv
    Notify = fun message -> ctx.Err.WriteLine message
    IsWindows = OperatingSystem.IsWindows()
}

/// Load a model from a path, or from a server for `xmla:<server>[;<database>]`.
let load (ctx: Ctx) (model: string) : Result<ModelSource * Database, TessError> =
    if XmlaAddress.isRemote model then
        XmlaAddress.parse model |> Result.bind (Remote.loadModel (access ctx))
    else
        ModelIO.loadPath model
