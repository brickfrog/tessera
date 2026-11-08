/// New, empty semantic models, shaped like the ones Power BI Desktop creates.
module Tessera.Scaffold

open System
open System.Globalization
open System.IO
open Microsoft.AnalysisServices.Tabular

/// The compatibility level new models get. 1702 is what current Power BI tooling writes (the
/// spaceparts-analytics sample PBIP uses it) and the first level with DAX user-defined functions;
/// Power BI Desktop raises a model's level itself when a feature needs more, but never lowers it.
let defaultCompatibilityLevel = 1702

/// The lowest level `init` accepts: sourceQueryCulture, which Desktop always writes, needs 1520.
let minCompatibilityLevel = 1520

let defaultCulture = "en-US"

/// Name used when nothing better can be derived from the destination.
let fallbackName = "Model"

type Options = {
    Name: string
    CompatibilityLevel: int
    /// A specific culture name such as "en-US" (see tryCulture).
    Culture: string
}

/// A model name from a destination path: "Sales.SemanticModel" -> "Sales", "sales.bim" -> "sales",
/// "./tmdl/Finance" -> "Finance".
let nameFor (destination: string) =
    let trimmed = Path.GetFullPath(destination).TrimEnd('/', '\\')
    let leaf = Path.GetFileName trimmed

    let strip (suffix: string) (s: string) =
        if s.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
            s.Substring(0, s.Length - suffix.Length)
        else
            s

    match leaf |> strip Pbip.semanticModelSuffix |> strip ".bim" with
    | name when String.IsNullOrWhiteSpace name -> fallbackName
    | name -> name

/// The canonical name of a specific culture ("en-us" -> "en-US"); None for unknown, neutral
/// ("en") or invariant cultures, which a model's culture can't be.
let tryCulture (name: string) =
    try
        let ci = CultureInfo.GetCultureInfo(name.Trim(), predefinedOnly = true)

        if ci.IsNeutralCulture || ci.Name = "" then
            None
        else
            Some ci.Name
    with :? CultureNotFoundException ->
        None

let defaults (destination: string) = {
    Name = nameFor destination
    CompatibilityLevel = defaultCompatibilityLevel
    Culture = defaultCulture
}

/// An empty database with the model properties Power BI Desktop sets on a new model. The database
/// itself stays unnamed, as Desktop leaves it; .bim output takes its name from the model.
let create (opts: Options) : Result<Database, string> =
    try
        let db = new Database(CompatibilityLevel = opts.CompatibilityLevel)

        let model =
            new Model(
                Name = opts.Name,
                Culture = opts.Culture,
                SourceQueryCulture = opts.Culture,
                DefaultPowerBIDataSourceVersion = PowerBIDataSourceVersion.PowerBI_V3,
                DiscourageImplicitMeasures = true
            )

        model.DataAccessOptions.LegacyRedirects <- true
        model.DataAccessOptions.ReturnErrorValuesAsNull <- true
        db.Model <- model
        Ok db
    with e ->
        Error e.Message

/// Create a new model and write it to destination. Refuses a non-empty destination unless force is set.
let init (format: ModelIO.OutputFormat) (opts: Options) (force: bool) (destination: string) =
    match create opts with
    | Error reason -> Error(SaveFailed(Path.GetFullPath destination, reason))
    | Ok db -> ModelIO.save format ModelIO.SaveOptions.defaults force db destination
