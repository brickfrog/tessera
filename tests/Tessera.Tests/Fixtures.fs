module Tessera.Tests.Fixtures

open System
open System.IO
open Microsoft.AnalysisServices.Tabular

/// A temp folder that deletes itself.
type TempDir() =
    let path =
        Path.Combine(Path.GetTempPath(), "tessera-tests", Guid.NewGuid().ToString("N"))

    do Directory.CreateDirectory path |> ignore

    member _.Path = path

    member _.Combine([<ParamArray>] parts: string array) =
        Path.Combine(Array.append [| path |] parts)

    interface IDisposable with
        member _.Dispose() =
            try
                Directory.Delete(path, true)
            with _ ->
                ()

let write (path: string) (text: string) =
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

/// A small model exercising most object kinds tessera addresses.
let sampleDatabase () =
    let db = new Database(Name = "Sample", ID = "Sample", CompatibilityLevel = 1702)

    let m =
        new Model(Name = "Model", Culture = "en-US", DiscourageImplicitMeasures = true)

    db.Model <- m

    let sales =
        new Table(Name = "Sales", Description = "  Fact table.\n\n   Indented second paragraph.  ")

    sales.Columns.Add(new DataColumn(Name = "Amount", DataType = DataType.Decimal, SourceColumn = "Amount"))

    sales.Columns.Add(
        new DataColumn(Name = "DateKey", DataType = DataType.Int64, SourceColumn = "DateKey", IsHidden = true)
    )

    sales.Columns.Add(new DataColumn(Name = "Path/With\\Slashes", DataType = DataType.String, SourceColumn = "P"))
    sales.Measures.Add(new Measure(Name = "Total", Expression = "SUM(Sales[Amount])", FormatString = "#,0"))
    sales.Measures.Add(new Measure(Name = "Total (hidden)", Expression = "[Total]", IsHidden = true))
    sales.Partitions.Add(new Partition(Name = "Sales", Source = new MPartitionSource(Expression = "let x = 1 in x")))
    m.Tables.Add sales

    let date = new Table(Name = "Date")

    let dateKey =
        new DataColumn(Name = "DateKey", DataType = DataType.Int64, SourceColumn = "DateKey")

    let year =
        new DataColumn(Name = "Year", DataType = DataType.Int64, SourceColumn = "Year")

    let month =
        new DataColumn(Name = "Month", DataType = DataType.String, SourceColumn = "Month")

    date.Columns.Add dateKey
    date.Columns.Add year
    date.Columns.Add month
    let h = new Hierarchy(Name = "Calendar")
    h.Levels.Add(new Level(Name = "Year", Ordinal = 0, Column = year))
    h.Levels.Add(new Level(Name = "Month", Ordinal = 1, Column = month))
    date.Hierarchies.Add h
    date.Partitions.Add(new Partition(Name = "Date", Source = new MPartitionSource(Expression = "let x = 1 in x")))
    m.Tables.Add date

    let ti =
        new Table(Name = "Time Intelligence", CalculationGroup = new CalculationGroup())

    ti.Columns.Add(new DataColumn(Name = "Period", DataType = DataType.String, SourceColumn = "Name"))

    ti.CalculationGroup.CalculationItems.Add(
        new CalculationItem(Name = "Current", Expression = "SELECTEDMEASURE()", Ordinal = 0)
    )

    ti.CalculationGroup.CalculationItems.Add(
        new CalculationItem(Name = "YTD", Expression = "TOTALYTD(SELECTEDMEASURE(), 'Date'[DateKey])", Ordinal = 1)
    )

    ti.Partitions.Add(new Partition(Name = "Time Intelligence", Source = new CalculationGroupSource()))
    m.Tables.Add ti

    m.Relationships.Add(
        new SingleColumnRelationship(Name = "Sales-Date", FromColumn = sales.Columns.["DateKey"], ToColumn = dateKey)
    )

    m.Roles.Add(new ModelRole(Name = "Readers", ModelPermission = ModelPermission.Read))
    m.Perspectives.Add(new Perspective(Name = "Sales View"))
    m.Cultures.Add(new Culture(Name = "da-DK"))

    m.Expressions.Add(
        new NamedExpression(
            Name = "ServerName",
            Kind = ExpressionKind.M,
            Expression = "\"localhost\" meta [IsParameterQuery=true, Type=\"Text\", IsParameterQueryRequired=true]"
        )
    )

    m.Functions.Add(new Function(Name = "Double", Expression = "(x: INT64) => x * 2"))
    db

/// PBIP file shapes as Power BI Desktop writes them (see TabularEditor/spaceparts-analytics).
let pbipJson (reportFolder: string) =
    $$"""{
  "$schema": "https://developer.microsoft.com/json-schemas/fabric/pbip/pbipProperties/1.0.0/schema.json",
  "version": "1.0",
  "artifacts": [ { "report": { "path": "{{reportFolder}}" } } ],
  "settings": { "enableAutoRecovery": true }
}"""

let pbirByPath (relativeModelPath: string) =
    $$"""{
  "$schema": "https://developer.microsoft.com/json-schemas/fabric/item/report/definitionProperties/2.0.0/schema.json",
  "version": "4.0",
  "datasetReference": { "byPath": { "path": "{{relativeModelPath}}" } }
}"""

let pbirByConnection =
    """{
  "version": "4.0",
  "datasetReference": { "byConnection": { "connectionString": "Data Source=powerbi://api.powerbi.com/v1.0/myorg/WS" } }
}"""

let pbism (version: string) =
    $$"""{ "version": "{{version}}", "settings": { "qnaEnabled": true } }"""

/// Write a PBIP project for the sample model: root/Name.pbip, Name.Report, Name.SemanticModel (TMDL).
let writePbipProject (root: string) (name: string) =
    let sm = Path.Combine(root, name + ".SemanticModel")
    let report = Path.Combine(root, name + ".Report")
    TmdlSerializer.SerializeDatabaseToFolder(sampleDatabase (), Path.Combine(sm, "definition"))
    write (Path.Combine(sm, "definition.pbism")) (pbism "4.2")
    write (Path.Combine(report, "definition.pbir")) (pbirByPath $"../{name}.SemanticModel")
    write (Path.Combine(root, name + ".pbip")) (pbipJson $"{name}.Report")
    sm
