# tessera

`tess` is a cross-platform command line for Power BI and Analysis Services semantic models. It reads and writes
`.bim`, TMDL folders, and Power BI Project (PBIP) folders, and is built in F# on Microsoft's own TOM library
(`Microsoft.AnalysisServices`). It runs on Linux, macOS and Windows on .NET 10.

Status: early. `convert`, `info`, `list`, `get`, `find`, `diff`, `deps` and `bpa` work.

## Usage

```sh
tess info  ./MyReport.pbip                       # summary: compatibility level, object counts
tess list  ./MyReport.pbip -k measure -t Sales   # one object reference per line
tess list  ./model.bim --name '*Margin*' --json  # machine-readable
tess get   ./MyReport.pbip "[Total Sales]"         # one object, as TMDL
tess get   ./MyReport.pbip "'Sales'[Total]" -p expression   # just the DAX
tess get   ./MyReport.pbip Sales --json          # TOM JSON (add --children for columns etc.)
tess find  ./MyReport.pbip "Budget Rate"          # names, DAX, M, descriptions, annotations...
tess find  ./MyReport.pbip 'CALCULATE\s*\(' -E -k measure --in expression
tess diff  ./main/Sales.SemanticModel ./branch/Sales.SemanticModel   # object-level changes
tess diff  ./server-export.bim ./Sales.pbip --ignore lineageTag --json
tess deps  ./MyReport.pbip "[Gross Margin %]"     # what it uses, and what uses it
tess deps  ./MyReport.pbip "Date[Date]" --referenced-by --depth 0
tess bpa   ./MyReport.pbip                       # Best Practice Analyzer: built-in rules + the model's own
tess bpa   ./model.bim -r BPARules.json --min-severity 2 --trx bpa.trx   # TE rule files, CI output
tess convert ./model.bim ./Sales.SemanticModel   # .bim -> PBIP (TMDL + definition.pbism + .platform)
tess convert ./Sales.SemanticModel ./model.bim   # PBIP -> .bim
tess convert ./tmdl ./tmdl --force                # re-serialize in place (keeps its line endings)
```

### Accepted model paths

You can point any command at any of these:

- a `.bim` file
- a TMDL folder, or any `.tmdl` file inside one
- a `X.SemanticModel` folder, or its `definition.pbism`
- a `.pbip` file, followed through `X.Report/definition.pbir` to its local model
- a `X.Report` folder
- a project folder containing exactly one of the above

### Output formats (`convert`)

Unless you pass `--format`, the format is inferred from the destination:

| Destination | Format |
|---|---|
| `*.bim` | `bim` |
| `*.SemanticModel` | `pbip`: TMDL in `definition/`, plus `definition.pbism` and `.platform` |
| anything else | `tmdl` folder |

The defaults match Power BI Desktop's output:

- tabs for indentation
- UTF-8 without BOM
- expressions written as-is
- restricted information (e.g. passwords) stripped

Line endings default to `--eol auto`: tess matches the files already at the destination, so overwrites
don't rewrite every line in git. With nothing there, it matches the source, and falls back to LF.
Use `--eol lf|crlf`, `--indent-spaces N` or `--include-sensitive` to change these. `convert` won't write over a
non-empty destination without `--force`. With `--force`, stale `.tmdl` files are removed and unrelated files are
left alone. Re-saving a PBIP folder keeps its `.platform` `logicalId` and its `definition.pbism` settings.

### Object references

Every object has a stable address of the form `kind:segment/segment`. Within a name, `/` and `\` are escaped
with `\`.

```
model
table:Sales
column:Sales/Amount            measure:Sales/Total          hierarchy:Date/Calendar
level:Date/Calendar/Year       partition:Sales/Sales        calcitem:Time Intelligence/YTD
calendar:Date/Fiscal           relationship:<name>          role:Readers
perspective:Sales View         culture:da-DK                expression:ServerName
function:DaxLib.Format         datasource:SQL
```

### Pointing at objects

Commands that take an object accept any of these forms:

| Form | Example | Resolves to |
|---|---|---|
| reference | `measure:Sales/Total` | exactly that object |
| DAX, qualified | `'Sales'[Total]`, `Sales[Total]` | the column, measure or hierarchy with that name in that table |
| DAX, unqualified | `[Total]` | the measure with that name; failing that, a uniquely named column or hierarchy |
| bare name | `Sales`, `Readers` | the table with that name; failing that, any uniquely named object |

Names match exact-case first, then case-insensitively, since Tabular treats names case-insensitively. Inside
DAX quoting, `''` escapes a quote and `]]` escapes a bracket. If more than one object matches, tess lists the
candidates.

### Property paths

`get -p` takes paths into an object's TOM JSON:
- nested properties are joined with dots, e.g. `source.expression`
- annotations and other named items use their name, e.g. `annotations.PBI_FormatHint`
- other arrays use an index, e.g. `[0]`

A path that names a group, such as `-p source`, prints everything under it. Restricted information like
passwords is never printed.

`find --in` takes path wildcards. A wildcard matches if it matches the whole path or just the last segment, so
`--in expression` covers both `expression` and `source.expression`. `find` searches every property except
lineage tags, and prints `ref  property[:line]  text`, one hit per matching line.

### Comparing models (`diff`)

`diff` compares two models object by object, in any mix of formats, and prints:

```
~ measure:Sales/Total
    formatString: "#,0" -> "0.00"
    expression:
      - SUM(Sales[Amount])
      + SUMX(Sales, Sales[Amount])
> measure:Sales/Margin -> measure:Sales/Margin %
+ column:Sales/Discount
- role:Readers
```

How objects are matched:
- Objects are matched by reference.
- Relationships are matched by their endpoints (`Sales[DateKey] -> Date[DateKey]`), because their names are
  GUIDs in Power BI.
- A removed and an added object of the same kind that share a `lineageTag` are reported as a rename, with any
  other property changes listed under it.
- Renaming a table renames every child's reference, so children that have lineage tags show up as renames too.

What is compared:
- Properties are compared using the same paths as `get -p`. `--ignore` takes path wildcards, e.g.
  `--ignore lineageTag,annotations.PBI_*`.
- A change of compatibility level shows up on `model`.
- The order of child objects is not compared.

### Dependencies (`deps`)

`deps` reads every DAX expression in the model:
- measure expressions, plus their format string, detail rows and KPI expressions
- calculated columns and calculated tables
- calculation items and calculation group expressions
- row-level security filters
- user-defined functions

It adds structural links: sort-by columns, hierarchy levels, and relationship columns. How a reference resolves:

| Written as | Resolves to |
|---|---|
| `'Sales'[X]`, `Sales[X]` | a column named X in Sales, otherwise a measure |
| `[X]` | a column in the expression's own table, then a measure in it, then a measure anywhere |
| `Sales`, `'Sales'` | the table (or calendar) |
| `Name(...)` | a user-defined function, if one has that name (built-in functions aren't objects) |

Some names belong to the expression itself and are not model references:
- `VAR` names and function parameters, which shadow tables of the same name
- columns created inside the expression, like `"Local"` in `ADDCOLUMNS(t, "Local", ...)`
- `''[X]` columns of anonymous tables

Comments and strings are never treated as references. A reference that names nothing in the model is listed as
`? <text>`. Each link shows the property and line it comes from, e.g. `expression:5`. `--depth` follows links
further (0 means all the way), and circular references are marked rather than followed.

### Best Practice Analyzer (`bpa`)

`bpa` checks a model against Best Practice Analyzer rules in Tabular Editor's JSON format. Existing TE2/TE3
rule files run unchanged as long as they only use the members listed below (see Compatibility):

```
tess bpa ./Sales.pbip                              # built-in rules, plus rules stored in the model
tess bpa ./Sales.pbip -r BPARules.json -r team.json --no-default-rules
tess bpa ./Sales.pbip --category DAX,Performance --min-severity 2
tess bpa ./Sales.pbip --list-rules                 # what would run, and whether each rule is supported
tess bpa ./Sales.pbip --json                       # findings, ignored findings, unsupported rules, summary
tess bpa ./Sales.pbip --trx bpa.trx                # VSTest results, one test per rule (Azure DevOps etc.)
```

Text output is one finding per line, most severe first, then a summary:

```
error    relationship:Sales-Product  TESS_RELATIONSHIP_DATA_TYPES  Relate columns of the same data type
warning  measure:Sales/Margin %      TESS_DIVIDE  Use DIVIDE when the divisor can be zero
info     measure:Sales/Total         TESS_MEASURE_DESCRIPTION  Describe visible measures
3 findings (1 error, 1 warning, 1 info); 25 rules checked
```

Severity 1 is info, 2 a warning and 3 (or more) an error. `bpa` exits 1 when there are findings from the rules
it ran. `--min-severity` and `--category` (case-insensitive, repeatable) choose which rules run. Rules that
can't run are listed on stderr (in JSON, under `unsupported`). They don't change the exit code.

Which rules run, lowest precedence first (a later rule replaces an earlier one with the same ID):
1. tessera's built-in rules (unless `--no-default-rules`)
2. `--rules` files, in order
3. files listed in the model's `BestPracticeAnalyzer_ExternalRuleFiles` annotation (a JSON array of paths,
   relative to the `.bim` file's folder or the TMDL folder; URLs are not downloaded)
4. rules stored in the model's `BestPracticeAnalyzer` annotation

`--no-model-rules` skips 3 and 4, like TE2's `-AX`.

#### Rule format

A rule file is a JSON array of rules:

```json
[
  {
    "ID": "MY_RULE",
    "Name": "Hide foreign keys",
    "Category": "Model layout",
    "Description": "Shown in the docs and tooltips.",
    "Severity": 2,
    "Scope": "DataColumn, CalculatedColumn",
    "Expression": "IsVisible and UsedInRelationships.Any(FromColumn == outerIt)",
    "CompatibilityLevel": 1200
  }
]
```

Only `ID`, `Scope` and `Expression` are required. As in TE, property names are case-insensitive, and comments,
trailing commas and extra fields such as `Source` are accepted. `Name` defaults to the ID, and `Severity`
defaults to 1. `LearnMoreUrl` is read. `FixExpression` is read but not applied; applying fixes is future work. A rule whose
`CompatibilityLevel` is above the model's is skipped.

`Scope` is a comma-separated list. It can also be TE's flags number.

| Scope | Objects |
|---|---|
| `Model` | the model |
| `Table` | tables that are neither calculated tables nor calculation groups |
| `CalculatedTable`, `CalculationGroup` | calculated tables; calculation group tables |
| `DataColumn`, `CalculatedColumn`, `CalculatedTableColumn` | columns by kind (`Column` means all three) |
| `Measure`, `KPI` | measures; measures' KPIs |
| `Hierarchy`, `Level`, `Partition`, `Calendar` | per table |
| `Relationship` | single-column relationships |
| `Perspective`, `Culture`, `NamedExpression`, `UserDefinedFunction` (or `Function`) | model-level objects |
| `ModelRole`, `TablePermission`, `ModelRoleMember` | roles, their row filters, their members |
| `ProviderDataSource` (or `DataSource`), `StructuredDataSource` | legacy and Power Query data sources |
| `Variation`, `CalculationItem` | column variations; calculation items |

Plural forms and spaces are accepted (`Calculated Columns`, `Hierarchies`). A rule naming a scope tess doesn't
know is reported as unsupported.

#### Expressions

`Expression` is a Dynamic LINQ predicate, evaluated once for each object in scope. tess evaluates it with
[System.Linq.Dynamic.Core](https://github.com/zzzprojects/System.Linq.Dynamic.Core) (Apache-2.0). The objects
it runs on mirror Tabular Editor's TOMWrapper, so rules use the same names. Examples:

- every object: `Name`, `Description`, `ObjectType`, `ObjectTypeName`, `Model`, `HasAnnotation(name)`,
  `GetAnnotation(name)`, `GetAnnotations()`, `HasExtendedProperty(name)`, `GetExtendedProperty(name)`,
  `TranslatedNames[culture]`, `TranslatedDescriptions[culture]`, `DependsOn`, `ReferencedBy`, `Tokenize()`
- tables, columns, measures, hierarchies: `IsHidden`, `IsVisible` (a column in a hidden table isn't
  visible), `DisplayFolder`, `TranslatedDisplayFolders`, `InPerspective[perspective]`, `DaxObjectName`,
  `DaxObjectFullName`
- columns: `Table`, `DataType`, `FormatString`, `SummarizeBy`, `SortByColumn`, `IsKey`, `IsAvailableInMDX`,
  `DataCategory`, `SourceColumn`, `Expression`, `Type`, `UsedInRelationships`, `UsedInHierarchies`,
  `UsedInLevels`, `UsedInSortBy`, `UsedInVariations`, `ObjectLevelSecurity[role]`, ...
- measures: `Expression`, `FormatString`, `FormatStringExpression`, `DetailRowsExpression`, `KPI`, ...
- tables: `Columns`, `Measures`, `Hierarchies`, `Partitions`, `RowLevelSecurity[role]`, `SourceExpression`, ...
- the model: `Tables`, `AllColumns`, `AllMeasures`, `AllHierarchies`, `AllLevels`, `AllPartitions`,
  `AllCalculationItems`, `Relationships`, `Perspectives`, `Cultures`, `Roles`, `Expressions`, `DataSources`,
  `Functions`, `Database.CompatibilityLevel`, ...
- relationships: `FromTable`, `FromColumn`, `ToTable`, `ToColumn`, `FromCardinality`, `ToCardinality`,
  `CrossFilteringBehavior`, `IsActive`, ...

Enum properties compare with strings or enum members (`DataType = "Double"`, `DataType == DataType.Double`).
Collections support `Count`, `[0]`, `["name"]` and LINQ (`Any`, `All`, `Count(...)`, `Where`, ...).

TE's own additions to the expression language also work:
- `outerIt` is the enclosing object, and `current` is the object being checked.
- Type names are case-insensitive (`RegEx.IsMatch`).
- `""` escapes a quote inside a string, and backslashes are literal (`"\s*"`).
- In `Tokenize()` rules, TE's DAX token names (`DIV`, `INTEGER_LITERAL`, `IFERROR`, ...) stand for their codes.

Member access is null-safe, as in TE: a property of a null object is the property type's default, and a null
string behaves as `""`.

`DependsOn` and `ReferencedBy` come from the same analysis as `deps`, with two differences:
- They only hold DAX references. Sort-by, hierarchy and relationship links have their own `UsedIn...`
  properties.
- In DAX a table owns (a calculated table's expression, detail rows, calculation group selection
  expressions), an unqualified `[X]` never refers to the table's own columns, as in TE.

`ReferencedBy` has typed views (`Measures`, `Columns`, `Tables`, `Roles`, and `AllMeasures` and friends for
indirect references), plus `Deep()` and `AnyVisible`. `DependsOn` is a dictionary from object to references.
Each reference has `FullyQualified`, `Property` and `Line`. A KPI's expressions belong to the KPI, and a row
filter belongs to its table permission.

If an expression doesn't compile for one of its scopes, the rule is reported as unsupported, with the parser's
error. A runtime error fails just that rule. Neither stops the run.

#### Ignoring findings

As in TE, a `BestPracticeAnalyzer_IgnoreRules` annotation holds `{"RuleIDs": ["RULE_ID", ...]}`:
- On an object, it suppresses that object's findings for those rules. They are counted as ignored, and listed
  under `ignored` in JSON.
- On the model, it turns those rules off entirely.

TE's older spelling, `BestPractizeAnalyzer_IgnoreRules`, is read too. TOM calculation items have no
annotations, so ignore their findings on the model.

#### Built-in rules

tessera ships its own rule set; `tess bpa <model> --list-rules` shows it with descriptions in `--json`.

| ID | Severity | What it flags |
|---|---|---|
| `TESS_UNUSED_HIDDEN_COLUMN` | 2 | hidden columns that no DAX, relationship, hierarchy, sort-by or variation uses |
| `TESS_UNUSED_HIDDEN_MEASURE` | 1 | hidden measures no other DAX uses |
| `TESS_MEASURE_FORMAT_STRING` | 2 | visible measures without a format string (or format string expression) |
| `TESS_MEASURE_DESCRIPTION` | 1 | visible measures without a description |
| `TESS_NUMERIC_COLUMN_SUMMARIZED` | 1 | visible numeric columns that summarize by default |
| `TESS_FLOATING_POINT_COLUMN` | 2 | `Double` columns |
| `TESS_RELATIONSHIP_DATA_TYPES` | 3 | relationships between columns of different data types |
| `TESS_RELATIONSHIP_BIDIRECTIONAL` | 2 | relationships that filter both ways |
| `TESS_RELATIONSHIP_MANY_TO_MANY` | 2 | many-to-many relationships |
| `TESS_RELATIONSHIP_INTEGER_KEYS` | 1 | relationships whose key isn't a whole number |
| `TESS_HIDE_FOREIGN_KEYS` | 2 | visible columns on the many side of a relationship |
| `TESS_TEXT_MONTH_SORT` | 2 | text month columns without a sort-by column |
| `TESS_MARK_DATE_TABLE` | 2 | models with date columns but no table marked as a date table |
| `TESS_AUTO_DATE_TABLE` | 2 | Power BI auto date/time tables |
| `TESS_IFERROR` | 2 | `IFERROR` in DAX |
| `TESS_DIVIDE` | 2 | `/` by anything but a constant (use `DIVIDE`) |
| `TESS_QUALIFY_COLUMNS` | 2 | column references without their table |
| `TESS_UNQUALIFIED_MEASURES` | 2 | measure references with a table |
| `TESS_EVALUATEANDLOG` | 3 | `EVALUATEANDLOG` left in DAX |
| `TESS_CALCULATED_COLUMN_AS_MEASURE` | 1 | numeric calculated columns that are only ever aggregated |
| `TESS_EMPTY_EXPRESSION` | 3 | measures, calculated columns/tables and calculation items with no expression |
| `TESS_EMPTY_CALCULATION_GROUP` | 2 | calculation groups without items |
| `TESS_NAME_WHITESPACE` | 2 | names with leading or trailing spaces |
| `TESS_TODO_COMMENT` | 1 | `TODO` in DAX comments |
| `TESS_EMPTY_PERSPECTIVE` | 1 | perspectives with no tables |

#### Compatibility

All 145 rules in the community files `BPARules-standard.json`, `BPARules-standard-lax.json` and
`BPARules-PowerBI.json` (TabularEditor/BestPracticeRules) and Microsoft's `BPARules.json`
(microsoft/Analysis-Services) compile and run. On the SpaceParts sample model, and on synthetic models covering
roles, translations, perspectives, KPIs and calculation groups, they find the same objects as TE2's own
analyzer. Known differences:
- Rules that need data from a deployed model (e.g. a measure's `DataType`, or `Vertipaq_*` annotations that
  VertiPaq Analyzer writes) behave as they do in TE on a model loaded from files.
- Object names in the output use tess references, e.g. `table:Sales`.
- Only read-only members are available, so rules can't call TE's methods that change the model.

### JSON and exit codes

`--json` (or `-j`) writes one JSON document to stdout, errors included (`{"error": {"code", "message"}}`).

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | the "negative" result of the Unix tool a command mirrors: `find` found nothing (like grep); `diff` found differences (like diff); `bpa`/`validate` found issues |
| 2 | usage error |
| 3 | input error (model not found, unrecognised, ambiguous, failed to load) |
| 4 | output error (destination not empty, write failed) |
| 70 | internal error |

## Development

```sh
dotnet build
dotnet test                                         # Expecto + FsCheck
dotnet run --project tests/Tessera.Tests -- --summary   # same tests, Expecto's own runner
dotnet run --project src/Tessera.Cli -- info <path>
dotnet tool restore && dotnet fantomas src tests    # format
dotnet pack src/Tessera.Cli -c Release -o artifacts && dotnet tool install -g tessera --add-source ./artifacts
```

Set `TESSERA_BPA_RULES=<file>[:<file>...]` to run BPA rule files (e.g. from TabularEditor/BestPracticeRules)
in a test that reports how many rules are supported. It runs against `TESSERA_SAMPLE_MODEL` if that is set,
otherwise against a fixture.

Set `TESSERA_SAMPLE_MODEL=<path>` to also run a round-trip test against a real model, such as a clone of
[TabularEditor/spaceparts-analytics](https://github.com/TabularEditor/spaceparts-analytics).

Layout:

- `src/Tessera.Core` holds the model logic and never prints anything. Commands return typed results and
  `TessError` values.
- `src/Tessera.Cli` handles argument parsing (Argu) and text/JSON rendering:
  - `Output.fs` holds the JSON helpers.
  - `Common.fs` holds exit codes, `Outcome`, `Ctx` and argument helpers.
  - `Commands/<Name>Command.fs` holds one command each: its `summary`, `Args`, renderers and `run`.

To add a command:
1. Write the logic in Core, with tests.
2. Add `src/Tessera.Cli/Commands/<Name>Command.fs`, following an existing command.
3. Add it to `Tessera.Cli.fsproj`, before `Args.fs`.
4. Add a case to `CliArgs` in `Args.fs`, plus its usage line (`<Name>Command.summary`).
5. Add a dispatch line in `App.fs`.
6. Put its CLI tests in `tests/Tessera.Tests/<Name>CommandTests.fs`, using `CliHarness` (`tess`, `json`,
   `project`).

## Prior art

- [Tabular Editor 2](https://github.com/TabularEditor/TabularEditor) (MIT) is the inspiration and the reference
  for TOM behaviour; for example, the Power BI compatibility-mode heuristic follows its
  `PowerBiCompatibilityModeHelper`. `bpa` follows its Best Practice Analyzer: the rule format, scopes, ignore
  annotations and the TOMWrapper names that rules use. Its DAX token codes are ported from TE2's lexer.
- tessera is independent of Tabular Editor ApS and its commercial Tabular Editor 3 and `te` CLI.

## License

MIT
