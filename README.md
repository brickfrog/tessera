# tessera

[![CI](https://github.com/brickfrog/tessera/actions/workflows/ci.yml/badge.svg)](https://github.com/brickfrog/tessera/actions/workflows/ci.yml)

`tess` is a cross-platform command line for Power BI and Analysis Services semantic models. It reads and writes
`.bim`, TMDL folders, and Power BI Project (PBIP) folders, and is built in F# on Microsoft's own TOM library
(`Microsoft.AnalysisServices`). It runs on Linux, macOS and Windows on .NET 10.

Status: early. `init`, `convert`, `info`, `list`, `get`, `find`, `diff`, `deps`, `validate`, `rename`, `set`,
`add`, `remove`, `move`, `bpa`, `lint`, `script`, `translations`, `perspective` and `format` work.

## Install

Release binaries are self-contained single files and need no .NET install. Download the archive for your platform
from the [releases page](https://github.com/brickfrog/tessera/releases) (`linux-x64`, `linux-arm64`, `win-x64`,
`osx-arm64`, `osx-x64`), unpack it, and put `tess` (`tess.exe` on Windows) on your `PATH`:

```sh
tar -xzf tess-<version>-linux-x64.tar.gz && install tess-<version>-linux-x64/tess ~/.local/bin/
```

The Linux binaries are built for glibc-based distributions and need ICU (`libicu`, e.g. `apt install libicu74`),
because tess uses real culture data rather than invariant globalization. There is no musl/Alpine build yet.
The macOS binaries are not notarized. If Gatekeeper blocks one, run `xattr -d com.apple.quarantine tess`.
Each release also lists `SHA256SUMS`.

With the .NET 10 SDK, you can install it as a .NET tool instead. Download `tessera.<version>.nupkg` from the release, then:

```sh
dotnet tool install -g tessera --add-source ./folder-with-the-nupkg
```

## Usage

```sh
tess init  ./Sales.SemanticModel                # new, empty model (PBIP; .bim / TMDL by path)
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
tess deps  ./MyReport.pbip --unused               # hidden objects nothing uses
tess validate ./MyReport.pbip                     # broken references, cycles, ... (exit 1 on errors)
tess validate ./model.bim --fail-on warning --rule unresolved-reference --json
tess rename ./MyReport.pbip "[Revenue]" "Net Revenue"          # preview: the rename + every DAX reference it fixes
tess rename ./MyReport.pbip "[Revenue]" "Net Revenue" --save   # ... and write it back
tess set   ./MyReport.pbip "[Revenue]" -p formatString='#,0.00' -p isHidden=true --save   # set properties
tess get   ./MyReport.pbip "[Revenue]" --properties   # what `set` can write there, with types and values
tess add   ./MyReport.pbip measure:Sales/Margin -e "[Revenue] - [Cost]" --save   # create an object
tess add   ./MyReport.pbip relationship --from "'Sales'[DateKey]" --to "'Date'[DateKey]" --save
tess remove ./MyReport.pbip "Sales[Old Key]" "[Unused]" --save  # refused while other DAX uses them
tess move   ./MyReport.pbip "[Revenue]" "Measures" --save       # rewrites 'Sales'[Revenue] references
tess bpa   ./MyReport.pbip                       # Best Practice Analyzer: built-in rules + the model's own
tess bpa   ./model.bim -r BPARules.json --min-severity 2 --trx bpa.trx   # TE rule files, CI output
tess bpa rules list ./MyReport.pbip              # the rules in effect, and where each comes from
tess bpa rules ignore ./MyReport.pbip TESS_DIVIDE "[Ratio]" --save   # suppress one finding (TE's annotation)
tess script export ./MyReport.pbip -t Sales -o sales.dax   # DAX objects as a DAX script (TE3's format)
tess script apply  ./MyReport.pbip sales.dax --save        # ... edited, and applied back
tess convert ./model.bim ./Sales.SemanticModel   # .bim -> PBIP (TMDL + definition.pbism + .platform)
tess convert ./Sales.SemanticModel ./model.bim   # PBIP -> .bim
tess convert ./tmdl ./tmdl --force                # re-serialize in place (keeps its line endings)
tess translations export ./MyReport.pbip --culture da-DK -o da-DK.csv   # a file for the translator
tess translations import ./MyReport.pbip da-DK.csv --save              # ... and the filled-in file back
tess perspective add ./MyReport.pbip Finance Sales "[Budget]" --save   # perspective membership
tess perspective list ./MyReport.pbip Finance
tess lint  ./MyReport.pbip                       # DAX code actions: unused VARs, unqualified columns, ...
tess lint  ./MyReport.pbip --fix --save          # ... and rewrite what can be fixed safely
tess format --expr "calculate(sum(Sales[Amount]),all('Date'))"   # format DAX, SQLBI style, offline
tess format ./MyReport.pbip --save               # ... every DAX expression in a model (--check for CI)
tess connect "powerbi://api.powerbi.com/v1.0/myorg/Finance"       # check a server connection, list its models
tess info  "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales"   # read-only commands on a deployed model
tess query "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" "EVALUATE 'Date'" --json
tess deploy ./MyReport.pbip "powerbi://api.powerbi.com/v1.0/myorg/Finance" Sales   # dry run; --execute to deploy
tess refresh "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" Orders --type full   # dry run; --execute to refresh
tess data init ./MyReport.pbip ./fixtures --rows 100   # fixture data for the local DAX engine: CSV per table
tess data check ./MyReport.pbip ./fixtures       # ... checked against the model's columns, types and relationships
tess query ./MyReport.pbip --data ./fixtures "EVALUATE SUMMARIZECOLUMNS('Date'[Year], \"Sales\", [Total])"   # offline
tess query ./MyReport.pbip --coverage            # which measures the local engine can evaluate, and what the rest need
tess test init ./MyReport.pbip --data ./fixtures  # a test suite to fill in: one test per measure
tess test run ./MyReport.pbip --ci github         # run .tess-tests/*.test.yaml offline, over the fixture data
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

### New models (`init`)

`init` writes an empty model. It picks the format from the path the way `convert` does, and writes the same
support files. The model is named after the folder or file, without `.SemanticModel` or `.bim`.

```sh
tess init ./Sales.SemanticModel                                 # PBIP: definition/ + definition.pbism + .platform
tess init ./finance.bim --name Finance --culture da-DK
tess init ./model --compatibility-level 1601 --force            # TMDL folder; replace what's there
```

The model gets the properties Power BI Desktop puts on a new model:
- `culture` and `sourceQueryCulture` (`--culture`, default `en-US`)
- `defaultPowerBIDataSourceVersion: powerBI_V3`
- `dataAccessOptions` with `legacyRedirects` and `returnErrorValuesAsNull`

It also sets `discourageImplicitMeasures`, because models built in code should use explicit measures, and
calculation groups require it anyway. The database is left unnamed, as Desktop leaves it. Desktop's UI-state
annotations, such as `PBI_QueryOrder`, are not written.

The default compatibility level is 1702:
- current Power BI tooling writes it (the spaceparts-analytics sample uses it)
- it is the first level with DAX user-defined functions
- Power BI Desktop raises a model's level when a feature needs a higher one, but never lowers it

Pass `--compatibility-level` for older engines. The minimum is 1520, the lowest level that supports
`sourceQueryCulture`. `init` won't write over a non-empty destination without `--force`.

To open the model in Power BI Desktop, you also need a `.pbip` file and a `X.Report` folder that points at it.

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

An object's own sub-objects are part of it: `kpi.targetExpression`, `formatStringDefinition.expression`,
`detailRowsDefinition.expression`, `calculationGroup.precedence`, `alternateOf.summarization` and
`extendedProperties.<name>` are properties of the measure, table or column, in `get -p`, `find` and `diff` alike.
Child objects that have their own reference (columns, measures, calculation items, ...) and a role's members are
not.

`get --properties` lists the properties [`set`](#setting-properties-set) can write on an object: the path, its
type, its current value, and the allowed values of enums. `--all` adds read-only properties, with the reason.

```
$ tess get ./Sales.pbip "Sales[Amount]" --properties
alternateOf                     object
alternateOf.baseColumn          reference                -> column
annotations.<name>              string
annotations.SummarizationSetBy  string     Automatic
dataType                        enum       decimal       string|int64|double|dateTime|decimal|boolean|binary|unknown|variant
isHidden                        bool       false
sortByColumn                    reference                -> column
summarizeBy                     enum       sum           default|none|sum|min|max|count|average|distinctCount
...
```

`--json` writes an array with one entry per property, sorted by path:

```json
[ { "property": "summarizeBy", "type": "enum", "writable": true, "value": "sum",
    "allowed": [ "default", "none", "sum", "min", "max", "count", "average", "distinctCount" ] },
  { "property": "sortByColumn", "type": "reference", "writable": true, "value": null, "refersTo": "column" },
  { "property": "name", "type": "string", "writable": false, "value": "Amount",
    "reason": "use `tess rename`, which also updates the DAX that refers to it" } ]
```

Each entry has:
- `type`: `string`, `expression` (DAX, M or a query), `bool`, `int`, `number`, `enum`, `reference` or
  `object` (a sub-object).
- `value`: a JSON string, boolean or number, a reference such as `"column:Date/MonthNo"`, `{}` for a sub-object
  that exists, or `null` when unset.
- optional keys: `allowed` (enums only), `refersTo` (references only: an object kind) and `reason` (read-only
  properties only).

`annotations.<name>` and `extendedProperties.<name>` stand for new items of any name. Existing ones are listed
by name. Text output shows the first line of long values; `--json` has them in full.

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
- When a table is renamed, its columns, measures, levels and partitions follow it, even without lineage tags.
  So does a relationship into it: same relationship, new endpoints, so it shows as modified.

`--stat` prints one line per changed object, and `--name-only` just their references. `--json` writes the
change set:

```json
{ "command": "diff", "summary": { "created": 0, "deleted": 0, "modified": 1, "moved": 1, "total": 2 },
  "changes": [ { "objectPath": "measure:Sales/Net Revenue", "objectType": "Measure", "changeKind": "moved",
                 "movedFromObjectPath": "measure:Sales/Revenue", "label": "...", "properties": [] },
               { "objectPath": "measure:Sales/Margin", "objectType": "Measure", "changeKind": "modified",
                 "label": "measure:Sales/Margin",
                 "properties": [ { "property": "expression", "before": "...", "after": "..." } ] } ] }
```

`changeKind` is `created`, `deleted`, `modified` or `moved` (renamed). The editing commands use the same shape,
the one the Tabular Editor CLI uses.

What is compared:
- Properties are compared using the same paths as `get -p`. `--ignore` takes path wildcards, e.g.
  `--ignore lineageTag,annotations.PBI_*`.
- A change of compatibility level shows up on `model`.
- The order of child objects is not compared.
- Translations show on the object they translate, as `translations.<culture>.caption`, `.description` and
  `.displayFolder`, not on the culture. So renaming or moving a translated object doesn't show every culture as
  changed. `get -p` and `find` use the same paths.
- A perspective's membership is one `members` property, one reference per line, so it diffs line by line:

  ```
  ~ perspective:Finance
      members:
          table:Sales
        + measure:Sales/Budget
  ```

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

The argument of `NAMEOF([X])` and `TABLEOF([X])` is always a model reference, even when the expression has a
string `"X"` (as a field parameter's label often is).

Comments and strings are never treated as references. A reference that names nothing in the model is listed as
`? <text>`. Each link shows the property and line it comes from, e.g. `expression:5`. `--depth` follows links
further (0 means all the way), and circular references are marked rather than followed.

#### Unused objects (`deps --unused`)

`tess deps <model> --unused` lists the tables, columns, measures, hierarchies and functions that nothing uses.
Reports can use any visible object, and tess can't see reports, so by default only hidden objects are listed:
hidden and unused means dead. An object counts as hidden if it, or its table, is hidden or private.
`--include-visible` checks visible objects too: what nothing *in the model* uses.

```
$ tess deps ./Sales.pbip --unused
measure:Sales/Old Margin
column:Sales/Legacy Key  used only by unused measure:Sales/Old Margin
```

Usage is transitive: something used only by unused objects is unused too, and is listed with them (`used only
by unused ...`), so removing those first frees it. An object is used if any of these use it:
- the DAX of a used object (as `deps` reads it), including a used measure's format string, detail rows and KPI
- a used column's sort-by column, variations (their hierarchy and column) and group-by columns (field parameters)
- a used hierarchy's levels
- a calculation item of a used calculation group
- a relationship, row-level security filter, calendar or aggregation column (`alternateOf`): these always
  count as used
- its own parts: a used column, measure or hierarchy keeps its table, and a used table keeps its key column

Some rules of thumb:
- A table reference, such as `COUNTROWS(Sales)`, uses the table but not its columns.
- Perspectives and translations don't count: they list objects, they don't use them.
- Calculated-table columns are listed with a note, since removing one means changing the table's DAX.
- Calculation items, levels, partitions, roles, relationships, M expressions and data sources are never
  listed. tess doesn't read M, so it can't tell which M expressions are used.
- Power BI's auto date/time template table counts as used.
- When in doubt, an object counts as used. For example, a hidden table that only a relationship touches is
  never listed.

`-k` and `-t` narrow the list, as in `list`. `--json` writes an array of `{ ref, kind, name, table, hidden,
usedBy, note }`. The exit code is 0, unless `--fail-on-unused` is given and something is listed (then 1).

### Validation (`validate`)

`validate` checks for structural problems: things Analysis Services or Power BI would reject, or that can't
work. It doesn't check style. Each issue prints as `severity  ref  property:line  message  [rule]`, followed by a
summary line such as `2 errors, 1 warning`.

| Rule | Severity | Reports |
|---|---|---|
| `unresolved-reference` | error | a DAX reference to a table, `'T'[X]` or `T[X]` that doesn't exist (as `deps` resolves it) |
| | warning | an unqualified `[X]` that matches nothing, since it may be a column of a table expression |
| `circular-reference` | error | measures, calculated columns, calculated tables, calculation items or functions that depend on themselves, through their DAX expressions. Each loop is reported once, e.g. `measure:Sales/A -> measure:Sales/B -> measure:Sales/A` |
| `invalid-token` | error | an unterminated string, quoted name, `[name]`, date literal or `/*` comment, or a character DAX doesn't use, in any DAX property |
| `empty-expression` | warning | a measure, calculated column, calculated table, calculation item or function with no DAX |
| `duplicate-name` | error | a column, measure and hierarchy that share a name in one table, or measures that share a name across tables (ignoring case). TOM itself rejects duplicate tables, and duplicate columns or measures within one table, when it loads a model |
| `relationship-datatype-mismatch` | warning | a relationship between columns of different data types |
| `relationship-self-join` | error | a relationship from a table to itself |
| `invalid-sort-by` | warning | a column sorted by itself, or by a column that has its own sort-by column |
| `tom-validation` | error | TOM's own validation, e.g. a sort-by column, level or relationship that names a column that doesn't exist |

Options:
- `--rule` picks rules (repeatable or comma-separated).
- `--min-severity error|warning|info` hides less severe issues.
- `--fail-on error|warning|info` sets the exit code: 1 if any issue is this severe or more. The default is
  `error`. Issues hidden by `--min-severity` still count, so hiding warnings can't turn a failing check green.
- `--no-group` lists issues that share a root cause one by one (see below).

`--json` writes the [findings envelope](#findings-json-and-ci-annotations), with `"valid": false` when there are
errors. `--ci github|azdo` also writes CI annotations to stderr.

Issues with one root cause are grouped. In practice that means a name many expressions use but that nothing
has, such as a column renamed or removed outside tess: every `unresolved-reference` naming it is one group, and
so is every reference to a missing table or its columns. Names match ignoring case. The first issue stands for
the group, with a count and the missing name, and the summary still counts every issue:

```
error  measure:Sales/A  expression:1  'Old'[X] does not exist: there is no table 'Old'  [unresolved-reference]  +2 more for 'Old'
3 errors (2 grouped under their root cause; --no-group lists them)
```

How grouped issues are reported:
- **JSON** keeps every finding. The group's first finding gets `dependents` (the other findings' `objectPath`s,
  each listed once), and the others get `rootCause` (the first finding's `objectPath`). Findings outside a group
  have neither key. `rootCause` says which finding stands for the group: fixing that one object doesn't fix the
  others, but restoring the missing name fixes them all.
- **CI annotations** are written once per group, with the count in the message.
- **`--no-group`** lists every issue on its own, and the JSON has no grouping keys.

Exit codes don't change. Grouping applies after `--rule` and `--min-severity`.

Other rules don't cascade, so they aren't grouped. Every rule checks an object's own definition: a measure
that uses a broken measure isn't reported, and TOM's validation doesn't read DAX. To see what a broken object affects, use
`tess deps <model> <object> --referenced-by --depth 0`. TOM also doesn't say which name it couldn't find, so its
own errors (e.g. a relationship to a missing column) aren't grouped with the DAX that names that column.
### Editing commands

The editing commands (`rename`, `set`, `add`, `remove`, `move`, `script apply`, `translations import`,
`perspective add|remove`, `lint --fix`, `bpa rules add|remove|ignore|unignore` and `format` on a model) follow one
set of conventions, the same as the Tabular Editor CLI's.

**Saving:**
- **A dry run by default.** The command shows what would change and writes nothing; stderr says
  `Dry run - nothing saved. Add --save to persist.`
- **`--save`** writes the model back in place: its own format, location and line endings, with restricted
  information kept.
- **`--save-to <path>`** writes elsewhere, like `convert` (add `--force` to overwrite a non-empty destination).

**Output:**
- The change as a line diff, like `diff`.
- `--stat` prints one line per object.
- `--name-only` prints just the changed references.
- Notes and warnings go to stderr, so stdout is only the change.
- `--json` writes the change set (see `diff`) plus `dryRun`, `introducedErrors` (findings), `warnings`, `notes`,
  `saved` (the format and path written, or `null`) and `files` (report files `rename` and `move` change, see
  [Reports](#reports-pbir)).

**Safety:** a change that introduces new errors is shown, but `--save` refuses it (exit 1) unless `--force`.
The checks are the `validate` rules an edit can break: unresolved and circular references, duplicate names,
invalid tokens, and TOM's validation. Errors the model already had don't count, even on an object the edit
renamed.

### Renaming (`rename`)

`tess rename <model> <object> <new name>` renames an object and rewrites every DAX reference to it. Only the
reference tokens change; every other character of every expression stays as it was. It follows the
[editing conventions](#editing-commands): a dry run unless `--save` or `--save-to`.

What is rewritten, in all the DAX `deps` reads, resolved the same way (`VAR`s and parameters shadow tables):

| Renaming | Rewrites |
|---|---|
| a measure | `[Old]`, `'T'[Old]`, `T[Old]` that resolve to it, as `[New]` (`]]` escaped) |
| a column | qualified and unqualified references that resolve to it |
| a table (also calculated and calculation-group tables) | the table in `Old`, `'Old'`, `Old[X]`, `'Old'[X]` |
| a calendar | `Old`, `'Old'` where they resolve to it |
| a function | calls, `Old(...)` |

The arguments of `NAMEOF` and `TABLEOF` are references like any other, so field parameters
(`{ ("Revenue", NAMEOF('Sales'[Revenue]), 0) }`) follow a rename. Their labels are text and stay as they are; a
warning names a field parameter whose label still shows the old name.

Renaming a table or column also updates the `sourceColumn` of calculated-table columns that map to it, such as
`Sales[Amount]` in a table defined as `FILTER(Sales, ...)`: the table's DAX now returns `Sales[Net]`, so its column
must map to that. The column keeps its own name. A `sourceColumn` like `[Value1]` names a column the table's own DAX
makes, and stays.

A measure qualified with a table other than its own (`'Date'[Revenue]` for a measure in `Sales`) isn't rewritten:
tess, like Tabular Editor 2, resolves a qualified name only in the table it names, so that reference doesn't resolve
to the measure (`validate` reports it). A warning names each one.

Unquoted table references stay unquoted only if the new name is a plain identifier (letters, digits, `_`), isn't a
DAX keyword, and isn't already used as a name in that expression; otherwise they're written `'New'` (`''`
escaped). Function names must be identifiers (dots allowed).

Hierarchies, levels, partitions, calculation items, roles, perspectives, cultures, relationships and data sources
are renamed only: DAX can't refer to them by name (reports can name hierarchies and levels; see
[Reports](#reports-pbir)). Sort-by columns, hierarchy levels, relationships, perspectives
and translations point at objects, not names, so they follow any rename. Power BI's `PBI_QueryOrder` model
annotation, which lists queries by name, is kept in step.

Not rewritten:
- comments and string literals, even ones that look like references (`// [Old]`, `"Old"`)
- M: named expressions (Power Query parameters and queries) are renamed, with a warning listing the M that
  mentions the old name; the same warning appears for tables and data sources whose name appears in M
- reports, except the field references of PBIR reports in the same PBIP project (see [Reports](#reports-pbir))

`rename` refuses:
- empty names, names with leading or trailing spaces, and names with line breaks or other control characters
- names already taken, case-insensitively: by a column, measure or hierarchy in the same table; by any measure (for
  a measure); by any table or calendar (for a table); by any function (for a function); by a sibling, otherwise
- the model object
- a new name that would change what another reference means. For example, a column named like a measure in another
  table would capture that table's unqualified `[Name]` references. tess checks this by applying the rename to a
  copy and comparing every reference. References that named nothing and now resolve are reported as warnings.
  Because `deps` doesn't know DAX keywords, this also refuses renaming a table to a word an expression already
  uses bare, such as `NOT`, `IN` or `RETURN`.

The same name is a no-op. A case-only rename (`Sales` to `SALES`) is allowed and rewrites references to match.
In a TMDL or PBIP folder, the object's file (`tables/SALES.tmdl`, `roles/...`) is renamed too, also on
case-insensitive file systems (Windows, macOS). Saves only rewrite `.tmdl` files whose content changed.
#### Reports (PBIR)

When the model is part of a PBIP project, `rename` and `move` also update the report: every `X.Report` folder
next to the `X.SemanticModel` folder whose `definition.pbir` points at it (`byPath`). In the PBIR format
(`X.Report/definition/**/*.json`), visuals, pages, bookmarks and filters name model fields structurally, and
those names are rewritten in place, so every other byte of each file stays as it was:

| Renaming or moving | Rewrites |
|---|---|
| a table | `"Entity"` of `SourceRef`s and of `From` sources |
| a column | `"Property"` of `Column` (and date-variation) references to it |
| a measure | `"Property"` of `Measure` references to it |
| a hierarchy, a level | `"Hierarchy"` of `Hierarchy` references, `"Level"` of `HierarchyLevel` references |
| a measure, moved | the `"Entity"` of its own `Measure` references |

References resolve as Power BI resolves them: a field's table is its `SourceRef` `Entity`, or the source a
`Source` alias names in the enclosing `From`. Names match ignoring case. The edits show in the dry run, as a line
diff per file (`~ report Sales.Report/definition/pages/.../visual.json  (2 references)`), in `--stat` and
`--name-only`, and in the `--json` `files` array (`path`, `display`, `references`, `written`, and the changed
`lines` with `line`, `before` and `after`). They are written with `--save`, after the model and only if it saved,
and never with `--save-to`, since the report still points at the original model (a warning says so).

Not rewritten:
- `queryRef`, `nativeQueryRef` and formatting selectors (`"metadata"`). These are a visual's own names for its
  fields: Power BI itself keeps a `queryRef` after a field is renamed, and the format describes the native
  reference name as the name native expressions (such as visual calculations) use. Visuals keep working with them
  as they are.
- free text, such as titles, and literal values (filters and slicer selections store data values). This includes
  a field parameter's saved selection, stored as the text `NAMEOF` returned (`'Sales'[Total]`): after a rename,
  that selection no longer matches and must be made again
- report-level measures (`reportExtensions.json`, and fields with `"Schema": "extension"`): a warning lists the
  file when it mentions the old name
- a moved measure named in a filter through a `From` alias, which the filter's other fields share: a warning
  lists the files
- PBIR-Legacy reports (`X.Report/report.json`), which Power BI doesn't support editing outside Power BI Desktop:
  a warning lists the file when it mentions the old name. Save the report in PBIR format to have tess update it.
- reports in other folders, and reports connected to a published model (`byConnection`)

### Setting properties (`set`)

`tess set <model> <object> -p <path>=<value> ...` sets properties on one object. It follows the
[editing conventions](#editing-commands): a dry run unless `--save` or `--save-to`, the change shown as a diff,
and new errors refused unless `--force`. `-p` is repeatable, and all assignments apply as one change.

```sh
tess set ./Sales.pbip "[Revenue]" -p formatString='#,0.00' -p displayFolder=KPIs --save
tess set ./Sales.pbip "Date[Month]" -p sortByColumn=MonthNo -p summarizeBy=none --save
tess set ./Sales.pbip "[Revenue]" -p formatStringDefinition.expression='"#,0." & REPT("0", 2)'
tess set ./Sales.pbip "[Revenue]" -p kpi.targetExpression='[Budget]' -p kpi.statusGraphic='Traffic Light'
tess set ./Sales.pbip Sales -p annotations.Owner=finance -p isHidden=true
tess set ./Sales.pbip "[Revenue]" -p expression=- < revenue.dax      # the value from stdin
tess set ./Sales.pbip "[Revenue]" --unset description --unset annotations.Owner
```

**Paths** are the ones `get -p` prints: TOM's JSON property names, such as `formatString`, `isHidden`,
`displayFolder`, `expression`, `dataCategory`, `summarizeBy` and `sortByColumn`. They match case-insensitively.
- **Nested paths** reach into sub-objects: `formatStringDefinition.expression`, `detailRowsDefinition.expression`,
  `kpi.targetExpression`, `calculationGroup.precedence`, `source.expression` (a partition's M or DAX).
  A missing sub-object is created, except a calculation group: that would make the table a calculation group.
  Unsetting a sub-object's last property, or the sub-object itself (`--unset kpi`), removes it.
- **Named items:** `annotations.<name>` and `extendedProperties.<name>`. Everything after the first dot is the
  name, so it may contain dots. Setting one adds or updates it; an existing one matches case-insensitively. A new
  extended property is a string one. An existing JSON one stays JSON and takes the whole value as JSON text, even
  though `get -p` flattens it into `extendedProperties.<name>.value...`.

`tess get <model> <object> --properties` lists every path `set` accepts on that object.

**Values** are read according to the property's type:

| Type | Accepted |
|---|---|
| string, expression | the text as-is (quote it for the shell) |
| bool | `true` or `false`, any case |
| int, number | `42`, `-1`, `0.25` (invariant culture) |
| enum | a member's TOM name, any case: `summarizeBy=none`, `dataType=int64`, `crossFilteringBehavior=bothDirections`. Members the model's compatibility level lacks are refused |
| reference | an object, in any form `get` accepts |
| `-` | read the value from stdin, taken literally (minus one trailing line break). Only one `-p` can use stdin |
| `null` | clear a string or reference, reset anything else to TOM's default (`isAvailableInMdx` goes back to `true`, `summarizeBy` to `default`), remove a sub-object, annotation or extended property. `--unset <path>` does the same. To store the text `null`, pass it on stdin |

Reference properties:

| Object | Property | Refers to |
|---|---|---|
| column | `sortByColumn` | a column of the same table: a bare name (`MonthNo`) is looked up there first |
| level | `column` | a column of the hierarchy's table |
| relationship | `fromColumn`, `toColumn` | a column; a bare name is looked up in that end's current table first |
| model | `defaultMeasure` | a measure |
| partition (legacy query source) | `source.dataSource` | a data source |
| named expression | `parameterValuesColumn`, `expressionSource` | a column; a named expression |
| column | `alternateOf.baseColumn`, `alternateOf.baseTable` | a column; a table (aggregations) |

A reference to an object of the wrong kind is refused ("that is measure:Sales/Total, not a column").

`set` refuses, with exit code 2 unless noted:
- unknown properties (`unknown-property`), suggesting close matches: `did you mean 'formatString'?`
- `name`: use [`rename`](#renaming-rename), which also fixes DAX references (`read-only-property`)
- properties that hold child objects, like `columns` or `measures`: point `set` at the child itself
  (`read-only-property`)
- read-only properties (`read-only-property`):
  - what TOM computes, like a column's `type`
  - properties the model's compatibility level or mode doesn't support (the reason names the level needed)
  - a partition's `source` itself (its properties can be set)
  - passwords, which TOM's JSON masks so a change couldn't be shown
  - a Power Query data source's `credential`, `connectionDetails` and `options`
  - query-group references
- values that don't fit the type, the same path twice, a reference to the wrong kind or to a column of another
  table where the same table is required, or a value TOM rejects (`invalid-property-value`)
- a reference to an object that doesn't exist or is ambiguous (exit code 3, `object-not-found` /
  `ambiguous-object`)

Nothing is written unless every assignment is valid. A value that is valid but breaks the model, such as DAX that
refers to a missing table, is shown along with the errors it introduces, and `--save` refuses it unless `--force`.

A relationship whose `fromColumn` or `toColumn` changes is shown as the same relationship, modified, under its new
endpoints.


### Adding objects (`add`)

`tess add <model> <ref> [options]` creates one object. The [reference](#object-references) says what it is,
where it goes and its name. `add` follows the [editing conventions](#editing-commands), so it is a dry run unless
you pass `--save` or `--save-to`. A save is refused if the new object introduces errors, such as a measure whose
DAX names a missing table, unless you pass `--force`.

```sh
tess add ./Sales.pbip measure:Sales/Margin -e "[Revenue] - [Cost]" --format-string "#,0" --save
tess add ./Sales.pbip column:Sales/Profit -e "Sales[Amount] - Sales[Cost]"           # calculated column
tess add ./Sales.pbip column:Sales/Region --data-type string --source-column region   # data column
tess add ./Sales.pbip table:Budget -m "let Source = ... in Source" --mode import
tess add ./Sales.pbip table:Numbers -e "GENERATESERIES(1, 10)"                        # calculated table
tess add ./Sales.pbip "table:Time Intelligence" --calculation-group
tess add ./Sales.pbip "calcitem:Time Intelligence/YTD" -e "CALCULATE(SELECTEDMEASURE(), DATESYTD('Date'[Date]))"
tess add ./Sales.pbip hierarchy:Date/Calendar --levels Year,Quarter,Month
tess add ./Sales.pbip level:Date/Calendar/Day --column Date
tess add ./Sales.pbip relationship --from "'Sales'[DateKey]" --to "'Date'[DateKey]"
tess add ./Sales.pbip role:Denmark --filter "'Customer'=Customer[Country] = \"DK\""
tess add ./Sales.pbip perspective:Finance --include Sales --include "'Date'[Year]"
tess add ./Sales.pbip culture:da-DK
tess add ./Sales.pbip expression:Server -m '"localhost" meta [IsParameterQuery=true, Type="Text", IsParameterQueryRequired=true]'
tess add ./Sales.pbip function:My.Double -e "(x: INT64) => x * 2"
tess add ./Sales.pbip "partition:Sales/Sales 2024" -m "let Source = ... in Source"
cat margin.dax | tess add ./Sales.pbip measure:Sales/Margin -e - --save            # DAX from stdin
```

| Kind | Needs | Optional |
|---|---|---|
| `measure:T/N` | `-e <DAX>` | `--format-string`, `--display-folder`, `--description`, `--hidden` |
| `column:T/N` | `-e <DAX>` (calculated), or `--data-type <type>` (data column) | `--source-column` (data; default: the column's name), `--format-string`, `--display-folder`, `--description`, `--hidden` |
| `table:N` | one of `-e <DAX>` (calculated), `-m <M>` (Power Query) or `--calculation-group` | `--mode` (M), `--precedence` and `--column` (calculation group), `--description`, `--hidden` |
| `hierarchy:T/N` | `--levels <col,col,...>`, top level first | `--display-folder`, `--description`, `--hidden` |
| `level:T/H/N` | | `--column` (default: the column named like the level), `--ordinal`, `--description` |
| `calcitem:T/N` | `-e <DAX>` | `--format-string-expression <DAX>`, `--ordinal`, `--description` |
| `relationship[:N]` | `--from <column> --to <column>` | `--cardinality`, `--cross-filter`, `--inactive` |
| `role:N` | | `--permission`, `--filter 'Table'=<DAX>` (repeatable), `--description` |
| `perspective:N` | | `--include <object>` (repeatable), `--description` |
| `culture:N` | | |
| `expression:N` | `-m <M>` | `--description` |
| `function:N` | `-e <DAX>` | `--description`, `--hidden` |
| `partition:T/N` | `-m <M>` | `--mode`, `--description` |

An option a kind doesn't take, such as `--levels` on a measure, is a usage error. `-e -` or `-m -` reads the
expression from stdin, without its trailing line break. Only one option per command can read stdin.

Values:
- `--data-type`: `string`, `int64`, `double`, `decimal`, `datetime`, `boolean` or `binary`.
- `--mode`: `import`, `directquery` or `dual`.
- `--cardinality`: `many-to-one`, `one-to-one` or `many-to-many`. `--from` is the many side.
- `--cross-filter`: `single` or `both`.
- `--permission`: `read`, `readrefresh`, `refresh`, `administrator` or `none`.

Wherever `add` takes an object (`--from`, `--to`, `--include`), any form `get` accepts works. `--include` takes
tables, columns, measures and hierarchies, one per option. `--levels` splits on commas, so for a column whose name
contains one, add that level with `add level:T/H/N --column <name>`. DAX that starts with `-` or `--` works as a separate argument
(`-e "-- comment ..."`). The `--expression=<value>` form isn't supported.

What new objects get (Power BI Desktop's defaults, where the spaceparts-analytics sample shows them):
- **Lineage tags:** a fresh `lineageTag` (a lowercase GUID) on every table, column, measure, hierarchy, level,
  named expression and function.
- **Data columns:** `summarizeBy: sum` for `int64`, `double` and `decimal`, otherwise `none`, plus the annotation
  `SummarizationSetBy = Automatic`. The source column defaults to the column's name.
- **Tables:** an M or calculated table gets one partition named after it, with `mode: import` unless you pass
  `--mode`. An M table starts with no columns: add data columns, or let Power BI read them from the query on refresh.
  A calculated table's columns are inferred by the engine.
- **Calculation groups:** tess's own default, the shape Tabular Editor uses: a `Name` column (rename it with
  `--column`) sorted by a hidden `Ordinal` column, and a `calculationGroup` partition with no explicit mode. The precedence is one more than the highest in the model, unless you pass
  `--precedence`. A new group has no calculation items. If the model doesn't have `discourageImplicitMeasures`
  set, `add` sets it, since calculation groups require it, and the change shows on `model`.
- **Levels and calculation items** are added last, unless you pass `--ordinal` (0 is first). The items after the
  new one move down a place.
- **Relationships:**
  - The name is a new GUID, as in Power BI. Write `relationship:<name>` to choose one, or just `relationship` for the
    GUID.
  - The defaults are many-to-one, a single cross-filter direction, and active. One-to-one relationships default to
    `both`, as in Desktop.
  - If an active relationship already joins the same two tables, the new one is made inactive, as Desktop does,
    and `add` warns. Use it with `USERELATIONSHIP`. Other ambiguous paths, through third tables, aren't detected.
  - Columns of different data types are added, with a warning.
- **Roles** have `read` permission unless you pass `--permission`.
- **Partitions** take the mode of the table's existing partitions.
- **Power BI's `PBI_QueryOrder`** model annotation, if present, lists a new M table or named expression at the end.

Names are checked as [`rename`](#renaming-rename) checks them: no empty names, no leading or trailing spaces, no
control characters. Function names must be identifiers. Taken names are refused, case-insensitively: by a column,
measure or hierarchy in the same table, by any measure (for a measure), by any table or calendar (for a table), by
any function (for a function), or by a sibling otherwise. A table and a named expression can't share a name,
because both are Power Query queries. Cultures must be specific culture names, such as `da-DK`, and are written in
their canonical case. The table, hierarchy or column an object goes into must exist. Names match case-insensitively,
and the new reference uses the real name, so `measure:sales/X` adds `measure:Sales/X`. If nothing matches, `add`
suggests close names.

`add` refuses:
- columns in calculated tables, because their columns come from the DAX, and in calculation groups, whose columns
  are fixed
- a second partition for a calculated table or a calculation group, or an M partition in a table whose partitions
  have another source type
- calculation items outside calculation groups, and a level on a column that's already in the hierarchy
- relationships from a table to itself, or over columns another relationship already joins (in either direction)
- calculation groups below compatibility level 1470, calculation item ordinals below 1500, and functions below 1702
- data sources and calendars, which `add` doesn't create

**`--if-not-exists`:** if an object with that reference already exists, in any case, `add` does nothing, notes
it, and exits 0. For a relationship, an existing relationship over the same columns counts too. A different kind
of object holding the name is still a collision.

Not supported: column data types for calculated columns (inferred), KPIs, detail rows, translations (use
[`translations import`](#translations-translations)), role members,
object-level security, and the Power Query parameter metadata of named expressions (write the `meta [...]` record
in the M yourself).
### Removing objects (`remove`)

`tess remove <model> <object>... [--cascade] [--if-exists]` removes one or more objects, as one change, with
everything inside them: a table's columns, measures, hierarchies and partitions, a hierarchy's levels. It follows
the [editing conventions](#editing-commands): a dry run unless `--save` or `--save-to`.

**Dependents first.** If DAX outside what's being removed still refers to it (as `deps --referenced-by` shows), the
removal is refused (exit 3) and the references are listed:

```
tess: Can't remove measure:Sales/Revenue: other objects refer to it:
  measure:Sales/Margin %  expression:5  [Revenue]  -> measure:Sales/Revenue
```

- References from inside the removed set don't count, so a table can go together with the measures that use it.
  Neither does a role's row filter on a removed table: the filter goes with the table.
- **`--cascade`** also removes the dependents, and theirs, as long as they're objects whose definition is their DAX:
  measures, calculated columns, calculated tables, calculation items and functions. A role or a table-level
  expression (detail rows, calculation group defaults) that refers to it is never removed for that; it still blocks.
- **`--force`** removes it anyway and leaves those references broken. Each is reported as a warning, and the
  qualified ones (`'T'[X]`) also as `introducedErrors`. An unqualified `[X]` that names nothing is only a `validate`
  warning, since it may be a column of a table expression.

**Structural fallout.** TOM itself doesn't clean up when an object goes; links to it would dangle and the model
couldn't be saved. tess handles each case, and says what it did on stderr (`notes` in JSON):

| When this goes | What happens |
|---|---|
| a column | relationships that use it are removed (any DAX that relies on a relationship, like `USERELATIONSHIP`, names the column and so blocks); hierarchy levels on it are removed, the other levels renumbered (so they show as modified) and a hierarchy with no levels left is removed; sort-by columns pointing at it are cleared; so are `alternateOf`, group-by columns, calendar associations and a parameter's values column |
| a table | the same for each of its columns; its relationships, perspective entry and roles' permissions on it (row filter and object-level security) go; it's taken out of Power BI's `PBI_QueryOrder` annotation |
| a relationship or hierarchy | variations that use it are removed |
| anything | its perspective memberships, object-level security entries and translations go |

Relationships are removed rather than refused because one can't exist without its columns, and DAX that depends on a
particular relationship refers to its columns, so it is caught by the dependents check.

`remove` refuses (exit 3):
- the model itself
- a table's last partition (remove the table instead)
- a column of a calculated table (it comes from the table's expression) or of a calculation group
- a data source or shared expression a partition still uses
- anything that would still leave TOM with a link to a removed object (a safety net: tess checks TOM's own
  validation after the removal)

**`--if-exists`** skips objects that don't exist, with a note, and exits 0 (so a script can run the same removal
twice). An ambiguous object is still an error.

Not updated: M (Power Query) that mentions a removed table, expression or data source by name (listed as a warning),
the linguistic schema, and report visuals.

### Moving measures (`move`)

`tess move <model> <measure> <table> [--display-folder <folder>]` moves a measure to another table. It keeps its name,
lineage tag and every other property, and it follows the [editing conventions](#editing-commands). The change set
shows it as `moved` (`measure:Sales/Revenue -> measure:Measures/Revenue`).

Measure names are unique across the model, so unqualified `[Revenue]` references stay as they are. References that
qualify it with its old table, `'Sales'[Revenue]` and `Sales[Revenue]`, are rewritten to the new table token by
token, quoted as `rename` quotes table names, with every other character untouched. Comments and strings are never
rewritten. As with `rename`, tess applies the move to a copy and refuses (exit 3) if any reference would then mean
something else. The usual case is the measure's own unqualified column reference, such as `SUMX(Sales, [Amount])`,
since `[Amount]` resolves against the measure's own table. Qualify it first (`Sales[Amount]`).

The measure's translations follow it. So do its perspective memberships: if the new table isn't in a perspective the
measure is in, the table is added to it with just that measure, so the measure stays visible (noted on stderr).
`--display-folder` sets the folder in the same change.

`move` refuses (exit 3) a column (its data comes from its table's partitions), a calculation item, a table that
doesn't exist, a calculation group table, and a target table with a column or hierarchy of the same name. Moving to
the table it's already in changes nothing. In a PBIP project, the report's references to the measure follow it (see
[Reports](#reports-pbir)).

### Best Practice Analyzer (`bpa`)

`bpa` checks a model against Best Practice Analyzer rules in Tabular Editor's JSON format. Existing TE2/TE3
rule files run unchanged as long as they only use the members listed below (see Compatibility):

```
tess bpa ./Sales.pbip                              # built-in rules, plus rules stored in the model
tess bpa ./Sales.pbip -r BPARules.json -r team.json --no-default-rules
tess bpa ./Sales.pbip --category DAX,Performance --min-severity 2
tess bpa ./Sales.pbip --list-rules                 # what would run, and whether each rule is supported
tess bpa ./Sales.pbip --path 'measure:Sales/*'     # only findings on matching objects
tess bpa ./Sales.pbip --json                       # the findings envelope, plus ignored findings and rule errors
tess bpa ./Sales.pbip --trx bpa.trx                # VSTest results, one test per rule (Azure DevOps etc.)
```

Text output is one finding per line, most severe first, then a summary:

```
error    relationship:Sales-Product  TESS_RELATIONSHIP_DATA_TYPES  Relate columns of the same data type
warning  measure:Sales/Margin %      TESS_DIVIDE  Use DIVIDE when the divisor can be zero
info     measure:Sales/Total         TESS_MEASURE_DESCRIPTION  Describe visible measures
3 findings (1 error, 1 warning, 1 info); 25 rules checked
```

Severity 1 is info, 2 a warning and 3 (or more) an error. `bpa` exits 1 when a finding is at least as severe
as `--fail-on` (`error`, `warning` or `info`; default `error`). `--min-severity` and `--category`
(case-insensitive, repeatable) choose which rules run. Rules that can't run are listed on stderr (in JSON, under
`ruleErrors`). They don't change the exit code. `--json` writes the
[findings envelope](#findings-json-and-ci-annotations) with these extra keys:
- `model`, `rulesEvaluated`, `violations`
- `ignored`: findings suppressed by annotations
- `ruleErrors`: `{ruleId, ruleName, stage, error}`
- `skippedRules`, `warnings`
- `rules`: counts

`--ci github|azdo` writes CI annotations to stderr. In JSON, `warnings` lists rule files the model refers to that
couldn't be read, `notes` things deliberately not done (URLs not fetched), and `fetched` the URLs requested.

`--path <pattern>` (repeatable or comma-separated) reports only findings on objects whose reference matches: a glob
over the whole reference (`measure:Sales/*`, `*:Sales/*` for everything in a table), or else a prefix (`column:`,
`measure:Sales/`). Case-insensitive. Every rule still runs; the exit code, the summary, `--trx` and `--ci` only
count the matching findings.

#### Which rules run

Rules come from five layers. They merge by rule ID (case-insensitive): every rule from every layer runs, and where
two layers define the same ID, the higher layer's definition wins. Highest first:

1. **`--rules` files**, in order (a later file wins).
2. **`TESS_BPA_RULES`**: a list of rule files separated like `PATH` (`:` on Linux and macOS, `;` on Windows),
   relative to the current directory; a later file wins.
3. **Config files**, each a TE rule file:
   - the project's `.tessera/bpa.json`, the nearest one found walking up from the model's folder to the root of
     the git repository it's in (the first folder holding `.git`). Outside a repository, only the model's own
     folders count: the `.bim` file's folder or the TMDL folder, and for PBIP the `X.SemanticModel` folder and the
     project folder holding it.
   - the user's `bpa.json`: `$XDG_CONFIG_HOME/tessera/bpa.json`, or `~/.config/tessera/bpa.json`, on Linux and
     macOS; `%APPDATA%\tessera\bpa.json` on Windows. The project's file wins over the user's.
4. **The model's own rules:** rules stored in its `BestPracticeAnalyzer` annotation, over the files listed in its
   `BestPracticeAnalyzer_ExternalRuleFiles` annotation (a JSON array of paths, relative to the `.bim` file's
   folder or the TMDL folder). Among those files the first listed wins, as in TE.
5. **tessera's built-in rules.**

A file you name (layers 1 to 3) must exist and be valid, or `bpa` stops (exit 3). A file the model lists that is
missing or invalid is a warning, so a model can always be analyzed.

Switches: `--no-builtin` (or `--no-default-rules`) drops layer 5, `--no-model-rules` layer 4 (like TE2's `-AX`),
and `--no-config` layers 2 and 3. `--builtin-only` runs only layer 5 and can't be combined with `--rules` or
`--no-builtin`. `tess bpa rules list` shows which layer each rule in effect comes from.

How this differs from TE: TE2 merges the same kinds of sources by ID, but gives the model's rules the last word
and rules passed on its command line the least. tess turns that around, so whoever runs the analysis (a CI job, a
team's config) decides what a rule means, not the model being checked. To keep a model's own version of a rule,
give it its own ID.

#### External rule URLs

tess makes no network requests unless asked to. A `BestPracticeAnalyzer_ExternalRuleFiles` entry that is a URL is
skipped with a note naming it, unless you pass `--allow-external-rules`. With it:
- only `https://` URLs are fetched, and not ones with a user name or password in them;
- the request is a plain GET with no cookies, credentials or anything about the model;
- it times out after 15 seconds, and a file over 4 MB is refused; redirects must stay on `https://`;
- each URL requested is named on stderr (also with `--json`), and a failed fetch is a warning.

Fetched rules belong to the model's layer, in the position the URL has in the list.

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
annotations, so ignore their findings on the model. `tess bpa rules ignore` writes these annotations.

#### Managing rules (`bpa rules`)

```sh
tess bpa rules list <model> [--layer builtin,model,config,env,cli]   # the rules in effect
tess bpa rules show <model> <rule-id>
tess bpa rules add <model> --id ID --scope Measure -e 'Name.Length > 40' [--to model|<file>] --save
tess bpa rules add <model> --from team.json --to model --save        # copy a rule file's rules into the model
tess bpa rules remove <model> <rule-id>... [--to model|<file>] --save
tess bpa rules ignore <model> <rule-id>[,<rule-id>...] [<object>...] --save
tess bpa rules unignore <model> <rule-id>[,<rule-id>...] [<object>...] --save
```

`list` and `show` take the same rule switches as `bpa` (`--rules`, `--no-builtin`, `--builtin-only`,
`--no-model-rules`, `--no-config`, `--allow-external-rules`), after the subcommand. Each rule shows its layer, its
source and the lower layers' definitions it overrides (`overrides` in JSON), and whether the model's ignore
annotation turns it off (`disabled`).

`add` builds a rule from `--id`, `--name`, `--category`, `--description`, `--severity` (default 1), `--scope`,
`--expression` (`-e`), `--fix-expression`, `--compatibility-level` (default 1200) and `--learn-more-url`, or
copies the rules of a TE rule file (`--from`). A rule that doesn't compile is added with a warning, as TE allows.
An ID that's already there is refused (exit 3) unless `--replace`. `remove` takes one or more IDs (`--from` is
the same as `--to`); removing a built-in rule is refused, with a pointer to `ignore`.

Where they write, `--to`:
- `model` (the default): the model's `BestPracticeAnalyzer` annotation, as TE writes it (an indented JSON
  array; the annotation is removed when it empties). This is a model edit and follows the
  [editing conventions](#editing-commands): a dry run unless `--save` or `--save-to`. The change shows as a
  diff of the annotation.
- a file path: a TE rule file, created if it's missing (`.tessera/bpa.json` is one). A dry run shows the rules
  added (`+`) and removed (`-`); `--save` writes the file. The file is edited as JSON, so other rules and fields
  tess doesn't know stay as they were, and its line endings are kept; comments and trailing commas are not
  (a note says so). `--save-to`, `--force`, `--stat` and `--name-only` are model-only.

`ignore` adds the IDs to each object's `BestPracticeAnalyzer_IgnoreRules` annotation, in TE's format
(`{"RuleIDs":["A","B"]}`), and `unignore` removes them, removing the annotation when it empties (TE would leave
`{"RuleIDs":[]}`, which means the same). Objects take any form `get` accepts. Without objects, the rule is
ignored on the model, which turns it off for the whole model. Both are model edits, as above. IDs that no rule in
effect has are warned about but still written, since TE accepts any ID. An object with no annotations
(calculation items) is refused (exit 3).

To manage rules for a model that is literally in a folder named `rules`, write `tess bpa ./rules`.

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

### Translations (`translations`)

`tess translations export` writes a file for translators: one row for each object, culture and translatable
property, with the untranslated text next to the translation. `tess translations import` applies the
filled-in file.

```sh
tess translations export ./Sales.pbip                          # CSV on stdout, every culture
tess translations export ./Sales.pbip --culture da-DK -o da-DK.csv
tess translations export ./Sales.pbip --only-translated --json # just what is translated, as JSON
tess translations import ./Sales.pbip da-DK.csv                # preview the changes
tess translations import ./Sales.pbip da-DK.csv --save
tess translations import ./Sales.pbip fr-FR.csv --create-cultures --save
tess translations cultures ./Sales.pbip                        # how much of each culture is translated
```

The CSV file has a header row and RFC 4180 quoting: fields with commas, quotes or line breaks, or with spaces
at either end, are quoted, with quotes doubled. It is UTF-8 with LF line endings. `--bom` adds a byte-order
mark, which Excel needs to read it as UTF-8.

```
object,culture,property,source,translation
measure:Sales/Total,da-DK,Caption,Total,I alt
measure:Sales/Total,da-DK,Description,"Net sales, after returns",
table:Sales,da-DK,Caption,Sales,Salg
```

- `object` is a [reference](#object-references). On import, any form `get` accepts works.
- `property` is `Caption`, `Description` or `DisplayFolder`.
- `source` is the untranslated value: the object's name, description or display folder.
- `translation` is empty where there isn't one.

`--format json` (or an `-o` file ending in `.json`, or `--json`) writes the same rows as a JSON array of
`{object, culture, property, source, translation}` objects.

**What's exported:** rows are grouped by culture, in the model's culture order, with objects in model order, so
re-exported files diff cleanly. Captions are always listed. Descriptions and display folders are listed when the
object has one, or a translation of it exists, since there is otherwise nothing to translate.
`--only-translated` lists only rows that have a translation. Objects and properties are the ones TOM keeps
translations for:

| Object | Properties |
|---|---|
| model, table, level, perspective, named expression, function, calendar | Caption, Description |
| column, measure, hierarchy | Caption, Description, DisplayFolder |
| role | Description |

Calculation items are left out: TOM writes their translations to TMDL but can't read them back.

**Import** follows the [editing conventions](#editing-commands): a dry run unless you pass `--save` or
`--save-to`. The change shows on each translated object, e.g. `translations.da-DK.caption: (none) -> "I alt"`.
- Each row sets its translation. An empty translation removes it.
- Rows the file doesn't have are left alone, so a file with only some rows or cultures is fine. `--culture` imports
  only those cultures' rows.
- The format comes from `--format`, else the file's extension, else its content (`[` means JSON). `-` reads
  stdin. A byte-order mark is skipped. Header columns can be in any order and any case; `source` is optional, and
  extra columns are ignored.
- Cultures match case-insensitively. A culture the model doesn't have is refused unless you pass
  `--create-cultures`, which adds it (it must be a specific culture name such as `fr-FR`).
- If a row's `source` no longer matches the model, for example because a description changed after the export,
  the row is imported with a warning that lists the lines.

Import refuses the whole file (exit 3), naming the file and line, for:
- an object that doesn't exist, or is ambiguous
- a property the object can't have translated, such as a table's `DisplayFolder`. TOM accepts it, but drops it
  on save.
- an unknown property or culture
- two rows that give the same property different translations
- a malformed file: a missing header column, a row with too many or too few fields, or an unclosed quote

The line is where the row starts, counting line breaks inside quoted fields. In JSON files it's the item number.
`export` writes a file the translator fills in: exporting, then importing that file unchanged, changes nothing.

`get -p translations.da-DK.caption` shows one translation, and `find` searches translations. `set` doesn't
write them and points at `translations import`. `tess translations cultures` prints each culture's translated
and total rows per property; `--json` writes `[{ref, name, translated, total, properties: {caption: {translated,
total}, ...}}]`.

### Perspectives (`perspective`)

`tess perspective add|remove <model> <perspective> <object>...` changes which tables, columns, measures and
hierarchies a perspective shows. It follows the [editing conventions](#editing-commands). `tess perspective list`
shows membership.

```sh
tess perspective add    ./Sales.pbip Finance Sales "'Date'[Year]" "[Budget]" --save
tess perspective remove ./Sales.pbip Finance "Sales[Internal Key]" --save
tess perspective list   ./Sales.pbip                # each perspective, with member counts
tess perspective list   ./Sales.pbip Finance        # its members, one reference per line (--json too)
```

The perspective is a name, matched case-insensitively, or a `perspective:<name>` reference. The objects can be in
any form `get` accepts. In TOM a perspective lists its tables, and each table lists the members it shows, so:
- **Adding a table** adds it with all its columns, measures and hierarchies, like `add perspective:X --include`.
  Adding a table that's partly in adds the rest.
- **Adding a column, measure or hierarchy** adds its table too, with just that member.
- **Removing a table** removes it with all its members.
- **Removing a table's last member** removes the table too, since a table with no members shows nothing, and says so
  on stderr.
- What's already in, or not in, the perspective is noted on stderr. It isn't an error.

`perspective` refuses (exit 3) a perspective that doesn't exist (create it with `tess add <model> perspective:<name>`),
and levels, calculation items and other objects perspectives don't hold. The change shows as the perspective's
`members`, line by line. `list --json` writes `[{ref, name, tables, columns, measures, hierarchies}]`, or
`{perspective, members: [{ref, kind}]}` for one perspective.
### DAX scripts (`script`)

A DAX script is Tabular Editor 3's text format for many objects' DAX and properties in one file: export it, edit
it, apply it back. `tess script export <model> [objects...]` writes one; `tess script apply <model> <script>`
creates or updates each object it defines.

```
-------------------
-- Measure: [Total]
-------------------
MEASURE 'Sales'[Total] = SUM ( Sales[Amount] )
    Description = "Revenue, before returns"
    FormatString = "#,0.00"
    DisplayFolder = "KPIs"
    Visible = FALSE

-----------------------------------------
-- Calculation Group: 'Time Intelligence'
-----------------------------------------
CALCULATIONGROUP 'Time Intelligence'[Period]
    Precedence = 0

    CALCULATIONITEM "YTD" =
        TOTALYTD (
            SELECTEDMEASURE (),
            'Date'[Date]
        )
        FormatString = "0.00"
        Ordinal = 0
```

```sh
tess script export ./Sales.pbip                          # every DAX object, to stdout
tess script export ./Sales.pbip "[Total]" Sales -o x.dax  # a table stands for its measures, calculated columns and own DAX; an existing file needs --force
tess script export ./Sales.pbip -k measure,calcitem -t Sales --json
tess script apply  ./Sales.pbip x.dax                    # dry run: the changes, as `diff` shows them
tess script apply  ./Sales.pbip - --save < x.dax         # from stdin
```

What a script holds, and the properties each object takes (names are case-insensitive):

| Object | Written as | Properties |
|---|---|---|
| measure | `MEASURE 'Table'[Name] = <DAX>` | `Description`, `FormatString`, `DisplayFolder`, `Visible`, `DetailRows`, `KpiStatusExpression`, `KpiStatusDescription`, `KpiStatusGraphic`, `KpiTrendExpression`, `KpiTrendDescription`, `KpiTrendGraphic`, `KpiTargetExpression`, `KpiTargetDescription`, `KpiTargetFormatString`, `FormatStringDefinition` |
| calculated column | `COLUMN 'Table'[Name] = <DAX>` | `Description`, `FormatString`, `DisplayFolder`, `Visible`, `Datatype` |
| calculated table | `TABLE 'Name' = <DAX>` | `Description`, `Visible`, `DetailRows` |
| calculation group | `CALCULATIONGROUP 'Table'[Column]` (the column its items are listed in) | `Description`, `Visible`, `Precedence` |
| calculation item | `CALCULATIONITEM "Name" = <DAX>`, after its group | `Description`, `FormatString`, `Ordinal` |
| function | `FUNCTION Name = (params) => <DAX>` | `Description`, `Visible` |

Values:
- **Strings** are in double quotes, with `""` for a quote. They may span lines.
- **`Visible`** takes `TRUE` or `FALSE`; **`Ordinal`** and **`Precedence`** take whole numbers.
- **`Datatype`** takes `BOOLEAN`, `DOUBLE`, `INTEGER`, `DATETIME`, `CURRENCY` (decimal) or `STRING`.
- **DAX properties:** `DetailRows`, the `Kpi*Expression`s, and a calculation item's `FormatString` take DAX.
- **A measure's `FormatString`:** a lone string is the static format string; any other DAX is a dynamic format
  string. `FormatStringDefinition = <DAX>` (a tess extension) is always the dynamic one. export writes it when the
  DAX is a lone string, or when the measure also has a static format string. A static `FormatString` removes a
  dynamic one, unless the same object also sets `FormatStringDefinition`.

How a script is read:
- **Where DAX ends.** An expression runs until:
  - a comma outside parentheses;
  - a line that starts with a property name, `=` and a value of that property's kind (so the DAX line
    `Visible = 1` is not the `Visible` property, which takes `TRUE` or `FALSE`), outside parentheses;
  - or the next object keyword.
- **Commas.** Properties may be written with a leading comma, as Tabular Editor 3 does after an empty expression.
- **Comments.** A comment after an expression belongs to it if it's on the expression's last line, or indented
  deeper than the object's keyword. The banners at the margin don't belong to any expression.
- **Layout.** An expression that starts on the line after `=` loses the line break and its common indentation.
  Leading and trailing whitespace is not part of any expression, and neither is the kind of line break.
- **DAX queries.** The `DEFINE` section of a DAX query is a script (`DEFINE MEASURE 'T'[M] = ...`,
  `DEFINE COLUMN ...`, `DEFINE TABLE ...`, `DEFINE FUNCTION ...`); its `EVALUATE` sections are skipped. Query
  `VAR`s are not supported.
- **Errors.** Syntax errors, and objects that can't be applied, are reported as `script.dax:line:column: message`.
  With `--json`, the error also has `source`, `line` and `column`.

`apply` follows the [editing conventions](#editing-commands): a dry run unless `--save` or `--save-to`, the changes
as a diff, new errors refused unless `--force`. It works like this:
- **New or existing.** Each object is updated if it exists and created if it doesn't, the way `add` creates it.
  Objects and properties the script doesn't mention are left alone.
- **Forward references.** All objects are applied before the model is validated, so they may refer to objects
  defined later in the script.
- **`=` omitted.** Without `=`, an existing object keeps its expression and only its properties change.
- **Empty values.** `=` followed by nothing, or an empty value, clears that expression or property: `DetailRows`
  and dynamic format strings are removed.
- **No-op detection.** Expressions that differ only in leading and trailing whitespace or line breaks count as
  unchanged. So exporting and applying back changes nothing, even for a `.bim` file with `\r\n` in its DAX.
- **Ordinals.** `Ordinal` sets an item's ordinal as given; other items are not renumbered.
- **Refusals.** `apply` refuses:
  - a new object without an expression;
  - an expression on a data column, or on a table that isn't calculated (properties alone are fine);
  - a calculation group whose `[Column]` isn't its items' column (use `tess rename` for that);
  - a measure name that another table already uses;
  - a new object in a table that doesn't exist.

`export` writes objects in model order: for each table, its own DAX (or its calculation group with its items, by
ordinal), then its calculated columns and measures; functions come last. Expressions are written as stored, with
`\n` line breaks. Multi-line ones start on the line after `=` and are indented. `export` reads the script back
before writing it. An object whose DAX would not read back exactly is named in a warning. Examples are a line that
reads like a property (`Visible = TRUE`) or a comma outside parentheses (`1,1`, from a locale that writes decimals
that way).
### Linting DAX (`lint`)

`lint` checks every DAX expression in the model for code that can be written better: Tabular Editor 3's
[code actions](https://docs.tabulareditor.com/te3/features/code-actions.html), offline. Rules that match one of
TE3's code actions use its id and name; `TS` ids are tessera's own. It reads measures, calculated columns and
tables, calculation items, KPIs, format string and detail rows expressions, row filters and functions.

```
tess lint ./Sales.pbip                             # report (exit 1 on errors, like validate)
tess lint ./Sales.pbip --rule DI003,DI004 --fail-on warning --ci github
tess lint ./Sales.pbip --fix                       # show the rewrites (a dry run)
tess lint ./Sales.pbip --fix --rule DI001 --save   # remove unused variables, and save
```

| ID | Severity | Name | Before | After |
|---|---|---|---|---|
| `TS001` | error | Replace non-standard whitespace | `1 + 2`, with U+00A0 before the `+` | `1 + 2` |
| `DI001` | warning | Remove unused variable | `VAR a = 1 VAR b = 2 RETURN b` | `VAR b = 2 RETURN b` |
| `DI002` | warning | Remove all unused variables | `VAR a = 1 VAR b = a RETURN 123` | `123` |
| `DI003` | warning | Remove table name from measure references | `'Sales'[Total] * 1.25` | `[Total] * 1.25` |
| `DI004` | warning | Add table name to column references | `SUMX(Sales, [Amount])` | `SUMX(Sales, 'Sales'[Amount])` |
| `DI008` | warning | Remove superfluous CALCULATE | `VAR x = [Total] RETURN CALCULATE(x, 'Date'[Year] = 2025)` | (not fixed) |
| `DI014` | warning | Replace IFERROR with DIVIDE | `IFERROR(([A] - [B]) / [B], BLANK())` | `DIVIDE([A] - [B], [B])` |
| `DR014` | info | Simplify using IN | `'Doc'[Type] = "Invoice" \|\| 'Doc'[Type] = "Credit"` | `'Doc'[Type] IN {"Invoice", "Credit"}` |

What each rule reports, and when it doesn't fix:
- **`TS001`:** no-break, zero-width, typographic and other non-standard spaces (U+00A0, U+200B, U+FEFF, ...)
  between tokens. Each becomes a regular space. Strings, quoted names, `[names]` and comments are left alone,
  since names can contain such characters. TE3 3.27.1 rejects these characters, and `validate` reports the
  zero-width ones as `invalid-token`.
- **`DI001`, `DI002`:** `DI001` is a `VAR` that nothing refers to. `DI002` is one used only by other unused
  variables. The fix removes every variable the `RETURN` doesn't need, so fixing either id removes both kinds.
  A definition on lines of its own is removed with its lines, including a comment at the end of its last line.
  Comments on other lines stay. When every variable goes, the block becomes its `RETURN` expression. That
  isn't done where the block is an operand (`2 * VAR a = 1 RETURN 3 + 4`), because it would change how the
  expression parses. Variables whose name is defined twice in one expression, or is also a function parameter,
  are not analysed.
- **`DI003`, `DI004`:** the fix writes `[Measure]` or `'Table'[Column]`. The table is always quoted, like TE's
  `DaxObjectFullName`. Columns resolve against the expression's own table, as in `deps`. A column isn't fixed
  when a measure anywhere has the same name, since `[Name]` might mean either. References inside `NAMEOF(...)`
  are left as written, because field parameters store the name NAMEOF returns. These match `bpa`'s
  `TESS_QUALIFY_COLUMNS` and `TESS_UNQUALIFIED_MEASURES`, and after `lint --fix` those rules have nothing left to
  report, except measures inside `NAMEOF`, which `bpa` still flags.
- **`DI008`:** `CALCULATE(x, ...)` or `CALCULATETABLE(x, ...)` where `x` is a variable. A variable is evaluated
  where it is defined, so CALCULATE's filters don't change it. That is almost always a mistake, so it is
  reported, not fixed: the fix TE3 offers (removing CALCULATE) would hide it. UDF parameters are not variables:
  an `EXPR` parameter is evaluated inside CALCULATE.
- **`DI014`:** only when `a / b` is the whole first argument, with a single top-level `/` and nothing that binds
  more loosely around it (`*` before the division, `^` and signs are fine). `IFERROR(..., BLANK())` loses its
  `BLANK()`, which is DIVIDE's default. **This fix changes edge-case results.** DIVIDE returns `x` when `b` is
  zero or blank, where `a / b` gives infinity (or NaN for 0/0) and IFERROR passes that through. DIVIDE also
  doesn't catch errors raised inside `a` or `b`. Usually that is what the IFERROR was meant to do. Use
  `--rule` to leave `DI014` out if not.
- **`DR014`:** two or more `=` comparisons of the same column, measure or variable, joined by `||`, as a whole
  expression or argument. The values must be non-zero numbers or non-empty strings, all of one kind. `=`
  treats BLANK as equal to `0`, `""` and `FALSE`, but `IN` doesn't, so for these values the two agree.
  Chains containing comments are reported but not fixed.

Options:
- `--rule`, `--min-severity`, `--fail-on` and `--ci` work as for [`validate`](#validation-validate).
  `--rule` ids are case-insensitive.
- Text output has `validate`'s layout, plus a line saying how many findings `--fix` would fix.
- `--json` writes the [findings envelope](#findings-json-and-ci-annotations). Each finding has `ruleName`, a
  `category` (TE3's `Improvement` or `Readability`, or `Correctness` for `TS001`) and `fixable`. The envelope
  adds a top-level `fixable` count.

With `--fix`, `lint` is an [editing command](#editing-commands):
- It is a dry run unless `--save` or `--save-to`. `--stat` and `--name-only` work as for the other editing
  commands, and `--json` writes the change set.
- It fixes the findings that `--rule` and `--min-severity` select.
- Only the rewritten tokens change. Fixes repeat until nothing more applies (removing one variable can leave
  another unused), so a second `--fix` changes nothing.
- Every fix is checked before it is applied: the rewritten DAX must lex at least as cleanly, keep a VAR
  structure that parses, and refer to the same objects. A fix that fails the check is reported with
  `fixable: false`.
- Findings left unfixed are counted in a warning on stderr. `--fail-on` and `--ci` are for reports, and are
  refused with `--fix` (exit 2), as are `--save`, `--save-to`, `--force`, `--stat` and `--name-only`
  without it.

Findings can't be suppressed on individual objects yet.

### Formatting DAX (`format`)

`format` lays DAX out in the style of SQLBI's
[Rules for DAX code formatting](https://www.sqlbi.com/articles/rules-for-dax-code-formatting/), the style
[DAX Formatter](https://www.daxformatter.com) popularised. It runs entirely offline: nothing is sent anywhere.

```
tess format --expr "calculate(sum(Sales[Amount]),all('Date'))"   # loose DAX, to stdout
tess format - < measure.dax                                     # stdin
tess format query.dax --json                                    # {success, formatted, errors}
tess format ./Sales.pbip                                        # every DAX expression in a model (a dry run)
tess format ./Sales.pbip "[Revenue]" -t Date --save             # some of them, and save
tess format ./Sales.pbip --check                                # CI: exit 1 if anything isn't formatted
```

Before and after:

```
calculate(sumx(Orders,Orders[Amount]),filter(all(Customers),calculate(countrows(Sales),all(Calendar[Date]))>42+8-25*(3-1)+2-1+2-1+calculate(2+2-2+2-2)-calculate(4)))
```

```
CALCULATE (
    SUMX ( Orders, Orders[Amount] ),
    FILTER (
        ALL ( Customers ),
        CALCULATE ( COUNTROWS ( Sales ), ALL ( Calendar[Date] ) )
            > 42 + 8 - 25 * ( 3 - 1 ) + 2 - 1 + 2 - 1
                + CALCULATE ( 2 + 2 - 2 + 2 - 2 )
                - CALCULATE ( 4 )
    )
)
```

```
var total = sum(Sales[Amount]) // all rows
var top = calculate(sum(Sales[Amount]), topn(10, values(Product[Name]), [Revenue]))
return if(total > 0, divide(top, total))
```

```
VAR total = SUM ( Sales[Amount] ) // all rows
VAR top = CALCULATE ( SUM ( Sales[Amount] ), TOPN ( 10, VALUES ( Product[Name] ), [Revenue] ) )
RETURN
    IF ( total > 0, DIVIDE ( top, total ) )
```

The style:
- **Calls:** built-in function names in upper case, with a space before `(` and spaces inside the parentheses:
  `SUM ( Sales[Amount] )`, `BLANK ()`. A call that fits on its line stays on one line. Otherwise each argument
  goes on a line of its own, indented 4 from where the call starts, with the comma at the end of the line and
  `)` under the function name. `SWITCH` keeps each value and its result on one line when they fit.
- **Operators** are spaced. A chain of operators that doesn't fit is split before an operator, and continues
  one indent in. Arithmetic and `&` keep as many operands on a line as fit, but an operand containing a function
  call starts a line of its own. `&&`, `||` and comparisons put each operand on a line. After a multi-line
  operand, the next operator follows its closing bracket: `) / 'Budget Rate'[Rate]`.
- **VAR/RETURN:** a `VAR`'s value stays on the `VAR` line if it fits, otherwise it goes on the next line,
  indented. `RETURN` is on a line of its own, with its expression indented below it.
- **Queries:** `DEFINE`, `EVALUATE`, `ORDER BY` and `START AT` start lines. Definitions are indented under
  `DEFINE`, and a `MEASURE`, `COLUMN`, `TABLE` or `FUNCTION` definition is on the line before its expression,
  as SQLBI's rules say.
- **Keywords** (`VAR`, `RETURN`, `IN`, `NOT`, `TRUE`, `ASC`, `DEFINE`, ...) are upper case. So are words that
  are only sometimes keywords (`YEAR`, `DESC`, `ROWS`, `BLANKS FIRST`, UDF types like `TABLE EXPR`), but only
  where they're in a keyword's place: a whole argument of a built-in function, words side by side, or a type
  after `:`. Calls to user-defined functions, and the model's table, calendar and variable names, keep their
  case.
- **Kept as written:** strings, quoted and bracketed names, numbers, how tables are quoted (`Sales[x]` stays
  unquoted, `'Sales'[x]` stays quoted), and comments. A comment on a token's line stays on that line, and a
  comment on a line of its own stays on a line of its own. A comment after an argument moves past its comma.
  One blank line is kept where the source had blank lines between `VAR`s, definitions and comments.
- Line endings follow the input (CRLF in, CRLF out).

Options:
- `--width N`: the line width to fit (default 100; the article doesn't give one).
- `--long`: SQLBI's strict rule. A call with two or more arguments, or whose only argument is a function call,
  always has its arguments on separate lines, even when it would fit.
- `--no-space-after-function`: `SUM( x )` rather than `SUM ( x )`.
- `--semicolons` (loose DAX only): the input separates arguments with `;` and writes decimals with `,`
  (`IF ( [a] > 1,5; 2; 3 )`). The output keeps that dialect. Without the option, a `;` is an error, so
  semicolon-separated DAX is never silently read as comma-separated (where `1,5` would be two arguments).
  A model's DAX is always comma-separated.

What it reads:
- `-` reads stdin, `--expr` takes the DAX as an argument, and any file that isn't a model file is read as DAX.
  Model files (`.bim`, `.pbip`, `.pbism`, `.pbir`, `.tmdl`) and folders are [models](#accepted-model-paths).
  Loose DAX can be an expression, a function (`(x: INT64) => x * 2`) or a query (`DEFINE` / `EVALUATE`).
- In a model: measures (with their format string, detail rows and KPI expressions), calculated columns, calculated
  tables, calculation items and calculation group expressions, detail rows expressions, row filters and functions.
  M (partitions and named expressions) is left alone.
- With a model, objects after the path limit what's formatted (any form `get` accepts; a table stands for every
  expression in it, row filters on it included), and so do `-k` and `-t`, as for `list`.

Safety: the formatter changes only whitespace and the case of keywords and built-in function names. Before it
returns anything, it lexes the result and checks that it has the input's tokens in the input's order (keywords
and built-in function names may differ in case), with the same comments in the same order, and that lexemes the
lexer splits (`==`, a `1,5` decimal) weren't pulled apart. In a model, it also checks that every reference
resolves to the same object as before. A result that fails a check is refused, never written, and reported as a
tessera bug (exit 70 for loose DAX; a warning for an expression in a model). Formatting is idempotent: formatting formatted DAX changes nothing.

Output and exit codes:
- Loose DAX: the formatted DAX on stdout. `--json` writes `{"success", "formatted", "errors"}`, like
  `te util format-dax`. Each error has `line`, `column` (1-based) and `message`. DAX that can't be read
  (unbalanced brackets, an unterminated string, an operator without an operand, `VAR` without `RETURN`, ...) exits
  3 with its errors on stderr, nothing on stdout, and the input file left alone.
- A model is an [editing command](#editing-commands): a dry run unless `--save` or `--save-to`, the change shown
  as a diff (`--stat`, `--name-only`, `--json` as usual). Expressions that can't be read are left as they are,
  with a warning each.
- `--check` changes nothing. It exits 1 if anything would change, like `prettier --check` or `fantomas --check`.
  For a model it lists the expressions that would change, one `<ref> <property>` per line. With `--json` it
  writes `{"check": true, "expressions", "unformatted": [{ref, kind, property}], "failed", "warnings"}`. For loose
  DAX, `--json` adds `"changed"`.

Not done: the formatter doesn't rewrite code. It doesn't qualify column references or remove table names from
measure references, as SQLBI's rules also ask; `lint` does those. It doesn't add or remove quotes around table
names either. Comments laid out as a column to the right of the code (`--|` boxes) keep their text and order,
but not their alignment.

### Connected mode (`connect`, `query`, `deploy`, remote models)

tess works on files and stays offline unless a command is given a server. Then it talks to that server over XMLA,
through Microsoft's own client libraries (AMO/TOM and ADOMD.NET). **This part has been built and tested against a
fake server only; it has not yet been run against a real one.** `docs/xmla-manual-test.md` is the checklist for
that.

```sh
export TESS_XMLA_TOKEN="$(az account get-access-token --resource https://analysis.windows.net/powerbi/api --query accessToken -o tsv)"
tess connect "powerbi://api.powerbi.com/v1.0/myorg/Finance"            # can I get in? which models are there?
tess info "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales"    # any read-only command, on the deployed model
tess diff "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" ./Sales.SemanticModel
tess convert "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" ./Sales.SemanticModel   # download as files
tess query "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" "EVALUATE TOPN(10, 'Date')"
tess query "xmla:...;Sales" -f totals.dax --json
tess deploy ./Sales.SemanticModel "powerbi://api.powerbi.com/v1.0/myorg/Finance" Sales          # dry run
tess deploy ./Sales.SemanticModel "powerbi://api.powerbi.com/v1.0/myorg/Finance" Sales --execute --overwrite
tess refresh "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" Orders --type full       # dry run
tess refresh "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales" Orders --type full --execute
```

#### Naming a server model

A model on a server is written `xmla:<server>;<database>`, wherever a command takes a model path. Quote it: `;`
ends a command in every shell, and workspace names often have spaces. The prefix is required, so a path can never
be mistaken for a server (`localhost:51234` and `host\instance` look exactly like paths) and no command opens a
connection unless its argument says so. `connect` and `deploy` take a server as its own argument, where the
prefix is optional.

| Server | Form | Works on |
|---|---|---|
| Power BI Premium / PPU / Fabric workspace | `powerbi://api.powerbi.com/v1.0/myorg/<workspace>` | Linux, macOS, Windows |
| Azure Analysis Services | `asazure://<region>.asazure.windows.net/<server>` | Linux, macOS, Windows |
| SSAS over HTTP (IIS `msmdpump.dll`) | `https://<host>/olap/msmdpump.dll` | Windows; elsewhere only with a user name and password |
| SSAS over TCP | `<host>`, `<host>:<port>`, `<host>\<instance>` | Windows only |
| Power BI Desktop's local model | `localhost:<port>` | Windows only |

The Windows-only rows are a limit of Microsoft's client library, which has no TCP transport on other systems.
tess says so before trying.

The database can be left out when the server has exactly one (as Power BI Desktop does). A wrong name is answered
with the names that exist. A full connection string also works after the prefix
(`xmla:Data Source=...;Initial Catalog=...;Timeout=60`); its extra properties are passed on. A connection string
with a `Password`, `User ID` or anything token-like is refused (exit 2): credentials never go on the command line.

#### Signing in

Credentials come from environment variables, or from files (or standard input) that variables point at. They are
never read from the command line, never printed, never written to JSON output, and removed from every error message
(tess scrubs the secrets it holds, and anything shaped like a password, bearer token or JWT).

| To sign in with | Set |
|---|---|
| an access token you already have | `TESS_XMLA_TOKEN`, or `TESS_XMLA_TOKEN_FILE=<path>` (`-` for standard input) |
| a service principal | `TESS_XMLA_CLIENT_ID`, `TESS_XMLA_TENANT_ID`, and `TESS_XMLA_CLIENT_SECRET` or `TESS_XMLA_CLIENT_SECRET_FILE`. Without those, `AZURE_CLIENT_ID`, `AZURE_TENANT_ID` and `AZURE_CLIENT_SECRET` are used |
| your own account (device code) | `TESS_XMLA_AUTH=device-code` and `TESS_XMLA_CLIENT_ID` (your own app registration); optionally `TESS_XMLA_TENANT_ID` |
| a user name and password (SSAS) | `TESS_XMLA_USER`, and `TESS_XMLA_PASSWORD` or `TESS_XMLA_PASSWORD_FILE` |
| the current Windows account (SSAS) | nothing |

`TESS_XMLA_AUTH` picks one explicitly: `auto` (the default), `token`, `spn`, `device-code`, `basic` or `integrated`.
With `auto`, a cloud server (`powerbi://`, `asazure://`) uses the token if there is one, then the service principal;
with neither, tess stops and says what to set. It never starts an interactive sign-in by itself. Other servers use
the user name and password if both are set, otherwise the current Windows account. A token or service principal in
the environment is never sent to a non-cloud server.

A token must be issued for the server's resource: `https://analysis.windows.net/powerbi/api` for Power BI,
`https://*.asazure.windows.net` for Azure Analysis Services. A static token isn't renewed; tess reads its expiry from
the token only to tell the client library.

Device-code sign-in prints a URL and a code on stderr and waits for you to enter the code in a browser, on any
device. It needs `TESS_XMLA_CLIENT_ID`: the application (client) ID of a public client app registration of your
own, with the Power BI Service delegated permission and "Allow public client flows" on. tess ships no application
ID. The sign-in lasts for that one command, unless you opt in to keeping it:

```sh
export TESS_XMLA_AUTH=device-code TESS_XMLA_CLIENT_ID=<your app> TESS_XMLA_TOKEN_CACHE=~/.cache/tessera/xmla-token-cache
```

`TESS_XMLA_TOKEN_CACHE` names a file where the sign-in (including a refresh token) is kept between runs. It is
created with mode 0600 on Linux and macOS; on Windows it gets its folder's permissions, so keep it in your profile.
Nothing else in tess ever writes a credential to disk. Delete the file to sign out.

#### What gets contacted

Only commands given a server contact anything, and only these, all through the client libraries:

- the server in the address;
- for `powerbi://` and `asazure://`, Microsoft's sign-in service (`login.microsoftonline.com`, or the national
  cloud's), unless you supply a token;
- the service's own name resolution: the Power BI workspace lookup on the address's host, and for `asazure://`
  the region's host and `global.asazure.windows.net`.

tess adds no telemetry and no other calls. The test suite checks, on the compiled assemblies, that
`Tessera.Core` uses no network type outside the explicit BPA rule download, that the CLI uses none itself, and that
in `Tessera.Xmla` only the live connector does.

#### Remote models in other commands

`info`, `list`, `get`, `find`, `diff`, `deps`, `validate`, `bpa`, `lint`, `script export` and `convert` accept a
remote model as their source. The model is read once and the connection closed; the result is the same as for the
model's files. `convert` from a remote model downloads it. `bpa` uses the built-in, user and `--rules` layers for a
remote model, but no project config (it has no folder).

The editing commands show their dry run against a remote model and can `--save-to` files, but `--save` is refused:
`deploy` is how a model gets onto a server. Commands that load models only from files (`format --save`,
`translations`, `perspective`, `script apply`, `bpa rules`) say so (exit 3) without connecting.

#### `connect`

`tess connect <server>` connects, and prints the product and version, how it signed in (never the credential), and
the databases with their compatibility levels. With `--json`, each database also has a `source` you can paste as a
model argument. Nothing is remembered: there are no sessions or profiles, and every command connects by itself.

#### `query`

```sh
tess query <xmla-model> "<DAX>"        # or: -f <file>, or - for standard input
tess query <xmla-model> -f q.dax --csv > out.csv
tess query <xmla-model> "EVALUATE 'Date'" --max-rows 0 --json
```

A query may hold several `EVALUATE` statements; each gives one result. The default output is an aligned table per
result, `--csv` is RFC 4180 with results separated by an empty line, and `--json` is described below. `--max-rows`
caps each result (default 1000, 0 for all); a capped result is marked `truncated` and stderr says so. Queries to a
server are sent as they are: tess doesn't check them first. On model files, `query` needs `--data <fixture>` and
runs offline: see [the local DAX engine](#the-local-dax-engine-query---data).

The JSON shape is shared with the local DAX engine, so the two can be compared value by value:

```json
{ "command": "query",
  "source": "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales",
  "results": [
    { "columns": [ { "name": "Date[Year]", "type": "int64" }, { "name": "[Total]", "type": "decimal" } ],
      "rows": [ [ 2023, 1234.5 ], [ 2024, null ] ],
      "rowCount": 2,
      "truncated": false } ],
  "durationMs": 212 }
```

- **`results`:** one entry per `EVALUATE`, in order.
- **`columns[].name`:** exactly as the engine names it (`Table[Column]`, `[Measure or expression name]`).
- **`columns[].type`:** `string`, `int64`, `double`, `decimal` (DAX currency), `dateTime`, `boolean`, `binary`, or
  `variant` when the engine doesn't declare one type for the column.
- **`rows`:** one array per row, values in column order. Row order is the engine's; without `ORDER BY` it is not
  defined, so compare such results as multisets.
- **`rowCount`:** the rows present. **`truncated`:** `--max-rows` cut the result short.

Each value has exactly one spelling:

| Value | JSON |
|---|---|
| BLANK | `null` (an empty string is `""`: they stay distinct) |
| `int64` | an integer: `42` |
| `double` | a number with a `.` or exponent, shortest digits that round-trip: `1.0`, `0.1`, `1E+21`. `NaN`, `Infinity` and `-Infinity` are those strings |
| `decimal` | a number with exact digits and at least one decimal place, no exponent, no trailing zeros: `1234.5`, `12.0` |
| `dateTime` | a string, ISO 8601 with no zone: `"2024-01-31T00:00:00"`; a fraction only if there is one |
| `boolean` | `true` / `false` |
| `string` | a string |
| `binary` | a base64 string |

Read `decimal` values with a decimal type; a parser that goes through binary floating point can lose digits. In a
`variant` column a number with a decimal point may be a `double` or a `decimal`. In text and CSV output BLANK is
empty, booleans are `TRUE`/`FALSE`, and CSV writes an empty string as `""`. The type is `Tessera.QueryResult` in
`Tessera.Core`; `QueryValue.toJson` is the encoder.

#### `deploy`

```sh
tess deploy <model> <server> <database>            # dry run: connects and reads, changes nothing
tess deploy <model> <server> <database> --execute  # deploys; an existing database also needs --overwrite
```

`deploy` always connects, reads the database if it exists, and works out the model the server would hold
afterwards. A dry run prints what would change (the same diff as `tess diff`, with `--stat`, `--name-only` and
`--ignore`), then the TMSL `createOrReplace` script it would run. `--tmsl` prints only the script; `--json` gives
`changes`, `kept`, `warnings`, `introducedErrors`, `tmsl`, `creates`, `dryRun` and `deployed` in one document.
`<model>` can be another `xmla:` model, to copy between servers.

By default a deployment replaces the model's metadata and leaves alone what belongs to the server:

| Kept from the server unless | Option |
|---|---|
| partitions and refresh policies of tables it already has, so refresh state and incremental-refresh partitions survive | `--deploy-partitions` (with `--skip-refresh-policy`: still keep them for tables with a refresh policy) |
| data sources | `--deploy-connections` |
| shared (M) expressions | `--deploy-shared-expressions` |
| roles | `--deploy-roles` |
| role members | `--deploy-role-members` (implies `--deploy-roles`) |

`--deploy-full` is all of them. The changes shown are against that merged model, so a dry run doesn't report
partitions or roles that would in fact stay. Tables new to the server get the model's partitions. A database that
doesn't exist yet takes everything from the model except role members.

Two things stop `--execute`:
- the database exists and `--overwrite` isn't given (exit 4);
- the deployed model would have validation errors the current one doesn't, without `--force` (exit 1). The dry run
  lists them.

If the server rejects the script or reports errors running it, that is exit 5 with its messages. After a
deployment, new or changed tables may need a refresh: see `refresh` below (`deploy` doesn't run one).

Where this differs from the Tabular Editor tools, and why:

- **Arguments.** `te deploy` names the source with `-s`/`-d` and the target with `--target-server`. tess takes the
  model as its first argument, like every other command, then the server and database.
- **`--execute` doesn't prompt.** `te` asks for confirmation and wants `--execute --force` in CI. tess never prompts;
  `--force` means only "despite new errors".
- **Overwriting needs `--overwrite`**, as in TE2 (`-O`). `te` overwrites unless `--create-only`.
- **A new database gets the model's roles** even without `--deploy-roles`. TE2 would create it without roles,
  leaving row-level security off.
- **The server's data sources are kept even when the model has none.** TE2 keeps them only if the model has some.
- **Passwords in the model's data sources are not deployed.** The script is written without restricted
  information, so a dry run never prints a credential; TE2 includes it. After `--deploy-connections` (or creating a
  database with data sources), set their credentials on the server.
- **Not done:** TE2's two-step deployment for a table or column that changes between calculated and loaded (tess
  warns instead), a BPA gate, skipping linguistic metadata, deployment profiles, and refreshing afterwards.

#### `refresh`

```sh
tess refresh "xmla:<server>;<database>"                               # dry run: the whole model, type automatic
tess refresh "xmla:<server>;<database>" Sales "partition:Orders/2024" --type full
tess refresh "xmla:<server>;<database>" Sales --type full --execute   # refreshes
```

`refresh` connects, reads the model to find what the selectors name, and prints what the refresh covers and the
TMSL script it would run. Nothing is refreshed without `--execute`. `--tmsl` prints only the script, and `--json`
gives one document either way. It only runs on a server model: model files hold no data (exit 2, without
connecting). **Like the rest of connected mode, `refresh` has never been run against a real server.**

What to refresh comes after the model, in the selector syntax of the other commands. Nothing, or `model`, is the
whole model. A table is `Sales`, `'Sales'` or `table:Sales`. A partition is `partition:Sales/2024` or
`'Sales'[2024]`; a bare name that no table has is taken as a partition if exactly one has it. A table and its only
partition often share a name: the bare name is the table. Names are matched without regard to case and written
into the script as the server has them. A selector that names something else (a measure, a column) is exit 3.
A partition of a table that is also named is dropped (with a warning), as is everything else when the model is.

| `--type` | Does | Takes |
|---|---|---|
| `automatic` (default) | loads and recalculates only what isn't in a ready state | model, tables, partitions |
| `full` | reloads the data and recalculates everything that depends on it | model, tables, partitions |
| `dataOnly` | reloads the data, without recalculating | model, tables, partitions |
| `calculate` | recalculates what needs it; loads nothing | model, tables, partitions |
| `clearValues` | removes the data | model, tables, partitions |
| `defragment` | cleans up column dictionaries | model, tables |
| `add` | appends what a partition's query returns to the rows it has | partitions |

Those are all the types TMSL documents; any other is a usage error. The "Takes" column is Microsoft's reference,
and tess refuses the rest (exit 3) before running anything, where the client library would script it and leave it
to the server.

- **`--max-parallelism <n>`** wraps the refresh in a TMSL `sequence` with `maxParallelism`, so at most `n` objects
  are processed at once. Without it the server decides.
- **`--apply-refresh-policy` / `--no-apply-refresh-policy`** write `applyRefreshPolicy` into the script. With
  neither, the property is left out and the server applies the policy of tables that have one (its default). With
  `--no-apply-refresh-policy`, a table's partitions are refreshed as they are, which is how to reload a whole
  incremental-refresh table. Power BI only.
- **`--effective-date YYYY-MM-DD`** is the date an applied policy treats as today (and implies applying it). It is
  written as `MM/DD/YYYY`, the form in Microsoft's reference.
- The refresh-policy options apply to tables, so they can't be combined with a partition (exit 3). The dry run
  warns when a policy will be applied (the server may add, merge and remove partitions), and when the options are
  given but can't matter (no table in the refresh has a policy, or the type ignores policies).

With `--execute`, tess writes `Refreshing...` to stderr, then progress as the server reports it, then
`Refreshed '<database>' on <server> in <seconds>`. If the server rejects the script or reports errors running it,
that is exit 5 with its messages (`refresh-failed` in JSON, where the document still has `progress`).

Progress comes from a trace of tess's own session (the client library's `Server.SessionTrace`). `--progress` picks
how much:

| `--progress` | Shows |
|---|---|
| `objects` (default) | the steps that finish once per table or partition (the source query, rows read, processing, the commit) and every error |
| `all` | every "Progress Report" event the server sends (begin, current, end, error), including the per-column ones |
| `none` | nothing: the session isn't traced, and only the start, finish and duration are reported |

```
Refreshing...
  [ReadData] Finished reading data for the 'Sales' table. (1,234,567 rows, 12.3s)
  [Process] Finished processing partition 'Sales' of table 'Sales'. (13.0s)
Refreshed 'Sales' on powerbi://api.powerbi.com/v1.0/myorg/Finance in 14.2s.
```

The text in each line is the server's own. If the trace can't be started or stops, the refresh still runs and tess
says why there is no progress (`traceError` in JSON). Which events a server sends, and what they say, has only been
checked against a fake: use `--progress all` to see what yours sends, and `--progress none` if the trace itself
gets in the way.

`--json` has `source`, `server`, `database`, `type`, `dryRun`, `refreshed`, `objects` (what the script names, each
with a `ref`), `covers` (per table: `partitions`, `wholeTable`, `refreshPolicy`), `applyRefreshPolicy`,
`effectiveDate` (ISO), `maxParallelism`, `warnings`, `tmsl`, `serverMessages`, `progress` (each event's `phase`,
`step`, `object`, `objectPath`, `text`, `integerData`, `durationMs`), `traceError` and `durationMs`. With `--json`
nothing is written to stderr.

Where this differs from `te refresh`, and why:

- **Objects are selectors after the model**, not `--table` and `--partition T.P`, as in every other tess command.
- **`--execute` doesn't prompt**, as with `deploy`.
- **`--apply-refresh-policy` takes no value.** `te` accepts `true`, `false` or a table name; tess has the pair of
  flags, and one script applies one setting to every table in it. Run two refreshes to treat tables differently.
- **Not done:** refresh override profiles (TMSL `overrides`: other data sources, queries or parameters for one
  refresh), writing the trace to a file (`--trace <path>`), VertiPaq statistics after a refresh, cancelling a
  running refresh (interrupting tess closes the session; what the server then does hasn't been observed), and
  waiting for trace events the server sends after the script returns (they are dropped).

#### Not in tess yet

`vertipaq`, sessions and profiles (`te connect` keeps an active connection; tess doesn't), interactive
browser sign-in, and certificate and managed-identity sign-in.

What each needs, for whoever picks it up:
- **Refresh overrides:** `Refresh.Request` (Core) would carry the TMSL `overrides` array; the builder writes it
  after `objects`. `IXmlaSession.ExecuteTraced` is the trace seam, if something else needs server events.
- **`vertipaq`:** DMV queries (`$SYSTEM.DISCOVER_STORAGE_TABLE_COLUMNS` and friends). `IXmlaSession.Query` runs
  them as it stands; Dax.Model.Extractor would instead need the raw connection, which the seam deliberately hides.
- **Sessions and profiles:** somewhere to keep an address and auth mode (never a secret) per shell, and a decision
  on where; then `Source.load` reads it when no model is given.
- **Interactive sign-in:** needs the MSAL broker the single-file builds leave out (see `Tessera.Cli.fsproj`).
### Fixture data (`data`)

A fixture is a directory of data files, one per table, that the local DAX engine evaluates a model against: no
server, no refresh. `tess data` creates one, checks it against the model, and summarizes it. `tess query <model>
--data <dir>` runs queries over it: see [the local DAX engine](#the-local-dax-engine-query---data). `tess test`
runs a model's tests over it: see [testing a model](#testing-a-model-test).

**Fixture data never leaves your machine.** tess reads the files from the directory and does nothing else with
them: no upload, no telemetry, no network call of any kind.

```sh
tess data init  ./Sales.pbip ./fixtures                      # a header-only CSV per table, to fill in
tess data init  ./Sales.pbip ./fixtures --rows 1000 --seed 7 # ... or 1000 rows of synthetic data per table
tess data check ./Sales.pbip ./fixtures                      # files, columns, types, keys, relationships
tess data info  ./Sales.pbip ./fixtures --json               # tables, row counts, distinct counts per column
tess data embed ./Sales.pbip ./fixtures ./with-data.bim      # the model with the fixture's rows inside it
```

#### The format

- **One file per table:** `<Table>.csv` or `<Table>.json`, directly in the directory. Other files and
  subdirectories are ignored. File names match table names ignoring case.
- **Which tables:** every table except calculated tables and calculation groups, which the engine computes. A
  table with no file is empty; `check` lists those in one warning.
- **Which columns:** the table's data columns, by their model names (not the source column names), in any order,
  matched ignoring case. Calculated columns are computed by the engine: if a file has one it is ignored, with a
  warning. A data column the file leaves out is BLANK in every row, with a warning.

Table names that can't be file names are escaped with `%XX` (the UTF-8 bytes in hex), so the same directory works
on Windows, macOS and Linux:

| In the table name | In the file name |
|---|---|
| `%` | `%25` |
| `< > : " / \ \| ? *` and control characters | `%3C` `%3E` `%3A` `%22` `%2F` `%5C` `%7C` `%3F` `%2A`, `%09`... |
| a trailing `.` or space | `%2E`, `%20` |
| a Windows device name (`CON`, `PRN`, `AUX`, `NUL`, `COM1`.., `LPT1`..), alone or before a dot | its first letter escaped: `%43ON` |

Everything else is kept as it is, so `Invoiced Sales` is `Invoiced Sales.csv` and `Ventes/Région` is
`Ventes%2FRégion.csv`. `data init` writes the right names. Reading accepts any `%XX`.

**CSV** is RFC 4180: comma-separated, UTF-8 (a byte-order mark is fine), LF or CRLF line ends, the first line
naming the columns.

- A field with a comma, a quote or a line break goes in double quotes; a quote inside quotes is written twice.
- **An empty field is BLANK.** In a text column, `""` (quoted) is the empty string, which DAX tells apart from
  BLANK with `==` and `ISBLANK`. In other columns `""` is BLANK too.
- Nothing is trimmed, except what the engine itself drops: trailing spaces of text.
- A row with more or fewer fields than the header is an error. Empty lines are skipped (in a one-column file an
  empty line is a BLANK row).
- Values are written in the invariant culture, whatever your locale:

| Column data type | Written as | Examples |
|---|---|---|
| Whole number (`int64`) | digits, optional sign | `42`, `-7` |
| Decimal number (`double`) | decimal point, optional exponent; no `NaN` or `Infinity` | `1.5`, `-2.5e-3` |
| Fixed decimal number (`decimal`) | decimal point, at most 4 decimals | `499.95`, `0.0001` |
| True/false (`boolean`) | `true` / `false` (any case), or `1` / `0` | `true` |
| Date/time (`dateTime`) | ISO 8601: a date, a date and time (`T` or a space), or a time | `2024-01-31`, `2024-01-31T13:05:00`, `13:05` |
| Text (`string`) | as it is | `Bike`, `"Lock, heavy"` |

A date/time may end in `Z`, which is dropped: values are never shifted between time zones. Other UTC offsets are
refused. A time alone is that time on 1899-12-30, which is how the engine stores a time.

**JSON** is an array of row objects, for when you want the types explicit:

```json
[
  { "ProductKey": 1, "Name": "Bike", "Price": 499.95, "Active": true, "Launched": "2020-03-01" },
  { "ProductKey": 2, "Name": null, "Price": "49.0000" }
]
```

`null` and a left-out property are BLANK. A JSON value must be of the column's kind: a number for the number
types, `true`/`false` for True/false, a string for text and dates. A JSON string is also accepted for any type and
is read exactly as the same text in a CSV field (so `"9007199254740993"` keeps an integer that a JSON number might
not, and `""` is BLANK except in a text column). Comments and trailing commas are allowed.

As in the engine, text is case-insensitive: values that differ only in case are one value, shown in the casing
that comes first in the file, and `"A"` matches `"a"` across a relationship. A model whose `collation` is
case-sensitive or binary (`..._CS_...`, `..._BIN2`) is loaded case-sensitively. Accents always count.

#### `data init`

`tess data init <model> <dir>` writes `<Table>.csv` for every table that takes data: the header row only, or with
`--rows N`, N rows of synthetic data. The files are UTF-8 without a byte-order mark, with LF line ends.

The synthetic data is made to be queryable, not realistic:
- A key, a column marked unique, and the one side of a relationship get a different value in every row.
- The many side of a relationship gets values of its one side, so no row is left without a match.
- Other columns get small numbers, a few repeating texts (`Colour 3`), and dates from 2020 to 2024. Nothing is
  BLANK.
- A table gets fewer rows where it must: a unique True/false column allows two.

The rows depend only on the model, `--rows` and `--seed` (default 1): the same command writes the same bytes on
any machine. `data init` won't replace a table's file, of either format, without `--force` (exit 4); other files
in the directory are left alone. Where a relationship ends at a calculated table (a `CALENDAR()` date table, say), the
key on the fixture's side is generated on its own, and may not match what the engine computes.

#### `data check`

`check` reads the fixture and reports findings as `severity  ref  file:line  message  [rule]`, with a summary
line. `--min-severity`, `--fail-on` (default `error`), `--rule` and `--ci` work as in
[`validate`](#validation-validate): exit 1 if any issue is at or above `--fail-on`, 3 if the directory or model
can't be read.

| Rule | Severity | Reports |
|---|---|---|
| `unknown-table` | error | a `.csv` or `.json` file whose name is no table of the model |
| `duplicate-file` | error | more than one file for a table (`Sales.csv` and `Sales.json`); neither is loaded |
| `calculated-table` | warning | a file for a calculated table or calculation group: ignored |
| `missing-table` | warning | the tables with no file, in one finding |
| `malformed-file` | error | a file that isn't UTF-8, has no header, or stops being CSV or JSON; with the line |
| `malformed-row` | error | a row with the wrong number of fields, or a JSON row that isn't an object |
| `unknown-column` | error | a column the table doesn't have |
| `duplicate-column` | error | a column twice in a header or a JSON row |
| `ignored-column` | warning | a calculated column in a file |
| `missing-column` | warning | a data column the file doesn't have |
| `invalid-value` | error | a value that isn't of its column's data type, with file, line and column |
| `duplicate-key` | error | a value that repeats on the one side of a relationship |
| `blank-key` | error | a BLANK on the one side of a relationship, or in the table's key column |
| `unmatched-key` | warning | rows on the many side with no match on the one side |
| `not-unique` | error | a value that repeats in a column marked as key or unique |
| `unchecked-relationship` | info | a relationship that ends at a calculated table or column, so can't be checked |

`unmatched-key` is a warning because DAX has an answer for it: those rows belong to the blank row of the one-side
table, and the engine will put them there. An invalid value is reported once, as `invalid-value`, and not again
as a BLANK or unmatched key. After 10 invalid values in one column of one file, the rest are counted in a single
finding.

`--json` writes the [findings envelope](#findings-json-and-ci-annotations) with `"command": "data check"` and
`"source": "data"`. Findings also carry `file` (full path), `line` and `columnName` where they apply, and the
envelope adds `"valid"` (no errors), `"directory"` and `"tables": [{table, file, rows}]`.

#### `data embed`

```sh
tess data embed ./Sales.pbip ./fixtures ./Sales-with-data.bim
```

`embed` writes a copy of the model that carries the fixture inside it: every loaded table becomes a calculated
table whose expression is a `DATATABLE` of the fixture's rows. The copy needs no data source, so it can be
deployed to any workspace (`tess deploy`, then refresh it there) and answers queries the way the model does
locally against the fixture. That is how the [conformance corpus](#the-dax-conformance-corpus) is run on a real
server.

- **Kept:** every column with its name, type and properties, so relationships, sort-by columns, hierarchies,
  measures and calculated columns still resolve. Calculated tables and calculation groups are untouched.
- **Removed:** partitions, refresh policies, data sources and shared (M) expressions: what only a refresh needs.
- **Values:** numbers in plain digits, text quoted, `TRUE`/`FALSE`, `BLANK ()`, dates as `dt"2024-01-31T13:05:00"`.
  An empty table keeps its columns and types. Where a constant can't be exactly the value, stderr says so (and
  `--json` lists it under `warnings`): a date/time loses a fraction of a second, a fixed decimal with more than
  15 digits may be rounded by the server, and a table with no data columns gets a hidden column `_`.

The output format follows the destination as for `convert` (`--format bim|tmdl|pbip`), and like `convert` it
refuses a destination that isn't empty without `--force` (exit 4). A fixture with errors is refused (exit 3):
its invalid values would be deployed as BLANK. `--json` writes `{"command": "data embed", "directory", "output":
{format, path, files}, "tables": [{table, rows, columns}], "warnings": [...]}`.

#### `data info`

`info` lists each table with its file and row count, and each column with its data type, the number of distinct
values, and the number of BLANK rows. BLANK counts as one of the distinct values. `--json` writes `{"command":
"data info", "directory", "tables": [{table, ref, file, rows, columns: [{name, ref, dataType, distinct, blanks}]}],
"computed": [...], "issues": {errors, warnings}}`. `info` exits 0 even when the fixture has errors (it says so on
stderr); use `check` to see them.

### The local DAX engine (`query --data`)

`tess query` on model files evaluates DAX on your machine, over [fixture data](#fixture-data-data): no server, no
refresh, no network.

```sh
tess query ./Sales.pbip --data ./fixtures "EVALUATE SUMMARIZECOLUMNS ( 'Date'[Year], \"Sales\", [Total] )"
tess query ./Sales.pbip --data ./fixtures -f totals.dax --json      # or - for standard input; --csv; --max-rows
tess query ./Sales.pbip --coverage                                  # what the engine can evaluate of this model
tess query ./Sales.pbip --data ./fixtures --now 2024-03-05T14:30:00 "EVALUATE ROW ( \"d\", TODAY () )"   # a fixed clock
```

The options, the output and the JSON are those of [`query` on a server](#query): the same `QueryResult`, with
`"source"` the model's path. The fixture is checked first (as `tess data check` does) and a fixture with errors is
refused (exit 3, `fixture-has-errors`).

**What it is.** An independent reimplementation of DAX's semantics, written against the public documentation:
Microsoft Learn's DAX reference, [dax.guide](https://dax.guide) and SQLBI's articles, cited in the code next to each
rule. **It has not been checked against a real Analysis Services engine yet.** Its tests are hand-worked expected
values with their sources, identities DAX guarantees, and a check of filter propagation against its definition on
random models; a conformance corpus against published results is the next step.

**The one rule: never a wrong answer.** A function, an argument form, a data-type combination or a model feature the
engine doesn't implement exactly is an error, `not-supported` (exit 3), naming the thing and where it is:

```
tess: Not supported by the local DAX engine: the function PATHITEM (measure 'Sales'[Label] 2:5)
```

That includes the places where the sources don't settle what the real engine does. A subset that is right is the
point; so expect refusals, and read them as "not yet" rather than as a problem with your DAX. `IFERROR` never hides a
refusal. The other errors are `dax-syntax` (with line and column), `dax-invalid` (an unknown name, the wrong
arguments, a circular dependency) and `dax-error` (an error DAX itself raises: `ERROR ( )`, a failed conversion).

#### What it supports

- **Values:** whole, decimal and fixed decimal numbers, text, true/false, dates and BLANK, with DAX's rules for
  each operator (`BLANK + 5` is 5, `BLANK * 5` is BLANK, `BLANK = 0` but not `BLANK == 0`, `5 / 0` is infinity, text
  compares without case). `&&`, `||`, `IF`, `SWITCH` and `COALESCE` evaluate only what they need; `IF.EAGER`
  evaluates both branches. Whole-number `+`, `-` and `*` wrap around past 64 bits as the engine's do (SQLBI,
  "Rounding errors with different data types"); `ISERROR` and `IFERROR` treat infinity and NaN as errors; a BLANK
  written as a branch of an `IF` whose other branch is TRUE/FALSE is FALSE.
- **Tables as values, with data lineage:** a column of a table expression remembers the model column it came from,
  which is what makes `VALUES ( T[c] )` usable as a filter, what `TREATAS` assigns, and what an expression loses.
  Result columns are named as the engine names them (`Sales[Amount]`, `[Total]`, `[Value]`) and typed statically
  where DAX types them so (a column that is BLANK in every row still has its type; an `IF` with a whole and a decimal
  branch is a decimal; one with a text and a number is a variant).
- **Evaluation contexts:** the filter context; nested row contexts with `EARLIER`; context transition in
  `CALCULATE` and in every measure reference, covering the expanded table and keeping duplicate rows together;
  variables evaluated where they are defined, on first use, once.
- **Relationships:** many-to-one, single column. Filters travel from the one side; both ways where the model says
  so or under `CROSSFILTER`; inactive relationships under `USERELATIONSHIP`; expanded tables (a table used as a
  filter filters the tables it relates to, and `ALL ( Sales )` removes filters from them); the blank row for keys
  with no match (`VALUES` and `ALL` have it, `DISTINCT`, `ALLNOBLANKROW` and a plain table reference don't).
- **`CALCULATE` / `CALCULATETABLE`:** the documented order (filter arguments in the original context, context
  transition, modifiers, then the filters); True/False filters over one or several columns of a table; table
  filters; `KEEPFILTERS`; `ALL`, `ALLEXCEPT`, `ALLNOBLANKROW`, `REMOVEFILTERS`, `ALLCROSSFILTERED`,
  `USERELATIONSHIP`, `CROSSFILTER` (NONE, ONEWAY, BOTH); arbitrarily shaped filters, which lose their pairing when
  one of their columns is replaced.
- **Queries:** `DEFINE MEASURE` (overriding a model measure for the query's own code only), `DEFINE VAR`,
  `DEFINE FUNCTION`, `DEFINE COLUMN` (a calculated column on a model table, for the query) and `DEFINE TABLE` (a
  calculated table, for the query), several `EVALUATE` statements, `ORDER BY` by result columns or expressions,
  `START AT` (the rows before the values, in the order of the keys and their directions, are skipped).
- **User-defined functions,** the model's and the query's, with `VAL` parameters (evaluated once, in the caller's
  context, and converted to the parameter's type) and `EXPR` parameters (evaluated where the body uses them).
- **Calculated columns and calculated tables,** computed in dependency order when the model is loaded. A
  calculated table may read the tables related to it, as a date table does
  (`CALENDAR ( MIN ( Sales[Date] ), MAX ( Sales[Date] ) )`). A circular
  dependency is an error, including the classic one: two calculated columns that each use `CALCULATE` in a table
  with no unique column. A calculated column or table that uses something unsupported doesn't stop the rest: only
  the queries that read it fail, with its error.
- **Functions** (anything else is `not-supported`):

| Family | Functions |
|---|---|
| Aggregation | `AVERAGE` `AVERAGEA` `AVERAGEX` `CONCATENATEX` `COUNT` `COUNTA` `COUNTAX` `COUNTBLANK` `COUNTROWS` `COUNTX` `DISTINCTCOUNT` `DISTINCTCOUNTNOBLANK` `MAX` `MAXA` `MAXX` `MIN` `MINA` `MINX` `PRODUCTX` `RANKX` `SUM` `SUMX` |
| Date and time | `CALENDAR` `CALENDARAUTO` `DATE` `DATEDIFF` `DATEVALUE` `DAY` `EDATE` `EOMONTH` `HOUR` `MINUTE` `MONTH` `NETWORKDAYS` `NOW` `QUARTER` `SECOND` `TIME` `TIMEVALUE` `TODAY` `UTCNOW` `UTCTODAY` `WEEKDAY` `WEEKNUM` `YEAR` `YEARFRAC` |
| Filter | `ALL` `ALLCROSSFILTERED` `ALLEXCEPT` (also as a table) `ALLNOBLANKROW` `CALCULATE` `CALCULATETABLE` `CROSSFILTER` `DISTINCT` `EARLIER` `EARLIEST` `FILTER` `HASONEFILTER` `HASONEVALUE` `ISCROSSFILTERED` `ISFILTERED` `KEEPFILTERS` `REMOVEFILTERS` `SELECTEDVALUE` `TREATAS` `USERELATIONSHIP` `VALUES` |
| Information | `CONTAINS` `CONTAINSROW` `CUSTOMDATA` `ISBLANK` `ISEMPTY` `ISERROR` `ISEVEN` `ISINSCOPE` `ISLOGICAL` `ISNONTEXT` `ISNUMBER` `ISODD` `ISTEXT` `USERCULTURE` `USERNAME` `USEROBJECTID` `USERPRINCIPALNAME` |
| Logical | `AND` `BLANK` `COALESCE` `ERROR` `FALSE` `IF` `IF.EAGER` `IFERROR` `NOT` `OR` `SWITCH` `TRUE` |
| Math | `ABS` `CEILING` `CONVERT` `CURRENCY` `DIVIDE` `EVEN` `EXP` `FACT` `FLOOR` `GCD` `INT` `ISO.CEILING` `LCM` `LN` `LOG` `LOG10` `MOD` `MROUND` `ODD` `PI` `POWER` `QUOTIENT` `ROUND` `ROUNDDOWN` `ROUNDUP` `SIGN` `SQRT` `SQRTPI` `TRUNC` |
| Relationship | `LOOKUPVALUE` `RELATED` `RELATEDTABLE` |
| Statistical | `GEOMEAN` `GEOMEANX` `MEDIAN` `MEDIANX` `PERCENTILE.EXC` `PERCENTILE.INC` `PERCENTILEX.EXC` `PERCENTILEX.INC` `RANK.EQ` `STDEV.P` `STDEV.S` `STDEVX.P` `STDEVX.S` `VAR.P` `VAR.S` `VARX.P` `VARX.S` |
| Table manipulation | `ADDCOLUMNS` `CROSSJOIN` `CURRENTGROUP` `DATATABLE` `EXCEPT` `GENERATE` `GENERATEALL` `GROUPBY` `IGNORE` `INTERSECT` `NATURALINNERJOIN` `NATURALLEFTOUTERJOIN` `NONVISUAL` `ROLLUPADDISSUBTOTAL` `ROLLUPGROUP` `ROW` `SELECTCOLUMNS` `SUMMARIZE` `SUMMARIZECOLUMNS` `TOPN` `UNION` |
| Text | `COMBINEVALUES` `CONCATENATE` `CONTAINSSTRING` `CONTAINSSTRINGEXACT` `EXACT` `FIND` `FIXED` `FORMAT` `LEFT` `LEN` `LOWER` `MID` `REPLACE` `REPT` `RIGHT` `SEARCH` `SUBSTITUTE` `TOCSV` `TOJSON` `TRIM` `UNICHAR` `UNICODE` `UPPER` `VALUE` |
| Time intelligence | `CLOSINGBALANCEMONTH` `CLOSINGBALANCEQUARTER` `CLOSINGBALANCEYEAR` `DATEADD` `DATESBETWEEN` `DATESINPERIOD` `DATESMTD` `DATESQTD` `DATESYTD` `ENDOFMONTH` `ENDOFQUARTER` `ENDOFYEAR` `FIRSTDATE` `FIRSTNONBLANK` `FIRSTNONBLANKVALUE` `LASTDATE` `LASTNONBLANK` `LASTNONBLANKVALUE` `NEXTDAY` `NEXTMONTH` `NEXTQUARTER` `NEXTYEAR` `OPENINGBALANCEMONTH` `OPENINGBALANCEQUARTER` `OPENINGBALANCEYEAR` `PARALLELPERIOD` `PREVIOUSDAY` `PREVIOUSMONTH` `PREVIOUSQUARTER` `PREVIOUSYEAR` `SAMEPERIODLASTYEAR` `STARTOFMONTH` `STARTOFQUARTER` `STARTOFYEAR` `TOTALMTD` `TOTALQTD` `TOTALYTD` |

  `SUMMARIZE` takes a table, group-by columns (its own, or of tables it has a many-to-one relationship to) and
  name/expression pairs; not `ROLLUP`. `SUMMARIZECOLUMNS` takes group-by columns, filter tables (with
  `KEEPFILTERS`) and name/expression pairs, anywhere outside a row context: in a query, in `CALCULATETABLE`, in a
  measure. Existing combinations for columns of one table; group-by tables that a filter table spans together are
  grouped by the combinations it has, the others cross joined; the blank row is a group; rows whose expressions are
  all BLANK (except `IGNORE`d ones) are dropped. `ROLLUPADDISSUBTOTAL` (with `ROLLUPGROUP` and its flag columns) adds
  the subtotal rows, and `ISINSCOPE` tells them apart. Not `ROLLUP`, `ROLLUPISSUBTOTAL`, `NONVISUAL`, nor a rollup
  with a group-level filter. `NATURALINNERJOIN` and `NATURALLEFTOUTERJOIN` join tables with no model columns
  (`DATATABLE`, table constructors, computed columns) on their columns of the same name. `FIRSTNONBLANK` and its
  relatives go through the column's values in ascending order.

#### Dates, times and the clock

A date is a number of days since December 30, 1899, with the time of day as its fraction, on the real calendar
on both sides of 1900: DAX is not Excel, and February 1900 has 28 days. `DATE`, `TIME`, `DATEVALUE`, `TIMEVALUE`,
`EOMONTH`, `EDATE`, `TODAY`, `NOW`, `UTCTODAY`, `UTCNOW` and the `[Date]` column of `CALENDAR` and `CALENDARAUTO`
are dates (`dateTime` in results); `YEAR`, `QUARTER`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECOND`, `WEEKDAY`,
`WEEKNUM`, `DATEDIFF` and `NETWORKDAYS` are whole numbers; `YEARFRAC` is a decimal number.

- **The clock.** `TODAY`, `NOW`, `UTCTODAY` and `UTCNOW` return one instant, read once when the model is loaded:
  the calculated tables and columns and the query all see the same one. By default it is the machine's clock
  (local time for `TODAY` and `NOW`, UTC for the other two). `--now <ISO 8601>` sets it: `--now 2024-03-05`,
  `--now 2024-03-05T14:30:00`. The date and time as written is what `NOW` returns. With a UTC offset
  (`2024-03-05T14:30:00+02:00`, `...Z`) the UTC functions return that instant in UTC; without one they return the
  same as `NOW`. The machine's time zone is never used with `--now`, so a result is the same everywhere. In code:
  `Engine.prepareWith { Engine.Options.defaults with Clock = Some clock } db store`.
- **`DATE`** follows dax.guide's rules, each of which it shows a result for: a year from 0 to 49 has 2000 added and
  from 50 to 99 has 1900; months above 12 and at or below 0 move the year (`DATE ( 2008, 0, 2 )` is December 2,
  2007); days past the month's end and negative days move the month; day 0 and a year above 9999 are errors;
  decimals round; BLANK is 0. **`TIME`** wraps at 24 hours and takes 0 to 32767 in each argument.
- **`WEEKDAY`** takes return types 1, 2, 3 and 11 to 17; **`WEEKNUM`** 1, 2, 11 to 17 (the week with January 1 is
  week 1) and 21 (ISO 8601). **`DATEDIFF`** counts boundaries crossed, not whole intervals, with weeks from Sunday,
  and is negative when the first date is the later one. **`EOMONTH`** rounds its months and **`EDATE`** truncates
  them, as their pages say. **`NETWORKDAYS`** takes the weekend numbers 1 to 7 and 11 to 17 and a table of
  holidays. **`CALENDARAUTO`** scans every date column that is not a calculated column or in a calculated table,
  and takes the month a fiscal year ends in.
- **Text as a date.** The engine reads text by the model's culture and then tries other formats, most of which is
  written down nowhere. The local engine reads ISO 8601 text under any culture (`"2024-01-31"`,
  `"2024-01-31T13:45:00"`, `"13:45"`), and under an `en-US` model also the forms the documentation shows results
  for: `"10/15/2020"` and `"10-15-2020"` (day first when month first is no date: `"28/02/2020"`),
  `"March 3, 2008 3:00 PM"`, `"Feb 14, 2010"`. Any other text is refused.
- **Refused, because the sources leave it open or disagree:** `YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECOND`,
  `WEEKDAY`, `WEEKNUM`, `EOMONTH`, `EDATE` and `YEARFRAC` of BLANK (only `QUARTER`, `DATEDIFF` and `NETWORKDAYS`
  say what BLANK gives); the calendar parts of a plain number below 61 (Microsoft Learn dates such a number as
  Excel does, a day later than the zero date gives); `DATE` with a negative year; `HOUR`, `MINUTE` or `SECOND`
  where a fraction of a second would change the answer if it were rounded; `EOMONTH` and `EDATE` of a date with a
  time of day; `EDATE` before March 1900; `YEARFRAC` on the actual/actual basis, with a start after the end, or
  on a 30/360 basis with a date on the 31st or the last day of February; `DATEDIFF` where a fraction of a second
  would change the answer if it were rounded; `NETWORKDAYS` with a start after the end, a time of day, or
  dates as text; return types and weekend numbers the pages don't list.

#### Text functions and FORMAT

`LEN`, `LEFT`, `RIGHT`, `MID`, `UPPER`, `LOWER`, `TRIM`, `REPT`, `SUBSTITUTE`, `REPLACE`, `SEARCH`, `FIND`, `EXACT`,
`CONTAINSSTRING`, `CONTAINSSTRINGEXACT`, `COMBINEVALUES`, `UNICHAR`, `UNICODE`, `CONCATENATE`, `FIXED` and `FORMAT`
are implemented for **en-US**. The model's culture is read from the model; `FORMAT`'s locale argument overrides it.

- **Culture.** Any culture other than en-US is refused, for `FORMAT` (model culture or locale argument), `FIXED`,
  `UPPER`/`LOWER`, and for a number with decimals turned into text by `&` or a text function (the decimal separator
  is the culture's). A model that names no culture is refused too: a server would use its own language. Nothing is
  ever formatted by .NET, so the machine's culture changes nothing.
- **`FORMAT` numbers.** `General Number`, `Currency`, `Fixed`, `Standard`, `Percent`, `Scientific`, `Yes/No`,
  `True/False`, `On/Off`, and custom formats: `0` `#` `.` `,` (grouping, and scaling by 1000 right before the decimal
  point), `%`, `E+`/`E-`/`e+`/`e-`, quoted and `\`-escaped literals, one to three `;` sections. A custom format is
  literal text, digit placeholders, literal text: placeholders in two places (`0-0`) are refused.
- **`FORMAT` dates.** `General Date`, `Long Date`, `Short Date`, `Long Time`, `Medium Time`, `Short Time`, and custom
  formats: `d dd ddd dddd dddddd w ww m mm mmm mmmm q y yy yyyy h hh n nn s ss`, `AM/PM am/pm A/P a/p AMPM`, `/`, `:`,
  literals. `m` right after `h` (with or without `:`) is minutes.
- **Upper-case date letters.** A run of `D M Y H N S W Q` in upper case is read as the lower-case letters
  (`YYYY`, `MMM DD`, `DDDD`, `HH:NN:SS`). The pages write the characters in lower case and show only `\QQ yyyy`
  (DAX Guide) and `MMMM` (Power BI's own auto date/time table, whose Month column is shown as January). The
  characters are Visual Basic's (OLE Automation), which Microsoft says were not case-sensitive before VB 2005. The
  2010 DAX pages describe .NET's case-sensitive characters instead; where they give an upper-case letter another
  meaning it is refused: `M`/`MM` right after the hour (`hh:MM`: month there, minutes in Visual Basic), `H` with
  AM/PM (always 24-hour there), `Y` alone (the year there, the day of the year in Visual Basic) and the formats `D`,
  `M` and `Y` on their own, and a run in mixed case (`Mmm`: the month then minutes there). Mixed-case AM/PM
  (`Am/Pm`) is refused too.
- **BLANK.** `FORMAT ( BLANK (), ... )` is the empty text. A BLANK argument read as text is the empty text
  (`LEN ( BLANK () )` is 0), but a function that returns text refuses a BLANK text: no source says whether the real
  engine returns BLANK or the empty text.
- **Refused because the sources disagree** (Microsoft Learn against DAX Guide): `REPT` zero times and with a
  fraction; the formats `c`, `ddddd`, `ttttt` and `Medium Date`; `Percent` of a number that needs a thousand
  separator; TRUE or FALSE turned into text.
- **Refused because no source says:** a position or a count that is negative, fractional or past the end of the text
  (`MID ( "abcde", 4, 10 )`; `LEFT` and `RIGHT` past the end are documented and work); text with surrogate pairs
  where a position counts (it depends on the model's `UnicodeCharacterBehavior`); letters outside ASCII in `UPPER`,
  `LOWER` and as the text `SEARCH`/`CONTAINSSTRING` look for; kana, half- and full-width characters and combining
  marks in `SEARCH`; `SEARCH` in a case-sensitive model; a float whose decimal readings differ at the digits a format
  shows (`FORMAT ( 2.675, "0.00" )`: the float is just below 2.675); a negative number exactly halfway, one that
  rounds to zero, a negative `Currency` or `FIXED`, and a negative number through a one-section format that starts
  with literal text (`"$0.00"`); a number with a date format, a date with a number format, text as the value;
  `Short Time` before 10:00; `General Date` of a date with no time or of a time alone; `ww` of a Sunday; a time
  with a fraction of a second; dates before March 1, 1900; `TOCSV` and `TOJSON`.
- A measure's format string (`FormatString`, or a format string expression) is **not** applied to query results: the
  real engine doesn't apply it either.

#### Time intelligence

The classic time intelligence functions, over a date column:

| | Functions |
|---|---|
| First and last | `FIRSTDATE` `LASTDATE` `STARTOFMONTH` `STARTOFQUARTER` `STARTOFYEAR` `ENDOFMONTH` `ENDOFQUARTER` `ENDOFYEAR` |
| To date | `DATESMTD` `DATESQTD` `DATESYTD` `TOTALMTD` `TOTALQTD` `TOTALYTD` |
| Shifting | `DATEADD` `SAMEPERIODLASTYEAR` `PARALLELPERIOD` `PREVIOUSDAY` `PREVIOUSMONTH` `PREVIOUSQUARTER` `PREVIOUSYEAR` `NEXTDAY` `NEXTMONTH` `NEXTQUARTER` `NEXTYEAR` |
| Ranges | `DATESBETWEEN` `DATESINPERIOD` |
| Balances | `OPENINGBALANCEMONTH` `OPENINGBALANCEQUARTER` `OPENINGBALANCEYEAR` `CLOSINGBALANCEMONTH` `CLOSINGBALANCEQUARTER` `CLOSINGBALANCEYEAR` |

Each is implemented to the definition DAX Guide, SQLBI or Microsoft Learn publish for it (the header of
`Engine/Functions/TimeIntelligence.fs` lists them):

- **The dates argument** is a date column, which reads as `CALCULATETABLE ( DISTINCT ( column ) )` (so in a row
  context the row becomes the filter), or a table expression of one column that has the lineage of a date column
  (`VALUES ( 'Date'[Date] )`, a variable, the result of another of these functions).
- **The result** has the lineage of that column and only dates the column has, so it filters the column as a
  `CALCULATE` argument. `FIRSTDATE` and `LASTDATE` are also values.
- **`DATEADD`** by `MONTH`, `QUARTER` or `YEAR` works month by month: a month whose every date (of those the column
  has) is selected becomes the whole other month; otherwise each day moves, to the last day of a shorter month, and
  a selection with the last two days of its month runs to the end of the other month. `PARALLELPERIOD` returns
  whole months, quarters or years.
- **Year end dates** (`DATESYTD`, `TOTALYTD`, `STARTOFYEAR`, `ENDOFYEAR`, `PREVIOUSYEAR`, `NEXTYEAR`, the year
  balances) are a text literal: `"6/30"`, `"06-30"`, `"2000-06-30"`.
- **The date-table rule:** a filter argument of `CALCULATE` on the date column of a date table also removes the
  filters on every other column of that table, as if `ALL ( 'Date' )` were written next to it. A date table is one
  marked as a date table (data category `Time`, with the date column as its key), or one whose date column is the
  one side of an active relationship from another date column. A filter on any other column removes nothing else.

Refused, because the sources don't settle them (each error says which):

- calendar-based time intelligence: a calendar as the dates argument, the `WEEK` interval and the week functions,
  `DATEADD`'s Extension and Truncation and `DATESINPERIOD`'s EndBehavior;
- a True/False expression as the dates argument; a table of dates that are not the values of a model column; a date
  column with a time of day;
- a year end date that reads differently by culture (`"6/7"`, the day first as in `"30/6"`, a month name) or is in
  February;
- `FIRSTDATE` / `LASTDATE` as a table, and `DATESMTD` / `QTD` / `YTD`, when no date is selected; `DATESMTD` /
  `QTD` / `YTD` (and `TOTALMTD` / `QTD` / `YTD`) when other filters hide the first date of the period, where SQLBI's
  and DAX Guide's definitions give different dates;
- `DATEADD` and `SAMEPERIODLASTYEAR` of a selection of dates with gaps on a complete date table: Microsoft Learn
  says it is an error, SQLBI moves each month on its own;
- `DATEADD` of the last day of a month, selected without the day before it, to a longer month; `PARALLELPERIOD` of
  a selection that skips a whole month, quarter or year; `DATESINPERIOD` from a day the other month doesn't have
  (unless it is a month end), or from a BLANK start;
- the date-table rule where it is not documented and would change the result: `KEEPFILTERS` on a date filter, a
  filter that pairs the date column with other columns, a key column of a table that is not marked, the key of a
  relationship that is not active, and a context transition that filters the date column while other filters on
  the date table hide that date (a measure iterated over `ALL ( 'Date'[Date] )` under a filter on the year) or
  while a filter the rule would remove also restricts another table (a snowflaked table, a filter pairing a Date
  column with another table's, `SUMMARIZE`'s own extension columns). Iterating `VALUES ( 'Date'[Date] )` or
  `ADDCOLUMNS ( SUMMARIZE ( … ) )` by date under a filter on the year is fine, as is `SUMMARIZECOLUMNS` by date
  with the year as its filter argument.

On a date column that is not unique or has gaps (a fact table's), `DATEADD` and `SAMEPERIODLASTYEAR` of a selection
with gaps are an error, with the message the Power BI community quotes from the engine: "Function 'DATEADD' expects a contiguous selection when the date column is
not unique, has gaps or it contains time portion."

#### Numbers, conversion and the identity functions

`RAND` and `RANDBETWEEN` are known names but always refused: a random answer can't be checked against a server's.

Arithmetic follows the published descriptions of the engine (SQLBI's "Rounding errors with different data types in
DAX" and "Understanding numeric data type conversions", Microsoft Learn's data types tables); where they say nothing
the engine refuses rather than pick:

- whole numbers and fixed decimal numbers wrap around on `+`, `-` and whole × whole, as SQLBI shows
  (`5000000000000000000 + 5000000000000000000` is `-8446744073709551616`); a result that lands on the two values
  that mean BLANK (2^63 - 1 and -2^63) is refused, and so is `SUM`/`SUMX` past the 64-bit range;
- a whole or fixed decimal number compared with a decimal number is compared as decimal numbers (floating point), as
  the comparison table says: `CURRENCY ( 0.1 ) = 0.1` is TRUE, and so is `9007199254740993 = 9007199254740992.0`;
- a number times a date follows the multiplication table: a whole number times a date is a whole number, a fixed
  decimal or decimal number times a date a decimal number;
- fixed decimal results round half away from zero to four places; a value whose decimal reading and binary value
  disagree about the rounding (`ROUND ( 1.005, 2 )`, `CURRENCY ( 2.00005 )`) is refused, since which one the engine
  rounds isn't documented;
- `DIVIDE` returns what `/` returns, a fixed decimal number for a fixed decimal numerator;
- text read as a number takes only what doesn't depend on the culture: `"1,234"`, `"$5"` or `" 12 "` is refused,
  `"abc"` is an error; `CONVERT` and `VALUE` read text as a date as the date functions do (see "Text as a date").

`USERNAME`, `USERPRINCIPALNAME`, `USEROBJECTID`, `USERCULTURE` and `CUSTOMDATA` come from the connection on a server;
the local engine has none, so it refuses them unless the caller says who is asking:

```fsharp
Engine.withIdentity : Identity -> Engine.Prepared -> Engine.Prepared
// { Identity.none with UserName = Some "CONTOSO\\ann"; UserCulture = Some "en-US" }
```

A field left `None` stays refused (`CUSTOMDATA` is then BLANK, as for a connection without it). The original
`Prepared` is unchanged; calculated columns and tables never see the identity. On the command line:

```sh
tess query model.bim --data fixture "EVALUATE { USERNAME () }" --user-name 'CONTOSO\ann'
```

with `--user-name`, `--user-principal-name`, `--user-object-id`, `--user-culture` and `--custom-data` (for `--data`
only: a server takes them from the connection).

#### Window functions

`INDEX`, `OFFSET`, `WINDOW`, `RANK` and `ROWNUMBER`, with `ORDERBY` (ASC/DESC, `BLANKS DEFAULT`/`FIRST`/`LAST`),
the blanks argument (`DEFAULT`, `KEEP`, `FIRST`, `LAST`), `PARTITIONBY` and `MATCHBY`. The header of
`Engine/Functions/Window.fs` has the sources and the rules.

- **The current row** is found by apply semantics (SQLBI): each model column of the relation takes its value from
  the row context, or, when no row context has it, its values in the filter context (all combinations, no
  auto-exist); every matching row of the relation is a current row, and the results are put together without
  repeats. `RANK` with more than one current row (a total) is BLANK.
- **Ties.** Rows that tie on every `ORDERBY` key are fine for `RANK`. For the others the engine adds columns in a way
  no source documents, so a result that depends on how ties are broken is refused; a relation whose rows nothing
  can tell apart is the documented error ("…'s Relation parameter may have duplicate rows, which isn't allowed"),
  and so is a model table with no key or unique column whose order ties.
- An omitted `ORDERBY` orders by the relation's other columns; since the order they are added in is not
  guaranteed, every order is tried and the call is refused if they disagree.

Refused: an omitted relation (it is `ALLSELECTED` of the `ORDERBY` and `PARTITIONBY` columns), `reset` and axes
(visual calculations), a column two nested row contexts both have (Learn: an error; SQLBI: the innermost),
`RELATED` and related tables' columns in `PARTITIONBY`/`MATCHBY`, an `ORDERBY` column that is not the relation's,
BLANK next to zero or negatives and mixed types in a key (as everywhere), a BLANK or fractional delta or bound, a
model table with no key or unique column whose order happens not to tie, and calls where the sources' readings of
which columns identify the current row (Learn, DAX Guide, SQLBI) give different results (`RANK` ordered by
expressions only, say).

#### What it refuses

- **Not implemented yet:** `ALLSELECTED`, calendar-based time intelligence, the trigonometric functions, window
  functions (`OFFSET`, `INDEX`, `WINDOW`, `RANK`, `ROWNUMBER`), visual calculations,
  query parameters, detail rows, KPIs, natural joins of tables with model columns (joined over their expanded
  tables). Row-level and object-level
  security are not applied (a query runs as an administrator's would).
- **Model shapes:** many-to-many and one-to-one relationships (a one-to-one relationship puts each table in the
  other's expanded table and can give both a blank row; treating it as a two-way many-to-one would answer
  `ALL ( table )` and table filters wrongly), relationships on several columns, DirectQuery and
  Dual tables: every table connected to one is closed to queries, the rest of the model works. A calculation group
  is closed when a query names it. Active relationships that give two filter paths between two tables.
- **Where the sources don't say what the engine does,** for instance:
  - a one-side key that repeats or is BLANK;
  - keys with no match on an inactive relationship only, or at the start of a chain of relationships (whether the
    blank row exists);
  - a filter that crosses a both-directions relationship while keeping every row of the many side;
  - `TOPN` without an order-by on more rows than asked for, and `CONCATENATEX` without one over different texts
    (the engine defines no order), and sorting where BLANK meets zero or negative values, or values of different
    types (also in `START AT`);
  - whole-number arithmetic that reaches -2^63 or 2^63-1 (the documented range stops short of them, and the engine
    uses one of them for BLANK); a total of an aggregation that overflows;
  - `BLANK () && BLANK ()`, `BLANK () || BLANK ()` and `NOT BLANK ()` (the sources disagree);
  - text turned into a number when it has spaces around it, a currency symbol, thousands separators or `%`;
  - a variable whose value is an error when the result may not need it (the engine may or may not evaluate it);
  - `MAXX` / `MINX` with their variant argument TRUE, and over numbers mixed with dates or TRUE/FALSE;
  - a date or TRUE/FALSE turned into text by `&` (the format depends on the model's culture), text read as a date
    in arithmetic, a date on the left of `*`, a whole number times a date with a time of day;
  - `DISTINCTCOUNT` of a column while its table's blank row is visible (whether that BLANK is counted: write
    `COUNTROWS ( DISTINCT ( ... ) )` or `COUNTROWS ( VALUES ( ... ) )` to say which);
  - a context transition, or a column read, inside a user-defined function that was called from a row context
    (the documentation says what the function's parameters see of the caller's row context, not its body);
  - a name that is both a column of the row context and a measure (except in `ORDER BY`, where it is the result's
    column);
  - a table built on a model table with columns added, used as a filter or iterated with a measure, when that table
    relates to others (whether those are still filtered).

Without `ORDER BY`, rows come in the order the engine produced them. As on a server, that order is not defined:
compare such results as sets.

#### `--coverage`

`tess query <model> --coverage` reads the model only (no data) and lists every measure, calculated column and
calculated table that uses something the engine doesn't support, with what; then how many of each kind it can
evaluate, and the unsupported things ranked by how many objects use them. It follows references: a measure that
calls an unsupported measure or function is unsupported, and so is everything that depends, over relationships, on a
calculated table or column that can't be computed (a date table that can't be computed closes most of a model).
`--json` writes `{"command": "query coverage", "source", "summary": {measures, calculatedColumns,
calculatedTables: {total, supported}}, "objects": [{ref, kind, supported, unsupported: [...]}], "unsupported":
[{reason, objects}]}`.

Coverage runs each function's static check of its argument forms, the same one the evaluator runs before a call. A
refusal that depends on where a function is used (`KEEPFILTERS` on an iterator's table), on the evaluation context (a
function called from a row context) or on the data (a key that repeats, a blank row that can't be decided, values
of mixed types) only shows when evaluating: coverage is a lower bound on what is refused, not a guarantee.

#### For developers

The engine is `src/Tessera.Core/Engine/`:

```fsharp
Engine.prepare        : Database -> DataStore -> Result<Engine.Prepared, TessError>   // bind, compute calculated columns/tables
Engine.evaluateQuery  : Engine.Prepared -> string -> Result<QueryResult list, TessError>
Engine.evaluateScalar : Engine.Prepared -> string -> Result<QueryValue, TessError>    // no filter context, no row context
Engine.coverage       : Database -> (ObjectRef * string list) list
Engine.failures       : Engine.Prepared -> (ObjectRef * TessError) list               // what prepare couldn't compute
Engine.functions      : (string * string) list                                        // name, family
```

A `Prepared` is immutable and can be shared between threads. Every DAX function is an entry in a registry, in a
file per family under `Engine/Functions/`; a function receives its arguments unevaluated together with the
evaluation context, so iterators and `CALCULATE` are ordinary entries. Adding a family is a new file, a `<Compile>`
line and a line in `Registry.fs`; the header of `Engine/Types.fs` documents the API with examples. Each entry also
carries a static check of its argument forms, which is what `coverage` runs.

Performance, on a 100,000-row fact table with three dimensions (see `EnginePerfTests`, on a laptop): `SUMX` over
the fact about 0.15 s, `CALCULATE` with two filters under 0.01 s, `SUMMARIZECOLUMNS` by a dimension column with a
measure 0.1 to 0.2 s (by two dimensions with a filter 0.35 s), a measure per row of the fact (100,000 context
transitions) 0.35 s.

### Testing a model (`test`)

`tess test` runs DAX assertion tests for a model. On model files it evaluates them with
[the local DAX engine](#the-local-dax-engine-query---data) over [fixture data](#fixture-data-data): no server, no
Power BI licence, no network, so a model's measures can be tested in CI. Given an `xmla:` model, the same suites
run on the server instead.

```sh
tess test init    ./Sales.bim --data ./fixtures     # a suite with one pending test per measure, to fill in
tess test run     ./Sales.bim                       # run .tess-tests/*.test.yaml next to the model
tess test run     ./Sales.bim --tag revenue --ci github --trx results.trx
tess test run     "xmla:powerbi://api.powerbi.com/v1.0/myorg/Finance;Sales"     # the same suites, on the server
tess test snapshot ./Sales.bim --save               # record the results of the snapshot tests
tess test compare ./Sales.bim --against ./Sales-refactored.bim                  # same results from both?
tess test compare ./Sales.bim --against "xmla:...;Sales"                        # the local engine against the server
tess test list                                      # check the suites, without a model
```

#### A worked example

`Sales.bim` has a table `Sales` (Id, Region, Year, Amount) and three measures: `Revenue = SUM ( Sales[Amount] )`,
`Average Sale` and `Revenue Label = FORMAT ( [Revenue], "#,0" )`. `fixtures/Sales.csv` holds three rows:

```
Id,Region,Year,Amount
1,North,2023,100.50
2,North,2024,200
3,South,2024,49.50
```

`.tess-tests/revenue.test.yaml`:

```yaml
suite: Revenue
data: ../fixtures                 # the fixture directory, relative to this file
defaults:
  tags: [revenue]
tests:
  - name: total revenue
    measure: Revenue
    expect: 350

  - name: revenue in {year}       # one test per matrix entry
    matrix:
      - { year: 2023, revenue: 100.5 }
      - { year: 2024, revenue: 249.5 }
    measure: Revenue
    filters:
      Sales[Year]: "{year}"
    expect: "{revenue}"

  - name: average sale, to the cent
    measure: Average Sale
    expect: { value: 116.67, tolerance_abs: 0.005 }

  - name: revenue by region
    query: |
      EVALUATE SUMMARIZECOLUMNS ( Sales[Region], "Revenue", [Revenue] )
    expect:
      table:
        - { Region: North, Revenue: 300.5 }
        - { Region: South, Revenue: 49.5 }

  - name: no negative sales
    query: EVALUATE FILTER ( Sales, Sales[Amount] < 0 )
    expect: { empty: true }

  - name: an empty model has no revenue
    tables:
      Sales: []                   # inline rows instead of the fixture's file
    assert: ISBLANK ( [Revenue] )

  - name: the label
    measure: Revenue Label
    expect: "350"
```

```
$ tess test run ./Sales.bim
Revenue  (/work/.tess-tests/revenue.test.yaml)
  pass         total revenue
  pass         revenue in 2023
  pass         revenue in 2024
  pass         average sale, to the cent
  pass         revenue by region
  pass         no negative sales
  pass         an empty model has no revenue
  unsupported  the label
               Not supported by the local DAX engine: the function FORMAT (measure 'Sales'[Revenue Label] 1:1)

8 tests: 7 passed, 1 unsupported (0.4 s)
```

A failing test says what differs:

```
  FAIL         total revenue
               [Value] is 350.0, expected 351
```

#### Outcomes and the exit code

| Outcome | Means | Finding |
|---|---|---|
| `passed` | the expectation holds | none |
| `failed` | it doesn't: the message and `differences` say how, value by value | `TEST_FAIL`, error |
| `error` | the query failed and the test doesn't expect it to, or the test can't be run as written | `TEST_ERROR`, error |
| `unsupported` | the local engine refused: the test uses something it doesn't implement (it names what) | `TEST_UNSUPPORTED`, warning |
| `skipped` | not run: `pending`, `skip`, after a failure with `--fail-fast`, or a test with its own data on a server | none |

`test run` exits 1 when any test failed or errored, when a suite is invalid, and **when no test reached a verdict**
(none passed and none failed: `TEST_NONE_RAN`). An unsupported test is not a verdict. By default it doesn't fail the
run: the engine is a subset, a refusal says nothing about your model, and failing would leave a team with a red
build they can't fix. It is never silent, though: it is counted in the summary line, it is a warning finding (and a
`--ci` annotation), and a run where nothing but unsupported or pending tests is left exits 1. `--strict` (the same
as `--fail-on warning`) makes any unsupported test fail the run, for a suite that is meant to be fully covered.

What stops a run altogether, with its usual exit code and no report: no suites found (3, `no-test-suites`), a
model that can't be loaded (3), a server that can't be reached (5), a `--trx` / `--junit` file that can't be
written (4), and, when the first test that needs it gets there, a fixture that can't be loaded or has errors (3)
or a snapshot file that can't be read (3). An invalid suite stops it too, as findings (exit 1): **suites are checked before the model is
loaded or a connection made**, and nothing runs if one of them is wrong, so a broken file can't pass as "no tests
failed".

#### Where suites are

`tess test run <model> [suites...]` takes suite files or directories (searched recursively for `*.test.yaml`,
`*.test.yml` and `*.test.json`). With none given it uses the `.tess-tests` directory next to the model: beside a
`.bim` or `.pbip` file, beside a model folder (inside it, for a folder that holds a `.pbip`); failing that,
`.tess-tests` in the current directory (the only place looked in for an `xmla:` model).

#### The suite format

The format follows the Tabular Editor CLI's `.te-tests/*.test.yaml` where it can, so a suite written for `te test`
runs here: `suite`, `description`, `defaults` (`tolerance`, `tolerance_abs`, `tags`), and tests with `name`,
`query` / `query_file`, `tags`, `tolerance` / `tolerance_abs`, `matrix` and `expect` (a column's value, `gt` / `gte`
/ `lt` / `lte`, `rows`, `empty`, `columns`, `error`). What tess adds is marked below. A JSON file (`*.test.json`)
with the same keys works too (comments and trailing commas allowed).

Suite keys:

| Key | |
|---|---|
| `suite` | the suite's name (default: the file's name) |
| `description` | |
| `data` | *(tess)* the fixture directory the tests evaluate over, relative to the suite file. `--data` replaces it |
| `tables` | *(tess)* inline rows, for every test of the suite: see below |
| `defaults` | `tolerance`, `tolerance_abs`, `tags`, and *(tess)* `filters`, for every test |
| `tests` | the list of tests |
| `connection` | te's; read and not used (a note on stderr says so): the model is the one on the command line |

Test keys:

| Key | |
|---|---|
| `name` | required; unique in the suite, ignoring case |
| `description`, `tags` | `tags` add to the defaults' |
| `query` / `query_file` | a DAX query with one `EVALUATE` (`query_file` is relative to the suite) |
| `measure` | *(tess)* a measure's name, exactly: `measure: Revenue` evaluates `[Revenue]` |
| `expression` | *(tess)* any scalar DAX expression |
| `assert` | *(tess)* a scalar DAX expression that must be `TRUE`; takes no `expect` |
| `expect` | what must hold: see below. Required, except for `assert` and pending tests |
| `filters` | *(tess)* the filter context: `Table[Column]: value` or a list of values |
| `matrix` | run the test once per combination of values |
| `data`, `tables` | *(tess)* the test's own fixture directory, and its own inline rows |
| `tolerance`, `tolerance_abs` | for this test's numbers |
| `pending: true`, `skip: <reason>` | *(tess)* don't run it (it is reported as skipped) |
| `compare` | te's; read and not used |

A test has exactly one of `query`, `query_file`, `measure`, `expression`, `assert`. An unknown key is an error, with
its line and the key that was probably meant: a misspelt `expect` must not turn a test into one that checks nothing.

**Values** are typed as YAML types them: `350` and `1.5` are numbers, `true` a boolean, `null` (or `~`) BLANK,
`2024-01-31` (unquoted) a date, anything quoted is text. BLANK only equals BLANK, never 0 or `""`; text compares
exactly; a whole number equals the same decimal number (`350` matches a decimal `350.0`).

**`expect`** for `measure` and `expression` is the value itself (`expect: 350`, `expect: null`), or a mapping:
`value`, `gt` / `gte` / `lt` / `lte`, `tolerance` / `tolerance_abs`, or `error`, or `snapshot: true`.

**`expect`** for a query is a mapping. Every key that isn't one of these is a column of the result:

| Key | |
|---|---|
| *column*: value, or `{ value, gt, gte, lt, lte, tolerance, tolerance_abs }` | the column's value in a result of exactly one row (`EVALUATE ROW ( "A", ..., "B", ... )` asserts several) |
| `rows: 5` or `rows: { gte: 1, lte: 100 }` | the number of rows |
| `empty: true` / `false` | no rows / at least one |
| `columns: [Region, Revenue]` | the result has these columns |
| `table:` a list of rows (`- { Region: North, Revenue: 300.5 }`) | *(tess)* the whole result: these rows and columns, no others |
| `file: expected.csv` (or `.json`) | *(tess)* the whole result, from a file next to the suite: a CSV with a header row, a JSON array of row objects, or a result as `tess query --json` writes one |
| `ordered: true` / `false` | *(tess)* whether row order matters for `table`, `file` and `snapshot`. Default: only when the query has an `ORDER BY` |
| `snapshot: true` | *(tess)* the result recorded by `tess test snapshot` |
| `error: true`, `error: <code>`, or `error: { code, message }` | the query must fail; with the error's code, and/or text its message contains |

Columns are named as you'd say them: `Revenue` is the result's `[Revenue]`, `Region` its `Sales[Region]` (when only
one column ends that way), and the full name always works. In a CSV file an empty field is BLANK and `""` the empty
string, as in fixtures, and each field is read as the type of the result column it is compared with.

**Tolerance.** `tolerance` is relative (0.01 is 1%, of the larger of the two numbers), `tolerance_abs` absolute; with
both, either is enough. They apply to every number, whole and fixed decimal included. A test's replaces the
defaults', a column's replaces the test's. With neither, decimal numbers (doubles) are equal to about 9 significant
digits and whole and fixed decimal numbers exactly.

**Errors.** The codes are tess's: `dax-syntax`, `dax-invalid` (an unknown name, the wrong arguments), `dax-error`
(an error DAX raises: `ERROR ( )`, a failed conversion) locally; on a server every query error is `query-failed`,
so match the `message` in a suite that runs on both. A test the local engine refuses is `unsupported` even when it
expects an error, unless it expects `error: not-supported` by name.

**Filters** build the filter context the test is evaluated in, as a visual's filters would:

```yaml
filters:
  Date[Year]: 2024
  "'Product Category'[Name]": [Bikes, Helmets]      # a name with a space needs its quotes, and the YAML key its own
  Sales[Order Date]: { dax: "DATE ( 2024, 1, 31 )" }   # any DAX scalar expression
```

Each is a `TREATAS ( { values }, column )` filter of a `CALCULATE` around the expression (for a query, of a
`CALCULATETABLE` around each `EVALUATE`'s table; `DEFINE` and `ORDER BY` stay). `tess test list --json` shows the
query each test runs as. The local engine refuses `SUMMARIZECOLUMNS` under an outer filter, so a filtered query
that uses it is `unsupported` there: put the filter in the query (`SUMMARIZECOLUMNS ( ..., TREATAS ( ... ), ... )`).

**Matrix.** A mapping of names to values runs the test for every combination; a list of mappings runs those
combinations, which lets each carry its expected value (as in the example). `{name}` is replaced in every value of
the test (the query, a query file's text, filters, expectations): a value that is exactly `"{name}"` becomes the
variable's value with its type; inside a longer text it becomes the value's text. Only declared names are replaced,
so `{ 1, 2 }` in a query is safe. Each test is named `name [year=2024, ...]`, unless the name has a placeholder of
its own.

**Inline rows** (`tables`) give a table's rows in the suite, as the [fixture JSON format](#the-format) has them,
instead of a file: for small tests whose data should sit next to the assertion. They go on top of the `data`
directory if there is one (a table given inline replaces its file); with no directory, every other table is empty.
A test's `tables` replace the suite's for the same table. They go through the same loader and checks as a fixture
directory, and a problem stops the run naming the suite file, the table's line and the row.

#### `test run`

`tess test run <model> [suites...]`, with:

| Option | |
|---|---|
| `--data <dir>` | the fixture directory, instead of the suites' `data` (a test's own `data` stays). Model files only |
| `--filter <text>` | only tests whose name contains the text, or that have it as a tag (repeatable, comma-separated) |
| `--tag <tag>` | only tests with the tag |
| `--fail-fast` | stop at the first failure or error; the rest are skipped |
| `--fail-on error\|warning`, `--strict` | see [outcomes](#outcomes-and-the-exit-code) |
| `--ci github\|azdo` | annotations on stderr, as for `validate` |
| `--trx <file>`, `--junit <file>` | also write the results as a Visual Studio TRX file and/or JUnit XML, for CI dashboards |
| `--force` | let `--trx` / `--junit` replace a file that isn't a test report (they replace an earlier report freely) |

`--json` writes the [findings envelope](#findings-json-and-ci-annotations) with `"command": "test run"`, plus
`"source"`, `"suites": [{name, file, summary, tests: [{name, outcome, line, tags, durationMs, code?, message?,
differences?, query}]}]`, `"invalidSuites": [{file, validationErrors}]` and `"testSummary": {passed, failed,
errored, unsupported, skipped, invalidSuites, durationMs, total}`. In the report files an unsupported test is "not
executed" (TRX) or skipped (JUnit), and a failure with `--strict`.

**Against a server** (`xmla:<server>;<database>`) each test's query is sent as it is, over one connection, with the
credentials of [connected mode](#connected-mode-connect-query-deploy-remote-models). The server has its own data:
`--data` is refused, and a test with its own `data` or `tables` is skipped, with the reason. To test a deployed
model against known data, deploy it with the fixture inside (`tess data embed`).

#### `test init`

`tess test init <model>` writes `.tess-tests/measures.test.yaml` next to the model (`-o <file>` for another place;
the name must end in `.test.yaml`): one test per measure, `pending: true`, to fill in. `--data <dir>` writes the
suite's `data` (relative to the suite). `--snapshot` writes snapshot tests instead of pending ones, which is the
quick way to guard a refactor: `init --snapshot`, `snapshot --save`, change the model, `run`. It never overwrites a
file (exit 4). A suite of pending tests doesn't pass: `run` exits 1 until at least one test reaches a verdict.

#### `test snapshot`

A test with `expect: { snapshot: true }` passes when its result is the one recorded in `<suite>.snapshot.json`,
next to `<suite>.test.yaml`. `tess test snapshot <model> [suites...]` evaluates those tests and shows what
recording would change (`+` new, `~` changed with the differences, `=` unchanged, `-` recorded but no longer a
test, `!` can't be recorded); **it is a dry run unless `--save` is given**. `--filter` / `--tag` record some tests
and leave the other snapshots alone.

The file is meant for version control: tests by name, a row per line, rows sorted unless the query has an `ORDER
BY`, LF line ends, no timestamps; recording an unchanged model changes no byte. A test that errors or is
unsupported is never recorded (exit 1, and what was recorded stays). In `run`, a snapshot test with nothing
recorded fails, saying how to record it. A snapshot compares types too (a whole number that became a decimal is a
difference), unless the test has a tolerance, which compares numbers by value.

#### `test compare`

`tess test compare <model> --against <other> [suites...]` runs each test's query on both and reports the tests
whose results differ: `<other>` is another version of the model files (the refactor check; `--against-data` gives
it another fixture) or an `xmla:` model (the local engine against the deployed model, or `xmla:` against `xmla:`
for two deployments). The tests' expectations play no part, except that a test expecting an error passes when both
sides fail. A difference reads "*against's value*, expected *the model's value*". `--tolerance` / `--tolerance-abs`
replace the tests' tolerances; a value that differs in type only (a whole number 1, a decimal 1.0) is not a
difference unless `--types` is given. The other options, the JSON (`"command": "test compare"`, with `"against"`)
and the exit codes are those of `run`; a test either side's local engine refuses is `unsupported`.

#### Differences from `te test`

- **It runs offline.** `te test` needs a deployed model; `data`, `tables`, `filters`, `measure`, `expression`,
  `assert`, `table`, `file`, `snapshot`, `pending` and the `unsupported` outcome are tess's.
- The model is a positional argument, as everywhere in tess, not `--model` / `-s` / `-d`; a suite's `connection`
  is not used.
- Suites live in `.tess-tests`, not `.te-tests` (give `.te-tests` as an argument to run a te suite).
- Snapshots are per test and live next to the suite, written by `snapshot --save`; te's `snapshot -o baseline.json`
  / `--diff` over every measure is `init --snapshot` plus `snapshot`.
- `compare --against` takes model files as well as servers; te's `--source-a` / `--source-b` take two deployed
  models.
- Finding codes `TEST_FAIL`, `TEST_ERROR` and `TEST_SUITE_INVALID` are te's; `TEST_UNSUPPORTED` and `TEST_NONE_RAN`
  are tess's. `testSummary` adds `unsupported`.
- **BLANK is not 0.** `expect: 0` fails for a BLANK result (`COUNTROWS` of no rows is BLANK, not 0): write
  `expect: null`, or `COUNTROWS ( ... ) + 0` in the query. te's own example suite expects `0` from such a query, so
  te may be laxer here; tess stays strict because a measure that went BLANK is the regression a test is for.
- Not there: `te test spec` and the JSON Schema for editors, `test use`, and a `now:` setting to pin the clock.
- The suite format was matched to te's published reference, not to its binary: `matrix` (both shapes are accepted
  here) and `compare` (read and ignored) are the parts that reference doesn't spell out.

### Findings JSON and CI annotations

`validate`, `bpa`, `lint` and `test` (and later `schemacheck`) report findings in one shape, the one the Tabular
Editor CLI uses, so tooling written for one reads the other:

```json
{ "command": "validate", "durationMs": 361,
  "summary": { "errors": 1, "warnings": 0, "info": 0, "total": 1 },
  "findings": [ { "severity": "error", "source": "validate", "code": "unresolved-reference",
                  "message": "'Nope'[Revenue] does not exist: there is no table 'Nope'",
                  "object": "Gross Margin %", "objectType": "Measure",
                  "objectPath": "measure:Invoiced Sales/Gross Margin %",
                  "expressionPosition": { "property": "expression", "lineNumber": 5, "column": 26 },
                  "fixable": false } ],
  "valid": false }
```

How the fields work:
- **Severity:** `error`, `warning` or `info`.
- **`code`:** stable. It's the `validate` rule id, the BPA rule ID, or the `lint` rule id (`DI001`...).
- **`objectPath`:** a tessera reference, so it pastes into any command.
- **`objectType`:** one of `Model`, `Table`, `Column`, `Measure`, `Hierarchy`, `Level`, `Partition`,
  `CalculationItem`, `Calendar`, `Relationship`, `Role`, `Perspective`, `Culture`, `NamedExpression`, `Function`,
  `DataSource`; and, for `test`, `Test` and `TestSuite` (which have no `objectPath`: they aren't model objects).
- **Optional keys** (`objectPath`, `expressionPosition`, `ruleName`, `category`) are left out rather than written
  as `null`. Where the position is only known to the line, `expressionPosition` has no `column`.
- **`fixable`:** whether `tess lint --fix` fixes it. Always `false` for `validate` and `bpa`, which don't
  apply fixes.

`--ci github` writes one `::error` / `::warning` / `::notice` line per finding to stderr. `--ci azdo` writes
`##vso[task.logissue ...]` lines, with info findings as plain lines so info-only runs stay green. Each line carries
the code as its title.

### JSON and exit codes

`--json` (or `-j`) writes one JSON document to stdout, errors included (`{"error": {"code", "message"}}`).

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | the "negative" result of the Unix tool a command mirrors: `find` found nothing (like grep); `diff` found differences (like diff); `bpa`/`validate`/`lint` found issues at or above `--fail-on` (default `error`); `test run` had a failed or errored test, an invalid suite, or no test that reached a verdict; `test compare` found differences; `format --check` found DAX that isn't formatted; an edit's `--save` refused because it would introduce errors |
| 2 | usage error, including invalid arguments for an editing command: an invalid new name (`rename`, `add`), a property `set` can't write or a value it can't take, or an option `add` doesn't take for that kind |
| 3 | input error: model not found, unrecognised, ambiguous or failed to load. For the editing commands also a name that's taken, a missing object or parent, an object that can't be created, removed or moved there, an object others still refer to (`remove`), an edit that would change what other references mean (`rename`, `move`), a translation file that can't be applied (`translations import`), or an object a perspective can't hold. For `script`, also a script that can't be read or applied, or an object a script can't hold. For `bpa`, also a rule file you named that can't be read; for `bpa rules`, a rule ID that isn't there (or already is, for `add`) or an object that can't carry an ignore annotation. For `format`, DAX that can't be read (malformed) |
| 4 | output error (destination not empty, write failed); `deploy --execute` on a database that exists, without `--overwrite` |
| 5 | a server couldn't be reached, refused the credentials (or none are configured), has no such database, or reported an error (a failed `query`, `deploy` or `refresh`) |
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

CI (`.github/workflows/ci.yml`) checks formatting, then builds and tests on Linux, Windows and macOS. A further job
runs the suite against a pinned clone of the sample model below.
Set `TESSERA_BPA_RULES=<file>[:<file>...]` to run BPA rule files (e.g. from TabularEditor/BestPracticeRules)
in a test that reports how many rules are supported. (This is a test setting; `tess bpa` itself reads
`TESS_BPA_RULES`, which the CLI tests hide, along with `HOME`, `XDG_CONFIG_HOME` and `APPDATA`.) It runs against `TESSERA_SAMPLE_MODEL` if that is set,
otherwise against a fixture.

Set `TESSERA_XMLA_SERVER` and `TESSERA_XMLA_DATABASE` (with credentials in the `TESS_XMLA_*` variables) to also run
read-only tests against a real server; see `docs/xmla-manual-test.md`. The CLI tests never see real credentials,
and fail if a command that should be offline asks for a connection.

Set `TESSERA_SAMPLE_MODEL=<path>` to also run a round-trip test against a real model, such as a clone of
[TabularEditor/spaceparts-analytics](https://github.com/TabularEditor/spaceparts-analytics).

Releases: push a tag such as `v0.2.0`. `.github/workflows/release.yml` then:
- runs the tests
- builds the self-contained single-file binaries and the tool package, stamped with the tag's version
  (`-p:Version=0.2.0`; `Directory.Build.props` holds the default for local builds)
- attaches them all to a GitHub release

`tess --version` prints that version. The `+<commit>` suffix .NET adds to the informational version is left out.
To build a binary locally:

```sh
dotnet publish src/Tessera.Cli -c Release -r linux-x64 --self-contained -p:PublishSingleFile=true -o publish
```

Single-file binaries are compressed and not trimmed, because TOM relies on reflection. They leave out MSAL's native
sign-in broker, which tess never uses (it signs in with a token, a service principal or a device code, all in
managed code), and the optional `msasxpress.dll` transport compression on Windows.

Layout:

- `src/Tessera.Core` holds the model logic and never prints anything. Commands return typed results and
  `TessError` values.
- `src/Tessera.Xmla` is the only assembly that opens connections to servers. Everything goes through
  `IXmlaConnector` / `IXmlaSession` (`Session.fs`); `Live.fs` is the one implementation that uses the client
  libraries' connection types, and tests use a fake. `Remote.fs` holds the logic on top, `Credentials.fs` reads the
  `TESS_XMLA_*` variables, and `Failures.fs` turns client-library errors into `TessError`s with secrets scrubbed.
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

### The DAX parser (`Tessera.Core`)

A real DAX parser, the first step of a local DAX engine. It has no command of its own yet: it is the library the
evaluator is written against. Three modules, none of which knows about a model:

- `DaxAst` is the tree. Every node carries the span (offset and length) of its source text. Names are unresolved,
  exactly as written: `Sales` (a table, a variable or a parameter), `'Sales'`, `Sales[Amount]` (a column or a
  measure), `[Amount]`. Parentheses the source had are kept as nodes, so a tree's shape is the source's grouping.
- `DaxParser` reads text into it and never throws:

  ```fsharp
  DaxParser.parseExpression "CALCULATE ( [Total], 'Date'[Year] = 2024 )"   // Result<Expr, ParseError list>
  DaxParser.parseFunction   "(amount: NUMERIC, rate = 0.1) => amount * rate" // Result<FunctionDef, ...>
  DaxParser.parseQuery      "DEFINE MEASURE Sales[X] = 1 EVALUATE Sales ORDER BY [X] DESC"
  DaxParser.parse           text    // whichever of the three the text is
  ```

  Each has a `...With { Semicolons = true }` form for DAX that separates arguments with `;` and writes decimals
  with `,`. An error has its offset, length, line, column and a message that says what to fix:
  `1:18: expected ')' to close the '(' at 1:4, found the end of the text`.
- `DaxPrinter` writes a tree back as minimal one-line DAX (`Format` remains the formatter). Parsing what it prints
  gives the same tree.

Operator precedence, loosest first: `||`; `&&`; `NOT`; `=` `==` `<>` `<` `<=` `>` `>=` `IN`; `&`; `+` `-`; `*` `/`;
the sign; `^`. Operators of one level group left to right. This is Microsoft's
[operator precedence table](https://learn.microsoft.com/en-us/dax/dax-operator-reference), with `&&` and `||`
placed as the [DAX Guide](https://dax.guide/operators/) places them. `NOT` is always a prefix operator, never a
function: `NOT (a) = b` is `NOT ((a) = b)`. The two sources disagree about `IN` (Microsoft lists it with the
comparisons, the DAX Guide above `^`); the parser follows Microsoft.

Limits, so that nothing can overflow the stack: expressions may nest 256 levels deep, and a tree is at most
`DaxParser.maxDepth` (500) levels deep, which also caps an unparenthesised chain of one operator at 499 operators.
Anything deeper is an error, not a tree.

Things worth knowing when using the tree:

- A keyword argument (`DESC`, `MONTH`, `BOTH`, `INTEGER`) is an ordinary `Identifier`: without the function's
  signature it cannot be told from a table or variable of that name. The `Keyword` pattern reads one in upper case.
  Several keywords side by side (`ASC BLANKS LAST`) are one `Words` node.
- An argument left out (`RANKX ( T, [m], , DESC )`) is an `Omitted` node. `F()` has no arguments.
- A bare `TRUE` is a literal, `TRUE()` a call.
- `DaxAst.references` lists the names an expression refers to, leaving out the VARs in scope. A test checks it
  against `Deps` for every expression, and names every reason the two may differ.

### The DAX conformance corpus

`tests/conformance` is a corpus of small DAX queries with the results a real engine gives, each taken from a
published source (the DAX reference, DAX Guide, SQLBI) and saying which: its format and rules are in
[`tests/conformance/README.md`](tests/conformance/README.md). It is the independent check on the local DAX
engine: no expected value came from running or reading an engine. **It has not been confirmed against a server
yet**; many cases are worked out by hand from documented rules, and some are marked uncertain.

- `tests/Tessera.Tests/Conformance.fs` reads and checks the corpus, and runs any engine against a case:

  ```fsharp
  Conformance.run : (Database -> DataStore -> string -> Result<QueryResult list, TessError>) -> Case -> Outcome
  // Outcome: Pass | Fail of Mismatch (a value-level diff) | NotSupported of reason | Error of message
  ```

- `dotnet test` checks every case file (the model builds, the rows are a valid fixture, the query parses, the
  expectation is well-formed) and that every model embeds its rows exactly. The `conformance (engine)` test list
  is skipped until the engine exists: set `engine` at the top of `ConformanceTests.fs`. `expectedUnsupported`
  there lists the tags the engine may leave for later. It takes any tag, a function's name included (`round`,
  `format`): the area tags (`math-functions`...) mark the cases in that area's folder, and cases elsewhere call
  such functions too, tagged only with the function.
- `QueryResult.compare` (`Tessera.Core`) is the comparison: columns by name, rows as a multiset unless ordered,
  doubles within a tolerance, int64 and decimal exactly, and differences of type (`1` against `1.0`, a column
  declared `double` instead of `decimal`) reported apart from differences of value. `QueryResult.ofJson` reads
  the `tess query --json` shape back.
- To run the corpus against a real server (it deploys scratch databases named `tessera-conformance-*`):

  ```sh
  TESSERA_XMLA_SERVER='powerbi://api.powerbi.com/v1.0/myorg/<workspace>' \
    dotnet run --project tests/Tessera.Tests -c Release -- conformance-server --out ./conformance-report
  ```

  See `docs/xmla-manual-test.md`, section G. `python3 tests/conformance/check-quotes.py` checks every quoted
  source line against its page (the only part that uses the network, and only when you run it).

## Prior art

- [Tabular Editor 2](https://github.com/TabularEditor/TabularEditor) (MIT) is the inspiration and the reference
  for TOM behaviour; for example, the Power BI compatibility-mode heuristic follows its
  `PowerBiCompatibilityModeHelper`. `bpa` follows its Best Practice Analyzer: the rule format, scopes, ignore
  annotations and the TOMWrapper names that rules use. Its DAX token codes are ported from TE2's lexer.
- tessera is independent of Tabular Editor ApS and its commercial Tabular Editor 3 and `te` CLI.

## License

tessera's source is MIT (`LICENSE`). Release binaries and the tool package also bundle third-party
components under their own terms, notably Microsoft's Analysis Management Objects (TOM) under the Microsoft
Software License Terms. See [`THIRD-PARTY-NOTICES.md`](THIRD-PARTY-NOTICES.md).
