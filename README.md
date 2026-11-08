# tessera

`tess` is a cross-platform command line for Power BI and Analysis Services semantic models. It reads and writes
`.bim`, TMDL folders, and Power BI Project (PBIP) folders, and is built in F# on Microsoft's own TOM library
(`Microsoft.AnalysisServices`). It runs on Linux, macOS and Windows on .NET 10.

Status: early. `convert`, `info` and `list` work. See the roadmap below.

## Usage

```sh
tess info  ./MyReport.pbip                       # summary: compatibility level, object counts
tess list  ./MyReport.pbip -k measure -t Sales   # one object reference per line
tess list  ./model.bim --name '*Margin*' --json  # machine-readable
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

### JSON and exit codes

`--json` (or `-j`) writes one JSON document to stdout, errors included (`{"error": {"code", "message"}}`).

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | findings reported (reserved for `bpa`, `diff`, `validate`) |
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

Set `TESSERA_SAMPLE_MODEL=<path>` to also run a round-trip test against a real model, such as a clone of
[TabularEditor/spaceparts-analytics](https://github.com/TabularEditor/spaceparts-analytics).

Layout:

- `src/Tessera.Core` holds the model logic and never prints anything. Commands return typed results and
  `TessError` values.
- `src/Tessera.Cli` handles argument parsing (Argu) and text/JSON rendering.

## Prior art

- [Tabular Editor 2](https://github.com/TabularEditor/TabularEditor) (MIT) is the inspiration and the reference
  for TOM behaviour; for example, the Power BI compatibility-mode heuristic follows its
  `PowerBiCompatibilityModeHelper`.
- tessera is independent of Tabular Editor ApS and its commercial Tabular Editor 3 and `te` CLI.

## License

MIT
