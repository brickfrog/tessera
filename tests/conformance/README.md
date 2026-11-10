# DAX conformance corpus

Small DAX queries with the results a real engine should give, each with the published source that says so. It is
the independent check on the local DAX engine: **no expected value here came from running or reading an engine.**
Every one is either shown by a source or worked out by hand from documented rules, and says which.

None of it has been confirmed against a server yet. `docs/xmla-manual-test.md` (section G) has the one command
that deploys the models to a workspace, runs every query there and reports each expectation that is wrong.

```
tests/conformance/
  models/<name>.json        shared models, with their rows
  <area>/<case>.json        one case per file; the path without .json is its id (blank/add-number)
  check-quotes.py           dev tool: checks every quoted line against the page it cites (uses the network)
```

The harness is `tests/Tessera.Tests/Conformance.fs`; `ConformanceTests.fs` checks every file on each test run
(the model builds in TOM, the rows pass `Fixture.check`, the query parses with `DaxParser`, the expected result is
well-formed, the metadata is complete). Files are JSON; comments and trailing commas are allowed.

## A case

```json
{
  "title": "BLANK + 5 is 5: next to a number, BLANK is zero",
  "model": "scalar",
  "query": "EVALUATE ROW ( \"r\", BLANK () + 5 )",
  "expected": [
    { "columns": [ { "name": "[r]", "type": "int64" } ], "rows": [ [ 5 ] ] }
  ],
  "ordered": false,
  "source": {
    "url": "https://learn.microsoft.com/en-us/power-bi/connect-data/desktop-data-types",
    "quote": "BLANK + 5",
    "supports": "the table of BLANK in operations gives 5 for BLANK + 5"
  },
  "confidence": "published",
  "tags": [ "blank", "operator-add" ]
}
```

| Property | |
|---|---|
| `title` | one line: the fact the case settles. One fact per case, since one error fails a whole query. |
| `model` | the name of a shared model, or `{ "base": "<name>", ...changes }` (below). |
| `query` | a DAX query: a string, or an array of lines. `DEFINE MEASURE` / `VAR` / `TABLE` / `COLUMN` / `FUNCTION` go here. |
| `expected` | an array with one result per `EVALUATE`, in the [`tess query` JSON shape](../../README.md#query) (`rowCount` and `truncated` may be left out); or `{ "error": "<why the query must fail>" }`. |
| `ordered` | `true` when the query's `ORDER BY` fixes the order of every row and that order is part of the expectation. Default `false`: rows are compared as a multiset. |
| `tolerance` | relative tolerance for doubles, if the default (1e-9) isn't right. |
| `source` | `{ "url", "quote"?, "supports" }`, or an array of them. `quote` is a line or two of the page, word for word; `supports` says what in the page justifies the expectation. |
| `confidence` | `published`: the source shows this query's or formula's output, or states the result outright. `derived`: worked out by hand from rules the sources document. |
| `derivation` | required when `derived`: the rules applied and the arithmetic, on this case's data. |
| `uncertain`, `alternative` | `"uncertain": true` when the sources disagree or are silent. The expectation is then the likelier reading, and `alternative` states the other reading and the result it would give. Only for `derived`. |
| `tags` | lower-case: the functions (`calculate`, `rankx`) and features (`context-transition`, `blank-row`) exercised. |
| `notes` | anything else worth knowing. |

Expected values are spelled as `tess query --json` spells them: BLANK is `null`, an `int64` has no decimal
point, a `double` and a `decimal` (DAX currency) have one, a `dateTime` is `"2024-01-31T00:00:00"`. In a column of
type `variant` a value's type is not in its spelling: a number without a point is an int64, with one a double, and
anything else is written `{ "type": "decimal", "value": 12.5 }` (also `dateTime`, `double` for `"NaN"`).

A column may leave out `"type"` when no source settles it (the type of an all-BLANK column, say). Its values are
still compared, numbers by value whatever their type. Use this sparingly: the type is part of the answer.

Column names are the engine's: `Table[Column]` for a model column, `[name]` for a column an expression names
(`ROW`, `ADDCOLUMNS`, `SUMMARIZECOLUMNS`), `[Value]` for a one-column table constructor.

### Tags with a meaning

- `assumption:<id>`: the case is designed to settle one of the items marked `ASSUMPTION` in
  `src/Tessera.Core/DaxValue.fs`. The ids are listed in `ConformanceTests.fs`, which fails if one has no case.
- Areas an engine may leave for later carry their area's tag, so they can be skipped as a group:
  `time-intelligence`, `allselected`, `format`, `text-functions`, `date-functions`, `math-functions`,
  `calculation-groups`, `window-functions`. A case in a folder of that name must have the tag.

## A model

```json
{
  "description": "what is in it, with the totals cases rely on",
  "culture": "en-US",
  "tables": [
    { "name": "Product",
      "columns": [ { "name": "ProductKey", "type": "int64", "key": true },
                   { "name": "Month", "type": "string", "sortBy": "MonthNo" } ],
      "calculatedColumns": [ { "name": "Double", "type": "int64", "expression": "Product[ProductKey] * 2" } ],
      "measures": [ { "name": "Products", "expression": "COUNTROWS ( Product )" } ],
      "rows": [ [ 1, "Jan" ], [ 2, null ] ] }
  ],
  "calculatedTables": [ { "name": "Colors", "expression": "VALUES ( Product[Color] )",
                          "columns": [ { "name": "Color", "type": "string" } ] } ],
  "relationships": [ { "from": "Sales[ProductKey]", "to": "Product[ProductKey]",
                       "cardinality": "many-to-one", "crossFilter": "single", "active": true } ],
  "functions": [ { "name": "AddTax", "expression": "( amount : NUMERIC ) => amount * 1.1" } ],
  "calculationGroups": [ { "name": "Time", "column": "Calc", "precedence": 0,
                           "items": [ { "name": "Current", "expression": "SELECTEDMEASURE ()" } ] } ]
}
```

- Column types: `string`, `int64`, `double`, `decimal`, `dateTime`, `boolean`. Rows are arrays in column order,
  spelled like expected values; `null` is BLANK and `""` the empty string.
- A relationship's `from` is the many side. `cardinality` is `many-to-one` (default), `one-to-one` or
  `many-to-many`; `crossFilter` is `single` (default: `to` filters `from`) or `both`.
- A calculated table declares its `columns` only if a relationship, `sortBy` or `key` needs them.
- Culture defaults to `en-US`. Everything else is TOM's default (compatibility level 1702).

Most cases use a shared model as it is. A case that needs something more changes it for itself:

```json
"model": { "base": "sales",
           "measures": [ { "table": "Sales", "name": "Big", "expression": "..." } ],
           "calculatedColumns": [ { "table": "Product", "name": "Rank", "type": "int64", "expression": "..." } ],
           "calculatedTables": [ ... ], "functions": [ ... ], "calculationGroups": [ ... ], "tables": [ ... ],
           "rows": { "Sales": [ [ ... ] ] },
           "addRelationships": [ ... ], "relationships": [ ...replaces them all... ] }
```

Each distinct change is its own model, and on a server its own database, so prefer `DEFINE` in the query, and
prefer a new shared model to many one-off changes.

## Writing cases

1. Find the source first: the [DAX reference](https://learn.microsoft.com/en-us/dax/),
   [DAX Guide](https://dax.guide), [SQLBI articles](https://www.sqlbi.com/articles/). Fetch the page; don't
   cite from memory. Adapt the example onto a shared model's data, quote at most a line or two, and cite.
2. `published` only if the page shows the result or states it outright. If you had to apply its rule to other
   data, it is `derived`: write the arithmetic.
3. Never get an expected value by running tessera, and don't read the engine's code for one. If the sources
   don't settle it, write the case anyway, as `uncertain`, with both readings.
4. Make the query deterministic: `ORDER BY` every row if order matters (and set `ordered`), name result columns,
   no `NOW`, `TODAY`, `RAND` or anything that depends on the server's locale beyond the model's `en-US`.
5. Run `dotnet test --filter "conformance"`, and `python3 tests/conformance/check-quotes.py` for the quotes.
