# Engine benchmark

A benchmark of the local DAX engine on the spaceparts sample model. It is not part of the solution, the tests or
CI: build and run it by hand.

```sh
git clone https://github.com/TabularEditor/spaceparts-analytics /tmp/spaceparts   # pinned: a74c5833
dotnet run --project src/Tessera.Cli -- data init "/tmp/spaceparts/Invoiced Sales.pbip" /tmp/fx200 --rows 200 --seed 7
dotnet build -c Release bench/Tessera.Bench
dotnet bench/Tessera.Bench/bin/Release/net10.0/Tessera.Bench.dll "/tmp/spaceparts/Invoiced Sales.pbip" /tmp/fx200 \
    --timeout 60 --out results.tsv [--compare earlier.tsv] [--filter regex] [--check]
```

Each query runs in a child process (one prepared model per child), so a query that runs past `--timeout` seconds
is killed and the rest still run. The clock is fixed (2026-08-21 12:00) so TODAY gives the same answer every run.
Each result is fingerprinted (a hash of its JSON, or of its error), and `--compare` reports every query whose
answer differs from an earlier run's: speed must never buy a different answer. `--check` runs the engine in its
incremental check mode (every filter context whose rows are worked out incrementally is recomputed in full and
compared; slow): the run reports how many were checked and fails if any differed.

The queries:

- `station-matrix/N`: SUMMARIZECOLUMNS by `Region[Station]` of `[CF - Bar Chart X Min]` and `[CF - Bar Chart X Max]`
  (each a MINX / MAXX over `ALLSELECTED ( Region[Station] )` of `[Gross Margin % vs PY pp]`), for the first N
  stations (TREATAS) or all of them. Its values are 0 everywhere on synthetic data, so its fingerprint says little;
- `allselected-shape/N`: the same shape over `[Revenue]` and `[Gross Margin %]`, whose values differ by station;
- `share-of-selection`, `rankx-customers`: ALLSELECTED in CALCULATE, RANKX over a dimension;
- `total/<measure>` and `by <Table>[<Column>]/<measure>`: every measure at the grand total and grouped by a column of
  each dimension;
- `coverage`: `Engine.coverage` of the model.

Results, and how they changed, are in [RESULTS.md](RESULTS.md).
