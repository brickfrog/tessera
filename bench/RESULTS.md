# Benchmark results

On a 24-core Linux desktop (.NET 10, Release), spaceparts at commit a74c5833 with `tess data init --rows N --seed 7`.
Times are of `Engine.evaluateQuery` alone (prepare is listed separately); "timeout" is the `--timeout` the run used.
Peak working set is the child process's, the largest of the run's children.

## Before (c84f8e0)

| | 200 rows | 2,000 rows | 20,000 rows |
|---|---:|---:|---:|
| prepare | 1.3–1.6 s | 1.4–1.7 s | 1.7–2.1 s |
| coverage | 0.13 s | 0.12 s | 0.13 s |
| station-matrix/2 | 0.52 s | 3.5 s | > 30 s |
| station-matrix/5 | 1.9 s | 20.0 s | > 30 s |
| station-matrix/10 | 7.0 s | > 30 s | > 30 s |
| station-matrix/20 | 27.5 s | > 30 s | > 30 s |
| station-matrix/all | > 60 s (> 30 min reported) | > 30 s | > 30 s |
| allselected-shape/20 | 1.6 s | 15.1 s | |
| allselected-shape/all | 115.6 s | > 300 s | |
| share-of-selection | 0.23 s | 9.5 s | |
| rankx-customers | 0.11 s | 3.9 s | |
| the 344 total/by queries: sum of those that finished | 55.5 s | 222.4 s | 434.6 s |
| ... that timed out | 10 (60 s) | 14 (30 s) | 95 (30 s) |
| peak working set | 109 MB | 111 MB | 171 MB (of the children that finished) |

The ten 200-row timeouts are `[CF - Bar Chart X Min]` / `Max` grouped by a dimension: the same quadratic
ALLSELECTED shape as the station matrix.

## After (this branch)

Every answer of every run is the same as before (`--compare`: 0 changed at 200, 2,000 and 20,000 rows). Also run
once with a temporary mode (since removed) that recomputed every cache hit and compared: no difference, at 200
rows on every query, and through the whole test suite.

| | 200 rows | 2,000 rows | 20,000 rows |
|---|---:|---:|---:|
| prepare | 1.1–1.4 s | 1.4–1.6 s | 1.5–1.6 s |
| station-matrix/2 | 0.12 s | 0.54 s | > 30 s |
| station-matrix/5 | 0.11 s | 0.71 s | > 30 s |
| station-matrix/10 | 0.18 s | 0.92 s | > 30 s |
| station-matrix/20 | 0.27 s | 1.9 s | > 30 s |
| station-matrix/all | **2.0 s** | > 60 s | > 30 s |
| allselected-shape/20 | 0.06 s | 1.1 s | 20.8 s |
| allselected-shape/all | 0.62 s | > 60 s | > 30 s |
| share-of-selection | 0.17 s | 4.6 s | > 30 s |
| rankx-customers | 0.02 s | 0.38 s | 20.3 s |
| the 344 total/by queries: sum of those that finished | 108.6 s | 30.2 s | (subset, below) |
| ... that timed out | 0 (60 s) | 14 (60 s) | (subset, below) |
| peak working set | 304 MB | 229 MB | 307 MB |

At 200 rows the ten queries that timed out now finish (`[CF - Bar Chart X Min]` / `Max` by a dimension, 12 s
each: 10 groups x 200 stations of distinct evaluations), which is most of the 108.6 s; the other 334 sum to 7.5 s
(55.5 s before, with the ten excluded). At 2,000 rows the 330 that finished both times: 222.4 s -> 30.2 s.
At 20,000 rows only the totals and the Customer and Date groups were rerun (the full run takes hours with
timeouts): totals 73.6 s -> 39.8 s, by Customer 64.1 s -> 49.4 s (15 -> 13 timeouts), by Date 101.7 s -> 75.4 s
(12 -> 7 timeouts), counting the queries that finished both times.

Peak working set is higher: the caches keep row sets (`VisCache` up to about 64 MB per relationship set-up) and
measure values (up to 200,000 per query). The before numbers counted only children that finished; these count
killed ones too.

By commit, at 200 rows: visible rows shared (station-matrix/20 27.5 s -> 5.5 s), predicate filters per query
(-> 2.6 s), measure values per query (-> 0.28 s; the full matrix 2 s), Check answers kept (prepare 1.4 -> 1.1 s).

## What is left, and the redesign it needs

With the caches, a result is computed once per distinct context; what remains is the cost of computing a distinct
one. The profile of every slow query left has the same shape: an iterator over a dimension with a measure per row
(spaceparts' `[Gross Profit]` and `[Revenue]` are `SUMX ( 'Budget Rate', CALCULATE ( SUM ( ... ) ) ... )`), where
each row's context transition makes a filter context that differs from its neighbours' in one filter, and
`Filters.compute` then works out the fact table's visible rows from scratch: all of its own filters and every
relationship, O(fact rows) per transition. An iterator over D dimension rows costs D x F; at 2,000 rows that is 4
million row tests per `[Revenue]`, and `[CF - Bar Chart X Min]` by a dimension needs that for every (group, station).

Two ways out, neither a local change:

- **Incremental filter contexts.** A context made by `Filters.add` from a parent knows which filters changed. A
  table whose visible rows the changed filters can't reach keeps its parent's rows (the `Deps` sets of `VisCache`
  already say which); a table they reach is its parent's rows intersected with what the new filters allow, which
  for a transition on a dimension row is the fact rows of one key (an index lookup, `RowsOfId`), not a scan. That
  needs `compute` split into "parent rows minus removed filters" and "intersect added filters", which is only
  exact when no removed filter took part in a paired (Tuples / Rows) filter or a relationship that filters both
  ways; those would fall back to the full computation.
- **Aggregating by group.** `SUMX ( T, CALCULATE ( SUM ( F[x] ) ) )` with F reaching T is one pass over F's visible
  rows, summing per T key, instead of one CALCULATE per T row. That is the storage-engine query a real engine runs,
  but it means recognising the pattern (and its variants: a measure instead of SUM, a divisor read from T, as
  `[Gross Profit]` has) and proving each one equivalent, so it belongs to a planned rewrite rather than a cache.
