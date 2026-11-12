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

## After the caches (wave 5, 3a4d821)

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

## Incremental filter contexts (wave 6, perf/engine-delta)

A context made from another works out a table's rows from the rows under the filters it kept (shared by every row
of an iteration) intersected with what its added filters allow; see "Incremental filter contexts" in the author's
guide (Engine/Types.fs) for the argument and the fallbacks.

Before and after were run back to back on the same quiet machine (before: the 3a4d821 build), with the same
fixtures, filters and timeouts. `--compare`: **0 changed answers** at 200 rows (all 354 queries), 2,000 rows (all
queries that finished both times: everything but the 16 that time out in both) and 20,000 rows (the subset: totals,
by Customer, by Date, station-matrix/2, allselected-shape/20, share-of-selection, rankx-customers; a full run takes
hours). Queries that timed out in the before run have no answer to compare with: at 2,000 rows station-matrix/all,
allselected-shape/all and the 14 `[CF - Bar Chart X Min]` / `Max` queries (they time out after too); at 20,000 rows
station-matrix/2, share-of-selection and 16 total/by queries, which now finish.

Check mode (every incremental result recomputed in full, without caches, and compared): the whole test suite
(1,960,547 results) and the 200-row benchmark (all queries, 22,203,220 results) with 0 differences.

| | 200 rows | 2,000 rows | 20,000 rows |
|---|---:|---:|---:|
| prepare | 1.1 s -> 1.2 s | 1.2–1.4 s -> 1.2–1.4 s | 1.5–1.7 s -> 1.5–1.6 s |
| station-matrix/2 | 0.13 s -> 0.14 s | 0.55 s -> 0.42 s | > 30 s -> **3.4 s** |
| station-matrix/5 | 0.13 s -> 0.14 s | 0.71 s -> 0.62 s | |
| station-matrix/10 | 0.22 s -> 0.18 s | 0.98 s -> 0.94 s | |
| station-matrix/20 | 0.32 s -> 0.30 s | 1.9 s -> 1.6 s | |
| station-matrix/all | 2.0 s -> 1.6 s | > 60 s -> > 60 s (171 s without a timeout) | |
| allselected-shape/20 | 0.05 s -> 0.07 s | 0.90 s -> 1.0 s | 20.0 s -> 12.5 s |
| allselected-shape/all | 0.67 s -> 0.71 s | > 60 s -> > 60 s (84 s without a timeout; before > 300 s) | |
| share-of-selection | 0.13 s -> 0.07 s | 4.5 s -> 1.6 s | > 30 s -> 18.2 s |
| rankx-customers | 0.014 s -> 0.010 s | 0.39 s -> 0.13 s | 21.3 s -> 1.4 s |
| total/by queries that finished both times: sum | 110.2 s -> 64.2 s (344) | 31.3 s -> 27.0 s (330) | 303.9 s -> 41.8 s (107 of 129) |
| ... that timed out | 0 -> 0 (60 s) | 14 -> 14 (60 s) | 22 -> 6 (30 s) |
| peak working set | 302 MB -> 136 MB | 225 MB -> 150 MB | 260 MB -> 285 MB |

At 20,000 rows the 16 total/by queries that now finish add 78 s (all of them `[Gross Margin %]`-like measures by
Customer and Date); the 6 still timing out are `[CF - Bar Chart X Min]` / `Max` at the total and by those two. The
working set is lower at 200 and 2,000 rows because rows worked out incrementally are no longer kept in `VisCache`
(their base's are); at 20,000 it is the largest child's, a killed one before and one that finishes now.

By commit: the incremental contexts themselves (20,000 rows: station-matrix/2 > 30 s -> 4.8 s); then keeping
incremental rows out of `VisCache`, shared by their base instead (2,000 rows: share-of-selection 1.6 s -> 1.1 s,
peak 185 MB -> 112 MB, in a run with only those queries).

**The target, the Station matrix at 2,000 rows in seconds, is not met** (171 s). The fixtures give every table N
rows, Budget Rate too, so `[Revenue]` and `[Gross Profit]` are SUMX over 2,000 Budget Rate rows with a CALCULATE
each; the matrix needs about 48 million such row contexts, and incremental contexts made each one independent of
the fact table's size (an index lookup) but not free: a transition, CALCULATE and SUM cost a few microseconds
whatever they read. What is left is the second option below.

## What is left: aggregating by group

The first option of the redesign below is done (wave 6). The second is not, and the brief limited it to the exact
shape `SUMX ( T, CALCULATE ( SUM ( F[x] ) ) )`. No measure of spaceparts has that shape: `[Revenue]` is
`SUMX ( 'Budget Rate', CALCULATE ( SUM ( ... ) ) / 'Budget Rate'[Rate] )` and `[Gross Profit]` the difference of
two such CALCULATEs over the rate, so the exact shape alone would show no number here; it was left for a planned
change rather than added untested by any real query. The design, and why it is exact:

- **What it computes.** Under a filter context C and no other row context, row r of the model table T (iterated as
  itself, not a table expression over it) makes C_r: C less the filters on T's expanded table, plus the rows equal
  to r (`closeRows`). Where the incremental conditions hold for F (`VisCache.Intersective` and `Safe`, nothing being
  computed, T in F's expanded table over regular many-to-one relationships), the rows of F under C_r are the rows of
  F under B (the kept part, the same for every r) whose row of T (`RowMap(F, T)`) is one of r's equals. One pass over
  F's rows under B, bucketing them by that row of T, gives every row's fact rows at once.
- **Why it is the same answer.** Each value is the existing SUM code run over exactly the row set the per-row
  evaluation would give (the union of the buckets of r's equals, as a `RowSet`), so the order of the additions and
  the type rules are SUM's own; SUMX then adds the values in T's row order as it does now. It must never re-implement
  the accumulation.
- **Where it must fall back to the row-by-row evaluation:** anything a transition can refuse (a date table's
  transition rule in `DateTable.onTransition`, calendars' `CalendarHooks.beforeAdd`, a calculated column being
  computed), calculation groups, a function called from a row context (`CallerRows`), a T that is not iterated as
  itself.
- **Check mode** compares every value with the row-by-row one, as for the incremental contexts.
- **The variant spaceparts needs** is better done at the CALCULATE rather than the SUMX: `CALCULATE ( SUM ( F[x] ) )`
  evaluated in a row context of T over a base B looks its value up in B's buckets, and the rest of the row's
  expression (the division by `'Budget Rate'[Rate]`) is evaluated as now. The argument above is unchanged (it is
  about that CALCULATE's value alone), and it covers `[Revenue]`, `[Gross Profit]` and the exact shape alike. That is
  wider than the brief allowed, so it is a proposal, not done.

## The redesign wave 5 proposed


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
