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
| the 344 total/by queries: sum of those that finished | 56.5 s | 222.4 s | 434.6 s |
| ... that timed out | 10 (60 s) | 14 (30 s) | 95 (30 s) |
| peak working set | 109 MB | 111 MB | 171 MB (of the children that finished) |

The ten 200-row timeouts are `[CF - Bar Chart X Min]` / `Max` grouped by a dimension: the same quadratic
ALLSELECTED shape as the station matrix.
