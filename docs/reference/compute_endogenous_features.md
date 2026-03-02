# Compute endogenous event-network statistics

Given a standardized relational event log, this helper derives simple
historical statistics for each event. These summaries can be fed into
model formulas or exported as part of inference-ready design matrices.

## Usage

``` r
compute_endogenous_features(
  event_log,
  stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency"),
  sort = TRUE
)
```

## Arguments

- event_log:

  A data.frame containing at least `sender`, `receiver`, and `time`
  columns.

- stats:

  Character vector describing which statistics to compute. Allowed
  values are `"sender_outdegree"`, `"receiver_indegree"`,
  `"reciprocity"`, and `"recency"`.

- sort:

  Logical; when `TRUE`, events are ordered by time prior to computing
  summaries (ties preserve input order).

## Value

The event log with added columns, one per requested statistic.
