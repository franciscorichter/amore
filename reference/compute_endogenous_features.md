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

## Details

The available statistics are defined for an event occurring at time
\\t_i\\ from sender \\s_i\\ to receiver \\r_i\\ as follows (before the
event is logged):

- `sender_outdegree`:

  \\\text{outdeg}\_{s_i}(t_i^-)\\, the number of events sent by \\s_i\\
  prior to \\t_i\\.

- `receiver_indegree`:

  \\\text{indeg}\_{r_i}(t_i^-)\\, the number of events received by
  \\r_i\\ prior to \\t_i\\.

- `reciprocity`:

  Indicator that the reverse dyad has ever been observed:
  \\\mathbb{1}\[\exists\\ j \< i : (s_j, r_j) = (r_i, s_i)\]\\.

- `recency`:

  If the dyad has previously interacted, the elapsed time since the most
  recent event: \\t_i - \max\\ t_j : j \< i, (s_j,r_j) = (s_i, r_i)
  \\\\; otherwise `NA`.
