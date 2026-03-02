# Standardize a relational event log

Module A focuses on preprocessing utilities. This helper normalizes user
supplied event logs into the canonical `sender`/`receiver`/`time`
structure expected elsewhere in the package. It also handles common
cleaning tasks such as sorting, dropping missing rows, and removing
loops.

## Usage

``` r
standardize_event_log(
  event_log,
  sender_col = "sender",
  receiver_col = "receiver",
  time_col = "time",
  sort = TRUE,
  drop_nas = TRUE,
  drop_loops = FALSE,
  strictly_increasing_time = FALSE,
  remove_duplicates = TRUE,
  keep_extra = TRUE
)
```

## Arguments

- event_log:

  A data.frame (or tibble) containing at least one row per event.

- sender_col, receiver_col, time_col:

  Column names storing the sender, receiver, and time information.

- sort:

  Logical; should the output be sorted by time (ties are kept in input
  order)?

- drop_nas:

  Logical; if `TRUE`, rows with missing sender/receiver/time are
  removed. Otherwise an error is thrown when NAs are present.

- drop_loops:

  Logical; when `TRUE`, self-loops (`sender == receiver`) are dropped.

- strictly_increasing_time:

  Logical; if `TRUE`, an error is raised when non-increasing time stamps
  are detected after sorting.

- remove_duplicates:

  Logical; drop duplicated combinations of sender/receiver/time.

- keep_extra:

  Logical; if `FALSE`, only the standardized columns are returned. When
  `TRUE`, additional columns from the original input are preserved.

## Value

A data.frame with columns `sender`, `receiver`, and `time`. The return
object is tagged with class `"amore_event_log"` for downstream dispatch.
