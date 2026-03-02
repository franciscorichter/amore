# Attach static covariates to an event log

This helper augments an event log with sender and/or receiver covariates
that live in separate lookup tables. It is designed for static
covariates (one row per actor). Dynamic covariates should be merged
manually before calling this helper.

## Usage

``` r
attach_static_covariates(
  event_log,
  sender_covariates = NULL,
  receiver_covariates = NULL,
  actor_col = "actor",
  sender_prefix = "sender_",
  receiver_prefix = "receiver_",
  allow_missing = TRUE
)
```

## Arguments

- event_log:

  A standardized event log containing columns `sender` and `receiver`.

- sender_covariates, receiver_covariates:

  Data frames with one row per actor. Each must include the identifier
  column specified by `actor_col`.

- actor_col:

  Name of the identifier column inside the covariate tables.

- sender_prefix, receiver_prefix:

  Prefixes applied to the appended covariate column names.

- allow_missing:

  Logical; if `FALSE`, missing actors trigger an error.

## Value

The input `event_log` with additional columns for each covariate table
supplied.
