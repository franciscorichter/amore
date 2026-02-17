# Simulate relational event sequences

Generate a simple relational event log for a sender set and receiver set
using a softmax allocation rule over dyadic intensities. The function is
intentionally lightweight so it can be used for quick experiments and
for stress-testing follow-up estimation routines.

## Usage

``` r
simulate_relational_events(
  n_events,
  senders,
  receivers,
  event_rate = 1,
  start_time = 0,
  horizon = Inf,
  baseline_logits = NULL,
  sender_covariates = NULL,
  sender_effects = NULL,
  receiver_covariates = NULL,
  receiver_effects = NULL,
  allow_loops = FALSE
)
```

## Arguments

- n_events:

  Number of events to generate.

- senders:

  Character vector listing the sender set \\\mathcal{S}\\.

- receivers:

  Character vector listing the receiver set \\\mathcal{R}\\.

- event_rate:

  Positive scalar controlling the expected number of events per unit
  time in the simulated point process.

- start_time:

  Initial time stamp.

- horizon:

  Optional maximum horizon; simulation stops once the cumulative time
  would exceed this value.

- baseline_logits:

  Optional `length(senders) x length(receivers)` matrix of baseline
  log-intensities. Defaults to zeros.

- sender_covariates:

  Optional numeric data.frame/matrix with one row per sender.

- sender_effects:

  Optional numeric vector of coefficients for `sender_covariates`.
  Required when sender covariates are supplied.

- receiver_covariates:

  Optional numeric data.frame/matrix with one row per receiver.

- receiver_effects:

  Optional numeric vector of coefficients for `receiver_covariates`.
  Required when receiver covariates are supplied.

- allow_loops:

  Logical; whether sender and receiver can coincide.

## Value

A tibble-like data.frame with columns `sender`, `receiver` and `time` of
length less than or equal to `n_events` (the horizon can truncate the
process).

## Examples

``` r
set.seed(1)
senders <- receivers <- LETTERS[1:3]
sender_cov <- data.frame(activity = c(0.5, -0.2, 1.1))
receiver_cov <- data.frame(popularity = c(0.1, 0.3, -0.4))
events <- simulate_relational_events(
  n_events = 5,
  senders = senders,
  receivers = receivers,
  event_rate = 2,
  sender_covariates = sender_cov,
  sender_effects = 1,
  receiver_covariates = receiver_cov,
  receiver_effects = 2
)
events
#>   sender receiver      time
#> 1      B        C 0.3775909
#> 2      A        B 0.4504443
#> 3      A        B 1.4503192
#> 4      B        C 1.6683535
#> 5      B        C 2.6853990
```
