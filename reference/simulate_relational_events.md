# Simulate relational event sequences

Generate a simple relational event log for a sender set and receiver set
using a softmax allocation rule over dyadic intensities. The process
follows the Gillespie algorithm, where the time between events is drawn
from an exponential distribution with rate equal to the sum of all
dyadic intensities.

## Usage

``` r
simulate_relational_events(
  n_events,
  senders,
  receivers,
  baseline_rate = 1,
  start_time = 0,
  horizon = Inf,
  baseline_logits = NULL,
  sender_covariates = NULL,
  sender_effects = NULL,
  receiver_covariates = NULL,
  receiver_effects = NULL,
  allow_loops = FALSE,
  n_controls = 0
)
```

## Arguments

- n_events:

  Number of events to generate.

- senders:

  Character vector listing the sender set \\\mathcal{S}\\.

- receivers:

  Character vector listing the receiver set \\\mathcal{R}\\.

- baseline_rate:

  Positive scalar. A constant baseline hazard multiplier applied to all
  dyads. Defaults to 1.

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

- n_controls:

  Integer; number of non-events (controls) to sample uniformly at random
  for each realized event. If `n_controls > 0`, the function returns a
  case-control data frame suitable for conditional logistic regression /
  GAM modeling. Defaults to 0.

## Value

If `n_controls = 0`, a data.frame with columns `sender`, `receiver` and
`time`. If `n_controls > 0`, it returns a long-format data.frame with
additional columns `stratum` (grouping an event with its controls) and
`event` (1 for the realized event, 0 for controls).

## Examples

``` r
set.seed(1)
senders <- receivers <- LETTERS[1:3]
sender_cov <- data.frame(activity = c(0.5, -0.2, 1.1))
receiver_cov <- data.frame(popularity = c(0.1, 0.3, -0.4))
# Standard event simulation
events <- simulate_relational_events(
  n_events = 5,
  senders = senders,
  receivers = receivers,
  sender_covariates = sender_cov,
  sender_effects = 1,
  receiver_covariates = receiver_cov,
  receiver_effects = 2
)
events
#>   sender receiver       time
#> 1      C        B 0.05297251
#> 2      B        A 0.06319316
#> 3      B        A 0.20346636
#> 4      C        B 0.23405456
#> 5      C        B 0.37673663

# Case-control generation for partial likelihood inference
cc_events <- simulate_relational_events(
  n_events = 5,
  senders = senders,
  receivers = receivers,
  sender_covariates = sender_cov,
  sender_effects = 1,
  n_controls = 2
)
head(cc_events)
#>   stratum event sender receiver       time
#> 1       1     1      C        A 0.03418054
#> 2       1     0      B        C 0.03418054
#> 3       1     0      A        B 0.03418054
#> 4       2     1      B        C 0.07395278
#> 5       2     0      C        A 0.07395278
#> 6       2     0      A        B 0.07395278
```
