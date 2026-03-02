# amore

![amore logo](reference/figures/logo-github-white.png)

[![R-CMD-check](https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml)
[![pkgdown
docs](https://img.shields.io/badge/docs-pkgdown-blue)](https://franciscorichter.github.io/amore/)
![status prototype](https://img.shields.io/badge/status-prototype-blue)

**amore** (Advanced Modelling of Relational Events) is an R package for
**simulation and inference** in relational event models (REMs). It
targets dynamic network data in continuous time, with a focus on
reproducible workflows: event logs, covariates, model fitting, and
diagnostics.

## What it aims to provide

- **Simulation** utilities for relational event streams and exogenous
  covariates.
- **Inference** tools for fitting REMs and working with likelihood-based
  / counting-process formulations.
- **Covariate engineering** helpers for both exogenous and endogenous
  statistics (e.g., reciprocity, recency, shared partners), in a
  consistent API.

## Installation

``` r
# install the development version from GitHub
# install.packages("pak")
pak::pak("franciscorichter/amore")

# alternatively, install from a local checkout
install.packages(".", repos = NULL, type = "source")
```

``` r
library(amore)
```

## Quick start

The Gillespie algorithm generates relational events where inter-event
times are exponentially distributed with rate equal to the sum of all
dyadic hazards, and dyads are selected proportionally to their
intensity.

``` r
library(amore)
set.seed(1)

p      <- 20
actors <- as.character(1:p)

# Dyadic covariate x ~ N(0,1) with true effect b1 = 1
x        <- matrix(rnorm(p * p), nrow = p, ncol = p)
b1       <- 1
baseline <- b1 * x

# Simulate 500 events + 1 control per event for partial likelihood
events <- simulate_relational_events(
  n_events       = 500,
  senders        = actors,
  receivers      = actors,
  baseline_logits = baseline,
  allow_loops    = FALSE,
  n_controls     = 1
)

head(events)
```

## Core data components

Module A (Preprocesses) organizes dynamic network workflows around four
objects that can be composed as needed:

1.  **Relational event data** — canonical log with `sender`, `receiver`,
    `time`, produced via simulations or ingested from data sources.
2.  **Exogenous covariates** — actor or dyad-level inputs not generated
    by the event process (e.g., geography, demographics). These can be
    simulated via
    [`simulate_actor_covariates()`](https://franciscorichter.github.io/amore/reference/simulate_actor_covariates.md)
    or supplied as `baseline_logits`/lookup tables.
3.  **Endogenous covariates (eventnet)** — summaries derived from the
    evolving event history (recency, reciprocity, shared partners). Use
    [`compute_endogenous_features()`](https://franciscorichter.github.io/amore/reference/compute_endogenous_features.md)
    to generate baseline statistics that can be extended with custom
    feature builders.
4.  **Inference data** — nested case-control tables returned by
    `simulate_relational_events(..., n_controls > 0)` to drive
    conditional logistic / GAM estimation.

A small preprocessing example:

``` r
library(amore)

# 1. Event log direct from a data source
raw_events <- data.frame(
  source = c("a", "b", "b", "c"),
  target = c("b", "c", "a", "a"),
  ts = c(2.1, 2.4, 3.0, 3.5)
)

event_log <- standardize_event_log(
  raw_events,
  sender_col = "source",
  receiver_col = "target",
  time_col = "ts",
  drop_loops = TRUE
)
```

``` r
# 2. Exogenous covariates
covs <- simulate_actor_covariates(
  senders = unique(event_log$sender),
  receivers = unique(event_log$receiver),
  covariate_names = c("activity", "popularity"),
  seed = 123
)

event_log <- attach_static_covariates(
  event_log,
  sender_covariates = covs$sender_covariates,
  receiver_covariates = covs$receiver_covariates
)
```

``` r
# 3. Endogenous stats from the evolving event net
event_log <- compute_endogenous_features(event_log,
  stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency")
)
```

### Exogenous covariate definitions

[`simulate_actor_covariates()`](https://franciscorichter.github.io/amore/reference/simulate_actor_covariates.md)
returns two lookup tables with one row per actor. For a sender (a) and
covariate name (k):

- **Static covariates** (default) are Gaussian draws (x\_{a,k} (0, ^2)).
  [`attach_static_covariates()`](https://franciscorichter.github.io/amore/reference/attach_static_covariates.md)
  stores them in the event log as `sender_<k>` or `receiver_<k>`. In the
  example, `activity` acts as a baseline propensity for sending events
  and `popularity` captures receiver-specific attractiveness.
- **Dynamic covariates** arise when `time_points` is provided. \[
  x\_{a,k}(t\_) = , x\_{a,k}(t\_{}) + *{a,k}(t*), *{a,k}(t*) (0, ^2). \]
  Each actor/covariate pair follows an independent AR(1) trajectory at
  the supplied time grid.

### Endogenous network statistics

All endogenous summaries are evaluated immediately **before** the (i)-th
event ((s_i, r_i, t_i)) is added to the log:

- **Sender outdegree:** (*{s_i}(t_i^-) =* {j \< i} \[s_j = s_i\]).
- **Receiver indegree:** (*{r_i}(t_i^-) =* {j \< i} \[r_j = r_i\]).
- **Reciprocity indicator:** (\_{(s_i,r_i)}(t_i^-) = \[ j \< i : (s_j,
  r_j) = (r_i, s_i)\]).
- **Recency:** (\_{(s_i,r_i)}(t_i^-) = t_i - { t_j : j \< i, (s_j, r_j)
  = (s_i, r_i) }) with the convention that the value is `NA` when the
  dyad has not appeared before.

``` r
# 4. Inference-ready case-control data
cases_controls <- simulate_relational_events(
  n_events = 100,
  senders = unique(event_log$sender),
  receivers = unique(event_log$receiver),
  baseline_logits = matrix(0, nrow = 3, ncol = 3),
  allow_loops = FALSE,
  n_controls = 1
)
```

### Sampling non-events from observed logs

To create case-control tables from empirical event data, use
[`sample_non_events()`](https://franciscorichter.github.io/amore/reference/sample_non_events.md)
to append synthetic controls to each realized event:

``` r
case_control_df <- sample_non_events(
  event_log,
  n_controls = 2,
  scope = "appearance",
  mode = "two",
  allow_loops = FALSE,
  seed = 2026
)

head(case_control_df[, c("sender", "receiver", "event", "stratum")])
```

The helper keeps the original events (`event = 1`) and appends
`n_controls` counterfactual dyads (`event = 0`) per stratum so
conditional logistic / GAM estimators can compare realized vs. sampled
alternatives. Candidate dyads are constructed via two knobs:

1.  **scope**
    - `"all"`: every actor ever seen in the data belongs to the sampling
      pool.
    - `"appearance"`: only actors that have appeared prior to the focal
      event are eligible, which mimics nested case-control sampling.
2.  **mode**
    - `"one"`: draw both sender and receiver from the same candidate set
      (useful for single-mode networks).
    - `"two"`: draw senders and receivers from separate candidate pools
      (default for bipartite or directed settings).

Set `allow_loops = TRUE` when self-ties should be considered and adjust
`max_attempts` to control resampling when many candidate pairs coincide
with the observed event.

The three sampling schemes we discussed earlier map directly onto these
knobs:

| Strategy label                | `scope`        | `mode`                 |
|-------------------------------|----------------|------------------------|
| **all + one-mode**            | `"all"`        | `"one"`                |
| **all + two-mode**            | `"all"`        | `"two"`                |
| **appearance + one/two-mode** | `"appearance"` | `"one"` **or** `"two"` |

The last option is listed twice because you may want either a
single-mode or a two-mode draw while still restricting to previously
active actors.

Mathematically, for each observed event ((s_i, r_i, t_i)) the function
draws control dyads ((s\_{i heta}, r\_{i heta})*{}^{n*{}}) according to
the chosen scope/mode, ensuring they are distinct from the realized
pair. The output stacks \[ {(s_i, r_i, t_i, =1)} *{}^{n*{}} {(s\_{i},
r\_{i}, t_i, =0)} \] into stratum (i) so likelihood contributions
compare true events against their matched controls.

### Inference with GAM

The case-control output lets you recover parameters via a GAM:

``` r
library(mgcv)

get_x  <- function(s, r) x[cbind(as.integer(s), as.integer(r))]
events$x_val <- mapply(get_x, events$sender, events$receiver)

cases    <- events[events$event == 1, ]
controls <- events[events$event == 0, ]
cases    <- cases[order(cases$stratum), ]
controls <- controls[order(controls$stratum), ]

fit_df <- data.frame(y = 1, delta_x = cases$x_val - controls$x_val)
fit    <- gam(y ~ delta_x - 1, family = binomial, data = fit_df)

coef(fit)
#> delta_x ≈ 1  (recovers b1)
```

### Exogenous dyadic covariates

The package ships a 56 × 56 US state distance matrix and supports
non-linear effects via `baseline_logits`. For example, using geographic
distance with a smooth true effect:

``` r
data("dist_matrix", package = "amore")

dist_log     <- log(dist_matrix / 100000 + 1)
true_effect  <- sin(-dist_log / 1.5)

events <- simulate_relational_events(
  n_events        = 800,
  senders         = rownames(dist_matrix),
  receivers       = rownames(dist_matrix),
  baseline_logits = true_effect,
  allow_loops     = FALSE,
  n_controls      = 1
)
```

See
[`vignette("exogenous-covariates")`](https://franciscorichter.github.io/amore/articles/exogenous-covariates.md)
for the full workflow including GAM recovery of the non-linear distance
effect.

## Documentation

- Reference site + vignette: <https://franciscorichter.github.io/amore/>
- Issue tracker: <https://github.com/franciscorichter/amore/issues>

For function usage:

``` r
?simulate_relational_events
?simulate_actor_covariates
```

## Development

- During development, work from the package root and let R load the
  in-tree code with:

  ``` r
  devtools::load_all()
  ```

- Document + namespace: `devtools::document()`

- Tests: `devtools::test()`

- Full check: `devtools::check()`

- Build pkgdown site:
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)

## License

MIT, see `LICENSE`.
