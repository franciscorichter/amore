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

# 3. Endogenous stats from the evolving event net
event_log <- compute_endogenous_features(event_log,
  stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency")
)

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

- Document + namespace:
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
- Tests:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
- Full check:
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
- Build pkgdown site:
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)

## License

MIT, see `LICENSE`.
