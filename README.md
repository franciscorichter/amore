# amore

<p align="center">
  <img src="man/figures/logo-github-light.png#gh-light-mode-only" width="180" alt="amore logo" />
  <img src="man/figures/logo-github-dark.png#gh-dark-mode-only" width="180" alt="amore logo" />
</p>

<p align="center">
  <a href="https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check" /></a>
  <a href="https://franciscorichter.github.io/amore/"><img src="https://img.shields.io/badge/docs-pkgdown-blue" alt="pkgdown docs" /></a>
  <img src="https://img.shields.io/badge/status-prototype-blue" alt="status prototype" />
</p>

**amore** (Advanced Modelling of Relational Events) is an R package for **simulation and inference** in relational event models (REMs). It targets dynamic network data in continuous time, with a focus on reproducible workflows: event logs, covariates, model fitting, and diagnostics.

## What it aims to provide

- **Simulation** utilities for relational event streams and exogenous covariates.
- **Inference** tools for fitting REMs and working with likelihood-based / counting-process formulations.
- **Covariate engineering** helpers for both exogenous and endogenous statistics (e.g., reciprocity, recency, shared partners), in a consistent API.

## Installation

```r
# install the development version from GitHub
# install.packages("pak")
pak::pak("franciscorichter/amore")

# alternatively, install from a local checkout
install.packages(".", repos = NULL, type = "source")
```

```r
library(amore)
```

## Quick start

The Gillespie algorithm generates relational events where inter-event
times are exponentially distributed with rate equal to the sum of all
dyadic hazards, and dyads are selected proportionally to their intensity.

```r
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

### Inference with GAM

The case-control output lets you recover parameters via a GAM:

```r
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
non-linear effects via `baseline_logits`.  For example, using geographic
distance with a smooth true effect:

```r
load(system.file("extdata", "dist-USA.RData", package = "amore"))

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

See `vignette("exogenous-covariates")` for the full workflow including GAM
recovery of the non-linear distance effect.

## Documentation

- Reference site + vignette: <https://franciscorichter.github.io/amore/>
- Issue tracker: <https://github.com/franciscorichter/amore/issues>

For function usage:

```r
?simulate_relational_events
?simulate_actor_covariates
```

## Development

- Document + namespace: `devtools::document()`
- Tests: `devtools::test()`
- Full check: `devtools::check()`
- Build pkgdown site: `pkgdown::build_site()`

## License

MIT, see `LICENSE`.
