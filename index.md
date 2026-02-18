# amore

<p align="center">
  <img src="man/figures/logo-github-white.png" width="200" alt="amore logo" />
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

```r
set.seed(1)

actors <- LETTERS[1:5]

covs <- simulate_actor_covariates(
  senders = actors,
  receivers = actors,
  covariate_names = c("activity", "popularity"),
  seed = 123
)

events <- simulate_relational_events(
  n_events = 100,
  senders = actors,
  receivers = actors,
  event_rate = 2,
  sender_covariates = covs$sender_covariates[, c("activity", "popularity")],
  sender_effects = c(0.8, -0.2),
  allow_loops = FALSE
)

head(events)
```

## Inference (package is growing)

The project scope includes estimation and inference for relational event models. The specific fitting functions will evolve as new estimators and covariate engines land, so the home page stays intentionally high-level.

If you are looking for a particular model family (e.g., Cox-type REMs, piecewise-constant hazards, Bayesian variants), please open an issue and describe the data structure you want to support.

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
