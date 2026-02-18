# amore

![status:
prototype](https://img.shields.io/badge/status-prototype-blue)[![R-CMD-check](https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franciscorichter/amore/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://franciscorichter.github.io/amore/)

Advanced Modelling of Relational Events (amore) is an R package for
simulating and prototyping relational event models, focused on dynamic
network data and covariate processes.

## Features

- Simulate exogenous actor covariates (static or time-varying AR(1)
  processes).
- Generate relational event sequences with covariate-driven intensities.
- Ready for extension toward endogenous statistics and estimation
  routines.

## Installation

``` r
# install the development version from GitHub
# install.packages("pak")
pak::pak("franciscorichter/amore")

# alternatively, install from a local checkout
install.packages(".", repos = NULL, type = "source")
```

Once installed, load the package and consult the help topics:

``` r
library(amore)
?simulate_relational_events
?simulate_actor_covariates
```

## Development

- Document + namespace: `devtools::document()`
- Tests: `devtools::test()`
- Full check: `devtools::check()`
- Build vignettes/pkgdown:
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)

## Documentation

Browse the reference site and vignette at
<https://franciscorichter.github.io/amore/>.

## License

MIT, see `LICENSE`.
