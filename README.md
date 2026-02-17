# amore <img src="https://img.shields.io/badge/status-prototype-blue" align="right" />

Advanced Modelling of Relational Events (amore) is an R package for
simulating and prototyping relational event models, focused on dynamic
network data and covariate processes.

## Features

- Simulate exogenous actor covariates (static or time-varying AR(1) processes).
- Generate relational event sequences with covariate-driven intensities.
- Ready for extension toward endogenous statistics and estimation routines.

## Getting started

```r
# install devtools if needed
# install.packages("devtools")
devtools::load_all(".")
??simulate_relational_events
```

## Development

- Document: `devtools::document()`
- Test: `devtools::test()`
- Check: `devtools::check()`

## License

MIT, see `LICENSE`.
