# Simulating relational events

``` r
library(amore)
```

## Simulating actor covariates

We can start by defining sender and receiver sets and generating
exogenous covariates. Static covariates return wide matrices, while
time-varying covariates are emitted in tidy format.

``` r
senders <- paste0("s", 1:3)
receivers <- paste0("r", 1:2)

covs <- simulate_actor_covariates(
  senders = senders,
  receivers = receivers,
  covariate_names = c("activity", "popularity"),
  time_points = 0:5,
  rho = 0.7,
  sd = 0.2,
  seed = 2024
)
head(covs$sender_covariates)
#>   actor time covariate        value
#> 1    s1    0  activity  0.275006442
#> 2    s1    1  activity  0.170910243
#> 3    s1    2  activity  0.351256861
#> 4    s1    3  activity  0.352809232
#> 5    s1    4  activity  0.001991379
#> 6    s1    5  activity -0.332991039
```

## Simulating event sequences

Using the covariates, we simulate 20 relational events. Sender effects
control how activity shapes outgoing intensity, and receiver effects do
the same for popularity.

``` r
static_sender <- reshape(
  covs$sender_covariates,
  direction = "wide",
  idvar = "actor",
  timevar = "covariate"
)
#> Warning in reshapeWide(data, idvar = idvar, timevar = timevar, varying =
#> varying, : multiple rows match for covariate=activity: first taken
#> Warning in reshapeWide(data, idvar = idvar, timevar = timevar, varying =
#> varying, : multiple rows match for covariate=popularity: first taken

events <- simulate_relational_events(
  n_events = 20,
  senders = senders,
  receivers = receivers,
  baseline_rate = 2,
  sender_covariates = static_sender[, c("value.activity", "value.popularity")],
  sender_effects = c(0.8, -0.2),
  allow_loops = FALSE
)
head(events)
#>   sender receiver      time
#> 1     s2       r2 0.1651009
#> 2     s1       r1 0.1791309
#> 3     s1       r2 0.5394382
#> 4     s2       r2 0.5718648
#> 5     s1       r2 0.5735172
#> 6     s1       r2 0.6641951
```
