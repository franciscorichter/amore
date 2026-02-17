# Simulate exogenous actor covariates

Create simple exogenous covariate structures for senders and receivers.
The function can return static values (one row per actor) or
time-stamped processes (one row per actor and time point) that follow
independent AR(1) dynamics.

## Usage

``` r
simulate_actor_covariates(
  senders,
  receivers,
  covariate_names,
  time_points = NULL,
  sd = 1,
  rho = 0,
  seed = NULL
)
```

## Arguments

- senders:

  Character vector of sender actors.

- receivers:

  Character vector of receiver actors.

- covariate_names:

  Character vector naming the covariates to simulate.

- time_points:

  Optional numeric vector of strictly increasing time stamps for
  time-varying covariates. When omitted, static covariates are returned.

- sd:

  Standard deviation of the innovation noise.

- rho:

  AR(1) coefficient used when `time_points` is supplied. Must be in (-1,
  1).

- seed:

  Optional integer to make the simulation reproducible.

## Value

A list with two elements: `sender_covariates` and `receiver_covariates`.
Each element is either a wide data.frame (static case) or a tidy
data.frame with columns `actor`, `time`, `covariate`, and `value`
(dynamic case).

## Examples

``` r
sender_cov <- simulate_actor_covariates(
  senders = letters[1:3],
  receivers = LETTERS[1:2],
  covariate_names = c("activity", "recency"),
  time_points = seq(0, 4),
  rho = 0.6,
  sd = 0.2,
  seed = 123
)
str(sender_cov)
#> List of 2
#>  $ sender_covariates  :'data.frame': 30 obs. of  4 variables:
#>   ..$ actor    : chr [1:30] "a" "a" "a" "a" ...
#>   ..$ time     : int [1:30] 0 1 2 3 4 0 1 2 3 4 ...
#>   ..$ covariate: chr [1:30] "activity" "activity" "activity" "activity" ...
#>   ..$ value    : num [1:30] -0.1401 0.2277 0.1625 0.1897 -0.0236 ...
#>  $ receiver_covariates:'data.frame': 20 obs. of  4 variables:
#>   ..$ actor    : chr [1:20] "A" "A" "A" "A" ...
#>   ..$ time     : int [1:20] 0 1 2 3 4 0 1 2 3 4 ...
#>   ..$ covariate: chr [1:20] "activity" "activity" "activity" "activity" ...
#>   ..$ value    : num [1:20] 0.107 0.243 0.31 0.297 0.117 ...
```
