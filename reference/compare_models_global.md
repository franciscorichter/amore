# Compare REM specifications with global covariate effects

**Superseded** by
[`rem()`](https://franciscorichter.github.io/amorem/reference/rem.md),
the unified front-end for fitting relational event models on
preprocessed case-control data. `compare_models_global()` remains fully
supported.

Implements the time-shifted partial likelihood of Lembo, Juozaitienė,
Vinciotti & Wit (2025) for fitting relational event models with **global
covariate effects** — covariates that are time-dependent but constant
across all interacting pairs (e.g.\\ temperature, time of day, the
residual baseline hazard). Standard case-control partial likelihood
cannot identify these because global terms cancel in the rate ratio;
this function follows the paper's Section 4 recipe: a random per-dyad
time shift breaks the cancellation, and with one non-event per event the
partial likelihood reduces to a degenerate logistic additive model fit
by [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

Per the paper's equations 11-13: \$\$ \mathcal{L}^{PS}(f, g) =
\prod\_{k=1}^n \frac{\exp\\\Delta_k(f; x\_{s_k r_k}) + \Delta_k(g;
x_k)\\} {1 + \exp\\\Delta_k(f; x\_{s_k r_k}) + \Delta_k(g; x_k)\\} \$\$
where each \\\Delta_k\\ is the difference between the (smooth) function
evaluated at the focal event time and at the sampled non-event's
*shifted* time \\t^\*\_k = t_k - h\_{s^\* r^\*}\\.

Shift distribution. Per-dyad shifts \\H\_{sr}\\ are drawn independently
from an exponential distribution with mean \\\nu \cdot \bar{\Delta t}\\
where \\\bar{\Delta t}\\ is the average inter-arrival time in
`event_log`. The paper's simulation studies find that \\\nu = 1\\ works
in practice and that the estimates are robust to choices in \\\[0.1,
10\]\\.

Specification format. Each entry of `models` is a named character vector
mapping a covariate name (a statistic in
[`endogenous_features()`](https://franciscorichter.github.io/amorem/reference/endogenous_features.md)
**or** a column of `global_covariates`) to an effect type:

- `"linear"` – linear `beta * x` term.

- `"nl"` – smooth `s(x)` (thin-plate, paper's default).

- `"tv"` – smooth `s(time, by = x)` (time-varying).

- `"tvnl"` – tensor product `te(time, x)`.

- `"global_smooth"` – smooth `s(x_global)` evaluated at the focal time
  vs. the non-event's shifted time (the paper's `g_b(x^{(b)}(t))`
  family).

- `"global_cyclic"` – cyclic smooth `s(x_global, bs = "cc")` for
  time-of-day-like covariates with a periodic domain.

- `"global_time"` – a smooth on `time` itself, recovering the residual
  time effect \\g_0(t)\\ of paper eq. 3.

## Usage

``` r
compare_models_global(
  event_log,
  models,
  global_covariates = NULL,
  scope = c("all", "appearance", "citation"),
  mode = c("one", "two"),
  half_life = NULL,
  shift_scale = 1,
  k = NULL,
  k_cyclic = 10,
  seed = NULL,
  keep_fits = FALSE
)
```

## Arguments

- event_log:

  Data frame with `sender`, `receiver`, `time`.

- models:

  Named list of specifications (see "Specification format" above).

- global_covariates:

  Optional data frame with a `time` column plus one column per global
  covariate referenced in `models`. The function evaluates each
  covariate at the focal event time and at the non-event's shifted time
  by stepwise lookup (LOCF on the `time` axis).

- scope, mode:

  Passed through to
  [`sample_non_events()`](https://franciscorichter.github.io/amorem/reference/sample_non_events.md).

- half_life:

  Required when any dyadic spec uses an exp-decay stat.

- shift_scale:

  Multiplier on the average inter-arrival time for the exponential shift
  distribution. Defaults to 1.

- k:

  Optional knot count for smooth terms (see
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html)). Defaults to
  `mgcv`'s automatic choice.

- k_cyclic:

  Knot count for `global_cyclic` smooths (paper uses 10 for
  time-of-day).

- seed:

  Integer seed for the case-control sample and the shift draws.

- keep_fits:

  Logical; when `TRUE`, the returned table carries the fitted model
  objects (one per spec, named by model, `NULL` for specs that failed)
  as `attr(result, "fits")`, e.g. for plotting estimated effects.
  Defaults to `FALSE`.

## Value

Data frame with one row per specification and columns `model`,
`n_terms`, `n_obs`, `log_lik`, `AIC`, `delta_AIC`.

## References

Lembo M, Juozaitienė R, Vinciotti V, Wit EC (2025). *Relational event
models with global covariates: an application to bike sharing*. Journal
of the Royal Statistical Society, Series C.
[doi:10.1093/jrsssc/qlaf058](https://doi.org/10.1093/jrsssc/qlaf058) .

## See also

[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md)
(linear, no globals),
[`compare_models_smooth()`](https://franciscorichter.github.io/amorem/reference/compare_models_smooth.md)
(smooth dyadic effects, no globals).

## Examples

``` r
# \donttest{
data(classroom_events)
# Hourly temperature track on the same time axis:
g <- data.frame(time = seq(0, max(classroom_events$time), length = 50),
                temperature = rnorm(50, 20, 5))
compare_models_global(
  classroom_events,
  models = list(
    dyadic_only = c(reciprocity_count        = "linear",
                    transitivity_count       = "linear"),
    with_global = c(reciprocity_count        = "linear",
                    transitivity_count       = "linear",
                    temperature              = "global_smooth",
                    time                     = "global_time")),
  global_covariates = g,
  seed = 11, k = 5)
#> Warning: compare_models_global: spec 'with_global' failed (NA/NaN/Inf in foreign function call (arg 3))
#>         model n_terms n_obs   log_lik      AIC delta_AIC
#> 1 dyadic_only       2   691 -305.5233 615.0466         0
#> 2 with_global       4   691        NA       NA        NA
# }
```
