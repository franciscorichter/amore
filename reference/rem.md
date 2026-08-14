# Fit a relational (hyper)event model on preprocessed case-control data

`rem()` is the unified front-end for fitting relational event models
from **already preprocessed** case-control data (e.g. produced by
`eventnet`), where the endogenous/exogenous covariates have already been
computed. It is intended to supersede
[`compare_models()`](https://franciscorichter.github.io/amorem/reference/compare_models.md),
[`compare_models_smooth()`](https://franciscorichter.github.io/amorem/reference/compare_models_smooth.md)
and
[`compare_models_global()`](https://franciscorichter.github.io/amorem/reference/compare_models_global.md),
which couple feature computation and fitting.

## Usage

``` r
rem(
  formula,
  data,
  method = c("gam", "clogit", "nn"),
  case = NULL,
  stratum = NULL,
  time = NULL,
  k = NULL,
  gam_method = NULL,
  nn = nn_control(),
  ...
)
```

## Arguments

- formula:

  A formula; see *Formula syntax*.

- data:

  A data.frame of preprocessed case-control data (wide for the `gam`
  method; long with a case indicator and stratum for `clogit`). For
  `method = "gam"`, long case-control input (a `event`/`IS_OBSERVED`
  indicator with control rows) is detected and widened automatically via
  [`widen_case_control()`](https://franciscorichter.github.io/amorem/reference/widen_case_control.md),
  with a message.

- method:

  Estimation backend; see *Description*.

- case:

  Optional name of the 0/1 event-indicator column for the `clogit` and
  `nn` backends. If `NULL` (default), the indicator is taken from the
  formula's left-hand side (e.g. `event ~ x`). Ignored by the `gam`
  method.

- stratum:

  Name of the column grouping each case with its controls (required by
  `clogit`).

- time:

  Name of the time column, required for `tv` / `tvnl` terms.

- k:

  Optional integer basis dimension passed to `s()` / `te()`.

- gam_method:

  Smoothness-selection method for the `gam` backend, passed to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). Defaults to
  `NULL`, which uses mgcv's own default (`"GCV.Cp"`) and reproduces the
  Intro-to-REM tutorial parameterization. Set to `"REML"` for the REML
  fit used in some papers.

- nn:

  An
  [`nn_control()`](https://franciscorichter.github.io/amorem/reference/nn_control.md)
  object with the architecture and training hyper-parameters for
  `method = "nn"`. Ignored by the other backends.

- ...:

  Reserved for future use.

## Value

An object of class `"rem"`: a list with the fitted model (`$fit`), the
`method`, the original `formula`, the parsed `terms`, and the number of
observations `n`. Has
[`summary()`](https://rdrr.io/r/base/summary.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`logLik()`](https://rdrr.io/r/stats/logLik.html) methods.

## Details

Two estimation backends are provided:

- `"gam"`:

  Degenerate logistic regression on a case-1-control design (Boschi,
  Lerner & Wit 2025): the response is a constant 1 and the linear
  predictor is built from event-minus-control differences. Supports
  smooth time-varying (`tv`), non-linear (`nl`) and time-varying
  non-linear (`tvnl`) effects via
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- `"clogit"`:

  Conditional logistic regression on a case-k-control design via
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  (linear terms only). The case/control strata are taken from `stratum`,
  or derived as `cumsum(case == 1)` when `stratum` is `NULL` (assuming
  each case is immediately followed by its controls, the eventnet
  blocked layout).

- `"nn"`:

  Flexible conditional-logistic models on the same case-k-control design
  as `clogit`, trained by (mini-batch) gradient descent on the exact
  risk-set softmax partial likelihood. Two architectures via
  [`nn_control()`](https://franciscorichter.github.io/amorem/reference/nn_control.md):
  a multilayer perceptron scoring the full covariate vector jointly
  (interaction-capable), or an `additive_spline` predictor —
  per-covariate B-spline expansions fitted by stochastic gradient, the
  STREAM construction of Filippi-Mazzola & Wit (2024, JRSS-C). No
  coefficient table; [`summary()`](https://rdrr.io/r/base/summary.html)
  reports in-sample (and, with a validation split, held-out) concordance
  and `plot(type = "pdp")` shows per-feature curves. Pure-R
  implementation, no extra dependencies.

## Formula syntax

The right-hand side lists covariates. A bare name is a **linear**
effect; wrap a name to request a smooth effect (`gam` method only):

- `tv(x)` — time-varying linear effect: `s(time, by = d_x)`.

- `nl(x)` — non-linear effect: `s(cbind(x_ev, x_nv), by = c(1, -1))`.

- `tvnl(x)` — time-varying non-linear effect (tensor product).

- `re(x)` — random effect of a grouping factor `x` (e.g. the sender),
  built from the matched `x_ev` / `x_nv` levels as
  `s(cbind(x_ev, x_nv), by = cbind(1, -1), bs = "re")`, contributing
  `f(event_level) - f(control_level)` (following the REM tutorial's
  species-invasiveness term). Falls back to a single column `x` when
  `x_ev` / `x_nv` are absent. Identified only when the event and control
  differ on `x`.

For the `gam` method the left-hand side is ignored (the response is the
constant case indicator); for `clogit` the left-hand side names the 0/1
event indicator column (e.g. `event ~ x`), unless `case` is given
explicitly.

## Column resolution

For a covariate `x`, the event/control difference is taken from column
`x`, else `d_x`, else `x_ev - x_nv`. Non-linear terms use
`transform_x_ev` / `transform_x_nv` when present (the eventnet
spline-transformed covariate), otherwise `x_ev` / `x_nv`. `tvnl` uses
`transformed_time` when present. Undirected logs (senders only, no
receiver/`TARGET` column) are supported.

## Name clashes with other relational-event packages

The name `rem()` is used by three CRAN packages: this one, relevent (on
CRAN since 2010) and redeem (2026). Attaching more than one of them
masks the others, and R will say so at load time.

The deeper issue is dispatch rather than masking. `amorem`'s fitted
object has class `"rem"`, and relevent and redeem register S3 methods on
that same class name, so merely *loading* one of them — attaching is not
required — can re-point [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`logLik()`](https://rdrr.io/r/stats/logLik.html) for objects fitted
here. If you work with more than one of these packages in a session,
call `amorem::rem()` explicitly and be deliberate about which package is
loaded when you print or plot a fit.

## See also

[`compare_models_smooth()`](https://franciscorichter.github.io/amorem/reference/compare_models_smooth.md)
(superseded),
[`simulate_relational_events()`](https://franciscorichter.github.io/amorem/reference/simulate_relational_events.md)
(whose `wide = TRUE` output is a valid input here),
[`simulate_directed_hyperevents_tvnl()`](https://franciscorichter.github.io/amorem/reference/simulate_directed_hyperevents_tvnl.md).

## Examples

``` r
set.seed(1)
w <- simulate_relational_events(
  n_events = 300, senders = paste0("a", 1:12), receivers = paste0("a", 1:12),
  n_controls = 1, endogenous_stats = "reciprocity_count",
  endogenous_effects = c(reciprocity_count = 0.6), wide = TRUE)
fit <- rem(~ reciprocity_count, data = w, method = "gam")
coef(fit)
#> reciprocity_count 
#>         0.9043626 
```
