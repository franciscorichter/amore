# amorem 1.0.1

## Bug fixes

* **`compare_models()` did not stratify on the matched sets under **survival**
  before 3.7-3.** The Cox formula was built with a namespace-qualified
  `survival::strata(stratum)`, and `terms(specials = )` does not match a
  qualified call, so the matched-set identifier was silently fitted as an
  ordinary factor covariate in an unstratified model. Coefficients and the
  `delta_AIC` column were unaffected; the absolute `log_lik` was shifted by
  `-n_events * log(n_strata)` and the degrees of freedom inflated by
  `n_strata - 1`, so reported `AIC` values were wrong. The same applied to the
  `frailty()` terms on the random-effects path, which is why the `coxme`
  two-axis fit did not complete. All four call sites now use bare specials,
  which every **survival** version matches, and regression tests assert that
  `AIC == -2 * log_lik + 2 * df` and that no stratum dummies enter as
  covariates. Anyone who published absolute log-likelihood or AIC values from
  `compare_models(n_controls > 1)` should re-run them.

# amorem 1.0.0

First CRAN release, under the name **amorem**. The package was renamed from the
working name `amore`, which collided (case-insensitively) with the archived CRAN
package `AMORE`. This release consolidates the 0.9.0 development line into the
first stable, installable version: the unified `rem()` front-end (the `clogit`,
`gam`, and `nn` backends, including the additive-spline architecture), the
Gillespie / tau-leap simulation engine, the endogenous-statistics catalogue, and
the martingale-residual goodness-of-fit family. Relative to 0.9.0 the package was renamed to **amorem** and the exported feature
functions dropped their `compute_` prefix --- `compute_endogenous_features()` and
`compute_hyperedge_features()` became `endogenous_features()` and
`hyperedge_features()`; the rest of the API is unchanged.

## New features

* **torch training engine for `rem(method = "nn")`.** `nn_control(engine =
  "torch")` trains the same model and conditional-logistic loss as the built-in
  pure-R engine using **torch** (libtorch autograd + Adam), markedly faster
  and, with `batch_strata`, scaling to large event logs (optionally on GPU).
  The fitted object is interchangeable with the R engine. `torch` is a
  *Suggests* dependency; the torch engine requires equal-sized strata.
* **`nn_uncertainty()`** quantifies uncertainty for a neural fit by a stratum
  bootstrap, returning partial-dependence uncertainty bands and a concordance
  confidence interval (with `print()` and `plot()` methods) — the inferential
  counterpart the point-prediction `nn` backend otherwise lacks.

# amorem 0.9.0

New neural backend and a small number of API refinements; the version
intended for the first CRAN release.

* **STREAM-style additive splines:** `nn_control(architecture =
  "additive_spline", batch_strata = )` fits per-covariate B-spline effects by
  (mini-batch) stochastic gradient on the exact case-control partial
  likelihood — the construction of Filippi-Mazzola & Wit (2024, JRSS-C,
  <doi:10.1093/jrsssc/qlae023>) — giving interpretable additive smooth
  curves on the same objective as `clogit`, with mini-batching for large
  event logs.
* **New `rem(method = "nn")` backend:** a multilayer perceptron scores every
  candidate in a case-control stratum and is trained on the
  conditional-logistic partial likelihood (softmax over each risk set) — a
  nonlinear, prediction-oriented counterpart of `clogit`. Pure-R implementation
  (no extra dependencies), configured via `nn_control()`; `summary()` reports
  in-sample (and, with a validation split, held-out) concordance and
  `plot(type = "pdp")` shows per-feature partial-dependence curves.
* **API:** the degenerate-logistic backend is now `method = "gam"` (was
  `"degenerate"`); the smooth-term wrappers are `tv()` / `nl()` / `tvnl()`
  (was `tve()` / `nle()` / `tvnle()`); `re()` is unchanged.
* `rem()`'s `case` argument now defaults to `NULL` and is taken from the
  formula's left-hand side (e.g. `event ~ x`) for the `clogit`/`nn` backends.
* `widen_case_control()` auto-detects the 0/1 indicator column (`event` or
  `IS_OBSERVED`) when `case` is not given.
* `widen_case_control()` now carries the sender/receiver identifiers of the
  case and its matched control into the output (`sender_ev`/`receiver_ev`/
  `sender_nv`/`receiver_nv`); the new `keep_ids` argument controls this
  (default `TRUE`). The dyads behind each pair are no longer lost, and `re()`
  grouping terms can reach the actor levels (#92).
* `rem(method = "gam")` now detects long-format case-control input (a
  `event`/`IS_OBSERVED` indicator with control rows) and widens it with
  `widen_case_control()` before fitting, emitting a message — instead of
  silently misreading raw per-row values as event-minus-control differences
  (#93).
* `compute_endogenous_features()` gains a `prior_log` argument for
  warm-starting the network state from events that precede the study window:
  its rows update the running state but never appear in the output, separating
  warm-starting from the non-event masking role of `history_log` (#94).
* `cpp_supported_stats()` is now exported.

# amorem 0.1.0

First release.

* `rem()` unified fitter for preprocessed case-control data, with a
  `gam` (case-1-control logistic via `mgcv::gam()`) and a `clogit` backend.
* Smooth-term formula wrappers for the `gam` backend (time-varying, non-linear,
  time-varying-non-linear) and an `re()` grouping random effect; `re()`
  reproduces the Intro-to-REM tutorial parameterization, and `rem()` exposes a
  `gam_method` argument.
* Simulation via `simulate_relational_events()` (Gillespie and tau-leap),
  the endogenous-statistic feature engine, non-event sampling, and the
  martingale-residual goodness-of-fit family.
