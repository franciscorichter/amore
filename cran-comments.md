## Submission

This is a patch release of **amorem**, version 1.0.1. It fixes one user-facing
correctness bug and makes no API changes.

`compare_models()` built its Cox formula with a namespace-qualified
`survival::strata(stratum)`. R's `terms(specials = )` does not match a
namespace-qualified call, so on **survival** < 3.7-3 the matched-set identifier
was silently fitted as an ordinary factor covariate in an unstratified model
rather than as a stratification. Coefficients and the reported `delta_AIC` were
unaffected, but the absolute `log_lik` was shifted by
`-n_events * log(n_strata)` and the degrees of freedom inflated by
`n_strata - 1`, so reported `AIC` values were wrong. The same applied to the
`frailty()` terms on the random-effects path.

All four call sites now use bare specials, which every **survival** version
matches, and `survival (>= 3.7-3)` is recorded in Imports. Regression tests
assert that `AIC == -2 * log_lik + 2 * df` and that no stratum dummies enter as
covariates; both fail against the previous behaviour.

The release also corrects the package `Description`, adds `BugReports`, adds the
`cph` role for the copyright holder named in LICENSE, and updates the vignettes.

## Test environments

* local: macOS (aarch64), R 4.5.1 — `R CMD check --as-cran`
* win-builder devel and release
* GitHub Actions (ubuntu-latest, R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes observed only in our local environment (not package defects)

* "checking HTML version of manual" — our local HTML Tidy is outdated and the
  V8 package is unavailable, so HTML validation and math rendering are skipped.

The case-insensitive name conflict with the archived package **AMORE**, raised
at the 1.0.0 submission, was resolved in that review; **amorem** has been on
CRAN since 2026-06-29.

## Reverse dependencies

None.
