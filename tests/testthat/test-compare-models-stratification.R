# Regression tests for the stratification defect fixed in this release.
#
# compare_models() used to build its Cox formula with a namespace-qualified
# `survival::strata(stratum)`. R's terms(specials = "strata") does not match a
# qualified call, so on survival < 3.7-3 the matched-set identifier was fitted
# as an ordinary factor covariate instead of a stratification: correct
# coefficients, but a log-likelihood shifted by -n_events * log(n_strata) and a
# df count inflated by n_strata - 1. Both assertions below pass on the fixed
# code and fail on the old output (df = 692, length(coef) = 692).

test_that("compare_models AIC is consistent with its own log_lik and df", {
  data(classroom_events)
  specs <- list(
    count      = c("reciprocity_count", "transitivity_count"),
    continuous = c("reciprocity_time_recent", "transitivity_time_recent"))
  tab <- compare_models(classroom_events, models = specs,
                        n_controls = 3, seed = 11)
  expect_equal(tab$AIC, -2 * tab$log_lik + 2 * tab$n_terms)
})

test_that("compare_models actually stratifies on the matched sets", {
  data(classroom_events)
  specs <- list(count = c("reciprocity_count", "transitivity_count"))
  tab <- compare_models(classroom_events, models = specs,
                        n_controls = 3, seed = 11, keep_fits = TRUE)
  fit <- attr(tab, "fits")[["count"]]
  # 2 statistics and nothing else: no stratum dummies leaking in as covariates
  expect_equal(length(stats::coef(fit)), 2L)
  expect_equal(as.numeric(attr(stats::logLik(fit), "df")), 2)
})

test_that("the sender-frailty path stratifies too", {
  data(classroom_events)
  specs <- list(count = c("reciprocity_count", "transitivity_count"))
  tab <- compare_models(classroom_events, models = specs,
                        n_controls = 3, seed = 11,
                        random_effects = "sender", keep_fits = TRUE)
  # With a frailty the AIC uses the fit's EFFECTIVE df, not n_terms, so the
  # exact identity of the test above does not apply here. What must hold is
  # that the implied df is the handful the frailty costs (~21) and not one
  # parameter per matched set, which is what the unstratified fit produced.
  implied_df <- (tab$AIC + 2 * tab$log_lik) / 2
  expect_true(all(implied_df > 2 & implied_df < 40))
  fit <- attr(tab, "fits")[["count"]]
  # the two statistics plus the frailty term, not one coefficient per stratum
  expect_lt(length(stats::coef(fit)), 10L)
})

test_that("the coxme two-axis path stratifies on the matched sets", {
  skip_if_not_installed("coxme")
  data(classroom_events)
  specs <- list(count = c("reciprocity_count", "transitivity_count"))
  tab <- compare_models(classroom_events, models = specs,
                        n_controls = 3, seed = 11,
                        random_effects = c("sender", "receiver"),
                        keep_fits = TRUE)
  fit <- attr(tab, "fits")[["count"]]
  skip_if(is.null(fit), "coxme two-axis fit did not converge")
  expect_equal(length(coxme::fixef(fit)), 2L)
})
