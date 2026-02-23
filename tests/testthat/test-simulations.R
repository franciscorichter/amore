test_that("simulate_relational_events generates valid sequences", {
  set.seed(123)
  actors <- letters[1:3]
  baseline <- matrix(0.25, nrow = length(actors), ncol = length(actors))
  events <- simulate_relational_events(
    n_events = 30,
    senders = actors,
    receivers = actors,
    baseline_rate = 3,
    baseline_logits = baseline,
    allow_loops = FALSE
  )

  expect_s3_class(events, "data.frame")
  expect_true(all(events$sender %in% actors))
  expect_true(all(events$receiver %in% actors))
  expect_lte(nrow(events), 30)
  expect_true(all(events$sender != events$receiver))
})

test_that("simulate_relational_events respects horizon", {
  set.seed(321)
  events <- simulate_relational_events(
    n_events = 10,
    senders = c("s1", "s2"),
    receivers = c("r1", "r2"),
    baseline_rate = 5,
    horizon = 0
  )
  expect_equal(nrow(events), 0)
})

test_that("simulate_actor_covariates returns expected static matrices", {
  covs <- simulate_actor_covariates(
    senders = c("s1", "s2"),
    receivers = "r1",
    covariate_names = c("activity", "popularity"),
    seed = 42
  )

  expect_equal(nrow(covs$sender_covariates), 2)
  expect_equal(nrow(covs$receiver_covariates), 1)
  expect_setequal(colnames(covs$sender_covariates), c("actor", "activity", "popularity"))
})

test_that("simulate_actor_covariates returns tidy dynamic data", {
  covs <- simulate_actor_covariates(
    senders = c("s1", "s2"),
    receivers = c("r1", "r2"),
    covariate_names = "intensity",
    time_points = 0:3,
    rho = 0.5,
    sd = 0.1,
    seed = 99
  )

  sender_df <- covs$sender_covariates
  receiver_df <- covs$receiver_covariates

  required_cols <- c("actor", "time", "covariate", "value")
  expect_true(all(required_cols %in% names(sender_df)))
  expect_equal(sort(unique(sender_df$time)), 0:3)
  expect_equal(length(unique(sender_df$actor)), 2)
  expect_true(all(required_cols %in% names(receiver_df)))
  expect_equal(length(unique(receiver_df$actor)), 2)
})
