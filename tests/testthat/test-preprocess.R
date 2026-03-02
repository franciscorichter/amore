test_that("standardize_event_log cleans and tags logs", {
  df <- data.frame(
    s = c("a", "b", "b", "b"),
    r = c("b", "b", "b", "a"),
    t = c(2, 1, 1, 3),
    score = 1:4
  )

  out <- standardize_event_log(
    df,
    sender_col = "s",
    receiver_col = "r",
    time_col = "t",
    drop_loops = TRUE,
    remove_duplicates = TRUE
  )

  expect_s3_class(out, "amore_event_log")
  expect_equal(names(out)[1:3], c("sender", "receiver", "time"))
  expect_true(all(out$sender != out$receiver))
  expect_true(is.unsorted(out$time) == FALSE)
  # two rows remain after dropping loops and duplicates
  expect_equal(nrow(out), 2)
})


 test_that("attach_static_covariates merges sender/receiver tables", {
  events <- data.frame(
    sender = c("a", "b", "c"),
    receiver = c("c", "a", "b"),
    time = c(1, 2, 3)
  )

  send_cov <- data.frame(actor = c("a", "b", "c"), act = c(1, 2, 3))
  recv_cov <- data.frame(actor = c("a", "b", "c"), pop = c(4, 5, 6))

  augmented <- attach_static_covariates(
    events,
    sender_covariates = send_cov,
    receiver_covariates = recv_cov
  )

  expect_true(all(c("sender_act", "receiver_pop") %in% names(augmented)))
  expect_equal(augmented$sender_act, c(1, 2, 3))
  expect_equal(augmented$receiver_pop, c(6, 4, 5))
})

test_that("compute_endogenous_features derives requested statistics", {
  events <- data.frame(
    sender = c("a", "b", "b", "a", "c"),
    receiver = c("b", "a", "c", "c", "a"),
    time = c(1, 2, 3, 4, 5)
  )

  feats <- compute_endogenous_features(events,
    stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency")
  )

  expect_true(all(c(
    "sender_outdegree", "receiver_indegree", "reciprocity", "recency"
  ) %in% names(feats)))

  expect_equal(feats$sender_outdegree,
    c(0, 0, 1, 1, 0),
    tolerance = 1e-8
  )
  expect_equal(feats$receiver_indegree,
    c(0, 0, 0, 1, 1),
    tolerance = 1e-8
  )
  expect_equal(feats$reciprocity, c(0, 1, 0, 0, 1))

  expect_true(is.na(feats$recency[1]))
  expect_true(is.na(feats$recency[3]))
})

test_that("sample_non_events supports scope/mode combinations", {
  events <- data.frame(
    sender = c("a", "b", "c", "a"),
    receiver = c("b", "c", "a", "c"),
    time = c(1, 2, 3, 4)
  )

  sampled_one <- sample_non_events(events,
    n_controls = 2,
    scope = "all",
    mode = "one",
    seed = 123
  )

  controls_one <- subset(sampled_one, event == 0)
  expect_equal(nrow(controls_one), 8)
  expect_true(all(controls_one$sender != controls_one$receiver))

  sampled_two <- sample_non_events(events,
    n_controls = 1,
    scope = "appearance",
    mode = "two",
    seed = 321
  )

  controls_two <- subset(sampled_two, event == 0)
  expect_equal(nrow(controls_two), 4)
  expect_true(all(controls_two$sender %in% c("a", "b", "c")))
  expect_true(all(controls_two$receiver %in% c("a", "b", "c")))

  by_stratum <- aggregate(event ~ stratum, sampled_two, length)
  expect_true(all(by_stratum$event == 2))
})
