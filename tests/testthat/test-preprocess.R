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

test_that("sample_non_events supports citation scope and removal risk", {
  events <- data.frame(
    sender = c("p1", "p2", "p3", "p4"),
    receiver = c("seed", "p1", "p2", "p3"),
    time = c(1, 2, 2, 3)
  )

  citation <- sample_non_events(events,
    n_controls = 1,
    scope = "citation",
    mode = "two",
    seed = 42
  )

  controls_cite <- subset(citation, event == 0)
  first_time <- tapply(events$time, events$sender, min)
  tol <- sqrt(.Machine$double.eps)

  for (row in seq_len(nrow(controls_cite))) {
    stratum <- controls_cite$stratum[row]
    t_i <- events$time[stratum]
    eligible_senders <- names(first_time)[abs(first_time - t_i) < tol]
    expect_true(controls_cite$sender[row] %in% eligible_senders)
    eligible_receivers <- names(first_time)[first_time < t_i]
    expect_true(controls_cite$receiver[row] %in% unique(c(eligible_receivers, events$receiver[stratum])))
  }

  invasion <- sample_non_events(events,
    n_controls = 1,
    scope = "all",
    mode = "two",
    risk = "remove",
    seed = 24
  )

  controls_inv <- subset(invasion, event == 0)
  realized <- data.frame(
    stratum = seq_len(nrow(events)),
    sender = events$sender,
    receiver = events$receiver
  )

  for (row in seq_len(nrow(controls_inv))) {
    prior <- realized[realized$stratum < controls_inv$stratum[row], ]
    if (nrow(prior)) {
      matches <- prior$sender == controls_inv$sender[row] & prior$receiver == controls_inv$receiver[row]
      expect_false(any(matches))
    }
  }
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

test_that("reciprocity variants are computed correctly", {
  events <- data.frame(
    sender   = c("a", "b", "a", "b"),
    receiver = c("b", "a", "b", "a"),
    time     = c(1,   3,   5,   8)
  )

  feats <- compute_endogenous_features(events,
    stats = c("reciprocity", "reciprocity_binary", "reciprocity_count",
              "reciprocity_time_recent", "reciprocity_time_first")
  )

  # Event 1: a->b at t=1.  No prior b->a.

  expect_equal(feats$reciprocity[1], 0L)
  expect_equal(feats$reciprocity_binary[1], 0L)
  expect_equal(feats$reciprocity_count[1], 0)
  expect_true(is.na(feats$reciprocity_time_recent[1]))
  expect_true(is.na(feats$reciprocity_time_first[1]))

  # Event 2: b->a at t=3.  Prior a->b at t=1.
  expect_equal(feats$reciprocity[2], 1L)
  expect_equal(feats$reciprocity_count[2], 1)
  expect_equal(feats$reciprocity_time_recent[2], 2)
  expect_equal(feats$reciprocity_time_first[2], 2)

  # Event 3: a->b at t=5.  Prior b->a at t=3.
  expect_equal(feats$reciprocity[3], 1L)
  expect_equal(feats$reciprocity_count[3], 1)
  expect_equal(feats$reciprocity_time_recent[3], 2)
  expect_equal(feats$reciprocity_time_first[3], 2)

  # Event 4: b->a at t=8.  Prior a->b at t=1 and t=5.
  expect_equal(feats$reciprocity_count[4], 2)
  expect_equal(feats$reciprocity_time_recent[4], 3)   # 8 - 5
  expect_equal(feats$reciprocity_time_first[4], 7)    # 8 - 1
})

test_that("reciprocity_exp_decay uses half_life correctly", {
  events <- data.frame(
    sender   = c("a", "a", "b"),
    receiver = c("b", "b", "a"),
    time     = c(0,   1,   3)
  )

  hl <- 2
  feats <- compute_endogenous_features(events,
    stats = "reciprocity_exp_decay", half_life = hl
  )

  # Event 3: b->a at t=3.  Two prior a->b events at t=0 and t=1.
  # exp(-(3-0)*ln2/2) + exp(-(3-1)*ln2/2)
  expected <- exp(-3 * log(2) / 2) + exp(-2 * log(2) / 2)
  expect_equal(feats$reciprocity_exp_decay[3], expected, tolerance = 1e-10)
})

test_that("transitivity stats are computed correctly", {
  # a->b at t=1, b->c at t=2  =>  two-path a->b->c exists for event a->c
  # a->c at t=4 should see transitivity via intermediary b
  events <- data.frame(
    sender   = c("a", "b", "a"),
    receiver = c("b", "c", "c"),
    time     = c(1,   2,   4)
  )

  feats <- compute_endogenous_features(events,
    stats = c("transitivity_binary", "transitivity_count",
              "transitivity_binary_ordered", "transitivity_count_ordered",
              "transitivity_time_recent", "transitivity_time_first",
              "transitivity_time_recent_ordered", "transitivity_time_first_ordered")
  )

  # Events 1 & 2 have no two-paths yet
  expect_equal(feats$transitivity_binary[1], 0L)
  expect_equal(feats$transitivity_binary[2], 0L)

  # Event 3: a->c at t=4.  Two-path a->b->c via b.
  # Unordered completion = max(t(a->b)=1, t(b->c)=2) = 2
  expect_equal(feats$transitivity_binary[3], 1L)
  expect_equal(feats$transitivity_count[3], 1)
  expect_equal(feats$transitivity_time_recent[3], 2)  # 4 - 2
  expect_equal(feats$transitivity_time_first[3], 2)   # same, only one k

  # Ordered: a->b at t=1 before b->c at t=2: valid
  expect_equal(feats$transitivity_binary_ordered[3], 1L)
  expect_equal(feats$transitivity_count_ordered[3], 1)
  expect_equal(feats$transitivity_time_recent_ordered[3], 2) # 4 - 2
})

test_that("transitivity ordered vs unordered differ when order is reversed", {
  # b->c at t=1, a->b at t=2 => two-path a->b->c exists (unordered)
  # but order restriction fails: a->b (t=2) is NOT before b->c (t=1)
  events <- data.frame(
    sender   = c("b", "a", "a"),
    receiver = c("c", "b", "c"),
    time     = c(1,   2,   4)
  )

  feats <- compute_endogenous_features(events,
    stats = c("transitivity_binary", "transitivity_binary_ordered",
              "transitivity_time_recent", "transitivity_time_recent_ordered")
  )

  # Event 3: a->c.  Intermediary b: a->b(t=2), b->c(t=1)
  # Unordered: exists (both happened before t=4)
  expect_equal(feats$transitivity_binary[3], 1L)
  expect_equal(feats$transitivity_time_recent[3], 2) # 4 - max(2,1) = 2

  # Ordered: need a->b before b->c.  a->b at t=2, b->c at t=1.
  # No b->c event after min(a->b times)=2 => invalid
  expect_equal(feats$transitivity_binary_ordered[3], 0L)
  expect_true(is.na(feats$transitivity_time_recent_ordered[3]))
})

test_that("cyclic closure stats are computed correctly", {
  # Cyclic two-path for event s->r: need r->k and k->s
  # r->k at t=1, k->s at t=2  =>  cyclic two-path exists for s->r
  events <- data.frame(
    sender   = c("b", "k", "a"),
    receiver = c("k", "a", "b"),
    time     = c(1,   2,   4)
  )

  feats <- compute_endogenous_features(events,
    stats = c("cyclic_binary", "cyclic_count", "cyclic_time_recent")
  )

  # Event 3: a->b at t=4.  Cyclic: need b->k and k->a.
  # b->k at t=1, k->a at t=2.  Intermediary k exists.
  expect_equal(feats$cyclic_binary[3], 1L)
  expect_equal(feats$cyclic_count[3], 1)
  expect_equal(feats$cyclic_time_recent[3], 2)  # 4 - max(1, 2) = 2

  # Earlier events have no cyclic two-paths
  expect_equal(feats$cyclic_binary[1], 0L)
  expect_equal(feats$cyclic_binary[2], 0L)
})

test_that("sending balance stats are computed correctly", {
  # Sending balance for s->r: need s->k and r->k (shared target)
  events <- data.frame(
    sender   = c("a", "b", "a"),
    receiver = c("k", "k", "b"),
    time     = c(1,   2,   5)
  )

  feats <- compute_endogenous_features(events,
    stats = c("sending_balance_binary", "sending_balance_count",
              "sending_balance_time_recent")
  )

  # Event 3: a->b at t=5.  Shared target k: a->k(t=1), b->k(t=2).
  expect_equal(feats$sending_balance_binary[3], 1L)
  expect_equal(feats$sending_balance_count[3], 1)
  expect_equal(feats$sending_balance_time_recent[3], 3)  # 5 - max(1,2) = 3

  expect_equal(feats$sending_balance_binary[1], 0L)
})

test_that("receiving balance stats are computed correctly", {
  # Receiving balance for s->r: need k->s and k->r (shared source)
  events <- data.frame(
    sender   = c("k", "k", "a"),
    receiver = c("a", "b", "b"),
    time     = c(1,   3,   5)
  )

  feats <- compute_endogenous_features(events,
    stats = c("receiving_balance_binary", "receiving_balance_count",
              "receiving_balance_time_recent")
  )

  # Event 3: a->b at t=5.  Shared source k: k->a(t=1), k->b(t=3).
  expect_equal(feats$receiving_balance_binary[3], 1L)
  expect_equal(feats$receiving_balance_count[3], 1)
  expect_equal(feats$receiving_balance_time_recent[3], 2)  # 5 - max(1,3) = 2

  expect_equal(feats$receiving_balance_binary[1], 0L)
})

test_that("transitivity_exp_decay uses half_life correctly", {
  events <- data.frame(
    sender   = c("a", "b", "a"),
    receiver = c("b", "c", "c"),
    time     = c(0,   1,   5)
  )

  hl <- 3
  feats <- compute_endogenous_features(events,
    stats = c("transitivity_exp_decay", "transitivity_exp_decay_ordered"),
    half_life = hl
  )

  # Event 3: a->c at t=5.  Two-path via b: completion = max(0, 1) = 1.
  # exp(-(5-1)*ln2/3)
  expected <- exp(-4 * log(2) / 3)
  expect_equal(feats$transitivity_exp_decay[3], expected, tolerance = 1e-10)
  expect_equal(feats$transitivity_exp_decay_ordered[3], expected, tolerance = 1e-10)
})

test_that("multiple intermediaries are counted correctly", {
  # Two intermediaries for a->c: via b and via d
  events <- data.frame(
    sender   = c("a", "b", "a", "d", "a"),
    receiver = c("b", "c", "d", "c", "c"),
    time     = c(1,   2,   3,   4,   6)
  )

  feats <- compute_endogenous_features(events,
    stats = c("transitivity_count", "transitivity_time_recent",
              "transitivity_time_first")
  )

  # Event 5: a->c at t=6.
  # Via b: a->b(1), b->c(2), completion = max(1,2) = 2
  # Via d: a->d(3), d->c(4), completion = max(3,4) = 4
  expect_equal(feats$transitivity_count[5], 2)
  expect_equal(feats$transitivity_time_recent[5], 2)  # 6 - max(2,4) = 6-4 = 2
  expect_equal(feats$transitivity_time_first[5], 4)   # 6 - min(2,4) = 6-2 = 4
})

test_that("exp_decay stats require half_life", {
  events <- data.frame(sender = "a", receiver = "b", time = 1)
  expect_error(
    compute_endogenous_features(events, stats = "reciprocity_exp_decay"),
    "half_life"
  )
  expect_error(
    compute_endogenous_features(events, stats = "transitivity_exp_decay"),
    "half_life"
  )
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
