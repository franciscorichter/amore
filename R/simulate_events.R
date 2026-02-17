#' Simulate relational event sequences
#'
#' Generate a simple relational event log for a sender set and receiver set
#' using a softmax allocation rule over dyadic intensities. The function is
#' intentionally lightweight so it can be used for quick experiments and for
#' stress-testing follow-up estimation routines.
#'
#' @param n_events Number of events to generate.
#' @param senders Character vector listing the sender set \eqn{\mathcal{S}}.
#' @param receivers Character vector listing the receiver set \eqn{\mathcal{R}}.
#' @param event_rate Positive scalar controlling the expected number of events
#'   per unit time in the simulated point process.
#' @param start_time Initial time stamp.
#' @param horizon Optional maximum horizon; simulation stops once the cumulative
#'   time would exceed this value.
#' @param baseline_logits Optional \code{length(senders) x length(receivers)}
#'   matrix of baseline log-intensities. Defaults to zeros.
#' @param sender_covariates Optional numeric data.frame/matrix with one row per
#'   sender.
#' @param sender_effects Optional numeric vector of coefficients for
#'   \code{sender_covariates}. Required when sender covariates are supplied.
#' @param receiver_covariates Optional numeric data.frame/matrix with one row per
#'   receiver.
#' @param receiver_effects Optional numeric vector of coefficients for
#'   \code{receiver_covariates}. Required when receiver covariates are supplied.
#' @param allow_loops Logical; whether sender and receiver can coincide.
#'
#' @return A tibble-like data.frame with columns \code{sender}, \code{receiver}
#'   and \code{time} of length less than or equal to \code{n_events} (the
#'   horizon can truncate the process).
#' @export
#'
#' @examples
#' set.seed(1)
#' senders <- receivers <- LETTERS[1:3]
#' sender_cov <- data.frame(activity = c(0.5, -0.2, 1.1))
#' receiver_cov <- data.frame(popularity = c(0.1, 0.3, -0.4))
#' events <- simulate_relational_events(
#'   n_events = 5,
#'   senders = senders,
#'   receivers = receivers,
#'   event_rate = 2,
#'   sender_covariates = sender_cov,
#'   sender_effects = 1,
#'   receiver_covariates = receiver_cov,
#'   receiver_effects = 2
#' )
#' events
simulate_relational_events <- function(
    n_events,
    senders,
    receivers,
    event_rate = 1,
    start_time = 0,
    horizon = Inf,
    baseline_logits = NULL,
    sender_covariates = NULL,
    sender_effects = NULL,
    receiver_covariates = NULL,
    receiver_effects = NULL,
    allow_loops = FALSE) {
  stopifnot(length(n_events) == 1, n_events > 0)
  stopifnot(length(event_rate) == 1, event_rate > 0)
  stopifnot(length(start_time) == 1)
  stopifnot(length(horizon) == 1)

  senders <- as.character(senders)
  receivers <- as.character(receivers)
  S <- length(senders)
  R <- length(receivers)

  if (S == 0 || R == 0) {
    stop("Both sender and receiver sets must be non-empty.")
  }

  if (is.null(baseline_logits)) {
    baseline_logits <- matrix(0, nrow = S, ncol = R)
  }
  if (!is.matrix(baseline_logits) || any(dim(baseline_logits) != c(S, R))) {
    stop("baseline_logits must be an S x R matrix.")
  }

  sender_score <- rep(0, S)
  if (!is.null(sender_covariates)) {
    sc <- as.matrix(sender_covariates)
    if (nrow(sc) != S) {
      stop("sender_covariates must have one row per sender.")
    }
    if (is.null(sender_effects)) {
      stop("sender_effects must be supplied when sender_covariates are used.")
    }
    sender_effects <- as.numeric(sender_effects)
    if (ncol(sc) != length(sender_effects)) {
      stop("Length of sender_effects must match number of sender covariates.")
    }
    sender_score <- as.numeric(sc %*% sender_effects)
  }

  receiver_score <- rep(0, R)
  if (!is.null(receiver_covariates)) {
    rc <- as.matrix(receiver_covariates)
    if (nrow(rc) != R) {
      stop("receiver_covariates must have one row per receiver.")
    }
    if (is.null(receiver_effects)) {
      stop("receiver_effects must be supplied when receiver_covariates are used.")
    }
    receiver_effects <- as.numeric(receiver_effects)
    if (ncol(rc) != length(receiver_effects)) {
      stop("Length of receiver_effects must match number of receiver covariates.")
    }
    receiver_score <- as.numeric(rc %*% receiver_effects)
  }

  log_weights <- baseline_logits + outer(sender_score, receiver_score, "+")

  if (!allow_loops) {
    same_actor <- outer(senders, receivers, "==")
    log_weights[same_actor] <- -Inf
  }

  weights <- exp(log_weights)
  weights[!is.finite(weights)] <- 0
  total_weight <- sum(weights)
  if (total_weight <= 0) {
    stop("No admissible dyads with positive intensity.")
  }

  event_senders <- character(n_events)
  event_receivers <- character(n_events)
  event_times <- numeric(n_events)
  current_time <- start_time
  event_counter <- 0L

  for (i in seq_len(n_events)) {
    current_time <- current_time + stats::rexp(1, rate = event_rate)
    if (current_time > horizon) {
      break
    }
    choice <- sample.int(S * R, size = 1, prob = as.vector(weights))
    s_idx <- ((choice - 1L) %/% R) + 1L
    r_idx <- ((choice - 1L) %% R) + 1L
    event_counter <- event_counter + 1L
    event_senders[event_counter] <- senders[s_idx]
    event_receivers[event_counter] <- receivers[r_idx]
    event_times[event_counter] <- current_time
  }

  if (event_counter == 0L) {
    return(data.frame(sender = character(0), receiver = character(0), time = numeric(0)))
  }

  data.frame(
    sender = event_senders[seq_len(event_counter)],
    receiver = event_receivers[seq_len(event_counter)],
    time = event_times[seq_len(event_counter)],
    stringsAsFactors = FALSE
  )
}
