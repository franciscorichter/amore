#' Simulate relational event sequences
#'
#' Generate a simple relational event log for a sender set and receiver set
#' using a softmax allocation rule over dyadic intensities. The process follows
#' the Gillespie algorithm, where the time between events is drawn from an
#' exponential distribution with rate equal to the sum of all dyadic intensities.
#'
#' @param n_events Number of events to generate.
#' @param senders Character vector listing the sender set \eqn{\mathcal{S}}.
#' @param receivers Character vector listing the receiver set \eqn{\mathcal{R}}.
#' @param baseline_rate Positive scalar. A constant baseline hazard multiplier
#'   applied to all dyads. Defaults to 1.
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
#' @param n_controls Integer; number of non-events (controls) to sample
#'   uniformly at random for each realized event. If \code{n_controls > 0}, the
#'   function returns a case-control data frame suitable for conditional logistic
#'   regression / GAM modeling. Defaults to 0.
#'
#' @return If \code{n_controls = 0}, a data.frame with columns \code{sender},
#'   \code{receiver} and \code{time}. If \code{n_controls > 0}, it returns a
#'   long-format data.frame with additional columns \code{stratum} (grouping an
#'   event with its controls) and \code{event} (1 for the realized event,
#'   0 for controls).
#' @export
#'
#' @examples
#' set.seed(1)
#' senders <- receivers <- LETTERS[1:3]
#' sender_cov <- data.frame(activity = c(0.5, -0.2, 1.1))
#' receiver_cov <- data.frame(popularity = c(0.1, 0.3, -0.4))
#' # Standard event simulation
#' events <- simulate_relational_events(
#'   n_events = 5,
#'   senders = senders,
#'   receivers = receivers,
#'   sender_covariates = sender_cov,
#'   sender_effects = 1,
#'   receiver_covariates = receiver_cov,
#'   receiver_effects = 2
#' )
#' events
#'
#' # Case-control generation for partial likelihood inference
#' cc_events <- simulate_relational_events(
#'   n_events = 5,
#'   senders = senders,
#'   receivers = receivers,
#'   sender_covariates = sender_cov,
#'   sender_effects = 1,
#'   n_controls = 2
#' )
#' head(cc_events)
simulate_relational_events <- function(
    n_events,
    senders,
    receivers,
    baseline_rate = 1,
    start_time = 0,
    horizon = Inf,
    baseline_logits = NULL,
    sender_covariates = NULL,
    sender_effects = NULL,
    receiver_covariates = NULL,
    receiver_effects = NULL,
    allow_loops = FALSE,
    n_controls = 0) {
  stopifnot(length(n_events) == 1, n_events > 0)
  stopifnot(length(baseline_rate) == 1, baseline_rate > 0)
  stopifnot(length(start_time) == 1)
  stopifnot(length(horizon) == 1)
  stopifnot(length(n_controls) == 1, n_controls >= 0)

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

  weights <- exp(log_weights) * baseline_rate
  weights[!is.finite(weights)] <- 0
  total_weight <- sum(weights)
  if (total_weight <= 0) {
    stop("No admissible dyads with positive intensity.")
  }

  # Normalize weights into probabilities
  probs <- as.vector(weights) / total_weight

  # Ensure enough valid non-events can be sampled if requested
  valid_dyads <- which(weights > 0)
  n_valid_dyads <- length(valid_dyads)
  if (n_controls > 0 && n_controls >= n_valid_dyads) {
    stop("Requested n_controls is >= the number of admissible dyads.")
  }

  event_senders <- character(n_events)
  event_receivers <- character(n_events)
  event_times <- numeric(n_events)

  if (n_controls > 0) {
    control_senders <- character(n_events * n_controls)
    control_receivers <- character(n_events * n_controls)
    control_times <- numeric(n_events * n_controls)
    control_strata <- integer(n_events * n_controls)
  }

  current_time <- start_time
  event_counter <- 0L

  for (i in seq_len(n_events)) {
    # Gillespie timing: rexp(1, rate = sum of all hazards)
    dt <- stats::rexp(1, rate = total_weight)
    current_time <- current_time + dt
    if (current_time > horizon) {
      break
    }

    choice <- sample.int(S * R, size = 1, prob = probs)
    s_idx <- ((choice - 1L) %% S) + 1L
    r_idx <- ((choice - 1L) %/% S) + 1L

    event_counter <- event_counter + 1L
    event_senders[event_counter] <- senders[s_idx]
    event_receivers[event_counter] <- receivers[r_idx]
    event_times[event_counter] <- current_time

    if (n_controls > 0) {
      # Sample 'n_controls' non-events uniformally from valid dyads excluding the chosen one
      non_event_pool <- setdiff(valid_dyads, choice)
      if (length(non_event_pool) < n_controls) {
        # Should not happen if `n_controls < n_valid_dyads` check holds initially
        # taking into account `choice` is one valid dyad.
        non_event_choices <- non_event_pool
      } else {
        # uniform sampling of non-events
        non_event_choices <- sample(non_event_pool, size = n_controls, replace = FALSE)
      }

      ctrl_start_idx <- (event_counter - 1L) * n_controls + 1L
      ctrl_end_idx <- event_counter * n_controls

      c_s_idxs <- ((non_event_choices - 1L) %% S) + 1L
      c_r_idxs <- ((non_event_choices - 1L) %/% S) + 1L

      control_senders[ctrl_start_idx:ctrl_end_idx] <- senders[c_s_idxs]
      control_receivers[ctrl_start_idx:ctrl_end_idx] <- receivers[c_r_idxs]
      control_times[ctrl_start_idx:ctrl_end_idx] <- current_time
      control_strata[ctrl_start_idx:ctrl_end_idx] <- event_counter
    }
  }

  if (event_counter == 0L) {
    if (n_controls == 0) {
      return(data.frame(sender = character(0), receiver = character(0), time = numeric(0)))
    } else {
      return(data.frame(
        stratum = integer(0), event = integer(0),
        sender = character(0), receiver = character(0), time = numeric(0)
      ))
    }
  }

  if (n_controls == 0) {
    return(data.frame(
      sender = event_senders[seq_len(event_counter)],
      receiver = event_receivers[seq_len(event_counter)],
      time = event_times[seq_len(event_counter)],
      stringsAsFactors = FALSE
    ))
  } else {
    realized_df <- data.frame(
      stratum = seq_len(event_counter),
      event = 1L,
      sender = event_senders[seq_len(event_counter)],
      receiver = event_receivers[seq_len(event_counter)],
      time = event_times[seq_len(event_counter)],
      stringsAsFactors = FALSE
    )

    c_records <- event_counter * n_controls
    control_df <- data.frame(
      stratum = control_strata[seq_len(c_records)],
      event = 0L,
      sender = control_senders[seq_len(c_records)],
      receiver = control_receivers[seq_len(c_records)],
      time = control_times[seq_len(c_records)],
      stringsAsFactors = FALSE
    )

    out <- rbind(realized_df, control_df)
    out <- out[order(out$time, decreasing = FALSE), ]
    rownames(out) <- NULL
    return(out)
  }
}
