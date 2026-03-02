#' Standardize a relational event log
#'
#' Module A focuses on preprocessing utilities. This helper normalizes user
#' supplied event logs into the canonical `sender`/`receiver`/`time` structure
#' expected elsewhere in the package. It also handles common cleaning tasks such
#' as sorting, dropping missing rows, and removing loops.
#'
#' @param event_log A data.frame (or tibble) containing at least one row per
#'   event.
#' @param sender_col,receiver_col,time_col Column names storing the sender,
#'   receiver, and time information.
#' @param sort Logical; should the output be sorted by time (ties are kept in
#'   input order)?
#' @param drop_nas Logical; if `TRUE`, rows with missing sender/receiver/time are
#'   removed. Otherwise an error is thrown when NAs are present.
#' @param drop_loops Logical; when `TRUE`, self-loops (`sender == receiver`) are
#'   dropped.
#' @param strictly_increasing_time Logical; if `TRUE`, an error is raised when
#'   non-increasing time stamps are detected after sorting.
#' @param remove_duplicates Logical; drop duplicated combinations of
#'   sender/receiver/time.
#' @param keep_extra Logical; if `FALSE`, only the standardized columns are
#'   returned. When `TRUE`, additional columns from the original input are
#'   preserved.
#'
#' @return A data.frame with columns `sender`, `receiver`, and `time`. The return
#'   object is tagged with class `"amore_event_log"` for downstream dispatch.
#' @export
standardize_event_log <- function(
    event_log,
    sender_col = "sender",
    receiver_col = "receiver",
    time_col = "time",
    sort = TRUE,
    drop_nas = TRUE,
    drop_loops = FALSE,
    strictly_increasing_time = FALSE,
    remove_duplicates = TRUE,
    keep_extra = TRUE) {
  if (!is.data.frame(event_log)) {
    stop("`event_log` must be a data.frame.")
  }

  required <- c(sender_col, receiver_col, time_col)
  missing_cols <- setdiff(required, names(event_log))
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  out <- if (keep_extra) {
    event_log
  } else {
    event_log[, required, drop = FALSE]
  }

  col_map <- c(sender_col, receiver_col, time_col)
  target_names <- c("sender", "receiver", "time")
  for (i in seq_along(col_map)) {
    colname <- col_map[i]
    target <- target_names[i]
    if (colname %in% names(out)) {
      names(out)[names(out) == colname] <- target
    }
  }

  base_cols <- c("sender", "receiver", "time")
  out$sender <- as.character(out$sender)
  out$receiver <- as.character(out$receiver)
  out$time <- as.numeric(out$time)

  na_rows <- !stats::complete.cases(out[, base_cols])
  if (any(na_rows)) {
    if (drop_nas) {
      out <- out[!na_rows, , drop = FALSE]
    } else {
      stop("Missing values detected in sender/receiver/time columns.")
    }
  }

  if (drop_loops) {
    out <- out[out$sender != out$receiver, , drop = FALSE]
  }

  if (remove_duplicates && nrow(out)) {
    dup_mask <- duplicated(out[, base_cols])
    out <- out[!dup_mask, , drop = FALSE]
  }

  if (sort && nrow(out)) {
    ord <- order(out$time, seq_len(nrow(out)))
    out <- out[ord, , drop = FALSE]
  }

  if (strictly_increasing_time && nrow(out) > 1) {
    diffs <- diff(out$time)
    if (any(diffs <= 0)) {
      stop("Time column must be strictly increasing when `strictly_increasing_time = TRUE`.")
    }
  }

  rownames(out) <- NULL
  class(out) <- unique(c("amore_event_log", class(out)))
  out
}

#' Attach static covariates to an event log
#'
#' This helper augments an event log with sender and/or receiver covariates that
#' live in separate lookup tables. It is designed for static covariates (one row
#' per actor). Dynamic covariates should be merged manually before calling this
#' helper.
#'
#' @param event_log A standardized event log containing columns `sender` and
#'   `receiver`.
#' @param sender_covariates,receiver_covariates Data frames with one row per
#'   actor. Each must include the identifier column specified by `actor_col`.
#' @param actor_col Name of the identifier column inside the covariate tables.
#' @param sender_prefix,receiver_prefix Prefixes applied to the appended
#'   covariate column names.
#' @param allow_missing Logical; if `FALSE`, missing actors trigger an error.
#'
#' @return The input `event_log` with additional columns for each covariate table
#'   supplied.
#' @export
attach_static_covariates <- function(
    event_log,
    sender_covariates = NULL,
    receiver_covariates = NULL,
    actor_col = "actor",
    sender_prefix = "sender_",
    receiver_prefix = "receiver_",
    allow_missing = TRUE) {
  if (!is.data.frame(event_log)) {
    stop("`event_log` must be a data.frame.")
  }
  required_cols <- c("sender", "receiver")
  missing_cols <- setdiff(required_cols, names(event_log))
  if (length(missing_cols)) {
    stop("Event log is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  out <- event_log

  append_covariates <- function(df, covariates, target_col, prefix) {
    if (is.null(covariates)) {
      return(df)
    }
    if (!is.data.frame(covariates)) {
      stop("Covariates must be provided as a data.frame.")
    }
    if (!actor_col %in% names(covariates)) {
      stop("Covariate table is missing the actor identifier column `", actor_col, "`.")
    }
    if (anyDuplicated(covariates[[actor_col]])) {
      stop("Covariate table for `", prefix, "` actors contains duplicate identifiers.")
    }

    value_cols <- setdiff(names(covariates), actor_col)
    if (!length(value_cols)) {
      stop("Covariate table for `", prefix, "` actors contains no covariate columns.")
    }

    matches <- match(df[[target_col]], covariates[[actor_col]])
    if (!allow_missing && any(is.na(matches))) {
      missing_ids <- unique(df[[target_col]][is.na(matches)])
      stop(
        "Missing covariate rows for actors: ",
        paste(missing_ids, collapse = ", ")
      )
    }

    cov_subset <- covariates[matches, value_cols, drop = FALSE]
    names(cov_subset) <- paste0(prefix, value_cols)
    df <- cbind(df, cov_subset)
    df
  }

  out <- append_covariates(out, sender_covariates, "sender", sender_prefix)
  out <- append_covariates(out, receiver_covariates, "receiver", receiver_prefix)
  out
}

#' Compute endogenous event-network statistics
#'
#' Given a standardized relational event log, this helper derives simple
#' historical statistics for each event. These summaries can be fed into model
#' formulas or exported as part of inference-ready design matrices.
#'
#' @param event_log A data.frame containing at least `sender`, `receiver`, and
#'   `time` columns.
#' @param stats Character vector describing which statistics to compute. Allowed
#'   values are `"sender_outdegree"`, `"receiver_indegree"`, `"reciprocity"`, and
#'   `"recency"`.
#' @param sort Logical; when `TRUE`, events are ordered by time prior to
#'   computing summaries (ties preserve input order).
#' @details The available statistics are defined for an event occurring at time
#'   \eqn{t_i} from sender \eqn{s_i} to receiver \eqn{r_i} as follows (before
#'   the event is logged):
#'   \describe{
#'     \item{`sender_outdegree`}{\eqn{\text{outdeg}_{s_i}(t_i^-)}, the number of
#'       events sent by \eqn{s_i} prior to \eqn{t_i}.}
#'     \item{`receiver_indegree`}{\eqn{\text{indeg}_{r_i}(t_i^-)}, the number of
#'       events received by \eqn{r_i} prior to \eqn{t_i}.}
#'     \item{`reciprocity`}{Indicator that the reverse dyad has ever been
#'       observed: \eqn{\mathbb{1}[\exists\ j < i : (s_j, r_j) = (r_i, s_i)]}.}
#'     \item{`recency`}{If the dyad has previously interacted, the elapsed time
#'       since the most recent event: \eqn{t_i - \max\{ t_j : j < i, (s_j,r_j) =
#'       (s_i, r_i) \}}; otherwise `NA`.}
#'   }
#' @return The event log with added columns, one per requested statistic.
#' @export
compute_endogenous_features <- function(
    event_log,
    stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency"),
    sort = TRUE) {
  if (!is.data.frame(event_log)) {
    stop("`event_log` must be a data.frame.")
  }
  required_cols <- c("sender", "receiver", "time")
  missing_cols <- setdiff(required_cols, names(event_log))
  if (length(missing_cols)) {
    stop("Event log is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  allowed <- c("sender_outdegree", "receiver_indegree", "reciprocity", "recency")
  bad <- setdiff(stats, allowed)
  if (length(bad)) {
    stop("Unsupported statistics requested: ", paste(bad, collapse = ", "))
  }
  stats <- intersect(allowed, stats)
  if (!length(stats)) {
    stop("At least one statistic must be requested.")
  }

  log_df <- event_log
  if (sort && nrow(log_df)) {
    ord <- order(log_df$time, seq_len(nrow(log_df)))
    log_df <- log_df[ord, , drop = FALSE]
  }

  n <- nrow(log_df)
  if (!n) {
    for (stat in stats) {
      log_df[[stat]] <- numeric(0)
    }
    return(if (identical(log_df, event_log)) log_df else log_df[NULL, ])
  }

  sender_counts <- numeric(0)
  receiver_counts <- numeric(0)
  seen_dyads <- new.env(parent = emptyenv())
  last_times <- numeric(0)

  get_count <- function(x, key) {
    if (!length(x)) return(0)
    val <- x[key]
    if (!length(val) || is.na(val)) return(0)
    val
  }

  dyad_key <- function(s, r) paste0(s, "->", r)

  if ("sender_outdegree" %in% stats) {
    log_df$sender_outdegree <- numeric(n)
  }
  if ("receiver_indegree" %in% stats) {
    log_df$receiver_indegree <- numeric(n)
  }
  if ("reciprocity" %in% stats) {
    log_df$reciprocity <- integer(n)
  }
  if ("recency" %in% stats) {
    log_df$recency <- rep(NA_real_, n)
  }

  for (i in seq_len(n)) {
    s <- log_df$sender[i]
    r <- log_df$receiver[i]
    t <- log_df$time[i]

    if ("sender_outdegree" %in% stats) {
      log_df$sender_outdegree[i] <- get_count(sender_counts, s)
    }
    if ("receiver_indegree" %in% stats) {
      log_df$receiver_indegree[i] <- get_count(receiver_counts, r)
    }
    if ("reciprocity" %in% stats) {
      reverse_key <- dyad_key(r, s)
      log_df$reciprocity[i] <- as.integer(!is.null(seen_dyads[[reverse_key]]))
    }
    if ("recency" %in% stats) {
      key <- dyad_key(s, r)
      last_time <- last_times[key]
      if (length(last_time) && !is.na(last_time)) {
        log_df$recency[i] <- t - last_time
      }
    }

    sender_counts[s] <- get_count(sender_counts, s) + 1
    receiver_counts[r] <- get_count(receiver_counts, r) + 1

    key_sr <- dyad_key(s, r)
    seen_dyads[[key_sr]] <- TRUE
    last_times[key_sr] <- t
  }

  log_df
}

#' Sample non-events for inference
#'
#' Given an observed event log, generate nested case-control data by sampling
#' counterfactual sender--receiver pairs according to predefined strategies.
#'
#' @param event_log Data frame with columns `sender`, `receiver`, and `time`.
#' @param n_controls Number of non-events (controls) to sample per realized
#'   event.
#' @param scope Candidate set definition. `"all"` uses every actor observed in
#'   the data; `"appearance"` restricts to actors that have appeared in prior
#'   events; `"citation"` matches citation networks where senders are restricted
#'   to the papers that debut at the current time and receivers must have
#'   appeared earlier.
#' @param mode `"one"` draws both sender and receiver from the same candidate
#'   pool (single-mode). `"two"` samples sender and receiver from separate pools
#'   (two-mode).
#' @param risk Strategy governing the risk set. `"standard"` (default) keeps all
#'   unrealized dyads available across strata, whereas `"remove"` deletes a dyad
#'   from the candidate pool after it has occurred (useful for processes such as
#'   species invasions where a pair cannot reoccur).
#' @param allow_loops Logical; can sampled non-events have identical sender and
#'   receiver?
#' @param seed Optional seed for reproducibility.
#' @param max_attempts Maximum resampling attempts per control before giving up
#'   (prevents infinite loops when candidate sets are small).
#'
#' @return A data.frame containing the original events (`event = 1`) and the
#'   sampled controls (`event = 0`), grouped by `stratum` identifiers.
#' @export
sample_non_events <- function(
    event_log,
    n_controls = 1,
    scope = c("all", "appearance", "citation"),
    mode = c("two", "one"),
    risk = c("standard", "remove"),
    allow_loops = FALSE,
    seed = NULL,
    max_attempts = 1000) {
  if (!is.data.frame(event_log)) {
    stop("`event_log` must be a data.frame.")
  }
  required_cols <- c("sender", "receiver", "time")
  missing_cols <- setdiff(required_cols, names(event_log))
  if (length(missing_cols)) {
    stop("Event log is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(n_controls) || length(n_controls) != 1 || n_controls < 1) {
    stop("`n_controls` must be a positive integer.")
  }
  n_controls <- as.integer(n_controls)

  scope <- match.arg(scope)
  mode <- match.arg(mode)
  risk <- match.arg(risk)

  if (!is.null(seed)) {
    old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    })
    set.seed(seed)
  }

  n_events <- nrow(event_log)
  if (n_events == 0) {
    stop("Event log contains no rows.")
  }

  sender_all <- unique(as.character(event_log$sender))
  receiver_all <- unique(as.character(event_log$receiver))
  combined_all <- sort(unique(c(sender_all, receiver_all)))

  sender_first_time <- tapply(event_log$time, event_log$sender, min)

  get_citation_senders <- function(time_val) {
    if (!length(sender_first_time)) {
      return(character(0))
    }
    tol <- if (is.finite(time_val)) sqrt(.Machine$double.eps) else 0
    matches <- abs(sender_first_time - time_val) < tol
    names(sender_first_time)[matches]
  }

  get_citation_receivers <- function(time_val) {
    if (!length(sender_first_time)) {
      return(character(0))
    }
    names(sender_first_time)[sender_first_time < time_val]
  }

  appearance_senders <- character(0)
  appearance_receivers <- character(0)
  appearance_combined <- character(0)

  get_candidates <- function(use_scope, time_val, current_sender, current_receiver) {
    if (use_scope == "all") {
      return(list(
        senders = sender_all,
        receivers = receiver_all,
        combined = combined_all
      ))
    }

    if (use_scope == "appearance") {
      senders <- if (length(appearance_senders)) appearance_senders else sender_all
      receivers <- if (length(appearance_receivers)) appearance_receivers else receiver_all
      combined <- if (length(appearance_combined)) appearance_combined else combined_all
      return(list(senders = senders, receivers = receivers, combined = combined))
    }

    senders <- get_citation_senders(time_val)
    if (!length(senders)) {
      senders <- character(0)
    }
    if (!current_sender %in% senders) {
      senders <- unique(c(senders, current_sender))
    }

    receivers <- get_citation_receivers(time_val)
    if (!length(receivers)) {
      receivers <- character(0)
    }
    if (!current_receiver %in% receivers) {
      receivers <- unique(c(receivers, current_receiver))
    }

    combined <- unique(c(senders, receivers))
    list(senders = senders, receivers = receivers, combined = combined)
  }

  dyad_key <- function(s, r) paste0(s, "->", r)

  removed_dyads <- new.env(parent = emptyenv())

  choose_pair <- function(cands, mode_choice) {
    if (mode_choice == "one") {
      needed <- if (allow_loops) 1L else 2L
      if (length(cands$combined) < needed && scope != "citation") {
        cands$combined <- combined_all
      }
      pair <- sample(cands$combined, size = 2, replace = allow_loops)
      list(sender = pair[1], receiver = pair[2])
    } else {
      if ((!length(cands$senders) || !length(cands$receivers)) && scope != "citation") {
        cands$senders <- sender_all
        cands$receivers <- receiver_all
      }
      s <- sample(cands$senders, size = 1)
      r <- sample(cands$receivers, size = 1)
      list(sender = s, receiver = r)
    }
  }

  has_viable_pair <- function(cands, mode_choice, current_sender, current_receiver) {
    check_pair <- function(s, r) {
      if (!allow_loops && identical(s, r)) {
        return(FALSE)
      }
      if (is.na(s) || is.na(r)) {
        return(FALSE)
      }
      if (identical(s, current_sender) && identical(r, current_receiver)) {
        return(FALSE)
      }
      if (risk == "remove" && !is.null(removed_dyads[[dyad_key(s, r)]])) {
        return(FALSE)
      }
      TRUE
    }

    if (mode_choice == "one") {
      combos <- cands$combined
      if (!length(combos)) {
        return(FALSE)
      }
      if (allow_loops) {
        for (s in combos) {
          for (r in combos) {
            if (check_pair(s, r)) return(TRUE)
          }
        }
        return(FALSE)
      }
      if (length(combos) < 2) {
        return(FALSE)
      }
      for (i in seq_along(combos)) {
        for (j in seq_along(combos)) {
          if (i == j) next
          if (check_pair(combos[i], combos[j])) return(TRUE)
        }
      }
      return(FALSE)
    }

    if (!length(cands$senders) || !length(cands$receivers)) {
      return(FALSE)
    }
    for (s in cands$senders) {
      for (r in cands$receivers) {
        if (check_pair(s, r)) return(TRUE)
      }
    }
    FALSE
  }

  extra_cols <- setdiff(names(event_log), required_cols)

  events_df <- event_log
  events_df$stratum <- seq_len(n_events)
  events_df$event <- 1L
  events_df <- events_df[, c("stratum", "event", required_cols, extra_cols), drop = FALSE]

  total_controls <- n_events * n_controls
  control_df <- data.frame(
    stratum = integer(total_controls),
    event = integer(total_controls),
    sender = character(total_controls),
    receiver = character(total_controls),
    time = numeric(total_controls),
    stringsAsFactors = FALSE
  )
  if (length(extra_cols)) {
    for (col in extra_cols) {
      control_df[[col]] <- NA
    }
  }

  ctrl_index <- 0L

  for (i in seq_len(n_events)) {
    cand_sets <- get_candidates(scope, events_df$time[i], events_df$sender[i], events_df$receiver[i])
    viable <- has_viable_pair(cand_sets, mode, events_df$sender[i], events_df$receiver[i])

    if (viable) {
      for (j in seq_len(n_controls)) {
        attempts <- 0L
        repeat {
          attempts <- attempts + 1L
          if (attempts > max_attempts) {
            stop("Unable to sample a valid non-event after ", max_attempts, " attempts.")
          }

          sampled <- choose_pair(cand_sets, mode)
          if (!allow_loops && sampled$sender == sampled$receiver) {
            next
          }
          if (sampled$sender == events_df$sender[i] && sampled$receiver == events_df$receiver[i]) {
            next
          }
          if (risk == "remove") {
            key <- dyad_key(sampled$sender, sampled$receiver)
            if (!is.null(removed_dyads[[key]])) {
              next
            }
          }
          break
        }

        ctrl_index <- ctrl_index + 1L
        control_df$stratum[ctrl_index] <- events_df$stratum[i]
        control_df$event[ctrl_index] <- 0L
        control_df$sender[ctrl_index] <- sampled$sender
        control_df$receiver[ctrl_index] <- sampled$receiver
        control_df$time[ctrl_index] <- events_df$time[i]
      }
    }

    appearance_senders <- union(appearance_senders, events_df$sender[i])
    appearance_receivers <- union(appearance_receivers, events_df$receiver[i])
    appearance_combined <- union(appearance_combined, c(events_df$sender[i], events_df$receiver[i]))

    if (risk == "remove") {
      removed_dyads[[dyad_key(events_df$sender[i], events_df$receiver[i])]] <- TRUE
    }
  }

  if (ctrl_index < total_controls) {
    control_df <- control_df[seq_len(ctrl_index), , drop = FALSE]
  }

  out <- rbind(events_df, control_df)
  out <- out[order(out$stratum, -out$event), , drop = FALSE]
  rownames(out) <- NULL
  out
}
