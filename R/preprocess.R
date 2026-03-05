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
#' Given a standardized relational event log, this helper derives historical
#' statistics for each event based on the evolving network.  The statistics
#' follow the taxonomy of Juozaitienė and Wit (2025, JRSS-A) and cover
#' reciprocity, transitivity, cyclic closure, sending balance and receiving
#' balance.  All definitions use the *continuous* convention (effects persist
#' even after a closure event).
#'
#' @param event_log A data.frame containing at least `sender`, `receiver`, and
#'   `time` columns.
#' @param stats Character vector of statistics to compute.  See **Details** for
#'   the full list of allowed values.
#' @param half_life Positive numeric; the half-life parameter \eqn{T} for
#'   exponential-decay statistics (`*_exp_decay*`).
#' @param sort Logical; when `TRUE`, events are ordered by time prior to
#'   computing summaries (ties preserve input order).
#'
#' @details All statistics are evaluated immediately **before** the event is
#'   logged.  They are grouped into five families.
#'
#'   **Degree / baseline:**
#'   \describe{
#'     \item{`sender_outdegree`}{Number of events previously sent by the
#'       sender.}
#'     \item{`receiver_indegree`}{Number of events previously received by the
#'       receiver.}
#'     \item{`recency`}{Elapsed time since the last event on the same ordered
#'       pair; `NA` when the dyad is brand new.}
#'   }
#'
#'   **Reciprocity** — reverse-dyad (receiver \eqn{\to} sender) history:
#'   \describe{
#'     \item{`reciprocity` / `reciprocity_binary`}{1 if the reverse dyad has
#'       ever been observed, 0 otherwise.}
#'     \item{`reciprocity_count`}{Total count of past reverse-dyad events.}
#'     \item{`reciprocity_exp_decay`}{Exponentially weighted sum of past
#'       reverse-dyad events (requires `half_life`).}
#'     \item{`reciprocity_time_recent`}{Elapsed time since the most recent
#'       reverse-dyad event; `NA` if none.}
#'     \item{`reciprocity_time_first`}{Elapsed time since the first
#'       reverse-dyad event; `NA` if none.}
#'   }
#'
#'   **Transitivity** — two-path \eqn{s \to k \to r}:
#'   \describe{
#'     \item{`transitivity_binary`}{1 if any intermediary \eqn{k} exists with
#'       both \eqn{(s,k)} and \eqn{(k,r)} before \eqn{t}.}
#'     \item{`transitivity_count`}{Number of such intermediaries.}
#'     \item{`transitivity_binary_ordered`}{Like binary but requiring
#'       \eqn{(s,k)} to precede \eqn{(k,r)}.}
#'     \item{`transitivity_count_ordered`}{Count with order restriction.}
#'     \item{`transitivity_exp_decay`}{Exp-decay weighted sum over two-paths
#'       (requires `half_life`).}
#'     \item{`transitivity_exp_decay_ordered`}{Exp-decay with order
#'       restriction.}
#'     \item{`transitivity_time_recent`}{Time since the most recently completed
#'       two-path; `NA` if none.}
#'     \item{`transitivity_time_first`}{Time since the earliest two-path; `NA`
#'       if none.}
#'     \item{`transitivity_time_recent_ordered`}{Time since the most recent
#'       ordered two-path; `NA` if none.}
#'     \item{`transitivity_time_first_ordered`}{Time since the earliest ordered
#'       two-path; `NA` if none.}
#'   }
#'
#'   **Cyclic closure** — two-path \eqn{r \to k \to s}, closed by
#'   \eqn{s \to r}:
#'   \describe{
#'     \item{`cyclic_binary`}{1 if any cyclic two-path exists.}
#'     \item{`cyclic_count`}{Number of cyclic intermediaries.}
#'     \item{`cyclic_time_recent`}{Time since the most recent cyclic two-path;
#'       `NA` if none.}
#'   }
#'
#'   **Sending balance** — shared target: both \eqn{s \to k} and \eqn{r \to k}
#'   exist:
#'   \describe{
#'     \item{`sending_balance_binary`}{1 if any shared target exists.}
#'     \item{`sending_balance_count`}{Number of shared targets.}
#'     \item{`sending_balance_time_recent`}{Time since the most recent
#'       shared-target two-path; `NA` if none.}
#'   }
#'
#'   **Receiving balance** — shared source: both \eqn{k \to s} and
#'   \eqn{k \to r} exist:
#'   \describe{
#'     \item{`receiving_balance_binary`}{1 if any shared source exists.}
#'     \item{`receiving_balance_count`}{Number of shared sources.}
#'     \item{`receiving_balance_time_recent`}{Time since the most recent
#'       shared-source two-path; `NA` if none.}
#'   }
#'
#' @return The event log with added columns, one per requested statistic.
#' @export
compute_endogenous_features <- function(
    event_log,
    stats = c("sender_outdegree", "receiver_indegree", "reciprocity", "recency"),
    half_life = NULL,
    sort = TRUE) {

  if (!is.data.frame(event_log)) {
    stop("`event_log` must be a data.frame.")
  }
  required_cols <- c("sender", "receiver", "time")
  missing_cols <- setdiff(required_cols, names(event_log))
  if (length(missing_cols)) {
    stop("Event log is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  allowed <- c(
    "sender_outdegree", "receiver_indegree", "recency",
    "reciprocity", "reciprocity_binary", "reciprocity_count",
    "reciprocity_exp_decay", "reciprocity_time_recent", "reciprocity_time_first",
    "transitivity_binary", "transitivity_count",
    "transitivity_binary_ordered", "transitivity_count_ordered",
    "transitivity_exp_decay", "transitivity_exp_decay_ordered",
    "transitivity_time_recent", "transitivity_time_first",
    "transitivity_time_recent_ordered", "transitivity_time_first_ordered",
    "cyclic_binary", "cyclic_count", "cyclic_time_recent",
    "sending_balance_binary", "sending_balance_count",
    "sending_balance_time_recent",
    "receiving_balance_binary", "receiving_balance_count",
    "receiving_balance_time_recent"
  )
  bad <- setdiff(stats, allowed)
  if (length(bad)) {
    stop("Unsupported statistics requested: ", paste(bad, collapse = ", "))
  }
  if (!length(stats)) {
    stop("At least one statistic must be requested.")
  }

  exp_decay_stats <- c("reciprocity_exp_decay", "transitivity_exp_decay",
                       "transitivity_exp_decay_ordered")
  if (any(exp_decay_stats %in% stats) &&
      (is.null(half_life) || !is.numeric(half_life) || half_life <= 0)) {
    stop("`half_life` must be a positive number when ",
         "exponential-decay statistics are requested.")
  }

  log_df <- event_log
  if (sort && nrow(log_df)) {
    ord <- order(log_df$time, seq_len(nrow(log_df)))
    log_df <- log_df[ord, , drop = FALSE]
  }

  n <- nrow(log_df)
  if (!n) {
    for (stat in stats) log_df[[stat]] <- numeric(0)
    return(log_df)
  }

  # --- Determine which families are needed ---
  trans_names <- c("transitivity_binary", "transitivity_count",
                   "transitivity_binary_ordered", "transitivity_count_ordered",
                   "transitivity_exp_decay", "transitivity_exp_decay_ordered",
                   "transitivity_time_recent", "transitivity_time_first",
                   "transitivity_time_recent_ordered",
                   "transitivity_time_first_ordered")
  cyc_names   <- c("cyclic_binary", "cyclic_count", "cyclic_time_recent")
  sb_names    <- c("sending_balance_binary", "sending_balance_count",
                   "sending_balance_time_recent")
  rb_names    <- c("receiving_balance_binary", "receiving_balance_count",
                   "receiving_balance_time_recent")
  need_triadic <- any(c(trans_names, cyc_names, sb_names, rb_names) %in% stats)

  # --- Tracking data structures ---
  dyad_key <- function(s, r) paste0(s, "->", r)

  sender_counts  <- numeric(0)
  receiver_counts <- numeric(0)
  dyad_last_time  <- new.env(parent = emptyenv())
  dyad_first_time <- new.env(parent = emptyenv())
  dyad_event_count <- new.env(parent = emptyenv())
  dyad_times      <- new.env(parent = emptyenv())

  if (need_triadic) {
    out_targets <- new.env(parent = emptyenv())
    in_sources  <- new.env(parent = emptyenv())
  }

  get_count <- function(x, key) {
    if (!length(x)) return(0)
    val <- x[key]
    if (!length(val) || is.na(val)) return(0)
    val
  }

  # --- Initialize output columns ---
  binary_set <- c("reciprocity", "reciprocity_binary",
                  "transitivity_binary", "transitivity_binary_ordered",
                  "cyclic_binary", "sending_balance_binary",
                  "receiving_balance_binary")
  count_set <- c("sender_outdegree", "receiver_indegree",
                 "reciprocity_count", "reciprocity_exp_decay",
                 "transitivity_count", "transitivity_count_ordered",
                 "transitivity_exp_decay", "transitivity_exp_decay_ordered",
                 "cyclic_count", "sending_balance_count",
                 "receiving_balance_count")
  for (stat in stats) {
    if (stat %in% binary_set) {
      log_df[[stat]] <- integer(n)
    } else if (stat %in% count_set) {
      log_df[[stat]] <- numeric(n)
    } else {
      log_df[[stat]] <- rep(NA_real_, n)
    }
  }

  # --- Triadic helper --------------------------------------------------
  # Computes binary / count / time / exp-decay stats for a given set of
  # intermediaries whose two edges are retrieved via get_e1_times / get_e2_times.
  compute_triadic <- function(s, r, t_now, prefix, intermediaries,
                              get_e1_times, get_e2_times) {
    res <- list()
    req <- stats[startsWith(stats, paste0(prefix, "_"))]
    if (!length(req)) return(res)

    n_k <- length(intermediaries)
    b_nm <- paste0(prefix, "_binary")
    c_nm <- paste0(prefix, "_count")
    if (b_nm %in% req) res[[b_nm]] <- as.integer(n_k > 0L)
    if (c_nm %in% req) res[[c_nm]] <- n_k

    if (n_k == 0L) {
      for (nm in req) {
        if (is.null(res[[nm]])) {
          res[[nm]] <- if (grepl("binary|count|exp", nm)) 0 else NA_real_
        }
      }
      return(res)
    }

    need_ord  <- any(grepl("ordered", req))
    need_exp  <- any(grepl("exp_decay", req))
    need_time <- any(grepl("time_", req))

    form_recent     <- -Inf
    form_first      <- Inf
    n_ordered       <- 0L
    form_ord_recent <- -Inf
    form_ord_first  <- Inf
    exp_sum         <- 0
    exp_ord_sum     <- 0

    for (ki in seq_along(intermediaries)) {
      k  <- intermediaries[ki]
      e1 <- get_e1_times(k)
      e2 <- get_e2_times(k)

      last_e1  <- max(e1)
      last_e2  <- max(e2)
      completion <- max(last_e1, last_e2)

      if (need_time) {
        if (completion > form_recent) form_recent <- completion
        first_completion <- max(min(e1), min(e2))
        if (first_completion < form_first) form_first <- first_completion
      }
      if (need_exp && !is.null(half_life)) {
        exp_sum <- exp_sum +
          exp(-(t_now - completion) * log(2) / half_life)
      }
      if (need_ord) {
        valid_e2 <- e2[e2 > min(e1)]
        if (length(valid_e2)) {
          n_ordered <- n_ordered + 1L
          latest_v   <- max(valid_e2)
          earliest_v <- min(valid_e2)
          if (latest_v > form_ord_recent) form_ord_recent <- latest_v
          if (earliest_v < form_ord_first) form_ord_first <- earliest_v
          if (need_exp && !is.null(half_life)) {
            exp_ord_sum <- exp_ord_sum +
              exp(-(t_now - latest_v) * log(2) / half_life)
          }
        }
      }
    }

    tr_nm <- paste0(prefix, "_time_recent")
    tf_nm <- paste0(prefix, "_time_first")
    if (tr_nm %in% req) res[[tr_nm]] <- t_now - form_recent
    if (tf_nm %in% req) res[[tf_nm]] <- t_now - form_first

    e_nm <- paste0(prefix, "_exp_decay")
    if (e_nm %in% req) res[[e_nm]] <- exp_sum

    bo_nm  <- paste0(prefix, "_binary_ordered")
    co_nm  <- paste0(prefix, "_count_ordered")
    tro_nm <- paste0(prefix, "_time_recent_ordered")
    tfo_nm <- paste0(prefix, "_time_first_ordered")
    eo_nm  <- paste0(prefix, "_exp_decay_ordered")
    if (bo_nm %in% req) res[[bo_nm]] <- as.integer(n_ordered > 0L)
    if (co_nm %in% req) res[[co_nm]] <- n_ordered
    if (n_ordered > 0L) {
      if (tro_nm %in% req) res[[tro_nm]] <- t_now - form_ord_recent
      if (tfo_nm %in% req) res[[tfo_nm]] <- t_now - form_ord_first
      if (eo_nm  %in% req) res[[eo_nm]]  <- exp_ord_sum
    } else {
      if (tro_nm %in% req) res[[tro_nm]] <- NA_real_
      if (tfo_nm %in% req) res[[tfo_nm]] <- NA_real_
      if (eo_nm  %in% req) res[[eo_nm]]  <- 0
    }

    res
  }

  # --- Main loop -------------------------------------------------------
  for (i in seq_len(n)) {
    s  <- as.character(log_df$sender[i])
    r  <- as.character(log_df$receiver[i])
    ti <- log_df$time[i]
    key_sr <- dyad_key(s, r)
    key_rs <- dyad_key(r, s)

    # Degree
    if ("sender_outdegree" %in% stats)
      log_df$sender_outdegree[i] <- get_count(sender_counts, s)
    if ("receiver_indegree" %in% stats)
      log_df$receiver_indegree[i] <- get_count(receiver_counts, r)

    # Recency (same-direction dyad)
    if ("recency" %in% stats) {
      lt <- dyad_last_time[[key_sr]]
      if (!is.null(lt)) log_df$recency[i] <- ti - lt
    }

    # Reciprocity family (reverse-dyad)
    has_reverse <- !is.null(dyad_event_count[[key_rs]])
    if ("reciprocity" %in% stats)
      log_df$reciprocity[i] <- as.integer(has_reverse)
    if ("reciprocity_binary" %in% stats)
      log_df$reciprocity_binary[i] <- as.integer(has_reverse)
    if ("reciprocity_count" %in% stats) {
      rc <- dyad_event_count[[key_rs]]
      log_df$reciprocity_count[i] <- if (is.null(rc)) 0 else rc
    }
    if ("reciprocity_exp_decay" %in% stats) {
      rs_t <- dyad_times[[key_rs]]
      log_df$reciprocity_exp_decay[i] <-
        if (is.null(rs_t)) 0 else sum(exp(-(ti - rs_t) * log(2) / half_life))
    }
    if ("reciprocity_time_recent" %in% stats) {
      lt_rs <- dyad_last_time[[key_rs]]
      if (!is.null(lt_rs)) log_df$reciprocity_time_recent[i] <- ti - lt_rs
    }
    if ("reciprocity_time_first" %in% stats) {
      ft_rs <- dyad_first_time[[key_rs]]
      if (!is.null(ft_rs)) log_df$reciprocity_time_first[i] <- ti - ft_rs
    }

    # Triadic statistics
    if (need_triadic) {
      s_out <- out_targets[[s]]; if (is.null(s_out)) s_out <- character(0)
      r_out <- out_targets[[r]]; if (is.null(r_out)) r_out <- character(0)
      s_in  <- in_sources[[s]];  if (is.null(s_in))  s_in  <- character(0)
      r_in  <- in_sources[[r]];  if (is.null(r_in))  r_in  <- character(0)

      # Transitivity: s -> k -> r
      if (any(trans_names %in% stats)) {
        ks <- setdiff(intersect(s_out, r_in), c(s, r))
        tri <- compute_triadic(s, r, ti, "transitivity", ks,
          function(k) dyad_times[[dyad_key(s, k)]],
          function(k) dyad_times[[dyad_key(k, r)]])
        for (nm in names(tri)) log_df[[nm]][i] <- tri[[nm]]
      }

      # Cyclic closure: r -> k -> s, closed by s -> r
      if (any(cyc_names %in% stats)) {
        ks <- setdiff(intersect(r_out, s_in), c(s, r))
        cyc <- compute_triadic(s, r, ti, "cyclic", ks,
          function(k) dyad_times[[dyad_key(r, k)]],
          function(k) dyad_times[[dyad_key(k, s)]])
        for (nm in names(cyc)) log_df[[nm]][i] <- cyc[[nm]]
      }

      # Sending balance: s -> k AND r -> k
      if (any(sb_names %in% stats)) {
        ks <- setdiff(intersect(s_out, r_out), c(s, r))
        sb <- compute_triadic(s, r, ti, "sending_balance", ks,
          function(k) dyad_times[[dyad_key(s, k)]],
          function(k) dyad_times[[dyad_key(r, k)]])
        for (nm in names(sb)) log_df[[nm]][i] <- sb[[nm]]
      }

      # Receiving balance: k -> s AND k -> r
      if (any(rb_names %in% stats)) {
        ks <- setdiff(intersect(s_in, r_in), c(s, r))
        rb <- compute_triadic(s, r, ti, "receiving_balance", ks,
          function(k) dyad_times[[dyad_key(k, s)]],
          function(k) dyad_times[[dyad_key(k, r)]])
        for (nm in names(rb)) log_df[[nm]][i] <- rb[[nm]]
      }
    }

    # --- Update state ---
    sender_counts[s]  <- get_count(sender_counts, s) + 1
    receiver_counts[r] <- get_count(receiver_counts, r) + 1
    dyad_last_time[[key_sr]] <- ti
    if (is.null(dyad_first_time[[key_sr]])) dyad_first_time[[key_sr]] <- ti
    prev_c <- dyad_event_count[[key_sr]]
    dyad_event_count[[key_sr]] <- if (is.null(prev_c)) 1L else prev_c + 1L
    dyad_times[[key_sr]] <- c(dyad_times[[key_sr]], ti)

    if (need_triadic) {
      cur_out <- out_targets[[s]]
      if (is.null(cur_out) || !r %in% cur_out) out_targets[[s]] <- c(cur_out, r)
      cur_in <- in_sources[[r]]
      if (is.null(cur_in) || !s %in% cur_in) in_sources[[r]] <- c(cur_in, s)
    }
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
