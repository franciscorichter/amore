#' Simulate exogenous actor covariates
#'
#' Create simple exogenous covariate structures for senders and receivers. The
#' function can return static values (one row per actor) or time-stamped
#' processes (one row per actor and time point) that follow independent AR(1)
#' dynamics.
#'
#' @param senders Character vector of sender actors.
#' @param receivers Character vector of receiver actors.
#' @param covariate_names Character vector naming the covariates to simulate.
#' @param time_points Optional numeric vector of strictly increasing time stamps
#'   for time-varying covariates. When omitted, static covariates are returned.
#' @param sd Standard deviation of the innovation noise.
#' @param rho AR(1) coefficient used when \code{time_points} is supplied. Must be
#'   in (-1, 1).
#' @param seed Optional integer to make the simulation reproducible.
#'
#' @return A list with two elements: \code{sender_covariates} and
#'   \code{receiver_covariates}. Each element is either a wide data.frame (static
#'   case) or a tidy data.frame with columns \code{actor}, \code{time},
#'   \code{covariate}, and \code{value} (dynamic case).
#' @export
#'
#' @examples
#' sender_cov <- simulate_actor_covariates(
#'   senders = letters[1:3],
#'   receivers = LETTERS[1:2],
#'   covariate_names = c("activity", "recency"),
#'   time_points = seq(0, 4),
#'   rho = 0.6,
#'   sd = 0.2,
#'   seed = 123
#' )
#' str(sender_cov)
simulate_actor_covariates <- function(
    senders,
    receivers,
    covariate_names,
    time_points = NULL,
    sd = 1,
    rho = 0,
    seed = NULL) {
  stopifnot(sd >= 0)
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

  covariate_names <- as.character(covariate_names)
  if (!length(covariate_names)) {
    stop("At least one covariate name must be provided.")
  }

  if (!is.null(time_points)) {
    time_points <- sort(unique(time_points))
    if (length(time_points) < 2) {
      stop("Provide at least two time points for dynamic covariates.")
    }
    if (any(diff(time_points) <= 0)) {
      stop("time_points must be strictly increasing.")
    }
    if (abs(rho) >= 1) {
      stop("rho must lie in (-1, 1) for stability.")
    }
  }

  build_static <- function(actors) {
    if (!length(actors)) {
      return(data.frame(actor = character(0)))
    }
    values <- matrix(stats::rnorm(length(actors) * length(covariate_names), sd = sd),
      nrow = length(actors), byrow = TRUE
    )
    df <- data.frame(actor = as.character(actors), values, stringsAsFactors = FALSE)
    colnames(df)[-1] <- covariate_names
    df
  }

  build_dynamic <- function(actors) {
    if (!length(actors)) {
      return(data.frame(
        actor = character(0), time = numeric(0),
        covariate = character(0), value = numeric(0)
      ))
    }
    n_time <- length(time_points)
    output <- vector("list", length(actors))
    for (i in seq_along(actors)) {
      actor_vals <- matrix(0, nrow = n_time, ncol = length(covariate_names))
      actor_vals[1, ] <- stats::rnorm(length(covariate_names), sd = sd / sqrt(1 - rho^2))
      if (n_time > 1) {
        for (t in 2:n_time) {
          actor_vals[t, ] <- rho * actor_vals[t - 1, ] + stats::rnorm(length(covariate_names), sd = sd)
        }
      }
      df <- expand.grid(
        time = time_points, covariate = covariate_names,
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
      )
      df$actor <- actors[i]
      df$value <- as.vector(actor_vals)
      output[[i]] <- df[, c("actor", "time", "covariate", "value")]
    }
    do.call(rbind, output)
  }

  if (is.null(time_points)) {
    list(
      sender_covariates = build_static(senders),
      receiver_covariates = build_static(receivers)
    )
  } else {
    list(
      sender_covariates = build_dynamic(senders),
      receiver_covariates = build_dynamic(receivers)
    )
  }
}
