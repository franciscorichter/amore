#' Fit a relational (hyper)event model on preprocessed case-control data
#'
#' `rem()` is the unified front-end for fitting relational event models from
#' **already preprocessed** case-control data (e.g. produced by `eventnet`),
#' where the endogenous/exogenous covariates have already been computed. It is
#' intended to supersede [compare_models()], [compare_models_smooth()] and
#' [compare_models_global()], which couple feature computation and fitting.
#'
#' Two estimation backends are provided:
#' \describe{
#'   \item{`"gam"`}{Degenerate logistic regression on a case-1-control
#'     design (Boschi, Lerner & Wit 2025): the response is a constant 1 and the
#'     linear predictor is built from event-minus-control differences. Supports
#'     smooth time-varying (`tv`), non-linear (`nl`) and time-varying
#'     non-linear (`tvnl`) effects via [mgcv::gam()].}
#'   \item{`"clogit"`}{Conditional logistic regression on a case-k-control
#'     design via [survival::clogit()] (linear terms only). The case/control
#'     strata are taken from `stratum`, or derived as `cumsum(case == 1)` when
#'     `stratum` is `NULL` (assuming each case is immediately followed by its
#'     controls, the eventnet blocked layout).}
#'   \item{`"nn"`}{Flexible conditional-logistic models on the same
#'     case-k-control design as `clogit`, trained by (mini-batch) gradient
#'     descent on the exact risk-set softmax partial likelihood. Two
#'     architectures via [nn_control()]: a multilayer perceptron scoring the
#'     full covariate vector jointly (interaction-capable), or an
#'     `additive_spline` predictor — per-covariate B-spline expansions fitted
#'     by stochastic gradient, the STREAM construction of Filippi-Mazzola &
#'     Wit (2024, JRSS-C). No coefficient table; `summary()` reports
#'     in-sample (and, with a validation split, held-out) concordance and
#'     `plot(type = "pdp")` shows per-feature curves. Pure-R implementation,
#'     no extra dependencies.}
#' }
#'
#' @section Formula syntax:
#' The right-hand side lists covariates. A bare name is a **linear** effect; wrap
#' a name to request a smooth effect (`gam` method only):
#' \itemize{
#'   \item `tv(x)`   — time-varying linear effect: `s(time, by = d_x)`.
#'   \item `nl(x)`   — non-linear effect: `s(cbind(x_ev, x_nv), by = c(1, -1))`.
#'   \item `tvnl(x)` — time-varying non-linear effect (tensor product).
#'   \item `re(x)`    — random effect of a grouping factor `x` (e.g. the
#'     sender), built from the matched `x_ev` / `x_nv` levels as
#'     `s(cbind(x_ev, x_nv), by = cbind(1, -1), bs = "re")`, contributing
#'     `f(event_level) - f(control_level)` (following the REM tutorial's
#'     species-invasiveness term). Falls back to a single column `x` when
#'     `x_ev` / `x_nv` are absent. Identified only when the event and control
#'     differ on `x`.
#' }
#' For the `gam` method the left-hand side is ignored (the response is the
#' constant case indicator); for `clogit` the left-hand side names the 0/1 event
#' indicator column (e.g. `event ~ x`), unless `case` is given explicitly.
#'
#' @section Column resolution:
#' For a covariate `x`, the event/control difference is taken from column `x`,
#' else `d_x`, else `x_ev - x_nv`. Non-linear terms use `transform_x_ev` /
#' `transform_x_nv` when present (the eventnet spline-transformed covariate),
#' otherwise `x_ev` / `x_nv`. `tvnl` uses `transformed_time` when present.
#' Undirected logs (senders only, no receiver/`TARGET` column) are supported.
#'
#' @section Name clashes with other relational-event packages:
#' The name `rem()` is used by three CRAN packages: this one,
#' \pkg{relevent} (on CRAN since 2010) and \pkg{redeem} (2026). Attaching
#' more than one of them masks the others, and R will say so at load time.
#'
#' The deeper issue is dispatch rather than masking. `amorem`'s fitted object
#' has class `"rem"`, and \pkg{relevent} and \pkg{redeem} register S3 methods
#' on that same class name, so merely *loading* one of them --- attaching is
#' not required --- can re-point `print()`, `summary()`, `plot()`, `predict()`
#' and `logLik()` for objects fitted here. If you work with more than one of
#' these packages in a session, call `amorem::rem()` explicitly and be
#' deliberate about which package is loaded when you print or plot a fit.
#'
#' @param formula A formula; see *Formula syntax*.
#' @param data A data.frame of preprocessed case-control data (wide for the
#'   `gam` method; long with a case indicator and stratum for `clogit`). For
#'   `method = "gam"`, long case-control input (a `event`/`IS_OBSERVED`
#'   indicator with control rows) is detected and widened automatically via
#'   [widen_case_control()], with a message.
#' @param method Estimation backend; see *Description*.
#' @param case Optional name of the 0/1 event-indicator column for the `clogit`
#'   and `nn` backends. If `NULL` (default), the indicator is taken from the
#'   formula's left-hand side (e.g. `event ~ x`). Ignored by the `gam` method.
#' @param stratum Name of the column grouping each case with its controls
#'   (required by `clogit`).
#' @param time Name of the time column, required for `tv` / `tvnl` terms.
#' @param k Optional integer basis dimension passed to `s()` / `te()`.
#' @param gam_method Smoothness-selection method for the `gam` backend,
#'   passed to [mgcv::gam()]. Defaults to `NULL`, which uses mgcv's own default
#'   (`"GCV.Cp"`) and reproduces the Intro-to-REM tutorial parameterization.
#'   Set to `"REML"` for the REML fit used in some papers.
#' @param nn An [nn_control()] object with the architecture and training
#'   hyper-parameters for `method = "nn"`. Ignored by the other backends.
#' @param ... Reserved for future use.
#'
#' @return An object of class `"rem"`: a list with the fitted model (`$fit`),
#'   the `method`, the original `formula`, the parsed `terms`, and the number of
#'   observations `n`. Has [summary()], [coef()], [plot()] and [logLik()]
#'   methods.
#'
#' @seealso [compare_models_smooth()] (superseded), [simulate_relational_events()]
#'   (whose `wide = TRUE` output is a valid input here),
#'   [simulate_directed_hyperevents_tvnl()].
#'
#' @importFrom survival clogit strata coxph Surv frailty
#'
#' @examples
#' set.seed(1)
#' w <- simulate_relational_events(
#'   n_events = 300, senders = paste0("a", 1:12), receivers = paste0("a", 1:12),
#'   n_controls = 1, endogenous_stats = "reciprocity_count",
#'   endogenous_effects = c(reciprocity_count = 0.6), wide = TRUE)
#' fit <- rem(~ reciprocity_count, data = w, method = "gam")
#' coef(fit)
#'
#' @export
rem <- function(formula, data,
                method = c("gam", "clogit", "nn"),
                case = NULL, stratum = NULL, time = NULL,
                k = NULL, gam_method = NULL, nn = nn_control(), ...) {
  method <- match.arg(method)
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.")
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")

  term_labels <- attr(stats::terms(formula), "term.labels")
  if (!length(term_labels)) {
    stop("`formula` must have at least one term on the right-hand side.")
  }
  parse_term <- function(lbl) {
    m <- regmatches(lbl, regexec("^(tvnl|tv|nl|re)\\((.+)\\)$", lbl))[[1]]
    if (length(m) == 3L) list(type = m[2], var = trimws(m[3]))
    else list(type = "linear", var = lbl)
  }
  terms_info <- lapply(term_labels, parse_term)

  if (method == "clogit") {
    if (!requireNamespace("survival", quietly = TRUE)) {
      stop("The `survival` package is required by rem(method = \"clogit\"). ",
           "Install it with install.packages(\"survival\").")
    }
    if (any(vapply(terms_info, function(t) t$type != "linear", logical(1)))) {
      stop("method = \"clogit\" supports linear terms only; smooth terms ",
           "(tv/nl/tvnl) require method = \"gam\".")
    }
    # Resolve the event-indicator column: explicit `case`, else the formula LHS.
    if (is.null(case)) {
      if (attr(stats::terms(formula), "response") == 0L) {
        stop("method = \"clogit\" needs a 0/1 event indicator: put it on the ",
             "formula's left-hand side (e.g. `event ~ x`), or pass `case`.")
      }
      case <- all.vars(formula)[1L]
    }
    ci <- data[[case]]
    if (is.null(ci)) stop("case column '", case, "' not found in `data`.")
    strat <- .derive_stratum(data, case, stratum)
    vars <- vapply(terms_info, function(t) t$var, character(1))
    miss <- setdiff(vars, names(data))
    if (length(miss)) {
      stop("Covariate column(s) not found in `data`: ", paste(miss, collapse = ", "))
    }
    cl <- data.frame(.case = as.integer(ci), .strat = strat,
                     stringsAsFactors = FALSE)
    for (v in vars) cl[[v]] <- data[[v]]
    fm <- stats::as.formula(paste0(
      ".case ~ ", paste(sprintf("`%s`", vars), collapse = " + "),
      " + strata(.strat)"))
    fit <- survival::clogit(fm, data = cl)
    return(structure(
      list(fit = fit, method = method, formula = formula,
           terms = terms_info, n = nrow(data), gam_formula = fm),
      class = "rem"))
  }

  if (method == "nn") {
    if (any(vapply(terms_info, function(t) t$type != "linear", logical(1)))) {
      stop("method = \"nn\" takes bare covariate names only; the network ",
           "learns nonlinearities itself (tv/nl/tvnl/re are for method = \"gam\").")
    }
    if (!inherits(nn, "nn_control")) {
      stop("`nn` must be an nn_control() object.")
    }
    # Same indicator resolution as clogit: explicit `case`, else formula LHS.
    if (is.null(case)) {
      if (attr(stats::terms(formula), "response") == 0L) {
        stop("method = \"nn\" needs a 0/1 event indicator: put it on the ",
             "formula's left-hand side (e.g. `event ~ x`), or pass `case`.")
      }
      case <- all.vars(formula)[1L]
    }
    ci <- data[[case]]
    if (is.null(ci)) stop("case column '", case, "' not found in `data`.")
    strat <- .derive_stratum(data, case, stratum)
    vars <- vapply(terms_info, function(t) t$var, character(1))
    miss <- setdiff(vars, names(data))
    if (length(miss)) {
      stop("Covariate column(s) not found in `data`: ", paste(miss, collapse = ", "))
    }
    non_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
    if (length(non_num)) {
      stop("method = \"nn\" needs numeric covariates; non-numeric: ",
           paste(non_num, collapse = ", "),
           ". Encode factors numerically (e.g. model.matrix) first.")
    }
    X <- as.matrix(data[, vars, drop = FALSE]); storage.mode(X) <- "double"
    fit <- .rem_nn_fit(X, strat, as.integer(ci) == 1L, nn, vars)
    return(structure(
      list(fit = fit, method = method, formula = formula,
           terms = terms_info, n = nrow(data), gam_formula = NULL),
      class = "rem"))
  }

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("The `mgcv` package is required by rem(method = \"gam\"). ",
         "Install it with install.packages(\"mgcv\").")
  }

  # Guard against long-format data reaching the wide `gam` backend (issue #93).
  # The gam design is case-1-control: one row per case, each covariate supplied
  # as an event-minus-control difference. If a 0/1 indicator column
  # (`event`/`IS_OBSERVED`) with control rows is present, `data` is a long
  # case-control log; fitting it directly would silently misread the raw
  # per-row covariate values as differences. Auto-widen (loudly) instead.
  ind_cand <- intersect(c("event", "IS_OBSERVED"), names(data))
  if (length(ind_cand)) {
    ind <- ind_cand[1L]
    iv <- suppressWarnings(as.integer(data[[ind]]))
    if (any(!is.na(iv) & iv == 0L)) {
      message("rem(method = \"gam\"): `data` looks long-format (column '", ind,
              "' contains control rows); widening it with ",
              "widen_case_control() before fitting. Pass already-wide data ",
              "(one row per case) to silence this.")
      data <- widen_case_control(data, case = ind, stratum = stratum)
      stratum <- NULL
    }
  }

  n <- nrow(data)
  if (!n) stop("`data` has no rows.")

  # event-minus-control difference for covariate v
  get_diff <- function(v) {
    if (!is.null(data[[v]])) return(as.numeric(data[[v]]))
    if (!is.null(data[[paste0("d_", v)]])) return(as.numeric(data[[paste0("d_", v)]]))
    ev <- data[[paste0(v, "_ev")]]; nv <- data[[paste0(v, "_nv")]]
    if (is.null(ev) || is.null(nv)) {
      stop("Cannot find a column for linear term '", v, "' ",
           "(looked for '", v, "', 'd_", v, "', '", v, "_ev'/'", v, "_nv').")
    }
    as.numeric(ev) - as.numeric(nv)
  }
  # case/control matrix for a non-linear term (prefers transform_ columns)
  get_evnv <- function(v) {
    for (pre in c("transform_", "")) {
      ev <- data[[paste0(pre, v, "_ev")]]; nv <- data[[paste0(pre, v, "_nv")]]
      if (!is.null(ev) && !is.null(nv)) {
        return(cbind(case = as.numeric(ev), ctrl = as.numeric(nv)))
      }
    }
    stop("Cannot find ev/nv columns for non-linear term '", v, "' ",
         "(looked for '[transform_]", v, "_ev'/'_nv').")
  }
  get_time <- function() {
    if (is.null(time)) stop("`time` (a column name) is required for tv/tvnl terms.")
    if (is.null(data[[time]])) stop("time column '", time, "' not found in `data`.")
    as.numeric(data[[time]])
  }
  get_time_trans <- function() {
    if (!is.null(data[["transformed_time"]])) as.numeric(data[["transformed_time"]])
    else get_time()
  }

  reserved <- c("one", ".I", ".time", ".T")
  df <- list(one = rep(1, n), .I = cbind(case = rep(1, n), ctrl = rep(-1, n)))
  rhs <- character(0)
  k_arg <- if (is.null(k)) "" else sprintf(", k = %d", k)
  need_time <- FALSE; need_ttrans <- FALSE
  bt <- function(x) paste0("`", x, "`")            # backtick for formula safety
  for (ti in terms_info) {
    v <- ti$var
    if (v %in% reserved) {
      stop("Covariate name '", v, "' clashes with an internal column; ",
           "please rename it in `data`.")
    }
    # Difference / matrix columns are named by the covariate itself so that
    # coef()/summary() report meaningful term names.
    if (ti$type == "linear") {
      df[[v]] <- get_diff(v)
      rhs <- c(rhs, bt(v))
    } else if (ti$type == "tv") {
      df[[v]] <- get_diff(v); need_time <- TRUE
      rhs <- c(rhs, sprintf("s(%s, by = %s%s)", bt(".time"), bt(v), k_arg))
    } else if (ti$type == "nl") {
      xc <- paste0(".X_", v); df[[xc]] <- get_evnv(v)
      rhs <- c(rhs, sprintf("s(%s, by = %s%s)", bt(xc), bt(".I"), k_arg))
    } else if (ti$type == "tvnl") {
      xc <- paste0(".X_", v); df[[xc]] <- get_evnv(v); need_ttrans <- TRUE
      rhs <- c(rhs, sprintf("te(%s, %s, by = %s%s)",
                            bt(".T"), bt(xc), bt(".I"), k_arg))
    } else if (ti$type == "re") {
      # Actor (or other grouping) random effect, matching the matched
      # case-control construction of the REM tutorial (Intro-to-REM, 3.4): an
      # n x 2 factor matrix holding the grouping level of the event in column 1
      # and of the matched control in column 2, with by = cbind(1, -1), so the
      # term contributes f(event_level) - f(control_level) -- identifiable
      # whenever the event and control differ on this grouping.
      ev <- data[[paste0(v, "_ev")]]; nv <- data[[paste0(v, "_nv")]]
      if (is.null(ev) || is.null(nv)) {
        single <- data[[v]]
        if (is.null(single)) {
          stop("Cannot find columns for the re() term '", v, "' (looked for '",
               v, "_ev'/'", v, "_nv' or '", v, "').")
        }
        ev <- single; nv <- single
      }
      fmat <- factor(c(as.character(ev), as.character(nv)))
      dim(fmat) <- c(n, 2L)
      rc <- paste0(".RE_", v); df[[rc]] <- fmat
      rhs <- c(rhs, sprintf("s(%s, by = %s, bs = \"re\")", bt(rc), bt(".I")))
    }
  }
  if (need_time)   df[[".time"]] <- get_time()
  if (need_ttrans) { tt <- get_time_trans(); df[[".T"]] <- cbind(tt, tt) }

  fm <- stats::as.formula(paste("one ~ -1 +", paste(rhs, collapse = " + ")))
  # Smoothness-selection method passed to mgcv::gam(). Default NULL lets mgcv
  # use its own default ("GCV.Cp"), which reproduces the Intro-to-REM tutorial
  # parameterization (see issue #88). Set gam_method = "REML" to recover the
  # REML fit used in some papers.
  gam_args <- list(formula = fm, family = stats::binomial(), data = df)
  if (!is.null(gam_method)) gam_args$method <- gam_method
  fit <- do.call(mgcv::gam, gam_args)

  structure(
    list(fit = fit, method = method, formula = formula,
         terms = terms_info, n = n, gam_formula = fm),
    class = "rem")
}

#' @export
print.rem <- function(x, ...) {
  cat("Relational event model (rem)\n")
  cat("  method : ", x$method, "\n", sep = "")
  cat("  formula: ", deparse(x$formula), "\n", sep = "")
  cat("  n      : ", x$n, "\n", sep = "")
  invisible(x)
}

#' @export
summary.rem <- function(object, ...) summary(object$fit, ...)

#' @export
coef.rem <- function(object, ...) stats::coef(object$fit, ...)

#' @export
plot.rem <- function(x, ...) plot(x$fit, ...)

#' @export
logLik.rem <- function(object, ...) stats::logLik(object$fit)

#' @export
predict.rem <- function(object, ...) stats::predict(object$fit, ...)

# Derive the case-control stratum vector aligned to data's rows.
# If `stratum` names a column, use it (forward-filling blanks, since eventnet
# leaves the stratum id empty on control rows); otherwise derive it as
# cumsum(case == 1), which labels each case-then-controls block.
.derive_stratum <- function(data, case, stratum) {
  if (!is.null(stratum)) {
    if (is.null(data[[stratum]])) {
      stop("stratum column '", stratum, "' not found in `data`.")
    }
    s <- as.character(data[[stratum]])
    blank <- is.na(s) | !nzchar(s)
    if (any(blank)) {                       # forward-fill from the case rows
      last <- NA_character_
      for (i in seq_along(s)) {
        if (!blank[i]) last <- s[i]
        s[i] <- last
      }
      if (anyNA(s)) {
        stop("Could not fill stratum for some rows; the first row must be a ",
             "case, or pass a fully populated `stratum` column.")
      }
    }
    return(s)
  }
  ci <- data[[case]]
  if (is.null(ci)) stop("case column '", case, "' not found in `data`.")
  cumsum(as.integer(ci) == 1L)
}
