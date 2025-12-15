#' Construct lagged regressors and aligned response
#'
#' @description
#' Helper for building lagged design matrices for time series or
#' longitudinal data. The function aligns a response vector `y`
#' and a single regressor `x` (possibly with several lags) and
#' returns a list with the cleaned response, the design matrix and
#' the row indices used.
#'
#' You can either:
#' * supply `y` and `x` directly, or
#' * use a formula of the form `y ~ x` together with `data`.
#'
#' @param formula An optional model formula. The left-hand side is
#'   taken as the response and the right-hand side as the regressor(s).
#'   Only one non-intercept regressor is currently allowed when
#'   lagging is used.
#' @param data Optional data frame in which to evaluate `formula`.
#' @param y Optional numeric or time series object with the response.
#'   Ignored if `formula` is supplied.
#' @param x Optional numeric vector with the regressor to be lagged.
#'   Ignored if `formula` is supplied. If `x` is not `NULL`, it must
#'   have the same length as `y` (when `y` is supplied).
#' @param lags Integer vector of lags to include. The default `lags = 0`
#'   means that only the contemporaneous regressor is used. For example,
#'   `lags = 0:2` gives columns corresponding to (x(t), x(t-1), x(t-2)).
#' @param include_intercept Logical; if `TRUE` (default) an intercept
#'   column named `"Intercept"` is prepended to the design matrix.
#' @param preserve_ts Logical; if `TRUE` and `y` is a `ts` object, the
#'   returned `y` will also be a `ts` object obtained via
#'   \code{\link[stats]{window}} starting at the first retained time
#'   point. Otherwise, `y` is returned as a numeric vector.
#'
#' @details
#' When using the formula interface, the function creates a model frame
#' internally. The first column is taken as the response. All remaining
#' columns are considered regressors. If the right-hand side contains
#' only an intercept, `x` is set to `NULL` and no lagged regressors are
#' constructed.
#'
#' If multiple regressors are present on the right-hand side, they are
#' combined into a matrix, but in the current implementation this is
#' only allowed when no lagging is requested (i.e., `lags` is `0`).
#' Attempting to lag multiple regressors will result in an error.
#'
#' For a given vector of lags, a lag matrix is built with one column
#' per lag. The column names are `"lag0"`, `"lag1"`, etc., corresponding
#' to the entries in `lags`. Rows which involve undefined values (due to
#' lagging) are dropped from both `y` and `X`. The indices of the rows
#' kept from the original series are returned in the `rows` component.
#'
#' @return
#' A list with components
#' \describe{
#'   \item{y}{The aligned response vector. If `preserve_ts = TRUE`
#'     and `y` was a `ts` object, then this is a `ts` object; otherwise
#'     a numeric vector.}
#'   \item{X}{The design matrix of lagged regressors, including an
#'     intercept column if `include_intercept = TRUE`. If `x` is `NULL`,
#'     then `X` is `NULL`.}
#'   \item{rows}{Integer vector of row indices from the original data
#'     that are retained after lagging.}
#'   \item{max_lag}{The maximum lag used, i.e. \code{max(lags)}.}
#' }
#'
#' @seealso
#' \code{\link[stats]{lag}}, \code{\link[stats]{embed}},
#' and \code{\link[stats]{ts}} for related time series utilities.
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## 1. Basic usage with y/x interface
#' ## ------------------------------------------------------------
#' set.seed(123)
#' n  <- 10
#' y  <- rnorm(n)
#' x  <- rnorm(n)
#'
#' ## Use current and one lag of x
#' ld1 <- lag_data(y = y, x = x, lags = 0:1)
#' ld1$y                # aligned response
#' ld1$X                # design matrix (Intercept, lag0, lag1)
#' ld1$rows             # indices retained
#'
#'
#' ## ------------------------------------------------------------
#' ## 2. Formula interface with a data frame
#' ## ------------------------------------------------------------
#' dat <- data.frame(
#'   y = rnorm(20),
#'   x = rnorm(20)
#' )
#'
#' ## Use y ~ x with one lag
#' ld2 <- lag_data(y ~ x, data = dat, lags = 0:2)
#' head(ld2$X)
#' length(ld2$y)
#'
#'
#' ## ------------------------------------------------------------
#' ## 3. Using the result in a regression / ARX setting
#' ## ------------------------------------------------------------
#' ## Here we regress y_t on an intercept and lagged x's
#' ld3 <- lag_data(y = y, x = x, lags = 0:2)
#'
#' ## Remove intercept column when passing to lm()
#' fit_lm <- lm(ld3$y ~ ld3$X[, -1])
#' coef(fit_lm)
#'
#'
#' ## ------------------------------------------------------------
#' ## 4. Time series example with preserve_ts = TRUE
#' ## ------------------------------------------------------------
#' y_ts <- Nile
#'
#' ## Regress Nile flow on its own lag-1 (simple ARX-like setup)
#' ld4 <- lag_data(y = y_ts, x = y_ts, lags = 1, preserve_ts = TRUE)
#'
#' ## y is now a ts object starting at the appropriate time
#' start(y_ts)
#' start(ld4$y)
#'
#' ## Fit an AR(1) model with lagged Nile as xreg
#' ## (intercept in X, lag in the second column)
#' fit_arx <- stats::arima(ld4$y,
#'                         order = c(1, 0, 0),
#'                         xreg  = ld4$X[, -1, drop = FALSE])
#' fit_arx
#'
#' @export
lag_data <- function(formula = NULL,
                          data = NULL,
                          y = NULL,
                          x = NULL,
                          lags = 0,
                          include_intercept = TRUE,
                          preserve_ts = FALSE) {

  # --- Handle formula interface ---
  if (!is.null(formula)) {

    # Get model frame
    mf <- model.frame(formula, data = data)

    # Extract y and RHS variables
    y <- mf[[1]]
    Xvars <- mf[-1]

    # If only intercept on RHS: no xreg
    if (ncol(Xvars) == 0 || all(sapply(Xvars, function(v) all(v == 1)))) {
      x <- NULL
    } else {
      # If multiple regressors: combine into a single matrix
      x <- as.matrix(Xvars)
      if (ncol(x) > 1 && !is.null(lags) && any(lags != 0)) {
        stop("Lagging multiple regressors not implemented in this version.")
      }
      # For now: require only 1 regressor for lagging
      if (ncol(x) > 1) stop("Only one RHS variable allowed when using lags.")
      x <- drop(x)  # ensure a vector
    }
  }

  # --- Now fallback to previous behaviour if formula NULL ---
  if (is.null(y) && is.null(x)) {
    stop("Need either formula=..., or y=..., or x=...")
  }

  # -----------------------------------------------------------
  # --- Build X matrix (single regressor) ----------------------
  # -----------------------------------------------------------
  if (!is.null(x)) {
    x <- as.numeric(x)
    n_x <- length(x)
    max_lag <- max(lags)

    # Build full lag matrix
    lagmat <- sapply(lags, function(k) {
      if (k == 0L) x else c(rep(NA, k), x[1:(n_x - k)])
    })
    colnames(lagmat) <- paste0("lag", lags)

    # Add intercept if requested
    if (include_intercept) {
      lagmat <- cbind(Intercept = 1, lagmat)
    }

    valid_idx <- (max_lag + 1):n_x
    X_clean <- lagmat[valid_idx, , drop = FALSE]

  } else {
    X_clean  <- NULL
    max_lag  <- 0
    valid_idx <- NULL
  }

  # -----------------------------------------------------------
  # --- Align y -----------------------------------------------
  # -----------------------------------------------------------
  if (!is.null(y)) {

    n_y <- length(y)

    if (max_lag >= n_y) {
      stop("max_lag too large: no y left after truncation.")
    }

    if (!is.null(x)) {
      row_idx <- valid_idx
    } else {
      row_idx <- seq_len(n_y)  # no lagging needed
    }

    if (preserve_ts && is.ts(y)) {
      t_start <- time(y)[row_idx[1]]
      y_clean <- window(y, start = t_start)
    } else {
      y_clean <- as.numeric(y)[row_idx]
    }

  } else {
    y_clean <- NULL
    row_idx <- valid_idx
  }

  # -----------------------------------------------------------
  # --- Return result -----------------------------------------
  # -----------------------------------------------------------
  list(
    y       = y_clean,
    X       = X_clean,
    rows    = row_idx,
    max_lag = max_lag
  )
}
