


.check_forecast_input <- function(fc_object, trans_fun, y0, level) {

  ## Check forecast object
  if (!inherits(fc_object, "forecast"))
    stop("`fc_object` must be a 'forecast' object from the forecast package.", call. = FALSE)

  ## Check model supports simulate()
  if (is.null(fc_object$model))
    stop("`fc_object$model` is NULL - cannot simulate future paths.", call. = FALSE)

  if (!is.function(stats::simulate))
    stop("Internal error: stats::simulate() not found.", call. = FALSE)

  ## Check that simulate() works on the model
  test_sim <- try(stats::simulate(fc_object$model, nsim = 1, future = TRUE), silent = TRUE)
  if (inherits(test_sim, "try-error"))
    stop("`fc_object$model` does not support simulate(..., future = TRUE).",
         "\nModel class: ", paste(class(fc_object$model), collapse = ", "),
         call. = FALSE)

  ## Check level
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 100)
    stop("`level` must be a single number between 0 and 100.", call. = FALSE)

  ## Check y0
  if (!is.numeric(y0) || length(y0) != 1)
    stop("`y0` must be a single numeric value.", call. = FALSE)

  ## Check trans_fun
  if (!is.function(trans_fun))
    stop("`trans_fun` must be a function of the form function(x, y0).", call. = FALSE)

  ## Try calling trans_fun to detect structural errors
  z <- try(trans_fun(c(1,2,3), y0 = y0), silent = TRUE)
  if (inherits(z, "try-error"))
    stop("`trans_fun` failed with test input: trans_fun(c(1,2,3), y0).\n",
         "Please ensure it has the form function(x, y0).", call. = FALSE)

  ## trans_fun must return numeric vector of same length
  if (!is.numeric(z) || length(z) != 3)
    stop("`trans_fun(x, y0)` must return a numeric vector of the same length as `x`.",
         call. = FALSE)

  ## Check fc_object$x exists and is numeric ts
  if (is.null(fc_object$x))
    stop("`fc_object$x` is NULL; cannot reconstruct history.", call. = FALSE)
  if (!is.numeric(fc_object$x))
    stop("`fc_object$x` must be numeric.", call. = FALSE)
  if (is.null(tsp(fc_object$x)))
    stop("`fc_object$x` must be a time series (ts) object.", call. = FALSE)

  ## Check forecast horizon
  if (!is.numeric(fc_object$mean) || is.null(tsp(fc_object$mean)))
    stop("`fc_object$mean` must be a numeric time series (ts).", call. = FALSE)

  invisible(TRUE)
}


#' Transform forecasts from model scale to data scale by simulation
#'
#' @description
#' `transform_forecast()` takes a univariate time series forecast fitted on a
#' transformed (model) scale and produces a new forecast object on the
#' original data scale. This is done by simulating future paths on the
#' model scale, transforming each path with a user-supplied function, and
#' then computing pointwise means and prediction intervals on the
#' transformed scale.
#'
#' @details
#' The function assumes that `fc_object` is a `"forecast"` object as
#' produced by the \pkg{forecast} package, and that `fc_object$model`
#' supports `simulate()` with arguments `nsim` and `future`.
#'
#' The transformation function `trans_fun` must have the form
#' `trans_fun(x, y0)`, where `x` is a numeric vector representing a path on
#' the model scale (for example log-values or percentage changes), and
#' `y0` is a scalar "starting value" on the output scale. The function must
#' return a numeric vector of the same length as `x` giving the
#' corresponding path on the output scale.
#'
#' Internally, `transform_forecast()` first reconstructs a historical series on
#' the output scale by applying `trans_fun()` to `fc_object$x` with the
#' supplied `y0`. It then simulates `nsim` future paths from
#' `fc_object$model` on the model scale, transforms each path to the
#' output scale using `trans_fun()` with `y0` equal to the last value of
#' the reconstructed historical series, and finally computes the pointwise
#' mean and prediction intervals (of nominal coverage `level`) across
#' the simulated paths. The result is returned as a new `"forecast"` object
#' with `x`, `mean`, `lower`, and `upper` on the output scale.
#'
#' @param fc A `"forecast"` object (from the \pkg{forecast} package)
#'   for a univariate time series, typically obtained via
#'   \code{forecast::forecast()}.
#' @param trans_fun A function of the form \code{function(x, y0) ...}
#'   that takes a numeric vector `x` on the model scale and a scalar
#'   starting value `y0` on the output scale and returns a numeric vector
#'   of the same length on the output scale.
#' @param xreg_future Optional matrix or vector of future values for regressors in ARMAX model. 
#' @param nsim Integer; number of simulated future paths to use. Larger
#'   values give smoother prediction intervals but take longer to compute.
#' @param level Numeric; prediction interval coverage in percent.
#' @param y0 Numeric; starting value on the output scale used to
#'   reconstruct the historical series from `fc_object$x`. For percentage
#'   change models it is often natural to set \code{y0 = 1} and interpret
#'   the resulting series as an index.
#'
#' @return
#' A `"forecast"` object similar to `fc_object`, but with the components
#' `x`, `mean`, `lower`, and `upper` defined on the output (data) scale.
#'
#' @seealso
#' \code{\link[forecast]{forecast}}, \code{\link[forecast]{auto.arima}},
#' \code{\link[stats]{simulate}}, \code{\link[stats]{ts}}.
#'
#' @examples
#' ## Example 1: Log-transform of the Canadian lynx data
#' if (requireNamespace("forecast", quietly = TRUE)) {
#'
#'   llynx   <- log(lynx)
#'   fit_log <- forecast::auto.arima(llynx)
#'   fc_log  <- forecast::forecast(fit_log, h = 20)
#'   forecast::autoplot(fc_log)
#'   ## transformation: log -> original scale
#'   trans_log <- function(z, y0) {
#'     exp(z)
#'   }
#'   fc_lynx <- transform_forecast(fc_log, trans_fun = trans_log,
#'                             nsim = 20)
#'   forecast::autoplot(fc_lynx) + ggplot2::theme_minimal()
#' }
#'
#'
#' \dontrun{
#' if (requireNamespace("forecast", quietly = TRUE)) {
#' ## Example 2 (variation): CO2 series, log-transform
#'   lco2    <- log(co2)
#'   fit_co2 <- forecast::auto.arima(lco2)
#'   fc_log_co2 <- forecast::forecast(fit_co2, h = 24)
#'   forecast::autoplot(fc_log_co2)
#'   trans_log <- function(z, y0) exp(z)
#'
#'   fc_co2 <- transform_forecast(fc_log_co2, trans_fun = trans_log,
#'                          nsim = 30)
#'   forecast::autoplot(fc_co2) + ggplot2::theme_minimal()
#'}
#'
#' # ## Example 3: Percentage change in income and consumption
#if (requireNamespace("forecast", quietly = TRUE) &&
#'     requireNamespace("fpp2", quietly = TRUE)) {
#'   income <- uschange[, "Income"]  # quarterly percentage changes (%)
#'   consumption <- uschange[, "Consumption"]  # quarterly percentage changes (%)
#' 
#'  forecast::checkresiduals(income)
#'  forecast::checkresiduals(consumption)
#'  
#'   trans_pct <- function(r, y0) {
#'   y0 * cumprod(1 + r / 100)
#' }
#' 
#' ## ARIMA model for income
#' fit_income_pct <- forecast::auto.arima(income)
#' fit_income_pct
#'
#' fc_income_pct  <- forecast::forecast(fit_income_pct, h = 48)
#' forecast::autoplot(fc_income_pct)
#' 
#'  fc_income  <- transform_forecast(fc_income_pct, trans_fun = trans_pct,
#'                            nsim = 200, level = 95, y0 = 1)
#'  forecast::autoplot(fc_income) + ggplot2::theme_minimal()
#'  
#'  ## ARIMA model for consumption
#' 
#'  fit_cons_pct0 <- forecast::auto.arima(consumption, seasonal = F)
#'  fit_cons_pct0
#'  fc_cons_pct0  <- forecast::forecast(fit_cons_pct0, h = 48)
#'  
#'  fc_cons0  <- transform_forecast(fc_cons_pct0, trans_fun = trans_pct, 
#'                                 nsim = 200, level = 95, y0 = 1)
#'  forecast::autoplot(fc_cons0)
#'  
#'  ## ARIMAX model for consumption with income as xreg
#'   
#'  fit_cons_pct <- forecast::auto.arima(consumption, xreg = income)
#'  fit_cons_pct
#'  
#'  fc_income_pct$mean |> head()
#'  
#'  fc_cons_pct  <- forecast::forecast(fit_cons_pct, h = 48, 
#'                  xreg = fc_income_pct$mean)
#'  forecast::autoplot(fc_cons_pct)
#'  
#'  fc_cons  <- transform_forecast(fc_cons_pct, trans_fun = trans_pct, 
#'                                 xreg_future = fc_income$mean, 
#'                                 nsim = 200, level = 95, y0 = 1)
#'  forecast::autoplot(fc_cons)
#' 
#' #'  
#'  }
#'  }
#'  
#' @export
transform_forecast <- function(fc,
                               trans_fun,          # e.g. trans_pct
                               xreg_future = NULL,
                               nsim        = 1000,
                               level       = 95,
                               y0          = 1    # baseline index level
                               ) {

  if (!inherits(fc, "forecast"))
    stop("'fc' must be an object of class 'forecast'")

  fit <- fc$model
  if (!inherits(fit, "Arima"))
    stop("'fc$model' must be an 'Arima' object from forecast::Arima()")

  h <- length(fc$mean)

  ## --- Construct index for the observed data -----------------------------

  # fc$x is the historical percentage-change series r_t
  r_hist <- as.numeric(fc$x)

  # Index for history based at y0 (e.g. 1):
  # y_t = y0 * cumprod(1 + r_t/100)
  y_hist <- trans_fun(r_hist, y0 = y0)

  # Last historical index level
  y_last <- tail(y_hist, 1)
##  cat("Last historical index level (y_last): ", y_last, "\n")

  ## --- xreg handling -----------------------------------------------------
  model_has_xreg <- !is.null(fit$xreg)

  if (model_has_xreg && is.null(xreg_future)) {
    stop("Model was fitted with xreg; you must supply 'xreg_future'.")
  }

  if (!model_has_xreg && !is.null(xreg_future)) {
    warning("Model was fitted without xreg; 'xreg_future' will be ignored.")
    xreg_future <- NULL
  }

  if (!is.null(xreg_future)) {
    xreg_future <- as.matrix(xreg_future)
    if (nrow(xreg_future) != h) {
      stop("nrow(xreg_future) must equal forecast horizon h = ", h,
           ", but is ", nrow(xreg_future), ".")
    }
  }

  ## --- Levels for prediction intervals ----------------------------------
  level <- sort(as.numeric(level))
  if (any(level <= 0 | level >= 100))
    stop("'level' must be between 0 and 100")

  ## --- Simulate future percentage changes and backtransform -------------
  sim_mat <- replicate(nsim, {
    r_sim <- if (is.null(xreg_future)) {
      simulate(fit, nsim = h, future = TRUE)
    } else {
      simulate(fit, nsim = h, future = TRUE, xreg = xreg_future)
    }

    r_sim <- as.numeric(r_sim)

    # Backtransform: future index starting from *last observed* index level
    trans_fun(r_sim, y0 = y_last)
  })

  # h x nsim matrix of index values
  mean_fc <- rowMeans(sim_mat)

  alpha <- (100 - level) / 200
  probs <- rbind(alpha, 1 - alpha)
  lower <- upper <- matrix(NA_real_, nrow = h, ncol = length(level))
  for (j in seq_along(level)) {
    q <- apply(sim_mat, 1, quantile, probs = probs[, j])
    lower[, j] <- q[1, ]
    upper[, j] <- q[2, ]
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")

  ## --- Time structure ----------------------------------------------------
  x_old   <- fc$x
  mean_ts <- fc$mean

  # ts for history index
  y_hist_ts <- ts(y_hist,
                  start     = start(x_old),
                  frequency = frequency(x_old))

  # ts for forecast mean
  mean_bt <- ts(mean_fc,
                start     = start(mean_ts),
                frequency = frequency(mean_ts))

  ## --- Build new forecast object ----------------------------------------
  fc_new        <- fc
  fc_new$x      <- y_hist_ts     # history on index/“absolute” scale
  fc_new$mean   <- mean_bt       # back-transformed forecasts
  fc_new$lower  <- lower
  fc_new$upper  <- upper
  fc_new$level  <- level
  fc_new$method <- paste0(fc$method,
                          " (back-transformed from % changes via simulation)")

  fc_new
}

























# transform_forecast <- function(fc,
#                                trans_fun,
#                                nsim        = 1000,
#                                level       = 95,
#                                y0          = 1,
#                                xreg_future = NULL,
#                                backtransform_x = TRUE) {
#   # fc: forecast object from forecast::forecast(Arima(...))
#   # trans_fun: inverse transformation, applied ELEMENTWISE
#   #            (vector in, same-length vector out), e.g. exp, or function(z,y0) y0*exp(z)
#   # nsim: number of simulated paths
#   # level: numeric vector of confidence levels (e.g. 95 or c(80,95))
#   # y0: passed to trans_fun
#   # xreg_future: future xreg values (matrix or vector) for horizon h
#   # backtransform_x: if TRUE, also backtransform fc$x (and fc$fitted if present)
# 
#   if (!inherits(fc, "forecast"))
#     stop("'fc' must be an object of class 'forecast'")
# 
#   fit <- fc$model
#   if (!inherits(fit, "Arima"))
#     stop("'fc$model' must be an 'Arima' object from the forecast package")
# 
#   # Forecast horizon
#   h <- length(fc$mean)
# 
#   # Did the model use xreg when fitted?
#   model_has_xreg <- !is.null(fit$xreg)
# 
#   # --- xreg_future checks ---------------------------------------------------
#   if (model_has_xreg && is.null(xreg_future)) {
#     stop("This ARIMA model was fitted with xreg, so you must supply 'xreg_future'.")
#   }
# 
#   if (!model_has_xreg && !is.null(xreg_future)) {
#     warning("Model was fitted without xreg; 'xreg_future' will be ignored.")
#     xreg_future <- NULL
#   }
# 
#   if (!is.null(xreg_future)) {
#     xreg_future <- as.matrix(xreg_future)
#     if (nrow(xreg_future) != h) {
#       stop("nrow(xreg_future) must equal forecast horizon h = ", h,
#            ", but is ", nrow(xreg_future), ".")
#     }
#   }
# 
#   # --- level checks ---------------------------------------------------------
#   level <- sort(as.numeric(level))
#   if (any(level <= 0 | level >= 100))
#     stop("'level' must be between 0 and 100")
# 
#   y_hist  <- trans_fun(as.numeric(fc$x), y0 = y0)
#   y_last  <- tail(y_hist, 1)
#   
#   
#     # --- Simulate on model (transformed) scale and backtransform -------------
#   sim_mat <- replicate(nsim, {
#     z_sim <- if (is.null(xreg_future)) {
#       simulate(fit, nsim = h, future = TRUE)
#     } else {
#       simulate(fit, nsim = h, future = TRUE, xreg = xreg_future)
#     }
# 
#     # Backtransform simulated path to original scale
# ##    trans_fun(as.numeric(z_sim), y0 = y0)
#         trans_fun(as.numeric(z_sim), y0 = y0) + y_last
#   })
# 
#   # sim_mat: h x nsim on ORIGINAL scale
#   mean_fc <- rowMeans(sim_mat)
# 
#   # lower/upper for each level
#   alpha <- (100 - level) / 200
#   probs <- rbind(alpha, 1 - alpha)
# 
#   lower <- upper <- matrix(NA_real_, nrow = h, ncol = length(level))
#   for (j in seq_along(level)) {
#     q <- apply(sim_mat, 1, quantile, probs = probs[, j])
#     lower[, j] <- q[1, ]
#     upper[, j] <- q[2, ]
#   }
#   colnames(lower) <- colnames(upper) <- paste0(level, "%")
# 
#   # Preserve time index from fc$mean
#   mean_ts <- fc$mean
#   mean_bt <- ts(mean_fc,
#                 start     = start(mean_ts),
#                 frequency = frequency(mean_ts))
# 
#   # --- Build new forecast object -------------------------------------------
#   fc_new <- fc
#   fc_new$mean  <- mean_bt
#   fc_new$lower <- lower
#   fc_new$upper <- upper
#   fc_new$level <- level
# 
#   # Optionally backtransform history and fitted values as well
#   if (backtransform_x && !is.null(fc$x)) {
#     x_old <- fc$x
#     x_bt  <- trans_fun(as.numeric(x_old), y0 = y0)
#     fc_new$x <- ts(x_bt,
#                    start     = start(x_old),
#                    frequency = frequency(x_old))
#   }
# 
#   if (backtransform_x && !is.null(fc$fitted)) {
#     f_old <- fc$fitted
#     f_bt  <- trans_fun(as.numeric(f_old), y0 = y0)
#     fc_new$fitted <- ts(f_bt,
#                         start     = start(f_old),
#                         frequency = frequency(f_old))
#   }
# 
#   fc_new$method <- paste0(fc$method, " (back-transformed via simulation)")
#   fc_new
# }





# transform_forecast <- function(fc,
#                                trans_fun,
#                                nsim        = 1000,
#                                level       = 95,
#                                y0          = 1,
#                                xreg_future = NULL) {
#   # fc: forecast object from forecast::forecast(Arima(...))
#   # trans_fun: function(z, y0) returning back-transformed vector
#   # nsim: number of simulated paths
#   # level: numeric vector of confidence levels, e.g. 95 or c(80, 95)
#   # y0: parameter passed to trans_fun
#   # xreg_future: future xreg (matrix / vector) for the forecast horizon
# 
#   if (!inherits(fc, "forecast"))
#     stop("'fc' must be an object of class 'forecast'")
# 
#   fit <- fc$model
#   if (!inherits(fit, "Arima"))
#     stop("'fc$model' must be an 'Arima' object from the forecast package")
# 
#   # Forecast horizon
#   h <- length(fc$mean)
# 
#   # Did the model use xreg when fitted?
#   model_has_xreg <- !is.null(fit$xreg)
# 
#   # --- Check xreg_future consistency ---------------------------------------
#   if (model_has_xreg && is.null(xreg_future)) {
#     stop("This ARIMA model was fitted with xreg, ",
#          "so you must supply 'xreg_future'.")
#   }
# 
#   if (!model_has_xreg && !is.null(xreg_future)) {
#     warning("Model was fitted without xreg; 'xreg_future' will be ignored.")
#     xreg_future <- NULL
#   }
# 
#   if (!is.null(xreg_future)) {
#     xreg_future <- as.matrix(xreg_future)
#     if (nrow(xreg_future) != h) {
#       stop("nrow(xreg_future) must equal forecast horizon h = ", h,
#            ", but is ", nrow(xreg_future), ".")
#     }
#   }
# 
#   # Ensure 'level' is a numeric vector
#   level <- sort(as.numeric(level))
#   if (any(level <= 0 | level >= 100))
#     stop("'level' must be between 0 and 100")
# 
#   # --- Simulation on model scale -------------------------------------------
#   # Each column of sim_mat is one simulated path on ORIGINAL (model) scale
#   sim_mat <- replicate(nsim, {
#     if (is.null(xreg_future)) {
#       z <- simulate(fit, nsim = h, future = TRUE)
#     } else {
#       z <- simulate(fit, nsim = h, future = TRUE, xreg = xreg_future)
#     }
#     # Back-transform this simulated path
#     trans_fun(z, y0 = y0)
#   })
# 
#   # sim_mat: h x nsim matrix on transformed-back scale
#   # --- Compute summary stats -----------------------------------------------
#   mean_fc <- rowMeans(sim_mat)
# 
#   # Build lower/upper matrices for each level
#   alpha  <- (100 - level) / 200
#   probs  <- rbind(alpha, 1 - alpha)  # 2 x L
# 
#   lower <- upper <- matrix(NA_real_, nrow = h, ncol = length(level))
#   for (j in seq_along(level)) {
#     q <- apply(sim_mat, 1, quantile, probs = probs[, j])
#     lower[, j] <- q[1, ]
#     upper[, j] <- q[2, ]
#   }
# 
#   # Preserve ts attributes for mean
#   mean_ts <- fc$mean
#   mean_bt <- ts(mean_fc,
#                 start     = stats::start(mean_ts),
#                 frequency = stats::frequency(mean_ts))
# 
#   # lower/upper as matrices with ts attributes if you like
#   colnames(lower) <- colnames(upper) <- paste0(level, "%")
# 
#   # --- Construct new forecast object --------------------------------------
#   fc_new <- fc
#   fc_new$mean  <- mean_bt
#   fc_new$lower <- lower
#   fc_new$upper <- upper
#   fc_new$level <- level
# 
#   # Optionally back-transform the historical series as well:
#   # (comment out if you prefer to leave fc$x as is)
# #  if (!is.null(fc$x)) {
# #    fc_new$x <- trans_fun(fc$x, y0 = y0)
# #  }
# 
# #   if (!is.null(fc$x)) {
# #   x_old <- fc$x
# #   fc_new$x <- ts(
# #     trans_fun(as.numeric(x_old), y0 = y0),
# #     start     = start(x_old),
# #     frequency = frequency(x_old)
# #   )
# # }
# 
#   fc_new$method <- paste0(fc$method, " (back-transformed via simulation)")
# 
#   fc_new
# }






#' #' @export
#' transform_forecast <- function(fc_object, trans_fun,
#'                            nsim = 2000L, level = 95, y0 = 1, xreg_future=NULL) {
#' 
#'   ## Input validation (same as before, but allow y_hist = NULL)
#'   .check_forecast_input(fc_object, trans_fun, y0, level)
#' 
#'   d <- fc_object$x
#'   h <- length(fc_object$mean)
#' 
#'   # ## 0) Historical series on output scale
#'   # if (is.null(y_hist)) {
#'   #   # reconstruct from model-scale data and y0
#'   #   y_hist <- trans_fun(as.numeric(d), y0)
#'   #   attributes(y_hist) <- attributes(d)
#'   # } else {
#'   #   # user-supplied; basic sanity check
#'   #   if (!is.numeric(y_hist) || is.null(tsp(y_hist)))
#'   #     stop("`y_hist` must be a numeric 'ts' object.", call. = FALSE)
#'   # }
#' 
#'   
#'   ## 0) Historical series on output scale
#'   y_hist <- trans_fun(as.numeric(d), y0)
#'   attributes(y_hist) <- attributes(d)
#' 
#'   
#'   ## last historical value on output scale
#'   Y_last <- as.numeric(utils::tail(y_hist, 1))
#' 
#'   ## 1) Simulated future on model scale
#'   sim_mat <- replicate(
#'     nsim,
#'     stats::simulate(fc_object$model, nsim = h, future = TRUE)
#'   )
#' 
#' 
#'   
#'   ## 2) Transform each path to output scale, anchored at Y_last
#'   level_mat <- apply(sim_mat, 2, trans_fun, y0 = Y_last)
#'   level_mat
#'   
#'   ## 3) Mean and prediction interval
#'   mean_level  <- rowMeans(level_mat)
#'   level
#'   alpha       <- (100 - level) / 200
#'   alpha
#' 
#'     lower_level <- apply(level_mat, 1, stats::quantile, probs = alpha)
#'   upper_level <- apply(level_mat, 1, stats::quantile, probs = 1 - alpha)
#' 
#'   upper_level
#'   lower_level
#'   
#'   
#'   
#'   ## 4) ts objects on same time scale as original forecast
#'   freq     <- stats::frequency(fc_object$mean)
#'   start_fc <- stats::start(fc_object$mean)
#' 
#'   mean_ts  <- stats::ts(mean_level,  start = start_fc, frequency = freq)
#'   lower_ts <- stats::ts(lower_level, start = start_fc, frequency = freq)
#'   upper_ts <- stats::ts(upper_level, start = start_fc, frequency = freq)
#' 
#'   ## 5) New forecast object
#'   fc_new        <- fc_object
#'   fc_new$x      <- y_hist
#'   fc_new$mean   <- mean_ts
#'   fc_new$level  <- level
#'   fc_new$lower  <- stats::setNames(cbind(lower_ts), paste0(level, "%"))
#'   fc_new$upper  <- stats::setNames(cbind(upper_ts), paste0(level, "%"))
#' 
#'   fc_new
#' }
#' 
#' 
#' 
#' 




















## EKSEMPEL

## library(forecast)
## library(ggplot2)

## ## Change in income
## library(fpp2)
## d <- uschange[,"Income"]      

## trans1 <- function(r, y0) {
##   y0 * cumprod(1 + r/100)
## }

## fit    <- auto.arima(d)
## fc_pct <- forecast(fit, h = 24)

## fc_lev <- transform_forecast(fc_pct, trans1, nsim = 5000, level = 95,y0=1)
## autoplot(fc_lev) + theme_minimal()

## ## Lynx
## llynx <- log(lynx)
## fit    <- auto.arima(llynx)
## fc_pct <- forecast(fit, h = 24)


## trans2 <- function(z, y0) exp(z)

## fit_log <- auto.arima(log(lynx))
## fc_log  <- forecast(fit_log, h = 20)

## fc_lynx <- transform_forecast(fc_log, trans_fun = trans2, y0 = 1)
## autoplot(fc_lynx) + theme_minimal()




