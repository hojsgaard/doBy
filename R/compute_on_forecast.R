


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
#' @param fc_object A `"forecast"` object (from the \pkg{forecast} package)
#'   for a univariate time series, typically obtained via
#'   \code{forecast::forecast()}.
#' @param trans_fun A function of the form \code{function(x, y0) ...}
#'   that takes a numeric vector `x` on the model scale and a scalar
#'   starting value `y0` on the output scale and returns a numeric vector
#'   of the same length on the output scale.
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
#'                             nsim = 20, level = 95, y0 = 1)
#'   plot(fc_lynx)
#'   forecast::autoplot(fc_lynx) + ggplot2::theme_minimal()
#'
#' ## Example 2 (variation): CO2 series, log-transform
#'   lco2    <- log(co2)
#'   fit_co2 <- forecast::auto.arima(lco2)
#'   fc_log_co2 <- forecast::forecast(fit_co2, h = 24)
#'   forecast::autoplot(fc_log_co2)
#'   trans_log <- function(z, y0) exp(z)
#'
#'   fc_co2 <- transform_forecast(fc_log_co2, trans_fun = trans_log,
#'                            nsim = 20, level = 95, y0 = 1)
#'   forecast::autoplot(fc_co2) + ggplot2::theme_minimal()
#' }
#' 
#' \dontrun{
#' # ## Example 3: Percentage change in income (uschange$Income)
#' if (requireNamespace("forecast", quietly = TRUE) &&
#'     requireNamespace("fpp2", quietly = TRUE)) {
#'   income <- uschange[, "Income"]  # quarterly percentage changes (%)
#' 
#'   ## transformation: pct change -> index with base 1
#'   trans_pct <- function(r, y0) {
#'     y0 * cumprod(1 + r / 100)
#'   }
#' 
#'   fit_pct <- forecast::auto.arima(income)
#'   fc_pct  <- forecast::forecast(fit_pct, h = 24)
#'   forecast::autoplot(fc_pct)
#'   fc_idx  <- transform_forecast(fc_pct, trans_fun = trans_pct,
#'                             nsim = 200, level = 95, y0 = 1)
#' 
#'   plot(fc_idx)
#'   forecast::autoplot(fc_idx) + ggplot2::theme_minimal()
#'  }
#'  }
#'  
#' @export
transform_forecast <- function(fc_object, trans_fun,
                           nsim = 2000L, level = 95, y0 = 1) {

  ## Input validation (same as before, but allow y_hist = NULL)
  .check_forecast_input(fc_object, trans_fun, y0, level)

  d <- fc_object$x
  h <- length(fc_object$mean)

  # ## 0) Historical series on output scale
  # if (is.null(y_hist)) {
  #   # reconstruct from model-scale data and y0
  #   y_hist <- trans_fun(as.numeric(d), y0)
  #   attributes(y_hist) <- attributes(d)
  # } else {
  #   # user-supplied; basic sanity check
  #   if (!is.numeric(y_hist) || is.null(tsp(y_hist)))
  #     stop("`y_hist` must be a numeric 'ts' object.", call. = FALSE)
  # }

  
  ## 0) Historical series on output scale
  y_hist <- trans_fun(as.numeric(d), y0)
  attributes(y_hist) <- attributes(d)

  
  ## last historical value on output scale
  Y_last <- as.numeric(utils::tail(y_hist, 1))

  ## 1) Simulated future on model scale
  sim_mat <- replicate(
    nsim,
    stats::simulate(fc_object$model, nsim = h, future = TRUE)
  )


  
  ## 2) Transform each path to output scale, anchored at Y_last
  level_mat <- apply(sim_mat, 2, trans_fun, y0 = Y_last)
  level_mat
  
  ## 3) Mean and prediction interval
  mean_level  <- rowMeans(level_mat)
  level
  alpha       <- (100 - level) / 200
  alpha

    lower_level <- apply(level_mat, 1, stats::quantile, probs = alpha)
  upper_level <- apply(level_mat, 1, stats::quantile, probs = 1 - alpha)

  upper_level
  lower_level
  
  
  
  ## 4) ts objects on same time scale as original forecast
  freq     <- stats::frequency(fc_object$mean)
  start_fc <- stats::start(fc_object$mean)

  mean_ts  <- stats::ts(mean_level,  start = start_fc, frequency = freq)
  lower_ts <- stats::ts(lower_level, start = start_fc, frequency = freq)
  upper_ts <- stats::ts(upper_level, start = start_fc, frequency = freq)

  ## 5) New forecast object
  fc_new        <- fc_object
  fc_new$x      <- y_hist
  fc_new$mean   <- mean_ts
  fc_new$level  <- level
  fc_new$lower  <- stats::setNames(cbind(lower_ts), paste0(level, "%"))
  fc_new$upper  <- stats::setNames(cbind(upper_ts), paste0(level, "%"))

  fc_new
}
























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




