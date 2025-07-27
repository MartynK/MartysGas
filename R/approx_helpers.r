#' Create an interpolation function for gas meter readings
#'
#' Given dates and corresponding cumulative meter values, this function
#' creates a linear interpolation function to estimate the meter
#' at arbitrary time points.
#'
#' @param dates Vector of POSIXct timestamps
#' @param values Vector of cumulative meter readings corresponding to `dates`
#' @return A function that can be used to interpolate the meter reading
#' @export
create_meter_fun <- function(dates, values) {
  approxfun(dates, values, rule = 2, na.rm = TRUE)
}

#' Approximate instantaneous usage rate from a meter interpolation function
#'
#' @param meter_fun Function created by `create_meter_fun`
#' @param x Time points at which the rate should be evaluated
#' @param h Step size for the numerical derivative
#' @return Estimated usage rate at `x`
#' @export
approx_rate <- function(meter_fun, x, h = 1e-6) {
  (meter_fun(x + h) - meter_fun(x)) / h
}

#' Average temperature between two timestamps
#'
#' @param df Data frame containing at least a time column and a temperature column
#' @param var Name of the temperature column
#' @param var_time Name of the time column
#' @param xmin,xmax Start and end times for the interval
#' @return Numeric average temperature over the interval
#' @export
get_avg_temp <- function(df, var = "tavg", var_time = "Date", xmin, xmax) {
  f <- approxfun(df[[var_time]], df[[var]])
  integrated <- NA
  try({
    integrated <- integrate(f, subdivisions = 10000,
                            rel.tol = 0.1,
                            lower = xmin, upper = xmax)$value
  }, silent = TRUE)
  duration <- as.numeric(difftime(xmax, xmin, units = "secs"))
  integrated / duration
}

#' Capture base plot output as a recordable object
#'
#' Evaluates an expression that generates a base R plot and
#' returns the plot as a `recordedplot` object.
#'
#' @param expr Expression containing plotting code
#' @return `recordedplot` object
#' @export
capture_plot <- function(expr) {
  expr
  p <- recordPlot()
  invisible(dev.off())
  p
}

