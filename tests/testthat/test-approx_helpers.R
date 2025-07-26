library(testthat)

# simple time series
x <- as.POSIXct("2024-01-01") + 0:5 * 3600
meter <- cumsum(rep(1,6))
fun <- create_meter_fun(x, meter)

test_that("approx_rate returns correct derivative", {
  expect_equal(approx_rate(fun, x[3]), 1, tolerance = 1e-3)
})

test_that("get_avg_temp integrates mean", {
  df <- data.frame(Date = x, tavg = meter)
  res <- get_avg_temp(df, xmin = x[1], xmax = x[6])
  expect_true(abs(res - mean(df$tavg)) < 1e-2)
})
