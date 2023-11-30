#' Simulate Weather Data
#'
#' This function generates simulated weather data for a given number of days. It 
#' specifically simulates daily average temperature and temperature range, accounting 
#' for autocorrelation and variability in the data. The function is capable of 
#' handling large simulations by segmenting the process.
#'
#' @param n An integer specifying the number of days for which to simulate weather 
#'          data. Default is 365, representing a year.
#' @param coefs A list containing the autocorrelation coefficients (acf) and standard 
#'              deviations (sigma) for the average temperature (`tavg_acf`, `tavg_sigma`) 
#'              and temperature range (`range_acf`, `range_sigma`). Default values are 
#'              set for a typical year.
#'
#' @return A dataframe containing the simulated daily weather data, including day in 
#'         year, simulated year, predicted average temperature (`pred_tavg`), and 
#'         predicted temperature range (`pred_range`), as well as minimum (`tmin`) 
#'         and maximum (`tmax`) temperatures for each day.
#'
#' @examples
#' # Simulate weather data for a default one-year period
#' weather_data <- simulate_weather()
#'
#' # Simulate weather data for 100 days with custom coefficients
#' custom_coefs <- list(tavg_acf = 0.8, tavg_sigma = 3.5, range_acf = 0.4, range_sigma = 3.0)
#' weather_data_custom <- simulate_weather(n = 100, coefs. = custom_coefs)
#'
#' @importFrom dplyr mutate bind_rows
#' @importFrom stats predict
#' @export
simulate_weather <- function(n = 365, 
                             coefs. = list(
                               tavg_acf = 0.833,
                               tavg_sigma = 3.73,
                               range_acf = 0.457,
                               range_sigma = 3.35)) {
  
  # Handle large simulations by segmenting
  if (n > 1000) {
    pr <- simulate_weather(1000)
    remain <- n - 1000
    for (i in 1:ceiling(remain / 1000)) {
      pr <- bind_rows(pr, simulate_weather(min(remain, 1000))) %>%
        mutate(year_sim = rep(1:ceiling(n()/365), each = 365)[1:n()])
      remain <- remain - 1000
    }
    return(pr)
  }
  
  # Initial data frame setup
  pr <- 
    data.frame(
      day_in_year = rep(1:365, ceiling(n / 365))[1:n],
      year_sim = rep(1:ceiling(n / 365), each = 365)[1:n]) %>%
    mutate(
      year_loc = (180 - abs(day_in_year - 180)) / 180)
  
  # Simulate average temperature and range
  pr$pred_tavg  <- predict(mod_tavg, newdata = pr) + 
    simulate_corr_resids(n = n, autocorrelation = coefs.$tavg_acf,
                         total_sd = coefs.$tavg_sigma)
  pr$pred_range <- predict(mod_range, newdata = pr) + 
    simulate_corr_resids(n = n, 
                         autocorrelation = coefs.$range_acf,
                         total_sd = coefs.$range_sigma)
  
  # Calculate minimum and maximum temperatures
  pr <- pr %>%
    mutate(tmin = pred_tavg - pred_range / 2,
           tmax = pred_tavg + pred_range / 2)
  
  return(pr)  
}

load(here::here("inst","function","backend","mod_tavg.Rdata"))
load(here::here("inst","function","backend","mod_range.Rdata"))

coefs <- list(
  tavg_acf = coef(mod_tavg$modelStruct$corStruct, 
                  unconstrained = FALSE),
  tavg_sigma = summary(mod_tavg)$sigma,
  range_acf = coef(mod_range$modelStruct$corStruct, 
                   unconstrained = FALSE),
  range_sigma = summary(mod_range)$sigma  
)