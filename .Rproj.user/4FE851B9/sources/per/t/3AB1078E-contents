#' Calculate Temperature Sum Over a Time Interval
#'
#' The `maketsum` function calculates the sum of average temperatures over a 
#' specified time interval, normalized by the length of the interval in hours. 
#' This function is designed to work with a dataset `obs_hours` containing 
#' temperature data (`tavg`) and corresponding dates (`Date`).
#'
#' @param date The end date of the interval for which the temperature sum is to be 
#'             calculated. This should be an object of class `Date`.
#' @param datelag The start date of the interval. This should also be an object of 
#'                class `Date`.
#'
#' @return The function returns the sum of average temperatures over the interval 
#'         between `datelag` and `date`, normalized by the total number of hours in 
#'         the interval.
#'
#' @examples
#' # Assuming temps_xtra is a dataframe with Date and tavg columns
#' datelag <- as.Date("2023-01-01")
#' date <- as.Date("2023-01-31")
#' temp_sum <- maketsum(date, datelag)
#'
#' @importFrom lubridate interval as.numeric
#' @export
maketsum <- function(date, datelag, obs_hours.) {
  # Calculate the interval in hours
  hours <- interval(datelag, date) %>% as.numeric
  hours <- hours / 3600
  
  # Sum temperatures within the interval
  tsum <- obs_hours.$tavg[obs_hours.$Date < date &
                           obs_hours.$Date > datelag] %>%
    sum
  
  return(tsum / hours)
}

#' Vectorized Version of maketsum
#'
#' The `Maketsum` function is a vectorized version of `maketsum`, allowing it to 
#' be applied over vectors of dates and datelags.
#'
#' @inheritParams maketsum
#'
#' @return A vector of normalized temperature sums for each pair of `date` and 
#'         `datelag` values.
#'
#' @examples
#' # Vectorizing over multiple intervals
#' dates <- as.Date(c("2023-01-31", "2023-02-28"))
#' datelags <- as.Date(c("2023-01-01", "2023-02-01"))
#' temp_sums <- Maketsum(dates, datelags)
#'
#' @export
Maketsum <- Vectorize(maketsum, vectorize.args = c("date", "datelag"))
