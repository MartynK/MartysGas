#' Convert Day of Year to Date
#'
#' This function converts a given day of the year to a date object. It's useful for 
#' converting numeric day-in-year representations back to standard date formats.
#'
#' @param day_in_year An integer or character representing the day of the year. 
#'                    For example, 1 for January 1st, 32 for February 1st, etc.
#' @param yr The year to which the day belongs, specified as an integer. 
#'           Default is 2023.
#'
#' @return A `Date` object representing the corresponding date in the specified year.
#'
#' @examples
#' # Convert the 100th day of 2023 to a date
#' date_from_day <- yday_inverse(100)
#'
#' # Convert the 50th day of 2022 to a date
#' date_from_day_2022 <- yday_inverse(50, 2022)
#'
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr `%>%`
#' @export
yday_inverse <- function(day_in_year, yr = 2023) {
  require(lubridate)
  require(dplyr)
  
  # Convert to character for parsing
  if (typeof(day_in_year) != "character") {
    day_in_year <- as.character(day_in_year)
  }
  
  # Parse and convert to date
  parse_date_time(x = paste(yr, day_in_year), orders = "yj") %>% 
    as.Date() %>% 
    return()
}
