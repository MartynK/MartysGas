# Ths script would get the exported data (from meteostat) from the 'data' folder
# and merges them into one file


#' Merge and Transform Meteostat Weather Data
#'
#' This function reads weather data files (Excel format) from a specified directory,
#' merges them, and performs various transformations including renaming columns,
#' calculating temperature range, average temperature, lagged temperatures,
#' and several date-related variables.
#'
#' @param data_dir A string specifying the directory containing the weather data files.
#' @param output_file A string specifying the path where the merged and transformed data
#'        should be saved as an Rdata file.
#' @return The function doesn't return anything but saves the processed data as an Rdata file.
#' @import dplyr
#' @import readxl
#' @import lubridate
#' @import here
#' @export
#'
#' @examples
#' merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
#'                          output_file = "data/meteostat_data.Rdata")
merge_transform_weather <- function(data_dir, gaz_dir, output_file) {
  
  # Load required libraries
  library(dplyr)
  library(readxl)
  library(lubridate)
  library(here)
  
  # List all files in the data directory
  fil <- here::here(data_dir) %>% list.files
  
  # Check if the directory is empty
  if (length(fil) == 0) {
    stop("Data directory is empty.")
  } else {
    
    # Loop through files and merge them
    for (i in seq_along(fil)) {
      
      file_path <- here::here(data_dir, fil[i])
      
      if (i == 1) {
        meteostat_weather <- read_excel(file_path)
      } else {
        meteostat_weather <- rbind(meteostat_weather, read_excel(file_path))
      }
    }
  } 
  
  # Data transformations
  meteostat_weather <- meteostat_weather %>%
    group_by(date) %>%
    slice_tail(n = 1) %>%
    ungroup %>%
    rename(Date = date) %>% 
    mutate(
      Date = as_datetime(Date),
      range = tmax - tmin,
      tsum  = (tmax + tmin + tavg) / 3,
      day_in_year = yday(Date),
      year = year(Date),
      ywint = as.factor(year(Date - 180 * 3600 * 24)),
      year_loc = (180 - abs( day_in_year - 180)) / 180,
      ablak = ifelse(Date > as.Date("2020-11-25"), 1, 0),
      day_in_wint = ifelse(day_in_year < 213, 
                           day_in_year + 365 - 213,
                           day_in_year - 213),
      dwint_2 = abs((day_in_wint - 182.5) / 182.5)
    ) %>%
    filter(!is.na(tsum))

  # jelleggörb. based on MVM's graph
  jelleggorb <- data.frame(
    month = 1:12,
    amount = c(
      336.0,
      287.8,
      229.9,
      114.2,
      12.9,
      1.6,
      1.6,
      1.6,
      32.2,
      147.9,
      233.1,
      334.4))
  
  
  obs_days <- read_excel( here::here(gaz_dir)
                     #, sheet = "Mero_rendetlen"
                     ,skip = 4
                     , col_names = FALSE
  ) %>%
    `colnames<-`( c("Date", "day_in_yr", "next_measurement", "Rate")) %>%
    .[,c(1,3,4)] %>%
    mutate( Date = as_datetime(Date)) %>%
    filter( is.na(Rate) == FALSE)
  
  obs_days <- left_join( meteostat_weather, obs_days, by = "Date")
  
  # this took about 3 min with a for loop :)
  # simulating temps per hour according to a simple sinus
  obs_hours <- expand.grid( hours_dat = 0:23,
                             Date = meteostat_weather$Date,
                             temp = 0) %>%
    mutate(tim = Date + hours(hours_dat),
           id = 1:n(),
           # day_in_yr = yday(Date),
           # day_in_wint = ifelse( day_in_yr < 185, 
           #                         day_in_yr + 180,
           #                         day_in_yr - 180)
    ) %>%
    left_join( ., meteostat_weather, by = "Date") %>%
    mutate( tact =  tmin + ( sin( (hours_dat-6)/12 * pi) + 1) * range / 2)
  
  obs_readings <- read_excel(here::here(gaz_dir), 
                              sheet = "Mero_rendetlen") %>%
    # 213 day in year == august 1st
    rename( Value = Mero, 
            Date = Datum, 
            Gas = Gaz, 
            Day = Nap)
  
  obs_readings <- obs_readings %>%
    mutate( Date = as_datetime(Date),
            datelag = lag(Date),
            tact = Maketsum( Date, datelag, obs_hours. = obs_hours),
            datemiddle = Date + as.duration(interval(Date , datelag))/2,
            ywint = ifelse(  yday(datemiddle) < 213, year( datemiddle), year( datemiddle)+1) %>% as.factor
            ,datenum = interval(min(Date), Date) %>% as.duration() %>% as.numeric("days")
            , heat_off = ifelse( tact > 17, "off", "on")
    ) %>%
    filter(Day!=0)
  
  obs_readings$dtmn <- obs_readings$datemiddle %>% as.numeric()
  obs_readings$dtmn <- (obs_readings$dtmn / 3600 /24 -17873) %>% round(digits=2)
  
  
  # Save the processed data
  save(meteostat_weather, obs_hours, obs_readings, obs_days, data, jelleggorb,
       file = here::here(output_file))
}



# merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
#                          gaz_dir  = "inst/extdata/gaz.xlsx",
#                          output_file = "data/meteostat_data.Rdata")