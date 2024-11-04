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
merge_transform_weather <- function(data_dir, gaz_dir, output_file,
                                    act_year = 2023) {

  get_avg_temp <- function( df = obs_days, var = "tavg", var_time = "Date",
                            xmin, xmax) {
    f <- approxfun( df[var_time][[1]], df[var][[1]])
    integrated <- NA
    try(silent = FALSE, {
      integrated <- integrate( f, subdivisions = 100000,
                               #stop.on.error = FALSE,
                               rel.tol = 0.1,
                               lower = xmin, upper = xmax)$value
    })
    
    duration <- difftime( xmax, xmin, units = "secs")
    return( integrated / as.numeric(duration))
  }
  
  
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
    # omitting superfuous predictors
    dplyr::select(!(c("prcp","snow","wdir","wspd","wpgt","pres","tsun"))) %>%
    ungroup %>%
    rename(Date = date) %>% 
    mutate(
      Date = as_datetime(Date),
      range = tmax - tmin,
      day_in_year = yday(Date),
      year = year(Date),
      ywint = as.factor(year(Date - 213 * 3600 * 24)),
      ablak = ifelse(Date > as.Date("2020-11-25"), 1, 0),
      day_in_wint = ifelse(day_in_year < 213, 
                           day_in_year + 365 - 213,
                           day_in_year - 213)) %>%
    filter(!is.na(tmin),!is.na(tmax),!is.na(tavg))

  # jelleggörb. based on MVM's graph
  jelleggorb <- data.frame(
    month = 1:12,
    amount = c(
      336.0,  287.8,  229.9,
      114.2,  12.9,   1.6,
      1.6,    1.6,    32.2,
      147.9,  233.1,  334.4))
  
  yday_august1st <- function(year.) {
    x <- as_date("2000-08-01")
    year(x) <- year.
    return(yday(x))
  }
  
  obs_readings <- read_excel(here::here(gaz_dir), 
                             sheet = "Mero_rendetlen") %>%
    # 213 day in year == august 1st
    rename( Value = Mero, 
            Date = Datum, 
            Gas = Gaz, 
            Day = Nap) %>%
    mutate( Date = as_datetime(Date),
            datelag = lag(Date),
            datemiddle = Date + as.duration(interval(Date , datelag))/2,
            ywint = ifelse(  yday(datemiddle) < yday_august1st(year(Date)), 
                             year( datemiddle)-1, 
                             year( datemiddle)) %>% as.factor
            ,datenum = interval(min(Date), Date) %>% 
              as.duration() %>% 
              as.numeric("days")
    ) %>%
    filter(Day!=0) 
  
  obs_readings$Value_trf <- 0
  for (i in 1:nrow(obs_readings)) {
    if (i == 1) {
      delta   <- - obs_readings$Value[1]
    }  else if ( obs_readings$Value[i] == 0 ) {
      # Counter restarts due to meter change
      delta <- obs_readings$Value_trf[i-1]
    }
    
    obs_readings$Value_trf[i] <- obs_readings$Value[i] + delta
  }

  # helper functions
  get_approx_meter <-  approxfun(obs_readings$Date,
                                 obs_readings$Value_trf, 
                                 rule = 2, na.rm = TRUE)
  
  get_approx_rate <- function(x) {
    # dx get_approx_meter
    h <- 1e-6
    return((get_approx_meter(x + h) - get_approx_meter(x)) / h)
  }
  
    
  obs_days <- meteostat_weather %>%
    mutate(Meter = get_approx_meter(Date),
           Rate = get_approx_rate(Date)*3600*24) %>%
    group_by(ywint) %>%
    mutate( Spent = Meter - min(Meter, na.rm = TRUE),
            Spent_perc = ifelse( year(Date) == act_year,
                                 Spent / 1730,
                                 Spent / max(Spent, na.rm = TRUE)))
    
  
  # this took about 3 min with a for loop :)
  # simulating temps per hour according to a simple sinus
  obs_hours <- expand.grid( hours_dat = 0:23,
                             Date = meteostat_weather$Date,
                             temp = 0) %>%
    mutate(tim = Date + hours(hours_dat),
           id = 1:n()) %>%
    left_join( ., meteostat_weather, by = "Date") %>%
    mutate( tavg =  tmin + ( sin( (hours_dat-6)/12 * pi) + 1) * range / 2,
            Date = tim) %>%
    select(!(c(tim))) %>%
    mutate(Meter = get_approx_meter(Date),
           Rate = get_approx_rate(Date)*3600*24) %>%
    group_by(ywint) %>%
    mutate( Spent = Meter - min(Meter, na.rm = TRUE),
            Spent_perc = ifelse( year(Date) == act_year,
                                 Spent / 1730,
                                 Spent / max(Spent, na.rm = TRUE)))
  
  obs_readings <- obs_readings %>%
    rowwise() %>%
    mutate(tavg_obs = get_avg_temp(xmin = datelag,
                                   xmax = Date,
                                   df = obs_hours, var = "tavg", 
                                   var_time = "Date"),
           heat_off = ifelse( tavg_obs > 17, "off", "on"))
  

  # Save the processed data
  save(meteostat_weather, obs_hours, obs_readings, 
       obs_days, data, jelleggorb,
       get_approx_meter, get_approx_rate, get_avg_temp,
       file = here::here(output_file))
}


# 
# merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
#                          gaz_dir  = "inst/extdata/gaz.xlsx",
#                          output_file = "data/meteostat_data.Rdata")