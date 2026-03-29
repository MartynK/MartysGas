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
                                    act_year = 2025) {
  
  # Load required libraries
  library(dplyr)
  library(readxl)
  library(lubridate)
  library(here)
  library(foreach)
  library(doParallel)

  # Setup parallel backend
  n_cores <- parallel::detectCores()
  n_workers <- min(6, max(1, n_cores - 2))  # Default 6, max ncores()-2
  cl <- makeCluster(n_workers)
  registerDoParallel(cl)
  on.exit(stopCluster(cl), add = TRUE)  # Ensure cleanup

  get_avg_temp <- function(xmin, xmax, f = temp_fun) {
    integrated <- NA_real_
    try({
      integrated <- integrate(
        f,
        lower = xmin,
        upper = xmax
        # let subdivisions & rel.tol default; they're usually fine
      )$value
    }, silent = TRUE)
    
    duration <- difftime(xmax, xmin, units = "secs")
    return(integrated / as.numeric(duration))
  }
  
  # List all files in the data directory
  fil <- here::here(data_dir) %>% list.files
  
  # Check if the directory is empty
  if (length(fil) == 0) {
    stop("Data directory is empty.")
  } else {
    
    # Loop through files and merge them in parallel
    meteostat_weather <- foreach(i = seq_along(fil),
                                 .combine = 'rbind',
                                 .packages = c('readxl', 'here')) %dopar% {
      file_path <- here::here(data_dir, fil[i])
      temp_df <- read_excel(file_path)

      # Standardize first column name to "date"
      if (names(temp_df)[1] != "date") {
        names(temp_df)[1] <- "date"
      }

      temp_df
    }

    message("Loaded ", nrow(meteostat_weather), " weather records from ",
            length(fil), " files using ", n_workers, " workers")
  } 
  
  # Data transformations
  # Calendar-year basis: year + yday (lubridate handles leap years)
  meteostat_weather <- meteostat_weather %>%
    group_by(date) %>%
    slice_tail(n = 1) %>%
    # omitting superfluous predictors
    dplyr::select(!(c("prcp","snow","wdir","wspd","wpgt","pres","tsun"))) %>%
    ungroup %>%
    rename(Date = date) %>%
    # Ensure Date is unique before approxfun uses it
    distinct(Date, .keep_all = TRUE) %>%
    mutate(
      Date = as_datetime(Date),
      range = tmax - tmin,
      day_in_year = yday(Date),
      year = year(Date),
      ablak = ifelse(Date > as.Date("2020-11-25"), 1, 0)
    ) %>%
    filter(!is.na(tmin),!is.na(tmax),!is.na(tavg))

  obs_readings <- read_excel(here::here(gaz_dir),
                             sheet = "Mero_rendetlen") %>%
    rename( Value = Mero,
            Date = Datum,
            Gas = Gaz,
            Day = Nap) %>%
    mutate( Date = as_datetime(Date),
            datelag = lag(Date),
            datemiddle = Date + as.duration(interval(Date , datelag))/2,
            year = year(datemiddle),
            datenum = interval(min(Date), Date) %>%
              as.duration() %>%
              as.numeric("days")
    ) %>%
    filter(Day != 0, Value > 0)
  
  # extract last date to truncate result
  last_reading <- obs_readings %>% slice_tail(n=1) %>% pull(Date)
  
  # Build a monotonic cumulative meter series (Value_trf).
  # Detect meter changes by a large DROP in Value (>500 m3).
  # When that happens, carry forward the previous cumulative
  # total and add the new meter's reading on top.
  obs_readings$Value_trf <- 0
  for (i in 1:nrow(obs_readings)) {
    if (i == 1) {
      delta <- -obs_readings$Value[1]
    } else if (obs_readings$Value[i] < obs_readings$Value[i - 1] - 500) {
      # Meter replaced — large drop in raw Value
      delta <- obs_readings$Value_trf[i - 1]
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
    group_by(year) %>%
    mutate( Spent = Meter - min(Meter, na.rm = TRUE),
            Spent_perc = ifelse( year(Date) == act_year,
                                 Spent / 1730,
                                 Spent / max(Spent, na.rm = TRUE))) %>%
    # crop data after last reading
    filter( Date < last_reading)

  # ---- Complete-date interpolation ----
  # obs_days may have gaps (missing calendar days) because
  # meteostat data can skip dates.  We build a gapless
  # daily sequence and fill in NAs via linear interpolation
  # so downstream cumulative / rolling calculations work.
  complete_dates <- data.frame(
    Date = seq(from = min(obs_days$Date),
               to   = max(obs_days$Date),
               by   = "day")
  )

  # Left-join keeps every calendar day; missing rows get NA
  obs_days_complete <- left_join(
    complete_dates, obs_days, by = "Date"
  )

  # Column-by-column linear interpolation of NAs
  for (i in 2:ncol(obs_days_complete)) {

    dat_act <- obs_days_complete$Date
    val <- obs_days_complete[, i]

    # Need at least 2 non-NA points for interpolation
    valid_idx <- !is.na(val)
    if (sum(valid_idx) > 1) {
      fun. <- approxfun(
        x = dat_act[valid_idx],
        y = val[valid_idx],
        rule = 2
      )
      # Only overwrite the NAs; keep original values intact
      obs_days_complete[, i] <- ifelse(
        is.na(val), fun.(dat_act), val
      )
    }
  }

  # Restore year and day_in_year from Date (safer than interpolating them)
  obs_days_complete$year <- year(obs_days_complete$Date)
  obs_days_complete$day_in_year <- yday(obs_days_complete$Date)

  message("Interpolated ", nrow(obs_days_complete),
          " complete daily records ",
          "(", nrow(obs_days_complete) - nrow(obs_days),
          " days filled)")

  obs_hours <- NA
  # this took about 3 min with a for loop :)
  # simulating temps per hour according to a simple sinus
  obs_hours <- expand.grid( hours_dat = 0:23,
                             Date = meteostat_weather$Date,
                             temp = 0) %>%
    mutate(tim = Date + hours(hours_dat),
           id = 1:n()) %>%
    left_join(., meteostat_weather, by = "Date",
              relationship = "many-to-many") %>%
    mutate( tavg =  tmin + ( sin( (hours_dat-6)/12 * pi) + 1) * range / 2,
            Date = tim) %>%
    select(!(c(tim))) %>%
    mutate(Meter = get_approx_meter(Date),
           Rate = get_approx_rate(Date)*3600*24) %>%
    group_by(year) %>%
    mutate( Spent = Meter - min(Meter, na.rm = TRUE),
            Spent_perc = ifelse( year(Date) == act_year,
                                 Spent / 1730,
                                 Spent / max(Spent, na.rm = TRUE))) %>%
    # crop data after last reading
    filter( Date < last_reading)

  temp_fun <- with(obs_hours, approxfun(Date, tavg, rule = 2))
  
  obs_readings <- obs_readings %>%
    rowwise() %>%
    mutate(
      tavg_obs = if_else(
        is.na(datelag),
        NA_real_,
        get_avg_temp(datelag, Date)
      ),
      heat_off = ifelse(is.na(tavg_obs) | tavg_obs > 17, "off", "on")
    )
  

  # Save the processed data
  save(meteostat_weather, obs_hours, obs_readings,
       obs_days, obs_days_complete,
       get_approx_meter, get_approx_rate, get_avg_temp,
       file = here::here(output_file))
}



# merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
#                          gaz_dir  = "inst/extdata/gaz.xlsx",
#                          output_file = "data/meteostat_data.Rdata")