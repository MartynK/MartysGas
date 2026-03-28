# ------------------------------------------------------------
# iter1_data_prep.r -- Data preparation entry point
#
# Run this script when:
#   - New weather Excel files arrive in
#     inst/extdata/meteostat_data/
#   - Gas meter readings are updated in inst/extdata/gaz.xlsx
#   - You want to pull fresh data via meteostat_query_daily()
#
# Produces: data/meteostat_data.Rdata
#   Contains: meteostat_weather, obs_hours, obs_readings,
#             obs_days, obs_days_complete,
#             get_approx_meter, get_approx_rate, get_avg_temp
# ------------------------------------------------------------

library(here)
library(dplyr)
library(lubridate)

# Source package functions (but NOT load_stuff.r, since we are
# rebuilding the data it loads)
source_all_files <- function(directory) {
  file_paths <- list.files(directory,
                           pattern = "\\.[rR]$",
                           full.names = TRUE)
  for (file_path in file_paths) {
    source(file_path)
  }
}
source_all_files(here::here("R"))

# --- Auto-query fresh weather data if DB is stale ---
# Check the latest date in existing Excel files. If it
# doesn't cover yesterday, pull fresh data from Meteostat.
existing_files <- list.files(
  here::here("inst", "extdata", "meteostat_data"),
  pattern = "\\.xlsx$", full.names = TRUE
)

if (length(existing_files) > 0) {
  # Peek at the most recently modified file's max date
  newest_file <- existing_files[
    which.max(file.mtime(existing_files))
  ]
  latest_date <- tryCatch({
    tmp <- readxl::read_excel(newest_file)
    max(as.Date(tmp[[1]]), na.rm = TRUE)
  }, error = function(e) as.Date(NA))
} else {
  latest_date <- as.Date(NA)
}

# Query if we're missing more than 2 days
if (is.na(latest_date) ||
    latest_date < (Sys.Date() - 2)) {
  message(
    "Weather data ends at ",
    ifelse(is.na(latest_date), "(none)", as.character(latest_date)),
    ". Querying Meteostat for fresh data..."
  )
  # Pull from the day after latest_date to today
  start_from <- if (is.na(latest_date)) {
    Sys.Date() - 365
  } else {
    latest_date
  }
  days_to_pull <- as.numeric(Sys.Date() - start_from)
  tryCatch({
    meteostat_query_daily(
      station   = "12843",
      end_date  = Sys.Date(),
      days_back = max(days_to_pull, 30),
      overwrite = TRUE
    )
    message("Fresh weather data saved.")
  }, error = function(e) {
    warning(
      "Meteostat query failed: ", e$message,
      "\nProceeding with existing data."
    )
  })
} else {
  message(
    "Weather data is current (latest: ",
    latest_date, "). Skipping API query."
  )
}

# --- Run the main data pipeline ---
# Reads all Excel weather files + gaz.xlsx, merges, transforms,
# interpolates missing dates, and saves to
# data/meteostat_data.Rdata
message("Running merge_transform_weather()...")
t_start <- Sys.time()

merge_transform_weather(
  data_dir    = "inst/extdata/meteostat_data",
  gaz_dir     = "inst/extdata/gaz.xlsx",
  output_file = "data/meteostat_data.Rdata",
  act_year    = year(Sys.Date())
)

t_elapsed <- difftime(Sys.time(), t_start, units = "secs")
message(
  "Pipeline completed in ",
  round(t_elapsed, 1), " seconds."
)

# --- Verify output ---
load(here::here("data", "meteostat_data.Rdata"))

message("\n=== Data Summary ===")
message(
  "Weather records: ", nrow(meteostat_weather),
  " (", min(meteostat_weather$Date),
  " to ", max(meteostat_weather$Date), ")"
)
message("Meter readings:  ", nrow(obs_readings))
message("Daily obs:       ", nrow(obs_days))
message("Complete daily:  ", nrow(obs_days_complete))
message("Hourly obs:      ", nrow(obs_hours))
latest_obs <- obs_readings %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Date) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::pull(Date)
message("Latest reading:  ", format(latest_obs, "%Y-%m-%d"))
