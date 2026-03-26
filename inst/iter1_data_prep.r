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
#             obs_days, obs_days_complete, jelleggorb,
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

# --- Optional: pull fresh weather data from Meteostat API ---
# Uncomment to query the latest daily weather data.
# Requires API key in inst/extdata/secrets/meteostat_api_key.txt
#
# meteostat_query_daily(
#   station   = "12843",
#   end_date  = Sys.Date(),
#   days_back = 365
# )

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
message(
  "Latest reading:  ",
  obs_readings %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::pull(Date)
)
