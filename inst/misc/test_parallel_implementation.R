# Test script for parallel implementation and error fixes
# This tests the updated merge_transform_weather() function

library(here)

# Source the updated function
source(here::here("R", "transform_meteostat_weather.r"))

cat(strrep("=", 70), "\n")
cat("Testing merge_transform_weather() with parallel processing\n")
cat(strrep("=", 70), "\n\n")

# Run the function and time it
cat("Running merge_transform_weather()...\n\n")

start_time <- Sys.time()

tryCatch({
  merge_transform_weather(
    data_dir = "inst/extdata/meteostat_data",
    gaz_dir  = "inst/extdata/gaz.xlsx",
    output_file = "data/meteostat_data_test.Rdata"
  )

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "secs")

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SUCCESS! Function completed without errors\n")
  cat("Elapsed time:", round(elapsed, 2), "seconds\n")
  cat(strrep("=", 70), "\n\n")

  # Load the results and verify
  load(here::here("data", "meteostat_data_test.Rdata"))

  cat("Verification:\n")
  cat("- meteostat_weather rows:", nrow(meteostat_weather), "\n")
  cat("- obs_hours rows:", nrow(obs_hours), "\n")
  cat("- obs_readings rows:", nrow(obs_readings), "\n")
  cat("- obs_days rows:", nrow(obs_days), "\n\n")

  cat("Checking first obs_readings$tavg_obs (should be NA):\n")
  cat("- tavg_obs[1]:", obs_readings$tavg_obs[1], "\n")
  cat("- tavg_obs[2]:", obs_readings$tavg_obs[2], "\n\n")

  cat("Test completed successfully!\n")

}, error = function(e) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("ERROR occurred:\n")
  cat(e$message, "\n")
  cat(strrep("=", 70), "\n")
})
