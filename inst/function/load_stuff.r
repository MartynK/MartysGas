pkgs <- c(
  "dplyr", "ggplot2", "lubridate", "nlme", "splines",
  "readxl", "readr", "ggpubr", "forecast", "quantreg",
  "lme4", "boot", "splines2", "here", "foreach", "doParallel"
)

invisible(lapply(pkgs, function(p) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE)
  )
}))

source_all_files <- function(directory) {
  file_paths <- list.files(directory, pattern = "\\.[rR]$", full.names = TRUE)
  
  for (file_path in file_paths) {
    source(file_path)
  }
}

source_all_files(here::here("R"))

# merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
#                          gaz_dir  = "inst/extdata/gaz.xlsx",
#                          output_file = "data/meteostat_data.Rdata")

# Core data: weather + gas observations + interpolated objects
load(here::here("data", "meteostat_data.Rdata"))

# NOTE: weather_models.Rdata (GLS + simulation) is NOT part
# of the core pipeline.  See inst/weathermodels/ for standalone
# weather modeling scripts if needed.
