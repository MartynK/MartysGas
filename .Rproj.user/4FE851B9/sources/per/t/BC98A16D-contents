library(dplyr)
library(ggplot2)

source_all_files <- function(directory) {
  file_paths <- list.files(directory, pattern = "\\.[rR]$", full.names = TRUE)
  
  for (file_path in file_paths) {
    source(file_path)
  }
}

source_all_files(here::here("R"))

merge_transform_weather( data_dir = "inst/extdata/meteostat_data",
                         gaz_dir  = "inst/extdata/gaz.xlsx",
                         output_file = "data/meteostat_data.Rdata")

load( here::here("data","meteostat_data.Rdata")) 
load_all_Rdata(directory=here::here("inst","function","backend")) # Load slow suff's output

#source( here::here( "inst", "function", "wrangling.r"))


