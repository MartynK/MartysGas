#' Load all .Rdata or .rdata files in a directory
#'
#' This function scans a specified directory for all files with .Rdata or .rdata
#' extensions and loads them into the R environment.
#'
#' @param directory A character string of the directory path to scan for .Rdata or .rdata files.
#'
#' @examples
#' load_all_Rdata("path/to/your/directory")
#'
#' @export
load_all_Rdata <- function(directory) {
  # List all .Rdata or .rdata files in the directory
  file_paths <- list.files(directory, pattern = "\\.Rdata$|\\.rdata$", full.names = TRUE)
  
  # Load each file
  for (file_path in file_paths) {
    load(file_path, envir = .GlobalEnv)
  }
}

load_all_Rdata(directory=here::here("inst","example_quarto","backend")) # Load slow suff's output