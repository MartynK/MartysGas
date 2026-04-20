# add_and_rerun.r -- Append a meter reading and rerun pipeline
#
# Reads inst/extdata/_add_reading.txt (line 1 = meter, line 2 = datetime),
# appends to gaz.xlsx, then reruns iter1 through iter5.
#
# Called by the add-meter skill. Not intended for interactive use.
library(openxlsx)
library(readxl)
library(dplyr)
library(here)
library(lubridate)

GAZ_PATH <- here::here("inst", "extdata", "gaz.xlsx")
TXT_PATH <- here::here("inst", "extdata", "_add_reading.txt")

# ── 1. Read the specification file ──
if (!file.exists(TXT_PATH)) {
  stop("Missing ", TXT_PATH, " — write meter value and datetime first.")
}

lines <- readLines(TXT_PATH, warn = FALSE)
lines <- trimws(lines[nzchar(lines)])

if (length(lines) < 1) {
  stop("_add_reading.txt is empty.")
}

new_mero <- as.numeric(lines[1])
if (is.na(new_mero)) {
  stop("Could not parse meter value: '", lines[1], "'")
}

# Datetime: use line 2 if present, otherwise Sys.time()
if (length(lines) >= 2) {
  new_date <- as.POSIXct(lines[2])
  if (is.na(new_date)) {
    stop("Could not parse datetime: '", lines[2], "'")
  }
} else {
  new_date <- Sys.time()
}

# ── 2. Load current data and compute derived columns ──
d <- read_excel(GAZ_PATH, sheet = "Mero_rendetlen")
last_row <- d[nrow(d), ]

new_gaz  <- new_mero - last_row$Mero
new_nap  <- as.numeric(difftime(new_date, last_row$Datum,
                                 units = "days"))
new_rate <- new_gaz / new_nap

if (new_gaz < 0) {
  warning("Negative gas delta (", round(new_gaz, 1),
          " m3) — meter value is LOWER than previous. ",
          "Check for typo or meter replacement.")
}

new_row <- data.frame(
  Mero  = new_mero,
  Datum = new_date,
  Gaz   = new_gaz,
  Nap   = new_nap,
  Rate  = new_rate
)

cat("=== New reading ===\n")
cat("  Meter: ", new_mero, " m3\n")
cat("  Date:  ", format(new_date, "%Y-%m-%d %H:%M"), "\n")
cat("  Delta: ", round(new_gaz, 1), " m3 over ",
    round(new_nap, 1), " days\n")
cat("  Rate:  ", round(new_rate, 2), " m3/day\n\n")

# ── 3. Append and save ──
d_new <- bind_rows(d, new_row)

wb <- createWorkbook()
addWorksheet(wb, "Mero_rendetlen")
writeData(wb, "Mero_rendetlen", d_new)
saveWorkbook(wb, GAZ_PATH, overwrite = TRUE)
cat("Saved gaz.xlsx (", nrow(d_new), " rows)\n\n")

# ── 4. Clean up the specification file ──
file.remove(TXT_PATH)

# ── 5. Rerun pipeline ──
# Source all R/ functions first (needed by merge_transform_weather)
source_all_files <- function(directory) {
  file_paths <- list.files(directory,
                           pattern = "\\.[rR]$",
                           full.names = TRUE)
  for (fp in file_paths) source(fp)
}
source_all_files(here::here("R"))

cat("=== Running pipeline ===\n")
cat("iter1 (data prep)...\n")
source(here::here("inst", "iter1_data_prep.r"))

cat("\niter2 (consumption curves)...\n")
source(here::here("inst", "iter2_consumption_curves.r"))

cat("\niter3 (cumulative heat model)...\n")
source(here::here("inst", "iter3_cumulative_heat_model.r"))

cat("\niter4 (conditional prediction)...\n")
source(here::here("inst", "iter4_conditional_prediction.r"))

cat("\niter5 (decision tool)...\n")
source(here::here("inst", "iter5_decision_tool.r"))

cat("\n=== Pipeline complete ===\n")
