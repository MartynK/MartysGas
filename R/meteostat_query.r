# Query Meteostat Daily Weather Data (RapidAPI)

#' Query Meteostat Daily Data and Save to Excel
#'
#' Pull daily station data from the Meteostat API (RapidAPI) for a date
#' window and save it to an Excel file under inst/extdata/ by default.
#'
#' @param station Meteostat station ID (e.g., "12843" for Kispest).
#' @param end_date End date (Date or string "YYYY-MM-DD"). Default: Sys.Date().
#' @param days_back Number of days before end_date to include (default 365).
#' @param out_dir Output directory relative to project root.
#' @param file_prefix Prefix for the output filename.
#' @param api_key RapidAPI key override (optional).
#' @param api_key_path Path to a text file containing the API key.
#' @param overwrite Whether to overwrite an existing output file.
#' @return A list with the data frame and output path (invisibly).
#' @export
#'
#' @examples
#' meteostat_query_daily(
#'   station = "12843",
#'   end_date = Sys.Date(),
#'   days_back = 365
#' )
meteostat_query_daily <- function(station = "12843",
                                  end_date = Sys.Date(),
                                  days_back = 365,
                                  out_dir = "inst/extdata/meteostat_data",
                                  file_prefix = "meteostat",
                                  api_key = NULL,
                                  api_key_path = "inst/extdata/secrets/meteostat_api_key.txt",
                                  overwrite = FALSE) {
  # Dependency checks
  for (pkg in c("httr2", "jsonlite", "writexl", "here")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Missing package: ", pkg,
           ". Install it with install.packages('", pkg, "').")
    }
  }

  # Input checks
  # Load API key from file (primary source unless api_key is provided)
  if (is.null(api_key) || is.na(api_key) || !nzchar(api_key)) {
    key_path_abs <- here::here(api_key_path)
    if (file.exists(key_path_abs)) {
      key_lines <- readLines(key_path_abs, warn = FALSE)
      key_lines <- trimws(key_lines[nzchar(key_lines)])
      if (length(key_lines) > 0) {
        api_key <- key_lines[1]
      }
    }
  }
  if (is.null(api_key) || is.na(api_key) || !nzchar(api_key)) {
    stop("Missing RapidAPI key. Fill ", api_key_path, ".")
  }
  end_date <- as.Date(end_date)
  if (is.na(end_date)) {
    stop("end_date must be a Date or YYYY-MM-DD string.")
  }
  if (!is.numeric(days_back) || days_back < 1) {
    stop("days_back must be a positive number.")
  }

  start_date <- end_date - as.integer(days_back)

  req <- httr2::request("https://meteostat.p.rapidapi.com/stations/daily") |>
    httr2::req_url_query(
      station = station,
      start = format(start_date, "%Y-%m-%d"),
      end = format(end_date, "%Y-%m-%d")
    ) |>
    httr2::req_headers(
      "x-rapidapi-host" = "meteostat.p.rapidapi.com",
      "x-rapidapi-key" = api_key
    )

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop("Meteostat API request failed with status ", status, ".")
  }

  payload <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = TRUE
  )

  if (is.null(payload$data) || nrow(payload$data) == 0) {
    stop("No data returned for the requested date range.")
  }

  daily <- as.data.frame(payload$data)

  out_dir_abs <- here::here(out_dir)
  if (!dir.exists(out_dir_abs)) {
    dir.create(out_dir_abs, recursive = TRUE)
  }

  out_file <- sprintf(
    "%s_%s_%s_%s.xlsx",
    file_prefix,
    station,
    format(start_date, "%Y%m%d"),
    format(end_date, "%Y%m%d")
  )
  out_path <- file.path(out_dir_abs, out_file)

  if (file.exists(out_path) && !isTRUE(overwrite)) {
    stop("Output file exists. Set overwrite = TRUE to replace it.")
  }

  writexl::write_xlsx(daily, out_path)

  invisible(list(
    data = daily,
    path = out_path,
    start_date = start_date,
    end_date = end_date
  ))
}
