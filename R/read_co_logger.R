#' Read Lascar EL-USB-CO (EasyLog) text exports
#'
#' Reads every EasyLog `.txt` export in a folder and stacks them into one
#' long table. Times are the logger's own clock, which does not follow
#' daylight saving, so they are parsed as naive clock time (tz "UTC") to
#' avoid the ambiguous hour on the autumn changeover.
#'
#' Two export layouts exist: older ones carry a "Warning Level" column,
#' v5 does not. Only time and CO are kept, so both read the same way.
#'
#' @param dir Folder holding the EasyLog `.txt` exports.
#' @return A tibble with `file`, `time`, `co` (ppm), `interval_s` (logging
#'   interval of that file) and `startup` (TRUE for the first two minutes of
#'   each file, where handling artefacts sit), sorted by time, duplicate
#'   timestamps dropped.
#' @export
read_co_logger <- function(dir) {
  files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) stop("No EasyLog .txt exports in ", dir)

  lst_dat <- lapply(files, function(f) {
    x <- utils::read.csv(f, header = FALSE, skip = 1, fill = TRUE,
                         col.names = paste0("V", 1:7))
    out <- dplyr::tibble(
      file = basename(f),
      time = as.POSIXct(x$V2, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      co   = suppressWarnings(as.numeric(x$V3))
    )
    out <- out[!is.na(out$time) & !is.na(out$co), ]
    out$interval_s <- stats::median(diff(as.numeric(out$time)))
    out$startup <- as.numeric(difftime(out$time, min(out$time),
                                       units = "mins")) < 2
    out
  })

  dat <- dplyr::bind_rows(lst_dat)
  dat <- dat[order(dat$time), ]
  dat[!duplicated(dat$time), ]
}
