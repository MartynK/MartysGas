# co1_events.r
# Carbon monoxide logger (Lascar EL-USB-CO, bathroom next to the Bosch Therm
# 4200 open-flue water heater): stack the EasyLog exports, separate events
# from the slow baseline, characterise each event (peak, duration, dose,
# decay rate), and estimate the COHb an adult would have reached had they
# stayed in the room for the whole event (CFK model, worst case).
#
# Produces:
#   data/co_events.Rdata          dat_co_min, dat_events, dat_base_day, dat_cohb
#   inst/sideffects/co_*.png      timeline, the two major events, baseline
#   inst/sideffects/co_events.csv the event table
#
# Single-zone reading of the decay: CO is inert on these time scales, so the
# post-peak fall of the excess over baseline is exp(-k t) with k the air
# exchange rate of the room the logger sits in. That only holds if the
# source has stopped and the room is roughly well mixed; both are
# assumptions, stated again in the output.

source(here::here("inst", "function", "load_stuff.r"))

EVENT_PPM     <- 10   # a minute whose max reaches this is an event minute
MERGE_GAP_MIN <- 10   # event minutes closer than this belong to one event
DECAY_MIN_EXC <- 3    # fit the decay only while excess over baseline > this
MAJOR_PPM     <- 300  # the two incidents Marton confirmed as real

dir_sfx <- here::here("inst", "sideffects")
dir.create(dir_sfx, showWarnings = FALSE)

# 1. Data, on a one-minute grid ------------------------------------------
# Files run at 10, 30 or 60 s. One-minute means smooth the 10 s files, the
# per-minute max keeps their peaks; peak comparisons across files are
# therefore only fair after the like-for-like check in section 4.
dat_co <- read_co_logger(here::here("inst", "extdata", "co_logger"))

dat_co_min <- dat_co %>%
  filter(!startup) %>%
  mutate(minute = lubridate::floor_date(time, "minute")) %>%
  group_by(minute, file, interval_s) %>%
  summarise(co = mean(co), co_max = max(co), .groups = "drop") %>%
  mutate(hour = lubridate::hour(minute), day = as.Date(minute))

# 2. Events ---------------------------------------------------------------
vec_ev_min <- dat_co_min$minute[dat_co_min$co_max >= EVENT_PPM]
gap <- c(Inf, diff(as.numeric(vec_ev_min)) / 60)
dat_ev_id <- tibble(minute = vec_ev_min,
                    event = cumsum(gap > MERGE_GAP_MIN | is.na(gap)))

# baseline before each event: median of the quiet hour preceding it
get_pre_base <- function(t0) {
  x <- dat_co_min$co[dat_co_min$minute < t0 &
                     dat_co_min$minute >= t0 - 3600 &
                     dat_co_min$co_max < EVENT_PPM]
  if (length(x) < 10) return(0)
  median(x)
}

# decay fit: log excess on hours after the peak, until the excess drops
# under DECAY_MIN_EXC or the curve turns up again (a second burst)
fit_decay <- function(seg, base, t_peak) {
  post <- seg %>% filter(minute >= t_peak) %>%
    mutate(exc = co - base, t_h = as.numeric(difftime(minute, t_peak,
                                                      units = "hours")))
  stop_at <- which(post$exc <= DECAY_MIN_EXC)[1]
  if (!is.na(stop_at)) post <- post[seq_len(stop_at - 1), ]
  rerise <- which(post$exc > 1.2 * cummin(post$exc) & post$exc > 5)[1]
  if (!is.na(rerise)) post <- post[seq_len(rerise - 1), ]
  if (nrow(post) < 4) return(c(k = NA, k_lo = NA, k_hi = NA, n = nrow(post)))
  mod <- lm(log(exc) ~ t_h, data = post)
  ci <- -rev(confint(mod)["t_h", ])
  c(k = -unname(coef(mod)["t_h"]), k_lo = ci[[1]], k_hi = ci[[2]],
    n = nrow(post))
}

lst_ev <- list()
for (e in unique(dat_ev_id$event)) {
  t_in <- range(dat_ev_id$minute[dat_ev_id$event == e])
  base <- get_pre_base(t_in[1])
  # extend to where the trace leaves and rejoins baseline (max 3 h after)
  seg <- dat_co_min %>% filter(minute >= t_in[1] - 1800,
                               minute <= t_in[2] + 3 * 3600)
  above <- seg$co > base + 1
  i_pk <- which.max(seg$co_max)
  i_start <- max(c(1, which(!above[seq_len(i_pk)]) + 1))
  after <- which(!above & seq_along(above) > i_pk)
  i_end <- if (length(after)) after[1] else nrow(seg)
  seg <- seg[i_start:i_end, ]
  t_peak <- seg$minute[which.max(seg$co_max)]
  dec <- fit_decay(seg, base, t_peak)

  lst_ev[[e]] <- tibble(
    event = e, file = seg$file[1], interval_s = seg$interval_s[1],
    start = seg$minute[1], peak_time = t_peak,
    peak_ppm = max(seg$co_max), base_ppm = base,
    min_to_peak = as.numeric(difftime(t_peak, seg$minute[1], units = "mins")),
    min_ge10 = sum(seg$co_max >= 10), min_ge35 = sum(seg$co_max >= 35),
    min_ge100 = sum(seg$co_max >= 100),
    min_total = nrow(seg),
    dose_ppm_min = sum(pmax(seg$co - base, 0)),
    k_per_h = dec[["k"]], k_lo = dec[["k_lo"]], k_hi = dec[["k_hi"]],
    n_decay = dec[["n"]],
    single_sample = sum(seg$co_max >= EVENT_PPM) == 1
  )
}
# events that share a peak after extension are one event: keep the widest
dat_events <- bind_rows(lst_ev) %>%
  arrange(peak_time, desc(min_total)) %>%
  distinct(peak_time, .keep_all = TRUE) %>%
  mutate(event = row_number(),
         half_life_min = 60 * log(2) / k_per_h,
         major = peak_ppm >= MAJOR_PPM,
         hour = lubridate::hour(peak_time),
         wday = lubridate::wday(peak_time, label = TRUE, week_start = 1))

source(here::here("inst", "co1b_cohb_and_figures.r"))
