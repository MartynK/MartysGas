# co1b_cohb_and_figures.r
# Second half of co1_events.r (sourced from it; needs dat_co, dat_co_min,
# dat_events in the session).
#   3. COHb by the Coburn-Forster-Kane equation, worst case: an adult at
#      rest who stayed beside the logger for the whole event
#   4. Like-for-like event counts: the July files ran at 10 to 30 s, the
#      autumn ones at 60 s, so July is thinned to 60 s instantaneous samples
#   5. Daily baseline outside events
#   6. Figures, tables, save

# 3. COHb, CFK (Peterson and Stewart parameterisation) ---------------------
# Parameters are textbook adult values, not Marton's; with them the
# equilibration time constant is about six hours, so a 30 to 60 minute
# event is far from steady state and the CFK curve, not ppm x a factor, is
# the right reading.
CFK <- list(
  pb = 760, pl = 713,         # barometric and dry alveolar pressure, mmHg
  va = 6000,                  # alveolar ventilation, mL/min, at rest
  dl = 30,                    # CO diffusing capacity, mL/min/mmHg
  vb = 5500,                  # blood volume, mL
  m = 218,                    # Haldane constant
  pco2 = 100,                 # pulmonary capillary O2 tension, mmHg
  vco = 0.007,                # endogenous CO production, mL/min
  hb_cap = 1.38 * 0.15,       # mL CO (or O2) per mL blood at Hb 15 g/dL
  cohb0 = 0.5                 # starting COHb, %, non-smoker
)

simulate_cohb <- function(ppm, p = CFK, dt = 1) {
  b <- 1 / p$dl + p$pl / p$va
  cohb <- p$cohb0 / 100 * p$hb_cap
  out <- numeric(length(ppm))
  for (i in seq_along(ppm)) {
  pico <- ppm[i] * 1e-6 * p$pl
    o2hb <- p$hb_cap - cohb
    d <- p$vco / p$vb + pico / (b * p$vb) -
      cohb * p$pco2 / (p$m * b * p$vb * o2hb)
    cohb <- cohb + d * dt
    out[i] <- 100 * cohb / p$hb_cap
  }
  out
}

# light activity (showering, cleaning): alveolar ventilation about 15 L/min
CFK_ACTIVE <- modifyList(CFK, list(va = 15000))

lst_cohb <- list()
for (i in seq_len(nrow(dat_events))) {
  ev <- dat_events[i, ]
  seg <- dat_co_min %>%
    filter(minute >= ev$start,
           minute <= ev$start + (ev$min_total + 60) * 60)
  # exposure is the logged trace for the event, then clean air: a person
  # who stayed for the event and left when it had passed
  ppm <- ifelse(seg$minute < ev$start + ev$min_total * 60, seg$co, 0)
  lst_cohb[[i]] <- tibble(event = ev$event, minute = seg$minute, ppm = ppm,
                          cohb = simulate_cohb(ppm),
                          cohb_active = simulate_cohb(ppm, CFK_ACTIVE))
}
dat_cohb <- bind_rows(lst_cohb)
dat_events <- dat_events %>%
  left_join(dat_cohb %>% group_by(event) %>%
              summarise(cohb_peak = max(cohb),
                        cohb_peak_active = max(cohb_active)), by = "event")

# 4. Like-for-like counts, thinned to 60 s ---------------------------------
count_events <- function(d) {
  t <- d$time[d$co >= EVENT_PPM]
  if (!length(t)) return(0L)
  sum(c(Inf, diff(as.numeric(t)) / 60) > MERGE_GAP_MIN)
}
dat_period <- dat_co %>%
  filter(!startup) %>%
  mutate(period = ifelse(time < as.POSIXct("2026-09-01", tz = "UTC"),
                         "2026-07/08", "2026-09/10")) %>%
  group_by(period) %>%
  group_modify(function(d, k) {
    d60 <- d %>% filter(as.integer(format(time, "%S")) <
                          ifelse(interval_s < 60, interval_s, 60))
    d60 <- d60[!duplicated(lubridate::floor_date(d60$time, "minute")), ]
    days <- as.numeric(sum(diff(as.numeric(d$time))[
      diff(as.numeric(d$time)) < 3600])) / 86400
    tibble(days = days, events_native = count_events(d),
           events_60s = count_events(d60))
  }) %>%
  ungroup() %>%
  mutate(per_day_native = events_native / days,
         per_day_60s = events_60s / days)

# 5. Daily baseline outside events ----------------------------------------
dat_base_day <- dat_co_min %>%
  filter(co_max < EVENT_PPM) %>%
  group_by(day) %>%
  summarise(mean = mean(co), night = mean(co[hour %in% 1:5]),
            p_pos = mean(co > 0), n = n(), .groups = "drop") %>%
  filter(n >= 360) %>%
  mutate(run = cumsum(c(1, diff(day) > 1)))   # so lines break at the gap

# 6. Figures and output ---------------------------------------------------
fig_timeline <- ggplot(dat_co_min, aes(minute, co_max)) +
  geom_line(linewidth = 0.2, colour = "grey30") +
  geom_point(data = dat_events, aes(peak_time, peak_ppm, colour = major),
             size = 1.5) +
  geom_hline(yintercept = c(35, 100), linetype = 2, colour = "grey50") +
  scale_y_continuous(trans = "log1p", breaks = c(0, 2, 10, 35, 100, 300, 900)) +
  scale_colour_manual(values = c(`FALSE` = "darkorange", `TRUE` = "red3"),
                      labels = c("event", "major incident"), name = NULL) +
  labs(x = NULL, y = "CO, ppm (per-minute max, log1p scale)",
       title = "Bathroom CO logger, all exports",
       subtitle = "Gap 2026-08-05 to 2026-09-17: battery failure") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(dir_sfx, "co_timeline.png"), fig_timeline,
       width = 10, height = 4.5, dpi = 150)

dat_major <- dat_cohb %>% semi_join(dat_events %>% filter(major), "event") %>%
  left_join(dat_events %>% select(event, peak_time), by = "event") %>%
  mutate(label = format(peak_time, "%Y-%m-%d"),
         t_min = as.numeric(difftime(minute, min(minute), units = "mins")),
         .by = event)
fig_major <- ggplot(dat_major, aes(t_min)) +
  geom_line(aes(y = ppm), colour = "grey20") +
  geom_line(aes(y = cohb * 50), colour = "red3") +
  facet_wrap(~label) +
  scale_y_continuous("CO, ppm",
                     sec.axis = sec_axis(~ . / 50, name = "COHb, % (CFK, worst case)")) +
  labs(x = "minutes from event start",
       title = "The two incidents: logged CO and modelled COHb",
       subtitle = "COHb assumes an adult at rest beside the logger throughout") +
  theme_bw() +
  theme(axis.title.y.right = element_text(colour = "red3"))
ggsave(file.path(dir_sfx, "co_major_events.png"), fig_major,
       width = 10, height = 4.5, dpi = 150)

fig_base <- ggplot(dat_base_day, aes(day, group = run)) +
  geom_line(aes(y = mean, colour = "all quiet minutes")) +
  geom_line(aes(y = night, colour = "01:00 to 05:59")) +
  geom_vline(xintercept = as.Date(dat_events$peak_time[dat_events$major]),
             linetype = 2, colour = "red3") +
  labs(x = NULL, y = "CO, ppm, daily mean outside events", colour = NULL,
       title = "Baseline outside events",
       subtitle = "Logger resolution 0.5 ppm; dashed lines are the two incidents") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(dir_sfx, "co_baseline.png"), fig_base,
       width = 10, height = 4, dpi = 150)

write.csv(dat_events, file.path(dir_sfx, "co_events.csv"), row.names = FALSE)
save(dat_co_min, dat_events, dat_base_day, dat_cohb, dat_period, CFK,
     file = here::here("data", "co_events.Rdata"))

print(dat_period)
print(as.data.frame(dat_events %>%
  transmute(peak_time, peak_ppm, base_ppm = round(base_ppm, 1), min_total,
            min_ge35, min_ge100, dose = round(dose_ppm_min),
            k = round(k_per_h, 1), t_half = round(half_life_min),
            cohb = round(cohb_peak, 1), cohb_act = round(cohb_peak_active, 1),
            single_sample)), row.names = FALSE, width = 200)
