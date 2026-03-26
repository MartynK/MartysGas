# ------------------------------------------------------------
# iter3_consumption_curves.r -- Gas consumption curve modeling
#
# Rewrite of iter6_mods.r using the consolidated data pipeline.
# Models the relationship between temperature and gas
# consumption rate, using both daily-average and hourly-
# simulated temperatures.
#
# Inputs : meteostat_data.Rdata (obs_hours, obs_readings,
#          obs_days, jelleggorb, get_approx_meter,
#          get_approx_rate)
# Outputs: data/consumption_curves.Rdata
# Runtime: ~10 s
# ------------------------------------------------------------

source(here::here("inst", "function", "load_stuff.r"))


# ============================================================
# 1. Prepare data
# ============================================================

# obs_hours replaces the old temps_xtra object.
# Column mapping:
#   temps_xtra$temp  -> obs_hours$tavg  (hourly simulated temp)
#   temps_xtra$tact  -> must be computed via Maketsum()
#
# obs_readings replaces the old gaz_rendetlen object.

# -- Compute tact (average temperature over the meter-reading
#    interval) for obs_readings, the same way old_iter2 did it
dat_readings <- obs_readings %>%
  ungroup() %>%
  mutate(
    tact = Maketsum(Date, datelag, obs_hours. = obs_hours)
  ) %>%
  filter(
    !is.na(tact),
    !is.infinite(tact),
    !is.na(ywint)
  )

# -- For obs_hours the "tact" equivalent is just tavg.
#    We rename it to tact so downstream models are consistent.
dat_hours <- obs_hours %>%
  ungroup() %>%
  rename(tact = tavg)

# -- Quick budget calculation (gas consumed vs. quota)
#    using most recent quota year's readings
USED_UP <- last(
  dat_readings$Value[nrow(dat_readings)] -
    dat_readings$Value[dat_readings$Date < "2023-08-01 0:00"]
)
KEDV_HATRA <- 1730 - USED_UP


# ============================================================
# 2. Exploratory figures: consumption vs temperature
# ============================================================

# fig_1: GAM curves of daily-avg temp vs gas rate, by ywint
fig_1 <- dat_hours %>%
  filter(!is.na(Rate)) %>%
  mutate(ywint = forcats::fct_drop(ywint)) %>%
  ggplot(aes(
    x = tact,
    y = Rate,
    fill = ywint,
    color = ywint
  )) +
  theme_bw() +
  geom_point(alpha = 0.05) +
  geom_smooth(
    se = FALSE, size = 2,
    method = "gam",
    formula = y ~ s(x, bs = "tp")
  ) +
  labs(
    x = "Hourly simulated temperature (C)",
    y = "Gas rate (m3/day)",
    title = "Consumption vs temperature (hourly)"
  )

# fig_2: Same but filtered to tact < 20 (heating days only)
fig_2 <- dat_hours %>%
  filter(!is.na(Rate)) %>%
  mutate(ywint = droplevels(ywint)) %>%
  filter(tact < 20) %>%
  ggplot(aes(
    x = tact,
    y = Rate,
    fill = as.factor(ywint),
    color = as.factor(ywint)
  )) +
  theme_bw() +
  geom_point(alpha = 0.05) +
  geom_smooth(
    se = FALSE, size = 2,
    method = "gam",
    formula = y ~ s(x, bs = "tp")
  ) +
  labs(
    x = "Hourly simulated temperature (C)",
    y = "Gas rate (m3/day)",
    title = "Consumption vs temperature (heating days)"
  )


# ============================================================
# 3. Filter outliers for modeling
# ============================================================

# Remove the winter we went to Australia (2019 ywint, warm
# days only kept) and cap at 50 m3/day rate as outlier guard
dat_hours_nice <- dat_hours %>%
  filter(
    ywint != "2019" | tact > 10
  ) %>%
  filter(Rate < 50 | is.na(Rate)) %>%
  mutate(
    heat_off = ifelse(
      day_in_year > 258 | day_in_year < 135,
      "on", "off"
    )
  )


# ============================================================
# 4. Fit consumption model: Rate ~ tact * spline(day_in_wint)
# ============================================================

# This interaction lets the temperature-consumption slope
# vary across the season (steeper in mid-winter)
mod_consumption <- lm(
  Rate ~ tact * ns(day_in_wint, df = 3),
  data = dat_hours_nice
)

# Diagnostic checks (uncomment as needed):
# summary(mod_consumption)
# plot(mod_consumption)
# acf(resid(mod_consumption), lag = 1500)


# ============================================================
# 5. Temperature model: predict tact from hour + day_in_wint
# ============================================================

# "Silly idea" (per original): we added the sinus for hourly
# variation, now we smooth it back out to get an average
# seasonal temperature curve
mod_temp <- lm(
  tact ~ I(sin(hours_dat / 24 * pi)) +
    ns(day_in_wint, df = 5),
  data = dat_hours
)


# ============================================================
# 6. Build prediction grid
# ============================================================

# 24 hours x 365 days = 8760 rows
dat_preds <- expand.grid(
  hours_dat   = 0:23,
  day_in_year = 1:365,
  id          = max(dat_hours_nice$id)
) %>%
  mutate(
    heat_off = ifelse(
      day_in_year > 258 | day_in_year < 135,
      "on", "off"
    ),
    date_good = yday_inverse(day_in_year),
    # day_in_wint: same transformation as in
    # merge_transform_weather()
    day_in_wint = ifelse(
      day_in_year < 213,
      day_in_year + 365 - 213,
      day_in_year - 213
    )
  ) %>%
  # Shift dates so the "year" starts in August
  mutate(
    date_good = ifelse(
      date_good < as.Date("2022-08-01"),
      date_good + years(1),
      date_good
    ) %>% as_date(),
    # Alternative parameterization: distance from mid-winter
    dwint_2 = abs((day_in_wint - 182.5) / 182.5)
  ) %>%
  group_by(hours_dat, day_in_year) %>%
  slice(1) %>%
  ungroup()

# -- Predict temperatures on the grid
dat_preds <- dat_preds %>%
  cbind(
    .,
    predict(
      mod_temp,
      newdata = dat_preds,
      interval = "prediction"
    ) %>%
      `colnames<-`(c("tact", "tact_min", "tact_max"))
  )

# -- Predict gas consumption on the grid
dat_preds$gas <- predict(mod_consumption, newdata = dat_preds)

# During non-heating season the model is unreliable;
# override with baseline hot-water-only rate
dat_preds$gas[dat_preds$heat_off == "off"] <- 0.6

# Scale so total yearly consumption matches 2500 m3
# (typical pre-efficiency total)
correction <- 2500 / sum(dat_preds$gas / 24)
dat_preds$gas_corr <- dat_preds$gas * correction


# ============================================================
# 7. Alternative "no autocorrelation" model
# ============================================================

# Uses the irregular obs_readings (one row per meter reading)
# so each observation is truly independent
mod_b <- lm(

  Rate ~ ns(tact, df = 1) + ywint,
  data = dat_readings
)

# Predictions from the simpler model
dat_nd <- expand.grid(
  tact  = seq(0, 20, 0.1),
  ywint = dat_readings$ywint[nrow(dat_readings)]
)
dat_nd$pred <- predict(mod_b, newdata = dat_nd)


# ============================================================
# 8. Save outputs
# ============================================================

save(
  mod_consumption, mod_temp, mod_b,
  dat_preds, dat_nd,
  dat_hours, dat_hours_nice, dat_readings,
  fig_1, fig_2,
  USED_UP, KEDV_HATRA,
  file = here::here("data", "consumption_curves.Rdata")
)

message("Saved: data/consumption_curves.Rdata")
