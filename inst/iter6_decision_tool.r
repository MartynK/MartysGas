# ------------------------------------------------------------
# iter6_decision_tool.r -- Gas consumption decision tool
#
# Quick-run script for daily/weekly check:
# "Should I amp up, dial down, or hold steady on gas usage?"
#
# Hungarian residential gas billing context:
#   QUOTA_M3     = 1729    # Annual subsidized volume (~63,645 MJ)
#   PRICE_CHEAP  = 102     # HUF/m3 below quota
#   PRICE_PENALTY = 767    # HUF/m3 above quota (~7.5x multiplier)
#   Quota year: Aug 1 - Jul 31
#   Meter reading: ~Jan 5 (atalany true-up)
#   Daily quota: ~4.74 m3/day (flat, pro-rata)
#   Heat pumps preferred above 10 C outdoor temp
#
# Inputs : data/cumulative_heat_predictions.Rdata
# Outputs: data/decision_snapshot.Rdata + console message
# Runtime: <10 s
# ------------------------------------------------------------

source(here::here("inst", "function", "load_stuff.r"))
load(here::here(
  "data", "cumulative_heat_predictions.Rdata"
))


# ============================================================
# 1. Billing constants
# ============================================================

QUOTA_M3         <- 1729   # Annual subsidized gas (m3)
PRICE_CHEAP      <- 102    # HUF/m3 below quota
PRICE_PENALTY    <- 767    # HUF/m3 above quota
HP_THRESHOLD     <- 10     # Outdoor C above which heat pump wins
QUOTA_START_MONTH <- 8     # August
QUOTA_START_DAY   <- 1
HOTWATER_PER_DAY  <- 0.4   # m3/day baseline for hot water


# ============================================================
# 2. Determine current calendar year boundaries
# ============================================================
#
# Tracking on a Jan-Jan basis because the meter reading
# is ~Jan 5 (atalany true-up) and the user does NOT
# dictate an August reading, so the practical budget
# window aligns with the calendar year.

today_date <- Sys.time()
current_year <- year(today_date)

year_start <- as.POSIXct(paste0(current_year, "-01-01"))
year_end   <- as.POSIXct(paste0(current_year, "-12-31"))

days_elapsed   <- as.numeric(difftime(
  today_date, year_start, units = "days"
))
days_remaining <- as.numeric(difftime(
  year_end, today_date, units = "days"
))


# ============================================================
# 3. Current meter state from obs_readings
# ============================================================

# Latest meter reading
dat_latest_reading <- obs_readings %>%
  ungroup() %>%
  arrange(Date) %>%
  slice_tail(n = 1)

# Meter value near Jan 5 of current year (the atalany anchor)
dat_jan_reading <- obs_readings %>%
  ungroup() %>%
  filter(year(Date) == current_year) %>%
  mutate(err_jan = abs(yday(Date) - 5)) %>%
  filter(err_jan == min(err_jan)) %>%
  slice(1)

meter_now       <- dat_latest_reading$Value_trf
meter_at_jan    <- dat_jan_reading$Value_trf
gas_consumed    <- meter_now - meter_at_jan
gas_budget_left <- QUOTA_M3 - gas_consumed


# ============================================================
# 4. Current position in calendar year
# ============================================================

day_in_year_now <- yday(today_date)

# Get the latest row from obs_days_complete in the current year
dat_current <- obs_days_complete %>%
  ungroup() %>%
  filter(
    year == current_year,
    Date <= today_date,
    !is.na(tavg_low_cumul)
  ) %>%
  arrange(desc(Date)) %>%
  slice(1)

cum_heat_now    <- dat_current$tavg_low_cumul
day_in_year_obs <- dat_current$day_in_year


# ============================================================
# 5. Conditional prediction: expected remaining heat need
# ============================================================

# Use the predict_future_needs() function from iter5
# full = FALSE gives us only the end-of-season estimate
dat_future <- predict_future_needs(
  conf_lev        = CONF_LEV,
  day_in_year_act = day_in_year_obs,
  cum_t_act       = cum_heat_now,
  sds.            = sds,
  full            = TRUE
)

# End-of-year expected cumulative heat need
end_of_season <- dat_future %>%
  filter(day_in_year == 365)

remaining_heat_lower <- end_of_season$lower_expected -
  cum_heat_now
remaining_heat_upper <- end_of_season$upper_expected -
  cum_heat_now

# Clamp negatives (if season is almost over)
remaining_heat_lower <- max(remaining_heat_lower, 0)
remaining_heat_upper <- max(remaining_heat_upper, 0)


# ============================================================
# 6. Translate heat need to gas need
# ============================================================

# Efficiency ratio: how many m3 of gas per degree*day of
# heating need, estimated from current season data
if (cum_heat_now > 0 && !is.na(dat_current$Spent)) {
  gas_heating_spent <- dat_current$Spent -
    HOTWATER_PER_DAY * day_in_year_obs
  efficiency_ratio <- gas_heating_spent / cum_heat_now
} else {
  # Fallback: historical average ~0.53 m3 per degree*day
  efficiency_ratio <- 0.53
}

# Expected gas remaining for heating
gas_need_lower <- remaining_heat_lower * efficiency_ratio +
  HOTWATER_PER_DAY * days_remaining
gas_need_upper <- remaining_heat_upper * efficiency_ratio +
  HOTWATER_PER_DAY * days_remaining

# Suggested daily rate to stay within budget
daily_rate_suggested_lower <- gas_need_lower / days_remaining
daily_rate_suggested_upper <- gas_need_upper / days_remaining

# Current daily rate (averaged over last 7 days)
dat_recent <- obs_days_complete %>%
  ungroup() %>%
  filter(Date >= today_date - days(7)) %>%
  summarise(rate_7d = mean(Rate, na.rm = TRUE))
current_daily_rate <- dat_recent$rate_7d


# ============================================================
# 7. Decision logic
# ============================================================

# Budget surplus against the WORST-CASE (upper) gas need.
# Positive = we have breathing room even if winter is harsh.
# Negative = we risk exceeding the quota.
budget_surplus_pessimistic <- gas_budget_left - gas_need_upper
budget_surplus_optimistic  <- gas_budget_left - gas_need_lower

# Decision thresholds (m3) — based on pessimistic scenario
if (budget_surplus_pessimistic > 100) {
  decision <- "AMP UP"
  decision_detail <- paste0(
    "Even in a harsh scenario you'd have ~",
    round(budget_surplus_pessimistic),
    " m3 to spare. Burn freely."
  )
} else if (budget_surplus_pessimistic < -50) {
  decision <- "DIAL DOWN"
  decision_detail <- paste0(
    "In a harsh scenario you'd overshoot by ~",
    round(abs(budget_surplus_pessimistic)),
    " m3. Cut back or use the heat pump."
  )
} else {
  decision <- "HOLD STEADY"
  decision_detail <- paste0(
    "Budget is tight (pessimistic surplus ~",
    round(budget_surplus_pessimistic),
    " m3, optimistic ~",
    round(budget_surplus_optimistic),
    " m3). Stay the course."
  )
}

# Recent temperature for heat pump note
recent_tavg <- obs_days_complete %>%
  ungroup() %>%
  filter(Date >= today_date - days(3)) %>%
  summarise(tavg_3d = mean(tavg, na.rm = TRUE)) %>%
  pull(tavg_3d)

hp_note <- if (!is.na(recent_tavg) &&
               recent_tavg > HP_THRESHOLD) {
  paste0(
    "  ** Recent 3-day avg temp: ",
    round(recent_tavg, 1),
    " C (above ", HP_THRESHOLD,
    " C). Heat pump is the better choice. **"
  )
} else {
  ""
}


# ============================================================
# 8. Print formatted recommendation
# ============================================================

cat("\n")
cat("==============================================\n")
cat("  GAS BUDGET DECISION TOOL\n")
cat("  ", format(today_date, "%Y-%m-%d %H:%M"), "\n")
cat("==============================================\n")
cat("\n")
cat("BUDGET STATUS:\n")
cat("  Tracking year: ",
    format(year_start, "%Y-%m-%d"), " to ",
    format(year_end, "%Y-%m-%d"), "\n")
cat("  Days elapsed:  ", round(days_elapsed),
    " / ", round(days_elapsed + days_remaining), "\n")
cat("  Gas consumed:  ", round(gas_consumed, 1),
    " m3 of ", QUOTA_M3, " m3 quota\n")
cat("  Gas remaining: ", round(gas_budget_left, 1), " m3\n")
cat("\n")
cat("EXPECTED REMAINING NEED (95% CI):\n")
cat("  Heat need left:  ",
    round(remaining_heat_lower), " - ",
    round(remaining_heat_upper), " C*days\n")
cat("  Gas need left:   ",
    round(gas_need_lower, 1), " - ",
    round(gas_need_upper, 1), " m3\n")
cat("  Efficiency:      ",
    round(efficiency_ratio, 3),
    " m3 per C*day\n")
cat("\n")
cat("DAILY RATE:\n")
cat("  Current (7-day): ",
    round(current_daily_rate, 2), " m3/day\n")
cat("  Suggested range: ",
    round(daily_rate_suggested_lower, 2), " - ",
    round(daily_rate_suggested_upper, 2), " m3/day\n")
cat("\n")
cat("----------------------------------------------\n")
cat("  DECISION:  >>> ", decision, " <<<\n")
cat("  ", decision_detail, "\n")
if (nchar(hp_note) > 0) {
  cat("\n")
  cat(hp_note, "\n")
}
cat("----------------------------------------------\n")
cat("\n")


# ============================================================
# 9. Budget burn-down plot with forecast bands
# ============================================================

# Prepare the current year's trajectory
dat_burndown <- obs_days_complete %>%
  ungroup() %>%
  filter(year == current_year) %>%
  mutate(gas_remaining = QUOTA_M3 - Spent)

# Anchor: gas remaining at the last observed day
gas_remaining_now <- dat_burndown %>%
  filter(day_in_year == day_in_year_obs) %>%
  slice(1) %>%
  pull(gas_remaining)

# If no exact match, fall back to gas_budget_left
if (length(gas_remaining_now) == 0 || is.na(gas_remaining_now)) {
  gas_remaining_now <- gas_budget_left
}

# Predicted burn-down: proper Monte Carlo from weather_simulated
#
# Each of the 100 simulated years gives a trajectory of
# daily heat need from today → Dec 31.  Cumulative gas use
# per trajectory grows monotonically, so the spread of
# trajectories naturally widens over time.  We take quantiles
# across trajectories at each future day.

# weather_simulated has: day_in_year, year_sim, pred_tavg
# Daily heat need = max(0, 20 - pred_tavg)
HEAT_THRESHOLD <- 20
weather_simulated$daily_heat <- pmax(
  0, HEAT_THRESHOLD - weather_simulated$pred_tavg
)

# For each simulated year, extract days from today's yday
# to day 365, then cumulate the heat -> gas trajectory
future_days <- seq(day_in_year_obs, 365)
n_future <- length(future_days)

sim_years <- unique(weather_simulated$year_sim)
# Pre-allocate a matrix: rows = future days, cols = sim years
gas_remaining_matrix <- matrix(
  NA_real_,
  nrow = n_future,
  ncol = length(sim_years)
)

for (i in seq_along(sim_years)) {
  sy <- sim_years[i]
  sim_dat <- weather_simulated[
    weather_simulated$year_sim == sy, ]

  # Get daily heat for the remaining portion of this sim year
  sim_future <- sim_dat[
    sim_dat$day_in_year %in% future_days, ]

  if (nrow(sim_future) < n_future) next

  # Order by day and compute cumulative gas consumption
  sim_future <- sim_future[order(sim_future$day_in_year), ]
  cum_gas_use <- cumsum(
    sim_future$daily_heat * efficiency_ratio +
      HOTWATER_PER_DAY
  )
  gas_remaining_matrix[, i] <- gas_remaining_now - cum_gas_use
}

# Compute quantiles across simulated trajectories
# (each row = one future day, quantile across columns)
# CONF_LEV is a z-score (e.g. 1.96), convert to probability
ALPHA <- pnorm(-CONF_LEV)  # e.g. 0.025 for z=1.96
dat_forecast <- data.frame(
  day_in_year       = future_days,
  gas_forecast_lwr  = apply(
    gas_remaining_matrix, 1,
    quantile, probs = ALPHA, na.rm = TRUE),
  gas_forecast_upr  = apply(
    gas_remaining_matrix, 1,
    quantile, probs = 1 - ALPHA, na.rm = TRUE),
  gas_forecast_mean = apply(
    gas_remaining_matrix, 1,
    mean, na.rm = TRUE)
)

# Convert day_in_year to actual Date using lubridate
dat_burndown <- dat_burndown %>%
  mutate(
    plot_date = make_date(current_year, 1, 1) +
      days(day_in_year - 1)
  )

dat_forecast <- dat_forecast %>%
  mutate(
    plot_date = make_date(current_year, 1, 1) +
      days(day_in_year - 1)
  )

# Trim observed line to end at the last observed day
dat_burndown_plot <- dat_burndown %>%
  filter(day_in_year <= day_in_year_obs)

# Anchor: forecast ribbon starts where observed line ends
anchor_date <- make_date(current_year, 1, 1) +
  days(day_in_year_obs - 1)
dat_anchor <- data.frame(
  plot_date         = anchor_date,
  gas_forecast_lwr  = gas_remaining_now,
  gas_forecast_upr  = gas_remaining_now,
  gas_forecast_mean = gas_remaining_now
)
dat_forecast_plot <- bind_rows(dat_anchor, dat_forecast)

# Quarter break dates for x-axis (lubridate)
quarter_dates <- make_date(
  current_year, c(1, 4, 7, 10), 1
)

fig_burndown <-
  ggplot() +
  theme_bw() +
  # Observed gas remaining (ends at today)
  geom_line(
    data = dat_burndown_plot,
    aes(x = plot_date, y = gas_remaining),
    color = "navy", linewidth = 1.2
  ) +
  # Forecast ribbon (starts from today)
  geom_ribbon(
    data = dat_forecast_plot,
    aes(
      x = plot_date,
      ymin = gas_forecast_lwr,
      ymax = gas_forecast_upr
    ),
    fill = "salmon", alpha = 0.3
  ) +
  # Forecast mean line (same extent as ribbon)
  geom_line(
    data = dat_forecast_plot,
    aes(x = plot_date, y = gas_forecast_mean),
    color = "red", linetype = "dashed", linewidth = 1
  ) +
  # Zero line (budget exhausted)
  geom_hline(
    yintercept = 0,
    color = "red", linetype = "dashed", linewidth = 0.5
  ) +
  # Current day marker
  geom_vline(
    xintercept = anchor_date,
    color = "grey50", linetype = "dotted"
  ) +
  scale_x_date(
    breaks = quarter_dates,
    date_labels = "%b",
    limits = make_date(
      current_year, c(1, 12), c(1, 31)
    )
  ) +
  scale_y_continuous(
    limits = c(-200, QUOTA_M3),
    breaks = seq(0, QUOTA_M3, by = 250)
  ) +
  labs(
    x = "",
    y = "Gas budget remaining (m³)",
    title = "Budget burn-down with forecast (Jan\u2013Dec)"
  )


# ============================================================
# 10. Save snapshot
# ============================================================

decision_snapshot <- list(
  timestamp         = today_date,
  gas_consumed      = gas_consumed,
  gas_budget_left   = gas_budget_left,
  remaining_heat    = c(
    lower = remaining_heat_lower,
    upper = remaining_heat_upper
  ),
  gas_need          = c(
    lower = gas_need_lower,
    upper = gas_need_upper
  ),
  efficiency_ratio  = efficiency_ratio,
  current_daily_rate = current_daily_rate,
  daily_rate_suggested = c(
    lower = daily_rate_suggested_lower,
    upper = daily_rate_suggested_upper
  ),
  decision          = decision,
  decision_detail   = decision_detail,
  recent_tavg       = recent_tavg
)

save(
  decision_snapshot,
  fig_burndown,
  file = here::here("data", "decision_snapshot.Rdata")
)

message("Saved: data/decision_snapshot.Rdata")
