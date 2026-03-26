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
# 2. Determine current quota year boundaries
# ============================================================

today_date <- Sys.time()

# Quota year runs Aug 1 - Jul 31
# If we are before August, the quota started last year
quota_year_start <- if (month(today_date) >= QUOTA_START_MONTH) {
  as.POSIXct(paste0(
    year(today_date), "-",
    QUOTA_START_MONTH, "-",
    QUOTA_START_DAY
  ))
} else {
  as.POSIXct(paste0(
    year(today_date) - 1, "-",
    QUOTA_START_MONTH, "-",
    QUOTA_START_DAY
  ))
}

quota_year_end <- quota_year_start + years(1)

# ywint value for current quota year (matches obs_days_complete)
act_ywint <- year(quota_year_start)

days_elapsed  <- as.numeric(difftime(
  today_date, quota_year_start, units = "days"
))
days_remaining <- as.numeric(difftime(
  quota_year_end, today_date, units = "days"
))


# ============================================================
# 3. Current meter state from obs_readings
# ============================================================

# Latest meter reading
dat_latest_reading <- obs_readings %>%
  ungroup() %>%
  arrange(Date) %>%
  slice_tail(n = 1)

# Meter value at start of quota year (nearest reading)
dat_quota_start_reading <- obs_readings %>%
  ungroup() %>%
  mutate(
    err_start = abs(difftime(quota_year_start, Date))
  ) %>%
  filter(err_start == min(err_start)) %>%
  slice(1)

meter_now       <- dat_latest_reading$Value_trf
meter_at_start  <- dat_quota_start_reading$Value_trf
gas_consumed    <- meter_now - meter_at_start
gas_budget_left <- QUOTA_M3 - gas_consumed


# ============================================================
# 4. Current position in heating season (day_in_wint)
# ============================================================

# day_in_wint uses the same definition as merge_transform_weather
day_in_year_now <- yday(today_date)
day_in_wint_now <- ifelse(
  day_in_year_now < 213,
  day_in_year_now + 365 - 213,
  day_in_year_now - 213
)

# Get the latest row from obs_days_complete that we have data for
dat_current_season <- obs_days_complete %>%
  ungroup() %>%
  filter(
    Date <= today_date,
    !is.na(tavg_low_cumul)
  ) %>%
  arrange(desc(Date)) %>%
  slice(1)

cum_heat_now    <- dat_current_season$tavg_low_cumul
day_in_wint_obs <- dat_current_season$day_in_wint


# ============================================================
# 5. Conditional prediction: expected remaining heat need
# ============================================================

# Use the predict_future_needs() function from iter5
# full = FALSE gives us only the end-of-season estimate
dat_future <- predict_future_needs(
  conf_lev        = CONF_LEV,
  day_in_wint_act = day_in_wint_obs,
  cum_t_act       = cum_heat_now,
  sds.            = sds,
  full            = TRUE
)

# End-of-season expected cumulative heat need
end_of_season <- dat_future %>%
  filter(day_in_wint == 364)

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
if (cum_heat_now > 0 && !is.na(dat_current_season$Spent)) {
  gas_heating_spent <- dat_current_season$Spent -
    HOTWATER_PER_DAY * day_in_wint_obs
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

# Budget surplus: positive means we have headroom
budget_surplus <- gas_budget_left -
  (gas_need_lower + gas_need_upper) / 2

# Decision thresholds (m3)
if (budget_surplus > 100) {
  decision <- "AMP UP"
  decision_detail <- paste0(
    "You have ~", round(budget_surplus),
    " m3 of headroom. Consider using gas more freely."
  )
} else if (budget_surplus < -50) {
  decision <- "DIAL DOWN"
  decision_detail <- paste0(
    "You are ~", round(abs(budget_surplus)),
    " m3 over the expected trajectory. ",
    "Reduce usage or rely on the heat pump."
  )
} else {
  decision <- "HOLD STEADY"
  decision_detail <- paste0(
    "You are within ~", round(abs(budget_surplus)),
    " m3 of the expected trajectory. Stay the course."
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
cat("  Quota year:    ",
    format(quota_year_start, "%Y-%m-%d"), " to ",
    format(quota_year_end, "%Y-%m-%d"), "\n")
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

# Prepare the current season's trajectory
dat_burndown <- obs_days_complete %>%
  ungroup() %>%
  filter(
    as.numeric(as.character(ywint)) == act_ywint |
      ywint == as.character(act_ywint)
  ) %>%
  mutate(gas_remaining = QUOTA_M3 - Spent)

# Predicted burn-down trajectory (from conditional prediction)
dat_forecast <- dat_future %>%
  filter(!is.na(lower_expected)) %>%
  mutate(
    # Remaining heat translated to remaining gas
    remaining_heat_lwr = lower_expected - cum_heat_now,
    remaining_heat_upr = upper_expected - cum_heat_now,
    gas_forecast_lwr = gas_budget_left -
      remaining_heat_lwr * efficiency_ratio -
      HOTWATER_PER_DAY *
      (day_in_wint - day_in_wint_obs),
    gas_forecast_upr = gas_budget_left -
      remaining_heat_upr * efficiency_ratio -
      HOTWATER_PER_DAY *
      (day_in_wint - day_in_wint_obs)
  )

fig_burndown <-
  ggplot() +
  theme_bw() +
  # Observed gas remaining

  geom_line(
    data = dat_burndown,
    aes(x = day_in_wint, y = gas_remaining),
    color = "navy", linewidth = 1.2
  ) +
  # Forecast bands
  geom_ribbon(
    data = dat_forecast,
    aes(
      x = day_in_wint,
      ymin = gas_forecast_upr,
      ymax = gas_forecast_lwr
    ),
    fill = "salmon", alpha = 0.3
  ) +
  # Zero line (budget exhausted)
  geom_hline(
    yintercept = 0,
    color = "red", linetype = "dashed"
  ) +
  # Current day marker
  geom_vline(
    xintercept = day_in_wint_obs,
    color = "grey50", linetype = "dotted"
  ) +
  scale_y_continuous(
    limits = c(-200, QUOTA_M3),
    breaks = seq(0, QUOTA_M3, by = 250)
  ) +
  labs(
    x = "Day in season (starts Aug 1st)",
    y = "Gas budget remaining (m3)",
    title = "Budget burn-down with forecast"
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
