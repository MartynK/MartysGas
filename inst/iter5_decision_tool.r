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
# Hot-water (non-heating) baseline, m3/day. iter4 estimates
# this from the data and falls back to 0.4 when the estimate
# is unreliable; reuse exactly that value so the decision tool
# and the efficiency chart share one definition.
HOTWATER_PER_DAY <- if (exists("hotwater_baseline") &&
                        !is.na(hotwater_baseline)) {
  hotwater_baseline
} else {
  0.4
}


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

# Budget anchor: the meter value at Jan 1 of the current
# calendar year (the tracking window is Jan-Dec).
#
# NOTE: a previous version anchored to "the meter reading
# nearest Jan 5". When no early-January reading exists (the
# usual case -- e.g. the first 2026 reading is Feb 2), that
# silently anchored to a much later reading and DROPPED all
# of January's consumption, overstating the remaining budget
# by hundreds of m3. We instead interpolate the meter at
# Jan 1 with get_approx_meter(), which is exactly how the
# burndown's `Spent` is defined -- keeping the headline
# numbers and the burndown chart consistent.
year_start_meter <- as.POSIXct(paste0(current_year, "-01-01"))
meter_now       <- dat_latest_reading$Value_trf
meter_at_jan    <- get_approx_meter(year_start_meter)
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

# Heating efficiency: gas spent on HEATING (total gas minus
# the per-day hot-water baseline) per degree*day of heating
# need. Hot water is handled by the separate HOTWATER_PER_DAY
# baseline (data-driven in iter4, 0.4 fallback), NOT bundled
# into this ratio -- so the forecast keeps using hot water
# through the summer when heating need is ~0. Uses the same
# basis as the iter4 efficiency chart's spent_tavg.
if (cum_heat_now > 0 && !is.na(dat_current$Spent)) {
  gas_heating_spent <- dat_current$Spent -
    HOTWATER_PER_DAY * day_in_year_obs
  efficiency_ratio <- gas_heating_spent / cum_heat_now
} else {
  # Fallback: historical average ~0.53 m3 per degree*day
  efficiency_ratio <- 0.53
}

# Expected gas need for the rest of the year = heating gas
# (efficiency x remaining heat) + hot water for the remaining
# days.
gas_need_lower <- remaining_heat_lower * efficiency_ratio +
  HOTWATER_PER_DAY * days_remaining
gas_need_upper <- remaining_heat_upper * efficiency_ratio +
  HOTWATER_PER_DAY * days_remaining

# Suggested daily rate to stay within budget
daily_rate_suggested_lower <- gas_need_lower / days_remaining
daily_rate_suggested_upper <- gas_need_upper / days_remaining

# Current daily rate.
# NOTE: obs_days_complete only spans up to the last meter
# reading, so a "last 7 calendar days" window is empty
# whenever today is >7 days after the latest reading (which
# is the normal case) and yields NaN. Instead use the rate
# implied by the most recent meter interval, which is the
# best estimate of the household's current consumption.
current_daily_rate <- dat_latest_reading$Rate

# Fallback: if the latest reading has no rate, average the
# last 7 days of complete observations that actually exist.
if (length(current_daily_rate) == 0 ||
    is.na(current_daily_rate)) {
  last_obs_date <- max(obs_days_complete$Date, na.rm = TRUE)
  dat_recent <- obs_days_complete %>%
    ungroup() %>%
    filter(Date >= last_obs_date - days(7)) %>%
    summarise(rate_7d = mean(Rate, na.rm = TRUE))
  current_daily_rate <- dat_recent$rate_7d
}


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

# Recent temperature for heat pump note.
# Use meteostat_weather (runs to today), NOT obs_days_complete
# which stops at the last meter reading and would yield NaN.
recent_tavg <- meteostat_weather %>%
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
cat("  Current (latest interval): ",
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

# Anchor the forecast at exactly the headline budget figure
# (gas_budget_left), so the chart's "today" point and the
# printed "Gas remaining" never disagree. Both are built on
# the same Jan-1 meter anchor (see section 3).
gas_remaining_now <- gas_budget_left

# Predicted burn-down.
#
# IMPORTANT: use the SAME conditional model that drives the
# verdict above -- predict_future_needs() -- so the chart, the
# printed recommendation, and the hand-coded efficiency chart
# all agree. (A previous version computed a separate
# conditional-normal band here using an empirical rho matrix;
# it drifted from both the verdict and the efficiency chart,
# producing an over-optimistic upper tail that touched the
# quota line when neither of the other two views did.)
#
# predict_future_needs() returns, for every day from today to
# Dec 31, the conditional 95% band of CUMULATIVE heating need
# (lower_expected / upper_expected), shrunk toward this year's
# observed trajectory. We convert each to gas remaining with
# the same efficiency_ratio + hot-water baseline used for the
# verdict. More heat needed -> more gas burned -> less budget
# left, so the heat upper bound maps to the gas LOWER bound.

# predict_future_needs() gives a valid 95% band only at the
# END of the season (day 365) -- that is all the verdict uses.
# For intermediate days its prop_var term can go negative and
# the band inverts, so we do NOT use it to draw the trajectory.
#
# Instead we draw a band that is guaranteed self-consistent:
#   - The MEAN line follows the seasonal shape of how heat
#     actually accrues (flat in summer, steep in winter), taken
#     from the unconditional mean curve mean_fun().
#   - The band WIDTH grows in proportion to the heat accrued so
#     far, so it is ~0 today and reaches exactly the verdict's
#     year-end CI on Dec 31. This makes the fan narrow in summer
#     (little heating, little to be uncertain about) and widen
#     through autumn/winter (winter severity is the unknown).

future_days   <- seq(day_in_year_obs, 365)
days_from_now <- future_days - day_in_year_obs

# Expected ADDITIONAL heat need accrued from today to day d
# (unconditional seasonal shape). At day 365 this equals the
# conditional remaining-need mean used by the verdict.
heat_accrued    <- mean_fun(future_days) -
  mean_fun(day_in_year_obs)
heat_accrued    <- pmax(heat_accrued, 0)
heat_accrued_eos <- heat_accrued[length(heat_accrued)]
heat_frac        <- if (heat_accrued_eos > 0) {
  heat_accrued / heat_accrued_eos
} else {
  rep(0, length(heat_accrued))
}

# Mean gas used by day d = heating gas (efficiency x heat
# accrued) + hot water (baseline x days elapsed). The hot
# water term keeps the burn-down sloping down gently through
# the summer even while heating need is ~0.
gas_used_mean <- efficiency_ratio * heat_accrued +
  HOTWATER_PER_DAY * days_from_now

# Year-end half-width from the verdict's conditional CI on
# remaining heat, scaled back along the season by heat_frac.
half_width_eos <- efficiency_ratio *
  (remaining_heat_upper - remaining_heat_lower) / 2
band_half <- half_width_eos * heat_frac

dat_forecast <- data.frame(
  day_in_year       = future_days,
  gas_forecast_mean = gas_remaining_now - gas_used_mean,
  # more heat -> more gas -> less budget left (lower bound)
  gas_forecast_lwr  = gas_remaining_now - gas_used_mean -
    band_half,
  gas_forecast_upr  = gas_remaining_now - gas_used_mean +
    band_half
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

# X-axis breaks: 1st of every second month (Jan, Mar, ..., Nov)
# plus the last day of the year, so both Jan and Dec are
# labelled and the quota deadline (Dec 31) is marked.
axis_breaks <- c(
  make_date(current_year, c(1, 3, 5, 7, 9, 11), 1),
  make_date(current_year, 12, 31)
)
axis_labels <- c(
  format(make_date(current_year, c(1, 3, 5, 7, 9, 11), 1), "%b"),
  "Dec 31"
)

# Probability of overshooting the quota at year-end:
# P(gas_forecast_end < 0).
end_fc <- dat_forecast[nrow(dat_forecast), ]
end_sd <- (end_fc$gas_forecast_upr - end_fc$gas_forecast_mean) /
  CONF_LEV
prob_overshoot <- if (end_sd > 0) {
  round(100 * pnorm(0, mean = end_fc$gas_forecast_mean,
                    sd = end_sd, lower.tail = TRUE))
} else {
  ifelse(end_fc$gas_forecast_mean < 0, 100, 0)
}

# Subtitle carries the verdict + the numbers that matter
burndown_subtitle <- sprintf(
  paste0("As of %s: %s m3 used, %s m3 left. ",
         "Projected year-end: %s m3 (range %s to %s). ",
         "Chance of overshooting quota: ~%s%%."),
  format(today_date, "%b %d"),
  round(gas_consumed), round(gas_budget_left),
  round(end_fc$gas_forecast_mean),
  round(end_fc$gas_forecast_lwr),
  round(end_fc$gas_forecast_upr),
  prob_overshoot
)

# A small label anchored at "today" on the observed line
dat_today_label <- data.frame(
  plot_date = anchor_date,
  y         = gas_remaining_now,
  lab       = paste0("Today: ", round(gas_remaining_now),
                     " m3 left")
)

fig_burndown <-
  ggplot() +
  theme_minimal(base_size = 13) +
  # Penalty zone: below the quota line (gas_remaining < 0)
  annotate("rect",
    xmin = make_date(current_year, 1, 1),
    xmax = make_date(current_year, 12, 31),
    ymin = -200, ymax = 0,
    fill = "red", alpha = 0.06
  ) +
  # Forecast ribbon: the "uncertain future" (which winter we get)
  geom_ribbon(
    data = dat_forecast_plot,
    aes(x = plot_date,
        ymin = gas_forecast_lwr, ymax = gas_forecast_upr),
    fill = "#d1495b", alpha = 0.18
  ) +
  # Forecast mean line
  geom_line(
    data = dat_forecast_plot,
    aes(x = plot_date, y = gas_forecast_mean),
    color = "#d1495b", linetype = "dashed", linewidth = 0.9
  ) +
  # Observed gas remaining: the "certain past"
  geom_line(
    data = dat_burndown_plot,
    aes(x = plot_date, y = gas_remaining),
    color = "#13315c", linewidth = 1.6
  ) +
  # Quota-exhausted line
  geom_hline(yintercept = 0, color = "red", linewidth = 0.6) +
  # "Today" marker + point + label
  geom_vline(
    xintercept = anchor_date,
    color = "grey55", linetype = "dotted"
  ) +
  geom_point(
    data = dat_today_label,
    aes(x = plot_date, y = y),
    color = "#13315c", size = 2.6
  ) +
  geom_label(
    data = dat_today_label,
    aes(x = plot_date, y = y, label = lab),
    hjust = 1.05, vjust = -0.4, size = 3.4,
    label.size = 0, fill = "white", color = "#13315c"
  ) +
  scale_x_date(
    breaks = axis_breaks,
    labels = axis_labels,
    limits = make_date(current_year, c(1, 12), c(1, 31)),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  scale_y_continuous(
    limits = c(-200, QUOTA_M3),
    breaks = seq(0, QUOTA_M3, by = 250)
  ) +
  labs(
    x = "",
    y = "Gas budget remaining (m³)",
    title = paste0("Gas budget burn-down \u2014 verdict: ",
                   decision),
    subtitle = burndown_subtitle,
    caption = paste0(
      "Solid navy = actual consumption (certain).  ",
      "Dashed red + shaded band = forecast for the rest of ",
      "the year;\nwider band = more winter uncertainty.  ",
      "Below the red line = over quota (penalty price)."
    )
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey25", size = 10),
    plot.caption = element_text(hjust = 0, color = "grey45"),
    panel.grid.minor = element_blank()
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
  recent_tavg       = recent_tavg,
  prob_overshoot    = prob_overshoot,
  current_year      = current_year,
  days_elapsed      = days_elapsed,
  days_remaining    = days_remaining,
  budget_surplus    = c(
    pessimistic = budget_surplus_pessimistic,
    optimistic  = budget_surplus_optimistic
  ),
  forecast_year_end = c(
    mean  = end_fc$gas_forecast_mean,
    lower = end_fc$gas_forecast_lwr,
    upper = end_fc$gas_forecast_upr
  )
)

save(
  decision_snapshot,
  fig_burndown,
  dat_burndown_plot,
  dat_forecast_plot,
  file = here::here("data", "decision_snapshot.Rdata")
)

message("Saved: data/decision_snapshot.Rdata")
