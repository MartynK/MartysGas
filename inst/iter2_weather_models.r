# ------------------------------------------------------------
# iter2_weather_models.r -- GLS temperature models & simulation
#
# Fits two GLS models for daily temperature patterns and runs
# a 100-year Monte Carlo weather simulation.  These are the
# "backend" models used by simulate_weather() and by the
# Quarto report.
#
# Depends on: data/meteostat_data.Rdata (from iter1_data_prep.r)
# Produces:   data/weather_models.Rdata
#   Contains: mod_tavg, mod_range, weather_simulated
#
# Typical wall-clock times (8-core machine, ~11k weather rows):
#   mod_tavg   ~ 10-40 s   (AR1 on daily tavg)
#   mod_range  ~ 10-40 s   (AR1 on daily range)
#   simulation ~ 20-60 s   (36 500 days = 100 years)
# ------------------------------------------------------------

# Load everything via the central loader: packages, R/ funcs,
# and the data objects we need (meteostat_weather, etc.)
source(here::here("inst", "function", "load_stuff.r"))

# De-duplicate: overlapping Excel files can leave duplicate
# (year, day_in_year) combos which corAR1 cannot tolerate.
# Keep the last observation for each (year, day_in_year) pair.
dat_weather <- meteostat_weather %>%
  group_by(year, day_in_year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(Date)

message("Weather rows after dedup: ", nrow(dat_weather),
        " (was ", nrow(meteostat_weather), ")")

# ============================================================
# 1. GLS model for daily average temperature
# ============================================================
#
# Natural spline with 4 df captures the seasonal cycle.
# corAR1 accounts for day-to-day autocorrelation in residuals,
# grouped by calendar year so the correlation structure resets
# each January 1.
message("Fitting mod_tavg (GLS with AR1 on tavg)...")

time_tavg <- system.time({
  mod_tavg <- gls(
    tavg ~ ns(day_in_year, df = 4),
    data        = dat_weather,
    correlation = corAR1(
      value = 0.8,
      form  = ~ day_in_year | year
    )
  )
})

message(
  "  mod_tavg done in ",
  round(time_tavg["elapsed"], 1), " s  |  ",
  "AIC = ", round(AIC(mod_tavg), 1)
)

# ============================================================
# 2. GLS model for daily temperature range (tmax - tmin)
# ============================================================
#
# Same spline structure but lower starting AR1 correlation
# (0.5 vs 0.8) -- range is less autocorrelated than average.
message("Fitting mod_range (GLS with AR1 on range)...")

time_range <- system.time({
  mod_range <- gls(
    range ~ ns(day_in_year, df = 4),
    data        = dat_weather,
    correlation = corAR1(
      value = 0.5,
      form  = ~ day_in_year | year
    )
  )
})

message(
  "  mod_range done in ",
  round(time_range["elapsed"], 1), " s  |  ",
  "AIC = ", round(AIC(mod_range), 1)
)

# ============================================================
# 3. 100-year weather simulation
# ============================================================
#
# simulate_weather() uses the GLS model coefficients +
# correlated residual draws to produce a synthetic daily
# weather series.  36 500 days = 100 years.
#
# NOTE: If this step exceeds 300 seconds on your machine,
# consider reducing to 10 years (n = 3650) for a quick check:
#   weather_simulated <- simulate_weather(n = 3650)
message("Running 100-year weather simulation ",
        "(n = 36500)...")

time_sim <- system.time({
  weather_simulated <- simulate_weather(n = 36500)
})

message(
  "  Simulation done in ",
  round(time_sim["elapsed"], 1), " s  |  ",
  nrow(weather_simulated), " rows"
)

# ============================================================
# 4. Save all outputs
# ============================================================
save(
  mod_tavg,
  mod_range,
  weather_simulated,
  file = here::here("data", "weather_models.Rdata")
)
message(
  "Saved to data/weather_models.Rdata"
)

# ============================================================
# 5. Model diagnostics summary
# ============================================================
message("\n=== Model Fit Summary ===")

# -- mod_tavg diagnostics --
message("\n--- mod_tavg (daily average temperature) ---")
message(
  "  Observations: ", nobs(mod_tavg),
  "  |  Residual SE: ",
  round(mod_tavg$sigma, 3)
)
# Extract the estimated AR1 correlation parameter
phi_tavg <- coef(
  mod_tavg$modelStruct$corStruct,
  unconstrained = FALSE
)
message("  Estimated AR1 phi: ", round(phi_tavg, 3))

# -- mod_range diagnostics --
message("\n--- mod_range (daily temperature range) ---")
message(
  "  Observations: ", nobs(mod_range),
  "  |  Residual SE: ",
  round(mod_range$sigma, 3)
)
phi_range <- coef(
  mod_range$modelStruct$corStruct,
  unconstrained = FALSE
)
message("  Estimated AR1 phi: ", round(phi_range, 3))

# -- Simulation summary --
message("\n--- Weather Simulation ---")
message(
  "  Simulated days: ", nrow(weather_simulated),
  "  (~", round(nrow(weather_simulated) / 365.25),
  " years)"
)
message(
  "  tavg range: [",
  round(min(weather_simulated$tavg), 1), ", ",
  round(max(weather_simulated$tavg), 1), "]"
)

# -- Total runtime --
total_secs <- sum(
  time_tavg["elapsed"],
  time_range["elapsed"],
  time_sim["elapsed"]
)
message(
  "\nTotal elapsed: ",
  round(total_secs, 1), " seconds"
)
