# ------------------------------------------------------------
# iter4_cumulative_heat_model.r -- Cumulative heating need
#
# Refactored from iter8.r (lines 1-340) and child2.qmd.
# Computes cumulative "missing degrees" per heating season,
# fits GLS + SD models, derives z-scores, and models the
# day-wise correlation structure for conditional prediction.
#
# Inputs : meteostat_data.Rdata (obs_days_complete already
#          has interpolated gaps, Meter, Rate, Spent etc.)
# Outputs: data/cumulative_heat_models.Rdata
# Runtime: ~30 s (cumulative loop + GLS fit)
# ------------------------------------------------------------

source(here::here("inst", "function", "load_stuff.r"))


# ============================================================
# 1. Constants
# ============================================================

# Current year of analysis -- change each season
ACT_YEAR <- 2025


# ============================================================
# 2. Cap tavg at 20 C and compute cumulative heating need
# ============================================================

# obs_days_complete is already available from meteostat_data.
# It has interpolated gaps and includes Meter, Rate, Spent,
# day_in_wint, ywint etc.  We just need to add tavg_capped
# and the cumulative heating need column.

obs_days_complete <- obs_days_complete %>%
  arrange(Date) %>%
  mutate(
    # Temperatures above 20 don't contribute to heating need
    tavg_capped = ifelse(tavg < 20, tavg, 20),
    # Placeholder; filled in the loop below
    tavg_low_cumul = 0
  )

# Sequential loop: accumulate (20 - tavg_capped) within each
# heating season.  When day_in_wint resets (new season), the
# cumulative sum resets to zero.
message("Computing cumulative heating need...")
pb <- txtProgressBar(style = 3)
for (i in 2:nrow(obs_days_complete)) {
  if (obs_days_complete$day_in_wint[i] >=
      obs_days_complete$day_in_wint[i - 1]) {
    # Same season: accumulate
    obs_days_complete$tavg_low_cumul[i] <-
      obs_days_complete$tavg_low_cumul[i - 1] -
      obs_days_complete$tavg_capped[i] + 20
  } else {
    # New season: reset
    obs_days_complete$tavg_low_cumul[i] <-
      obs_days_complete$tavg_capped[i] - 20
  }
  setTxtProgressBar(pb, i / nrow(obs_days_complete))
}
close(pb)


# ============================================================
# 3. Clean edge cases
# ============================================================

# Restore ywint as factor (was numeric for interpolation)
obs_days_complete <- obs_days_complete %>%
  mutate(ywint = as.factor(ywint))

# First observed year is incomplete -- cumulative is wrong
obs_days_complete$tavg_low_cumul[
  obs_days_complete$ywint == 1
] <- NA

# Mid-winter days with near-zero cumulative are artefacts
obs_days_complete$tavg_low_cumul[
  obs_days_complete$day_in_wint > 180 &
    obs_days_complete$tavg_low_cumul < 100
] <- NA

# Interpolated fractional day_in_wint values are unreliable
obs_days_complete$tavg_low_cumul[
  obs_days_complete$day_in_wint %% 1 != 0
] <- NA

# Drop rows where cumulative heat is NA (needed for GLS)
obs_days_complete <- obs_days_complete %>%
  filter(!is.na(tavg_low_cumul))


# ============================================================
# 4. Figure: cumulative heat lines by year
# ============================================================

fig_heatneed_per_year <-
  obs_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul)) +
  theme_bw() +
  geom_line(
    alpha = 0.7,
    mapping = aes(
      color = year,
      fill = year,
      group = factor(ywint)
    )
  ) +
  labs(
    x = "Day in season (starts Aug 1st)",
    y = "Cumulative missing degrees (C * days)"
  )


# ============================================================
# 5. GLS model for mean cumulative heat need
# ============================================================

# Natural spline with 8 df captures the S-shaped seasonal
# curve.  varExp accounts for increasing variance over the
# season (more days elapsed => more spread).
# NOTE: varExp can fail to converge with some data shapes.
# We increase iterations and provide a fallback without varExp.
mod_cum <- tryCatch({
  gls(
    tavg_low_cumul ~ ns(day_in_wint, df = 8),
    data    = obs_days_complete,
    weights = varExp(form = ~day_in_wint),
    na.action = na.omit,
    control = glsControl(
      maxIter = 200, msMaxIter = 200
    )
  )
}, error = function(e) {
  warning(
    "GLS with varExp did not converge: ", e$message,
    "\nFalling back to GLS without variance weighting."
  )
  gls(
    tavg_low_cumul ~ ns(day_in_wint, df = 8),
    data      = obs_days_complete,
    na.action = na.omit
  )
})

fig_modcum_gls <- capture_plot(plot(mod_cum))


# ============================================================
# 6. Observed SDs per day and SD model
# ============================================================

# For each day_in_wint, compute the SD of cumulative heat
# across all observed years.  This gives us a "pseudo
# prediction interval" that the GLS couldn't provide cleanly.
sds <- data.frame(
  day_in_wint = 1:364,
  sd_obs = NA_real_
)

for (i in 1:nrow(sds)) {
  sds$sd_obs[i] <-
    obs_days_complete %>%
    filter(day_in_wint == sds$day_in_wint[i]) %>%
    .$tavg_low_cumul %>%
    sd(na.rm = TRUE)
}

# Fit a smooth curve through the observed SDs
# No intercept (-1) because SD should be ~0 at day 0
mod_cum_obs <- lm(
  sd_obs ~ ns(day_in_wint, df = 5) - 1,
  data = sds
)

fig_cumobs_eff <- capture_plot(
  mod_cum_obs %>%
    effects::predictorEffects(partial.residuals = TRUE) %>%
    plot()
)

# Store predicted SD and mean alongside observed
sds$sd_pred   <- predict(mod_cum_obs)
sds$mean_pred <- predict(mod_cum, newdata = sds)


# ============================================================
# 7. Helper functions: sd_fun, mean_fun, return_std_tmp
# ============================================================

# Interpolation functions for SD and mean cumulative heat
sd_fun   <- approxfun(sds$day_in_wint, sds$sd_pred)
mean_fun <- approxfun(sds$day_in_wint, sds$mean_pred)

# Given a day and observed cumulative heat, return the
# z-score and its percentile (assuming normality)
return_std_tmp <- function(day_act, tmp_act) {
  m <- mean_fun(day_act)
  s <- sd_fun(day_act)
  z <- (tmp_act - m) / s
  return(list(
    z      = z,
    z_perc = pnorm(z)
  ))
}


# ============================================================
# 8. Z-scores for every observed day
# ============================================================

obs_days_complete <- obs_days_complete %>%
  mutate(
    z = return_std_tmp(
      day_in_wint, tavg_low_cumul
    )[["z"]],
    z_perc = return_std_tmp(
      day_in_wint, tavg_low_cumul
    )[["z_perc"]]
  )


# ============================================================
# 9. Correlation model: z-score at day d vs end-of-season
# ============================================================

# For each day, compute the correlation between that day's
# z-score and the final z-score (day 364).  This tells us
# how predictive the current cumulative heat is of the
# season-total.

final_z <-
  obs_days_complete %>%
  filter(day_in_wint == 364) %>%
  mutate(id = 1:n())

sds$cor_z <- NA_real_

for (i in 1:364) {
  act_z <-
    obs_days_complete %>%
    filter(day_in_wint == i)

  # Before day 155 (roughly January), the current season's
  # end hasn't happened yet, so we correlate with the
  # *next* season's final value (id offset by 1)
  if (i < 155) {
    act_z <- act_z %>%
      mutate(id = 2:(n() + 1)) %>%
      left_join(y = final_z, by = "id")
  } else {
    act_z <- act_z %>%
      mutate(id = 1:n()) %>%
      left_join(y = final_z, by = "id")
  }

  sds$cor_z[i] <- cor(
    act_z$z.x, act_z$z.y,
    use = "pairwise.complete.obs"
  )
}

# Fit a smooth spline to the correlation curve
mod_cor <- lm(
  cor_z ~ ns(day_in_wint, df = 3) - 1,
  data = sds
)

fig_cor_eff <- capture_plot(
  mod_cor %>%
    effects::predictorEffects(
      partial.residuals = TRUE
    ) %>%
    plot()
)

# Normalize predicted correlations so the max is 1.0
sds$cor_z_pred <- predict(mod_cor, newdata = sds)
sds <- sds %>%
  mutate(
    cor_z_pred = cor_z_pred / max(cor_z_pred, na.rm = TRUE)
  )

# Correlation interpolation function for downstream use
cor_fun <- approxfun(sds$day_in_wint, sds$cor_z_pred)


# ============================================================
# 10. Overlay figure: mean + 95% CI bands
# ============================================================

fig_heatneed_bands <-
  obs_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul)) +
  theme_bw() +
  geom_line(
    alpha = 0.7,
    mapping = aes(
      color = year,
      fill = year,
      group = factor(ywint)
    )
  ) +
  geom_line(
    data = sds,
    mapping = aes(y = mean_pred),
    color = "salmon4", linewidth = 1.5
  ) +
  geom_line(
    data = sds,
    mapping = aes(y = mean_pred + 1.96 * sd_pred),
    color = "red", linewidth = 1.5
  ) +
  geom_line(
    data = sds,
    mapping = aes(y = mean_pred - 1.96 * sd_pred),
    color = "red", linewidth = 1.5
  ) +
  labs(
    x = "Day in season (starts Aug 1st)",
    y = "Cumulative missing degrees (C * days)",
    title = "Heating need by year + mean/CI"
  )


# ============================================================
# 11. NLME logistic growth (COMMENTED OUT)
# ============================================================

# NOTE: The logistic growth model below was explored in the
# original iter8.r.  It takes ~60 s to fit and the GLS
# approach above provides a better balance of speed and
# flexibility.  Kept here for reference / future exploration.
#
# dat_days_grouped <- groupedData(
#   tavg_low_cumul ~ day_in_wint | year,
#   data = obs_days_complete %>%
#     filter(!is.na(tavg_low_cumul))
# )
#
# mod_nlme <- nlme(
#   tavg_low_cumul ~ SSlogis(day_in_wint, Asym, xmid, scal),
#   random = Asym ~ 1 | year,
#   data = dat_days_grouped
# )
#
# summary(mod_nlme)
# obs_days_complete$predlme <- predict(mod_nlme)


# ============================================================
# 12. Save outputs
# ============================================================

save(
  obs_days_complete,
  mod_cum, mod_cum_obs, mod_cor,
  sds,
  sd_fun, mean_fun, cor_fun, return_std_tmp,
  fig_heatneed_per_year, fig_heatneed_bands,
  fig_modcum_gls, fig_cumobs_eff, fig_cor_eff,
  ACT_YEAR,
  file = here::here("data", "cumulative_heat_models.Rdata")
)

message("Saved: data/cumulative_heat_models.Rdata")
