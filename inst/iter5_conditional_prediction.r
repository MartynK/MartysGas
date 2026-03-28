# ------------------------------------------------------------
# iter5_conditional_prediction.r -- Conditional heating
#   prediction and efficiency ratios
#
# From iter8.r (lines 340-480) + iter9.r (lines 130-293).
# Given observed cumulative heat on day d, predict the
# remaining heating need for the rest of the season using
# the correlation structure from iter4.  Translates into
# gas budget efficiency ratios.
#
# Inputs : data/cumulative_heat_models.Rdata
# Outputs: data/cumulative_heat_predictions.Rdata
# Runtime: ~20 s (rowwise prediction loop)
# ------------------------------------------------------------

source(here::here("inst", "function", "load_stuff.r"))
load(here::here("data", "cumulative_heat_models.Rdata"))


# ============================================================
# 1. Constants
# ============================================================

# Current year -- should match iter4
ACT_YEAR <- 2025

# Current calendar year for filtering
act_year <- ACT_YEAR

# Confidence level for prediction bands
CONF_LEV <- 1.96


# ============================================================
# 2. Per-year correlations and SDs for tavgl_left approach
#    (from iter9.r lines 128-175)
# ============================================================

# tavgl_left = "temperature-degrees left to accumulate"
# For each year, this is max(cumul) - current_cumul
obs_days_complete <- obs_days_complete %>%
  group_by(year) %>%
  mutate(
    tavg_low_cum_ratio = tavg_low_cumul /
      max(tavg_low_cumul, na.rm = TRUE),
    tavgl_left = max(tavg_low_cumul, na.rm = TRUE) -
      tavg_low_cumul
  ) %>%
  ungroup()

# Summary stats across completed seasons
tavgcum_exp_sd <- obs_days_complete %>%
  group_by(year) %>%
  filter(day_in_year == max(day_in_year)) %>%
  .$tavg_low_cumul %>%
  sd(na.rm = TRUE)

tavgcum_exp <- obs_days_complete %>%
  group_by(year) %>%
  filter(day_in_year == max(day_in_year)) %>%
  .$tavg_low_cumul %>%
  mean(na.rm = TRUE)

# Per-day correlation of tavgl_left with end-of-season total
# and per-day SD of tavgl_left (from iter9 lines 147-170)
vec_cors <- rep(NA_real_, 366)
vec_sds  <- rep(NA_real_, 366)

for (i in 1:364) {
  x <- obs_days_complete %>%
    ungroup() %>%
    filter(
      day_in_year == i,
      year != act_year
    ) %>%
    select(tavgl_left, year)

  y <- obs_days_complete %>%
    group_by(year) %>%
    filter(year != act_year) %>%
    filter(day_in_year == max(day_in_year)) %>%
    select(tavg_low_cumul, year)

  mat <- left_join(x, y, by = "year") %>%
    select(-year) %>%
    as.matrix()

  vec_cors[i] <- cor(mat, use = "pairwise.complete.obs")[1, 2]
  vec_sds[i]  <- sd(mat[, 1])
}

# Attach per-day correlation and SD to the main data
obs_days_complete <- obs_days_complete %>%
  rowwise() %>%
  mutate(
    tavgcum_cor = vec_cors[round(day_in_year) + 1],
    tavgcum_sd  = vec_sds[round(day_in_year) + 1]
  ) %>%
  ungroup()


# ============================================================
# 3. Models for mean and SD of remaining heat need
# ============================================================

# Mean remaining heat need as a function of day_in_year
mod_tavg_low_cum_mean <- lm(
  tavgl_left ~ ns(day_in_year, df = 5),
  data = obs_days_complete
)

# SD of remaining heat need (captures heteroscedasticity)
mod_tavg_low_cum_sd <- lm(
  tavgcum_sd ~ ns(day_in_year, df = 11),
  data = obs_days_complete
)

# Store predictions in the data
obs_days_complete$tavgl_left_pred <- predict(
  mod_tavg_low_cum_mean,
  newdata = obs_days_complete
)
obs_days_complete$tavgl_left_pred_sd <- predict(
  mod_tavg_low_cum_sd,
  newdata = obs_days_complete
)


# ============================================================
# 4. predict_future_needs() function
#    (from child2.qmd lines 329-364)
# ============================================================

# Given a day and observed cumulative heat, return a data
# frame of conditional predictions for the rest of the season.
# Uses the correlation structure from iter4 to shrink the
# unconditional band towards the observed trajectory.
predict_future_needs <- function(
    conf_lev = 1.96,
    day_in_year_act = 173,
    cum_t_act = 1530,
    sds. = sds,
    full = TRUE
) {
  cor_fun_local <- approxfun(
    sds.$day_in_year, sds.$cor_z_pred
  )
  cor_act     <- cor_fun_local(day_in_year_act)
  sd_expected <- sqrt(1 - cor_act^2)
  z_act       <- return_std_tmp(
    day_in_year_act, cum_t_act
  )$z
  z_expected  <- cor_act * z_act

  # full = TRUE returns the entire trajectory from
  # day_in_year_act to 365; FALSE returns only day 365
  if (full == FALSE) {
    dayz <- 365
  } else {
    dayz <- day_in_year_act:365
  }

  expecteds <-
    data.frame(day_in_year = dayz) %>%
    left_join(x = ., y = sds., by = "day_in_year") %>%
    mutate(
      prop_var = (cor_z_pred - cor_act) / (1 - cor_act),
      sd_act   = sd_expected * prop_var,
      z_act.   = seq(z_act, z_expected, length.out = n()),
      lower_expected = mean_fun(day_in_year) +
        (z_act. - sd_act * conf_lev) *
        sd_fun(day_in_year),
      upper_expected = mean_fun(day_in_year) +
        (z_act. + sd_act * conf_lev) *
        sd_fun(day_in_year)
    )

  return(expecteds)
}


# ============================================================
# 5. Prediction intervals and efficiency ratios
# ============================================================

# 95% CI for remaining heat need
obs_days_complete <- obs_days_complete %>%
  mutate(
    tavgcum_pred_lwr = tavgl_left_pred -
      CONF_LEV * tavgl_left_pred_sd,
    tavgcum_pred_upr = tavgl_left_pred +
      CONF_LEV * tavgl_left_pred_sd,
    # Efficiency ratio: gas spent per degree of heating need
    spent_tavg = Spent / tavg_low_cumul,
    # Gas budget remaining (0.4 m3/day hot water baseline)
    gas_left = 1730 - 0.4 * (365 - day_in_year) - Spent,
    # Suggested rate for the rest of the season
    spent_tavg_left_mean = gas_left / tavgl_left_pred,
    spent_tavg_left_upr  = gas_left / tavgcum_pred_upr,
    spent_tavg_left_lwr  = gas_left / tavgcum_pred_lwr
  )


# ============================================================
# 6. Jan 5 meter reading anchor
# ============================================================

# In the calendar year system, Jan 5 (yday = 5) is when
# the physical meter gets read.  For each year, find the
# meter value closest to Jan 5 and use it as the "start
# of year" anchor for gas_left calculations.
jan_gasvals <- obs_days_complete %>%
  filter(day_in_year == 5) %>%
  select(year, Meter_jan = Meter)

obs_days_complete <- obs_days_complete %>%
  left_join(jan_gasvals, by = "year") %>%
  mutate(
    # Gas consumed since the Jan 5 reading of this year
    gas_since_jan = Meter - Meter_jan,
    # Budget remaining against the 1729 m3 annual quota
    gas_left_jan  = 1729 - gas_since_jan
  )


# ============================================================
# 7. Key figures
# ============================================================

# Efficiency ratio over time, current calendar year
year_lim <- ACT_YEAR

# Date axis: Jan 1 - Dec 31 of current year
date_limits <- c(
  as.POSIXct(paste0(year_lim, "-01-01")),
  as.POSIXct(paste0(year_lim, "-12-31"))
)

fig_efficiency_ratio <-
  obs_days_complete %>%
  filter(year == year_lim) %>%
  ggplot(aes(
    x = Date,
    y = spent_tavg,
    color = gas_left_jan
  )) +
  theme_bw() +
  scale_color_gradient(
    high = "blue", low = "green",
    limits = c(0, 1729),
    name = "Gas left (m3)"
  ) +
  geom_line(linewidth = 1.5) +
  geom_line(
    aes(y = spent_tavg_left_mean),
    color = "grey50",
    linetype = "dashed", linewidth = 1.2
  ) +
  geom_line(
    aes(y = spent_tavg_left_lwr),
    color = "grey70",
    linetype = "dashed", linewidth = 1.2
  ) +
  geom_line(
    aes(y = spent_tavg_left_upr),
    color = "grey70",
    linetype = "dashed", linewidth = 1.2
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = date_limits
  ) +
  labs(
    x = "",
    y = "Gas efficiency (m3 / C*day)"
  )

# Conditional prediction bands figure
fig_conditional_bands <-
  obs_days_complete %>%
  ggplot(aes(x = day_in_year, y = tavg_low_cumul)) +
  theme_bw() +
  geom_line(
    alpha = 0.7,
    mapping = aes(
      color = year,
      group = factor(year)
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
    x = "Day of year",
    y = "Cumulative heating need (C * days)",
    title = "Unconditional + conditional bands"
  )


# ============================================================
# 8. Save outputs
# ============================================================

save(
  obs_days_complete,
  mod_tavg_low_cum_mean, mod_tavg_low_cum_sd,
  predict_future_needs,
  sd_fun, mean_fun, cor_fun, return_std_tmp,
  sds,
  tavgcum_exp, tavgcum_exp_sd,
  fig_efficiency_ratio, fig_conditional_bands,
  ACT_YEAR, CONF_LEV,
  file = here::here(
    "data", "cumulative_heat_predictions.Rdata"
  )
)

message("Saved: data/cumulative_heat_predictions.Rdata")
