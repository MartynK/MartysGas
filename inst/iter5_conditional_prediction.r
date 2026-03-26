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

# Which ywint factor level corresponds to ACT_YEAR
act_ywint <- obs_days_complete %>%
  ungroup() %>%
  mutate(ywint = as.numeric(as.character(ywint))) %>%
  filter(year(Date) == ACT_YEAR) %>%
  mutate(ywint = max(ywint)) %>%
  slice(1) %>%
  pull(ywint)

# Confidence level for prediction bands
CONF_LEV <- 1.96


# ============================================================
# 2. Per-year correlations and SDs for tavgl_left approach
#    (from iter9.r lines 128-175)
# ============================================================

# tavgl_left = "temperature-degrees left to accumulate"
# For each year, this is max(cumul) - current_cumul
obs_days_complete <- obs_days_complete %>%
  group_by(ywint) %>%
  mutate(
    tavg_low_cum_ratio = tavg_low_cumul /
      max(tavg_low_cumul, na.rm = TRUE),
    tavgl_left = max(tavg_low_cumul, na.rm = TRUE) -
      tavg_low_cumul
  ) %>%
  ungroup()

# Summary stats across completed seasons
tavgcum_exp_sd <- obs_days_complete %>%
  group_by(ywint) %>%
  filter(day_in_year == max(day_in_year)) %>%
  .$tavg_low_cumul %>%
  sd(na.rm = TRUE)

tavgcum_exp <- obs_days_complete %>%
  group_by(ywint) %>%
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
      day_in_wint == i,
      ywint != act_ywint
    ) %>%
    select(tavgl_left, ywint)

  y <- obs_days_complete %>%
    group_by(ywint) %>%
    filter(ywint != act_ywint) %>%
    filter(day_in_wint == max(day_in_wint)) %>%
    select(tavg_low_cumul, ywint)

  mat <- left_join(x, y, by = "ywint") %>%
    select(-ywint) %>%
    as.matrix()

  vec_cors[i] <- cor(mat, use = "pairwise.complete.obs")[1, 2]
  vec_sds[i]  <- sd(mat[, 1])
}

# Attach per-day correlation and SD to the main data
obs_days_complete <- obs_days_complete %>%
  rowwise() %>%
  mutate(
    tavgcum_cor = vec_cors[round(day_in_wint) + 1],
    tavgcum_sd  = vec_sds[round(day_in_wint) + 1]
  ) %>%
  ungroup()


# ============================================================
# 3. Models for mean and SD of remaining heat need
# ============================================================

# Mean remaining heat need as a function of day_in_wint
mod_tavg_low_cum_mean <- lm(
  tavgl_left ~ ns(day_in_wint, df = 5),
  data = obs_days_complete
)

# SD of remaining heat need (captures heteroscedasticity)
mod_tavg_low_cum_sd <- lm(
  tavgcum_sd ~ ns(day_in_wint, df = 11),
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
    day_in_wint_act = 173,
    cum_t_act = 1530,
    sds. = sds,
    full = TRUE
) {
  cor_fun_local <- approxfun(
    sds.$day_in_wint, sds.$cor_z_pred
  )
  cor_act     <- cor_fun_local(day_in_wint_act)
  sd_expected <- sqrt(1 - cor_act^2)
  z_act       <- return_std_tmp(
    day_in_wint_act, cum_t_act
  )$z
  z_expected  <- cor_act * z_act

  # full = TRUE returns the entire trajectory from
  # day_in_wint_act to 364; FALSE returns only day 364
  if (full == FALSE) {
    dayz <- 364
  } else {
    dayz <- day_in_wint_act:364
  }

  expecteds <-
    data.frame(day_in_wint = dayz) %>%
    left_join(x = ., y = sds., by = "day_in_wint") %>%
    mutate(
      prop_var = (cor_z_pred - cor_act) / (1 - cor_act),
      sd_act   = sd_expected * prop_var,
      z_act.   = seq(z_act, z_expected, length.out = n()),
      lower_expected = mean_fun(day_in_wint) +
        (z_act. - sd_act * conf_lev) *
        sd_fun(day_in_wint),
      upper_expected = mean_fun(day_in_wint) +
        (z_act. + sd_act * conf_lev) *
        sd_fun(day_in_wint)
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
    gas_left = 1730 - 0.4 * (365 - day_in_wint) - Spent,
    # Suggested rate for the rest of the season
    spent_tavg_left_mean = gas_left / tavgl_left_pred,
    spent_tavg_left_upr  = gas_left / tavgcum_pred_upr,
    spent_tavg_left_lwr  = gas_left / tavgcum_pred_lwr
  )


# ============================================================
# 6. January deadline calculations (from iter9 lines 215-237)
# ============================================================

# January 5 is the atalany true-up deadline.
# day_in_wint = 157 corresponds to ~Jan 5

jan_gasvals <- obs_days_complete %>%
  filter(day_in_wint == 157) %>%
  select(Meter, Spent, Date)

# For each row, compute gas remaining if Jan 5 were the
# deadline (ie. looking at the meter value on Jan 5 of
# that year)
obs_days_complete$gas_left_jan <- NA_real_
for (i in 1:nrow(obs_days_complete)) {
  act_yr <- year(obs_days_complete$Date[i])
  act_last_meter <- jan_gasvals$Meter[
    act_yr == year(jan_gasvals$Date)
  ]
  if (length(act_last_meter) == 1) {
    obs_days_complete$gas_left_jan[i] <-
      1730 - obs_days_complete$Meter[i] + act_last_meter
  }
}

# Suggested rate if targeting January deadline
obs_days_complete <- obs_days_complete %>%
  mutate(
    tavgl_left_jan = predict(
      mod_tavg_low_cum_mean,
      newdata = data.frame(day_in_wint = 157)
    ) - tavg_low_cumul,
    spent_tavg_left_jan = gas_left_jan / tavgl_left_jan
  )


# ============================================================
# 7. Key figures
# ============================================================

# Efficiency ratio over time, most recent season
ywint_lim <- obs_days_complete %>%
  filter(year(Date) == ACT_YEAR) %>%
  slice_tail(n = 1) %>%
  pull(ywint) %>%
  as.character()

# Date axis limits for the current season
date_limits <-
  obs_days_complete %>%
  filter(ywint == ywint_lim) %>%
  arrange(day_in_wint) %>%
  .[60, ] %>%
  pull(Date) %>%
  year() %>%
  paste0(., "-08-01") %>%
  as.POSIXct() %>%
  rep(., 2)
date_limits[2] <- date_limits[2] + 365 * 24 * 3600

fig_efficiency_ratio <-
  obs_days_complete %>%
  filter(ywint == ywint_lim) %>%
  ggplot(aes(
    x = Date,
    y = spent_tavg,
    color = gas_left
  )) +
  theme_bw() +
  scale_color_gradient(
    high = "blue", low = "green",
    limits = c(0, 1730)
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
  geom_line(
    aes(y = spent_tavg_left_jan),
    color = "salmon4",
    linetype = "solid", linewidth = 1.2
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = date_limits
  ) +
  labs(
    x = "",
    y = "Average gas consumption (m3 / C*day)"
  )

# Conditional prediction bands figure
fig_conditional_bands <-
  obs_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul)) +
  theme_bw() +
  geom_line(
    alpha = 0.7,
    mapping = aes(
      color = year,
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
    x = "Day in season",
    y = "Cumulative missing degrees (C * days)",
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
