# Archived Scripts

These are old iteration scripts and experiments that were superseded during the 2026-03 refactoring. They are kept for reference but are NOT part of the active pipeline.

## Notable files

- `old_weather_calc.r` — Gas vs. heat pump cost analysis with COP model. Interesting standalone analysis but never integrated into the main pipeline. Could be revived if heat pump modeling becomes a priority.
- `old_iter7.r` — GLS with AR(1) diagnostic analysis. Key finding: autocorrelation matters significantly in gas consumption models. This insight was incorporated into the cumulative heat modeling approach.
- `old_arima.r` / `old_arima_v2.r` — ARIMA/TBATS experiments. The ARIMA approach has fundamental issues with internal NAs in the time series object.
