# MartysGas

A collection of scripts exploring my household gas consumption and local weather data.  The project gradually evolved through a series of `iter*.r` files that capture different modelling attempts.  Utility functions now live under `R/` and are loaded together with packages via `inst/function/load_stuff.r`.

## Using the scripts

Every iteration script assumes the necessary packages and helper functions are loaded by sourcing `load_stuff.r`:

```r
source(here::here("inst", "function", "load_stuff.r"))
```

This script attaches common packages (dplyr, ggplot2, etc.), loads helper functions from `R/`, and reads previously saved data objects.
It also defines a small `capture_plot()` helper used by several iterations to

## Iteration overview

- **iter2.r**
  - Cleans gas meter readings and joins hourly temperature estimates.
  - Fits simple consumption models against splined temperature.
  - Produces exploratory plots of predicted vs. observed rates.
- **iter3.r**
  - Processes daily weather CSV data.
  - Demonstrates smoothing and seasonal plotting of temperature trends.
- **iter4.r**
  - Builds a sinusoidal model for hourly temperatures.
  - Creates a prediction grid for multiple years and saves the results.
- **iter5_rezsicsokk.r**
  - Plans the 2022/23 heating season under different usage margins.
  - Combines gas and weather data and fits spline models.
  - Produces forecasts and summary charts for several scenarios.
- **iter6_mods.r**
  - Advanced modelling with transformed data and season indicators.
  - Generates predicted consumption curves and compares with reality.
  - Saves results for later reuse.
- **iter6_mods_bu.r**
  - Backup of the previous iteration with similar analyses.
- **iter7.r**
  - Analyses observed daily and hourly data with linear and GLS models.
  - Checks residual autocorrelation and compares modelling approaches.
- **iter8.r**
  - Provides helper functions for interpolation and averaging.
  - Computes cumulative heating-degree days and shows comparison plots.
- **iter9.r**
  - Merges Meteostat weather with gas data for a fresh season.
  - Fills gaps via interpolation and tracks cumulative heating need.
- **inst/iters/alter_plot.r**, **arima.r**, **arima_v2.r**
  - Experiments with ARIMA forecasting and quantile regression on simulated weather.
- **inst/just_model/iter1.r**, **iter2.r**, **new model.r**
  - Standalone modelling attempts using spline-based approaches to predict gas usage.

Feel free to explore each script in chronological order to see how the analysis evolved.

Old commented-out experiments are parked under `inst/archive`.

