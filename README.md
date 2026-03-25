# MartysGas

An R package for analyzing household gas consumption and local weather data, with the goal of optimizing heating strategy decisions (gas boiler vs. heat pump).

The project evolved through a series of `iter*.r` scripts that capture different modelling attempts -- from simple linear regressions to mixed-effects models, ARIMA forecasting, and Monte Carlo weather simulations.

## Quick start

Every analysis script assumes the common setup:

```r
source(here::here("inst", "function", "load_stuff.r"))
```

This loads packages (dplyr, ggplot2, nlme, splines, forecast, etc.), sources all functions from `R/`, and reads the core weather + gas dataset (`data/meteostat_data.Rdata`).

## Project layout

```
R/                        Core exported functions (interpolation, simulation, API query)
inst/
  function/load_stuff.r   Central loader -- run this first
  function/backend/       Pre-computed GLS temperature models & 100-year simulation
  iter*.r                 Main analysis iterations (iter2 through iter9)
  just_model/             Standalone modeling (tropical year, grid search optimization)
  iters/                  ARIMA / time series experiments
  extdata/gaz.xlsx        Gas meter readings (manual, irregular)
  extdata/meteostat_data/ Daily weather data (37 Excel files, 1995-present)
  Report_heatneed/        Quarto report generation (.qmd + children)
data/                     Processed .Rdata objects (~100 MB)
tests/testthat/           Unit tests
```

## Exported functions

| Function | Purpose |
|----------|---------|
| `create_meter_fun()` | Linear interpolation from irregular gas meter readings |
| `approx_rate()` | Instantaneous usage rate via numerical derivative |
| `get_avg_temp()` | Average temperature over an interval (integration) |
| `maketsum()` / `Maketsum()` | Normalized temperature sums (scalar / vectorized) |
| `simulate_weather()` | Monte Carlo daily weather with AR(1) residuals |
| `simulate_corr_resids()` | Autocorrelated residual generation |
| `merge_transform_weather()` | Full pipeline: merge weather Excel files + gas data |
| `meteostat_query_daily()` | Query Meteostat RapidAPI for daily station data |
| `load_all_Rdata()` | Batch-load all .Rdata files from a directory |
| `yday_inverse()` | Convert day-of-year number back to a date |
| `capture_plot()` | Capture base R plot as a recordedplot object |

## Analysis iterations

The `inst/iter*.r` scripts trace the project's evolution:

- **iter2** -- Foundation: cleans gas data, joins with temperatures, fits spline models
- **iter3** -- Weather CSV exploration, smoothing, seasonal patterns
- **iter4** -- Sinusoidal hourly temperature model, multi-year prediction grid
- **iter5** -- Heating season forecasting with multiple usage scenarios
- **iter6** -- GAM modeling, transformed variables, season indicators
- **iter7** -- GLS with AR(1) correlation, residual autocorrelation checks
- **iter8** -- Cumulative heating-degree days, NLME logistic growth
- **iter9** -- Merges all 37 weather Excel files, full interpolation pipeline

**Standalone** (`inst/just_model/`): tropical year modeling with mixed effects, grid search optimization.

**Time series** (`inst/iters/`): ARIMA, TBATS, quantile regression on simulated weather ensembles.

Old experiments live in `inst/archive/`.

## Weather data

Daily weather data comes from the [Meteostat](https://meteostat.net/) API via RapidAPI. The function `meteostat_query_daily()` automates querying and saving to Excel. An API key is needed in `inst/extdata/secrets/meteostat_api_key.txt`.

Gas meter readings are manually entered in `inst/extdata/gaz.xlsx`.

## Reports

Quarto reports are generated from `inst/Report_heatneed/report.qmd`, which includes child documents for the preamble and analysis sections. The report runs `merge_transform_weather()` in its setup chunk and saves a workspace snapshot on completion.

## Development

```r
devtools::check()            # Package checks
devtools::test()             # Run testthat tests
devtools::document()         # Regenerate NAMESPACE from roxygen2 tags
devtools::build_vignettes()  # Build vignettes
```
