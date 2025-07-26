# MartysGas

This repository collects a range of scripts exploring household gas consumption and related weather data.  
All analyses rely on a common loader script which installs packages, loads helper functions and imports previously processed data.

```r
source(here::here("inst","function","load_stuff.r"))
```

## `iter` scripts
Below is a short overview of the main iterative analysis scripts found under `inst/`.

### iter2.r
- Reads raw gas meter data from Excel and cleans column names
- Computes hourly/daily averages and helper variables
- Fits initial linear models linking consumption to temperature
- Produces exploratory plots

### iter3.r
- Processes `weather.csv` to derive lagged temperature measures
- Applies smoothing splines to visualise yearly trends
- Plots daily mean temperature patterns by year

### iter4.r
- Loads extended temperature dataset
- Models sinusoidal hourly effects and day-of-year splines
- Predicts hourly temperatures for a range of years
- Saves the resulting predictions

### iter5_rezsicsokk.r
- Combines gas readings with weather and rate predictions
- Estimates consumption scenarios under price discount rules
- Uses spline models and generates comparison figures

### iter6_mods.r
- Advanced modelling using GAM approaches
- Simulates hourly temperatures and expected gas use
- Calculates correction factors and visualises targets

### iter6_mods_bu.r
- Older backup of modelling attempts based on transformed data

### iter7.r
- Loads prepared datasets via `load_stuff.r`
- Investigates daily vs hourly observations
- Fits linear and GLS models accounting for autocorrelation

### iter8.r
- Builds interpolation functions for meter readings and temperatures
- Completes missing days and computes cumulative heating need
- Fits non-linear mixed models and explores uncertainty
- Provides numerous diagnostic graphics

### iter9.r
- Refreshes weather/consumption data using `merge_transform_weather`
- Calculates temperature deficits and remaining gas
- Visualises progress against expected trajectories

### just_model/iter1.r – iter3.r
- Stand‑alone modelling workflow stored under `inst/just_model`
- Iter1 prepares data and helper functions
- Iter2 and iter3 extend the models and compute predictions

## Package structure
- `R/` contains reusable helper functions
- `data/` stores processed datasets
- `inst/function/load_stuff.r` sets up the environment
- `vignettes/` includes draft vignette material

Run the unit tests (if devtools is available) with:

```r
Rscript -e 'devtools::test()'
```
