# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MartysGas is an R package for tracking household gas consumption against the Hungarian subsidized gas quota ("rezsicsökkentés": 1,729 m³/year at ~102 HUF/m³ vs. ~767 HUF/m³ penalty rate). The quota year runs Aug 1 – Jul 31. The user pays flat-rate (átalány) with a physical meter reading ~Jan 5.

The pipeline produces a **decision tool**: "Given current consumption and weather uncertainty, should I amp up, hold steady, or dial down gas usage this week?"

## Package Structure

```
MartysGas/
├── R/                           # 8 source files, 12+ exported functions
├── inst/
│   ├── function/
│   │   ├── load_stuff.r         # Central loader (packages + R/ + data)
│   │   └── backend/             # Legacy GLS model scripts (reference)
│   ├── iter1_data_prep.r        # Step 1: Wrangle weather + gas data
│   ├── iter2_weather_models.r   # Step 2: GLS backend + 100yr simulation
│   ├── iter3_consumption_curves.r # Step 3: GAM rate curves (descriptive)
│   ├── iter4_cumulative_heat_model.r # Step 4: Degree-day models + z-scores
│   ├── iter5_conditional_prediction.r # Step 5: Forecasting + efficiency ratio
│   ├── iter6_decision_tool.r    # Step 6: "Amp up / hold / dial down"
│   ├── weathermodels/           # Alternative weather approaches (standalone)
│   │   ├── tropical_year.r      # Tropical year LME (runnable)
│   │   └── arima_v2_reference.r # ARIMA experiments (reference)
│   ├── misc/                    # Archived old experiments
│   ├── extdata/
│   │   ├── gaz.xlsx             # Gas meter readings (manual, irregular)
│   │   ├── meteostat_data/      # 37+ weather data Excel files (1995-present)
│   │   └── secrets/             # API key for Meteostat RapidAPI
│   └── Report_heatneed/        # Quarto report (.qmd + children + dashboard)
├── data/                        # Pipeline outputs (~25 MB)
├── tests/testthat/              # testthat tests
├── vignettes/                   # Package documentation
├── DESCRIPTION                  # Package: MartysGas v0.0.1
└── NAMESPACE                    # 11 exports (meteostat_query_daily pending)
```

## Analysis Pipeline

```
iter1_data_prep.r        → data/meteostat_data.Rdata
iter2_weather_models.r   → data/weather_models.Rdata
iter3_consumption_curves.r → data/consumption_curves.Rdata
iter4_cumulative_heat_model.r → data/cumulative_heat_models.Rdata
iter5_conditional_prediction.r → data/cumulative_heat_predictions.Rdata
iter6_decision_tool.r    → console recommendation + data/decision_snapshot.Rdata
Report_heatneed/report.qmd → loads all .Rdata, renders figures + dashboard
```

**When new data arrives**: Run iter1 (re-wrangles everything). Then re-run iter2–iter6 as needed.

## Billing Context (Hungarian Gas Quota)

```r
QUOTA_M3       <- 1729       # Annual subsidized volume (~63,645 MJ)
PRICE_CHEAP    <- 102        # HUF/m³ below quota
PRICE_PENALTY  <- 767        # HUF/m³ above quota (~7.5× multiplier)
# Quota year: Aug 1 – Jul 31 (ywint definition is correct)
# Meter reading: ~Jan 5 (átalány true-up)
# Pro-rata daily quota: ~4.74 m³/day (flat, not jelleggörbe)
```

## R Style Guide Options

### General policies 

-  Readability trumps performance and even functionality. (Tasks are usually quick and handle small objects.)
-  Quick operations should be kept within a file; if a task is long (2+ minutes), then it merits its own R script with a save() or save.image() at the end.
-  The train-of-thought within a project should be kept via naming the scripts (eg. numbering prefixes like 01_intro.r 02_desc_stats.r etc.) or related files (child1.qmd, child2.qmd...)
-  Length of a single file should be kept <500 lines (ideally 400 lines). Use save.image() and load() or source() appropriately.
-  After a long script, the end state or the relevant object should be saved under the data/ folder as an .Rdata or a .rda object denting the file name (eg. end_state_iter8_mixedmod_stuff.RData)
-  Each script should start with a comment briefly explaining what the script does. Then Library calls and helper function source()-ing as appropriate.
-  Be afraid of state change. If a state change occurs, try to give the changed object a different name. Try to identify common needs for objects (eg. data wrangling wise) at the beginning tand try to construct objects which are then used several times throughout the processes. Also don't skimp on simple 'throwaway' objects if some modification is required for a single task and would most likely not be needed elsewhere.
-  Prefer the long format for data; be aware that input data from the database itself may be in the wide format. In those cases validate the transformations.
  


### Pipe Operators

-  ✅ Use magrittr pipes %>% over native pipes |> 
-  Chain operations with pipes when >3 steps or when aesthetically better >=2
-  Break long pipe chains at logical points

### Iteration & Functional Programming

-  ✅ Use for loops over purrr/map functions especially if <3000 iterations are expected
-  Use vectorized operations when possible
-  Aim for pre-allocating vectors/lists in loops

### Naming Conventions - Variables

-  ✅ Use snake_case for all variables 
-  Use descriptive variable names (>3 characters)
-  Avoid abbreviations unless well-known

### Naming Conventions - Objects

-  ✅ Prefix data frames / tibbles with dat_
-  ✅ Use chunk-based naming: dat_chunkname_locf if using an object in a single chunk only
-  Prefix models with mod_ or model_
-  Prefix plots with fig_ or plot_
-  Prefix functions with fun_ or no prefix
-  Use lst_ for lists, vec_ for vectors


### Naming Conventions - Functions

-  Use verb_noun pattern for function names
-  Use snake_case for function names, capitalize if function is Vectorized
-  Start with action verbs (get_, create_, calculate_)
-  End with data type if appropriate (_df, _list, _plot)

### Code Organization

-  ✅ Descriptive chunk names in R Markdown, aim for uniqueness (you do this well)
-  Use # for major sections, ## for subsections, ### to keep things organized within a subsection
-  Load all libraries at top of script
-  Define constants/parameters at top after libraries, in ALL_CAPS
-  Use consistent indentation (2 spaces vs 4 spaces)

### Assignment & Operators

-  Use <- for assignment (R standard)
-  Space around operators: x + y not x+y
-  No space before comma, space after: c(1, 2, 3)
-  Always elaborate for if statements eg. if (cond == TRUE) {...

### Line Length & Formatting

-  Maximum 80 characters per line
-  Break long function calls across lines
-  Align parameters in multi-line function calls
-  Use trailing commas in multi-line lists

### Comments & Documentation

-  Use # for inline comments with space after
-  Use #' for roxygen2 documentation
-  Write comments explaining "why" and "what" too. Use copious amounts of comments.
-  Use TODO/FIXME/NOTE for code annotations
-  Document all function parameters and returns

### Error Handling & Defensive Programming

-  Always check for NULL/missing data before operations
-  Use stop() for critical errors, warning() for non-critical, and message() for good-to-know info
-  Validate function inputs at start of function
-  Use meaningful error messages
-  Use try()/tryCatch() for operations that might fail, especially if nested within a loop

## Key Considerations

- The main goal is to produce 'Reports' from input data.
- 'Reports' mainly consist of text, figures and tables, in a .qmd ecology.
- I prefer a structure where figures and tables are named and referenced in the Report.
- Reports are generated using Quarto from `inst/Report_heatneed/report.qmd`

### When to Refactor Large Files
  - Break files when they exceed ~400-500 lines
  - Split at logical section boundaries (e.g., after Primary Endpoint, before ROM analyses)
  - Each child document should end with `save.image(file = here::here("inst", "report", "state_after_childX.RData"))`
  - Next child document should start with `load(here::here("inst", "report", "state_after_childX.RData"))`

## Cross-Environment R Libraries (Ubuntu/WSL)

Pre-compiled R packages for Ubuntu/WSL2 are at:
`/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages/`

Activate with:
```r
.libPaths(c('/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages', .libPaths()))
```

This enables lme4, emmeans, zoo, effects, RcppArmadillo, and 200+ other compiled packages without system-level compilation.


## Development Workflow

### Essential Setup
Every analysis script assumes this setup first:
```r
source(here::here("inst", "function", "load_stuff.r"))
```

This single command:
- Loads common packages (dplyr, ggplot2, lubridate, nlme, splines, etc.)
- Sources all functions from R/ directory
- Loads preprocessed data from `data/meteostat_data.Rdata`
- Loads backend models and results from `inst/function/backend/`

### Common Commands

**Build and check package:**
```r
devtools::check()
devtools::build()
```

**Run tests:**
```r
devtools::test()
# Or via testthat directly:
testthat::test_check("MartysGas")
```

**Build vignettes:**
```r
devtools::build_vignettes()
```

### Analysis Architecture

The project follows an iterative exploration pattern:

1. **Data Processing**: Raw gas meter readings and weather data are cleaned and joined
2. **Model Development**: Multiple approaches tested including:
   - Spline-based temperature relationships
   - Sinusoidal temperature models
   - ARIMA forecasting
   - GLS models with correlation structures
3. **Simulation**: Weather data simulation for long-term forecasting
4. **Reporting**: Quarto reports in `inst/Report_heatneed/`

### Key Data Flow

- Weather data: `inst/extdata/meteostat_data/` → processed via `merge_transform_weather()` → `data/meteostat_data.Rdata`
- Gas data: `inst/extdata/gaz.xlsx` → processed in iter scripts → various `.rdata` files
- Models: Trained models saved in `data/` directory for reuse across scripts (consolidated from `inst/function/backend/`)

### Exported Functions (NAMESPACE)

Currently exported (11 functions):
- `create_meter_fun()`, `approx_rate()`, `get_avg_temp()`: Gas meter interpolation & rate calculation
- `capture_plot()`: Capture base R plots as objects
- `simulate_weather()`, `simulate_corr_resids()`: Monte Carlo weather simulation
- `maketsum()`, `Maketsum()`: Temperature sum calculations (scalar & vectorized)
- `load_all_Rdata()`: Batch data loading
- `yday_inverse()`: Day-of-year to date conversion
- `merge_transform_weather()`: Full weather + gas data pipeline

**Not yet exported** (needs `devtools::document()`):
- `meteostat_query_daily()`: Queries Meteostat RapidAPI for daily weather data, saves to Excel

### Script Pipeline (Updated 2026-03-25)

**Main pipeline** (`inst/iter*.r`) — run sequentially:

| Script | Lines | Runtime | Purpose |
|--------|-------|---------|---------|
| iter1_data_prep.r | 80 | ~30s | Wrangle weather + gas data → meteostat_data.Rdata |
| iter2_weather_models.r | 166 | ~60s | GLS temp models + 100yr simulation → weather_models.Rdata |
| iter3_consumption_curves.r | 256 | ~10s | GAM rate curves (descriptive) → consumption_curves.Rdata |
| iter4_cumulative_heat_model.r | 351 | ~120s | Degree-day models + z-scores → cumulative_heat_models.Rdata |
| iter5_conditional_prediction.r | 368 | ~30s | Forecasting + efficiency → cumulative_heat_predictions.Rdata |
| iter6_decision_tool.r | 387 | <10s | Decision: amp up / hold / dial down → decision_snapshot.Rdata |

**Alternative weather models** (`inst/weathermodels/`):

| Script | Lines | Purpose |
|--------|-------|---------|
| tropical_year.r | 153 | Runnable standalone: tropical year LME → tropical_year.Rdata |
| arima_v2_reference.r | 145 | Reference only: ARIMA/TBATS experiments |

**Archived scripts**: `inst/misc/` — old iterations, experiments, superseded approaches

## Modeling Approach Categories

Due to the experimental nature of this codebase, modeling approaches are scattered across multiple files. The following categories help organize the different techniques used:

### Climate/Weather Modeling Categories

1. **Sinusoidal/Trigonometric** - Simple seasonal and daily temperature cycles using sin/cos functions
   - Files: `iter4.r`, `just_model/iter1.r`, `just_model/new model.r`
   - Techniques: Daily cycles, tropical year components, hourly temperature variation

2. **Spline-Based Weather** - Natural splines for smooth seasonal patterns with GLS correlation
   - Files: `function/backend/mod_tavg.r`, `function/backend/mod_range.r`, `iters/arima_v2.r`
   - Techniques: `ns(day_in_year, df=4)` for temperature averages and ranges

3. **ARIMA/Time Series** - Auto-ARIMA and TBATS for complex seasonality forecasting
   - Files: `iters/arima.r`, `iters/arima_v2.r`
   - Techniques: `auto.arima()`, `tbats()`, time series decomposition

4. **Weather Simulation** - Monte Carlo simulation with autocorrelation for long-term risk assessment
   - Files: `R/simulate_weather.r`, `R/simulate_corr_resids.r`
   - Techniques: Multivariate normal with AR(1) correlation, 100-year simulations

### Gas Consumption Modeling Categories

1. **Linear Regression** - Basic temperature-consumption relationships with yearly variations
   - Files: `iter2.r`, `iter6_mods.r`, `iter7.r`
   - Techniques: `Rate ~ ns(tact, df=1) + ywint`, temperature thresholds

2. **GLS with Correlation** - Accounts for temporal autocorrelation in consumption data
   - Files: `iter7.r`, `function/backend/mod_tavg.r`
   - Techniques: `gls()` with `corAR1()` correlation structures

3. **GAM Models** - Non-linear smooth curves for temperature-consumption relationships
   - Files: `iter6_mods.r`
   - Techniques: `geom_smooth(method='gam')` with tensor product splines

4. **Mixed Effects** - Random effects for between-year/season variations
   - Files: `just_model/iter1.r`, `just_model/iter2.r`
   - Techniques: `lme()` with random effects by winter year

### Data Transformation Categories

1. **Interpolation Methods** - Linear interpolation and numerical derivatives for irregular data
   - Files: `R/approx_helpers.r`, `R/transform_meteostat_weather.r`
   - Techniques: `approxfun()`, numerical integration for averages

2. **Cumulative Heat Need** - Heating degree calculations and cumulative temperature deficits
   - Files: `iter8.r`, `iter9.r`
   - Techniques: `(20 - tavg_capped) * days`, `nlme()` with logistic growth

3. **Seasonal Adjustments** - Winter year definitions and heating season alignments
   - Files: `R/transform_meteostat_weather.r`, multiple iter scripts
   - Techniques: `ywint = ifelse(yday(date) < 213, year-1, year)`

### Core Modeling Paradigms

The codebase demonstrates several distinct analytical approaches:

- **Physics-based modeling**: Heating degree concepts and thermal relationships
- **Statistical time series**: ARIMA, autocorrelation, and temporal modeling  
- **Smooth function approximation**: Splines and GAMs for flexible curve fitting
- **Hierarchical modeling**: Mixed effects to handle grouping structures
- **Simulation-based inference**: Monte Carlo methods for uncertainty quantification
- **Interpolation/approximation**: Handling irregular data and rate calculations

## Change Log

### 2026-03 — Major Refactoring
- **New pipeline**: 6 numbered scripts (iter1 through iter6) replacing 15+ scattered iteration files
- **Fixed "double LOCF"**: Complete-date interpolation now in `merge_transform_weather()`, not duplicated across iter8/iter9/child2
- **Fixed bug**: Removed undefined `data` variable from `save()` in transform_meteostat_weather.r
- **Decision tool** (iter6): Prints weekly recommendation based on quota status and weather uncertainty
- **Report slimmed**: child2.qmd from ~835 lines to ~170 (loads pre-computed state, no inline modeling)
- **Archived**: 20+ old scripts to `inst/misc/`, stale data files to `data/archive/`
- **weathermodels/**: Tropical year LME as runnable standalone, ARIMA as reference
- **Billing context documented**: Hungarian gas quota (1,729 m³, Aug 1 – Jul 31, átalány)
- **Data reduced**: ~100 MB → ~25 MB (targeted `save()` instead of `save.image()`)

### 2026-02 — Automated Weather Querying
- Added `R/meteostat_query.r` with `meteostat_query_daily()` for RapidAPI weather data pulls
- API key stored in `inst/extdata/secrets/meteostat_api_key.txt`
- Regenerated `data/meteostat_data.Rdata` with latest data (Feb 2026)

### 2025-08 — Major Cleanup
- Consolidated all `.rdata` files to `data/` directory, removed duplicates from `inst/`
- Fixed column name mismatches, standardized variable naming across scripts

## Data Loading Hierarchy

```
load_stuff.r
├── Packages: dplyr, ggplot2, lubridate, nlme, splines, readxl, readr,
│             ggpubr, forecast, quantreg, lme4, boot, splines2, here,
│             foreach, doParallel
├── source_all_files(here::here("R"))       # All 8 R/ source files
├── load("data/meteostat_data.Rdata")       # Core weather + gas data
└── load("data/weather_models.Rdata")       # GLS models + simulation (if exists)
```

**Pipeline .Rdata chain** (each script loads its predecessor's output):
- `meteostat_data.Rdata` — weather, gas readings, obs_days_complete (produced by iter1)
- `weather_models.Rdata` — mod_tavg, mod_range, weather_simulated (produced by iter2)
- `consumption_curves.Rdata` — GAM model + figures (produced by iter3)
- `cumulative_heat_models.Rdata` — GLS + SD + z-scores (produced by iter4)
- `cumulative_heat_predictions.Rdata` — forecasts + efficiency (produced by iter5)
- `decision_snapshot.Rdata` — current recommendation (produced by iter6)

## Known Issues

- `NAMESPACE` out of sync: `meteostat_query_daily` has `@export` tag but needs `devtools::document()`
- Pipeline not yet validated end-to-end (iter1 through iter6 need to be run sequentially to generate .Rdata files)
- Old `data/iter6_mods.rdata` (44 MB) and individual model files still present; will be superseded when pipeline runs
- NLME logistic model in iter4 is commented out by default (may be slow >500s)