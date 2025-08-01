# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MartysGas is an R package for analyzing household gas consumption patterns and weather data correlations. The project evolved through a series of iterative scripts (`iter*.r`) exploring different modeling approaches to predict gas usage based on temperature data. It is very haphazard, similar steps have been attempted multiple times, and basic data transformations are run and rerun, so the "workflow" is nonexistent. Categorizing and building a logical train of thought in the steps within is the most important thing.

## Package Structure

- **R/**: Core utility functions exported by the package
- **inst/**: Main analysis scripts, iteration experiments, and data files
  - `iter*.r`: Sequential analysis iterations with increasing complexity
  - `function/load_stuff.r`: Central loader that attaches packages, sources R/ functions, and loads saved data
  - `extdata/`: Raw data files (gas readings, weather data)
  - `iters/`: Experimental scripts with ARIMA and alternative approaches
  - `just_model/`: Standalone modeling attempts
- **data/**: Processed datasets in RData format
- **tests/**: Unit tests using testthat framework
- **vignettes/**: Package documentation and examples

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

## Key considerations

- The main goal is to produce 'Reports' from input data.
- 'Reports' mainly consist of text, figures and tables, in a .qmd ecology.
- I prefer a structure where figures and tables are named and referenced in the Report.
- Reports are generated using Quarto from `inst/report/report.qmd`

### When to Refactor Large Files
  - Break files when they exceed ~400-500 lines
  - Split at logical section boundaries (e.g., after Primary Endpoint,
  before ROM analyses)
  - Each child document should end with `save.image(file =
  here::here("inst", "report", "state_after_childX.RData"))`
  - Next child document should start with `load(here::here("inst", "report",
   "state_after_childX.RData"))`

## R packages for Ubuntu

Ubuntu Compiled R Package Libraries for Cross-Environment Compatibility:
To enable full statistical analysis capabilities including mixed-effects
modeling and advanced plotting, Ubuntu-compiled R packages are stored at
@/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages/
and accessed via .libPaths() configuration. This directory contains over
200 compiled R packages including critical dependencies that require
system-level compilation (nloptr, lme4, effects, emmeans, zoo,
RcppArmadillo) which cannot be easily installed in restricted environments
 due to cmake and system library requirements. The Ubuntu packages are
fully compatible across similar Linux environments and can be activated by
 prepending the library path: .libPaths(c('/mnt/c/Users/mrkma/OneDrive/DKM
/Stats_R/R/_Libraries/_Ubuntu_packages', .libPaths())) before loading
packages. This approach enables complete statistical workflows including
lme4::lmer() mixed-effects models, emmeans::emmeans() contrasts,
zoo::na.locf() last-observation-carried-forward imputation, and
effects::predictorEffects() visualization without requiring admin
privileges or system-level package compilation. The compiled libraries
maintain full functionality across different computational environments
while preserving reproducibility and ensuring consistent statistical
analysis capabilities between development scripts (inst/iter1.r,
inst/iter2.r) and production report generation workflows.


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
- Models: Trained models saved in `inst/function/backend/` for reuse across scripts

### Exported Functions

Core utilities (see NAMESPACE):
- `create_meter_fun()`, `approx_rate()`: Gas meter data processing
- `simulate_weather()`, `simulate_corr_resids()`: Weather simulation
- `maketsum()`, `Maketsum()`: Temperature sum calculations
- `load_all_Rdata()`: Batch data loading
- `merge_transform_weather()`: Weather data preprocessing

### Script Chronology

The `iter*.r` scripts represent the project evolution:
- iter2-3: Basic data cleaning and exploration
- iter4: Sinusoidal temperature modeling
- iter5: Heating season forecasting with scenarios
- iter6: Advanced modeling with transformations
- iter7-8: Linear/GLS models and residual analysis
- iter9: Integration of updated weather data

Each script builds on previous work while exploring new modeling approaches.

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