# MartysGas Object Lifecycle Roadmap

This document traces the "birth and death" of key R objects throughout the MartysGas codebase, documenting where objects are created, transformed, used, and saved across the various analysis scripts.

## Main Iteration Scripts (`iter*.r`)

### iter2.r - Foundation Script
- **maketsum() is born** - Temperature sum calculation function created
- **Maketsum() is born** - Capitalized variant of temperature sum function
- **gaz_rendetlen is born** - Main gas consumption dataset with basic transformations
- **mod is born** - Linear model: `Rate ~ ns(tact, df=1) + ywint + heat_off`  
- **nd is born** - New data grid for temperature effect predictions
- **gaz_rendetlen begets multiple plots** - Various ggplot visualizations using main dataset
- **mod is used for prediction** - Model used to generate temperature effect plots

### iter3.r - Weather Exploration  
- **weather is born** - Weather dataset with derived variables (`range`, `tsum`, `dme`)
- **mod is born** - Sinusoidal + spline model: `tavg ~ sin(2*pi*datenum/365.25) + ns(dme)`
- **preds is born** - Comprehensive prediction grid (days × hours × years)
- **weather begets smoothed variables** - `tavg_smooth` and `range_smooth` created
- **preds is saved** - `save(preds, temps_xtra, file = "pred_temps.rdata")`

### iter4.r - Heating Season Planning
- **gaz is born** - Gas meter readings from Excel import
- **data is born** - Merged weather and gas consumption dataset  
- **dat_comp is born** - Complete cases subset of merged data
- **mod_spline is born** - Daily temperature spline model
- **preds is born** - Detailed prediction framework with multiple scenarios
- **const_0, const_5, const_10, const_15, const_20 are born** - Gas consumption scenario constants
- **data begets complex visualizations** - Multi-scenario forecast plots
- **pred_rate.rdata is loaded** - External rate predictions incorporated
- **jelleggorb is used** - Monthly consumption patterns applied

### iter5_rezsicsokk.r - Comprehensive Analysis
- **USED_UP, KEDV_HATRA are born** - Gas consumption tracking variables
- **temps_xtra_nice is born** - Filtered temperature dataset (removes outliers)
- **fig_1, fig_2 are born** - GAM-based visualization objects
- **mod is born** - Complex interaction model with splines and winter day effects
- **preds is born** - Comprehensive hourly prediction grid
- **mod_temp is born** - Temperature prediction model with sine curves
- **pred_realiz, pred_corrected are born** - Reality-adjusted predictions
- **fig_3, fig_4, fig_5_ojj, fig_6 are born** - Various forecast visualizations
- **mod_b is born** - Alternative linear model for comparison
- **nd is born** - New data for alternative model predictions
- **Complete workspace is saved** - `save.image(file = "iter6_mods.rdata")`

### iter6_mods_bu.r - Alternative Implementation
- **temps_xtra_nice is reborn** - Different filtering criteria applied
- **mod is reborn** - Different formula using `ns(id, df = 2)` instead of `day_in_wint`
- **preds is reborn** - Alternative prediction approach with join operations
- **heat_off logic changes** - Modified to use `tavg > 20` threshold

### iter7.r - Statistical Rigor
- **act_year is born** - Current year parameter (2023)
- **obs_days_mod, obs_hours_mod are born** - Modified observational datasets
- **mod_readings, mod_days, mod_hours are born** - Multiple temporal resolution models
- **mod_days_gls is born** - GLS model with AR(1) correlation structure
- **pr is born** - Prediction dataset for model comparison
- **Residual analyses are performed** - ACF and diagnostic plots created
- **obs_days_mod begets correlation analysis** - Temporal correlation structures explored

### iter8.r - Cumulative Heat Modeling
- **get_approx_meter(), get_approx_rate() are born** - Interpolation functions
- **get_avg_temp() is born** - Temperature averaging function  
- **obs_days_complete is born** - Complete daily time series via interpolation
- **mod_cum is born** - NLME logistic growth model for heating patterns
- **mod_cum is reborn** - GLS version with variance modeling
- **sds is born** - Standard deviation modeling dataset
- **fig_* objects are born** - Multiple visualization objects for heating analysis
- **obs_days_complete begets cumulative calculations** - Progressive heating degree tracking

### iter9.r - Integration & Forecasting
- **act_year, act_ywint are born** - Updated current year parameters (2024)
- **obs_days_complete is reborn** - Most sophisticated complete dataset
- **tavgcum_exp, tavgcum_exp_sd are born** - Expected cumulative temperature parameters
- **cors, sds are born** - Correlation and standard deviation arrays
- **mod_tavg_low_cum_mean, mod_tavg_low_cum_sd are born** - Advanced prediction models
- **spent_tavg_*, gas_left_* are born** - Comprehensive forecasting variables
- **Real-time dashboard outputs** - Advanced consumption efficiency visualizations

## Specialized Modeling Scripts

### just_model/iter1.r - Tropical Year Modeling
- **YR_TRP is born** - Tropical year length constant (365.24217)
- **START_DAY is born** - Reference date for tropical year calculations
- **tropical_year() is born** - Function for astronomical calendar components
- **plot_to_obj() is born** - Wrapper function for plot capture
- **dat is born** - Main dataset enhanced with tropical year components
- **mod is born** - Linear model using natural splines on tropical components
- **mod_lme is born** - Mixed effects version with random effects by `ywint`
- **dat_pred_2024 is born** - Prediction dataset for 2024
- **fig_1 through fig_7 are born** - Comprehensive visualization suite
- **Complete workspace is saved** - `save.image(file = "iter1.rdata")`

### just_model/iter2.r - Model Optimization
- **iter1.rdata is loaded** - All objects from iter1.r inherited
- **predict_nxt_n_month() is born** - Forward prediction function
- **return_rmse_lme() is born** - Model validation function
- **out is born** - Grid search optimization results
- **dividers is born** - Time split sequence for cross-validation
- **df_optimization is born** - Final optimization results
- **df_optimization is saved** - `save(df_optimization, file = "backend/df_optimization.rdata")`

### just_model/iter3.r - Production Predictions
- **dividers is reborn** - Time split points for rolling prediction
- **out is reborn** - Accumulated prediction results across periods
- **out_rmses is born** - RMSE calculations grouped by winter year
- **mod_final is born** - Final LME model fitted on all data
- **df_optimization is used** - Optimal parameters applied in production

### just_model/new_model.r - Alternative Implementation
- **START_DAY is reborn** - Different reference date ("2000-03-20")
- **predict_nxt_3_months() is born** - Simplified prediction function
- **Streamlined tropical year workflow** - More direct implementation approach

## Backend Infrastructure

### function/backend/mod_tavg.r
- **met_small is born** - Alias for `meteostat_weather`
- **mod_tavg is born** - GLS model: `tavg ~ ns(day_in_year, df=4)` with AR(1)
- **mod_tavg is saved** - `save(mod_tavg, file = "mod_tavg.Rdata")`

### function/backend/mod_range.r  
- **met_small is born** - Alias for `meteostat_weather`
- **mod_range is born** - GLS model: `range ~ ns(day_in_year, df=4)` with AR(1)
- **mod_range is saved** - `save(mod_range, file = "mod_range.Rdata")`

### function/backend/weather_simulated_100ys.r
- **weather_simulated is born** - 100-year simulated weather dataset (36,500 days)
- **simulate_weather() is used** - R/ directory function applied
- **weather_simulated is saved** - `save(weather_simulated, file = "weather_simulated.Rdata")`

## Alternative Approaches

### iters/arima.r - Time Series Modeling
- **dayinyr_to_dayinwint(), dayinwint_to_dayinyr() are born** - Date conversion functions
- **mean_temperature_ts is born** - Time series object with frequency=180
- **tbats_model is born** - TBATS model for complex seasonality
- **arima_model is born** - Auto-selected ARIMA model  
- **mod_spl is born** - GLS model combining hourly and daily components
- **simulate_year() is born** - Autocorrelated residual simulation function
- **pr is born** - Prediction grid with simulated ensemble values
- **qr_90 is born** - Quantile regression models (5th and 95th percentiles)
- **Time series decomposition plots** - ARIMA/TBATS forecast visualizations
- **Ensemble uncertainty bands** - Simulated weather variability displays

### iters/arima_v2.r - Refined Time Series
- **iter6_mods.rdata is loaded** - Baseline data imported
- **mod_spl is reborn** - Simpler time series implementation
- **30-replication ensemble** - Focused simulation approach
- **Streamlined visualization** - More targeted forecast displays

### iters/alter_plot.r - ARIMA Duplicate
- **Identical to arima.r** - Same objects and workflow (backup version)

## Cross-File Object Dependencies

### Data Flow Chains
1. **Raw Data** → `meteostat_weather` (loaded via load_stuff.r) → **Enhanced datasets across all scripts**
2. **Gas Data** → `gaz.xlsx` → `gaz_rendetlen`/`obs_readings` → **Processed consumption data**
3. **Backend Models** → `mod_tavg`, `mod_range` → **Weather simulation** → `weather_simulated`
4. **Optimization Results** → `df_optimization` → **Production parameters in iter3.r**

### Persistent Object Evolution
- **Temperature Data**: `temps_xtra` → `weather` → `obs_days` → `obs_days_complete`
- **Gas Models**: Simple `mod` → Complex interaction models → GLS with correlation → NLME growth models
- **Predictions**: Basic `nd` grids → Comprehensive `preds` matrices → Scenario ensembles
- **Visualizations**: Unnamed plots → `fig_*` objects → Dashboard-style displays

### Save/Load Patterns
- **iter3.r saves** → `pred_temps.rdata` → **Used in iter4.r**
- **iter5.r saves** → `iter6_mods.rdata` → **Used in arima_v2.r**  
- **iter1.r saves** → `iter1.rdata` → **Used in iter2.r**
- **iter2.r saves** → `df_optimization.rdata` → **Used in iter3.r**
- **Backend saves** → Model .Rdata files → **Used by simulation functions**

## Key Insights

### Object Lifecycle Patterns
1. **Progressive Refinement**: Objects evolve from simple to complex across iterations
2. **Parallel Development**: Multiple approaches to same problems in different directories  
3. **Selective Persistence**: Only critical objects saved between scripts
4. **Functional Evolution**: Functions developed in one script, refined in others

### Modeling Philosophy Evolution
- **iter2-4**: Linear and spline-based approaches
- **iter5-6**: GAM and interaction modeling  
- **iter7-8**: Statistical rigor with correlation structures
- **iter9**: Comprehensive forecasting integration
- **just_model/**: Astronomical precision with tropical year modeling
- **iters/**: Time series sophistication with ARIMA/TBATS

### Data Management Observations
- Inconsistent intermediate saving leads to long script dependencies
- Object naming becomes more systematic in later iterations
- Backend infrastructure supports reusable model components
- Alternative approaches maintain separate object namespaces

This roadmap reveals a sophisticated evolution from basic temperature-consumption modeling to comprehensive forecasting systems, with objects progressively gaining complexity and predictive power throughout the iterative development process.