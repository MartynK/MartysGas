# Alternative Weather Modeling Approaches

These scripts explore weather modeling techniques that are **not part of the main iter1-iter6 pipeline** but are runnable standalone and produce their own .Rdata outputs.

## tropical_year.r (Runnable)

Mixed-effects model using tropical year (365.24217 days) parameterization with sinusoidal components. Fits both LM and LME (random effects by winter year). Produces excellent temperature predictions. Run with `source(here::here("inst", "function", "load_stuff.r"))` first.

Saves: `data/tropical_year.Rdata`

## arima_v2_reference.r (Reference Only)

Refined ARIMA/TBATS experiment with 30-replication ensemble simulation. The core simulation logic was later absorbed into `R/simulate_corr_resids.r` and `R/simulate_weather.r`. Kept as reference for the approach.
