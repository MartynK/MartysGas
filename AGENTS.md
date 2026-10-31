# Repository Guidelines

## Project Structure & Module Organization
- `R/`: Core package functions used across scripts.
- `inst/`: Analysis scripts and supporting assets.
- `inst/function/load_stuff.r`: Common loader that attaches packages, sources `R/`, and loads saved data.
- `inst/iter*.r`: Chronological modeling iterations and experiments.
- `inst/extdata/`: Raw input data (gas readings, weather data).
- `data/`: Processed `.RData` objects saved for reuse.
- `tests/`: `testthat` tests (`tests/testthat/` + `tests/testthat.R`).
- `vignettes/`: Package documentation.

## Build, Test, and Development Commands
- `R -e "devtools::check()"`: Run package checks (build + tests).
- `R -e "devtools::build()"`: Build the package.
- `R -e "devtools::test()"`: Run all tests.
- `R -e "testthat::test_check('MartysGas')"`: Test via `testthat` directly.
- `R -e "devtools::build_vignettes()"`: Build vignettes.
- Common script setup:
  - `source(here::here("inst", "function", "load_stuff.r"))`

## Coding Style & Naming Conventions
- Indentation: consistent, prefer 2 spaces.
- Use `<-` for assignment and space around operators.
- Prefer magrittr pipes `%>%` over native `|>`.
- Variable naming: `snake_case`.
- Object prefixes: `dat_` (data frames), `mod_`/`model_` (models), `fig_`/`plot_` (plots), `lst_` (lists), `vec_` (vectors).
- Keep files under ~400–500 lines; split and save state to `data/` when needed.

## Testing Guidelines
- Framework: `testthat`.
- Tests live in `tests/testthat/`; follow `test-*.R` naming.
- Run tests with `devtools::test()` or `testthat::test_check("MartysGas")`.

## Commit & Pull Request Guidelines
- Commit history is informal (e.g., “updated data…”, “iter2.r runs now”).
- Use short, descriptive messages focused on the change outcome.
- PRs (if used) should include:
  - Purpose summary and key files touched.
  - Any data or model artifacts added to `data/`.
  - Runtime notes for heavy scripts (e.g., `inst/iter8.r`, `inst/iter9.r`).

## Data, Reports, and Environment Notes
- Reports are generated via Quarto from `inst/report/report.qmd`.
- Large computations should persist results to `data/` and be re-loadable.
- Optional Ubuntu R packages live at:
  - `/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages`
  - Activate with: `.libPaths(c('/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages', .libPaths()))`

## Carbon monoxide logger (since 2026-10-31)
- `inst/extdata/co_logger/`: raw Lascar EL-USB-CO EasyLog exports from the bathroom next to the Bosch Therm 4200 open-flue water heater. Never edit; add new exports beside them. The logger holds 32,510 readings and stops when full (v5 filled 2026-10-31 00:43).
- `R/read_co_logger.R`: stacks the exports; logger clock is parsed as naive time (no DST).
- `inst/co1_events.r` (sources `inst/co1b_cohb_and_figures.r`): events (>= 10 ppm, merged within 10 min), per-event peak, dose, decay rate, worst-case COHb by CFK; like-for-like event rates at 60 s; daily baseline outside events. Output `data/co_events.Rdata`, `inst/sideffects/co_*.png`, `inst/sideffects/co_events.csv`.
- History and reasoning: `JOURNAL.md`.
