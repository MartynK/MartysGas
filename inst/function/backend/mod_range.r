library(nlme)
library(dplyr)
library(splines)

load(here::here("data","meteostat_data.Rdata"))

met_small <- meteostat_weather#[1:5000,]

mod_range <- gls( range ~ ns( day_in_year, df = 4),
                  data = met_small, # serious impact on runtime
                  correlation = corAR1(value = .5,
                                       form = ~ day_in_year|year))

save(mod_range, file = here::here("inst","function","backend","mod_range.Rdata"))