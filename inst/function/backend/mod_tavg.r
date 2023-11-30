library(nlme)
library(dplyr)
library(splines)

load(here::here("data","meteostat_data.Rdata"))

met_small <- meteostat_weather#[1:4000,]

mod_tavg <- gls( tavg ~ ns( day_in_year,df = 4),
                 #random = ~1|year,
                 data = met_small, # serious impact on runtime
                 correlation = corAR1(value = .8,
                                      form = ~ day_in_year|year))

save(mod_tavg, file = here::here("inst","function","backend","mod_tavg.Rdata"))