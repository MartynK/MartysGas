library(dplyr)
library(lubridate)
library(splines)

load("tempsextra.rdata")

temps_xtra <- temps_xtra %>%
  mutate( day_in_year = yday(date),
          datetim = ymd_h( paste(date, hour)))

# silly idea, I added the sinus, now I subtract it
mod <- lm( temp ~ I(sin(hour/24*3.14159)) + ns(day_in_year,df=5),temps_xtra)

#mod %>% effects::predictorEffects() %>% plot

preds <- expand.grid( day_in_year = 1:365,
             hour = 1:24,
             years = 1990:2024)

preds$pred <- predict(mod, newdata = preds)

save(preds, temps_xtra, file = "pred_temps.rdata")
