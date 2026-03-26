# This was a randomized trial where I would construct a randomlist
# Whether to close the outer door to my house at night and see the amount of gas
# I would use per day and to note any change in gas consumption
# Correcting for the actual temperature outside

library(readxl)
trial <- read_excel(here::here("inst", "trial.xlsx"), na = "NA")
#View(trial)
trial$date <- as.Date(trial$date)

# Call:
#   lm(formula = dat_e$tavg[dat_e$ywint == "2021"] ~ dat_e$fit[dat_e$ywint == 
#                                                                "2021"])
# 
# Coefficients:
#   (Intercept)  dat_e$fit[dat_e$ywint == "2021"]  
# 22.38                             -2.59  

trial$expected <- 22.38 - 2.59 * trial$tave

cor(trial[,-3], use = "complete.obs")

mod <- lm( rate ~ tave + trt, data = trial)

car::vif(mod)

summary(mod)

plot( effects::predictorEffects(mod, partial.residuals =TRUE))
