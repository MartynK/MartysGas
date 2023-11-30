library(readxl)
trial <- read_excel("trial.xlsx", na = "NA")
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
