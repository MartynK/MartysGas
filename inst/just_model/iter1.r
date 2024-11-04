# Load packages
library(dplyr)
library(lubridate)
library(splines)
library(splines2)
library(ggplot2)
library(nlme)

# Load 'meteostat_weather' & 'obs_day' data
load( here::here("data","meteostat_data.Rdata")) 

# remove everything except meteostat_weather
rm(list = setdiff(ls(), "meteostat_weather"))


# Length of tropical year in days
YR_TRP <- 365.24217
# start the numbering when its the vernal equanox
START_DAY <- "2000-08-31"

# Function to calculate the tropical year related columns
tropical_year <- function(date
                          # , YR_TRP = YR_TRP
                          # , START_DAY = START_DAY
) {
  diff_day0 <- as.numeric(difftime(date, ymd(START_DAY), units = "days"))
  tropical_year <- diff_day0 / YR_TRP
  tropical_year_loc <- tropical_year - floor(tropical_year)
  tropical_year_sin_component <- sin(2*pi*(tropical_year_loc - (yday(START_DAY)*1) / YR_TRP)) + 1
  return(data.frame(diff_day0 = diff_day0,
                    tropical_year = tropical_year,
                    tropical_year_loc = tropical_year_loc,
                    tropical_year_loc_sin = cos(2*pi*(tropical_year_loc - yday(START_DAY) / YR_TRP)) + 1,
                    tropical_year_sin_component = tropical_year_sin_component))
}

# wrapper for plot() so it would end up in a  variable
plot_to_obj <- function(p) {
  # make the plot
  plot(p)
  # Capture the plot
  out <- recordPlot()
  # Close the null device
  dev.off()
  
  return(out)
}

# Make the main data frame
dat <- bind_cols( meteostat_weather, 
                  tropical_year(meteostat_weather$Date))

mod <- lm(tavg ~ 
            ns(tropical_year_loc_sin, df = 1)
          + ns(tropical_year_sin_component, df = 3)
          , data = dat)

#mod %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot() %>% try()
fig_1_effectplot <- mod %>% 
  effects::predictorEffects(partial.residuals = FALSE) %>% 
  plot_to_obj() %>% 
  try() 

car::vif(mod) %>% try()
anova(mod)


# Define the start date for the prediction
start_date <- as.Date("2024-01-01")

# Generate a sequence of 365 days
date_sequence <- seq.Date(from = start_date, by = "day", length.out = 365)

# Generate the tropical year related columns for the prediction
dat_pred_2024 <- tropical_year(date_sequence) %>% 
  mutate(Date = date_sequence)

dat_pred_2024 $pred <- predict(mod, newdata = dat_pred_2024 )

fig_2_prediction <-
  ggplot(dat_pred_2024 , aes(x = Date, y = pred, 
                 color = tropical_year_sin_component )) +
    geom_line() +
    geom_point( ) +
    geom_vline(xintercept = as.Date("2024-03-20"), linetype = "dashed") +
    theme_minimal()

fig_3_sin_components <-
  dat_pred_2024 %>%
    ggplot(aes(x = Date, y = tropical_year_sin_component)) +
    geom_line(color = "blue") +
    geom_line(aes(y = tropical_year_loc_sin), color = "red") +
    theme_minimal() +
    # text where colors are explained
    labs(caption = "Blue: sin component\nRed: loc_sin component",
         y = "Value")
fig_3_sin_components


#######################
# LME model

mod_lme <- lme(tavg ~ 
                 ns(tropical_year_loc_sin, df = 1)
               + ns(tropical_year_sin_component, df = 3)
               , data = dat 
               , random = ~ ns(tropical_year_loc_sin,df=1)[,] | ywint
               , control = lmeControl(msMaxIter = 200)
               )

BIC(mod_lme)

# plot the ranefs
try({
  fig_4_ranef <- mod_lme %>% ranef() 
  fig_4_ranef <- fig_4_ranef %>% plot
})

fig_5_effectplot_lme <-
  mod_lme %>% effects::predictorEffects() %>% plot_to_obj() %>% try()

# plot the predictions of the model
dat$pred_lme <- predict(mod_lme)

fig_6_prediction_lme <-
  ggplot(dat, aes(x = yday(Date), y = pred_lme, 
                   color = as.numeric(ywint),
                   group = ywint )) +
    geom_line() +
    theme_minimal() +
    labs(color = "Years",
         x = "Day in year",
         y = "Predicted temperature")


# include the new predictions in dat_pred_2024 and plot it
dat_pred_2024$pred_lme <- predict(mod_lme, newdata = dat_pred_2024, level = 0)

fig_7_prediction_2024_lme <-
  ggplot(dat_pred_2024, aes(x = Date, y = pred_lme, 
                            color = tropical_year_sin_component )) +
    geom_line(linewidth=2) +
    # include old preds also
    geom_line(aes(y = pred), color = "grey") +
    geom_vline(xintercept = as.Date("2024-03-20"), linetype = "dashed") +
    theme_minimal() +
    labs(title = "Prediction for 2024",
         caption = "grey: old prediction\nblue: new prediction",
         x = "Date",
         y = "Predicted temperature")

fig_7_prediction_2024_lme

save.image(here::here("inst","just_model","iter1.rdata"))