# Trying to simulate weather and learning about quantile regression in the meantime

library(forecast)
library(splines)
library(dplyr)
library(nlme)
library(ggplot2)

load( here::here( "inst", "iter6_mods.rdata"))

# Create a time series object with your mean temperature data
mean_temperature_ts <- ts(data$tavg, frequency = 180) # Assuming daily data

# Doing the decomposition as a first step, a bit problematic
mean_temperature_ts %>% decompose() %>% plot

# This is beyond (S)ARIMA, but it screws up (also)
tbats_model <- tbats(mean_temperature_ts,
                     use.box.cox = FALSE,
                     use.trend = FALSE)
summary(tbats_model)

# Set the number of future time points to simulate (e.g., 365 days)
n_future <- 365

# Forecast the future values
forecasted_values <- forecast(tbats_model, h = n_future)
plot(forecasted_values)

# Fit the ARIMA model
# Automatically select the best ARIMA model using the auto.arima() function
arima_model <- auto.arima(mean_temperature_ts,
                          max.p = 3,
                          max.q = 3,
                          start.p = 0,
                          start.q = 0,
                          #stepwise = FALSE, # SERIOUS performance impact
                          seasonal = TRUE)
summary(arima_model)

# Set the number of future time points to simulate (e.g., 365 days)
n_future <- 365

# Forecast the future values
forecasted_values <- forecast(arima_model, h = n_future)
plot(forecasted_values)

# Alternatively, simulate future values
simulated_values <- simulate(arima_model, n_future)
plot(simulated_values)


mod_spl <- gls( tavg ~ ns( day_in_year, df = 4),
               data = meteostat_weather[1:3000,], # serious impact on runtime
               correlation = corAR1(value = .8,
                                    form = ~ day_in_year|year))

resid(mod_spl,type = "normalized") %>% acf

mod_spl %>% effects::predictorEffects() %>% plot

simulate_year <- function( n = 365, 
                           autocorrelation = coef(mod_spl$modelStruct$corStruct, 
                                                  unconstrained = FALSE),
                           total_sd = summary(mod_spl)$sigma
) {
  #### Simulating a year
  # Create covariance matrix
  cov_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      cov_matrix[i, j] <- (autocorrelation ^ abs(i - j)) * (total_sd ^ 2)
    }
  }
  
  # Simulate the multivariate normal data
  simulated_resids <- mvtnorm::rmvnorm(1, mean = rep(0, n), 
                                       sigma = cov_matrix) %>% t
  
  return(simulated_resids)
}


# Check the autocorrelation of the simulated data
acf(simulate_year())
plot(simulate_year(), type = 'l')b# not the same data, invoking again


pr <- expand.grid( 
  rep = 1:30,
  day_in_year = 1:365)
pr$pred_fixed <- predict(mod_spl, newdata = pr)
pr$pred <- 0


# Create the progress bar
progress_bar <- txtProgressBar(min = 1, max = max(pr$rep), style = 3)

for (i in 1:max(pr$rep)) {
  pr$pred[((i-1)*365+1):(i*365)] <- simulate_year()
  
  # Update the progress bar
  setTxtProgressBar(progress_bar, i)
}

# Close the progress bar
close(progress_bar)

pr$pred <- pr$pred + pr$pred_fixed



data %>%
  ggplot( aes( x = day_in_year, y = tavg, group = year)) +
  theme_minimal() +
  geom_line(alpha = .1, color = "grey50") +
  geom_smooth(mapping = aes( x = day_in_year, y = tavg, group = NULL))

# És amit kerestem idáig...
library(quantreg)

qr_modf <- rq( tavg ~ ns( day_in_year, df = 4), 
             data = data, 
             tau = c(.05,.95))

summary(qr_modf)
plot(qr_modf$fitted.values)

pr$pred_q05 <- predict(qr_modf, newdata = pr
                       )[,1]
pr$pred_q95 <- predict(qr_modf, newdata = pr)[,2]


pr %>%
  ggplot( aes( x = day_in_year, y = pred, group = rep)) +
  theme_minimal() +
  geom_line(alpha = .1, color = "grey50") +
  geom_line( mapping=aes(y=pred_fixed), color = "red") +
  geom_line( mapping=aes(y=pred_q05), color = "cyan") +
  geom_line( mapping=aes(y=pred_q95), color = "cyan")



