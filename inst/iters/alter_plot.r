# Trying to simulate weather and learning about quantile regression in the meantime
# DEPENDSON: "make_weather_csv.r"


here::here( "make_weather_csv.r") %>% source

library(forecast)
library(splines)
library(dplyr)
library(nlme)
library(ggplot2)


dayinyr_to_dayinwint <- function(d) {
  
  ifelse( d < 213, 
            d + 365 - 213,
            d - 213)
}

dayinwint_to_dayinyr <- function(d) {
  
  ifelse( d < 152, 
          d + 213, # its after August
          d - 152) # its before August
}

mod_spl <- gls( tact ~ 
                  ns( day_in_wint, df = 4) 
                + ns( hours_dat, df = 2) 
               ,
               data = temps_xtra[
                            sample(1:nrow(temps_xtra), size = 1000)
                            ,], # serious impact on runtime
               correlation = corAR1(value = .9,
                                    form = ~ tim|year))

resid(mod_spl,type = "normalized") %>% acf

mod_spl %>% effects::predictorEffects(
  #partial.residuals = TRUE
  ) %>% plot

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
plot(simulate_year(), type = 'l')


pr <- expand.grid( 
  day_in_year = 1:365,
  rep = 1:10,
  hours_dat = c(1,5,9,12,17,21))
pr$day_in_wint <- dayinyr_to_dayinwint( pr$day_in_year)
pr$pred_fixed <- predict(mod_spl, newdata = pr)
pr$pred <- 0


# Create the progress bar
progress_bar <- txtProgressBar(min = 1, max = (nrow(pr)/365), style = 3)

for (i in 1:(nrow(pr)/365)) {
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

qr_90 <- rq( tavg ~ ns( day_in_year, df = 4), 
             data = data, 
             tau = c(.05,.95))

summary(qr_90)
plot(qr_90$fitted.values)

pr$pred_q05 <- predict(qr_90, newdata = pr)[,1]
pr$pred_q95 <- predict(qr_90, newdata = pr)[,2]


pr %>%
  ggplot(aes(x = day_in_wint, y = pred_fixed)) +
    geom_line(size= .1)

pr %>%
  ggplot( aes( x = day_in_wint, y = pred, group = rep)) +
  theme_minimal() +
  geom_line(alpha = .1, color = "grey50", size = .1) +
  geom_line( data = pr %>% filter( hours_dat == 13),
    mapping=aes(y=pred_fixed), color = "red") +
  geom_line( mapping=aes(y=pred_q05), color = "cyan") +
  geom_line( mapping=aes(y=pred_q95), color = "cyan")


data <-
  data %>%
    group_by(ywint) %>%
    arrange(day_in_wint) %>%
    mutate(sum_gas_pred = cumsum(Rate),
           sum_gas_perc = ifelse( ywint == 2022,
                                  sum_gas_pred / 1730,
                                  sum_gas_pred / sum(Rate, 
                                             na.rm = TRUE)),
           sum_gas_renorm = sum_gas_perc * 1730)

data %>%
  filter( is.na(sum_gas_pred) == FALSE) %>%
  ggplot( aes( x = day_in_wint, 
               y = sum_gas_renorm, 
               color = as.factor(year))) +
    theme_minimal() +
    geom_point() +
    scale_y_continuous(breaks=c(0,1730/2,1300,1500,1600,1700,1730)) +
    geom_smooth(mapping = aes(group=NULL, color = NULL)) +
    geom_vline( xintercept = yday(today()) + 152,
                color = "red")

 