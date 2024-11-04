library(nlme)
library(splines)

source( here::here( "inst", "function", "load_stuff.r"))

act_year <- 2024

get_approx_meter <-  approxfun(obs_readings$Date,
                               obs_readings$Value_trf, 
                               rule = 2, na.rm = TRUE)

get_approx_rate <- function(x) {
  # dx get_approx_meter
  h <- 1e-6
  return((get_approx_meter(x + h) - get_approx_meter(x)) / h)
}

get_avg_temp <- function( df = obs_days, var = "tavg", var_time = "Date",
                          xmin, xmax) {
  f <- approxfun( df[var_time][[1]], df[var][[1]])
  integrated <- integrate( f, subdivisions = 10000,
                           #stop.on.error = FALSE,
                           rel.tol = 0.1,
                           lower = xmin, upper = xmax)$value
  duration <- xmax - xmin
  return( integrated / duration)
}


obs_days <- obs_days %>%
  mutate(Meter = get_approx_meter(Date),
         Rate = get_approx_rate(Date)*3600*24) %>%
  group_by(ywint) %>%
  mutate( Spent = Meter - min(Meter, na.rm = TRUE),
          Spent_perc = ifelse( year(Date) == act_year,
                               Spent / 1730,
                               Spent / max(Spent, na.rm = TRUE)))





# Create a complete date sequence
complete_dates <- 
  data.frame(Date = seq(from = min(obs_days$Date), to = max(obs_days$Date), by = "day"))

# Step 2: Expand your dataframe by left.join, missings are in NA
obs_days_complete <- 
  left_join(complete_dates, obs_days,by="Date")


# Fix the missings column by column
for (i in 2:ncol(obs_days_complete)) {  # Starting from 2 to skip the Date column
  
  # Extract dates and values for the current column
  dat_act <- obs_days_complete$Date
  
  # handle ywint after numeric transformation
  if( colnames(obs_days_complete)[i] == "ywint") { 
    val <- as.numeric(obs_days_complete[, i])
  } else {
    val <- obs_days_complete[, i]
  }
  
  # Create the interpolation function only for non-NA values
  valid_idx <- !is.na(val)
  if(sum(valid_idx) > 1) {  # Ensure there are at least two points for interpolation
    fun. <- approxfun(x = dat_act[valid_idx], y = val[valid_idx], rule = 2)
    
    # Apply the function to interpolate NA values
    obs_days_complete[, i] <- ifelse(is.na(val), fun.(dat_act), val)
  }
}


# Cap the tavg at 20
obs_days_complete <- obs_days_complete %>%
  arrange(Date) %>%
  mutate(tavg_capped = ifelse(tavg < 20, tavg, 20),
         tavg_low_cumul = 0)

# Calculate the cumulative heating need as the 'missing' degrees * days 
pb <- txtProgressBar(style=3)
for (i in 2:nrow(obs_days_complete)) {
  if ( obs_days_complete$day_in_wint[i] >= obs_days_complete$day_in_wint[i-1]) {
    obs_days_complete$tavg_low_cumul[i] <- 
      obs_days_complete$tavg_low_cumul[i - 1] - obs_days_complete$tavg_capped[i] + 20
  } else {
    # in case of a new year, its automatically zero
    obs_days_complete$tavg_low_cumul[i] <- obs_days_complete$tavg_capped[i] - 20
  }
  setTxtProgressBar(pb,i/nrow(obs_days_complete))
}
close(pb)

# back transforming to factor
obs_days_complete <- obs_days_complete %>%
  mutate(ywint = as.factor( ywint)) %>%
  group_by(ywint) %>%
  mutate( tavg_low_cum_ratio = tavg_low_cumul / max(tavg_low_cumul, na.rm = TRUE))

# didnt observe the first year from the start,results not correct
obs_days_complete$tavg_low_cumul[obs_days_complete$ywint==1] <- NA 
# Few obviously wrong values, set to NA (0 heat need at a day in the middle of winter)
obs_days_complete$tavg_low_cumul[obs_days_complete$day_in_wint > 180 &
                                   obs_days_complete$tavg_low_cumul < 100] <- NA
# remove some problematic intrapolated values where day_in_wint is non integer
obs_days_complete$tavg_low_cumul[obs_days_complete$day_in_wint %% 1 != 0] <- NA
# remving NA values so that the model can be fitted easily
obs_days_complete <- obs_days_complete %>% filter(is.na(tavg_low_cumul) == FALSE)




fig_heatneed_per_year <- 
  obs_days_complete %>%
  ggplot(aes(x = day_in_wint, 
             y = tavg_low_cum_ratio
  )) +
  theme_bw() +
  geom_line(alpha=.7, mapping = aes(
    color = year
    #,fill=year
    ,group = factor(ywint))) +
  labs( x = "Day in the season (starts Aug.1st)",
        y = "'Missing degrees until 20' x Days ")


