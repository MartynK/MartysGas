library(nlme)
library(splines)

source( here::here( "inst", "function", "load_stuff.r"))

act_year <- 2023

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



date_min <- as.numeric(min(obs_readings$Date, na.rm = TRUE))
date_max <- as.numeric(max(obs_readings$Date, na.rm = TRUE))

appfun <- data.frame(
  date_series = seq(date_min,date_max, length.out = 1000) %>% 
                   as_datetime(),
  out_series = NA) %>% 
  mutate( out_series = get_approx_meter(date_series))


# figure exploring approx.fun
fig_approxfun_gascons <- 
  appfun %>%
    ggplot(aes(x=date_series,y=out_series)) +
      theme_bw() +
      geom_line() +
      geom_point(data = obs_readings,
                 mapping = aes(x = Date,
                               y = Value_trf),
                 color = "red") +
      labs(y = "Amount gas used (m3)",
           x = "Date")


obs_hours <- obs_hours %>%
  mutate(Meter = get_approx_meter(Date),
         Rate = get_approx_rate(Date)*3600*24) %>%
  group_by(ywint) %>%
  mutate( Spent = Meter - min(Meter, na.rm = TRUE),
          Spent_perc = ifelse( year(Date) == act_year,
                               Spent / 1730,
                               Spent / max(Spent, na.rm = TRUE)))



# Day-in_year, spent percent graph, based on days
fig_dayinyear_days <- 
  obs_days %>%
  filter(is.na(Spent_perc) == FALSE) %>%
  ggplot( aes( x = day_in_year, y = Spent_perc,
               group = ywint,
               color = ywint)) +
  theme_bw() +
  scale_x_continuous(breaks = c(0,90,180,270,365)) +
  geom_point()

# Day-in_year, spent percent graph, based on hours (takes longer)
fig_dayinyear_hours <- 
  obs_hours %>%
  filter(is.na(Spent_perc) == FALSE) %>%
  ggplot( aes( x = day_in_year, y = Spent_perc,
               group = ywint,
               color = ywint)) +
  theme_bw() +
  scale_x_continuous(breaks = c(0,90,180,270,365)) +
  geom_point()


# arrange so that the two plots can be compared (not much diff.)
fig_compare_day_hours_spentperc <- 
 ggpubr::ggarrange(fig_dayinyear_days,fig_dayinyear_hours,nrow = 2)



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
  mutate(ywint = as.factor( ywint))

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
    ggplot(aes(x = day_in_wint, y = tavg_low_cumul
    )) +
    theme_bw() +
    geom_line(alpha=.7, mapping = aes(
      color = year
      ,fill=year
      ,group = factor(ywint))) +
    labs( x = "Day in the season (starts Aug.1st)",
          y = "'Missing degrees until 20' x Days ")

library(lme4)
dat_days_complete <- obs_days_complete %>% filter(is.na(tavg_low_cumul) == FALSE)

# Convert the data to a groupedData object
dat_days_complete_grouped <- groupedData(tavg_low_cumul ~ day_in_wint | year, 
                                         data = dat_days_complete)

# Fit the nonlinear mixed-effects model
# takes about a minute
mod_cum <- nlme(tavg_low_cumul ~ SSlogis(day_in_wint, Asym, xmid, scal), 
              random = Asym ~ 1 | year, 
              data = dat_days_complete_grouped)


plot(mod_cum)
summary(mod_cum)
dat_days_complete$predlme <- predict(mod_cum)

# plot
dat_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul, group = ywint)) +
  theme_bw() +
  geom_line(alpha=.7, mapping = aes(
    color = year
    ,group = factor(ywint))) +
  geom_line(mapping = aes(y = predlme),color='salmon4',linewidth=1)

library(boot)

# Define a function to refit the model on a resampled dataset
boot_model <- function(data, indices) {
  d <- data[indices, ]
  mod <- nlme(tavg_low_cumul ~ SSlogis(day_in_wint, Asym, xmid, scal), 
              random = Asym ~ 1 | year, 
              data = d)
  predict(mod, newdata = new_data)
}

new_data <- data.frame(day_in_wint = seq(1, 365, by = 1)) # new year prediction over 365 days

# # Perform the bootstrap (e.g., 1000 resamples)
# set.seed(123)  # For reproducibility
# boot_results <- boot(dat_days_complete_grouped, boot_model, R = 100)
# 
# # Calculate confidence intervals for predictions
# boot_preds <- t(boot_results$t)
# boot_mean <- apply(boot_preds, 2, mean)
# boot_lower <- apply(boot_preds, 2, quantile, 0.025)
# boot_upper <- apply(boot_preds, 2, quantile, 0.975)
# 
# # Add to new_data and plot
# new_data$boot_mean <- boot_mean
# new_data$boot_lower <- boot_lower
# new_data$boot_upper <- boot_upper
# 
# ggplot(new_data, aes(x = day_in_wint)) +
#   geom_line(aes(y = boot_mean), color = 'salmon4', size = 1) +
#   geom_ribbon(aes(ymin = boot_lower, ymax = boot_upper), alpha = 0.2, fill = 'blue') +
#   theme_bw() +
#   labs(y = "Predicted tavg_low_cumul", title = "Bootstrapped Predictions with Random Effects")


################

# takes about 30 seconds
mod_cum <- gls(tavg_low_cumul ~ ns(day_in_wint,df=8),
               obs_days_complete,
               weights=varExp(form = ~ day_in_wint),
               na.action = na.omit)

#summary(mod_cum)
#mod_cum %>% effects::predictorEffects(partial.residuals=FALSE) %>% plot
plot(mod_cum)

#predict(mod_cum, interval='prediction')

sds <- data.frame(
  day_in_wint = 1:364,
  sd_obs = NA
)

for (i in 1:nrow(sds)) {
  sds$sd_obs[i] <- 
    obs_days_complete %>% 
      filter(day_in_wint == sds$day_in_wint[i]) %>% 
        .$tavg_low_cumul %>% 
          sd(na.rm = TRUE)
}
mod_cum_obs <- lm( sd_obs ~ ns(day_in_wint,df=5) - 1
                   ,sds)
mod_cum_obs %>% 
  effects::predictorEffects(
    partial.residuals = TRUE
    ) %>% 
  plot()


sds$sd_pred <- predict(mod_cum_obs)
sds$mean_pred <- predict(mod_cum, newdata = sds)

sd_fun <- approxfun(sds$day_in_wint,sds$sd_pred)
mean_fun <- approxfun(sds$day_in_wint,sds$mean_pred)

return_std_tmp <- function(day_act, tmp_act) {
  m <- mean_fun(day_act)
  s <- sd_fun(day_act)
  return(list(
    z = (tmp_act - m) / s,
    z_perc = pnorm((tmp_act - m) / s)
  ))
}

obs_days_complete <- 
  obs_days_complete %>%
    mutate(z = return_std_tmp(day_in_wint,tavg_low_cumul)[["z"]],
           z_perc = return_std_tmp(day_in_wint,tavg_low_cumul)[["z_perc"]])

final_z <- 
  obs_days_complete %>% 
    filter(day_in_wint == 364) %>% 
      mutate(id = 1:n())
sds$cor_z <- NA
for (i in 1:364) {
  act_z <- 
    obs_days_complete %>% 
      filter(day_in_wint == i) 
  
  if ( i < 155) {
    act_z <- act_z %>%
        mutate(id=2:(n()+1)) %>%
          left_join(y=final_z,by="id")
  } else {
    act_z <- act_z %>%
      mutate(id=1:n()) %>%
      left_join(y=final_z,by="id")
  }
  
  sds$cor_z[i] <- cor(act_z$z.x,act_z$z.y,use="pairwise.complete.obs")
}

sds %>%
  ggplot(aes(x=day_in_wint,y=cor_z)) +
    theme_bw() +
    geom_line() +
    geom_smooth()



mod_cor <- lm(cor_z~ns(day_in_wint,df=3) - 1,sds)
#summary(mod_cor)
mod_cor %>% effects::predictorEffects(partial.residuals=TRUE) %>% plot()
sds$cor_z_pred <- predict(mod_cor, newdata = sds)
sds <- sds %>% mutate(cor_z_pred = cor_z_pred/max(cor_z_pred,na.rm=TRUE))

sds %>%
  ggplot( aes(x = cor_z_pred, y = cor_z)) +
    theme_bw() +
    geom_point() +
    geom_abline(slope=1,intercept = 0, color = "salmon4")


obs_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul
             )) +
  theme_bw() +
  geom_line(alpha=.7, mapping = aes(
    color = year
    ,fill=year
    ,group = factor(ywint))) +
  geom_line(data = sds, mapping = aes(y = mean_pred),color='salmon4',linewidth=1.5) +
  geom_line(data = sds, mapping = aes(y = mean_pred + 1.96 * sd_pred),color='red',linewidth=1.5) +
  geom_line(data = sds, mapping = aes(y = mean_pred - 1.96 * sd_pred),color='red',linewidth=1.5)






conf_lev <- 1.96
day_in_wint_act <- 173
cum_t_act <- 1530

cor_fun <- approxfun(sds$day_in_wint,sds$cor_z_pred)
cor_act <- cor_fun( day_in_wint_act)
sd_expected  <- sqrt( 1 - cor_act^2) 
z_act <- return_std_tmp(day_in_wint_act, cum_t_act)$z
z_expected <- cor_act * z_act

# lower_expected_std <- z_expected - conf_lev * sd_expected
# upper_expected_std <- z_expected + conf_lev * sd_expected

expecteds <- 
  data.frame(day_in_wint = day_in_wint_act:364) %>%
  left_join(y=sds,by="day_in_wint") %>%
  mutate(prop_var = (cor_z_pred - cor_act) / (1 - cor_act),
         sd_act   = sd_expected * prop_var,
         z_act.   = seq(z_act,z_expected,length.out=n()),
         lower_expected = mean_fun(day_in_wint) + (z_act. - sd_act*conf_lev)*sd_fun(day_in_wint),
         upper_expected = mean_fun(day_in_wint) + (z_act. + sd_act*conf_lev)*sd_fun(day_in_wint)
         )

obs_days_complete %>%
  ggplot(aes(x = day_in_wint, y = tavg_low_cumul
  )) +
  theme_bw() +
  geom_line(alpha=.7, mapping = aes(
    color = year
    ,fill=year
    ,group = factor(ywint))) +
  geom_line(data = sds, mapping = aes(y = mean_pred),
            color='salmon4',linewidth=1.5) +
  geom_line(data = sds, mapping = aes(y = mean_pred + 1.96 * sd_pred),
            color='red',linewidth=1.5) +
  geom_line(data = sds, mapping = aes(y = mean_pred - 1.96 * sd_pred),
            color='red',linewidth=1.5) +
  geom_point(data = expecteds,mapping=aes(y=lower_expected),color = "green") +
  geom_point(data = expecteds,mapping=aes(y=upper_expected),color = "green") +
  geom_point(mapping = aes( x = day_in_wint_act, y =cum_t_act),color="green") 

  


# Percent heating req. elapsed (lower bound)
expecteds %>%
  last %>%
  mutate(result = cum_t_act / upper_expected) %>%
  .$result

# Percent heating req. elapsed (upper bound)
expecteds %>%
  last %>%
  mutate(result = cum_t_act / lower_expected) %>%
  .$result



# Gas consumed this year, percent of total planned
gas_begin <- 
  obs_days_complete %>%
    filter(is.na(Meter) == FALSE) %>%
    mutate(err_start = difftime(paste0(act_year,"-08-01"), Date)) %>%
    filter( abs(err_start) == min(abs(err_start))) %>%
    .$Meter %>%
    .[1]

gas_now <- 
  obs_days_complete %>%
    filter(is.na(Meter) == FALSE) %>%
    last %>%
    .$Meter

const_gas <- 1 # sequestered for hot water prod.

(gas_now - gas_begin) / (1730 - 365 * const_gas)

fig_a <-
  obs_days_complete %>% 
    filter(Spent > 0,
           tavg_low_cumul > 0) %>%
  ggplot(aes(x = tavg_low_cumul, y = Spent, 
             color = factor(year(Date)))) +
    geom_point() +
    geom_smooth(method = "lm", mapping = aes(color = "smootho"),
                formula = y ~ x - 1, color = "grey50") +
    # mean easy to interpret niveau
    geom_line(data = data.frame(x=c(0,3000),y=c(0,2000)),
              mapping = aes(x,y), color = "salmon4", linewidth = 2) +
    # ideal niveau
    geom_line(data = data.frame(x=c(0,3284),y=c(1728,1728)),
              mapping = aes(x,y), color = "black", linewidth = 1) +
    geom_line(data = data.frame(x=c(3284,3284),y=c(1728,0)),
              mapping = aes(x,y), color = "black", linewidth = 1) +
    geom_line(data = data.frame(x=c(3284,0),y=c(1728,0)),
              mapping = aes(x,y), color = "black", linewidth = 1) +
    # niveau for january
    geom_line(data = data.frame(x=c(0,1515),y=c(761,761)),
              mapping = aes(x,y), color = "blue", linewidth = 1) +
    geom_line(data = data.frame(x=c(1515,1515),y=c(761,0)),
              mapping = aes(x,y), color = "blue", linewidth = 1) +
    geom_line(data = data.frame(x=c(1515,0),y=c(761,0)),
              mapping = aes(x,y), color = "blue", linewidth = 1) +
    geom_line(data = data.frame(x=c(1515+300,0),y=c(761,0)),
              mapping = aes(x,y), color = "blue", linewidth = 1) +
    geom_line(data = data.frame(x=c(1515-300,0),y=c(761,0)),
              mapping = aes(x,y), color = "blue", linewidth = 1) +
    theme_minimal()

fig_b <- fig_a +
  scale_x_continuous(limits = c(0,2000)) +
  scale_y_continuous(limits = c(0,1000))


