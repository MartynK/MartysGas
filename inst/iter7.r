

source( here::here( "inst", "function", "load_stuff.r"))

act_year <- 2023


obs_days %>%
  filter(is.na(Spent_perc) == FALSE) %>%
  ggplot( aes( x = day_in_wint, y = Spent_perc,
               group = ywint,
               color = ywint)) +
  geom_point()

obs_hours %>%
  filter(is.na(Spent_perc) == FALSE) %>%
  ggplot( aes( x = day_in_wint, y = Spent_perc,
               group = ywint,
               color = ywint)) +
  geom_point()


mod_readings <- lm( Rate ~ I(tavg_obs - 20) * ywint, obs_readings)
anova(mod_readings)
summary(mod_readings)
mod_readings %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot

pr <- expand.grid(
  tavg_obs = seq(-10,20,1),
  ywint = unique(obs_readings$ywint)
)
pr$pr <- predict(mod_readings,newdata = pr)
pr %>%
  ggplot(aes(x = tavg_obs, y = pr, color = ywint)) +
  geom_line()

obs_days_mod <- obs_days %>% 
  ungroup() %>%
  filter(is.na(Spent_perc) == FALSE) %>%
  mutate(day_dataframe = as.numeric((Date - min(Date,na.rm = TRUE)) / (3600 * 24)))

mod_days <- lm( Rate ~ I(tavg - 20) * ywint, 
                obs_days_mod)
summary(mod_days)

obs_days_mod$res <- residuals(mod_days)
obs_days_mod$fit <- fitted(mod_days)
sd(obs_days_mod$res)

obs_days_mod %>%
  ggplot(aes(x = Date, y = fit)) +
  geom_point() +
  geom_point(mapping = aes(y = res),color = "cyan")

pr$tavg <- pr$tavg_obs
pr$pr_days <- predict(mod_days,newdata = pr)
pr %>%
  ggplot(aes(x = tavg_obs, y = pr_days, color = ywint)) +
  geom_line()

#####
#ACF intermezzo
library(nlme)

mod_days_gls <- gls( Rate ~ I(tavg - 20) * ywint, 
                     correlation = corAR1(form = ~ day_dataframe),
                     obs_days_mod)
summary(mod_days_gls)

obs_days_mod$res_gls <- residuals(mod_days_gls)
obs_days_mod$fit_gls <- fitted(mod_days_gls)
sd(obs_days_mod$res_gls)

obs_days_mod %>%
  ggplot(aes(x = Date, y = fit_gls)) +
  geom_point() +
  geom_point(mapping = aes(y = res),color = "cyan")

pr$pr_days_gls <- predict(mod_days_gls,newdata = pr) # ERROR
pr %>%
  ggplot(aes(x = tavg_obs, y = pr_days_gls, color = ywint)) +
  geom_line()

#####

obs_hours_mod <- obs_hours %>% filter(is.na(Spent_perc) == FALSE)
  
mod_hours <- lm( Rate ~ I(tavg - 20) * ywint, 
                obs_hours_mod )
summary(mod_hours)

obs_hours_mod$res <- residuals(mod_hours)
obs_hours_mod$fit <- fitted(mod_hours)
sd(obs_hours_mod$res)

obs_hours_mod %>%
  ggplot(aes(x = Date, y = fit)) +
  geom_point() +
  geom_point(mapping = aes(y = res),color = "cyan")

obs_hours_trans <- obs_hours_mod %>%
  mutate( Date = as_date(Date)) %>%
  group_by(Date) %>%
  mutate(Rate = mean(Rate,na.rm=TRUE),
         fit = mean(fit,na.rm=TRUE),
         res = Rate - fit) %>%
  slice(1)
sd(obs_hours_trans$res)



pr$pr_hours <- predict(mod_hours,newdata = pr)
pr %>%
  ggplot(aes(x = tavg_obs, y = pr_hours, color = ywint)) +
  geom_line()



pr %>%
  ggplot(aes(x = pr, y = pr_days, color = ywint)) +
  geom_point() +
  geom_abline(slope = 1, intercept=0, col = "salmon2")


pr %>%
  ggplot(aes(x = pr_hours, y = pr_days, color = ywint)) +
  geom_point() +
  geom_abline(slope = 1, intercept=0, col = "salmon2")


par(mfcol=c(1,2))
acf(obs_days_mod$res)
acf(obs_hours_mod$res)
par(mfcol=c(1,1))

####

mean_temps_realiz <- 
  obs_days %>%
    group_by(year) %>%
    mutate(tavg_mean = mean(tavg,na.rm=TRUE)) %>%
    slice(1) %>%
    .$tavg_mean 

mean_temps_sim <- 
  weather_simulated %>%
  group_by(year_sim) %>%
  mutate(tavg_mean = mean(pred_tavg,na.rm=TRUE)) %>%
  slice(1) %>%
  .$tavg_mean

par(mfcol=c(2,1))
mean_temps_realiz %>%
  hist(xlim = c(10,17))
mean_temps_sim %>%
  hist(xlim = c(10,17),breaks = 30)
par(mfcol=c(1,1))

realiz_pars <-
  mean_temps_realiz %>%
  list( mn = mean( .),
        sd = sd(.))

rank(mean_temps_sim, ties.method = "average") / (length(mean_temps_sim)+1) 
qnorm(rank(mean_temps_sim, ties.method = "average") / (length(mean_temps_sim)+1))

mean_temps_for_sim <- 
  realiz_pars[["mn"]] +
   realiz_pars[["sd"]] * qnorm(rank(mean_temps_sim, ties.method = "average") 
                                / (length(mean_temps_sim)+1))

weather_simulated <- weather_simulated %>%
  group_by(year_sim) %>%
  mutate(tavg_mean = mean(pred_tavg),
         tavg_corr_fac = mean_temps_for_sim[year_sim] - tavg_mean,
         tavg_corr = pred_tavg + tavg_corr_fac)

weather_simulated %>%
  ggplot(aes(x = day_in_year, y = tavg_corr, group = year_sim)) +
  geom_line(alpha = .1, color = "cyan") +
  geom_line( data = obs_days, mapping = aes(y=tavg,group=year),
             alpha = .2, color = "red")

weather_simulated <- weather_simulated %>%
  ungroup %>%
  mutate( ywint_sim = ifelse(  day_in_year < 213, 
                               year_sim - 1, year_sim),
          rown = 1:n())

with(weather_simulated,plot(rown,ywint_sim,type='l'))

a <- 0.15
b <- .5

weather_simulated <- weather_simulated %>%
  mutate(Rate_sim = (20 - tavg_corr) * b + a) %>%
  mutate(Rate_sim = ifelse( Rate_sim <= a, a, Rate_sim))

with(weather_simulated,plot(tavg_corr,Rate_sim))

simulated_consumption <-   weather_simulated %>%
    filter( ywint_sim != min(ywint_sim)) %>%
    filter( ywint_sim != max(ywint_sim)) %>%
    mutate( tavg_capped20 = ifelse( pred_tavg > 20, 20, pred_tavg)) %>%
    group_by(ywint_sim) %>%
    mutate(consumed_gas = sum(Rate_sim),
           tavg_whencold = mean(
             tavg_capped20
             #pred_tavg
             #c(tavg_corr[tavg_corr < 20],rep(20,sum(tavg_corr>20)))
             )) %>%
    slice(1) 

hist(simulated_consumption$consumed_gas,breaks = round(sqrt(100)+5))
plot(simulated_consumption$tavg_whencold,simulated_consumption$consumed_gas)



