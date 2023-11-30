# These are some good old fashioned analyses based on the available transformed data

library(splines)
library(ggplot2)

source( here::here( "data_transforms.r"))


# curves based on daily avg. temp
temps_xtra %>%
  ggplot( aes( x = tavg, 
               y = Rate, 
               fill = as.factor(ywint), 
               color = as.factor(ywint))) +
  theme_bw() +
  geom_point( alpha = .05) +
  geom_smooth(se = FALSE, size = 2,
              method = 'gam',
              formula = y ~ s(x, bs = "tp"))


# curves based on hourly simulated temp
temps_xtra %>%
  filter(tact < 20) %>%
  ggplot( aes( x = tact, 
               y = Rate, 
               fill = as.factor(ywint), 
               color = as.factor(ywint))) +
  theme_bw() +
  geom_point( alpha = .05)  +
  geom_smooth(se = FALSE, size = 2,
              method = 'gam',
              formula = y ~ s(x, bs = "tp"))


# filtering out the winter we went to Australia
# and handling warm days
temps_xtra_nice <- temps_xtra %>%
  filter( ywint != "2019" |
          tact > 10) %>%
  mutate( heat_off = ifelse( tavg > 20, "off", "on") %>% as.factor)


# creating a lm
mod <- lm( Rate ~ (tact + ns(id, df = 2)) * heat_off, 
           temps_xtra_nice)

summary(mod)

# plot(mod)
# acf(resid(mod),lag=1500)
# mod %>% effects::predictorEffects() %>% plot
# car::vif(mod, type = 'predictor')

# yday("2022-05-15")
# yday("2022-09-15")
preds <- expand.grid( hours_dat = 0:23,
                     day_in_year = 1:365,
                     id = max(temps_xtra_nice$id)) %>%
            mutate( heat_off = ifelse( day_in_year > 258 |
                                        day_in_year < 135,  "on", "off")) %>%
  left_join(., y = data %>% select(c("day_in_wint","day_in_year")),
            by = c( "day_in_year" = "day_in_year")) %>%
  group_by( hours_dat, day_in_year) %>%
  slice(1)



preds$tact <- predict(mod_temp, newdata = preds)

preds$gas <- predict( mod, newdata = preds)
preds$gas[preds$heat_off == "off"] <- .4 # has to be adjusted, model is dead wrong

correction <- 2500 / sum(preds$gas/24)

preds$gas_corr <- preds$gas * correction

const_0 <- 1730 / sum(preds$gas/24)
const_5 <- 1730 * .95 / sum(preds$gas/24)
const_10 <- 1730 * .90 / sum(preds$gas/24)
const_15 <- 1730 * .85 / sum(preds$gas/24)
const_20 <- 1730 * .80 / sum(preds$gas/24)


preds <- preds %>%
  mutate(
    gas_0  = gas * const_0
    ,gas_5  = gas * const_5
    ,gas_10 = gas * const_10
    ,gas_15 = gas * const_15
    ,gas_20 = gas * const_20
  ) %>%
  #rowwise() %>%
  mutate(
    date_good = (parse_date_time(x = as.character(day_in_wint), orders = "j") 
                 + days(180)) %>% as.Date,
    month = month(date_good) 
    ) %>%
  left_join( ., y = jelleggorb, by = "month") %>%
  mutate(
    jelleg = amount / days_in_month( date_good)
  )
    

preds_day <- preds %>%
  group_by(day_in_year) %>%
  mutate( 
    across(  .cols = c("tact", "gas", "gas_corr", "gas_0", "gas_5", "gas_10",
                       "gas_15", "gas_20"), .fns = mean)
          ) %>%
  slice(1)


preds_day %>%
  ggplot( aes( x = date_good, y = gas_0)) +
  theme_bw() +
  geom_line(color = "salmon4", size = 1.5) +
  geom_line(mapping = aes(y = gas_5), color = "grey85", size = 1) +
  geom_line(mapping = aes(y = gas_10), color = "grey80", size = 1) +
  geom_line(mapping = aes(y = gas_15), color = "grey50", size = 1) +
  geom_line(mapping = aes(y = gas_20), color = "grey85", size = 1) +
  scale_x_date(breaks = "7 weeks", limits = c(as.Date("2022-08-01"),as.Date("2023-08-01"))) +
  scale_y_continuous( breaks = c(.2,2:10)) +
  labs( x = "", y = "Gázfogyasztás; m3/nap") +
  annotate(geom="text", x=as.Date("2023-06-01"), y=4.5, label="1730 m3/év",
           color="salmon4") +
  annotate(geom="text", x=as.Date("2023-06-20"), y=4.1, label="5% margin: 1645 m3/év",
           color="grey65") +
  annotate(geom="text", x=as.Date("2023-06-20"), y=3.7, label="10% margin: 1555 m3/év",
           color="grey60") +
  annotate(geom="text", x=as.Date("2023-06-20"), y=3.3, label="15% margin: 1470 m3/év",
           color="grey40") +
  annotate(geom="text", x=as.Date("2023-06-20"), y=2.9, label="20% margin: 1385 m3/év",
           color="grey70") +
  geom_line( mapping = aes(y = jelleg), color = "green", alpha = .5) +
  # Inserting the realization of gas consumption
  geom_point( mapping = aes( x = as.Date(Date), y = Rate), data = gaz,
              color = "navy", size = .8, alpha = .99) +
  geom_line(mapping = aes( x = as.Date(Date), y = Rate), data = gaz,
            color = "navy", size = .95)





preds_day %>%
  ggplot( aes( x = date_good, y = tact)) +
  theme_bw() +
  geom_line(color = "salmon4", size = 1.5) +
  # Inserting the realization of the temp. data so far
  geom_point( mapping = aes( x = as.Date(Date), y = tavg), data = data,
              color = "lightblue", size = 1, alpha = .7) +
  geom_line(mapping = aes( x = as.Date(Date), y = tavg), data = data,
            color = "lightblue", size = .95) +
  scale_x_date(breaks = "7 weeks", 
               limits = c(as.Date("2022-08-01"),
                          as.Date("2023-08-01")))
  




