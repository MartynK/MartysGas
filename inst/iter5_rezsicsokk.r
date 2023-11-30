# Some calculations of medium sophistication for the planning on the heating season of 2022/23
# Price of gas is discounted up to 1730 m3/year, so thats our incentive...

library(readxl)
library(readr)
library(dplyr)
library(splines)
library(lubridate)
library(ggplot2)
library(nlme)

gaz <- read_excel("gaz.xlsx", range = "A4:D1343")
colnames(gaz) <- c("date", "day_in_year_good", "nextdate", "rate")
#View(gaz)

weather <- read_csv("weather.csv")

load("pred_rate.rdata")

data <- left_join( weather, gaz, by = "date") %>%
  mutate( day_in_yr = ifelse( day_in_year_good < 185, day_in_year_good + 180,day_in_year_good - 180))

dat_comp <- data[ complete.cases(data[,c("day_in_yr","tavg")]),]
dat_comp$range <- dat_comp$tmax - dat_comp$tmin

plot(data$date,data$tavg)

plot(data$day_in_yr,data$tavg)

smooth.spline(dat_comp$day_in_yr,
              dat_comp$tavg) %>% plot()

mod_spline <- lm( tavg ~ ns( day_in_yr, df = 8),
                  dat_comp)

effects::predictorEffects( mod_spline,
                           residuals = TRUE) %>% plot


preds <- data.frame( day_in_yr = 1:365,
                     ywint = as.factor(2021) )

preds <- preds %>%
  mutate( tavg = predict(mod_spline, newdata = preds) - 2.5 # Have to correct, sum not like reality otherwise
          , tavg_p2 = tavg + 2
         )
          
preds <- preds %>%
          mutate(
           gas = predict( mod4, newdata = preds),
           gas_p2 = predict( mod4, newdata = preds %>% mutate( tavg = tavg_p2)),
          date_good = as.Date( day_in_yr + 180, origin = "2022-01-01")) %>%
  mutate( 
    gas = ifelse( day_in_yr < 77 | day_in_yr > 320, .2, gas)
    )


sum(preds$gas_p2)


const_0 <- 1730 / sum(preds$gas)
const_5 <- 1730 * .95 / sum(preds$gas)
const_10 <- 1730 * .90 / sum(preds$gas)
const_15 <- 1730 * .85 / sum(preds$gas)
const_20 <- 1730 * .80 / sum(preds$gas)


preds <- preds %>%
  mutate(
    gas_0  = gas * const_0
    ,gas_5  = gas * const_5
    ,gas_10 = gas * const_10
    ,gas_15 = gas * const_15
    ,gas_20 = gas * const_20
  ) %>%
  rowwise() %>%
  mutate(
    jelleg = jelleggorb$amount[jelleggorb$month == 
                               month( date_good)
                               ] / days_in_month( date_good)
  )

preds %>%
  ggplot( aes( x = date_good, y = gas_0)) +
  theme_bw() +
  geom_line(color = "salmon4", size = 1.5) +
  geom_line(mapping = aes(y = gas_5), color = "grey85", size = 1) +
  geom_line(mapping = aes(y = gas_10), color = "grey80", size = 1) +
  geom_line(mapping = aes(y = gas_15), color = "grey50", size = 1) +
  geom_line(mapping = aes(y = gas_20), color = "grey85", size = 1) +
  scale_x_date(breaks = "1 month", limits = c(as.Date("2022-08-01"),as.Date("2023-08-01"))) +
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
  geom_line( mapping = aes(y = jelleg), color = "blue", alpha = .5)
  
  
