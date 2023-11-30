library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(nlme)
library(splines)
library(ggplot2)

#setwd("~/OneDrive_mrkmarton/-/Dinamikus Kiválóság Menedzsment - General/Stats, R/R/Martys gas")

weather <- read_csv("weather.csv") %>%
  mutate(range = tmax-tmin,
         tsum  = (tmax + tmin + tavg ) / 3,
         day_in_year = yday(date),
         year = year(date),
         dy1 = lag(tsum,1),
         dy2 = lag(tsum,2),
         dy3 = lag(tsum,3),
         dme = (tsum+dy1+dy2+dy3)/4) %>%
  filter( is.na(tsum) == FALSE)


smooth.spline( weather$day_in_year, weather$tsum) %>% plot(type="l")
abline(h=10,col="red")
abline(v=90,col="red")

weather %>%
  mutate(date_yday = parse_date_time(x = as.character(day_in_year), orders = "j") ) %>%
  ggplot( aes( x = date_yday, #day_in_year, 
               y = dme, group = year, color = year)) +
  theme_bw() +
  geom_line() +
  scale_x_datetime(date_breaks="4 weeks") +
  geom_vline( xintercept = as_datetime("2022-09-02"), color = "red")
