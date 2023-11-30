library(readxl)
library(readr)
library(dplyr)
library(splines)
library(lubridate)
library(ggplot2)
library(nlme)

gaz <- read_excel("gaz.xlsx", range = "A4:D1343")
colnames(gaz) <- c("date", "day_in_yr", "nextdate", "rate")
#View(gaz)

weather <- read_csv("weather.csv")

data <- left_join( weather, gaz, by = "date")

dat_comp <- data[ complete.cases(data[,c("day_in_yr","tavg")]),]
dat_comp$range <- dat_comp$tmax - dat_comp$tmin

plot(data$date,data$tavg)

plot(data$day_in_yr,data$tavg)

smooth.spline(dat_comp$day_in_yr,
              dat_comp$tavg) %>% plot()
matplot(1:365,sin(seq(0,3.15,length.out=365))*25,add=TRUE)

mod_spline <- lm( tavg ~ ns( day_in_yr, df = 6),
                  dat_comp)

effects::predictorEffects( mod_spline,
                           residuals = TRUE) %>% plot

preds <- data.frame( day_in_yr = 1:365)
preds$avg <- predict(mod_spline, newdata = preds)

dat_comp$pred <- predict(mod_spline)

mod_min <- lm( tmin ~ ns( tavg, df = 2) + ns(day_in_yr,df=2), dat_comp)
mod_max <- lm( tmax ~ ns( pred, df = 2) + ns(day_in_yr,df=2), dat_comp)
mod_ran <- lm( range ~ ns( pred, df = 2) + ns(day_in_yr,df=2), dat_comp)


anova(mod_min)

mod_min %>% predict() %>% plot
mod_max %>% predict() %>% plot
mod_ran %>% predict() %>% plot

plot(dat_comp$day_in_yr,dat_comp$range)
smooth.spline(dat_comp$day_in_yr,dat_comp$range) %>% plot


# effects::predictorEffects( mod_min,
#                            #residuals = TRUE
#                            ) %>% plot

preds$min <- predict(mod_min, newdata = data.frame( tavg = preds$avg, 
                                                    day_in_yr = preds$day_in_yr))
#preds$max <- predict(mod_max, newdata = preds)

preds$range <- smooth.spline(dat_comp$day_in_yr,dat_comp$range) %>% 
                 predict() %>% .$y %>% .[1:365]


corr <- data.frame(a = rep(NA, 365),
                   b = rep(NA, 365))
corr$b <- quantile( dat_comp$tmin,
               probs = seq(0,1,length.out=365))
corr$a <- quantile( preds$min,
               probs = seq(0,1,length.out=365))

plot(corr$a,corr$b)

mod_corr <- lm( b ~ ns(a,df=8),corr) #%>% effects::predictorEffects() %>% plot

preds$min2 <- predict( mod_corr, newdata = data.frame(a = preds$min))

dat_comp$tmin %>% hist(breaks = 30)

# dens <- data.frame( t = -11:30,
#                     min = rep(0,42))
# 
# for ( i in 1:nrow(dens)) {
#   dens$min[i]  <- sum( dat_comp$tmin < dens$t[i])
#   dens$med[i]  <- sum( dat_comp$tavg < dens$t[i])
#   dens$max[i]  <- sum( dat_comp$tmax < dens$t[i])
#   
#   if ( i > 1) {
#     dens$min[i] <- dens$min[i] - sum( dens$min[1:(i-1)])
#     dens$med[i] <- dens$med[i] - sum( dens$med[1:(i-1)])
#     dens$max[i] <- dens$max[i] - sum( dens$max[1:(i-1)])
#   }  
# }

preds$max2 <- preds$min2 + preds$range
preds$avg2 <- preds$min2 + preds$range/2

preds_def <- expand.grid( day = 1:365,
                          hour = 0:23)
preds_def$time <- preds_def$day + preds_def$hour/24

preds_def <- preds_def %>%
               arrange(time)

preds_def$temp <- 0

for (i in 1:nrow(preds_def)) {
  preds_def$temp[i] <- preds$min2[preds$day_in_yr == preds_def$day[i]] + 
    ( sin( (preds_def$hour[i]-6)/12*3.14159) + 1) *
    preds$range[preds$day_in_yr == preds_def$day[i]]/2
}


plot(preds_def$time[preds_def$day==1],
     preds_def$temp[preds_def$day==1],
     type='l')

plot(preds_def$time,
     preds_def$temp,
     type='l')


load("pred_rate.rdata")

get_lp <- function(t) {
  if (t <= -6.7) { 
    return(16.17 + 0.676 * (-6.7-t))
  } else if (t > 25) {
      return(0)
  }

  mi <- simp_out[ simp_out$t == max(simp_out$t[simp_out$t<t]),]
  ma <- simp_out[ simp_out$t == min(simp_out$t[simp_out$t>t]),]
  dt <- ma$t - mi$t
  rat <- (t - mi$t) / dt
  out <- ma$rate + (ma$rate - mi$rate) * rat
  return(out)
}

preds_def$rate <- 0
for (i in 1:nrow(preds_def)) {
  preds_def$rate[i] <- get_lp(preds_def$temp[i])
  
}
preds_def$rate[preds_def$temp>20] <- 0
preds_def$rate.hour <- preds_def$rate/24
plot(preds_def$time,preds_def$rate.hour,type='l')
sum(preds_def$rate.hour) # for 2021 was ~2500 :( ; eta?~ .75
hist(preds_def$rate.hour,breaks=70)

preds_def$kwh <- preds_def$rate.hour * 10.55 #forrás: internet
hist(preds_def$kwh,breaks=70)

charac <- data.frame(
  t   = c(-20, -7,  2,   7,   12),
  eta = c(1.69,2.28,3.53,4.41,5.75)
)
plot(charac,type = 'l')
cmod <- lm( eta ~ poly(t,degree=2),charac)
cmod %>% effects::predictorEffects( residuals = TRUE) %>% plot

get_cop <- function(t) {
  predict(cmod,newdata = data.frame( t = t))
}

preds_def$cop <- get_cop(preds_def$temp)
preds_def$cop[preds_def$temp>12] <- get_cop(12)
plot(preds_def$cop,type='l')

preds_def$kwh_pump <- preds_def$kwh / preds_def$cop

sum(preds_def$kwh_pump)

preds_def$cost_pump <- preds_def$kwh_pump * 45 #!!!
preds_def$cost_gas  <- preds_def$kwh      * 115 / 10.55 #!!!!
preds_def$cost_hybr <- ifelse( preds_def$cost_pump < preds_def$cost_gas, 
                               preds_def$cost_pump,
                               preds_def$cost_gas)

with(preds_def, plot( cost_pump,cost_gas))
abline(c(0,1))

sum( preds_def$cost_gas)
sum( preds_def$cost_pump)
sum( preds_def$cost_hybr)

# Gas -> now, :=1
# Pump +6%
sum( preds_def$cost_pump) / sum( preds_def$cost_gas)
# Hybrid -9%
sum( preds_def$cost_hybr) / sum( preds_def$cost_gas)


hist( preds_def$temp,breaks=50)


