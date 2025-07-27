# ------------------------------------------------------------
# * Approximate hourly temperature from daily data
# * Join gas meter readings with temperature data
# * Fit a simple model to explain hourly gas rate
# * Compare model predictions across years
# ------------------------------------------------------------

source(here::here("inst","function","load_stuff.r"))
# capture_plot helper is provided by approx_helpers.r

maketsum <- function(date,datelag) {
  
  hours <- interval( datelag, date ) %>% as.numeric
  hours <- hours / 3600 
  
  tsum <- temps_xtra$tavg[ temps_xtra$Date < date &
                             temps_xtra$Date > datelag
  ] %>%
    sum 
  
  return( tsum / hours)
}
Maketsum <- Vectorize(maketsum)

gaz_rendetlen <- read_excel("gaz.xlsx", sheet = "Mero_rendetlen")

gaz_rendetlen <- gaz_rendetlen %>% 
         rename( Value = Mérő, 
                       Date = Dátum, 
                       Gas = Gáz, 
                       Day = Nap) %>%
         mutate( Date = as_datetime(Date),
                 datelag = lag(Date),
                 tact = Maketsum( Date, datelag),
                 datemiddle = Date + as.duration(interval(Date , datelag))/2,
                 ywint = ifelse(  yday(datemiddle) < 185, year( datemiddle), year( datemiddle)+1) %>% as.factor
                 ,datenum = interval(min(Date), Date) %>% as.duration() %>% as.numeric("days")
                 , heat_off = ifelse( tsum > 17, "off", "on")
                 ) %>%
         filter(Day!=0)

gaz_rendetlen$dtmn <- gaz_rendetlen$datemiddle %>% as.numeric()
gaz_rendetlen$dtmn <- (gaz_rendetlen$dtmn / 3600 /24 -17873) %>% round(digits=2)

#View(gaz_rendetlen)

fig_spline_temp <- capture_plot(plot(temps_xtra$splined_temp, type = "l"))

fig_gasrate_scatter <- capture_plot(
  plot(gaz_rendetlen$tsum, gaz_rendetlen$Rate)
)

mod <- lm( Rate ~ 
           (
             ns( tsum, df = 1) 
           #+ ns(dtmn, df = 2)
            )
           #+ heat_off
           + ywint
           , 
           gaz_rendetlen)

fig_model_effects <- capture_plot(
  mod %>% effects::predictorEffects(residuals = TRUE) %>% plot()
)
nd <- expand.grid( tsum = seq(0,20,.1),
                   ywint = gaz_rendetlen$ywint[nrow(gaz_rendetlen)])
nd$pred <- predict( mod, newdata = nd)

preds_day %>%
ggplot( aes( x = tact, y = gas)) +
  theme_bw() +
  geom_line( color = "salmon") +
  geom_line( mapping = aes( x = tsum, y = pred), data = nd, color = "blue") +
  labs( caption = "Comparing two models; blue is virtually guaranteed to have no autocorrelation")

