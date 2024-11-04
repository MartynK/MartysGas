library(dplyr)
library(lubridate)
library(splines)
library(splines2)
library(ggplot2)
library(nlme)

load( here::here("data","meteostat_data.Rdata")) 
#load_all_Rdata(directory=here::here("inst","function","backend")) # Load slow suff's output


# Length of tropical year in days
YR_TRP <- 365.24217
# start the numbering when its the vernal equanox
START_DAY <- "2000-03-20"

tropical_year <- function(date
                          # , YR_TRP = YR_TRP
                          # , START_DAY = START_DAY
                          ) {
  diff_day0 <- as.numeric(difftime(date, ymd(START_DAY), units = "days"))
  tropical_year <- diff_day0 / YR_TRP
  tropical_year_loc <- tropical_year - floor(tropical_year)
  tropical_year_sin_component <- sin(2*pi*(tropical_year_loc - (yday(START_DAY)*1) / YR_TRP)) + 1
  return(data.frame(diff_day0 = diff_day0,
                    tropical_year = tropical_year,
                    tropical_year_loc = tropical_year_loc,
                    tropical_year_loc_sin = cos(2*pi*(tropical_year_loc - yday(START_DAY) / YR_TRP)) + 1,
                    tropical_year_sin_component = tropical_year_sin_component))
}


data <- bind_cols( meteostat_weather, 
  tropical_year(meteostat_weather$Date))

mod <- lm(tavg ~ 
            ns(tropical_year_loc_sin, df = 1)
          + ns(tropical_year_sin_component, df = 3)
          , data = data)

#mod %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot() %>% try()
mod %>% effects::predictorEffects(partial.residuals = FALSE) %>% plot() %>% try()

car::vif(mod) %>% try()
anova(mod)


# Define the start date
start_date <- as.Date("2024-01-01")

# Generate a sequence of 365 days
date_sequence <- seq.Date(from = start_date, by = "day", length.out = 365)

# Display the sequence
d2 <- tropical_year(date_sequence) %>% 
  mutate(Date = date_sequence)

d2$pred <- predict(mod, newdata = d2)


ggplot(d2, aes(x = Date, y = pred, 
               color = tropical_year_sin_component
               )) +
  geom_line() +
  geom_point( ) +
  geom_vline(xintercept = as.Date("2024-03-20"), linetype = "dashed") +
  theme_minimal()


par(mfrow = c(2,1))
plot(d2$tropical_year_sin_component)
abline(v = 80)
plot(d2$tropical_year_loc_sin)
abline(v = 80)
par(mfrow = c(1,1))


#######################

mod_lme <- lme(tavg ~ 
                 ns(tropical_year_loc_sin, df = 1)
               + ns(tropical_year_sin_component, df = 3)
               , data = data, random = ~ ns(tropical_year_loc_sin,df=2)[,] | ywint)

BIC(mod_lme)

# plot the ranefs
mod_lme %>% ranef() %>% plot()

mod_lme %>% effects::predictorEffects() %>% plot()

# plot the predictions of the model
data$pred_lme <- predict(mod_lme)

ggplot(data, aes(x = yday(Date), y = pred_lme, 
                 color = as.numeric(ywint),
                 group = ywint
                 )) +
  geom_line() +
  theme_minimal()

pred <- seq.Date(as.Date("2024-01-01"),
                 as.Date("2025-12-31"),
                 by="1 day") %>% 
  tropical_year()

pred$pred <- predict(mod_lme, newdata = pred, level = 0)


ggplot(pred, aes(x = diff_day0, y = pred, 
                 color = tropical_year_sin_component,
                 size = tropical_year_loc_sin
                 )) +
  geom_line() +
  geom_point( ) +
  #geom_vline(xintercept = as.Date("2024-03-20"), linetype = "dashed") +
  theme_minimal()


predict_nxt_3_months <- function(data, divider = as.Date("2023-11-20"), 
                                  df_b = 5, df_c = 4, df_rand = 2) {
  
  out <- data %>% mutate(pred=NA, pred_classic=NA, pred_level0=NA) %>% .[0,]
  
  past <- data %>% filter(Date <= divider)
  future <- data %>% filter(Date > divider, 
                            Date <= divider + 91) %>%
    mutate(pred=NA,
           pred_classic = NA,
           pred_level0 = NA)
  
  form <- as.formula(paste("tavg ~ ns(tropical_year_loc_sin, df = ", 
                           df_b, ") + ns(tropical_year_sin_component, df = ", 
                           df_c, ")"))
  if (df_rand >=1) {
    form_rand <- as.formula(paste("~ ns(tropical_year_loc_sin, df = ", 
                                  df_rand, ")[,] | ywint"))
  } else {
    form_rand <- as.formula("~ 1 | ywint")
  }
  
  try(silent=TRUE, {
    model_was_fitted <- try(silent=TRUE,{
      mod_past <- lme(form
                      , data = past
                      , random = form_rand
                      , control =  lmeControl(maxIter = 200,
                                              msMaxIter = 300,
                                              tolerance = 1e-5,
                                              niterEM = 40,
                                              msVerbose = FALSE,
                                              msTol = 1e-6))
    })
    
    mod_classic <- lm(form, data = past)
    
    if (inherits(model_was_fitted, "try-error")) {
      mod_past <- lme(form
                      , data = past
                      , random = ~ 1 | ywint
                      , control =  lmeControl(maxIter = 200,
                                              msMaxIter = 300,
                                              tolerance = 1e-5,
                                              niterEM = 40,
                                              msVerbose = FALSE,
                                              msTol = 1e-6))
    }
    
    try({
      future$pred[future$ywint %in% past$ywint] <- predict(mod_past, newdata = future[future$ywint %in% past$ywint,])
    })
    
    try({
      future$pred[!(future$ywint %in% past$ywint)] <- predict(mod_past, newdata = future[!(future$ywint %in% past$ywint),], level = 0)
    }, silent = TRUE)
    
    try({
      future$pred_level0 <- predict(mod_past, newdata = future, level = 0)
    }, silent = FALSE)
    
    try({
      future$pred_classic <- predict(mod_classic, newdata = future)
    })
    
    out <- bind_rows(out, future)
  })
  return(list(mod_past = mod_past, mod_classic = mod_classic, out = out))
}

return_rmse_lme <- function(data, divider = as.Date("2024-03-20"), 
                        df_b = 5, df_c = 4, df_rand = 2, type = "pred") {
  
  out <- predict_nxt_3_months(data, divider, df_b, df_c, df_rand)
  
  preds <- out$out %>%
    mutate(pred_lme = sum(sqrt((pred - tavg)^2)),
           pred_classic = sum(sqrt((pred_classic - tavg)^2)),
           pred_level0 = sum(sqrt((pred_level0 - tavg)^2))) %>%
    slice(1) %>%
    select(pred_lme, pred_classic, pred_level0)
  
  return(preds)
}


out <- expand.grid(df_b = 1:5, 
                   df_c = 1:5, 
                   df_rand =0:2,
                   rmse_classic = NA, 
                   rmse_level0 = NA, 
                   rmse_spec = NA)

dividers = seq.Date(from = as.Date(min(data$Date)+days(365*3)), 
                    to = as.Date(max(data$Date)), by = "3 months")

pb <- txtProgressBar(min = 0, max = nrow(out), style = 3)
for (i in 1:nrow(out)) {
  act <- sapply(dividers, return_rmse_lme, 
                        data = data, df_b = out$df_b[i], df_c = out$df_c[i], 
                df_rand = out$df_rand[i], type = "pred") %>%
    t()
  
  res <- data.frame(matrix(NA,ncol=3)) %>% `colnames<-`(colnames(act))
  for (j in 1:3) {
    res[1,j] <- act[,j] %>% as.numeric() %>% sum()
  }
  
  out$rmse_classic[i] <- res$pred_classic
  out$rmse_level0[i] <- res$pred_level0
  out$rmse_spec[i] <- res$pred_lme
  setTxtProgressBar(pb, i)
}
close(pb)

out <- out %>%
    mutate(gain_level0 = rmse_classic - rmse_level0)

df_optimization <- out
save(df_optimization, 
     file = here::here("inst","function",
                       "backend","df_optimization.rdata"))

# plot out like heatmap
out %>% 
  filter(df_rand == 1) %>%
  ggplot(aes(x = df_c, y = df_b, fill = rmse_level0)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "RMSE of the model",
       x = "df_c",
       y = "df_b")

# plot as line plot faceted by df_rand
out %>% 
  ggplot(aes(x = df_b, y = rmse_level0, 
             color = df_c, group = df_c)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "RMSE of the model") +
  geom_hline(yintercept = min( out$rmse_level0 ), linetype = "dashed", color = "salmon4") +
  facet_wrap(~df_rand, ncol = 1)


#######################
# go through dividers and resfit the model and collect the respective predictions

dividers = seq.Date(from = {min(dat$Date)+days(365*3)} %>% 
                      as.Date() %>% 
                      `month<-`(8) %>% 
                      `day<-`(1),
                    to = as.Date(max(data$Date)), by = "12 months")

out <- data %>% mutate(pred=NA,pred_classic=NA,pred_level0=NA) %>% .[0,]

pb <- txtProgressBar(min = 0, max = length(dividers), style = 3)

for (i in 1:length(dividers)) {
  
  act <- predict_nxt_3_months(data, dividers[i], df_b = 1, df_c = 3, df_rand = 2)
  
  out <- bind_rows(out, act$out)
  
  setTxtProgressBar(pb, i)
}

out %>%
  ggplot(aes(x = tavg, y = pred, color = ywint)) +
  geom_line() +
  theme_minimal()

out %>%
  ggplot(aes(x = Date, y = pred, color = ywint)) +
  geom_line() +
  theme_minimal()

out %>%
  ggplot(aes(x = Date, y = pred_classic -pred_level0, color = ywint)) +
  geom_point() +
  theme_minimal()

out %>%
  filter(Date > as.Date("2014-03-20")) %>%
  ggplot(aes(x = Date, y = pred_level0, color = ywint)) +
  geom_point() +
  geom_line(mapping = aes(y=pred_classic),color = "black") +
  theme_minimal()


out %>%
  ggplot(aes(x = Date, y = pred_level0 - tavg, color = ywint)) +
  geom_line() +
  #geom_line(mapping = aes(y = pred_classic - tavg), color = "black",alpha=.4) +
  theme_minimal()


# which has better rmse per year? pred or pred_all? calculate 

out_rmses <- out %>% 
  group_by(ywint) %>% 
  mutate( rmse = sum( (pred - tavg)^2)/n(),
          rmse_classic = sum( (pred_classic - tavg)^2)/n(),
          rmse_level0 = sum( (pred_level0 - tavg)^2)/n()) %>%
  select(ywint, rmse, rmse_classic, rmse_level0) %>%
  slice(1)


# plot on side-by-side historgarm

out_rmses %>% 
  tidyr::pivot_longer(cols = c(rmse, rmse_classic,rmse_level0)) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(position = "dodge", bins = 10) +
  theme_minimal()

out_rmse <- 
  out_rmses %>% 
    ungroup %>% 
    mutate(rmse=mean(rmse),
           rmse_classic=mean(rmse_classic),
           rmse_level0=mean(rmse_level0)) %>%
    slice(1)

out_rmses %>%
  ggplot(aes(x=rmse_classic, y=rmse_level0, color = ywint)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal()

(out_rmse)


# Fit final model on all the data

mod_final <- lme(tavg ~ 
                   ns(tropical_year_loc_sin, df = 1)
                 + ns(tropical_year_sin_component, df = 3)
                 , data = data
                 , random = ~ ns(tropical_year_loc_sin,df=2)[,] | ywint)

plot(mod_final)

# make the data to predict on
pred <- seq.Date(as.Date("2024-01-01"),
                 as.Date("2025-12-31"),
                 by="1 day") %>% 
  tropical_year()

pred$pred <- predict(mod_final, newdata = pred, level = 0)


# plot the results
pred %>%
  mutate(Date =  seq.Date(as.Date("2024-01-01"),
                          as.Date("2025-12-31"),by="1 day")) %>%
  ggplot(aes(x = Date, y = pred)) +
    geom_line() +
    geom_line(data = data, color = "salmon4",
              mapping = aes(x = as.Date(Date), y = tavg)) +
    geom_point( ) +
    scale_x_date(date_labels = "%b %Y",
                 limits= c(as.Date("2024-01-01"),
                           as.Date("2025-12-31"))) +
    theme_minimal()
