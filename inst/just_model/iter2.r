# Load packages
library(dplyr)
library(lubridate)
library(splines)
library(splines2)
library(ggplot2)
library(nlme)

# load the data
load(here::here("inst","just_model","iter1.rdata"))

predict_nxt_n_month <- function(data, divider = as.Date("2023-11-20"), 
                                df_b = 5, df_c = 4, df_rand = 2,
                                months = 12) {
  
  out <- data %>% mutate(pred=NA, pred_classic=NA, pred_level0=NA) %>% .[0,]
  
  past <- data %>% filter(Date <= divider)
  future <- data %>% filter(Date > divider, 
                            Date <= divider + ceiling(months * 30.5)) %>%
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
    }, silent = TRUE)
    
    try({
      future$pred[!(future$ywint %in% past$ywint)] <- predict(mod_past, newdata = future[!(future$ywint %in% past$ywint),], level = 0)
    }, silent = TRUE)
    
    try({
      future$pred_level0 <- predict(mod_past, newdata = future, level = 0)
    }, silent = TRUE)
    
    try({
      future$pred_classic <- predict(mod_classic, newdata = future)
    })
    
    out <- bind_rows(out, future)
  })
  return(list(mod_past = mod_past, mod_classic = mod_classic, out = out))
}

return_rmse_lme <- function(data, divider = as.Date("2024-03-20"), 
                            df_b = 5, df_c = 4, df_rand = 2, 
                            type = "pred", months = 12) {
  
  out <- predict_nxt_n_month(data, divider, df_b, df_c, 
                             df_rand, months = months)
  
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

dividers = seq.Date(from = {min(dat$Date)+days(365*3)} %>% 
                             as.Date() %>% 
                             `month<-`(8) %>% 
                             `day<-`(1), 
                    to = as.Date(max(dat$Date)), by = "12 months")

pb <- txtProgressBar(min = 0, max = nrow(out), style = 3)
for (i in 1:nrow(out)) {
  act <- sapply(dividers, return_rmse_lme, 
                data = dat, df_b = out$df_b[i], df_c = out$df_c[i], 
                df_rand = out$df_rand[i], type = "pred", months = 12) %>%
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
  mutate(gain_level0 = rmse_classic - rmse_level0,
         rmse_level0_scale = rmse_level0 / (
           max(rmse_level0) - min(rmse_level0)),
         rmse_level0_scale = ifelse(rmse_level0_scale >= median(rmse_level0_scale), 
                                    median(rmse_level0_scale), rmse_level0_scale)
  )
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
  ggplot(aes(x = df_b, y = rmse_level0_scale, 
             color = df_c, group = df_c)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "RMSE of the model") +
  geom_hline(yintercept = min( out$rmse_level0_scale ), 
             linetype = "dashed", color = "salmon4") +
  scale_y_log10() +
  facet_wrap(~df_rand, ncol = 1)
