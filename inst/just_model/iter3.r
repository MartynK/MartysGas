
dividers = seq.Date(from = {min(dat$Date)+days(365*3)} %>% 
                      as.Date() %>% 
                      `month<-`(8) %>% 
                      `day<-`(1),
                    to = as.Date(max(dat$Date)), by = "12 months")

out <- dat %>% mutate(pred=NA,pred_classic=NA,pred_level0=NA) %>% .[0,]

pb <- txtProgressBar(min = 0, max = length(dividers), style = 3)

for (i in 1:length(dividers)) {
  
  act <- predict_nxt_n_month(dat, dividers[i], df_b = 1, df_c = 3, df_rand = 2)
  
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

