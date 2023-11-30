# stuff from iter3

smooth.spline( weather$day_in_year, weather$tsum) %>% plot(type="l")
abline(h=10,col="red")
abline(v=90,col="red")

weather %>%
  mutate(date_yday = parse_date_time(x = as.character(day_in_year), orders = "j") ) %>%
  ggplot( aes( x = date_yday, #day_in_year, 
               y = dme, group = year, color = year)) +
  theme_bw() +
  geom_line() +
  scale_x_datetime(date_breaks="8 weeks") +
  geom_vline( xintercept = as_datetime("2022-09-02"), color = "red")

