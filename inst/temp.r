
hotwater_per_day <- 0.5
budget_heating <- 1730 - hotwater_per_day * 365

act_year_fac <- 
  obs_days_complete %>% 
  filter( year == act_year) %>% 
  slice_tail(n=1) %>% 
  .$ywint



obs_days_complete %>%
  filter( ywint == act_year_fac) %>%
  .[-1,] %>%
  rowwise() %>%
  mutate(
    #diw. = day_in_wint,
    expected_max_energy_needs = predict_future_needs(
      day_in_wint = day_in_wint,
      cum_t_act = tavg_low_cumul,
      full=FALSE
      )[["upper_expected"]],
    expected_min_energy_needs = predict_future_needs(
      day_in_wint = day_in_wint,
      cum_t_act = tavg_low_cumul,
      full=FALSE
    )[["lower_expected"]]
    ) %>%
  mutate(gas_heating = Spent - day_in_wint * hotwater_per_day) %>%
  ggplot(aes(x=Date,y=gas_heating/expected_max_energy_needs)) +
    theme_bw()+ 
    geom_point() +
    geom_line(mapping = aes(x = Date, 
                            y = (1730 - day_in_wint*hotwater_per_day)/
                              expected_max_energy_needs),
              color = "salmon2") +
    geom_hline(yintercept = budget_heating / 
                 predict_future_needs(
                   day_in_wint = 2,
                   cum_t_act = 0,
                   full=FALSE
                 )[["upper_expected"]],
               color = "salmon",
               linetype = "dashed")




