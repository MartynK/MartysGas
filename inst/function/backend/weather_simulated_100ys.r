
weather_simulated <- simulate_weather(n = 36500)

save(weather_simulated, file = here::here("inst","function","backend","weather_simulated.Rdata"))
