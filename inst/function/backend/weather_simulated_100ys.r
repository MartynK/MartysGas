
weather_simulated <- simulate_weather(n = 36500)

save(weather_simulated, file = here::here("data", "weather_simulated.Rdata"))
