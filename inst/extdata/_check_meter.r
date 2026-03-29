library(readxl); library(dplyr)
d <- read_excel(here::here("inst", "extdata", "gaz.xlsx"),
                sheet = "Mero_rendetlen")

d2 <- d %>%
  arrange(Datum) %>%
  mutate(prev_mero = lag(Mero),
         delta = Mero - prev_mero)

cat("=== Meter resets (Value drops > 100) ===\n")
resets <- d2 %>% filter(delta < -100)
print(resets %>% select(Datum, Mero, prev_mero, delta))

cat("\n=== Around 2020 (all readings) ===\n")
d2 %>%
  filter(Datum > as.POSIXct("2019-12-01"),
         Datum < as.POSIXct("2021-01-01")) %>%
  select(Datum, Mero, prev_mero, delta, Gaz) %>%
  print(n = 40)
