# 04c_event_availability.R
library(readr); library(dplyr); library(tidyr)

ev <- read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE) |>
  filter(role == "event") |> mutate(date = as.Date(est_date))
st <- read_csv("data/derived/station_daily_prcp.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date))

av <- ev |>
  inner_join(st, by = "date", relationship = "many-to-many") |>
  group_by(event_id, station) |>
  summarise(n_event_days = n(),
            n_value      = sum(!is.na(prcp_mm)),
            n_outside    = sum(status == "outside_catalog_years"),
            .groups = "drop") |>
  mutate(frac_with_value = round(n_value / n_event_days, 2))

wide <- av |> select(event_id, station, frac_with_value) |>
  pivot_wider(names_from = station, values_from = frac_with_value)
print(wide, n = 20, width = Inf)
write_csv(av, "data/derived/availability_by_event_station.csv")

good <- av |> group_by(event_id) |>
  summarise(stations_complete = sum(frac_with_value == 1), .groups = "drop") |>
  arrange(desc(stations_complete))
print(head(good, 15))