# 04a_station_search.R
# Systematic search of the GHCN-Daily catalog for precipitation stations in DC.
library(readr)
library(dplyr)
library(lubridate)

dir.create("data/raw/stations", recursive = TRUE, showWarnings = FALSE)
base <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/"
# pull station data from NOAA site
for (f in c("ghcnd-stations.txt", "ghcnd-inventory.txt", "readme.txt")) {
  dest <- file.path("data/raw/stations", f)
  if (!file.exists(dest)) download.file(paste0(base, f), dest, mode = "wb")
}
cat("Catalog retrieved (UTC):", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n")

event_years_vec <- year(as.Date(
  read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE)$est_date))

stn <- read_fwf("data/raw/stations/ghcnd-stations.txt",
                fwf_cols(id = c(1, 11), lat = c(13, 20), lon = c(22, 30),
                         elev = c(32, 37), state = c(39, 40), name = c(42, 71)),
                col_types = "cdddcc")
inv <- read_fwf("data/raw/stations/ghcnd-inventory.txt",
                fwf_cols(id = c(1, 11), lat = c(13, 20), lon = c(22, 30),
                         element = c(32, 35), first_year = c(37, 40), last_year = c(42, 45)),
                col_types = "cddcii")

# Candidates: state code DC, or coordinates inside an approximate DC extent
in_box <- function(lat, lon) lat > 38.79 & lat < 39.00 & lon > -77.12 & lon < -76.91
cand <- stn |> filter(state == "DC" | in_box(lat, lon))

prcp <- inv |> filter(element == "PRCP", id %in% cand$id) |>
  select(id, first_year, last_year)

res <- cand |> inner_join(prcp, by = "id") |>
  mutate(state_is_dc = state == "DC",
         in_dc_box   = in_box(lat, lon),
         event_years = mapply(function(a, b) sum(event_years_vec >= a & event_years_vec <= b),
                              first_year, last_year)) |>
  arrange(desc(event_years))
print(res, n = Inf, width = Inf)
write_csv(res, "data/derived/station_candidates_ghcnd.csv")