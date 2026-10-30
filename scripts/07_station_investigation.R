# 07_station_investigation.R
# Independent investigation: how well does Daymet at each station's 1-km cell agree with the
# station's daily value, and does the agreement depend on the station's observation hour?
# Reads only data/dc_flood.h5 (plus R/dcflood_functions.R).
#
# CAVEATS (state these in the report):
#  * GHCN-Daily stations are inputs to Daymet, so this is NOT an independent validation of Daymet.
#  * The 372 dates in the file are event dates +/- a buffer, so they over-represent wet weather.
#  * "Same date" and "previous date" Daymet alignments are tried because the station day window
#    differs from a calendar day: co-op and CoCoRaHS days end at a morning reading (local clock time),
#    the airport day is midnight-midnight EST (NWS climatological day).
source("R/dcflood_functions.R")
suppressPackageStartupMessages({ library(terra); library(ggplot2); library(readr) })
H5 <- "data/dc_flood.h5"
dir.create("figures", showWarnings = FALSE)

# ---- Daymet series at each station's cell -------------------------------------
cube  <- h5read(H5, "gridded/daymet/prcp")                       # [x, y, time]
dates <- as.Date(as.vector(h5read(H5, "gridded/daymet/date")))
x <- as.vector(h5read(H5, "gridded/daymet/x")); y <- as.vector(h5read(H5, "gridded/daymet/y"))
mask <- h5read(H5, "gridded/daymet/valid_mask")
stopifnot(dim(cube)[1] == length(x), dim(cube)[2] == length(y), dim(cube)[3] == length(dates))

r0  <- dc_get_raster(H5, date = dates[1])
st  <- dc_station_info(H5)
pts <- project(vect(st[, c("lon", "lat")], geom = c("lon", "lat"), crs = "EPSG:4326"), crs(r0))
xy  <- crds(pts)
st$ix <- vapply(xy[, 1], function(v) which.min(abs(x - v)), 1L)
st$iy <- vapply(xy[, 2], function(v) which.min(abs(y - v)), 1L)
st$cell_in_footprint <- mapply(function(i, j) mask[i, j] == 1, st$ix, st$iy)
print(st[, c("station_id", "station_class", "ix", "iy", "cell_in_footprint")])

dm_at <- function(i) {
  v <- cube[st$ix[i], st$iy[i], ]; v[v == -9999] <- NA
  tibble(station = st$station_id[i], date = dates, daymet = as.numeric(v))
}
dm <- bind_rows(lapply(seq_len(nrow(st)), dm_at))
dm <- dm |> left_join(
  dm |> transmute(station, date = date + 1, daymet_prev = daymet),   # Daymet value for the day before
  by = c("station", "date"))

# ---- Pair with station values -------------------------------------------------
sv <- bind_rows(lapply(st$station_id, function(id) .dc_station_full(h5 = H5, id = id))) |>
  filter(status %in% c("value", "value_quality_flagged")) |>
  mutate(obs_class = case_when(                       # station day window, by type
    obs_time_hhmm == "2400" ~ "airport: midnight-midnight EST",
    obs_time_hhmm == "" & startsWith(station, "US1") ~ "CoCoRaHS: 7 a.m. local",
    obs_time_hhmm == "" ~ "no hour reported",
    as.integer(substr(obs_time_hhmm, 1, 2)) <= 10 ~ "co-op morning (local clock)",
    TRUE ~ "co-op afternoon/evening (local clock)"))
pairs <- sv |> select(station, date, station_mm = prcp_mm, obs_class) |>
  inner_join(dm, by = c("station", "date")) |>
  tidyr::pivot_longer(c(daymet, daymet_prev), names_to = "alignment", values_to = "daymet_mm") |>
  mutate(alignment = recode(alignment, daymet = "Daymet same date", daymet_prev = "Daymet previous date")) |>
  filter(!is.na(daymet_mm))

metrics <- function(d) tibble(
  n_days = nrow(d),
  n_either_wet = sum(d$station_mm >= 1 | d$daymet_mm >= 1),
  bias_mm = mean(d$daymet_mm - d$station_mm),
  mae_mm = mean(abs(d$daymet_mm - d$station_mm)),
  corr = if (nrow(d) > 2 && sd(d$station_mm) > 0 && sd(d$daymet_mm) > 0) cor(d$station_mm, d$daymet_mm) else NA_real_,
  wet_agree = mean((d$station_mm >= 1) == (d$daymet_mm >= 1)))
res <- pairs |> group_by(station, obs_class, alignment) |> group_modify(~ metrics(.x)) |> ungroup()
print(res, n = Inf, width = Inf)
write_csv(res, "data/derived/station_vs_daymet_metrics.csv")

# ---- Figure -------------------------------------------------------------------
p <- ggplot(pairs, aes(station_mm, daymet_mm, colour = obs_class)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_x_sqrt() + scale_y_sqrt() +
  facet_grid(alignment ~ station) +
  labs(x = "Station daily value (mm, sqrt scale)", y = "Daymet at station cell (mm, sqrt scale)",
       colour = "Station day window",
       title = "Station vs Daymet, all dates in the file",
       subtitle = "Stations are inputs to Daymet, so this is not an independent validation. Dates are event dates +/- buffer.") +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
ggsave("figures/station_vs_daymet.png", p, width = 13, height = 6, dpi = 150)
