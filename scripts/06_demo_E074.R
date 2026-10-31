# 06_demo_E074.R  -- demonstration on one representative event, reading only data/dc_flood.h5
source("R/dcflood_functions.R")
H5 <- "data/dc_flood.h5"
EV <- "E074"
dir.create("figures", showWarnings = FALSE)

# 1. What is in the file?
print(dc_list(H5), n = Inf)
print(dc_sources(H5), width = Inf)
print(dc_describe(H5, "gridded/daymet/prcp"), n = Inf, width = Inf)
print(dc_station_info(H5), width = Inf)

# 2. The event and its dates
ev <- dc_events(H5) |> dplyr::filter(event_id == EV)
print(dplyr::select(ev, event_id, begin_est, end_est, duration_hours), width = Inf)
dates <- dc_event_dates(H5, EV); print(dates)

# 3. Rasters (one per event date) with geographic context
r1 <- dc_get_raster(H5, EV, date = dates[1]); print(r1)
zl <- range(unlist(lapply(dates, function(d) terra::values(dc_get_raster(H5, EV, date = d)))), na.rm = TRUE)
png("figures/E074_daymet.png", width = 1600, height = 800, res = 150)
par(mfrow = c(1, length(dates)))
for (d in dates) dc_plot_raster(H5, EV, date = d, zlim = zl)
dev.off()

# 4. Station series for the event window
ss <- dc_get_station_series(H5, EV)
print(ss |> dplyr::select(station, date, prcp_mm, status, obs_time_hhmm, in_event_dates), n = Inf)
p <- dc_plot_station_series(H5, EV)
ggplot2::ggsave("figures/E074_stations.png", p, width = 9, height = 10, dpi = 150)

# 5. Coverage / missingness for this event and for all events
print(dc_coverage(H5, EV), width = Inf)
cov_all <- dc_coverage(H5)
readr::write_csv(cov_all, "data/derived/coverage_all_events.csv")
print(cov_all |> dplyr::group_by(station) |>
        dplyr::summarise(events_complete = sum(frac_with_value == 1),
                         events_partial  = sum(frac_with_value > 0 & frac_with_value < 1),
                         events_none     = sum(frac_with_value == 0)), width = Inf)
