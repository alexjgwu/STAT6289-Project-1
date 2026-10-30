# dcflood_functions.R
# Reusable readers for data/dc_flood.h5. Every function takes the HDF5 path and
# reads ONLY from that file (no CSVs, no downloads).
#
# Time convention used throughout: event times are fixed EST (UTC-5, no daylight
# saving), parsed with tz "Etc/GMT+5". Station and Daymet daily values are labelled
# by DATE; see the metadata attributes on each dataset for what a "day" means.

suppressPackageStartupMessages({
  library(rhdf5); library(dplyr); library(tidyr); library(tibble)
})

EST_TZ <- "Etc/GMT+5"   # POSIX sign is reversed: this is UTC-5

# ---- internal helpers ---------------------------------------------------------
.dc_check <- function(h5) { stopifnot(is.character(h5), file.exists(h5)); invisible(h5) }
.dc_chr   <- function(x) as.vector(x)
.dc_flat  <- function(d) {            # rhdf5 returns 1-d arrays; flatten to plain vectors
  d <- as.data.frame(d)
  d[] <- lapply(d, function(x) if (is.array(x)) as.vector(x) else x)
  as_tibble(d)
}
.dc_parse_est <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = EST_TZ)
.dc_stations <- function(h5) {
  l <- h5ls(h5, recursive = 2)
  l$name[l$group == "/stations"]
}

# ---- 1. list contents ---------------------------------------------------------
# Returns a tibble of every group/dataset with type, shape and key attributes.
dc_list <- function(h5, show_attributes = TRUE) {
  .dc_check(h5)
  l <- h5ls(h5, recursive = TRUE)
  tab <- tibble(path = gsub("^//", "/", paste0(l$group, "/", l$name)),
                type = l$otype, class = l$dclass, dim = l$dim)
  tab$path <- gsub("//", "/", tab$path)
  if (show_attributes) {
    tab$n_attributes <- vapply(tab$path, function(p) {
      a <- try(h5readAttributes(h5, p), silent = TRUE)
      if (inherits(a, "try-error")) NA_integer_ else length(a)
    }, integer(1))
  }
  tab
}

dc_describe <- function(h5, path) {
  .dc_check(h5)
  a <- h5readAttributes(h5, path)
  tibble(attribute = names(a), value = vapply(a, function(v) paste(format(v), collapse = ", "), ""))
}

dc_events <- function(h5) {
  .dc_check(h5)
  ev <- .dc_flat(h5read(h5, "events/index"))
  ev$begin_est_dt <- .dc_parse_est(ev$begin_est)
  ev$end_est_dt   <- .dc_parse_est(ev$end_est)
  ev
}

dc_event_dates <- function(h5, event_id, role = c("event", "all")) {
  .dc_check(h5); role <- match.arg(role)
  d <- .dc_flat(h5read(h5, "events/event_days"))
  dcol <- intersect(c("est_date", "local_date"), names(d))[1]
  d <- d |> filter(event_id == !!event_id)
  if (role == "event" && "role" %in% names(d)) d <- d |> filter(role == "event")
  if (nrow(d) == 0) stop("Unknown event_id: ", event_id)
  sort(as.Date(d[[dcol]]))
}

# ---- 2. get a raster by event / source / date ---------------------------------
# source: "daymet" (only gridded source in this file).
# Returns a terra SpatRaster in Daymet's native LCC projection (metres), NoData = NA.
dc_get_raster <- function(h5, event_id = NULL, date = NULL, source = "daymet") {
  .dc_check(h5)
  if (!requireNamespace("terra", quietly = TRUE)) stop("Install the 'terra' package")
  if (!identical(source, "daymet")) stop("Unknown source '", source, "'. Available: daymet")
  base <- paste0("gridded/", source)
  dates <- .dc_chr(h5read(h5, paste0(base, "/date")))

  if (is.null(date)) {
    if (is.null(event_id)) stop("Give an event_id and/or a date")
    ed <- dc_event_dates(h5, event_id)
    if (length(ed) > 1)
      stop("Event ", event_id, " spans ", length(ed), " dates (", paste(ed, collapse = ", "),
           "). Pass date = one of them.")
    date <- ed
  }
  date <- as.character(as.Date(date))
  k <- match(date, dates)
  if (is.na(k)) stop("Date ", date, " is not in the cube. Dates in the file: ", length(dates),
                     " (first ", dates[1], ", last ", dates[length(dates)], ")")

  m <- h5read(h5, paste0(base, "/prcp"), index = list(NULL, NULL, k))
  m <- matrix(m, nrow = dim(m)[1], ncol = dim(m)[2])      # [x, y]
  m[m == -9999] <- NA
  x <- as.vector(h5read(h5, paste0(base, "/x"))); y <- as.vector(h5read(h5, paste0(base, "/y")))
  if (is.unsorted(x)) stop("x is not ascending; update dc_get_raster")
  if (y[1] < y[length(y)]) stop("y index 1 is not the northernmost row; update dc_get_raster")
  res_x <- abs(x[2] - x[1]); res_y <- abs(y[2] - y[1])
  crs <- try(h5readAttributes(h5, paste0(base, "/prcp"))$crs_proj4, silent = TRUE)
  if (inherits(crs, "try-error") || is.null(crs) || !nzchar(crs))
    crs <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m"
  r <- terra::rast(t(m), crs = crs,
                   extent = terra::ext(min(x) - res_x/2, max(x) + res_x/2,
                                       min(y) - res_y/2, max(y) + res_y/2))
  names(r) <- paste0(source, "_prcp_", date)
  terra::varnames(r) <- "prcp_mm_per_day"
  attr(r, "date") <- date; attr(r, "event_id") <- event_id
  r
}

# ---- 3. plot a raster with geographic context ---------------------------------
# Draws the DC outline (stored in /context/dc_boundary) and the stations (from their
# group attributes). Water bodies are NOT drawn; the Potomac is not in the file.
dc_plot_raster <- function(h5, event_id = NULL, date = NULL, source = "daymet",
                           show_stations = TRUE, zlim = NULL) {
  .dc_check(h5)
  r <- dc_get_raster(h5, event_id, date, source)
  bnd <- .dc_flat(h5read(h5, "context/dc_boundary"))
  lines_v <- terra::project(terra::vect(
    as.matrix(cbind(id = bnd$part, part = bnd$part, x = bnd$lon, y = bnd$lat)),
    type = "lines", crs = "EPSG:4326"), terra::crs(r))

  if (is.null(zlim)) zlim <- range(terra::values(r), na.rm = TRUE)
  ttl <- paste0(if (!is.null(event_id)) paste0(event_id, " | "), "Daymet ", attr(r, "date"))
  terra::plot(r, main = ttl, range = zlim, col = hcl.colors(50, "Blues 3", rev = TRUE),
              plg = list(title = "mm/day"), axes = TRUE, mar = c(5, 4, 3, 5), cex.main = 1)
  terra::lines(lines_v, lwd = 2, col = "black")

  if (show_stations) {
    st <- dc_station_info(h5)
    pts <- terra::project(terra::vect(st[, c("lon", "lat")], geom = c("lon", "lat"), crs = "EPSG:4326"), terra::crs(r))
    pch <- ifelse(st$station_class == "airport_not_in_DC", 17, 21)
    terra::points(pts, pch = pch, bg = "orange", col = "black", cex = 1.4)
    xy <- terra::crds(pts)
    text(xy[, 1], xy[, 2], labels = st$station_id, pos = 4, cex = 0.6, offset = 0.5)
  }
  mtext("Black: DC boundary. Circles: in-DC stations; triangle: airport outside DC. Water not drawn.",
        side = 1, line = 3.5, cex = 0.6)
  invisible(r)
}

# ---- station helpers ----------------------------------------------------------
dc_station_info <- function(h5) {
  .dc_check(h5)
  v <- function(x) if (is.null(x)) NA else as.vector(x)
  bind_rows(lapply(.dc_stations(h5), function(id) {
    a <- h5readAttributes(h5, paste0("stations/", id))
    tibble(station_id = id, name = v(a$name), station_class = v(a$station_class),
           lat = as.numeric(v(a$lat)), lon = as.numeric(v(a$lon)),
           dist_to_dc_boundary_m = as.numeric(v(a$dist_to_dc_boundary_m)),
           network = v(a$network), status_counts = v(a$status_counts),
           obs_time_counts = v(a$obs_time_counts))
  }))
}

.dc_station_full <- function(h5, id) {
  g <- paste0("stations/", id)
  tibble(station = id,
         date = as.Date(.dc_chr(h5read(h5, paste0(g, "/date")))),
         prcp_raw = as.vector(h5read(h5, paste0(g, "/prcp_mm"))),
         measurement_flag = .dc_chr(h5read(h5, paste0(g, "/measurement_flag"))),
         quality_flag     = .dc_chr(h5read(h5, paste0(g, "/quality_flag"))),
         source_flag      = .dc_chr(h5read(h5, paste0(g, "/source_flag"))),
         obs_time_hhmm    = .dc_chr(h5read(h5, paste0(g, "/obs_time_hhmm"))),
         status           = .dc_chr(h5read(h5, paste0(g, "/status")))) |>
    mutate(prcp_mm = ifelse(prcp_raw == -9999, NA_real_, prcp_raw))
}

# ---- 4. station series for an event window ------------------------------------
# One row per station per calendar date from (first event date - buffer) to
# (last event date + buffer). prcp_mm is NA when there is no value (see status).
dc_get_station_series <- function(h5, event_id, stations = NULL, buffer_days = 1) {
  .dc_check(h5)
  ev <- dc_events(h5) |> filter(event_id == !!event_id)
  if (nrow(ev) != 1) stop("Unknown event_id: ", event_id)
  ed <- dc_event_dates(h5, event_id)
  win <- seq(min(ed) - buffer_days, max(ed) + buffer_days, by = "day")
  if (is.null(stations)) stations <- .dc_stations(h5)
  bad <- setdiff(stations, .dc_stations(h5)); if (length(bad)) stop("Unknown station(s): ", paste(bad, collapse = ", "))

  out <- bind_rows(lapply(stations, function(id) {
    s <- .dc_station_full(h5, id)
    tibble(station = id, date = win) |>
      left_join(s |> select(-station), by = "date") |>
      mutate(status = ifelse(is.na(status) | status == "", "not_in_file", status))
  }))
  out |> mutate(event_id = event_id,
                in_event_dates = date %in% ed,
                event_begin_est = ev$begin_est, event_end_est = ev$end_est)
}

# ---- 5. plot a station series with time-zone labels and missing marks ---------
# Each bar spans the station's 24-h window, drawn on a fixed EST axis:
#  * airport (obs 2400): NWS climatological day, midnight-midnight local STANDARD time = EST all year
#  * co-op (reported hour): the hour is local CLOCK time (EDT in summer), converted to EST
#  * CoCoRaHS (US1..., no hour in record): 7 a.m. local clock time, converted to EST
#  * anything else with no hour: drawn midnight-midnight EST, faded (window unknown)
dc_station_windows <- function(s) {
  hh <- suppressWarnings(as.integer(substr(s$obs_time_hhmm, 1, 2)))
  mm <- suppressWarnings(as.integer(substr(s$obs_time_hhmm, 3, 4)))
  known   <- !is.na(hh) & !is.na(mm)
  airport <- known & hh == 24
  coop    <- known & hh < 24
  coco    <- !known & startsWith(s$station, "US1")
  local_end <- function(d, h, m) as.POSIXct(sprintf("%s %02d:%02d:00", d, h, m), tz = "America/New_York")
  day0 <- as.POSIXct(paste(s$date, "00:00:00"), tz = EST_TZ)
  s$t_end <- day0 + 86400
  s$t_end[coop] <- local_end(s$date[coop], hh[coop], mm[coop])
  s$t_end[coco] <- local_end(s$date[coco], 7L, 0L)
  s$t_start <- s$t_end - 86400
  s$t_mid   <- s$t_start + 43200
  s$window <- dplyr::case_when(
    airport ~ "airport: midnight-midnight EST (NWS climatological day)",
    coop    ~ "co-op: ends at reported hour, local clock",
    coco    ~ "CoCoRaHS: ends 7 a.m. local clock",
    TRUE    ~ "no hour reported (drawn midnight-midnight)")
  s
}

dc_plot_station_series <- function(h5, event_id, stations = NULL, buffer_days = 1) {
  .dc_check(h5)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install the 'ggplot2' package")
  library(ggplot2)
  s  <- dc_station_windows(dc_get_station_series(h5, event_id, stations, buffer_days))
  ev <- dc_events(h5) |> filter(event_id == !!event_id)

  hrs <- s |> filter(obs_time_hhmm != "") |> group_by(station) |>
    summarise(obs = paste(sort(unique(obs_time_hhmm)), collapse = "/"), .groups = "drop")
  s <- s |> left_join(hrs, by = "station") |>
    mutate(panel = paste0(station, "  (reads at: ",
                          ifelse(!is.na(obs), obs,
                                 ifelse(startsWith(station, "US1"), "7 a.m. local, CoCoRaHS", "not reported")), ")"))

  bars    <- s |> filter(!is.na(prcp_mm), prcp_mm > 0)
  miss    <- s |> filter(is.na(prcp_mm)) |> mutate(y = 0)
  flagged <- s |> filter(status == "value_quality_flagged")

  ggplot() +
    annotate("rect", xmin = ev$begin_est_dt, xmax = ev$end_est_dt, ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.18) +
    geom_rect(data = bars, aes(xmin = t_start, xmax = t_end, ymin = 0, ymax = prcp_mm, fill = window),
              colour = "white", linewidth = 0.3) +
    geom_point(data = flagged, aes(t_mid, prcp_mm), shape = 21, size = 3, fill = "gold", colour = "black") +
    geom_point(data = miss, aes(t_mid, y, colour = status), shape = 4, size = 3, stroke = 1.2) +
    facet_wrap(~panel, ncol = 1, scales = "fixed") +
    scale_fill_manual(values = c("airport: midnight-midnight EST (NWS climatological day)" = "#3B5B8C",
                                 "co-op: ends at reported hour, local clock" = "#3A3A3A",
                                 "CoCoRaHS: ends 7 a.m. local clock" = "#9A6B2F",
                                 "no hour reported (drawn midnight-midnight)" = "grey75"),
                      name = "Bar window", drop = TRUE) +
    scale_x_datetime(timezone = EST_TZ, date_labels = "%b %d\n%H:%M", date_breaks = "1 day",
                     name = "Time, EST (UTC\u22125, no daylight saving)") +
    scale_colour_brewer(palette = "Set1", name = "No value (x)") +
    labs(title = paste0(event_id, ": station daily precipitation"),
         subtitle = paste0("Shaded: event ", ev$begin_est, " to ", ev$end_est, " (EST)\n",
                           "Each bar spans the station's 24-h window. Reading hours are local clock time (EDT in summer),\n",
                           "shown here on the EST axis; the airport day is midnight-midnight EST all year. Gold ring = quality-flagged value."),
         y = "Precipitation over the window (mm)") +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.box = "vertical")
}

# ---- 6. coverage / missingness summary ----------------------------------------
# One row per event x station: days needed, days with a value, and by-status counts,
# for event dates only (buffer days excluded). Add Daymet rows with include_daymet.
dc_coverage <- function(h5, event_id = NULL, include_daymet = TRUE) {
  .dc_check(h5)
  days <- .dc_flat(h5read(h5, "events/event_days"))
  dcol <- intersect(c("est_date", "local_date"), names(days))[1]
  days <- days |> mutate(date = as.Date(.data[[dcol]]))
  if ("role" %in% names(days)) days <- days |> filter(role == "event")
  days <- days |> distinct(event_id, date)
  if (!is.null(event_id)) days <- days |> filter(event_id %in% !!event_id)

  st <- bind_rows(lapply(.dc_stations(h5), function(id) .dc_station_full(h5, id) |> select(station, date, status)))
  stn <- days |>
    inner_join(st, by = "date", relationship = "many-to-many") |>
    group_by(event_id, station) |>
    summarise(source = "station", days_needed = n(),
              days_with_value = sum(status %in% c("value", "value_quality_flagged")),
              days_quality_flagged = sum(status == "value_quality_flagged"),
              days_no_record = sum(status == "no_record"),
              days_returned_NA = sum(status == "returned_NA"),
              days_outside_catalog_years = sum(status == "outside_catalog_years"),
              days_download_failed = sum(status == "download_failed"), .groups = "drop") |>
    mutate(frac_with_value = days_with_value / days_needed)

  if (!include_daymet) return(stn)
  cube_dates <- as.Date(.dc_chr(h5read(h5, "gridded/daymet/date")))
  mask <- h5read(h5, "gridded/daymet/valid_mask")
  dm <- days |> group_by(event_id) |>
    summarise(days_needed = n(), days_in_cube = sum(date %in% cube_dates), .groups = "drop")
  # fraction of footprint cells with a real value, on each needed date
  frac <- vapply(seq_len(nrow(days)), function(i) {
    k <- match(days$date[i], cube_dates); if (is.na(k)) return(NA_real_)
    v <- h5read(h5, "gridded/daymet/prcp", index = list(NULL, NULL, k))
    v <- matrix(v, nrow = dim(v)[1], ncol = dim(v)[2])
    mean(v[mask == 1] != -9999)
  }, numeric(1))
  dm2 <- days |> mutate(f = frac) |> group_by(event_id) |>
    summarise(min_footprint_frac_valid = min(f, na.rm = TRUE), .groups = "drop")
  dmy <- dm |> left_join(dm2, by = "event_id") |>
    transmute(event_id, station = "DAYMET (grid)", source = "gridded",
              days_needed, days_with_value = days_in_cube,
              frac_with_value = days_in_cube / days_needed, min_footprint_frac_valid)
  bind_rows(stn, dmy) |> arrange(event_id, source, station)
}
