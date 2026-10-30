# 05_build_hdf5_stations.R
# Adds station precipitation and metadata tables to data/dc_flood.h5.
# Safe to rerun: existing /stations/* groups and /metadata station tables are replaced.
library(rhdf5); library(readr); library(dplyr); library(tidyr)

H5 <- "data/dc_flood.h5"
stopifnot(file.exists(H5))

STATIONS <- c("US1DCDC0009", "US1DCDC0014", "USC00182325", "USC00186350", "USW00013743")
AIRPORT  <- "USW00013743"

# ---- Sources: fill these in once ---------------------------------------------
# Evidence beyond the boundary test that Dalecarlia is inside DC
SECOND_SOURCE <- c(USC00182325 = "Location checked on Google Maps, accessed 2026-10-29: Dalecarlia Reservoir is inside DC.")
# Where you found that co-op observation hours are local clock time.
COOP_TIME_SOURCE     <- "NWS Manual 10-1315, Cooperative Station Observations (2007): https://www.weather.gov/media/lwx/coop/COOP%20Station%20Observations.pdf"
# Where you found that CoCoRaHS observers read at 7 a.m. local time.
COCORAHS_TIME_SOURCE <- "CoCoRaHS, observation time 0700 local: https://www.cocorahs.org/content.aspx?page=aboutqc (times vary in practice, most often 7 or 8 a.m.: https://dnr.state.mn.us/climate/historical/acis_stn_meta_coco.html)"
# Airport climatological day (checked): NWS, midnight to midnight local standard time.
AIRPORT_DAY_SOURCE   <- "NWS Chicago, Weather observations FAQ: https://www.weather.gov/lot/weather_observations_faq"
# Network code 1 = CoCoRaHS (checked): GHCN-Daily readme.
GHCND_README         <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt"

# ---- Inputs -------------------------------------------------------------------
grid <- read_csv("data/derived/station_daily_prcp.csv", show_col_types = FALSE,
                 col_types = cols(date = col_date(), prcp_mm = col_double(),
                                  first_year = col_integer(), last_year = col_integer(),
                                  .default = col_character()))
cand <- read_csv("data/derived/station_candidates_ghcnd.csv", show_col_types = FALSE)
stopifnot("dist_to_boundary_m" %in% names(cand))      # run 04a2 first
reqlog <- read_csv("data/derived/station_requests_log.csv", show_col_types = FALSE,
                   col_types = cols(.default = col_character()))
avail_file <- "data/derived/availability_by_event_station.csv"

# ---- Helpers ------------------------------------------------------------------
blank <- function(x) ifelse(is.na(x), "", x)
summ  <- function(x) {
  t <- table(x, useNA = "no")
  if (length(t) == 0) "none reported" else paste(names(t), t, sep = ":", collapse = ", ")
}
network_of <- function(id) switch(substr(id, 1, 3),
  US1 = paste("CoCoRaHS volunteer network (GHCN-Daily network code 1; see", GHCND_README, ")"),
  USC = "USC prefix (NWS cooperative observer network)",
  USW = "USW prefix (airport / WBAN station)", "unknown")

time_convention_of <- function(id) paste(
  "Daily total for the 24 h ending at the reading, labelled with the date of the reading",
  "(inferred from the data: see the station-vs-Daymet comparison).",
  if (id == AIRPORT)
    paste("Airport: NWS climatological day, midnight to midnight local standard time (EST) all year,",
          "i.e. 1 a.m. to 1 a.m. EDT in summer. Source:", AIRPORT_DAY_SOURCE)
  else if (startsWith(id, "US1"))
    paste("CoCoRaHS: observers read at 7 a.m. local clock time (07:00 EST in winter, 06:00 EST during daylight saving).",
          "No reading hour is in the record, so a given day may differ. Source:", COCORAHS_TIME_SOURCE)
  else
    paste("Co-op: the hour in obs_time_hhmm is local clock time (EDT in summer, so 0800 = 07:00 EST).",
          "Source:", COOP_TIME_SOURCE))

# Make a data frame safe for h5write: no Date/POSIXct/factor/list/logical columns,
# no NA strings, and no all-blank string columns (HDF5 cannot store zero-width strings).
tidy <- function(d) {
  d <- as.data.frame(d)
  for (nm in names(d)) {
    x <- d[[nm]]
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) x <- format(x)
    if (is.factor(x))  x <- as.character(x)
    if (is.list(x))    x <- vapply(x, function(v) paste(v, collapse = ";"), "")
    if (is.logical(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- ifelse(is.na(x), "", x)
      if (all(x == "")) x <- rep("none", length(x))
    }
    d[[nm]] <- x
  }
  d
}

# ---- Clear anything from an earlier run -----------------------------------------
kids <- h5ls(H5, recursive = 2)
have <- gsub("^//", "/", paste0(kids$group, "/", kids$name))
for (nm in c(paste0("stations/", STATIONS),
             "metadata/station_candidates", "metadata/station_requests",
             "metadata/availability_by_event_station")) {
  if (paste0("/", nm) %in% have) h5delete(H5, nm)
}
if (!any(h5ls(H5, recursive = 1)$name == "stations")) h5createGroup(H5, "stations")

# ---- Station groups -----------------------------------------------------------
for (id in STATIONS) {
  s  <- grid |> filter(station == id) |> arrange(date)
  cd <- cand |> filter(id == !!id)
  stopifnot(nrow(s) > 0, nrow(cd) == 1, !anyDuplicated(s$date))
  g  <- paste0("stations/", id)
  h5createGroup(H5, g)

  prcp <- s$prcp_mm; prcp[is.na(prcp)] <- -9999
  h5write(as.character(s$date), H5, paste0(g, "/date"))
  h5write(prcp,                 H5, paste0(g, "/prcp_mm"))
  h5write(blank(s$m_flag),      H5, paste0(g, "/measurement_flag"))
  h5write(blank(s$q_flag),      H5, paste0(g, "/quality_flag"))
  h5write(blank(s$s_flag),      H5, paste0(g, "/source_flag"))
  h5write(blank(s$obs_time),    H5, paste0(g, "/obs_time_hhmm"))
  h5write(blank(s$status),      H5, paste0(g, "/status"))

  cls <- if (id == AIRPORT) "airport_not_in_DC" else if (isTRUE(cd$inside_dc)) "in_DC" else "not_confirmed_in_DC"
  at <- list(
    station_id = id, name = cd$name,
    station_class = cls,
    lat = cd$lat, lon = cd$lon, elevation_m = cd$elev,
    catalog_state_code = cd$state,
    inside_dc_boundary_test = as.integer(isTRUE(cd$inside_dc)),
    dist_to_dc_boundary_m = cd$dist_to_boundary_m,
    boundary_source = "US Census cartographic boundary 2022, 1:500k (generalized; unreliable within a few hundred m)",
    second_source = if (id %in% names(SECOND_SOURCE)) SECOND_SOURCE[[id]] else "none",
    network = network_of(id),
    catalog_first_year = cd$first_year, catalog_last_year = cd$last_year,
    variable = "PRCP (daily precipitation)", units = "mm per day",
    nodata_value = -9999,
    nodata_note = "-9999 = no value (see status). 0 = a recorded zero-precipitation day.",
    status_counts = summ(s$status),
    obs_time_counts = summ(s$obs_time),
    time_convention = time_convention_of(id),
    source = "NOAA NCEI GHCN-Daily via the NCEI Access Data Service (daily-summaries)",
    retrieval_log = "/metadata/station_requests",
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  fid <- H5Fopen(H5)
  gid <- H5Gopen(fid, g)
  for (k in names(at)) h5writeAttribute(at[[k]], gid, k)
  H5Gclose(gid); H5Fclose(fid)
  cat(id, cls, "-", summ(s$status), "\n")
}

# ---- Search and retrieval tables ------------------------------------------------
h5write(tidy(cand),   H5, "metadata/station_candidates")
h5write(tidy(reqlog), H5, "metadata/station_requests")
if (file.exists(avail_file))
  h5write(tidy(read_csv(avail_file, show_col_types = FALSE)),
          H5, "metadata/availability_by_event_station")

# ---- Verify -------------------------------------------------------------------
chk <- h5read(H5, "stations/USW00013743/prcp_mm")
ref <- grid |> filter(station == "USW00013743") |> arrange(date) |> pull(prcp_mm)
ref[is.na(ref)] <- -9999
cat("\nReagan National round trip identical:", isTRUE(all.equal(as.numeric(chk), ref)), "\n")
rq <- h5read(H5, "metadata/station_requests")
cat("Request log rows written:", nrow(rq), "of", nrow(reqlog), "\n")
for (id in STATIONS) cat(id, ":", h5readAttributes(H5, paste0("stations/", id))$time_convention, "\n\n")
cat("File size:", round(file.size(H5) / 1e6, 2), "MB\n")
