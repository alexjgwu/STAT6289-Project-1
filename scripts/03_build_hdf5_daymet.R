# 03_build_hdf5_daymet.R
# Validate the AppEEARS Daymet GeoTIFFs and write the HDF5 database
# (event index + Daymet precipitation cube + metadata). Stations are added by a later script.
library(terra)
library(rhdf5)
library(readr)
library(dplyr)
library(lubridate)

H5  <- "data/dc_flood.h5"
RAW <- "data/raw/appeears"

# ---- Part 1: find and validate the GeoTIFFs ---------------------------------
files <- list.files(RAW, pattern = "^DAYMET.*_prcp_doy\\d{7}.*\\.tif$",
                    recursive = TRUE, full.names = TRUE)
m <- regmatches(basename(files), regexec("doy(\\d{4})(\\d{3})", basename(files)))
yr  <- as.integer(sapply(m, `[`, 2))
doy <- as.integer(sapply(m, `[`, 3))
date <- as.Date(paste0(yr, "-01-01")) + doy - 1
o <- order(date); files <- files[o]; yr <- yr[o]; doy <- doy[o]; date <- date[o]
cat("GeoTIFFs found:", length(files), "\n")

# dates requested vs dates received (Dec 31 of leap years is not in Daymet)
req <- sort(unique(as.Date(read_csv("data/derived/event_days_needed.csv",
                                    show_col_types = FALSE)$est_date)))
req <- req[!(month(req) == 12 & day(req) == 31 & leap_year(req))]
cat("Requested dates:", length(req), "| received:", length(date),
    "| missing:", length(setdiff(req, date)), "| unexpected:", length(setdiff(date, req)), "\n")
stopifnot(!anyDuplicated(date), setequal(req, date))

r <- rast(files)                      # errors if the grids differ
cat("Grid:", nrow(r), "rows x", ncol(r), "cols x", nlyr(r), "layers\n")
cat("Resolution:", res(r), "m\nCRS:", crs(r, proj = TRUE), "\n")

v <- values(r)
cat("NA cells:", sum(is.na(v)), "| cells equal to -9999:", sum(v == -9999, na.rm = TRUE),
    "| negative values:", sum(v < 0 & v != -9999, na.rm = TRUE),
    "| range (mm/day):", round(range(v[v != -9999], na.rm = TRUE), 2), "\n")
stopifnot(!any(v < 0 & v != -9999, na.rm = TRUE))
nodata_cells <- sum(is.na(v) | v == -9999)

# Footprint check: every cell must be NA on all days or on none, so that -9999
# means "outside the clipped area" and never "missing rainfall"
cell_na_days <- rowSums(is.na(v) | v == -9999)
cat("Cells always NA:", sum(cell_na_days == nlyr(r)),
    "| partly NA:", sum(cell_na_days > 0 & cell_na_days < nlyr(r)),
    "| never NA:", sum(cell_na_days == 0), "\n")
stopifnot(all(cell_na_days %in% c(0, nlyr(r))))
valid <- matrix(as.integer(cell_na_days == 0), nrow = nrow(r), ncol = ncol(r), byrow = TRUE)  # [y, x]

# ---- Arrays for HDF5 ---------------------------------------------------------
# terra array is [row, col, layer] = [y, x, time]. rhdf5 reverses dimensions on disk,
# so build the R array as [x, y, time]; the file then shows (time, y, x) in h5py etc.
arr <- as.array(r)
arr[is.na(arr)] <- -9999
arr <- aperm(arr, c(2, 1, 3))
x <- xFromCol(r, 1:ncol(r)); y <- yFromRow(r, 1:nrow(r))   # cell centres, y runs north -> south
nx <- length(x); ny <- length(y); nt <- length(date)

# ---- Part 2: write the HDF5 file --------------------------------------------
if (file.exists(H5)) file.remove(H5)
h5createFile(H5)
for (g in c("events", "gridded", "gridded/daymet", "stations", "metadata")) h5createGroup(H5, g)

# events
idx  <- read_csv("data/derived/event_index.csv", show_col_types = FALSE,
                 col_types = cols(.default = col_character()))
flags <- grep("^flag_", names(idx), value = TRUE)
idx[flags] <- lapply(idx[flags], function(z) as.integer(z == "TRUE"))
idx$duration_hours <- as.numeric(idx$duration_hours); idx$source_row <- as.integer(idx$source_row)
days <- read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE) |>
  mutate(est_date = as.character(est_date)) |> as.data.frame()
h5write(as.data.frame(idx), H5, "events/index")
h5write(days, H5, "events/event_days")

# Daymet cube: chunk = one day, so reading one date reads one chunk
h5createDataset(H5, "gridded/daymet/prcp", dims = c(nx, ny, nt), storage.mode = "double",
                H5type = "H5T_IEEE_F32LE", chunk = c(nx, ny, 1), level = 6, fillValue = -9999)
h5write(arr, H5, "gridded/daymet/prcp")
h5write(x, H5, "gridded/daymet/x"); h5write(y, H5, "gridded/daymet/y")
h5write(t(valid), H5, "gridded/daymet/valid_mask")      # [x, y]; 1 = inside the clipped area
h5write(as.character(date), H5, "gridded/daymet/date")
h5write(doy, H5, "gridded/daymet/doy"); h5write(yr, H5, "gridded/daymet/year")
h5write(basename(files), H5, "gridded/daymet/source_file")

# metadata tables
tasks <- read_csv("data/derived/appeears_tasks.csv", show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
h5write(as.data.frame(tasks), H5, "metadata/appeears_tasks")

# ---- Attributes ---------------------------------------------------------------
fid <- H5Fopen(H5)
did <- H5Dopen(fid, "gridded/daymet/prcp")
at <- list(
  long_name        = "Daymet daily precipitation",
  units            = "mm/day (daily total, water-equivalent depth)",
  nodata_value     = -9999,
  nodata_cells_in_cube = nodata_cells,
  dimensions       = "R index [x, y, time]; on disk (time, y, x). y index 1 = northernmost row",
  crs_proj4        = crs(r, proj = TRUE),
  crs_note         = "Daymet native Lambert Conformal Conic (WGS84), metres; not reprojected or resampled",
  resolution_m     = 1000,
  spatial_subset   = "bbox lon -77.17..-76.86, lat 38.74..39.045 (DC extent + ~0.05 deg buffer), AppEEARS area request",
  temporal_resolution = "daily",
  calendar         = "Daymet 365-day calendar; Dec 31 omitted in leap years (no requested date affected)",
  time_convention  = paste("Daymet daily value = one calendar day. Per the Daymet station-level guide, a 24-h day",
                           "based on local time, midnight to midnight; standard vs daylight time and the zone applied",
                           "at DC are NOT stated in the gridded guide, the AppEEARS README or the AppEEARS ISO 19115",
                           "metadata. Event times in this project are fixed EST (UTC-5); in summer a Daymet day may",
                           "differ from the EST calendar day by up to 1 hour."),
  source           = "Daymet V4 daily surface weather, ORNL DAAC, via NASA AppEEARS product DAYMET.004",
  doi              = "10.3334/ORNLDAAC/2129",
  layer            = "prcp",
  retrieval_method = paste0("AppEEARS API via R package appeears ", as.character(packageVersion("appeears")),
                            "; area request, native projection, GeoTIFF; one task per year (see /metadata/appeears_tasks)"),
  retrieved_utc    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  processing       = "Merged per-date GeoTIFFs into one cube; NA -> -9999; no unit conversion, resampling or reprojection"
)
for (k in names(at)) h5writeAttribute(at[[k]], did, k)
H5Dclose(did)

gid <- H5Gopen(fid, "gridded/daymet")
h5writeAttribute("Dates are Daymet calendar dates (YYYY-MM-DD from file day-of-year), not UTC instants", gid, "date_note")
h5writeAttribute(paste("valid_mask: 1 = cell inside the clipped area of interest, 0 = outside.",
                       "Outside cells are stored as -9999 in prcp on every date.",
                       "-9999 therefore marks 'no data (outside AOI)', never a missing rainfall observation;",
                       "0 mm is a real zero-rain value."), gid, "mask_note")
H5Gclose(gid)
H5Fclose(fid)

# ---- Read back and verify -----------------------------------------------------
back <- h5read(H5, "gridded/daymet/prcp")
cat("\nRound trip identical:", isTRUE(all.equal(back, arr, tolerance = 1e-5)), "\n")
print(h5ls(H5))
cat("File size:", round(file.size(H5) / 1e6, 2), "MB\n")