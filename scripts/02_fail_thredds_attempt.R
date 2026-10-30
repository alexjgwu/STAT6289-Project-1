# 02_fail_thredds_attempt.R
# FAILED ATTEMPT, kept for the record. All 28 yearly requests to ORNL THREDDS/NCSS
# (via daymetr::download_daymet_ncss) failed; see data/derived/daymet_download_log.csv.
# Daymet was retrieved through NASA AppEEARS instead (scripts 02a to 02c).

library(daymetr)
library(readr)
library(dplyr)
library(lubridate)

# ---- Settings ---------------------------------------------------------------
# DC approximate extent (verify against an official boundary), plus a buffer in degrees.
DC <- list(north = 38.995, south = 38.79, west = -77.12, east = -76.91)
BUFFER_DEG <- 0.05      # about 5.5 km N-S and 4.3 km E-W at DC latitude

# daymetr bbox order: c(lat_top, lon_left, lat_bottom, lon_right)
BBOX <- c(DC$north + BUFFER_DEG, DC$west - BUFFER_DEG,
          DC$south - BUFFER_DEG, DC$east + BUFFER_DEG)

OUT_DIR  <- "data/raw/daymet"                         # git-ignored raw downloads
LOG_FILE <- "data/derived/daymet_download_log.csv"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

# ---- Years needed -----------------------------------------------------------
days  <- read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE)
years <- sort(unique(year(days$est_date)))
cat("Years to download:", length(years), "\n", years, "\n\n")
cat("Bounding box (top, left, bottom, right):", BBOX, "\n\n")

# ---- Download one year at a time, logging every outcome ----------------------
find_files <- function(y) {
  list.files(OUT_DIR, pattern = paste0("prcp.*", y), full.names = TRUE)
}

download_year <- function(y) {
  existing <- find_files(y)
  if (length(existing) > 0) {
    return(tibble(year = y, status = "already_downloaded",
                  file = paste(basename(existing), collapse = ";"),
                  message = NA_character_))
  }
  res <- tryCatch({
    download_daymet_ncss(location = BBOX, start = y, end = y, param = "prcp",
                         frequency = "daily", path = OUT_DIR,
                         silent = TRUE, force = TRUE)
    f <- find_files(y)
    if (length(f) == 0) stop("call finished but no file found for this year")
    list(status = "success", file = paste(basename(f), collapse = ";"),
         message = NA_character_)
  }, error = function(err) {
    list(status = "failed", file = NA_character_, message = conditionMessage(err))
  })
  Sys.sleep(1)    # be polite to the server
  tibble(year = y, status = res$status, file = res$file, message = res$message)
}

log <- bind_rows(lapply(years, function(y) {
  cat("Year", y, "... ")
  r <- download_year(y)
  cat(r$status, "\n")
  r
}))

log <- log |>
  mutate(
    retrieved_utc   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    daymetr_version = as.character(packageVersion("daymetr")),
    bbox_top_left_bottom_right = paste(BBOX, collapse = ", "),
    buffer_deg      = BUFFER_DEG
  )

write_csv(log, LOG_FILE)
cat("\n--- Summary ---\n")
print(count(log, status))
if (any(log$status == "failed")) {
  print(filter(log, status == "failed") |> select(year, message))
}
cat("\nLog written to", LOG_FILE, "\n")