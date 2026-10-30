# 02c_appeears_request_all.R
# Submit AppEEARS area requests (Daymet prcp, native projection) for every date needed,
# one task per year, each with several date ranges. Resumable: re-run to continue.
library(appeears)
library(sf)
library(dplyr)
library(readr)
library(lubridate)

# ---- Settings ----------------------------------------------------------------
USER         <- Sys.getenv("EARTHDATA_USER")
stopifnot(nzchar(USER))
OUT_ROOT     <- "data/raw/appeears"                    # git-ignored raw downloads
LOG_FILE     <- "data/derived/appeears_tasks.csv"      # keep this in the repo
MAX_WAIT_MIN <- 90                                     # how long to poll in one run
PRODUCT      <- "DAYMET.004"
LAYER        <- "prcp"
dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

token <- rs_login(user = USER)          # unlock the "appeears" keyring if asked

# ---- Dates needed ------------------------------------------------------------
days  <- read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE)
dates <- sort(unique(as.Date(days$est_date)))

# Daymet drops Dec 31 in leap years: nothing to request for those dates
skip <- dates[month(dates) == 12 & day(dates) == 31 & leap_year(dates)]
if (length(skip) > 0) {
  cat("Dropping Dec 31 of leap years (not in Daymet):", format(skip), "\n")
  dates <- dates[!dates %in% skip]
}
years <- sort(unique(year(dates)))
cat("Dates to request:", length(dates), "across", length(years), "years\n\n")

ranges_for_year <- function(y) {
  d <- dates[year(dates) == y]
  tibble(d = d, grp = cumsum(c(TRUE, diff(d) != 1))) |>
    group_by(grp) |>
    summarise(start = min(d), end = max(d), .groups = "drop")
}

# ---- Region: the buffered DC box ---------------------------------------------
bbox <- st_bbox(c(xmin = -77.17, ymin = 38.74, xmax = -76.86, ymax = 39.045),
                crs = st_crs(4326))
roi  <- st_sf(geometry = st_as_sfc(bbox))

# ---- Build one task (native projection, several date ranges) -----------------
build_task <- function(name, rng) {
  df <- data.frame(task = name, subtask = LAYER,
                   start = format(rng$start[1]), end = format(rng$end[1]),
                   product = PRODUCT, layer = LAYER)
  tk <- as.character(rs_build_task(df = df, roi = roi, format = "geotiff"))
  tk <- sub('"projection":"geographic"', '"projection":"native"', tk, fixed = TRUE)
  dates_json <- paste(
    sprintf('{"startDate":"%s","endDate":"%s"}',
            format(rng$start, "%m-%d-%Y"), format(rng$end, "%m-%d-%Y")),
    collapse = ",")
  tk <- sub('"dates":\\[.*?\\]', paste0('"dates":[', dates_json, ']'), tk, perl = TRUE)
  stopifnot(jsonlite::validate(tk),
            grepl('"projection":"native"', tk, fixed = TRUE),
            lengths(regmatches(tk, gregexpr("startDate", tk))) == nrow(rng))
  structure(tk, class = "json")
}

tasks_now <- function() rs_list_task(user = USER)

# ---- Submit any year that has no task yet ------------------------------------
for (y in years) {
  nm <- paste0("daymet_prcp_", y)
  if (nm %in% tasks_now()$task_name) {
    cat(nm, ": already submitted, skipping\n")
    next
  }
  cat(nm, ": submitting", nrow(ranges_for_year(y)), "date range(s)\n")
  tryCatch(
    rs_request(request = build_task(nm, ranges_for_year(y)), user = USER,
               transfer = FALSE, verbose = FALSE),
    error = function(e) cat("  error:", conditionMessage(e), "\n")
  )
  Sys.sleep(3)
}

# ---- Snapshot of our tasks (earliest task per name) --------------------------
snapshot <- function() {
  tk <- tasks_now()
  tk <- tk[grepl("^daymet_prcp_", tk$task_name), c("task_name", "task_id", "status", "created")]
  tk |> arrange(created) |> distinct(task_name, .keep_all = TRUE) |> arrange(task_name)
}

# ---- Poll until nothing is waiting (or the time limit) -----------------------
deadline <- Sys.time() + MAX_WAIT_MIN * 60
repeat {
  log <- snapshot()
  cat(format(Sys.time(), "%H:%M:%S"), " ")
  print(table(log$status))
  waiting <- log$status %in% c("queued", "pending", "processing")
  if (!any(waiting) || Sys.time() > deadline) break
  Sys.sleep(120)
}

# ---- Download finished tasks -------------------------------------------------
n_expected <- sapply(years, function(y) sum(year(dates) == y))
names(n_expected) <- paste0("daymet_prcp_", years)

for (i in seq_len(nrow(log))) {
  if (log$status[i] != "done") next
  dest <- file.path(OUT_ROOT, sub("daymet_prcp_", "", log$task_name[i]))
  if (length(list.files(dest, pattern = "\\.tif$")) == 0) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    cat("Downloading", log$task_name[i], "...\n")
    tryCatch(rs_transfer(task_id = log$task_id[i], user = USER, path = dest),
             error = function(e) cat("  download error:", conditionMessage(e), "\n"))
  }
}

# ---- Log: what was requested, what arrived -----------------------------------
log <- log |>
  mutate(year       = as.integer(sub("daymet_prcp_", "", task_name)),
         n_expected = unname(n_expected[task_name]),
         n_tif      = sapply(year, function(y)
           length(list.files(file.path(OUT_ROOT, y), pattern = "\\.tif$"))),
         logged_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
write_csv(log, LOG_FILE)
print(as.data.frame(log), row.names = FALSE)
cat("\nLog written to", LOG_FILE, "\n")