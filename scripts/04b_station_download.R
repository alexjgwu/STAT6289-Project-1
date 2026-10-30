# 04b_station_download.R
# Download daily PRCP for chosen stations (needed years only), with retries, and
# build a complete station x date table with explicit status.
library(readr); library(dplyr); library(tidyr); library(lubridate)

STATIONS <- c("US1DCDC0009", "US1DCDC0014",      # CoCoRaHS volunteer stations (in DC)
              "USC00182325", "USC00186350",      # NWS co-op stations (in DC)
              "USW00013743")                      # Reagan National: AIRPORT, outside DC, kept as reference
TRIES   <- 4
REFETCH <- FALSE     # TRUE = download again; FALSE = reuse the saved raw file and log

RAW_F <- "data/raw/stations/ghcnd_prcp_raw.csv"
LOG_F <- "data/derived/station_requests_log.csv"

days   <- read_csv("data/derived/event_days_needed.csv", show_col_types = FALSE)
needed <- sort(unique(as.Date(days$est_date)))
cand   <- read_csv("data/derived/station_candidates_ghcnd.csv", show_col_types = FALSE)
span   <- cand |> filter(id %in% STATIONS) |> select(station = id, name, first_year, last_year)
dir.create("data/raw/stations", recursive = TRUE, showWarnings = FALSE)

fetch_year <- function(st, y) {
  d <- needed[year(needed) == y]
  url <- sprintf(paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries",
                        "&stations=%s&startDate=%s&endDate=%s&dataTypes=PRCP&format=csv",
                        "&includeAttributes=true&includeStationName=1&units=metric"),
                 st, format(min(d)), format(max(d)))
  msg <- NA_character_; out <- NULL
  for (k in seq_len(TRIES)) {
    res <- tryCatch(list(d = read_csv(url, show_col_types = FALSE,
                                      col_types = cols(.default = col_character())),
                         m = NA_character_),
                    error = function(e) list(d = NULL, m = conditionMessage(e)))
    if (!is.null(res$d)) { out <- res$d; msg <- NA_character_; break }
    msg <- res$m
    Sys.sleep(5 * k)
  }
  list(url = url, data = out, msg = msg, attempts = k)
}

if (REFETCH || !file.exists(RAW_F) || !file.exists(LOG_F)) {
  reqlog <- list(); got <- list()
  for (st in STATIONS) {
    s <- span[span$station == st, ]
    for (y in sort(unique(year(needed)))) {
      if (y < s$first_year || y > s$last_year) next          # outside the catalog's years
      r <- fetch_year(st, y)
      reqlog[[length(reqlog) + 1]] <- tibble(
        station = st, year = y, attempts = r$attempts,
        outcome = ifelse(is.null(r$data), "FAILED", ifelse(nrow(r$data) == 0, "ok_empty", "ok")),
        rows = ifelse(is.null(r$data), NA_integer_, nrow(r$data)),
        error = r$msg, url = r$url,
        retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      if (!is.null(r$data) && nrow(r$data) > 0) got[[length(got) + 1]] <- r$data
      Sys.sleep(1)
    }
    cat(st, "done\n")
  }
  write_csv(bind_rows(reqlog), LOG_F)
  write_csv(bind_rows(got), RAW_F)
}

reqlog <- read_csv(LOG_F, show_col_types = FALSE)
raw    <- read_csv(RAW_F, show_col_types = FALSE, col_types = cols(.default = col_character()))

cat("\nRequest outcomes:\n"); print(count(reqlog, station, outcome))
if (any(reqlog$outcome == "FAILED")) {
  cat("\nFAILED requests - rerun with REFETCH <- TRUE before counting 'no_record':\n")
  print(filter(reqlog, outcome == "FAILED") |> select(station, year, error))
}

# ---- Parse and build the complete grid ---------------------------------------
parsed <- raw |>
  transmute(station = STATION, date = as.Date(DATE), prcp_mm = as.numeric(PRCP),
            attrs = PRCP_ATTRIBUTES, returned = TRUE) |>
  separate(attrs, c("m_flag", "q_flag", "s_flag", "obs_time"), sep = ",", fill = "right") |>
  mutate(across(c(m_flag, q_flag, s_flag, obs_time), ~ na_if(.x, "")))

failed_years <- reqlog |> filter(outcome == "FAILED") |>
  select(station, year) |> mutate(req_failed = TRUE)

grid <- expand_grid(station = STATIONS, date = needed) |>
  left_join(span, by = "station") |>
  left_join(parsed, by = c("station", "date")) |>
  mutate(year = year(date)) |>
  left_join(failed_years, by = c("station", "year")) |>
  mutate(status = case_when(
    !is.na(prcp_mm) & !is.na(q_flag)                 ~ "value_quality_flagged",
    !is.na(prcp_mm)                                  ~ "value",
    year < first_year | year > last_year             ~ "outside_catalog_years",
    coalesce(req_failed, FALSE)                      ~ "download_failed",
    coalesce(returned, FALSE)                        ~ "returned_NA",
    TRUE                                             ~ "no_record")) |>
  select(-year, -req_failed, -returned)
write_csv(grid, "data/derived/station_daily_prcp.csv")

cat("\nStatus by station (needed dates only):\n")
print(grid |> count(station, status) |>
        pivot_wider(names_from = status, values_from = n, values_fill = 0), width = Inf)
cat("\nObservation time over needed dates with a value:\n")
has_val <- !is.na(grid$prcp_mm)
print(table(grid$station[has_val], grid$obs_time[has_val], useNA = "ifany"))