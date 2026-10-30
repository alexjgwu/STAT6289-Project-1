# 01_validate_events.R
# Validate flood-event CSV and build event index.

library(readr)
library(dplyr)
library(lubridate)

# ---- Settings ---------------------------------------------------------------
SRC       <- "data/raw_supplied/DC_flood_times.csv"      # supplied file (never modified)
KEEP_COPY <- "data/input/DC_flood_times_original.csv"

# Event times are defined as Eastern Standard Time (EST, UTC-5),
# including during summer months (no daylight saving).
TZ <- "Etc/GMT+5"

BUFFER_DAYS <- 1   # extra days either side of each event, for day-boundary ambiguity

dir.create("data/input",   recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

# ---- Preserve the original file ---------------------------------------------
stopifnot(file.exists(SRC))
if (!file.exists(KEEP_COPY)) file.copy(SRC, KEEP_COPY)
md5 <- unname(tools::md5sum(KEEP_COPY))
cat("Original preserved at", KEEP_COPY, "\nMD5:", md5, "\n\n")

# ---- Read everything as text, then validate ---------------------------------
raw <- read_csv(KEEP_COPY, col_types = cols(.default = col_character()))
expected <- c("begin_date", "begin_time", "end_date", "end_time")
stopifnot(all(expected %in% names(raw)))
cat("Rows read:", nrow(raw), "\n")
cat("Missing/blank cells:", sum(is.na(raw) | raw == ""), "\n")
cat("Exact duplicate rows:", sum(duplicated(raw)), "\n\n")

# ---- Parse timestamps in fixed EST ---------------------------------------------
ev <- raw |>
  mutate(
    source_row  = row_number(),                         # row number in the original file
    begin_local = ymd_hm(paste(begin_date, begin_time), tz = TZ, quiet = TRUE),
    end_local   = ymd_hm(paste(end_date,   end_time),   tz = TZ, quiet = TRUE)
  )

# With a fixed-offset zone every valid clock time exists, so any NA is malformed text
bad <- ev |> filter(is.na(begin_local) | is.na(end_local))
cat("Rows with unparseable timestamps:", nrow(bad), "\n")
if (nrow(bad) > 0) print(bad)
stopifnot(nrow(bad) == 0)

ev <- ev |>
  arrange(begin_local, end_local) |>
  mutate(
    event_id       = sprintf("E%03d", row_number()),
    begin_utc      = with_tz(begin_local, "UTC"),
    end_utc        = with_tz(end_local,   "UTC"),
    duration_hours = as.numeric(difftime(end_local, begin_local, units = "hours")),
    # ---- flags ----
    flag_end_before_begin = end_local < begin_local,
    flag_zero_duration    = end_local == begin_local,
    flag_crosses_midnight = ymd(end_date) > ymd(begin_date),
    flag_end_placeholder  = end_time %in% c("23:59", "00:00"),
    flag_overlaps_prev    = coalesce(begin_local < lag(end_local), FALSE)
  )

stopifnot(!any(ev$flag_end_before_begin))

# Sanity check: UTC must always equal EST clock time + 5 hours
stopifnot(all(as.numeric(difftime(
  force_tz(as.POSIXct(format(ev$begin_utc, tz = "UTC"), tz = "UTC"), "UTC"),
  force_tz(as.POSIXct(format(ev$begin_local, tz = TZ), tz = "UTC"), "UTC"),
  units = "hours")) == 5))

# ---- Summary -----------------------------------------------------------------
cat("\n--- Summary ---\n")
cat("Events:", nrow(ev), "\n")
cat("Date range:", format(min(ev$begin_local), "%Y-%m-%d", tz = TZ), "to",
    format(max(ev$end_local), "%Y-%m-%d", tz = TZ), "\n")
cat("Duration (hours): median", round(median(ev$duration_hours), 2),
    "| max", round(max(ev$duration_hours), 2), "\n")
cat("Cross midnight:", sum(ev$flag_crosses_midnight),
    "| zero duration:", sum(ev$flag_zero_duration),
    "| end placeholder (23:59/00:00):", sum(ev$flag_end_placeholder),
    "| overlap previous event:", sum(ev$flag_overlaps_prev), "\n\n")

flagged <- ev |>
  filter(flag_zero_duration | flag_end_placeholder | flag_overlaps_prev) |>
  select(event_id, source_row, begin_date, begin_time, end_date, end_time,
         starts_with("flag_"))
print(flagged, n = Inf, width = Inf)

# ---- Calendar days needed (EST dates), with buffer ---------------------------
days_for <- function(id, b_date, e_date) {
  d0 <- ymd(b_date); d1 <- ymd(e_date)
  core <- seq(d0, d1, by = "day")
  all  <- seq(d0 - BUFFER_DAYS, d1 + BUFFER_DAYS, by = "day")
  tibble(event_id = id, est_date = all,
         role = ifelse(all %in% core, "event", "buffer"))
}
days <- bind_rows(Map(days_for, ev$event_id, ev$begin_date, ev$end_date))

cat("\nEvent-days:", n_distinct(days$est_date[days$role == "event"]),
    "| unique dates including buffer:", n_distinct(days$est_date), "\n")

# ---- Write outputs -----------------------------------------------------------
fmt_local <- function(x) format(x, "%Y-%m-%dT%H:%M:%S%z", tz = TZ)
fmt_utc   <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ",  tz = "UTC")

out <- ev |>
  mutate(begin_est = fmt_local(begin_local), end_est = fmt_local(end_local),
         begin_utc = fmt_utc(begin_utc),     end_utc = fmt_utc(end_utc),
         assumed_timezone = "EST (UTC-5), no daylight saving") |>
  select(event_id, source_row, begin_date, begin_time, end_date, end_time,
         begin_est, end_est, begin_utc, end_utc, assumed_timezone,
         duration_hours, starts_with("flag_"))

write_csv(out,  "data/derived/event_index.csv")
write_csv(days, "data/derived/event_days_needed.csv")
cat("\nWrote data/derived/event_index.csv and data/derived/event_days_needed.csv\n")