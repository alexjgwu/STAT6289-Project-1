# 02b_appeears_test_1996.R
# Test request: Daymet prcp for the 1996-01-19 event (plus a buffer day each side),
# native projection. Checks that early years are available.
library(appeears)
library(sf)

USER      <- Sys.getenv("EARTHDATA_USER")
stopifnot(nzchar(USER))
TASK_NAME <- "probe_1996"
OUT_DIR   <- "data/raw/appeears_test_1996"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

token <- rs_login(user = USER)          # unlock the "appeears" keyring if asked

# ---- Region: same buffered DC box as before ---------------------------------
bbox <- st_bbox(c(xmin = -77.17, ymin = 38.74, xmax = -76.86, ymax = 39.045),
                crs = st_crs(4326))
roi  <- st_sf(geometry = st_as_sfc(bbox))

# ---- Build the task, then switch the projection to native --------------------
df <- data.frame(
  task    = TASK_NAME,
  subtask = "prcp",
  start   = "1996-01-18",
  end     = "1996-01-20",
  product = "DAYMET.004",
  layer   = "prcp"
)
task  <- rs_build_task(df = df, roi = roi, format = "geotiff")
task2 <- sub('"projection":"geographic"', '"projection":"native"',
             as.character(task), fixed = TRUE)
stopifnot(jsonlite::validate(task2))
stopifnot(grepl('"projection":"native"', task2, fixed = TRUE))
task2 <- structure(task2, class = "json")
cat(task2, "\n\n")

# ---- Submit only if a task with this name does not already exist -------------
find_task <- function() {
  tk <- rs_list_task(user = USER)
  tk[tk$task_name == TASK_NAME, , drop = FALSE]
}

existing <- find_task()
if (nrow(existing) == 0) {
  cat("Submitting new task...\n")
  tryCatch(
    rs_request(request = task2, user = USER, transfer = FALSE, verbose = TRUE),
    error = function(e) cat("Submission call returned an error (may just be a timeout):\n",
                            conditionMessage(e), "\n")
  )
  Sys.sleep(5)
} else {
  cat("Task named", TASK_NAME, "already exists; not resubmitting.\n")
}

tk <- find_task()
stopifnot(nrow(tk) >= 1)
tk <- tk[order(tk$created), ]
ID <- tk$task_id[1]                      # earliest task with this name
cat("Task ID:", ID, "\nCreated:", tk$created[1], "\n")
writeLines(c(ID, tk$created[1]), file.path(OUT_DIR, "task_id.txt"))

# ---- Poll until it leaves the queue (no resubmitting) ------------------------
repeat {
  st <- rs_list_task(user = USER)
  st <- st$status[st$task_id == ID]
  cat(format(Sys.time(), "%H:%M:%S"), "status:", st, "\n")
  if (!st %in% c("queued", "pending", "processing")) break
  Sys.sleep(60)
}

# ---- Download if it finished -------------------------------------------------
if (st == "done") {
  rs_transfer(task_id = ID, user = USER, path = OUT_DIR)
  print(list.files(OUT_DIR, recursive = TRUE))
} else {
  cat("Task ended with status:", st, "- not downloading.\n")
}