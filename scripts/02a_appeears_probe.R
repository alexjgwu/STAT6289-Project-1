# 02_appeears_probe.R
# Probe AppEEARS for the Daymet product and layers and build (but do NOT submit) one task.
library(appeears)
library(sf)
library(dplyr)

USER <- Sys.getenv("EARTHDATA_USER")        # set in your user-level ~/.Renviron
stopifnot(nzchar(USER))
token <- rs_login(user = USER)              # uses the password saved in the keyring

# A. Find the Daymet product
prods <- rs_products()
is_daymet <- apply(prods, 1, function(r) any(grepl("daymet", r, ignore.case = TRUE)))
print(tibble::as_tibble(prods[is_daymet, ]), n = Inf, width = Inf)
PRODUCT <- "DAYMET.004"                     # ProductAndVersion column

# B. List its layers
layers <- rs_layers(PRODUCT)
print(tibble::as_tibble(layers), n = Inf, width = Inf)
LAYER <- "prcp"

# C. Build one test task (a 2025 event, DC box + buffer)
bbox <- st_bbox(c(xmin = -77.17, ymin = 38.74, xmax = -76.86, ymax = 39.045),
                crs = st_crs(4326))
roi <- st_sf(geometry = st_as_sfc(bbox))
df <- data.frame(task = "probe_2025", subtask = "prcp",
                 start = "2025-07-08", end = "2025-07-10",
                 product = PRODUCT, layer = LAYER)
task <- rs_build_task(df = df, roi = roi, format = "geotiff")

# The package defaults to projection "geographic", which resamples the data.
# Daymet's own grid needs "native" (the task is JSON, edited as text).
cat(grepl('"projection":"geographic"', as.character(task), fixed = TRUE), "\n")
str(task, max.level = 4)
rs_logout(token)