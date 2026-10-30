# 04a2_station_boundary_check.R (revised)
library(sf); library(dplyr); library(readr)

dir.create("data/raw/boundary", recursive = TRUE, showWarnings = FALSE)
zipf <- "data/raw/boundary/cb_2022_us_state_500k.zip"
if (!file.exists(zipf))
  download.file("https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip",
                zipf, mode = "wb")
unzip(zipf, exdir = "data/raw/boundary")
dc <- st_read("data/raw/boundary/cb_2022_us_state_500k.shp", quiet = TRUE) |>
  filter(STUSPS == "DC")

cand <- read_csv("data/derived/station_candidates_ghcnd.csv", show_col_types = FALSE)
cand$inside_dc <- NULL; cand$dist_to_boundary_m <- NULL; cand$borderline <- NULL   # allow reruns

pts <- st_as_sf(cand, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Distances need metres: project to UTM zone 18N (EPSG:32618)
dc_m  <- st_transform(st_geometry(dc), 32618)
pts_m <- st_transform(pts, 32618)

cand$inside_dc          <- lengths(st_intersects(pts_m, dc_m)) > 0
cand$dist_to_boundary_m <- round(as.numeric(st_distance(pts_m, st_boundary(dc_m))))
cand$borderline         <- cand$dist_to_boundary_m < 2000      # within 2 km of the line

show <- cand |>
  filter(inside_dc != state_is_dc | borderline | id %in% c("USC00182325", "USC00186350", "USW00013743")) |>
  mutate(lat = round(lat, 4), lon = round(lon, 4)) |>
  select(id, name, state, lat, lon, state_is_dc, inside_dc, dist_to_boundary_m, event_years) |>
  arrange(desc(event_years))
print(show, n = Inf, width = Inf)
write_csv(cand, "data/derived/station_candidates_ghcnd.csv")