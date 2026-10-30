# 05b_add_context.R
# Stores the DC outline inside the HDF5 file so plotting functions never need outside files.
library(rhdf5); library(sf); library(dplyr)

H5 <- "data/dc_flood.h5"
stopifnot(file.exists(H5))

shp <- list.files("data", pattern = "cb_2022_us_state_500k\\.shp$", recursive = TRUE, full.names = TRUE)
if (length(shp) == 0) {
  zp <- list.files("data", pattern = "cb_2022_us_state_500k.*\\.zip$", recursive = TRUE, full.names = TRUE)
  stopifnot("Census boundary file not found under data/ - rerun 04a2 or point `shp` at it" = length(zp) > 0)
  td <- file.path(tempdir(), "cb"); dir.create(td, showWarnings = FALSE)
  unzip(zp[1], exdir = td)
  shp <- list.files(td, pattern = "\\.shp$", full.names = TRUE)
}
dc <- st_read(shp[1], quiet = TRUE) |> filter(STUSPS == "DC") |> st_transform(4326)
stopifnot(nrow(dc) == 1)

xy <- as.data.frame(st_coordinates(dc))
grp <- setdiff(names(xy), c("X", "Y"))
xy$part <- as.integer(interaction(xy[grp], drop = TRUE))
out <- data.frame(part = xy$part, lon = xy$X, lat = xy$Y)

# replace on rerun
if (!"context" %in% h5ls(H5, recursive = 1)$name) h5createGroup(H5, "context")
kids <- h5ls(H5, recursive = TRUE)
if (any(kids$group == "/context" & kids$name == "dc_boundary")) h5delete(H5, "context/dc_boundary")
h5write(out, H5, "context/dc_boundary")

fid <- H5Fopen(H5); did <- H5Dopen(fid, "context/dc_boundary")
h5writeAttribute("EPSG:4326 (lon/lat, degrees); ring vertices in order; 'part' separates rings", did, "crs_note")
h5writeAttribute("US Census cartographic boundary 2022, 1:500k (generalized). Context only; not survey-grade.", did, "source")
H5Dclose(did); H5Fclose(fid)
cat("DC outline stored:", nrow(out), "vertices in", length(unique(out$part)), "ring(s)\n")
print(h5ls(H5, recursive = 1))
