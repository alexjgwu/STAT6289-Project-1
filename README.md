# Environmental Database for DC Flood Events

Midterm project for STAT6289 Computing Environments. Builds HDF5 file (`data/dc_flood.h5`) of Daymet gridded precipitation and DC-area station precipitation for 125 requested flood events, with R functions that read only from that file.

**Assigned sources:** Daymet V4R1 (gridded) and DC-area precipitation stations (NOAA GHCN-Daily).

## Quick start (no downloads needed)

``` r
renv::restore()                      # install the package versions used
source("R/dcflood_functions.R")
H5 <- "data/dc_flood.h5"
dc_list(H5)                          # contents
dc_plot_raster(H5, "E074", date = "2017-07-28")
dc_plot_station_series(H5, "E074")
dc_coverage(H5, "E074")
```

`scripts/06_demo_E074.R` runs a full demonstration on event E074 (2017-07-28 11:45 to 2017-07-29 07:40 EST). `availability_report.Rmd` generates the availability and missingness report for each station.

## Data Sources

| Source | What | Access | Citation / DOI |
|------------------|------------------|------------------|------------------|
| Daymet V4R1 daily precipitation (prcp) | 1 km grid, mm/day, Lambert Conformal Conic | NASA AppEEARS API (product DAYMET.004) through the R package `appeears` 1.2 | Thornton et al., ORNL DAAC. DOI 10.3334/ORNLDAAC/2129 |
| Daymet day definition | Daily step is a 24-hour day, local time, midnight to midnight; 365-day calendar | <https://daac.ornl.gov/DAYMET/guides/Daymet_V4_Stn_Level_CrossVal.html> | ORNL DAAC, Daymet V4 station-level cross-validation user guide, accessed 2026-10-30 |
| GHCN-Daily PRCP | Daily station precipitation with measurement, quality and source flags and observation time | NOAA NCEI Access Data Service (`daily-summaries`) | Menne, M.J., I. Durre, B. Korzeniewski, S. McNeill, K. Thomas, X. Yin, S. Anthony, R. Ray, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012: Global Historical Climatology Network - Daily (GHCN-Daily), Version 3. NOAA National Climatic Data Center. <https://doi.org/10.7289/V5D21VHZ> (accessed 2026-10-29) |
| US Census cartographic boundary 2022 (1:500k states) | DC outline for station classification and plotting | <https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip> | US Census Bureau. Context only |
| GHCN-Daily readme | Station ID network codes (1 = CoCoRaHS), flag definitions | <https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt> | NOAA NCEI |
| NWS climatological day | Airport daily totals use midnight to midnight local standard time | <https://www.weather.gov/lot/weather_observations_faq> | National Weather Service |
| CoCoRaHS observation time | Daily precipitation observed at 0700 local time; late and multi-day reports accepted | <https://www.cocorahs.org/content.aspx?page=aboutqc> | CoCoRaHS, "About quality control" page, accessed 2026-10-30 |
| CoCoRaHS observation time varies | Most often 7:00 or 8:00 a.m., at the volunteer's convenience | <https://dnr.state.mn.us/climate/historical/acis_stn_meta_coco.html> | Minnesota DNR, "Retrieve Precipitation Data from CoCoRaHS Stations", accessed 2026-10-30 |
| Co-op observation rules | Local clock time (standard in winter, daylight in summer); observational day = 24 h ending at the observation | <https://www.weather.gov/media/lwx/coop/COOP%20Station%20Observations.pdf> | National Weather Service, NWSM 10-1315, "Cooperative Station Observations", 2007 |
| Event table | 125 flood events, begin/end times | Supplied with the assignment, stored in `data/raw_supplied/DC_flood_times.csv` | Supplied |

Event times are fixed EST (UTC-5), as stated in the assignment, with no daylight saving. They are parsed with the time zone `Etc/GMT+5` (the sign reversed in POSIX).

## Processing

1.  **Event validation** (`01_validate_events.R`). Assigns IDs E001 to E125, parses times, flags zero-duration events (1), midnight crossings (24), overlaps (2) and placeholder end times of 23:59 or 00:00 (4). Lists the 372 calendar dates needed (event days plus a one-day buffer).
2.  **Daymet retrieval.** The first plan (pulling data from ORNL THREDDS/NCSS through the `daymetr` package) failed for all 28 years; the log is kept in `data/derived/daymet_download_log.csv`. The code for retrieval using `daymetr` was saved in `scripts/02_fail_thredds_attempt.R`. The data were retrieved from AppEEARS instead, one task per year, native projection (no resampling), GeoTIFF. All 372 relevant dates were retrieved.
3.  **Station search and retrieval** (`04a`, `04a2`, `04b`). Catalog search found 70 GHCN-Daily candidates; five were selected. Each station's location was classified as in DC or out against the Census outline. Station values were requested in metric units from the NCEI service (`units=metric`), so they arrive in mm; no conversion was applied by this project. Daymet is stored in its native mm/day with no unit conversion, reprojection or resampling.
4.  **HDF5 build** (`03`, `05`, `05b`).
5.  **Demonstration and investigation** (`06`, `07`). Shows every function on event E074, then compares Daymet at each station's grid cell with the station's values on the same date and the previous date.

## HDF5 structure

```         
/events/index                 125 rows. Event IDs, begin/end in EST and UTC, flags
/events/event_days            Event/buffer dates per event (EST date)
/gridded/daymet/prcp          float32 [x, y, time] in R (on disk time, y, x); y index 1 = north; -9999 = NoData
/gridded/daymet/{x, y}        projected coordinates (m), cell centres
/gridded/daymet/{date, doy, year, source_file}
/gridded/daymet/valid_mask    1 = inside the retrieved footprint
/stations/<station_id>/       date, prcp_mm (-9999 = no value), measurement_flag, quality_flag,
                              source_flag, obs_time_hhmm, status. Attributes: name, lat, lon,
                              station_class, distance to the DC boundary, network, status_counts,
                              obs_time_counts, time_convention, source, second_source
/metadata/                    appeears_tasks, station_candidates, station_requests,
                              availability_by_event_station
/context/dc_boundary          DC outline, lon/lat vertices (EPSG:4326)
```

**Storage choices.** The Daymet cube is float32 (Daymet's native type), chunked one day at a time (35 × 39 × 1) with deflate level 6, so reading one date reads one chunk and the file stays small (about 0.9 MB). −9999 is the fill value, so NoData is never confused with 0 mm. Station series are short 1-D datasets, one per variable, so each can be read alone. Tables (events, retrieval logs) are compound datasets.

Dataset attributes record units (mm/day), NoData value (-9999, distinct from a recorded 0 mm), the CRS (Daymet LCC, WGS84, metres), the DOI, the retrieval method and time, and the time convention.

Stations in the file: US1DCDC0009 and US1DCDC0014 (in DC), USC00182325 Dalecarlia Reservoir (in DC), USC00186350 National Arboretum (in DC), and USW00013743 Reagan National Airport (about 150 m outside DC; it was the most robust station in the immediate area so kept as a reference).

## Functions (`R/dcflood_functions.R`)

All read only from the HDF5 file.

| Function | Purpose |
|------------------------------------|------------------------------------|
| `dc_list()`, `dc_describe()` | List contents and attributes |
| `dc_get_raster(h5, event_id, date, source)` | Return one day's raster as a terra object |
| `dc_plot_raster()` | Plot it with the DC outline and stations |
| `dc_get_station_series(h5, event_id, stations, buffer_days)` | Station values for an event window |
| `dc_plot_station_series()` | Plot with EST labels, the event window and missing-value marks |
| `dc_coverage()` | Per-event, per-source coverage and missingness |
| `dc_sources()` | Sources, variables, units and date ranges |

## Rebuild from scratch

With `daymetr` no longer working, an Earthdata account (free) for AppEEARS was created. Account credentials should stay in your user-level `~/.Renviron` and the system keyring and should never be committed.

1.  `renv::restore()`
2.  Set `EARTHDATA_USER` in `~/.Renviron` and store the password in a keyring named `appeears` (see the script header).
3.  Run the scripts in order: `01`, then `02a`, `02b`, `02c`, `03`, `04a`, `04a2`, `04b`, `04c`, `05`, `05b`, `06`, `07`, then knit `availability_report.Rmd`. `02a` and `02b` are optional checks. Do not rerun `02_fail_thredds_attempt.R`: it is kept as a record and would overwrite its failure log.

AppEEARS results expire after 30 days, so a rebuild submits new requests. Re-running the requests may return files that differ in metadata, but values should be identical.

## Limitations

- **Daymet day boundary.** Daymet's day is 24 hours, local time, midnight to midnight ([ORNL guide](https://daac.ornl.gov/DAYMET/guides/Daymet_V4_Stn_Level_CrossVal.html)). The guide does not say whether summer days use daylight time. We assume local standard time, which equals the EST event clock (as for the NWS climatological day). If Daymet uses daylight time in summer, its summer day runs 23:00 to 23:00 EST, so rain between 23:00 and midnight EST could belong to the next Daymet date. The report lists the summer events this could affect.
- **Station day windows.** Each station value covers the 24 hours ending at the reading. The airport uses the NWS climatological day: midnight to midnight EST all year. Co-op hours are local clock time (NWS co-op manual), so 0800 in summer is 07:00 EST. CoCoRaHS observers read at 0700 local time (CoCoRaHS); we assume this is clock time, including daylight saving, as for co-op observers, so in summer their day ends at 06:00 EST. That a value is filed under the date of the reading is inferred from the data, not stated by NOAA. For E074 the storm appears under 07-28 at Reagan National and under 07-29 at the morning stations. Morning stations agree with Daymet's previous date (correlation 0.92 to 0.96), not the same date.
- CoCoRaHS records carry no reading hour, so the 7 a.m. rule is the network standard, not a per-day fact.
- Station data feeds Daymet, so station/Daymet comparisons are not independent validation.
- The boundary file is generalised, so stations within a few hundred metres of the border are uncertain. Reagan National is classed as outside DC.
- Water bodies are not drawn on the maps.
- Events: E094 has zero duration; E075 and E112 overlap the previous event; E014, E035, E062 and E088 have placeholder end times.
- Volunteer-station gaps before 2008 and 2011 reflect stations that did not exist yet, not missing data.
- Only GHCN-Daily was searched for stations. In-DC gauges reported only to other networks (CoCoRaHS-only observers, USGS, DC agencies, private networks) are not included.
- CoCoRaHS accepts multi-day reports. Only daily PRCP was downloaded, so days covered by a multi-day total may appear as no_record at the two CoCoRaHS stations (not checked).

## AI use acknowledgment

I used Claude (Anthropic), via Claude chatbot, as an assistant throughout this project. It assisted in planning the workflow and the HDF5 structure, drafting and debugging the R code (the AppEEARS tests and retrieval after the `daymetr` route failed, the station search and download, the HDF5 build, the access functions, the plots and the investigation script), and helped structure the README and the availability report. I ran all code myself, checked the investigation outputs against the data, found and cited the sources for observation-time and day-boundary conventions, and made the decisions about sources and methods.
