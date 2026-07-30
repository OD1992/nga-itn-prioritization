# ============================================================
# 01_create_urban_wards.R
# NGA ITN Microstratification — Step 1
# Classify wards as Urban / Rural using the 80% population rule
# and split urban wards by state.
# ============================================================
#
# INPUTS  (under BASE_PATH/data)
#   - shapefiles/states.shp                       State boundaries
#   - shapefiles/lgas.shp                         LGA boundaries (with statecode)
#   - shapefiles/grid3_nga_boundary_vaccwards.shp Ward polygons (with wardcode, lgacode, urban)
#   - population/allage_population_2025.tif       WorldPop 2025 all-age population
#   - urban_extent/ghs_smod.tif                   GHSL settlement model (urban classification)
#
# OUTPUTS  (under out_dir, set in Config below)
#   - wards_urban_classified.gpkg                  All wards with urban_share + ward_class
#   - urban_wards/<STATE>_urban_wards.csv          One CSV per state of urban wards
#   - urban_rural_summary_by_state.csv             Counts and % urban per state
# ============================================================

# ---- 0. Packages ----
# pkgs <- c("sf", "terra", "tidyverse", "exactextractr", "readr", "purrr")
# to_install <- setdiff(pkgs, rownames(installed.packages()))
# if (length(to_install)) install.packages(to_install)
# invisible(lapply(pkgs, library, character.only = TRUE))
library(sf)
library(terra)
library(tidyverse)
library(exactextractr)

# ---- 1. Config ----
URBAN_THRESHOLD <- 0.80     # 80% rule
SMOD_URBAN_MIN  <- 21       # GHS-SMOD codes >= 21 are urban (peri-urban + urban centres)

BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

ward_shp_path <- file.path(DATA_PATH, "shapefiles/grid3_nga_boundary_vaccwards.shp")
state_shp_path <- file.path(DATA_PATH, "shapefiles/states.shp")
lga_shp_path   <- file.path(DATA_PATH, "shapefiles/lgas.shp")
pop_path       <- file.path(DATA_PATH, "population/allage_population_2025.tif")
smod_path      <- file.path(DATA_PATH, "urban_extent/ghs_smod.tif")

out_dir       <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
urban_out_dir <- file.path(out_dir, "urban_wards")
dir.create(urban_out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 2. Load ward boundaries (joined with LGA + State) ----
states <- st_read(state_shp_path, quiet = TRUE) |>
  dplyr::select(statecode, statename)

lgas <- st_read(lga_shp_path, quiet = TRUE) |>
  dplyr::select(lgacode, lganame, statecode)

wards_raw <- st_read(ward_shp_path, quiet = TRUE) |>
  filter(!st_is_empty(geometry)) |>
  dplyr::select(wardname, wardcode, lgacode, urban) |>
  left_join(st_drop_geometry(lgas),   by = "lgacode") |>
  left_join(st_drop_geometry(states), by = "statecode") |>
  st_make_valid() |>
  st_transform(4326)

# Normalise column names to lower-case for downstream code
names(wards_raw) <- tolower(names(wards_raw))

# Add a stable ward id (ungrouped — wardcode is unique, so groups would all be size 1)
wards <- wards_raw |>
  mutate(ward_id = dplyr::row_number())

# ---- 3. Load rasters ----
pop  <- rast(pop_path)
smod <- rast(smod_path)

# Reproject SMOD to match population raster (categorical => nearest neighbour)
smod <- project(smod, pop, method = "near")

# ---- 4. Build binary urban mask ----
urban_mask <- smod
values(urban_mask) <- as.integer(values(smod) >= SMOD_URBAN_MIN)

# ---- 5. Population-weighted urban share per ward ----
message("Extracting total population per ward ...")
wards$pop_total <- exact_extract(pop, wards, "sum")

message("Extracting urban population per ward ...")
pop_urban_rast  <- pop * urban_mask
wards$pop_urban <- exact_extract(pop_urban_rast, wards, "sum")

wards <- wards |>
  mutate(
    urban_share = ifelse(pop_total > 0, pop_urban / pop_total, 0),
    ward_class  = ifelse(urban_share >= URBAN_THRESHOLD, "Urban", "Rural")
  )

# ---- 6. Save full classified ward layer ----
st_write(
  wards,
  file.path(out_dir, "wards_urban_classified.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ---- 7. Per-state urban ward CSVs ----
# After tolower(), the joined columns are: statename, wardname, lganame, etc.
urban_tbl <- wards |>
  st_drop_geometry() |>
  filter(ward_class == "Urban") |>
  dplyr::select(statename, lganame, wardname, wardcode, ward_id,
                pop_total, pop_urban, urban_share, urban) |>
  arrange(statename, desc(urban_share))

urban_tbl |>
  group_split(statename) |>
  walk(\(df) {
    st_name <- gsub("[^A-Za-z0-9]+", "_", unique(df$statename))
    write_csv(df, file.path(urban_out_dir, paste0(st_name, "_urban_wards.csv")))
  })

# ---- 8. Quick summary ----
summary_tbl <- wards |>
  st_drop_geometry() |>
  count(statename, ward_class) |>
  tidyr::pivot_wider(names_from = ward_class, values_from = n, values_fill = 0) |>
  mutate(pct_urban = round(100 * Urban / (Urban + Rural), 1)) |>
  arrange(desc(pct_urban))

write_csv(summary_tbl, file.path(out_dir, "urban_rural_summary_by_state.csv"))
print(summary_tbl)

message("Done. Urban wards written to: ", urban_out_dir)
