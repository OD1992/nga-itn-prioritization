# ============================================================
# 02_pfpr_ranking_itn_priority_unweighted.R
# NGA ITN Microstratification — Step 2 (UNWEIGHTED variant)
# Rank urban wards by PfPR within each state and apply
# ITN prioritization logic. This version does NOT weight PfPR
# by population — it uses a simple (unweighted) mean of PfPR
# pixels falling within each ward polygon.
#
# Key differences vs. 02_pfpr_ranking_itn_priority.R:
#   - No population raster is loaded.
#   - Ward-level PfPR is computed as the simple mean of PfPR
#     pixel values within the ward (area-weighted by pixel
#     coverage fraction, but NOT weighted by population).
#   - Prioritization uses the "top 80% of urban wards by PfPR
#     rank within each state" rule (in addition to the
#     endemicity-band check).
# ============================================================
#
# DEPENDS ON OUTPUT OF 01_create_urban_wards.R
#   - <out_dir>/wards_urban_classified.gpkg
#
# ADDITIONAL INPUT  (under BASE_PATH/data)
#   - malaria/pfpr_INLA_mean.tif      MAP / INLA modelled PfPR (children 2-10)
#
# OUTPUTS  (under out_dir)
#   - wards_pfpr_ranked_unweighted.gpkg                       All wards with pfpr + rank + ITN flag
#   - itn_priority_unweighted/<STATE>_itn_priority.csv        Per-state ITN priority list (urban wards)
#   - itn_priority_summary_by_state_unweighted.csv            Counts of prioritised vs deprioritised wards
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
# Decision rule (urban wards):
#   Prioritize the TOP 80% of urban wards (by PfPR rank) within each state,
#   provided their PfPR endemicity is moderate or high.
#   The bottom 20% (lowest PfPR rank) and any urban wards in the very-low/low
#   endemicity bands are deprioritized.
# Rural wards are always prioritised when RURAL_AUTO_INCLUDE = TRUE.
RURAL_AUTO_INCLUDE   <- TRUE
URBAN_TOP_FRACTION   <- 0.80                       # top 80% of urban wards within each state
PRIORITIZE_BANDS     <- c("moderate", "high")
DEPRIORITIZE_BANDS   <- c("very low", "low")

BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

out_dir          <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
priority_out_dir <- file.path(out_dir, "itn_priority_unweighted")
dir.create(priority_out_dir, showWarnings = FALSE, recursive = TRUE)

wards_path <- file.path(out_dir, "wards_urban_classified.gpkg")
pfpr_path  <- file.path(DATA_PATH, "malaria/pfpr_INLA_mean.tif")

# ---- 2. Load wards (output of script 01) ----
wards <- st_read(wards_path, quiet = TRUE) |>
  st_transform(4326)

# Defensive: ensure expected columns are present
required_cols <- c("statename", "lganame", "wardname", "wardcode",
                   "ward_id", "ward_class", "urban_share")
missing_cols <- setdiff(required_cols, names(wards))
if (length(missing_cols)) {
  stop("Missing columns in wards_urban_classified.gpkg: ",
       paste(missing_cols, collapse = ", "))
}

# ---- 3. Load PfPR raster (NO population raster) ----
pfpr <- rast(pfpr_path)

# Make sure PfPR raster CRS matches the wards (WGS84 / EPSG:4326)
if (!is.na(crs(pfpr)) && crs(pfpr, proj = TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {
  pfpr <- project(pfpr, "EPSG:4326", method = "bilinear")
}

# ---- 4. Unweighted mean PfPR per ward ----
# Simple area-based mean: each pixel contributes proportional to the
# fraction of its area that lies inside the ward polygon. No population weighting.
message("Extracting UNWEIGHTED mean PfPR per ward ...")
wards$pfpr_mean <- exact_extract(
  pfpr, wards,
  fun = "mean"          # unweighted (area-fraction) mean
)

# PfPR endemicity bands (NMEP / WHO-style)
#   < 1%       very low
#   1 – 10%    low
#   10 – 35%   moderate
#   > 35%      high
PFPR_BANDS  <- c(-Inf, 1, 10, 35, Inf) / 100
PFPR_LABELS <- c("very low", "low", "moderate", "high")

wards <- wards |>
  mutate(
    pfpr_pct      = pfpr_mean * 100,
    pfpr_category = cut(
      pfpr_mean,
      breaks         = PFPR_BANDS,
      labels         = PFPR_LABELS,
      right          = FALSE,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  )

# ---- 5. Rank urban wards by PfPR within each state ----
# Highest PfPR -> rank 1 within the state. Rural wards get NA.
wards <- wards |>
  group_by(statename) |>
  mutate(
    pfpr_rank_in_state = ifelse(
      ward_class == "Urban",
      rank(-pfpr_mean, ties.method = "first"),
      NA_integer_
    ),
    n_urban_in_state = sum(ward_class == "Urban", na.rm = TRUE),
    # Cut-off rank for "top 80%" of urban wards (within each state).
    # ceiling() guarantees that small states still get at least one ward in.
    urban_top_cutoff = ceiling(URBAN_TOP_FRACTION * n_urban_in_state),
    in_urban_top80   = ward_class == "Urban" &
                       !is.na(pfpr_rank_in_state) &
                       pfpr_rank_in_state <= urban_top_cutoff
  ) |>
  ungroup()

# ---- 6. ITN prioritization decision ----
# An urban ward is PRIORITIZED iff:
#   (a) it sits in the top 80% of urban wards by PfPR rank in its state, AND
#   (b) its PfPR endemicity band is moderate or high.
# Otherwise (very-low / low band, or in the bottom 20% by rank) it is DEPRIORITIZED.
wards <- wards |>
  mutate(
    itn_priority = case_when(
      ward_class == "Rural" & RURAL_AUTO_INCLUDE                                       ~ "Prioritized (rural)",
      ward_class == "Urban" & in_urban_top80 &
        as.character(pfpr_category) %in% PRIORITIZE_BANDS                              ~ "Prioritized (urban top 80% & moderate/high PfPR)",
      ward_class == "Urban" & !in_urban_top80                                          ~ "Deprioritized (urban bottom 20% by PfPR rank)",
      ward_class == "Urban" & as.character(pfpr_category) %in% DEPRIORITIZE_BANDS      ~ "Deprioritized (urban very-low/low PfPR)",
      TRUE                                                                             ~ "Review"
    )
  )

# ---- 7. Save ranked layer ----
st_write(
  wards,
  file.path(out_dir, "wards_pfpr_ranked_unweighted.gpkg"),
  delete_dsn = TRUE,
  quiet      = TRUE
)

# ---- 8. Per-state ITN priority CSVs (urban wards only) ----
priority_tbl <- wards |>
  st_drop_geometry() |>
  filter(ward_class == "Urban") |>
  dplyr::select(statename, lganame, wardname, wardcode, ward_id,
                ward_class, urban_share, pfpr_mean, pfpr_pct, pfpr_category,
                pfpr_rank_in_state, n_urban_in_state, urban_top_cutoff,
                in_urban_top80, itn_priority) |>
  arrange(statename, ward_class, pfpr_rank_in_state)

priority_tbl |>
  group_split(statename) |>
  walk(\(df) {
    st_name <- gsub("[^A-Za-z0-9]+", "_", unique(df$statename))
    write_csv(df, file.path(priority_out_dir, paste0(st_name, "_itn_priority.csv")))
  })

# ---- 9. State-level summary ----
summary_tbl <- priority_tbl |>
  count(statename, itn_priority) |>
  tidyr::pivot_wider(names_from = itn_priority, values_from = n, values_fill = 0) |>
  arrange(statename)

write_csv(summary_tbl, file.path(out_dir, "itn_priority_summary_by_state_unweighted.csv"))
print(summary_tbl)

message("Done. UNWEIGHTED ITN priority lists written to: ", priority_out_dir)
