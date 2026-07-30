# ============================================================
# 02_pfpr_ranking_itn_priority.R
# NGA ITN Microstratification — Step 2
# Rank urban wards by PfPR within each state and apply
# ITN prioritization logic based on PfPR endemicity category.
# ============================================================
#
# DEPENDS ON OUTPUT OF 01_create_urban_wards.R
#   - <out_dir>/wards_urban_classified.gpkg
#
# ADDITIONAL INPUT  (under BASE_PATH/data)
#   - malaria/pfpr_INLA_mean.tif      MAP / INLA modelled PfPR (children 2-10)
#
# OUTPUTS  (under out_dir)
#   - wards_pfpr_ranked.gpkg                       All wards with pfpr + rank + ITN flag
#   - itn_priority/<STATE>_itn_priority.csv        Per-state ITN priority list (urban wards only)
#   - itn_priority_summary_by_state.csv            Counts of prioritised vs deprioritised wards
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
#   pfpr_category in {moderate, high}  -> Prioritized
#   pfpr_category in {very low, low}   -> Deprioritized
# Rural wards are always prioritised when RURAL_AUTO_INCLUDE = TRUE.
RURAL_AUTO_INCLUDE   <- TRUE
PRIORITIZE_BANDS     <- c("moderate", "high")
DEPRIORITIZE_BANDS   <- c("very low", "low")

BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

out_dir          <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
priority_out_dir <- file.path(out_dir, "itn_priority")
dir.create(priority_out_dir, showWarnings = FALSE, recursive = TRUE)

wards_path <- file.path(out_dir, "wards_urban_classified.gpkg")
pop_path   <- file.path(DATA_PATH, "population/allage_population_2025.tif")
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

# ---- 3. Load PfPR + population rasters ----
pop  <- rast(pop_path)
pfpr <- rast(pfpr_path)

# Resample PfPR onto pop grid (continuous => bilinear)
pfpr_on_pop <- project(pfpr, pop, method = "bilinear")

# ---- 4. Population-weighted mean PfPR per ward ----
message("Extracting population-weighted mean PfPR per ward ...")
wards$pfpr_mean <- exact_extract(
  pfpr_on_pop, wards,
  fun     = "weighted_mean",
  weights = pop
)

# PfPR endemicity bands (NMEP / WHO-style)
#   < 1%       very low
#   1 – 10%    low
#   10 – 35%   moderate
#   > 35%      high
# pfpr_mean is a proportion (0-1) from MAP/INLA, so breaks are on the same scale.
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
# Ranking is kept for ordering / reporting (highest PfPR first within state),
# but the prioritisation decision below is driven by pfpr_category, not by rank.
wards <- wards |>
  group_by(statename) |>
  mutate(
    pfpr_rank_in_state = ifelse(
      ward_class == "Urban",
      rank(-pfpr_mean, ties.method = "first"),
      NA_integer_
    ),
    n_urban_in_state = sum(ward_class == "Urban", na.rm = TRUE)
  ) |>
  ungroup()

# ---- 6. ITN prioritization decision (category-driven) ----
wards <- wards |>
  mutate(
    itn_priority = case_when(
      ward_class == "Rural" & RURAL_AUTO_INCLUDE                                  ~ "Prioritized (rural)",
      ward_class == "Urban" & as.character(pfpr_category) %in% PRIORITIZE_BANDS   ~ "Prioritized (urban moderate/high PfPR)",
      ward_class == "Urban" & as.character(pfpr_category) %in% DEPRIORITIZE_BANDS ~ "Deprioritized (urban very-low/low PfPR)",
      TRUE                                                                        ~ "Review"
    )
  )

# ---- 7. Save ranked layer ----
st_write(
  wards,
  file.path(out_dir, "wards_pfpr_ranked.gpkg"),
  delete_dsn = TRUE,
  quiet      = TRUE
)

# ---- 8. Per-state ITN priority CSVs (urban wards only) ----
priority_tbl <- wards |>
  st_drop_geometry() |>
  filter(ward_class == "Urban") |>
  dplyr::select(statename, lganame, wardname, wardcode, ward_id,
                ward_class, urban_share, pfpr_mean, pfpr_pct, pfpr_category,
                pfpr_rank_in_state, itn_priority) |>
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

write_csv(summary_tbl, file.path(out_dir, "itn_priority_summary_by_state.csv"))
print(summary_tbl)

message("Done. ITN priority lists written to: ", priority_out_dir)
