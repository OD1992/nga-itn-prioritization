# ============================================================
# 04_operational_feasibility_accessibility.R
# NGA ITN Microstratification — Step 4
# Apply OPERATIONAL FEASIBILITY (access-to-cities travel time)
# to the urban wards already PRIORITIZED by the WEIGHTED PfPR step.
#
# Workflow logic (in plain English)
# ---------------------------------
# 1. Take the output of the population-WEIGHTED ITN prioritization
#    (script 02_pfpr_ranking_itn_priority.R). The decision column
#    `itn_priority` already takes values such as:
#      - "Prioritized (rural)"
#      - "Prioritized (urban moderate/high PfPR)"
#      - "Deprioritized (urban very-low/low PfPR)"
#      - "Review"
# 2. KEEP ONLY the urban wards flagged
#      "Prioritized (urban moderate/high PfPR)"
#    (rural wards and deprioritized urban wards are NOT carried
#    into the operational-feasibility step).
# 3. Overlay the MAP / Weiss et al. "accessibility to cities"
#    raster (motorized travel time, in minutes, to the nearest
#    urban centre).
# 4. Compute a population-weighted mean travel time per ward,
#    classify it into operational-feasibility bands, and assign a
#    deployment tier so NMEP can sequence the campaign.
#
# Why this matters for NMEP
# -------------------------
# Two prioritized urban wards may both be "high PfPR / urban", but
# if one is 20 minutes from a city and the other is 3 hours away,
# the logistics of campaign delivery (cold chain, supervision,
# courier routing, micro-planning) are completely different. This
# script makes those operational realities visible so NMEP can
# resource and sequence distribution accordingly.
# ============================================================
#
# DEPENDS ON OUTPUT OF 02 (WEIGHTED)
#   - <out_dir>/wards_pfpr_ranked.gpkg
#
# ADDITIONAL INPUTS  (under BASE_PATH/data)
#   - accessibility/accessibility_to_cities_2015.tif
#       MAP / Weiss et al. motorized travel time to nearest city,
#       in MINUTES. (Falls back to any *.tif in that folder.)
#   - population/allage_population_2025.tif
#
# OUTPUTS  (under out_dir)
#   - wards_itn_feasibility.gpkg                  Prioritized urban wards + access + tier
#   - itn_feasibility/<STATE>_itn_feasibility.csv  Per-state operational list
#   - itn_feasibility_summary_by_state.csv         State-level tier counts
#   - itn_feasibility_national_summary.csv         National tier counts + pop
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
# Only this exact priority status is carried into operational feasibility.
TARGET_PRIORITY <- "Prioritized (urban moderate/high PfPR)"

# Operational-feasibility bands on motorized travel time (minutes).
# Five bands → 4 deployment tiers (High and Moderate collapse into Tier 2).
#   Very high : <  15 min   (peri-urban core, immediate reach)
#   High      :  15– 30 min
#   Moderate  :  30– 60 min
#   Low       :  60–120 min
#   Very low  : >120 min     (remote / hard-to-reach)
ACCESS_BANDS  <- c(-Inf, 15, 30, 60, 120, Inf)
ACCESS_LABELS <- c("Very high", "High", "Moderate", "Low", "Very low")

# Final deployment tiers (urban-prioritized x feasibility):
#   Tier 1: Very high access (<15 min)        -> deploy first
#   Tier 2: High OR Moderate access (15–60)   -> standard logistics
#   Tier 3: Low access (60–120 min)           -> reinforced logistics
#   Tier 4: Very low access (>120 min)        -> hard-to-reach micro-plan
TIER_LABELS <- c("Tier 1 — Deploy first",
                 "Tier 2 — Standard logistics",
                 "Tier 3 — Reinforced logistics",
                 "Tier 4 — Hard-to-reach micro-plan",
                 "Review")

BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

out_dir             <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
feasibility_out_dir <- file.path(out_dir, "itn_feasibility")
dir.create(feasibility_out_dir, showWarnings = FALSE, recursive = TRUE)

wards_path  <- file.path(out_dir, "wards_pfpr_ranked.gpkg")           # WEIGHTED
pop_path    <- file.path(DATA_PATH, "population/allage_population_2025.tif")
access_dir  <- file.path(DATA_PATH, "accessibility")
access_path <- file.path(access_dir, "accessibility_to_cities_2015.tif")

# Fallback: pick the first .tif in the accessibility folder if the
# canonical filename above is not present.
if (!file.exists(access_path)) {
  cand <- list.files(access_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(cand) == 0) {
    stop("No accessibility raster found in: ", access_dir,
         "\nExpected MAP/Weiss et al. travel-time-to-cities GeoTIFF (minutes).")
  }
  access_path <- cand[1]
  message("Using accessibility raster: ", access_path)
}

# ---- 2. Load wards (weighted PfPR-ranked layer from script 02) ----
wards_all <- st_read(wards_path, quiet = TRUE) |>
  st_transform(4326)

required_cols <- c("statename", "lganame", "wardname", "wardcode",
                   "ward_id", "ward_class", "urban_share",
                   "pfpr_mean", "pfpr_pct", "pfpr_category",
                   "pfpr_rank_in_state", "itn_priority")
missing_cols <- setdiff(required_cols, names(wards_all))
if (length(missing_cols)) {
  stop("Missing columns in wards_pfpr_ranked.gpkg: ",
       paste(missing_cols, collapse = ", "),
       "\nRun 02_pfpr_ranking_itn_priority.R first.")
}

# ---- 3. KEEP ONLY prioritized urban wards (moderate/high PfPR) ----
# itn_priority is already defined upstream — we only carry forward:
#   "Prioritized (urban moderate/high PfPR)"
# Rural wards and deprioritized urban wards are excluded from the
# operational-feasibility analysis.
priorities_seen <- unique(wards_all$itn_priority)
if (!TARGET_PRIORITY %in% priorities_seen) {
  stop("Target priority '", TARGET_PRIORITY,
       "' not found in itn_priority. Got: ",
       paste(priorities_seen, collapse = " | "))
}

wards <- wards_all |>
  filter(itn_priority == TARGET_PRIORITY)

message("Prioritized urban wards carried into feasibility step: ",
        nrow(wards), " of ", nrow(wards_all))

# ---- 4. Load rasters: accessibility (travel time, minutes) + population ----
pop    <- rast(pop_path)
access <- rast(access_path)

# Some MAP accessibility rasters use a sentinel value (e.g. -9999, 65535)
# for "no data / unreachable". Force those to NA before resampling.
NA_SENTINELS <- c(-9999, -1, 65535)
for (v in NA_SENTINELS) access <- classify(access, cbind(v, NA))

# Resample access onto the population grid so the ward extraction is
# consistent with the PfPR step in 02 (continuous => bilinear).
access_on_pop <- project(access, pop, method = "bilinear")

# ---- 5. Population-weighted mean travel time per ward ----
message("Extracting population-weighted mean travel time per ward ...")
wards$travel_min <- exact_extract(
  access_on_pop, wards,
  fun     = "weighted_mean",
  weights = pop
)

# Classify into feasibility bands
wards <- wards |>
  mutate(
    access_category = cut(
      travel_min,
      breaks         = ACCESS_BANDS,
      labels         = ACCESS_LABELS,
      right          = FALSE,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  )

# ---- 6. Operational-feasibility deployment tier ----
wards <- wards |>
  mutate(
    deployment_tier = case_when(
      access_category == "Very high"                ~ "Tier 1 — Deploy first",
      access_category %in% c("High", "Moderate")    ~ "Tier 2 — Standard logistics",
      access_category == "Low"                      ~ "Tier 3 — Reinforced logistics",
      access_category == "Very low"                 ~ "Tier 4 — Hard-to-reach micro-plan",
      TRUE                                          ~ "Review"
    ),
    deployment_tier = factor(deployment_tier, levels = TIER_LABELS, ordered = TRUE)
  )

# Total population per ward (for state-level workload sizing)
wards$pop_total <- exact_extract(pop, wards, fun = "sum")

# In-state ranking by feasibility (lower travel time = better)
wards <- wards |>
  group_by(statename) |>
  mutate(access_rank_in_state = rank(travel_min, ties.method = "first")) |>
  ungroup()

# ---- 7. Save the combined feasibility layer ----
st_write(
  wards,
  file.path(out_dir, "wards_itn_feasibility.gpkg"),
  delete_dsn = TRUE,
  quiet      = TRUE
)

# ---- 8. Per-state operational lists ----
feasibility_tbl <- wards |>
  st_drop_geometry() |>
  dplyr::select(statename, lganame, wardname, wardcode, ward_id,
                ward_class, urban_share,
                pfpr_mean, pfpr_pct, pfpr_category, pfpr_rank_in_state,
                itn_priority,
                travel_min, access_category, access_rank_in_state,
                pop_total, deployment_tier) |>
  arrange(statename, deployment_tier, desc(pfpr_mean))

feasibility_tbl |>
  group_split(statename) |>
  walk(\(df) {
    st_name <- gsub("[^A-Za-z0-9]+", "_", unique(df$statename))
    write_csv(df, file.path(feasibility_out_dir,
                            paste0(st_name, "_itn_feasibility.csv")))
  })

# ---- 9. State-level summary (counts of prioritized urban wards per tier) ----
state_summary <- feasibility_tbl |>
  count(statename, deployment_tier) |>
  tidyr::pivot_wider(names_from  = deployment_tier,
                     values_from = n,
                     values_fill = 0) |>
  arrange(statename)

write_csv(state_summary,
          file.path(out_dir, "itn_feasibility_summary_by_state.csv"))

# ---- 10. National summary (counts + population covered per tier) ----
national_summary <- feasibility_tbl |>
  group_by(deployment_tier) |>
  summarise(
    n_wards           = dplyr::n(),
    pop_covered       = sum(pop_total, na.rm = TRUE),
    median_travel_min = median(travel_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(deployment_tier)

write_csv(national_summary,
          file.path(out_dir, "itn_feasibility_national_summary.csv"))

print(national_summary)
print(state_summary)

message("Done. Operational feasibility outputs (urban-prioritized only) written to:\n  ",
        feasibility_out_dir, "\n  ",
        file.path(out_dir, "wards_itn_feasibility.gpkg"))
