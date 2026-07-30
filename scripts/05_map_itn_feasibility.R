# ============================================================
# 05_map_itn_feasibility.R
# NGA ITN Microstratification — Step 5
# Produce maps showing the OPERATIONAL FEASIBILITY of the
# prioritized urban wards (deployment tiers + travel time).
#
# Three families of maps are produced:
#   (a) Per-state TIER map     — prioritized urban wards coloured
#                                by deployment tier (1–4).
#   (b) Per-state TRAVEL-TIME  — same wards, continuous colour
#                                scale of mean travel time (minutes).
#   (c) National OVERVIEW      — Nigeria-wide map of prioritized
#                                urban wards by tier, with state
#                                boundaries.
#
# Together these answer two NMEP questions:
#   1. WHERE are the prioritized urban wards we have to reach?
#   2. HOW HARD will it be to reach each one (logistics tier)?
# ============================================================
#
# DEPENDS ON OUTPUT OF 04_operational_feasibility_accessibility.R
#   - <out_dir>/wards_itn_feasibility.gpkg     (prioritized urban only)
#   - <out_dir>/wards_pfpr_ranked.gpkg         (full ward set, for context)
#
# OUTPUTS  (under out_dir/maps_feasibility)
#   - maps_feasibility/<STATE>_itn_feasibility_tier.png
#   - maps_feasibility/<STATE>_itn_feasibility_travel.png
#   - maps_feasibility/_NATIONAL_itn_feasibility_tier.png
#   - maps_feasibility/_index.csv
# ============================================================

# ---- 0. Packages ----
# pkgs <- c("sf", "tidyverse", "ggspatial", "scales")
# to_install <- setdiff(pkgs, rownames(installed.packages()))
# if (length(to_install)) install.packages(to_install)
library(sf)
library(tidyverse)
library(ggspatial)
library(scales)

# ---- 1. Config ----
BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

out_dir   <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
maps_dir  <- file.path(out_dir, "maps_feasibility")
dir.create(maps_dir, showWarnings = FALSE, recursive = TRUE)

feasibility_path <- file.path(out_dir, "wards_itn_feasibility.gpkg")  # prioritized urban only
context_path     <- file.path(out_dir, "wards_pfpr_ranked.gpkg")       # full ward set
lga_shp          <- file.path(DATA_PATH, "shapefiles/lgas.shp")
state_shp        <- file.path(DATA_PATH, "shapefiles/states.shp")      # for national map

PNG_WIDTH_IN  <- 10
PNG_HEIGHT_IN <- 8
PNG_DPI       <- 200

# Tier palette — green (easy) to red (hard), traffic-light style.
TIER_LEVELS <- c("Tier 1 — Deploy first",
                 "Tier 2 — Standard logistics",
                 "Tier 3 — Reinforced logistics",
                 "Tier 4 — Hard-to-reach micro-plan")
TIER_COLORS <- c(
  "Tier 1 — Deploy first"            = "#138A36",  # green
  "Tier 2 — Standard logistics"      = "#F5C400",  # amber
  "Tier 3 — Reinforced logistics"    = "#E07A1F",  # orange
  "Tier 4 — Hard-to-reach micro-plan"= "#B91C1C"   # red
)

# Background tones for "context" wards (rural + deprioritized urban)
CONTEXT_FILL <- "#E2E8F0"
CONTEXT_LINE <- "white"

# ---- 2. Load data ----
prio <- st_read(feasibility_path, quiet = TRUE) |> st_transform(4326)
ctx  <- st_read(context_path,     quiet = TRUE) |> st_transform(4326)

# Keep tier order
prio <- prio |>
  mutate(deployment_tier = factor(deployment_tier, levels = TIER_LEVELS,
                                  ordered = TRUE))

lgas <- st_read(lga_shp, quiet = TRUE) |> st_transform(4326)
names(lgas) <- tolower(names(lgas))

states_layer <- if (file.exists(state_shp)) {
  st_read(state_shp, quiet = TRUE) |> st_transform(4326)
} else {
  # Fallback: dissolve LGAs to states if no states shapefile present
  lgas |> group_by(statecode) |> summarise(.groups = "drop")
}
names(states_layer) <- tolower(names(states_layer))

# Context wards = everything that is NOT in the prioritized urban set
prio_ids <- unique(prio$ward_id)
ctx <- ctx |> mutate(is_prioritized = ward_id %in% prio_ids)

# ---- 3. Helper: per-state TIER map ----
build_state_tier_map <- function(state_name) {

  state_prio <- prio |> filter(statename == state_name)
  state_ctx  <- ctx  |> filter(statename == state_name, !is_prioritized)
  state_lgas <- lgas |> filter(statecode %in% unique(c(state_prio$statecode,
                                                       state_ctx$statecode)))

  n_total   <- nrow(state_prio)
  tier_cnt  <- table(factor(state_prio$deployment_tier, levels = TIER_LEVELS))
  caption_txt <- sprintf(
    "Prioritized urban wards: %d  ·  T1: %d · T2: %d · T3: %d · T4: %d",
    n_total, tier_cnt[1], tier_cnt[2], tier_cnt[3], tier_cnt[4]
  )

  ggplot() +
    geom_sf(data = state_ctx,
            fill = CONTEXT_FILL, colour = CONTEXT_LINE, linewidth = 0.12) +
    geom_sf(data = state_prio,
            aes(fill = deployment_tier),
            colour = "white", linewidth = 0.15) +
    geom_sf(data = state_lgas,
            fill = NA, colour = "#1E293B", linewidth = 0.45) +
    scale_fill_manual(values = TIER_COLORS, drop = FALSE, name = NULL,
                      na.translate = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.25,
                     style = "ticks", text_cex = 0.7) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(0.7, "cm"), width = unit(0.7, "cm"),
                           style = north_arrow_minimal()) +
    labs(
      title    = paste0(state_name, " — ITN deployment feasibility"),
      subtitle = "Prioritized urban wards (moderate/high PfPR), coloured by access-to-cities tier",
      caption  = caption_txt
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", colour = "#21295C", size = 16),
      plot.subtitle    = element_text(colour = "#1C7293", size = 11),
      plot.caption     = element_text(colour = "#64748B", size = 10, hjust = 0),
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      legend.key.width = unit(0.8, "cm"),
      panel.grid.major = element_line(colour = "#F1F5F9", linewidth = 0.3),
      axis.text        = element_text(colour = "#94A3B8", size = 8),
      axis.title       = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

# ---- 4. Helper: per-state TRAVEL-TIME map (continuous) ----
build_state_travel_map <- function(state_name) {

  state_prio <- prio |> filter(statename == state_name)
  state_ctx  <- ctx  |> filter(statename == state_name, !is_prioritized)
  state_lgas <- lgas |> filter(statecode %in% unique(c(state_prio$statecode,
                                                       state_ctx$statecode)))

  med_t <- median(state_prio$travel_min, na.rm = TRUE)
  caption_txt <- sprintf(
    "Population-weighted median travel time (prioritized urban wards): %.0f min",
    med_t
  )

  ggplot() +
    geom_sf(data = state_ctx,
            fill = CONTEXT_FILL, colour = CONTEXT_LINE, linewidth = 0.12) +
    geom_sf(data = state_prio,
            aes(fill = travel_min),
            colour = "white", linewidth = 0.15) +
    geom_sf(data = state_lgas,
            fill = NA, colour = "#1E293B", linewidth = 0.45) +
    scale_fill_viridis_c(
      option   = "magma",
      direction = -1,
      name     = "Travel time\n(minutes)",
      labels   = label_number(accuracy = 1),
      na.value = CONTEXT_FILL
    ) +
    annotation_scale(location = "bl", width_hint = 0.25,
                     style = "ticks", text_cex = 0.7) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(0.7, "cm"), width = unit(0.7, "cm"),
                           style = north_arrow_minimal()) +
    labs(
      title    = paste0(state_name, " — Access to cities (travel time)"),
      subtitle = "Prioritized urban wards only · MAP/Weiss motorized travel time",
      caption  = caption_txt
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", colour = "#21295C", size = 16),
      plot.subtitle    = element_text(colour = "#1C7293", size = 11),
      plot.caption     = element_text(colour = "#64748B", size = 10, hjust = 0),
      legend.position  = "right",
      legend.title     = element_text(size = 10),
      legend.text      = element_text(size = 9),
      panel.grid.major = element_line(colour = "#F1F5F9", linewidth = 0.3),
      axis.text        = element_text(colour = "#94A3B8", size = 8),
      axis.title       = element_blank()
    )
}

# ---- 5. Helper: NATIONAL TIER overview ----
build_national_tier_map <- function() {

  tier_cnt <- table(factor(prio$deployment_tier, levels = TIER_LEVELS))
  caption_txt <- sprintf(
    "Nigeria-wide prioritized urban wards: %d  ·  T1: %d · T2: %d · T3: %d · T4: %d",
    nrow(prio), tier_cnt[1], tier_cnt[2], tier_cnt[3], tier_cnt[4]
  )

  ggplot() +
    geom_sf(data = states_layer,
            fill = CONTEXT_FILL, colour = "white", linewidth = 0.4) +
    geom_sf(data = prio,
            aes(fill = deployment_tier),
            colour = NA) +
    geom_sf(data = states_layer,
            fill = NA, colour = "#1E293B", linewidth = 0.4) +
    scale_fill_manual(values = TIER_COLORS, drop = FALSE, name = NULL,
                      na.translate = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.25,
                     style = "ticks", text_cex = 0.7) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                           style = north_arrow_minimal()) +
    labs(
      title    = "Nigeria — ITN deployment feasibility (urban prioritized wards)",
      subtitle = "Coloured by access-to-cities tier · State boundaries shown",
      caption  = caption_txt
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", colour = "#21295C", size = 18),
      plot.subtitle    = element_text(colour = "#1C7293", size = 12),
      plot.caption     = element_text(colour = "#64748B", size = 10, hjust = 0),
      legend.position  = "bottom",
      legend.text      = element_text(size = 10),
      legend.key.width = unit(1.0, "cm"),
      panel.grid.major = element_line(colour = "#F1F5F9", linewidth = 0.3),
      axis.text        = element_text(colour = "#94A3B8", size = 8),
      axis.title       = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

# ---- 6. Build and save maps ----
state_names <- prio |>
  st_drop_geometry() |>
  distinct(statename) |>
  arrange(statename) |>
  pull(statename)

message("Generating per-state feasibility maps for ", length(state_names), " states ...")

walk(state_names, \(st) {
  message(" - ", st)
  st_slug <- gsub("[^A-Za-z0-9]+", "_", st)

  p_tier   <- build_state_tier_map(st)
  p_travel <- build_state_travel_map(st)

  ggsave(file.path(maps_dir, paste0(st_slug, "_itn_feasibility_tier.png")),
         plot = p_tier,
         width = PNG_WIDTH_IN, height = PNG_HEIGHT_IN,
         dpi = PNG_DPI, bg = "white")

  ggsave(file.path(maps_dir, paste0(st_slug, "_itn_feasibility_travel.png")),
         plot = p_travel,
         width = PNG_WIDTH_IN, height = PNG_HEIGHT_IN,
         dpi = PNG_DPI, bg = "white")
})

message("Generating national overview map ...")
p_nat <- build_national_tier_map()
ggsave(file.path(maps_dir, "_NATIONAL_itn_feasibility_tier.png"),
       plot = p_nat,
       width = 12, height = 11,
       dpi = PNG_DPI, bg = "white")

# ---- 7. Index CSV (per state: ward counts by tier + median travel time) ----
index_tbl <- prio |>
  st_drop_geometry() |>
  group_by(statename, deployment_tier) |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  pivot_wider(names_from = deployment_tier, values_from = n, values_fill = 0)

# add population-weighted median travel time per state
median_travel <- prio |>
  st_drop_geometry() |>
  group_by(statename) |>
  summarise(median_travel_min = median(travel_min, na.rm = TRUE),
            n_prioritized_urban = dplyr::n(),
            .groups = "drop")

index_tbl <- index_tbl |>
  left_join(median_travel, by = "statename") |>
  arrange(desc(n_prioritized_urban))

write_csv(index_tbl, file.path(maps_dir, "_index.csv"))

message("Done. Feasibility maps written to: ", maps_dir)
