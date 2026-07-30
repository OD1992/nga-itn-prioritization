# ============================================================
# 03_map_itn_priority_unweighted.R
# NGA ITN Microstratification — Step 3 (UNWEIGHTED variant)
# Produce per-state maps showing URBAN ward ITN priority,
# using the unweighted PfPR ranking + top-80% rule.
# Rural wards are drawn as muted background context (not coloured).
# ============================================================
#
# DEPENDS ON OUTPUT OF 02_pfpr_ranking_itn_priority_unweighted.R
#   - <out_dir>/wards_pfpr_ranked_unweighted.gpkg
#
# OUTPUTS  (under out_dir/maps_unweighted)
#   - maps_unweighted/<STATE>_itn_priority_map.png   One landscape PNG per state
#   - maps_unweighted/_index.csv                     Per-state ward counts by priority
# ============================================================

# ---- 0. Packages ----
# pkgs <- c("sf", "tidyverse", "ggspatial")
# to_install <- setdiff(pkgs, rownames(installed.packages()))
# if (length(to_install)) install.packages(to_install)
library(sf)
library(tidyverse)
library(ggspatial)   # scale bar + north arrow

# ---- 1. Config ----
BASE_PATH <- "/mnt/efs/stratification/Nigeria/HBHI_2026/07_PostHoc/urban_microstrat"
DATA_PATH <- file.path(BASE_PATH, "data")

out_dir   <- file.path(BASE_PATH, "Version_28_04_2026/Outputs")
maps_dir  <- file.path(out_dir, "maps_unweighted")
dir.create(maps_dir, showWarnings = FALSE, recursive = TRUE)

wards_path <- file.path(out_dir, "wards_pfpr_ranked_unweighted.gpkg")
lga_shp    <- file.path(DATA_PATH, "shapefiles/lgas.shp")

PNG_WIDTH_IN  <- 10
PNG_HEIGHT_IN <- 8
PNG_DPI       <- 200

# Urban categories produced by 02_pfpr_ranking_itn_priority_unweighted.R
URBAN_PRIORITY_LEVELS <- c(
  "Prioritized (urban top 80% & moderate/high PfPR)",
  "Deprioritized (urban bottom 20% by PfPR rank)",
  "Deprioritized (urban very-low/low PfPR)"
)
URBAN_PRIORITY_COLORS <- c(
  "Prioritized (urban top 80% & moderate/high PfPR)" = "#065A82",  # deep blue
  "Deprioritized (urban bottom 20% by PfPR rank)"    = "#9A3412",  # dark amber
  "Deprioritized (urban very-low/low PfPR)"          = "#F59E0B"   # amber
)
RURAL_FILL  <- "#E2E8F0"   # muted neutral
RURAL_LINE  <- "white"

# ---- 2. Load data ----
wards <- st_read(wards_path, quiet = TRUE) |>
  st_transform(4326)

# Restrict the colour scale to urban categories only
wards <- wards |>
  mutate(
    urban_priority = factor(
      ifelse(ward_class == "Urban", as.character(itn_priority), NA),
      levels = URBAN_PRIORITY_LEVELS
    )
  )

lgas <- st_read(lga_shp, quiet = TRUE) |>
  st_transform(4326)
names(lgas) <- tolower(names(lgas))

# ---- 3. Helper: build a single state map (urban-only colouring) ----
build_state_map <- function(state_name) {

  state_wards <- wards |> filter(statename == state_name)
  state_lgas  <- lgas  |> filter(statecode %in% unique(state_wards$statecode))

  # Split into rural (background) and urban (coloured)
  rural_wards <- state_wards |> filter(ward_class == "Rural")
  urban_wards <- state_wards |> filter(ward_class == "Urban")

  # Per-state caption focuses on the urban decision
  n_urban   <- nrow(urban_wards)
  n_kept    <- sum(urban_wards$urban_priority == "Prioritized (urban top 80% & moderate/high PfPR)", na.rm = TRUE)
  n_drop_r  <- sum(urban_wards$urban_priority == "Deprioritized (urban bottom 20% by PfPR rank)",    na.rm = TRUE)
  n_drop_b  <- sum(urban_wards$urban_priority == "Deprioritized (urban very-low/low PfPR)",          na.rm = TRUE)
  n_rural   <- nrow(rural_wards)

  caption_txt <- sprintf(
    "Urban wards: %d  (Prioritized %d · Deprior. bottom 20%% %d · Deprior. low PfPR %d) · Rural context: %d wards",
    n_urban, n_kept, n_drop_r, n_drop_b, n_rural
  )

  ggplot() +
    # Rural context layer (no colour mapping)
    geom_sf(data = rural_wards,
            fill = RURAL_FILL, colour = RURAL_LINE, linewidth = 0.12) +
    # Urban priority layer
    geom_sf(data = urban_wards,
            aes(fill = urban_priority),
            colour = "white", linewidth = 0.15) +
    # LGA boundaries on top
    geom_sf(data = state_lgas,
            fill = NA, colour = "#1E293B", linewidth = 0.45) +
    scale_fill_manual(
      values  = URBAN_PRIORITY_COLORS,
      drop    = FALSE,
      name    = NULL,
      na.translate = FALSE       # don't show NA (rural) in the legend
    ) +
    annotation_scale(location = "bl", width_hint = 0.25,
                     style = "ticks", text_cex = 0.7) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(0.7, "cm"), width = unit(0.7, "cm"),
                           style = north_arrow_minimal()) +
    labs(
      title    = paste0(state_name, " — Urban ITN priority wards (unweighted PfPR)"),
      subtitle = "Urban wards (≥ 80% urban pop.) ranked by simple-mean PfPR; top 80% prioritized within each state",
      caption  = caption_txt
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", colour = "#21295C", size = 16),
      plot.subtitle    = element_text(colour = "#1C7293", size = 11),
      plot.caption     = element_text(colour = "#64748B", size = 9, hjust = 0),
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      legend.key.width = unit(1.0, "cm"),
      panel.grid.major = element_line(colour = "#F1F5F9", linewidth = 0.3),
      axis.text        = element_text(colour = "#94A3B8", size = 8),
      axis.title       = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

# ---- 4. Build and save one PNG per state ----
# Skip states with no urban wards (nothing to prioritize there).
state_names <- wards |>
  st_drop_geometry() |>
  filter(ward_class == "Urban") |>
  distinct(statename) |>
  arrange(statename) |>
  pull(statename)

message("Generating ", length(state_names), " state maps (unweighted) ...")

walk(state_names, \(st) {
  message(" - ", st)
  p <- build_state_map(st)
  fname <- paste0(gsub("[^A-Za-z0-9]+", "_", st), "_itn_priority_map.png")
  ggsave(
    filename = file.path(maps_dir, fname),
    plot     = p,
    width    = PNG_WIDTH_IN,
    height   = PNG_HEIGHT_IN,
    dpi      = PNG_DPI,
    bg       = "white"
  )
})

# ---- 5. Index CSV (state-level urban-priority summary) ----
index_tbl <- wards |>
  st_drop_geometry() |>
  filter(ward_class == "Urban") |>
  count(statename, urban_priority, .drop = FALSE) |>
  pivot_wider(names_from = urban_priority, values_from = n, values_fill = 0) |>
  mutate(
    n_urban_total = rowSums(across(any_of(URBAN_PRIORITY_LEVELS))),
    pct_prioritized = round(
      100 * `Prioritized (urban top 80% & moderate/high PfPR)` / pmax(n_urban_total, 1),
      1
    )
  ) |>
  arrange(desc(n_urban_total))

write_csv(index_tbl, file.path(maps_dir, "_index.csv"))

message("Done. Maps written to: ", maps_dir)
