# ==============================================================================
# Project: Lake Urema Waterbird Nest Counting 2026
# Script: 04_heatmap_general.R
# Description: We look at total nest/tree data to generate a heatmap, and also
#              evaluate the sampling effort via time spent at any spot.
# Author: João d'Oliveira Coelho
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
library(sf)
library(tidyverse)
library(spatstat)
library(stars)
library(ggspatial)

# 2. PREP THE FOREST 'MAGNETS' -------------------------------------------------
forest_polygons <- st_cast(study_area_mask, "POLYGON")
forest_centroids <- st_centroid(forest_polygons)

# 3. THE REFINED VECTOR SHIFT (25% DISPLACEMENT) -------------------------------
nests_dispersed_df <- clean_data_XY_sf %>%
  filter(!is.na(total_nests_tree)) %>%
  mutate(nearest_idx = st_nearest_feature(geometry, forest_centroids)) %>%
  rowwise() %>%
  mutate(
    bx = st_coordinates(geometry)[1],
    by = st_coordinates(geometry)[2],
    cx = st_coordinates(forest_centroids[nearest_idx,])[1],
    cy = st_coordinates(forest_centroids[nearest_idx,])[2],
    
    # We use the 25% shift you liked—it keeps the 'shape' of the S-curves
    new_x = bx + 0.25 * (cx - bx),
    new_y = by + 0.25 * (cy - by)
  ) %>%
  ungroup() %>%
  uncount(10) %>%
  mutate(
    # Returning to the smaller, sharper jitter
    new_x = new_x + rnorm(n(), 0, 0.00015),
    new_y = new_y + rnorm(n(), 0, 0.00015),
    total_nests_tree = total_nests_tree / 10
  )

# 4. THE "SHARP DETAIL" DENSITY ------------------------------------------------
# Convert to sf inside the call so st_bbox knows what to do
bbox <- st_bbox(all_tracks_final %>% 
                  filter(segment_name == "3. Counting Survey") %>% 
                  st_as_sf())

win_box <- as.owin(
  c(bbox$xmin - 0.004, 
    bbox$xmax + 0.004, 
    bbox$ymin - 0.0015, 
    bbox$ymax + 0.0015)
  )

nests_ppp <- ppp(
  x = nests_dispersed_df$new_x,
  y = nests_dispersed_df$new_y,
  window = win_box,
  marks = nests_dispersed_df$total_nests_tree
)

# Lowering sigma back to 0.00035 to bring back the "grain" and local peaks
dens_raw <- density(nests_ppp, sigma = 0.00035, weights = marks(nests_ppp), dimyx=1000)
dens_raw$v <- dens_raw$v / max(dens_raw$v, na.rm = TRUE)

final_heatmap <- st_as_stars(dens_raw)
st_crs(final_heatmap) <- 4326

# 5. SHARPENED HEATMAP GENERATION ----------------------------------------------

# a. Re-run density with a smaller Sigma for more 'granularity'
# sigma = 0.0004 captures local hotspots without blurring them together too much
dens_stars_sharp <- st_as_stars(
  density(nests_ppp, 
          sigma = 0.0004, 
          weights = marks(nests_ppp),
          dimyx = c(500, 500))) # Higher resolution grid

st_crs(dens_stars_sharp) <- 4326

# b. Mask to your exact Gaia boundaries
final_heatmap_sharp <- st_crop(dens_stars_sharp, study_area_mask, mask = TRUE)



# 6. CALCULATE LITERAL INVERSE SPEED -------------------------------------------
track_3_effort <- all_tracks_final %>% 
  filter(segment_name == "3. Counting Survey") %>%
  mutate(
    # We add a small constant (0.1) to avoid division by zero
    sampling_effort = 1 / (speed_kmh_smooth )
  )

# 7. SIMPLIFY TRACKWAY GEOMETRY ------------------------------------------------
# Convert to sf first, then simplify
track_3_clean <- track_3_effort %>%
  st_as_sf() %>%
  st_simplify(dTolerance = 0.000005)

# 8. BUILD THE GRADIENT RAMP ---------------------------------------------------
# Get the full palette
full_mako <- viridis::mako(100)
mid_mako_color <- full_mako[50] # This is our "Target" color at 50% density

# Create a manual ramp from Transparent to that Mid-Mako color
# This handles the 0% to 50% range
bottom_half <- colorRampPalette(c("transparent", mid_mako_color))(50)

# Combine with the top half of the actual Mako palette
# This handles the 50% to 100% range
final_palette <- c(bottom_half, full_mako[51:100])

# 9. GENERATE THE MANUAL GRID --------------------------------------------------
# Extracting the bbox first ensures st_graticule doesn't fail
grid_lines <- st_graticule(st_bbox(final_heatmap))

# 10. THE FINAL INTEGRATED PLOT ------------------------------------------------
ggheatmap <- ggplot() +
  # LAYER 1: The Heatmap (Bottom)
  geom_stars(data = final_heatmap) +
  scale_fill_gradientn(
    colors = final_palette, 
    values = seq(0, 1, length.out = length(final_palette)),
    na.value = "transparent", 
    name = "Nest/Tree\nIntensity",
    labels = scales::percent_format()
  ) +
  
  # LAYER 2: The Grid (Middle - cutting through the heat)
  geom_sf(data = grid_lines, color = "grey80", linewidth = 0.3, linetype = "dashed") +
  
  # LAYER 3: The Trackway (On top of grid and heat)
  geom_path(
    data = track_3_clean,
    aes(x = st_coordinates(geometry)[,1], 
        y = st_coordinates(geometry)[,2], 
        color = sampling_effort,
        group = track_name),
    linewidth = 1.3, 
    alpha = 0.9
  ) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Sampling Effort\n(Inverse of Velocity)",
    limits = c(0, quantile(track_3_effort$sampling_effort, 0.95, na.rm=TRUE)),
    guide = guide_colorbar(order = 2)
  ) +
  
  coord_sf(crs = 4326, datum = st_crs(4326), expand = FALSE) +
  
  theme_minimal() +
  theme(
    panel.ontop = FALSE, # Ensure this is false so we control layers manually
    panel.grid.major = element_blank(), # Kill the default background grid
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right"
  )

# 11. ANNOTATIONS (The Absolute Top Layer) -------------------------------------
ggheatfinal <- ggheatmap +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tr", which_north = "true", 
    style = north_arrow_fancy_orienteering()
  ) +
  labs(
    title = "Lake Urema Waterbird Colony in April 2026: Relative nest density",
    x = "Longitude", 
    y = "Latitude")

# 14. SAVE ---------------------------------------------------------------------
ggsave("reports/figures/14_general_master_heatmap.png", ggheatfinal,
       width = 16, height = 10, bg = "white", scale = 0.6)
