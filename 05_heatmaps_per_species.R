# ==============================================================================
# Project: Lake Urema Waterbird Nest Counting 2026
# Script: 05_heatmaps_per_species.R
# Description: For each species observed, a heatmap plot is generated via a
#              looping logic.
# Author: João d'Oliveira Coelho
# ==============================================================================

library(sf)
library(tidyverse)
library(spatstat)
library(stars)

# --- 1. SETUP DATA ---
track_sf <- all_tracks_final %>%
  filter(segment_name == "3. Counting Survey") %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  mutate(time_numeric = as.numeric(time)) %>%
  arrange(time_numeric)

forest_polygons <- st_cast(study_area_mask, "POLYGON")
forest_centroids <- st_centroid(forest_polygons)

# --- 2. SPECIES LOOP ---
for (sp_code in names(species_map)) {
  sp_name <- species_map[sp_code]
  sp_color <- species_colors[sp_name]
  
  sp_obs <- clean_2026 %>%
    filter(!!sym(sp_code) > 0) %>%
    mutate(time_obs_num = as.numeric(timestamp))
  
  if (nrow(sp_obs) == 0) next
  
  # --- 3. TEMPORAL MATCHING (Retaining your preferred 20s window) ---
  matched_indices <- sapply(sp_obs$time_obs_num, function(t) which.min(abs(track_sf$time_numeric - t)))
  time_diffs <- abs(track_sf$time_numeric[matched_indices] - sp_obs$time_obs_num)
  
  mapping_df <- data.frame(
    nest_count = sp_obs[[sp_code]],
    time_diff = time_diffs,
    idx = matched_indices
  ) %>% filter(time_diff <= 20) 
  
  if (nrow(mapping_df) == 0) next
  
  # --- 4. SPATIAL DISPLACEMENT ---
  boat_coords <- st_coordinates(track_sf[mapping_df$idx, ])
  boat_sf <- st_as_sf(as.data.frame(boat_coords), coords = c("X", "Y"), crs = 4326)
  nearest_forest_idx <- st_nearest_feature(boat_sf, forest_centroids)
  forest_coords <- st_coordinates(forest_centroids[nearest_forest_idx, ])
  
  displaced_x <- boat_coords[,1] + 0.25 * (forest_coords[,1] - boat_coords[,1])
  displaced_y <- boat_coords[,2] + 0.25 * (forest_coords[,2] - boat_coords[,2])
  
  # --- 5. GENERATE ABUNDANCE HEATMAP ---
  bbox <- st_bbox(track_sf)
  win <- as.owin(c(bbox$xmin - 0.004, bbox$xmax + 0.004, bbox$ymin - 0.0015, bbox$ymax + 0.0015))
  
  set.seed(42)
  jitter_x <- displaced_x + rnorm(length(displaced_x), 0, 0.00005)
  jitter_y <- displaced_y + rnorm(length(displaced_y), 0, 0.00005)
  
  ppp_obs <- ppp(jitter_x, jitter_y, window = win, marks = mapping_df$nest_count)
  dens <- density(ppp_obs, sigma = 0.0008, weights = marks(ppp_obs), dimyx = 800)
  dens$v <- dens$v / max(dens$v, na.rm = TRUE)
  
  dens_stars <- st_as_stars(dens)
  st_crs(dens_stars) <- 4326
  
  # --- 6. NEW: PREP OBSERVATION BUBBLES (No Jitter) ---
  # These points are anchored directly to the boat GPS track
  obs_bubbles_sf <- st_as_sf(as.data.frame(boat_coords), 
                             coords = c("X", "Y"), 
                             crs = 4326) %>%
    mutate(nest_count = mapping_df$nest_count)
  
  # --- 7. FINAL PLOT ---
  fig <- ggplot() +
    # Layer 1: The Heat (Projected into Forest)
    geom_stars(data = dens_stars) +
    scale_fill_gradient(low = "transparent", high = sp_color, na.value = "transparent", 
                        name = "Nest Density (estimated)") + 
    
    # Layer 2: The Boat Track (Now just a background guide)
    geom_sf(data = track_sf, color = "black", linewidth = 0.3, alpha = 0.1) +
    
    # Layer 3: Raw Observation Bubbles (On the Boat Line)
    geom_sf(data = obs_bubbles_sf, 
            aes(size = nest_count), 
            color = sp_color, 
            alpha = 0.7, 
            show.legend = "point") +
    scale_size_continuous(range = c(1, 6), name = "Nests Counts (from Boat position)") +
    
    coord_sf(crs = 4326, expand = FALSE) +
    theme_minimal() +
    labs(title = paste("Abundance Heatmap:", sp_name),
         subtitle = "Bubbles (Boat Sightings) vs. Heat (Flooded Trees with Nests)",
         x = "Longitude", y = "Latitude") +
    theme(panel.grid.major = element_line(color = "grey90", linetype = "dashed", linewidth = 0.2))
  
  fname <- str_replace_all(tolower(sp_name), " ", "_")
  ggsave(paste0("reports/heatmaps/heatmap_", fname, ".png"), plot = fig, width = 16, height = 10, scale = 0.7)
}
