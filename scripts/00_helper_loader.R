# ==============================================================================
# Script: 00_helper_loader.R
# Description: Centralized data loader and spatial prep for Urema 2026.
#              Run this before scripts 04 and 05.
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
library(sf)
library(tidyverse)
library(spatstat)
library(stars)
library(readxl)
library(janitor)
library(lubridate)

# 2. CONFIGURATION & SPECIES MAPPING -------------------------------------------
# Mapping code names to full taxonomic/common names for reporting
species_map <- c(
  "darter"            = "African darter",
  "reed_corm"         = "Reed cormorant",
  "wb_corm"           = "White-breasted cormorant",
  "openbill"          = "Openbill stork",
  "yb_stork"          = "Yellow-billed stork",
  "great_egret"       = "Great egret",
  "yb_egret"          = "Yellow-billed egret",
  "s_ibis"            = "Sacred ibis",
  "grey_heron"        = "Grey heron",
  "african_spoonbill" = "African spoonbill"
)

species_colors <- c(
  "African darter"           = "#1abc9c", 
  "Reed cormorant"           = "#2ecc71", 
  "White-breasted cormorant" = "#3498db", 
  "Openbill stork"           = "#9b59b6", 
  "Yellow-billed stork"      = "#f1c40f", 
  "Great egret"              = "#e67e22", 
  "Yellow-billed egret"      = "#e74c3c", 
  "Sacred ibis"              = "#34495e", 
  "Grey heron"               = "#95a5a6", 
  "African spoonbill"        = "#e84393", 
  "Black-headed heron"       = "black", 
  "Total"                    = "#2c3e50"
)

# 3. DATA INGESTION & CLEANING -------------------------------------------------
raw_2026 <- read_excel("data/raw/2026waterbirdNestCounts.xlsx", sheet = "Sheet1") %>%
  clean_names()

clean_2026 <- raw_2026 %>%
  mutate(
    # Fix the Excel 1899 epoch timestamp issue
    time_string = format(time, "%H:%M:%S"),
    date_string = format(date, "%Y-%m-%d"),
    timestamp   = ymd_hms(paste(date_string, time_string)),
    
    # Ensure all species columns are numeric and NAs are treated as zero
    across(all_of(names(species_map)), ~replace_na(as.numeric(.), 0))
  ) %>%
  select(-time_string, -date_string) %>% 
  rowwise() %>%
  # Calculate total nests per tree across all mapped species
  mutate(total_nests_tree = sum(c_across(all_of(names(species_map))))) %>%
  ungroup()

# 4. CORE FILE INGESTION -------------------------------------------------------

all_tracks_final <- read_csv("data/processed/2026_segmented_trackways.csv")

# 5. SPATIAL RECOVERY (The CSV-to-SF Fix) ---------------------------------------
track_sf <- all_tracks_final %>%
  filter(segment_name == "3. Counting Survey") %>%
  mutate(geom_clean = str_replace_all(geometry, "c\\(|\\)", "")) %>%
  separate(geom_clean, into = c("longitude", "latitude"), sep = ", ", convert = TRUE) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(time_numeric = as.numeric(time)) %>%
  arrange(time_numeric)

# 6. FOREST MAGNET GENERATION (Centralized Logic) ------------------------------
nw_poly <- st_read("data/raw/nw_lobe.kml") %>% st_make_valid() %>% st_union()
se_poly <- st_read("data/raw/se_lobe.kml") %>% st_make_valid() %>% st_union()
study_area_mask <- st_union(nw_poly, se_poly)

nw_magnet <- st_centroid(nw_poly)

# Create 3 magnets for SE Lobe via point averaging
se_points_y <- st_sample(se_poly, size = 1000, type = "regular") %>% 
  st_as_sf() %>% st_set_crs(4326) %>%
  mutate(y = st_coordinates(.)[,2])

se_bbox_vals <- st_bbox(se_poly)
y_breaks <- seq(se_bbox_vals$ymin, se_bbox_vals$ymax, length.out = 4)

se_magnets <- map(1:3, function(i) {
  cluster <- se_points_y %>% filter(y >= y_breaks[i], y <= y_breaks[i+1])
  if(nrow(cluster) > 0) return(st_centroid(st_union(cluster)))
}) %>% compact() %>% do.call(c, .)

all_magnets <- c(st_geometry(nw_magnet), se_magnets)

# 7. TEMPORAL JOIN (Match Nests to GPS Track) ----------------------------------
# This creates the clean_data_XY_sf object needed for heatmaps
clean_data_XY_sf <- clean_2026 %>%
  mutate(time_obs_num = as.numeric(timestamp)) %>%
  rowwise() %>%
  mutate(
    # Find the index of the track point with the closest timestamp
    nearest_idx = which.min(abs(track_sf$time_numeric - time_obs_num)),
    time_diff = abs(track_sf$time_numeric[nearest_idx] - time_obs_num)
  ) %>%
  # Retain only matches within your 20-second threshold
  filter(time_diff <= 20) %>% 
  ungroup() %>%
  # Attach the physical geometry from the trackway to the nest observation
  st_set_geometry(st_geometry(track_sf)[.$nearest_idx])

message("✅ 00_load_data: Environment prepped with track_sf and all_magnets.")