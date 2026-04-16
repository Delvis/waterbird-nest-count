# ==============================================================================
# Project: Lake Urema Waterbird Nest Counting 2026
# Script: 03_trackways.R
# Description: Processes GPX trackways, calculates movement velocity, and 
#              generates a multi-panel figure of survey phases.
# Author: João d'Oliveira Coelho
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
library(sf)
library(tidyverse)
library(lubridate)
library(patchwork)
library(zoo)
library(ggspatial)

# 2. DATA INGESTION & GPX PROCESSING -------------------------------------------
path <- "data/raw/"
files <- list.files(path, pattern = "\\.gpx$", full.names = TRUE)

# Function to extract points with source metadata
load_gpx_points <- function(file) {
  st_read(file, layer = "track_points", quiet = TRUE) %>%
    mutate(track_name = basename(file)) %>%
    select(track_name, time, geometry)
}

# Combine and calculate velocity metrics
all_tracks_speed <- map_df(files, load_gpx_points) %>%
  mutate(time = ymd_hms(time)) %>%
  group_by(track_name) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    dist_m      = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),
    time_diff_s = as.numeric(difftime(time, lag(time), units = "secs")),
    # Speed in km/h with a 3-point rolling mean to smooth GPS jitter
    speed_kmh   = if_else(time_diff_s > 0, (dist_m / time_diff_s) * 3.6, 0),
    speed_kmh_smooth = rollmean(speed_kmh, k = 3, fill = NA)
  ) %>%
  filter(!is.na(speed_kmh_smooth)) %>%
  ungroup()

# 3. TEMPORAL SEGMENTATION -----------------------------------------------------
# Splitting tracks into meaningful survey phases based on timestamps
all_tracks_final <- all_tracks_speed %>%
  mutate(
    hour_min = format(time, "%H:%M"),
    segment_name = case_when(
      track_name == "TRACKWAY1_going-in-bird-nest-counting.gpx" ~ "1. Going In",
      track_name == "TRACKWAY2_checking-full-extent-and-finding-out-end-of-it.gpx" ~ "2. Colony Extent Check",
      
      # Combined counting periods
      (track_name == "TRACKWAY3_counting-part-1.gpx" & hour_min < "11:15") | 
        (track_name == "TRACKWAY4_counting-part-2-and-way-back.gpx" & hour_min >= "12:17" & hour_min < "12:51") ~ "3. Counting Survey",
      
      track_name == "TRACKWAY3_counting-part-1.gpx" & hour_min >= "11:15" ~ "4. Break at Hippo House",
      track_name == "TRACKWAY4_counting-part-2-and-way-back.gpx" & hour_min < "12:17" ~ "5. Return to Count Spot",
      track_name == "TRACKWAY4_counting-part-2-and-way-back.gpx" & hour_min >= "12:51" ~ "6. Back to Lauching Spot",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(segment_name))

# 4. VISUALIZATION FUNCTION ----------------------------------------------------
plot_segment_with_axes <- function(seg_name, data) {
  # 1. Filter data
  df_sub <- data %>% 
    filter(segment_name == seg_name) %>%
    st_as_sf()
  
  # 2. Calculate bounding box in Decimals
  coords  <- st_coordinates(df_sub$geometry)
  x_range <- range(coords[,1]); y_range <- range(coords[,2])
  span    <- max(diff(x_range), diff(y_range)) * 1.1
  x_mid   <- mean(x_range); y_mid <- mean(y_range)
  
  ggplot(df_sub) +
    # Layer 1: CONTINUOUS LINES (Trackways)
    geom_path(
      aes(x = st_coordinates(geometry)[,1], 
          y = st_coordinates(geometry)[,2], 
          color = speed_kmh_smooth,
          group = track_name), 
      linewidth = 1.1, alpha = 0.8, lineend = "round", linejoin = "round"
    ) +
    
    # Layer 2: The Professional Scale Bar
    # Now that we use coord_sf below, this will correctly say '100m' or '1km'
    annotation_scale(
      location = "bl", 
      width_hint = 0.25, 
      style = "ticks",
      unit_category = "metric",
      text_cex = .7
    ) +
    
    # Layer 3: The North Arrow (Orienteering style)
    annotation_north_arrow(
      location = "tr", 
      which_north = "true",
      height = unit(0.5, "cm"), 
      width = unit(0.5, "cm"),
      style = north_arrow_orienteering(text_size = 5)
    ) +
    
    coord_sf(
      crs = st_crs(4326),
      datum = st_crs(4326), 
      xlim = c(x_mid - span/2, x_mid + span/2), 
      ylim = c(y_mid - span/2, y_mid + span/2),
      expand = FALSE
    ) +
    
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 3), 
      labels = function(x) format(x, nsmall = 2)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 4), 
      labels = function(y) format(y, nsmall = 2)
    ) +
    
    scale_color_viridis_c(option = "magma", name = "Speed (km/h)",
                          limits = range(data$speed_kmh_smooth, na.rm = TRUE)) +
    theme_minimal() +
    labs(subtitle = seg_name, x = NULL, y = NULL) +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 7, color = "grey40"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    )
}
# 5. ASSEMBLE AND SAVE ---------------------------------------------------------
segment_list <- sort(unique(all_tracks_final$segment_name))
plot_list    <- lapply(segment_list, plot_segment_with_axes, data = all_tracks_final)

fig_trackways_final <- wrap_plots(plot_list, ncol = 3) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Colony Survey Dynamics in April 7, 2026",
    subtitle = "Spatially accurate tracks with movement velocity across 6 key phases"
  ) & 
  theme(legend.position = "right")

# Export results
write_csv(all_tracks_final, "data/processed/2026_segmented_trackways.csv")

ggsave("reports/figures/13_trackways_speed.png", 
       fig_trackways_final, width = 16, height = 10, scale = 0.6)

message("Trackway processing complete. Data and Figure 13 saved.")

