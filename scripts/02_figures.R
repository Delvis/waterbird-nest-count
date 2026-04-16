# ==============================================================================
# Project: Lake Urema Waterbird Nest Counting 2026
# Script: 02_figures.R
# Description: Full suite of visualizations for the 2026 survey, covering 
#              historical trends, diversity indices, and nesting strategies.
# Author: João d'Oliveira Coelho
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggdendro)
library(scales)

# 2. DATA INGESTION & COLORS ---------------------------------------------------
# Load the standardized files from Script 01
summary_2026   <- read_csv("data/processed/2026_species_summary.csv")
full_data      <- read_csv("data/processed/historical_comparison_updated.csv")

source("scripts/00_helper_loader.R")

hist_long <- full_data %>%
  pivot_longer(cols = -Year, names_to = "species", values_to = "nests")

# 3. HISTORICAL TRENDS (FIG 1-3) -----------------------------------------------

# Figure 1: Total Colony Trend
fig_total_trend <- hist_long %>%
  filter(species == "Total") %>%
  ggplot(aes(x = Year, y = nests)) +
  geom_line(color = species_colors["Total"], size = 1.2) +
  geom_point(color = species_colors["Total"], size = 3) +
  expand_limits(y = 0) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(full_data$Year)) +
  labs(title = "Total Active Waterbird Nests at Lake Urema's Colony (2014-2026)",
       y = "Number of Nests", x = "Year")

# Figure 2: Individual Species Trends (Faceted)
fig_species_trends <- hist_long %>%
  filter(species != "Total") %>%
  ggplot(aes(x = Year, y = nests, color = species)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 2, show.legend = FALSE) +
  facet_wrap(~species, scales = "free_y") + 
  expand_limits(y = 0) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_color_manual(values = species_colors) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Species-Specific Nesting Trends (2014-2016)",
       y = "Number of Nests", x = "Year")

# Figure 3: Species Composition
fig_composition <- hist_long %>%
  filter(species != "Total" & nests > 0) %>%
  ggplot(aes(x = factor(Year), y = nests, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  labs(title = "Species Composition Trends (2014-2026)",
       y = "Number of Nests", x = "Year", fill = "Species")

# 4. ABUNDANCE & GROWTH (FIG 4-5) ----------------------------------------------

# Figure 4: 2026 Ranked Abundance
fig_2026_ranked <- hist_long %>%
  filter(Year == 2026 & species != "Total" & nests > 0) %>%
  ggplot(aes(x = reorder(species, nests), y = nests, fill = species)) +
  geom_col(show.legend = FALSE) +
  # Adding the numbers floating after the bars
  geom_text(aes(label = nests), 
            hjust = -0.2,            # Nudge text to the right
            size = 3.5,              # Font size
            fontface = "bold") + 
  coord_flip() + 
  scale_fill_manual(values = species_colors) +
  # Expand the y-axis slightly so the numbers don't get cut off the edge
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), 
                     labels = scales::label_number(accuracy = 1)) +
  theme_minimal() +
  labs(title = "Ranked Nesting Abundance: 2026 Season",
       subtitle = "Total counts per species at Lake Urema",
       x = "", y = "Number of Nests")

# Figure 5: Net Change (2025 vs 2026)
fig_growth <- full_data %>%
  filter(Year %in% c(2025, 2026)) %>%
  pivot_longer(cols = -Year, names_to = "species", values_to = "nests") %>%
  filter(species != "Total") %>%
  group_by(species) %>%
  summarise(
    n2025 = nests[Year == 2025],
    n2026 = nests[Year == 2026],
    growth = n2026 - n2025
  ) %>%
  filter(growth != 0) %>%
  mutate(
    # Handle New Colonists (avoid division by zero)
    pct_change = ifelse(n2025 == 0, Inf, (growth / n2025) * 100),
    label_text = case_when(
      pct_change == Inf ~ paste0("+", growth, " (New)"),
      growth > 0 ~ paste0("+", growth, " (+", round(pct_change, 1), "%)"),
      TRUE ~ paste0(growth, " (", round(pct_change, 1), "%)")
    )
  ) %>%
  ggplot(aes(x = reorder(species, growth), y = growth, fill = growth > 0)) +
  geom_col(show.legend = FALSE) +
  # Colored text labels with percentages
  geom_text(aes(label = label_text, 
                hjust = ifelse(growth > 0, -0.1, 1.1),
                color = growth > 0), 
            size = 3.5, 
            fontface = "bold",
            show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  # Darker versions of the colors for the text
  scale_color_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#c0392b")) + 
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25))) + 
  theme_minimal() +
  labs(title = "Net Change in Nesting Pairs (2025 to 2026)",
       subtitle = "Labels: Raw Difference (Percent Change)",
       x = "", y = "Difference in Nest Counts")

# 5. ECOLOGICAL INDICES (FIG 6-8) ----------------------------------------------

# Figure 6: Shannon Diversity (H)
fig_diversity <- full_data %>%
  pivot_longer(cols = -Year, names_to = "species", values_to = "nests") %>%
  filter(species != "Total" & nests > 0) %>%
  group_by(Year) %>%
  summarise(
    # Shannon Diversity Formula
    shannon = -sum((nests/sum(nests)) * log(nests/sum(nests)))
  ) %>%
  ggplot(aes(x = Year, y = shannon)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(color = "#e67e22", size = 3) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(full_data$Year)) +
  labs(title = "Colony Diversity Index (Shannon H)",
       subtitle = "Higher values indicate a more balanced/diverse community",
       y = "Diversity Index", x = "Year")

# Figure 7: Presence/Absence Matrix
fig_heatmap <- hist_long %>%
  filter(species != "Total") %>%
  mutate(present = ifelse(nests > 0, "Present", "Absent")) %>%
  ggplot(aes(x = factor(Year), y = species, fill = present)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("Present" = "#1abc9c", "Absent" = "#ecf0f1")) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "Presence/Absence Matrix (2014-2026)",
       subtitle = "Visualizing consistency and new arrivals in the Urema colony",
       x = "Year", y = "", fill = "Status")

# Figure 8: Community Clusters
# This shows if 2026 was a "normal" year or a total outlier in terms of community makeup.

# Prepare a matrix of species counts per year
cluster_data <- full_data %>%
  select(-Total) %>%
  column_to_rownames("Year")

# Calculate distance and cluster
dist_matrix <- dist(as.matrix(cluster_data))
clusters <- hclust(dist_matrix)

# Plot the dendrogram
fig_cluster <- ggdendrogram(clusters, rotate = FALSE, size = 2) +
  theme_minimal() +
  labs(title = "Cluster Analysis of Nesting Seasons",
       subtitle = "Years on the same branch have similar species compositions",
       # Mathematical formula for Euclidean Distance: d = sqrt(sum(x_i - x_j)^2)
       y = expression(paste("Ecological Distance: ", d[ij] == sqrt(sum((x[ik] - x[jk])^2, k==1, n)))),
       x = "Dendogram Years")

# 6. NESTING DYNAMICS (FIG 9-12) -----------------------------------------------

# Figure 9 & 10: Density and Max Load

fig_density_2026 <- summary_2026 %>%
  filter(species_name != "Total (all species)") %>%
  ggplot(aes(x = reorder(species_name, nests_per_tree), y = nests_per_tree, fill = species_name)) +
  geom_col(show.legend = FALSE) +
  # UPDATED: color = species_name added inside aes()
  geom_text(aes(label = nests_per_tree, color = species_name), 
            hjust = -0.2, 
            fontface = "bold", 
            size = 5,
            show.legend = FALSE) + 
  coord_flip() +
  # Apply the palette to both the bar fill and the text color
  scale_fill_manual(values = species_colors) +
  scale_color_manual(values = species_colors) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  labs(title = "Nesting Density: 2026 Season",
       subtitle = "Average number of nests per occupied tree",
       x = "", y = "Average Nests / Tree")

fig_max_load <- summary_2026 %>%
  filter(species_name != "Total (all species)") %>%
  ggplot(aes(x = reorder(species_name, max_nests_tree), y = max_nests_tree, color = species_name)) +
  geom_point(size = 4) +
  geom_segment(aes(x = species_name, xend = species_name, y = 0, yend = max_nests_tree), size = 1.2) +
  geom_text(aes(label = max_nests_tree), vjust = -1, fontface = "bold") +
  coord_flip() +
  scale_color_manual(values = species_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Maximum Nest Load: 2026 Season",
       subtitle = "Highest number of nests recorded in a single tree (by species)",
       x = "", y = "Max Nests in One Tree")

# Figure 11: Strategy (Regression)
# Precise Model Calculation
species_only <- summary_2026 %>% filter(species_name != "Total (all species)")
fit <- lm(no_of_nests ~ 0 + no_of_trees, data = species_only)
slope_val <- coef(fit)[1]
r2_val <- summary(fit)$r.squared

# Equation Formatting
eq_label <- paste0("italic(y) == ", round(slope_val, 2), "*italic(x)")
r2_label <- paste0("italic(R)^2 == ", round(r2_val, 3))

# Use the midpoint of the range to find the empty "center" of the graph
mid_x <- (max(species_only$no_of_trees) + min(species_only$no_of_trees)) / 2
mid_y <- mid_x * slope_val

# Because Y is 1400 and X is 200, we need to scale the slope for the display
x_range <- max(species_only$no_of_trees)
y_range <- max(species_only$no_of_nests)
# The 0.6 multiplier accounts for the ggsave width(10)/height(6) ratio
angle_deg <- atan(slope_val * (x_range / y_range) * 0.6) * (180 / pi)

fig_strategy <- species_only %>%
  ggplot(aes(x = no_of_trees, y = no_of_nests, color = species_name)) +
  # The Regression Line
  geom_smooth(method = "lm", formula = y ~ x + 0, 
              se = FALSE, color = "#95a5a6", linetype = "dashed", size = 0.8) +
  # ABOVE THE LINE: Equation
  annotate("text", x = mid_x, y = mid_y + 50, 
           label = eq_label, parse = TRUE, angle = angle_deg, 
           color = "#7f8c8d", size = 4, fontface = "italic") +
  # BELOW THE LINE: R-Squared
  annotate("text", x = mid_x, y = mid_y - 50, 
           label = r2_label, parse = TRUE, angle = angle_deg, 
           color = "#7f8c8d", size = 3.8, fontface = "italic") +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = species_name), 
                  force = 5,
                  point.padding = 0.8,
                  box.padding = 0.8, 
                  direction = "both",
                  max.overlaps = 20, 
                  fontface = "bold",
                  size = 3.5, # Slightly smaller text can also prevent touching
                  show.legend = FALSE) +
  scale_color_manual(values = species_colors) +
  theme_minimal() +
  labs(title = "Nesting Strategy: Tree Occupancy vs. Abundance (2026 Season)",
       subtitle = "The regression line identifies the average community-wide tree utilization",
       x = "Number of Trees Occupied", 
       y = "Total Number of Nests",
       color = "Species")

# Figure 12: Sociality Index
fig_sociality <- summary_2026 %>%
  filter(species_name != "Total (all species)") %>%
  ggplot(aes(x = reorder(species_name, nests_per_tree))) +
  # Area between Avg and Max
  geom_linerange(aes(ymin = nests_per_tree, ymax = max_nests_tree, color = species_name), 
                 size = 2, alpha = 0.3) +
  # UPDATED: Diamond for Average (matched size and species color)
  geom_point(aes(y = nests_per_tree, color = species_name), 
             size = 4, shape = 18) +
  # Point for Max
  geom_point(aes(y = max_nests_tree, color = species_name), 
             size = 4) +
  coord_flip() +
  scale_color_manual(values = species_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Sociality Index: Average vs. Peak Tree Density (2026 Season)",
       # UPDATED: Unicode symbols in subtitle
       subtitle = "⬥ Average Nests/Tree  |  ● Max Nests in a Single Tree",
       x = "", y = "Nests per Tree")

# 7. EXPORT --------------------------------------------------------------------
ggsave("reports/figures/01_total_trend.png", fig_total_trend, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/02_species_trends_faceted.png", fig_species_trends, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/03_composition_years.png", fig_composition, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/04_2026_ranked_abundance.png", fig_2026_ranked, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/05_growth_comparison.png", fig_growth, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/06_community_diversity.png", fig_diversity, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/07_presence_matrix.png", fig_heatmap, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/08_year_clusters.png", fig_cluster, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/09_density_per_species_2026.png", fig_density_2026, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/10_max_load_2026.png", fig_max_load, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/11_strategy.png", fig_strategy, width = 16, height = 9, scale = 0.6)
ggsave("reports/figures/12_sociality.png", fig_sociality, width = 16, height = 9, scale = 0.6)

message("All 12 figures exported to reports/figures/")