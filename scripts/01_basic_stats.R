# ==============================================================================
# Project: Lake Urema Waterbird Nest Counting 2026
# Script: 01_basic_stats.R
# Description: Cleans raw 2026 field data and generates summary statistics 
#              and historical comparisons for report tables.
# Author: João d'Oliveira Coelho
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
library(readxl)
library(tidyverse)
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

# 4. GENERATE TABLE 1: 2026 SPECIES SUMMARY ------------------------------------
table_1_2026 <- clean_2026 %>%
  pivot_longer(
    cols      = all_of(names(species_map)), 
    names_to  = "species_code", 
    values_to = "nests"
  ) %>%
  filter(nests > 0) %>%
  mutate(species_name = species_map[species_code]) %>%
  group_by(species_name) %>%
  summarise(
    no_of_nests    = sum(nests),
    no_of_trees    = n_distinct(tree),
    nests_per_tree = round(no_of_nests / no_of_trees, 2),
    max_nests_tree = max(nests),
    .groups        = "drop"
  ) %>%
  arrange(desc(no_of_nests))

# Add global total row for the 2026 season
total_row <- data.frame(
  species_name   = "Total (all species)",
  no_of_nests    = sum(clean_2026$total_nests_tree),
  no_of_trees    = n_distinct(clean_2026$tree),
  nests_per_tree = round(sum(clean_2026$total_nests_tree) / n_distinct(clean_2026$tree), 2),
  max_nests_tree = max(clean_2026$total_nests_tree)
)

table_1_final <- bind_rows(table_1_2026, total_row)

# 5. GENERATE TABLE 2: HISTORICAL COMPARISON (2014-2026) -----------------------
historical_df <- read_csv("data/raw/historical_nest_counts.csv")

# Extract species totals from Table 1 and format for historical merge
totals_2026_wide <- table_1_2026 %>%
  select(species_name, no_of_nests) %>%
  pivot_wider(names_from = species_name, values_from = no_of_nests) %>%
  mutate(
    Year  = 2026, 
    Total = sum(clean_2026$total_nests_tree)
  )

# Combine current year with previous monitoring data
table_2_comparison <- bind_rows(historical_df, totals_2026_wide) %>%
  # Replace NAs with zero for years where specific species were absent
  mutate(across(everything(), ~replace_na(., 0)))

# 6. EXPORT RESULTS ------------------------------------------------------------
# Console output for quick verification
message("--- Table 1: 2026 Results ---")
print(table_1_final)

message("--- Table 2: Historical Comparison (Updated) ---")
print(table_2_comparison)

# Save processed datasets for downstream scripts (Figures/Heatmaps)
write_csv(clean_2026, "data/processed/2026_cleaned_nest_data.csv")
write_csv(table_1_final, "data/processed/2026_species_summary.csv")
write_csv(table_2_comparison, "data/processed/historical_comparison_updated.csv")

message("Processing complete. Files saved to data/processed/")