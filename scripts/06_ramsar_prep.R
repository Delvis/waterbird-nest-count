# ==============================================================================
# Script: 06_ramsar_prep.R
# Description: Calculates Ramsar significance levels (Nests * 2 = Adults)
# ==============================================================================

library(tidyverse)

# 1. LOAD AND CLEAN RAMSAR CRITERIA
# Using read_csv2 for semicolon-delimited file
ramsar_raw <- read_csv2("data/raw/RamsarCriteria2026.csv") %>%
  mutate(
    # Standardize names for joining
    species_clean = str_to_lower(trimws(Species)),
    # Clean numeric threshold (remove spaces like "1 000")
    threshold_1pct = as.numeric(str_replace_all(`1% Threshold`, " ", ""))
  ) %>%
  select(species_clean, threshold_1pct)

# 2. JOIN AND CALCULATE SIGNIFICANCE
# Logic: Each nest = 2 adults. Compare adults to Ramsar 1% threshold.
ramsar_matrix_data <- hist_long %>%
  filter(species != "Total") %>%
  mutate(species_clean = str_to_lower(trimws(species))) %>%
  left_join(ramsar_raw, by = "species_clean") %>%
  mutate(
    # Convert nests to breeding adults for Ramsar comparison
    est_adults = nests * 2,
    
    # Determine the status
    status = case_when(
      nests == 0 ~ "Absent",
      !is.na(threshold_1pct) & est_adults >= threshold_1pct ~ "Passes 1% Threshold",
      nests > 0 ~ "Present",
      TRUE ~ "Absent"
    ),
    
    # Standardize factor levels for the legend
    status = factor(status, levels = c("Passes 1% Threshold", "Present", "Absent"))
  )

# 3. VERIFICATION
message("--- Ramsar status calculated (Nests * 2 used for population estimate) ---")

# 4. AUXILIARY COLUMN FOR FIGURE:
threshold_labels <- ramsar_matrix_data %>%
  distinct(species, threshold_1pct) %>%
  mutate(label = scales::comma(threshold_1pct))