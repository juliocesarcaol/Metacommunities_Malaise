##########################################
# Script: Taxonomic Resolution & Collection Dates
# Author: Julio C. Carrion-Olmedo
# Description:
#   - Summarizes taxonomic resolution across localities
#   - Visualizes percentages of records identified at 
#     different taxonomic levels
#   - Plots collection date ranges and individual dates
##########################################

# -------------------------
# 1. Load Data & Libraries
# -------------------------

# Load combined dataset with BINs, localities, and metadata
combined_data <- readRDS("combined_data.rds")  
# Expected columns: bin_uri, locality, biome, ecoregion, collection_date_start, collection_date_end, etc.

# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# -------------------------
# 2. Taxonomic Resolution
# -------------------------

# Total number of records per locality
total_records <- combined_data %>%
  group_by(locality) %>%
  summarise(total = n(), .groups = "drop")

# Merge taxonomic resolution table with total records
# `resolucion_tax` should include columns: locality, n_kingdom ... n_species
df_tax_perc <- resolucion_tax %>%
  left_join(total_records, by = "locality") %>%
  pivot_longer(
    cols = n_kingdom:n_species,
    names_to = "level",
    values_to = "n_identified"
  ) %>%
  mutate(percentage = round((n_identified / total) * 100, 1))

# Define taxonomic hierarchy (order is important for plotting)
tax_levels <- c("n_kingdom","n_phylum","n_class","n_order",
                "n_family","n_genus","n_species")
df_tax_perc$level <- factor(df_tax_perc$level, levels = tax_levels)

# Define desired order of localities
locality_order <- c(
  "Baru", "Guanacaste", "IslaCocos", "Quetzales",
  "BarroColorado", "Galapagos", "CerroBlanco",
  "Mashpi", "Pululahua", "Paluguillo"
)

# Apply locality order
df_tax_perc <- df_tax_perc %>%
  mutate(locality = factor(locality, levels = locality_order))

# Keep only higher resolution levels
df_tax_perc_filtered <- df_tax_perc %>%
  filter(level %in% c("n_order", "n_family", "n_genus", "n_species"))

# Labels for plot legend
level_labels <- c(
  "n_order"   = "Order",
  "n_family"  = "Family",
  "n_genus"   = "Genus",
  "n_species" = "Species"
)

# -------------------------
# 3. Plot: Taxonomic Resolution
# -------------------------

ggplot(df_tax_perc_filtered, aes(x = locality, y = percentage, fill = level)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(percentage, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.5, size = 3
  ) +
  scale_fill_viridis_d(option = "D", labels = level_labels) +
  labs(
    title = "Percentage of Records Identified by Taxonomic Level",
    x = "Locality",
    y = "Percentage of Records",
    fill = "Taxonomic Level"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# 4. Collection Dates
# -------------------------

# Convert dates to Date objects
combined_data <- combined_data %>%
  mutate(
    collection_date_start = as.Date(collection_date_start),
    collection_date_end   = as.Date(collection_date_end)
  )

# Replace known incorrect date ("2006-10-20") with NA
combined_data <- combined_data %>%
  mutate(
    collection_date_start = ifelse(
      collection_date_start == "2006-10-20",
      NA,
      collection_date_start
    )
  )

# Summarize date ranges per locality
date_ranges <- combined_data %>%
  group_by(locality) %>%
  summarise(
    start_date = min(collection_date_start, na.rm = TRUE),
    end_date   = max(collection_date_end, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(locality = factor(locality, levels = locality_order))

# -------------------------
# 5. Plot: Date Ranges per Locality
# -------------------------

ggplot(date_ranges, aes(x = locality)) +
  geom_linerange(
    aes(ymin = start_date, ymax = end_date),
    size = 2, color = viridis::viridis(1, option = "D")
  ) +
  geom_point(aes(y = start_date), shape = 21, fill = "white", size = 3) +
  geom_point(aes(y = end_date),   shape = 21, fill = "white", size = 3) +
  labs(
    title = "Collection Date Ranges per Locality",
    x = "Locality",
    y = "Collection Dates"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# 6. Plot: Individual Collection Dates
# -------------------------

ggplot(combined_data, aes(x = factor(locality, levels = locality_order), y = collection_date_start)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(
    title = "Individual Collection Dates per Locality",
    x = "Locality",
    y = "Collection Start Date"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
