##########################################
# Script: Taxonomic Composition Analysis
# Author: Julio C. Carrion-Olmedo
# Description:
#   - This script analyzes taxonomic composition of biological records across multiple localities.
#   - It calculates the top 10 most abundant orders and families based on raw counts.
#   - Generates stacked barplots for each locality:
#       * Raw counts (absolute number of records per taxon)
#       * Relative percentages (proportion of records per taxon within each locality)
#   - Orders and families not in the top 10 are grouped as "Others".
#   - Locality order is predefined for consistent visualization across plots.
#   - Uses the viridis color palette for better accessibility and clarity.
#   - Provides a reproducible framework for taxonomic summaries and visualizations.
##########################################


# --- 1) Load Data ---
# Load the combined dataset containing BINs, localities, and other metadata
combined_data <- readRDS("combined_data.rds")  

# Expected columns: bin_uri, locality, biome, ecoregion, order, family, etc.


# --- 2) Load Required Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(viridis)


# --- 3) Define Locality Order ---
# Set the order of localities for consistent plotting
locality_order <- c(
  "Baru", "Guanacaste", "IslaCocos", "Quetzales",
  "BarroColorado", "Galapagos", "CerroBlanco",
  "Mashpi", "Pululahua", "Paluguillo"
)


# --- 4) Top 10 Orders ---
# Identify the 10 most abundant orders across all localities
top10_order <- combined_data %>%
  group_by(order) %>%
  summarise(total_count = n(), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(order)

# Prepare dataframe for plotting orders
df_order <- combined_data %>%
  mutate(order_plot = ifelse(order %in% top10_order, order, "Others")) %>%
  group_by(locality, order_plot) %>%
  summarise(count = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(locality) %>%
  mutate(order_plot = fct_reorder(order_plot, count, .fun = sum, .desc = TRUE)) %>%
  ungroup() %>%
  mutate(locality = factor(locality, levels = locality_order)) %>%
  group_by(locality) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# --- Plot: Stacked barplot (raw counts, orders) ---
ggplot(df_order, aes(x = locality, y = count, fill = order_plot)) +
  geom_col() +
  labs(
    title = "Top 10 Most Abundant Orders by Locality (Raw Counts)",
    x = "Locality", y = "Number of Records", fill = "Order"
  ) +
  scale_fill_viridis_d(option = "viridis", na.value = "grey70") +
  theme_minimal(base_size = 14)

# --- Plot: Stacked barplot (percentages, orders) ---
ggplot(df_order, aes(x = locality, y = percentage, fill = order_plot)) +
  geom_col() +
  labs(
    title = "Top 10 Most Abundant Orders by Locality (Percentage)",
    x = "Locality", y = "Percentage of Records", fill = "Order"
  ) +
  scale_fill_viridis_d(option = "viridis", na.value = "grey70") +
  theme_minimal(base_size = 14)


# --- 5) Top 10 Families ---
# Identify the 10 most abundant families across all localities
top10_family <- combined_data %>%
  group_by(family) %>%
  summarise(total_count = n(), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(family)

# Prepare dataframe for plotting families
df_family <- combined_data %>%
  mutate(family_plot = ifelse(family %in% top10_family, family, "Others")) %>%
  group_by(locality, family_plot) %>%
  summarise(count = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(locality) %>%
  mutate(family_plot = fct_reorder(family_plot, count, .fun = sum, .desc = TRUE)) %>%
  ungroup() %>%
  mutate(locality = factor(locality, levels = locality_order)) %>%
  group_by(locality) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# --- Plot: Stacked barplot (raw counts, families) ---
ggplot(df_family, aes(x = locality, y = count, fill = family_plot)) +
  geom_col() +
  labs(
    title = "Top 10 Most Abundant Families by Locality (Raw Counts)",
    x = "Locality", y = "Number of Records", fill = "Family"
  ) +
  scale_fill_viridis_d(option = "viridis", na.value = "grey70") +
  theme_minimal(base_size = 14)

# --- Plot: Stacked barplot (percentages, families) ---
ggplot(df_family, aes(x = locality, y = percentage, fill = family_plot)) +
  geom_col() +
  labs(
    title = "Top 10 Most Abundant Families by Locality (Percentage)",
    x = "Locality", y = "Percentage of Records", fill = "Family"
  ) +
  scale_fill_viridis_d(option = "viridis", na.value = "grey70") +
  theme_minimal(base_size = 14)
