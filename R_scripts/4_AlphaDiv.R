# ============================================
# 1) Load Data
# ============================================

# Load the combined dataset containing BINs, localities, and other metadata
combined_data <- readRDS("combined_data.rds")  # Columns: bin_uri, locality, biome, ecoregion, etc.

# Load required libraries
library(vegan)     # for diversity metrics
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(viridis)   # for the viridis color palette

# Display the citation for vegan
citation("vegan")

# ============================================
# 2) Prepare abundance table
# ============================================

# Pivot the abundance table to long format: each row is a BIN in a locality
tabla_larga_bin <- tabla_abund %>%
  pivot_longer(-locality, names_to = "BIN", values_to = "abundance")

# Create an abundance matrix with localities as rows and BINs as columns
mat_abund <- tabla_abund %>% 
  column_to_rownames("locality") %>% 
  as.matrix()

# ============================================
# 3) Calculate alpha diversity metrics
# ============================================

# Species richness (number of BINs per locality)
richness <- specnumber(mat_abund)

# Shannon diversity index
shannon <- vegan::diversity(mat_abund, index = "shannon")

# Simpson diversity index
simpson <- vegan::diversity(mat_abund, index = "simpson")

# Pielou's evenness (Shannon / log(richness))
pielou <- shannon / log(richness)

# Fisher's alpha
fisher <- fisher.alpha(mat_abund)

# ============================================
# 4) Calculate endemism
# ============================================

# Identify BINs present in each locality
bin_localities <- tabla_larga_bin %>%
  filter(abundance > 0) %>%
  group_by(BIN) %>%
  summarise(
    localities = list(unique(locality)),
    n_localities = n_distinct(locality),
    .groups = "drop"
  )

# Filter only endemic BINs (appear in a single locality)
bin_endemics <- bin_localities %>%
  filter(n_localities == 1)

# Expand the list of localities for each endemic BIN and count per locality
endemism <- bin_endemics %>%
  unnest(cols = c(localities)) %>%
  group_by(localities) %>%
  summarise(endemism = n(), .groups = "drop") %>%
  column_to_rownames("localities")

# Align endemism counts with the rows of the abundance matrix
endemism_vec <- endemism[rownames(mat_abund), "endemism"]
endemism_vec[is.na(endemism_vec)] <- 0  # Assign 0 if locality has no endemic BINs

# ============================================
# 5) Combine all metrics into a single data frame
# ============================================

alpha_div <- data.frame(
  locality = rownames(mat_abund),
  richness = richness,
  endemism = endemism_vec,
  pielou = pielou,
  shannon = shannon,
  simpson = simpson,
  fisher = fisher
)

# Inspect the alpha diversity table
alpha_div

# ============================================
# 6) Prepare data for plotting
# ============================================

# Convert to long format for ggplot
alpha_div_long <- alpha_div %>%
  pivot_longer(
    cols = -locality,
    names_to = "metric",
    values_to = "value"
  )

# Set metric order for plotting
metric_order <- c("richness", "endemism", "pielou", "shannon", "simpson", "fisher")
alpha_div_long$metric <- factor(alpha_div_long$metric, levels = metric_order)

# Set locality order
locality_order <- c(
  "Baru", "Guanacaste", "IslaCocos", "Quetzales",
  "BarroColorado", "Galapagos", "CerroBlanco",
  "Mashpi", "Pululahua", "Paluguillo"
)
alpha_div_long$locality <- factor(alpha_div_long$locality, levels = locality_order)

# ============================================
# 7) Plot alpha diversity metrics
# ============================================

ggplot(alpha_div_long, aes(x = locality, y = value, fill = locality)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~metric, scales = "free_y", nrow = 3, ncol = 2) +  # 3x2 grid
  scale_fill_viridis(discrete = TRUE, option = "D") +           # viridis colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),          # rotate x-axis labels
    legend.position = "none"                                     # hide legend
  ) +
  labs(
    title = "Alpha Diversity Metrics by Locality",
    x = "Locality",
    y = "Value"
  )



library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Select only the richness-related metrics
richness_metrics <- alpha_div %>%
  select(locality, richness, endemism, fisher)

# Pivot to long format
richness_long <- richness_metrics %>%
  pivot_longer(-locality, names_to = "metric", values_to = "value")

# Set locality order
locality_order <- c(
  "Baru", "Guanacaste", "IslaCocos", "Quetzales",
  "BarroColorado", "Galapagos", "CerroBlanco",
  "Mashpi", "Pululahua", "Paluguillo"
)
richness_long$locality <- factor(richness_long$locality, levels = locality_order)

# Set metric order
metric_order <- c("richness", "endemism", "fisher")
richness_long$metric <- factor(richness_long$metric, levels = metric_order)

# Define dodge width for bars and labels
dodge <- position_dodge(width = 0.8)

# Create grouped barplot with values on top
ggplot(richness_long, aes(x = locality, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_text(aes(label = round(value, 0)),  # Round values to 0 decimals
            position = dodge,
            vjust = -0.5,                 # Position above the bar
            size = 3.5) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Richness-related Metrics by Locality",
    x = "Locality",
    y = "Value",
    fill = "Metric"
  )
