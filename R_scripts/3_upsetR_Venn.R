# ============================================
# COMPLETE SCRIPT: BIN INTERSECTIONS
# UpSetR + Euler Diagrams with Consistent Colors
# ============================================

# --- Libraries ---
library(dplyr)       # Data wrangling
library(tidyr)       # Reshaping data
library(UpSetR)      # UpSet plots for set intersections
library(eulerr)      # Euler/Venn diagrams
library(grid)        # Required for eulerr plotting


# ============================================
# 1) Load Data
# ============================================

combined_data <- readRDS("combined_data.rds")  # Data frame with columns: bin_uri, locality, biome, ecoregion, etc.

# ============================================
# 2) Summarize Localities by Biome and Ecoregion
# ============================================

# Unique localities per biome
localities_by_biome <- combined_data %>%
  distinct(locality, biome) %>%        # Keep only unique locality-biome pairs
  group_by(biome) %>%
  summarise(localities = paste(locality, collapse = ", ")) %>%
  ungroup()

localities_by_biome

# Unique localities per ecoregion
localities_by_ecoregion <- combined_data %>%
  distinct(locality, ecoregion) %>%    # Keep only unique locality-ecoregion pairs
  group_by(ecoregion) %>%
  summarise(localities = paste(locality, collapse = ", ")) %>%
  ungroup()

localities_by_ecoregion

# List of all unique localities
all_localities <- unique(unlist(strsplit(localities_by_biome$localities, ",\\s*")))
all_localities

# ============================================
# 3) Assign Colors to Localities
# ============================================

colors_by_country <- c(
  "Galapagos"      = "#336699",
  "Paluguillo"     = "#336699",
  "Mashpi"         = "#336699",
  "Pululahua"      = "#336699",
  "CerroBlanco"    = "#336699",
  "IslaCocos"      = "#a0da39",
  "Guanacaste"     = "#a0da39",
  "Quetzales"      = "#a0da39",
  "Baru"           = "#a0da39",
  "BarroColorado"  = "#277f8e"
)

# ============================================
# 4) Create Presence/Absence Matrix
# ============================================

# Each row = BIN, each column = locality
# 1 = BIN present, 0 = absent
presence <- combined_data %>%
  distinct(bin_uri, locality) %>%       # Unique BIN-locality combinations
  mutate(pres = 1) %>%                  # Presence = 1
  pivot_wider(names_from = locality,    # Columns = localities
              values_from = pres,
              values_fill = 0) %>%      # Missing values = 0
  as.data.frame()

# Remove BINs with NA
presence <- presence[!is.na(presence$bin_uri), ]

# ============================================
# 5) Create Sets for UpSetR
# ============================================

# Each locality becomes a vector of BINs present there
sets_list <- lapply(names(presence)[-1], function(nm) {
  presence$bin_uri[ presence[[nm]] == 1 ]
})
names(sets_list) <- names(presence)[-1]

# Count number of localities per BIN (for ordering intersections)
bin_locality_count <- presence %>%
  rowwise() %>%
  mutate(n_localityes = sum(c_across(-bin_uri))) %>%
  ungroup() %>%
  arrange(n_localityes)

n_locs <- length(sets_list)  # Total number of localities

# ============================================
# 6) UpSetR Plot
# ============================================

# Plot set intersections using UpSetR
upset(
  fromList(sets_list),
  nsets = n_locs,
  nintersects = 50,          # Number of intersections to display
  order.by = "degree",       # Order intersections by number of sets
  decreasing = FALSE,        # Show rare BINs first
  sets.bar.color = colors_by_country,   # Locality colors
  main.bar.color = "gray40",            # Intersection bars color
  point.size = 1.5,                     # Size of points in the matrix
  line.size = 0.2,                      # Thickness of connecting lines
  text.scale = c(2, 2, 1.5, 1.5, 1.5, 1.5)  # Scaling for text labels
)

# ============================================
# 7) Euler Diagram
# ============================================

# Convert list of sets into eulerr-compatible format
venn_sets <- setNames(lapply(sets_list, identity), names(sets_list))

set.seed(123)  # For reproducibility

# Fit Euler diagram
fit <- euler(venn_sets)

# Plot Euler diagram
plot(
  fit,
  fills = list(fill = colors_by_country, alpha = 0.4),  # Set colors with transparency
  labels = list(col = "black", cex = 1.2),             # Label color and size
  edges = list(col = "black"),                         # Edge color
  quantities = list(cex = 1.2)                         # Show counts of intersections
)

# ============================================
# 8) Quick Overview of Data Categories
# ============================================

list(
  realm         = sort(unique(combined_data$realm)),
  biome         = sort(unique(combined_data$biome)),
  ecoregion     = sort(unique(combined_data$ecoregion)),
  region        = sort(unique(combined_data$region)),
  country_iso   = sort(unique(combined_data$country_iso)),
  country_ocean = sort(unique(combined_data$country.ocean))
)
