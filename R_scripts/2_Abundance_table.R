##########################################
# 1. Load Information from previous script
##########################################

combined_data <- readRDS("combined_data.rds")

##########################################
# 2. Summaries and abundance tables
##########################################

# --- A. Abundance table by locality and BIN
# Count the number of records per BIN within each locality
# and reshape into a wide matrix (localities as rows, BINs as columns).
tabla_abund <- combined_data %>%
  group_by(locality, bin_uri) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = bin_uri,
    values_from = count,
    values_fill = 0
  )

# --- B. Taxonomic resolution by rank
# For each locality, calculate how many records have non-missing
# taxonomic assignments at different ranks (kingdom → species).
resolucion_tax <- combined_data %>%
  group_by(locality) %>%
  summarise(
    n_kingdom = sum(!is.na(kingdom) & kingdom != ""),
    n_phylum  = sum(!is.na(phylum)  & phylum != ""),
    n_class   = sum(!is.na(class)   & class != ""),
    n_order   = sum(!is.na(order)   & order != ""),
    n_family  = sum(!is.na(family)  & family != ""),
    n_genus   = sum(!is.na(genus)   & genus != ""),
    n_species = sum(!is.na(species) & species != "")
  )

# --- C. Create concatenated taxonomic label
# Build a unified taxonomic string for each record
# (format: kingdom;phylum;class;order;family;genus;species),
# replacing missing values with "NA".
combined_data <- combined_data %>%
  mutate(
    taxon_label = paste0(
      ifelse(is.na(kingdom) | kingdom == "", "NA", kingdom), ";",
      ifelse(is.na(phylum)  | phylum == "", "NA", phylum), ";",
      ifelse(is.na(class)   | class == "", "NA", class), ";",
      ifelse(is.na(order)   | order == "", "NA", order), ";",
      ifelse(is.na(family)  | family == "", "NA", family), ";",
      ifelse(is.na(genus)   | genus == "", "NA", genus), ";",
      ifelse(is.na(species) | species == "", "NA", species)
    )
  )

# --- D. Abundance table by taxonomic labels
# Count the number of records per concatenated taxonomic label
# within each locality and reshape into a wide matrix
# (localities as rows, taxon labels as columns).
tabla_abund_tax <- combined_data %>%
  group_by(locality, taxon_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = taxon_label,
    values_from = count,
    values_fill = 0
  )
