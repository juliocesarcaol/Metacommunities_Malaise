#############################################
# Load packages
#############################################
library(sf)
library(tidyverse)
library(scales)  # for rescaling

#############################################
# 1. Load coordinates
#############################################
df <- read.csv("coords_unicas.csv")  # adjust path if needed

#############################################
# 2. Convert to sf object (WGS84) and then to metric projection
#############################################
coords_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
coords_m <- st_transform(coords_sf, 3857)
mat <- st_coordinates(coords_m)

#############################################
# 3. Define colors by locality
#############################################
colors_by_country <- c(
  "Guanacaste"     = "#597a2d",
  "Baru"           = "#c5dcc0",
  "Mashpi"         = "#597a2d",
  "Quetzales"      = "#dcc7a0",
  "Pululahua"      = "#dcc7a0",
  "BarroColorado"  = "#c5dcc0",
  "CerroBlanco"    = "#c5dcc0",
  "Paluguillo"     = "#dcc7a0",
  "Galapagos"      = "#d6eefe",
  "IslaCocos"      = "#d6eefe"
)

#############################################
# 4. Calculate richness by locality
#############################################
# sets_list should be a list of BINs per locality
total_richness <- length(unique(unlist(sets_list)))
richness_by_locality <- sapply(sets_list, function(x) length(unique(x)))
sizes <- richness_by_locality[df$Locality]

#############################################
# 5. Calculate number of shared BINs
#############################################

# Generate binary table: rows = BINs, columns = localities
bin_matrix <- bin_table %>%
  mutate(Present = 1) %>%
  pivot_wider(names_from = Locality, values_from = Present, values_fill = list(Present = 0)) %>%
  column_to_rownames("BIN") %>%
  mutate_all(~ as.logical(.))  # convert all to logical

# Calculate exclusive intersections for pairs
intersect_exact <- matrix(0, n, n)
rownames(intersect_exact) <- localities
colnames(intersect_exact) <- localities

for(i in 1:(n-1)){
  for(j in (i+1):n){
    # Rows where both localities are present
    both <- bin_matrix[[i]] & bin_matrix[[j]]
    # Rows where NOT present in any other locality
    others <- rowSums(bin_matrix[ , -c(i,j)]) == 0
    # Count BINs appearing only in this pair
    intersect_exact[i,j] <- sum(both & others)
    intersect_exact[j,i] <- intersect_exact[i,j]  # symmetric
  }
}

intersect_exact

# Scale line widths
max_lwd <- 15
min_lwd <- 0.05
lwd_scaled <- (intersect_exact / max(intersect_exact)) * (max_lwd - min_lwd) + min_lwd

#############################################
# 6. Plot
#############################################
plot(mat[,1], mat[,2],
     xlab = "X (meters)",
     ylab = "Y (meters)",
     pch = 19,
     col = colors_by_country[df$Locality],
     cex = rescale(sizes, to = c(1,4)),
     main = "Localities: color = country, size = richness")

# Locality labels and richness
text(mat[,1], mat[,2],
     labels = df$Locality,
     pos = 3,
     cex = 0.8)
text(mat[,1], mat[,2],
     labels = paste0("R=", sizes),
     pos = 1,
     cex = 0.8)

# Draw lines with width proportional to shared BINs
n <- nrow(mat)

for(i in 1:(n-1)){
  for(j in (i+1):n){
    loc_i <- df$Locality[i]
    loc_j <- df$Locality[j]
    
    # Draw line with thickness proportional to number of shared BINs
    segments(
      mat[i,1], mat[i,2],
      mat[j,1], mat[j,2],
      col = "gray70",
      lwd = lwd_scaled[loc_i, loc_j]
    )
    
    # Number of shared BINs at midpoint
    xm <- (mat[i,1] + mat[j,1]) / 2
    ym <- (mat[i,2] + mat[j,2]) / 2
    text(xm, ym, labels = intersect_exact[loc_i, loc_j], cex = 0.6, col = "gray40")
  }
}
