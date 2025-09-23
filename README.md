# Metacommunities_Malaise
This repository contains R scripts and workflows for analyzing DNA barcode data from the BOLD systems API. It includes automated pipelines for downloading datasets and producing visual and statistical analyses of biodiversity patterns across multiple sites of EC, CR, and PA


# BOLD Biodiversity Analysis

This repository contains R scripts and workflows for retrieving, cleaning, and analyzing DNA barcode data from the [BOLD Systems API](https://www.boldsystems.org/).  
The pipeline is designed for biodiversity research in Ecuador, Costa Rica, and Panama, with automated routines for downloading, merging, summarizing, and visualizing barcode datasets across multiple localities.

---

## Features
- 🔑 Connect to BOLD Systems API with API key authentication  
- 📥 Download and store barcode datasets by process IDs  
- 💾 Use local `.rds` files for faster workflows  
- 🧹 Clean and merge datasets with standardized locality metadata  
- 📊 Generate concise summaries and export results as `.csv`  
- 🎨 Visualization-ready datasets for downstream analyses (e.g., ggplot2, vegan)

---

## Requirements
- **R (≥ 4.2.0)**
- R packages:
  - `BOLDconnectR`, `msa`, `Biostrings`, `dplyr`, `tidyr`, `ggplot2`,  
    `viridis`, `ggsci`, `plotly`, `sunburstR`, `vegan`, `tibble`

---
