##########################################
# 1. Installation and loading of libraries
##########################################

# Install BOLDconnectR package from GitHub
# (only needed the first time)
# devtools::install_github("https://github.com/boldsystems-central/BOLDconnectR")
# Install Bioconductor dependencies
# Uncomment if not already installed
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("msa")
# BiocManager::install("Biostrings")

# Load main libraries
library(BOLDconnectR)
library(msa)
library(Biostrings)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggsci)
library(plotly)
library(sunburstR)
library(vegan)
library(tibble)


##########################################
# 2. Data Download
##########################################
# Set BOLD API key
bold.apikey("59EC87C3-929D-4F6A-814A-FDA1D46C333C")

# Load list of process IDs from CSV file
processids <- read.csv2("seqs_ids.csv")

# ----------------------------------------
# Download data from BOLD (only needed the first time)
# ----------------------------------------
#Ecuador
cerroblanco_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMCEB)
pululahua_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMPUL)
floreana_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMFLO)
isabela_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMISA)
santacruz_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMSCZ)
sancristobal_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMSCB)
paluguillo_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMPAL)
mashpi_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_EMMAS)
#Costa Rica
baru_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_CRMBA)
cocos_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_CRMCO)
guanacaste_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_CRMGU)
quetzales_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_CRMQU)
#Panama
panama_data<-bold.fetch(get_by = "processid",identifiers = processids$processid_GMPAN)

# ----------------------------------------
# Load data from local RDS files (faster workflow)
# ----------------------------------------

cerroblanco_data <- readRDS("cerroblanco_data.rds")
floreana_data    <- readRDS("floreana_data.rds")
santacruz_data   <- readRDS("santacruz_data.rds")
sancristobal_data<- readRDS("sancristobal_data.rds")
isabela_data     <- readRDS("isabela_data.rds")
mashpi_data      <- readRDS("mashpi_data.rds")
paluguillo_data  <- readRDS("paluguillo_data.rds")
pululahua_data   <- readRDS("pululahua_data.rds")
panama_data      <- readRDS("panama_data.rds")
baru_data        <- readRDS("baru_data.rds")
cocos_data       <- readRDS("cocos_data.rds")
guanacaste_data  <- readRDS("guanacaste_data.rds")
quetzales_data   <- readRDS("quetzales_data.rds")


##########################################
# 3. Cleaning, merging, and summarizing datasets
##########################################

# Add locality column to each dataset
mashpi_data        <- mashpi_data %>% mutate(locality = "Mashpi")
paluguillo_data    <- paluguillo_data %>% mutate(locality = "Paluguillo")
pululahua_data     <- pululahua_data %>% mutate(locality = "Pululahua")
cerroblanco_data   <- cerroblanco_data %>% mutate(locality = "CerroBlanco")
floreana_data      <- floreana_data %>% mutate(locality = "Floreana")
isabela_data       <- isabela_data %>% mutate(locality = "Isabela")
sancristobal_data  <- sancristobal_data %>% mutate(locality = "SanCristobal")
santacruz_data     <- santacruz_data %>% mutate(locality = "SantaCruz")

# Costa Rica and Panama datasets
panama_data        <- panama_data %>% mutate(locality = "BarroColorado")
baru_data          <- baru_data %>% mutate(locality = "Baru")
cocos_data         <- cocos_data %>% mutate(locality = "IslaCocos")
guanacaste_data    <- guanacaste_data %>% mutate(locality = "Guanacaste")
quetzales_data     <- quetzales_data %>% mutate(locality = "Quetzales")

# Merge all datasets into a single dataframe
combined_data <- bind_rows(
  floreana_data, mashpi_data, isabela_data, paluguillo_data,
  sancristobal_data, santacruz_data, pululahua_data, cerroblanco_data,
  panama_data, baru_data, cocos_data, guanacaste_data, quetzales_data
)

# ----------------------------------------
# Dataset list (for summaries and processing)
# ----------------------------------------
datasets <- list(
  cerroblanco  = cerroblanco_data,
  floreana     = floreana_data,
  santacruz    = santacruz_data,
  sancristobal = sancristobal_data,
  isabela      = isabela_data,
  mashpi       = mashpi_data,
  paluguillo   = paluguillo_data,
  pululahua    = pululahua_data,
  panama       = panama_data,
  baru         = baru_data,
  cocos        = cocos_data,
  guanacaste   = guanacaste_data,
  quetzales    = quetzales_data
)

# Generate concise summaries for each dataset
all_data_summary <- lapply(datasets, function(df) {
  bold.data.summarize(bold_df = df, summary_type = "concise_summary")$concise_summary
})

# View summaries
all_data_summary

# Convert list of summaries to long-format dataframe
all_data_summary_df <- bind_rows(all_data_summary, .id = "Locality")

# Pivot to wide format (localities as columns)
all_data_summary_wide <- all_data_summary_df %>%
  pivot_wider(names_from = Locality, values_from = Value)

# View wide-format summary
all_data_summary_wide

# Export summary table to CSV
write.csv(all_data_summary_wide, "all_data_summary.csv", row.names = FALSE)
