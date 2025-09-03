
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/toxicities/raw"
outdir <- "data/toxicities/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Lefebvre_Robertson_2010_Table2.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Fix species
  mutate(species=recode(species,
                        "Delphinus" = "Delphinus delphis",             
                        "Eopsetta exillis" = "Lyopsetta exilis",     
                        "Errex zachirus" = "Glyptocephalus zachirus",        
                        "Megaptera novaengliae" = "Megaptera novaeangliae", 
                        "Pelicanus occidentalis" = "Pelecanus occidentalis",
                        "Pleuronectes decurrens" = "Pleuronichthys decurrens",
                        "Pleuronectes vetulus" = "Parophrys vetulus"))

# Check names
freeR::check_names(data$species)

# Get taxa
taxa <- freeR::taxa(data$species)

# 
data_out <- data %>% 
  left_join(taxa %>% select(-species), by=c("species"="sciname"))


# Export data
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "Lefebvre_Robertson_2010_toxicities.Rds"))
