
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
data_orig <- readxl::read_excel(file.path(indir, "Trainer_etal_2008_Table12-2.xlsx"))



# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Format species
  mutate(species=recode(species,
                        "Cancer magister" = "Metacarcinus magister",       
                        "Cancer spidus" = "Spirontocaris spinus",          
                        "Crassostrea gigans" = "Magallana gigas",      
                        "Crassostrea japonica" = "Ylistrum japonicum",   
                        "Engraulis enchrasicolus" = "Engraulis encrasicolus", 
                        # "Ensis spp." = "",              
                        # "Ensis spp., Solen spp." = "", 
                        "Errex zachirus" = "Glyptocephalus zachirus",         
                        "Loligo opalescens" = "Doryteuthis opalescens",       
                        "Microstomus pacifcus" = "Microstomus pacificus",   
                        # "Not specified" = "",           
                        "Pecten novaezealandiae" = "Pecten novaezelandiae", 
                        "Pleuronectes decurrens" = "Pleuronichthys decurrens",  
                        "Pleuronectes vetulus" = "Parophrys vetulus",   
                        "Ruditapes decussata" = "Ruditapes decussatus",     
                        "Ruditapes decussate" = "Ruditapes decussatus",    
                        "Tiostrea chilensis" = "Ostrea chilensis",      
                        "Volsella modiolus" = "Brachidontes modiolus"))

# Check
freeR::check_names(data$species)
freeR::uniq(data$comm_name)

# Get taxa
taxa <- freeR::taxa(data$species)

# Add taxa
data_out <- data %>% 
  # Add class
  left_join(taxa %>% select(sciname, class), by=c("species"="sciname")) %>% 
  # Add syndrome
  mutate(syndrome="Amnesic")


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "Trainer_etal_2008_toxicities.Rds"))

