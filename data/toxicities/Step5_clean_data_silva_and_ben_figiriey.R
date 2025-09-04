
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
silva_orig <- readxl::read_excel(file.path(indir, "Silva_etal_2018_Tables13.xlsx"))
ben_orig <- readxl::read_excel(file.path(indir, "Ben-Figirey_etal_2020_Table2.xlsx"))


# Format Silva et al. (2018)
################################################################################

# Format data
silva <- silva_orig %>% 
  # Rename
  rename(class=type) %>% 
  # Add
  mutate(dataset="Silva et al. (2018)",
         syndrome="Paralytic") %>% 
  # Format toxicity
  mutate(toxicity_ug_kg=recode(toxicity_ug_kg, "<LOQ"="0") %>% as.numeric(),
         toxicity_mg_kg=toxicity_ug_kg/1000) %>% 
  # Format names
  mutate(species=recode(species, 
                        "Holothuria (Platyperona) sanctori" = "Holothuria sanctori")) %>% 
  # Format class
  mutate(class=recode(class,
                      "Barnacle"="Maxillopoda",
                      "Bivalve"="Bivalvia", 
                      "Limpet"="Gastropoda",
                      "Sea cucumber"="Holothuroidea",
                      "Sea slug"="Gastropoda",
                      "Sea snail"="Gastropoda",
                      "Sea urchin"="Echinoidea",
                      "Starfish"="Asteroidea"))
  
# Inspect
str(silva)
freeR::complete(silva)

# Inspect more
table(silva$class)

# Check names
freeR::check_names(silva$species)
# Correct
# "Diadema africanum" 
# "Patella aspera"              
# "Patella gomesii"                   
# "Patella ordinaria"                 
# "Phorcus lineatus" 

# Add taxa
# taxa <- freeR::taxa(silva$species)
# silva_out <- silva %>% 
#   left_join(taxa %>% select(-species), by=c("species"="sciname"))


# Format Ben-Figirey et al. (2020)
################################################################################

# Format data
ben <- ben_orig %>% 
  # Rename
  rename(date_orig=date) %>% 
  # Format date
  mutate(date=ifelse(grepl("/", date_orig),
                     dmy(date_orig) %>% as.character(),
                     as.numeric(date_orig) %>% as.Date(., origin = "1899-12-30") %>% as.character()),
         date=ymd(date)) %>% 
  # Format species
  mutate(species=recode(species, 
                        "Chlamys varia"="Mimachlamys varia")) %>% 
  # Format toxicity
  mutate(toxicity_mg_kg=toxicity_ug_kg/1000)

# Inspect more
freeR::check_names(ben$species)

# Add taxa
taxa <- freeR::taxa(ben$species)
ben_out <- ben %>% 
  left_join(taxa %>% select(-species), by=c("species"="sciname"))

# Export data
################################################################################

# Export
saveRDS(silva, file=file.path(outdir, "Silva_etal_2018_toxicities.Rds"))
saveRDS(ben_out, file=file.path(outdir, "Ben-Figirey_etal_2020_toxicities.Rds"))



