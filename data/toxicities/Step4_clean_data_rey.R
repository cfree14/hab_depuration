
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
data_orig <- readxl::read_excel(file.path(indir, "Rey_etal_2025_Table1.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Add
  mutate(dataset="Rey et al. 2025",
         reference=dataset,
         syndrome="Paralytic",
         date_orig=date,
         tissue="edible part") %>% 
  # Format date
  mutate(date=ifelse(grepl("/", date_orig),
                     dmy(date_orig) %>% as.character(),
                     as.numeric(date_orig) %>% as.Date(., origin = "1899-12-30") %>% as.character()),
         date=ymd(date)) %>% 
  # Format group
  mutate(group=recode(group,
                      "bivalve" = "Bivalvia",   
                      "cnidarian" = "Cnidaria",   
                      "crustacean" = "Crustacea",   
                      "echinoderm" = "Echinodermata",  
                      "gastropod" = "Gastrapoda")) %>% 
  # Fix some species
  mutate(species=case_when(species=="n.i." & group=="Cnidaria" ~ "Cnidaria sp.",
                           species=="n.i." & group=="Crustacea" ~ "Crustacea sp.",
                           species=="n.i." & group=="Gastrapoda" ~ "Gastrapoda sp.",
                           T ~ species)) %>% 
  # Add common name
  mutate(comm_name=recode(species,
                          "Balanus sp." = "Barnacle sp.",             
                          "Carcinus maenas" = "European green crab",        
                          "Haliotis sp." = "Abalone sp.",             
                          "Haliotis tuberculata" = "Green ormer",     
                          "Littorina sp." = "Periwinkle sp.",           
                          "Luidia sarsi" = "L. sarsi starfish",             
                          "Mytilus galloprovincialis" = "Mediterranean mussel",
                          "Nassarius sp." = "Nassarius dog whelk sp.",           
                          "Nucella sp." = "Nucella dog whelk sp.",             
                          "Ophiothrix sp." = "Brittle star sp.",           
                          "Patella sp." = "Limpet sp.",              
                          "Patella ulyssiponensis" = "Rough limpet",   
                          "Polybius henslowii" = "Henslow's swimming crab")) %>% 
  # Convert ug/kg to mg/kg
  mutate(toxicity_mg_kg=toxicity_ug_kg/1000) %>% 
  # Arrange
  select(dataset, reference, syndrome, 
         code, date_orig, date, region, group, species, comm_name, 
         toxicity_mg_kg, toxicity_ug_kg, toxins_detected, toxins_quantified, everything())
  
# Inspect
str(data)
freeR::complete(data)

# Inspect more
freeR::check_names(data$species)
freeR::uniq(data$class)

# Check names
freeR::check_names(data$species)

# Add taxa
taxa <- freeR::taxa(data$species)
data_out <- data %>% 
  # Add taxa
  left_join(taxa %>% select(-species), by=c("species"="sciname")) %>% 
  # Add genus
  mutate(genus=ifelse(is.na(genus), stringr::word(species, 1, 1), genus)) %>% 
  # Fill
  group_by(genus) %>% 
  fill(type:family, .direction="updown") %>% 
  ungroup()


# Export data
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "Rey_etal_2025_toxicities.Rds"))



