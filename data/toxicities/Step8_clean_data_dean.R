
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
data_orig <- readxl::read_excel(file.path(indir, "Dean_etal_2020_AppendixA.xlsx"), na="nt")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Format species
  mutate(species=recode(species,
                        "Alyconium digitatum" = "Alcyonium digitatum",                       
                        "Baccinum undatum" = "Buccinum undatum",                             
                        "Colus gracilus" = "Colus gracilis",    
                        "Hinia reticulata" = "Tritia reticulata",                            
                        "Liocarcinus holsatus" = "Polybius holsatus",                          
                        "Paguris bernhardus" = "Pagurus bernhardus")) %>% 
  # Format 
  mutate(pst_ugkg_fld=recode(pst_ugkg_fld, "nd"="0") %>% as.numeric(.),
         pst_ugkg_ms=recode(pst_ugkg_ms, "nd"="0", "Nd"="0") %>% as.numeric(.)) %>% 
  # Add max
  rowwise() %>% 
  mutate(pst_ugkg=pmax(pst_ugkg_fld, pst_ugkg_ms, na.rm=T)) %>% 
  ungroup() %>% 
  # Convert
  mutate(pst_mgkg=pst_ugkg/1000) %>% 
  # Format sample id
  mutate(sample_id=sub(".*?CEND", "CEND", sample_id, perl = TRUE),
         sample_id=gsub("CEND ", "CEND", sample_id)) %>% 
  # Split sample id and year
  separate(sample_id, into=c("sample_id", "year"), sep=" ", remove=T, convert=T)

# Inspect
str(data)
  
# Check
freeR::check_names(data$species)
freeR::uniq(data$comm_name)

# Get taxa
taxa <- freeR::taxa(data$species)

# Add taxa
data_out <- data %>% 
  # Add class
  left_join(taxa %>% select(sciname, class), by=c("species"="sciname")) 

# Export data
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "Dean_etal_2020_toxicities.Rds"))

