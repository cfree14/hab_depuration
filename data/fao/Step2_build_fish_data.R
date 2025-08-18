
# Downloaded data from:
# https://www.fao.org/fishery/en/collection/capture?lang=en

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "/Users/cfree/Dropbox/Chris/UCSB/data/fao/global_production/processed"
outdir <- "data/fao/processed"

# Read production data
data_orig <- readRDS(file=file.path(indir, "1950_2023_fao_global_production.Rds"))

# Read OBIS data (add ISO3 where this gets formatted!)
obis_orig <- readRDS("data/obis/processed/HAB_OBIS_data.Rds") 

# Things to do
# 1. Fill missing taxa info
# 2. Filter out known low-trophic
# 3. Add Lmax to output

# Format OBIS data
################################################################################

# Format OBIS data
obis <- obis_orig %>% 
  # Add I3O3
  mutate(iso3=countrycode::countrycode(sovereign, "country.name", "iso3c")) %>% 
  # Reduce to countries with ciguarera
  filter(syndrome=="Ciguatera")

# Inspect
freeR::complete(obis)
unique(obis$syndrome)

# Vulnerable ISOs
vulnerable_isos <- obis %>% pull(iso3) %>% unique()


# Identify vulnerable finfish species
################################################################################

# Years to look over
yrs <- 2014:2023
nyrs <- length(yrs)

# Freshwater ISSCAAPs
freshwater_isscaaps <- c("Miscellaneous diadromous fishes", 
                         "Miscellaneous freshwater fishes", 
                         "Salmons, trouts, smelts", 
                         "Shads")

# Identify recent marine capture finfish from countries of interest
spp_key <- data_orig  %>% 
  # Reduce
  filter(year %in% yrs) %>% 
  # Marine finfish (exclude frewshwater ISSCAAPs)
  filter(area_type=="Marine" & prod_type=="Capture" & taxa_group=="Pisces" & !isscaap %in% freshwater_isscaaps) %>% 
  # Species specific
  filter(level=="species") %>% 
  # In countries of interest
  filter(iso3 %in% vulnerable_isos) %>% 
  # Species
  count(isscaap, comm_name, sci_name)
 
  
# Loop up species
# spp_fb2 <- freeR::fishbase(dataset="ecosystem", species=spp_key$sci_name[1:10], cleaned = F)
# spp_fb3 <- freeR::fishbase(dataset="ecology", species=spp_key$sci_name[1:10], cleaned = T)
spp_fb <- freeR::fishbase(dataset="species", species=spp_key$sci_name, cleaned = T)
spp_fb_use <- spp_fb %>% 
  filter(database=="FishBase") %>% 
  unique()
freeR:::which_duplicated(spp_fb_use$species)

# Add and filter
spp_key2 <- spp_key %>% 
  # Add size and habitat
  left_join(spp_fb_use %>% select(species, habitat, lmax_cm), by=c("sci_name"="species")) %>%
  # Reduce to large reef-associated fish
  filter(habitat=="reef-associated" & lmax_cm>=25)

# Get taxa
taxa <- freeR::taxa(spp_key2$sci_name) %>% 
  select(class:genus, sciname)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to data of interest
  filter(prod_type=="Capture" & year %in% yrs & iso3 %in% obis$iso3 & sci_name %in% spp_key2$sci_name & measure=="Live weight, mt") %>% 
  filter(isscaap!="Herrings, sardines, anchovies") %>% 
  # Summarize
  group_by(isscaap, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(value)/nyrs) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(landings_mt)) %>% 
  # Add rank
  mutate(rank=1:n()) %>% 
  # Add taxa info
  left_join(taxa, by=c("sci_name"="sciname")) %>% 
  # Fill missing taxa info
  mutate(genus=ifelse(is.na(genus), stringr::word(sci_name, 1, 1), genus)) %>% 
  group_by(genus) %>% 
  fill(class:family, .direction = "updown") %>% 
  ungroup() %>% 
  # Add Lmax
  left_join(spp_key2 %>% select(sci_name, lmax_cm), by=c("sci_name")) %>% 
  # Arrange
  select(isscaap, class, order, family, genus, sci_name, comm_name, lmax_cm, rank, landings_mt, everything()) %>% 
  arrange(desc(landings_mt))

freeR::complete(data)


# Export
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "FAO_vulnerable_finfish_species.Rds"))
