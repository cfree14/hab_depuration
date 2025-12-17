
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
 
  
# Look up species
# spp_fb2 <- freeR::fishbase(dataset="ecosystem", species=spp_key$sci_name[1:10], cleaned = F)

# Look up diet info
spp_diet <- freeR::fishbase(dataset="ecology", species=spp_key$sci_name, cleaned = T)
freeR:::which_duplicated(spp_diet$species)
spp_diet1 <- spp_diet %>% 
  # Remove blank SeaLifeBase 
  filter(database!="SeaLifeBase") %>% 
  # Calculate mean trohic level b/c get repeates
  group_by(species, prey_type) %>% 
  summarize(troph_diet=mean(troph_diet, na.rm=T),
            troph_food=mean(troph_food, na.rm=T)) %>% 
  ungroup() %>% 
  unique() %>% 
  # Add more prey type
  mutate(prey_type=ifelse(!is.na(prey_type) & (troph_diet <= 2.2 | troph_food <= 2.2), "mainly plants/detritus (troph. 2-2.19)", prey_type)) %>% 
  mutate(prey_type=ifelse(!is.na(prey_type), "unknown", prey_type))
freeR:::which_duplicated(spp_diet1$species)

# Look up habitat, Lmax, citguarta records
spp_traits <- freeR::fishbase(dataset="species", species=spp_key$sci_name, cleaned = T)
freeR:::which_duplicated(spp_traits$species)
spp_traits1 <- spp_traits %>% 
  # Remove blank SeaLifeBase 
  filter(database!="SeaLifeBase") %>% 
  select(species, habitat, lmax_cm, dangerous) %>% 
  unique() %>% 
  mutate(ciguatera_yn=ifelse(grepl("ciguatera", dangerous), "yes", "no"))
freeR:::which_duplicated(spp_traits1$species)

# Expand
spp_key2 <- spp_key %>% 
  # Add size and habitat
  left_join(spp_traits1, by=c("sci_name"="species")) %>%
  # Add prey type
  left_join(spp_diet1 %>% select(species, prey_type), by=c("sci_name"="species"))

# Filter
cig_spp <- c("Lagodon rhomboides", "Epinephelus coioides", "Pterois volitans", "Mugil cephalus")
spp_key3 <- spp_key2 %>% 
  # Reduce to large reef-associated fish
  filter(sci_name %in% cig_spp | ciguatera_yn=="yes" | (habitat=="reef-associated" & lmax_cm>=25 & prey_type!="mainly plants/detritus (troph. 2-2.19)"))

# Get taxa
taxa <- freeR::taxa(spp_key3$sci_name) %>% 
  select(class:genus, sciname)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to data of interest
  filter(prod_type=="Capture" & year %in% yrs & iso3 %in% obis$iso3 & sci_name %in% spp_key3$sci_name & measure=="Live weight, mt") %>% 
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
  left_join(spp_key3 %>% select(sci_name, lmax_cm, habitat, prey_type, ciguatera_yn), by=c("sci_name")) %>% 
  # Arrange
  select(isscaap, class, order, family, genus, sci_name, comm_name, 
         habitat, prey_type, lmax_cm, ciguatera_yn, rank, landings_mt, everything()) %>% 
  arrange(desc(landings_mt))

freeR::complete(data)


# Export
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "FAO_vulnerable_finfish_species.Rds"))
