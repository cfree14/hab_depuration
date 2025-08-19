
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

# Read data
data_orig <- readRDS(file=file.path(indir, "1950_2023_fao_global_production.Rds"))

# Read data (add ISO3 in other)
obis_orig <- readRDS("data/obis/processed/HAB_OBIS_data.Rds") 
obis <- obis_orig %>% 
  mutate(iso3=countrycode::countrycode(sovereign, "country.name", "iso3c"))
freeR::complete(obis)
unique(obis$syndrome)


# Build production data
################################################################################

# Build data
yrs <- 2014:2023
nyrs <- length(yrs)
data <- data_orig  %>% 
  # Reduce
  filter(year %in% yrs) %>% 
  # Marine bivalves
  filter(area_type=="Marine" & taxa_group=="Mollusca" & !isscaap %in% c("Squids, cuttlefishes, octopuses", "Abalones, winkles, conchs", "Freshwater molluscs")) %>% 
  # Species specific
  filter(level=="species") %>% 
  # Format a few common names
  mutate(comm_name=recode(comm_name,
                          "Whelk"="Common whelk",
                          "Atlantic surf clam"="Atlantic surfclam",
                          "Northern quahog(=Hard clam)"="Northern quahog",
                          "Pacific cupped oyster"="Pacific oyster",
                          "American cupped oyster"="Eastern oyster",
                          "Common edible cockle"="Common cockle",
                          "Great Atlantic scallop"="King scallop",
                          "American sea scallop"="Atlantic sea scallop",
                          "South American rock mussel"="Brown mussel",
                          "Southern Australia scallop"="Commercial scallop",
                          "Yesso scallop"="Japanese scallop", 
                          "New Zealand mussel"="Greenshell mussel", 
                          "Green mussel"="Green-lipped mussel", 
                          "Cholga mussel"="Ribbed mussel")) %>% 
  # Correct scientific names
  mutate(sci_name=recode(sci_name,
                         "Lutraria oblonga" =  "Lutraria magna",
                         "Mytilus unguiculatus" =  "Mytilus coruscus",
                         "Spisula sibyllae"  =  "Spisula sachalinensis" ))
  
# Inpsect
table(data$measure)
table(data$isscaap)

# Check names
freeR::check_names(data$sci_name)
# Aliger gigas
# Anadara kagoshimensis 
# Anadara similis
# Aptyxis syracusana 
# Atrina maura 
# Austrofusus glans 
# Crassula aequilatera 
# Dallocardia muricata
# Donax dentifer
# Ensis leei 
# Glycymeris nummaria
# Glycymeris ovata
# Hiatula diphos 
# Iliochione subrugosa
# Larkinia grandis 
# Magallana bilineata 
# Magallana gigas
# Magallana sikamea
# Melongena patula 
# Monoplex parthenopeus 
# Mytella strigata
# Polititapes aureus
# Polititapes rhomboides
# Rochia nilotica
# Saccostrea cuccullata 
# Sinistrofulgur sinistrum 
# Spisula murchisoni 

# Build taxa key
spp <- sort(unique(data$sci_name))
taxa <- freeR::taxa(spp)

# Get taxonomic classification using WoRMS (via worrms in taxize)
# You could also use ITIS, NCBI, or GBIF if preferred
spp_not_matched <- spp[!spp %in% taxa$sciname]
spp_not_matched_info <- taxize::classification(spp_not_matched , db = "worms")
spp_not_matched_df <- purrr::map2_df(spp_not_matched_info,
                         names(spp_not_matched_info),
                         ~ mutate(.x, species = .y)) %>% 
  # Reduce
  filter(rank %in% c("Class", "Order", "Family", "Genus")) %>%
  select(species, rank, name) %>% 
  mutate(rank=tolower(rank)) %>% 
  # Spread
  tidyr::pivot_wider(names_from = rank, values_from = name) %>% 
  rename(sciname=species)

# Merge taxa keys
taxa_key <- bind_rows(taxa, spp_not_matched_df) %>% 
  # Simplify
  select(class, order, family, genus, sciname)

# Add species info to data
data1 <- data %>% 
  # Add taxa info
  left_join(taxa_key, by=c("sci_name"="sciname")) %>% 
  # Arrange
  select(area_code:isscaap, class, order, family, genus, sci_name, comm_name, everything()) %>% 
  # Remove
  select(-c(sci_name_nwords, level))

# Inspect
freeR::complete(data1)

# Export
saveRDS(data1, file=file.path(outdir, "FAO_vulnerable_bivalve_catch_data.Rds"))

