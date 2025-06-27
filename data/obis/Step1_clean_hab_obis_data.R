
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(jsonlite)

# Directories
indir <- "data/obis/raw"
outdir <- "data/obis/processed"
plotdir <- "figures"

# Source
# https://hab.ioc-unesco.org/
# https://obis.org/node/33dec23c-af65-4fb1-a437-79543c562ef0
# https://mapper.obis.org/?nodeid=33dec23c-af65-4fb1-a437-79543c562ef0

#  Read data
data_orig <- readr::read_tsv(file.path(indir, "Occurrence.tsv")) %>% 
  janitor::clean_names("snake") 

# Read EEZ
eez <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/eezs/processed/EEZ_v12_polygons.Rds")
eez_df <- eez %>% sf::st_drop_geometry()


# Clean data
################################################################################

# Get first word
get_first_word <- function(x) {
  sapply(strsplit(x, "\\s+"), `[`, 1)
}

# Clumns with some data
cols <- colnames(data_orig)
cols_empty <- cols[freeR::complete(data_orig) == nrow(data_orig)]
cols_wdata <- cols[!cols %in% cols_empty]

# Clean data
data <- data_orig %>%
  # Reduce to columns with some data
  select(all_of(cols_wdata)) %>% 
  select(-species) %>% 
  # Rename
  rename(year=date_year, 
         long_dd=decimal_longitude, 
         lat_dd=decimal_latitude,
         bathy_m=bathymetry,
         depth_m=depth,
         shore_km=shoredistance, 
         sst_c=sst,
         salinity_psu=sss,
         species=scientific_name) %>% 
  # Add genus
  mutate(genus=get_first_word(species)) %>% 
  # Simplify 
  select(year, lat_dd, long_dd, 
         bathy_m, shore_km, sst_c, depth_m, salinity_psu,
         genus, species) %>% 
  # Add syndrome
  mutate(syndrome=case_when(genus %in% c("Pseudo-nitzschia", "Nitzschia") ~ "Amnesic",
                            genus %in% c("Azadinium", "Amphidoma") ~ "Azaspiracid",
                            genus %in% c("Gambierdiscus", "Fukuyoa") ~ "Ciguatera",
                            genus %in% c("Dinophysis", "Prorocentrum") ~ "Diarrhetic",
                            genus=="Karenia" ~ "Neurotoxic",
                            genus %in% c("Alexandrium", "Gymnodinium", "Pyrodinium", "Gonyaulax") ~ "Paralytic", # Gonyaulax is old name for Alexandrium
                            genus %in% c("Microcystis", "Planktothrix", "Anabaena", "Oscillatoria", # microcystins
                                         "Nodularia", # nodularins
                                         "Raphidiopsis") ~ "Cyanotoxin", # homoanatoxins
                            T ~ "Other"))

# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(genus, species, syndrome)

# Genera in other syndrome  group
spp_key %>% 
  filter(syndrome=="Other") %>% pull(genus) %>% unique() %>% sort()

# Quick plot
ggplot(data, aes(x=long_dd, y=lat_dd, color=syndrome)) +
  facet_wrap(~syndrome, ncol=4) +
  geom_point()

# Add EEZ
################################################################################




# Convert data to sf POINT object
data_sf <- sf::st_as_sf(data, coords = c("long_dd", "lat_dd"), crs = sf::st_crs(eez))

# Find index of nearest polygon for each point
nearest_idx <- sf::st_nearest_feature(data_sf, eez)

# Assign eez_id based on nearest polygon
data_eez <- data %>% 
  mutate(eez_id=eez$eez_id[nearest_idx]) %>% 
  # Add EZ details
  left_join(eez_df %>% select(eez_id, eez, territory, sovereign), by=c("eez_id")) %>% 
  # Arrange
  select(year, sovereign, territory, eez, eez_id, lat_dd, long_dd, 
         everything())

# Inspect
freeR::complete(data_eez)


# Export
################################################################################

# Export data
saveRDS(data_eez, file=file.path(outdir, "HAB_OBIS_data.Rds"))



# Old code for dining data inside EEZ
# # Disable fetaure if fixing geometry doesn't work
# sf::sf_use_s2(FALSE)
# 
# # Convert data frame to an sf POINT object
# data_sf <- sf::st_as_sf(data, coords = c("long_dd", "lat_dd"), crs = sf::st_crs(eez))
# 
# # Perform spatial join to get the eez_id from the polygons
# data_with_eez <- sf::st_join(data_sf, eez %>% select(eez_id))
# 
# # Convert back to a regular dataframe with lat/lon columns
# data_with_eez_df <- data_with_eez %>%
#   mutate(long_dd = sf::st_coordinates(.)[,1],
#          lat_dd = sf::st_coordinates(.)[,2]) %>%
#   sf::st_drop_geometry() %>% 
#   # Add EZ details
#   left_join(eez_df, by=c("eez_id")) %>% 
#   # Arrange
#   select(year, sovereign, territory, eez, lat_dd, long_dd, 
#          everything())

