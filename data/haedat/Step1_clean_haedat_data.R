
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/haedat/raw"
outdir <- "data/haedat/processed"
plotdir <- "figures"

# Source
# https://haedat.iode.org/browseEvents.php
# USer guide: https://oceanexpert.org/document/28321

# Read data
data_orig <- read.csv(file=file.path(indir, "haedat_search.csv"), 
                      na=c("", "NULL"), as.is=T,  fileEncoding = "latin1")

# Inspect
str(data_orig)

# Read EEZ
eez <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/eezs/processed/EEZ_v12_polygons.Rds")
eez_df <- eez %>% sf::st_drop_geometry()


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(event_id=event_name,
         year=event_year, 
         lat_dd=latitude,
         long_dd=longitude,
         date=event_date,
         date_beg=initial_date,
         date_end=final_date,
         syndrome=syndrome_name,
         location=location_text,
         country=country_name,
         causative_spp0=causative_species_name0,
         causative_spp1=causative_species_name1,
         causative_spp2=causative_species_name2,
         causative_spp3=causative_species_name3) %>% 
  # Format region
  mutate(region=stringr::str_to_title(region)) %>% 
  # Format syndrome
  mutate(syndrome=recode(syndrome,
                         "ASP"="Amnesic",
                         "AZP"="Azaspiracid",
                         "CFP (Ciguatera Fish Poisoning)"="Ciguatera",
                         "DSP"="Diarrhetic",
                         "NSP"="Neurotoxic",
                         "PSP"="Paralytic",
                         "OTHER"="Other",
                         "Cyanobacterial toxins effects"="Cyanotoxins",
                         "Aerosolized toxins effects"="Aerosols")) %>% 
  # Fill some missing syndromes based on toxins
  mutate(syndrome=case_when(is.na(syndrome) & toxin=="Domoic acid" ~ "Amnesic",
                            is.na(syndrome) & toxin=="Microcystins" ~ "Cyanotoxins",
                            is.na(syndrome) & toxin=="Nodularins" ~ "Cyanotoxins",
                            is.na(syndrome) & toxin=="Saxitoxins" ~ "Paralytic",
                            is.na(syndrome) & toxin=="Okadaic acid" ~ "Diarrhetic",
                            T ~ syndrome)) %>% 
  # Format toxin
  mutate(toxin=stringr::str_to_sentence(toxin)) %>% 
  # Format country
  mutate(country=recode(country, "COMORES"="COMOROS"),
         country=countrycode::countrycode(country, "country.name", "country.name"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  # Make event id unique (a single event can cause multiple syndromes?)
  mutate(event_id=make.unique(event_id, sep="_")) %>% 
  # Simplify
  select(event_id, 
         year, date, date_beg, date_end, 
         country, iso3, region, location, lat_dd, long_dd, syndrome, toxin, 
         causative_spp0, causative_spp1, causative_spp2, causative_spp3)
  

# Inspect
str(data)
freeR::complete(data)

# Unique id?
freeR::which_duplicated(data$event_id)

# Inspect more
table(data$syndrome)
sort(unique(data$toxin))
table(data$causative_spp0)

# Inspect key
toxin_key <- data %>% 
  count(toxin, syndrome)

# Region key
region_key <- data %>% 
  count(country, region)


# Add EEZ
################################################################################

# Disable fetaure if fixing geometry doesn't work
sf::sf_use_s2(FALSE)

# Reduce to data with GPS coords
data_gps <- data %>% 
  filter(!is.na(lat_dd) & !is.na(long_dd))

# Convert data frame to an sf POINT object
data_sf <- sf::st_as_sf(data_gps, coords = c("long_dd", "lat_dd"), crs = sf::st_crs(eez))

# Perform spatial join to get the eez_id from the polygons
data_with_eez <- sf::st_join(data_sf, eez %>% select(eez_id))

# Convert back to a regular dataframe with lat/lon columns
data_with_eez_df <- data_with_eez %>%
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>%
  sf::st_drop_geometry() %>% 
  left_join(eez_df)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "haedat_hab_event_database.Rds"))


# Plot data
################################################################################

# Get world
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")

# Plot
ggplot(data, aes(x=long_dd, y=lat_dd, color=syndrome)) +
  facet_wrap(~syndrome) +
  # Land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Points
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(ylim=c(-65, 85)) +
  # Theme
  theme_bw() +
  theme(legend.position = "none")






