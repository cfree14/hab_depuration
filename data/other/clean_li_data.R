
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("data/other/Yang_etal_2022_ciguatera_records.xlsx")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Recode names
  mutate(species=stringr::str_trim(species),
         species=recode(species,
                        "Amberjack" = "Seriola fasciata",
                        "Australian spanish mackerel" = "Scomberomorus commerson",
                        "B. Vetula" = "Balistes vetula",
                        # "Balistes capriscus" = "",         
                        "Black moray eel (m. Helena)" = "Muraena helena", 
                        "Carnivorous"= "Carnivorous spp.",                  
                        "Cheilinus undulates" = "Cheilinus undulatus",
                        "Common two-banded seabream" = "Diplodus vulgaris",
                        "Crenimugil crenilabi" = "Crenimugil crenilabis",         
                        "Dog snapper" = "Lutjanus jocu",
                        "Dusky grouper" = "Epinephelus marginatus",
                        "Gambierdiscus"  = "Pterois volitans",             
                        "Grey snapper"= "Lutjanus griseus",
                        "Grouper"  = "Variola louti",                    
                        "Grouper (serranidae)"   = "Serranidae spp.",      
                        "Groupers" = "Epinephelus spp.",                   
                        "Herbivorous"  = "Herbivorous spp.",                
                        "Horse eye jack"= "Caranx latus",               
                        "Horse-eye jack" = "Caranx latus",              
                        "K. Sectatrix" = "Kyphosus sectatrix",               
                        "Lionfish" = "Pterois volitans",                    
                        "Liza vaigiensis" = "Ellochelon vaigiensis",             
                        "Lutjanus monostigm" = "Lutjanus monostigma",          
                        "Moray eels"  = "Gymnothorax spp.",                
                        "NM"  = "Unknown Japanese fish spp.",                         
                        "Omnivorous"  = "Omnivorous spp.",                
                        "Red snapper" = "Lutjanus bohar",
                        "Samoan moray eel" = "Samoan moray eel spp.",           
                        "Sawtooth barracuda" = "Sphyraena putnamae",
                        "Schoolmaster snapper"  = "Lutjanus apodus",
                        "Seriola spp." = "Seriola spp.",              
                        "Spanish mackerel" = "Scomberomorus commerson",
                        "Tectus niloticus"  = "Rochia nilotica",
                        "White-edged grouper" = "Variola albimarginata", # "Epinephelus albomarginatus",
                        "Yellow goatfish" = "Mulloidichthys martinicus",
                        "Yellow jack" = "Carangoides bartholomaei"))

# Check names
freeR::check_names(data$species)

# Build key
species <- sort(unique(data$species))

# Get FishLife traits
fl <- freeR::fishlife(species)

# Get FB traits
fb <- freeR::fishbase(dataset="species", species=species, cleaned = T)
fb_use <- fb %>% 
  filter(database=="FishBase")

# Add to data
data1 <- data %>% 
  left_join(fb_use %>% select(species, lmax_cm), by="species")

ggplot(data1, aes(x=lmax_cm, y=toxicity_ug_kg)) +
  geom_point()



