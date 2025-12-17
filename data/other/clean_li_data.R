
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("data/other/Li_etal_2023_ciguatera_records.xlsx", na=c("NM", "NC", "NC[4]"))


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
                        "Carangoides bartholomaei" = "Caranx bartholomaei",
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
                        "Yellow jack" = "Caranx bartholomaei"),
         species=case_when(family=="Lycodontïa .lamnlcur" ~ "Gymnothorax javanicus", # Lycodontis javanicus in paper = Gymnothorax javanicus (Bleeker, 1859)
                           T ~ species)) %>% 
  # Remove family
  select(-family) %>% 
  # Format method
  mutate(method=recode(method,
                       "HPLC/MS[3]" = "HPLC/MS",
                       "LC–MS/MS[6]" = "LC-MS/MS",
                       "LC/MS/MS" = "LC-MS/MS",
                       "LC–MS/MS"="LC-MS/MS",
                       "LC/MS" = "LC-MS",
                       "MBA[7]" = "MBA",
                       "RBA[1]" = "RBA",
                       "microplate reader (Molecular Devices Spectra Max M2)"="Microplate reader",
                       "Microplate reader (Molecular Devices Spectra Max M2)"="Microplate reader")) %>% 
  # Format country
  mutate(country=case_when(region=="Nuku Hiva Island (Marquesas)" ~ "French Polynesia",
                           region=="Guadeloupe" ~ "Guadeloupe",
                           T ~ country))

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$region)
table(data$country)
table(data$method)
sort(unique(data$method))

# Check names
# Correct: Balistes capriscus, Gambierdiscus toxicus, Rochia nilotica
freeR::check_names(data$species)

# Species traits
################################################################################

# Build key
species <- sort(unique(data$species))

# Get FishLife traits
# fl <- freeR::fishlife(species)

# Get FB traits
fb <- freeR::fishbase(dataset="species", species=species, cleaned = T)
fb_use <- fb %>% 
  filter(database=="FishBase")


# Add traits to data
################################################################################

# Add to data
data1 <- data %>% 
  # Add traits
  left_join(fb_use %>% select(species, comm_name, habitat, lmax_cm, dangerous), by="species") %>% 
  # Fill missing sci names
  mutate(species=ifelse(is.na(species), "Unknown species", species)) %>% 
  # Fill missing common names
  mutate(comm_name=ifelse(is.na(comm_name), species, comm_name)) %>% 
  # Arrange
  select(species, comm_name, habitat, dangerous, lmax_cm, everything())


# Plots
################################################################################

ggplot(data1, aes(x=lmax_cm, y=toxicity_ug_kg)) +
  geom_point()

# Plot toxicity
ggplot(data1, aes(y=reorder(comm_name, desc(toxicity_ug_kg)), x=toxicity_ug_kg)) +
  geom_point() +
  # Labels
  labs(x="Toxicity (ug/kg)", y="") +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export
saveRDS(data1, file="data/other/Li_etal_2023_ciguatera_records.Rds")
