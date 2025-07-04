
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/raw"
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea_plus.xlsx"), sheet="Done")


# Build species skey
################################################################################

# Build species key
spp_key <- data_orig %>% 
  select(comm_name, sci_name) %>% 
  unique() %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Update sci names
  mutate(sci_name=recode(sci_name_orig, 
                         "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                         "Bellamya aeruginosa" = "Sinotaia aeruginosa", # freshwater
                         "Cancer magister" = "Metacarcinus magister",                                                         
                         # "Mactra veneriformis" = "Mactra quadrangularis",
                         "Hiatula rostrata"="Hiatula diphos", # based on sleuthing, correct name for Solen rostratus [Lightfoot, 1786] ·
                         "Neomysis awatschensi" = "Neomysis awatschensis",                                                     
                         "Ostrea rivularis" = "Magallana rivularis",   # uncertain > taxon inquirendum                                                     
                         "Patinopecten yessoensis"  = "Mizuhopecten yessoensis"))

# Check names
freeR::check_names(spp_key$sci_name)

# Check for duplicates
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)

# Taxa
taxa <- freeR::taxa(species=spp_key$sci_name)


# Add taxa to data
spp_key1 <- spp_key %>%
  # Add taxa info
  left_join(taxa, by=c("sci_name"="sciname")) %>% 
  # Fill missing genus
  mutate(genus=stringr::word(sci_name, 1)) %>% 
  # Fill missing based on genus
  group_by(genus) %>% 
  fill(type:family, .direction = "updown") %>% 
  ungroup() %>% 
  # Fill fish/invert quickly
  mutate(type=ifelse(genus %in% c("Hiatula", "Magallana", "Sinotaia"), "invert", type)) %>% 
  # Remove species
  select(-species)



# Format data
################################################################################

# Add taxa info
data <- data_orig %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Add taxa info
  left_join(spp_key1 %>% select(sci_name_orig, sci_name, type))

# Inspect
table(data$syndrome)
table(data$hab_species)


# Tissues
################################################################################

tissues <- data %>% 
  count(type, tissue)

ggplot(tissues, aes(x=n, y=reorder(tissue, n))) +
  facet_grid(type~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of measurements", y="") +
  # Theme
  theme_bw()





