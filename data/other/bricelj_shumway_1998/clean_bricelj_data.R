
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("data/other/bricelj_shumway_1998/Bricelj_Shumway_1998_Table4.xlsx", na="-")


# Format data
################################################################################

# Format data 
data <- data_orig %>% 
  # Fix some species
  mutate(species=recode(species,
                        "Cardium edule" = "Cerastoderma edule",          
                        "Crassostrea cucullata" = "Saccostrea cuccullata",
                        "Crassostrea iridescens" = "Striostrea prismatica",
                        "Patinopecten yessoensis" = "Mizuhopecten yessoensis",
                        "Saxidomus giganteus" = "Saxidomus gigantea")) %>% 
  # Format peak toxicity
  mutate(peak_toxicity=gsub("~", "", peak_toxicity_orig),
         peak_toxicity=case_when(grepl("-", peak_toxicity) ~ sub(".*-\\s*", "", peak_toxicity),
                                 T ~ peak_toxicity) %>% as.numeric()) %>% 
  relocate(peak_toxicity, .after=peak_toxicity_orig) %>% 
  # Format rate
  rename(rate_perc_d_orig=dep_perc_d) %>% 
  mutate(rate_perc_d = gsub("X- = ", "", rate_perc_d_orig),
         rate_perc_d = gsub(".*=\\s*", "", rate_perc_d),
         rate_perc_d = recode(rate_perc_d, 
                              "8.9 - 18.1"="13.5") %>% as.numeric(.) / 100) %>% 
  relocate(rate_perc_d, .after=rate_perc_d_orig) %>% 
  # Convert to k and half life
  mutate(k= log( (rate_perc_d-1) * -1) * -1) %>% 
  mutate(hlife_d=log(2)/k) %>% 
  # Arrange
  select(group:rate_perc_d, k, hlife_d, everything())
  

# Inspect
str(data)
freeR::complete(data)

# Check names
freeR::check_names(data$species)


# Export data
################################################################################

# Export
saveRDS(data, "data/other/bricelj_shumway_1998/Bricelj_Shumway_1998_Table4.Rds")


