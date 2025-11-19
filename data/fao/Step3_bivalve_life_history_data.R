
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

# To do
# 1) Add taxa to species key
# 2) Impute missing values by genus, family, etc.


# Build production data
################################################################################

# Years
yrs <- 2014:2023
nyrs <- length(yrs)

# Build data
data <- data_orig  %>% 
  # Reduce
  filter(year %in% yrs) %>% 
  # Marine bivalves
  filter(area_type=="Marine" & taxa_group=="Mollusca" & !isscaap %in% c("Squids, cuttlefishes, octopuses", "Abalones, winkles, conchs", "Freshwater molluscs")) %>% 
  # Species specific
  filter(level=="species")

# Summarize
stats <- data %>% 
  group_by(species_code, taxa_group, isscaap, comm_name, sci_name_orig, sci_name) %>% 
  summarize(catch_mt=sum(value[year %in% yrs])/nyrs) %>% 
  ungroup()


# Look up life history
################################################################################

# Get FB data
fb_spp <- freeR::fishbase(species=stats$sci_name, level="species", dataset="species", cleaned=T)
fb_stocks <- freeR::fishbase(species=stats$sci_name, level="species", dataset="stocks", cleaned=T)

# Prep temp data
temp <- fb_stocks %>% 
  # Simplify
  select(species, env_temp, temp_preferred, temp_pref50, temp_min, temp_max) %>% 
  # Format preferred temp
  mutate(temp_c1=temp_pref50,
         temp_c2=temp_preferred,
         temp_c3=(temp_min+temp_max)/2,
         temp_c=ifelse(!is.na(temp_c1), temp_c1,
                       ifelse(!is.na(temp_c2), temp_c2, temp_c3))) %>% 
  # Format climate
  rename(climate=env_temp) %>% 
  mutate(climate=factor(climate, levels=c("tropical", "subtropical", "temperate", "boreal", "polar")))

# Plot temperatures
ggplot(temp, aes(y=climate, x=temp_c)) +
  geom_boxplot()


# Build final dataset
################################################################################

# Build data
stats1 <- stats %>% 
  left_join(fb_spp %>% select(species, lmax_cm), by=c("sci_name"="species")) %>% 
  left_join(temp %>% select(species, climate, temp_c), by=c("sci_name"="species"))


# Export data
################################################################################


