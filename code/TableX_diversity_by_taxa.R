
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
tabledir <- "tables"
outdir <- "data/fao/processed"

# Read production data
data_orig <- readRDS(file=file.path(outdir, "FAO_vulnerable_bivalve_catch_data.Rds"))


# Compute overall diversity
################################################################################

# All SeaLifeBase
all_fish <- freeR::all_fish()
freeR::which_duplicated(all_fish$sciname)

# Count
orders_all <- all_fish %>% 
  count(order) %>% 
  rename(nspecies=n)
families_all <- all_fish %>% 
  count(family) %>% 
  rename(nspecies=n)
genera_all <- all_fish %>% 
  count(genus) %>% 
  rename(nspecies=n)

# Compute harvest diversity
################################################################################

# Species stats
spp <- data_orig %>% 
  group_by(isscaap, class, order, family, genus, sci_name, comm_name) %>% 
  summarize(prod_mt=sum(value, na.rm = T)/10) %>% 
  ungroup()

# Orders
orders <- spp %>% 
  count(order) %>% 
  rename(nharvested=n) %>% 
  left_join(orders_all) %>% 
  mutate(type="Order") %>% 
  rename(catg=order)

# Families
families <- spp %>% 
  count(family) %>% 
  rename(nharvested=n) %>% 
  left_join(families_all) %>% 
  mutate(type="Family") %>% 
  rename(catg=family)

# Genera
genera <- spp %>% 
  count(genus) %>% 
  rename(nharvested=n) %>% 
  left_join(genera_all) %>% 
  mutate(type="Genus") %>% 
  rename(catg=genus)

# Mere
merge <- bind_rows(orders, families, genera)

# Export
write.csv(merge, file=file.path(tabledir, "TableSX_taxa_diversity.csv"), row.names=F)


