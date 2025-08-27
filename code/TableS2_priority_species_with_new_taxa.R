
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

# Read data
data_orig <- read.csv(file=file.path(tabledir, "TableSX_priority_species_new_taxa_raw.csv"))
diversity_orig <- read.csv(file=file.path(tabledir, "TableSX_taxa_diversity.csv"))

# Setup
################################################################################

# Build data
data <- data_orig %>% 
  # Rename
  rename(taxa_group="label") %>% 
  # Add diversity stats
  left_join(diversity_orig %>% select(-type), c("taxa_group"="catg")) %>% 
  # Fill missing diversity stats
  mutate(nspecies=case_when(taxa_group=="Ostreida" ~ 88, # Two families: Ostreidae (78), Gryphaeidae (10)
                            T ~ nspecies)) %>% 
  # Build species label
  mutate(spp_label=paste0(comm_name, " (", sci_name, ")")) %>% 
  # Summarize
  group_by(syndrome, type, taxa_group, nharvested, nspecies) %>% 
  summarize(species=paste(spp_label, collapse=", "),
            prod_mt_000=sum(prod_mt)/1000) %>% 
  ungroup() %>% 
  # Order types
  mutate(type=factor(type, levels=c("Order", "Family", "Genus"))) %>% 
  # Sort
  arrange(syndrome, type, desc(prod_mt_000)) %>% 
  # Arrange
  select(syndrome, type, taxa_group, species, prod_mt_000, everything())

# Inspect
freeR::complete(data)

# Export
write.csv(data, file=file.path(tabledir, "TableSX_priority_species_new_taxa.csv"), row.names=F)


