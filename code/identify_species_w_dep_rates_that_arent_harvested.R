
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
prod_orig <- readRDS(file="/Users/cfree/Dropbox/Chris/UCSB/data/fao/global_production/processed/1950_2023_fao_global_production.Rds")
prod_spp <- sort(unique(prod_orig$sci_name))

# Read depuration data
dep_orig <- readRDS("data/lit_review/round1/processed/database.Rds") 


# Setup
################################################################################

# Species key
spp <- dep_orig %>% 
  select(class, order, family, genus, sci_name, comm_name) %>% 
  unique() %>% 
  mutate(harvested_yn=sci_name %in% prod_spp)
