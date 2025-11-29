
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rotl)
library(ape)
library(phytools)
library(picante)
library(tidyverse)
library(caper)
library(phyr)
library(ggtree)

# Directories
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables"

# Load data
load(file=file.path(outdir, "phylogenetic_regression.Rdata"))
tree_orig <- tree

#  Build table
################################################################################

# Format preds
preds <- preds_full %>% 
  # Simplify
  select(order, family, genus, sci_name, comm_name, lmax_cm, k, temp_c, study_type, rate_d, rate_d_lo, rate_d_hi) %>% 
  # Round 
  mutate_at(vars(rate_d, rate_d_lo, rate_d_hi), function(x) round(x, 3)) %>% 
  # Add rate label
  mutate(rate_label=paste0(rate_d, " (", rate_d_lo, "-", rate_d_hi, ")")) %>% 
  # Remove rate info
  select(-c(rate_d, rate_d_lo, rate_d_hi)) %>% 
  # Spread
  spread(key="study_type", value="rate_label") %>% 
  # Arrange
  arrange(order, sci_name)

# Export table
################################################################################

write.csv(preds, file=file.path(tabledir, "TableS6_fitted_species_predictions.csv"), row.names = F)
