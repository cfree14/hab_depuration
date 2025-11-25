
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

# Read data
data_orig <- read.csv(file.path(tabledir, "TableSX_phylo_model_comparison.csv"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  mutate(r2_stat=paste0(round(r2,2), " (", round(r2_lo,2), "-", round(r2_hi,2), ")"),
         elpd_delta=(elpd_loo-max(elpd_loo)) %>% round(., 1),
         elpd_stat=paste0(round(elpd_loo, 1), " (", round(elpd_loo_sd, 1), ")"),
         p_stat=paste0(round(p_loo, 1), " (", round(p_loo_sd, 1), ")"),
         looic_stat=paste0(round(looic, 1), " (", round(looic_loo_sd, 1), ")")) %>% 
  # Arrange
  arrange(desc(elpd_delta)) %>% 
  select(model, elpd_delta, elpd_stat, looic_stat, p_stat, r2_stat, everything()) %>% 
  # Remove useless
  select(-c(r2, r2_se, r2_lo, r2_hi, looic, looic_loo_sd, p_loo, p_loo_sd))

# Export
writexl::write_xlsx(data, path=file.path(tabledir, "TableSX_phylo_model_comparison.xlsx"))




