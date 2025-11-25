

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

# Load phy
load(file=file.path(outdir, "phylogenetic_regression.Rdata"))
phy_orig <- preds_all_full 

# Load nested
load(file=file.path(outdir, "nested_regression.Rdata"))
nest_orig <- preds_stats 

fit_data <- fit$data

# Setup
################################################################################

# Build data
phy <- phy_orig %>% 
  select(sci_name, study_type, rate_d) %>% 
  rename(species=sci_name,
         rate_d_phy=rate_d)

# Build data
nest <- nest_orig %>% 
  select(species, study_type, est) %>% 
  rename(rate_d_nest=est)

# Merge
data <- phy %>% 
  left_join(nest) %>% 
  filter(study_type=="field") %>% 
  mutate(data_yn=ifelse(species %in% fit_data$species, "yes", "no"))


# Plot data
################################################################################

ggplot(data, aes(x=rate_d_phy, y=rate_d_nest, color=data_yn)) +
  geom_point() +
  lims(x=c(0,0.1), y=c(0,0.1))

