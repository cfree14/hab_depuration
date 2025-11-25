
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
datadir <- "data/lit_review/processed"
plotdir <- "figures"
outdir <- "output"

# Read data
data_orig <- readRDS(file.path(datadir, "database.Rds"))

# Read data for predictions
pred_data_all <- readRDS("data/fao/processed/harvested_bivalve_traits.Rds")

# Step 1. Bivalve phylogeny
# Step 2. Format prediction data
# Step 3. Format training data


# Step 1. Retrieve bivalve phylogeny
################################################################################

# 1. Search for bivalve OTT id
biv_search <- rotl::tnrs_match_names("Bivalvia", context_name = "Animals")
biv_ott <- biv_search$ott_id[1]

# 2. Get the subtree for Bivalvia
bivalvia_tree <- rotl::tol_subtree(ott_id = biv_ott) %>% 
  ape::compute.brlen(., method = "Grafen")

# 3. Plot bivalve tree
plot(bivalvia_tree, cex = 0.1, no.margin = TRUE)


# Step 2. Prediction data
################################################################################

# Species in predictions
pred_spp <- pred_data_all$sci_name

# ROTL info on species in predictions
pred_spp_rotl_info <- rotl::tnrs_match_names(pred_spp) %>% 
  # Add scientific name
  mutate(sci_name=stringr::str_to_sentence(search_string)) %>% 
  # Build tip label as it appears in the tree
  mutate(tip.label=paste0(gsub(" ", "_", unique_name), "_ott", ott_id)) 

# Add ROTL id
pred_data_all1 <- pred_data_all %>% 
  # Add OTT id
  left_join(pred_spp_rotl_info %>% select(sci_name, tip.label), by=c("sci_name"))



# Step 3. Training data
################################################################################

# Build dataset of interest
data <- data_orig %>% 
  # Bivalve PST depurarion rates
  filter(class=="Bivalvia" & syndrome=="Paralytic" & subtoxin=="Total" ) %>% 
  # Remove records that aren't species specific
  filter(!grepl("spp.", sci_name)) %>% 
  # Remove NA and positive depuration rates
  filter(!is.na(rate_d) & rate_d < 0)

# Inspect
freeR::complete(data)


# 1c. Prepare species life history
################################################################################

# Species
data_spp <- sort(unique(data$sci_name))

# Use traits assessmbled for predictions
traits1 <- pred_data_all %>% 
  # Reduce to species with data
  filter(sci_name %in% data_spp) %>% 
  # Simplify
  select(sci_name, lmax_cm, temp_c, k, lmax_cm_type, temp_c_type, k_type)

# Look up traits for missing
missing_spp <- data %>% 
  # Reduce to species without data
  filter(!sci_name %in% traits1$sci_name) %>% 
  # Simplify
  select(class, order, family, genus, sci_name) %>% 
  unique()

# Retrieve missing
fb_spp <- freeR::fishbase(species=missing_spp$sci_name, level="order", dataset="species", cleaned=T)
fb_stocks <- freeR::fishbase(species=missing_spp$sci_name, level="order", dataset="stocks", cleaned=T)
fb_vonb <- freeR::fishbase(species=missing_spp$sci_name, level="order", dataset="vonb", cleaned=T)

# Prep temp data
temp <- fb_stocks %>% 
  # Simplify
  select(class, order, family, genus, species, env_temp, temp_preferred, temp_pref50, temp_min, temp_max) %>% 
  # Format preferred temp
  mutate(temp_c1=temp_pref50,
         temp_c2=temp_preferred,
         temp_c3=(temp_min+temp_max)/2,
         temp_c=ifelse(!is.na(temp_c1), temp_c1,
                       ifelse(!is.na(temp_c2), temp_c2, temp_c3))) %>% 
  # Format climate
  rename(climate=env_temp) %>% 
  mutate(climate=tolower(climate))

# Prep Von b
vonb <- fb_vonb %>% 
  filter(vonb_quality!="doubtful") %>% 
  group_by(class, order, family, genus, species) %>% 
  summarize(k=median(k, na.rm=T)) %>% 
  ungroup()

# Generate genus/family avgs: lmax
fb_spp_gen <- fb_spp %>% 
  group_by(genus) %>% 
  summarize(lmax_cm_gen=median(lmax_cm, na.rm = T)) %>% 
  ungroup()
fb_spp_fam <- fb_spp %>% 
  group_by(family) %>% 
  summarize(lmax_cm_fam=median(lmax_cm, na.rm = T)) %>% 
  ungroup()

# Generate genus/family avgs: temp_c
temp_gen <- temp %>% 
  group_by(genus) %>% 
  summarize(temp_c_gen=median(temp_c, na.rm = T)) %>% 
  ungroup()
temp_fam <- temp %>% 
  group_by(family) %>% 
  summarize(temp_c_fam=median(temp_c, na.rm = T)) %>% 
  ungroup() 
temp_ord <- temp %>% 
  group_by(order) %>% 
  summarize(temp_c_ord=median(temp_c, na.rm = T)) %>% 
  ungroup() 
temp_class <- temp %>% 
  group_by(class) %>% 
  summarize(temp_c_class=median(temp_c, na.rm = T)) %>% 
  ungroup() 

# Generate genus/family avgs: K
vonb_gen <- vonb %>% 
  group_by(genus) %>% 
  summarize(k_gen=median(k, na.rm = T)) %>% 
  ungroup()
vonb_fam <- vonb %>% 
  group_by(family) %>% 
  summarize(k_fam=median(k, na.rm = T)) %>% 
  ungroup()
vonb_ord <- vonb %>% 
  group_by(order) %>% 
  summarize(k_ord=median(k, na.rm = T)) %>% 
  ungroup()
vonb_class <- vonb %>% 
  group_by(class) %>% 
  summarize(k_class=median(k, na.rm = T)) %>% 
  ungroup()

# Add traits for missing species
traits2 <- missing_spp %>% 
  # Add species specific values
  left_join(fb_spp %>% select(species, lmax_cm), by=c("sci_name"="species")) %>% 
  left_join(temp %>% select(species, temp_c), by=c("sci_name"="species")) %>% 
  left_join(vonb %>% select(species, k), by=c("sci_name"="species")) %>% 
  # Add genus specific values
  left_join(fb_spp_gen, by=c("genus")) %>% 
  left_join(temp_gen, by=c("genus")) %>% 
  left_join(vonb_gen, by=c("genus")) %>% 
  # Add family specific values
  left_join(fb_spp_fam, by=c("family")) %>% 
  left_join(temp_fam, by=c("family")) %>% 
  left_join(vonb_fam, by=c("family")) %>% 
  # Select final
  mutate(lmax_cm_use=case_when(!is.na(lmax_cm) ~ lmax_cm,
                               is.na(lmax_cm) & !is.na(lmax_cm_gen) ~ lmax_cm_gen,
                               T ~ lmax_cm_fam),
         temp_c_use=case_when(!is.na(temp_c) ~ temp_c,
                               is.na(temp_c) & !is.na(temp_c_gen) ~ temp_c_gen,
                               T ~ temp_c_fam),
         k_use=case_when(!is.na(k) ~ k,
                               is.na(k) & !is.na(k_gen) ~ k_gen,
                               T ~ k_fam)) %>% 
  # Mark source
  mutate(lmax_cm_type=case_when(!is.na(lmax_cm) ~ "species",
                               is.na(lmax_cm) & !is.na(lmax_cm_gen) ~ "genus",
                               T ~ "family"),
         temp_c_type=case_when(!is.na(temp_c) ~ "species",
                              is.na(temp_c) & !is.na(temp_c_gen) ~  "genus",
                              T ~ "family"),
         k_type=case_when(!is.na(k) ~ "species",
                         is.na(k) & !is.na(k_gen) ~  "genus",
                         T ~ "family")) %>% 
  # Simplify
  select(sci_name, lmax_cm_use, temp_c_use, k_use, lmax_cm_type, temp_c_type, k_type) %>% 
  rename(lmax_cm=lmax_cm_use, temp_c=temp_c_use, k=k_use)

# Merge
traits <- bind_rows(traits1, traits2)


# 1d. Add life history traits to data
################################################################################

# Add traits
data1 <- data %>% 
  # Add traits
  left_join(traits)

# Inspect
freeR::complete(data1)


# 1e. Final formatting (eliminate species not in phylogeny)
################################################################################

# ROTL info on species in dataset
data_spp_rotl_info <- rotl::tnrs_match_names(data_spp) %>% 
  mutate(search_string=stringr::str_to_sentence(search_string))

# Add ROTL id
data2 <- data1 %>% 
  # Add OTT id
  left_join(data_spp_rotl_info %>% select(search_string, ott_id), by=c("sci_name"="search_string")) %>% 
  # Build tip label
  mutate(tip.label=paste0(gsub(" ", "_", sci_name), "_ott", ott_id))


# Export data
################################################################################

# Export data
saveRDS(bivalvia_tree , file=file.path(outdir, "bivalve_phylogeny.Rds"))
saveRDS(pred_data_all1, file=file.path(outdir, "bivalve_prediction_data.Rds"))
saveRDS(data2, file=file.path(outdir, "bivalve_pst_depuration_rates.Rds"))





