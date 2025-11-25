
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

# Taxa
taxa <- freeR::taxa(data$sci_name)

# Summarize
stats <- data %>% 
  # Average annual catch
  group_by(species_code, taxa_group, isscaap, comm_name, sci_name_orig, sci_name) %>% 
  summarize(catch_mt=sum(value[year %in% yrs])/nyrs) %>% 
  ungroup() %>% 
  # Add taxa
  left_join(taxa, by=c("sci_name"="sciname")) %>% 
  # Add missing genus
  mutate(genus=ifelse(is.na(genus), str_extract(sci_name, "^[^ ]+"), genus)) %>% 
  # Fill missing taxa info
  arrange(sci_name) %>% 
  group_by(genus) %>% 
  fill(family, .direction = "updown") %>% 
  fill(order, .direction = "updown") %>% 
  fill(class, .direction = "updown") %>% 
  ungroup() %>% 
  # Fill in more missing taxa info
  mutate(family=case_when(genus=="Mytella" ~ "Mytilidae",
                          genus %in% c("Dallocardia", "Mexicardia") ~ "Cardiidae",
                          genus %in% c("Gafrarium", "Iliochione") ~ "Veneridae",
                          genus=="Hiatula" ~ "Psammobiidae",
                          genus=="Larkinia" ~ "Arcidae",
                          genus=="Ylistrum" ~ "Pectinidae",
                          T ~ family),
         order=case_when(genus=="Mytella" ~ "Mytilida",
                         genus %in% c("Dallocardia", "Mexicardia") ~ "Cardiida",
                         genus %in% c("Gafrarium", "Iliochione")  ~ "Venerida",
                         genus=="Hiatula" ~ "Cardiida",
                         genus=="Larkinia" ~ "Arcida",
                         genus=="Ylistrum" ~ "Pectinida",
                          T ~ order),
         class=ifelse(is.na(class), "Bivalvia", class)) %>% 
  # Arrange
  select(-type) %>% 
  select(species_code:isscaap, class:genus, everything()) 

freeR::complete(stats)

freeR::check_names(stats$sci_name)


# Look up life history
################################################################################

# Get FB data
fb_spp <- freeR::fishbase(species=stats$sci_name, level="order", dataset="species", cleaned=T)
fb_stocks <- freeR::fishbase(species=stats$sci_name, level="order", dataset="stocks", cleaned=T)
fb_vonb <- freeR::fishbase(species=stats$sci_name, level="order", dataset="vonb", cleaned=T)


# Climates
table(fb_stocks$env_temp)

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
  mutate(climate=tolower(climate)) %>% 
  mutate(climate=factor(climate, levels=c("tropical", "subtropical", "temperate", "deep-water", "boreal", "polar")))

# Prep von b
vonb <- fb_vonb %>% 
  filter(vonb_quality!="doubtful") %>% 
  group_by(class, order, family, genus, species) %>% 
  summarize(k=median(k, na.rm=T)) %>% 
  ungroup()

# Generate genus/family avgs
fb_spp_gen <- fb_spp %>% 
  group_by(genus) %>% 
  summarize(lmax_cm_gen=median(lmax_cm, na.rm = T)) %>% 
  ungroup()
fb_spp_fam <- fb_spp %>% 
  group_by(family) %>% 
  summarize(lmax_cm_fam=median(lmax_cm, na.rm = T)) %>% 
  ungroup()

# Generate genus/family avgs
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

# K
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


# Plot temperatures
ggplot(temp, aes(y=climate, x=temp_c)) +
  geom_boxplot()


# Build final dataset
################################################################################

# Build data
stats1 <- stats %>% 
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
  # Add order specific values
  left_join(temp_ord, by=c("order")) %>% 
  left_join(vonb_ord, by=c("order")) %>% 
  # Add class specific values
  left_join(temp_class, by=c("class")) %>% 
  left_join(vonb_class, by=c("class")) %>% 
  # Add final
  mutate(lmax_cm_use=ifelse(!is.na(lmax_cm), lmax_cm, 
                            ifelse(!is.na(lmax_cm_gen), lmax_cm_gen, lmax_cm_fam)),
         temp_c_use=ifelse(!is.na(temp_c), temp_c, 
                      ifelse(!is.na(temp_c_gen), temp_c_gen, 
                             ifelse(!is.na(temp_c_fam), temp_c_fam,
                                    ifelse(!is.na(temp_c_ord), temp_c_ord, temp_c_class)))),
         k_use=ifelse(!is.na(k), k, 
                           ifelse(!is.na(k_gen), k_gen, 
                                  ifelse(!is.na(k_fam), k_fam,
                                         ifelse(!is.na(k_ord), k_ord, k_class))))) %>% 
  mutate(lmax_cm_type=ifelse(!is.na(lmax_cm), "species", 
                            ifelse(!is.na(lmax_cm_gen), "genus", "family")),
         temp_c_type=ifelse(!is.na(temp_c), "species",
                       ifelse(!is.na(temp_c_gen), "genus", 
                              ifelse(!is.na(temp_c_fam), "family", 
                                     ifelse(!is.na(temp_c_ord), "order", "class")))),
         k_type=ifelse(!is.na(k), "species",
                            ifelse(!is.na(k_gen), "genus", 
                                   ifelse(!is.na(k_fam), "family", 
                                          ifelse(!is.na(k_ord), "order", "class"))))) %>% 
  # Reduce
  select(species_code:catch_mt, lmax_cm_use, lmax_cm_type, temp_c_use, temp_c_type, k_use, k_type) %>% 
  rename(lmax_cm=lmax_cm_use, temp_c=temp_c_use, k=k_use)

# Inspect
freeR::complete(stats1)


# Export data
################################################################################

# Export data
saveRDS(stats1, file=file.path(outdir, "harvested_bivalve_traits.Rds"))


