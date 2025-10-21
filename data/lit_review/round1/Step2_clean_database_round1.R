
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/round1/raw"
outdir <- "data/lit_review/round1/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea_plus.xlsx"), sheet="Done", na = "N/A")

# Read taxa key for those not in SeaLifeBase
taxa_key_missing <- readxl::read_excel(file.path(indir, "taxa_key_for_species_not_in_sealifebase.xlsx"))

# Read derived rates
rates_orig <- readxl::read_excel("data/extracted_data/processed/fitted_model_results.xlsx")

# Things to do
# 1. Format toxin/subtoxin - progress
# 2. Check derived vs. digitized
# 3. Confirm positive vs. negative k values (mark true positives) - progress
# X. Confirm that all derived k's have been added - progress

# DONE
# X. Confirm agreement between k and half-life



# Build species key
################################################################################

# Build species key
spp_key <- data_orig %>% 
  select(comm_name, sci_name) %>% 
  unique() %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Update sci names
  mutate(sci_name=recode(sci_name_orig, 
                         "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                         "Bellamya aeruginosa" = "Sinotaia quadrata", # freshwater, I used to think Sinotaia aeruginosa
                         "Cancer magister" = "Metacarcinus magister",                                                         
                         # "Mactra veneriformis" = "Mactra quadrangularis",
                         "Hiatula rostrata"="Hiatula diphos", # based on sleuthing, correct name for Solen rostratus [Lightfoot, 1786] ·
                         # "Neomysis awatschensi" = "Neomysis awatschensis",                                                     
                         "Ostrea rivularis" = "Magallana rivularis",   # uncertain > taxon inquirendum                                                     
                         "Patinopecten yessoensis"  = "Mizuhopecten yessoensis",
                         "Crassostrea gigas"="Magallana gigas"))

# Check names
freeR::check_names(spp_key$sci_name)

# Check for duplicates
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)

# Taxa
taxa <- freeR::taxa(species=spp_key$sci_name) 
taxa_full <- bind_rows(taxa, taxa_key_missing)

# Add taxa to data
spp_key1 <- spp_key %>%
  # Add taxa info
  left_join(taxa_full, by=c("sci_name"="sciname")) %>% 
  # Fill missing genus
  mutate(genus=stringr::word(sci_name, 1)) %>% 
  # Fill missing based on genus
  # group_by(genus) %>% 
  # fill(type:family, .direction = "updown") %>% 
  # ungroup() %>% 
  # Remove species
  select(-species) %>% 
  # Rename
  rename(invert_yn=type)


# Formatting steps
# 1) Basic formatting
# 2) Add derived rates
# 3) Expand rate derivation

# 1) Basic formatting
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Add taxa info
  left_join(spp_key1 %>% select(-comm_name) %>% unique(), by="sci_name_orig") %>% 
  # Format tissue
  # Digestive tract includes the hepatopancreas/digestive gland
  rename(tissue_orig=tissue) %>% 
  mutate(tissue=case_when(class=="Malacostraca" & tissue_orig %in% c("hepatopancreas", "digestive gland") ~ "hepatopancreas",
                           class=="Malacostraca" & tissue_orig %in% c("whole", "soft tissue") ~ "soft tissue",
                           class=="Gastropoda" & tissue_orig %in% c("muscle", "foot") ~ "foot",
                           class=="Gastropoda" & tissue_orig %in% c("digestive gland", "hepatopancreas") ~ "hepatopancreas",
                           class=="Bivalvia" & tissue_orig %in% c("digestive gland", "hepatopancreas") ~ "hepatopancreas",
                           class=="Bivalvia" & tissue_orig %in% c("whole", "soft tissue", "tissue", "edible portion", "whole flesh", "meat") ~ "soft tissue",
                            T ~ tissue_orig)) %>% 
  # Order
  select(-include_YN) %>% 
  select(id, paper_id, article_title, 
         comm_name, sci_name_orig, sci_name, genus, family, order, class, invert_yn,
         syndrome, hab_species, biotoxin, subtoxin,
         study_type, exp_type, treatment, feed_scenario,
         tissue_orig, tissue, 
         rate_type, source, datafile, datafile_id, 
         ncomp, rate_hr, hlife_hr, rate_d, hlife_d, notes,
         everything())

# Inspect
freeR::complete(data1) # HAB species can be missing; ultimately rates will get filled
str(data1)

# HAB things
table(data1$syndrome)
table(data1$hab_species)
table(data1$biotoxin)
table(data1$subtoxin)

# Check HAB species
# Correct: Azadinium spinosum, Alexandrium pacificum, Protoceratium reticulatum
freeR::check_names(data1$hab_species)

# Study type
table(data1$study_type)

# Experiment type
table(data1$exp_type)
table(data1$treatment)

# Feeding
table(data1$feed_scenario)

# Rate type
table(data1$rate_type)

# Check syndrome and toxin
toxin_key <- data1 %>% 
  count(syndrome, biotoxin)

# Check if totals are present 
check_tot <- data1 %>% 
  group_by(paper_id) %>% 
  summarize(subtoxins=paste(sort(unique(subtoxin)), collapse=", ")) %>% 
  ungroup() %>% 
  mutate(type=case_when(grepl("Total", subtoxins) ~ "Total present",
                        T ~ "Other"))


# Inspect tissues
################################################################################

# Tissue stats
tissues <- data1 %>% 
  group_by(class, tissue) %>% 
  summarize(n=n_distinct(id))

# Plot tissue stats
ggplot(tissues, aes(x=n, 
                    y=tidytext::reorder_within(tissue, desc(n), class))) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))

# Tissue stats
tissues_spp <- data1 %>% 
  group_by(class, tissue) %>% 
  summarize(n=n_distinct(sci_name))

# Plot tissue stats
ggplot(tissues_spp, aes(x=n, 
                    y=tidytext::reorder_within(tissue, desc(n), class))) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of species", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))


# 2) Add derived rates
################################################################################

# Format derived rates
rates <- rates_orig %>%
  # Simplify
  select(file, treatment, k) %>% 
  # Rename
  rename(datafile=file, 
         datafile_id=treatment,
         rate_d_derived=k) %>% 
  # Confirm that this rate is in database
  # If id is NA, then the dep rate is missing from the database and present in extracted data
  left_join(data1 %>% select(id, datafile, datafile_id), by=c("datafile", "datafile_id"))
freeR::complete(rates)

# Add to data
data2 <- data1 %>% 
  # Add to data
  left_join(rates %>% select(-id), by=c("datafile", "datafile_id")) %>% 
  # Use derived data when data is not reported
  mutate(rate_d=ifelse(is.na(rate_d), rate_d_derived, rate_d))


# 3) Compute more rates
################################################################################

# Compute more rates
data3 <- data2 %>% 
  # Format daily rate
  mutate(rate_d=rate_d, 
         hlife_d=ifelse(is.na(hlife_d), log(2)/abs(rate_d), hlife_d),
         rate_d=ifelse(is.na(rate_d), log(2)/hlife_d*-1, rate_d)) %>% 
  # Format hourly rate
  mutate(rate_hr=rate_hr, 
         hlife_hr=ifelse(is.na(hlife_hr), log(2)/abs(rate_hr), hlife_hr),
         rate_hr=ifelse(is.na(rate_hr), log(2)/hlife_hr*-1, rate_hr)) %>% 
  # Fill daily rate
  mutate(rate_d=ifelse(is.na(rate_d), rate_hr*24, rate_d),
         hlife_d=ifelse(is.na(hlife_d), hlife_hr/24, hlife_d)) %>% 
  # Fill hour rate
  mutate(rate_hr=ifelse(is.na(rate_hr), rate_d/24, rate_hr),
         hlife_hr=ifelse(is.na(hlife_hr), hlife_d*24, hlife_hr)) %>% 
  # Check half life
  mutate(hlife_d_check=log(2)/abs(rate_d),
         hlife_d_pdiff=abs(hlife_d-hlife_d_check)/hlife_d_check,
         hlife_d_prob=abs(hlife_d_pdiff)>0.01) %>% 
  # Mark useable rates
  mutate(rate_use=ifelse(subtoxin=="Total" & study_type=="lab" , "yes", "no")) %>% 
  relocate(rate_use, .after=ncomp)

# Inspect
# Should have the same number of missing rates/half-lives
freeR::complete(data3)

# Check rates
ggplot(data3 %>% filter(rate_hr<0), aes(x=hlife_hr, y=abs(rate_hr), color=hlife_d_prob)) +
  geom_point() +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="log10") +
  theme_bw()

# Check rates
ggplot(data3, aes(x=hlife_d, y=abs(rate_d), color=hlife_d_prob)) +
  geom_point() +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="log10") +
  theme_bw()

# Remove useless
data_out <- data3 %>% 
  select(-c(hlife_d_check, hlife_d_pdiff, hlife_d_prob, rate_d_derived))

# Ultimately, everything but notes should be full, except maybe some rare cases
freeR::complete(data_out)


# Export
################################################################################

# Save
saveRDS(data_out, file=file.path(outdir, "database_round1.Rds"))




