
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/raw"
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea_plus.xlsx"), sheet="Done", na = "N/A")

# Read taxa key for those not in SeaLifeBase
taxa_key_missing <- readxl::read_excel(file.path(indir, "taxa_key_for_species_not_in_sealifebase.xlsx"))

# Things to do
# 2. Harmonize experiment type and treatments
# 4. Add sub-biotoxin?
# 5. Record 1 or 2 compartement (or no model)


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
                         "Patinopecten yessoensis"  = "Mizuhopecten yessoensis"))

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


# Format data
################################################################################

# Add taxa info
data <- data_orig %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Add taxa info
  left_join(spp_key1 %>% select(-comm_name) %>% unique(), by="sci_name_orig") %>% 
  # Format daily rate
  mutate(rate_d=abs(rate_d), 
         hlife_d=ifelse(is.na(hlife_d), log(2)/rate_d, hlife_d),
         rate_d=ifelse(is.na(rate_d), log(2)/hlife_d, rate_d)) %>% 
  # Format hourly rate
  mutate(rate_hr=abs(rate_hr), 
         hlife_hr=ifelse(is.na(hlife_hr), log(2)/rate_hr, hlife_hr),
         rate_hr=ifelse(is.na(rate_hr), log(2)/hlife_hr, rate_hr)) %>% 
  # Fill daily rate
  mutate(rate_d=ifelse(is.na(rate_d), rate_hr*24, rate_d),
         hlife_d=ifelse(is.na(hlife_d), hlife_hr/24, hlife_d)) %>%
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
  select(id, article_title, 
         comm_name, sci_name_orig, sci_name, genus, family, order, class, invert_yn,
         syndrome, hab_species, biotoxin,
         study_type, exp_type, treatment, feed_scenario,
         tissue, tissue_orig,
         everything())

# Inspect
freeR::complete(data)
str(data)

# HAB things
table(data$syndrome)
table(data$hab_species)
table(data$biotoxin)

# Study type
table(data$study_type)

# Experiment type
table(data$exp_type)
table(data$treatment)

# Feeding
table(data$feed_scenario)

# Rate type
table(data$rate_type)

# Check rates
ggplot(data, aes(x=hlife_hr, y=rate_hr)) +
  geom_point() +
  theme_bw()

# Check rates
ggplot(data, aes(x=hlife_d, y=rate_d)) +
  geom_point() +
  theme_bw()


# Tissue key
################################################################################

# Tissue stats
tissues <- data %>% 
  group_by(class, tissue) %>% 
  summarize(n=n_distinct(id))

# Plot tissue stats
ggplot(tissues, aes(x=n, y=reorder(tissue, n))) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="") +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))


# Export
################################################################################

# Save
saveRDS(data, file=file.path(outdir, "database.Rds"))




