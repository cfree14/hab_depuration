
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/round2/raw"
outdir <- "data/lit_review/round2/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Biotoxin depuration database - round 2.xlsx"), sheet="Final")

# Read derived rates
rates_orig <- readxl::read_excel("data/extracted_data/processed/fitted_model_results_round2.xlsx")


# 1) Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Update sci names
  rename(sci_name_orig=sci_name) %>% 
  mutate(sci_name_orig=stringr::str_squish(sci_name_orig),
         sci_name=recode(sci_name_orig,
                         "Azumapecten farreri" = "Scaeochlamys farreri",
                         "Cancer magister" = "Metacarcinus magister",
                         "Patinopecten yessoensis" = "Mizuhopecten yessoensis",
                         "Tapes semidecussatus" = "Ruditapes philippinarum",
                         "Chlamys farreri" = "Scaeochlamys farreri",
                         # "Diplodus sargus"  = correct,  
                         # "Scaeochlamys farreri" = correct,
                         "Venus gallina" = "Chamelea gallina")) %>% 
  relocate(sci_name, .after=sci_name_orig) %>% 
  # Update common names
  mutate(comm_name=stringr::str_squish(comm_name),
         comm_name=recode(comm_name, 
                          "Razor clam"="Pacific razor clam",
                          "Manila clams"="Manila clam")) %>% 
  # Format HAB species
  mutate(hab_species=recode(hab_species,
                            "A. catenella" = "Alexandrium catenella",
                            "A. pacificum" = "Alexandrium pacificum",
                            "A.minutum"  = "Alexandrium minutum",
                            "genera Gambierdiscus and Fukuyoa" = "Gambierdiscus spp., Fukuyoa spp.",
                            "Pseudonitzschia" = "Pseudo-nitzschia sp.",
                            "Pseudo-nizschia sp." = "Pseudo-nitzschia sp.",
                            "Nitzschia pungens (Pseudo-nizschia sp.)" = "Pseudo-nitzschia pungens",
                            "Ptychodiscus brevis"="Karenia brevis",
                            "Promcentrum lima" = "Prorocentrum lima",
                            "Nassarius semiplicata" = "Nassarius sinarum",
                            "Gonyaulax tamarensis" = "Alexandrium tamarense", 
                            "Alexandrium catenatum" = "Gymnodinium catenatum")) %>% 
  # Format compartments
  mutate(ncomp=recode(ncomp, 
                      "none"="no model")) %>% 
  # Format tissues
  mutate(tissue=recode(tissue,
                       "edible part (foot, mantle, siphon and addnctor muscles)" = "edible tissue",                                
                       "edible tissues" = "edible tissue",
                       "gall bladder" = "gallbladder",
                       "gill" = "gills",
                       "heptopancreas" = "hepatopancreas",
                       "soft whole-body" = "whole", 
                       "summed tissue burden (muscle, intestine, gills, stomach, gall bladder, liver and spleen)" = "whole",
                       "total flesh" = "whole",
                       "visceral mass" = "viscera",
                       "whole flesh" = "whole", 
                       "whole tissue" = "whole")) %>% 
  # Format feeding scenario
  mutate(feed_scenario=recode(feed_scenario,
                              "fed clean"="fed non-toxic",
                              "not stated"="unknown")) %>% 
  # Format non-numeric half lives
  mutate(hlife_hr=recode(hlife_hr,
                         "ND"="") %>% as.numeric(.),
         hlife_d=recode(hlife_d,
                        "n.a."="") %>% as.numeric(.)) %>% 
  # Remove useless
  select(-c("checked?", "type", "keywords_authors", "keywords_plus", "abstract", "include_YN"))


# Inspect
str(data)
freeR::complete(data)

# Syndrome
table(data$syndrome)

# HAB species
# Correct: Alexandrium pacificum, Nassarius sinarum, Protoceratium reticulatum
table(data$hab_species)
freeR::check_names(data$hab_species)

# Biotoxin
table(data$biotoxin)

# Subtoxin
table(data$subtoxin)

# Number of compartments
table(data$ncomp)

# Tissue
freeR::uniq(data$tissue)

# Study type
table(data$study_type)

# Feeding
table(data$feed_scenario)

# Experiment
table(data$exp_type)


# Check names
################################################################################

# Check scientific names
# Correct: Diplodus sargus, Scaeochlamys farreri
freeR::check_names(data$sci_name)

# Check species
spp_key <- data %>% 
  count(comm_name, sci_name)
freeR::which_duplicated(spp_key$comm_name) # must be zero
freeR::which_duplicated(spp_key$sci_name) # must be zero


# 2) Add derived rates
################################################################################

# Format derived rates
rates <- rates_orig %>%
  # Simplify
  select(file, treatment, k) %>% 
  # Rename
  rename(datafile=file, 
         datafile_id=treatment,
         rate_d_derived=k) 


# Add to data
data1 <- data %>% 
  # Add to data
  left_join(rates, by=c("datafile", "datafile_id")) %>% 
  # Use derived data when data is not reported
  mutate(rate_d=ifelse(is.na(rate_d), rate_d_derived, rate_d))


# 3) Compute more rates
################################################################################

# Compute all rates
data2 <- data1 %>% 
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
         hlife_d_prob=abs(hlife_d_pdiff)>0.01) #%>% 
  # Mark useable rates
  # mutate(rate_use=ifelse(subtoxin=="Total" & study_type=="lab" , "yes", "no")) %>% 
  # relocate(rate_use, .after=ncomp)

# Inspect
# Should have the same number of missing rates/half-lives
freeR::complete(data2)

# Check rates
# Confirmed that the 1 incorrect one is the fault of the study authors 
# (using values reported by the authors; no way to know which is wrong/right)
ggplot(data2 %>% filter(rate_hr<0), aes(x=hlife_hr, y=abs(rate_hr), color=hlife_d_prob)) +
  geom_point() +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="log10") +
  theme_bw()

# Check rates
ggplot(data2, aes(x=hlife_d, y=abs(rate_d), color=hlife_d_prob)) +
  geom_point() +
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="log10") +
  theme_bw()

# Remove useless
data_out <- data2 %>% 
  select(-c(hlife_d_check, hlife_d_pdiff, hlife_d_prob, rate_d_derived))

# Ultimately, everything but notes and HAB species should be full, except maybe some rare cases
# Its okay for HAB species to be missing
# I confirmed that the 8 missing rates should indeed be missing; 
freeR::complete(data_out)


# Are any papers with subtoxin dep rates missing TOTAL dep rates? 
################################################################################

key <- data_out %>% 
  group_by(paper_id, comm_name, syndrome, tissue, exp_type, treatment) %>% 
  summarize(subtoxins=paste(subtoxin, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(total_yn=ifelse(grepl("Total", subtoxins), "yes", "no")) %>% 
  filter(total_yn=="no") %>% 
  unique()


# Export
################################################################################

# Save
saveRDS(data_out, file=file.path(outdir, "database_round2.Rds"))



