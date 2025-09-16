
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

# Read taxa key for those not in SeaLifeBase
# taxa_key_missing <- readxl::read_excel(file.path(indir, "taxa_key_for_species_not_in_sealifebase.xlsx"))

# Things to do
# 4. Add sub-biotoxin?


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Update sci names
  mutate(sci_name=stringr::str_squish(sci_name),
         sci_name=recode(sci_name,
                         "Azumapecten farreri" = "Scaeochlamys farreri",
                         "Cancer magister" = "Metacarcinus magister",
                         "Patinopecten yessoensis" = "Mizuhopecten yessoensis",
                         "Tapes semidecussatus" = "Ruditapes philippinarum"))

# Check names
freeR::check_names(data$sci_name)


