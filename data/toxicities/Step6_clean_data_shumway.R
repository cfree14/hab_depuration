
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/toxicities/raw"
outdir <- "data/toxicities/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Shumway_1995_Table12_finereader.xlsx"))


# Format data
################################################################################

# Fix scientific names
# Split toxicity into value and units
# Convert toxicities to common units
# Export

# Format data
data <- data_orig 

# Inspect
freeR::complete(data)

# Check name
freeR::check_names(data$species)





