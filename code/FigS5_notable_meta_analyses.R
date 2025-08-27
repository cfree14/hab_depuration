
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "figures"

# Read data
# Alvarez et al., 2020
# Chen and Chou, 2002
# Min et al., 2018
min_orig <- readxl::read_excel(file.path(indir, "Min_etal_2018_Fig1.xlsx"))


# Setup
################################################################################


# Plot data
################################################################################

ggplot(min_orig, mapping=aes(x=day, y=toxicity, color=treatment, shape=species)) + 
  geom_point()
