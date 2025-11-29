
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/lit_review/processed"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))


# Build table
################################################################################

# Format data
data <- data_orig %>% 
  # Remove NA and pos rates
  filter(rate_d<0 & !is.na(rate_d)) %>% 
  # Recode some common namae
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
  # Summarize 
  group_by(class, family, genus, sci_name, comm_name, syndrome) %>% 
  summarize(npapers=n_distinct(paper_id)) %>% 
  ungroup() %>% 
  # Spread
  spread(key="syndrome", value="npapers") %>% 
  # Arrange
  select(class, family, genus, sci_name, comm_name,
         Paralytic, Amnesic, Diarrhetic, Cyanotoxin, Neurotoxic, Ciguatera,
         Azaspiracid, Other)


# Export table
################################################################################

# Write
write.csv(data, file=file.path(tabledir, "TableS3_species_in_database.csv"), row.names=F)


