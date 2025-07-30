
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "data/extracted_data/raw/images"

# This doesn't seem to match figure

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Qiu_etal_2020_TableS3.xlsx"), sheet="Wide", na=c("nd", "<LOD"))

# Format data
data <- data_orig %>% 
  # Gather
  gather(key="day", value="toxicity", 4:ncol(.)) %>% 
  mutate(day=as.numeric(day)) %>% 
  # Summarize
  group_by(species, treatment, day) %>% 
  summarize(toxicity=sum(toxicity, na.rm=T)) %>% 
  ungroup()


# Calculate total
data_tot <- data %>% 
  group_by(species, day) %>% 
  summarize(toxicity=sum(toxicity, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(treatment="Total")

# Merge
data1 <- bind_rows(data, data_tot)

# Export
write.csv(data1, file=file.path(indir, "Qiu_etal_2020_TableS3.csv"), row.names = F)

ggplot(data1, aes(x=day, y=toxicity, color=treatment)) +
  facet_wrap(~species) +
  geom_line() +
  geom_point() +
  theme_bw()


