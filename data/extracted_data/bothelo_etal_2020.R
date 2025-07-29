
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "data/extracted_data/raw/images"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Botelho_etal_2020_supp_data.xlsx"))

# Format data
data <- data_orig %>% 
  # Gather
  gather(key="date", value="toxicity", 3:ncol(.)) %>% 
  # Recode species
  mutate(species=recode(species,
                        "M"="Mussel",
                        "C"="Cockle",
                        "RS"="Razor shell")) %>% 
  # Separate
  separate(col=toxicity, into=c("toxicity", "toxicity_sd"), sep="\\(") %>% 
  # Formate
  mutate(toxicity=gsub("\\*", "", toxicity) %>% as.numeric(),
         toxicity_sd=gsub(")", "", toxicity_sd) %>% as.numeric()) %>% 
  # Format date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Calculate day
  mutate(day=difftime(date, min(date), units="days") %>% as.numeric())


# Calculate total
data_tot <- data %>% 
  group_by(species, date, day) %>% 
  summarize(toxicity=sum(toxicity, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(toxin="Total")

# Merge
data1 <- bind_rows(data, data_tot)

# Export
write.csv(data1, file=file.path(indir, "Botelho_etal_2020_supp_data.csv"), row.names = F)

ggplot(data1, aes(x=day, y=toxicity, color=toxin)) +
  facet_wrap(~species) +
  geom_line() +
  geom_point() +
  theme_bw()


