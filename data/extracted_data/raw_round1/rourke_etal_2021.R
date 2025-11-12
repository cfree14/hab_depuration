
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw_round1"
plotdir <- "data/extracted_data/raw_round1/images"

# Read data
pst_dh_orig <- readxl::read_xlsx(file.path(indir, "work", "Rourke_etal_2021_SuppData.xlsx"), sheet=1)
ast_dh_orig <- readxl::read_xlsx(file.path(indir, "work", "Rourke_etal_2021_SuppData.xlsx"), sheet=2)
lst_mb_orig <- readxl::read_xlsx(file.path(indir, "work", "Rourke_etal_2021_SuppData.xlsx"), sheet=5)
ast_mb_orig <- readxl::read_xlsx(file.path(indir, "work", "Rourke_etal_2021_SuppData.xlsx"), sheet=6)


# Setup
################################################################################

# PST DH
pst_dh <- pst_dh_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("species", "site", "treatment", "date", "toxicity")) %>% 
  # Add syndrome
  mutate(syndrome="Paralytic", 
         sheet=1) %>% 
  # Format date
  mutate(date=lubridate::ymd(date))

# AST DH
ast_dh <- ast_dh_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("species", "site", "treatment", "date", "toxicity")) %>% 
  # Add syndrome
  mutate(syndrome="Amnesic", 
         sheet=2) %>% 
  # Format date
  mutate(date=lubridate::ymd(date))

# LST MB
lst_mb <- lst_mb_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("species", "site", "date", "dtx1", "dtx_esters")) %>% 
  # Add syndrome
  mutate(syndrome="Diarrhetic", 
         sheet=5) %>% 
  # Add toxicity
  mutate(toxicity=dtx1+dtx_esters) %>% 
  select(-c(dtx1, dtx_esters)) %>% 
  # Format date
  mutate(date=date %>% substr(., 1, 10) %>% lubridate::ymd(.))

# AST MB
ast_mb <- ast_mb_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("species", "site", "date", "toxicity")) %>% 
  # Add syndrome
  mutate(syndrome="Amnesic", 
         sheet=6) %>% 
  # Format date
  mutate(date=date %>% substr(., 1, 10) %>% lubridate::ymd(.))

# Merge
data <- bind_rows(pst_dh, ast_dh, lst_mb, ast_mb) %>% 
  # Format species
  mutate(species=recode(species, 
                        "Atlantic sea scallops" = "Scallop",           
                        "Blue mussel" = "Mussel",               
                        "Blue mussels" = "Mussel",              
                        "Eastern oysters" = "Oyster",              
                        "Soft-shell clam" = "Clam",       
                        "Soft-shell clams" = "Clam")) %>% 
  # Arrange
  arrange(sheet, syndrome, site, species, treatment, date) %>% 
  select(sheet, syndrome, site, species, treatment, date, toxicity, everything())

# Inspect
str(data)
table(data$site)
table(data$treatment)
table(data$syndrome)
table(data$species)

# Plot
ggplot(data, aes(x=date, y=toxicity, color=treatment)) +
  facet_grid(species~site, scale="free") +
  geom_point()




