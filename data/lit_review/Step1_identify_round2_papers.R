

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Read round 1 papers
data1_orig <- readxl::read_excel("data/lit_review/round1/raw/20250618_depuration_biotoxin_marine_ocean_sea.xls") %>% janitor::clean_names("snake")
data2_orig <- readxl::read_excel("data/lit_review/round2/raw/savedrecs.xls") %>% janitor::clean_names("snake")


# Filter round 2 paper
################################################################################

# Remove round 1 papers
data2 <- data2_orig %>% 
  # Reduce to new articles
  filter(!article_title %in% data1_orig$article_title ) %>% 
  # Add paper id
  mutate(nauthors=stringr::str_count(authors, ";"),
         first_author=stringr::str_extract(authors, "^[^,]*") %>% stringr::str_to_title(),
         paper_id=ifelse(nauthors==1, 
                         paste0(first_author, " (", publication_year, ")"),
                         paste0(first_author, " et al. (", publication_year, ")")),
         paper_id=make.unique(paper_id, sep="-")) %>% 
  # Arrange
  select(paper_id, everything())

# Confirm that id is unqiue
freeR::which_duplicated(data2$paper_id)

# Appaarently, some round 1 papers weren't in round 2 - interesting!
data1 <- data1_orig %>% 
  filter(!article_title %in% data2_orig$article_title )

# Export
writexl::write_xlsx(data2, path="data/lit_review/round2/raw/2025_07_30_round2_wos_search.xlsx")
