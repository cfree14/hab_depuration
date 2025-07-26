
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea.xls"))
# data_orig <- readxl::read_excel(file.path(indir, "20250620_dep_exc_clr_bio_tox_mar_oce.xls"))

# Things to do:
# Fill in missing countries

# Format data
################################################################################

# Function to extract the country of the first author
get_first_author_country <- function(affiliation_str) {
  if (is.na(affiliation_str) || affiliation_str == "") return(NA)
  # Extract the first affiliation
  first_affiliation <- sub("^(\\[[^]]+\\])\\s*([^;]+);?.*$", "\\2", affiliation_str)
  # Extract the last part after the last comma
  parts <- strsplit(first_affiliation, ",")[[1]]
  country <- trimws(tail(parts, 1))
  return(country)
}

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(source=source_title,
         year=publication_year,
         title=article_title) %>% 
  # Extract country
  mutate(country_orig=sapply(addresses, get_first_author_country)) %>% 
  # Simplify
  select(year,
         title,
         authors,
         source,
         volume, 
         issue,
         doi,
         document_type,
         language,
         country_orig) %>% 
  # Format
  mutate(source=stringr::str_to_title(source),
         source=gsub(" & ", " and ", source),
         source=gsub(" And ", " and ", source),
         source=gsub(" Of ", " of ", source),
         source=gsub(" In ", " in ", source)) %>% 
  # Clean country
  mutate(country=countrycode::countrycode(country_orig, "country.name", "country.name"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c"))


# Inspect data
str(data)
freeR::complete(data)

# Inspect more
table(data$language)
table(data$document_type)

# Export data
# write.csv(data, file=file.path(outdir, "2024_11_06_WOS_search.csv"), row.names=F)
write.csv(data, file=file.path(outdir, "2025_06_18_WOS_search.csv"), row.names=F)

