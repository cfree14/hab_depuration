
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/lit_review/processed"

# Read data
data_orig1 <- readRDS("data/lit_review/round1/processed/database_round1.Rds")
data_orig2 <- readRDS("data/lit_review/round2/processed/database_round2.Rds")


# Merge data
################################################################################

# Inspect
colnames(data_orig1)
colnames(data_orig2)

# Format data 1 for merge
data1 <- data_orig1 %>% 
  # Add round
  mutate(round="Round 1") %>% 
  select(round, everything()) %>% 
  # Remove useless
  select(-id) %>% 
  # Remove taxa (going to add again)
  select(-c(genus, family, order, class, invert_yn)) %>% 
  # Rename
  rename(title=article_title)

# Format data 2 for merge
data2 <- data_orig2 %>% 
  # Remove useless
  select(-sort) %>% 
  # Add round
  mutate(round="Round 2") %>% 
  select(round, everything()) %>% 
  # Add tissue (and format down below)
  mutate(tissue_orig=tissue) %>%
  relocate(tissue_orig, .before=tissue)
  

# Merge data
data <- bind_rows(data1, data2)

# Inspect
freeR::complete(data)


# Format data
################################################################################

# Format data
data3 <- data %>% 
  # Fix some sci names
  mutate(sci_name=recode(sci_name,
                         "Chlamys farreri"="Scaeochlamys farreri",
                         "Crassostrea gigas"="Magallana gigas",
                         "Chlamys nobilis"="Mimachlamys crassicostata",
                         "Crassostrea gasar"="Crassostrea tulipa")) %>% 
  # Fix some common names
  mutate(comm_name=recode(comm_name,
                          "Sea scallops"="Atlantic sea scallop",
                          "Wedge shell"="Abrupt wedge shell"),
         comm_name=case_when(sci_name=="Perna viridis" ~ "Asian green mussel",
                             sci_name=="Perna canaliculus" ~ "Green-lipped mussel",
                             T ~ comm_name)) %>% 
  # Format feeding scenario
  mutate(feed_scenario=recode(feed_scenario, 
                              "fed clean"="fed non-toxic"))

freeR::check_names(data3$sci_name)


# Check data
################################################################################

# Syndrome
table(data3$syndrome)

# Study type
table(data3$study_type)

# Feeding scenario
table(data3$feed_scenario)

# Rate type
table(data3$rate_type)

# Number of compartments
table(data3$ncomp)

# Experiments
table(data3$exp_type)


# Check species
################################################################################

# Species key
spp_key <- data3 %>% 
  count(comm_name, sci_name)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)

# Get species info
taxa <- freeR::taxa(spp_key$sci_name)
spp_key$sci_name[!spp_key$sci_name %in% taxa$sciname] %>% unique() %>% sort() # species missing taxa info

# Read key for missing taxa
taxa_missing <- readxl::read_excel("data/lit_review/processed/taxa_key_for_species_not_in_sealifebase.xlsx") %>% 
  filter(!sciname %in% taxa$sciname)

# Merge taxa keys
taxa_full <- bind_rows(taxa, taxa_missing) %>% 
  select(-species)
freeR::which_duplicated(taxa_full$sciname)

# Add taxa to data
data4 <- data3 %>% 
  # Add taxa info
  left_join(taxa_full, by=c("sci_name"="sciname")) %>% 
  rename(invert_yn=type) %>% 
  # Fix some info
  mutate(class=case_when(sci_name=="Austromegabalanus psittacus" ~ "Thecostraca",
                         T ~ class)) %>% 
  mutate(order=case_when(sci_name=="Austromegabalanus psittacus" ~ "Balanomorpha",
                         T ~ order)) %>% 
  # Arrange
  select(round:sci_name, invert_yn, class, order, family, genus, everything()) %>% 
  # Format tissues
  mutate(tissue=recode(tissue, 
                       "kidneys"="kidney",
                       "intestinal tract"="gastrointestinal tract",
                       "whole fish"="whole",
                       "edible portion"="edible tissue",
                       "digestive tissue"="gastrointestinal tissues",
                       "adductor muscles"="adductor muscle",
                       "remaining tissue (not gills or digestive gland)"="soft tissue",
                       "edible tissue (foot, mantle, siphon, adductor muscles)"="edible tissue",
                       "non-edible tissue (gill, digistive gland, gonad)"="gills+hepatopancreas+gonads")) %>% 
  # Format tissues
  mutate(tissue=case_when(class=="Bivalvia" & tissue %in% c("whole", "edible tissue", "flesh", 
                                                            "remaining tissues", "non-viscera", "total tissue", 
                                                            "whole flesh", "whole tissue") ~ "soft tissue",
                          class=="Bivalvia" & tissue %in% c("digestive gland") ~ "hepatopancreas",
                          T ~ tissue))

# Inspect
freeR::complete(data4) # all taxa info should be complete


# Check toxins
################################################################################

# Check toxins
toxin_key <- data4 %>% 
  count(syndrome, biotoxin)

# Check subtoxins
subtoxin_key <- data4 %>% 
  count(syndrome, biotoxin, subtoxin)

# Check tissues
################################################################################

# Tissue stats
tissue_stats1 <- data4 %>% 
  count(class, tissue)

# All
ggplot(tissue_stats1, aes(y=tidytext::reorder_within(tissue, desc(n), class),
                          x=n)) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(y="", x="Count") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))

# Just bivalves
ggplot(tissue_stats1 %>% filter(class=="Bivalvia"), aes(y=tidytext::reorder_within(tissue, desc(n), class),
                          x=n)) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(y="", x="Count") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))


# Build paper key
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

# Read full paper attributes - Round 1
paper_metadata_orig1 <- readxl::read_excel("data/lit_review/round1/raw/20250618_depuration_biotoxin_marine_ocean_sea.xls", col_types = "text") %>%
  janitor::clean_names("snake") %>% 
  mutate(country_orig=sapply(addresses, get_first_author_country))

# Read full paper attributes - Round 2
paper_metadata_orig2 <- readxl::read_excel("data/lit_review/round2/raw/savedrecs.xls", col_types = "text") %>%
  janitor::clean_names("snake") %>% 
  mutate(country_orig=sapply(addresses, get_first_author_country))

# Merge full paper attributes
paper_metadata_orig <- bind_rows(paper_metadata_orig2, paper_metadata_orig1) 
colnames(paper_metadata_orig)

# Simplify metadata
paper_metadata <- paper_metadata_orig %>% 
  # Simplify
  select(publication_type, article_title, source_title, language, document_type, publication_year, doi, country_orig) %>%  # (paper_id, 
  # Rename
  rename(year=publication_year,
         journal=source_title) %>% 
  # Make unique
  unique()
freeR::which_duplicated(paper_metadata$article_title)

# Build paper stats
paper_key <- data4 %>% 
  # Reduce to unique papers
  group_by(round, paper_id, title) %>% 
  summarize(nspp=n_distinct(sci_name)) %>% 
  ungroup()

# Confirm unique
freeR::which_duplicated(paper_key$paper_id)
freeR::which_duplicated(paper_key$title)
table(paper_key$round)

# Add metadata
paper_key2 <- paper_key %>% 
  # Recode some titles to match key
  mutate(title=recode(title,
                      "Age-dependent antioxidant responses to the bioconcentration of microcystin-LR in the mysid crustacean, Neomysis awatschensis"="Age-dependent antioxidant responses to the bioconcentration of microcystin-LR in the mysid crustacean, Neomysis awatschensi")) %>%
  # Add paper meta-data
  left_join(paper_metadata, by=c("title"="article_title")) %>% 
  # Fill missing countries
  mutate(country_orig=case_when(paper_id=="Drum et al. (1993)" ~ "USA",
                                paper_id=="Vasconcelos et al. (1995)" ~ "Portugal",
                                paper_id=="Wohlgeschaffen et al. (1992)" ~ "Canada",
                                paper_id=="Scarratt et al. (1991)" ~ "Canada",
                                T ~ country_orig)) %>% 
  # Format messy countries
  mutate(country_orig=recode(country_orig,
                             "England"="Great Britain",
                             "Scotland"="Great Britain",
                             "WA 98112"="WA 98112 USA",
                             "FL 33149"="FL 33149 USA")) %>% 
  # Format country
  mutate(country=countrycode::countrycode(country_orig, "country.name", "country.name"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  # Format year
  mutate(year=as.numeric(year))

# Inspect
freeR::complete(paper_key2)
head(paper_key2)


# Final species key
################################################################################

spp_key_out <- data4 %>% 
  select(comm_name, sci_name:genus) %>% 
  unique() %>% 
  arrange(comm_name)

# Export data
################################################################################

# Export species key
writexl::write_xlsx(spp_key_out, path=file.path(outdir, "species_key.xlsx"))

# Export paper meta-data
saveRDS(paper_key2, file=file.path(outdir, "database_paper_metadata.Rds"))

# Export database
saveRDS(data4, file=file.path(outdir, "database.Rds"))
writexl::write_xlsx(data4, path=file.path(outdir, "database.xlsx"))



