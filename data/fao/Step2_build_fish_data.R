
# Downloaded data from:
# https://www.fao.org/fishery/en/collection/capture?lang=en

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "/Users/cfree/Dropbox/Chris/UCSB/data/fao/global_production/processed"
outdir <- "data/fao/processed"

# Read data
data_orig <- readRDS(file=file.path(indir, "1950_2023_fao_global_production.Rds"))

# Read depuration data
dep_orig <- readRDS("data/lit_review/round1/processed/database.Rds") %>% 
  mutate(syndrome=recode(syndrome, "Brevetoxin"="Neurotoxic"))
unique(dep_orig$syndrome)
dep_spp_key <- dep_orig %>% 
  select(comm_name, sci_name) %>% unique()

# Read data (add ISO3 in other)
obis_orig <- readRDS("data/obis/processed/HAB_OBIS_data.Rds") 
obis <- obis_orig %>% 
  mutate(iso3=countrycode::countrycode(sovereign, "country.name", "iso3c")) %>% 
  filter(syndrome=="Ciguatera")
freeR::complete(obis)
unique(obis$syndrome)


# Format depuration data
################################################################################

# Species-toxin key
dep <- dep_orig %>% 
  select(sci_name, comm_name, syndrome) %>% 
  unique() %>% 
  filter(syndrome=="Ciguatera")

# Build species key
################################################################################

# Build data
yrs <- 2014:2023
nyrs <- length(yrs)
spp_key <- data_orig  %>% 
  # Reduce
  filter(year %in% yrs) %>% 
  # Marine finfish
  filter(area_type=="Marine" & prod_type=="Capture" & taxa_group=="Pisces" & 
           !isscaap %in% c("Miscellaneous diadromous fishes", 
                           "Miscellaneous freshwater fishes", 
                           "Salmons, trouts, smelts", 
                           "Shads")) %>% 
  # Species specific
  filter(level=="species") %>% 
  # In countries of interest
  filter(iso3 %in% obis$iso3) %>% 
  # Species
  count(isscaap, comm_name, sci_name)
 
  
# Loop up species
# spp_fb2 <- freeR::fishbase(dataset="ecosystem", species=spp_key$sci_name[1:10], cleaned = F)
# spp_fb3 <- freeR::fishbase(dataset="ecology", species=spp_key$sci_name[1:10], cleaned = T)
spp_fb <- freeR::fishbase(dataset="species", species=spp_key$sci_name, cleaned = T)
spp_fb_use <- spp_fb %>% 
  filter(database=="FishBase") %>% 
  unique()
freeR:::which_duplicated(spp_fb_use$species)

# Add and filter
spp_key2 <- spp_key %>% 
  left_join(spp_fb_use %>% select(species, habitat, lmax_cm), by=c("sci_name"="species")) %>% 
  filter(habitat=="reef-associated" & lmax_cm>=25)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to data of interest
  filter(prod_type=="Capture" & year %in% yrs & iso3 %in% obis$iso3 & sci_name %in% spp_key2$sci_name & measure=="Live weight, mt") %>% 
  filter(isscaap!="Herrings, sardines, anchovies") %>% 
  # Summarize
  group_by(isscaap, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(value)/nyrs) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(landings_mt)) %>% 
  # Add rank
  mutate(rank=1:n())

# Any with depuration rates?
data_dep <- data %>% 
  filter(sci_name %in% dep$sci_name)

# Same but calculate on your own
data_dep2 <- data_orig %>% 
  filter(iso3 %in% obis$iso3 & sci_name %in% dep$sci_name & year %in% yrs) %>% 
  # Summarize
  group_by(isscaap, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(value)/nyrs) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(landings_mt))

# Top 50
data50 <- data %>% 
  slice(1:50)

# Rank of ones with depuration rates
dep_rank <- data %>% 
  filter()


# Stats for manuscript
################################################################################

# 

# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     axis.title.y=element_blank(),
                     legend.text=element_text(size=8),
                     legend.title=element_blank(),
                     strip.text=element_text(size=9),
                     plot.title=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data50, aes(x=landings_mt/1e3, 
                 y=reorder(comm_name, desc(landings_mt)))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (1000s mt)", y="", title="Ciguatera") +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_finfish_priority_species.png"), 
       width=4.5, height=6.5, units="in", dpi=600)




