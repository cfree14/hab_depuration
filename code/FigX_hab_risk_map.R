
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
haedir <- "data/haedat/processed"
obisdir <- "data/obis/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(haedir, "haedat_hab_event_database.Rds"))
obis_orig <- readRDS(file=file.path(obisdir, "HAB_OBIS_data.Rds"))

# Get world
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")

# Get EEZs
eezs <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/eezs/processed/EEZ_v12_polygons.Rds")
eezs_df <- eezs %>% sf::st_drop_geometry()


# Build data
################################################################################

# Biotoxins do
sort(unique(data_orig$syndrome))
syndromes_do <- c("Paralytic", "Diarrhetic", "Amnesic", "Ciguatera", "Cyanotoxins",
                  "Azaspiracid", "Neurotoxic", "Other")

# Build data
data <- data_orig %>% 
  filter(syndrome %in% syndromes_do)

# Build OBIS stats
obis <- obis_orig %>% 
  # Count OBIS observations by EEZ
  count(syndrome, eez_id, eez) %>% 
  # Remove observations outside EEZ waters
  filter(!is.na(eez_id)) %>% 
  mutate(syndrome=recode(syndrome, "Cyanotoxin"="Cyanotoxins"))

# Spatialize OBIS stats
obis_sf <- obis %>% 
  left_join(eezs %>% select(eez_id, geometry)) %>% 
  sf::st_as_sf()


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=long_dd, y=lat_dd, color=syndrome)) +
  facet_wrap(~syndrome, ncol=2) +
  # Land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # OBIS observations
  geom_sf(data=obis_sf, aes(fill=syndrome), inherit.aes=F, color=NA) +
  # HAEDAT observations
  # geom_point() +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(ylim=c(-60, 85)) +
  # Theme
  theme_bw() + my_theme
#g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_hab_risk_map.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




