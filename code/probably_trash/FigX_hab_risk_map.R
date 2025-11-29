
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

# Perc rank
percentile_rank <- function(x) {
  ecdf(x)(x)
}
rescale_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Build OBIS stats
obis <- obis_orig %>% 
  # Count OBIS observations by EEZ
  count(syndrome, eez_id, eez, territory) %>% 
  # Remove observations outside EEZ waters
  filter(!is.na(eez_id)) %>% 
  mutate(syndrome=recode(syndrome, "Cyanotoxin"="Cyanotoxins")) %>% 
  # Group by syndrome and calculate stat
  group_by(syndrome) %>% 
  mutate(n_perc=percentile_rank(n),
         n_scaled=rescale_01(n)) %>% 
  ungroup()

ggplot(obis, aes(x=n_scaled, y=n_perc)) +
  facet_wrap(~syndrome, ncol=4) +
  geom_point()

# Syndrome stats
stats <- obis %>% 
  group_by(syndrome) %>% 
  summarize(n_terrs=n_distinct(territory)) %>% 
  ungroup() %>% 
  mutate(label=paste(n_terrs, "nations")) %>% 
  arrange(desc(n_terrs))
syndrome_order <- c(stats$syndrome[stats$syndrome!="Other"], "Other")
stats_ordered <- stats %>% 
  mutate(syndrome=factor(syndrome, levels=syndrome_order))

# Spatialize OBIS stats
obis_sf <- obis %>% 
  left_join(eezs %>% select(eez_id, geometry)) %>% 
  sf::st_as_sf() %>% 
  # Order synrome
  mutate(syndrome=factor(syndrome, levels=syndrome_order))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8), # angle=45, hjust=0, vjust=0.5
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot() +
  facet_wrap(~syndrome, ncol=2) +
  # Land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.1, inherit.aes = F) +
  # OBIS observations
  geom_sf(data=obis_sf, aes(fill=n_perc), inherit.aes=F, color=NA) +
  # HAEDAT observations
  # geom_point() +
  # Plot EEZ label
  geom_text(data=stats_ordered, mapping=aes(label=label),
            x=-165, y=-50, size=2.4, hjust=0, color="grey10") +
  # Legend
  #scale_alpha_continuous(name="Number of\nOBIS observations", trans="log10") +
  scale_fill_gradientn(name="Relative\noccurence", #trans="log10",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(ylim=c(-60, 85)) +
  # Theme
  theme_bw() + my_theme
#g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_hab_risk_map.png"), 
       width=6.5, height=6.8, units="in", dpi=600)




