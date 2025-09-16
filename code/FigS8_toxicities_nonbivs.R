
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/toxicities/raw"
outdir <- "data/toxicities/processed"
plotdir <- "figures"

# Read data
data <- readRDS(file=file.path(outdir, "toxicity_data.Rds"))


# Build data
################################################################################

# Mg/kg = ug/g = ppm
thresh <- tibble(syndrome=c("Amnesic",
                            "Diarrhetic",
                            "Paralytic",
                            "Neurotoxic"),
                 action_level_mg_kg=c(20,
                                      0.16,
                                      0.8, 
                                      0.8))

# Build stats
stats <- data %>% 
  # Filter
  filter(!is.na(toxicity_mgkg_use) & class!="Bivalvia") %>% 
  # Identify maximum
  arrange(syndrome, comm_name, desc(toxicity_mgkg_use)) %>% 
  group_by(syndrome, comm_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Add thresh
  left_join(thresh) %>% 
  filter(toxicity_mgkg_use >= action_level_mg_kg) %>% 
  # Format class
  mutate(class=recode(class, 
                      "Actinopterygii"="Bony fish",
                      "Ascidiacea" = "Sea squirts",    
                      "Asteroidea" = "Starfish",    
                      "Echinoidea" = "Sea urchins",    
                      "Elasmobranchii"="Sharks and rays",
                      "Gastropoda"="Snails",    
                      "Malacostraca"="Crabs, lobsters, shrimps",  
                      "Maxillopoda"="Barnacles",   
                      "Merostomata"="Horseshoe crabs",    
                      "Thecostraca"="Barnacles"))

# 
stats1 <- stats %>% 
  filter(syndrome!="Paralytic")
stats2 <- stats %>% 
  filter(syndrome=="Paralytic" & class=="Snails")
stats3 <- stats %>% 
  filter(syndrome=="Paralytic" & class!="Snails")

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(stats3, aes(x=tidytext::reorder_within(comm_name, desc(toxicity_mgkg_use), syndrome), 
                  y=toxicity_mgkg_use, 
                  shape=toxicity_mgkg_use_type, 
                  color=class)) +
  # facet
  facet_grid(.~syndrome, scales="free_x", space="free_x") +
  # Data
  geom_segment(mapping=aes(y=0.1, yend=toxicity_mgkg_use), linewidth = 0.2) +
  geom_point(stat="identity") +
  # Ref line
  geom_hline(data=thresh, mapping=aes(yintercept=action_level_mg_kg), color="grey30", linewidth=0.6) + # , linetype="dashed"
  # Labels
  labs(y="Toxicity (mg/kg)", x="") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(trans="log10", 
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 10^6, 10^7),
                     labels=c("0.01", "0.1", "1", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million")) +
  # Legends
  scale_color_discrete(name="Taxa group") +
  scale_shape_manual(name="Unit type", values=c(21, 16)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.9, 0.7))
g1

# Export
ggsave(g, filename=file.path(plotdir, "FigS8_toxicities_for_non_bivalves.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



