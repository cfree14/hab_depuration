
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
data_spp <- read.csv(file=file.path(outdir, "data_for_proposal.csv"), as.is=T)


# Build data
################################################################################

# Classes
sort(unique(data_spp$class))

# Summarize
stats <- data_spp %>% 
  # Remove unknown taxa
  filter(!is.na(order)) %>% 
  # Format condition
  mutate(condition=ifelse(!condition %in% c("Amnesic", "Paralytic", "Diarrhetic"), "Other", condition)) %>%
  mutate(condition=factor(condition, levels=c("Amnesic", "Paralytic", "Diarrhetic", "Other"))) %>%
  # Summarize number of species
  group_by(class, comm_name, condition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Octopus",
                      "Gastropoda"="Abalone",
                      "Malacostraca"="Crustaceans",
                      "Maxillopoda"="Zooplankton"))

# Class order
class_order <- stats %>% 
  group_by(class) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(class) %>% rev()

# Species order
species_order <- stats %>% 
  mutate(class=factor(class, levels=class_order)) %>% 
  group_by(class, comm_name) %>% 
  summarize(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(class, desc(n)) %>% 
  pull(comm_name)

# Conditon order
condition_order <- stats %>% 
  group_by(condition) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(condition)

# Order data
stats_ordered <- stats %>% 
  mutate(class=factor(class, class_order),
         comm_name=factor(comm_name, species_order),
         condition=factor(condition, condition_order))

# Plot data
g1 <- ggplot(stats_ordered, aes(x=condition, 
                      fill=n, 
                      y=comm_name)) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_tile() +
  # Labels
  labs(x="Toxin type", y="") +
  # Legend
  scale_fill_gradientn(name="Number of studies", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        legend.key.size = unit(0.2, "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_text(angle = 0))
g1


g2 <- ggplot(stats_ordered, aes(x=n, fill=condition, y=factor(comm_name, levels=stats_spp_order$comm_name)))+
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of studies", y="") +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        axis.text.y=element_blank(),
        legend.key.size = unit(0.2, "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_text(angle = 0))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

