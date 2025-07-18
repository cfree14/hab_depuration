
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/raw"
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))
  
# Format data
data <- data_orig %>% 
  # Recode some common namae
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name))


# Stats for paper
################################################################################

n_distinct(data$sci_name)
n_distinct(data$genus)
n_distinct(data$family)
n_distinct(data$order)
n_distinct(data$class)

# Number of species by class
npapers_tot <- n_distinct(data$id)
data %>% 
  group_by(class) %>% 
  summarize(npapers=n_distinct(id),
            p_papers=npapers/npapers_tot)
n_distinct(data$id[data$class!="Actinopterygii"])

# Number of papers by species
spp_stats <- data %>% 
  group_by(comm_name) %>% 
  summarize(npapers=n_distinct(id),
            p_papers=npapers/npapers_tot) %>% 
  arrange(desc(npapers))

# Number of papers by biotoxin
data %>% 
  group_by(syndrome) %>% 
  summarize(npapers=n_distinct(id),
            p_papers=npapers/npapers_tot) %>% 
  arrange(desc(npapers))

# Build data
################################################################################

# Classes
sort(unique(data$class))

# Summarize
stats <- data %>% 
  # Simplify
  select(id, class, comm_name, syndrome) %>% 
  unique() %>% 
  # Summarize number of species
  group_by(class, comm_name, syndrome) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Cephalopods",
                      "Gastropoda"="Gastropods",
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
syndrome_order <- stats %>% 
  filter(syndrome!="Other") %>% 
  group_by(syndrome) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(syndrome) %>% c(., "Other")

# Order data
stats_ordered <- stats %>% 
  mutate(class=factor(class, class_order),
         comm_name=factor(comm_name, species_order),
         syndrome=factor(syndrome, syndrome_order))


# Build data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.y=element_blank(),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=7),
                    # Gridlines
                    panel.grid.major.x = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.4, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot bar chart
g1 <- ggplot(stats_ordered, aes(x=n, fill=syndrome, y=comm_name))+
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="black", lwd=0.2, position=position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Number of papers\n\n\n", y="") +
  # Legend
  scale_fill_ordinal(name="Toxin syndrome") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.65,0.35),
        strip.text = element_blank())
g1

# Plot raster
g2 <- ggplot(stats_ordered, aes(x=syndrome, 
                      fill=n, 
                      y=comm_name)) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_tile() +
  # Labels
  labs(x="Toxin syndrome", y="") +
  # Legend
  scale_fill_gradientn(name="Number of papers", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_text(angle = 0))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.45, 0.55))

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_species_sample_size.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# spp <- sort(unique(data_spp$sci_name))
# df1 <- freeR::fishbase(species=spp, dataset = "species")
# df2 <- freeR::fishbase(species=spp, dataset = "ecology", cleaned=T)




