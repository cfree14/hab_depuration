
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
data_orig <- readRDS(file.path(outdir, "database.Rds")) %>% 
  # Recode some common name
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name))

# Plot data
################################################################################

# Tissue stats
tissues <- data_orig%>% 
  # Summarize
  group_by(class, tissue) %>% 
  summarize(n=n_distinct(id)) %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Cephalopods",
                      "Gastropoda"="Gastropods",
                      "Malacostraca"="Crustaceans",
                      "Maxillopoda"="Zooplankton"))


# Build data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot tissue stats
g <- ggplot(tissues, aes(x=n, 
                    y=tidytext::reorder_within(tissue, n, class))) +
  facet_grid(class~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text.y = element_text(angle = 0))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_tissues_sampled.png"), 
       width=6.5, height=5.5, units="in", dpi=600)
