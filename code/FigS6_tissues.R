
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))

# Format data
data <- data_orig %>% 
  # Recode some common name
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Teleostei"="Finfish",
                      "Ascidiacea" = "Sea squirts",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Cephalopods",
                      "Dinophyceae" = "Phytoplankton",
                      "Gastropoda"="Gastropods",
                      "Malacostraca"="Crustaceans",
                      "Thecostraca"="Crustaceans",
                      "Mammalia" = "Mammals",
                      "Copepoda"="Copepods",
                      "Maxillopoda"="Zooplankton"))

# Plot data
################################################################################

# Number of papers by class
class_n <- data %>% 
  group_by(class) %>% 
  summarize(n_class=n_distinct(paper_id)) %>% 
  ungroup()

# Tissue stats
tissues <- data %>% 
  # Summarize
  group_by(class, tissue) %>% 
  summarize(n=n_distinct(paper_id)) %>% 
  ungroup() %>% 
  # Add number in class
  left_join(class_n) %>% 
  mutate(p=n/n_class) %>% 
  # Build class label
  mutate(label=paste0(class, " (n=", n_class, ")"))

# Stats for papers reporting 1 tissue
tissue1_stats <- data %>% 
  # Count tissues
  group_by(paper_id) %>% 
  mutate(ntissues=n_distinct(tissue)) %>% 
  ungroup() %>% 
  # Reduce to paper with 1 tissue
  filter(ntissues==1) %>% 
  # Summarize
  count(class, tissue) %>% 
  group_by(class) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()


# Build data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot tissue stats
g1 <- ggplot(tissues, aes(x=p, 
                    y=tidytext::reorder_within(tissue, desc(p), label))) +
  facet_grid(label~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of papers", y="", tag="A") +
  # Axes
  scale_x_continuous(labels=scales::percent_format()) +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text.y = element_text(angle = 0))
g1

# 1 tissue papers
g2 <- ggplot(tissue1_stats, aes(y=class, x=prop, fill=tissue)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of papers", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Tissue", values=RColorBrewer::brewer.pal(n_distinct(tissue1_stats$tissue), "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.title=element_blank(),
        legend.position="left", 
        legend.key.size = unit(0.2, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.8, 0.2))

# Export
ggsave(g, filename=file.path(plotdir, "FigS6_tissues_sampled.png"), 
       width=6.5, height=7, units="in", dpi=600)

