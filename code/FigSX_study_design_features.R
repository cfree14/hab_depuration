
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
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
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

# Number of species evaluated
nspp <- data_orig %>% 
  group_by(id) %>% 
  summarize(nspp=n_distinct(comm_name)) %>% 
  ungroup()

# Number of tissues evaluated
ntissues <- data_orig %>% 
  group_by(id) %>% 
  summarize(ntissues=n_distinct(tissue)) %>% 
  ungroup()

# Types of experiments conducted
stats_exp <- data_orig %>% 
  # Remove none
  filter(exp_type!="none") %>% 
  # Count by experiment type
  group_by(exp_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup()

# Field vs. lab
stats_type_class <- data_orig %>% 
  group_by(class, study_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  group_by(class) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()
stats_type_tot <- data_orig %>% 
  group_by(study_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(class="Overall")
stats_type <- bind_rows(stats_type_class, stats_type_tot) %>% 
  mutate(study_type=stringr::str_to_sentence(study_type),
         study_type=factor(study_type,
                           levels=c("Lab", "Field", "Field (non-toxic site)")))
stats_type_order <- stats_type %>% 
  group_by(class) %>% 
  summarize(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n))

# Feeding
stats_feed <- data_orig %>% 
  group_by(study_type, feed_scenario) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  group_by(study_type) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Reduce to lab studies
  filter(study_type=="lab") %>% 
  mutate(study_type=stringr::str_to_sentence(study_type)) %>% 
  # Recode
  mutate(feed_scenario=recode(feed_scenario, 
                              "unsure-Chinese"="Unknown"),
         feed_scenario=stringr::str_to_sentence(feed_scenario),
         feed_scenario=factor(feed_scenario, 
                              levels=c("Starved", "Fed clean", "Ambient seawater", "Unknown")))
  


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.4, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Nuber of species evaluated
g1 <- ggplot(nspp, aes(x=nspp)) +
  geom_histogram(binwidth = 1) +
  # Labels
  labs(x="Number of species", y="Number of papers", tag="A") +
  scale_x_continuous(breaks=1:10) +
  # Theme
  theme_bw() + base_theme 
g1

# Number of tissues
g2 <- ggplot(ntissues, aes(x=ntissues)) +
  geom_histogram(binwidth = 1) +
  # Labels
  labs(x="Number of tissues", y="Number of papers", tag="B") +
  scale_x_continuous(breaks=1:10) +
  # Theme
  theme_bw() + base_theme
g2

# Field vs. lab
g3 <- ggplot(stats_type, aes(y=factor(class, stats_type_order$class), 
                             x=prop, 
                             fill=study_type)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  geom_text(data=stats_type_order, mapping=aes(label=paste0("n=", n),
                                               y=factor(class, class)), 
            x=1.02, hjust=0, inherit.aes = F, size=1.9, color="grey50") +
  # Labels
  labs(x="Percent of papers", y="", tag="C") +
  scale_x_continuous(labels=scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.15)) +
  # Legend
  scale_fill_ordinal(name="Study type") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y = element_blank())
g3

# Feeding scenario
g4 <- ggplot(stats_feed, aes(x=study_type, y=prop, fill=feed_scenario)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(y="Percent of papers", x="Study type", tag="D") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="Feeding scenario") +
  # Theme
  theme_bw() + base_theme
g4

# Experiment type
g5 <- ggplot(stats_exp, aes(y=reorder(exp_type, desc(n)), x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="Experiment type", tag="E") +
  # Theme
  theme_bw() + base_theme
g5

# Merge
layout_matrix <- matrix(data=c(1,2,3,3,3,
                               4,4,5,5,5), byrow=T, ncol=5)

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, 
                             layout_matrix=layout_matrix, heights=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_study_design_features.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





