
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
  


# Build data
################################################################################

# Prepare data
data <- data_orig %>% 
  # Recode some common name
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Cephalopods",
                      "Gastropoda"="Gastropods",
                      "Malacostraca"="Crustaceans",
                      "Maxillopoda"="Zooplankton")) %>% 
  # Filter
  filter(!is.na(hlife_d))

# Stats
stats <- data %>% 
  group_by(genus) %>% 
  summarise(hlife_d=median(hlife_d)) %>% 
  ungroup() %>% 
  arrange(desc(hlife_d))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Half life
g1 <- ggplot(data, aes(x=hlife_d, y=factor(genus, stats$genus))) +
  geom_boxplot() + 
  # Labels
  labs(x="Half life (day)", y="Genus", tag="A") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1,1,10,100),
                     labels=c("0.1", "1" , "10" , "100")) +
  # Theme
  theme_bw() + my_theme 
g1

# Decay rate
g2 <- ggplot(data, aes(x=rate_d, y=factor(genus, stats$genus))) +
  geom_boxplot() + 
  # Labels
  labs(x=expression("Decay constant, k ("*day^{-1}*")"), 
       y="Genus", tag="B") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_depuration_rates.png"), 
       width=6.5, height=4.0, units="in", dpi=600)



