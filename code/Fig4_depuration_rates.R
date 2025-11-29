
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/round1/raw"
outdir <- "data/lit_review/round1/processed"
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
  # remove one crazy outlier that should really be an increase
  filter(!is.na(hlife_d) & rate_d<0 & rate_d < -0.0001) %>% 
  # Add percent daily loss
  mutate(perc_loss_d=(1-exp(rate_d)))

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
                   axis.title.y=element_blank(),
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


# Decay rate
g1 <- ggplot(data, aes(x=abs(rate_d), y=factor(genus, stats$genus))) +
  geom_boxplot() + 
  # Labels
  labs(x="Decay constant, k (1/day)", # x=expression("Decay constant, k ("*day^{-1}*")"), 
       y="Genus", tag="A") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + my_theme
g1

# Loss per day
g2 <- ggplot(data, aes(x=perc_loss_d, y=factor(genus, stats$genus))) +
  geom_boxplot() + 
  # Labels
  labs(x="Daily loss (%)", y="Genus", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
g2

# Half life
g3 <- ggplot(data, aes(x=hlife_d, y=factor(genus, stats$genus))) +
  geom_boxplot() + 
  # Labels
  labs(x="Half life (day)", y="Genus", tag="C") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1, 1, 10, 100, 1000),
                     labels=c("0.1", "1" , "10" , "100", "1000")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_depuration_rates.png"), 
       width=6.5, height=4.0, units="in", dpi=600)




# # Plot data
# ################################################################################
# 
# # Setup theme
# my_theme <-  theme(axis.text=element_text(size=7),
#                    axis.title=element_text(size=8),
#                    axis.title.y=element_text(),
#                    legend.text=element_text(size=7),
#                    legend.title=element_text(size=8),
#                    plot.tag=element_text(size=9),
#                    # Gridlines
#                    panel.grid.major.x = element_blank(), 
#                    panel.grid.minor.x = element_blank(),
#                    panel.background = element_blank(), 
#                    axis.line = element_line(colour = "black"),
#                    # Legend
#                    legend.key = element_rect(fill = NA, color=NA),
#                    legend.background = element_rect(fill=alpha('blue', 0)))
# 
# 
# # Decay rate
# g1 <- ggplot(data, aes(x=rate_d, y=factor(genus, stats$genus))) +
#   geom_boxplot() + 
#   geom_point(mapping=aes(fill=syndrome), pch=21) +
#   # Labels
#   labs(x=expression("Decay constant, k ("*day^{-1}*")"), 
#        y="Genus", tag="A") +
#   scale_x_continuous(trans="log10", 
#                      breaks=c(0.001, 0.01, 0.1, 1, 10, 100),
#                      labels=c("0.001", "0.01", "0.1", "1", "10", "100")) +
#   # Theme
#   theme_bw() + my_theme +
#   theme(legend.position="none")
# g1
# 
# # Loss per day
# g2 <- ggplot(data, aes(x=perc_loss_d, y=factor(genus, stats$genus))) +
#   geom_boxplot() + 
#   geom_point(mapping=aes(fill=syndrome), pch=21) +
#   # Labels
#   labs(x="Daily loss (%)", y="Genus", tag="B") +
#   scale_x_continuous(labels=scales::percent_format()) +
#   # Theme
#   theme_bw() + my_theme +
#   theme(legend.position="none",
#         axis.text.y=element_blank(),
#         axis.title.y=element_blank())
# g2
# 
# # Half life
# g3 <- ggplot(data, aes(x=hlife_d, y=factor(genus, stats$genus))) +
#   geom_boxplot() + 
#   geom_point(mapping=aes(fill=syndrome), pch=21) +
#   # Labels
#   labs(x="Half life (day)", y="Genus", tag="C") +
#   scale_x_continuous(trans="log10", 
#                      breaks=c(0.1,1,10,100),
#                      labels=c("0.1", "1" , "10" , "100")) +
#   # Legend
#   scale_fill_discrete(name="Syndrome") +
#   # Theme
#   theme_bw() + my_theme +
#   theme(axis.text.y=element_blank(),
#         axis.title.y=element_blank())
# g3
# 
# # Merge
# g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.35, 0.25, 0.4))
# 
# # Export
# ggsave(g, filename=file.path(plotdir, "Fig4_depuration_rates_points.png"), 
#        width=6.5, height=4.0, units="in", dpi=600)
# 
