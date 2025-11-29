
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rotl)
library(ape)
library(phytools)
library(picante)
library(tidyverse)
library(caper)
library(phyr)
library(ggtree)

# Directories
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- read.csv(file.path(tabledir, "TableSX_phylo_model_comparison.csv"))

# Load data
load(file=file.path(outdir, "phylogenetic_regression.Rdata"))


# Build data
################################################################################

# Final model
fit <- fit3

# Study type
study_type_orig <- brms::conditional_effects(fit, effects = "study_type")
study_type <- study_type_orig$study_type %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# Tissue
tissue_orig <- brms::conditional_effects(fit, effects = "tissue")
tissue <- tissue_orig$tissue %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# Temp
temp_orig <- brms::conditional_effects(fit, effects = "temp_c")
temp <- temp_orig$temp_c %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# Temp
lmax_orig <- brms::conditional_effects(fit, effects = "lmax_cm")
lmax <- lmax_orig$lmax_c %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# K
k_orig <- brms::conditional_effects(fit, effects = "k")
k <- k_orig$k %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot study type
g1 <- ggplot(study_type, aes(y=reorder(study_type, desc(rate_d)), 
                         x=rate_d)) +
  geom_point() +
  geom_segment(aes(x=rate_d_lo, xend=rate_d_hi)) +
  # Labels
  labs(x="Depuration rate (1/d)", y="", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Plot tissue
g2 <- ggplot(tissue, aes(y=reorder(tissue, desc(rate_d)), 
                         x=rate_d)) +
  geom_point() +
  geom_segment(aes(x=rate_d_lo, xend=rate_d_hi)) +
  # Labels
  labs(x="Depuration rate (1/d)", y="", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Plot temp
g3 <- ggplot(temp, aes(x = temp_c, y = rate_d)) +
  geom_ribbon(aes(ymin = rate_d_lo, ymax = rate_d_hi), alpha = 0.2) +
  geom_line() +
  # Labels
  labs(x = "Temperature (°C)", y = "Depuration rate (1/day)", tag="C") +
  # Theme
  theme_bw() + my_theme
g3

# Plot lmax
g4 <- ggplot(lmax, aes(x = lmax_cm, y = rate_d)) +
  geom_ribbon(aes(ymin = rate_d_lo, ymax = rate_d_hi), alpha = 0.2) +
  geom_line() +
  # Labels
  labs(x = "Max length (cm)", y = "Depuration rate (1/day)", tag="D") +
  # Theme
  theme_bw() + my_theme
g4

# Plot K
g5 <- ggplot(k, aes(x = k, y = rate_d)) +
  geom_ribbon(aes(ymin = rate_d_lo, ymax = rate_d_hi), alpha = 0.2) +
  geom_line() +
  # Labels
  labs(x = "Growth rate (1/yr)", y = "Depuration rate (1/day)", tag="E") +
  # Theme
  theme_bw() + my_theme
g5

# Merge
layout_matrix <- matrix(data=c(1,3,
                               2,4,
                               2,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "FigS11_conditional_effects.png"), 
       width=6.5, height=6, units="in", dpi=600)



