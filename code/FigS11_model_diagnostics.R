
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

# Conditional residuals on log-scale
resid_df <- tidybayes::add_residual_draws(fit, 
                                          newdata = data, 
                                          allow_new_levels = TRUE,
                                          ndraws = 10)

# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
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


# Residuals
g1 <- ggplot(resid_df, aes(x=.residual)) +
  geom_histogram(fill="grey80") + 
  geom_vline(xintercept=0) +
  # Label
  labs(x="Residual", y="Number of draws", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Density overlay
g2 <- bayesplot::pp_check(fit) +
  # Labels
  labs(x="Log-transformed depuration rate", y="Density", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.2, 0.8))
g2

# Scatter plot
g3 <- bayesplot::pp_check(fit, type = "scatter") +
  # Facet
  facet_wrap(~rep_id, ncol=5) +
  # Labels
  labs(tag='C') +
  scale_x_continuous(breaks=seq(-10,2,2)) +
  scale_y_continuous(breaks=seq(-10,2,2)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
layout_matrix <- matrix(data=c(1,2,
                               3,3, 
                               3,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "FigS11_model_diagnostics.png"), 
       width=6.5, height=6, units="in", dpi=600)



