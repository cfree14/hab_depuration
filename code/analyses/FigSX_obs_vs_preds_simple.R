
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
# library(rotl)
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
tree_orig <- readRDS(file=file.path(outdir, "bivalve_phylogeny.Rds"))
pred_data_orig <- readRDS(file=file.path(outdir, "bivalve_prediction_data.Rds"))
data_orig <- readRDS(file=file.path(outdir, "bivalve_pst_depuration_rates.Rds"))


# Read models
load(file.path(outdir, "phylogenetic_models.Rdata"))

# Get obs
data <- fit3$data




# Predict depuration rates
################################################################################

# Make predictions
# Rows=random draws, Columns=species predicted to
preds_log <- brms::posterior_epred(
  fit3,
  newdata    =data,
  allow_new_levels = T, 
  re_formula = NULL  # NULL= include all random effects (NA = include no group-level effects)
)
dim(preds_log)

# Exponentiate predictions
preds <- exp(preds_log)

# Compute posterior stats
preds_stats <- as_tibble(t(apply(preds, 2, function(x) {
  c(rate_d = mean(x), 
    rate_d_lo = quantile(x, 0.025), 
    rate_d_hi = quantile(x, 0.975))
}))) %>% 
  setNames(c("rate_d", "rate_d_lo", "rate_d_hi"))


# Merge pred stats with data
preds_full <- bind_cols(obs, preds_stats) %>% 
  # Exponentiate observed rate
  mutate(rate_d_obs=exp(rate_d_log)) %>% 
  # Format study type
  mutate(study_type=stringr::str_to_sentence(study_type),
         study_type=recode(study_type, "Lab"="Laboratory")) %>% 
  # Format tissue type
  mutate(tissue=stringr::str_to_sentence(tissue))


# Plot observations vs. predictions
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Limits
x1 <- min(c(preds_full$rate_d, preds_full$rate_d_obs))
x2 <- max(c(preds_full$rate_d, preds_full$rate_d_obs))

# Plot observations vs. predictions
g <- ggplot(preds_full, aes(x=rate_d_obs, y=rate_d, color=tissue, shape=study_type)) +
  geom_errorbar(aes(ymin=rate_d_lo, ymax=rate_d_hi), alpha=0.3) +
  geom_point(size=1.5) +
  # Labels
  labs(x="Observed\ndepuration rate (1/day)", y="Predicted\ndepuration rate (1/day)") +
  # Reference line
  geom_abline(slope=1) +
  # Scales
  scale_x_continuous(trans="log10", lim=c(x1, x2)) +
  scale_y_continuous(trans="log10", lim=c(x1, x2)) +
  # Legend
  scale_color_discrete(name="Tissue") +
  scale_shape_manual(name="Study type", values=c(1,3,16)) +
  coord_fixed() +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_obs_vs_preds_simple.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")
