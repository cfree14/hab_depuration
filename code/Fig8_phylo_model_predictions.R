
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

# Load data
load(file=file.path(outdir, "phylogenetic_regression.Rdata"))
tree_orig <- tree
data_orig <- data

# Prep data
################################################################################

# Orders that aren't in data
pred_orders <- sort(unique(preds_all_full$order))
data_orders <- sort(unique(data_orig$order))
pred_orders[!pred_orders %in% data_orders]

# Orders
order_hyper_p <- ranef(fit3)$order[,,"Intercept"] %>% 
  as.data.frame() %>% 
  setNames(c("rate_d", "rate_d_sd", "rate_d_lo", "rate_d_hi")) %>% 
  mutate(order=rownames(.)) %>% 
  mutate(rate_d=exp(rate_d)) %>% 
  arrange(rate_d)
orders1 <- c(order_hyper_p$order[1:2], "Arcida")
orders2 <- c(order_hyper_p$order[3:7], "Limida")

# Reduce to just lab
data_all <- preds_all_full %>% 
  # Just lab
  filter(study_type=="field") %>% 
  # Mark if data available
  mutate(data_yn=ifelse(sci_name %in% data_orig$sci_name, "yes", "no")) 
data <- data_all %>% 
  # Suppress Tridacna gigas (Lmax) and Crassostrea rhizophorae (K) b/c rates are ridiculous
  filter(!sci_name %in% c("Tridacna gigas", "Crassostrea rhizophorae")) 

# Count species without predictions
data %>% 
  filter(data_yn=="no") %>% 
  pull(sci_name) %>% 
  n_distinct()

# Prep data
data1 <- data %>% 
  filter(order %in% orders1) %>% 
  mutate(order=factor(order, orders1))

# Prep data
data2 <- data %>% 
  filter(order %in% orders2) %>% 
  mutate(order=factor(order, orders2),
         order=forcats::fct_recode(order, 
                                  "M" ="Myida",
                                  "L"="Limida")) 

# Xlim
xmin <- min(data$rate_d_lo) # /5 if plotting label
xmax <- max(data$rate_d_hi)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.text.y = element_text(size=7),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot one
g1 <- ggplot(data1, aes(x=rate_d, 
                  y=tidytext::reorder_within(comm_name, rate_d, order),
                  color=data_yn)) +
  # Grid
  facet_grid(order~., scales="free_y", space="free_y") +
  # Data
  geom_segment(mapping=aes(x=rate_d_lo, xend=rate_d_hi), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Value
  # geom_text(mapping=aes(x=xmin, 
  #                       y=tidytext::reorder_within(sci_name, rate_d, order),
  #                       label=round(rate_d, 3) %>% format(., nsmall=3),
  #                       ), hjust=0, color="black", size=1.8) +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(trans="log10", 
                     lim=c(xmin, xmax),
                     breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                     labels=c("0.0001", "0.001", "0.01", "0.1", "1", "10")) +
  # Legend
  scale_color_manual(name="Rate type", values=c("forestgreen", "darkorange")) +
  # Theme
  theme_bw() + my_theme
g1

# Plot two
g2 <- ggplot(data2, aes(x=rate_d, 
                        y=tidytext::reorder_within(comm_name, rate_d, order),
                        color=data_yn)) +
  # Grid
  facet_grid(order~., scales="free_y", space="free_y") +
  # Data
  geom_segment(mapping=aes(x=rate_d_lo, xend=rate_d_hi), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Value
  # geom_text(mapping=aes(x=xmin, 
  #                       y=tidytext::reorder_within(sci_name, rate_d, order),
  #                       label=round(rate_d, 3) %>% format(., nsmall=3),
  # ), hjust=0, color="black", size=1.8) +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(trans="log10", 
                     lim=c(xmin, xmax),
                     breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                     labels=c("0.0001", "0.001", "0.01", "0.1", "1", "10")) +
  # Legend
  scale_color_manual(name="Rate type", values=c("forestgreen", "darkorange")) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig8_phylo_model_predictions.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

