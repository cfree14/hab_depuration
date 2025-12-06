
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


# Prep data
################################################################################

# Tip labels
data_tip_labels <- data$tip.label

# Reduce tree to relevant species
tree <- ape::drop.tip(tree_orig, setdiff(tree_orig$tip.label, data_tip_labels))

# Order by rate
# preds_spp_order <- preds_full %>% 
#   filter(study_type=="field") %>% 
#   arrange(desc(rate_d))

# Format tree for plotting
tree1_plot <- tree
tree1_plot$tip.label <- tree1_plot$tip.label %>% gsub("_ott[0-9]+", "", .) %>% gsub("_", " ", .)

# Get tip order
p <- ggtree(tree1_plot)
tip_order <- p$data[p$data$isTip, ] |>
  dplyr::arrange(y) |>         # top to bottom (small y to large y)
  dplyr::pull(label)
tip_order

# Format preds
preds <- preds_full %>% 
  mutate(species_label=paste0(sci_name, " (", comm_name, ")"),
         study_type=stringr::str_to_sentence(study_type))



# Plot data
################################################################################

# Plot tree
g1 <- ggtree(tree, lwd=0.3) + 
  # Label tips
  # geom_tiplab(fontface="italic") +
  # Limits
  # xlim(c(0, 2)) +
  # Theme
  theme_tree() +
  theme(plot.margin = margin(b=27.2, t=17.1)) # manipulate to align with top and bottom of boxplot; 5.1 if no top legend
g1

# Plot rates
g2 <- ggplot(preds, aes(x=rate_d, 
                       y=factor(sci_name, tip_order), # preds_spp_order$sci_name
                       color=study_type)) +
  # Facet
  # facet_grid(order~., space="free_y", scales="free_y") +
  # Plot data
  geom_segment(mapping=aes(x=rate_d_lo, xend=rate_d_hi), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  scale_x_continuous(trans="log10",
                     breaks=c(0.001, 0.01, 0.1, 1)) +
  scale_y_discrete(sec.axis = dup_axis()) +
  # tidytext::scale_y_reordered() +
  # Legend
  scale_color_manual(name="Rate type", values=c( "#35B779", "#440154")) +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        # Texts size
        axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        legend.margin = margin(t=-5, b=-12),
        # Horizontal gridlines only
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        # Move axis to left side
        axis.text.y.right=element_text(face="italic"),
        axis.title.y.left = element_blank(),
        axis.text.y.left  = element_blank(),
        axis.ticks.y.left = element_blank(),
        # Supress y-axis line
        panel.border = element_blank(),
        axis.line.x=element_line(linewidth=0.2),
        # Panels
        panel.spacing=unit(0, "lines"))
g2

# Merge plot
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_phylo_model_fit.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

