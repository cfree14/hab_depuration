
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
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))


# Build bivalve phylogenetic tree
################################################################################

# 1. Search for bivalve OTT id
biv_search <- rotl::tnrs_match_names("Bivalvia", context_name = "Animals")
biv_ott <- biv_search$ott_id[1]

# 2. Get the subtree for Bivalvia
bivalvia_tree <- rotl::tol_subtree(ott_id = biv_ott) %>% 
  ape::compute.brlen(., method = "Grafen")

# 3. Plot bivalve tree
plot(bivalvia_tree, cex = 0.1, no.margin = TRUE)


# Build dataset
################################################################################

# Build dataset of interest
data <- data_orig %>% 
  # Subset to species of interest
  filter(class=="Bivalvia" & syndrome=="Paralytic" & subtoxin=="Total" ) %>% # & study_type=="lab"
  # Identified to species
  filter(!grepl("spp.", sci_name)) %>% 
  # Soft tissues
  # filter(tissue=="soft tissue") %>% 
  # Negative depuration rates
  filter(!is.na(rate_d) & rate_d < 0)

# Species in dataset
data_spp <- sort(unique(data$sci_name))

# ROTL info on species in dataset
data_spp_rotl_info <- rotl::tnrs_match_names(data_spp) %>% 
  mutate(search_string=stringr::str_to_sentence(search_string))

# Add ROTL id
data1 <- data %>% 
  # Add OTT id
  left_join(data_spp_rotl_info %>% select(search_string, ott_id), by=c("sci_name"="search_string")) %>% 
  # Build tip label
  mutate(tip.label=paste0(gsub(" ", "_", sci_name), "_ott", ott_id))


# Finalize datasets
################################################################################

# Identify intersecting species
common_spp <- intersect(data1$tip.label, bivalvia_tree$tip.label)

# Reduce tree to species with data
tree1 <- ape::drop.tip(bivalvia_tree, setdiff(bivalvia_tree$tip.label, common_spp))

# Compute branch lengths on tree
# tree2 <- ape::compute.brlen(tree1, method = "Grafen")

# Plot tree of specues with data
plot(tree1, cex = 0.1, no.margin = TRUE)

# Reduce data to sepcies in tree
data2 <- data1 %>% 
  # Reduce to species in tree
  filter(tip.label %in% common_spp)

# Named vector of traits for analysis
rates <- data2 %>% 
  # Extract
  pull(rate_d) %>% 
  # Transform
  abs() %>% log() %>% 
  # Name
  setNames(data2$tip.label)


# Perform analysis
################################################################################

# Compute Pagel's lambda
# Pagel’s λ – how tree-like the trait is (0 = no phylo signal, 1 = Brownian motion on the tree).
lambda <- phytools::phylosig(tree = tree1, 
                             x = rates,
                             method = "lambda",
                             test = TRUE)
lambda_val <- lambda$lambda

# Compute Blomberg’s K
# Blomberg’s K – compares observed trait variance across the tree to BM expectations (K ~ 1 ~ BM; K > 1 strong signal; K < 1 weak).
K <- phytools::phylosig(tree = tree1, 
                            x = rates,
                            method = "K",
                            test = TRUE)
K
K_val <- K$K

# Build stats text
lam_text <- paste("Pagel’s λ:", round(lambda_val, 3))
k_text <- paste("Blomberg’s K:", round(K_val, 3))
stat_text <- paste0(lam_text, "\n", k_text)

# Visualize data
################################################################################

# Plotting the tree
tree1_plot <- tree1
tree1_plot$tip.label <- tree1$tip.label %>% gsub("_ott[0-9]+", "", .) %>% gsub("_", " ", .)

# Get tip order
p <- ggtree(tree1_plot)
tip_order <- p$data[p$data$isTip, ] |>
  dplyr::arrange(y) |>         # top to bottom (small y to large y)
  dplyr::pull(label)
tip_order

# Plotting data
data2_plot <- data2 %>% 
  # Order species in tree order
  mutate(sci_name=factor(sci_name, levels=tip_order))


# Theme
my_theme <-  theme(axis.text=element_text(size=8),
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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Phylogenetic tree
g1 <- ggtree(tree1_plot, lwd=0.3) + 
  # Print text
  annotate(geom="text", x=0, y=length(tip_order)-1, label=stat_text, hjust=0, size=2.2) +
  # Label tips
  # geom_tiplab(fontface="italic") +
  # Limits
  # xlim(c(0, 2)) +
  # Theme
  theme_tree() +
  theme(plot.margin = margin(b=29.1, t=5.1)) # manipulate to align with top and bottom of boxplot
g1

# Depuration rates
g2 <- ggplot(data2_plot, aes(y=reorder(sci_name, tip.label), 
                             x=abs(rate_d))) +
  geom_boxplot(lwd=0.3) +
  # Labels
  labs(x="PST depuration rate (1/day)", y="") +
  scale_x_continuous(trans="log10") +
  scale_y_discrete(sec.axis = dup_axis()) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y.right=element_text(face="italic"),
        axis.title.y.left = element_blank(),
        axis.text.y.left  = element_blank(),
        axis.ticks.y.left = element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_phylogenetic_analysis.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Assemble life history
################################################################################

# Examine life history
fb_spp <- rfishbase::species(species_list = data_spp, server="sealifebase") # size
# fb_ecol <- rfishbase::ecology(species_list = data_spp, server="sealifebase")
# fb_diet <- rfishbase::diet(species_list = data_spp, server="sealifebase")
# fb_ecosystem <- rfishbase::ecosystem(species_list = data_spp, server="sealifebase")
fb_stocks <- rfishbase::stocks(species_list = data_spp, server="sealifebase") # temperature 

# Format and merge life history
fb_spp1 <- fb_spp %>% 
  select(Species, Length)
fb_stocks1 <- fb_stocks %>% 
  select(Species, TempPreferred)
spp_key <- fb_spp1 %>% 
  left_join(fb_stocks1) %>% 
  rename(species=Species,
         length_mm=Length,
         temp_c=TempPreferred) %>% 
  mutate(length_mm=case_when(species=="Nodipecten subnodosus" ~ 120, # Fig 2: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2109.2010.02652.x
                             species=="Magallana rivularis" ~ 45, # same as M. gigas
                             T ~ length_mm),
         # Set NAs to constant (garbage - workflow development only)
         temp_c=ifelse(is.na(temp_c), 17, temp_c))


# Regression analysis
################################################################################

# Add life history traits to data
data3 <- data2 %>% 
  # Add life history
  left_join(spp_key, by=c("sci_name"="species")) 

# Summarize for caper
# Caper sucks and seems to require the data to be a DATAFRAME and have ROW NAMES
data3_sum <- data3 %>% 
  group_by(sci_name, tip.label, length_mm, temp_c) %>% 
  summarize(rate_d=median(rate_d)) %>% 
  ungroup() %>% 
  # Fill in more missing 
  mutate(length_mm=case_when(sci_name=="Hiatula diphos"~ 120,
                             T ~ length_mm),
         temp_c=case_when(sci_name=="Hiatula diphos"~ 23,
                          T ~ temp_c)) %>% 
  as.data.frame()
rownames(data3_sum) <- data3_sum$tip.label

# Format tree to caper's liking
tree1_clean <- tree1
tree1_clean$node.label <- NULL


# Build
comp <- caper::comparative.data(phy = tree1_clean,
                                data = data3_sum,
                                names.col = "tip.label",
                                vcv = T, 
                                warn.dropped = TRUE)




# PGLS model with lambda estimated
m_pgls <- pgls(rate_d ~ length_mm + temp_c, data = comp, lambda = "ML")
summary(m_pgls)
# Show lambda estaimte
# Shows test to see if different from 0
# SHows test to see if different from 1
# Then shows effect and signifance of correlates


# More regression analysis
################################################################################

# 1. Rename species column
data3 <- data3 |>
  rename(species = tip.label)

# 2. Align tree and data
data3 <- data3[data3$species %in% tree1$tip.label, ]
tree1_sub <- drop.tip(tree1,
                      setdiff(tree1$tip.label, unique(data3$species)))

data3$species <- factor(data3$species)

stopifnot(all(levels(data3$species) %in% tree1_sub$tip.label))

# 3. Fit phylogenetic mixed model (simple version)
fit <- pglmm(
  log(abs(rate_d)) ~ temp_c + length_mm +
    (1 | species),
  data      = data3,
  family    = "gaussian",
  cov_ranef = list(species = tree1_sub)
)

summary(fit)

# 3. Fit phylogenetic mixed model (simple version)
fit <- pglmm(
  log(abs(rate_d)) ~ length_mm + temp_c + tissue + study_type + (1 | species),
  data      = data3,
  family    = "gaussian",
  cov_ranef = list(species = tree1_sub)
)

summary(fit)



