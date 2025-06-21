
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_wos <- read.csv(file=file.path(outdir, "2025_06_20_WOS_search.csv"), as.is=T)
data_spp <- read.csv(file=file.path(outdir, "data_for_proposal.csv"), as.is=T)


# Prep data
################################################################################

# Year stats
ystats <- data_wos %>% 
  count(year) 

# Journal stats
jstats <- data_wos %>% 
  count(source, ) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  mutate(source=recode(source,
                       "Journal of Toxicology and Environmental Health-Part A-Current Issues" = "Journal of Toxicology and Environmental Health",
                       "Journal of Venomous Animals and Toxins Including Tropical Diseases" =" Journal of Venomous Animals and Toxins",
                       "Food Additives and Contaminants Part A-Chemistry Analysis Control Exposure and Risk Assessment" = "Food Additives and Contaminants Part A"))

# Summarize
stats <- data_spp %>% 
  # Remove unknown
  filter(!is.na(order)) %>% 
  # Format condition
  mutate(condition=ifelse(!condition %in% c("Amnesic", "Paralytic", "Diarrhetic"), "Other", condition)) %>% 
  mutate(condition=factor(condition, levels=c("Amnesic", "Paralytic", "Diarrhetic", "Other"))) %>% 
  # Summarize
  group_by(type, order, condition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode type
  mutate(type=recode(type, 
                     "fish"="Fish",
                     "invert"="Invertebrate")) 

order_order <- stats %>% 
  group_by(type, order) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(type, desc(n))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag=element_text(size=9),
                   plot.title = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Year
g1 <- ggplot(ystats, aes(x=year, y=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year of publication", y="Number of papers", tag="A") +
  # scale_x_continuous(breaks=seq(1995, 2025, 5)) +
  # Theme
  theme_bw() + my_theme
g1

# Journal
g2 <- ggplot(jstats, aes(y=reorder(source, desc(n)), x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="", tag="B") +
  theme_bw() + my_theme +
  theme(axis.text.y=element_text(size=6))
g2

# Taxa/toxins
g3 <- ggplot(stats, aes(x=n, y=factor(order, order_order$order), fill=condition)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity", size=0.2, color="grey30", 
           position=position_stack(reverse = TRUE)) +
  # Labels
  labs(y="", x="Number of papers\n(based on 108 of ≥320 candidate papers)", tag="C") +
  # Legend
  scale_fill_manual(name="Toxin type", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey80")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.2, "cm"))
g3

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               3,2), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, 
                             heights=c(0.45, 0.55), widths=c(0.45, 0.55))


# Export
ggsave(g, filename=file.path(plotdir, "Fig3_proposal_metaanalysis.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


