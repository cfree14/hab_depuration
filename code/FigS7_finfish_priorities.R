
# Downloaded data from:
# https://www.fao.org/fishery/en/collection/capture?lang=en

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/fao/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "FAO_vulnerable_finfish_species.Rds"))

# Read depuration data
dep_orig <- readRDS("data/lit_review/round1/processed/database.Rds") 


# Build data
################################################################################

# Species-toxin key
dep <- dep_orig %>% 
  select(sci_name, comm_name, syndrome) %>% 
  unique() %>% 
  filter(syndrome=="Ciguatera")

# Build data
data <- data_orig %>% 
  # Label ones with dep rates
  mutate(rate_yn=ifelse(sci_name %in% dep$sci_name, "Rate", "No rate")) 


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     axis.title.y=element_blank(),
                     legend.text=element_text(size=8),
                     legend.title=element_blank(),
                     strip.text=element_text(size=9),
                     plot.title=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data %>% slice(1:20), 
            aes(x=landings_mt/1e3, 
                y=reorder(comm_name, desc(landings_mt)),
                fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (1000s mt)", y="", title="Ciguatera") +
  # Legend
  scale_fill_ordinal(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS7_finfish_priority_species.png"), 
       width=4.5, height=4.0, units="in", dpi=600)




