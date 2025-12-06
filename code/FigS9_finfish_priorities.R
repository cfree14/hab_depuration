
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
  mutate(rate_yn=ifelse(sci_name %in% dep$sci_name, "Rate", "No rate")) %>% 
  #Format common name
  mutate(comm_name=recode(comm_name, 
                          "Little tunny(=Atl.black skipj)"="Little tunny"),
         species_label=paste0(comm_name, " (", sci_name, ")"))


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     axis.title.y=element_blank(),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.title=element_text(size=7),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data %>% slice(1:50), 
            aes(x=landings_mt/1e3, 
                y=reorder(species_label, desc(landings_mt)),
                fill=ciguatera_yn)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (1000s mt)", y="") +
  scale_x_continuous(trans="log10") +
  # Legend
  scale_fill_discrete(name="Ciguatera observed?") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS9_finfish_priority_species.png"), 
       width=6.5, height=5.5, units="in", dpi=600)
 



