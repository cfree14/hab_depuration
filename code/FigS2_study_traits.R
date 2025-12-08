
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/lit_review/processed/"
plotdir <- "figures"

# Read data
data <- readRDS(file=file.path(datadir, "database_paper_metadata.Rds"))

# Get world
world_orig <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Read field sites
sites <- readxl::read_excel(file.path(datadir, "/field_site_coordinates.xlsx"))


# Build data
################################################################################

# Year stats
ystats <- data %>% 
  count(year) 

# Journal stats
jstats <- data %>% 
  # Count journals
  count(journal) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  # Format journals
  mutate(journal=stringr::str_to_title(journal),
         journal=gsub("And", "&", journal),
         journal=gsub("In", "in", journal),
         journal=gsub("The", "the", journal),
         journal=gsub("Of", "of", journal)) #%>% 
  # # Format
  # mutate(journal=recode(journal,
  #                      "Journal of Toxicology and Environmental Health-Part A-Current Issues" = "Journal of Toxicology and Environmental Health",
  #                      "Journal of Venomous Animals and Toxins Including Tropical Diseases" =" Journal of Venomous Animals and Toxins",
  #                      "Food Additives and Contaminants Part A-Chemistry Analysis Control Exposure and Risk Assessment" = "Food Additives and Contaminants Part A"))

# Country stats
cstats <- data %>% 
  count(country, iso3) %>% 
  filter(!is.na(country))

# Are all countries in map?
cstats$iso3[!cstats$iso3%in%world_orig$adm0_a3]

# Spatialize
world <- world_orig %>% 
  # Add sample size
  left_join(cstats, by=c("adm0_a3"="iso3"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=8),
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
  # Annotate
  annotate(geom="text", x=min(ystats$year), y=max(ystats$n), label=paste(nrow(data), "papers"), 
           color="grey30", size=2.2, hjust=0, vjust=0.5) +
  # Labels
  labs(x="Year of publication", y="Number of papers", tag="A") +
  scale_x_continuous(breaks=seq(1980, 2025, 5)) +
  scale_y_continuous(breaks=seq(0,16,2)) +
  # Theme
  theme_bw() + my_theme
g1

# Journal
g2 <- ggplot(jstats, aes(y=reorder(journal, desc(n)), x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Plot
g3 <- ggplot() +
  geom_sf(data=world, mapping=aes(fill=n), lwd=0.1) +
  # Plot field sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd), size=0.6) +
  # Labels
  labs(tag="C", x="", y="") +
  # Legend
  scale_fill_gradientn(name="Number\nof papers", na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(ylim=c(-53,80)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               3,2), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_study_traits.png"), 
       width=6.5, height=5.0, units="in", dpi=600)


