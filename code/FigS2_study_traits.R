
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/round1/raw"
outdir <- "data/lit_review/round1/processed"
plotdir <- "figures"

# Read data
data <- read.csv(file=file.path(outdir, "2025_06_18_WOS_search.csv"), as.is=T)

# Get world
world_orig <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Read field sites
sites <- readxl::read_excel("data/lit_review/round1/raw/field_site_coordinates.xlsx")


# Build data
################################################################################

# Year stats
ystats <- data %>% 
  count(year) 

# Journal stats
jstats <- data %>% 
  count(source) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  mutate(source=recode(source,
                       "Journal of Toxicology and Environmental Health-Part A-Current Issues" = "Journal of Toxicology and Environmental Health",
                       "Journal of Venomous Animals and Toxins Including Tropical Diseases" =" Journal of Venomous Animals and Toxins",
                       "Food Additives and Contaminants Part A-Chemistry Analysis Control Exposure and Risk Assessment" = "Food Additives and Contaminants Part A"))

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
ggsave(g, filename=file.path(plotdir, "Fig1_study_traits.png"), 
       width=6.5, height=5.0, units="in", dpi=600)


