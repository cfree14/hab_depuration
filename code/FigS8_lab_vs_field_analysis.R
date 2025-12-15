
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to obs with rates
  filter(!is.na(rate_d) & rate_d<0) %>% 
  # Abs rate
  mutate(rate_d=abs(rate_d)) %>% 
  # Summarize
  group_by(syndrome, comm_name, sci_name, study_type, tissue) %>% 
  summarize(n=n(),
            rate_d=mean(rate_d, na.rm=T)) %>% 
  ungroup() %>% 
  # Reduce to ones with both lab and field available
  group_by(syndrome, comm_name, sci_name, tissue) %>% 
  mutate(ntypes=n()) %>% 
  ungroup() %>% 
  filter(ntypes==2) %>% 
  select(-ntypes) %>% 
  # Build label
  mutate(label=paste0(comm_name, " (", tissue, ")"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   strip.text.y = element_text(angle = 0),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(y=label, x=rate_d, shape=study_type, color=study_type, group=label)) +
  facet_grid(syndrome~., scales="free_y", space="free_y") +
  # Data
  geom_line(color="grey30") + 
  geom_point(size=2) + 
  # Labels
  labs(x=expression("Decay constant, k (day"^-1*")"), #"Depuration rate (day-1)", 
       y="") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10),
                     labels=c("0.001", "0.01", "0.1", "1", "10")) +
  # Legend
  scale_color_discrete(name="") +
  scale_shape_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS8_lab_vs_field_analysis.png"), 
       width=6.5, height=5.5, units="in", dpi=600)
