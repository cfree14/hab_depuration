
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


# Build data
################################################################################

# Decay params
n0 <- 120
k <- -0.03

# Timeline
days <- 1:120
toxs <- n0*exp(k*days)
data <- tibble(day=days,
               toxicity=toxs)

# Sampling scheme
samp <- data %>% 
  slice(seq(1, nrow(data), 7)) %>% 
  # Remove ones after two clean tests
  filter(day<=60) %>% 
  # Mark 
  mutate(program=ifelse(day %in% c(1, 36, 43, 50, 57), "Optimized (5 tests)", "Standard (9 tests)"),
         program=factor(program, levels=c("Standard (9 tests)", "Optimized (5 tests)")))

# Plot data
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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=day, y=toxicity)) +
  # Reference line
  geom_hline(yintercept=30, linetype="dashed", color="grey40") +
  annotate(geom="text", x=0, y=30, label="Action threshold", 
           color="grey40", hjust=0, vjust=-1, size=2.4) +
  # Data
  geom_line() +
  geom_point(data=samp, mapping=aes(fill=program), pch=21, size=3) +
  # Labels
  labs(x="Day of testing", y="Toxicity (ppm)") +
  # Legend
  scale_fill_ordinal(name="Monitoring program") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_dep_traj_example.png"), 
       width=4.5, height=3, units="in", dpi=600)

