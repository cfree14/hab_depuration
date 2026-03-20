
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
  group_by(syndrome, comm_name, sci_name, tissue, study_type) %>% 
  summarize(n=n(),
            rate_d=mean(rate_d, na.rm=T)) %>% 
  ungroup() %>% 
  # Reduce to ones with both lab and field available
  filter(study_type!="field (non-toxic site)") %>% 
  group_by(syndrome, comm_name, sci_name, tissue) %>% 
  mutate(ntypes=n()) %>% 
  ungroup() %>% 
  filter(ntypes==2) %>% 
  select(-ntypes) %>% 
  # Build label
  mutate(label=paste0(comm_name, " (", tissue, ")")) %>% 
  # Format study type
  mutate(study_type=stringr::str_to_sentence(study_type) %>% recode(., "Lab"="Laboratory")) %>% 
  # Arrange toxins
  mutate(syndrome=factor(syndrome,
                         levels=c("Paralytic", "Amnesic", "Diarrhetic", "Neurotoxic", "Other") %>% rev())) %>% 
  # Add conversion factor
  group_by(syndrome, comm_name, sci_name, tissue) %>% 
  mutate(conv=rate_d[study_type=="Laboratory"]/rate_d[study_type=="Field"]) %>% 
  ungroup()


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
                   legend.margin = margin(b=-5),
                   legend.position = "top",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(y=label, x=rate_d, shape=study_type, color=study_type, group=label)) +
  facet_grid(syndrome~., scales="free_y", space="free_y") +
  # Data
  geom_line(color="grey30") + 
  geom_point(size=2) + 
  # Conversion
  geom_text(mapping=aes(x=0.001, y=label, label=format(round(conv, 1), nsmall = 1)), 
            size=2.4, color="grey30", hjust=0) +
  # Labels
  labs(x=expression("Decay constant, k (day"^-1*")"), #"Depuration rate (day-1)", 
       y="") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10),
                     labels=c("0.001", "0.01", "0.1", "1", "10")) +
  # Legend
  scale_color_manual(name="", values=c("#35B779", "#440154")) +
  scale_shape_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_lab_vs_field_analysis.png"), 
       width=6.5, height=5.5, units="in", dpi=600)
