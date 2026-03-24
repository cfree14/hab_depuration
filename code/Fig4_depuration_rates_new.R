
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

# Prepare data
data <- data_orig %>% 
  # Recode some common name
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Teleostei"="Finfish",
                      "Ascidiacea"="Other",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Other",
                      "Dinophyceae"="Other",
                      "Gastropoda"="Gastropods",
                      "Malacostraca"="Crustaceans",
                      "Mammalia"="Other",
                      "Maxillopoda"="Zooplankton",
                      "Copepoda"="Copepods",
                      "Thecostraca"="Crustaceans")) %>% 
  # Order class
  mutate(class=factor(class,
                      levels=c("Bivalves", "Crustaceans", "Finfish", "Copepods", "Other", "Gastropods") %>% rev())) %>% 
  # Filter
  # remove one crazy outlier that should really be an increase
  filter(!is.na(hlife_d) & rate_d<0 & rate_d < -0.0001) %>% 
  # Add percent daily loss
  mutate(perc_loss_d=(1-exp(rate_d))) %>% 
  # Order syndromes
  mutate(syndrome=factor(syndrome, levels=c("Paralytic", "Diarrhetic", "Amnesic", 
                                            "Cyanotoxin", "Neurotoxic", "Ciguatera", 
                                            "Azaspiracid", "Other")))

# Stats
stats <- data %>% 
  group_by(class, genus) %>% 
  summarise(hlife_d=median(hlife_d),
            perc_loss_d=median(perc_loss_d),
            rate_d=median(rate_d)) %>% 
  ungroup() %>% 
  arrange(desc(hlife_d))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   panel.spacing = unit(0, "lines"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Decay rate
g1 <- ggplot(data, aes(x=abs(rate_d), y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  # geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) + 
  geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  geom_point(data=stats, mapping=aes(x=abs(rate_d)), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Decay constant, k (1/day)", # x=expression("Decay constant, k ("*day^{-1}*")"), 
       y="Genus", tag="A") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100")) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text = element_blank(),
        legend.position = "none")
g1

# Loss per day
g2 <- ggplot(data, aes(x=perc_loss_d, y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  # geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) + 
  geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  geom_point(data=stats, mapping=aes(x=perc_loss_d), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Daily loss (%)", y="Genus", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text = element_blank(),
        legend.position = "none")
g2

# Half life
g3 <- ggplot(data, aes(x=hlife_d, y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  # geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) + 
  geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  geom_point(data=stats, mapping=aes(x=hlife_d), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Half life (day)", y="Genus", tag="C") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1, 1, 10, 100, 1000),
                     labels=c("0.1", "1" , "10" , "100", "1000")) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        strip.text.y = element_text(angle = 0))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.37, 0.26, 0.37))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_depuration_rates_new.png"), 
       width=6.5, height=6, units="in", dpi=600)



# BoXplot version


# Decay rate
g1 <- ggplot(data, aes(x=abs(rate_d), y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) +
  # geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  # geom_point(data=stats, mapping=aes(x=abs(rate_d)), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Decay constant, k (1/day)", # x=expression("Decay constant, k ("*day^{-1}*")"), 
       y="Genus", tag="A") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100")) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text = element_blank(),
        legend.position = "none")
g1

# Loss per day
g2 <- ggplot(data, aes(x=perc_loss_d, y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) +
  # geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  # geom_point(data=stats, mapping=aes(x=perc_loss_d), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Daily loss (%)", y="Genus", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text = element_blank(),
        legend.position = "none")
g2

# Half life
g3 <- ggplot(data, aes(x=hlife_d, y=factor(genus, stats$genus))) +
  facet_grid(class~., space="free_y", scale="free_y") +
  geom_boxplot(fill="grey80", color="grey30", outliers = F, lwd=0.2) +
  # geom_point(aes(fill=syndrome), pch=21, alpha=0.7, stroke=0.1) +
  # geom_point(data=stats, mapping=aes(x=hlife_d), fill="black", pch=16, size=1) +
  # Labels
  labs(x="Half life (day)", y="Genus", tag="C") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1, 1, 10, 100, 1000),
                     labels=c("0.1", "1" , "10" , "100", "1000")) +
  # Legend
  scale_fill_manual(name="Toxin syndrome", values=RColorBrewer::brewer.pal(8, "Set3")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        strip.text.y = element_text(angle = 0))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.37, 0.26, 0.37))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_depuration_rates_new_boxplot.png"), 
       width=6.5, height=6, units="in", dpi=600)
