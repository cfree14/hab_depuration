
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/raw"
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "database.Rds"))


# Build data
################################################################################

# Fomrat data 
data <- data_orig %>% 
  # Recode some common name
  mutate(comm_name=ifelse(grepl("Pacific oyster", comm_name), "Pacific oyster", comm_name)) %>% 
  # Recode class
  mutate(class=recode(class,
                      "Actinopterygii"="Finfish",
                      "Bivalvia"="Bivalves",      
                      "Cephalopoda"="Cephalopods",
                      "Gastropoda"="Gastropods",
                      "Malacostraca"="Crustaceans",
                      "Maxillopoda"="Zooplankton"))

# Number of papers
n_distinct(data$id)

# Number of species evaluated
nspp <- data %>% 
  group_by(id) %>% 
  summarize(nspp=n_distinct(comm_name)) %>% 
  ungroup()

# Number of tissues evaluated
ntissues <- data %>% 
  group_by(id) %>% 
  summarize(ntissues=n_distinct(tissue)) %>% 
  ungroup()

# Field vs lab
stats_type_class <- data %>% 
  # Study types in a paper
  group_by(class, id) %>% 
  summarize(study_type=paste(unique(study_type), collapse=", ")) %>% 
  ungroup() %>% 
  # Compute totals
  group_by(class, study_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  # Compute percent
  group_by(class) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()
stats_type_tot <- data %>% 
  # Study types in a paper
  group_by(class, id) %>% 
  summarize(study_type=paste(unique(study_type), collapse=", ")) %>% 
  ungroup() %>% 
  # Compute totals
  group_by(study_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  # Computer percent
  mutate(prop=n/sum(n)) %>% 
  mutate(class="Overall")
stats_type <- bind_rows(stats_type_class, stats_type_tot) %>% 
  mutate(study_type=stringr::str_to_sentence(study_type),
         study_type=recode(study_type, 
                           "Lab, field"="Lab/field",
                           "Field, lab"="Lab/field"),
         study_type=factor(study_type,
                           levels=c("Lab", "Field", "Field (non-toxic site)", "Lab/field")))
stats_type_order <- stats_type %>% 
  group_by(class) %>% 
  summarize(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n))

# Feeding
stats_feed <- data %>%
  # Reduce to lab studies
  filter(study_type=="lab") %>% 
  # Format scenario
  mutate(feed_scenario=recode(feed_scenario, 
                              "unsure-Chinese"="Unknown"),
         feed_scenario=stringr::str_to_sentence(feed_scenario)) %>% 
  # Group by paper
  group_by(id) %>% 
  summarize(feed_scenario=paste(sort(unique(feed_scenario)), collapse=", ")) %>% 
  ungroup() %>% 
  # Count
  count(feed_scenario) %>% 
  mutate(prop=n/sum(n)) %>% 
  # Recode
  mutate(feed_scenario=recode(feed_scenario,
                              "Fed clean, Starved"="Fed clean vs. starved"),
         feed_scenario=factor(feed_scenario,
                              levels=c("Fed clean",
                                       "Starved",
                                       "Fed clean vs. starved",
                                       "Ambient seawater")))

table(stats_feed$feed_scenario)

# Types of experiments conducted
stats_exp <- data %>% 
  # Remove none
  filter(exp_type!="none") %>% 
  # Recode experiments
  mutate(exp_type=recode(exp_type,
                         # Temperature
                         "temp"="Temperature (°C)",
                         "temp_exposure"="Temperature+Exposure",
                         "temp_diet_type"="Temperature+Diet type",
                         "temp_fed_starved"="Temperature+Fed/starved",
                         "temp_salinity_size_fed"="Temperature+Salinity+Body size+Fed/starved",
                         # Location/years
                         "years"="Years",
                         "sites"="Locations",
                         "sites (ponds)"="Locations",
                         "sites (population)"="Locations",
                         "sites/years"="Locations+Years",
                         # Other
                         "size"="Body size (mm)",
                         "lab_field"="Lab vs. field",
                         "water_column"="Surface vs. seafloor",
                         "catalysts"="Depuration enhancers",
                         "instrument"="Toxicity instrument",
                         "body_condition"="Body condition",
                         # Food
                         "food_type"="Diet type",
                         "fed_starved"="Fed vs. starved",
                         "food_amount"="Food amount (mg/day)",
                         # Exposure
                         "exposure"="Exposure (mg/L toxin)",
                         "exposure_diet"="Exposure diet",
                         "size_exposure"="Exposure+Body size",
                         "bloom_nutrient_cond"="Nutrient conditions during bloom")) %>% 
  # Count by experiment type
  group_by(exp_type) %>% 
  summarize(n=n_distinct(id)) %>% 
  ungroup() %>% 
  # Add experiment type
  mutate(exp_catg=case_when(exp_type %in% c("Locations", 
                                            "Years", 
                                            "Locations+Years") ~ "Locations/\nyears",
                            exp_type %in% c("Fed vs. starved", 
                                            "Diet type", 
                                            "Body condition",
                                            "Food amount (mg/day)") ~ "Diet\nfactors",
                            exp_type %in% c("Exposure (mg/L toxin)", 
                                            "Exposure diet",
                                            "Exposure+Body size", 
                                            "Nutrient conditions during bloom") ~ "Exposure\nlevels",
                            exp_type %in% c("Temperature (°C)", 
                                            "Temperature+Fed/starved", 
                                            "Temperature+Diet type", 
                                            "Temperature+Salinity+Body size+Fed/starved", 
                                            "Temperature+Exposure") ~ "Temperature +",
                            T ~ "Other")) 

# Labels
labels_exp <- stats_exp %>% 
  group_by(exp_catg) %>% 
  summarize(n=sum(n), 
            y=n_distinct(exp_type)) %>% 
  ungroup() %>% 
  mutate(n_label=paste0("n=",n))

# Quick plot
ggplot(stats_exp, aes(y=reorder(exp_type, desc(n)), x=n)) +
  facet_grid(exp_catg~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  geom_text(data=labels_exp, mapping=aes(y=y, x=max(stats_exp$n), label=n_label)) +
  # Labels
  labs(x="Number of papers", y="Experiment type", tag="E") +
  # Theme
  theme_bw()

# Stats rate calculated?
stats_comp <- data %>% 
  group_by(id) %>%
  summarize(rate_type=paste(unique(rate_type), collapse = ", ")) %>% 
  ungroup() %>% 
  count(rate_type) %>% 
  mutate(prop=n/sum(n))

# Stats model
stats_model <- data %>% 
  group_by(id) %>%
  summarize(ncomp=paste(unique(ncomp), collapse = ", ")) %>% 
  ungroup() %>% 
  count(ncomp) %>% 
  mutate(prop=n/sum(n))
stats_model 
sum(stats_model$n)
# freeR::which_duplicated(stats_model$id)





# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=5),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.4, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Nuber of species evaluated
g1 <- ggplot(nspp, aes(x=nspp)) +
  geom_histogram(binwidth = 1) +
  # Labels
  labs(x="Number of species", y="Number of papers", tag="A") +
  scale_x_continuous(breaks=1:10) +
  # Theme
  theme_bw() + base_theme 
g1

# Number of tissues
g2 <- ggplot(ntissues, aes(x=ntissues)) +
  geom_histogram(binwidth = 1) +
  # Labels
  labs(x="Number of tissues", y="Number of papers", tag="B") +
  scale_x_continuous(breaks=1:10) +
  # Theme
  theme_bw() + base_theme
g2

# Field vs. lab
g3 <- ggplot(stats_type, aes(y=factor(class, stats_type_order$class), 
                             x=prop, 
                             fill=study_type)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  geom_text(data=stats_type_order, mapping=aes(label=paste0("n=", n),
                                               y=factor(class, class)), 
            x=1.02, hjust=0, inherit.aes = F, size=1.9, color="grey50") +
  # Labels
  labs(x="Percent of papers", y="", tag="C") +
  scale_x_continuous(labels=scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.15)) +
  # Legend
  scale_fill_ordinal(name="Study type") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y = element_blank())
g3

# Feeding scenario
g4 <- ggplot(stats_feed, aes(x="Lab", y=prop, fill=feed_scenario)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  # Labels
  labs(y="Percent of papers", x="Study type", tag="D") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="Feeding scenario") +
  # Theme
  theme_bw() + base_theme
g4

# Rate type
g5 <- ggplot(stats_comp, aes(x="", y=prop, fill=rate_type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(y="Percent of papers", x="", tag="E") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="Rate type", na.value="grey70") +
  # Theme
  theme_bw() + base_theme
g5

# Experiment type
g6 <- ggplot(stats_exp, aes(y=reorder(exp_type, desc(n)), x=n)) +
  facet_grid(exp_catg~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="Experiment type", tag="F") +
  # Theme
  theme_bw() + base_theme
g6

# Merge
layout_matrix <- matrix(data=c(1,2,3,3,3,
                               4,4,5,5,5), byrow=T, ncol=5)

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g6, 
                             layout_matrix=layout_matrix, heights=c(0.3, 0.7))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_study_design_features.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





