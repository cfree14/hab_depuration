
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/toxicities/raw"
outdir <- "data/toxicities/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/hab_depuration/data/extracted_data/raw_round1/timeseries/Rourke_etal_2021_SuppData1.xlsx")

# Center k values, max out y position


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Recode species
  mutate(species=recode(species, 
                        "Atlantic sea scallops"="Atlantic sea scallop",
                        "Blue mussels"="Blue mussel",
                        "Eastern oysters"="Eastern oyster")) %>% 
  # Eliminate softshell clam experimental cages
  filter(!(species=="Soft-shell clam" & treatment=="Experimental cages"))

# Data to fit to
data_fit <- data %>% 
  # Dep events
  filter(phase=="Depuration") %>%  
  # Add event id
  mutate(event_id=paste(species, event, sep="-")) %>% 
  # Adjust zeroes
  mutate(toxicity=ifelse(toxicity==0, min(toxicity[toxicity>0])*0.1, toxicity))


# Identify events
event_key <- data_fit %>% 
  # Unique events
  count(species, event) %>% 
  # Add event id
  mutate(event_id=paste(species, event, sep="-"))

# Loop through and fit
x <- event_key$event_id[1]
fits <- purrr::map_df(event_key$event_id, function(x){
  
  # Subset data
  sdata <- data_fit %>% 
    # Event data
    filter(event_id==x) %>% 
    # Reset date
    mutate(day=day-min(day))
  
  # Dates and days
  dates <- seq(min(sdata$date), max(sdata$date), by="1 day")
  days <- min(sdata$day):max(sdata$day)
  
  # Fit model - LM approach
  fit <- lm(log(toxicity) ~ day, sdata)
  preds <- predict(fit, newdata = tibble(day=days)) %>% exp()
  coefs <- coef(fit)
  k <- coefs[2]
  
  # Fit model - NLS approach
  fit <- nls(toxicity ~ N*exp(-k*day), data=sdata, start=list(N=max(sdata$toxicity), k=0.05))
  preds <- predict(fit, newdata = tibble(day=days))
  coefs <- coef(fit)
  k <- coefs["k"]
  
  # Build
  spp <- sdata$species[1]
  event <- sdata$event[1]
  df <- tibble(species=spp,
               event=event,
               event_id=x,
               date=dates,
               day=days,
               toxicity=preds,
               k=k)
  
})

# Compute stats for label positions
k_lab_ypos <- data_fit %>% 
  group_by(species) %>% 
  summarize(toxicity=max(toxicity)*0.95) %>% 
  ungroup()

k_lab_xpos <- data_fit %>% 
  group_by(event) %>% 
  summarize(date_min=min(date),
            date_max=max(date)) %>% 
  ungroup() %>% 
  mutate(date=date_min + (date_max - date_min) / 2)

# K labels
k_labs <- fits %>% 
  # Simplify
  select(species, event, event_id, k) %>% 
  unique() %>% 
  # Add label
  mutate(k_label=paste0("k=", round(k, 3))) %>% 
  # Add yposition
  left_join(k_lab_ypos, by="species") %>% 
  # Add xposition
  left_join(k_lab_xpos, by="event")
  


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8, face="bold", hjust=0),
                   plot.title=element_text(size=9),
                   # Strip
                   strip.background = element_rect(fill=NA, color=NA),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=date, y=toxicity)) + # color=phase
  facet_wrap(~species, scales="free_y", ncol=1) +
  # Data
  geom_point(pch=16, color="grey60") +
  # Plot fits
  geom_line(data=fits, aes(x=date, y=toxicity, group=event_id), inherit.aes = F) +
  # Plot k values
  geom_text(data=k_labs, aes(x=date, y=toxicity, label=k_label), hjust=0.5, size=2.2) +
  # Labels
  labs(x="Date", y="Toxicity (mg STX eq./kg)") +
  # Axis
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = c(0.01, 0.01)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS4_field_study_utility.png"),
       width=6.5, height=4.5, units="in", dpi=600)
# ggsave(g, filename=file.path(plotdir, "FigS4_field_study_utility_no_fits.png"),
#        width=6.5, height=4.5, units="in", dpi=600)
