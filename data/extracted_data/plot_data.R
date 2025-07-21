
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "data/extracted_data/figures"

# Data must be XLSX and have columns:
# species, treatment, day, toxicity

# Helper functions
################################################################################

# This function should:
# Read data
# Fit models
# Plot data, models, and coefficients
# Export figure

# Testing
# datafile="Kim_etal_2017_Fig1.xlsx"; day=7; y_title="Toxicity (ug/g)"; legend_title="Exposure (ug/L)"; title="Kim et al. (2017)"
#datafile="Houle_etal_2023_Fig8.xlsx"; day=0; y_title="Toxicity (ug /100g)"; legend_title="Tissue"; title="Houle et al. (2023)"

# Plot data
plot_data <- function(datafile, day=NA, y_title="Toxicity", legend_title="Treatment", title=NA){
  
  # Read data and take notes
  ##################################
  
  # Read data
  data <- readxl::read_excel(file.path(indir, datafile))
  
  # Add species
  species_yn <- "species" %in% colnames(data)
  if(species_yn==F){
    data$species <- "None"
  }
  
  # Add treatment
  treatment_yn <- "treatment" %in% colnames(data)
  if(treatment_yn==F){
    data$treatment <- "None"
  }
  
  # Filter for depuration
  day1 <- day
  day2 <- max(data$day)
  data_dep <- data %>% 
    filter(day>=day1)
  
  # Fit models
  ##############################################################################
  
  # Fit models
  models <- data_dep %>%
    group_by(species, treatment) %>%
    nest() %>%
    mutate(
      model = map(data, ~ lm(log(toxicity) ~ day, data = .x)), # model object
      coefs = map(model, broom::tidy), # model coefficients
      stats = map(model, broom::glance) # model fit metrics
    )
  
  # Max toxicity by species
  ymax <- data %>% 
    group_by(species) %>% 
    summarize(ymax=max(toxicity)) %>% 
    ungroup
  
  # Build coefficients
  coefs <- models %>%
    # Unnest coefficients
    select(species, treatment, coefs) %>%
    unnest(coefs) %>% 
    # Simplify
    select(species, treatment, term, estimate) %>% 
    # Rename parameters
    # Spread
    spread(key="term", value="estimate") %>% 
    # Set names
    setNames(c("species", "treatment", "n0", "k")) %>% 
    # Format N0
    mutate(n0=exp(n0)) %>% 
    # Add ymax 
    left_join(ymax, by="species") %>% 
    # group_by(species) %>% 
    # mutate(ymax=pmax(ymax, max(n0))) %>% 
    # ungroup() %>% 
    # Treatment order
    group_by(species) %>% 
    mutate(y_rank=1:n(),
           y= ymax- ymax*0.05*y_rank) %>% 
    ungroup()
    
  
  # Build data to plot
  x <- 1
  fits <- purrr::map_df(1:nrow(coefs), function(x){
    
    # Get params
    species <- coefs$species[x]
    treatment <- coefs$treatment[x]
    k <- coefs$k[x]
    n0 <- coefs$n0[x]
    
    # Simulate
    days <- seq(day1, day2, length.out=20)
    toxs <- n0 * exp(k*days)
    #plot(toxs ~ days)
    
    # Merge
    df <- tibble(species=species,
                 treatment=treatment,
                 day=days,
                 toxicity=toxs)
    
  })
  

  # Plot data
  ##############################################################################
  
  # Base theme
  base_theme <- theme(axis.text=element_text(size=7),
                      axis.title=element_text(size=8),
                      legend.text=element_text(size=7),
                      legend.title=element_text(size=8),
                      strip.text=element_text(size=8),
                      plot.title=element_text(size=9),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      # Legend
                      legend.key.size=unit(0.5, "cm"),
                      legend.key = element_rect(fill = NA, color=NA),
                      legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Species column?
  spp_yn <- "species" %in% colnames(data)
  spp_n <- ifelse(spp_yn==T, n_distinct(data$species), 1)
  
  # Plot data
  g <- ggplot(data, aes(x=day, 
                   y=toxicity,
                   color=as.character(treatment))) +
    # Plot reference line
    geom_vline(xintercept=day, linetype="dashed", color="grey50") +
    # Plot data
    # geom_line(linetype="dotted") +
    geom_point() +
    # Plot fits
    geom_line(data=fits) +
    geom_text(data=coefs, mapping=aes(label=paste0("k = ", round(k, 5)), x=day2, y=y), 
              hjust=1, size=1.8, show.legend=F) +
    # Axes
    scale_x_continuous(lim=c(0,NA)) +
    scale_y_continuous(lim=c(0,NA)) +
    # Legend
    scale_color_ordinal(name=legend_title) +
    # Labels
    labs(x="Day", y=y_title, title=title) +
    # Theme
    theme_bw() + base_theme
  
  # Facet by species
  if(spp_yn){
    g <- g + 
      facet_wrap(~species, scales="free_y")
  }
  
  # Suppress lengend
  if(treatment_yn==F){
    g <- g + 
      theme(legend.position="none")
  }
  
  # Print
  print(g)
  
  # Set size of export
  width <- case_when(spp_n==1 & treatment_yn==F ~ 2.75, # 1 species, no legend
                     spp_n==1 & treatment_yn==T ~ 3.75, # 1 species, legend
                     spp_n>1 & treatment_yn==F ~ 5.5, # 1 species, no legend
                     spp_n>1 & treatment_yn==T ~ 6.5, # 1 species, legend
                     T ~ 6.5)
  
  
  # Export
  outname <- gsub(".xlsx", "_US.png", datafile)
  ggsave(g, filename=file.path(indir, outname),
         width=width, height=2.5, units="in", dpi=600)
  
  
}


# Plot data
################################################################################

# Kim et al. (2017)
plot_data(datafile="Kim_etal_2017_Fig1.xlsx", 
          day=7, y_title="Toxicity (ug/g)", legend_title="Exposure (ug/L)", title="Kim et al. (2017)")

 # Kim et al. (2018)
plot_data(datafile="Kim_etal_2018_Fig1.xlsx", 
          day=7, y_title="Toxicity (mg/g)", legend_title="Exposure (mg/L)", title="Kim et al. (2018)")

# Lewis et al. (2022)
plot_data(datafile="Lewis_etal_2022_Fig3.xlsx",
          day=7, y_title="Toxicity (ug/kg)", legend_title="Treatment", title="Lewis et al. (2022)")

# Gibble et al. (2016) -- this needs checking
plot_data(datafile="Gibble_etal_2016_Fig1-5.xlsx",
          day=0, y_title="Toxicity (ng/g)", legend_title="Treatment", title="Gibble et al. (2016)")

# Min et al. (2018)
plot_data(datafile="Min_etal_2018_Fig1.xlsx",
          day=7, y_title="Toxicity (ug/g)", legend_title="Exposure (ug/L)", title="Min et al. (2018)")

# Takata et al. (2008)
plot_data(datafile="Takata_etal_2008_Fig1.xlsx",
          day=0, y_title="Toxicity (MU/g)", legend_title="Year", title="Takata et al. (2008)")

# Lavaud et al. (2021) - problems here
plot_data(datafile="Lavaud_etal_2021_Fig2.xlsx",
          day=1, y_title="Toxicity (ug/100g)", title="Lavaud et al. (2021)")

# Yang et al. (2021)
plot_data(datafile="Yang_etal_2021_Fig1-3.xlsx",
          day=0, y_title="Toxicity (MU/100g)", legend_title="Food type", title="Yang et al. (2021)")

# Kwong et al. (2006)
plot_data(datafile="Kwong_etal_2006_Fig2.xlsx",
          day=6, y_title="Toxicity (ug/100g)", legend_title="Tissue", title="Kwong et al. (2006)")

# Martins et al. (2006)
plot_data(datafile="Martins_etal_2020_Fig1.xlsx",
          day=5, y_title="Toxicity (ug/kg)", legend_title="Toxin", title="Martins et al. (2020)")

# Sekiguchi et al. (2001)
plot_data(datafile="Sekiguchi_etal_2001_Fig2.xlsx",
          day=5, y_title="Toxicity (nmol/specimen)", legend_title="Toxin", title="Sekiguchi et al. (2001)")

# Chen et al. (2002)
plot_data(datafile="Chen_etal_2002_Fig1.xlsx",
          day=33, y_title="Toxicity (MU/g)",  title="Chen et al. (2002)")

# Houle et al. (2023) - lab
plot_data(datafile="Houle_etal_2023_Fig8.xlsx",
          day=0, y_title="Toxicity (ug /100g)", legend_title="Tissue", title="Houle et al. (2023)")

# Houle et al. (2023) - field (I don't think this one went well)
# data <- read.xlsx(file.path(indir, "Houle_etal_2023_Fig2.xlsx")) %>% 
#   mutate(date=lubridate::mdy(date),
#          day=difftime(date, min(date), units = "days") %>% as.numeric()) 
# plot_data(datafile=, day=0, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Houle et al. (2023)")

# Qiu et al. (2018)
plot_data(datafile="Qiu_etal_2018_Fig4.xlsx",
          day=5, y_title="Toxicity (umol/kg)", legend_title="Toxin", title="Qiu et al. (2018)")

# Svensson et al. (2003) - the data points aren't quite right - not connected right
plot_data(datafile="Svensson_etal_2003_Fig2.xlsx", 
          day=5, y_title="Toxicity (ug/g)", legend_title="Food type", title="Svensson et al. (2003)")




