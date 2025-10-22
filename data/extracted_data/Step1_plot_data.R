
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/timeseries"
plotdir <- "data/extracted_data/raw/images"

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
#datafile="Houle_etal_2023_Fig8.xlsx"; y_title="Toxicity (ug /100g)"; legend_title="Tissue"; title="Houle et al. (2023)"

# Plot data
plot_data <- function(datafile, y_title="Toxicity", legend_title="Treatment", title=NA){
  
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
  data_dep <- data %>% 
    filter(phase=="Depuration") %>%
    mutate(toxicity=pmax(0.0001, toxicity))
  day1 <- min(data_dep$day)
  day2 <- max(data_dep$day)
  
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
    days <- seq(day1, day2, length.out=50)
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
    geom_vline(xintercept=day1, linetype="dashed", color="grey50") +
    # Plot data
    geom_line(linewidth=0.1) +
    geom_point(size=1.2) +
    # Plot fits
    geom_line(data=fits, linewidth=1) +
    geom_text(data=coefs, mapping=aes(label=paste0("k = ", round(k, 5)), x=day2, y=y), 
              hjust=1, size=1.8, show.legend=F) +
    # Axes
    scale_x_continuous(lim=c(0,NA)) +
    scale_y_continuous(lim=c(0,NA)) +
    # Legend
    scale_color_discrete(name=legend_title) +
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
  ggsave(g, filename=file.path(plotdir, outname),
         width=width, height=2.5, units="in", dpi=600)
  
}


# Loop through files
################################################################################

# Files to analyze
files2eval <- list.files(path=indir, pattern=".xlsx")
files2eval <- files2eval[!grepl("\\$", files2eval)]

# Loop through
i <- files2eval[1]
for(i in files2eval){
  
  # Params
  filename <- i
  title <- filename %>% 
    gsub(".xlsx", "", .) %>% 
    gsub("_", " ", .) %>% 
    gsub("etal", "et al.", .)
  
  # Plot data
  plot_data(datafile=filename, y_title="Toxicity", legend_title="Treatment", title=title)
    
  
}

# Check 1 file
################################################################################

plot_data(datafile="Bogan_etal_2006_Table1.xlsx", y_title="Toxicity", legend_title="Treatment", title="Bogan_etal_2006_Table1.xlsx")

plot_data(datafile="Bogan_etal_2007_Tables1+2.xlsx", y_title="Toxicity", legend_title="Treatment", title="Bogan_etal_2006_Table1.xlsx")


plot_data(datafile="Houle_etal_2023_Fig6.xlsx", y_title="Toxicity", legend_title="Treatment", title="Houle_etal_2023_Fig6.xlsx")


plot_data(datafile="Rourke_etal_2021_SuppData1.xlsx", y_title="Toxicity", legend_title="Treatment", title="Rourke_etal_2021_SuppData1.xlsx")
plot_data(datafile="Rourke_etal_2021_SuppData2.xlsx", y_title="Toxicity", legend_title="Treatment", title="Rourke_etal_2021_SuppData2.xlsx")
plot_data(datafile="Rourke_etal_2021_SuppData5.xlsx", y_title="Toxicity", legend_title="Treatment", title="Rourke_etal_2021_SuppData5.xlsx")
plot_data(datafile="Rourke_etal_2021_SuppData6.xlsx", y_title="Toxicity", legend_title="Treatment", title="Rourke_etal_2021_SuppData6.xlsx")


