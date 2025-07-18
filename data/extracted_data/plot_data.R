
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "data/extracted_data/figures"



# Helper functions
################################################################################

# Testing
data <- read.csv(file.path(indir, "Kim_etal_2017_Fig1.csv"), as.is=T)
day <- 7; y_title="Toxicity (ug/g)"; legend_title="Exposure (ug/L)"; title="Kim et al. (2017)"

# Plot data
plot_data <- function(data, day=NA, y_title="Toxicity", legend_title="Treatment", title=NA){
  
  # Add treatment
  if(!"treatment" %in% colnames(data)){
    data$treatment <- "None"
  }
  
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
                      legend.key = element_rect(fill = NA, color=NA),
                      legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Species column?
  spp_yn <- "species" %in% colnames(data)
  
  # Plot data
  g <- ggplot(data, aes(x=day, 
                   y=toxicity,
                   color=as.character(treatment))) +
    # Plot reference line
    geom_vline(xintercept=day, linetype="dashed", color="grey50") +
    # Plot data
    geom_line(linetype="dotted") +
    geom_point() +
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
  
  # Print
  print(g)
  
}


# Plot data
################################################################################

# Kim et al. (2017)
data <- read.csv(file.path(indir, "Kim_etal_2017_Fig1.csv"), as.is=T)
plot_data(data=data, day=7, y_title="Toxicity (ug/g)", legend_title="Exposure (ug/L)", title="Kim et al. (2017)")

# Kim et al. (2018)
data <- read.csv(file.path(indir, "Kim_etal_2018_Fig1.csv"), as.is=T)
plot_data(data=data, day=7, y_title="Toxicity (ug/g)", legend_title="Exposure (ug/L)", title="Kim et al. (2018)")

# Lewis et al. (2022)
data <- readxl::read_excel(file.path(indir, "Lewis_etal_2022_Fig3.xlsx"))
plot_data(data=data, day=7, y_title="Toxicity (ug/g)", legend_title="Treatment", title="Lewis et al. (2022)")

# Gibble et al. (2016) -- this needs checking
data <- readxl::read_excel(file.path(indir, "Gibble_etal_2016_Fig1-5.xlsx"))
plot_data(data=data, day=0, y_title="Toxicity (ug/g)", legend_title="Treatment", title="Lewis et al. (2022)")

# Min et al. (2018)
data <- readxl::read_excel(file.path(indir, "Min_etal_2018_Fig1.xlsx"))
plot_data(data=data, day=7, y_title="Toxicity (ug/g)", legend_title="Exposure (ug/L)", title="Min et al. (2018)")

# Takata et al. (2008)
data <- readxl::read_excel(file.path(indir, "Takata_etal_2008_Fig1.xlsx"))
plot_data(data=data, day=0, y_title="Toxicity (MU/g)", legend_title="Exposure (ug/L)", title="Takata et al. (2008)")

# Lavaud et al. (2021) - problems here
data <- readxl::read_excel(file.path(indir, "Lavaud_etal_2021_Fig2.xlsx"))
plot_data(data=data, day=0, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Lavaud et al. (2021)")

# Yang et al. (2021)
data <- readxl::read_excel(file.path(indir, "Yang_etal_2021_Fig1-3.xlsx"))
plot_data(data=data, day=0, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Yang et al. (2021)")

# Kwong et al. (2006)
data <- readxl::read_excel(file.path(indir, "Kwong_etal_2006_Fig2.xlsx"))
plot_data(data=data, day=6, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Kwong et al. (2006)")

# Kwong et al. (2006)
data <- read.csv(file.path(indir, "Martins_etal_2020_Fig1.csv"))
plot_data(data=data, day=5, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Martins et al. (2020)")

# Sekiguchi et al. (2001)
data <- read.csv(file.path(indir, "Sekiguchi_etal_2001_Fig2.csv"))
plot_data(data=data, day=5, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Sekiguchi et al. (2001)")

# Chen et al. (2002)
data <- read.csv(file.path(indir, "Chen_etal_2002_Fig1.csv"))
plot_data(data=data, day=33, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Chen et al. (2002)")

# Houle et al. (2023) - lab
data <- read.csv(file.path(indir, "Houle_etal_2023_Fig8.csv"))
plot_data(data=data, day=0, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Houle et al. (2023)")

# Houle et al. (2023) - field (I don't think this one went well)
data <- read.csv(file.path(indir, "Houle_etal_2023_Fig2.csv")) %>% 
  mutate(date=lubridate::mdy(date),
         day=difftime(date, min(date), units = "days") %>% as.numeric()) 
plot_data(data=data, day=0, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Houle et al. (2023)")

# Qiu et al. (2018)
data <- read.csv(file.path(indir, "Qiu_etal_2018_Fig4.csv"))
plot_data(data=data, day=5, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Qiu et al. (2018)")

# Svensson et al. (2003) - the data points aren't quite right - not connected right
data <- read.csv(file.path(indir, "Svensson_etal_2003_Fig2.csv"))
plot_data(data=data, day=5, y_title="Toxicity (ug/100g)", legend_title="Exposure (ug/L)", title="Svensson et al. (2003)")




