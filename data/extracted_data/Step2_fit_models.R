
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
#indir <- "data/extracted_data/raw/timeseries"
indir <- "data/extracted_data/raw_round2/data"
outdir <- "data/extracted_data/processed"
#plotdir <- "data/extracted_data/raw/images"
plotdir <- "data/extracted_data/raw_round2/images"

# Source helper functions
source(file.path("data/extracted_data/helper_functions.R"))

# Files to analyze
files2eval <- list.files(path=indir, pattern=".xlsx")
files2eval <- files2eval[!grepl("\\$", files2eval)]


# Build data
################################################################################

# Loop through files
#x <- files2eval[17]
results <- purrr::map_df(files2eval, function(x){
  
  # Read file
  sdata_orig <- readxl::read_excel(file.path(indir, x))
  
  # Format data
  sdata <- sdata_orig %>% 
    # Remove columns that should not contribute to id
    select(., -any_of(c("date", "figure", "note"))) %>%  
    # Add id
    mutate(id = apply(select(., -day, -toxicity, -phase), 1, paste, collapse = "-")) %>% 
    # Arrange
    select(id, everything()) %>% 
    arrange(id, day) %>% 
    # Reduce to depuration phase and recalculate day
    filter(phase=="Depuration") %>% 
    group_by(id) %>% 
    mutate(day_use=day-min(day)) %>% 
    ungroup() %>% 
    relocate(day_use, .after=day) %>% 
    # Add a little zero (or negative)
    mutate(toxicity=ifelse(toxicity<=0, 0.1*min(toxicity[toxicity>0]), toxicity))
  
  # Loop through treatments
  treatments <- unique(sdata$id)
  # y <- treatments[1]
  df <- purrr::map_df(treatments, function(y){
    
    # Filter data
    tdata <- sdata %>% 
      filter(id==y)
    
    # Fit 1-compartment model
    model1 <- fit_1comp(time=tdata$day_use, toxicity=tdata$toxicity)
    
    # Fit 2-compartment model
    
    # Compare models
    
    # Record results
    out <- model1 %>% 
      mutate(treatment=y) %>% 
      select(treatment, everything())
    
  })
  
  # Format
  out <- df %>% 
    mutate(file=x) %>% 
    select(file, everything())

  
})

# Format data
results1 <- results %>% 
  # Add reference and figure
  mutate(reference=gsub(".xlsx", "", file) %>% gsub("_etal_", " et al. ", .)) %>% 
  # Fill in blank treatments
  mutate(treatment=ifelse(treatment=="", reference, treatment)) %>% 
  # Arrange
  select(file, reference, everything())
  

# Export
# xlsx::write.xlsx(x=results1, file=file.path(outdir, "fitted_model_results.xlsx"))

# round 2
xlsx::write.xlsx(x=results1, file=file.path(outdir, "fitted_model_results_round2.xlsx"))




