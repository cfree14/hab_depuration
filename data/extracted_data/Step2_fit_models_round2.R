
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw_round2/data"
outdir <- "data/extracted_data/processed"
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
  # y <- treatments[23]
  df <- purrr::map_df(treatments, function(y){
    
    # Filter data
    tdata <- sdata %>% 
      filter(id==y)
    
    # Fit 1-compartment model (LM)
    model1_lm <- fit_1comp_lm(time=tdata$day_use, toxicity=tdata$toxicity)
    
    # Fit 1-compartment model (NLS)
    model1 <- try(fit_one_comp(time=tdata$day_use, toxicity=tdata$toxicity))
    
    # Fit 2-compartment model (NLS)
    model2 <- try(fit_two_comp(time=tdata$day_use, toxicity=tdata$toxicity))
    
    # Extract coefficients
    coefs_1_lm <- get_coefs_1_lm(model1_lm)
    
    # If 1-compartment model works....
    if(!inherits(model1, "try-error")){
      
      # Extract coefficients
      coefs_1_nls <- get_coefs_1_nls(model1)
      
      # Calculate AIC statistics
      model1_aic <- aicc_from_fit(fit=model1, n=nrow(tdata)) %>% 
        rename(p1=p, RSS1=RSS, AIC1=AIC, AICC1=AICc)
      
      # If it doesn't work, record empty
    }else{
      coefs_1_nls <- tibble(N0=NA, k=NA)
      model1_aic <- tibble(p1=NA, RSS1=NA, AIC1=NA, AICC1=NA)
    }
    
    # If 2-compartment model works....
    if(!inherits(model2, "try-error")){
      
      # Extract coefficients
      coefs_2_nls <- get_coefs_2_nls(model2)
      
      # Calculate AIC statistics
      model2_aic <- aicc_from_fit(fit=model2, n=nrow(tdata)) %>% 
        rename(p2=p, RSS2=RSS, AIC2=AIC, AICC2=AICc)
      
      # If it doesn't work, record empty
    }else{
      coefs_2_nls <- tibble(N1=NA, N2=NA, k1=NA, k2=NA)
      model2_aic <- tibble(p2=NA, RSS2=NA, AIC2=NA, AICC2=NA)
    }
    
    # Record results
    out <- tibble(treatment=y) %>% 
      bind_cols(coefs_1_lm, coefs_1_nls, model1_aic, coefs_2_nls, model2_aic)
    
  })
  
  # Format
  out <- df %>% 
    mutate(file=x) %>% 
    select(file, everything())

  
})


# Format results
################################################################################

# Format data
results1 <- results %>% 
  # Add reference and figure
  mutate(reference=gsub(".xlsx", "", file) %>% gsub("_etal_", " et al. ", .)) %>% 
  # Fill in blank treatments
  mutate(treatment=ifelse(treatment=="", reference, treatment)) %>% 
  # Arrange
  select(file, reference, everything()) %>% 
  # Add k to use
  # If LM method finds posivite k or NLS method hits bound, use LM method
  mutate(k_use = ifelse(!is.na(k), k, k_lm)) %>% 
  # Best model
  mutate(best_model=ifelse(AICC1 <= AICC2 | is.na(!AICC2), 1, 2))


table(results1$best_model)

# Inspect
freeR::complete(results1)


# Compare Ks from LM and NLS models
ggplot(results1, aes(x=k_lm, y=k)) +
  geom_point() +
  geom_abline(slope=1)


# Export data
################################################################################

# Export data
xlsx::write.xlsx(x=results1, file=file.path(outdir, "fitted_model_results_round2.xlsx"))




