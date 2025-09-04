
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
data_orig <- readxl::read_excel(file.path(indir, "Shumway_1995_Table12_finereader.xlsx"))




# Format data
################################################################################

# Convert to mgkg
value <- 1000
units <- "ug/100g"
conv2mgkg <- function(value, units){
  
  if(units %in% c("MU", "MU/100g", "MU/g", "ng/g") | is.na(units) | is.na(value)){
    out <- NA
  }
  
  if(units=="ppm" & !is.na(value)){
    out <- value
  }
  
  if(units=="ug/100g" & !is.na(value)){
    out <- value * 0.01
  }
  
  if(units=="ng/g" & !is.na(value)){
    out <- value * 0.001
  }
  
  return(out)
  
}

# Convert to MU/100g
value <- 1000
conv2mu100g <- function(value, units){
  
  if(units %in% c("MU", "ug/100g", "ng/g", "ppm") | is.na(units) | is.na(value)){
    out <- NA
  }
  
  if(units=="MU/100g" & !is.na(value)){
    out <- value
  }
  
  if(units=="MU/g" & !is.na(value)){
    out <- value * 100
  }
  
  return(out)
  
}

# Convert MU/100g to mg/kg
mu100g_to_mgkg <- function(value){
  out <- value / 100 * 0.18 
}

# Format data
################################################################################

# Convert toxicities to common units

# Format data
data <- data_orig %>% 
  # Fix scientific names
  mutate(species=recode(species, 
                        # "Babylonia japonica" = "",    
                        "Charonia sauliae" = "Charonia lampas",           
                        # "Littorina sp."          
                        "Niotha clathrata" = "Nassarius conoidalis",           
                        "Oliva vidua fulminans" = "Oliva vidua",     
                        # "Patella sp."            
                        # "Penacidae sp."          
                        # "Penaeid sp."            
                        "Polinices duplicata" = "Neverita duplicata",        
                        "Rapana venosa venosa" = "Rapana venosa",      
                        "Schlzophrys aspera" = "Schizophrys aspera",         
                        "Tectus nilotica maxima" = "Rochia nilotica",     
                        "Thais haemastoma" = "Stramonita haemastoma",           
                        "Thais lapillus" = "Nucella lapillus",             
                        # "Thalamita sp."         
                        "Tutufa lissostoma" = "Tutufa bufo",          
                        "Zeuxis siquijorensis" = "Nassarius siquijorensis",       
                        "Zidona angulata" = "Zidona dufresnii")) %>% 
  # Split toxicity
  separate(toxicity_long, into=c("toxicity", "toxicity_units"), sep=" ", remove=F) %>% 
  # Format units
  mutate(toxicity_units=ifelse(toxicity_units %in% c("detected", "provided"), NA, toxicity_units)) %>% 
  # Format values
  mutate(toxicity=recode(toxicity,
                         "0-27"="27",
                         "275-3200"="3200",
                         "71-876"="876",
                         "not"="0",
                         "none"="0",
                         "trace"="0",
  ) %>% as.numeric()) %>% 
  # Final formatting
  mutate(toxicity=ifelse(toxicity_long=="not provided", NA, toxicity),
         toxicity_units=ifelse(toxicity==0 & is.na(toxicity_units), "ug/100g", toxicity_units)) %>% 
  # Convert toxicity
  rowwise() %>% 
  mutate(toxicity_mgkg=conv2mgkg(toxicity, toxicity_units),
         toxicity_mu100g=conv2mu100g(toxicity, toxicity_units),
         toxicity_mgkg_conv=mu100g_to_mgkg(toxicity_mu100g)) %>%
  ungroup() %>% 
  # Arrange
  select(order:toxicity_units, toxicity_mgkg, toxicity_mu100g, toxicity_mgkg_conv, 
         everything())

# Inspect
freeR::complete(data)
sort(unique(data$toxicity))
table(data$toxicity_units)

# Check name
freeR::check_names(data$species)

# Get taxa
taxa <- freeR::taxa(data$species)

# Add taxa
data_out <- data %>% 
  # Add taxa
  left_join(taxa %>% select(sciname, class), by=c("species"="sciname")) %>% 
  mutate(class = case_when(taxa_group=="Gastropods" & is.na(class) ~ "Gastropoda",
                           taxa_group %in% c("Crabs", "Shrimp") & is.na(class) ~ "Malacostraca",
                           T ~ class)) %>% 
  # Arrange
  select(order:taxa_group, class, everything())


# Export data
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "Shumway_etal_1995_toxicities.Rds"))

