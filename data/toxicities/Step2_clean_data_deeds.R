
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
data_orig <- readxl::read_excel(file.path(indir, "Deeds_etal_2008_Table245.xlsx"), na=c("ND", "N/A"))

# Format data
################################################################################

# Convert to mgkg
value <- 1000
units <- "μg/100g"
conv2mgkg <- function(value, units){
  
  if(units %in% c("MU", "MU/100g", "MU/g") | is.na(units) | is.na(value)){
    out <- NA
  }
  
  if(units=="μg/100g" & !is.na(value)){
    out <- value * 0.01
  }
  
  return(out)
  
}

# Convert to MU/100g
value <- 1000
units <- "μg/100g"
conv2mu100g <- function(value, units){
  
  if(units %in% c("MU", "μg/100g") | is.na(units) | is.na(value)){
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
data <- data_orig %>% 
  # Rename
  rename(region=location,
         toxicity_long=toxicity_max) %>% 
  # Format data
  mutate(species=recode(species,
                        "Adelomedon brasiliana"  = "Pachycymbiola brasiliana",                        
                        "Arothron manillensis" = "Arothron manilensis",                              
                        "Cancer magister"  = "Metacarcinus magister",                                 
                        "Demania reynaudi"= "Demania reynaudii",                                   
                        "Euzanthus exsculptus" = "Euxanthus exsculptus",                             
                        # "Lophozozymus octodentatus"  = "",                        
                        "Lunatia heros (=Euspira heros, Polinices heros)"= "Euspira heros",   
                        "Lunatia heros (as Polinicies heros)"  = "Euspira heros",              
                        "Nassarius siguijorensis"= "Nassarius siquijorensis",                         
                        # "Nassarius succinctus"  = "",                             
                        # "Neptunea decemcostata" = "",                            
                        "Niotha clathrata" = "Nassarius conoidalis",                                 
                        "Oliva vidua fulminans" = "Oliva vidua",                             
                        # "Pilumnus pulcher" = "",                                   
                        "Polinices lewisii"  = "Neverita lewisii",                               
                        # "Procambarus clarkii" = "",                               
                        "Scarus (= Ypsiscarus) ovifrons"  = "Scarus ovifrons",
                        "Takifugu radiates" = "Takifugu radiatus",                                  
                        "Tectus nilotica maxima"  = "Rochia nilotica",
                        # "Telmessus acutidens"  = "",                              
                        "Tetraodon cochinchinensis" = "Pao cochinchinensis",                        
                        "Tetraodon suvatii"  = "Pao suvattii",                                
                        "Tetraodon turgidus" = "Pao turgidus",                               
                        "Thais lapillus" = "Nucella lapillus",                                    
                        "Thais lima" = "Nucella lima",                                       
                        "Turbo argyrostoma" = "Turbo argyrostomus",                                 
                        "Turbo marmorata" = "Turbo marmoratus",                                  
                        "Xanthias lividus" = "Juxtaxanthias lividus",                                 
                        "Zidona angulata*" = "Zidona dufresnii")) %>% 
  # Fix some common names
  mutate(comm_name=case_when(species=="Busycon spp." ~ "Busycon whelk",
                             species=="Argobuccinum sp." ~ "Argobuccinum whelk",
                             species=="Pilumnus vespertilio" ~ "Common hairy crab",
                             species=="Nassarius siquijorensis" ~ "Burned nassa",
                             species=="Nucella lima" ~ "File dog winkle",
                             species=="Takifugu radiatus" ~ "Nashifugu puffer",
                             species=="Pao cochinchinensis" ~ "Cochin puffer",
                             species=="Arothron firmamentum" ~ "Starry toado",
                             species=="Arothron stellatus" ~ "Starry puffer",
                             species=="Rochia nilotica" ~ "Commercial top shell",
                             species=="Tectus pyramis" ~ "Pyram top shell",
                             species=="Adelomelon ancilla" ~ "Ancilla volute",
                             species=="Pachycymbiola brasiliana" ~ "Brazilian volute",
                             species=="Turbo argyrostomus" ~ "Silver-mouthed turban",
                             species=="Turbo marmoratus" ~ "Marbled turban",
                             T ~ comm_name)) %>% 
  # Split toxicity and units
  mutate(toxicity=stringr::word(toxicity_long, 1, 1),
         toxicity_units=stringr::word(toxicity_long, 2, -1)) %>% 
  # Format units
  mutate(toxicity_units=recode(toxicity_units,
                               "MU" = "MU",                                          
                               "MU 100 g-1" = "MU/100g",                                 
                               "MU 100 g-1 tissue" = "MU/100g",                             
                               "MU g-1" = "MU/g",                                        
                               "MU g-1 whole" = "MU/g", 
                               "MU g-1 whole crab" = "MU/g", 
                               "MU* 100 g-1" = "MU/100g", 
                               "μg STX eq./100g" = "μg/100g",                              
                               "μg STX eq./100g tissue" = "μg/100g", 
                               "μg STX eq./100g tissue (GTX 2 and GTX 3 only)"  = "μg/100g",
                               "μg STX eq./100g whole" = "μg/100g")) %>% 
  # Clean up units
  mutate(toxicity_units=ifelse(grepl("STX|GTX|toxins", toxicity_units), NA, toxicity_units)) %>% 
  # Clean up toxicity values
  mutate(toxicity=gsub(",", "", toxicity) %>% toupper(.),
         toxicity=recode(toxicity,
                         "<2" = "2",
                         "46-58"="58",
                         "50-500"="500",
                         "71-876"="876",
                         "200-250"="250",
                         "176-600"="600",
                         "~3000-4000" = "4000",
                         "PSP"="",
                         "GTX"="",
                         "STX"="",
                         "TOXIC"="",
                         "TRACE"="",
                         "POSITIVE"="") %>% as.numeric()) %>% 
  # Convert toxicity
  rowwise() %>% 
  mutate(toxicity_mgkg=conv2mgkg(toxicity, toxicity_units),
         toxicity_mu100g=conv2mu100g(toxicity, toxicity_units),
         toxicity_mgkg_conv=mu100g_to_mgkg(toxicity_mu100g)) %>% 
  ungroup() %>% 
  # Add toxicity to use
  mutate(toxicity_mgkg_use=ifelse(!is.na(toxicity_mgkg), toxicity_mgkg, toxicity_mgkg_conv),
         toxicity_mgkg_use_type=ifelse(!is.na(toxicity_mgkg), "Reported", "Converted")) %>% 
  # Add syndrome 
  mutate(syndrome="Paralytic") %>% 
  # Arrange
  select(table, syndrome, algae, comm_name, species, tissue, 
         toxicity_long, toxicity, toxicity_units, toxicity_mgkg, toxicity_mu100g, toxicity_mgkg_conv,
         region, reference, incident, everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$tissue)
table(data$algae)
table(data$toxicity_units)
table(data$toxicity)

# Check
freeR::check_names(data$species)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)
taxa_key <- freeR::taxa(spp_key$species)

# If key
if(F){
  spp_key1 <- spp_key %>% 
    left_join(taxa_key %>% select(-species), by=c("species"="sciname")) %>% 
    # Add genus
    mutate(genus=ifelse(is.na(genus), stringr::word(species, 1, 1), genus)) %>% 
    # Fill
    group_by(genus) %>% 
    fill(type:family, .direction="updown")
  write.csv(spp_key1, file=file.path(indir, "deeds_species_temp.csv"), row.names = F)
}else{
  spp_key2 <- readxl::read_excel(file.path(indir, "deeds_species.xlsx"))
}

data_out <- data %>% 
  # Add species info
  left_join(spp_key2) %>% 
  # Fix whole
  mutate(class=ifelse(comm_name %in% c("Crab", 'Mangrove crabs'), "Malacostraca", class))




# Export data
################################################################################

saveRDS(data_out, file=file.path(outdir, "deeds_etal_2018_max_toxicities_psp.Rds"))

