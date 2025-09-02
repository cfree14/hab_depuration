
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

# Format data
data <- data_orig %>% 
  # Rename
  rename(toxicity_long=toxicity_max) %>% 
  # Format data
  mutate(species=recode(species,
                        "Adelomedon brasiliana"  = "Pachycymbiola brasiliana",                        
                        "Arothron manillensis" = "rothron manilensis",                              
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
  # Arrange
  select(table, algae, comm_name, species, tissue, 
         toxicity_long, toxicity, toxicity_units,
         location, reference, incident, everything())

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



