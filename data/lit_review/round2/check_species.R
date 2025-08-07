
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/round2/raw"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Biotoxin depuration database - round 2.xlsx"))


# Build species key
################################################################################

x <- 1
spp_orig <- purrr::map_df(1:nrow(data_orig), function(x){
  
  # Separate common/species names
  comms <- data_orig$comm_name[x]
  scis <- data_orig$sci_name[x]
  comms_vec <- strsplit(comms, split=", ") %>% unlist()
  scis_vec <- strsplit(scis, split=", ") %>% unlist()
  df <- tibble(paper_id=data_orig$paper_id[x], 
               comm_name=comms_vec,
               sci_name=scis_vec)
  
})

spp <- spp_orig %>% 
  select(comm_name, sci_name) %>% 
  unique() %>% 
  # Correct
  mutate(sci_name=recode(sci_name,
                         "Acanthocardia tuberculatum" = "Acanthocardia tuberculata",     
                          "Acartia omorii"             = "Acartia (Acartiura) omorii",      
                          # "Anodonta cygnea"           = "",       
                          # "Azolla filiculoides"       = "",       
                          "Azumapecten farreri"       = "Chlamys farreri",      
                          # "Brachionus plicatilis"     = "",       
                          "Chlamys opercularis"       = "Aequipecten opercularis",       
                          # "Choromytilus meridionalis" = "",     
                          "Clupea harengus pallasi"  = "Clupea pallasii pallasii",       
                          # "Crenomytilus grayanus"    = "",       
                          # "Diplodus sargus"          = "",      
                          # "Enhydra lutris kenyoni"   = "",       
                          # "Enhydra lutris nereis"    = "",       
                          "Ensis arcuata"            = "Ensis magnus",      
                          # "Euplotes balteatus"       = "",       
                          "Favella taraikaensis"     = "Schmidingerella taraikaensis",       
                          "Halichondria okadai"      = "Halichondria (Halichondria) okadai",      
                          # "Kulikovia alborostrata"   = "",       
                          "Mactra veneriformis"      = "Mactra quadrangularis",       
                          # "Magallana gigas"          = "", 
                          "Musculista senhousia"     = "Arcuatula senhousia",      
                          "Patinopecten yessoensis"  = "Mizuhopecten yessoensis",       
                          "Phoca caspica"            = "Pusa caspica",      
                          "Pseudopleuronectes platessoides" = "Hippoglossoides platessoides", 
                          "Saccostrea forskali"      = "Saccostrea cuccullata",      
                          "Scapharca broughtonii"    = "Anadara broughtonii",       
                          "Scapharca subcrenata"     = "Anadara kagoshimensis",  
                          "Seriola dumerlii"         = "Seriola dumerili",       
                          "Sinanodonta arcaeformis"  = "Anemina arcaeformis",       
                          # "Sinanodonta woodiana"     = "",      
                          # "Stylocheilus striatus"    = "",       
                          # "Trachemys scripta"        = "",       
                          "Unio douglasiae"          = "Nodularia douglasiae",      
                          "Venus gallina"            = "Chamelea gallina",
                         "Venerupis philippinarum" = "Ruditapes philippinarum")) %>% 
  unique() 

# Any duplicated common names?
freeR::which_duplicated(spp$comm_name)

# Any duplicated scientific names?
freeR::which_duplicated(spp$sci_name)

# Check scientific names
freeR::check_names(spp$sci_name)


