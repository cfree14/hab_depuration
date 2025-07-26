
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/lit_review/raw"
outdir <- "data/lit_review/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea_plus.xlsx"))



# Build species 
################################################################################

x <- 1
df <- purrr::map_df(1:nrow(data_orig), function(x){
  
  # Separate common/species names
  comms <- data_orig$comm_name[x]
  scis <- data_orig$sci_name[x]
  comms_vec <- strsplit(comms, split=", ") %>% unlist()
  scis_vec <- strsplit(scis, split=", ") %>% unlist()
  df <- tibble(paper_id=data_orig$id[x], 
               syndrome=data_orig$syndrome[x],
               comm_name=comms_vec,
               sci_name=scis_vec)
  
  
})

# Format data
################################################################################

# Build species key
spp_key <- df %>% 
  # Select
  select(comm_name, sci_name) %>% 
  rename(sci_name_orig=sci_name) %>% 
  # Reduce
  unique() %>% 
  # Format scientific name
  mutate(sci_name=recode(sci_name_orig, 
                         "Acanthopagrus schlegeli"="Acanthopagrus schlegelii",
                         "Bellamya aeruginosa" = "Sinotaia aeruginosa", # freshwater
                         "Cancer magister" = "Metacarcinus magister",                                                         
                         "Mactra veneriformis" = "Mactra quadrangularis",
                         "Neomysis awatschensi" = "Neomysis awatschensis",                                                     
                         "Ostrea rivularis" = "Magallana rivularis",                                                        
                         "Patinopecten yessoensis"  = "Mizuhopecten yessoensis")) %>% 
  # Reduce
  filter(!is.na(sci_name) & !grepl("spp", sci_name))

# Check names
# Hiatula rostrata is unknown
# Lithobates pipiens is terrestrial (northen leopard frog)
# Lunella undulata is correct
# Magallana rivularis is correct-ish ("uncertain > taxon inquirendum")
# Meriones shawi is terrestrial (rodent)
# Sinotaia aeruginosa is correct but is freshwater
freeR:: check_names(spp_key$sci_name)
freeR::which_duplicated(spp_key$sci_name)

# Taxa
taxa <- freeR::taxa(species=spp_key$sci_name)


# Add taxa to data
df1 <- df %>% 
  # Add corrected sci name
  rename(sci_name_orig=sci_name) %>% 
  left_join(spp_key %>% select(sci_name_orig, sci_name)) %>% 
  # Add taxa info
  left_join(taxa, by=c("sci_name"="sciname")) 

write.csv(df1, file=file.path(outdir, "data_for_proposal.csv"))



