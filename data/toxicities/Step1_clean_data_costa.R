
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
costa1_orig <- read.csv(file.path(indir, "Costa_etal_2017_Table1.csv"), na.strings = c("N/A", "nr"))
costa2_orig <- read.csv(file.path(indir, "Costa_etal_2017_Table2.csv"), na.strings = c("N/A", "nr"))

# Merge data
################################################################################

# Merge
costa <- bind_rows(costa1_orig, costa2_orig) %>% 
  # Rename
  rename(toxicity_long_orig=toxicity) %>% 
  # Format syndrome
  mutate(syndrome=recode(syndrome,
                         "ASP"="Amnesic",
                         "DSP"="Diarrhetic",
                         "PSP"="Paralytic")) %>% 
  # Format names
  mutate(species=recode(species,
                        "Adelomelon brasiliana" = "Pachycymbiola brasiliana",  
                        "Asterina pectinifera" = "Patiria pectinifera",     
                        "Asterins amurensis" = "Asterias amurensis",
                        "Balanus Balanoides" = "Semibalanus balanoides",        
                        "Balanus cariosus" = "Semibalanus cariosus",
                        "Cancer magister" = "Metacarcinus magister",         
                        "Nassarius siguijorensis" = "Nassarius siquijorensis",  
                        "Neptunea decemcostata" = "Neptunea decemcostata",  ####
                        "Niotha clathrata" = "Nassarius conoidalis",
                        "Oliva vidua fulminans" = "Oliva vidua",
                        "Panulirus versicolour" = "Panulirus versicolor",     
                        "Plicopurpura columellaris" = "Plicopurpura columellaris",
                        "Polinices lewissi" = "Neverita lewisii",        
                        "Procambarus clarkii" = "Procambarus clarkii",       
                        "Searlesia dira" = "Lirabuccinum dirum",           
                        "Tectus niloticus maximus" = "Rochia nilotica",  
                        "Telmessus acutidens" = "Telmessus acutidens",       
                        "Turbo argyrostoma" = "Turbo argyrostomus",         
                        "Turbo marmorata" = "Turbo marmoratus",          
                        "Zidona angulata" = "Zidona dufresnii")) %>% 
  # Seperate toxicity and units
  mutate(toxicity_orig=stringr::word(toxicity_long_orig, 1, 1),
         toxicity_units=stringr::word(toxicity_long_orig, 2, -1)) %>% 
  # Format toxicity
  mutate(toxicity_orig=toxicity_orig %>% gsub(">", "", .) %>% as.numeric()) %>% 
  # Format units
  mutate(toxicity_units=recode(toxicity_units,
                               "mg DA/kg" = "mg/kg", 
                               "mg OA eq/kg" = "mg/kg",   
                               "mg OAeq.kg!1" = "mg/kg",     
                               "mg OAeq/kg" = "mg/kg", 
                               "mg STX eq./kg" = "mg/kg", 
                               "mg STX eq.kg!1" = "mg/kg",  
                               "mg STXeq.kg!1" = "mg/kg",    
                               "mg STXeq/kg" = "mg/kg")) %>% 
  # Arrange
  select(-c(phylum, class, family)) %>% 
  select(feeding_type, species, syndrome, 
         toxicity_long_orig, toxicity_orig, toxicity_units, 
         tissue, region, reference, everything())
  

# Inspect
str(costa)
table(costa$feeding_type)
table(costa$syndrome)
table(costa$tissue)
table(costa$toxicity_units)

# Check names
freeR::check_names(costa$species)

# If building key
if(F){
  
  # Species
  spp_key <- costa %>% count(species) %>% select(-n)
  taxa_key <- freeR::taxa(spp_key$species)
  name_key <- freeR::fb_comm_name(spp_key$species)
  spp_key1 <- spp_key %>% 
    left_join(name_key %>% select(species, comm_name), by="species") %>% 
    left_join(taxa_key %>% select(type:genus, sciname), by=c("species"="sciname"))
  write.csv(spp_key1, file=file.path(indir, "costa_species_key_temp.csv"), row.names=F)
 
}else{
  
  spp_key2 <- readxl::read_excel(file.path(indir, "costa_species_key.xlsx"))
  
}

# Add species info
costa_out <- costa %>% 
  left_join(spp_key2)


# Export data
################################################################################

saveRDS(costa_out, file=file.path(outdir, "costa_etal_2017_max_toxicities_psp.Rds"))


# Plot data
################################################################################

# Build data
data <- costa %>% 
  filter(!is.na(tissue) & !is.na(toxicity_orig)) %>% 
  # Get max value
  arrange(syndrome, species, tissue, desc(toxicity_orig)) %>% 
  group_by(syndrome, species, tissue) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Build species tissue label
  mutate(tissue=tolower(tissue),
         label=paste0(species, " (", tissue, ")"))

# Mg/kg = ug/g = ppm
thresh <- tibble(syndrome=c("Amnesic",
                            "Diarrhetic",
                            "Paralytic"),
                 action_level_mg_kg=c(20,
                                      0.16,
                                      0.8))


ggplot(data, aes(x=toxicity_orig, y=reorder(label, desc(toxicity_orig)))) +
  facet_grid(syndrome~., scales="free_y", space="free_y") +
  geom_bar(stat="identity", fill="grey70") + 
  geom_vline(data=thresh, mapping=aes(xintercept = action_level_mg_kg)) +
  # Action threshold
  # Labels
  labs(x="Toxicity (mg/kg)", y="") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     labels=c("0.01", "0.1", "1", "10", "100", "1,000", "10,000")) +
  # Theme
  theme_bw()






