
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
costa_orig <- readRDS(file=file.path(outdir, "costa_etal_2017_max_toxicities_psp.Rds"))
deeds_orig <- readRDS(file=file.path(outdir, "deeds_etal_2018_max_toxicities_psp.Rds"))
lefe_orig <- readRDS(file=file.path(outdir, "Lefebvre_Robertson_2010_toxicities.Rds"))
jester_orig <- readxl::read_excel(file.path(indir, "Jester_etal_2009.xlsx"))


# Prep data
################################################################################

colnames(costa_orig)
colnames(deeds_orig)
colnames(lefe_orig)
colnames(jester_orig)

# Prep Costa
costa <- costa_orig %>% 
  # Add 
  mutate(dataset="Costa et al. (2017)",
         units="mg/kg") %>% 
  # Rename
  rename(toxicity_long=toxicity_long_orig,
         toxicity_mgkg=toxicity_orig) %>% 
  # Simplify
  select(dataset, class, comm_name, species, syndrome, tissue, region, reference, toxicity_long, toxicity_mgkg)

# Prep Deeds
deeds <- deeds_orig %>% 
  # Add 
  mutate(dataset="Deeds et al. (2018)") %>% 
  # Simplify
  select(dataset, class, comm_name, species, syndrome, tissue, region, reference, 
         toxicity_long, toxicity_mgkg, toxicity_mu100g, toxicity_mgkg_conv)

# Prep Lefebvre_Robertson_2010
lefe <- lefe_orig %>% 
  # Reduce
  filter(class=="Actinopterygii") %>% 
  # Add 
  mutate(dataset="Lefebvre & Robertson (2010)",
         syndrome="Amnesic") %>% 
  # Rename
  rename(toxicity_mgkg=toxicity_ug_g) %>% 
  # Simplify
  select(dataset, class, comm_name, species, syndrome, tissue, region, reference, toxicity_mgkg)

# Prep Jester
jester <- jester_orig %>%   
  # Add 
  mutate(dataset="Jester et al. (2009)",
         reference="Jester et al. (2009)",
         class=ifelse(grepl("crab", comm_name), "Malacostraca", "Actinopterygii")) %>% 
  # Rename
  rename(region=location) %>% 
  # Convert
  mutate(toxicity_mgkg=toxicity_ug100g*0.01) %>% 
  # Simplify: class, 
  select(dataset, class, comm_name, species, syndrome, tissue, region, reference, toxicity_mgkg)

# Merge data
################################################################################

# Merge data
data <- bind_rows(costa, deeds, lefe, jester) %>% 
  # Format ref
  mutate(reference=stringr::str_squish(reference)) %>% 
  # Format tissue
  mutate(tissue=stringr::str_to_sentence(tissue),
         tissue=recode(tissue, 
                       "Egg"="Eggs",
                       "Whole body"="Whole")) %>% 
  # Format some sci name
  mutate(species=recode(species,
                        "Thais lamellosa"="Nucella lamellosa",
                        "Polinices heros"="Euspira heros",
                        "Busycon spp"="Busycon spp.",
                        "Nassarius sp."="Nassarius spp.")) %>% 
  # Format some common names
  mutate(comm_name=case_when(species=="Atergatis floridus" ~ "Green egg crab",
                             species=="Atergatopsis germaini" ~ "Taiwanese crab",
                             species=="Balanus spp." ~ "Balanus barnacle",
                             species=="Carcinoscorpius rotundicauda" ~ "Mangrove horseshoe crab",
                             species=="Eriphia sebana" ~ "Smooth redeyed crab",
                             species=="Concholepas concholepas" ~ "Chilean abalone",
                             species=="Haliotis tuberculata" ~ "Tuberculate abalone",
                             species=="Atergatis floridus" ~ "Green egg crab",
                             species=="Euspira heros" ~ "Northern moon snail",
                             species=="Lambis lambis" ~ "Common spider conch",
                             species=="Lophozozymus pictor" ~ "Mosaic reef crab", 
                             species=="Nassarius siquijorensis" ~ "Burned nassa",
                             species=="Nassarius sp." ~ "Nassa dog whelk",
                             species=="Natica lineata" ~ "Lined moon snail",
                             species=="Natica vitellus" ~ "Calf moon snail",
                             species=="Neptunea spp." ~ "Neptune snail",
                             species=="Oliva vidua" ~ "Black olive",
                             species=="Portunus pelagicus" ~ "Blue swimming crab",
                             species=="Zosimus aeneus" ~ "Devil crab",
                             species=='Procambarus clarkii' ~ "Red swamp crayfish",
                             species=="Actaeodes tomentosus" ~ "Spiny-legged rock crab",
                             species=="Eriphia scabricula" ~ "Hairy banded crab",
                             species=="Hemigrapsus oregonensis" ~ "Yellow shore crab",
                             species=="Metopograpsus frontalis" ~ "Mangrove shore crab",
                             species=="Nassarius conoidalis" ~ "Cone-shaped nassa",
                             species=="Neptunea decemcostata" ~ "New England neptune",
                             species=="Neverita lewisii" ~ "Lewis's moon snail",
                             species=="Nucella lapillus" ~ "Atlantic dogwinkle",
                             species=="Nucella lima" ~ "File dogwinkle",
                             species=="Rapana venosa" ~ "Veined rapa whelk",
                             species=="Zidona dufresnii" ~ "Angulate volute",
                             species=="Turbo argyrostomus" ~ "Silver-mouthed turban",
                             species=="Turbo marmoratus" ~ "Green turban",
                             species=="Tectus pyramis" ~ "Pyramid top shell",
                             species=="Tectus fenestratus" ~ "Fenestrate top shell",
                             species=="Telmessus acutidens" ~ "West Pacific helmet crab",
                             species=="Schizophrys aspera" ~ "Common decorator crab",
                             species=="Nassarius spp." ~ "Nassa dog whelk",
                             species=="Nucella lamellosa" ~ "Frilled dogwinkle",
                             species=="Demania reynaudii" ~ "D. reynaudii crab",
                             species=="Euxanthus exsculptus" ~ "Lumpy rock crab",
                             species=="Juxtaxanthias lividus" ~ "Ksull crab",
                             species=="Lophozozymus octodentatus" ~ "Sponge crab",
                             species=="Neoxanthias impressus" ~ "N. impressus crab",
                             species=="Platypodia granulosa" ~ "Curry puff crab",
                             species=="Platypodia pseudogranulosa" ~ "Flat round crab",
                             T ~ comm_name)) %>% 
  # Record toxicity to use
  mutate(toxicity_mgkg_use = ifelse(!is.na(toxicity_mgkg), toxicity_mgkg, toxicity_mgkg_conv),
         toxicity_mgkg_use_type = ifelse(!is.na(toxicity_mgkg), "Reported", "Converted"),
         toxicity_mgkg_use_type = ifelse(is.na(toxicity_mgkg_use), NA, toxicity_mgkg_use_type))

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$syndrome)
table(data$tissue)


# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$species)
freeR::which_duplicated(spp_key$comm_name)
# duped <- freeR::which_duplicated(spp_key$species)
# spp_key %>% filter(species %in% duped) %>% arrange(species)

# Export
saveRDS(data, file=file.path(outdir, "toxicity_data.Rds"))



# Plot data
################################################################################

# Mg/kg = ug/g = ppm
thresh <- tibble(syndrome=c("Amnesic",
                            "Diarrhetic",
                            "Paralytic"),
                 action_level_mg_kg=c(20,
                                      0.16,
                                      0.8))

# Build stats
stats <- data %>% 
  # Filter
  filter(!is.na(toxicity_mgkg_use) & !is.na(tissue)) %>% 
  # Species tissue label
  mutate(species_tissue=paste0(comm_name, " (", tissue, ")")) %>% 
  # Identify maximum
  arrange(syndrome, species_tissue, desc(toxicity_mgkg_use)) %>% 
  group_by(syndrome, species_tissue) %>% 
  slice(1) %>% 
  ungroup()

# Build stats
stats <- data %>% 
  # Filter
  filter(!is.na(toxicity_mgkg_use)) %>% 
  # Identify maximum
  arrange(syndrome, comm_name, desc(toxicity_mgkg_use)) %>% 
  group_by(syndrome, comm_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Add thresh
  left_join(thresh) %>% 
  filter(toxicity_mgkg_use >= action_level_mg_kg) %>% 
  # Format class
  mutate(class=recode(class, 
                      "Actinopterygii"="Bony fish",
                      "Ascidiacea" = "Sea squirts",    
                      "Asteroidea" = "Starfish",    
                      "Echinoidea" = "Sea urchins",    
                      "Elasmobranchii"="Sharks and rays",
                      "Gastropoda"="Snails",    
                      "Malacostraca"="Crabs, lobsters, shrimps",  
                      "Maxillopoda"="Barnacles",   
                      "Merostomata"="Horseshoe crabs",    
                      "Thecostraca"="Barnacles"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(stats, aes(x=tidytext::reorder_within(comm_name, desc(toxicity_mgkg_use), syndrome), 
                  y=toxicity_mgkg_use, 
                  shape=toxicity_mgkg_use_type, 
                  color=class)) +
  # facet
  facet_grid(.~syndrome, scales="free_x", space="free_x") +
  # Data
  geom_segment(mapping=aes(y=0.1, yend=toxicity_mgkg_use), linewidth = 0.2) +
  geom_point(stat="identity") +
  # Ref line
  geom_hline(data=thresh, mapping=aes(yintercept=action_level_mg_kg), color="grey30", linewidth=0.6) + # , linetype="dashed"
  # Labels
  labs(y="Toxicity (mg/kg)", x="") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(trans="log10", 
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 10^6, 10^7),
                     labels=c("0.01", "0.1", "1", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million")) +
  # Legends
  scale_color_discrete(name="Taxa group") +
  scale_shape_manual(name="Unit type", values=c(21, 16)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.9, 0.7))
g

# Export 
ggsave(g, filename=file.path(plotdir, "FigS8_toxicities_for_non_bivalves.png"), 
       width=6.5, height=4.5, units="in", dpi=600)








