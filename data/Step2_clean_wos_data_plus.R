
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "20250618_depuration_biotoxin_marine_ocean_sea_plus.xlsx"))



# Build species 
################################################################################

x <- 1
df <- purrr::map_df(1:nrow(data_orig), function(x){
  
  # Separate common/species names
  condition <- 
  comms <- data_orig$comm_name[x]
  scis <- data_orig$sci_name[x]
  comms_vec <- strsplit(comms, split=", ") %>% unlist()
  scis_vec <- strsplit(scis, split=", ") %>% unlist()
  df <- tibble(paper_id=data_orig$id[x], 
               biotoxin=data_orig$condition[x],
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
                         #"Hiatula rostrata" = "",       # can't fins                                                 
                         # "Lithobates pipiens" = "", # Northern leopard frog
                         "Mactra veneriformis" = "Mactra quadrangularis",
                         "Neomysis awatschensi" = "Neomysis awatschensis",                                                     
                         "Ostrea rivularis" = "Magallana rivularis",                                                        
                         "Patinopecten yessoensis"  = "Mizuhopecten yessoensis")) %>% 
  # Reduce
  filter(!is.na(sci_name) & !grepl("spp", sci_name))

# Check names
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
  left_join(taxa, by=c("sci_name"="sciname")) %>% 
  # Add condition 
  left_join(data_orig %>% select(id, condition), by=c("paper_id"="id"))

write.csv(df1, file=file.path(outdir, "data_for_proposal.csv"))

# Summarize
stats <- df1 %>% 
  # Remove unknown
  filter(!is.na(order)) %>% 
  # Format condition
  mutate(condition=ifelse(!condition %in% c("Amnesic", "Paralytic", "Diarrhetic"), "Other", condition)) %>% 
  mutate(condition=factor(condition, levels=c("Amnesic", "Paralytic", "Diarrhetic", "Other"))) %>% 
  # Summarize
  group_by(type, order, condition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode type
  mutate(type=recode(type, 
                     "fish"="Fish",
                     "invert"="Invertebrate")) 

order_order <- stats %>% 
  group_by(type, order) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(type, desc(n))
  

ggplot(stats, aes(x=n, y=factor(order, order_order$order), fill=condition)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(y="", x="Number of studies\n(small sample of eventual full sample)") +
  # Legend
  scale_fill_manual(name="Toxin type", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey70")) +
  # Theme
  theme_bw()

df1 %>% 
  # Remove unknown
  filter(!is.na(order)) %>% 
  pull(paper_id) %>% n_distinct()


n_distinct(taxa$family)
n_distinct(taxa$order)

  