
# Downloaded data from:
# https://www.fao.org/fishery/en/collection/capture?lang=en

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "/Users/cfree/Dropbox/Chris/UCSB/data/fao/global_production/processed"
outdir <- "data/fao/processed"

# Read data
data_orig <- readRDS(file=file.path(indir, "1950_2023_fao_global_production.Rds"))

# Read depuration data
dep_orig <- readRDS("data/lit_review/round1/processed/database.Rds") %>% 
  mutate(syndrome=recode(syndrome, "Brevetoxin"="Neurotoxic"))
unique(dep_orig$syndrome)
dep_spp_key <- dep_orig %>% 
  select(comm_name, sci_name) %>% unique()

# Read data (add ISO3 in other)
obis_orig <- readRDS("data/obis/processed/HAB_OBIS_data.Rds") 
obis <- obis_orig %>% 
  mutate(iso3=countrycode::countrycode(sovereign, "country.name", "iso3c"))
freeR::complete(obis)
unique(obis$syndrome)

# Fix scientific names
# Shortern/harmonize common names
# Mark ones that represent new orders/families/genus
# Add number with rates in topright

# Format depuration data
################################################################################

# Species-toxin key
dep <- dep_orig %>% 
  select(sci_name, comm_name, syndrome) %>% 
  unique()

# Build production data
################################################################################

# Build data
yrs <- 2014:2023
nyrs <- length(yrs)
data <- data_orig  %>% 
  # Reduce
  filter(year %in% yrs) %>% 
  # Marine bivalves
  filter(area_type=="Marine" & taxa_group=="Mollusca" & !isscaap %in% c("Squids, cuttlefishes, octopuses", "Freshwater molluscs")) %>% 
  # Species specific
  filter(level=="species") %>% 
  # Format a few common names
  mutate(comm_name=recode(comm_name,
                          "Whelk"="Common whelk",
                          "Atlantic surf clam"="Atlantic surfclam",
                          "Northern quahog(=Hard clam)"="Northern quahog",
                          "Pacific cupped oyster"="Pacific oyster",
                          "American cupped oyster"="Eastern oyster",
                          "Common edible cockle"="Common cockle",
                          "Great Atlantic scallop"="King scallop",
                          "American sea scallop"="Atlantic sea scallop",
                          "South American rock mussel"="Brown mussel",
                          "Southern Australia scallop"="Commercial scallop",
                          "Yesso scallop"="Japanese scallop", 
                          "New Zealand mussel"="Greenshell mussel", 
                          "Green mussel"="Green-lipped mussel", 
                          "Cholga mussel"="Ribbed mussel")) %>% 
  # Correct scientific names
  mutate(sci_name=recode(sci_name,
                         "Lutraria oblonga" =  "Lutraria magna",
                         "Mytilus unguiculatus" =  "Mytilus coruscus",
                         "Spisula sibyllae"  =  "Spisula sachalinensis" ))
  
# Inpsect
table(data$measure)
table(data$isscaap)

# Check names
freeR::check_names(data$sci_name)
# Aliger gigas
# Anadara kagoshimensis 
# Anadara similis
# Aptyxis syracusana 
# Atrina maura 
# Austrofusus glans 
# Crassula aequilatera 
# Dallocardia muricata
# Donax dentifer
# Ensis leei 
# Glycymeris nummaria
# Glycymeris ovata
# Hiatula diphos 
# Iliochione subrugosa
# Larkinia grandis 
# Magallana bilineata 
# Magallana gigas
# Magallana sikamea
# Melongena patula 
# Monoplex parthenopeus 
# Mytella strigata
# Polititapes aureus
# Polititapes rhomboides
# Rochia nilotica
# Saccostrea cuccullata 
# Sinistrofulgur sinistrum 
# Spisula murchisoni 

# Summarize data
################################################################################

# Overall stats
stats_tot <- data %>% 
  # Average annual catch
  group_by(taxa_group, isscaap, comm_name, sci_name) %>% 
  summarize(prod_mt=sum(value, na.rm=T)/nyrs,
            ncountries=n_distinct(iso3)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(prod_mt))

# Production type stats
stats <- data %>% 
  group_by(taxa_group, isscaap, comm_name, sci_name, prod_type) %>% 
  summarize(prod_mt=sum(value, na.rm=T)/10) %>% 
  ungroup()


# Function to build data
################################################################################

syndrome <- "Neurotoxic"
syndromes <- unique(dep$syndrome)
build_data <- function(syndrome){
  
  # Syndrome
  syndrome_do <- syndrome
  
  # Species with rates
  species_w_rates <- dep %>% 
    filter(syndrome %in% syndrome_do) %>% 
    pull(sci_name) %>% sort()
  
  # Identify countries where the syndrome occurs
  isos <- obis %>% 
    filter(syndrome==syndrome_do) %>% 
    pull(iso3) %>% unique() %>% sort()
  
  # Reduce production to countries with syndrome
  sdata <- data %>% 
    # Reduce to relevant species
    filter(iso3 %in% isos)
  
  # Overall stats
  stats_tot <- sdata %>%
    # Average annual catch
    group_by(taxa_group, isscaap, comm_name, sci_name) %>%
    summarize(prod_mt=sum(value, na.rm=T)/nyrs,
              ncountries=n_distinct(iso3)) %>%
    ungroup() %>%
    # Arrange
    arrange(desc(prod_mt)) %>% 
    # Reduce to top-20
    slice(1:20)
  
  # Build data
  stats <- sdata %>% 
    # Calculate annual average
    group_by(taxa_group, isscaap, comm_name, sci_name, prod_type) %>% 
    summarize(prod_mt=sum(value, na.rm=T)/nyrs) %>% 
    ungroup() %>% 
    # Mark whether depuration rates are available
    mutate(syndrome=syndrome_do,
           rate_yn=ifelse(sci_name %in% species_w_rates, "Rate", "No rate")) %>% 
    # Arrange
    select(syndrome, everything()) %>% 
    # Reduce
    filter(sci_name %in% stats_tot$sci_name) %>% 
    # Order
    mutate(comm_name=factor(comm_name, stats_tot$comm_name),
           rate_yn=factor(rate_yn, levels=c("Rate", "No rate")),
           prod_type=factor(prod_type, levels=c("Capture", "Aquaculture")))
  
  # Plot data
  ggplot(stats, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Annual production\n(millions mt)", y="", title=syndrome_do) +
    #scale_x_continuous(trans="log10")
    # Legend
    scale_alpha_manual(name="", values=c(0.5, 1)) +
    scale_fill_manual(name="", values=c("blue", "red")) +
    # Theme
    theme_bw() +
    theme(legend.position = c(0.8, 0.8))
  
  # Return
  return(stats)
  
}

# Build data
para <- build_data(syndrome="Paralytic")
amne <- build_data(syndrome="Amnesic")
cyan <- build_data(syndrome="Cyanotoxin")
brev <- build_data(syndrome="Neurotoxic")
diar <- build_data(syndrome="Diarrhetic")
azas <- build_data(syndrome="Azaspiracid")

# Merge for creating count key and species
merged <- bind_rows(para, amne, cyan, brev, diar, azas)

# Build key
count_key <- merged %>% 
  # Summarize max
  group_by(syndrome, comm_name, rate_yn) %>% 
  summarize(prod_mt=sum(prod_mt)) %>% 
  ungroup() %>% 
  # Summarize
  group_by(syndrome) %>% 
  summarize(comm_name=comm_name[prod_mt==min(prod_mt)],
            prod_mt_max=max(prod_mt),
            nspp_w_rates=sum(rate_yn=="Rate")) %>% 
  ungroup() %>% 
  mutate(label=paste(nspp_w_rates, "species"))

# Confirm that FAO common names match database common names
display_spp_key <- merged %>% 
  count(comm_name, sci_name) %>% 
  rename(comm_name_fao=comm_name) %>% 
  left_join(dep_spp_key, by="sci_name") %>% 
  rename(comm_name_us=comm_name) %>% 
  mutate(check=comm_name_fao==comm_name_us)


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(),
                   legend.margin = margin(t=-2, b=-2, r=0, l=0),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Paralytic
g1 <- ggplot(para, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Paralytic"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Paralytic") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.6, 0.8))
g1

# Amnesic
g2 <- ggplot(amne, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Amnesic"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Amnesic") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g2

# Diarrhetic
g3 <- ggplot(diar, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Diarrhetic"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Diarrhetic") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g3

# Cyanotoxins
g4 <- ggplot(cyan, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Cyanotoxin"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Cyanotoxins") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g4

# Neurotoxic
g5 <- ggplot(brev, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Neurotoxic"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Neurotoxic") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g5

# Azaspiracid
g6 <- ggplot(azas, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Add label
  geom_text(data=count_key %>% filter(syndrome=="Azaspiracid"), 
            aes(x=prod_mt_max/1e6, y=comm_name, label=label), 
            inherit.aes=F, hjust=1, color="blue", size=2.2) +
  # Labels
  labs(x="Annual production\n(millions mt)", y="", title="Azaspiracid") +
  # Legend
  scale_alpha_manual(name="", values=c(0.5, 1)) +
  scale_fill_manual(name="", values=c("blue", "red")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none")
g6

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, 
                             g4, g5, g6, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "Fig8_bivalve_priority_species.png"), 
       width=6.5, height=6, units="in", dpi=600)

