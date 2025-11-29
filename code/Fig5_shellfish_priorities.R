
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
tabledir <- "tables"
outdir <- "data/fao/processed"

# Read data
prod_orig <- readRDS(file=file.path(outdir, "FAO_vulnerable_bivalve_catch_data.Rds"))

# Read depuration data
dep_orig <- readRDS("data/lit_review/round1/processed/database.Rds") 

# Read data (add ISO3 in other)
obis_orig <- readRDS("data/obis/processed/HAB_OBIS_data.Rds") 
obis <- obis_orig %>% 
  mutate(iso3=countrycode::countrycode(sovereign, "country.name", "iso3c"))
freeR::complete(obis)
unique(obis$syndrome)



# Format depuration data
################################################################################

# Format depuration data
dep <- dep_orig %>% 
  # Recode syndrome
  mutate(syndrome=recode(syndrome, "Brevetoxin"="Neurotoxic")) %>% 
  # Reduce to syndrom/species combos
  select(syndrome, class, order, family, genus, sci_name, comm_name) %>% 
  unique()

# Species with depuration data
dep_spp_key <- dep %>% 
  select(comm_name, sci_name) %>% 
  unique()

# Species with production data
prod_spp_key <- prod_orig %>% 
  count(class, order, family, genus, sci_name, comm_name)


# Function to build data
################################################################################

# For testing: syndrome <- "Neurotoxic"
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
  sdata <- prod_orig %>% 
    # Reduce to relevant species
    filter(iso3 %in% isos)
  
  # Overall stats
  nyrs <- n_distinct(sdata$year)
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

# For testing: data1 <- para
id_new_taxa_levels <- function(data1){
  
  # Species with rates
  syndrome_do <- unique(data1$syndrome)
  species_w_rates <- dep %>% 
    filter(syndrome %in% syndrome_do)
  
  # Species in top-20 without depuration rates
  spp_no_rates <- data1 %>% 
    # Reduce to species without rates
    filter(rate_yn=="No rate") %>% 
    group_by(syndrome, taxa_group, isscaap, comm_name, sci_name) %>% 
    summarize(prod_mt=sum(prod_mt)) %>%
    ungroup() %>% 
    # Add taxanomic info
    left_join(prod_spp_key %>% select(sci_name, order, family, genus), by="sci_name") %>% 
    # Check is anything new
    mutate(order_new=ifelse(!order %in% species_w_rates$order, "yes", "no"),
           family_new=ifelse(!family %in% species_w_rates$family, "yes", "no"),
           genus_new=ifelse(!genus %in% species_w_rates$genus, "yes", "no")) %>% 
    # Build label
    mutate(label=case_when(order_new=="yes" ~ paste0("***", order),
                           family_new=="yes" ~ paste0("**", family),
                           genus_new=="yes" ~ paste0("*", genus),
                           T ~ ""),
           label_simple=case_when(order_new=="yes" ~ "***",
                                  family_new=="yes" ~ "**",
                                  genus_new=="yes" ~ "*",
                                  T ~ ""))
    
  # Return
  return(spp_no_rates)
  
}


# Build data
################################################################################

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

# Build new labels
para_new <- id_new_taxa_levels(para)
amne_new <- id_new_taxa_levels(amne)
cyan_new <- id_new_taxa_levels(cyan)
brev_new <- id_new_taxa_levels(brev)
diar_new <- id_new_taxa_levels(diar)
azas_new <- id_new_taxa_levels(azas)

# Merge for easy manuscript stats
merge_new <- bind_rows(para_new, amne_new, cyan_new, brev_new, diar_new, azas_new) %>% 
  # Simplify
  select(syndrome, sci_name, comm_name, prod_mt, label) %>% 
  # Remove ones that aren't new
  filter(label!="") %>% 
  # Add type
  mutate(type=case_when(str_starts(label, "\\*\\*\\*") ~ "Order",
                        str_starts(label, "\\*\\*")    ~ "Family",
                        str_starts(label, "\\*")       ~ "Genus",
                        T ~ "") %>% factor(., levels=c("Order", "Family", "Genus"))) %>% 
  mutate(label=gsub("\\*", "", label)) %>% 
  # Arrange
  arrange(syndrome, type, desc(prod_mt)) %>% 
  select(syndrome, type, label, comm_name, sci_name, prod_mt, everything())

# Export
write.csv(merge_new, file=file.path(tabledir, "TableSX_priority_species_new_taxa_raw.csv"), row.names = F)


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
  # Mark species that would contribute no dep rate info
  geom_text(data=para_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.1, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
        legend.position = c(0.6, 0.45))
g1

# Amnesic
g2 <- ggplot(amne, aes(y=comm_name, x=prod_mt/1e6, alpha=prod_type, fill=rate_yn)) +
  geom_bar(stat="identity") +
  # Mark species that would contribute no dep rate info
  geom_text(data=amne_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.1, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
  # Mark species that would contribute no dep rate info
  geom_text(data=diar_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.1, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
  # Mark species that would contribute no dep rate info
  geom_text(data=cyan_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.01, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
  # Mark species that would contribute no dep rate info
  geom_text(data=brev_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.1, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
  # Mark species that would contribute no dep rate info
  geom_text(data=azas_new, 
            mapping=aes(y=comm_name, x=prod_mt/1e6+0.1, label=label_simple), 
            inherit.aes = F,
            color="red", hjust=0, size=2.4) +
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
ggsave(g, filename=file.path(plotdir, "Fig5_bivalve_priority_species.png"), 
       width=6.5, height=6, units="in", dpi=600)

