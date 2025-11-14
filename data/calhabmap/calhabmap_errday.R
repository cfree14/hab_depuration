
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rerddap)
library(tidyverse)

# Base URL
base_url <- "https://erddap.sccoos.org/erddap/"

# Datasets
out <- ed_search(query = "HABS-", which = "tabledap", url = base_url)
dataset_ids <- out$info$dataset_id


# Merge data
################################################################################

# Loop through datasets and merge
data_orig <- purrr::map_df(dataset_ids, function(x){
  
  # Download
  df <- tabledap(x = x,
                 url = base_url)
  df_out <- df %>% 
    # Add dataset id
    mutate(dataset_id=x) %>% 
    # Format sample id
    mutate(SampleID=as.character(SampleID))
  
  
})


# Build data
################################################################################

colnames(data)

# Build data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lat_dd=latitude,
         long_dd=longitude, 
         temp_c=temp) %>% 
  # Add date
  mutate(date=substr(time, 1, 10) %>% lubridate::ymd(.),
         year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Add location 
  mutate(location=gsub("HABs-", "", dataset_id) %>% 
           str_replace_all(., "(?<!^)([A-Z])", " \\1")) %>% 
  # Arrange
  select(dataset_id, location, location_code:sample_id, 
         year, month, date, time, everything())

# Site key
site_key <- data %>% 
  group_by(dataset_id, location, location_code, lat_dd, long_dd) %>% 
  summarize(nyr=n_distinct(year),
            nobs=n()) %>% 
  ungroup() %>% 
  arrange(lat_dd)


# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="small")

# Plot land
ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Point
  geom_point(site_key, mapping=aes(x=long_dd, y=lat_dd)) +
  ggrepel::geom_text_repel(site_key, mapping=aes(x=long_dd, y=lat_dd, label=location), 
            hjust=0, size=2.5) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(-116,-125), ylim=c(32.5, 42)) +
  # Theme
  theme_bw()


# Plot data
################################################################################

ggplot(data, aes(y=factor(location, levels=site_key$location), 
                          x=date, size=temp_c, color=temp_c)) +
  geom_point() +
  # Labels
  labs(x="Date", y="") +
  #  Legend
  scale_size_continuous(name="Temp (°C)") +
  scale_color_gradientn(name="Temp (°C)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw()






