
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
data_orig <- readxl::read_excel(file.path(indir, "2024_11_06_WOS_search.xls"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(source=source_title,
         year=publication_year,
         title=article_title) %>% 
  # Simplify
  select(year,
         title,
         authors,
         source,
         volume, 
         issue,
         doi,
         document_type,
         language) %>% 
  # Format
  mutate(source=stringr::str_to_title(source))

# Inspect data
str(data)
freeR::complete(data)

# Inspect more
table(data$language)
table(data$document_type)

# Export data
write.csv(data, file=file.path(outdir, "2024_11_06_WOS_search.csv"), row.names=F)


# Plot data
################################################################################

# Year stats
ystats <- data %>% 
  count(year) 

# Journal stats
jstats <- data %>% 
  count(source, ) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  mutate(source=recode(source,
                       "Food Additives And Contaminants Part A-Chemistry Analysis Control Exposure & Risk Assessment"="Food Additives And Contaminants Part A"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Year
g1 <- ggplot(ystats, aes(x=year, y=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year of publication", y="Number of papers") +
  # Theme
  theme_bw() + my_theme
g1

# Journal
g2 <- ggplot(jstats, aes(y=reorder(source, desc(n)), x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of papers", y="") +
  theme_bw() + my_theme
g2

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               3,2), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_year_journal_stats.png"), 
       width=10.5, height=6.5, units="in", dpi=600)


