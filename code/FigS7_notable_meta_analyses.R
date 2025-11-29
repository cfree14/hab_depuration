
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/extracted_data/raw"
plotdir <- "figures"

# Read data
# Alvarez et al., 2020
# Chen and Chou, 2002
# Min et al., 2018
chen_lab_orig <- readxl::read_excel(file.path(indir, "Chen_Chou_2002_Fig1.xlsx")) 
chen_field_orig <- readxl::read_excel(file.path(indir, "Chen_Chou_2002_Fig5.xlsx"))
min_orig <- readxl::read_excel(file.path(indir, "Min_etal_2018_Fig1.xlsx"))


# Build data
################################################################################

# Format Min et al. (2018)
min <- min_orig %>% 
  filter(day>=7) %>% 
  # Reset days
  group_by(species, treatment) %>% 
  mutate(day=day-min(day)) %>% 
  ungroup() %>% 
  # Format treatment
  mutate(treatment=paste(treatment, "ug/L"))

# Format Chen and Chou 2002
chen <- bind_rows(chen_lab_orig %>% mutate(type="Lab") %>% filter(day>=36),
                  chen_field_orig %>% mutate(type="Field") %>% filter(day>=17)) %>% 
  select(type, day, toxicity) %>% 
  # Reset days
  group_by(type) %>% 
  mutate(day=day-min(day)) %>% 
  ungroup()



# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
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

# Plot Chen
g1 <- ggplot(chen, aes(x=day, y=toxicity, color=type)) +
  geom_point() +
  geom_line() +
  # Labels
  labs(x="Day", y="Toxicity (MU/g)", tag="A", title="Chen & Chou (2002)") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8))
g1

# Plot Min
g2 <- ggplot(min, mapping=aes(x=day, y=toxicity, shape=species, color=treatment)) + 
  geom_point() +
  # Labels
  labs(x="Day", y="Toxicity (ug/g)", tag="B", title="Min et al. (2018)") +
  # Legends
  scale_shape_manual(name="Age group", values=c(16, 21)) +
  scale_color_discrete(name="Toxin exposure") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "FigS7_notable_meta_analyses.png"), 
       width=6.5, height=2.75, units="in", dpi=600)





data <- chen %>% 
  filter(type=="Field")

time <- data$day
toxicity <- data$toxicity
plot <- T
fit_1comp_model <- function(time, toxicity, plot=F){
  
  # Starting parameters
  n0_start <- max(toxicity)             
  k_start  <- 0.2
  
  # Build data
  df <- tibble(time=time,
               toxicity=toxicity)
  
  # Fit model
  fit <- nlsLM(
    toxicity ~ n0 * exp(-k * time),
    data  = df,
    start = list(n0 = n0_start, k = k_start),
    lower = c(0, 1e-8)
  )
  
  # Plot quickly
  plot_fit(data=df, fit=fit)
  
  # Return
  return(fit)
  
}

data <- df
model <- fit_1comp_model(time=time, toxicity=toxicity)
plot_fit <- function(data, fit){
  
  # Make predictions
  times <- seq(min(data$time), max(data$time), length.out=200)
  newdata <- tibble(time=times)
  toxicity_pred <- predict(fit, newdata = newdata)
  preds <- tibble(time=times,
                  toxicity=toxicity_pred)
  
  
  # Plot data and fit
  g <- ggplot(data, mapping=aes(x=time, y=toxicity)) +
    geom_point() + 
    geom_line(data=preds) +
    # Labels
    labs(x="Time", y="Toxicity") +
    # Theme
    theme_bw()
  g
  
}






# One-compartment model: y = C0 * exp(-k * t)
fit_one_comp <- function(df) {
  start_C0 <- max(df$y)              # sensible start
  start_k  <- 0.2
  nlsLM(
    y ~ C0 * exp(-k * t),
    data  = df,
    start = list(C0 = start_C0, k = start_k),
    lower = c(0, 1e-8)
  )
}

# Two-compartment model: y = A*exp(-k1*t) + B*exp(-k2*t)
fit_two_comp <- function(df) {
  # starts: split initial value + distinct rates
  start_A  <- 0.6 * df$y[df$t == min(df$t)][1]
  start_B  <- 0.4 * df$y[df$t == min(df$t)][1]
  start_k1 <- 0.4     # "fast"
  start_k2 <- 0.05    # "slow"
  nlsLM(
    y ~ A * exp(-k1 * t) + B * exp(-k2 * t),
    data  = df,
    start = list(A = start_A, B = start_B, k1 = start_k1, k2 = start_k2),
    lower = c(0, 0, 1e-8, 1e-8)
  )
}







