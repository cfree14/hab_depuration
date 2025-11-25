
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rotl)
library(ape)
library(phytools)
library(picante)
library(tidyverse)
library(caper)
library(phyr)
library(ggtree)

# Directories
outdir <- "output"
plotdir <- "figures"

# Read data
tree_orig <- readRDS(file=file.path(outdir, "bivalve_phylogeny.Rds"))
pred_data_orig <- readRDS(file=file.path(outdir, "bivalve_prediction_data.Rds"))
data_orig <- readRDS(file=file.path(outdir, "bivalve_pst_depuration_rates.Rds"))


# Prepare data
################################################################################

# Format data
data <- data_orig %>% 
  rename(species=sci_name) %>% 
  mutate(rate_d_log=abs(rate_d) %>% log())

# Format predictions
pred_data <- pred_data_orig %>% 
  rename(species=sci_name) %>% 
  mutate(tissue="soft tissue",
         study_type="field")


s# Fit model
################################################################################

# Specify priors
priors <- c(
  brms::prior(normal(0, 5), class = "Intercept"),
  brms::prior(normal(0, 2), class = "b"),
  brms::prior(exponential(1), class = "sd"),
  brms::prior(exponential(1), class = "sigma")
)

# Fit model
fit <- brms::brm(
  formula =  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k + (1 | order/family/genus/species),
  data    = data,
  family  = gaussian(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  control = list(adapt_delta = 0.95)
)

# Inspect
fit
summary(fit)


# Inspect random effects
################################################################################

# Extract random effects
re_list <- brms::ranef(fit_tax)

# Build df
x <- 1
re_df <- purrr::map_df(1:length(re_list), function(x){
  
  # Subset 
  type <- names(re_list[x])
  re <- re_list[x] %>% 
    # Convert to dataframe
    as.data.frame() %>% 
    # Rename
    set_names(c("est_log", "est_log_se", "est_log_lo", "est_log_hi")) %>% 
    # Add
    mutate(group=rownames(.),
           type=type) %>% 
    remove_rownames() %>% 
    # Arrange
    select(type, group, everything())
  
})

# Format
re <- re_df %>% 
  # Type
  mutate(type=sub(".*:(\\s*)?", "", type),
         type=stringr::str_to_sentence(type),
         type=factor(type, levels=c("Order", "Family", "Genus", "Species"))) %>% 
  # Group
  mutate(group=sub(".*_(\\s*)?", "", group)) %>% 
  # Remove species
  filter(type!="species") %>% 
  # Format
  mutate(est=exp(est_log),
         est_lo=exp(est_log_lo),
         est_hi=exp(est_log_hi))

ggplot(re, aes(y=tidytext::reorder_within(group, est, type), 
               x=est)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  geom_segment(mapping=aes(x=est_lo, xend=est_hi)) +
  geom_point() +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  scale_x_continuous(trans="log10") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()


# Predictions
################################################################################

# Make predictions
preds <- brms::posterior_epred(
  fit,
  newdata = pred_data,
  allow_new_levels = T, 
  re_formula = NULL
) %>% exp()

# Summarise predictions
preds_stats <- as_tibble(t(apply(preds, 2, function(x) {
  c(rate_d    = mean(x),
    rate_d_lo = quantile(x, 0.025),
    rate_d_hi = quantile(x, 0.975))
}))) %>%
  setNames(c("est", "est_lo", "est_hi")) %>% 
  bind_cols(pred_data, .)

# Plot predictions
ggplot(preds_stats, aes(y=tidytext::reorder_within(species, est, order),
                        x=est)) +
  # Facet
  facet_grid(order~., space="free_y", scales="free_y") +
  # Data
  geom_segment(mapping=aes(x=est_lo, xend=est_hi)) +
  geom_point() +
  # Labels
  labs(x="Depuration rate", y="") +
  scale_x_continuous(trans="log10",
                     breaks=c(0.0001, 0.001, 0.01, 0.1, 1),
                     labels=c("0.0001", "0.001", "0.01", "0.1", "1")) +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()

# Export predictions
################################################################################

# Save data
save(fit, preds_stats, file=file.path(outdir, "nested_regression.Rdata"))


