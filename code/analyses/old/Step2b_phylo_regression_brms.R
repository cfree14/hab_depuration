
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


# 1. Prep data
################################################################################

# Tip labels
pred_tip_labels <- pred_data_orig$tip.label
data_tip_labels <- data_orig$tip.label
all_tip_labels <- c(pred_tip_labels, data_tip_labels) %>% unique()

# Reduce tree to relevant species
tree <- ape::drop.tip(tree_orig, setdiff(tree_orig$tip.label, all_tip_labels)) #work to understand this better

# Levels on tree
length(tree$tip.label)
tree_levels <- tree$tip.label

# Subset data to species in tree
data <- data_orig %>% 
  # Reduce to species in tree
  filter(tip.label %in% tree_levels) %>% 
  # Log the depuration rate
  mutate(rate_d_log=abs(rate_d) %>% log(.)) %>% 
  # Create species column that matches tree (same names and order)
  mutate(species=factor(tip.label, levels=tree_levels))

# Subset prediction data to species in tree
pred_data <- pred_data_orig %>% 
  # Reduce to those in tree
  filter(tip.label %in% tree_levels) %>% 
  # Order based on tree
  mutate(tip.label=factor(tip.label, levels=tree_levels))

# Build phylogentic correlation matrix
Aphylo <- vcv(tree, corr = TRUE)  # correlation matrix
Aphylo <- Aphylo[tree_levels, tree_levels]


# 2. Fit model
################################################################################

# Mildly informative priors (tune as you like)
priors <- c(
  brms::prior(normal(0, 5), class = "Intercept"),
  brms::prior(normal(0, 2), class = "b"),          # fixed effects
  brms::prior(exponential(1), class = "sd"),       # random SDs
  brms::prior(exponential(1), class = "sigma")     # residual SD
)

# Fit model
fit <- brms::brm(
  formula = rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
    (1 | gr(species, cov = Aphylo)),
  data   = data,
  data2  = list(Aphylo = Aphylo),
  family = gaussian(),
  prior  = priors,
  chains = 4,
  cores  = 4,
  iter   = 4000,
  control = list(adapt_delta = 0.95)
)

# Inspect model
fit
summary(fit)
# plot(fit)

# Retrieve draws
draws <- brms::as_draws_df(fit)


# Diagnostics
################################################################################

# Checking observed data vs. simulated draws
bayesplot::pp_check(fit) # overall distribution
bayesplot::pp_check(fit, type = "scatter") # y vs. y_rep
bayesplot::pp_check(fit, type = "dens_overlay", group = "species")

# Conditional residuals on log-scale
resid_df <- tidybayes::add_residual_draws(fit, 
                                          newdata = data, 
                                          allow_new_levels = TRUE,
                                          ndraws = 200)

# Then plot residuals vs fitted, vs predictors, etc.
dev.off()
hist(resid_df$.residual)
plot(.residual ~ rate_d_log, resid_df)

loo_phy   <- loo::loo(fit)
plot(loo_phy)

# Inspect high leverage points
k <- loo_phy$diagnostics$pareto_k
bad <- which(k > 0.7)
data_high_k <- data[bad, ]   # inspect influential studies/species


# Compare to non-phylo model
################################################################################

fit_nophy <- brms::brm(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
    (1 | species),              # no Aphylo, uncorrelated RE
  data   = data,
  family = gaussian(),
  prior  = priors,
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95)
)

# Compare LOO (WAIC?)
# If the phylogenetic model has lower LOOIC (or WAIC) and the difference is non-trivial 
# (and SE not huge), that’s strong evidence that accounting for phylogeny 
# improves out-of-sample predictive performance.
# elpd_diff = difference in expected log predictive density (higher = better predictive performance)
loo_phy   <- loo::loo(fit)
loo_nophy <- loo::loo(fit_nophy)
loo::loo_compare(loo_phy, loo_nophy)


# R2 / variance explained
# Phylogeny doesn't improve R2 - they are statistically same
brms::bayes_R2(fit)
brms::bayes_R2(fit_nophy)


# Compute ICC
################################################################################

# Names will look something like:
# sd_species__Intercept and sigma
names(draws)[grepl("sd_", names(draws)) | names(draws) == "sigma"]

# Compute ICC posterior
icc_phylo <- with(draws, {
  var_species <- sd_species__Intercept^2
  var_resid   <- sigma^2
  var_species / (var_species + var_resid)
})

# ICC stats
summary(icc_phylo)
quantile(icc_phylo, probs = c(0.025, 0.5, 0.975))

# Quick posterior visualiation
icc_df <- tibble(icc=icc_phylo)
ggplot(icc_df, mapping=aes(y=icc, x="")) +
  geom_boxplot()
ggplot(icc_df, mapping=aes(y=icc, x="")) +
  geom_violin()
ggplot(icc_df, mapping=aes(x=icc)) +
  geom_density()

# Create nice one
ggplot(icc_df, mapping=aes(y=icc, x="")) +
  geom_violin(fill="grey90", draw_quantiles = c(0.5)) +
  # Label median
  # annotate(geom="text", )
  # Labels
  labs(x="", y="Phylogentic ICC") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))


# Species random effects
################################################################################

# Extract random effects
re_species <- phyr::ranef(fit)$species

# Inspect
dim(re_species)

# Format random effects
re <- re_species %>% 
  as.data.frame() %>% 
  # Add species
  mutate(species=rownames(.) %>% as.character()) %>% 
  mutate(species=gsub("_ott.*", "", species) %>% gsub("_", " ", .)) %>% 
  remove_rownames() %>% 
  select(species, everything()) %>% 
  # Rename
  setNames(c("species", "est_log", "est_log_sd", "est_log_lo", "est_log_hi")) %>% 
  # Exponentiate
  mutate(est=exp(est_log),
         est_lo=exp(est_log_lo),
         est_hi=exp(est_log_hi))
re

# Plot species RE
ggplot(re, aes(y=reorder(species, desc(est)), x=est)) +
  # geom_segment(mapping=aes(x=est_lo, xend=est_hi, y=species)) +
  geom_point() +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  # Theme
  theme_bw()


# Predict depuration rates for fitted species: lab vs. field
################################################################################

# Build data to predict to
pdata_lab <- data %>%
  select(order, family, genus, sci_name, species, lmax_cm, temp_c, k) %>%
  distinct() %>%
  mutate(study_type = "lab",
         tissue     = "soft tissue")
pdata_field <- pdata_lab %>% 
  mutate(study_type="field")
pdata <- bind_rows(pdata_lab, pdata_field)

# Make predictions
# Rows=random draws, Columns=species predicted to
preds_log <- brms::posterior_epred(
  fit,
  newdata    = pdata,
  allow_new_levels = T, 
  re_formula = NULL  # NULL= include all random effects (NA = include no group-level effects)
)
dim(preds_log)

# Exponentiate predictions
preds <- exp(preds_log)

# Compute posterior stats
preds_stats <- as_tibble(t(apply(preds, 2, function(x) {
  c(rate_d = mean(x), 
    rate_d_lo = quantile(x, 0.025), 
    rate_d_hi = quantile(x, 0.975))
}))) %>% 
  setNames(c("rate_d", "rate_d_lo", "rate_d_hi"))

# Merge stats with data
preds_full <- bind_cols(pdata, preds_stats)

# Order
preds_spp_order <- preds_full %>% 
  filter(study_type=="field") %>% 
  arrange(desc(rate_d))

# Plot
ggplot(preds_full, aes(x=rate_d, 
                       y=factor(sci_name, preds_spp_order$sci_name), 
                       color=study_type)) +
  geom_segment(mapping=aes(x=rate_d_lo, xend=rate_d_hi), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  scale_x_continuous(trans="log10") +
  # Legend
  scale_color_discrete(name="Rate type") +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())


# Run Pagels lambda and Blombergs k on random effects
################################################################################

# On random effects
#############################

# Trait
trait <- brms::ranef(fit)$species[, , "Intercept"][, "Estimate"]

# Blomberg's K
K <- phylosig(tree, trait , method = "K")
K

# Pagel’s lambda (max likelihood)
lambda <- phylosig(tree, trait , method = "lambda")
lambda

# On predicted rates
#############################

trait_df <- preds_full %>% 
  filter(study_type=="lab")
trait_tip_label <- trait_df$species
trait1 <- trait_df$rate_d 
names(trait1) <- trait_tip_label
trait1

# Blomberg's K
K <- phylosig(tree, trait1 , method = "K")
K

# Pagel’s lambda (max likelihood)
lambda <- phylosig(tree, trait1, method = "lambda")
lambda


# Predict depuration rates for unstudied species
################################################################################

# Build data to predict to
pdata_all_lab <- pred_data %>%
  select(order, family, genus, sci_name, tip.label, lmax_cm, temp_c, k) %>%
  distinct() %>%
  mutate(species=tip.label, 
         study_type = "lab",
         tissue     = "soft tissue")
pdata_all_field <- pdata_all_lab %>% 
  mutate(study_type="field")
pdata_all <- bind_rows(pdata_all_lab, pdata_all_field)

# Make predictions
# Rows=random draws, Columns=species predicted to
preds_all_log <- brms::posterior_epred(
  fit,
  newdata    = pdata_all,
  allow_new_levels=T, 
  re_formula = NULL # NULL= include all random effects (NA = include no group-level effects)
)
dim(preds_all_log)

# Exponentiate predictions
preds_all <- exp(preds_all_log)

# Compute posterior stats
preds_all_stats <- as_tibble(t(apply(preds_all, 2, function(x) {
  c(rate_d = mean(x), 
    rate_d_lo = quantile(x, 0.025), 
    rate_d_hi = quantile(x, 0.975))
}))) %>% 
  setNames(c("rate_d", "rate_d_lo", "rate_d_hi"))

# Merge stats with data
preds_all_full <- bind_cols(pdata_all, preds_all_stats)

# Order
preds_all_spp_order <- preds_all_full %>% 
  filter(study_type=="field") %>% 
  arrange(desc(rate_d))

# Plot
ggplot(preds_all_full, aes(x=rate_d, 
                       y=factor(sci_name, preds_all_spp_order$sci_name), 
                       color=study_type)) +
  geom_segment(mapping=aes(x=rate_d_lo, xend=rate_d_hi), position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Labels
  labs(x="Depuration rate (1/day)", y="") +
  scale_x_continuous(trans="log10") +
  # Legend
  scale_color_discrete(name="Rate type") +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())


# Export
################################################################################

# Export
save(fit, preds_all_full, file=file.path(outdir, "phylogenetic_regression.Rdata"))

