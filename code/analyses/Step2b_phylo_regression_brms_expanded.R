
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
tabledir <- "tables"

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


# 2. Fit models
################################################################################

# Priors for FE model
priors_fe <- c(
  brms::prior(normal(0, 5), class = "Intercept"),
  brms::prior(normal(0, 2), class = "b"),          # fixed effects
  brms::prior(exponential(1), class = "sigma")     # residual SD
)

# Priors for RE models
priors_re <- c(
  brms::prior(normal(0, 5), class = "Intercept"),
  brms::prior(normal(0, 2), class = "b"),          # fixed effects
  brms::prior(exponential(1), class = "sd"),       # random SDs
  brms::prior(exponential(1), class = "sigma")     # residual SD
)

fit <- F
if(fit){

  # Fixed effects model
  fit0 <- brms::brm(
    formula = rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k,
    data   = data,
    data2  = list(Aphylo = Aphylo),
    family = gaussian(),
    prior  = priors_fe,
    chains = 4,
    cores  = 4,
    iter   = 4000,
    control = list(adapt_delta = 0.95)
  )
  summary(fit0)
  
  # Phylogenetic model
  fit1 <- brms::brm(
    formula = rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
      (1 | gr(species, cov = Aphylo)),
    data   = data,
    data2  = list(Aphylo = Aphylo),
    family = gaussian(),
    prior  = priors_re,
    chains = 4,
    cores  = 4,
    iter   = 4000,
    control = list(adapt_delta = 0.95)
  )
  summary(fit1)
  
  # Non-phylogenetic model
  fit2 <- brms::brm(
    rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
      (1 | species),              # no Aphylo, uncorrelated RE
    data   = data,
    family = gaussian(),
    prior  = priors_re,
    chains = 4, cores = 4, iter = 4000,
    control = list(adapt_delta = 0.95)
  )
  summary(fit2)
  
  # Fit model
  fit3 <- brms::brm(
    formula =  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k + (1 | order/family/genus/species),
    data    = data,
    family  = gaussian(),
    prior   = priors_re,
    chains  = 4,
    cores   = 4,
    iter    = 4000,
    control = list(adapt_delta = 0.95)
  )
  summary(fit3)
  
  # Export models
  save(fit0, fit1, fit2, fit3, file=file.path(outdir, "phylogenetic_models.Rdata"))

}else{
  
  # Read models
  load(file.path(outdir, "phylogenetic_models.Rdata"))
  
}

# Compare models
################################################################################

# Compare LOO (WAIC?)
# If the phylogenetic model has lower LOOIC (or WAIC) and the difference is non-trivial 
# (and SE not huge), that’s strong evidence that accounting for phylogeny 
# improves out-of-sample predictive performance.
# elpd_diff = difference in expected log predictive density (higher = better predictive performance)
loo0   <- loo::loo(fit0)
loo1 <- loo::loo(fit1)
loo2 <- loo::loo(fit2)
loo3 <- loo::loo(fit3)
loo::loo_compare(loo0, loo1, loo2, loo3)

# R2 / variance explained
model_list <- list(fit0, fit1, fit2, fit3)
x <-1
model_stats_orig <- purrr::map_df(1:length(model_list), function(x){
  
  # Model
  model_do <- model_list[[x]]
  
  # Compute LOO stats
  loo_stats <- loo::loo(model_do)
  loo_stats$estimates
  
  # Compute R2
  r2_stats <- brms::bayes_R2(model_do) %>% 
    as_tibble() %>% 
    setNames(c("r2", "r2_se", "r2_lo", "r2_hi")) %>% 
    # Add Model
    mutate(model=paste("Model", x-1)) %>% 
    # Add LOO stats
    mutate(elpd_loo=loo_stats$estimates[1, "Estimate"],
           elpd_loo_sd=loo_stats$estimates[1, "SE"],
           p_loo=loo_stats$estimates[2, "Estimate"],
           p_loo_sd=loo_stats$estimates[2, "SE"],
           looic=loo_stats$estimates[3, "Estimate"],
           looic_loo_sd=loo_stats$estimates[3, "SE"])
  
})

# Build model stats
model_stats <- model_stats_orig %>% 
  # Arrange
  select(model, everything()) %>% 
  # Rename
  mutate(model=recode(model,
                      "Model 0"="Fixed effects only",
                      "Model 1"="Phylogenetic random effects",
                      "Model 2"="Non-phylogenetic random effects",
                      "Model 3"="Taxanomically nested random effects"))
model_stats

# Export
write.csv(model_stats, file=file.path(tabledir, "TableSX_phylo_model_comparison.csv"), row.names=F)



# Diagnostics on best model
################################################################################

# Select best model
fit <- fit3

# Checking observed data vs. simulated draws
bayesplot::pp_check(fit2) # overall distribution
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

# Recalculate loo
loo_final   <- loo::loo(fit)
plot(loo_final)

# Inspect high leverage points
k <- loo_final$diagnostics$pareto_k
bad <- which(k > 0.7)
data_high_k <- data[bad, ]   # inspect influential studies/species


# Compute ICC
################################################################################

# # Retrieve draws
# draws <- brms::as_draws_df(fit)
# 
# # Names will look something like:
# # sd_species__Intercept and sigma
# names(draws)[grepl("sd_", names(draws)) | names(draws) == "sigma"]
# 
# # Compute ICC posterior
# icc_phylo <- with(draws, {
#   var_species <- sd_species__Intercept^2
#   var_resid   <- sigma^2
#   var_species / (var_species + var_resid)
# })
# 
# # ICC stats
# summary(icc_phylo)
# quantile(icc_phylo, probs = c(0.025, 0.5, 0.975))
# 
# # Quick posterior visualiation
# icc_df <- tibble(icc=icc_phylo)
# ggplot(icc_df, mapping=aes(y=icc, x="")) +
#   geom_boxplot()
# ggplot(icc_df, mapping=aes(y=icc, x="")) +
#   geom_violin()
# ggplot(icc_df, mapping=aes(x=icc)) +
#   geom_density()
# 
# # Create nice one
# ggplot(icc_df, mapping=aes(y=icc, x="")) +
#   geom_violin(fill="grey90", draw_quantiles = c(0.5)) +
#   # Label median
#   # annotate(geom="text", )
#   # Labels
#   labs(x="", y="Phylogentic ICC") +
#   scale_y_continuous(labels=scales::percent_format()) +
#   # Theme
#   theme_bw() +
#   theme(axis.text.y = element_text(angle = 90, hjust = 0.5))


# Species random effects
################################################################################

# # Extract random effects
# re_species <- phyr::ranef(fit)$species
# 
# # Inspect
# dim(re_species)
# 
# # Format random effects
# re <- re_species %>% 
#   as.data.frame() %>% 
#   # Add species
#   mutate(species=rownames(.) %>% as.character()) %>% 
#   mutate(species=gsub("_ott.*", "", species) %>% gsub("_", " ", .)) %>% 
#   remove_rownames() %>% 
#   select(species, everything()) %>% 
#   # Rename
#   setNames(c("species", "est_log", "est_log_sd", "est_log_lo", "est_log_hi")) %>% 
#   # Exponentiate
#   mutate(est=exp(est_log),
#          est_lo=exp(est_log_lo),
#          est_hi=exp(est_log_hi))
# re
# 
# # Plot species RE
# ggplot(re, aes(y=reorder(species, desc(est)), x=est)) +
#   # geom_segment(mapping=aes(x=est_lo, xend=est_hi, y=species)) +
#   geom_point() +
#   # Labels
#   labs(x="Depuration rate (1/day)", y="") +
#   # Theme
#   theme_bw()


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
  scale_x_continuous(trans="log10",
                     breaks=c(0.001, 0.01, 0.1, 1)) +
  # Legend
  scale_color_discrete(name="Rate type") +
  # Theme
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())


# Run Pagels lambda and Blombergs k on estimated values (for curiousity)
################################################################################

# On random effects
#############################

# # Trait
# trait <- brms::ranef(fit)$species[, , "Intercept"][, "Estimate"]
# 
# # Blomberg's K
# K <- phylosig(tree, trait , method = "K")
# K
# 
# # Pagel’s lambda (max likelihood)
# lambda <- phylosig(tree, trait , method = "lambda")
# lambda

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
save(tree, data, pred_data, 
     fit0, fit1, fit2, fit3, preds_full, preds_all_full, 
     file=file.path(outdir, "phylogenetic_regression.Rdata"))

