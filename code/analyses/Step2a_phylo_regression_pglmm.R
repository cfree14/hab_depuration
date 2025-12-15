
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


# Step 1. Prepare data
################################################################################

# Format data
data <- data_orig %>% 
  rename(species=sci_name) %>% 
  mutate(rate_d_log=abs(rate_d) %>% log())

# Identify intersecting species
common_spp <- intersect(data$tip.label, tree_orig$tip.label)

# Reduce tree to species with data
tree <- ape::drop.tip(tree_orig, setdiff(tree_orig$tip.label, common_spp))

# Tree levels
tree_levels <- tree$tip.label

# Reduce data to species in tree
data1 <- data %>% 
  filter(tip.label %in% common_spp) %>% 
  mutate(tip.label=factor(tip.label, levels=tree_levels))


# Step 2. Fit models
################################################################################

# M0: fixed-effects only
fit_M0 <- glm(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k,
  data   = data1,
  family = "gaussian"
)

# M1: non-phylogenetic species RE
fit_M1 <- pglmm(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
    (1 | tip.label) + (1 | paper_id),
  data   = data1,
  family = "gaussian",
  REML = FALSE
)

# M2: phylo + non-phylo species RE
# species__ creates both a non-phylo and a phylo RE term
fit_M2 <- pglmm(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k +
    (1 | tip.label__),
  data      = data1,
  family    = "gaussian",
  cov_ranef = list(tip.label = tree),
  REML = FALSE
)

# M3: phylo species RE
##################################

# build a covariance matrix from the tree and reorder to match data
Vsp <- vcv(tree, corr = TRUE)      # or without corr = TRUE if you prefer
Vsp <- Vsp[tree_levels, tree_levels] # make sure ordering matches

# phylogenetic random intercept only
re_phylo <- list(
  1,                    # random intercept (design matrix)
  sp    = data1$tip.label,  # grouping factor
  covar = Vsp               # phylogenetic covariance
)

# M3: phylo species RE
fit_M3 <- pglmm(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k,
  data      = data1,
  family    = "gaussian",
  random.effects = list(re_phylo),
  REML = FALSE
)

# M4: phylo species RE
fit_M4 <- lme4::lmer(
  rate_d_log ~ study_type + tissue + temp_c + lmax_cm + k + (1 | order/family),
  data      = data1,
  REML = FALSE
)

fit_M4 
VarCorr(fit_M4)
lme4::isSingular(fit_M4, tol = 1e-05)



# Step 3. Compare models
################################################################################

# Function to count number of parameters
count_params_pglmm <- function(model) {
  k_fixed <- length(model$B)            # fixed effects
  k_re    <- length(model$s2r)              # random-effect variances
  k_resid <- 1                              # Gaussian residual variance
  k_total <- k_fixed + k_re + k_resid
  tibble(
    fixed_effects = k_fixed,
    random_effects = k_re,
    residual_var = k_resid,
    total_params = k_total
  )
}
count_params_glm <- function(model){
  k_fixed <- length(coef(model))            # fixed effects
  k_re    <- 0              # random-effect variances
  k_resid <- 1                              # Gaussian residual variance
  k_total <- k_fixed + k_re + k_resid
  tibble(
    fixed_effects = k_fixed,
    random_effects = k_re,
    residual_var = k_resid,
    total_params = k_total
  )
}

nparams <- bind_rows(count_params_glm(fit_M0),
                      count_params_pglmm(fit_M1),
                      count_params_pglmm(fit_M2),
                      count_params_pglmm(fit_M3))

models <- tibble(model=c("M0-fixed", "M1-non-phylo RE", "M2-both RE", "M3-phylo RE", "M4-Nested"),
                 # ncoef=nparams$total_params,
                 logLik = c(logLik(fit_M0), fit_M1$logLik, fit_M2$logLik, fit_M3$logLik, logLik(fit_M4)),
                 aic=c(AIC(fit_M0), fit_M1$AIC, fit_M2$AIC, fit_M3$AIC, AIC(fit_M4)),
                 bic=c(BIC(fit_M0), fit_M1$BIC, fit_M2$BIC, fit_M3$BIC, BIC(fit_M4))) %>% 
  arrange(aic)
models


# Step 3. Inspect model
################################################################################

# Phylogenetic ICC (heritability)
#######################################

# 1|tip.label = non-phylo
# 1|tip.label__ = phylo

# Extract variance components
var_species <- fit$s2r        # variance of 1|species
var_resid   <- fit$s2resid    # residual variance

# Phylogenetic ICC (heritability)
icc_phylo <- var_species / (var_species + var_resid)
icc_phylo


# Step 4. Make predictions
################################################################################

# Build data to predict to
pdata_lab <- data %>%
  select(order, family, genus, species, lmax_cm, temp_c, k) %>%
  distinct() %>%
  mutate(study_type = "lab",
         tissue     = "soft tissue")
pdata_field <- pdata_lab %>% 
  mutate(study_type="field")
pdata <- bind_rows(pdata_lab, pdata_field)

# Proof that predict.communityPGLMM does not currently support predictions
predict(object=fit, 
        newdata=pdata)



