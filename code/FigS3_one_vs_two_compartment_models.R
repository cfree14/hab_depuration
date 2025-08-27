
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# ------------------------------------------------------------
# Biotoxin depuration: simulate, fit 1- vs 2-compartment models,
# compare with AICc, and plot fits by dataset.
# ------------------------------------------------------------

# Packages
library(tidyverse)
library(minpack.lm)   # for nlsLM (more robust than nls)

set.seed(123)

# -----------------------
# 1) Simulate data
# -----------------------

# Time grid (days)
t <- 0:30

# One-compartment truth: C(t) = C0 * exp(-k * t)
C0_1 <- 100
k1_true <- 0.18
y1_true <- C0_1 * exp(-k1_true * t)

# Two-compartment truth: C(t) = A*exp(-k_fast*t) + B*exp(-k_slow*t)
A_true  <- 60
B_true  <- 40
k_fast  <- 0.50
k_slow  <- 0.05
y2_true <- A_true * exp(-k_fast * t) + B_true * exp(-k_slow * t)

# Add multiplicative log-normal noise (keeps values positive)
# (~ 15% CV)
noise_sd <- 0.15
y1_obs <- y1_true * exp(rnorm(length(t), mean = 0, sd = noise_sd))
y2_obs <- y2_true * exp(rnorm(length(t), mean = 0, sd = noise_sd))

dat <- bind_rows(
  tibble(dataset = "One-compartment data", t = t, y = y1_obs),
  tibble(dataset = "Two-compartment data", t = t, y = y2_obs)
)

# -----------------------
# 2) Model fitting helpers
# -----------------------

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

# AICc (Gaussian error) from RSS and p
# k = p + 1 to account for residual variance
aicc_from_fit <- function(fit, n) {
  p   <- length(coef(fit))
  k   <- p + 1
  RSS <- sum(resid(fit)^2)
  # AIC without constants (constants cancel in comparisons)
  AIC_val  <- n * log(RSS / n) + 2 * k
  AICc_val <- AIC_val + (2 * k * (k + 1)) / (max(n - k - 1, 1e-8))
  tibble(p = p, k = k, RSS = RSS, AIC = AIC_val, AICc = AICc_val)
}

# Safe fit wrapper (returns NULL on failure)
safe_fit <- function(f, df) {
  tryCatch(f(df), error = function(e) NULL)
}

# -----------------------
# 3) Fit both models to each dataset & compare by AICc
# -----------------------

fits <- dat %>%
  dplyr::group_by(dataset) %>%
  dplyr::group_map(~{
    df <- .x
    label <- .y$dataset[[1]]  # <- use .y, not df$dataset
    
    fit1 <- safe_fit(fit_one_comp, df)
    fit2 <- safe_fit(fit_two_comp, df)
    
    out <- dplyr::bind_rows(
      if (!is.null(fit1)) aicc_from_fit(fit1, n = nrow(df)) %>%
        dplyr::mutate(dataset = label, model = "One-compartment"),
      if (!is.null(fit2)) aicc_from_fit(fit2, n = nrow(df)) %>%
        dplyr::mutate(dataset = label, model = "Two-compartment")
    )
    
    out
  }) %>%
  list_rbind() %>%
  dplyr::group_by(dataset) %>%
  dplyr::mutate(
    dAICc = AICc - min(AICc),
    weight = exp(-0.5 * dAICc) / sum(exp(-0.5 * dAICc))
  ) %>%
  dplyr::ungroup()

print(dplyr::arrange(fits, dataset, AICc))

# -----------------------
# 4) Predictions for plotting
# -----------------------

# Refit (to keep the objects for predict()), then create a tidy grid
pred_grid <- tibble(t = seq(min(t), max(t), length.out = 300))

preds <- dat %>%
  group_by(dataset) %>%
  group_modify(~{
    df <- .x
    # fit both models
    fit1 <- safe_fit(fit_one_comp, df)
    fit2 <- safe_fit(fit_two_comp, df)
    
    # predictions (if fit exists)
    p1 <- if (!is.null(fit1)) {
      tibble(t = pred_grid$t,
             fit = as.numeric(predict(fit1, newdata = pred_grid)),
             model = "One-compartment")
    } else tibble()
    
    p2 <- if (!is.null(fit2)) {
      tibble(t = pred_grid$t,
             fit = as.numeric(predict(fit2, newdata = pred_grid)),
             model = "Two-compartment")
    } else tibble()
    
    bind_rows(p1, p2)
  }) %>%
  ungroup()

# -----------------------
# 5) Plot: data + both model fits, faceted by dataset
# -----------------------

ggplot(dat, aes(x = t, y = y)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(data = preds, aes(y = fit, color = model), linewidth = 1) +
  facet_wrap(~ dataset, scales = "free_y") +
  labs(x = "Days",
       y = "Toxin concentration (arbitrary units)",
       color = "Fitted model",
       title = "One- vs Two-compartment depuration: data and model fits",
       subtitle = "Each dataset is fit by both models; AICc selects the more parsimonious model") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
