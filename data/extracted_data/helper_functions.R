

# Fit 1 compartment model
fit_1comp_lm <- function(time, toxicity){
  
  # Fit model
  model <- lm(log(toxicity) ~ time)
  
  # Return
  return(model)
  
}

get_coefs_1_lm <- function(model){
  
  # Inspect coefficients and metrics
  coefs = broom::tidy(model) # model coefficients
  stats = broom::glance(model) # model fit metrics
  
  # Build out
  df <- tibble(n0_lm=exp(coef(model)[1]),
               k_lm=coef(model)[2],
               r2=stats$r.squared,
               r2_adj=stats$adj.r.squared,
               pvalue=stats$p.value,
               nobs=stats$nobs,
               df=stats$df,
               nll=stats$logLik,
               aic=stats$AIC)
  return(df)
  
}

get_coefs_1_nls <- function(model){
  df <- tibble(N0=coef(model)[1],
               k=coef(model)[2])
}

get_coefs_2_nls <- function(model){
  df <- tibble(N1=coef(model)[1],
               N2=coef(model)[2],
               k1=coef(model)[3]*-1,
               k2=coef(model)[4]*-1)
}


# One-compartment model: y = N0 * exp(-k * t)
fit_one_comp <- function(time, toxicity) {
  start_N0 <- max(toxicity)              # sensible start
  start_k  <- -0.2
  df <- tibble(time=time,
               toxicity=toxicity)
  minpack.lm::nlsLM(
    toxicity ~ N0 * exp(k * time),
    data  = df,
    start = list(N0 = start_N0, k = start_k)
  )
}

# Two-compartment model: y = A*exp(-k1*t) + B*exp(-k2*t)
fit_two_comp <- function(time, toxicity) {
  
  # Build dataframe
  df <- tibble(time=time,
               toxicity=toxicity)
  
  # Fit 1-compartment model to help set initial values
  model <- lm(log(toxicity) ~ time)
  N0 <- coef(model)[1]
  k <- abs(coef(model)[2])
  
  # Set starts
  start_N1  <- 0.7 * N0
  start_N2  <- 0.3 * N0
  start_k1 <- k * 1.3     # "fast"
  start_k2 <- k *0.7    # "slow"
  
  # Fit model
  model <- minpack.lm::nlsLM(
    toxicity ~ N1 * exp(-k1 * time) + N2 * exp(-k2 * time),
    data  = df,
    start = list(N1 = start_N1, N2 = start_N2, k1 = start_k1, k2 = start_k2),
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
  tibble(p = p, RSS = RSS, AIC = AIC_val, AICc = AICc_val)
}
