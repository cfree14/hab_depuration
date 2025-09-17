

# Fit 1 compartment model
fit_1comp <- function(time, toxicity){
  
  # Fit model
  model <- lm(log(toxicity) ~ time)
  
  # Inspect coefficients and metrics
  coefs = broom::tidy(model) # model coefficients
  stats = broom::glance(model) # model fit metrics
  
  # Build out
  df <- tibble(n0=exp(coef(model)[1]),
               k=coef(model)[2],
               r2=stats$r.squared,
               r2_adj=stats$adj.r.squared,
               pvalue=stats$p.value,
               nobs=stats$nobs,
               df=stats$df,
               nll=stats$logLik,
               aic=stats$AIC)
  
  # Return
  return(df)
  
}
