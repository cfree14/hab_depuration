
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
data <- readxl::read_excel(file.path(indir, "Chen_Chou_2002_Fig5.xlsx")) %>% 
  filter(day>=17) %>% 
  mutate(day=day-min(day))


# Helper functions
################################################################################


# Plot fit
plot_fit <- function(fit){

  # Extract data
  data <- fit$data

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

  print(g)

}

# Fit 1-compartment model
# time <- data$day; toxicity <- data$toxicity; plot <- T
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

  # Add data to fit
  fit$data <- df

  # Plot quickly
  if(plot==T){
    plot_fit(fit=fit)
  }

  # Return
  return(fit)

}

# Test function
time <- data$day; toxicity <- data$toxicity
model <- fit_1comp_model(time=time, toxicity=toxicity, plot=T)

# Fit two-compartment model
fit_2comp_model <- function(time, toxicity, plot=F){

  # Starting parameters
  start_A  <- 0.6 * max(toxicity)
  start_B  <- 0.4 * max(toxicity)
  start_k1 <- 0.4     # "fast"
  start_k2 <- 0.05    # "slow"

  # Build data
  df <- tibble(time=time,
               toxicity=toxicity)

  # Fit model
  fit <- nlsLM(
    toxicity ~ A * exp(-k1 * time) + B * exp(-k2 * time),
    data  = df,
    start = list(A = start_A, B = start_B, k1 = start_k1, k2 = start_k2),
    lower = c(0, 0, 1e-8, 1e-8)
  )

  # Add data to fit
  fit$data <- df

  # Plot quickly
  if(plot==T){
    plot_fit(fit=fit)
  }

  # Return
  return(fit)

}

# Fit models
model1 <- fit_1comp_model(time=time, toxicity=toxicity, plot=T)
model2 <- fit_2comp_model(time=time, toxicity=toxicity, plot=T)

# Compute AICs
model <- model1
aicc_from_fit <- function(model) {

  # Number of data points
  n <- nrow(model$data)

  # AICc (Gaussian error) from RSS and p
  # k = p + 1 to account for residual variance
  p   <- length(coef(model))
  k   <- p + 1
  RSS <- sum(resid(model)^2)

  # AIC without constants (constants cancel in comparisons)
  AIC_val  <- n * log(RSS / n) + 2 * k
  AICc_val <- AIC_val + (2 * k * (k + 1)) / (max(n - k - 1, 1e-8))

  # Collate
  tibble(n=n, p = p, k = k, RSS = RSS, AIC = AIC_val, AICc = AICc_val)

}

# Compare models
compare_models <- function(model1, model2){

  # Extract data
  data <- model1$data

  # Make predictions
  times <- seq(min(data$time), max(data$time), length.out=200)
  preds1 <- predict(model1, newdata=tibble(time=times))
  preds2 <- predict(model2, newdata=tibble(time=times))
  preds <- tibble(time=times,
                  toxicity1=preds1,
                  toxicity2=preds2) %>%
    gather(key="model", value="toxicity", 2:3) %>%
    mutate(model=recode(model,
                        "toxicity1"="1-compartment",
                        "toxicity2"="2-compartment"))

  # Plot data
  g <- ggplot(data, aes(x=time, y=toxicity)) +
    geom_point() +
    geom_line(data=preds, mapping=aes(color=model)) +
    # Labels
    labs(x="Time", y="Toxicity") +
    # Legend
    scale_color_discrete(name="Model type") +
    # Theme
    theme_bw()
  g
  print(g)

  # Compute AIC
  stats1 <- aicc_from_fit(model1) %>% mutate(model="1-compartment")
  stats2 <- aicc_from_fit(model2) %>% mutate(model="2-compartment")

  # Merge
  stats <- bind_rows(stats1, stats2) %>%
    select(model, everything()) %>%
    mutate(delta_AICc=AICc-min(AICc)) %>%
    arrange(delta_AICc)

  # Return
  return(stats)

}

compare_models(model1, model2)











# LM approach
################################################################################

# Plot fit
plot_fit <- function(fit){

  # Extract data
  df <- fit$data

  # Make predictions
  times <- seq(min(df$time), max(df$time), length.out=200)
  newdata <- tibble(time=times)
  toxicity_pred <- predict(fit, newdata = newdata)
  preds <- tibble(time=times,
                  toxicity=exp(toxicity_pred))

  # Plot data and fit
  g <- ggplot(df, mapping=aes(x=time, y=toxicity)) +
    geom_point() +
    geom_line(data=preds) +
    # Labels
    labs(x="Time", y="Toxicity") +
    # Theme
    theme_bw()
  g

  print(g)

}

# Fit 1-compartment model
# time <- data$day; toxicity <- data$toxicity; plot <- T
fit_1comp_model <- function(time, toxicity, plot=F){
  
  # Starting parameters
  n0_start <- max(toxicity)             
  k_start  <- 0.2
  
  # Build data
  df <- tibble(time=time,
               toxicity=toxicity)
  
  # Fit model
  fit <-  lm(log(toxicity) ~ time, data = df)
  
  # Add data to fit
  fit$data <- df
  
  # Plot quickly
  if(plot==T){
    plot_fit(fit=fit)
  }
  
  # Return
  return(fit)
  
}

# Test function
time <- data$day; toxicity <- data$toxicity
model <- fit_1comp_model(time=time, toxicity=toxicity, plot=T)




# Fit two-compartment model
fit_2comp_model <- function(time, toxicity, plot=F){
  
  # Build data
  df <- tibble(time=time,
               toxicity=toxicity)
  
  # Starting parameters
  start_A  <- 0.6 * max(toxicity)
  start_B  <- 0.4 * max(toxicity)
  start_k1 <- 0.4     # "fast"
  start_k2 <- 0.05    # "slow"
  
  # Fit model
  fit <- nlsLM(
    log(toxicity) ~ log(a*exp(-k1*time) + b*exp(-k2*time)),
    data = df,
    start = list(a = start_A, b = start_B, k1 = start_k1, k2 = start_k2),
    lower = c(a=1e-12, b=1e-12, k1=1e-6, k2=1e-6),
    control = nls.lm.control(maxiter = 500, ptol = 1e-10, ftol = 1e-10)
  )
  
  # Add data to fit
  fit$data <- df
  
  # Plot quickly
  if(plot==T){
    plot_fit(fit=fit)
  }
  
  # Return
  return(fit)
  
}

# Fit models
model1 <- fit_1comp_model(time=time, toxicity=toxicity, plot=T)
model2 <- fit_2comp_model(time=time, toxicity=toxicity, plot=T)

AIC(model1)
AIC(model2)

AICcmodavg::AICc(model1)
AICcmodavg::AICc(model2)

# Compute AICs
model <- model2
aicc_from_fit <- function(model) {
  
  # Number of data points
  n <- nrow(model$data)
  
  # AICc (Gaussian error) from RSS and p
  # k = p + 1 to account for residual variance
  p   <- length(coef(model))
  k   <- p + 1
  RSS <- sum(resid(model)^2)
  
  # AIC without constants (constants cancel in comparisons)
  AIC_val  <- n * log(RSS / n) + 2 * k
  AICc_val <- AIC_val + (2 * k * (k + 1)) / (max(n - k - 1, 1e-8))
  
  # Collate
  tibble(n=n, p = p, k = k, RSS = RSS, AIC = AIC_val, AICc = AICc_val)
  
}

# Compare models
compare_models <- function(model1, model2){
  
  # Extract data
  data <- model1$data
  
  # Make predictions
  times <- seq(min(data$time), max(data$time), length.out=200)
  preds1 <- predict(model1, newdata=tibble(time=times)) %>% exp()
  preds2 <- predict(model2, newdata=tibble(time=times)) %>% exp()
  preds <- tibble(time=times,
                  toxicity1=preds1,
                  toxicity2=preds2) %>% 
    gather(key="model", value="toxicity", 2:3) %>% 
    mutate(model=recode(model,
                        "toxicity1"="1-compartment",
                        "toxicity2"="2-compartment"))
  
  # Plot data
  g <- ggplot(data, aes(x=time, y=toxicity)) +
    geom_point() + 
    geom_line(data=preds, mapping=aes(color=model)) +
    # Labels
    labs(x="Time", y="Toxicity") +
    # Legend
    scale_color_discrete(name="Model type") +
    # Theme
    theme_bw()
  g
  print(g)
  
  # Compute AIC
  stats1 <- aicc_from_fit(model1) %>% mutate(model="1-compartment")
  stats2 <- aicc_from_fit(model2) %>% mutate(model="2-compartment")
  
  # Merge
  stats <- bind_rows(stats1, stats2) %>% 
    select(model, everything()) %>% 
    mutate(delta_AICc=AICc-min(AICc)) %>% 
    arrange(delta_AICc)
  
  # Return
  return(stats)
  
}

compare_models(model1, model2)




