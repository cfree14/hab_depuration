# Load libraries
library(tidyverse)
library(minpack.lm)  # for robust nls fitting
library(broom)

# Read data
data <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/hab_depuration/data/extracted_data/raw/Kim_etal_2018_Fig1.xlsx")

# Filter to depuration phase
depuration <- data %>%
  filter(phase == "depuration")

# Log-transform for model comparison
depuration <- depuration %>%
  mutate(log_mc_lr = log(mc_lr_mg_g_dw))

# Plot observed depuration data
ggplot(depuration, aes(x = day, y = mc_lr_mg_g_dw,
                       color = factor(concentration_mg_L))) +
  geom_point() +
  geom_line() +
  facet_wrap(~species) +
  scale_y_log10() +
  labs(x = "Day", y = "MC-LR (mg/g dw, log scale)", color = "Concentration (mg/L)") +
  theme_minimal()

# ---- Fit exponential decay model: C(t) = C0 * exp(-k * t) ----

fit_decay_models <- depuration %>%
  group_by(species, concentration_mg_L) %>%
  nest() %>%
  mutate(model = map(data, ~ try(nlsLM(mc_lr_mg_g_dw ~ C0 * exp(-k * day),
                                       data = .x,
                                       start = list(C0 = max(.x$mc_lr_mg_g_dw), k = 0.5),
                                       control = nls.lm.control(maxiter = 500)),
                                 silent = TRUE)),
         tidy_model = map(model, ~ if (inherits(.x, "try-error")) NULL else tidy(.x))) %>%
  unnest(tidy_model) %>%
  filter(term == "k") %>%
  rename(decay_constant = estimate)

# Print decay constants
print(fit_decay_models)

# ---- Overlay fitted curves on plot ----

# Generate predictions
predictions <- depuration %>%
  group_by(species, concentration_mg_L) %>%
  do({
    dat <- .
    fit <- try(nlsLM(mc_lr_mg_g_dw ~ C0 * exp(-k * day),
                     data = dat,
                     start = list(C0 = max(dat$mc_lr_mg_g_dw), k = 0.5),
                     control = nls.lm.control(maxiter = 500)), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)
    tibble(
      day = seq(min(dat$day), max(dat$day), length.out = 100),
      pred = predict(fit, newdata = data.frame(day = seq(min(dat$day), max(dat$day), length.out = 100))),
      species = dat$species[1],
      concentration_mg_L = dat$concentration_mg_L[1]
    )
  }) %>% bind_rows()

# Plot with fitted curves
ggplot(depuration, aes(x = day, y = mc_lr_mg_g_dw,
                       color = factor(concentration_mg_L))) +
  geom_point() +
  geom_line(data = predictions, aes(y = pred), linetype = "dashed") +
  facet_wrap(~species) +
  labs(x = "Day", y = "MC-LR (mg/g dw)", color = "Concentration (mg/L)") +
  theme_minimal()

# ---- Statistical comparison of decay constants ----

# Use linear model on log-transformed data for comparison
comparison_model <- lm(log_mc_lr ~ day * species * factor(concentration_mg_L), data = depuration)
summary(comparison_model)
