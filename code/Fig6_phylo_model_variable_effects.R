

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

# Load data
load(file=file.path(outdir, "phylogenetic_regression.Rdata"))


# Build data
################################################################################

# Best model
fit <- fit3

# Posterior draws of coefficients
draws <- brms::as_draws_df(fit)

# Format fixed effects
# posterior::summarize_draws(draws) handy for future reference
coefs_fe1 <- draws %>%
  # Beep only fixed effects (b_*)
  select(starts_with("b_")) %>%
  # Gather
  pivot_longer(cols = everything(), names_to = "term", values_to = "beta") %>%
  # Calculate stats by term
  group_by(term) %>%
  summarise(beta_med = median(beta),
            beta_lo  = quantile(beta, 0.025),
            beta_hi  = quantile(beta, 0.975),
            .groups = "drop") %>%
  # Convert to multiplicative & percent effects
  mutate(mult_med = exp(beta_med),
         mult_lo  = exp(beta_lo),
         mult_hi  = exp(beta_hi),
         pct_med  = (mult_med - 1) * 100,
         pct_lo   = (mult_lo  - 1) * 100,
         pct_hi   = (mult_hi  - 1) * 100) %>% 
  # Remove intercept
  filter(term!="b_Intercept") %>% 
  # Categorize terms
  mutate(term_catg=case_when(grepl("tissue", term) ~ "Tissue", 
                             grepl("study_type", term) ~ "Location",
                             T ~ "Life history")) %>% 
  # Format term
  mutate(term = stringr::str_remove(term, "^b_"),
         term=gsub("tissue|study_type", "", term),
         term=stringr::str_to_sentence(term),
         term=recode(term,
                     "Fieldnonmtoxicsite"="Field (non-toxic site)",
                     "Soft"="Soft tissue",
                     "Footpadductormuscles"="Foot and adductor muscle",
                     "Gillspmantle"="Gills and mantle",
                     "Gillspmantlepgonads"="Gills, mantle, and gonads", 
                     "Adductormuscle"="Adductor muscle",
                     "K"="Growth rate (1/yr)",
                     "Lmax_cm"="Max length (cm)",
                     "Temp_c"="Temperature (°C)")) %>% 
  # Arrange
  arrange(term_catg, term) %>% 
  select(term_catg, term, everything())

# Build refs
coefs_fe2 <- tibble(term_catg=c("Location", "Tissue"),
                   term=c("**Lab", "**Soft tissue"),
                   beta_med=c(0,0),
                   mult_med=c(1,1),
                   pct_med=c(0,0))

# Bind
coefs_fe <-bind_rows(coefs_fe1, coefs_fe2) %>% 
  # Direction
  mutate(direction=case_when(beta_med==0 ~ 'Reference',
                             beta_med<0 ~ "Decreases",
                             beta_med>0 ~ "Increases"),
         direction=factor(direction, levels=c("Decreases", "Reference", "Increases"))) %>% 
  # Significance
  mutate(significance=case_when(beta_lo>0 | beta_hi < 0 | beta_med==0 ~ "Significant",
                                T ~ "Non-significant"))
  

# Plot fixed effects
ggplot(coefs_fe, aes(x=pct_med, 
                     y=tidytext::reorder_within(term, pct_med, term_catg))) +
  # Facet
  facet_grid(term_catg~., space="free_y", scales="free_y") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Data
  geom_segment(mapping=aes(x=pct_lo, xend=pct_hi)) +
  geom_point() +
  # Labels
  labs(x="Percent change in depuration rate\n(per unit change)", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()

# Conditional effects of fixed effects
################################################################################

# Basically the marginal/partial effects

# Working with conditional effects
ce_temp <- brms::conditional_effects(fit, effects = "temp_c")
ce_lmax <- brms::conditional_effects(fit, effects = "lmax_cm")
ce_k <- brms::conditional_effects(fit, effects = "k")
brms::conditional_effects(fit, effects = "study_type")
brms::conditional_effects(fit, effects = "tissue")

# Try a combo - neato!
brms::conditional_effects(fit, effects = c("study_type:temp_c"))

# Visualize on log scale (current scale)
plot(ce_temp)
plot(ce_lmax)
plot(ce_k)

# Extract data
temp_df <- ce_temp$temp_c %>% 
  # Exponentiate
  mutate(rate_d = exp(estimate__),
         rate_d_lo = exp(lower__),
         rate_d_hi = exp(upper__))

# Visualize
ggplot(temp_df,
       aes(x = temp_c, y = rate_d)) +
  geom_ribbon(aes(ymin = rate_d_lo, ymax = rate_d_hi), alpha = 0.2) +
  geom_line() +
  labs(x = "Temperature (°C)",
       y = "Depuration rate (1/day)") +
  theme_bw()

temp_df <- ce_temp$temp_c %>% mutate(type="Temp")
lmax_df <- ce_lmax$lmax_cm %>% mutate(type="Lmax")
k_df <- ce_k$k %>% mutate(type="K")
lh_df <- bind_rows(temp_df, lmax_df, k_df) %>% 
  rename(effect=effect1__,
         est=estimate__,
         est_se=se__,
         est_lo=lower__,
         est_hi=upper__) %>% 
  mutate(rate_d=exp(est),
         rate_d_hi=exp(est_hi),
         rate_d_lo=exp(est_lo))

ggplot(lh_df, aes(x = effect, y = rate_d)) +
  # facet
  facet_wrap(~type, nrow=1, scales="free_x") +
  # Data
  geom_ribbon(aes(ymin = rate_d_lo, ymax = rate_d_hi), alpha = 0.2) +
  geom_line() +
  # Labels
  labs(x = "Value", y = "Depuration rate (1/day)") +
  # Theme
  theme_bw()


# Random effects
################################################################################


# Extract random effects
re_list <- brms::ranef(fit)

# Build df
x <- 1
re_df <- purrr::map_df(1:length(re_list), function(x){
  
  # Subset 
  type <- names(re_list[x])
  re <- re_list[x] %>% 
    # Convert to dataframe
    as.data.frame() %>% 
    # Rename
    set_names(c("est", "est_se", "est_lo", "est_hi")) %>% 
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
  filter(type!="Species") %>% 
  # Add multiplicative effects
  mutate(mult=exp(est),
         mult_lo=exp(est_lo),
         mult_hi=exp(est_hi)) %>% 
  # Add percent
  mutate(pct=(mult-1)*100,
         pct_lo=(mult_lo-1)*100,
         pct_hi=(mult_hi-1)*100) %>% 
  # Direction
  mutate(direction=case_when(est==0 ~ 'Reference',
                             est<0 ~ "Decreases rate",
                             est>0 ~ "Increases rate"),
         direction=factor(direction, levels=c("Decreases rate", "Reference", "Increases rate"))) %>% 
  # Significance
  mutate(significance=case_when(est_lo>0 | est_hi < 0 | est==0 ~ "Significant",
                                T ~ "Non-significant"))

# Plot RE (est scale)
ggplot(re, aes(y=tidytext::reorder_within(group, est, type), 
               x=est)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Data
  geom_segment(mapping=aes(x=est_lo, xend=est_hi)) +
  geom_point() +
  # Labels
  labs(x="Effect on log depuration rate)", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()

# Plot RE (est scale)
ggplot(re, aes(y=tidytext::reorder_within(group, mult, type), 
               x=mult)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  # Data
  geom_segment(mapping=aes(x=mult_lo, xend=mult_hi)) +
  geom_point() +
  # Labels
  labs(x="Multiplicative effect on log depuration rate", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()

# Plot RE (est scale)
ggplot(re, aes(y=tidytext::reorder_within(group, pct, type), 
               x=mult)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  # Data
  geom_segment(mapping=aes(x=pct_lo, xend=pct_hi)) +
  geom_point() +
  # Labels
  labs(x="Percent effect on log depuration rate", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw()


# Official plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot fixed effects
g1 <- ggplot(coefs_fe, aes(x=beta_med, 
                           y=tidytext::reorder_within(term, beta_med, term_catg),
                           color=direction,
                           alpha=significance)) +
  # Facet
  facet_grid(term_catg~., space="free_y", scales="free_y") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Data
  geom_segment(mapping=aes(x=beta_lo, xend=beta_hi)) +
  geom_point() +
  # Labels
  labs(x="Change in log depuration rate\n(per unit change)", y="", tag="A", title="Fixed effects") +
  tidytext::scale_y_reordered() +
  # Legend
  scale_color_manual(name="Impact on depuration rate", values=c("red", "black", "blue"), 
                     guide = guide_legend(title.position = "top")) +
  scale_alpha_manual(name="", values=c(0.4, 1), guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.title.align = 0.5,
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot RE (est scale)
g2 <- ggplot(re, aes(y=tidytext::reorder_within(group, est, type), 
                     x=est,
                     color=direction,
                     alpha=significance)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Data
  geom_segment(mapping=aes(x=est_lo, xend=est_hi)) +
  geom_point() +
  # Labels
  labs(x="Effect on log depuration rate\n", y="", tag="B", title="Random effects") +
  tidytext::scale_y_reordered() +
  # Legend
  scale_color_manual(name="", values=c("red", "black", "blue"), drop=F) +
  scale_alpha_manual(name="", values=c(0.4, 1), guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Combine
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_phylo_model_effects.png"), 
       width=6.5, height=4.75, units="in", dpi=600)

