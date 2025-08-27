
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"


# Build data
################################################################################

# Decay params
n0 <- 120
k <- -0.03

# Timeline
days <- 0:120
toxs <- n0*exp(k*days)
data <- tibble(day=days,
               toxicity=toxs)

# Sampling scheme
samp <- data %>% 
  slice(seq(1, nrow(data), 7)) %>% 
  # Remove ones after two clean tests
  filter(day<=60) %>% 
  # Mark 
  mutate(program=ifelse(day %in% c(0, 35, 42, 49, 56), "Optimized (5 tests)", "Standard (9 tests)"),
         program=factor(program, levels=c("Standard (9 tests)", "Optimized (5 tests)")))


# Build data for second example
################################################################################

# Parameters
K <- 120        # Asymptote
t50 <- 20       # Day at which curve reaches half of K
s <- 0.3        # Steepness (adjust to taste)
ndays <- 40

# Logistic function with t50
sigmoid <- function(t, K, t50, s) {
  K / (1 + exp(-s * (t - t50)))
}

# Simulate
t <- 0:(ndays - 1)
N <- sigmoid(t, K, t50, s)

# Dataframe
df <- data.frame(phase="Deputation", 
                 day = t, 
                 toxicity = N)

# Plot
plot(df$day, df$toxicity, type = "l", lwd = 2,
     xlab = "Day", ylab = "Toxicity")
abline(h = K/2, v = t50, lty = 2, col = "red") # show half-point

# Merge
data1 <- data %>% 
  mutate(day=day+ndays,
         phase="Depuration")
data2 <- bind_rows(df, data1)

# Standard Sampling scheme
samp2_std <- data2 %>% 
  slice(seq(1, nrow(data2), 7)) %>% 
  filter(day <= 100) %>% 
  mutate(program="Standard")

# Projection curve 1
n0 <- samp2_std$toxicity[samp2_std$day==21]
days <- 0:140
toxs <- n0*exp(k*days)
proj1 <- tibble(day=days+21,
               toxicity=toxs) %>% 
  filter(day<= max(data2$day))

proj1_pts <- proj1 %>% 
  slice(seq(1, nrow(.), 7)) %>% 
  filter(day %in% c(35, 42, 49, 56))

# Projection curve 2
n0 <- samp2_std$toxicity[samp2_std$day==35]
days <- 0:140
toxs <- n0*exp(k*days)
proj2 <- tibble(day=days+35,
                toxicity=toxs) %>% 
  filter(day<= max(data2$day))

proj2_pts <- proj2 %>% 
  slice(seq(1, nrow(.), 7)) %>% 
  filter(day %in% c(70, 77, 84, 91))

# Optimized sampling
samp2_opt <- samp2_std %>% 
  filter(day %in% c(0, 7, 14, 21, 35, 70, 77, 84, 91)) %>% 
  mutate(program="Optimized")

# Merge
samp2 <- bind_rows(samp2_std, samp2_opt) %>% 
  mutate(program=recode_factor(program, 
                               "Standard"="Standard (15 tests)", 
                               "Optimized"="Optimized (9 tests)"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data, aes(x=day, y=toxicity)) +
  # Reference line
  geom_hline(yintercept=30, linetype="dashed", color="grey40") +
  annotate(geom="text", x=120, y=30, label="Action threshold", 
           color="grey40", hjust=1, vjust=-1, size=2.4) +
  # Data
  geom_line() +
  geom_point(data=samp, mapping=aes(fill=program), pch=21, size=3) +
  # Labels
  labs(x="Day of testing", y="Toxicity (ppm)", tag="A", title="After peak toxicity") +
  # Legend
  scale_fill_manual(name="Monitoring program", values=c("forestgreen", "darkorange")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.6, 0.8))
g1

# Plot
g2 <- ggplot(data2, aes(x=day, y=toxicity)) +
  # Phase line
  geom_vline(xintercept=40, color="grey40") +
  annotate(geom="text", x=0, y=115, label="Accumulation\nphase", 
           color="grey40", hjust=0, vjust=0.5, size=2.4) +
  annotate(geom="text", x=50, y=115, label="Depuration\nphase", 
           color="grey40", hjust=0, vjust=0.5, size=2.4) +
  # Reference line
  geom_hline(yintercept=30, linetype="dashed", color="grey40") +
  annotate(geom="text", x=160, y=30, label="Action threshold", 
           color="grey40", hjust=1, vjust=-1, size=2.4) +
  # Data
  geom_line() +
  # Projections
  geom_line(data=proj1, color="forestgreen") +
  geom_line(data=proj2, color="forestgreen") +
  # Sampling points
  geom_point(data=samp2,  mapping=aes(fill=program), pch=21, size=3) +
  geom_point(data=proj1_pts,  color="forestgreen", pch=21, size=3) +
  geom_point(data=proj2_pts,  color="forestgreen", pch=21, size=3) +
  # Labels
  labs(x="Day of testing", y="Toxicity (ppm)", tag="B", title="Before peak toxicity") +
  # Legend
  scale_fill_manual(name="Monitoring program", values=c("darkorange", "forestgreen")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.8))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_dep_traj_example.png"), 
       width=6.5, height=3, units="in", dpi=600)
 
