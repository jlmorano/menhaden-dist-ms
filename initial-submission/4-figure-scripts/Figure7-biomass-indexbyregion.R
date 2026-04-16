# Figure7-biomass-indexbyregion.R
######################################
# Janelle L. Morano

# Use index by region output from sdmTMB model to generate Fig 7


# last updated 23 July 2025
###############################################
###############################################

library(tidyverse)
library(tictoc)
library(cowplot)


inds.sp <- read.csv("/Users/janellemorano/Git/menhaden-dist-ms/data/index.spring.byregion500.csv")
inds.fa <- read.csv("/Users/janellemorano/Git/menhaden-dist-ms/data/index.fall.byregion500.csv")

#There's no GME or SCGAFL (and RICTNY is mis-named)
pal6 <- c("#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26")
inds.sp$region <- factor(inds.sp$region, levels = c("MA", "NYCTRI", "NJ", "DEMD", "VA", "NC"))
inds.fa$region <- factor(inds.fa$region, levels = c("MA", "NYCTRI", "NJ", "DEMD", "VA", "NC"))

ggplot(inds.sp, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region))


f7a <- ggplot(inds.sp, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  scale_fill_manual(values = pal6) +
  scale_color_manual(values = pal6) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region), linewidth =1.2) +
  scale_y_log10(n.breaks = 10, expand = c(0, 0), limits = c(0.001, 1000000000)) +
  theme_classic() +
  # labs(x = "", y = "Estimate of Biomass (kg, log scale)") +
  labs(x = "", y = "") +
  theme(legend.position = "none")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig7A-regional-biomass-estimates.png", width=3.5, height = 4)

f7b <- ggplot(inds.fa, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  scale_fill_manual(values = pal6) +
  scale_color_manual(values = pal6) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region), linewidth =1.2) +
  scale_y_log10(n.breaks = 10, expand = c(0, 0), limits = c(0.001, 1000000000)) +
  theme_classic() +
  labs(x = "", y = " ") +
  # theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig7B-regional-biomass-estimates.png", width=3.5, height = 4)

plot_grid(f7a, f7b, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig7-regional-biomass-estimates.png", width=7, height = 4)