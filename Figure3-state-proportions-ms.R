# Figure3-state-proportions-ms.R
######################################
# Janelle L. Morano

# Figure 3. State proportions of presence

# last updated 1 May 2025
###############################################
###############################################


library(tidyverse)
library(mgcv) #1.9-1
library(cowplot)



#----- Load Model Results

# Presence GAM model predictions
predictions.pgam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")
names(predictions.pgam.list)
# [1] "predictions.pa.gam.5.spring.df" "predictions.pa.gam.5.fall.df"   "predictions.pa.gam.1.spring.df"
# [4] "predictions.pa.gam.1.fall.df" 





#----- Part A. Annual proportion by state ------------------------------------
###########################################################################################

#----- Sum the annual fits to find proportion by state and year

# Spring
annualobs.sum.sp <- predictions.pgam.list[[1]]  |>
  group_by(Year) |>
  summarise(annual.sum.fit = sum(fit, na.rm = TRUE))
predictions.pgam.list[[1]] <- predictions.pgam.list[[1]] %>%
  left_join(annualobs.sum.sp, by = "Year")
predictions.pgam.list[[1]] <- predictions.pgam.list[[1]] %>%
  mutate(prop.fit = fit/ annual.sum.fit)

# Fall, observed
annualobs.sum.fa <- predictions.pgam.list[[2]] |>
  group_by(Year) |>
  summarise(annual.sum.fit = sum(fit, na.rm = TRUE))
predictions.pgam.list[[2]] <- predictions.pgam.list[[2]] %>%
  left_join(annualobs.sum.fa, by = "Year")
predictions.pgam.list[[2]] <- predictions.pgam.list[[2]] %>%
  mutate(prop.fit = fit/ annual.sum.fit)



#----- Plot the proportions
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
predictions.pgam.list[[1]]$State <- factor(predictions.pgam.list[[1]]$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
predictions.pgam.list[[2]]$State <- factor(predictions.pgam.list[[2]]$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

f2a1 <- ggplot(subset(predictions.pgam.list[[1]], Year %in% 1989:2023), aes(x = factor(Year), y = prop.fit, fill = State)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x = "", y = "Proportion of Total Presence") +
  theme(legend.position = "none")

f2a2 <- ggplot(subset(predictions.pgam.list[[2]], Year %in% 1989:2023), aes(x = factor(Year), y = prop.fit, fill = State)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x = "", y = "Proportion of Total Presence") +
  theme(legend.position = "none")


plot_grid(f2a1, f2a2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/Fig2A-PA-GAM-state-proportions.png", width=7, height = 4)




#----- Plot the proportions, but only after 1989
f2a1.crop <- ggplot() +
  geom_bar(data = subset(predictions.pgam.list[[1]], Year %in% c(1989:2023)) , aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x= " ", y = "Relative Proportion of Probability of Presence") +
  theme(legend.position = "none")
f2a2.crop <- ggplot() +
  geom_bar(data = subset(predictions.pgam.list[[2]], Year %in% c(1989:2023)), aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x= " ", y = "Relative Proportion of Probability of Presence") +
  theme(legend.position = "none")


plot_grid(f2a1.crop, f2a2.crop, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/Fig2A-PA-GAM-state-proportions-1989-2023crop.png", width=7, height = 4)

