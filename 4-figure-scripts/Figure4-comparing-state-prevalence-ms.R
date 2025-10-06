# Figure4-comparing-state-prevalence-ms.R
######################################
# Janelle L. Morano

# Figure 4. Comparison of State prevalence

# last updated 19 July 2025
###############################################
###############################################


library(tidyverse)
library(mgcv) #1.9-1
library(cowplot)



#----- Load Model Results

# Presence GAM model predictions
predictions.pgam.list <- readRDS("PA-GAM-predictions.rds")
names(predictions.pgam.list)
# "predictions.pa.gam.7.spring.df" "predictions.pa.gam.6.fall.df" 





#----- Part A. Annual prevalence by state ------------------------------------
###########################################################################################

#----- Each state region and year has a fit for each survey and that fit is a repeat value. So, I only need 1 value per state and year.

# Spring
annual.prop.sp <- predictions.pgam.list[[1]]  |>
  group_by(Year, State) |>
  summarise(
    state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
    .groups = 'drop')

# Fall, observed
annual.prop.fa <- predictions.pgam.list[[2]]  |>
  group_by(Year, State) |>
  summarise(
    state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
    .groups = 'drop')



#----- Plot the comparative prevalences
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
annual.prop.sp$State <- factor(annual.prop.sp$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
annual.prop.fa$State <- factor(annual.prop.fa$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

f2a1 <- ggplot(subset(annual.prop.sp, Year %in% 1989:2023), aes(x = factor(Year), y = state.annual.fit, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_x_discrete(breaks = as.character(seq(1990, 2020, by = 5))) +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  labs(x = "", y = "Annual State Prevalence") +
  theme(legend.position = "none")

f2a2 <- ggplot(subset(annual.prop.fa, Year %in% 1989:2023), aes(x = factor(Year), y = state.annual.fit, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_x_discrete(breaks = as.character(seq(1990, 2020, by = 5))) +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0, 1.75)) +
  labs(x = "", y = "Annual State Prevalence") +
  theme(legend.position = "none")


plot_grid(f2a1, f2a2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "Fig4-PA-GAM-compare-state-prevalences.png", width=7, height = 4)
