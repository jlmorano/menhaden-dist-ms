# Figure2-GAM-ms.R
######################################
# Janelle L. Morano

# Visualizations (graphs and maps) from the Presence GAM models run in "MSmodel-Presence-GAM-Models-Atlantic-menhaden-ms.R" for the Atlantic menhaden manuscript

# last updated 25 March 2025
###############################################
###############################################


library(tidyverse)
library(mgcv)
library(cowplot)


###########################################################################################
#----- Load Model Results ------------------------------------------------------------
###########################################################################################

# Remember the models,
# 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
# 3. Presence ~ s(Year) + State + s(Survey, bs = "re") + WaterTemp


# Survey Data
# data.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/data.list.rds")
# # Prediction data
# preddata.pgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-summaries.rds")
# Presence GAM model output
pgam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-results.rds")
# # Presence GAM model output summaries only
# pgam.summaries <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-summaries.rds")
# Presence GAM model predictions
predictions.pgam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")



###########################################################################################
#----- Part A. Annual proportion by state ------------------------------------
###########################################################################################

#----- Scale the maximum annual fits by state and year

# Spring
annual.max.sp <- predictions.pgam.list[[1]]  |>
  group_by(Year) |>
  summarise(annual.max.fit = max(fit, na.rm = TRUE))

predictions.pgam.list[[1]] <- predictions.pgam.list[[1]] %>%
  left_join(annual.max.sp, by = "Year") 
predictions.pgam.list[[1]] <- predictions.pgam.list[[1]] %>%
  mutate(prop.fit = fit/ annual.max.fit)


# Fall
annual.max.fa <- predictions.pgam.list[[2]]  |>
  group_by(Year) |>
  summarise(annual.max.fit = max(fit, na.rm = TRUE))
predictions.pgam.list[[2]] <- predictions.pgam.list[[2]] %>%
  left_join(annual.max.fa, by = "Year") 
predictions.pgam.list[[2]] <- predictions.pgam.list[[2]] %>%
  mutate(prop.fit = fit/ annual.max.fit)


#----- Plot the proportions
pal7 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26", "#fde725")
predictions.pgam.list[[1]]$State <- factor(predictions.pgam.list[[1]]$State, levels = c("GMA", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
predictions.pgam.list[[2]]$State <- factor(predictions.pgam.list[[2]]$State, levels = c("GMA", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

f2a1 <- ggplot() +
  geom_bar(data = predictions.pgam.list[[1]], aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal7) +
  # geom_errorbar(data = pred.o1.df, aes(x=Ecological.Num, ymin=probability-se, ymax=probability+se), width = 0.3
  theme_classic()

f2a2 <- ggplot() +
  geom_bar(data = predictions.pgam.list[[2]], aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal7) +
  theme_classic()


plot_grid(f2a1, f2a2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/Fig2-PA-GAM-stateoverlap.png", width=7, height = 4)



###########################################################################################
#----- Part B. All state trends overlayed in 2 figs: spring and fall ------------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal7 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26", "#fde725")
pal7lite <- adjustcolor(pal7, alpha.f = 0.5) #make transparent


#----- Spring
f2b1 <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  # # 1. Presence ~ s(Year) + s(Survey, bs = "re")
  geom_ribbon(data = subset(predictions.pgam.list[[1]]), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit)), fill = "grey") +
  geom_line(data = subset(predictions.pgam.list[[1]]), aes(x=Year, y=fit), color = "black", linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,0.8)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring (Federal and State)")


#----- Fall
f2b2 <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,0.8)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Fall (Federal and State)")


plot_grid(f2b1, f2b2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/Fig2-PA-GAM-stateoverlap.png", width=7, height = 4)





###########################################################################################
#----- Part C. State by state: spring and fall ------------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal7 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26", "#fde725")
pal7lite <- adjustcolor(pal7, alpha.f = 0.5) #make transparent

# List of States to plot
states <- unique(na.omit(predictions.pgam.list[[1]]$State))
states <- factor(states, levels = c("GMA", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Create an empty list to store plots
fig2c <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig2c[[paste("Spring -", i)]] <- ggplot() + 
    geom_ribbon(data = subset(predictions.pgam.list[[1]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal7lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions.pgam.list[[1]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal7[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    ylim(c(0, 0.8)) +
    xlim(c(1972, 2023)) +
    labs(x = " ", y = "Presence") +
    ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig2c[[paste("Fall -", i)]] <- ggplot() + 
    geom_ribbon(data = subset(predictions.pgam.list[[2]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal7lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions.pgam.list[[2]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal7[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    ylim(c(0, 0.8)) +
    xlim(c(1972, 2023)) +
    labs(x = " ", y = "Presence") +
    ggtitle(paste("Fall -", i))  # Title with state name
}

combined_plot <- plot_grid(plotlist = fig2c, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)

