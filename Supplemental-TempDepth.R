# Supplemental-TempDepth.R
######################################
# Janelle L. Morano

# Figures for manuscript
# Supplemental Fig 1. Depth vs Temperature
# Figure 2A. Presence vs Temperature
# Figure 2B. Presence vs Depth
# Figure 2C. Biomass vs Temperature
# Figure 2D. Biomass vs Depth
# Supplemental Fig 2. Biomass vs Temperature by Survey


# last updated 13 May 2025
###############################################
###############################################

library(tidyverse)
library(cowplot)


# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")




###########################################################################################
#----- Supplemental Fig 1. Depth vs Temperature ------------------------------------
###########################################################################################

# Linear fit of Spring Depth to WaterTemp
lm_model1 <- lm(Depth ~ WaterTemp, data = data.list[[1]])
# Get the R-squared value
rsq1 <- summary(lm_model1)$r.squared

# Linear fit of Fall Depth to WaterTemp
lm_model2 <- lm(Depth ~ WaterTemp, data = data.list[[2]])
# Get the R-squared value
rsq2 <- summary(lm_model2)$r.squared


# Plot Depth v Temp
ggplot(data.list[[1]], aes(x = WaterTemp, y = Depth)) +
  geom_point(color = "#006164", shape = 21, alpha = 1/3) +
  geom_point(data = data.list[[2]], aes(x = WaterTemp, y = Depth), color = "#DB4325", shape = 21, alpha = 1/3) +
  geom_smooth(method = "lm", aes(x = WaterTemp, y = Depth), color = "black", se = TRUE) +
  geom_smooth(data = data.list[[2]], method = "lm", aes(x = WaterTemp, y = Depth), color = "grey20", se = TRUE) +
  annotate("text", x = 20, y = -50, label = paste("Spring R² = ", round(rsq1, 2)), color = "#006164", size = 4) +
  annotate("text", x = 37, y = -25, label = paste("Fall R² = ", round(rsq2, 2)), color = "#DB4325", size = 4) +
  theme(text = element_text(size = 12)) +
  labs(x= "Water Temperature (°C)", y = "Depth (m)") +
  theme_classic()

#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/SupFig1.depth-v-temp.png", width=6, height = 4)




###########################################################################################
#----- Figure 2A. Presence vs Temperature  ------------------------
###########################################################################################

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = WaterTemp, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = WaterTemp, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 12),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 40, by = 5)) + 
  labs(x= "Water Temperature (°C)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig2A.presence-v-temp.png", width=6, height = 4)



###########################################################################################
#----- Figure 2B. Presence vs Depth  ------------------------
###########################################################################################

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = Depth, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = Depth, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 12),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 550, by = 50)) + 
  labs(x= "Depth (m)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig2B.presence-v-depth.png", width=6, height = 4)



###########################################################################################
#----- Figure 2C. Biomass vs Temperature  ------------------------
###########################################################################################
# Linear fit of Spring Biomass to WaterTemp
lm_model3 <- lm(log(Biomass+1) ~ WaterTemp, data = data.list[[1]])
# Get the R-squared value
rsq3 <- summary(lm_model3)$r.squared

# Linear fit of Fall Biomass to WaterTemp
lm_model4 <- lm(log(Biomass+1) ~ WaterTemp, data = data.list[[2]])
# Get the R-squared value
rsq4 <- summary(lm_model4)$r.squared


ggplot(data = data.list[[1]], aes(x=WaterTemp, y=log(Biomass+1))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = data.list[[2]], aes(x=WaterTemp, y=log(Biomass+1)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = WaterTemp, y = log(Biomass+1)), color = "black", se = TRUE) +
  geom_smooth(data = data.list[[2]], method = "lm", aes(x = WaterTemp, y = log(Biomass+1)), color = "grey20", se = TRUE) +
  annotate("text", x = 32, y = 1.1, label = paste("Spring R² = ", round(rsq3, 2)), color = "#006164", size = 4) +
  annotate("text", x = 37, y = 0.5, label = paste("Fall R² = ", round(rsq4, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 40, by = 5)) + 
  labs(x= "Water Temperature (°C)", y = "log(Biomass+1) (kg/tow)")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig2C.biomass-v-temp.png", width=6, height = 4)



###########################################################################################
#----- Figure 2D. Biomass vs Depth  ------------------------
###########################################################################################
# Linear fit of Spring Biomass to Depth
lm_model5 <- lm(log(Biomass+1) ~ Depth, data = data.list[[1]])
# Get the R-squared value
rsq5 <- summary(lm_model5)$r.squared

# Linear fit of Fall Biomass to Depth
lm_model6 <- lm(log(Biomass+1) ~ Depth, data = data.list[[2]])
# Get the R-squared value
rsq6 <- summary(lm_model6)$r.squared


ggplot(data.list[[1]], aes(x=Depth, y=log(Biomass+1))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = data.list[[2]], aes(x=Depth, y=log(Biomass+1)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = Depth, y = log(Biomass+1)), color = "black", se = TRUE) +
  geom_smooth(data = data.list[[2]], method = "lm", aes(x = Depth, y = log(Biomass+1)), color = "grey20", se = TRUE) +
  annotate("text", x = 450, y = 0.5, label = paste("Spring R² = ", round(rsq5, 2)), color = "#006164", size = 4) +
  annotate("text", x = 325, y = -0.5, label = paste("Fall R² = ", round(rsq6, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 550, by = 50)) + 
  labs(x= "Depth (m)", y = "log(Biomass+1) (kg/tow)")
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figure-output/Fig2D.biomass-v-depth.png", width=6, height = 4)



###########################################################################################
#----- Report mean Temp and Depth ------------------------------
###########################################################################################
# Associated with menhaden presence
data.list[[1]] |>
  filter(Presence == 1) |>
  summarise(
    mean_temp = mean(WaterTemp, na.rm = TRUE),
    median_temp = median(WaterTemp, na.rm = TRUE)
  )
# mean_temp median_temp
# 1  15.16043        15.3

data.list[[2]] |>
  filter(Presence == 1) |>
  summarise(
    mean_temp = mean(WaterTemp, na.rm = TRUE),
    median_temp = median(WaterTemp, na.rm = TRUE)
  )
# mean_temp median_temp
# 1  22.99266        23.2


# Now for all samples, by state
df1 <- data.list[[1]] |>
  group_by(State) |>
  summarise(
    mean_depth = mean(Depth, na.rm = TRUE),
    mean_temp = mean(WaterTemp, na.rm = TRUE))

df2 <- data.list[[2]] |>
  group_by(State) |>
  summarise(
    mean_depth = mean(Depth, na.rm = TRUE),
    mean_temp = mean(WaterTemp, na.rm = TRUE))

meantemp <- bind_cols(df1, df2)
meantemp$tempdiff <- meantemp$mean_temp...6 - meantemp$mean_temp...3
meantemp$depthdiff <- meantemp$mean_depth...5 - meantemp$mean_depth...2

###########################################################################################
#----- Supplemental Fig 2. Biomass vs Temperature by Survey ------------------------------
###########################################################################################

s2a <- ggplot(data.list[[1]], aes(x=WaterTemp, y=log(Biomass+1), color = Survey)) +
  geom_point() +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  labs(x= "Water Temperature (°C)", y = "log(Biomass+1) (kg/tow)")
s2b <- ggplot(data.list[[2]], aes(x=WaterTemp, y=log(Biomass+1), color = Survey)) +
  geom_point() +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  labs(x= "Water Temperature (°C)", y = "log(Biomass+1) (kg/tow)")
plot_grid(s2a, s2b, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/SupFig2.biomass-v-temp-bySurvey.png", width=10, height = 4)