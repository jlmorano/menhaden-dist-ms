# Figure2A-D-S2-Envtl-Covariates.R
######################################
# Janelle L. Morano

# Figures for manuscript
# 1. NEW Supplemental Fig X. Correlations between variables 
# 2. DELETESupplemental Fig 2. Depth vs Temperature
# 3. Figure 2A. Presence vs Temperature
# 4. Figure 2B. Presence vs Depth
# 5. Figure 2C. Biomass vs Temperature
# 6. Figure 2D. Biomass vs Depth
# 7. Unused Fig. Biomass vs Temperature by Survey


# last updated 24 June 2025
###############################################
###############################################


library(tidyverse)
library(cowplot)
library(here)
dir <- here()



# Read in saved RDS
data.list <- readRDS(here(dir, "2-input-data", "menhaden.data.list.062026.rds"))



###########################################################################################
#----- 1. NEW Supplemental Fig X. Correlations between variables ------------------------------------
###########################################################################################
library(corrplot)
library(ggcorrplot)
library(patchwork)

# Function to process data and create the plot
make_corr_plot <- function(data, plot_title) {
  data <- data |>
  select(Depth, "Bottom Temp" = BotTemp, "Bottom Salinity" = BotSalin, "Surface Temp" = SurfTemp, "Surface Salinity" = SurfSalin, "Chlorophyll-a" = mean_chlor_a)

  corr.mat <- cor(data, method = "pearson", use = "complete.obs")

  ggcorrplot(corr.mat, hc.order = TRUE, type = "lower", lab = TRUE, outline.color = "white", tl.col = "black", tl.srt = 45) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5))
}

p.spring <- make_corr_plot(data.list[[1]], "Spring")
p.fall <- make_corr_plot(data.list[[2]], "Fall")

p.spring + p.fall + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(file = here(dir, "5-figures", "SupFigX.correlations.png"), width=10, height = 5)




###########################################################################################
#----- NEW Fig X. Envt'l Covariates by State and over Years -------------------------------
###########################################################################################
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")

plot_data <- bind_rows(
  list("Spring" = data.list[[1]], "Fall" = data.list[[2]]), 
  .id = "Source"
)

# Surface Temperature using facet_wrap
ggplot(plot_data, aes(x = Year, y = SurfTemp, color = State, fill = State)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = TRUE, alpha = 0.5, linewidth = 1) +
  scale_color_manual(values = pal8) +
  scale_fill_manual(values = pal8) +
  facet_wrap(~Source) + 
  theme_classic() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(), # Cleans up the facet labels
        strip.text = element_text(face = "bold")) +
  labs(y = "Surface Temperature (°C)")


# Surface Salinity using facet_wrap
ggplot(plot_data, aes(x = Year, y = SurfSalin, color = State, fill = State)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = TRUE, alpha = 0.5, linewidth = 1) +
  scale_color_manual(values = pal8) +
  scale_fill_manual(values = pal8) +
  facet_wrap(~Source) + 
  theme_classic() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(), # Cleans up the facet labels
        strip.text = element_text(face = "bold")) +
  labs(y = "Surface Salinity (ppt)")


# Chlorophyll-a using facet_wrap
ggplot(plot_data, aes(x = Year, y = mean_chlor_a, color = State, fill = State)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = TRUE, alpha = 0.5, linewidth = 1) +
  scale_color_manual(values = pal8) +
  scale_fill_manual(values = pal8) +
  facet_wrap(~Source) + 
  theme_classic() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(), # Cleans up the facet labels
        strip.text = element_text(face = "bold")) +
  labs(y = "Chlorophyll-a (mg/m³)")





###########################################################################################
#----- DELETE Supplemental Fig 2. Depth vs Temperature ------------------------------------
###########################################################################################

# # Linear fit of Spring Depth to WaterTemp
# lm_model1 <- lm(Depth ~ WaterTemp, data = data.list[[1]])
# # Get the R-squared value
# rsq1 <- summary(lm_model1)$r.squared

# # Linear fit of Fall Depth to WaterTemp
# lm_model2 <- lm(Depth ~ WaterTemp, data = data.list[[2]])
# # Get the R-squared value
# rsq2 <- summary(lm_model2)$r.squared


# # Plot Depth v Temp
# ggplot(data.list[[1]], aes(x = WaterTemp, y = Depth)) +
#   geom_point(color = "#006164", shape = 21, alpha = 1/3) +
#   geom_point(data = data.list[[2]], aes(x = WaterTemp, y = Depth), color = "#DB4325", shape = 21, alpha = 1/3) +
#   geom_smooth(method = "lm", aes(x = WaterTemp, y = Depth), color = "black", se = TRUE) +
#   geom_smooth(data = data.list[[2]], method = "lm", aes(x = WaterTemp, y = Depth), color = "grey20", se = TRUE) +
#   annotate("text", x = 20, y = -50, label = paste("Spring R² = ", round(rsq1, 2)), color = "#006164", size = 4) +
#   annotate("text", x = 37, y = -25, label = paste("Fall R² = ", round(rsq2, 2)), color = "#DB4325", size = 4) +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Water Temperature (°C)", y = "Depth (m)") +
#   theme_classic()

# #ggsave(file = "SupFig1.depth-v-temp.png", width=6, height = 4)






###########################################################################################
#----- Figure 2A. Presence vs Temperature  ------------------------
###########################################################################################

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = SurfTemp, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = SurfTemp, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  scale_x_continuous(lim = c(0, 40), breaks = seq(0, 40, by = 5)) + 
  labs(x= "Water Temperature (°C)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(file = "Fig2A.presence-v-temp.png", width=6, height = 4)



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
  theme(text = element_text(size = 16),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 550, by = 50)) +
  labs(x= "Depth (m)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(file = "Fig2B.presence-v-depth.png", width=3, height = 4)



###########################################################################################
#----- Figure 2X. Presence vs Salinity  ------------------------
###########################################################################################
str(data.list$alldata.fall)

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = SurfSalin, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = SurfSalin, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 550, by = 50)) +
  labs(x= "Surface Salinity (PSU)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(file = "Fig2X.presence-v-salinity.png", width=3, height = 4)



###########################################################################################
#----- Figure 2X. Presence vs Chlorophyll-a  ------------------------
###########################################################################################

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = Depth, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = Depth, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 550, by = 50)) +
  labs(x= "Depth (m)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(file = "Fig2X.presence-v-chlorophyll.png", width=3, height = 4)



###########################################################################################
#----- Figure 2C. Biomass vs Temperature  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to SurfTemp
lm_model3 <- lm(log(Weight.kg+1) ~ SurfTemp, data = data.list[[1]])
# Get the R-squared value
rsq3 <- summary(lm_model3)$r.squared

# Linear fit of Fall Weight.kg to SurfTemp
lm_model4 <- lm(log(Weight.kg+1) ~ SurfTemp, data = data.list[[2]])
# Get the R-squared value
rsq4 <- summary(lm_model4)$r.squared


ggplot(data = data.list[[1]], aes(x=SurfTemp, y=log(Weight.kg+1))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = data.list[[2]], aes(x=SurfTemp, y=log(Weight.kg+1)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = SurfTemp, y = log(Weight.kg+1)), color = "black", se = TRUE) +
  geom_smooth(data = data.list[[2]], method = "lm", aes(x = SurfTemp, y = log(Weight.kg+1)), color = "grey20", se = TRUE) +
  annotate("text", x = 32, y = 1.1, label = paste("Spring R² = ", round(rsq3, 2)), color = "#006164", size = 4) +
  annotate("text", x = 37, y = 0.5, label = paste("Fall R² = ", round(rsq4, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(lim = c(0, 40), breaks = seq(0, 40, by = 5)) +
  labs(x= "SurfTemp (°C)", y = "log(Weight.kg+1) (kg/tow)")
ggsave(file = "Fig2C.biomass-v-temp.png", width=6, height = 4)



###########################################################################################
#----- Figure 2D. Biomass vs Depth  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to Depth
lm_model5 <- lm(log(Weight.kg+1) ~ Depth, data = data.list[[1]])
# Get the R-squared value
rsq5 <- summary(lm_model5)$r.squared

# Linear fit of Fall Weight.kg to Depth
lm_model6 <- lm(log(Weight.kg+1) ~ Depth, data = data.list[[2]])
# Get the R-squared value
rsq6 <- summary(lm_model6)$r.squared


ggplot(data.list[[1]], aes(x=Depth, y=log(Weight.kg+1))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = data.list[[2]], aes(x=Depth, y=log(Weight.kg+1)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = Depth, y = log(Weight.kg+1)), color = "black", se = TRUE) +
  geom_smooth(data = data.list[[2]], method = "lm", aes(x = Depth, y = log(Weight.kg+1)), color = "grey20", se = TRUE) +
  annotate("text", x = 450, y = 0.5, label = paste("Spring R² = ", round(rsq5, 2)), color = "#006164", size = 4) +
  annotate("text", x = 325, y = -0.5, label = paste("Fall R² = ", round(rsq6, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(lim = c(0, 550), breaks = seq(0, 550, by = 50)) + 
  labs(x= "Depth (m)", y = "log(Weight.kg+1) (kg/tow)")
ggsave(file = "Fig2D.biomass-v-depth.png", width=6, height = 4)



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




