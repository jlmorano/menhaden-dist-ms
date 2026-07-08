# Figure2A-D-S2-Envtl-Covariates.R
######################################
# Janelle L. Morano

# Figures for manuscript
# 0. NEW Supplemental Table S2. Means of environmental covariates state and season
# 1. NEW Supplemental Fig X. Correlations between variables 
# 2. DELETESupplemental Fig 2. Depth vs Temperature
# 3. Figure 2A. Presence vs Temperature
# 4. Figure 2B. Presence vs Depth
# 5. Figure 2C. Biomass vs Temperature
# 6. Figure 2D. Biomass vs Depth
# 7. Unused Fig. Biomass vs Temperature by Survey


# last updated 6 July 2026
###############################################
###############################################


library(tidyverse)
library(cowplot)
library(here)
dir <- here()



# Read in saved RDS
data.list <- readRDS(here(dir, "2-input-data", "menhaden.data.list.062026.rds"))

# Factor order seasons and states
data.list[[1]] <- data.list[[1]] |>
 mutate(
  Season = factor(Season, levels = c("Spring", "Fall")),
  State = factor(State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
  ) |>
  filter(if_any(c(Season, State)), ~ !is.na(.))
data.list[[2]] <- data.list[[1]] |>
 mutate(
  Season = factor(Season, levels = c("Spring", "Fall")),
  State = factor(State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
  ) |>
  filter(!is.na(Season, State))


###########################################################################################
#----- 0. NEW Supplemental Table S2. Means of environmental covariates state and season ------
###########################################################################################

# Overall by season
spring.means <- data.list[[1]] |>
  group_by(Presence) |>
  summarise(across(
    c(Depth, SurfTemp, SurfSalin, mean_chlor_a), 
    list(mean = \(x) mean(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE))
    ))

fall.means <- data.list[[2]] |> 
  group_by(Presence) |>
  summarise(across(
    c(Depth, SurfTemp, SurfSalin, mean_chlor_a), 
    list(mean = \(x) mean(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE))
    ))

view(spring.means)
view(fall.means)

write.table(spring.means, "clipboard", sep = "\t", row.names = FALSE)
write.table(fall.means, "clipboard", sep = "\t", row.names = FALSE)


# Overall by season, state
spring.means2 <- data.list[[1]] |>
  group_by(Presence, State) |>
  summarise(across(
    c(Depth, SurfTemp, SurfSalin, mean_chlor_a), 
    list(mean = \(x) mean(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE))
    ))

fall.means2 <- data.list[[2]] |> 
  group_by(Presence, State) |>
  summarise(across(
    c(Depth, SurfTemp, SurfSalin, mean_chlor_a), 
    list(mean = \(x) mean(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE))
    ))

view(spring.means2)
view(fall.means2)

write.table(spring.means2, "clipboard", sep = "\t", row.names = FALSE)
write.table(fall.means2, "clipboard", sep = "\t", row.names = FALSE)

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
  facet_wrap(~Season) + 
  theme_classic() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(), # Cleans up the facet labels
        strip.text = element_text(face = "bold")) +
  labs(y = "Surface Temperature (°C)")
ggsave(file = here(dir, "5-figures", "NEWFigX.state-temp-years.png"), width=10, height = 5)

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
ggsave(file = here(dir, "5-figures", "NEWFigX.state-salinity-years.png"), width=10, height = 5)

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
ggsave(file = here(dir, "5-figures", "NEWFigX.state-chloro-years.png"), width=10, height = 5)




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
ggsave(here(dir, "5-figures", file = "Fig2A.presence-v-temp.png"), width=4, height = 4)



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
ggsave(here(dir, "5-figures", file = "Fig2B.presence-v-depth.png"), width=4, height = 4)



###########################################################################################
#----- Figure 2C. Presence vs Salinity  ------------------------
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
  labs(x= "Surface Salinity (psu)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(here(dir, "5-figures", file = "Fig2C.presence-v-salinity.png"), width=4, height = 4)



###########################################################################################
#----- Figure 2D. Presence vs Chlorophyll-a  ------------------------
###########################################################################################

ggplot() +
  geom_histogram(data = subset(data.list[[1]], Presence == 1), aes(x = mean_chlor_a, fill = "Spring"), 
                 color = "#006164", bins = 30, alpha = 0.90) +
  geom_histogram(data = subset(data.list[[2]], Presence == 1), aes(x = mean_chlor_a, fill = "Fall"), 
                 color = "#DB4325", bins = 30, alpha = 0.75) +
  scale_fill_manual(values = c("Spring" = "#006164", "Fall" = "#DB4325")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  labs(x= "Chlorophyll-a (mg/m³)", y = "Count of Menhaden Presence") +
  labs(fill = "Season")
ggsave(here(dir, "5-figures", file = "Fig2D.presence-v-chlorophyll.png"), width=4, height = 4)



###########################################################################################
#----- Figure 2E. Biomass vs Temperature  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to SurfTemp
lm_model3 <- lm(log(Weight.kg) ~ SurfTemp, data = subset(data.list[[1]], Weight.kg > 0))# Get the R-squared value
rsq3 <- summary(lm_model3)$r.squared

# Linear fit of Fall Weight.kg to SurfTemp
lm_model4 <- lm(log(Weight.kg) ~ SurfTemp, data = subset(data.list[[2]], Weight.kg > 0))
# Get the R-squared value
rsq4 <- summary(lm_model4)$r.squared


ggplot(data = subset(data.list[[1]], Weight.kg > 0), aes(x=SurfTemp, y=log(Weight.kg))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = subset(data.list[[2]], Weight.kg > 0), aes(x=SurfTemp, y=log(Weight.kg)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = SurfTemp, y = log(Weight.kg)), color = "grey20", se = TRUE) +
  geom_smooth(data = subset(data.list[[2]], Weight.kg > 0), method = "lm", aes(x = SurfTemp, y = log(Weight.kg)), color = "grey40", se = TRUE) +
  # annotate("text", x = 32, y = 1.1, label = paste("Spring R² = ", round(rsq3, 2)), color = "#006164", size = 4) +
  # annotate("text", x = 37, y = 0.5, label = paste("Fall R² = ", round(rsq4, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  # scale_x_continuous(lim = c(0, 40), breaks = seq(0, 40, by = 5)) +
  labs(x= "Water Temperature (°C)", y = "log(Biomass) (kg/tow)")
ggsave(here(dir, "5-figures", file = "Fig2E.biomass-v-temp.png"), width=4, height = 4)



###########################################################################################
#----- Figure 2F. Biomass vs Depth  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to Depth
lm_model5 <- lm(log(Weight.kg) ~ Depth, data = subset(data.list[[1]], Weight.kg > 0))
# Get the R-squared value
rsq5 <- summary(lm_model5)$r.squared

# Linear fit of Fall Weight.kg to Depth
lm_model6 <- lm(log(Weight.kg) ~ Depth, data = subset(data.list[[2]], Weight.kg > 0))
# Get the R-squared value
rsq6 <- summary(lm_model6)$r.squared


ggplot(data = subset(data.list[[1]], Weight.kg > 0), aes(x=Depth, y=log(Weight.kg))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = subset(data.list[[2]], Weight.kg > 0), aes(x=Depth, y=log(Weight.kg)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = Depth, y = log(Weight.kg)), color = "grey20", se = TRUE) +
  geom_smooth(data = subset(data.list[[2]], Weight.kg > 0), method = "lm", aes(x = Depth, y = log(Weight.kg)), color = "grey40", se = TRUE) +
  # annotate("text", x = 450, y = 0.5, label = paste("Spring R² = ", round(rsq5, 2)), color = "#006164", size = 4) +
  # annotate("text", x = 325, y = -0.5, label = paste("Fall R² = ", round(rsq6, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(lim = c(0, 200), breaks = seq(0, 200, by = 50)) + 
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)")
ggsave(here(dir, "5-figures", file = "Fig2F.biomass-v-depth.png"), width=4, height = 4)


###########################################################################################
#----- Figure 2G. Biomass vs Salinity  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to Salinity
lm_model7 <- lm(log(Weight.kg) ~ SurfSalin, data = subset(data.list[[1]], Weight.kg > 0))
# Get the R-squared value
rsq7 <- summary(lm_model7)$r.squared

# Linear fit of Fall Weight.kg to Salinity
lm_model8 <- lm(log(Weight.kg) ~ SurfSalin, data = subset(data.list[[2]], Weight.kg > 0))
# Get the R-squared value
rsq8 <- summary(lm_model8)$r.squared


ggplot(subset(data.list[[1]], Weight.kg > 0), aes(x=SurfSalin, y=log(Weight.kg))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = subset(data.list[[2]], Weight.kg > 0), aes(x=SurfSalin, y=log(Weight.kg)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = SurfSalin, y = log(Weight.kg)), color = "grey20", se = TRUE) +
  geom_smooth(data = subset(data.list[[2]], Weight.kg > 0), method = "lm", aes(x = SurfSalin, y = log(Weight.kg)), color = "grey50", se = TRUE) +
  # annotate("text", x = 450, y = 0.5, label = paste("Spring R² = ", round(rsq7, 2)), color = "#006164", size = 4) +
  # annotate("text", x = 325, y = -0.5, label = paste("Fall R² = ", round(rsq8, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  labs(x= "Salinity (ppt)", y = "log(Biomass) (kg/tow)")
ggsave(here(dir, "5-figures", file = "Fig2G.biomass-v-salinity.png"), width=4, height = 4)



###########################################################################################
#----- Figure 2H. Biomass vs Chlorophyll  ------------------------
###########################################################################################
# Linear fit of Spring Weight.kg to Chlorophyll
lm_model9 <- lm(log(Weight.kg) ~ mean_chlor_a, data = subset(data.list[[1]], Weight.kg > 0))
# Get the R-squared value
rsq9 <- summary(lm_model9)$r.squared

# Linear fit of Fall Weight.kg to Chlorophyll
lm_model10 <- lm(log(Weight.kg) ~ mean_chlor_a, data = subset(data.list[[2]], Weight.kg > 0))
# Get the R-squared value
rsq10 <- summary(lm_model10)$r.squared


ggplot(subset(data.list[[1]], Weight.kg > 0), aes(x=mean_chlor_a, y=log(Weight.kg))) +
  geom_point(color = "#006164", alpha = 0.75) +
  geom_point(data = subset(data.list[[2]], Weight.kg > 0), aes(x=mean_chlor_a, y=log(Weight.kg)), color = "#DB4325", alpha = 0.75) +
  geom_smooth(method = "lm", aes(x = mean_chlor_a, y = log(Weight.kg)), color = "grey20", se = TRUE) +
  geom_smooth(data = subset(data.list[[2]], Weight.kg > 0), method = "lm", aes(x = mean_chlor_a, y = log(Weight.kg)), color = "grey50", se = TRUE) +
  # annotate("text", x = 65, y = 3.75, label = paste("Spring R² = ", round(rsq9, 2)), color = "#006164", size = 4) +
  # annotate("text", x = 70, y = 1.2, label = paste("Fall R² = ", round(rsq10, 2)), color = "#DB4325", size = 4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  labs(x= "Chlorophyll (mg/m³)", y = "log(Biomass) (kg/tow)")
ggsave(here(dir, "5-figures", file = "Fig2H.biomass-v-chlorophyll.png"), width=4, height = 4)




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




