# FigureSupp-landings.R
######################################
# Janelle L. Morano

# Supplemental Fig XX. Landings by state


# last updated 30 April 2025
###############################################
###############################################

library(tidyverse)


land <- read.csv("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden_FOSS_landings.csv", header = TRUE)
colnames(land)
str(land)
land$Metric.Tons <- as.numeric(land$Metric.Tons)

unique(land$State)
land$State <- factor(land$State, levels = c("MAINE", "NEW HAMPSHIRE", "MASSACHUSETTS", "RHODE ISLAND", "CONNECTICUT", "NEW YORK", "NEW JERSEY", "DELAWARE", "MARYLAND", "VIRGINIA", "NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "FLORIDA-EAST"))

library(scales)   

ggplot(data = land, aes(x = Year, y = Metric.Tons, color = State)) +
  geom_line(size = 0.8) +
  scale_color_viridis_d() +
  scale_y_log10(labels = label_number(accuracy = 1)) +
  theme(text = element_text(size = 14)) +
  labs(x = " ", y = "log(Metric Tons)") +
  theme_classic()

ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/SupFigX.landings.png", dpi = 400, width=9, height = 9)


land$Region <- case_when(
  land$State %in% c("MAINE", "NEW HAMPSHIRE", "MASSACHUSETTS", "RHODE ISLAND", "CONNECTICUT") ~ "North",
  land$State %in% c("NEW YORK", "NEW JERSEY", "DELAWARE", "MARYLAND", "VIRGINIA") ~ "Middle",
  land$State %in% c("NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "FLORIDA-EAST") ~ "South")

ggplot(data = land, aes(x = Year, y = Metric.Tons, color = Region, group = State)) +
  geom_line(size = 0.8, alpha = 0.7) +
  scale_size_manual(values = c("North" = 1.2, "Middle" = 0.5, "South" = 0.5), guide = "none") +
  scale_color_manual(values = c("North" = "#440154", "Middle" = "#5ec962", "South" = "#fde725")) +
  scale_y_log10(labels = label_number(accuracy = 1)) +
  theme(text = element_text(size = 14)) +
  labs(x = " ", y = "log(Metric Tons)") +
  theme_classic()


#----- Scale the maximum annual landings by state and year

annual.land <- land |>
  group_by(Year) |>
  summarise(annual.sum.mt = sum(Metric.Tons, na.rm = TRUE))

land <- land %>%
  left_join(annual.land, by = c("Year"))

land$prop.annual.landings <- land$Metric.Tons / land$annual.sum.mt


ggplot() +
  geom_bar(data = land, aes(x = Year, y = prop.annual.landings, fill = State), position = "fill", stat = "identity") +
  scale_fill_viridis_d() +
  theme_classic() +
  labs(x= " ", y = "Proportion of Total Landing (Metric Tons)")

# 2018-2023
ggplot() +
  geom_bar(data = subset(land, Year %in% c(2018:2023)), aes(x = Year, y = prop.annual.landings, fill = State), position = "fill", stat = "identity") +
  scale_fill_viridis_d() +
  theme_classic() +
  labs(x= " ", y = "Proportion of Total Landing (Metric Tons)")
