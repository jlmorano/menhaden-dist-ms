# FigureX-menhaden-monthly.R
######################################
# Janelle L. Morano

# Figure X. Monthly Distribution of Menhaden

# last updated 16 April 2026
###############################################
###############################################


library(tidyverse)
library(here)

dir <- here::here()

data <- read.csv(file = here(dir, "2-input-data", "combined-allsurveys-menhaden-data-20260414.csv"), header = TRUE)


ggplot(data, aes(x = Month, y = MenhadenTotal, group = Survey, color = Survey)) +
    geom_point() +
    theme_classic()
