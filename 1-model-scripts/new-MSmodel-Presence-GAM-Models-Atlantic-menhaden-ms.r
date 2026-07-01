# new-MSmodel-Presence-GAM-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Duplicate of MSmodel-Presence-GAM-Models-Atlantic-menhaden.R, but with new models added and some models removed.

# last updated 30 June 2026 (WaterTemp changed to SurfTemp so that it can be used alt to BotTemp)

###############################################
###############################################

#-----Table of Contents -------------------------------

#----- Presence-Absence GAMMs
# Complexity from high to low
# 1. Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + BotTemp + BotSalin

# ***(best for fall) 6. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp
# ***(best for spring) 7. Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp
# 8. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(SurfTemp)

rm(list = ls())

library(tidyverse)
library(mgcv)
library(here)
dir <- here::here()

# Read in sample survey data with environmental covariates
alldata <- readRDS(here(dir, "2-input-data", "combined-allsurveys-menhaden-data-20260414-with-chl.rds"))
alldata$State <- as.factor(alldata$State)
alldata$Survey <- as.factor(alldata$Survey)
alldata$Season <- as.factor(alldata$Season)


# Create Spring and Fall datasets for models (verify 1972-2023)
alldata.spring <- alldata |>
  filter(Year >=1972 & Year <= 2023) |>
  filter(Season == "SPRING") |>
  arrange(Year)
alldata.fall <- alldata |>
  filter(Year >=1972 & Year <= 2023) |>
  filter(Season == "FALL") |>
  arrange(Year)


# Create list of datasets
data.list <- list(alldata.spring = alldata.spring, alldata.fall = alldata.fall)
saveRDS(data.list, here(dir, "2-input-data", "menhaden.data.list.062026.rds"))
colSums(is.na(data.list[[1]]))


# Create complete only
alldata.spring.comp <- alldata.spring |>
  filter(!is.na(Depth) & !is.na(SurfTemp) & !is.na(SurfSalin) & !is.na(BotTemp) & !is.na(BotSalin) & !is.na(mean_chlor_a))
alldata.fall.comp <- alldata.fall |>
  filter(!is.na(Depth) & !is.na(SurfTemp) & !is.na(SurfSalin) & !is.na(BotTemp) & !is.na(BotSalin) & !is.na(mean_chlor_a))  
data.list.comp <- list(alldata.spring = alldata.spring.comp, alldata.fall = alldata.fall.comp)
saveRDS(data.list.comp, here(dir, "2-input-data", "menhaden.data.list.comp.062026.rds"))

##############################################
#----- Presence/Absence GAM 
##############################################

pa.gam.list <- list()
pa.gam.summaries <- list()

#----- 1. Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + s(Depth) + s(SurfTemp) + s(SurfSalin) + s(mean_chlor_a)
for (name in names(data.list.comp)) {
  new.name <- paste0("m1_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Depth) + s(SurfTemp) +s(SurfSalin) + s(mean_chlor_a) + s(Survey, bs = "re"),
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 2. Presence ~ Year + State + Survey + s(Depth) + s(SurfTemp) + s(SurfSalin) + s(mean_chlor_a)
for (name in names(data.list.comp)) {
  new.name <- paste0("m2_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ Year + State + Depth + SurfTemp + s(Survey, bs = "re"), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}



#----- 3. Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + s(Depth) + s(SurfTemp) + s(SurfSalin) + s(BotTemp) + s(BotSalin) + s(mean_chlor_a)
for (name in names(data.list)) {
  new.name <- paste0("m3_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + Depth + SurfTemp + s(Survey, bs = "re"),
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}





unique(data.list[[1]]$Year)
plot(pa.gam.list[[3]], select = 2, shade = TRUE)
abline(h = 0, lty = "dashed")
  
plot(pa.gam.list[[5]], residuals = TRUE, pages = 1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)
summary(pa.gam.list[[3]])
gratia::draw(pa.gam.list[[3]])

library(marginaleffects)
plot_predictions(pa.gam.list[[3]], condition = "Year", type = "link")


#----- Check the models ---------------------------------------------------------------
library(corrplot)
cor(data.list[[1]][, c("Depth", "SurfTemp", "SurfSalin", "BotTemp", "BotSalin", "mean_chlor_a")], use = "complete.obs")
summary(pa.gam.list[[3]])

library(gratia)
gratia::draw(pa.gam.list[[4]])


# Get model coefficients to compare
library(modelsummary)
# pa.gam.table <- modelsummary(pa.gam.list, output = "data.frame")
pa.gam.table <- modelsummary(pa.gam.list, output = "gam-model-results-table-2026.docx")
plot(pa.gam.list[[1]], pages = 1, all.terms = TRUE, rug = TRUE, shade = TRUE)
coef(pa.gam.list[[1]])
