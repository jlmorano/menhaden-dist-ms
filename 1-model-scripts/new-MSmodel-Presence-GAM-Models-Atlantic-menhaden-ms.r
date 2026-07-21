# new-MSmodel-Presence-GAM-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Duplicate of MSmodel-Presence-GAM-Models-Atlantic-menhaden.R, but with new models added and some models removed.

# last updated 2 July 2026 (WaterTemp changed to SurfTemp so that it can be used alternatively to BotTemp)

###############################################
###############################################

#-----Table of Contents -------------------------------

#----- Presence-Absence GAMMs
#----- Presence-Absence GAMMs
# 1. Presence ~ s(Year) + s(Survey, bs = "re")
# 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
# 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(SurfTemp)
# 4. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth)
# 5. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(SurfTemp)
# ***(best for fall) 6. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp
# ***(best for spring) 7. Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp
# 8. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(SurfTemp)
# 
# NEW - Add salinity
# 9. (for fall) Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin
# 10. (for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin

# NEW - Add chlorophyll-a, but truncate to 1997-2023
# 11. (for fall) Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + chlor_a
# 12. (for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + chlor_a


rm(list = ls())
gc()

library(tidyverse)
library(mgcv)
library(gratia)
library(marginaleffects)
library(tictoc)
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


# Create 1997-2023 with chlor-a
alldata.spring.comp <- alldata.spring |>
  filter(Year >=1997 & Year <= 2023) 
alldata.fall.comp <- alldata.fall |>
  filter(Year >=1997 & Year <= 2023) 
data.list.comp <- list(alldata.spring = alldata.spring.comp, alldata.fall = alldata.fall.comp)
saveRDS(data.list.comp, here(dir, "2-input-data", "menhaden.data.list.comp.062026.rds"))



##############################################
#----- Presence/Absence GAM 
##############################################

pa.gam.list <- list()
pa.gam.summaries <- list()

tic()
#----- 1. Presence ~ s(Year) + s(Survey, bs = "re")
for (name in names(data.list)) {
  new.name <- paste0("m1_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + s(Survey, bs = "re"),
                                family = binomial(link = "logit"),
                                method = "REML",
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

#----- 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
for (name in names(data.list)) {
  new.name <- paste0("m2_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re"),
                                family = binomial(link = "logit"),
                                method = "ML",
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(SurfTemp)
for (name in names(data.list)) {
  new.name <- paste0("m3_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(SurfTemp), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 4. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth)
for (name in names(data.list)) {
  new.name <- paste0("m4_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth), 
                                family = binomial(link = "logit"), 
                                method = "ML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 5. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(SurfTemp)
for (name in names(data.list)) {
  new.name <- paste0("m5_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(SurfTemp), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 6. ***(best for fall) Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp
for (name in names(data.list)) {
  new.name <- paste0("m6_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 7. ***(best for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp
for (name in names(data.list)) {
  new.name <- paste0("m7_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 8. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(SurfTemp)
for (name in names(data.list)) {
  new.name <- paste0("m8_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(SurfTemp), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


# NEW - Add salinity
# 9. (for fall) Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin
for (name in names(data.list)) {
  new.name <- paste0("m9_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

# 10. (for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin
for (name in names(data.list)) {
  new.name <- paste0("m10_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

# NEW - Add chlorophyll-a, but truncate to 1997-2023
# 11. (for fall) Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + mean_chlor_a
for (name in names(data.list.comp)) {
  new.name <- paste0("m11_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + mean_chlor_a, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

# 12. (for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + mean_chlor_a
for (name in names(data.list.comp)) {
  new.name <- paste0("m12_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + SurfSalin + mean_chlor_a, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


 #NEW - Add chlorophyll-a, drop salinity, truncate to 1997-2023
# 13. (for fall) Presence ~ s(Year, by State) + State + s(Survey, bs = "re") + Depth + SurfTemp + mean_chlor_a
for (name in names(data.list.comp)) {
  new.name <- paste0("m13_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp + mean_chlor_a, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

# 14. (for spring) Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + mean_chlor_a
for (name in names(data.list.comp)) {
  new.name <- paste0("m14_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + SurfTemp + mean_chlor_a, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list.comp[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

#----- Save model runs as RDS
saveRDS(pa.gam.list, file = "PA-GAM-results-2026-July20.rds")
saveRDS(pa.gam.summaries, file = "PA-GAM-summaries-2026-July20.rds")
toc()

pa.gam.list <- readRDS("PA-GAM-results-2026-July20.rds")
pa.gam.summaries <- readRDS("PA-GAM-summaries-2026-July20.rds")




##############################################
#----- Model Diagnostics
##############################################

plot(pa.gam.list[[9]], select = 3, shade = TRUE)
plot(pa.gam.list[[12]], select = 3, shade = TRUE)
plot(pa.gam.list[[14]], select = 3, shade = TRUE)

summary(pa.gam.list[[1]])
summary(pa.gam.list[[2]])
summary(pa.gam.list[[3]])
summary(pa.gam.list[[4]])
summary(pa.gam.list[[5]])
summary(pa.gam.list[[6]])
summary(pa.gam.list[[7]])
summary(pa.gam.list[[8]])
summary(pa.gam.list[[9]]) #Best for spring
summary(pa.gam.list[[10]])
summary(pa.gam.list[[11]])
summary(pa.gam.list[[12]]) #Best for fall
summary(pa.gam.list[[14]]) #fall add salinity

AIC(pa.gam.list[[1]], pa.gam.list[[2]], pa.gam.list[[3]], pa.gam.list[[4]], pa.gam.list[[5]], pa.gam.list[[6]], pa.gam.list[[7]], pa.gam.list[[8]], pa.gam.list[[9]], pa.gam.list[[10]], pa.gam.list[[11]], pa.gam.list[[12]], pa.gam.list[[13]], pa.gam.list[[14]])

library(performance)
compare_performance(pa.gam.list[[1]], pa.gam.list[[2]], pa.gam.list[[3]], pa.gam.list[[4]], pa.gam.list[[5]], pa.gam.list[[6]], pa.gam.list[[7]], pa.gam.list[[8]], pa.gam.list[[9]], pa.gam.list[[10]], pa.gam.list[[11]], pa.gam.list[[12]], pa.gam.list[[13]], pa.gam.list[[14]])


concurvity(pa.gam.list[[4]], full = TRUE)
concurvity(pa.gam.list[[4]], full = FALSE)


# partial effects on the link scale


# Best for spring
plot_predictions(pa.gam.list[[9]], condition = c("Year", "State"), type = "link") +
  labs(x = "Year", y = "Linear predictor (link scale)") +
  theme_classic()

# Best for fall
plot_predictions(pa.gam.list[[12]], condition = c("Year", "State"), type = "link") +
  labs(x = "Year", y = "Linear predictor (link scale)") +
  theme_classic()

# fall: add salinity
plot_predictions(pa.gam.list[[14]], condition = c("Year", "State"), type = "link") +
  labs(x = "Year", y = "Linear predictor (link scale)") +
  theme_classic()


# how the slope of fitted function changes across range of the covariate
plot_slopes(pa.gam.list[[9]], variables = "Year", condition = c("Year", "State"), type = "link") +
geom_hline(yintercept = 0, linetype = "dashed") +
labs(y = "1st derivative of linear predictor (link scale)")

plot_slopes(pa.gam.list[[12]], variables = "Year", condition = c("Year", "State"), type = "link") +
geom_hline(yintercept = 0, linetype = "dashed") +
labs(y = "1st derivative of linear predictor (link scale)")

plot_slopes(pa.gam.list[[14]], variables = "Year", condition = c("Year", "State"), type = "link") +
geom_hline(yintercept = 0, linetype = "dashed") +
labs(y = "1st derivative of linear predictor (link scale)")

# GAM effects on the response scale
plot_predictions(pa.gam.list[[9]], condition = c("Year", "State"), type = "response", rug = FALSE) +
  labs(x = "Year", y = "Predicted probability of presence") +
  theme_classic()
plot_predictions(pa.gam.list[[12]], condition = c("Year", "State"), type = "response", rug = FALSE) +
  labs(x = "Year", y = "Predicted probability of presence") +
  theme_classic()
plot_predictions(pa.gam.list[[14]], condition = c("Year", "State"), type = "response", rug = FALSE) +
  labs(x = "Year", y = "Predicted probability of presence") +
  theme_classic()

# Average smooth effect of
plot_predictions(pa.gam.list[[9]], condition = c("Year", "State", "SurfTemp"), type = "response", points = 0.5) +
  labs(x = "Year", y = "Expected response") +
  theme_classic()
plot_predictions(pa.gam.list[[12]], condition = c("Year", "State", "SurfTemp"), type = "response", points = 0.5) +
  labs(x = "Year", y = "Expected response") +
  theme_classic()
plot_predictions(pa.gam.list[[14]], condition = c("Year", "State", "SurfTemp"), type = "response", points = 0.5) +
  labs(x = "Year", y = "Expected response") +
  theme_classic()

#########
plot(pa.gam.list[[5]], select = 2, shade = TRUE)
abline(h = 0, lty = "dashed")

unique(data.list[[1]]$Year)
plot(pa.gam.list[[3]], select = 2, shade = TRUE)
abline(h = 0, lty = "dashed")
  
plot(pa.gam.list[[8]], residuals = TRUE, pages = 1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue", seWithMean = TRUE)
summary(pa.gam.list[[5]])
gratia::draw(pa.gam.list[[3]])

library(marginaleffects)
plot_predictions(pa.gam.list[[3]], condition = "Year", type = "link")


#----- Check the models ---------------------------------------------------------------
library(corrplot)
cor(data.list[[1]][, c("Depth", "SurfTemp", "SurfSalin", "BotTemp", "BotSalin", "mean_chlor_a")], use = "complete.obs")
summary(pa.gam.list[[3]])

library(gratia)
gratia::draw(pa.gam.list[[12]])
gratia::draw(pa.gam.list[[14]])
gratia::draw(pa.gam.list[[3]])
gratia::draw(pa.gam.list[[4]])

# Get model coefficients to compare
library(modelsummary)
# pa.gam.table <- modelsummary(pa.gam.list, output = "data.frame")
pa.gam.table <- modelsummary(pa.gam.list, output = "gam-model-results-table-2026NEW.docx")
pa.gam.table <- modelsummary(pa.gam.list, output = "gam-model-results-table-2026NEW.html")
plot(pa.gam.list[[1]], pages = 1, all.terms = TRUE, rug = TRUE, shade = TRUE)
coef(pa.gam.list[[1]])








###########################################################################################
#----- Make New Data for Predictions --------------------------------------------------
###########################################################################################

#-- Alldata Spring
alld.preddata.pa.gam.spring <- data.frame()
for (i in unique(data.list$alldata.spring$State)) {
  for (j in unique(data.list$alldata.spring$Year)) {
    for(k in unique(data.list$alldata.spring$Survey)) {
      new <- data.frame(State = i,
                        Year = j,
                        Survey = k,
                        Depth = median(data.list$alldata.spring$Depth[data.list$alldata.spring$State == i & data.list$alldata.spring$Year == j], na.rm=TRUE),
                        SurfTemp = median(data.list$alldata.spring$SurfTemp[data.list$alldata.spring$State == i & data.list$alldata.spring$Year == j], na.rm=TRUE),
                        SurfSalin = median(data.list$alldata.spring$SurfSalin[data.list$alldata.spring$State == i & data.list$alldata.spring$Year == j], na.rm=TRUE),
                        mean_chlor_a = median(data.list$alldata.spring$mean_chlor_a[data.list$alldata.spring$State == i & data.list$alldata.spring$Year == j], na.rm=TRUE))
      alld.preddata.pa.gam.spring <- rbind(alld.preddata.pa.gam.spring, new)
    }
  }
}

#-- Alldata Fall
alld.preddata.pa.gam.fall <- data.frame()
for (i in unique(data.list$alldata.fall$State)) {
  for (j in unique(data.list$alldata.fall$Year)) {
    for (k in unique(data.list$alldata.fall$Survey)) {
      new <- data.frame(State = i,
                        Year = j,
                        Survey = k,
                        Depth = median(data.list$alldata.fall$Depth[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE),
                        SurfTemp = median(data.list$alldata.fall$SurfTemp[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE),
                        SurfSalin = median(data.list$alldata.fall$SurfSalin[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE),
                        mean_chlor_a = median(data.list$alldata.fall$mean_chlor_a[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE))
      alld.preddata.pa.gam.fall <- rbind(alld.preddata.pa.gam.fall, new)
    }
  }
}


#----- Create a list of the New Predicting Data datasets
preddata.pa.gam.list <- list(alld.preddata.pa.gam.spring = alld.preddata.pa.gam.spring, alld.preddata.pa.gam.fall = alld.preddata.pa.gam.fall)

# Convert State to factor with ordered levels from North to South
preddata.pa.gam.list[[1]]$State <- factor(preddata.pa.gam.list[[1]]$State, levels = c("MENH", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
preddata.pa.gam.list[[2]]$State <- factor(preddata.pa.gam.list[[2]]$State, levels = c("MENH", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
# Convert Survey to factor
preddata.pa.gam.list[[1]]$Survey <- factor(preddata.pa.gam.list[[1]]$Survey)
preddata.pa.gam.list[[2]]$Survey <- factor(preddata.pa.gam.list[[2]]$Survey)

                                          
saveRDS(preddata.pa.gam.list , file = "PA-GAM-preddata.rds")

preddata.pa.gam.list <- readRDS("PA-GAM-preddata.rds")




###########################################################################################
#----- Predictions using model #7SPRING for spring; model #6 for fall --------------------------
###########################################################################################

# SPRING m7: Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + s(SurfTemp)
# pa.gam.list[[13]]
predictions.pa.gam.7.spring <- predict(pa.gam.list[[13]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response", exclude = "s(Survey)")

# FALL m6: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + SurfTemp
# pa.gam.list[[12]]
predictions.pa.gam.6.fall <- predict(pa.gam.list[[12]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response", exclude = "s(Survey)")

# Create Indiv Datasets of Predictions
predictions.pa.gam.7.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.pa.gam.7.spring))
predictions.pa.gam.6.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.pa.gam.6.fall))

# Make list of Dataframes of Predictions
predictions.pa.gam.list <- list(predictions.pa.gam.7.spring.df = predictions.pa.gam.7.spring.df,
                                predictions.pa.gam.6.fall.df = predictions.pa.gam.6.fall.df)
saveRDS(predictions.pa.gam.list, file = "PA-GAM-predictions.rds")