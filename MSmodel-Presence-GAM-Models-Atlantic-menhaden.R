# MSmodel-Presence-GAM-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Data and choice GAM models for the Atlantic menhaden manuscript.

# last updated 24 March 2025

###############################################
###############################################

#-----Table of Contents -------------------------------

#----- Presence-Absence GAMMs
# 1. Presence ~ s(Year) + s(Survey, bs = "re")
# 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
# 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
# 4. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp + Chlorophyll-a


#----- Summarize AIC, Deviance explained, Rsquared

#----- Predictions on selected model
# Save data for graphing in external file


###########################################################################################


library(tidyverse)
library(mgcv)

# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/data.list.rds")




###########################################################################################
#----- Presence/Absence GAM --------------------------------------------------
###########################################################################################

#----- 1. Presence ~ s(Year) + s(Survey, bs = "re")
pa.gam.list <- list()
pa.gam.summaries <- list()
for (name in names(data.list)) {
  new.name <- paste0("1_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + s(Survey, bs = "re"), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
}

#----- 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
for (name in names(data.list)) {
  new.name <- paste0("2_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re"), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
}


#----- 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
for (name in names(data.list)) {
  new.name <- paste0("3_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 4. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp + AveChlor
# for (name in names(data.list)) {
#   new.name <- paste0("4_", name)
#   pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp) + s(AveChlor), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
#   pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
# }




#----- Save model runs as RDS
saveRDS(pa.gam.list, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-results.rds")
saveRDS(pa.gam.summaries, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-summaries.rds")


#----- Check the models ---------------------------------------------------------------
# Get model coefficients to compare
library(modelsummary)
pa.gam.table <- modelsummary(pa.gam.list, output = "data.frame")
write.csv(pa.gam.table, "/Users/janellemorano/Git/menhaden-dist-ms/gam-table.csv")


# Review individuals
summary(pa.gam.list[[1]])
gam.check(pa.gam.list[[2]])
concurvity(pa.gam.list[[2]], full = TRUE) #if values are high, >0.8, run with FALSE
concurvity(pa.gam.list, full = FALSE)
plot(pa.gam.list[[1]], rug = TRUE, residuals = TRUE, pch = 1, cex = 1, shift = coef(pa.gam.list[[1]])[1])


# Likelihood Ratio Test
# anova(null or simple model, complex_model, test = "LRT")
anova(pa.gam.list[[1]], pa.gam.list[[3]], test = "LRT")
# models were not all fitted to the same size of dataset
  
summary(pa.gam.list[[1]])$n
summary(pa.gam.list[[3]])$n
# summary(pa.gam.list[[5]])$n



###########################################################################################
#----- Make New Data for Predictions --------------------------------------------------
###########################################################################################

#-- Alldata Spring
alld.preddata.pa.gam.spring <- data.frame()
for (i in unique(data.list$alldata.spring$State)) {
  for (j in unique(data.list$alldata.spring$Year)) {
    for(k in unique(data.list$alldata.spring$Survey)) {
      new <- data.frame(Year = j,
                        State = i,
                        Survey = k,
                        WaterTemp = median(data.list$alldata.spring$WaterTemp[data.list$alldata.spring$State == i & data.list$alldata.spring$Year == j], na.rm=TRUE))
      alld.preddata.pa.gam.spring <- rbind(alld.preddata.pa.gam.spring, new)
    }
  }
}

#-- Alldata Fall
alld.preddata.pa.gam.fall <- data.frame()
for (i in unique(data.list$alldata.fall$State)) {
  for (j in unique(data.list$alldata.fall$Year)) {
    for (k in unique(data.list$alldata.fall$Survey)) {
      new <- data.frame(Year = j,
                        State = i,
                        Survey = k,
                        WaterTemp = median(data.list$alldata.fall$WaterTemp[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE))
      alld.preddata.pa.gam.fall <- rbind(alld.preddata.pa.gam.fall, new)
    }
  }
}


#----- Create a list of the New Predicting Data datasets
preddata.pa.gam.list <- list(alld.preddata.pa.gam.spring = alld.preddata.pa.gam.spring, alld.preddata.pa.gam.fall = alld.preddata.pa.gam.fall)
saveRDS(preddata.pa.gam.list , file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-preddata.rds")

preddata.pa.gam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-preddata.rds")




###########################################################################################
#----- Predictions using model #3  (and #1)--------------------------------------------------
###########################################################################################

# 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
predictions.pa.gam.3.spring <- predict(pa.gam.list[[5]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response", exclude = "s(Survey)")
predictions.pa.gam.3.fall <- predict(pa.gam.list[[6]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response", exclude = "s(Survey)")
# 1. Presence ~ s(Year) + s(Survey, bs = "re")
predictions.pa.gam.1.spring <- predict(pa.gam.list[[1]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response", exclude = "s(Survey)")
predictions.pa.gam.1.fall <- predict(pa.gam.list[[2]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response", exclude = "s(Survey)")

# Create Indiv Datasets of Predictions
predictions.pa.gam.3.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.pa.gam.3.spring))
predictions.pa.gam.3.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.pa.gam.3.fall))
predictions.pa.gam.1.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.pa.gam.1.spring))
predictions.pa.gam.1.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.pa.gam.1.fall))

# Make list of Dataframes of Predictions
predictions.pa.gam.list <- list(predictions.pa.gam.3.spring.df = predictions.pa.gam.3.spring.df, predictions.pa.gam.3.fall.df = predictions.pa.gam.3.fall.df, predictions.pa.gam.1.spring.df = predictions.pa.gam.1.spring.df, predictions.pa.gam.1.fall.df = predictions.pa.gam.1.fall.df)
saveRDS(predictions.pa.gam.list, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")
    
