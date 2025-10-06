# MSmodel-Presence-GAM-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Data and choice GAM models for the Atlantic menhaden manuscript.

# last updated 27 July 2025

###############################################
###############################################

#-----Table of Contents -------------------------------

#----- Presence-Absence GAMMs
# 1. Presence ~ s(Year) + s(Survey, bs = "re")
# 2. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
# 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp)
# 4. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth)
# 5. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(WaterTemp)
# ***(best for fall) 6. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp
# ***(best for spring) 7. Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + WaterTemp
# 8. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)



#----- Predictions on selected model
# Save data for graphing in external file


###########################################################################################


library(tidyverse)
library(mgcv)

# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
colSums(is.na(data.list[[1]]))


###########################################################################################
#----- Presence/Absence GAM --------------------------------------------------
###########################################################################################

#----- 1. Presence ~ s(Year) + s(Survey, bs = "re")
pa.gam.list <- list()
pa.gam.summaries <- list()
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


#----- 3. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp)
for (name in names(data.list)) {
  new.name <- paste0("m3_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp), 
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


#----- 5. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(WaterTemp)
for (name in names(data.list)) {
  new.name <- paste0("m5_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(WaterTemp), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 6. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp
for (name in names(data.list)) {
  new.name <- paste0("m6_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 7. Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + WaterTem
for (name in names(data.list)) {
  new.name <- paste0("m7_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + WaterTemp, 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}


#----- 8. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)
for (name in names(data.list)) {
  new.name <- paste0("m8_", name)
  pa.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp), 
                                family = binomial(link = "logit"), 
                                method = "REML", 
                                data = data.list[[name]])
  pa.gam.summaries[[new.name]] <- summary(pa.gam.list[[new.name]])
}

#----- Save model runs as RDS
saveRDS(pa.gam.list, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-results.rds")
saveRDS(pa.gam.summaries, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-summaries.rds")

pa.gam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-results.rds")
pa.gam.summaries <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-summaries.rds")




#----- Check the models ---------------------------------------------------------------
# Get model coefficients to compare
library(modelsummary)
# pa.gam.table <- modelsummary(pa.gam.list, output = "data.frame")
pa.gam.table <- modelsummary(pa.gam.list, output = "gam-model-results-table.docx")

# Print list of model formulas (only the spring models since the falls are duplicates)
for (i in seq(1, length(pa.gam.list), by = 2)) {
  cat("Model", i, "formula:\n")
  print(formula(pa.gam.list[[i]]))
  cat("\n")
}
# Open the Word doc "gam-model-results-table.docx" and add formulas


# Review individuals
# m1: Presence ~ s(Year) + s(Survey, bs = "re")
summary(pa.gam.list[[1]])
gratia::draw(pa.gam.list[[1]])
summary(pa.gam.list[[2]])
gratia::draw(pa.gam.list[[2]])

# m2: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
summary(pa.gam.list[[3]])
gratia::draw(pa.gam.list[[3]])
summary(pa.gam.list[[4]])
gratia::draw(pa.gam.list[[4]])

# m3: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp)
summary(pa.gam.list[[5]])
gratia::draw(pa.gam.list[[5]])
summary(pa.gam.list[[6]])
gratia::draw(pa.gam.list[[6]])

# m4: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth)
summary(pa.gam.list[[7]])
gratia::draw(pa.gam.list[[7]])
summary(pa.gam.list[[8]])
gratia::draw(pa.gam.list[[8]])

# m5: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(Depth) + s(WaterTemp)
summary(pa.gam.list[[9]])
gratia::draw(pa.gam.list[[9]])
summary(pa.gam.list[[10]])
gratia::draw(pa.gam.list[[10]])

# ***(best for fall) m6: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp
summary(pa.gam.list[[11]])
gratia::draw(pa.gam.list[[11]])
plot(pa.gam.list[[12]], select =11)

summary(pa.gam.list[[12]]) #***(best for fall)
gratia::draw(pa.gam.list[[12]])
plot(pa.gam.list[[12]], pages = 1, scheme = 1, seWithMean = TRUE)
# library(DHARMa)

# ***(best for spring) m7. Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + WaterTemp
summary(pa.gam.list[[13]]) #***(best for spring)
gratia::draw(pa.gam.list[[13]])
plot(pa.gam.list[[13]], pages = 1, scheme = 1, seWithMean = TRUE)

summary(pa.gam.list[[14]])
gratia::draw(pa.gam.list[[14]])

# m8: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)
summary(pa.gam.list[[15]])
gratia::draw(pa.gam.list[[15]])


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
      new <- data.frame(State = i,
                        Year = j,
                        Survey = k,
                        Depth = median(data.list$alldata.fall$Depth[data.list$alldata.fall$State == i], na.rm=TRUE),
                        WaterTemp = median(data.list$alldata.fall$WaterTemp[data.list$alldata.fall$State == i & data.list$alldata.fall$Year == j], na.rm=TRUE))
      alld.preddata.pa.gam.fall <- rbind(alld.preddata.pa.gam.fall, new)
    }
  }
}


#----- Create a list of the New Predicting Data datasets
preddata.pa.gam.list <- list(alld.preddata.pa.gam.spring = alld.preddata.pa.gam.spring, alld.preddata.pa.gam.fall = alld.preddata.pa.gam.fall)

# Convert State to factor with ordered levels from North to South
preddata.pa.gam.list[[1]]$State <- factor(preddata.pa.gam.list[[1]]$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
preddata.pa.gam.list[[2]]$State <- factor(preddata.pa.gam.list[[2]]$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
# Convert Survey to factor
preddata.pa.gam.list[[1]]$Survey <- factor(preddata.pa.gam.list[[1]]$Survey)
preddata.pa.gam.list[[2]]$Survey <- factor(preddata.pa.gam.list[[2]]$Survey)

                                          
saveRDS(preddata.pa.gam.list , file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-preddata.rds")

preddata.pa.gam.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-preddata.rds")




###########################################################################################
#----- Predictions using model #7SPRING for spring; model #6 for fall --------------------------
###########################################################################################

# SPRING m7: Presence ~ s(Year) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)
# pa.gam.list[[13]]
predictions.pa.gam.7.spring <- predict(pa.gam.list[[13]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response", exclude = "s(Survey)")

# FALL m6: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp
# pa.gam.list[[12]]
predictions.pa.gam.6.fall <- predict(pa.gam.list[[12]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response", exclude = "s(Survey)")

# Create Indiv Datasets of Predictions
predictions.pa.gam.7.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.pa.gam.7.spring))
predictions.pa.gam.6.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.pa.gam.6.fall))

# Make list of Dataframes of Predictions
predictions.pa.gam.list <- list(predictions.pa.gam.7.spring.df = predictions.pa.gam.7.spring.df,
                                predictions.pa.gam.6.fall.df = predictions.pa.gam.6.fall.df)
saveRDS(predictions.pa.gam.list, file = "/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")
    
