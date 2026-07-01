# spatial-join-remote-sensing-data.r
######################################
# Janelle L. Morano

# Load chlorophyll data from the "chl" folder and calculate seasonal averages for spring = Month = c(3, 4) and fall = Month = c(9, 10). Then match the chlorophyll data to the sample survey data by Year, Month, Latitude, and Longitude.


# last updated 30 June 2025
###############################################
###############################################

rm(list = ls())

library(sf)
library(dplyr)
library(data.table)
library(here)
dir <- here::here()



#----- 1. Load chlorophyll data from the "chl" folder

  #----- 1.1 Define folder path
  chl.folder <- here::here("chl")

  #----- 1.2 List all files in folder
  chl.files <- list.files(path = chl.folder, pattern = "chl_seasonal_data_.*\\.csv", full.names = TRUE)

  #----- 1.3 Combine all chlorophyll files into a single dataframe and calculate seasonal averages for spring = Month = c(3, 4) and fall = Month = c(9, 10)
  message("Calculating seasonal chlorophyll averages with data.table...")
  list.chl.files <- lapply(chl.files, function(file) {
    dt <- fread(file, select = c("time", "latitude", "longitude", "chlor_a"))
      
    dt[, Month := month(as.IDate(time))]
      dt <- dt[Month %in% c(3, 4, 9, 10)]
      dt[, Season := fifelse(Month %in% c(3, 4), "SPRING", "FALL")]
      dt[, Year := year(as.IDate(time))]
      dt[, lon_180 := fifelse(longitude > 180, longitude - 360, longitude)]
      
      # Aggregate to seasonal means per coordinate
      dt.summary <- dt[, .(mean_chlor_a = mean(chlor_a, na.rm = TRUE)), 
                      by = .(latitude, lon_180, Year, Season)]
      return(dt.summary)
    })

  chl.grid.dt <- rbindlist(list.chl.files)

  # Collapse duplicates across boundaries
  chl.grid.dt <- chl.grid.dt[, .(mean_chlor_a = mean(mean_chlor_a, na.rm = TRUE)), 
                              by = .(latitude, lon_180, Year, Season)]

  #----- 1.4 Convert the grid to an sf spatial object (using WGS84 EPSG:4326)
  chl.grid.sf <- st_as_sf(chl.grid.dt, coords = c("lon_180", "latitude"), crs = 4326)




#----- 2. Load survey data
message("Loading survey data...")
allsurveys <- read_csv(here(dir, "2-input-data", "combined-allsurveys-menhaden-data-20260414.csv"), show_col_types = FALSE)

allsurveys <- allsurveys |> 
  mutate(row_id = row_number()) |> # add distinct row_id to keep track of rows after filtering
  mutate(Year = as.integer(Year))

# Filter surveys to only include years where chlorophyll data actually exists
shared.years <- intersect(allsurveys$Year, chl.grid.sf$Year)
allsurveys.filtered <- allsurveys |> filter(Year %in% shared.years)



#----- 3. Perform spatial nearest-feature matching to assign chlorophyll values to survey stations
message("Performing spatial nearest-feature matching...")

matched.surveys.list <- list()
# Loop through each Year and Season combo to find the geographically closest pixel
for (yr in shared.years) {
  for (seas in c("SPRING", "FALL")) {
    
    # Slice data for this specific time block
    survey.slice <- allsurveys.filtered |> filter(Year == yr, Season == seas)
    grid.slice   <- chl.grid.sf |> filter(Year == yr, Season == seas)
    
    # Skip if either slice has no data
    if (nrow(survey.slice) == 0 || nrow(grid.slice) == 0) next
    
    # Convert this survey slice to an sf object
    survey.slice.sf <- st_as_sf(survey.slice, coords = c("Longitude", "Latitude"), crs = 4326)
    
    # Find the index of the absolute nearest chlorophyll pixel for every survey station
    nearest_pixel_idx <- st_nearest_feature(survey.slice.sf, grid.slice)
    
    # Extract the chlorophyll values and attach back to the slice
    survey.slice$mean_chlor_a <- grid.slice$mean_chlor_a[nearest_pixel_idx]
    
    # Store the matched data frame
    matched.surveys.list[[paste(yr, seas)]] <- survey.slice
  }
}

# Combine all matched slices back into one master data frame
allsurveys.final <- bind_rows(matched.surveys.list)

# Verify results
print(range(allsurveys.final$mean_chlor_a, na.rm = TRUE))
summary(allsurveys.final$mean_chlor_a)



#----- 4. Merge the matched chlorophyll data back with the original survey data
allsurveys.complete <- allsurveys |>
  left_join(
    allsurveys.final |> select(row_id, mean_chlor_a),
    by = "row_id"
  ) |>
  select(-row_id) # Remove the temporary row_id column


# Save the updated dataframe with chlorophyll data
saveRDS(allsurveys.complete, here(dir, "2-input-data", "combined-allsurveys-menhaden-data-20260414-with-chl.rds"))
