# download-remote-sensing-data.r

# Purpose: Download remote sensing data (e.g., chlorophyll-a) from ERDDAP for the specified years and geographic range.

rm (list = ls())  # Clear the workspace
library(terra)
library(rerddap)
library(dplyr)
library(lubridate)
library(here)
dir <-here()



#----- 1. Define data source and parameters

  #----- 1.1 Remote sensing data source
    # Source Options
    # OceanWatch: "https://oceanwatch.pifsc.noaa.gov/erddap/"

  # Define the dataset ID for download
  url <- "https://oceanwatch.pifsc.noaa.gov/erddap/"

    # Search for specific dataset at url
    fieldsearch <- rerddap::ed_search(query = "Chlorophyll-a", which = "griddap", url = url)
    head(fieldsearch$info)

  # Define the dataset ID for download
  dataset <- "esa-cci-chla-8d-v6-0"


  #----- 1.2 Field name(s) of dataset
    # Fetch all metadata for this specific dataset
    dataset.info <- rerddap::info(datasetid = dataset, url = url)
    names(dataset.info$alldata)

  # Define the data field to download (e.g., chlorophyll-a)
  fields <- "chlor_a"


  #----- 1.3 Define geographic and temporal range
  years <- 1997:2023
  lat.range <- c(49, 29)  # Reversed to match the server's North-to-South layout
  lon.range <- c(278, 298)  #Convert to 0-360 by adding 360 to negative (i.e. from c(-82, -62))



# Fetch the structural metadata for the coordinates
meta <- rerddap::info("esa-cci-chla-8d-v6-0", url = "https://oceanwatch.pifsc.noaa.gov/erddap/")
meta$alldata$longitude
meta$alldata$latitude



#----- 2. Download data as fall and spring chunks through years
#----- 2. Loop Through Years using Small, Fast 2-Month Windows
for (yr in years) {
  message(paste("=====> Processing Year:", yr))
  
  if (yr == 1997) {
    chunks <- list(
      sep_oct = c("1997-09-04T00:00:00Z", "1997-10-31T00:00:00Z"),
      nov_dec = c("1997-11-01T00:00:00Z", "1997-12-31T00:00:00Z")
    )
  } else {
    chunks <- list(
      jan_feb = c(paste0(yr, "-01-01T00:00:00Z"), paste0(yr, "-02-28T00:00:00Z")),
      mar_apr = c(paste0(yr, "-03-01T00:00:00Z"), paste0(yr, "-04-30T00:00:00Z")),
      may_jun = c(paste0(yr, "-05-01T00:00:00Z"), paste0(yr, "-06-30T00:00:00Z")),
      jul_aug = c(paste0(yr, "-07-01T00:00:00Z"), paste0(yr, "-08-31T00:00:00Z")),
      sep_oct = c(paste0(yr, "-09-01T00:00:00Z"), paste0(yr, "-10-31T00:00:00Z")),
      nov_dec = c(paste0(yr, "-11-01T00:00:00Z"), paste0(yr, "-12-31T00:00:00Z"))
    )
  }
  
  year_filtered_list <- list()
  
  # Process each tiny chunk
  for (chunk_name in names(chunks)) {
    time_bounds <- chunks[[chunk_name]]
    
    tryCatch({
      grid.chunk <- rerddap::griddap(
        datasetx = dataset, 
        url = url,
        time = time_bounds,
        latitude = lat.range,
        longitude = lon.range,
        fields = fields,
        store = disk() 
      )
      
      df.raw <- as.data.frame(grid.chunk)
      
      # FIXED Operator here: changed %in= to %in%
      df.filtered <- df.raw %>%
        mutate(time = as.Date(time)) %>%
        filter(month(time) %in% c(3, 4, 9, 10)) 
      
      if (nrow(df.filtered) > 0) {
        year_filtered_list[[chunk_name]] <- df.filtered
      }
      
      rm(grid.chunk, df.raw, df.filtered)
      gc()
      
    }, error = function(e) {
      message(paste("   Skipped/Failed block", chunk_name, "due to:", e$message))
    })
  }
  
  # Save the consolidated matching data for the year
  if (length(year_filtered_list) > 0) {
    df.annual_seasonal <- do.call(rbind, year_filtered_list)
    filename <- paste0("chl_seasonal_data_", yr, ".csv")
    write.csv(df.annual_seasonal, filename, row.names = FALSE)
    
    message(paste(" Successfully saved consolidated file:", filename))
    rm(df.annual_seasonal)
    gc()
  } else {
    message(paste(" No seasonal data collected for year:", yr))
  }
}







############################### with tidync
library(rerddap)
library(tidync)
library(dplyr)
library(lubridate)

#----- 1. Define data source and parameters
url <- "https://oceanwatch.pifsc.noaa.gov/erddap/"
dataset <- "esa-cci-chla-8d-v6-0"
fields <- "chlor_a" 

years <- 1997:2023
lat.range <- c(29, 49)   # tidync handles orientation automatically
lon.range <- c(278, 298) 

#----- 2. Loop Through Years with tidync
for (yr in years) {
  message(paste("=====> Processing Year:", yr))
  
  if (yr == 1997) {
    time_bounds <- c("1997-09-04T00:00:00Z", "1997-12-31T00:00:00Z")
  } else {
    time_bounds <- c(paste0(yr, "-01-01T00:00:00Z"), paste0(yr, "-12-31T00:00:00Z"))
  }
  
  tryCatch({
    # 1. Download the raw NetCDF file to local disk (DO NOT use as.data.frame here)
    nc_file <- rerddap::griddap(
      datasetx = dataset, 
      url = url,
      time = time_bounds,
      latitude = lat.range,
      longitude = lon.range,
      fields = fields,
      store = disk() # Saves raw .nc file to local cache
    )
    
    # Extract the actual file path of the downloaded NetCDF
    raw_path <- nc_file$summary$filename
    
    # 2. Initialize tidync on the raw file
    src <- tidync(raw_path)
    
    # 3. Extract the data grid using hyper_tibble()
    # This completely bypasses the broken rerddap parser!
    df.raw <- src %>% 
      hyper_tibble()
    
    # 4. Convert the server's internal time format and filter for your months
    # Note: ERDDAP NetCDFs usually store time as seconds since 1970-01-01
    df.filtered <- df.raw %>%
      mutate(time = as.Date(as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))) %>%
      filter(month(time) %in% c(3, 4, 9, 10))
    
    # 5. Save the matching seasonal data
    if (nrow(df.filtered) > 0) {
      filename <- paste0("chl_seasonal_data_", yr, ".csv")
      write.csv(df.filtered, filename, row.names = FALSE)
      message(paste(" Successfully saved consolidated file:", filename))
    } else {
      message(paste(" No matching seasonal data found for year:", yr))
    }
    
    # Clean up memory
    rm(nc_file, src, df.raw, df.filtered)
    gc()
    
  }, error = function(e) {
    message(paste("   Failed processing year", yr, "due to:", e$message))
  })
}
