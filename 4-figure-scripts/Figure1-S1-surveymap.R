# Figure1-S1-surveymap.R
######################################
# Janelle L. Morano

# Supplemental Figure 1. map of surveys
# Figure 1. Map of state geographic areas
# AND Map inset for Fig 3 & 4 state region reference

# last updated 27 March 2025
###############################################
###############################################

setwd()

library(sf)
library(tidyverse)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)



#----- Load Survey shapefiles and survey locations ------------------------------------------

### NEFSC
nefsc <- st_read("~/BTS_Strata.shp")
# STR2 are the strata numbers
st_crs(nefsc)
# CRS: NAD83 (EPSG,4269)

### NEAMAP
neamap <- st_read("~/NMDepthStrataPolgyons.shp")
st_crs(neamap)
# CRS: NAD83 (EPSG,4269)

### CTLISTS
ct <- st_read("~/sitegrid_unproj.shp")
st_crs(ct)
# CRS: NAD83 (EPSG,4269)

### GA EMTS
# These are the intended survey points, which may differ from the recorded lat/lon in the data. This is due to some unexplained errors, so for mapping, only using the planned locations.
ga <- read.csv("~/GAEMTS Stations 2019.csv", header = TRUE)
ga.sf <- st_as_sf(ga, coords = c("LonBeg", "LatBeg"), crs = 4269)
st_crs(ga.sf)

### All other surveys with only Lat/Lon coordinates and no .shp
# Will need to ignore the GA data in here
state <- read.csv("~/statesurvey_menhaden_data_20240726.csv", header = TRUE)
state.sf <- st_as_sf(state, coords = c("Longitude", "Latitude"), crs = 4269)
st_crs(state.sf)
# sapply(state, function(x) sum(is.na(x)))



#----- Generate Figure 1 -----------------------------------------------------------------
pal3 <- c("#b5de2b", "#482878", "#1f9e89")

legendcols <- c(pal3[3], pal3[2])

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

#----- Fig.1. Map of surveys, color-coded by survey
spal <- brewer.pal(n = 12, name = "Set3")
#skip 9 b/c looks grey 

ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = nefsc, color = "white", fill = spal[1]) +
  geom_sf(data = neamap, color = spal[2]) +
  geom_sf(data = ct, color = spal[3]) +
  geom_sf(data = ga.sf, color = spal[4]) +
  geom_point(data = subset(state, Survey %in% "DEBay30ft"), aes(Longitude, Latitude), color = spal[5], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "MDGill"), aes(Longitude, Latitude), color = spal[6], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NCp915"), aes(Longitude, Latitude), color = spal[7], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = spal[8], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = spal[10], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "SEAMAP"), aes(Longitude, Latitude), color = spal[11], size = 0.25) +
  coord_sf (xlim = c(-83,-62), ylim = c (28,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  # theme_void() +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  # theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") 
# Legend will have to be added later
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/Fig-federal-state-survey-identify-map.png", width=5.5, height = 6)



#----- Generate Figure 2 Reference Color by State --------------------------------------------

#----- Add state designation to NEFSC shapefile (which will match the NEFSC/NEAMAP data)
# Add inshore/offshore designation
nefsc <- nefsc %>%
  mutate(inoffshore = case_when(STRATA <2000 ~ "offshore",
                                STRATA >3000 & STRATA <5000 ~ "inshore",                              STRATA >3000 & STRATA <5000 ~ "inshore",
                                STRATA >5000 ~ "offshore",
                                STRATA >=7000 & STRATA <8000 ~ "inshore",
                                STRATA >=8000 ~ "offshore")) %>%
  # Add state designation. Strata are assigned to the closest state waters. RI-CT-NY are collectively assigned because of the overlap of the strata with the span of the states and the location of the mouth of Long Island Sound.
  mutate(State = case_when(STRATA >=3570 & STRATA <=3990 ~ "GME", # Inshore GME (north of 42 lat) 
                           STRATA >=1260 & STRATA <=1420 ~ "GME", # Offshore GME
                           STRATA ==1490 ~ "GME", # Offshore GME
                           STRATA >=5430 & STRATA <=5480 ~ "GME", # Offshore GME
                           STRATA ==3460 ~ "MA", #Inshore MA
                           STRATA >=3480 & STRATA <=3560 ~ "MA", #Inshore MA
                           STRATA >=1090 & STRATA <=1250  ~ "MA", #Offshore MA (south of 42 lat)
                           STRATA >=3010 & STRATA <=3140 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3450 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3470 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3490 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3910 ~ "RICTNY", #Inshore RICTNY
                           STRATA >=1010 & STRATA <=1140 ~ "RICTNY", #Offshore RICTNY
                           STRATA >=3150 & STRATA <=3230 ~ "NJ", #Inshore NJ                
                           STRATA >=1730 & STRATA <=1760 ~ "NJ", #Offshore NJ
                           STRATA >=3240 & STRATA <=3290 ~ "DEMD", #Inshore DEMD
                           STRATA >=1690 & STRATA <=1720 ~ "DEMD", #Offshore DEMD
                           STRATA >=3300 & STRATA <=3380 ~ "VA", #Inshore VA
                           STRATA >=1650 & STRATA <=1680 ~ "VA", #Offshore VA
                           STRATA >=3390 & STRATA <=3580 ~ "NC", #Inshore NC and south
                           STRATA >=1610 & STRATA <=1640 ~ "NC", #Offshore NC and south
                           .default = "NC"))



pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha = 0.3) #make transparent


#----- Highlight Federal: Map Federal Data darker than State data
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = subset(nefsc, State %in% "GME"), color = "white", fill = pal8[1]) +
  geom_sf(data = subset(nefsc, State %in% "MA"), color = "white", fill = pal8[2]) +
  geom_sf(data = subset(nefsc, State %in% "RICTNY"), color = "white", fill = pal8[3]) +
  geom_sf(data = subset(nefsc, State %in% "NJ"), color = "white", fill = pal8[4]) +
  geom_sf(data = subset(nefsc, State %in% "DEMD"), color = "white", fill = pal8[5]) +
  geom_sf(data = subset(nefsc, State %in% "VA"), color = "white", fill = pal8[6]) +
  geom_sf(data = subset(nefsc, State %in% "NC"), color = "white", fill = pal8[7]) +
  geom_point(data = subset(state, State %in% "RICTNY"), aes(Longitude, Latitude), color = pal8lite[3], size = 0.25) +
  geom_point(data = subset(state, State %in% "NJ"), aes(Longitude, Latitude), color = pal8lite[4], size = 0.25) +
  geom_point(data = subset(state, State %in% "DEMD"), aes(Longitude, Latitude), color = pal8lite[5], size = 0.25) +
  geom_point(data = subset(state, State %in% "VA"), aes(Longitude, Latitude), color = pal8lite[6], size = 0.25) +
  geom_point(data = subset(state, State %in% "NC"), aes(Longitude, Latitude), color = pal8lite[7], size = 0.25) +
  geom_point(data = subset(state, State %in% "SCGAFL"), aes(Longitude, Latitude), color = pal8lite[8], size = 0.25) +
  coord_sf (xlim = c(-82.5,-64), ylim = c (28,45), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-82.5,-64), ylim = c (28,43), expand = FALSE ) + #tighter crop
  # theme_void() +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme(text = element_text(size = 14)) +
  # theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "Fig1-state-regions-map.png", width=5.5, height = 6)


