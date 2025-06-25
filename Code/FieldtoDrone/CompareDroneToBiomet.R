# Biometric gradients were established at US-SRG and US-SRM for validation of 
# drone-collected data. This script compares the area, height, and diameter 
# estimates of the drone and ground collected data for the trees in the 
# gradients at both flux sites. 
#
# This script performs the following steps for each site:
#   1. Imports the field measurement data
#   2. Creates a function to determine averages and errors for measurements
#   3. Visualizes the distribution of values for each measurement type

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(dplyr)
library(stringr)
library(terra)
library(sf)

library(ggplot2)
library(tidyverse)
library(patchwork)

#===============================================================================
#Format field observations------------------------------------------------------
#===============================================================================
SRGdat <-  read.csv("./Data/GitData/US-SRG_BiometGrad_07062025.csv")%>%
  mutate(Site = "US-SRG")%>%
  select(-Latitude, -Longitude)
SRMdat <-  read.csv("./Data/GitData/US-SRM_BiometGrad_12062025.csv")%>%
  mutate(Site = "US-SRM")%>%
  select(-Latitude, -Longitude)
coords <- read.csv("./Data/GitData/BiometCoords_25062025.csv", #issue with digits dropping, so import separate coord file as character
                   colClasses = "character")%>%
  mutate(Latitude  = str_sub(Latitude, 1, -2),
         Longitude = str_sub(Longitude, 1, -2))

#convert to UTM to work with rast and polys
coords_utm <- coords %>%
  mutate(across(c(Latitude, Longitude), as.numeric)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs = 32612)
coords_utm <- coords_utm %>%
  mutate(
    Easting = st_coordinates(.)[, 1],
    Northing = st_coordinates(.)[, 2]
  )%>%
  st_drop_geometry()

#form df
SRGstat <- merge(SRGdat, coords_utm, by = c("ID", "Site"))%>%
  group_by(ID)%>%
  mutate(Area = pi*((CanopyDiameter/2)^2))%>%
  reframe(Easting = Easting,
          Northing = Northing, 
          AvgHeight = mean(CanopyHeight, na.rm = T),
            SdHeight = sd(CanopyHeight, na.rm = T),
            AvgCanDi = mean(CanopyDiameter, na.rm = T),
            SdCanDi = sd(CanopyDiameter, na.rm = T),
            AvgArea = mean(Area, na.rm = T),
            SdArea = sd(Area, na.rm = T))%>%
  distinct()
SRMstat <- merge(SRMdat, coords_utm, by = c("ID", "Site"))%>%
  group_by(ID)%>%
  mutate(Area = pi*((CanopyDiameter/2)^2))%>%
  reframe(Easting = Easting,
          Northing = Northing,
            AvgHeight = mean(CanopyHeight, na.rm = T),
            SdHeight = sd(CanopyHeight, na.rm = T),
            AvgCanDi = mean(CanopyDiameter, na.rm = T),
            SdCanDi = sd(CanopyDiameter, na.rm = T),
            AvgArea = mean(Area, na.rm = T),
            SdArea = sd(Area, na.rm = T))%>%
  distinct()

#===============================================================================
#Format remote observations-----------------------------------------------------
#===============================================================================

canpols <- vect("./Data/GitData/canopypolys.shp")

SRGchm <- readRDS("./Data/GitData/SRGchm.RDS")
SRGchm[SRGchm < 0 | SRGchm > 20] <- NA

SRMchm <- rast("./Data/GitData/SRMchmRast.tif")

#overlap biomet points with canpols
SRGcoords <- coords_utm%>%
  filter(Site == "US-SRG")%>%
  select(-Site)

write.csv(SRGcoords, "./QGIS/SRGcoords.csv")

srgpoints <- vect(SRGcoords, geom = c("Easting", "Northing"), crs = "EPSG:32612")
srg_biomet_polys <- extract(canpols, srgpoints)



#generate stats
max_height <- extract(chm_masked, canpol, fun = max, na.rm = T)
names(max_height)[2] <- "Height"

areas <- expanse(canpol, unit = "m")
diameters <- 2 * sqrt((areas / pi))

TreeMeas <- data.frame(
  ID = canpol$id,
  Height = max_height$Height,
  Area = areas,
  Diameter = diameters,
  Survey = "Drone"
)%>%
  select(-ID)

#mask chm with canpols 

#calc stats

