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
SRGchm <- readRDS("./Data/GitData/SRGchm.RDS")
SRGchm[SRGchm < 0 | SRGchm > 20] <- NA

# Load canopy polygons and corrected tree point locations
canpols <- vect("./Data/GitData/canopypolys.shp")
corrected_coords <- vect("./QGIS/SRGCorrectedTreePoints.shp")
corrected_coords_utm <- project(corrected_coords, crs(canpols))


r <- rast(canpols, resolution=0.1)
if (!("polyID" %in% names(canpols))) {
  canpols$polyID <- 1:length(canpols)
}
r_poly <- rasterize(canpols, r, field="polyID")

extracted_polys <- terra::extract(r_poly, corrected_coords_utm)
ids <- unique(extracted_polys[,2]) 
SRGbiomet_polys <- canpols[canpols$polyID %in% ids, ]

max_height <- terra::extract(SRGchm, SRGbiomet_polys, fun = max, na.rm = TRUE)
names(max_height)[2] <- "Height"
areas <- expanse(SRGbiomet_polys, unit = "m")
diameters <- 2 * sqrt(areas / pi)

poly_attr <- data.frame(
  polyID = SRGbiomet_polys$polyID,
  Height_Drone = max_height$Height,
  Area_Drone = areas,
  Diameter_Drone = diameters
)
extracted_polys <- terra::extract(r_poly, corrected_coords_utm)
colnames(extracted_polys) <- c("point_row", "polyID")
points_df <- data.frame(
  point_row = 1:length(corrected_coords_utm),
  pointID = corrected_coords_utm$id  # replace 'id' with actual point ID field name
)

TreeMeas <- extracted_polys %>%
  left_join(points_df, by = "point_row") %>%
  left_join(poly_attr, by = "polyID") %>%
  select(ID = pointID, Height_Drone, Area_Drone, Diameter_Drone) %>%
  mutate(Survey = "Drone")


comp_df_srg <- merge(TreeMeas, SRGstat, by = "ID")%>%
  select(ID, Height_Drone, Area_Drone, Diameter_Drone, AvgHeight, AvgCanDi, AvgArea)

#===============================================================================
#Plotting-----------------------------------------------------------------------
#===============================================================================
height_long <- comp_df_srg %>%
  select(ID, Drone = Height_Drone, Ground = AvgHeight) %>%
  pivot_longer(cols = c(Drone, Ground),
               names_to = "Source", values_to = "Value") %>%
  mutate(Trait = "Height")

diameter_long <- comp_df_srg %>%
  select(ID, Drone = Diameter_Drone, Ground = AvgCanDi) %>%
  pivot_longer(cols = c(Drone, Ground),
               names_to = "Source", values_to = "Value") %>%
  mutate(Trait = "Canopy Diameter")

area_long <- comp_df_srg %>%
  select(ID, Drone = Area_Drone, Ground = AvgArea) %>%
  pivot_longer(cols = c(Drone, Ground),
               names_to = "Source", values_to = "Value") %>%
  mutate(Trait = "Canopy Area")

plot_data <- bind_rows(height_long, diameter_long, area_long)

ggplot(plot_data, aes(x = factor(ID), y = Value, fill = Source)) +
  geom_col(position = position_dodge(), alpha = 0.8) +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "",
       x = "Tree ID", y = NULL) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#===============================================================================
#residuals
resids_df <- comp_df_srg %>%
  filter(ID %in% 1:10) %>%                 # keep your ID filter if needed
  transmute(
    ID,
    Height   = Height_Drone   - AvgHeight,
    Diameter = Diameter_Drone - AvgCanDi,
    Area     = Area_Drone     - AvgArea
  )

resids_long <- resids_df %>%
  pivot_longer(-ID, names_to = "Trait", values_to = "Residual")

resids_hd <- resids_long %>% 
  filter(Trait %in% c("Height", "Diameter"))

ylim <- max(abs(resids_hd$Residual))

ggplot(resids_hd,
       aes(x = factor(ID), y = Residual, fill = Trait)) +
  geom_col() +
  facet_wrap(~ Trait) +                     # two facets
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-ylim, ylim)) +
  scale_fill_manual(values = rep("steelblue", 2), guide = "none") +
  labs(x = "Tree ID", y = "Residual (Drone - Ground)") +
  theme_minimal()


