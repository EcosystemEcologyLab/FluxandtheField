# At the US-SRM flux tower site, we completed a drone-based LiDAR scan. This 
# script generates and visualizes a canopy height model for the site. 
# 
# This script executes the following steps:
#   1. Obtains a CHM from the LiDAR collection 
#   2. Crops the CHM to a specified distance around the tower

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(lidR)
library(terra)

#===============================================================================
#Create canopy height model (CHM) from LiDAR------------------------------------
#===============================================================================
# LidarPath <- "Z:/Drone_Wim/SRERMesq5-27-2025/SRERMesq5-27-25/lidars/terra_las/cloud_merged.las"
# lidar <- readLAS(LidarPath)
# 
# lidar <- classify_ground(lidar, csf())
# las_norm <- normalize_height(lidar, tin())
# 
# chm_raster <- rasterize_canopy(las_norm, res = 1, algorithm = p2r(subcircle = 0.2))
# chm_raster[chm_raster < 0 | chm_raster > 20] <- NA
# 
# writeRaster(chm_raster, "./SRMchmRast.tif", overwrite = T)

#===============================================================================
#Focus CHM around flux tower----------------------------------------------------
#===============================================================================
chm_raster <- rast("./Data/GitData/SRMchmRast.tif")

#center raster at tower
ext <- ext(chm_raster)
center_x <- (ext[1] + ext[2]) / 2
center_y <- (ext[3] + ext[4]) / 2

zoom_radius <- 100 #will be square... not true radius

zoom_extent <- ext(
  center_x - zoom_radius,
  center_x + zoom_radius,
  center_y - zoom_radius,
  center_y + zoom_radius
)

chm_zoom <- crop(chm_raster, zoom_extent)

plot(chm_zoom, main = "CHM at 100m", axes = F)
