#CHM for SRM

library(lidR)
library(terra)

LidarPath <- "Z:/Drone_Wim/SRERMesq5-27-2025/SRERMesq5-27-25/lidars/terra_las/cloud_merged.las"

lidar <- readLAS(LidarPath)

lidar <- classify_ground(lidar, csf())

las_norm <- normalize_height(lidar, tin())

chm_raster <- rasterize_canopy(las_norm, res = 1, algorithm = p2r(subcircle = 0.2))

chm_raster[chm_raster < 0 | chm_raster > 20] <- NA

#writeRaster(chm_raster, "./SRMchmRast.tif", overwrite = T)

#zoom in to 30m of the tower
ext <- ext(chm_raster)
center_x <- (ext[1] + ext[2]) / 2
center_y <- (ext[3] + ext[4]) / 2

# Define zoom area (e.g., 50m x 50m)
zoom_width <- 100
zoom_height <- 100

zoom_extent <- ext(
  center_x - zoom_width / 2,
  center_x + zoom_width / 2,
  center_y - zoom_height / 2,
  center_y + zoom_height / 2
)

# Crop to zoomed extent
chm_zoom <- crop(chm_raster, zoom_extent)

# Plot
plot(chm_zoom, main = "CHM at 100m", axes = F)
