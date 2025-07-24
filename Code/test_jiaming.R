library(lidR)
library(sf)
library(terra)
library(raster)
library(dplyr)
library(ggplot2)

out_shp_path <- "./Data/testJiamingcrowns2.shp"


LidarPath <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

las <- readLAS(LidarPath)
las <- classify_ground(las, algorithm = csf())
dtm <- rasterize_terrain(las, algorithm = tin(), res = 1)
las <- normalize_height(las, dtm)
crop_radius <- 100
crop_area_m2 <- (2 * crop_radius)^2
crop_area_ha <- crop_area_m2 / 10000

center_x <- mean(las@data$X)
center_y <- mean(las@data$Y)
las_filt <- filter_poi(las, {
  Z >= 0 & Z <= 7.5 &
    X >= (center_x - crop_radius) & X <= (center_x + crop_radius) &
    Y >= (center_y - crop_radius) & Y <= (center_y + crop_radius)
})

#load chm and crop to 100m of tower
chm_load <- rast("./Data/0.5mSRGchm.tif")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")
chm_crop <- crop(chm_load, survey_outline)
plot(chm_crop)
chm <- chm_crop

#create custom window function for ttop identification
# custom_ws <- function(x) {
#   y <- ceiling(1.211*x+0.024)
#   y[x < 1] <- 3
#   return(y)
# }


# custom_ws <- function(x) {
#   y <- ceiling(1.211 * x + 0.024)
#   y[x < 2] <- 5
#   return(y)
# }
custom_ws <- function(x) {
  y <- ceiling(0.5 * x + 0.024)
  y[x < 3] <- 6
  return(y)
}
#smooth chm raster 
kernel <- matrix(1,3,3)
chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)

#identify tree tops
ttops <- locate_trees(las = chm_smoothed, algorithm = lmf(ws = custom_ws, hmin=1)) # lms 2.5

plot(chm_smoothed)
plot(ttops, col = "black", add = TRUE, cex = 0.5)








if (nrow(ttops) > 0) {
  
  # Crown delineation / segmentation
  tree_segments <- segment_trees(
    las = las_filt,
    algorithm = dalponte2016(
      chm = chm_smoothed,
      treetops = ttops,
      th_tree = 2,
      th_seed = 0.5,
      th_cr = 0.5,
      max_cr = 20
    )
  )
  
  # Count the number of unique tree IDs (exclude NAs)
  num_of_trees <- length(unique(tree_segments$treeID[!is.na(tree_segments$treeID)]))
  
  if (num_of_trees > 0) {
    # Compute crown metrics with concave hull geometry (better for irregular crowns)
    crowns <- crown_metrics(tree_segments, func = .stdtreemetrics, geom = "concave")
    
    # The crowns object is a Spatial* or sf object with crown polygons and metrics
    # Convert crowns to sf if not already
    if (!inherits(crowns, "sf")) {
      crowns_sf <- st_as_sf(crowns)
    } else {
      crowns_sf <- crowns
    }
    
    # Optional: inspect crown area metric
    print(summary(crowns_sf$convhull_area))  # check crown area distribution
    
    # Save crown polygons as shapefile
    st_write(crowns_sf, out_shp_path, append = FALSE)
    
  } else {
    print("No full trees to delineate **********************************")
  }
  
  counter <- counter + 1
  print(paste("Tested trees", counter, sep = "-"))
  
} else {
  print("No treetops detected **********************************")
}


#mask crown output by survey boundary

survbound <- vect("./QGIS/100msurveybuffer.shp")
survbound_sf <- st_as_sf(survbound)

# Filter crowns whose centroid is within the survey boundary
crowns_sf_centroids <- st_centroid(crowns_sf)

# Identify which centroids fall inside the boundary
inside <- st_within(crowns_sf_centroids, survbound_sf, sparse = FALSE)[,1]

# Subset only those crowns
filt_crowns <- crowns_sf[inside, ]

filt_crowns$area <- st_area(filt_crowns) |> as.numeric()

#===============================================================================
#compare to manually delineated crowns
# n.b. for a circle, not square

chm <- readRDS("./Data/GitData/SRGchm.RDS")
chm[chm < 0 | chm > 20] <- NA

#use manually traced polygons to calc canopy stats
sfpath <- "./Data/GitData/canopypolys.shp"
canpol <- vect(sfpath)
#plot(canpol)

chm_crop <- crop(chm, canpol)
chm_masked <- mask(chm_crop, canpol)

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

summary(TreeMeas$Area)














auto_df <- data.frame(
  Area = filt_crowns$area,
  Method = "R"
)

# Create a dataframe from the TreeMeas area
manual_df <- data.frame(
  Area = TreeMeas$Area,
  Method = "Manual"
)

# Combine both into one dataframe
area_comparison <- rbind(auto_df, manual_df)


# Density plot
ggplot(area_comparison, aes(x = Area, fill = Method)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Canopy Area Distribution",
       x = "Area (m2)",
       y = "Density") +
  theme_minimal()
ggplot(area_comparison, aes(x = Area, fill = Method)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "",
    x = "Canopy Area (m²)",
    y = "Count of Crowns"
  ) +
  theme_minimal()





# Test for significant difference in area distributions
wilcox.test(Area ~ Method, data = area_comparison)
