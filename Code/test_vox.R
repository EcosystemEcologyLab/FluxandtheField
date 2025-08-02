library(VoxR)
library(lidR)
library(raster)
library(tidyverse)
library(data.table)

#
LidarPath <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

las <- readLAS(LidarPath)
las <- classify_ground(las, algorithm = csf())
dtm <- rasterize_terrain(las, algorithm = tin(), res = 1)
las <- normalize_height(las, dtm)

#crop to 1-ha around tower
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

#voxelize las
vox_side <- 0.025  # 5-cm
pts_xyz <- data.table::as.data.table(las_filt@data)[, .(X, Y, Z)]
vox_space <- vox(pts_xyz, res = vox_side)

#calculate biomass units/conversion
vox_space$voxel_volume <- vox_side^3  #m3 per voxel
biomass_density <- 0.78 * 1000        #kg/m3
vox_space$biomass_kg <- vox_space$voxel_volume * biomass_density

#make table and filter grass points
vox_dt <- as.data.table(vox_space)
vox_dt <- vox_dt[, {
  if (.N >= 3) .SD
  else if (max(z) >= 0.65) .SD
  else NULL
}, by = .(x, y)]

#calc biomass + height per column
biomass_by_xy <- vox_dt[, .(biomass_kg = sum(biomass_kg)), by = .(x, y)]
height_by_xy <- vox_dt[, .(max_z = max(z)), by = .(x, y)]

#biomass/m2
biomass_by_xy[, biomass_kg_m2 := biomass_kg / (vox_side^2)]

#rasterize
biomass_raster <- rast(x = biomass_by_xy[, .(x, y, biomass_kg)],
                       type = "xyz",
                       crs = crs(las_filt))
#height_raster <- rast(x = height_by_xy, type = "xyz", crs = crs(las_filt))
#plot(biomass_raster, col = viridis(100), main = "Biomass (kg per column)")

biomass_sum <- terra::extract(biomass_raster, survey_outline, fun = sum, na.rm = TRUE)
biomass_sum*0.001

#summarize total
total_biomass_kg <- sum(biomass_by_xy$biomass_kg)
total_biomass_Mg <- total_biomass_kg / 1000
biomass_Mg_per_ha <- total_biomass_Mg / crop_area_ha
cat("Total biomass: ", round(total_biomass_Mg, 2), "Mg\n")
cat("Biomass per hectare: ", round(biomass_Mg_per_ha, 2), "Mg/ha\n")

#writeRaster(biomass_raster, "./Data/voxtest.tif", overwrite = TRUE)

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#mask with canopy polygons for biomet grad
# Load libraries
library(terra)
library(sf)
library(dplyr)
library(data.table)

#select biomet tree polygons
canpols <- vect("./Data/GitData/canopypolys.shp")
corrected_coords <- vect("./QGIS/SRGCorrectedTreePoints.shp")

corrected_coords_utm <- project(corrected_coords, crs(canpols))
r <- rast(canpols, resolution = 0.1)
if (!("polyID" %in% names(canpols))) {
  canpols$polyID <- 1:length(canpols)
}
manual_mapping <- data.frame(
  TreeID = 1:10,
  polyID = c(243, 248, 339, 335, 141, 142, 306, 26, 32, 3)
)
unique_polys <- unique(manual_mapping$polyID)
SRGbiomet_polys <- canpols[canpols$polyID %in% unique_polys, ]

#make cnopy mask
r_mask <- rasterize(SRGbiomet_polys, r, field = "polyID")

#convert voxels to raster
vox_dt[, X_bin := round(x, 1)]
vox_dt[, Y_bin := round(y, 1)]

r_biomass <- rast(
  data.frame(x = vox_dt$X_bin, y = vox_dt$Y_bin, biomass_kg = vox_dt$biomass_kg),
  type = "xyz", crs = crs(r_mask)
)

#mask vox raster
r_biomass_aligned <- resample(r_biomass, r_mask, method = "bilinear")
masked_biomass <- mask(r_biomass_aligned, r_mask)

#extract values
biomass_by_poly <- terra::extract(masked_biomass, SRGbiomet_polys, fun = sum, na.rm = TRUE)
colnames(biomass_by_poly)[2] <- "Biomass_kg"

#add correct IDs
poly_biomass_table <- data.frame(
  polyID = SRGbiomet_polys$polyID,
  Biomass_kg = biomass_by_poly$Biomass_kg
)
tree_biomass <- left_join(manual_mapping, poly_biomass_table, by = "polyID")

saveRDS(tree_biomass, "./Data/biomet_vox_biomass.RDS")






tree_biomass <-tree_biomass%>%
  mutate(method = "Voxelization")%>%
  rename(ID = TreeID)

combd <- merge(SRGdat, tree_biomass, by = "ID")

library(dplyr)
library(ggplot2)
library(tidyr)

# Reshape data to long format
combd_long <- combd %>%
  select(ID, biomass_allometric = biomass, biomass_voxel = Biomass_kg) %>%
  pivot_longer(cols = c(biomass_allometric, biomass_voxel),
               names_to = "Method", values_to = "Biomass_kg")

# Clean method labels
combd_long$Method <- recode(combd_long$Method,
                            biomass_allometric = "Allometry",
                            biomass_voxel = "Voxelization")

# Calculate total biomass per method across all trees
total_by_method <- combd_long %>%
  group_by(Method) %>%
  summarize(Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE))

print(total_by_method)

# Reorder IDs by total biomass per tree (optional for nicer plot)
total_biomass_per_tree <- combd_long %>%
  group_by(ID) %>%
  summarize(Total_Biomass_kg = sum(Biomass_kg))

combd_long$ID <- factor(combd_long$ID,
                        levels = total_biomass_per_tree %>% arrange(Total_Biomass_kg) %>% pull(ID))

# Plot with bars grouped by tree and totals labeled in the plot subtitle
ggplot(combd_long, aes(x = ID, y = Biomass_kg, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Allometry" = "#1b9e77", "Voxelization" = "#d95f02")) +
  labs(
    x = "Tree ID", y = "Biomass (kg)", 
    title = "Comparison of Biomass by Method",
    subtitle = paste0(
      "Allometry Total: ", round(total_by_method$Total_Biomass_kg[total_by_method$Method == "Allometry"], 1), " kg;  ",
      "Voxelization Total: ", round(total_by_method$Total_Biomass_kg[total_by_method$Method == "Voxelization"], 1), " kg"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
