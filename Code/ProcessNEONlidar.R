library(lidR)
library(VoxR)
library(terra)
library(tidyverse)
library(data.table)

# Define folder with NEON .laz files
folder_path <- "C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/FluxandtheField/Data/NEON/DP1.30003.001/neon-aop-products/2017/FullSite/D14/2017_SRER_1/L1/DiscreteLidar/ClassifiedPointCloud"

# List all .laz files in the folder
las_files <- list.files(folder_path, pattern = "\\.laz$", full.names = TRUE)







LidarPath <- las_files[1] #srm_las

las <- readLAS(LidarPath)
las <- classify_ground(las, algorithm = csf())
dtm <- rasterize_terrain(las, algorithm = tin(), res = 1)
las <- normalize_height(las, dtm)


# Create LAScatalog
ctg <- catalog(las_files)

# CRS string for SRER (UTM Zone 12N NAD83)
crs_string <- "EPSG:26912"  # You can also use full proj4 string if needed

# Set processing options
opt_independent_files(ctg) <- TRUE
opt_progress(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0   # no overlap, tile by tile processing
opt_chunk_size(ctg) <- 0

# Function to process each tile
process_tile <- function(tile, ...) {
  tile_path <- attr(tile, "chunk_path")
  tile_id <- tools::file_path_sans_ext(basename(tile_path))
  
  # Assign CRS manually to tile
  projection(tile) <- crs_string
  
  # Normalize height
  tile <- normalize_height(tile, tin())
  
  # Filter vegetation points with classes 3,4,5 and height 0-7.5 m
  tile <- lasfilter(tile, Classification %in% c(3,4,5) & Z >= 0 & Z <= 7.5)
  if (is.empty(tile)) return(NULL)
  
  # Convert to data.table for voxelization
  pts_xyz <- as.data.table(tile@data)[, .(X, Y, Z)]
  vox_side <- 0.025
  vox_space <- vox(pts_xyz, res = vox_side)
  
  # Calculate biomass per voxel
  vox_space$voxel_volume <- vox_side^3
  biomass_density <- 0.78 * 1000  # kg/m3
  vox_space$biomass_kg <- vox_space$voxel_volume * biomass_density
  vox_dt <- as.data.table(vox_space)
  
  # Filter voxels to remove grasslike noise
  vox_dt <- vox_dt[, {
    if (.N >= 3) .SD
    else if (max(z) >= 0.65) .SD
    else NULL
  }, by = .(x, y)]
  
  if (nrow(vox_dt) == 0) return(NULL)
  
  biomass_by_xy <- vox_dt[, .(biomass_kg = sum(biomass_kg)), by = .(x, y)]
  biomass_by_xy[, biomass_kg_m2 := biomass_kg / (vox_side^2)]
  
  # Rename columns for terra raster xyz format
  df <- as.data.frame(biomass_by_xy[, .(x, y, biomass_kg)])
  names(df) <- c("x", "y", "z")
  
  biomass_rast <- rast(x = df,
                       type = "xyz",
                       crs = crs_string)
  
  tile_area_m2 <- terra::expanse(biomass_rast, unit = "m")
  tile_area_ha <- tile_area_m2 / 10000
  total_kg <- sum(biomass_by_xy$biomass_kg, na.rm = TRUE)
  total_Mg <- total_kg / 1000
  Mg_ha <- total_Mg / tile_area_ha
  
  list(
    tile_id = tile_id,
    total_biomass_Mg = total_Mg,
    biomass_Mg_per_ha = Mg_ha,
    biomass_raster = biomass_rast
  )
}

# Run the processing over all tiles
results_list <- catalog_apply(ctg, process_tile)
