#########################################################################
# This code cleans the point cloud and generates Individual Tree Crown  #
# segments                                                              #
# input: a point cloud.                                                 #
# outputs: cleaned point cloud for outliers: normalized to topography:  #
#         canopy height model, tree tops, tree segmentation, save to    #
#         shape files                                                   #
#########################################################################

# clean up memory in global environment
rm(list = ls(globalenv()))

# import libraries
library(lidR)
library(sf)
library(terra)
library(raster)
# keep commented for bulk operations
#library(rgl) 

# 3D window open: keep comment for HPC
#rgl.open()

# function for local maxima filter variable custom window size based on tree height

site = 'ABBY'
year = '2022'

if (site == 'ONAQ') {    #for woodland sites like ONAQ
  custom_ws <- function(x) {
    y <- ceiling(1.211*x+0.024)
    y[x < 1] <- 3
    return(y)
  }
}else{
  custom_ws <- function(x) {   #for forest sites
    y <- (1/8) * x 
    y[x < 32] <- 4
    y[x > 80] <- 10
    return(y)
  }
}


# set up project directory

lasdir <- paste('/uufs/chpc.utah.edu/common/home/dycelab/users/jlu/vegfracover/data/NEON/', site, '/lidar',sep = "")    #directory to "classified_point_cloud_colorized.laz" data
chmdir <- paste('/uufs/chpc.utah.edu/common/home/dycelab/users/jlu/vegfracover/data/NEON/', site, '/CHM',sep = "")   #directory to NEON chm data
outdir <- paste('/uufs/chpc.utah.edu/common/home/dycelab/users/jlu/vegfracover/results/crowns_', site, '_',year,'_new/',sep = "")  #output directory for tree crown shapefiles

# listing data dir
subdirs <- list.dirs(lasdir, recursive = FALSE)
subdirs_year <- subdirs[grep(year, basename(subdirs))]
laslist <- c()
for (subdir in subdirs_year) {
  # List files in the subdirectory
  files <- list.files(subdir, pattern = "_classified_point_cloud.*\\.laz$", full.names = TRUE)
  # Append to the files_2022 vector
  laslist <- c(laslist, files)
}


counter <- 0
for (item in laslist) {
  
  mod_name <- gsub(".laz", "", item)
  base_name <- basename(mod_name)
  ID = extracted <- sub(".*_(\\d+_\\d+)_.*", "\\1", base_name)
  output_name <- paste0("NEON_", site, "_",year,"_",ID)
  out_shp_path <- paste0(outdir, paste0(output_name, ".shp"))
  
  if (!file.exists("out_shp_path")){
    
    try({
      
      
      # List all subdirectories
      subdirs <- list.dirs(path = chmdir, recursive = FALSE)
      # Filter subdirectories that start with "2017"
      year_subdirs <- grep(paste0(year, "-"), subdirs, value = TRUE)
      # Now search for the file in these filtered subdirectories
      chmfile <- list.files(path = year_subdirs, pattern = paste0(ID, ".*CHM\\.tif$"), full.names = TRUE, recursive = TRUE)
      chm <- raster(chmfile)
      
      
      file_path <- item
      # open Lidar data
      las <- readLAS(files = file_path, filter = "-drop_z_below 0")
      # mean and SD for threshold based filtering
      meanZ <- mean(las$Z, na.rm= TRUE)
      sdZ <- sd(las$Z, na.rm = TRUE)
      # thresholds filtering for bad Z (points)
      threshold1 <- meanZ + 5 * sdZ
      threshold2 <- meanZ - 5 * sdZ
      las_filter1 <- las[las$Z <= threshold1]
      las_filter2 <- las_filter1[las_filter1$Z >= threshold2]
      # generate DTM
      dtm_idw <- rasterize_terrain(las_filter2, res = 1, algorithm = knnidw(k = 4, p = 1))
      # height normalization(lidr documentation)
      las_normalized <- normalize_height(las_filter2, dtm_idw, na.rm = TRUE)
      # drop z more than 120
      las_normalized <- las_normalized[las_normalized$Z <= 120]
      
      
      kernel <- matrix(1,3,3)
      chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)
      # tree tops
      ttops <- locate_trees(las = chm_smoothed, algorithm = lmf(ws = custom_ws, hmin=1)) # lms 2.5
      
      if (nrow(ttops) > 0 ) {
        # crown delineation / segmentation (works well from seed=0.5, cr =0.6)
        tree_segments <- segment_trees(las = las_normalized,
                                       algorithm = dalponte2016(chm = chm_smoothed,
                                                                treetops = ttops,
                                                                th_tree =  2,
                                                                th_seed = 0.5,
                                                                th_cr = 0.5,
                                                                max_cr = 20))   #max_cr = 10 for woodlands sites like ONAQ
        num_of_trees <- length(unique(tree_segments$treeID) |> na.omit())
      }
      
      
      # check for partial trees
      if (num_of_trees > 0){
        # draw crowns
        crowns <- crown_metrics(tree_segments, func = .stdtreemetrics, geom = "concave")
        #plot(crowns["convhull_area"], main = "Crown area") # uncomment to plot
        
        # convert to shp files
        #cvx_hulls_sf <- st_as_sf(crowns["convhull_area"], coords = c("X", "Y"), crs = st_crs(las))
        cvx_hulls_sf <- st_as_sf(crowns, coords = c("X", "Y"), crs = st_crs(las))
        
        # modified name for saving files
        # output_name <- sub("_classified_point_cloud_colorized$", "", base_name)
        
        # save as shape files
        st_write(cvx_hulls_sf, out_shp_path, append=FALSE )
      }else{
        print("No full trees to deliniate **********************************")
      }
      counter <- counter + 1
      print(paste('Tested trees', counter, sep = "-"))
    },
    silent = TRUE)
  }
}
