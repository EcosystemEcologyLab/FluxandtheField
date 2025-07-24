#mesquite mask
mesmask <- rast("X:/moore/SRER/RF_allyr_SRER_norton.tif")


# Number of bands
nlyr(mesmask)

# Band names
names(mesmask)

# Print summary info for each band
summary(mesmask)

# Inspect first few values of each band
values <- values(mesmask, mat = FALSE)
head(values)

# Load packages
library(lidR)
library(sf)
library(terra)

# Read in LiDAR file and set some color palettes
col <- height.colors(50)
col1 <- pastel.colors(900)

chm <- rast("./Data/0.5mSRGchm.tif")
survey_outline <- vect("./QGIS/100msurveybuffer.shp")
chm_crop <- crop(chm, survey_outline)
plot(chm_crop, col = col)


# Generate kernel and smooth chm
schm <- chm
kernel <- matrix(1,3, 3)
schm <- terra::focal(x = chm_crop, w = kernel, fun = median, na.rm = TRUE)
plot(schm, col = col)

ttops <- locate_trees(las = schm, algorithm = lmf(ws = function(x) { 0.5 * x + 2 }))
plot(schm, col = col)
plot(ttops, col = "black", add = TRUE, cex = 0.5)




#-------------------
#using ForestTools
library(ForestTools)
winFun <- function(h) pmin( 0.1 + 2.5 * h, 6 )
# winFun <- function(h) pmin( 0.3 + 1.5 * h, 5 )
# winFun <- function(h) pmin( 0.3 + 1.7 * h, 5 )

ttops <- vwf(schm, winFun, minHeight = 0.5, warnings = T, minWinNeib = "queen", IDfield = "treeID", resolution_round = 5)

plot(schm, col = col)
plot(ttops, col = "black", add = TRUE, cex = 0.5)



segments <- mcws(ttops, chm)



crowns_li <- crown_metrics(MBA_treeseg_li, func = NULL,  geom = "concave")
windows()
plot(sf::st_geometry(crowns_li), reset = FALSE)

lidR::plot(MBA_treeseg_li, color ="treeID")