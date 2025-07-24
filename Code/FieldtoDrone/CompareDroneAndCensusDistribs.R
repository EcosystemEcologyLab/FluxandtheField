# At the US-SRG flux tower site, we conducted a woody plant census within a 100m
# radius of the tower and collected LiDAR with a drone. This script compares the 
# distributions of tree height, canopy diameter, and canopy area between the two
# data collections. 
# 
# This script executes the following steps:
#   1. Obtains a CHM from the LiDAR collection
#   2. Masks CHM with outlines of tree canopies to obtain height and diameter
#   3. Combines ground and drone datasets into a single data frame
#   4. Visualizes distributions of height, diameter, and area
#   5. Performs Kolmogorov-Smirnov and Wilcoxon statistical tests

#===============================================================================
#Load necessary packages--------------------------------------------------------
#===============================================================================
library(lidR)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

#===============================================================================
#Obtain canopy height model (CHM) from LiDAR------------------------------------
#===============================================================================
# LidarPath <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"
# 
# las <- readLAS(LidarPath)
# 
# if (is.null(las@data$Classification) || !any(las@data$Classification %in% c(2))) {
#   las <- classify_ground(las, algorithm = csf())
# }
# 
# dtm <- rasterize_terrain(las, res = 0.5, algorithm = knnidw(k = 10, p = 2))
# dsm <- rasterize_canopy(las, res = 0.5, algorithm = p2r(subcircle = 0.15))
# chm <- dsm - dtm
# chm[chm < 0 | chm > 20] <- NA
# 
# plot(chm)
# writeRaster(chm, "./Data/0.5mSRGchm.tif", overwrite = T)

#===============================================================================
#Create a dataframe with field and drone observations----------------------------
#===============================================================================
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

#===============================================================================
#Combine drone and ground datasets----------------------------------------------
#===============================================================================
#combine with ground data:
lindsey_dat <- read.csv("./Data/GitData/US-SRG_WoodyPlantCensus_28052025.csv")
filt_lindsey_dat <- lindsey_dat%>%
  group_by(ID)%>%
  reframe(Species = Species,
            Diameter = mean(CrownDiameter, na.rm = T),
            Height = Height)%>%
  filter(Species != "",)%>%
  ungroup()%>%
  select(-ID)%>%
  mutate(Survey = "Ground",
         Area = pi*((Diameter/2)^2))%>%
  filter(Species %in% c("MES", "HAC", "CHAC", "GTN", "WTA", "SALIX", "ACIA", "WAC"))%>%
  select(-Species)

combd_dat <- bind_rows(filt_lindsey_dat, TreeMeas)

#===============================================================================
#Visualize distributions--------------------------------------------------------
#===============================================================================
combd_dat$Survey <- factor(combd_dat$Survey, levels = c("Ground", "Drone"))
p1 <- ggplot(combd_dat, aes(x = Height, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Height (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.position = "none")

p2 <- ggplot(combd_dat, aes(x = Diameter, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Canopy Diameter (m)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none")

p3 <- ggplot(combd_dat, aes(x = Area, fill = Survey)) +
  geom_histogram(position = "identity", alpha = 0.85, bins = 30) +
  scale_fill_manual(values = c("Drone" = "#91bfdb", "Ground" = "#fc8d59")) +
  theme_minimal() +
  labs(title = "",
       x = "Canopy Area (m-2)",
       y = "Count",
       fill = "Survey")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15))

p1+p2+p3

#===============================================================================
#Statistically compare distributions -------------------------------------------
#===============================================================================

ks.test(filt_lindsey_dat$Height, TreeMeas$Height)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$Diameter, TreeMeas$Diameter)
# ^ distribs are sig dif
ks.test(filt_lindsey_dat$Area, TreeMeas$Area)
# ^ distribs are sig dif

wilcox.test(Height ~ Survey, data = combd_dat)
# ^ medians are not sig dif
wilcox.test(Diameter ~ Survey, data = combd_dat)
# ^ medians are sig dif
wilcox.test(Area ~ Survey, data = combd_dat)
# ^ medians are sig dif
